;;; wm.el --- Code to manage webmentions    -*- lexical-binding: t; -*-

;; First saved in 2025 by  Piers Cawley


;; Author: Piers Cawley <piers@singingtogether.co.uk>


;;; Commentary:

;; One of these days, this will be fully automated, but right now, this is just
;; a pile of helper code

;;; Code:
(use-package webmentions
  :straight (:type git :host github :repo "wkearn/webmentions")
  :init (require 'webmentions))

(defvar wm-webmention-endpoint "https://webmention.io/api/mentions.jf2")
(defvar wm-site-dir (locate-dominating-file (or load-file-name buffer-file-name) ".git/"))
(defvar wm-data-dir (expand-file-name "data/" wm-site-dir))
(defvar wm-last-mention-timestamp)
(defvar wm-site-key)
(defvar wm-hugo-public-dir (expand-file-name "public/" wm-site-dir))
(defvar wm-base-url "https://bofh.org.uk/")
(defun wm-last-checked ()
  ;; Eventually use the most recent web mention in our feed
  nil)

(defun wm--fetch-all ()
  "Fetch all the webmentions relating to our domain.

We fetch them 100 a time and return a vector. The domain of interest is grabbed
from the `WM_API_DOMAIN' environment variable, and the necessary Webmention API
token comes from `WM_API_TOKEN', also in the environment. The idea being that
these values don't sneak into a git repo and can be easily supplied by any CI
tools, or something like `direnv'.i"
  (let ((all-entries (vector))
        (page-size 100)
        (wm-domain (or (getenv "WM_API_DOMAIN")
                       (error "WM_API_DOMAIN not in the environment")))
        (wm-token (or (getenv "WM_API_TOKEN")
                      (error "WM_API_TOKEN not in the environment"))))
    (let (entries
          (page-index 0)
          (more? t))
      (while more?
        (request
          wm-webmention-endpoint
          :params `(("domain"   . ,wm-domain)
                    ("token"    . ,wm-token)
                    ("page"     . ,page-index)
                    ("per-page" . ,page-size)
                    ("sort-dir" . "up"))
          :parser 'json-parse-buffer
          :sync t
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (if-let* ((entries (gethash "children" data (vector))))
                          (progn
                            (setq all-entries (vconcat all-entries entries)
                                  more? (and entries (eql page-size (length entries)))
                                  page-index (1+ page-index)))
                        (setq more? nil)))))
        ;; Be a good netizen
        (sleep-for 1)))
    all-entries))

(defun wm-new-mentions-hash ()
  "Make a new empty hash to hold categorised webmention data."
  (copy-hash-table #s(hash-table
                      test equal
                      data ("like-of" [] "in-reply-to" []
                            "mention-of" [] "repost-of" []
                            "other" []))))


(defun wm--add-mention-to-hash-table (acc mention)
  "Helps reduce a list of mentions into a two level hash."
  (require 'dash)
  (let* ((path (--> mention
                    (gethash "wm-target" it)
                    (url-generic-parse-url it)
                    (url-path-and-query it)
                    (car it)))
         (mentions-hash (or (gethash path acc nil)
                            (wm-new-mentions-hash)))
         (mention-type (gethash "wm-property" mention))
         (mentions (or (gethash mention-type mentions-hash)
                       (progn
                         (setq mention-type "other")
                         (gethash mention-type mentions-hash))))
         (new-mentions (if (seq-contains mentions mention)
                           mentions
                         (vconcat mentions (list mention)))))
    (puthash mention-type new-mentions mentions-hash)
    (puthash path mentions-hash acc)
    acc))

(defun wm-unflatten-mentions (mentions-vec)
  (seq-reduce 'wm--add-mention-to-hash-table mentions-vec
              (make-hash-table :test 'equal)))

(defun wm-fetch-mentions ()
  "Fetch the webmentions of `wm-domain'."
  (interactive)
  (require 'request)
  (require 'dash)
  (require 'seq)
  (save-current-buffer
    (let ((all-entries (wm--fetch-all)))
      (with-temp-file (expand-file-name "webmentions.json" wm-data-dir)
        (erase-buffer)
        (json-insert (wm-unflatten-mentions all-entries))))))


(defun wm-url-for-file (file)
  "Given a file name"
  (require 's)
  (format "%s%s"
          wm-base-url
          (s-chop-prefixes
           (list (expand-file-name wm-site-dir)
                 "testing/"
                 "public/")
           (expand-file-name file wm-site-dir))))


(defun wm-outgoing-links ()
  "Collect outgoing link dom elements from the current buffer's h-entry."
  (require 'dom)
  (--> (libxml-parse-html-region (point-min) (point-max))
       (dom-by-tag it 'article)
       (dom-by-class it "h-entry")
       (dom-by-tag (car it) 'a)
       (mapcar (lambda (node)
                 (require 'dom)
                 (url-expand-file-name
                  (dom-attr node 'href)
                  (wm-url-for-file (buffer-file-name))))
               it)
       (-reject (-partial #'s-prefix-p "mailto:") it)
       (-uniq it)))

(defun wm-send-mentions (file)
  "Send all the mentions in the given file"
  (interactive "fFile: ")
  (save-excursion
    (find-file file)
    (let ((source (wm-url-for-file file)))
      (dolist (target (wm-outgoing-links))
        (message "Sending %s -> %s" source target)
        (ignore-errors
          (webmention-send-post source target))))))


;;; This section is straight up lifted from
(provide 'wm)
;;; wm.el ends here
