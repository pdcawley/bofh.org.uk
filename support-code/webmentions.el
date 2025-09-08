;;; webmentions.el --- Code to manage webmentions    -*- lexical-binding: t; -*-

;; First saved in 2025 by  Piers Cawley


;; Author: Piers Cawley <piers@singingtogether.co.uk>


;;; Commentary:

;; One of these days, this will be fully automated, but right now, this is just
;; a pile of helper code

;;; Code:

(defvar wm-webmention-endpoint "https://webmention.io/api/mentions.jf2")
(defvar wm-site-dir (locate-dominating-file (or load-file-name buffer-file-name) ".git/"))
(defvar wm-data-dir (expand-file-name "data/mentions/" wm-site-dir))
(defvar wm-last-mention-timestamp)
(defvar wm-site-key)
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

(defun wm-toplevel-reducer (acc mention)
  (require 'dash)
  (let* ((path (--> mention
                    (gethash "wm-target" it)
                    (url-generic-parse-url it)
                    (url-path-and-query it)
                    (car it)))
         (subgroup (vconcat (gethash path acc (vector)) (vector mention))))

    (puthash path subgroup acc)
    acc))

(defun wm-group-mentions-by-type (mentions-vec)
  (seq-reduce (lambda (acc mention)
                (let* ((type (gethash "wm-property" mention))
                       (group (vconcat (gethash type acc (vector))
                                       (vector mention))))
                  (puthash type group acc)
                  acc))
              mentions-vec (make-hash-table :test 'equal)))

(defun wm-fixup-simple-hash (mentions-hash)
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (puthash key (wm-group-mentions-by-type value) result))
             mentions-hash)
    result))

(defun wm-unflatten-mentions (mentions-vec)
  (wm-fixup-simple-hash
   (seq-reduce 'wm-toplevel-reducer mentions-vec
               (make-hash-table :test 'equal))))


(defun wm-fetch-mentions ()
  "Fetch the webmentions of `wm-domain'."
  (interactive)
  (require 'request)
  (require 'dash)
  (require 'seq)
  (require 'ht)
  (save-current-buffer
    (let ((all-entries (wm--fetch-all)))
      (with-temp-file (expand-file-name "all.json" wm-data-dir)
        (message "Writing to %s" (buffer-file-name))
        (erase-buffer)
        (json-insert (wm-unflatten-mentions all-entries))))))

(provide 'webmentions)
;;; webmentions.el ends here
