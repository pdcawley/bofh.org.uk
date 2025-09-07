;;; webmentions.el --- Code to manage webmentions    -*- lexical-binding: t; -*-

;; First saved in 2025 by  Piers Cawley


;; Author: Piers Cawley <pdcawley@Studio-Mini.lan>


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


(defun wm-fetch-mentions ()
  "Fetch the webmentions of `wm-domain'."
  (interactive)
  (use-package request
    :autoload request)
  (use-package dash
    :autoload (-compose -rpartial -partial))
  (require 'seq)
  (require 'ht)
  (save-current-buffer
    (let ((page-index 0)
          (page-size 100)
          (more? t)
          (all-entries (vector))
          entries)
      (while more?
        (request
          wm-webmention-endpoint
          :params `(("domain" . ,(or (getenv "WM_API_DOMAIN")
                                     (error "WM_API_DOMAIN not set!")))
                    ("token" . ,(or (getenv "WM_API_TOKEN")
                                    (error "WM_API_TOKEN not set!")))
                    ("page" . ,page-index)
                    ("per-page" . ,page-size)
                    ("sort-dir" . "up"))
          :parser 'json-parse-buffer
          :sync t
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (if-let* ((entries (ht-get data "children")))
                          (progn
                            (setq all-entries (vconcat all-entries entries)
                                  more? (and entries (eql page-size (length entries)))
                                  page-index (1+ page-index)))
                        (setq more? nil))))))
      (let ((last-entry (seq-elt all-entries (1- (length all-entries))))
            (site-dirlocals-file (expand-file-name ".dir-locals.el" ())))
        (modify-dir-local-variable nil
                                   'wm-last-mention-timestamp
                                   (ht-get last-entry "wm-received")
                                   'add-or-replace
                                   site-dirlocals-file)
        (when-let* ((buf (get-file-buffer site-dirlocals-file)))
          (save-buffer buf)
          (kill-buffer buf)))
      (let ((mentions-by-filename
             (seq-group-by
              (-compose (-rpartial #'expand-file-name wm-site-dir)
                        (-partial #'format "data/mentions%smentions.json")
                        #'url-filename
                        #'url-generic-parse-url
                        (-rpartial #'ht-get "wm-target"))
              all-entries)))
        (unless (file-exists-p wm-data-dir)
          (message "Making data-dir: %s" wm-data-dir)
          (make-directory wm-data-dir))
        (pcase-dolist (`(,file . ,value-list) mentions-by-filename)
          (message "Saving file %s" file)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (erase-buffer)
            (json-insert (vconcat value-list))))))))



(provide 'webmentions)
;;; webmentions.el ends here
