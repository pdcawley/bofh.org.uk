;;; hugo-support.el --- Code to support building hugo from org -*- lexical-binding: t; -*-

;; Author: Piers Cawley <piers@singingtogether.co.uk>

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
  
;; (straight-use-package 'org)

(dolist (pkg '(
               ;; Org and ox-hugo support
               tomelr
               ox-hugo
               json-mode
               (restclient :type git :host github
                           :repo "pashky/restclient.el"
                           :fork (:host github :repo "pdcawley/restclient.el"))
               ob-restclient
               (ob-yaml :type git :host github :repo "llhotka/ob-yaml")
               ob-http
               ob-raku
               ;; Required for webmentions.el to work
               request
               dash
               ht
               org-transclusion
               ))
  (straight-use-package pkg))

(with-eval-after-load 'org
  (setopt org-babel-results-keyword "results"
          org-babel-default-header-args
          '((:session . "none")
            (:results . "drawer replace")
            (:comments . "both")
            (:exports . "code")
            (:cache . "no")
            (:eval . "never-export")
            (:hlines . "no")
            (:tangle . "no")
            (:noweb . "yes")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((css . t)
     (dot . t)
     (emacs-lisp . t)
     (http . t)
     (org . t)
     (perl . t)
     (haskell . t)
     (shell . t)
     (sql . t)
     (raku . t)
     (yaml . t)
     (shell . t)
     (restclient . t))))

(add-to-list 'load-path (expand-file-name "./support-code/"))

(with-eval-after-load 'ox-hugo
  (defun +org-hugo-set-shortcode-props (code &rest props)
    (setf (alist-get code org-hugo-special-block-type-properties)
          props))
  (+org-hugo-set-shortcode-props "newthought" :trim-pre nil :trim-post t)
  (+org-hugo-set-shortcode-props "marginnote" :trim-pre t :trim-post t))

(require 'webmentions)

(defun script/export-to-hugo ()
  (message "Exporting current buffer to hugo")
  (setq org-confirm-babel-evaluate nil)
  (org-transclusion-add-all)
  (org-hugo-export-wim-to-md t))

(provide 'setup-org-export)
;;; setup-org-export.el ends here
