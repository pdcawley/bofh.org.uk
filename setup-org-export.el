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
  
(straight-use-package 'org)

(dolist (pkg '(
               ;;               (tomelr :type git :host github "kaushalmodi/tomelr")
               tomelr
               ox-hugo
               json-mode
               ob-restclient
               ;; (restclient :type git :host github :repo "pashky/restclient.el"
               ;;             :fork (:host github :repo "pdcawley/restclient.el"))

               ;; (ob-restclient :type git :host github :repo "alf/ob-restclient.el"
               ;;                :fork (:host github :repo "pdcawley/ob-restclient.el"))
               )
             )
  (message "Installing %S" pkg)
  (straight-use-package pkg))

(provide 'setup-org-export)
