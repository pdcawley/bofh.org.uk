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

(dolist (pkg '(org-mode
	       ox-hugo
	       json-mode
	       (restclient :type git :host github :repo "pashky/restclient.el"
			   :fork (:host github :repo "pdcawley/restclient.el"))
	       
	       (ob-restclient :type git :host github :repo "alf/ob-restclient.el"
			      :fork (:host github :repo "pdcawley/ob-restclient.el"))))
  (straight-use-package pkg))

(provide 'setup-org-export)
