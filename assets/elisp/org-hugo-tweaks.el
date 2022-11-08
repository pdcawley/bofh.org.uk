;;; org-hugo-tweaks.el -*- lexical-binding: t; -*-

(defun +org-hugo-inline-src-block-advice (inline-src-block _contents info)
  (let* ((lang (org-element-property :language inline-src-block))
         (code (org-element-property :value inline-src-block)))
    (format "<code class='inline inline-%s'>%s</code>" lang code)))

(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-inline-src-block :override #'+org-hugo-inline-src-block-advice))

(provide 'org-hugo-tweaks)
