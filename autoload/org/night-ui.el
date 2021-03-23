;;; autoload/org/night-ui.el -*- lexical-binding: t; -*-

(after! org
  (defun night/modify-org-done-face ()
    (setq org-fontify-done-headline t)
    (set-face-attribute 'org-done nil :strike-through "black")
    (set-face-attribute 'org-headline-done nil
                        :strike-through "black" ; doesn't work for me
                        :foreground "light gray"))
  (night/modify-org-done-face)

  (progn
    (face-spec-set 'org-level-5 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "brightblue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "brightblue"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-6 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "brightcyan"))
          (((class color) (min-colors 16) (background dark)) (:foreground "brightcyan"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-7 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "deepskyblue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "deepskyblue"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-8 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "Purple"))
          (((class color) (min-colors 16) (background dark)) (:foreground "Purple"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (defface org-level-9 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
          (((class color) (min-colors 8)) (:foreground "green"))))
      "Face used for level 9 headlines."
      :group 'org-faces)
    (setq org-level-faces (append org-level-faces (list 'org-level-9)))
    (setq org-n-level-faces (length org-level-faces))))
