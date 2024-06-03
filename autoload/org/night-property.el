;;; autoload/org/night-property.el -*- lexical-binding: t; -*-
;;;
(cl-defun night/org-property-get
    (&key
     (direction nil)
     (properties nil)
     (return-mode
      :atom
      ;; :alist
      ))
    ;; [[https://emacs.stackexchange.com/questions/81405/org-mode-how-do-i-get-a-property-from-the-children-in-the-current-subtree][org properties - org-mode: How do I get a property from the children in the current subtree? - Emacs Stack Exchange]]
  "Get the value of the specified PROPERTIES of the current heading.
If not found, search in the given DIRECTION (:up or :down) to find the nearest heading with the specified properties.
DIRECTION can be a single direction (:up or :down) or a list of directions to try in order.
PROPERTIES can be a list of property names or a single property name as a string.
RETURN-MODE can be :alist (default) to return an association list or :atom to return a single value if only one property is specified."
  (let ((props (if (listp properties) properties (list properties)))
        (result nil))
    (cl-labels ((get-properties ()
                  (dolist (prop props)
                    (let ((value (org-entry-get nil prop nil)))
                      (when value
                        (setq result (cons (cons prop value) result))))))
                (search-up ()
                  (save-excursion
                    (while (and (> (night/org-current-level) 1)
                                (not result))
                      (org-up-heading-safe)
                      (get-properties))))
                (search-down ()
                  ;; @NotImplemented
                  ;; (save-excursion
                  ;;   (while (and ((not (eobp)))
                  ;;               (not result))
                  ;;     (org-next-visible-heading 1)
                  ;;     (get-properties)))
                  ))
      (get-properties)
      (unless result
        (let ((directions (if (listp direction) direction (list direction))))
          (cl-dolist (dir directions)
            (pcase dir
              (:up (search-up))
              (:down (search-down))
              (_ (error "Invalid direction: %s. Use :up or :down" dir)))
            (when result
              (cl-return)))))
      (setq result (reverse result))
      ;; We should reverse the alist so that the properties asked for first, appear first.

      (if (eq return-mode :atom)
          (cdar result)
        ;; (if
        ;; (= (length result) 1)
        ;;     (cdar result)
        ;;   (error "Return mode :atom requires exactly one property, but got %d" (length result)))
        result))))

(defun night/org-nearest-id-get ()
  (interactive)
  (night/org-property-get :direction '(:up :down) :properties '("ID" "CUSTOM_ID") :return-mode :atom))
;;;
