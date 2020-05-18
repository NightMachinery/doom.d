;;; ~/doom.d/night-macos-gui.el -*- lexical-binding: t; -*-

  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . '()))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
