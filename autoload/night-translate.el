;;; ~/doom.d/autoload/night-translate.el -*- lexical-binding: t; -*-

(require 'google-translate)
(require 'google-translate-smooth-ui)
(setq google-translate-translation-directions-alist
      '(("en" . "fa") ("fa" . "en") ("en" . "en") ))
