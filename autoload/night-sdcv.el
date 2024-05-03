;;; autoload/night-sdcv.el -*- lexical-binding: t; -*-
;;;
(require 'sdcv)

(after! (sdcv)
  (setq sdcv-say-word-p nil)
  ;; say word after translation
  ;; We don't have the audio files installed anyways.
  ;; `sdcv-pronounce-word'

  (setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic")) ; setup directory of stardict dictionary

  (setq sdcv-dictionary-simple-list
        ;; setup dictionary list for simple search
        ;; The list is in reverse order. (The last item will be displayed first.)
        ;; =sdcv -l= lists all available dicts on your system
        '(
          ;; "Oxford English Dictionary 2nd Ed. P2"
          ;; "Oxford English Dictionary 2nd Ed. P1"
          "Moin"
          "Longman Pronunciation Dictionary 3rd Ed. (En-En)"
          "Merriam-Webster's Collegiate 11th Ed. (En-En)"
          ))

  (setq sdcv-dictionary-complete-list ; setup dictionary list for complete search
        '(
          "Oxford English Dictionary 2nd Ed. P2"
          "Oxford English Dictionary 2nd Ed. P1"
          "Moin"
          "Longman Pronunciation Dictionary 3rd Ed. (En-En)"
          "Merriam-Webster's Collegiate 11th Ed. (En-En)"
          ))

  (defun night/sdcv-startup ()
    (interactive)
    (night/disable-line-numbers))
  (add-hook 'sdcv-mode-hook #'night/sdcv-startup)

  (defun night/sdcv-search-pointer (&optional word)
    "Get current word.
And display complete translations in other buffer."
    (interactive)
    ;; Display details translate result.
    (sdcv-search-detail (or word (sdcv-region-or-word))))

  (defun night/sdcv-goto-sdcv ()
    "Switch to sdcv buffer in other window."
    (setq sdcv-previous-window-configuration (current-window-configuration))
    (let* ((buffer (sdcv-get-buffer))
           (window (get-buffer-window buffer)))
      ;; (+popup-buffer buffer)
      (pop-to-buffer buffer)
      ;; (display-buffer buffer)
      ;; (if (null window)
      ;;     (switch-to-buffer-other-window buffer)
      ;;   (select-window window))
      ))
  (advice-add 'sdcv-goto-sdcv :override #'night/sdcv-goto-sdcv)

  (comment
   (sdcv-pronounce-word "apple")
   (sdcv-search-detail "apple")
   (sdcv-search-with-dictionary-args
    "apple"
    sdcv-dictionary-complete-list))
;;;
  (map! :map sdcv-mode-map
        :n
        "q" #'+popup/quit-window)
  (map!
   :leader
   "st" #'night/sdcv-search-pointer
   "sT" #'+lookup/dictionary-definition
   )
  )
;;;
