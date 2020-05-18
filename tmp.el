(map :leader
     :desc "Jump to char" "/ c" #'avy-goto-char)
(comment   (night/set-leader-keys "z u" #'(lambda (&optional frame)
                                            (interactive)
                                            (message "hi 8")
                                            )))
