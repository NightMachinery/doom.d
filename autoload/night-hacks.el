(when (display-graphic-p)               ; https://emacs.stackexchange.com/questions/61013/how-do-i-make-emacs-not-complain-about-not-being-able-to-start-a-new-server
  (setq warning-minimum-level :emergency))
