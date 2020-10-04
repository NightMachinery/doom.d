;;; autoload/org/night-drill.el -*- lexical-binding: t; -*-

(require 'org-drill)
;; (setq org-drill-use-visible-cloze-face-p t)
(setq org-drill-add-random-noise-to-intervals-p t) ;  If you tend to add items in large, infrequent batches, the lack  of variation in interval scheduling can lead to the problem of “lumpiness” –  one day a large batch of items are due for review, the next there is almost  nothing, a few days later another big pile of items is due, and so on.
(setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
