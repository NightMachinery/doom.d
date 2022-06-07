;;; night-info.el ---                                -*- lexical-binding: t; -*-
;;;
(map! :map Info-mode-map
      :nvo "n" #'Info-search-next
      :nvo "S" #'Info-search-backward   ;; There is no =Info-search-previous=, seemingly. This command searches backward (prompts for a query).
      )
;;;
(provide 'night-info)
;;; night-info.el ends here
