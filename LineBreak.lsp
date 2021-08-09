; =={ Line Break }==
; Break a line when crossing

(defun c:lbreak (/ selset) 
  (setq selset (ssget "_+.:E:S" 
                      '((-4 . "<or") (0 . "LINE") (0 . "POLYLINE") (-4 . "or>"))
               )
  )
)