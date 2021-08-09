; =={ Batch Print }==
; A lisp version of batch print
(defun c:bprint (/ selset idx borders border border-heights borders-for-sort 
                 minheight border-name
                ) 
  ; border mode default(Block.Xindi) or custom
  (setq g:custom-mode (LM:choose-mode))
  (setq border-name (if g:custom-mode (LM:input-bordername) "Border.*"))
  (setq selset (ssget (list (cons 2 border-name) (cons 0 "INSERT"))))
  (if selset 
    (progn 
      (setq idx 0)
      (repeat (sslength selset) 
        (setq border         (bp:get-border (ssname selset idx))
              borders        (cons border borders)
              border-heights (cons 
                               (bp:delta-y 
                                 (bp:get-minpoint border)
                                 (bp:get-maxpoint border)
                               )
                               border-heights
                             )
              idx            (1+ idx)
        )
      )

      ; Sorting not required if only one drawing selected
      (if (> idx 1) 
        (progn 
          (setq minheight (apply 'min border-heights))
          (setq borders-for-sort (bp:get-borders-for-sort minheight borders))
          ; sort borders according to min point(insertion point)
          (setq borders (bp:sort-borders borders-for-sort))
        )
      )

      ; (princ "\n")
      ; (princ borders)
      (bp:plot-borders borders)
    )
  )
  (princ)
)

(defun bp:get-border (ename / blockref name minpoint maxpoint) 
  (setq blockref (vlax-ename->vla-object ename)
        name     (vlax-get-property blockref 'EffectiveName)
  )
  (vla-getboundingbox blockref 'minpoint 'maxpoint)
  (setq minpoint (bp:pt->2d (vlax-safearray->list minpoint)))
  (setq maxpoint (bp:pt->2d (vlax-safearray->list maxpoint)))
  (list (cons "name" name) 
        (cons "minpoint" minpoint)
        (cons "maxpoint" maxpoint)
  )
)

(defun bp:get-borders-for-sort (minheight borders / border row-record newborder 
                                borders-for-sort
                               ) 
  (foreach border borders 
    (setq row-record (cons "row" (bp:calc-row minheight border)))
    (setq newborder (cons row-record border))
    (setq borders-for-sort (cons newborder borders-for-sort))
  )
)

(defun bp:plot-borders (borders / border) 
  (princ "\nStart printing...")
  (foreach border borders 
    (bp:plot-border border)
  )
  (princ "\nFinished")
)

(defun bp:plot-border (border / acadObject acadDocument activeLayout plot p1 p2) 
  (setq acadObject (vlax-get-Acad-object))
  (setq acadDocument (vla-get-ActiveDocument acadObject))
  (setq activeLayout (vla-Get-ActiveLayout acadDocument))
  (setq plot (vla-get-plot acadDocument))
  (setq p1 (bp:make-pt (bp:get-minpoint border)))
  (setq p2 (bp:make-pt (bp:get-maxpoint border)))
  (vla-setwindowtoplot activeLayout p1 p2)
  (vla-displayplotpreview plot acFullPreview)
  ; (vla-plottodevice plot)
)

(defun bp:config-paper-size (border) 
  (setq name (cdr (assoc "name" border)))
  (cond 
    ((= name "Border.A1"))
    ((= name "Border.A2"))
    ((= name "Border.A2R"))
    ((= name "Border.A3"))
    ((= name "Border.A4"))
  )
  ; vla-put-canonicalmedianame
)

(defun bp:pt->2d (point) 
  (list (car point) (cadr point))
)

(defun bp:delta-y (point1 point2) 
  (- (cadr point2) (cadr point1))
)

(defun bp:calc-row (minheight border / y) 
  (setq y (bp:get-y border))
  (- 0 (LM:round (/ y minheight)))
)

(defun bp:sort-borders (borders-for-sort) 
  (vl-sort borders-for-sort 
           'bp:compare
  )
)

(defun bp:get-row (border) 
  (cdr (assoc "row" border))
)

(defun bp:get-x (border) 
  (nth 1 (assoc "minpoint" border))
)

(defun bp:get-y (border) 
  (nth 2 (assoc "minpoint" border))
)

(defun bp:get-minpoint (border) 
  (cdr (assoc "minpoint" border))
)

(defun bp:get-maxpoint (border) 
  (cdr (assoc "maxpoint" border))
)


(defun bp:compare (b1 b2 / row1 row2 x1 x2) 
  (setq row1 (bp:get-row b1)
        row2 (bp:get-row b2)
        x1   (bp:get-x b1)
        x2   (bp:get-x b2)
  )
  (or (< row1 row2) (and (= row1 row2) (< x1 x2)))
)

(defun bp:make-pt (point / pt pt-sa) 
  (setq pt (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (setq pt-sa (vlax-safearray-fill pt point))
)

;; Round  -  Lee Mac
;; Rounds 'n' to the nearest integer
(defun LM:round (n) 
  (fix (+ n (if (minusp n) -0.5 0.5)))
)

(defun LM:choose-mode (/ mode) 
  (initget "Default Custom")
  (setq mode (cond 
               ((getkword "\nBorder Mode [Default/Custom] <Default>:"))
               ("Default")
             )
  )
  (if (= mode "Custom") "Custom" nil)
)

(defun LM:input-bordername () 
  (getstring "\nBorder name for matching:")
)

(vl-load-com)
(princ "\n:: BatchPrint.lsp | Version 1.0 | \\U+00A9 zenius ")
(princ "\n:: \"bprint\" to start ::")
(princ)