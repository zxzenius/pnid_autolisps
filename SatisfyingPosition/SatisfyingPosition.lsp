; =={ SatisfyingPosition }==
; Reposition enitities to rounded coordinates
(defun c:satisfyingposition () (c:satpos))

(defun c:satpos (/ selset idx ename obj kind)
  (setq selset (ssget)
  )
  (if selset
    (progn
      (setq idx 0)
      (repeat (sslength selset)
        (setq ename (ssname selset idx)
              kind (cdr (assoc 0 (entget ename)))
              obj   (vlax-ename->vla-object ename)
              idx   (1+ idx)
        )
        (cond ((= kind "INSERT")
               (vla-put-insertionpoint obj (round-coords (vla-get-insertionpoint obj)))
              )
          
              ((= kind "LINE")
               (vla-put-startpoint obj (round-coords (vla-get-startpoint obj)))
               (vla-put-endpoint obj (round-coords (vla-get-endpoint obj)))
              )
          
              ((= kind "LWPOLYLINE")
               (vla-put-coordinates obj (round-coords (vla-get-coordinates obj)))
              )
          
              ((or (= kind "CIRCLE") (= kind "ELLIPSE") (= kind "ARC"))
               (vla-put-center obj (round-coords (vla-get-center obj)))
              )
          
              (t nil)
        )
      )
    )
  )
)

; Round coordinates of 2D/3D point
; point <Variant>
; return <Variant>
(defun round-coords (point / pt cood i)
  (setq pt   (vlax-variant-value point)
        i 0)
  (foreach cood (vlax-safearray->list pt)
    (vlax-safearray-put-element pt i (LM:round cood))
    (setq i (1+ i))
  )
  (vlax-make-variant pt)
)

(defun variant->list (var)
  (vlax-safearray->list (vlax-variant-value var))
)

(defun LM:round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
)
(vl-load-com)
(print ":: SatisfyingPosition.lsp | zenius")
(print ":: \"satpos\" to start ::")
