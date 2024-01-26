; =={ Line Break }==
; Break a line when crossing

(defun c:lbreak (/ line1 line2 lst pnt)
  (if (and (setq line1 (z:select-line "\nLine to break:"))
           (setq line2 (z:select-line "\nCross line:"))
      )
    (progn
      (LM:startundo (LM:acdoc))
      (setq lst (LM:intersections (vlax-ename->vla-object line1) (vlax-ename->vla-object line2)  acextendnone))
      (foreach pnt lst
        (z:make-gap line1 pnt 3)
      )
      (LM:endundo (LM:acdoc))
    )
  )
  nil
)


(defun z:select-line2 (msg / lst ename reset)
  (setq reset T)
  (setq lst nil)
  (while reset
    (if (setq lst (entsel msg))
      (progn
        (setq ename (car lst))
        (if (or (z:is-line ename) (z:is-pline ename))
          (progn
            (setq reset nil)
          )
          (progn
            (prompt "\nNot support for current object.")
            (setq reset T)
          )
        )
      )
    )
  )
 lst
)


; Select one line/pline
; msg: hint message
; return: ename
(defun z:select-line (msg / sset)
  (princ msg)
  (if (setq sset (ssget "_+.:E:S" '((0 . "LINE,*POLYLINE"))))
    (ssname sset 0)
    nil
  )
)


(defun z:entype (ename)
  (cdr (assoc 0 (entget ename)))
)


(defun z:is-line (ename)
  (= (z:entype ename) "LINE")
)


(defun z:is-pline (ename / entype)
  (setq entype (z:entype ename))
  (or (= entype "POLYLINE") (= entype "LWPOLYLINE"))
)


(defun LM:intersections ( ob1 ob2 mod / lst rtn )
    (if (and (vlax-method-applicable-p ob1 'intersectwith)
             (vlax-method-applicable-p ob2 'intersectwith)
             (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
        )
        (repeat (/ (length lst) 3)
            (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
                  lst (cdddr lst)
            )
        )
    )
    (reverse rtn)
)


(defun c:inter ( / obj1 obj2 )    
    (if (and (setq obj1 (car (entsel "\nSelect 1st Object: ")))
             (setq obj2 (car (entsel "\nSelect 2nd Object: ")))
        )
        (foreach pnt (LM:intersections (vlax-ename->vla-object obj1) (vlax-ename->vla-object obj2) acextendnone)
            (entmake (list '(0 . "POINT") (cons 10 pnt)))
        )
    )
    (princ)
)


(defun z:make-gap (ename pnt gap / helper int lst)
  (setq helper 
         (vlax-ename->vla-object 
           (entmakex (list '(0 . "CIRCLE") (cons 10 pnt) (cons 40 gap)))
         )
  )
  (princ "\nXXXXXX\n")
  (princ ename)
  (princ "\nxxxxxx\n")
  (princ helper)
  (princ "\nIIIIIII\n")
  (if (setq lst (LM:intersections (vlax-ename->vla-object ename) helper acextendnone))
    (progn
      (foreach int lst
        (command-s
          "_.break" (list ename pnt) "_F"
          "_non" pnt
          "_non" int
        )
      )
    )
  )
  ; (vla-delete helper)
)


(defun c:mgap (/ ename pnt)
  (if (and (setq ename (z:select-line "\nSelect line to break: ")) (setq pnt (getpoint "Center point: ")))
    (progn
      (LM:startundo (LM:acdoc))
      (z:make-gap ename pnt 3)
      (LM:endundo (LM:acdoc))
    )
  )
)


;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)


(vl-load-com) (princ)
; (c:lbreak)