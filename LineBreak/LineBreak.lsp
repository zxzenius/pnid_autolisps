;;--={ Crossing Line Break }=--
;;
;; This program breaks one line of two crossing lines at intersection
;; points with gap.
;;

(defun c:clbreak (/ line1 line2 lst pnt)
  (defun *error* ( msg )
    (LM:endundo (LM:acdoc))
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  (setq line1 (z:select-line "\nSelect Line to Break:"))
  ; Highlight selected line
  (redraw line1 3)
  (setq line2 (z:select-line "\nSelect Crossing Line:"))
  (redraw line1 4)
  (LM:startundo (LM:acdoc))
  (setq lst (LM:intersections (vlax-ename->vla-object line1) (vlax-ename->vla-object line2)  acextendnone))
  (foreach pnt (reverse lst)
    (z:make-gap line1 pnt lb-gap)
  )
  (LM:endundo (LM:acdoc))
  (princ)
)


;;--={ Line Break }=--
;;
;; This program breaks a line at specifed point with gap.
;;
(defun c:lbreak (/ line pnt)
  (defun *error* ( msg )
    (LM:endundo (LM:acdoc))
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  (setq line (z:select-line "\nSelect Line to Break: "))
  (redraw line 3)
  (if (setq pnt (getpoint "Select Break(Center) Point: "))
    (progn
      (redraw line 4)
      (LM:startundo (LM:acdoc))
      (z:make-gap line pnt lb-gap)
      (LM:endundo (LM:acdoc))
    )
    (redraw line 4)
  )
)


;;--={ Line Break Gap }=--
;;
;; This program set default line break gap.
;;
(defun c:lbgap (/ default-gap val)
  (setq default-gap lb-gap)
  (if (setq val (getreal (strcat "\nEnter Line Break Gap [" (rtos lb-gap) "]:")))
    (setq lb-gap val)
    (setq lb-gap default-gap)
  )
)

(defun z:select-line (msg / sel ename)
  (while
    (not
      (progn
        (setvar 'errno 0)
        (setq sel
          (entsel msg)
        )
        (cond
          ((= 7 (getvar 'errno))
           (prompt "\nMissed, try again.")
          )
          ((null sel)
           nil
          )
          ((setq ename (car sel))
           (if (not (or (z:is-line ename) (z:is-pline ename)))
             (prompt "\nObject must be a Line/PLine.")
             T
           )
          )
        )
      )
    )
  )
  ename
)


; Select one line/pline
; msg: hint message
; return: ename
(defun z:select-line2 (msg / sset)
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


(defun z:make-gap (ename pnt gap / helper int lst cmd)
  (setq helper 
         (vlax-ename->vla-object 
           (entmakex (list '(0 . "CIRCLE") (cons 10 pnt) (cons 40 gap)))
         )
  )
  (if (setq lst (LM:intersections (vlax-ename->vla-object ename) helper acextendnone))
    (progn
      (setq cmd (getvar 'cmdecho))
      (setvar 'cmdecho 0)
      (foreach int lst
        (command-s
          "_.break" (list ename pnt) "_F"
          "_non" pnt
          "_non" int
        )
      )
      (setvar 'cmdecho cmd)
    )
  )
  (vla-delete helper)
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


(vl-load-com)
(setq lb-gap 3)
(princ
  (strcat
    "\n:: LineBreak.lsp | V1.0 | zenius "
    "\n:: \"CLBreak\" - Crossing Line Break | \"LBreak\" - One Line Break ::"
    "\n:: \"LBGap\" - Setting Line Break Gap ::"
    "\n:: Current Line Break Gap: " (rtos lb-gap)
  )
)
(princ)
