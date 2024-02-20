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
  
  (z:gap-info)
  (setq line1 (z:select-line "\nSelect line to be broken [Gap]"))
  ; Highlight selected line
  ; (redraw line1 3)
  (setq line2 (z:select-line "\nSelect the crossing line: "))
  ; (redraw line1 4)
  (LM:startundo (LM:acdoc))
  (setq lst (LM:intersections (vlax-ename->vla-object line1) (vlax-ename->vla-object line2)  acextendnone))
  (foreach pnt lst
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
  
  (z:gap-info)
  (setq line (z:select-line "\nSelect line [Gap]"))
  ; (redraw line 3)
  (if (setq pnt 
        (vlax-curve-getclosestpointto (vlax-ename->vla-object line) (getpoint "\nSpecify break point: "))
      )
    (progn
      ; (redraw line 4)
      (LM:startundo (LM:acdoc))
      (z:make-gap line pnt lb-gap)
      (LM:endundo (LM:acdoc))
    )
    ; (redraw line 4)
  )
  (princ)
)


(defun z:select-line (msg / sel ename with-opt)
  (if
    (and
      (> (strlen msg) 1)
      (= (substr msg (strlen msg) 1) "]")
    )
    (setq with-opt T)
    (setq with-opt nil)
  )
  (while
    (not
      (progn
        (setvar 'errno 0)
        (if with-opt
          (progn
            (initget "Gap")
            (setq msg (strcat msg ": "))
          )
        )
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
          ((= "Gap" sel)
           (z:lbgap)
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


;; Intersections  -  Lee Mac
;; Returns a list of all points of intersection between two objects.
;; obj1,obj2 - [vla] VLA-Objects with intersectwith method applicable
;; mode      - [int] acextendoption enum of intersectwith method
;; Returns: [lst] List of 3D WCS intersection points, else nil

(defun LM:intersections ( obj1 obj2 mode / lst rtn )
  (setq lst (vlax-invoke obj1 'intersectwith obj2 mode))
  (repeat (/ (length lst) 3)
    (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
          lst (cdddr lst)
    )
  )
  ; *zenius*
  ; The order of the points is from-end-to-start of the obj1,
  ; which is perfect for triming, no need to reverse.
  ; (reverse rtn)
  rtn
)


(defun z:make-gap (ename pnt gap / helper cmd)
  (setq helper 
         (vlax-ename->vla-object 
           (entmakex (list '(0 . "CIRCLE") (cons 10 pnt) (cons 40 gap)))
         )
  )
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command-s
    "_.trim"
    (vlax-vla-object->ename helper) ""
    (list ename pnt)
    ""
  )
  (setvar 'cmdecho cmd)
  (vla-delete helper)
)


;;
;; This program set line break gap.
;;
(defun z:lbgap (/ val)
  (if (setq val (getreal (strcat "\nEnter Line Break Gap [" (rtos lb-gap) "]: ")))
    (progn
      (setq lb-gap val)
      (z:gap-info)
    )
  )
)


(defun z:gap-info ()
  (princ
    (strcat
      "\n:: Current Line Break Gap is <"
      (rtos lb-gap) 
      "> ::"
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


;;; GLOBLE VAR - LINE BREAK GAP
(setq lb-gap 3)
;;;

(vl-load-com)
(princ
  (strcat
    "\n:: LineBreak.lsp | V1.1 | zenius ::"
    "\n:: A line break tool for P&ID :: "
    "\n:: \"CLBreak\" - Crossing Line Break | \"LBreak\" - Single Line Break ::"
  )
)
(princ)
