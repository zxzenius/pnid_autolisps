; =={ Inline Block Replace }==
; Replace target blockref with new block

(vl-load-com)
(princ "\n:: InlineBlockReplace.lsp | Version 1.2 | \\U+00A9 zenius ")
(princ "\n:: \"ibr\" to start ::")
(princ)

(defun c:ibr (/ selset bname blockref idx old-name replaced)
  ; Emulate an entsel selection behaviour.
  (setq selset (ssget '((0 . "INSERT"))))
  (if (and selset (> (sslength selset) 0))
    (progn
      (setq bname (ibr:input-blockname))
      (if (tblsearch "BLOCK" bname)
        (progn
          (setq idx 0)
          (setq replaced nil)
          (repeat (sslength selset)
            (setq blockref (vlax-ename->vla-object (ssname selset idx)))
            (setq old-name (ibr:replace blockref bname))
            (if old-name
              (setq replaced (cons old-name replaced))
            )
            (setq idx (1+ idx))
          )
          (ibr:report (reverse replaced) bname)
          (princ "\nFinished")
        )
        (princ (strcat "\nBlock \"" bname "\" not found in this drawing."))
      )
    )
  )
  (princ)
)

(defun ibr:report (old-names bname / uniq)
  (foreach name old-names
    (if (not (member name uniq))
      (setq uniq (cons name uniq))
    )
  )
  (foreach name (reverse uniq)
    (princ
      (strcat "\n" name " -> " bname
              " (x"
              (itoa (length (vl-remove-if-not '(lambda (x) (= x name)) old-names)))
              ")"
      )
    )
  )
)

; Returns the effective name of the replaced block, or nil on failure.
(defun ibr:replace (blockref bname / acadObject acadDocument mSpace new-blockref result old-name)
  (setq old-name (vla-get-effectivename blockref))
  (setq acadObject (vlax-get-Acad-object))
  (setq acadDocument (vla-get-ActiveDocument acadObject))
  (setq mSpace (vla-get-ModelSpace acadDocument))
  (setq new-blockref (vla-insertblock mSpace
                     (vla-get-insertionpoint blockref)
                     bname
                     (vla-get-xscalefactor blockref)
                     (vla-get-yscalefactor blockref)
                     (vla-get-zscalefactor blockref)
                     (vla-get-rotation blockref)
                     )
  )
  (vla-put-layer new-blockref (vla-get-layer blockref))
  (setq result (vl-catch-all-apply 'ibr:copy-attr (list blockref new-blockref)))
  (if (vl-catch-all-error-p result)
    (progn
      ; Roll back so a failed replacement leaves the original block untouched.
      (vla-delete new-blockref)
      (princ (strcat "\nAttribute copy failed: " (vl-catch-all-error-message result)))
      nil
    )
    (progn
      (vla-delete blockref)
      old-name
    )
  )
)

(defun ibr:input-blockname ()
  (getstring "\nBlock name:")
)

(defun ibr:copy-attr (ref1 ref2)
  (foreach attr1 (vlax-invoke ref1 'getattributes)
    (LM:vl-setattributevalue ref2 (vla-get-tagstring attr1) (vla-get-textstring attr1))
  )
)

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)