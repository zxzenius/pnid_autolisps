; =={ Inline Block Replace }==
; Replace target blockref with new block

(vl-load-com)
(princ "\n:: InlineBlockReplace.lsp | Version 1.0 | \\U+00A9 zenius ")
(princ "\n:: \"ibr\" to start ::")
(princ)

(defun c:ibr (/ selset bname blockref idx)
  ; Emulate an entsel selection behaviour.
  (setq selset (ssget '((0 . "INSERT"))))
  (if (> (sslength selset) 0)
    (progn
      (setq bname (ibr:input-blockname))
      (setq idx 0)
      (repeat (sslength selset)
        (setq blockref (vlax-ename->vla-object (ssname selset idx)))
        (ibr:replace blockref bname)
        (setq idx (1+ idx))
      )
      (princ "\nFinished")
    )
  )
)

(defun ibr:replace (blockref bname / acadObject acadDocument mSpace new-blockref)
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
  (ibr:copy-attr blockref new-blockref)
  (vla-delete blockref)
)

(defun ibr:input-blockname ()
  (getstring "\nBlock name:")
)

(defun ibr:copy-attr (ref1 ref2 / attrs1)
  (setq attrs1 (vlax-variant-value (vla-getattributes ref1)))
  (foreach attr1 (vlax-safearray->list attrs1)
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