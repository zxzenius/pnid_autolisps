; =={ FlattenLayer }==
; Flatten selected objects onto a single layer while preserving their
; visual appearance (color, linetype, lineweight, transparency).
; Objects with ByLayer properties get those values baked in as explicit
; overrides so they look identical on the target layer.

(vl-load-com)
(princ "\n:: FlattenLayer.lsp | Version 1.0 | \\U+00A9 zenius ")
(princ "\n:: \"flatlay\" to start ::")
(princ)

(defun c:flatlay (/ selset target-layer idx obj count)
  (defun *error* (msg)
    (fl:endundo (fl:acdoc))
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq selset (ssget))
  (if (null selset)
    (princ "\nNo objects selected.")
    (progn
      (setq target-layer (fl:get-target-layer))
      (if target-layer
        (progn
          (fl:startundo (fl:acdoc))
          (setq idx 0 count 0)
          (repeat (sslength selset)
            (setq obj (vlax-ename->vla-object (ssname selset idx))
                  idx (1+ idx)
            )
            (if (fl:flatten obj target-layer)
              (setq count (1+ count))
            )
          )
          (fl:endundo (fl:acdoc))
          (princ
            (strcat "\n" (itoa count) " object(s) flattened to layer \""
                    target-layer "\"."
            )
          )
        )
        (princ "\nCancelled.")
      )
    )
  )
  (princ)
)

; Prompt user for the target layer name; defaults to "0".
; Returns layer name string, or nil if cancelled.
(defun fl:get-target-layer (/ input)
  (setq input (getstring "\nTarget layer <0>: "))
  (if (or (null input) (= input ""))
    "0"
    (progn
      (fl:ensure-layer input)
      input
    )
  )
)

; Create layer if it does not already exist.
(defun fl:ensure-layer (name)
  (if (null (tblsearch "LAYER" name))
    (entmakex
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 name)
            '(70 . 0)
            '(62 . 7)
      )
    )
  )
)

; Bake ByLayer properties into explicit overrides, then move to target layer.
; Returns T on success, nil on failure.
(defun fl:flatten (obj target-layer / layer-obj result)
  (setq result
    (vl-catch-all-apply
      (function
        (lambda ()
          (setq layer-obj
            (vla-item (vla-get-layers (fl:acdoc)) (vla-get-layer obj))
          )
          (fl:bake-color obj layer-obj)
          (fl:bake-linetype obj layer-obj)
          (fl:bake-lineweight obj layer-obj)
          (fl:bake-transparency obj layer-obj)
          (vla-put-layer obj target-layer)
        )
      )
    )
  )
  (if (vl-catch-all-error-p result)
    (progn
      (princ
        (strcat "\nSkipped ("
                (vl-catch-all-error-message result) ")"
        )
      )
      nil
    )
    T
  )
)

; If entity color is ByLayer (256), copy the layer's true-color value
; onto the entity.  Handles both ACI and RGB (TrueColor) cases.
(defun fl:bake-color (obj layer-obj)
  (if (= (vla-get-color obj) 256)
    (vla-put-truecolor obj (vla-get-truecolor layer-obj))
  )
)

; If entity linetype is "ByLayer", set it to the layer's linetype.
(defun fl:bake-linetype (obj layer-obj / lt)
  (if (= (strcase (vla-get-linetype obj)) "BYLAYER")
    (progn
      (setq lt (vla-get-linetype layer-obj))
      (vla-put-linetype obj lt)
    )
  )
)

; If entity lineweight is ByLayer (-1), set it to the layer's lineweight.
(defun fl:bake-lineweight (obj layer-obj)
  (if (= (vla-get-lineweight obj) -1)
    (vla-put-lineweight obj (vla-get-lineweight layer-obj))
  )
)

; If entity transparency is "ByLayer", copy the layer's transparency.
; Wrapped in catch-all because EntityTransparency may not exist on all
; entity types or AutoCAD versions prior to 2011.
(defun fl:bake-transparency (obj layer-obj / tp-val lt-val)
  (setq tp-val
    (vl-catch-all-apply 'vla-get-entitytransparency (list obj))
  )
  (if (and (not (vl-catch-all-error-p tp-val))
           (= (strcase tp-val) "BYLAYER")
      )
    (progn
      (setq lt-val
        (vl-catch-all-apply 'vla-get-entitytransparency (list layer-obj))
      )
      (if (not (vl-catch-all-error-p lt-val))
        (vl-catch-all-apply 'vla-put-entitytransparency (list obj lt-val))
      )
    )
  )
)

;;; --- Undo helpers ---

(defun fl:startundo (doc)
  (fl:endundo doc)
  (vla-startundomark doc)
)

(defun fl:endundo (doc)
  (while (= 8 (logand 8 (getvar 'undoctl)))
    (vla-endundomark doc)
  )
)

(defun fl:acdoc nil
  (eval
    (list 'defun 'fl:acdoc 'nil
      (vla-get-activedocument (vlax-get-acad-object))
    )
  )
  (fl:acdoc)
)
