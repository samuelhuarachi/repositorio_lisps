



(defun percorre_cxcabo()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 2 "CX_CodUpgradeCabo"))))
	(setq all (ssget "x" '((-4 . "<AND") (2 . "CX_CodUpgradeCabo,CX_CodUpgrade")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(setq contador_num 1 )
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq novo (cons 1 (itoa contador_num)))
				(setq trocar (subst novo (assoc 1 (entget (entnext obj))) (entget (entnext obj))))
				(entmod trocar)
				
				
				(setq contador_num (+ contador_num 1))
				(setq qtd (- qtd 1))
			)
		)
	)
)



(defun c:numera_cx()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(percorre_cxcabo)
	(command "_.REGEN")
	

)