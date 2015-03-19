



(defun percorre_objetos1()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	(setq all2 (ssget "x" (List (cons 8 "erro_digitacao"))))
	(if (/= all2 nil)
		(progn
			(command "erase" all2 "")
		)
	)
	(setq all (ssget "x" (List (cons 2 "CX_CodUpgrade"))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq contador2 1)
				
				(while (/= (retorna_attrib obj contador2) nil)
					
					;equipamentos
					(if (or (= contador2 2) (= contador2 5)  (= contador2 8) (= contador2 11) (= contador2 14) 
					(= contador2 17) (= contador2 20) (= contador2 23)  (= contador2 26)   )
						(progn
							
							(if (and
							(/= (retorna_attrib obj contador2) "EQUALIZADOR") 
							(/= (retorna_attrib obj contador2) "AMPLIFICADOR") 
							(/= (retorna_attrib obj contador2) "ACOPLADOR") 
							(/= (retorna_attrib obj contador2) "BLOQUEADOR AC") 
							(/= (retorna_attrib obj contador2) "FONTE") 
							(/= (retorna_attrib obj contador2) "LPI") 
							(/= (vl-string-trim " " (retorna_attrib obj contador2)) "")
							(/= (retorna_attrib obj contador2) "TAP") )
								(progn
									(command "layer" "m" "erro_digitacao" "c" "red" "" "")
									(command "circle" coord 7)
									(command "circle" coord 8)
								)
							)
						)
					)
					
					;instalados
					(if (or (= contador2 4) (= contador2 7) (= contador2 10)  (= contador2 13)  (= contador2 16)
					(= contador2 19)  (= contador2 22)  (= contador2 25)  (= contador2 28)  )
						(progn
							(if (and
									(/= (vl-string-search "600" (strcase (retorna_attrib obj contador2))) nil) ;600 marcar erro, ou /1
								)
								(progn
									(command "layer" "m" "erro_digitacao" "c" "red" "" "")
									(command "circle" coord 7)
									(command "circle" coord 8)
								)
							)
						)
					)
					
					(setq contador2 (+ contador2 1))
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun percorre_objetos2()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	(setq all (ssget "x" (List (cons 2 "CX_CodUpgradeCabo"))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				(setq rettt (vl-string-trim " " (strcase (retorna_attrib obj 2) ) ))
				(if (and (/= rettt "RETIRAR")  (/= rettt "LANCAR") )
					(progn
						
						(setq lista (vl-string->list rettt))
						(setq lista (vl-remove (ascii "Ç") lista))
						
						(if (= (length lista) 6)
							(progn
								(if  (and  (=  (nth 0 lista) 76 )   (=  (nth 3 lista) 199 ) (=  (nth 1 lista) 65 ) ) 
									(progn
										(setq novo (cons 1 "Lancar"))
										(setq trocar (subst novo (assoc 1 (entget (entnext (entnext  obj)))) (entget (entnext (entnext  obj)))))
										(entmod trocar)
									)
									(progn
										(command "layer" "m" "erro_digitacao" "c" "red" "" "")
										(command "circle" coord 7)
										(command "circle" coord 8)
									)
								)
							)
							(progn
								(command "layer" "m" "erro_digitacao" "c" "red" "" "")
								(command "circle" coord 7)
								(command "circle" coord 8)
							)
						)
						
					
						
					)
				)
				(setq qtd (- qtd 1))
			)
		)
	)
)


(defun c:verifica_erro_digitacao()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(percorre_objetos1)
	(percorre_objetos2)
	
	
	(princ)
)