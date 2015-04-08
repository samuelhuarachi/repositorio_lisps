(defun retorna_attrib (BLOCO_AUX NUM_ATTRIB)
  (setq VALOR_ATTRIB nil)
  (setq EL BLOCO_AUX)
  (setq CONTADOR 1)
  (while (<= CONTADOR NUM_ATTRIB)
    (setq EL (entnext EL))
    (setq CONTADOR (1+ CONTADOR))
  )
  (setq VALOR_ATTRIB (cdr (assoc 1 (entget EL))))
  VALOR_ATTRIB
)

(defun percorre_elementos()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	(setq all (ssget "x" (List (cons 2 "CX_CodUpgrade"))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(princ "\rAguarde...")
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				;percorre atributos
				(setq contador1 1)
				(while (/= (retorna_attrib obj contador1) nil)
					(setq valor1 (retorna_attrib obj contador1))
					
					;(if (or (= valor1 "ACOPLADOR") (= valor1 "TAP") )
					;	(progn
					;		(setq valor2 (retorna_attrib obj (+ contador1 1)))
					;		(if (/= valor2 "PIN-F/600")
					;			(progn
					;				(setq novoValor (vl-string-subst  "/1" "/600" valor2))
					;				(corrigir_valores (+ contador1 1) obj novoValor )
					;			)
					;		)
					;	)
					;)
					
					(if (or
						(= (retorna_attrib obj contador1) "EQUALIZADOR")
						(= (retorna_attrib obj contador1) "ACOPLADOR")
						(= (retorna_attrib obj contador1) "TAP") )
						(progn
							;passivo
							;antigo
							(setq valor2 (retorna_attrib obj (+ contador1 1)))
							(if (/= (vl-string-search "/" valor2) nil)
								(progn
									(setq valor2_array (sparser valor2 "/"))
									(setq tam2 (length valor2_array))
									(if (> tam2 1)
										(progn
											(setq ultimoel (nth (- tam2 1) valor2_array))
											(setq tamString (strlen ultimoel)) ;Tamanho da última string  2000 = 4
											(setq tamTotal (strlen valor2)) ;Tamanho do texto total  1/6/2000
											
											(setq stringSemu (substr  valor2 1 (- tamTotal tamString) )) ;Gera a string sem o último elemento 1/6/
											
											
											;(setq novoValor (vl-string-subst  (strcat "/" pantigo ) (strcat "/" ultimoel ) valor2))
											(corrigir_valores (+ contador1 1) obj (strcat stringSemu pantigo) )
										)
									)
									
								)
							)
							
							;passivo
							;novo
							(setq valor2 (retorna_attrib obj (+ contador1 2)))
							(if (/= (vl-string-search "/" valor2) nil)
								(progn
									(setq valor2_array (sparser valor2 "/"))
									(setq tam2 (length valor2_array))
									(if (> tam2 1)
										(progn
											(setq ultimoel (nth (- tam2 1) valor2_array))
											(setq tamString (strlen ultimoel)) ;Tamanho da última string  2000 = 4
											(setq tamTotal (strlen valor2)) ;Tamanho do texto total  1/6/2000
											
											(setq stringSemu (substr  valor2 1 (- tamTotal tamString) )) ;Gera a string sem o último elemento 1/6/
											
											
											;(setq novoValor (vl-string-subst  (strcat "/" pantigo ) (strcat "/" ultimoel ) valor2))
											(corrigir_valores (+ contador1 2) obj (strcat stringSemu pnovo) )
										)
									)
									
								)
							)
							
							
						)
					)
					
					
					
					
					(if (= (retorna_attrib obj contador1) "AMPLIFICADOR") 
						(progn
							;ativo
							;antigo
							(setq valor2 (retorna_attrib obj (+ contador1 1)))
							(if (/= (vl-string-search "/" valor2) nil)
								(progn
									(setq valor2_array (sparser valor2 "/"))
									(setq tam2 (length valor2_array))
									(if (> tam2 1)
										(progn
											(setq ultimoel (nth (- tam2 1) valor2_array))
											(setq tamString (strlen ultimoel)) ;Tamanho da última string  2000 = 4
											(setq tamTotal (strlen valor2)) ;Tamanho do texto total  1/6/2000
											
											(setq stringSemu (substr  valor2 1 (- tamTotal tamString) )) ;Gera a string sem o último elemento 1/6/
											
											
											;(setq novoValor (vl-string-subst  (strcat "/" pantigo ) (strcat "/" ultimoel ) valor2))
											(corrigir_valores (+ contador1 1) obj (strcat stringSemu aantigo) )
										)
									)
									
								)
							)
							
							;ativo
							;novo
							(setq valor2 (retorna_attrib obj (+ contador1 2)))
							(if (/= (vl-string-search "/" valor2) nil)
								(progn
									(setq valor2_array (sparser valor2 "/"))
									(setq tam2 (length valor2_array))
									(if (> tam2 1)
										(progn
											(setq ultimoel (nth (- tam2 1) valor2_array))
											(setq tamString (strlen ultimoel)) ;Tamanho da última string  2000 = 4
											(setq tamTotal (strlen valor2)) ;Tamanho do texto total  1/6/2000
											
											(setq stringSemu (substr  valor2 1 (- tamTotal tamString) )) ;Gera a string sem o último elemento 1/6/
											
											
											;(setq novoValor (vl-string-subst  (strcat "/" pantigo ) (strcat "/" ultimoel ) valor2))
											(corrigir_valores (+ contador1 2) obj (strcat stringSemu anovo) )
										)
									)
									
								)
							)
							
							
							
						)
					)
					
					
					
					(setq contador1 (+ contador1 1))
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun sparser (str delim / ptr lst)
	(while (setq ptr (vl-string-search delim str))
		(setq lst (cons (substr str 1 ptr) lst))
		(setq str (substr str (+ ptr 2)))
	)
	(reverse (cons str lst))
)


(defun corrigir_valores(posicao obj_ valor1)
	
	(if (= posicao 1)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext   obj_))) (entget (entnext  obj_))))
			(entmod trocar)
		)
	)
	(if (= posicao 2)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext  obj_)))) (entget (entnext (entnext obj_)))))
			(entmod trocar)
		)
	)
	(if (= posicao 3)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext obj_))))) (entget (entnext (entnext(entnext obj_))))))
			(entmod trocar)
		)
	)
	(if (= posicao 4)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext obj_)))))) (entget (entnext (entnext(entnext (entnext obj_)))))))
			(entmod trocar)
		)
	)
	(if (= posicao 5)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext  (entnext obj_))))))) (entget (entnext (entnext(entnext (entnext  (entnext obj_))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 6)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext  obj_)))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext  obj_)))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 7)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext   obj_))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext   obj_))))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 8)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext   obj_)))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext   obj_)))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 9)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext  obj_))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext  obj_))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 10)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext  obj_)))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext  obj_)))))))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 11)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext obj_))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext obj_))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 12)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext obj_)))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext obj_)))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 13)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext  obj_))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext  obj_))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 14)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext obj_)))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext obj_)))))))))))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 15)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 16)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext (entnext  obj_)))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext (entnext  obj_)))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 17)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 18)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext obj_)))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext obj_)))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 19)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext (entnext  obj_))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext (entnext  obj_))))))))))))))))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 20)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext   obj_)))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext   obj_)))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 21)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext obj_))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext obj_))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 22)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext  (entnext obj_)))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext  (entnext  obj_)))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 23)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext obj_))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext obj_))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 24)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext obj_)))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext obj_)))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 25)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext (entnext obj_))))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	
	(if (= posicao 26)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext(entnext(entnext obj_)))))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext (entnext(entnext obj_)))))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 27)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext(entnext(entnext(entnext obj_))))))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext (entnext(entnext(entnext obj_))))))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	(if (= posicao 28)
		(progn
			(setq novo (cons 1 valor1))
			(setq trocar (subst novo (assoc 1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext(entnext(entnext(entnext (entnext  obj_)))))))))))))))))))))))))))))) (entget (entnext (entnext(entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext  (entnext  (entnext  (entnext(entnext(entnext(entnext (entnext(entnext(entnext  (entnext obj_)))))))))))))))))))))))))))))))
			(entmod trocar)
		)
	)
	
	
)


(defun string_null(sss)
	(setq sss (vl-string-trim " " sss))
	(if (= sss "")
		(progn
			(setq resp1 nil)
		)
		(progn
			(setq resp1 1)
		)
	)
)

(defun c:corrige_tap_acoplador()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(setq pantigo "")
	(setq pnovo "")
	(setq aantigo "")
	(setq anovo "")
	
	;(while (= pantigo "")
		(setq pantigo (vl-string-trim " " (getstring "\nPassivo - antigo: ")))
	;	(if (= pantigo "")
	;		(progn
	;			(alert "O valo não pode estar vazio!")
	;		)
	;	)
	;)
	
	;(while (= pnovo "")
		(setq pnovo (vl-string-trim " "(getstring "\nPassivo - novo: ")))
	;	(if (= pnovo "")
	;		(progn
	;			(alert "O valo não pode estar vazio!")
	;		)
	;	)
	;)
	
	;(while (= aantigo "")
		(setq aantigo (vl-string-trim " "(getstring "\nAtivo - antigo: ")))
	;	(if (= aantigo "")
	;		(progn
	;			(alert "O valo não pode estar vazio!")
	;		)
	;	)
	;)
	
	;(while (= anovo "")
		(setq anovo (vl-string-trim " "(getstring "\nAtivo - novo: ")))
	;	(if (= anovo "")
	;		(progn
	;			(alert "O valo não pode estar vazio!")
	;		)
	;	)
	;)
	
	(percorre_elementos)
	(princ)
)