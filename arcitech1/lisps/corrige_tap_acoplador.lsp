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
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				;percorre atributos
				(setq contador1 1)
				(while (/= (retorna_attrib obj contador1) nil)
					(setq valor1 (retorna_attrib obj contador1))
					(if (/= valor1 nil)
						(progn
							;(setq valor1 (strcase (vl-string-trim " " valor1)))
							;(if (= valor1 "PIN-F/600")
							;	(progn
							;		(corrigir_valores contador1 obj "-" )
							;		(if (or (= contador1 3) (= contador1 4) )
							;			(progn
							;				(corrigir_valores 2 obj "ACOPLADOR")
							;			)
							;		)
							;		(if (or (= contador1 6) (= contador1 7) )
							;			(progn
							;				(corrigir_valores 5 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 9) (= contador1 10) )
							;			(progn
							;				(corrigir_valores 8 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 12) (= contador1 13) )
							;			(progn
							;				(corrigir_valores 11 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 15) (= contador1 16) )
							;			(progn
							;				(corrigir_valores 14 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 18) (= contador1 19) )
							;			(progn
							;				(corrigir_valores 17 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 21) (= contador1 22) )
							;			(progn
							;				(corrigir_valores 20 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 24) (= contador1 25) )
							;			(progn
							;				(corrigir_valores 23 obj "ACOPLADOR" )
							;			)
							;		)
							;		(if (or (= contador1 27) (= contador1 28) )
							;			(progn
							;				(corrigir_valores 26 obj "ACOPLADOR" )
							;			)
							;		)
							;	)
							;)
						)
					)
					
					(if (or (= valor1 "ACOPLADOR") (= valor1 "TAP") )
						(progn
							(setq valor2 (retorna_attrib obj (+ contador1 1)))
							(if (/= valor2 "PIN-F/600")
								(progn
									(setq novoValor (vl-string-subst  "/1" "/600" valor2))
									(corrigir_valores (+ contador1 1) obj novoValor )
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

(defun c:corrige_tap_acoplador()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(percorre_elementos)
	
	(princ)
)