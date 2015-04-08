(defun somar_cabos_function()
	(setq lista_soma_cabos nil)
	(setq all (ssget "X" '((2 . "CX_CodUpgradeCabo"))))

	(setq qtdLancar500 0)
	(setq qtdLancar750 0)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq comprimento (retorna_attrib obj 4))
				(setq value2 (strcase (vl-string-trim " " (retorna_attrib obj 2))))
				(setq value3 (strcase (vl-string-trim " " (retorna_attrib obj 3))))
				
				(if (= value2 "LANÇAR")
					(progn
						(setq value2 "LANCAR")
					)
				)
				(setq chave1 (strcat value2 value3))
				
				(setq procura1 (assoc chave1 lista_soma_cabos))
				(if (/= procura1 nil)
					(progn
						(setq comprimentoE (atof (nth 1 procura1)))
						(setq soma1 (+ (atof comprimento) comprimentoE))
						(setq lista_soma_cabos (subst (list (strcat value2 value3) (rtos soma1 2 2))(assoc chave1 lista_soma_cabos) lista_soma_cabos))
					)
					(progn
						(setq lista_soma_cabos (cons (list (strcat value2 value3) comprimento ) lista_soma_cabos))
					)
				)
				
				;Calcular quantos conectores existem
				(if (= chave1 "LANCAR500")
					(progn
						(setq qtdLancar500 (+ qtdLancar500 1))
					)
				)
				(if (= chave1 "LANCAR750")
					(progn
						(setq qtdLancar750 (+ qtdLancar750 1))
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
)


(defun c:somar_cabos()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(somar_cabos_function)
	
	;Exibe resultado 
	
	(setq tam1 (length lista_soma_cabos))
	(setq mensagem "")
	(while (> tam1 0)
		
		(setq mensagem (strcat mensagem (nth 0 (nth (- tam1 1) lista_soma_cabos))  " ==> " (nth 1 (nth (- tam1 1) lista_soma_cabos)) "\n" ))
		
		(setq tam1 (- tam1 1))
	)
	
	(if (= lista_soma_cabos nil)
		(progn
			(alert "Nenhum elemento encontrado!")
		)
		(progn
			(alert  mensagem  )
		)
	)
	
	
	
	
	(princ)
)

