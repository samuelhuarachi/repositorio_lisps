

(defun soma_cabos()
	(verifica_duplicidade "NET_UPGRADE" "INSERT")
	(setq lista_soma_cabos nil)
	(setq all (ssget "X" '((2 . "CX_CodUpgradeCabo"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
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
				
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun conta_tap()
	(verifica_duplicidade "NET-TAP" "INSERT")
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET-TAP")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	
	(setq lista_soma_tap nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (vl-string-trim " " (strcase (cdr (assoc 8 (entget obj))))))
				(setq blockName (vl-string-trim " "  (strcase (strcase (cdr (assoc 2 (entget obj)))) )))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				
				(setq chave1 blockName)
				(setq procura1 (assoc chave1 lista_soma_tap))
				
				(if (/= procura1 nil)
					(progn
						(setq comprimentoE  (nth 1 procura1))
						(setq lista_soma_tap (subst (list chave1 (+ comprimentoE 1))(assoc chave1 lista_soma_tap) lista_soma_tap))
					)
					(progn
						(setq lista_soma_tap (cons (list chave1 1 ) lista_soma_tap))
					)
				)
				
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
)


(defun exibe_relatorio()
	(command "_textscr")
	(princ "\n----------------")
	(princ "\n--------------------------------")
	(princ "\n------------------------------------------------")
	(princ "\n----------------------------------------------------------------")
	(setq contador 0)
	(setq elemento (nth contador lista_soma_cabos))
	(while (/= elemento nil)
		(princ (strcat "\n"  (nth 0 elemento) " => " (nth 1 elemento)))
		(setq contador (+ contador 1))
		(setq elemento (nth contador lista_soma_cabos))
	)
	
	(setq contador 0)
	(setq elemento (nth contador lista_soma_tap))
	(while (/= elemento nil)
		(princ (strcat "\n"  (nth 0 elemento) " => " (rtos (nth 1 elemento) 2 2)))
		(setq contador (+ contador 1))
		(setq elemento (nth contador lista_soma_tap))
	)
	
	
)

(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")

(defun c:preenche_planilha()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(soma_cabos)
	(conta_tap)
	(exibe_relatorio)
	
	(princ)
)