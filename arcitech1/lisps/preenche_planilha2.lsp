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


(defun verfifica_valores(vvv posicaoAtrr objV)
	
	(setq vvv (vl-string-trim " " (strcase vvv)))
	
	(if (or (= vvv "TAP") (= vvv "ACOPLADOR")(= vvv "EQUALIZADOR"))
		(progn
			(setq valorNovo (vl-string-trim " " (strcase (retorna_attrib objV   (+ posicaoAtrr 1) )  )) )
			(setq procura1 (assoc valorNovo lista_relatorio))
			(if (/= procura1 nil)
				(progn
					(setq comprimentoE  (nth 1 procura1))
					(setq lista_relatorio (subst (list valorNovo (+ comprimentoE 1))(assoc valorNovo lista_relatorio) lista_relatorio))
				)
				(progn
					(setq lista_relatorio (cons (list valorNovo 1 ) lista_relatorio))
				)
			)
		)
	)
	
)

(defun lendo_informacoes()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	
	(verifica_duplicidade "NET_UPGRADE" "INSERT")
	(setq lista_relatorio nil)
	(setq all (ssget "x" '((-4 . "<AND") (2 . "CX_CodUpgrade")(0 . "INSERT")(-4 . "AND>"))))
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
				
				(setq contadorAtritubo 1)
				(setq valor1 (retorna_attrib obj contadorAtritubo))
				
				(while (/= valor1 nil)
					(verfifica_valores valor1 contadorAtritubo obj)
					
					
					(setq valor1 (retorna_attrib obj contadorAtritubo))
					(setq contadorAtritubo (+ contadorAtritubo 1))
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
	(setq elemento (nth contador lista_relatorio))
	(while (/= elemento nil)
		(princ (strcat "\n"  (nth 0 elemento) " => " (rtos (nth 1 elemento) 2 2)))
		(setq contador (+ contador 1))
		(setq elemento (nth contador lista_relatorio))
	)
	
	
)


(defun abre_arq()
	(setq  ARQUIVO_CSV (open (getfiled (strcat "Escolha a Pasta") "" "csv" 1) "w"))
)

(defun fecha()
	(close ARQUIVO_CSV)
)

(defun verifica_se_tap(eee / valor2 valor2Sparser tamanhoArray1 respFunction)
	(setq respFunction nil)
	(setq valor2 (nth 0 eee))
	(setq valor2Sparser (sparser valor2 "/"))
	
	(setq tamanhoArray1 (length valor2Sparser))
	
	(if (= tamanhoArray1 3)
		(progn
			(setq respFunction T)
		)
	)
	
	respFunction
	
)

(defun extrai_tap1()
	;lista_relatorio
	(setq contador 0)
	(setq elem (nth contador lista_relatorio))
	
	(setq lista_ordem1 nil)
	(setq lista_ordem2 nil)
	
	;Monta a ordem de extração
	;Gera duas listas que serão usadas para fazer a extração
	(while (/= elem nil)
		(setq resp1 (verifica_se_tap elem))
		(if resp1
			(progn
				(setq valor2Sparser (sparser (nth 0 elem) "/"))
				(if (=  (member (nth 0 valor2Sparser) lista_ordem1)  nil  )
					(progn
						(setq lista_ordem1 (cons  (nth 0 valor2Sparser)  lista_ordem1))
					)
				)
				(if (=  (member (nth 1 valor2Sparser) lista_ordem2)  nil  )
					(progn
						(setq lista_ordem2 (cons  (nth 1 valor2Sparser)  lista_ordem2))
					)
				)
			)
		)
		(setq contador (+ contador 1))
		(setq elem (nth contador lista_relatorio))
	)
	
	
	;Ordena as listas
	(setq lista_ordem1 (reverse (vl-sort lista_ordem1 '(lambda (x1 x2) (< (atoi x1) (atoi x2))))))
	(setq lista_ordem2 (reverse (vl-sort lista_ordem2 '(lambda (x1 x2) (< (atoi x1) (atoi x2))))))
	
	(setq cont_1 (length lista_ordem1))
	
	
	(while (> cont_1 0)
		(setq procurar_por1 (nth (- cont_1 1) lista_ordem1))
		(setq cont_2 (length lista_ordem2))
		(while (> cont_2 0)
			(setq procurar_por2 (nth (- cont_2 1) lista_ordem2))
			
			(setq contador 0)
			(setq elem (nth contador lista_relatorio))
			(while (/= elem nil)
				(setq resp1 (verifica_se_tap elem))
				(if resp1
					(progn
						(setq valor2Sparser (sparser (nth 0 elem) "/"))
						
						(if (and (= (nth 0 valor2Sparser) procurar_por1 )  (= (nth 1 valor2Sparser) procurar_por2  )  )
							(progn
								(write-line (strcat "Taps - " (nth 0 valor2Sparser) " Vias;RMT10" (nth 0 valor2Sparser) "-" (nth 1 valor2Sparser) ";" (itoa (nth 1 elem)) ) ARQUIVO_CSV)
						
							)
						)
						
						;Taps - 2 Vias;RMT102-17;2/17/1;6
					)
				)
				
				(setq contador (+ contador 1))
				(setq elem (nth contador lista_relatorio))
			)
			
			
			
			(setq cont_2 (- cont_2 1))
		)
		(setq cont_1 (- cont_1 1))
	)
	
	
	
	
)


(defun salvar_arquivo()
	(abre_arq)
	
	
	(write-line "Equipamentos;Part Number;Node" ARQUIVO_CSV)
	(write-line "Amplificadores;FLEX-MAX321e - MODULO;" ARQUIVO_CSV)
	(write-line "Amplificadores;FLEX-MAX321e - MOD + HOUSING;" ARQUIVO_CSV)
	(write-line "Amplificadores;FLEX-MAX601e - MODULO;" ARQUIVO_CSV)
	(write-line "Amplificadores;FLEX-MAX601e - MOD + HOUSING;" ARQUIVO_CSV)
	(write-line "Amplificadores;GS7000;" ARQUIVO_CSV)
	(write-line "Amplificadores;SG4000;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 0WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 1WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 2WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 3WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 4WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 5WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 6WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 7WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 8WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 9WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 10WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 11WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 12WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 13WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 14WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 15WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 16WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 17WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 18WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 19WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 20WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 21WC;" ARQUIVO_CSV)
	(write-line "Forward Pads;10-A- 22WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 0WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 1WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 2WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 3WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 4WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 5WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 6WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 7WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 8WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 9WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 10WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 11WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 12WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 13WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 14WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 15WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 16WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 17WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 18WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 19WC;" ARQUIVO_CSV)
	(write-line "Return Pads;10-A- 20WC;" ARQUIVO_CSV)
	(write-line "Forward Equalizers;P-1G-CS12;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS11;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS10;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS09;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS08;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS07;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS06;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS05;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS04;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS03;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-CS02;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ02;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ03;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ04;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ05;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ06;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ07;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ08;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ09;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ10;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ11;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ12;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ13;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ14;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ15;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ16;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ17;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ18;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ19;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ20;" ARQUIVO_CSV)

	(write-line "Forward Equalizers;P-1G-EQ00;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/0;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/1;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/2;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/3;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/4;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/5;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/6;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/7;" ARQUIVO_CSV)

	(write-line "Return Equalizers;7-REF85/8;" ARQUIVO_CSV)

	;trata os taps
	;lista_relatorio
	;lista_soma_cabos
	
	(extrai_tap1)
	
	
	(setq find1 "2WAY/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;RLS10-2;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;RLS10-2;"   ) ARQUIVO_CSV)
		)
	)
	(setq find1 "3WAY/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;RLS10-3;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;RLS10-3;"   ) ARQUIVO_CSV)
		)
	)
	(setq find1 "DC8/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-8;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-8;"   ) ARQUIVO_CSV)
		)
	)
	(setq find1 "DC12/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-12;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-12;"  ) ARQUIVO_CSV)
		)
	)
	(setq find1 "DC16/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-16;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;RLDC10-16;"   ) ARQUIVO_CSV)
		)
	)
	
	(write-line "Couplers Internos;7-DC-4-5-1000;" ARQUIVO_CSV)

	(write-line "Couplers Internos;7-DC-8-5-1000;" ARQUIVO_CSV)

	(write-line "Couplers Internos;7-DC-12-5-1000;" ARQUIVO_CSV)
	
	(write-line "Fontes;LPI;" ARQUIVO_CSV)

	(write-line "Fontes;90V - 15A;" ARQUIVO_CSV)
	
	(setq find1 "EQ/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "EQ Linha;FFE-8-100N;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "EQ Linha;FFE-8-100N;"  ) ARQUIVO_CSV)
		)
	)
	
	(setq find1 "LANCAR500")
	(setq procura2 (assoc find1 lista_soma_cabos) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Cabos;.500;"  (nth 1 procura2) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Cabos;.500;"  ) ARQUIVO_CSV)
		)
	)
	(setq find1 "LANCAR750")
	(setq procura2 (assoc find1 lista_soma_cabos) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Cabos;.750;"  (nth 1 procura2) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Cabos;.750;"  ) ARQUIVO_CSV)
		)
	)
	
	;CABOS
	;CABOS
	
	(write-line "Conectores;PIN-500;" ARQUIVO_CSV)
	(write-line "Conectores;PIN-750;" ARQUIVO_CSV)
	(write-line "HP;HP;" ARQUIVO_CSV)
	(write-line "KM (STRAND);KM (STRAND);" ARQUIVO_CSV)
	
	;lista_soma_cabos
	
	(fecha)
)


(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")

(defun c:rtt()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(lendo_informacoes)
	(soma_cabos)
	(exibe_relatorio)
	(salvar_arquivo)
	
	(princ)
)