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
				(if (=  (member (nth 0 valor2Sparser) lista_ordem1)  nil)
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

(defun extrair_tap01(valor1 valor2)
	
	(setq contador002 0)
	(setq achouFlag 0)
	
	(while (/= (nth contador002 lista_relatorio) nil)
		(setq elem (nth contador002 lista_relatorio))
		(setq resp1 (verifica_se_tap elem))
		(if resp1
			(progn
				
				(setq valor2Sparser (sparser (nth 0 elem) "/"))
				
				(if (and   (= achouFlag 0) (= (nth 0 valor2Sparser) valor1)   (= (nth 1 valor2Sparser) valor2)  )
					(progn
						
						(write-line (strcat "Taps - " valor1 " Vias;SAT" valor1 "G-" valor2 ";" (itoa (nth 1 elem))) ARQUIVO_CSV)
						(setq achouFlag 1)
					)
				)
			)
		)
		
		(setq contador002 (+ contador002 1))
	)
	
	(if  (= achouFlag 0)
		(progn
			(write-line (strcat "Taps - " valor1 " Vias;SAT" valor1 "G-" valor2 ";")  ARQUIVO_CSV  )
		)
	)
	
)

(defun salvar_arquivo()
	(abre_arq)
	
	(write-line "sep=;" ARQUIVO_CSV)
	(write-line "Equipamentos;Part Number;Qtde" ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GNMKR LE THERM;" (itoa GNMKR_LE_THERM)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GNMKR LE THERM - HOUSING;" (itoa GNMKR_LE_THERM_HOUSING)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GM-HGD-1GHz;" (itoa GM_HGD_1GHz)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GM-HGD-1GHz - HOUSING;" (itoa GM_HGD_1GHz_HOUSING)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GM-HGBT-1GHz;" (itoa GM_HGBT_1GHz)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GM-HGBT-1GHz - HOUSING;" (itoa GM_HGBT_1GHz_HOUSING)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;GS7000;" (itoa GS7000)) ARQUIVO_CSV)
	(write-line (strcat "Amplificadores;SG4000;" (itoa SG4000)) ARQUIVO_CSV)
	
	(setq contador01 0)
	(while (<= contador01 20)
		(setq ppp (assoc (itoa contador01) lista_fwpad))
		(if (/= ppp nil)
			(progn
				(setq qtdd (nth 1 ppp))
				(write-line (strcat "Forward Pads;FP -" (itoa contador01) ";" (itoa qtdd)) ARQUIVO_CSV)
				;(setq lista_fwpad (vl-remove (assoc (itoa contador01) lista_fwpad) lista_fwpad) )
			)
			(progn
				(write-line (strcat "Forward Pads;FP -" (itoa contador01) ";") ARQUIVO_CSV)
			)
		)
		(setq contador01 (+ contador01 1))
	)
	
	
	(setq contador01 0)
	(while (<= contador01 20)
		(setq ppp (assoc (itoa contador01) lista_rpad))
		(if (/= ppp nil)
			(progn
				(setq qtdd (nth 1 ppp))
				(write-line (strcat "Return Pads;RP - " (itoa contador01) ";" (itoa qtdd)) ARQUIVO_CSV)
				(setq lista_rpad (vl-remove (assoc (itoa contador01) lista_rpad) lista_rpad) )
			)
			(progn
				(write-line (strcat "Return Pads;RP - " (itoa contador01) ";") ARQUIVO_CSV)
			)
		)
		(setq contador01 (+ contador01 1))
	)
	
	;feq
	(write-line (strcat "Forward Equalizers;FEQ-C162;" (itoa FEQ_C162)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C146;" (itoa FEQ_C146)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C130;" (itoa FEQ_C130)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C114;" (itoa FEQ_C114)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C9.8;" (itoa FEQ_C98)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C8.1;" (itoa FEQ_C81)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C6.5;" (itoa FEQ_C65)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C4.9;" (itoa FEQ_C49)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C3.3;" (itoa FEQ_C33)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-C1.6;" (itoa FEQ_C16)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-0;" (itoa FEQ_0)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-1.5;" (itoa FEQ_15)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-3.0;" (itoa FEQ_30)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-4.5;" (itoa FEQ_45)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-6.0;" (itoa FEQ_60)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-7.5;" (itoa FEQ_75)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FE-9.0;" (itoa FE_90)) ARQUIVO_CSV)
	(write-line (strcat "Forward Equalizers;FEQ-10.5;" (itoa FEQ_105)) ARQUIVO_CSV)
	
	
	
	(setq contador01 0)
	(while (<= contador01 12)
		(setq ppp (assoc (itoa contador01) lista_req))
		(if (/= ppp nil)
			(progn
				(setq qtdd (nth 1 ppp))
				(write-line (strcat "Return Equalizers;REQ-" (itoa contador01) ";" (itoa qtdd)) ARQUIVO_CSV)
				(setq lista_req (vl-remove (assoc (itoa contador01) lista_req) lista_req) )
			)
			(progn
				(write-line (strcat "Return Equalizers;REQ-" (itoa contador01) ";") ARQUIVO_CSV)
			)
		)
		(setq contador01 (+ contador01 1))
	)
	
	
	;trata os taps
	;lista_relatorio
	;lista_soma_cabos
	
	;(extrai_tap1)
	;lista_relatorio
	
	(extrair_tap01 "2" "4")
	(extrair_tap01 "2" "8")
	(extrair_tap01 "2" "11")
	(extrair_tap01 "2" "14")
	(extrair_tap01 "2" "17")
	(extrair_tap01 "2" "20")
	(extrair_tap01 "2" "23")
	(extrair_tap01 "4" "8")
	(extrair_tap01 "4" "11")
	(extrair_tap01 "4" "14")
	(extrair_tap01 "4" "17")
	(extrair_tap01 "4" "20")
	(extrair_tap01 "4" "23")
	(extrair_tap01 "8" "11")
	(extrair_tap01 "8" "14")
	(extrair_tap01 "8" "17")
	(extrair_tap01 "8" "20")
	(extrair_tap01 "8" "23")
	
	
	;(write-line (strcat "Couplers Externos;SAS2G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Externos;SAS3UG;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Externos;SADC8G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Externos;SADC12G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Internos;GM-Plugin-2Way;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Internos;GM-Plugin-DC8;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;(write-line (strcat "Couplers Internos;GM-Plugin-DC12;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	
	
	(setq find1 "2WAY/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;SAS2G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;SAS2G;"  ) ARQUIVO_CSV)
		)
	)
	
	(setq find1 "3WAY/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;SAS3UG;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;SAS3UG;"  ) ARQUIVO_CSV)
		)
	)
	
	
	
	(setq find1 "DC8/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;SADC8G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;SADC8G;") ARQUIVO_CSV)
		)
	)
	
	
	(setq find1 "DC12/1")
	(setq procura2 (assoc find1 lista_relatorio) )
	(if (/= procura2 nil)
		(progn
			(write-line (strcat "Couplers Externos;SADC12G;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
		)
		(progn
			(write-line (strcat "Couplers Externos;SADC12G;" ) ARQUIVO_CSV)
		)
	)
	
	
	;(setq find1 "DC16/1")
	;(setq procura2 (assoc find1 lista_relatorio) )
	;(if (/= procura2 nil)
	;	(progn
	;		(write-line (strcat "Couplers Externos;RLDC10-16;" (itoa (nth 1 procura2)) ) ARQUIVO_CSV)
	;	)
	;	(progn
	;		(write-line (strcat "Couplers Externos;RLDC10-16;"   ) ARQUIVO_CSV)
	;	)
	;)
	
	(write-line (strcat "Couplers Internos;GM-Plugin-2Way;"  ) ARQUIVO_CSV)
	(write-line (strcat "Couplers Internos;GM-Plugin-DC8;"  ) ARQUIVO_CSV)
	(write-line (strcat "Couplers Internos;GM-Plugin-DC12;"  ) ARQUIVO_CSV)
	
	;(write-line "Couplers Internos;7-DC-4-5-1000;" ARQUIVO_CSV)
	;(write-line "Couplers Internos;7-DC-8-5-1000;" ARQUIVO_CSV)
	;(write-line "Couplers Internos;7-DC-12-5-1000;" ARQUIVO_CSV)
	
	
	
	
	
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



(defun sparser (str delim / ptr lst)
	(while (setq ptr (vl-string-search delim str))
		(setq lst (cons (substr str 1 ptr) lst))
		(setq str (substr str (+ ptr 2)))
	)
	(reverse (cons str lst))
)

(defun carrega_planilha_amplificadores()
	
	(setq ARQUIVO_CSV (open (getfiled "Selecione a tabela dos amplificadores" "C:\\" "csv" 0)  "r")
 ;(setq ARQUIVO_CSV (open "//ipanema/lisp/NET/TIPO_RUAS.csv" "r"))
;(setq ARQUIVO_CSV (getfiled "//ipanema/lisp/NET/TIPO_RUAS.csv" 0))
	;(setq ARQUIVO_CSV (open "C:\\Bandeirante_Gecad\\Lista_IP.csv" "r")
         LINHA_CSV (read-line ARQUIVO_CSV)
         LISTA_LINHA nil
         LISTA_CSV nil
   )
   (setq lista_fwpad nil)
   (setq lista_rpad nil)
   (setq lista_req nil)
   
   (setq GNMKR_LE_THERM 0)
   (setq GNMKR_LE_THERM_HOUSING 0)
   (setq GM_HGD_1GHz 0)
   (setq GM_HGD_1GHz_HOUSING 0)
   (setq GM_HGBT_1GHz 0)
   (setq GM_HGBT_1GHz_HOUSING 0)
   (setq GS7000 0)
   (setq SG4000 0)
   (setq FEQ_C162 0)
   (setq FEQ_C146 0)
   (setq FEQ_C130 0)
   (setq FEQ_C114 0)
   (setq FEQ_C98 0)
   (setq FEQ_C81 0)
   (setq FEQ_C65 0)
   (setq FEQ_105 0)
   (setq FE_90 0)
   (setq FEQ_75 0)
   (setq FEQ_60 0)
   (setq FEQ_45 0)
   (setq FEQ_30 0)
   (setq FEQ_15 0)
   (setq FEQ_0 0)
   (setq FEQ_C16 0)
   (setq FEQ_C33 0)
   (setq FEQ_C49 0)
   (setq FEQ_C65 0)
   (setq FEQ_C81 0)
   
   (while (/= LINHA_CSV nil)
		(setq LISTA_LINHA (sparser LINHA_CSV ";"))
		(if (> (length LISTA_LINHA) 4)
			(progn
			
				(setq modelo (nth 2 LISTA_LINHA))
				(if (/= modelo "")
					(progn
						(if (= modelo "GNMKR_LE_THERM")
							(progn
								(setq GNMKR_LE_THERM (+ GNMKR_LE_THERM 1))
							)
						)
						(if (= modelo "GNMKR LE THERM - HOUSING")
							(progn
								(setq GNMKR_LE_THERM_HOUSING (+ GNMKR_LE_THERM_HOUSING 1))
							)
						)
						(if (= modelo "GM-HGD-1GHz")
							(progn
								(setq GM_HGD_1GHz (+ GM_HGD_1GHz 1))
							)
						)
						(if (= modelo "GM-HGD-1GHz - HOUSING")
							(progn
								(setq GM_HGD_1GHz_HOUSING (+ GM_HGD_1GHz_HOUSING 1))
							)
						)
						(if (= modelo "GM-HGBT-1GHz")
							(progn
								(setq GM_HGBT_1GHz (+ GM_HGBT_1GHz 1))
							)
						)
						(if (= modelo "GM-HGBT-1GHz - HOUSING")
							(progn
								(setq GM_HGBT_1GHz_HOUSING (+ GM_HGBT_1GHz_HOUSING 1))
							)
						)
						(if (= modelo "GS7000")
							(progn
								(setq GS7000 (+ GS7000 1))
							)
						)
						(if (= modelo "SG4000")
							(progn
								(setq SG4000 (+ SG4000 1))
							)
						)
					)
				)
				
				
				(setq fwpad (nth 5 LISTA_LINHA))
				(if (/= fwpad "")
					(progn
						(setq valor1 (vl-string-search "FP" (vl-string-trim " " fwpad)))
						(if (/= valor1 nil)
							(progn
								(setq ress (vl-string-trim " "(vl-string-subst "" "-"  (vl-string-trim " " (vl-string-subst "" "FP" (vl-string-trim " " fwpad))))))
								(if (/= ress "")
									(progn
										(setq ppp (assoc ress lista_fwpad))
										(if (= ppp nil)
											(progn
												(setq lista_fwpad (cons (list ress 1) lista_fwpad))
											)
											(progn
												
												(setq lista_fwpad (subst (list ress (+ (nth 1 ppp) 1) ) (assoc ress lista_fwpad) lista_fwpad))
											)
										)
										
										
									)
								)
								
							)
						)
						
					)
				)
				
				(setq fweq (nth 6 LISTA_LINHA))
				(if (/= fweq "")
					(progn
						;FEQ-C162
						(setq lista (vl-string->list fweq))
						(setq lista (vl-remove (ascii " ") lista))
						(setq fweq (vl-list->string lista))
						(setq fweq (vl-string-trim " " fweq))
						(setq fweq (strcase fweq))
						(if (/= fweq "FWEQ")
							(progn
								(if (= fweq "FEQ-10.5")
									(progn
										(setq FEQ_105 (+ FEQ_105 1))
									)
								)
								(if (= fweq "FE-9.0")
									(progn
										(setq FE_90 (+ FE_90 1))
									)
								)
								(if (= fweq "FEQ-7.5")
									(progn
										(setq FEQ_75 (+ FEQ_75 1))
									)
								)
								(if (= fweq "FEQ-6.0")
									(progn
										(setq FEQ_60 (+ FEQ_60 1))
									)
								)
								(if (= fweq "FEQ-4.5")
									(progn
										(setq FEQ_45 (+ FEQ_45 1))
									)
								)
								(if (= fweq "FEQ-3.0")
									(progn
										(setq FEQ_30 (+ FEQ_30 1))
									)
								)
								(if (= fweq "FEQ-1.5")
									(progn
										(setq FEQ_15 (+ FEQ_15 1))
									)
								)
								(if (= fweq "FEQ-0")
									(progn
										(setq FEQ_0 (+ FEQ_0 1))
									)
								)
								(if (= fweq "FEQ-C1.6")
									(progn
										(setq FEQ_C16 (+ FEQ_C16 1))
									)
								)
								(if (= fweq "FEQ-C3.3")
									(progn
										(setq FEQ_C33 (+ FEQ_C33 1))
									)
								)
								(if (= fweq "FEQ-C4.9")
									(progn
										(setq FEQ_C49 (+ FEQ_C49 1))
									)
								)
								(if (= fweq "FEQ-C6.5")
									(progn
										(setq FEQ_C65 (+ FEQ_C65 1))
									)
								)
								(if (= fweq "FEQ-C8.1")
									(progn
										(setq FEQ_C81 (+ FEQ_C81 1))
									)
								)
								(if (= fweq "FEQ-C162")
									(progn
										(setq FEQ_C162 (+ FEQ_C162 1))
									)
								)
								(if (= fweq "FEQ-C146")
									(progn
										(setq FEQ_C146 (+ FEQ_C146 1))
									)
								)
								(if (= fweq "FEQ-C130")
									(progn
										(setq FEQ_C130 (+ FEQ_C130 1))
									)
								)
								(if (= fweq "FEQ-C114")
									(progn
										(setq FEQ_C114 (+ FEQ_C114 1))
									)
								)
								(if (= fweq "FEQ-C9.8")
									(progn
										(setq FEQ_C98 (+ FEQ_C98 1))
									)
								)
							)
						)
					)
				)
				
				(setq req (nth 8 LISTA_LINHA))
				(if (/= req "")
					(progn
						(setq valor1 (vl-string-search "REQ" (vl-string-trim " " req)))
						
						
						(if (/= valor1 nil)
							(progn
								(setq ress (vl-string-trim " "(vl-string-subst "" "-"  (vl-string-trim " " (vl-string-subst "" "REQ" (vl-string-trim " " req))))))
								(if (/= ress "")
									(progn
										
										(setq lista (vl-string->list ress))
										(setq lista (vl-remove (ascii " ") lista))
										(setq ress (vl-list->string lista))
										(setq ress (vl-string-trim " " ress))
										(setq ress (strcase ress))
										
										(setq ppp (assoc ress lista_req))
										(if (= ppp nil)
											(progn
												(setq lista_req (cons (list ress 1) lista_req))
											)
											(progn
												(setq lista_req (subst (list ress (+ (nth 1 ppp) 1) ) (assoc ress lista_req) lista_req))
											)
										)
									)
								)
							)
						)
					)
				)
				
				
				
				
				(setq rpad (nth 7 LISTA_LINHA))
				(if (/= rpad "")
					(progn
						(setq valor1 (vl-string-search "RP" (vl-string-trim " " rpad)))
						(if (/= valor1 nil)
							(progn
								(setq ress (vl-string-trim " "(vl-string-subst "" "-"  (vl-string-trim " " (vl-string-subst "" "RP" (vl-string-trim " " rpad))))))
								(if (/= ress "")
									(progn
										(setq ppp (assoc ress lista_rpad))
										(if (= ppp nil)
											(progn
												(setq lista_rpad (cons (list ress 1) lista_rpad))
											)
											(progn
												
												(setq lista_rpad (subst (list ress (+ (nth 1 ppp) 1) ) (assoc ress lista_rpad) lista_rpad))
											)
										)
									)
								)
							)
						)
					)
				)
				
				
				
				
				
			)
		)
		
		
		(setq LINHA_CSV (read-line ARQUIVO_CSV))
		(setq LISTA_LINHA nil)
   )
   (close ARQUIVO_CSV)
)

(defun c:rtt()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(setq ARQUIVO_CSV2 nil)
	(carrega_planilha_amplificadores)
	
	(lendo_informacoes)
	(soma_cabos)
	(exibe_relatorio)
	(salvar_arquivo)
	
	(princ)
)