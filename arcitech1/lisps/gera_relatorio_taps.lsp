(defun procura_numero_tap(c)
	(setq numero_encontrado nil)
 (sam_zoom c 10)
 (setq textos (ssget "C" (polar c (/ pi 4) 2.3) (polar c (* (/ pi 4) 5) 2.3)   (List (cons -4 "<AND") (cons 0 "TEXT")   (cons 8 "TPSYM01")  (cons -4 "AND>")  )  ))
	
	(if (/= textos nil)
		(progn
			(setq qtd2  (sslength textos))
			(if (/= qtd2 1)
				(progn
					(f_nao_configurado2 c)
					;(parar)
				)
				(progn
					(setq numero_encontrado (vl-string-trim " " (cdr (assoc 1 (entget (ssname textos 0)))) ))
				)
			)
			
		)
		(progn
			(f_nao_configurado2 c)
			;(command "circle" c 10)
			;(command "circle" (polar c (/ pi 4) 3) 2)
			;(command "circle" (polar c (* (/ pi 4) 5) 3) 2)
			;(sam_zoom c 10)
			;(parar)
		)
	)
	
	numero_encontrado
)

(defun percorre_elementos()
	
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "TPSYM01")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(setq lista_dos_blocos nil)
	(setq lista_relatorio nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blockName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq procura1 (assoc blockName lista_dos_blocos))
				(if (= procura1 nil)
					(progn
						(setq lista_dos_blocos (cons (list blockName) lista_dos_blocos))
					)
				)
				
				;Trata os taps
				(setq resp (vl-string-search "TAP" blockName))
				(setq numeroTap (retorna_attrib obj 1))
				(if (/= numeroTap nil)
					(progn
						(setq num1 (atof numeroTap))
						(if (= num1 0)
							(progn
								(setq numeroTap nil)
							)
						)
					)
				)
				
				(if (and (or (= numeroTap "CADHB4")  (= numeroTap nil) )  (/= resp nil)  )
					(progn
						(if (= blockName "2HOTTAP")
							(progn
								(setq angulo1 (cdr (assoc 50 (entget obj))))
								(setq coord (polar coord (+ angulo1 (/ pi 2))  2))
							)
						)
						(setq numeroTap (procura_numero_tap coord))
					)
				)
				
				
				(if (/= resp nil)
					(progn
						(if (or  (= numeroTap nil) (= numeroTap "") )
							(progn
								(f_nao_configurado2 coord)
							)
							(progn
								
								(setq chave (strcat blockName "-" numeroTap))
								(setq procura2 (assoc chave lista_relatorio))
								(if (= procura2 nil)
									(progn
										(setq lista_relatorio (cons (list chave 1) lista_relatorio))
									)
									(progn
										(setq total2 (nth 1 procura2))
										(setq total2 (+ total2 1))
										
										(setq lista_relatorio (subst (list chave total2)  (assoc chave lista_relatorio)  lista_relatorio  ))
										
									)
								)
								
							)
						)
					)
					(progn
						;(f_nao_configurado2 coord)
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	
)

(defun f_nao_configurado2(c)
	(command "layer" "m" "Nao_Configurado" "c" "green" "" "")
	(command "circle" c 11)
	(command "circle" c 12)
)



(defun abre_arq()
	
	(setq pathMapProject (getvar "DWGPREFIX"))
	(setq dwgNameFile (getvar "DWGNAME"))
	(setq nomeDaPasta   (vl-string-subst  ".csv" ".dwg" dwgNameFile) )
	(setq nomeDaPasta   (vl-string-subst  ".csv" ".DWG" nomeDaPasta) )
	(setq  ARQUIVO_CSV (open (strcat pathMapProject nomeDaPasta) "w"))
)

(defun fecha()
	(close ARQUIVO_CSV)
)

(defun grava_info12()
	
	(setq tam2 (length lista_relatorio))
	(while (> tam2 0)
		
		(setq el1 (nth (- tam2 1) lista_relatorio))
		
		(setq val1 (sparser (nth 0 el1) "-") )
		(setq val2 (itoa (nth 1 el1)))
		
		(write-line (strcat (nth 0 val1) ";" (nth 1 val1) ";" val2) ARQUIVO_CSV)
		(setq tam2 (- tam2 1))
	)
	
)


(defun grava_info12_dc()
	
	(setq tam2 (length lista_relatorio_dc))
	(while (> tam2 0)
		
		(setq el1 (nth (- tam2 1) lista_relatorio_dc))
		
		(setq val1 (sparser (nth 0 el1) "-") )
		(setq val2 (itoa (nth 1 el1)))
		
		(write-line (strcat (nth 0 val1) ";" (nth 1 val1) ";" val2) ARQUIVO_CSV)
		(setq tam2 (- tam2 1))
	)
	
)




(defun percorre_dc()

	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "CPSYM01")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(setq lista_blocos_dc nil)
	(setq lista_relatorio_dc nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blockName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (or (= blockName "2WAY01")  (= blockName  "2W")  (= blockName "DC01") )
					(progn
						(setq numeroTap (retorna_attrib obj 1))
						
						(if (/= numeroTap nil)
							(progn
								(setq num1 (atof numeroTap))
								(if (= num1 0)
									(progn
										(setq numeroTap nil)
									)
								)
							)
						)
						(if (= numeroTap nil)
							(progn
								(setq numeroTap "000")
							)
							
						)
						
						
						
						(setq chave (strcat blockName "-" numeroTap))
						(setq procura2 (assoc chave lista_relatorio_dc))
						(if (= procura2 nil)
							(progn
								(setq lista_relatorio_dc (cons (list chave 1) lista_relatorio_dc))
							)
							(progn
								(setq total2 (nth 1 procura2))
								(setq total2 (+ total2 1))
								
								(setq lista_relatorio_dc (subst (list chave total2)  (assoc chave lista_relatorio_dc)  lista_relatorio_dc  ))
								
								
							)
						)
						
					)
				)
				
				
				
				(setq procu22 (assoc blockName lista_blocos_dc))
				(if (= procu22 nil)
					(progn
						(setq lista_blocos_dc (cons (list blockName) lista_blocos_dc))
					)
				)
				
				
				
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun c:gera_relatorio_taps()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(percorre_elementos)
	(percorre_dc)
	
	(if (or (/= lista_relatorio_dc nil)  (/= lista_relatorio nil) )
		(progn
		
			(abre_arq)
			(write-line "nome bloco;numero;quantidade" ARQUIVO_CSV)

			(if (/= lista_relatorio nil)
				(progn
					(grava_info12)
			))
			(if (/= lista_relatorio_dc nil)
				(progn
					(grava_info12_dc)
			))
			
			(fecha)
			
		)
	)
	
	(princ)
)