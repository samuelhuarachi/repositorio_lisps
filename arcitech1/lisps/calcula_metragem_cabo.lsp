;samcircle_p_OOOraios_OOOlayername_OOOcolor


(defun retorna_pontos_leader(obj / contador contador encontrados elemento id1 coord1 coord2)
	(setq contador 1)
	(setq sair 0)
	(setq encontrados 0)
	(setq lista_resultado nil)
	(while (or (= sair 0) (< encontrados 2))
		(setq elemento (nth contador (entget obj)))
		(setq id1 (car elemento))
		(if (= id1 10)
			(progn
				(setq encontrados (+ encontrados 1))
				
				(if (= encontrados 1)
					(progn
						(setq coord1 (cdr elemento))
						(setq lista_resultado (cons coord1 lista_resultado))
					)
				)
				(if (= encontrados 2)
					(progn
						(setq coord2 (cdr elemento))
						(setq lista_resultado (cons coord2 lista_resultado))
						(setq sair 1)
					)
				)
			)
		)
		(setq contador (+ contador 1))
	)
	
	lista_resultado
	
)
	
	
(defun percorre_leaders()
	
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET_UPGRADE")(0 . "LEADER")(-4 . "AND>"))))
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
				(setq numeroVertices  (cdr (assoc 76 (entget obj))))
				(if (= numeroVertices 2)
					(progn
						;Pega os ponto leader
						(setq ponto_leader (retorna_pontos_leader obj))
						
						;Pega os dois pontos do leader
						(setq ponto1 (nth 0 ponto_leader))
						(setq ponto2 (nth 1 ponto_leader)) ;o ponto 2 é o ponto que fica em cima da rede
						
						;Procura rede
						(command "zoom" "c" ponto2 30)
						(setq vertice1 (polar ponto2 (/ pi 4) 0.3))
						(setq vertice2 (polar ponto2 (* (/ pi 4) 5) 0.3))
						(command "zoom" "w" vertice1 vertice2)
						(setq rede_find (ssget "C" vertice1 vertice2 (List (cons -4 "<AND") (cons 0 "LWPOLYLINE")   (cons 8 "NET-CBTR" )  (cons -4 "AND>")  )  ))
						(if (/= rede_find nil)
							(progn
								;(setq metragem (calcula_metragem_rede rede_find))
								(setq quantidadeRedes (sslength rede_find))
								(if (/= quantidadeRedes 1)
									(progn
										(samcircle ponto2 3 "layer_temporariaErro_MaisRedeDeUmaRedeEncontrada" "red")
										(samcircle ponto2 3.5 "layer_temporariaErro_MaisRedeDeUmaRedeEncontrada" "red")
										(samcircle ponto2 4 "layer_temporariaErro_MaisRedeDeUmaRedeEncontrada" "red")
										(samcircle ponto2 4.5 "layer_temporariaErro_MaisRedeDeUmaRedeEncontrada" "red")
									)
									(progn
										(setq metragem (GetCurveLength (ssname rede_find 0)))
										(command "layer" "m" "layer_temporaria4" "c" "142" "" "")
										(command "text" "bc" ponto2 1 0 (rtos metragem 2 2))
										
										(if (/= metragem 0)
											(progn
												;Pega o valor digitado pelo usuário
												;Procura pelo bloco que contem as infomrações digitadas pelo usuário
												(setq objInformacoes (procura_atualizacoes ponto2 ponto1))
												(if (/= objInformacoes nil)
													(progn
														(setq metragem_usuario (atof (retorna_attrib objInformacoes 4)))
														(setq valorMaxMin (* metragem  (/  (- 100.000 tolerancia) 100.000)   ))
														(setq differenca (- metragem valorMaxMin))
														
														(setq valorMin (- metragem differenca))
														(setq valorMax (+ metragem differenca))
														
														(if (and (> metragem_usuario valorMin)   (< metragem_usuario valorMax)  )
															(progn
																
															)
															(progn
																;(command "layer" "m" "layer_temporariaErro" "c" "red" "" "")
																;samcircle_p_OOOraios_OOOlayername_OOOcolor
																
																(samcircle ponto2 3 "layer_temporariaErro" "red")
																(samcircle ponto2 3.5 "layer_temporariaErro" "red")
																(samcircle ponto2 4 "layer_temporariaErro" "red")
																(samcircle ponto2 4.5 "layer_temporariaErro" "red")
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
						
						;(getstring "samuel")
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
)

(defun calcula_metragem_rede(all / obj qtd contador  elemento coord1)
	(setq retorno_metragem 0)
	(setq pontoAntigo nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			
			(if (> qtd 0)
				(progn
					
				)
				(progn
					(while (>= qtd 0)
						(setq obj (ssname all qtd))
						(setq contador 1)
						(setq elemento "")
						(while (/= elemento nil)
							(setq elemento (nth contador (entget obj)))
							
							(if (/= elemento nil)
								(progn
									(setq id1 (car elemento))
									(if (= id1 10)
										(progn
											(setq coord1 (cdr elemento))
											
											(if (/= pontoAntigo nil)
												(progn
													(command "layer" "m" "layer_temporaria3" "c" "cyan" "" "")
													(command "line" coord1 pontoAntigo "")
													(setq retorno_metragem (+ retorno_metragem (distance coord1 pontoAntigo) ))
												)
											)
											
											;samcircle_p_OOOraios_OOOlayername_OOOcolor
											(samcircle coord1 nil  nil nil)
											(setq pontoAntigo coord1)
										)
									)
								)
							)
							
							(setq contador (+ contador 1))
						)
						
						(setq qtd (- qtd 1))
					)
				)
			)
			
			
		)
	)
	
	retorno_metragem
)


(defun ajusta_layer_calcula_metragem_cabo()

	(command "layer" "off" "*" "" "")
	
	(command "layer" "ON" "NET_UPGRADE" "" "")
	(command "layer" "ON" "NET-CBTR" "" "")
	(command "layer" "ON" "layer_temporaria3" "" "")
	(command "layer" "ON" "layer_temporaria4" "" "")
	(command "layer" "ON" "layer_temporaria1" "" "")
	(command "layer" "ON" "layer_temporaria2" "" "")
	(command "layer" "ON" "layer_temporaria5" "" "")
)

(defun apaga_layers_desnecessarias_002()
	(setq all (ssget "X" '((8 . "layer_temporariaErro,layer_temporariaErro_MaisRedeDeUmaRedeEncontrada"))))
	(sam_delete all)
)

(defun GetCurveLength (curve / )
	(vl-load-com)
	(setq curve (vlax-ename->vla-object curve))
	(vlax-curve-getDistAtParam curve
	(vlax-curve-getEndParam curve)
	)
)



(defun c:calcula_metragem_cabo()
	
	
	(setq resp1 (strcase (getstring "\nAjustar layer [S/N]")))
	(if (= resp1 "S")
		(progn
			(ajusta_layer_calcula_metragem_cabo)
		)
	)
	
	(setq tolerancia (getstring "\nTolerância para a margem de erro (em porcentagem %). Valor padrão igual 10%"))
	(if (= tolerancia "")
		(progn
			(setq tolerancia 10.000) ;Poncentagem da tolerância sobre o comprimento dos cabos. 
		)
		(progn
			(setq tolerancia (atof tolerancia))
		)
	)
	
	
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(apaga_layers_desnecessarias_002)
	
	
	(princ "\nCalcula_metragem_cabo")
	(percorre_leaders)
	
	(princ "\n=========   LAYERS GERADAS  =================")
	(princ "\n 'layer_temporariaErro'")
	(princ "\n 'layer_temporariaErro_MaisRedeDeUmaRedeEncontrada'")
	
	(princ)
)

