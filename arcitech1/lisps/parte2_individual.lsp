;ultima_funcao______varGlobal
;remove_letras_string
;remove_letras_string_____STRING1
;remove_numeros_string
;remove_numeros_string_____STRING1
;CriarLink
;criarlink
;CriarLink_____ent_link_codigo
;criarlink_____ent_link_codigo
;GetId
;GetId_____entd_nomeLink
;getid
;getid_____entd_nomeLink
;faz_janela
;faz_janela_____COORD_TIPO_PROPRIEDADE_TAMANHO
;desenha_linha
;desenha_linha_____p1_p2_layer_cor
;desenha_circulo
;desenha_circulo_____ponto_raio_layer_cor
;quantidade_elementos_array
;quantidade_elementos_array_____array1
;pega_propriedade_objeto
;pega_propriedade_objeto_____obj_prop
;gerar_lista_elementos
;gerar_lista_elementos_____layer_tipo_elementos
;retorna_attrib
;retorna_attrib_____BLOCO_numATTRIB
;gera_layer
;gera_layer_____nomeLayer_Cor
;arredonda_coordenada
;arredonda_coordenada_____coord_casas
;exibir_porcentagem
;exibir_porcentagem_____Qtd
;verxdata
;verxdata_____semParametros
;ViewExtents
;ViewExtents_____semParametros
;viewextents
;viewextents_____semParametros
;distancia_ponto_reta
;distancia_ponto_reta_____p1,p2,ponto_insercao
;sam_paraBaixo
;sam_paraCima
;sam_metade
;sam_metade___________p1,p2
;GetKey
;GetKey____Sem atributos
;remove_espaco_string
;remove_letras_string



;cuidado ao usar a variavel contador

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


(defun carrega_lista_posicao_novos()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 "NET-TAP"))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET-TAP")(0 . "INSERT")(-4 . "AND>"))))
	(setq lista1 nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq valor1 (retorna_attrib obj 1))
				
				;(write-line (strcat x1 ";" y1 ";" valor1) ARQUIVO_CSV)
				
				(setq lista1 (cons  (list (strcat x1 y1) valor1  ) lista1))
				
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


(defun carrega_lista_memoria()
	(setq ARQUIVO_CSV (getfiled "Selecione o arquivo CSV" "c:" "csv" 0))
	
	(setq ARQUIVO_CSV (open ARQUIVO_CSV "r"))
	(setq lista_csv_save nil)
	(setq contador1 0)
	(if (/= ARQUIVO_CSV nil)
		(progn

			(setq 
				 LINHA_CSV (read-line ARQUIVO_CSV)
				 LISTA_LINHA nil
				 LISTA_CSV nil
			)

			(while (/= LINHA_CSV nil)

				(setq LISTA_LINHA (sparser LINHA_CSV ";"))
				
				(setq lista_csv_save (cons (list (strcat  (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA)  )  LINHA_CSV  ) lista_csv_save ))
				
				(setq coord_aux1 (list (atof (nth 0 LISTA_LINHA) )  (atof (nth 1 LISTA_LINHA))  0))
				
				
				
				(setq LINHA_CSV (read-line ARQUIVO_CSV))
				(setq LISTA_LINHA nil)
				
				
			)
			(close ARQUIVO_CSV)
			(setq ARQUIVO_CSV nil)
		)
	)
	
	
)


			

(defun carrega_arquivo()
 
	(setq contador1 0)
	(setq procura nil)
	(setq forcar_procura 1)
	(if (/= lista_informacoes nil)
		(progn
			(setq info1 (assoc (nth 0 (nth 0 lista_informacoes)) lista_csv_save))
			(setq LINHA_CSV (nth 1 info1))
			(setq LISTA_LINHA (sparser LINHA_CSV ";"))
			(setq procura (assoc  (strcat (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA))  lista_informacoes ))
		)
		(progn
			
			(setq coord (cdr (assoc 10 (entget tapObject))))
			(setq x1 (rtos (car coord) 2 3))
			(setq y1 (rtos (cadr coord) 2 3))
			
			(setq info1 (assoc (strcat x1 y1) lista_csv_save))
			(setq LINHA_CSV (nth 1 info1))
			(setq LISTA_LINHA (sparser LINHA_CSV ";"))
			(setq forcar_procura 0)
		)
	)
	
	(setq x (atof (nth 0 LISTA_LINHA)))
	(setq y (atof (nth 1 LISTA_LINHA)))
	
	(if (/= procura nil)
		(progn
			
			(setq nomeBloco1 (vl-string-trim " " (strcase (nth 3 LISTA_LINHA))))
			(setq nomeBloco2 (vl-string-trim " " (strcase (nth 5 procura))))
			
			
			
			(if (= nomeBloco1 nomeBloco2)
				(progn
					(setq contador1 (+ contador1 1))
					(setq Antigo  (  nth 2 LISTA_LINHA  ) )
					(setq Novo  (  nth 3 procura  ) )
					
					
					
					(setq antigoArray  (nth 1 (sparser (nth 1 procura) "/")))
					(setq novoArray  (nth 1 (sparser (nth 2 procura) "/")))
					
					(if (and (= Novo novoArray) (= Antigo antigoArray))
						(progn
							(command "layer" "m" "Configurado_Ok" "c" "yellow" "" "")
							(command "circle" (list x y 0) 5)
							(command "circle" (list x y 0) 6)
						)
						(progn
							;lista_informacoes
							
							;(command "zoom" "c" (nth 4 procura) 10)
							;(getstring "Divergencia")
							
							(command "layer" "m" "Divergencia_Valores_incopativeis" "c" "133" "" "")
							(command "circle" (list x y 0) 7)
							(command "circle" (list x y 0) 8)
						)
					)
				)
				(progn
					(command "layer" "m" "Nomes_dos_blocos_incopativeis" "c" "190" "" "")
					(command "circle" (list x y 0) 9)
					(command "circle" (list x y 0) 10)
				)
			)
			
			
		)
		(progn
			
			(if (= forcar_procura 1)
				(progn
					(setq procura (forca_procura  (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA) ))
				)
				(progn
					(setq procura "")
				)
			)
			
			(if (/= procura "")
				(progn
					
					(setq nomeBloco1 (vl-string-trim " " (strcase (nth 3 LISTA_LINHA))))
					(setq nomeBloco2 (vl-string-trim " " (strcase (nth 5 procura))))
					
					(if (= nomeBloco1 nomeBloco2)
						(progn
							(setq Antigo  (nth 2 LISTA_LINHA) )
							(setq Novo  (nth 3 procura) )
							
							(setq antigoArray  (nth 1 (sparser (nth 1 procura) "/")))
							(setq novoArray  (nth 1 (sparser (nth 2 procura) "/")))
							
							(if (and (= Novo novoArray) (= Antigo antigoArray))
								(progn
									(command "layer" "m" "Configurado_Ok" "c" "yellow" "" "")
									(command "circle" (list x y 0) 5)
									(command "circle" (list x y 0) 6)
								)
								(progn
									(command "layer" "m" "Divergencia_Valores_incopativeis" "c" "133" "" "")
									(command "circle" (list x y 0) 7)
									(command "circle" (list x y 0) 8)
								)
							)
						)
						(progn
							
							(command "layer" "m" "Nomes_dos_blocos_incopativeis" "c" "190" "" "")
							(command "circle" (list x y 0) 9)
							(command "circle" (list x y 0) 10)
							
						)
					)
					
					
				)
				(progn
					
					
					(setq lista_posicao_tap nil)
					(setq coord (cdr (assoc 10 (entget tapObject))))
					(setq x1 (rtos (car coord) 2 3))
					(setq y1 (rtos (cadr coord) 2 3))
					(setq lista_posicao_tap (cons (list (strcat x1 y1) tapObject  ) lista_posicao_tap))
					
					
					(setq searchElement (assoc (strcat (nth 0 LISTA_LINHA)   (nth 1 LISTA_LINHA)) lista_posicao_tap))
					;(setq x (atof (nth 0 LISTA_LINHA)))
					;(setq y (atof (nth 1 LISTA_LINHA)))
					
					(if (/= searchElement nil)
						(progn
							
							(setq valorTap (retorna_attrib (nth 1 searchElement) 1) )
							(setq blockName3 (cdr (assoc 2 (entget (nth 1 searchElement)))))
							(setq logErro 0)
							;Verificando se os blocos possem os mesmos valores
							(if (/= (vl-string-trim " "	(strcase blockName3))  (vl-string-trim " "	(strcase (nth 3 LISTA_LINHA)))   )
								(progn
									
									(f_nome_dos_blocos_incompativeis)
									(setq logErro 1)
								)
							)
							
							
							
							;Verificando se a numera��o dos blocos s�o compat�veis
							(if (/= (vl-string-trim " "	(strcase valorTap))  (vl-string-trim " "	(strcase (nth 2 LISTA_LINHA)))   )
								(progn
									
									(f_numeracao_dos_blocos_incompativeis)
									(setq logErro 1)
								)
							)
							
							
							(if (= logErro 0)
								(progn
									;Faz a marcacao no mapa indicando que est� tudo ok
									(f_tudo_ok)
								)
							)
							
							
							
						)
						(progn
							
							
							(f_nao_configurado)
							
						)
					)
					
					
					
					
					
					
				)
			)
			
			
		)
	)
	


 
)


(defun forca_procura(x y)
	(setq coordTxt (list (atof x)  (atof y) 0))
	(setq qtd3 (length  lista_informacoes))
	(setq sair 0)
	(setq resultado "")
	(while (and (> qtd3 0) (= sair 0) )

		(setq coordLista (nth 4 (nth (- qtd3 1) lista_informacoes)))
		
		(setq distancia (distance coordTxt coordLista)) 
		
		(if (< distancia 0.01)
			(progn
				(setq sair 1)
				(setq resultado (nth (- qtd3 1) lista_informacoes))
			)
		)
		
		
		(setq qtd3 (- qtd3 1))
	)
	
	resultado
)

(defun relaciona_leaders()

	(princ "\nSelecione LEADER")
	(setq all (ssget '((0 . "LEADER"))))
	
	
	(setq lista_informacoes nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq numeroVertices  (cdr (assoc 76 (entget obj))))
				
				(if (= numeroVertices 2)
					(progn
						;procura coordenadas
						(setq contador 1)
						(setq sair 0)
						(setq encontrados 0)
						(while (or (= sair 0) (< encontrados 2))
							
							(setq elemento (nth contador (entget obj)))
							
							(setq id1 (car elemento))
							
							(if (= id1 10)
								(progn
									(setq encontrados (+ encontrados 1))
									
									(if (= encontrados 1)
										(progn
											(setq coord1 (cdr elemento))
										)
									)
									(if (= encontrados 2)
										(progn
											(setq coord2 (cdr elemento))
											(setq sair 1)
										)
									)
									
								)
							)
							
							(setq contador (+ contador 1))
						)
						
						(setq objPolyline nil)
						(setq objInformacoes (procura_atualizacoes coord1 coord2))
						(if (= objInformacoes nil) 
							(progn
								
							)
							(progn
								
								(setq blockName (strcase (cdr (assoc 2 (entget objInformacoes)))))
								
								(if (= blockName "CX_CODUPGRADECABO" nil)
									(progn
										
									)
									(progn
										
										(setq objPolyline (procura_polyline coord2 coord1))
									)
								)
								
								
							)
						)
						
						
						(if (and (/= objInformacoes nil) (/= objPolyline nil))
							(progn
								
								
								(setq listaObjetoPolyline (pontos_polyline objPolyline  ))
								(command "zoom" "c" (nth 0 listaObjetoPolyline) 50)
								
								(setq all2 (ssget "WP"  listaObjetoPolyline))
								(if (= all2 nil)
									(progn
										(setq all2 (ssget "WP"  listaPontoPolyline2))
									)
								)
								
								
								(if (/= all2 nil)
									(progn
										
										(setq qtd2 (- (sslength all2) 1))
										(setq achouObj 0)
										(while (>= qtd2 0)
											(setq obj2 (ssname all2 qtd2))
											
											(if (= (strcase (cdr (assoc 0 (entget obj2)))) "INSERT")
												(progn
													
													
													
													
													(setq layerName2 (strcase (cdr (assoc 8 (entget obj2)))))
													(setq blockName2 (strcase (cdr (assoc 2 (entget obj2)))))
													
													(if (or (= layerName2 "NET-TAP")   (= blockName2 "DC")   )
														(progn
															
															(setq procurarPor nil)
															(if (= layerName2 "NET-TAP")
																(progn
																	(setq procurarPor "TAP")
																)
															)
															(if (= blockName2 "DC")
																(progn
																	(setq procurarPor "ACOPLADOR")
																)
															)
															
															(setq contador2 1)
															(setq atributoFind (retorna_attrib objInformacoes contador2) )
															(if (/= atributoFind nil)
																(progn
																	(setq atributoFind (vl-string-trim " "	(strcase atributoFind)))
																)
															)
															(setq sair 0)
															(setq resultadoPesquisa "N") ;nao encontrado
															
															
															
															(while (and (/= atributoFind nil)  (= sair 0))
																
																(if (= atributoFind procurarPor)
																	(progn
																		(setq resultadoPesquisa "S") ;encontrado
																		(setq sair 1)
																	)
																)
																
																
																(setq contador2 (+ contador2 1))
																(setq atributoFind (retorna_attrib objInformacoes contador2) )
																(if (/= atributoFind nil)
																	(progn
																		(setq atributoFind (vl-string-trim " "	(strcase atributoFind)))
																	)
																	(progn
																		(setq sair 1)
																	)
																)
															)
															
															(if (= resultadoPesquisa "N")
																(progn
																	(princ (strcat "N�o encontrou o valor " procurarPor " nos atributos do bloco de informa��es"))
																	(setq coord_objI (cdr (assoc 10 (entget objInformacoes))))
																	(command "layer" "m" "erro1" "c" "red" "" "")
																	(command "circle" coord_objI 10)
																	(command "circle" coord_objI 11)
																	(command "circle" coord_objI 12)
																	(sam_zoom coord_objI 20)
																	(getstring "\nPressione [ESC] para sair")
																)
															)
															
															(if (= resultadoPesquisa "S")
																(progn
																	
																	;objInforma��es
																	(setq valorAntigo (retorna_attrib objInformacoes contador2 ))
																	(setq valorNovo (retorna_attrib objInformacoes (+ contador2 1)))
																	
																	
																	
																	(setq Novo (retorna_attrib obj2 1))
																	
																	(setq coord3 (cdr (assoc 10 (entget obj2))))
																	(setq x1 (rtos (car coord3) 2 3))
																	(setq y1 (rtos (cadr coord3) 2 3))
																	
																	(setq lista_informacoes (cons (list (strcat x1 y1 ) valorAntigo valorNovo  Novo coord3 blockName2) lista_informacoes))
																	
																	(setq achouObj 1)
																	
																)
																(progn
																	(command "layer" "m" "Divergencia2" "c" "241" "" "")
																	(command "circle" (list (atof x1) (atof y1) 0) 4)
																	(command "circle" (list (atof x1) (atof y1) 0) 3)
																
																)
															)
															
															
															
															
														)
													)
												)
											)
											
											
											
											
											(setq qtd2 (- qtd2 1))
										)
										
										(if (= achouObj 0)
											(progn
												;(getstring "n�o encontrou o objeto")
												
												
											)
										)
										
										
										
									)
									(progn
										;(getstring "zzzz")
									)
								)
							)
						)
						
						
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
		(progn
			
			(princ "\nSelecione o objeto TAP")
			(setq tapObject (ssget '((8 . "NET-TAP,DC"))))
			(setq tapObject (ssname tapObject 0))
			
		)
	)
	
	
)

(defun pontos_polyline2(obj)

	(setq el (entget obj)
	 gr (car el) )
	(while gr
		(setq nr (car gr))
		(if (= nr 10)
			(progn
				(setq xv (cadr gr)
						yv (caddr gr)
						zv (cadddr gr)
				)
				(setq tx (strcat (rtos xv) " " (rtos yv)))
				
				
				(setq ponto1 (list xv yv 0))
				(if (and (/= ponto1 nil)(/= ponto2 nil))
					(progn
					
						(setq x1 (rtos (car ponto1) 2 3))
						(setq y1 (rtos (cadr ponto1) 2 3))
						
						
						(setq x2 (rtos (car ponto2) 2 3))
						(setq y2 (rtos (cadr ponto2) 2 3))
						
						(if (/= (strcat x1 y1) (strcat x2 y2)  )
							(progn
								
								;Configura ordem dos pontos
								
								(setq pontoMeio (polar ponto1 (angle ponto1 ponto2)  (/ (distance ponto1 ponto2) 2) ))
								(setq ponto3 (polar pontoMeio  (+ (angle ponto1 ponto2) (/ pi 2))   (* distancia12 2)   ))
								
								
								(setq x1 (rtos (car ponto1) 2 3))
								(setq y1 (rtos (cadr ponto1) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons (cdr elemento) listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								(setq x1 (rtos (car ponto3) 2 3))
								(setq y1 (rtos (cadr ponto3) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons (cdr elemento) listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								(setq x1 (rtos (car ponto2) 2 3))
								(setq y1 (rtos (cadr ponto2) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons (cdr elemento) listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								(command "layer" "m" "samuel1" "c" "cyan" "" "")
								(command "line" ponto1 ponto3 "")
								(command "zoom" "c" ponto3 10)
								;(getstring "ldldl")
								(command "line" ponto3 ponto2 "")
								(command "zoom" "c" ponto2 10)
								;(getstring "ldldl")
							
							)
						
						)
						
					)
				)
				(setq ponto2 ponto1)
				
				
			)
		)
		(setq el (cdr el)
			  gr (car el)
		)
	)
	
	
)












(defun ListPoliSel (entidade)
 (if entidade
  (progn

		(setq en_lay (assoc 8 (entget entidade))
					ls_vtc (entget entidade)
					tp_ent (cdr (assoc 0 ls_vtc))
					ls_par '()
					ls_pts '()																					; reseta lista de pontos da pline
					ls_xdt '()
					ls_dat '()
		)
	 
	 
	   
	 (if (= tp_ent "LINE")
	   (progn
	     (setq ls_pts (append ls_pts (list (cdr (assoc 10 ls_vtc))))
	           ls_pts (append ls_pts (list (cdr (assoc 11 ls_vtc))))
	     )
	   )
	   	
	  (progn
			 (setq is 0)
				(foreach par ls_vtc
				 (setq is (+ is 1))
				 
				 (if (= (car par) 10)
				   (setq vl_vtc (list (abs (car (cdr par))) (cadr (cdr par))) ;(cdr par)
		         vl_fat (cdr (nth (+ is 2) ls_vtc))
		         Ls_Par (append ls_par (list (list vl_vtc vl_fat)))
		       )
				 )
			  )    
				
				
				(if (= (cdr (assoc 70 (entget entidade))) 1)
				  (setq ls_par (append ls_par (list (car ls_par))))
				)  
				
				(setq k 0)
				(setq an_div 10)
				(while (< k (length ls_par))
					(setq v1 (car (nth k ls_par))
								v2 (car (nth (1+ k) ls_par))
								ft_v1 (cadr (nth k ls_par))													; 2H/D
					)
					
					(if (and (/= ft_v1 0.0) v2)
						(progn
							(setq d1 (distance v1 v2)																	; corda
										h1 (/ (* d1 (abs ft_v1)) 2.0)															; flecha
										d2 (/ d1 2.0)
										an_arc (/ (* (atan (abs ft_v1)) 4.0) 2.0)											; �ngulo do arco
										ra_arc (/ d2 (sin an_arc))																	; raio do arco
										ca (- ra_arc h1)																				; dist�ncia do p0 at� a corda
										pm (polar v1 (angle v1 v2) d2)											; ponto m�dio da corda
										pt_cen (polar pm (+ (angle v1 v2) (* (if (minusp ft_v1) 1.5 0.5) PI)) ca)				; centro do arco
										an_ini (angle pt_cen v1)																												; angulo inicial do arco
										an_fim (angle pt_cen v2)																												; angulo final do arco
										cp_arc (* (/ (* 2 an_arc 180.0) PI) (/ (* 2 PI ra_arc) 360))	; comprimento total do arco
										cp_inc (* an_div (/ (* 2 PI ra_arc) 360))											; comprimento do incremento do arco
							)
							(setq an_bas (* (/ PI 180) an_div)															; valor do �ngulo base em radianos
										an_pto an_ini																					; inicializa �ngulo do ponto 
										cp_per 0																					; inicializa comprimento percorrido
							)
							(while (< cp_per cp_arc)
								(setq pt_pln (polar pt_cen an_pto ra_arc)
											an_pto (if (minusp ft_v1) (- an_pto an_bas) (+ an_pto an_bas))
											cp_per (+ cp_per cp_inc)
								)
								(if (not (member (list (append pt_pln (list 0.0))) ls_pts))
									(if (= (getvar "acadver") "14i")
											(setq ls_pts (append ls_pts (list pt_pln)))
											(setq ls_pts (append ls_pts (list (append pt_pln (list 0.0)))))
									)
								)
							)
						)
						
						(progn
						    	
								(if (not (member v1 ls_pts))
									(if (= (getvar "acadver") "14i")
											(setq ls_pts (append ls_pts (list pt_pln)))
											(setq ls_pts (append ls_pts (list v1)))
									)
								)
						)
						
						
						
					)
					(setq k (1+ k))
				)
				
				(if (equal (car ls_par) (last ls_par))
				  (setq ls_pts (append ls_pts (list (caar ls_par))))
				)  
				
	  	)
		)
		
		
	)
 )
 
 ls_pts
)


(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite '" nomeComando "'"))
	(princ)
)

(setq basePath "C:\\arcitech1\\")
(setq nomeComando "ppi")
(setq pathLisp (strcat basePath "parte2_individual.lsp"))
(load (strcat basePath "lisps_aux\\funcoes.lsp"))


(defun c:ppi()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(setq posicao_inicial (ViewExtents))
	
	;Carrega a lista do csv na mem�ria
	(carrega_lista_memoria)
	
	(relaciona_leaders)
	
	(carrega_arquivo)
	
	(command "zoom" "w" (nth 0 posicao_inicial) (nth 1 posicao_inicial))
	(alert "\nLayer geradas")
	(princ "\nDivergencia2")
	(princ "\nConfigurado_Ok")
	(princ "\nNomes_dos_blocos_incopativeis")
	(princ "\nNao_Configurado")
	(princ "\nFim...")
	(princ)
)