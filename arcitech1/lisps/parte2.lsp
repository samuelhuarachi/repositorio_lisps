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

;lista_informacoes - São as informações que relacionam o objeto informação,
;com os objetos que ficam dentro da nuvem
(defun carrega_arquivo()
 
	;(setq ARQUIVO_CSV (getfiled "Selecione o arquivo CSV" "c:" "csv" 0))
	(setq ARQUIVO_CSV (open ARQUIVO_PATH "r"))

	(setq contador1 0)
	(if (/= ARQUIVO_CSV nil)
		(progn

			(setq 
				 LINHA_CSV (read-line ARQUIVO_CSV)
				 LISTA_LINHA nil
				 LISTA_CSV nil
			)
			
			;Percorrendo o csv do arquivo base
			(while (/= LINHA_CSV nil)

				(setq LISTA_LINHA (sparser LINHA_CSV ";"))
				
				;Procura a coordenada do arquivo base, na lista_informações*
				;*lista_informações - São as informações que relacionam o objeto informação,
				;com os objetos que ficam dentro da nuvem
				(setq procura (assoc  (strcat (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA))  lista_informacoes ))
				
				;(assoc "700281.0317529290.421" lista_informacoes)
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
										(f_tudo_ok)
									)
									(progn
										
										(f_numeracao_dos_blocos_incompativeis)
									)
								)
							)
							(progn
								;Gera os círculos no mapa, indicando que houve erro
								;(parar)
								(f_nome_dos_blocos_incompativeis)
							)
						)
						
						
					)
					(progn
						
						(setq procura (forca_procura  (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA) ))
						
						;(getstring "ldldld")
						
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
												;Faz a marcacao no mapa indicando que está tudo ok
												(f_tudo_ok)
											)
											(progn
												
												(f_numeracao_dos_blocos_incompativeis)
											)
										)
									)
									(progn
										(f_nome_dos_blocos_incompativeis)
										
									)
								)
								
								
							)
							(progn
								
								;O objeto pode estar na nuvem, mas essa nuvem ter mais de dois taps
								
								
								
								
								
								
								;Se chegou até esse ponto. Significa que o objeto não tem núvem.
								;Ou seja, não foi alterado.
								(setq searchElement (assoc (strcat (nth 0 LISTA_LINHA)   (nth 1 LISTA_LINHA)) lista_posicao_tap))
								
								(if (= searchElement nil)
									(setq searchElement (forca_procura_ponto_isolado  (nth 0 LISTA_LINHA) (nth 1 LISTA_LINHA) ) )
								)
								
								
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
										
										
										
										;Verificando se a numeração dos blocos são compatíveis
										(if (/= (vl-string-trim " "	(strcase valorTap))  (vl-string-trim " "	(strcase (nth 2 LISTA_LINHA)))   )
											(progn
												
												(f_numeracao_dos_blocos_incompativeis)
												(setq logErro 1)
											)
										)
										
										
										(if (= logErro 0)
											(progn
												;Faz a marcacao no mapa indicando que está tudo ok
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
				

				(setq LINHA_CSV (read-line ARQUIVO_CSV))
				(setq LISTA_LINHA nil)
			)
			
			
			(close ARQUIVO_CSV)
			(setq ARQUIVO_CSV nil)
		)
	)
 
)


(defun forca_procura_ponto_isolado(x y / elemEncontrado all qtd obj coord x1 y1 distancia1 pontoBase)
	(setq elemEncontrado nil)
	(setq menorDist 10000000000000)
	
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET-TAP")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq pontoBase (list (atof x) (atof y) 0))
				
				(setq distancia1 (distance pontoBase coord ))
				
				(if (and (< distancia1 1.7)  (< distancia1 menorDist)  )
					(progn
						(setq menorDist distancia1)
						(setq elemEncontrado (list (strcat x1 y1) obj))
					)
				)
				
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	elemEncontrado
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
	(setq lista_muitos_taps nil) ;gera a veriável que irá armazenar a quantidade de taps presentes
	
	
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET_UPGRADE")(0 . "LEADER")(-4 . "AND>"))))
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
								
								(if (= blockName "CX_CODUPGRADECABO")
									(progn
										
									)
									(progn
										(setq objPolyline (procura_polyline coord2 coord1))
									)
								)
							)
						)
						
						;Verifica quantos taps foram excluídos do bloco de informações
						;(setq qtdTapExcluidos (conta_qtd_taps_excluidos objInformacoes))
						
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
								
								(setq qtd_taps 0) ;contador que irá marcar a quantidade de taps que ficam dentro da nuvem
								(setq qtd_base_tap 0)
								(setq qtd_dc_base 0)
								(setq qtd_acoplador 0)
								(setq lista_objetos_base_tap nil)
								(setq lista_objetos_tap_mapa nil)
								(setq lista_objetos_base_dc nil)
								(setq lista_objetos_dc_mapa nil)
								(if (/= all2 nil)
									(progn
										
										;Retorna o objeto no ponto base
										(setq objPontoBase (retorna_pontos_base all2))
										
										(setq qtd2 (- (sslength all2) 1))
										(setq achouObj 0)
										(while (>= qtd2 0)
											(setq obj2 (ssname all2 qtd2))
											
											;Se o objeto for um circulo, com as informações do ponto base
											(if (= (strcase (cdr (assoc 0 (entget obj2)))) "CIRCLE")
												(progn
													
													(setq layerName2 (strcase (cdr (assoc 8 (entget obj2)))))
													(if (= layerName2 "PONTO_BASE")
														(progn
															(setq blockName2 (strcase (GetId obj2 "nome_bloco")))
															
															;Verifica se a string é do tipo tap
															(setq seTap (vl-string-search "TAP" blockName2))
															(if (/= seTap nil)
																(progn
																	(setq qtd_base_tap (+ qtd_base_tap 1))
																	(setq lista_objetos_base_tap (cons obj2 lista_objetos_base_tap))
																)
															)
															
															
															;Monta lista objeto DC
															;qtd_dc_base
															(setq dcFinding (vl-string-search "DC" blockName2))
															(if (/= dcFinding nil)
																(progn
																	(setq qtd_dc_base (+ qtd_dc_base 1))
																	(setq lista_objetos_base_dc (cons obj2 lista_objetos_base_dc))
																)
															)
															
														)
													)
													
												)
											)
											
											
											(if (= (strcase (cdr (assoc 0 (entget obj2)))) "INSERT")
												(progn
												
													(setq layerName2 (strcase (cdr (assoc 8 (entget obj2)))))
													(setq blockName2 (strcase (cdr (assoc 2 (entget obj2)))))
													
													(if (or (= layerName2 "NET-TAP")   (= blockName2 "DC")  (= blockName2 "3WAY") )
														(progn
															
															(setq procurarPor nil)
															(if (= layerName2 "NET-TAP")
																(progn
																	(setq procurarPor "TAP")
																	(setq qtd_taps (+ qtd_taps 1))
																	(setq lista_objetos_tap_mapa (cons obj2 lista_objetos_tap_mapa))
																)
															)
															(if (or  (= blockName2 "DC")  (= blockName2 "3WAY") )
																(progn
																	(setq procurarPor "ACOPLADOR")
																	(setq qtd_acoplador (+ qtd_acoplador 1))
																	(setq lista_objetos_dc_mapa (cons obj2 lista_objetos_dc_mapa))
																	
																)
															)
															
															
															;==== Percorrendo atributos do bloco informações - i
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
															;==== Percorrendo atributos do bloco informações - f
															
															
															(if (= resultadoPesquisa "S")
																(progn
																	;objInformações
																	(setq valorAntigo (retorna_attrib objInformacoes contador2 ))
																	(setq valorNovo (retorna_attrib objInformacoes (+ contador2 1)))
																	(setq Novo (retorna_attrib obj2 1))
																	
																	(setq coord3 (cdr (assoc 10 (entget obj2))))
																	(setq x1 (rtos (car coord3) 2 3))
																	(setq y1 (rtos (cadr coord3) 2 3))
																	
																	;Muda o ponto de inserção do TAP, para o ponto de inserção do ponto
																	;Pois os objetos estão em pontos diferentes
																	(if (/= objPontoBase nil)
																		(progn
																			(setq coord3 (cdr (assoc 10 (entget objPontoBase))))
																			(setq x1 (rtos (car coord3) 2 3))
																			(setq y1 (rtos (cadr coord3) 2 3))
																		)
																	)
																	
																	(if (/= blockName2 "DC")
																		(progn
																			(setq lista_informacoes (cons (list (strcat x1 y1 ) valorAntigo valorNovo  Novo coord3 blockName2  objPontoBase) lista_informacoes))
																		)
																	)
																	
																	
																	(setq achouObj 1)
																)
															)
															
															
															
															(if (/= resultadoPesquisa "S")
																(progn
																	;(parar)
																	;(command "layer" "m" "Divergencia2" "c" "241" "" "") ;existe tap, ou dc. E no bloco das informações precisa estar declarado TAP, e DC
																	;(command "circle" (list (atof x1) (atof y1) 0) 4)
																	;(command "circle" (list (atof x1) (atof y1) 0) 3)
																)
															)
															
														)
													)
												)
											)
											
											(setq qtd2 (- qtd2 1))
										)
										
										;dentro da 'nuvem'
										
										;Se a quantidade taps dentro da nuvem for maior do que 1
										(if (or (> qtd_base_tap 1)  (> qtd_taps 1)  (> qtd_dc_base 0)  (> qtd_acoplador 0) )
											(progn
												
												(setq lista_muitos_taps  (cons (list qtd_taps qtd_base_tap  lista_objetos_tap_mapa 
												lista_objetos_base_tap   objInformacoes lista_objetos_base_dc lista_objetos_dc_mapa) lista_muitos_taps))
												
											)
										)
										
										
										(if (= achouObj 0)
											(progn
												;(getstring "não encontrou o objeto")
												
												
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
	)
	
	
)

(defun conta_qtd_taps_excluidos(obj / contadorAtributos qtdExcluidosInformacoes valA valExclusao )
	(setq contadorAtributos 1)
	(setq qtdExcluidosInformacoes 0)
	(while ( /=  (retorna_attrib obj contadorAtributos) nil )
		
		(setq valA  (retorna_attrib obj contadorAtributos))
		(setq valA (vl-string-trim " "	(strcase valA)))
		
		(if (= valA "TAP")
			(progn
				(setq valExclusao  (retorna_attrib obj (+ contadorAtributos 2)))
				(setq valExclusao (vl-string-trim " " (strcase valExclusao)))
				(if (= valExclusao "0")
					(progn
						(setq qtdExcluidosInformacoes (+ qtdExcluidosInformacoes 1))
					)
				)
				
			)
		)
		
		(setq contadorAtributos (+ contadorAtributos 1))
	)
	
	qtdExcluidosInformacoes
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
										an_arc (/ (* (atan (abs ft_v1)) 4.0) 2.0)											; ângulo do arco
										ra_arc (/ d2 (sin an_arc))																	; raio do arco
										ca (- ra_arc h1)																				; distância do p0 até a corda
										pm (polar v1 (angle v1 v2) d2)											; ponto médio da corda
										pt_cen (polar pm (+ (angle v1 v2) (* (if (minusp ft_v1) 1.5 0.5) PI)) ca)				; centro do arco
										an_ini (angle pt_cen v1)																												; angulo inicial do arco
										an_fim (angle pt_cen v2)																												; angulo final do arco
										cp_arc (* (/ (* 2 an_arc 180.0) PI) (/ (* 2 PI ra_arc) 360))	; comprimento total do arco
										cp_inc (* an_div (/ (* 2 PI ra_arc) 360))											; comprimento do incremento do arco
							)
							(setq an_bas (* (/ PI 180) an_div)															; valor do ângulo base em radianos
										an_pto an_ini																					; inicializa ângulo do ponto 
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


(defun gera_lista_posicao_base()
 
	(setq ARQUIVO_PATH (getfiled "Selecione o arquivo CSV" "c:" "csv" 0))
	(setq ARQUIVO_CSV (open ARQUIVO_PATH "r"))

	(setq contador1 0)
	(if (/= ARQUIVO_CSV nil)
		(progn
			(setq 
				 LINHA_CSV (read-line ARQUIVO_CSV)
				 LISTA_LINHA nil
				 LISTA_CSV nil
			)
			
			;Percorrendo o csv do arquivo base
			(while (/= LINHA_CSV nil)

				(setq LISTA_LINHA (sparser LINHA_CSV ";"))
				
				;(assoc "700281.0317529290.421" lista_informacoes)
				(setq x (atof (nth 0 LISTA_LINHA)))
				(setq y (atof (nth 1 LISTA_LINHA)))
				
				
				(command "layer" "m" "ponto_base" "c" "green" "" "")
				(command "circle" (list x y 0) 0.1)
				(setq objCirculo (entlast))
				(CriarLink objCirculo "nome_bloco" (nth 3 LISTA_LINHA))
				(CriarLink objCirculo "valor" (nth 2 LISTA_LINHA))
				
				
				
				(setq LINHA_CSV (read-line ARQUIVO_CSV))
				(setq LISTA_LINHA nil)
				
			)
			
			(close ARQUIVO_CSV)
			(setq ARQUIVO_CSV nil)
		)
	)
)

(defun exclui_ponto_base()
	(setq sel (ssget "x" '((-4 . "<AND") (8 . "ponto_base")(0 . "CIRCLE")(-4 . "AND>"))))
	(command "erase" sel "")
)

(defun exclui_layer_descessarias()
	(setq sel (ssget "x" '((-4 . "<AND") (8 . "layer_temporaria1")(-4 . "AND>"))))
	(command "erase" sel "")
)

(defun excluir_layer_clean()
	(setq sel (ssget "x" '((-4 . "<AND") (8 . "Erro_valores_incoerentes,Erro_valores_incoerentes_DC,Divergencia2,ponto_base,Nao_Configurado,Configurado_Ok,Divergencia_Valores_incopativeis,Nomes_dos_blocos_incopativeis")(-4 . "AND>"))))
	(sam_delete sel)
)

(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")


;lbbb = lista de objetos da base
;lmmm = lista de objeto do mapa atual
(defun verifica_qt_atualizacoes(lbbb lmmm / achou11 tam tam2 qtdAlteration objB valorBase blocoBase key1  key2 blocoNameMap objMApA
valorMapa)
	(setq qtdAlteration 0)
	(setq tam (length lbbb))
	(while (> tam 0)
		(setq objB (nth (- tam 1) lbbb))
		(setq valorBase (GetId objB "valor"))
		(setq blocoBase (GetId objB "nome_bloco"))
		(setq key1 (strcat valorBase blocoBase))

		;Percorrer os objetos do mapa e ver se tem alguem igual a ele (numero + tipo do bloco)
		(setq tam2 (length lmmm))
		(setq achou11 0)
		(while (> tam2 0)
			(setq objMApA (nth (- tam2 1) lmmm))
			
			;pegar informações do objeto do mapa
			(setq blocoNameMap (strcase (cdr (assoc 2 (entget objMApA)))))
			(setq valorMapa (strcase (retorna_attrib objMApA 1)))
			(setq key2 (strcat valorMapa blocoNameMap))
			
			(if (= key1 key2)
				(progn
					(setq lmmm (vl-remove (nth (- tam2 1) lmmm) lmmm))
					(setq achou11 1)
				)
				(progn
					
					
				)
			)
			
			
			;(setq lista1 (list 1 2 3 4 5 6 7 8))
			;(vl-remove (nth 7 lista1) lista1)
			
			
			(setq tam2 (- tam2 1))
		)
		
		(if (= achou11 0)
			(progn
				(setq qtdAlteration (+ qtdAlteration 1))
			)
		)
		
		(setq tam (- tam 1))
	)
	qtdAlteration
)


(defun excluir_elemento_lista_quando_necessario_BASE_DC(lista_base primeiraLista)
	(setq lista_retorno lista_base )
	(setq tam4 (length lista_base))
	(setq encontrou 0)
	(setq sair1 0)
	(while(and (= sair1 0) (> tam4 0))
		(setq el_m (nth (- tam4 1) lista_base))
		
		(setq valor3 (GetId el_m "valor"))
		(setq nome_bloco3 (GetId el_m "nome_bloco"))
		
		(setq ProcurarBloco (nth 0 primeiraLista))
		;(setq ProcurarValor (nth 1 primeiraLista))
		
		(if  (=   ProcurarBloco   (strcat nome_bloco3 valor3) )
			(progn
				(setq encontrou 1)
				
				(setq lista_retorno (vl-remove (nth (- tam4 1) lista_base) lista_base))
				(setq sair1 1)
			)
		)
		
		(setq tam4 (- tam4 1))
	)
	lista_retorno
)

(defun excluir_elemento_lista_quando_necessario_BASE(lista_base primeiraLista)
	(setq lista_retorno lista_base )
	(setq tam4 (length lista_base))
	(setq encontrou 0)
	(setq sair1 0)
	(while(and (= sair1 0) (> tam4 0))
		(setq el_m (nth (- tam4 1) lista_base))
		
		(setq valor3 (GetId el_m "valor"))
		(setq nome_bloco3 (GetId el_m "nome_bloco"))
		
		(setq ProcurarBloco (strcat "TAP"(nth 0 primeiraLista)))
		(setq ProcurarValor (nth 1 primeiraLista))
		
		(if (and (= ProcurarValor valor3) (= ProcurarBloco nome_bloco3) )
			(progn
				(setq encontrou 1)
				
				(setq lista_retorno (vl-remove (nth (- tam4 1) lista_base) lista_base))
				(setq sair1 1)
			)
		)
		
		(setq tam4 (- tam4 1))
	)
	lista_retorno
)




(defun excluir_elemento_lista_quando_necessario_MAPA_DC(lista_base primeiraLista)
	(setq lista_retorno lista_base )
	(setq tam4 (length lista_base))
	(setq encontrou 0)
	(setq sair1 0)
	(while(and (= sair1 0) (> tam4 0))
		(setq el_m (nth (- tam4 1) lista_base))
		
		(setq valor3 (retorna_attrib el_m 1))
		(if (= valor3 nil)
			(progn
				(setq valor3 "")
			)
		)
		(setq nome_bloco3 (strcase (cdr (assoc 2 (entget el_m)))))
		
		(setq ProcurarBloco (nth 0 primeiraLista))
		
		
		(if (= ProcurarBloco (strcat nome_bloco3 valor3))
			(progn
				(setq encontrou 1)
				
				(setq lista_retorno (vl-remove (nth (- tam4 1) lista_base) lista_base))
				(setq sair1 1)
			)
		)
		
		(setq tam4 (- tam4 1))
	)
	lista_retorno
)


(defun excluir_elemento_lista_quando_necessario_MAPA(lista_base primeiraLista)
	(setq lista_retorno lista_base )
	(setq tam4 (length lista_base))
	(setq encontrou 0)
	(setq sair1 0)
	(while(and (= sair1 0) (> tam4 0))
		(setq el_m (nth (- tam4 1) lista_base))
		
		(setq valor3 (retorna_attrib el_m 1))
		(setq nome_bloco3 (strcase (cdr (assoc 2 (entget el_m)))))
		
		(setq ProcurarBloco (strcat "TAP"(nth 0 primeiraLista)))
		(setq ProcurarValor (nth 1 primeiraLista))
		
		(if (and (= ProcurarValor valor3) (= ProcurarBloco nome_bloco3) )
			(progn
				(setq encontrou 1)
				
				(setq lista_retorno (vl-remove (nth (- tam4 1) lista_base) lista_base))
				(setq sair1 1)
			)
		)
		
		(setq tam4 (- tam4 1))
	)
	lista_retorno
)


(defun carrega_lista_layer1( / all qtd obj layerName coord  x1 y1  )
	(setq lista_layer1 nil)
	
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "Nao_Configurado,Nomes_dos_blocos_incopativeis,Divergencia_Valores_incopativeis")(0 . "CIRCLE")(-4 . "AND>"))))
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
				
				(setq lista_layer1 (cons  (list (strcat x1 y1)   obj) lista_layer1))
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
)


(defun apagar_layer_anteriores(lllll3)
	(carrega_lista_layer1)
	
	(setq tamll (length lllll3))
	
	(while (> tamll 0)
		(setq elmm (nth (- tamll 1)  lllll3   )   )
		(setq coordsfd (cdr (assoc 10 (entget elmm))))
		(setq x1123 (rtos (car coordsfd) 2 3))
		(setq y1231 (rtos (cadr coordsfd) 2 3))
		
		
		(setq procuraELL "")
		(while (/= procuraELL nil)
			(setq procuraELL (assoc (strcat x1123 y1231 )   lista_layer1 ))
			(if (/= procuraELL nil)
				(progn
					(sam_delete (nth 1 procuraELL))
					(setq lista_layer1 (vl-remove (assoc (strcat x1123 y1231 )   lista_layer1 ) lista_layer1))
					
				)
			)
		)
		
		
		
		(setq tamll (- tamll 1))
	)
	
)

;Quando exitir muitos objetos(tap,dc) dentro da nuvem
(defun configura_muitos_taps()


	(if (/= lista_muitos_taps nil)
		(progn
			
			(setq quantidade_el_lista (length lista_muitos_taps))
			(while (> quantidade_el_lista 0)
				
				(setq informacoes1 (nth (- quantidade_el_lista 1) lista_muitos_taps))
				
				(setq quatidade_taps_mapa (nth 0 informacoes1) )
				(setq quatidade_taps_BASE (nth 1 informacoes1) )
				
				(setq lista_obj_ponto_mapa (nth 2 informacoes1))
				(setq lista_obj_ponto_base (nth 3 informacoes1))
				(setq objeto_informacoes (nth 4 informacoes1))
				(setq lista_obj_ponto_base_dc (nth 5 informacoes1))
				(setq lista_obj_ponto_mapa_dc (nth 6 informacoes1))
				
				
				;Apagar layers feitas anteriormente, pois podem estar erradas
				;visto que as funções anteriores não tratam nuvens, com vários taps
				(apagar_layer_anteriores  lista_obj_ponto_base)
				(apagar_layer_anteriores  lista_obj_ponto_base_dc)
				
				
				(setq qtdTapExcluidos (conta_qtd_taps_excluidos objeto_informacoes))
				
				(if (< (- quatidade_taps_BASE qtdTapExcluidos) 0)
					(progn
						;(princ "\nErro -----")
						;(parar)
					)
				)
				
				;Percorrendo bloco das informações e comparando com as informações da base e do mapa
				(setq contadorAtributos 1)
				(setq qtdExcluidosInformacoes 0)
				(while ( /=  (retorna_attrib objeto_informacoes contadorAtributos) nil )
					
					(setq valA  (retorna_attrib objeto_informacoes contadorAtributos))
					(setq valA (vl-string-trim " "	(strcase valA)))
					
					
					(if (= valA "ACOPLADOR")
						(progn
							(setq primeiro  (vl-string-trim " "	(strcase (retorna_attrib objeto_informacoes (+ contadorAtributos 1)))))
							(setq segundo  (vl-string-trim " "	(strcase (retorna_attrib objeto_informacoes (+ contadorAtributos 2)))))
							(setq acao (define_a_acao_do_tap primeiro segundo))
							
							(if (= acao "atualizacao")
								(progn
									(setq primeiraLista (sparser primeiro "/")) ;Mapa base
									(setq segundoLista (sparser segundo "/")) ;Mapa atual
									
									
									(if (or  (= lista_obj_ponto_base_dc nil)  (= lista_obj_ponto_mapa_dc nil)  )
										(progn
											(command "layer" "m" "Erro_valores_incoerentes_DC" "c" "red" "" "")
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
										)
									)
									
									(setq lista_obj_ponto_base_dc (excluir_elemento_lista_quando_necessario_BASE_DC lista_obj_ponto_base_dc primeiraLista))
									(setq lista_obj_ponto_mapa_dc (excluir_elemento_lista_quando_necessario_MAPA_DC lista_obj_ponto_mapa_dc segundoLista))
									
								)
							)
							(if (= acao "adicionar")
								(progn
									(setq segundoLista (sparser segundo "/")) ;Mapa atual
									(setq lista_obj_ponto_mapa_dc (excluir_elemento_lista_quando_necessario_MAPA_DC lista_obj_ponto_mapa_dc segundoLista))
								)
							)
							(if (= acao "exclusao")
								(progn
									(setq primeiraLista (sparser primeiro "/")) ;Mapa base
									(setq lista_obj_ponto_base_dc (excluir_elemento_lista_quando_necessario_BASE_DC lista_obj_ponto_base_dc primeiraLista))
								)
							)
							
						)
					)
					
					(if (= valA "TAP")
						(progn
							(setq primeiro  (vl-string-trim " "	(strcase (retorna_attrib objeto_informacoes (+ contadorAtributos 1)))))
							(setq segundo  (vl-string-trim " "	(strcase (retorna_attrib objeto_informacoes (+ contadorAtributos 2)))))
							
							(setq acao (define_a_acao_do_tap primeiro segundo))
							
							(if (= acao "atualizacao")
								(progn
									(setq primeiraLista (sparser primeiro "/")) ;Mapa base
									(setq segundoLista (sparser segundo "/")) ;Mapa atual
									
									
									(if (or  (= lista_obj_ponto_base nil)  (= lista_obj_ponto_mapa nil)  )
										(progn
											;(command "layer" "m" "Erro_valores_incoerentes" "c" "red" "" "")
											(command "layer" "m" "Erro_valores_incoerentes" "c" "red" "" "")
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
											(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
										)
									)
									(setq lista_obj_ponto_base (excluir_elemento_lista_quando_necessario_BASE lista_obj_ponto_base primeiraLista))
									(setq lista_obj_ponto_mapa (excluir_elemento_lista_quando_necessario_MAPA lista_obj_ponto_mapa segundoLista))
									
								)
							)
							
							(if (= acao "adicionar")
								(progn
									(setq segundoLista (sparser segundo "/")) ;Mapa atual
									(setq lista_obj_ponto_mapa (excluir_elemento_lista_quando_necessario_MAPA lista_obj_ponto_mapa segundoLista))
								)
							)
							
							(if (= acao "exclusao")
								(progn
									(setq primeiraLista (sparser primeiro "/")) ;Mapa base
									(setq lista_obj_ponto_base (excluir_elemento_lista_quando_necessario_BASE lista_obj_ponto_base primeiraLista))
								)
							)
							
							
							(if (and (/= segundo "0") (/= segundo ""))
								(progn
									
								)
								(progn
									;(command "layer" "m" "Erro_valor_do_tap_vazio" "c" "red" "" "")
									;(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
									;(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
									;(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
									;(parar)
								)
							)
							
							
							
						)
					)
					
					(setq contadorAtributos (+ contadorAtributos 1))
				)
				; ====  Trata a diferença das informações que sobram nas listas dos DC(s) - i
				(setq tamLista1 (length lista_obj_ponto_base_dc))
				(setq tamLista2 (length lista_obj_ponto_mapa_dc))
				
				
				;(parar)
				(if (/= tamLista1 tamLista2)
					(progn
						(command "layer" "m" "Erro_valores_incoerentes_DC" "c" "red" "" "")
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
			
					)
					(progn
						
						(if (/= tamLista1 0)
							(progn
								(setq tamanho5 tamLista1)
								(while (> tamanho5 0)
									
									(setq elm1 (nth (- tamanho5 1) lista_obj_ponto_base_dc )  )
									(setq elm2 (nth (- tamanho5 1) lista_obj_ponto_mapa_dc )  )
									
									
									(setq info1 (getid elm1 "nome_bloco")) ;valores da base
									(setq info2 (getid elm1 "valor"))		;valores da base
									
									
									(setq info3 (retorna_attrib elm2 1)) ;valores do bloco no mapa atual
									(if (= info3 nil)
										(progn
											(setq info3 "")
										)
									)
									(setq info4 (strcase (cdr (assoc 2 (entget elm2))))) ;valores do bloco no mapa atual
									
									(if (= tamLista1 1)
										(progn
											;(princ (strcat  info1 "/" info4 "---" info2 "/" info3))
											
											(if (/=  (strcat info1 info2)  (strcat info4 info3)  )
												(progn
													(command "layer" "m" "Erro_valores_incoerentes_DC" "c" "red" "" "")
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
												)
											)
										)
									)
									
									(if (> tamLista1 1)
										(progn
										
											(setq lista_retorno (verifica_igualdade_nas_listas_DC lista_obj_ponto_base_dc lista_obj_ponto_mapa_dc))
											(if (/= lista_retorno nil)
												(progn
													(command "layer" "m" "Erro_valores_incoerentes_DC" "c" "blue" "" "")
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
												)
											)
										)
									)
									
									
									(setq tamanho5 (- tamanho5 1))
								)
							)
						)
						
					)
				)
				
				
				
				
				; ====  Trata a diferença das informações que sobram nas listas dos DC(s) - f
				
				; ====  Trata a diferença das informações que sobram nas listas dos taps - i
				(setq tamLista1 (length lista_obj_ponto_base))
				(setq tamLista2 (length lista_obj_ponto_mapa))
				
				
				(if (/= tamLista1 tamLista2)
					(progn
						;(princ "\nErro -----")
						;(parar)
						
						(command "layer" "m" "Erro_valores_incoerentes" "c" "red" "" "")
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
						(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
			
					)
					(progn
						
						(if (/= tamLista1 0)
							(progn
								(setq tamanho5 tamLista1)
								(while (> tamanho5 0)
									
									(setq elm1 (nth (- tamanho5 1) lista_obj_ponto_base )  )
									(setq elm2 (nth (- tamanho5 1) lista_obj_ponto_mapa )  )
									
									
									(setq info1 (getid elm1 "nome_bloco"))
									(setq info2 (getid elm1 "valor"))
									(setq info3 (retorna_attrib elm2 1))
									(setq info4 (strcase (cdr (assoc 2 (entget elm2)))))
									
									(if (= tamLista1 1)
										(progn
											;(princ (strcat  info1 "/" info4 "---" info2 "/" info3))
											
											(if (and (/= info1 info4)  (/= info2 info3)  )
												(progn
													(command "layer" "m" "Erro_valores_incoerentes" "c" "red" "" "")
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
												)
											)
										)
									)
									
									(if (> tamLista1 1)
										(progn
											
											(setq lista_retorno (verifica_igualdade_nas_listas lista_obj_ponto_base lista_obj_ponto_mapa))
											(if (/= lista_retorno nil)
												(progn
													(command "layer" "m" "Erro_valores_incoerentes" "c" "red" "" "")
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 8.5)
													(command "circle" (cdr (assoc 10 (entget objeto_informacoes))) 9)
												)
											)
											
											;(parar)
											
											;(sam_zoom (cdr (assoc 10 (entget objeto_informacoes))) 10)
											;(parar)
										)
									)
									
									
									(setq tamanho5 (- tamanho5 1))
								)
							)
						)
						
						
					)
				)
				
				; ====  Trata a diferença das informações que sobram nas listas dos taps - f
				
				
				
				
				;=========== Ex =====================
				;(setq quantidadeAtualizacoes (verifica_qt_atualizacoes lista_obj_ponto_base lista_obj_ponto_mapa))
				;Veriifca se no objeto Informações tem a quantidade correta de infomrações
				;(setq resposta1 (verifica_alteracoes_quantidade objeto_informacoes quantidadeAtualizacoes objeto_informacoes))
				;(setq resp1 (verifica_quantidade_tap_informacoes objeto_informacoes quatidade_taps_mapa))
				
				
				
				(setq quantidade_el_lista (- quantidade_el_lista 1))
			)
			
		)
		
	)
)



(defun verifica_igualdade_nas_listas_DC(l1 l2)

	
	(setq tam10 (length l1))
	
	(while (> tam10 0)
		
		(setq elem1 (nth (- tam10 1) l1))
		(setq base1_info_bloco (getid elem1 "nome_bloco"))
		(setq base1_info_valor (getid elem1 "valor"))
		
		(setq tamL2 (length l2))
		(while (> tamL2 0)
			(setq l2Elmm (nth (- tamL2 1) l2))
			(setq info_mapa_valor (retorna_attrib l2Elmm 1))
			(if (= info_mapa_valor nil)
				(progn
					(setq info_mapa_valor "")
				)
			)
			(setq info_mapa_bloco (strcase (cdr (assoc 2 (entget l2Elmm)))))
			
			(if (= (strcat base1_info_bloco base1_info_valor ) (strcat info_mapa_bloco info_mapa_valor ) )
				(progn
					
					(setq l2 (vl-remove (nth (- tamL2 1) l2) l2))
					
				)
			)
			
			
			(setq tamL2 (- tamL2 1))
		)
		;(parar)
		
		(setq tam10 (- tam10 1))
	)
	
	;(princ "\n==============================================")
	;(princ (strcat "\n" (rtos (length l2) 2 2)  ))
	
	l2
)


;base x mapa
(defun verifica_igualdade_nas_listas(l1 l2)
	(setq tam10 (length l1))
	
	(while (> tam10 0)
		
		(setq elem1 (nth (- tam10 1) l1))
		(setq base1_info_bloco (getid elem1 "nome_bloco"))
		(setq base1_info_valor (getid elem1 "valor"))
		
		(setq tamL2 (length l2))
		(while (> tamL2 0)
			(setq l2Elmm (nth (- tamL2 1) l2))
			(setq info_mapa_valor (retorna_attrib l2Elmm 1))
			(setq info_mapa_bloco (strcase (cdr (assoc 2 (entget l2Elmm)))))
			
			(if (and   (= info_mapa_bloco base1_info_bloco)  (= base1_info_valor info_mapa_valor)  )
				(progn
					
					(setq l2 (vl-remove (nth (- tamL2 1) l2) l2))
					
				)
			)
			
			
			(setq tamL2 (- tamL2 1))
		)
		;(parar)
		
		(setq tam10 (- tam10 1))
	)
	
	;(princ "\n==============================================")
	;(princ (strcat "\n" (rtos (length l2) 2 2)  ))
	
	l2
)

;A ação do tap pode ser de atualizacao, de exclusao, ou de adição
(defun define_a_acao_do_tap(primeiro segundo / acao)
	(setq acao "atualizacao")
	(if (or  (= primeiro "-") (= primeiro "0")) 
		(progn
			(setq acao "adicionar")
		)
	)
	(if (or (= segundo "0")(= segundo "-"))
		(progn
			(setq acao "exclusao")
		)
	)
	
	acao
)


;Essa funcao retorna 0 ou 1. 1 está ok, 0 está errado
(defun verifica_alteracoes_quantidade (obj1 qtd1 objInformacoes / qtdTapEncontrados contador2 atributoFind  retornoFuncao )
	(setq qtdTapEncontrados 0)
	(setq contador2 1)
	(setq atributoFind (retorna_attrib obj1 contador2) )
	(if (/= atributoFind nil)
		(progn
			(setq atributoFind (vl-string-trim " "	(strcase atributoFind)))
		)
	)
	(while (/= atributoFind nil)
		
		(if (= atributoFind "TAP")
			(progn
				(setq qtdTapEncontrados (+ qtdTapEncontrados 1))
			)
		)
		
		
		(setq contador2 (+ contador2 1))
		(setq atributoFind (retorna_attrib objInformacoes contador2) )
		(if (/= atributoFind nil)
			(progn
				(setq atributoFind (vl-string-trim " "	(strcase atributoFind)))
			)
		)
	)
	
	(if (/= qtdTapEncontrados qtd1)
		(progn
			(setq retornoFuncao 0)
		)
		(progn
			(setq retornoFuncao 1)
		)
	)
	
	retornoFuncao
)



(defun c:pp()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(setq ponto_tela_inicial (ViewExtents))
	
	(excluir_layer_clean)
	(exclui_ponto_base)
	(gera_lista_posicao_base)
	(relaciona_leaders)
	(carrega_lista_posicao_tap)
	(carrega_arquivo)
	
	(configura_muitos_taps)
	(exclui_layer_descessarias)
	
	(princ "\n==========    LAYER GERADAS    ===============")
	(princ "\nDivergencia2")
	(princ "\nConfigurado_Ok")
	(princ "\nDivergencia_Valores_incopativeis")
	(princ "\nNomes_dos_blocos_incopativeis")
	(princ "\nNao_Configurado")
	(princ "\nFim...")
	
	(command "zoom" "w"  (nth 0 ponto_tela_inicial) (nth 1 ponto_tela_inicial))
	
	(princ)
)