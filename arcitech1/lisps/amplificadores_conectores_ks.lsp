
(defun procura_head1(coord33)
	(setq PONTO1111 (polar coord33 (/ pi 4) 50))
	(setq PONTO2222 (polar coord33 (* (/ pi 4) 5) 50))
	(command "zoom" "w" PONTO1111 PONTO2222)
	;(setq JANELA (ssget "C" PONTO1111 PONTO2222 (list (cons 8 "NET_UPGRADE"))))
	
	
	(setq JANELA (ssget "C" PONTO1111 PONTO2222   (List (cons -4 "<AND") (cons 0 "LWPOLYLINE")   (cons 8 "NET_UPGRADE")  (cons -4 "AND>")  )))
	
	(setq meus_objetos nil)
	(if (/= JANELA nil)
		(progn
			(setq qtd2 (- (sslength JANELA) 1))
			(while (>= qtd2 0)
				(setq obj2 (ssname JANELA qtd2))
				
				(setq listaObjetoPolyline (pontos_polyline obj2  ))
				(command "zoom" "c" (nth 0 listaObjetoPolyline) 50)	
				(setq todos_objs (ssget "WP"  listaObjetoPolyline))
				(if (= todos_objs nil)
					(progn
						(setq todos_objs (ssget "WP"  listaPontoPolyline2))
					)
				)
				
				(if (/= todos_objs nil)
					(progn
						(setq qtd3 (- (sslength todos_objs) 1))
						(setq sair 0)
						(while (   and ( >= qtd3 0 )  (= sair 0)   )
							
							(setq obj3 (ssname todos_objs qtd3))
							(setq tipoObj3 (strcase (cdr (assoc 0 (entget obj3)))))
							(if (= tipoObj3 "INSERT")
								(progn
									(setq block3Name (strcase (cdr (assoc 2 (entget obj3)))))
									(if (or (= block3Name "AMP3") (= block3Name "LE") (= block3Name "AMP3AC"))
										(progn
											(setq coord3  (cdr (assoc 10 (entget obj3))))
											(setq x3 (rtos (car coord3) 2 3))
											(setq y3 (rtos (cadr coord3) 2 3))
											
											(if (= (strcat x3 y3) (strcat x1111 y1111))
												(progn
													(setq meus_objetos todos_objs)
													(setq sair 1)
												)
											)
											
											
										)
									)
									
									
									
								)
							)
							
							(setq qtd3 (- qtd3 1))
						)
					)
				)
				
				
				
				(setq qtd2 (- qtd2 1))
			)
		)
	)
	
	(if (/= meus_objetos nil)
		(progn
			
		)
		(progn
			;(command "zoom" "c" coord33 10)
			;(alert "Amplificador não encontrado")
			;(parar)
		)
	)
	
	meus_objetos
	
)

(defun percorre_elemetnos()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 2 "AMP3,"))))
	(setq all (ssget "x" '((-4 . "<AND") (2 . "AMP3,LE,AMP3AC")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1111 (rtos (car coord) 2 3))
				(setq y1111 (rtos (cadr coord) 2 3))
				
				(command "layer" "m" "amplificador_conferido" "c" "green" "" "")
				(command "circle" coord 17)
				(command "circle" coord 14.5)
				
				(command "zoom" "c" coord 100)
				(setq meus_objetos2 (procura_head1 coord))
				(if (= meus_objetos2 nil)
					(progn
						(command "layer" "m" "amplificador_erro" "c" "red" "" "")
						(command "circle" coord 18)
						(command "circle" coord 20)
						(setq logerro 1)
					)
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
)


(defun percorre_elemetnos2()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 2 "AMP3,"))))
	(setq all (ssget "x" '((-4 . "<AND") (2 . "AMP3,LE,AMP3AC")(0 . "INSERT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(setq lista_contagem nil)
	(setq pin500 0)
	(setq ks 0)
	(setq todos_cabos_mapa nil)
	(setq lista_controle nil)
	
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1111 (rtos (car coord) 2 3))
				(setq y1111 (rtos (cadr coord) 2 3))
				
				(command "layer" "m" "amplificador_conferido" "c" "green" "" "")
				(command "circle" coord 17)
				(command "circle" coord 14.5)
				
				(command "zoom" "c" coord 100)
				(setq meus_objetos2 (procura_head1 coord))
				(if (/= meus_objetos2 nil)
					(progn
						(setq qtd4 (- (sslength meus_objetos2) 1))
						(setq lista_log1 "")
						(while (>= qtd4 0)
							(setq obj4 (ssname meus_objetos2 qtd4))
							(setq tipoObj4 (strcase (cdr (assoc 0 (entget obj4)))))
							(setq layerName1 (strcase (cdr (assoc 8 (entget obj4)))))
							;(princ (strcat "\n" layerName1))
							
							(if (and  (/= layerName1 "ELETRONICO")   (= tipoObj4 "INSERT")  )
								(progn
									(setq block4Name (strcase (cdr (assoc 2 (entget obj4)))))
									
									(command "layer" "m" "linha_perimetro" "c" "yellow" "" "")
									(setq lista_cabos (procura_redes_connect obj4))
									
									(if (/= (vl-string-search "LPI" block4Name) nil)
										(progn
											;Estaremos tratando as fontes
											(setq ks (+ ks 1))
											(setq lista_log1 (strcat lista_log1 " ### LPI(fonte) "))
										)
									)
									(if (= block4Name "LE")
										(progn
											(setq pin500 (+ pin500 2))
											(setq lista_log1 (strcat lista_log1 " ### LE "))
										)
									)
									(if (= block4Name "AMP3")
										(progn
											(setq pin500 (+ pin500 4))
											(setq lista_log1 (strcat lista_log1 " ### AMP3 "))
										)
									)
									(if (= block4Name "AMP3AC")
										(progn
											(setq pin500 (+ pin500 4))
											(setq lista_log1 (strcat lista_log1 " ### AMP3AC "))
										)
									)
									
									(if (/= (vl-string-search "TAP" block4Name) nil)
										(progn
											(setq ks (+ ks 1))
											(setq lista_log1 (strcat lista_log1 " ### TAP "))
										)
									)
									(if (/= (vl-string-search "DC" block4Name) nil)
										(progn
											(setq pin500 (+ pin500 1))
											(setq ks (+ ks 1))
											(setq lista_log1 (strcat lista_log1 " ### DC "))
										)
									)
									(if (/= (vl-string-search "2WAY" block4Name) nil)
										(progn
											(setq pin500 (+ pin500 1))
											(setq ks (+ ks 1))
											(setq lista_log1 (strcat lista_log1 " ### 2WAY "))
										)
									)
									(if (/= (vl-string-search "3WAY" block4Name) nil)
										(progn
											(setq pin500 (+ pin500 2))
											(setq ks (+ ks 1))
											(setq lista_log1 (strcat lista_log1 " ### 3WAY "))
										)
									)
									
									
									;lista_controle
									
									(setq tam3 (length lista_cabos))
									(while (> tam3 0)
										;(setq elm (nth (- tam3 1) lista_cabos))
										(setq tam4 (length (nth (- tam3 1) lista_cabos)))
										
										(while (> tam4 0)
											
											(setq elm (nth (- tam4 1) (nth (- tam3 1) lista_cabos)))
											(setq coord (nth 1 elm))
											(setq x1 (rtos (car coord) 2 3))
											(setq y1 (rtos (cadr coord) 2 3))
											(setq chave (strcat x1 y1))
											(setq pesquisa (assoc chave lista_controle))
											(if (= pesquisa nil)
												(progn
													(setq oldlayer (getvar "CLAYER")) ; get current layer
													(command "layer" "m" "samuel_ok" "c" "cyan" "" "")
													(command "circle" coord 1.3)
													(command "layer" "m" oldlayer "c" "yellow" "" "")
													
													(setq todos_cabos_mapa (cons elm todos_cabos_mapa))
													(setq lista_controle (cons (list chave) lista_controle) )
												)
											)
											
											(setq tam4 (- tam4 1))
										)
										
										
										(setq tam3 (- tam3 1))
										
									)
									
									
								)
							)
							(setq qtd4 (- qtd4 1))
						)
					)
				)
				(setq qtd (- qtd 1))
			)
		)
	)
	
	;todos_cabos_mapa
	;pin500
	;ks
)


(defun verifica_se_rede_existe(pnovo / PONTO1 PONTO2 JANELA)
	
	(command "zoom" "c" pnovo 1)
	(setq PONTO1 (polar pnovo (/ pi 4) 0.2))
	(setq PONTO2 (polar pnovo (* (/ pi 4) 5) 0.2))

	(command "zoom" "w" PONTO1 PONTO2)
	(setq JANELA (ssget "C" PONTO1 PONTO2 (list (cons -4 "<AND") (cons 0 "LWPOLYLINE")   (cons 8 "NET-CBTR,NET-CBTP")  (cons -4 "AND>")  )))
	
	JANELA
)


;Pega pontos finais da polyline
(defun percorre_pontos_polyline(obj  / el gr nr xv yv contador pontoinicial pontofinal)
	(setq el (entget obj) gr (car el) )
	(setq contador 1)
	(while gr
		(setq nr (car gr))
		(if (= nr 10)
			(progn
				(setq xv (cadr gr)
						yv (caddr gr)
						zv (cadddr gr)
				)
				
				(if (= contador 1)
					(progn
						(setq pontoinicial (list xv yv 0 ))
					)
				)
				
				(setq pontofinal (list xv yv 0 ))
				
				(setq contador (+ contador 1))
			)
		)
		(setq el (cdr el)
			  gr (car el)
		)
		
	)
	
	(list pontoinicial pontofinal )
)

(defun procura_por_rede_verde(p1 p2 / ang1 distancia1 distancia_atual fatorx pnovo 
 objRede qtd obj pontoext distanciaDosPontos1 distanciaDosPontos2 listaRedesConectadas)
	
	(setq ang1 (angle p1 p2))
	(setq distancia1 (distance p1 p2))
	(setq distancia_atual 0)
	(setq fatorx 0)
	(setq listaRedesConectadas nil)
	(while (<= distancia_atual distancia1 )
		
		(setq pnovo (polar p1  ang1  (* 0.05 fatorx) ))
		(command "zoom" "c" pnovo 1)
		
		(setq objRede (verifica_se_rede_existe pnovo))
		(if objRede
			(progn
				(setq qtd (- (sslength objRede) 1))
				(while (>= qtd 0)
					(setq obj (ssname objRede qtd))
					(setq pontoext (percorre_pontos_polyline obj))
					
					(setq distanciaDosPontos1 (distance pnovo (nth 0 pontoext) ))
					(setq distanciaDosPontos2 (distance pnovo (nth 1 pontoext) ))
					
					(if (or (< distanciaDosPontos1 0.2)  (< distanciaDosPontos2 0.2) )
						(progn
							(setq pprocura (assoc obj listaRedesConectadas))
							(if (= pprocura nil)
								(progn
									
									(setq varEx nil)
									(if (< distanciaDosPontos1 0.2)
										(progn
											(setq varEx (nth 0 pontoext))
										)
									)
									(if (< distanciaDosPontos2 0.2)
										(progn
											(setq varEx (nth 1 pontoext))
										)
									)
									
									(setq listaRedesConectadas (cons (list obj varEx) listaRedesConectadas))
								)
							)
							
						)
					)
					
					
					(setq qtd (- qtd 1))
				)
			
			
			)
		)
		
		(setq fatorx (+ fatorx 1))
		(setq distancia_atual (distance p1 pnovo))
	)
	
	listaRedesConectadas
)

(defun verifica_ponto_tri(pontoTriangulo obj / PONTO1 PONTO2 blockName all qtd achou obj2 lista_ver)
	(setq PONTO1 (polar pontoTriangulo (/ pi 4) 0.08))
	(setq PONTO2 (polar pontoTriangulo (* (/ pi 4) 5) 0.08))
	(setq blockName  (cdr (assoc 2 (entget obj))))
	
	(command "zoom" "w" PONTO1 PONTO2)
	(setq all (ssget "C" PONTO1 PONTO2 (List (cons -4 "<AND") (cons 0 "INSERT")   (cons 2 blockName)  (cons -4 "AND>")  )))
	(setq achou 0)
	(setq lista_ver nil)
	(setq lista_ver (cons (list obj) lista_ver))
	
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj2 (ssname all qtd))
				(if (/= (assoc obj2 lista_ver) nil)
					(progn
						(setq achou 1)
					)
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
	achou
)


(defun procura_em_circulo (meio1 raio / contagemAng pnew qtd objRede pontoext distanciaDosPontos1
 distanciaDosPontos2 pprocura obj listaRedesConectadas pontoExt2)
	;percorre circulo
	(setq contagemAng 0)
	(setq listaRedesConectadas nil)
	(while (<= contagemAng 360)
		(setq pnew (polar meio1 (/ (* pi contagemAng) 180) raio))
		(command "zoom" "c" pnew 3)
		(command "circle" pnew 0.03)
		
		(setq objRede (verifica_se_rede_existe pnew))
		(if objRede
			(progn
			
				(setq qtd (- (sslength objRede) 1))
				(while (>= qtd 0)
					(setq obj (ssname objRede qtd))
					(setq pontoext (percorre_pontos_polyline obj))
					
					(setq distanciaDosPontos1 (distance pnew (nth 0 pontoext) ))
					(setq distanciaDosPontos2 (distance pnew (nth 1 pontoext) ))
					
					(if (or (< distanciaDosPontos1 0.2)  (< distanciaDosPontos2 0.2) )
						(progn
							(setq pprocura (assoc obj listaRedesConectadas))
							(if (= pprocura nil)
								(progn
								
									(setq pontoExt2 nil)
									(if (< distanciaDosPontos1 0.4)
										(progn
											(setq pontoExt2 (nth 0 pontoext))
										)
									)
									(if (< distanciaDosPontos2 0.4)
										(progn
											(setq pontoExt2 (nth 1 pontoext))
										)
									)
									
									(setq listaRedesConectadas (cons (list obj pontoExt2) listaRedesConectadas))
								)
							)
						)
					)
					(setq qtd (- qtd 1))
				)
			)
		)
		
		
		(setq contagemAng (+ contagemAng 1))
	)
	listaRedesConectadas
)

(defun marca_encontrado (l1)
	(setq tam (length l1))
	(while (> tam 0)
		(command "circle" (nth 1 (nth (- tam 1) l1) ) 0.3)
		
		(setq tam (- tam 1))
	)
)

(defun verifica_se_exist_ampl(p / PONTO1 PONTO2 all)
	(setq PONTO1 (polar p (/ pi 4) 0.1))
	(setq PONTO2 (polar p (* (/ pi 4) 5) 0.1))
	
	(command "zoom" "w" PONTO1 PONTO2)
	(setq all (ssget "C" PONTO1 PONTO2 (List (cons -4 "<AND") (cons 0 "INSERT")   (cons 2 "AMP3AC")  (cons -4 "AND>")  )))
	
	all
)

(defun procura_redes_connect(obj / blockName anguloRotacao  insercao resposta invertido)
	(command "layer" "m" "linha_perimetro" "c" "yellow" "" "")
	
	(setq blockName (vl-string-trim " " (strcase (cdr (assoc 2 (entget obj))))))
	;Se o resultado for 1, significa que o bloco não foi invertido
	(setq invertido (cdr (assoc 41 (entget obj))))
	
	;Essa variável irá guardar todos os cabos conectados no objeto
	;E será a variável de retorno
	(setq lista_redes_conectadas nil)
	
	
	(if (/= (vl-string-search "LPI" blockName) nil)
		(progn
			(command "zoom" "c" coord 10)
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			
			(command "line" insercao (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) "")
			(command "line" (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1) "")
			(command "line" (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1) (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) "")
			(command "line" (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1) "")
			(command "line" (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1)  (polar (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1) (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001 ) "" )
			
			
			(setq l1 (procura_por_rede_verde  (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1)  (polar (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1) (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001 )   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
			(setq l1 (procura_por_rede_verde (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (polar  (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1) (+ anguloRotacao pi) 1) ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
			
			(setq l1 (procura_por_rede_verde (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1) (polar (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1)   (+ anguloRotacao (samuel_radianos 90)) 1)))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
			
			(setq l1 (procura_por_rede_verde  insercao (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001)   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			(setq l1 (procura_por_rede_verde (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) (polar (polar insercao (- anguloRotacao (samuel_radianos 90)) 0.5000000000000001) anguloRotacao 1) ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
		)
	)
	
	(if (or (= blockName "LE") (= blockName "AMP3") (= blockName "AMP3AC") )
		(progn
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			(setq invertido  (cdr (assoc 41 (entget obj))))
			
			(setq pontoTriangulo (polar  insercao  anguloRotacao 6.062177826512224))
			(setq resposta (verifica_ponto_tri pontoTriangulo obj))
			(if (= resposta 0)
				(progn
					(setq pontoTriangulo (polar  insercao  (+ anguloRotacao pi) 6.062177826512224))
					(setq anguloRotacao (+ anguloRotacao pi))
				)
			)
			
			(command "line" insercao (polar insercao (- anguloRotacao pi ) 0.3323423291321701) "")
			(setq l1 (procura_por_rede_verde 
				insercao (polar insercao (- anguloRotacao pi ) 0.3323423291321701)
			))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
			
			(command "line" insercao (polar insercao (- anguloRotacao (/ pi 2) ) 3.500000000007229) "")
			(command "line" (polar insercao (- anguloRotacao (/ pi 2) ) 3.500000000007229) pontoTriangulo "")
			(command "line"  pontoTriangulo  (polar insercao  (+ anguloRotacao (/ pi 2) )   3.500000000007229 ) "" )
			(command "line"  (polar insercao  (+ anguloRotacao (/ pi 2) )   3.500000000007229 ) insercao ""  )
			
			
			;Verifica as pontas do triângulo, e meios também
			(if (=  blockName "AMP3")
				(progn
					
					
					(command "line" insercao  (polar insercao (+ anguloRotacao pi)  0.3323423290494243) "")
					(setq l1 (procura_por_rede_verde insercao  (polar insercao (+ anguloRotacao pi)  0.3323423290494243)  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					(command "line" (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.3323423290494243 ) "" )
					
					(setq l1 (procura_por_rede_verde 
						(polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.3323423290494243 )
					))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					(command "line" (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.3738855367352605 ) "" )
					(setq l1 (procura_por_rede_verde 
						(polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.3738855367352605 )
					))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
				)
			)
			
			
			(if (=  blockName "AMP3AC")
				(progn
					(if (= invertido 1)
						(progn
							(command "line" (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.6323743077355164 ) "" )
							
							(setq l1 (procura_por_rede_verde 
								(polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.6323743077355164 )
							))
							(if (/= l1 nil)
								(progn
									(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
								)
							)
							
							
							
							(setq meio1 (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ))
							(setq l1 (procura_em_circulo meio1 0.6323743077355191)) ;Retorna a lista com
							;todas as redes que estão conectadas nesse DC
							(if (/= l1 nil)
								(progn
									(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
								)
							)
							(setq tam (length l1))
							(while (> tam 0)
								(command "circle" (nth 1 (nth (- tam 1) l1)) 0.08)
								(setq tam (- tam 1))
							)
							
						)
						(progn
							(command "line" (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.6323743077355164 ) "" )
							
							(setq l1 (procura_por_rede_verde 
								(polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.6323743077355164 )
							))
							(if (/= l1 nil)
								(progn
									(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
								)
							)
							
							
							(setq meio1 (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ))
							(setq l1 (procura_em_circulo meio1 0.6323743077355191)) ;Retorna a lista com
							;todas as redes que estão conectadas nesse DC
							(if (/= l1 nil)
								(progn
									(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
								)
							)
							(setq tam (length l1))
							(while (> tam 0)
								(command "circle" (nth 1 (nth (- tam 1) l1)) 0.08)
								(setq tam (- tam 1))
							)
						)
					)
					
				)
			)
			
			
			(if (or (=  blockName "AMP3") (=  blockName "AMP3AC"))
				(progn
					
					(command "line" pontoTriangulo  (polar pontoTriangulo anguloRotacao  0.3323423290494243) "")
					(setq l1 (procura_por_rede_verde pontoTriangulo  (polar pontoTriangulo anguloRotacao  0.3323423290494243)  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
					
					(if (= invertido 1)
						(progn
							(command "line" (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.3323423290494243 ) "" )
							
							(setq l1 (procura_por_rede_verde (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (- anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (- anguloRotacao (samuel_radianos 60) )  0.3323423290494243 )  ))
						)
						(progn
							(command "line" (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.3323423290494243 ) "" )
							
							(setq l1 (procura_por_rede_verde (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 )  (polar (polar insercao  (+ anguloRotacao (samuel_radianos 29) ) 3.543135675291228 ) (+ anguloRotacao (samuel_radianos 60) )  0.3323423290494243 )  ))
						)
					)
					
					
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
				)
			)
			
			
			
			
			(setq l1 (procura_por_rede_verde insercao (polar insercao (- anguloRotacao (/ pi 2) ) 3.500000000007229)   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			(setq l1 (procura_por_rede_verde (polar insercao (- anguloRotacao (/ pi 2) ) 3.500000000007229)  pontoTriangulo   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			(setq l1 (procura_por_rede_verde   pontoTriangulo  (polar insercao  (+ anguloRotacao (/ pi 2) )   3.500000000007229 )   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			(setq l1 (procura_por_rede_verde (polar insercao  (+ anguloRotacao (/ pi 2) )   3.500000000007229 ) insercao   ))
			(marca_encontrado l1)
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
		)
	)
	
	(if (/= (vl-string-search "DC" blockName) nil)
		(progn
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			(setq raio 1.687500000000000)
			(setq meio1 (polar insercao anguloRotacao raio))
			(setq l1 (procura_em_circulo meio1 raio)) ;Retorna a lista com
			;todas as redes que estão conectadas nesse DC
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			(setq tam (length l1))
			(while (> tam 0)
				(command "circle" (nth 1 (nth (- tam 1) l1)) 0.3)
				(setq tam (- tam 1))
			)
			;(command "line"  insercao  (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  "")
			;(setq l1 (procura_por_rede_verde insercao  (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  ))
		)
	)
	
	(if (/= (vl-string-search "TAP" blockName) nil)
		(progn
		
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			
			;Estilo hexágono
			(if (/= (vl-string-search "TAP8" blockName) nil)
				(progn
				
					(setq aresta1 1.837602605538331)
					(command "line" insercao (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) "")
					(setq l1 (procura_por_rede_verde  insercao (polar insercao  (- anguloRotacao (/ pi 3))  aresta1)   ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					(command "line" (polar insercao  (- anguloRotacao (/ pi 3))  aresta1)   (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) "")
					(setq l1 (procura_por_rede_verde (polar insercao  (- anguloRotacao (/ pi 3))  aresta1)   (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1)   ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					(command "line" (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) "")
					(setq l1 (procura_por_rede_verde (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1)   ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					(command "line"  (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 )  "")
					(setq l1 (procura_por_rede_verde  (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 )  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
					(command "line"  (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 )  (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1) "")
					(setq l1 (procura_por_rede_verde  (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 )  (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1) ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
					(command "line" (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1)   (polar  (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1)   (- anguloRotacao (* (/ pi 3) 2)) aresta1)  "")
					(setq l1 (procura_por_rede_verde  (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1)   (polar  (polar (polar (polar (polar (polar insercao  (- anguloRotacao (/ pi 3))  aresta1) anguloRotacao aresta1) (+ anguloRotacao (/ pi 3)) aresta1) (+ anguloRotacao (* (/ pi 3) 2)  ) aresta1 ) (+ anguloRotacao pi) aresta1)   (- anguloRotacao (* (/ pi 3) 2)) aresta1)  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
				)
			)
			
			
			;Estilo circunferência
			(if (/= (vl-string-search "TAP2" blockName) nil)
				(progn
					(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
					(setq insercao  (cdr (assoc 10 (entget obj))))
					(setq raio 1.742332893241236)
					(setq meio1 (polar insercao anguloRotacao raio))
					(setq l1 (procura_em_circulo meio1 raio)) ;Retorna a lista com
					;todas as redes que estão conectadas nesse DC
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					(setq tam (length l1))
					(while (> tam 0)
						(command "circle" (nth 1 (nth (- tam 1) l1)) 0.3)
						
						(setq tam (- tam 1))
					)
				)
			)
			
			;se for diferente de tap8, e diferente de tap2
			(if (and  (= (vl-string-search "TAP8" blockName) nil)  (= (vl-string-search "TAP2" blockName) nil)   )
				(progn
					;Estilo quadrado
					
					
					(command "line"  insercao  (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  "")
					(setq l1 (procura_por_rede_verde insercao  (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					(command "line"   (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 )   "")
					(setq l1 (procura_por_rede_verde  (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 )  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 )  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
					(command "line"  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 )
						(polar  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 ) (+ anguloRotacao (/ pi 2)) 3.375000000021834 )   "")
					(setq l1 (procura_por_rede_verde   (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 )   (polar  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 ) (+ anguloRotacao (/ pi 2)) 3.375000000021834 ) ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					
					(command "line"   (polar  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 ) (+ anguloRotacao (/ pi 2)) 3.375000000021834 )  (polar insercao  (+ anguloRotacao (/ pi 2)   )  1.687500000010917 )  "" )
					(setq l1 (procura_por_rede_verde  (polar  (polar (polar insercao (- anguloRotacao (/ pi 2)  ) 1.687500000010917 ) anguloRotacao  3.375000000021834 ) (+ anguloRotacao (/ pi 2)) 3.375000000021834 )  (polar insercao  (+ anguloRotacao (/ pi 2)   )  1.687500000010917 )  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
					(command "line" (polar insercao  (+ anguloRotacao (/ pi 2)   )  1.687500000010917 )  insercao  "")
					(setq l1 (procura_por_rede_verde  (polar insercao  (+ anguloRotacao (/ pi 2)   )  1.687500000010917 )  insercao  ))
					(marca_encontrado l1)
					(if (/= l1 nil)
						(progn
							(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
						)
					)
					
					
				)
			)
			
		)
	)
	
	
	(if (= blockName "3WAY")
		(progn
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			(setq raio 1.687500000000022)
			(setq meio1 (polar insercao anguloRotacao raio))
			(setq l1 (procura_em_circulo meio1 raio)) ;Retorna a lista com
			;todas as redes que estão conectadas nesse DC
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			(setq tam (length l1))
			(while (> tam 0)
				(command "circle" (nth 1 (nth (- tam 1) l1)) 0.3)
				(setq tam (- tam 1))
			)
			
		)
	)
	
	(if (= blockName "2WAY")
		(progn
			(setq anguloRotacao  (cdr (assoc 50 (entget obj))))
			(setq insercao  (cdr (assoc 10 (entget obj))))
			(setq raio 1.687500000401749)
			(setq meio1 (polar insercao anguloRotacao raio))
			(setq l1 (procura_em_circulo meio1 raio)) ;Retorna a lista com
			;todas as redes que estão conectadas nesse DC
			(if (/= l1 nil)
				(progn
					(setq lista_redes_conectadas (cons l1  lista_redes_conectadas))
				)
			)
			
			
			(setq tam (length l1))
			(while (> tam 0)
				(command "circle" (nth 1 (nth (- tam 1) l1)) 0.3)
				(setq tam (- tam 1))
			)
		)
	)
	

	
	lista_redes_conectadas
)

(defun ajusta_layers_arcitech1()
	(command "layer" "off" "*" "" "")
	(command "layer" "ON" "NET_UPGRADE" "" "")
	(command "layer" "ON" "NET-TAP" "" "")
	(command "layer" "ON" "NET-SPLT" "" "")
	(command "layer" "ON" "QUADRA" "" "")
	(command "layer" "ON" "QUADRAS" "" "")
	(command "layer" "ON" "NET-AMP" "" "")
	(command "layer" "ON" "NET-CBTR" "" "")
	(command "layer" "ON" "NET-CBTP" "" "")
)

(defun c:amplificadores_conectores_ks()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(setq resp1 (strcase (getstring "\nAjustar layers? [s/n]")))
	(if (= resp1 "S")
		(progn
			(ajusta_layers_arcitech1)
		)
	)
	
	(setq logerro 0)
	(setq resp (strcase (getstring "\nVerificar erros? [s/n]") ))
	(if (= resp "S")
		(progn
			(percorre_elemetnos)
		)
	)
	
	(if (= logerro 1)
		(progn
			(alert "Foram encontrados erros no mapas \nAmplificadores fora do limite \nOs pontos com erros estão marcados na layer 'amplificador_erro'")
			(setq resp (strcase (getstring "\nContinuar mesmo assim? [s/n]") ))
			(if (= resp "N")
				(progn
					(exit)
				)
			)
		)
	)
	
	(percorre_elemetnos2)
	
	(princ)
)






