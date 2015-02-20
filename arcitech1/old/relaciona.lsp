

(defun relaciona_leaders()
	
	
	;(setq all (ssget "x" (List (cons 8 layer))))
	(setq all (ssget "x" '((-4 . "<AND") (8 . "NET_UPGRADE")(0 . "LEADER")(-4 . "AND>"))))
	
	
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
						
						;(command "layer" "m" "teste1" "c" "yellow" "" "")
						;(command "circle" coord2 1)
						;(command "circle" coord1 1)
						
						
						
						(setq objInformacoes nil)
						(setq objPolyline nil)
						
						;Salva flechas conferidas, e não vai mais precisar conferir
						(setq x11 (rtos (car coord2) 2 3))
						(setq y11 (rtos (cadr coord2) 2 3))
						(setq procura4 (assoc (strcat x11 y11) listaFeitos))
						(if(= procura4 nil)
							(progn
								
								(setq listaFeitos (cons (list (strcat x11 y11))  listaFeitos))
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
								
							)
							
						)
						
						
						(if (and (/= objInformacoes nil) (/= objPolyline nil) (= procura4 nil) )
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
											(setq layerName2 (strcase (cdr (assoc 8 (entget obj2)))))
											
											(if (or  (= layerName2 "NET-TAP")  (= layerName2 "TPSYM01") )
												(progn
													
													(setq achouObj 1)
													
													;(getstring "ldlddlld")
													
												)
											)
											
											(setq qtd2 (- qtd2 1))
										)
										
										(if (= achouObj 0)
											(progn
												
												(setq coordObj1  (cdr (assoc 10 (entget objInformacoes))))
												
												(command "layer" "m" "Erro_encontrado" "c" "magenta" "" "")
												(command "circle" coordObj1 10)
												(command "circle" coordObj1 9)
												(command "circle" coordObj1 8)
												
												(command "zoom" "c" coordObj1 12)
												
												(getstring "Não encontrou o objeto")
												
												(setq all3 (ssget "x" (List (cons 8 "layer_temporaria1"))))
												(command "erase" all3 "")
												
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


(defun pontos_polyline(obj)

	(setq elemento "")
	(setq contador 1)
	(setq listaPontoPolyline nil)
	(setq listaPontoPolyline2 nil)
	(setq ponto1 nil)
	(setq ponto2 nil)
	(setq distancia12 (cdr (assoc 42 (entget obj))))
	
	(setq pontoVerificados nil )
	(setq pontoVerificados2 nil )
	
	(setq primeiro 1)
	(setq primeiroPonto nil)
	
	(while (/= elemento nil)
							
		(setq elemento (nth contador (entget obj)))
		
		(setq id1 (car elemento))
		
		(if (= id1 10)
			(progn
			
				(setq ponto1 (cdr elemento))
				
				(if (= primeiro 1)
					(progn
						
						(setq primeiroPonto ponto1)
						(setq primeiro 0)
					)
				)
				
				
				(if (and (/= ponto1 nil)(/= ponto2 nil))
					(progn
					
						(setq x1 (rtos (car ponto1) 2 3))
						(setq y1 (rtos (cadr ponto1) 2 3))
						
						
						(setq x2 (rtos (car ponto2) 2 3))
						(setq y2 (rtos (cadr ponto2) 2 3))
						
						(if (/= (strcat x1 y1) (strcat x2 y2)  )
							(progn
						
								(setq pontoMeio (polar ponto1 (angle ponto1 ponto2)  (/ (distance ponto1 ponto2) 2) ))
								(setq ponto3 (polar pontoMeio  (+ (angle ponto1 ponto2) (/ pi 2))   (* distancia12 2)   ))
								
								
								(setq x1 (rtos (car ponto1) 2 3))
								(setq y1 (rtos (cadr ponto1) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons ponto1 listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								(setq x1 (rtos (car ponto3) 2 3))
								(setq y1 (rtos (cadr ponto3) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons ponto3 listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								(setq x1 (rtos (car ponto2) 2 3))
								(setq y1 (rtos (cadr ponto2) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline (cons ponto2 listaPontoPolyline) )
										(setq pontoVerificados (cons (list (strcat x1 y1) ) pontoVerificados))
									)
								)
								
								;####################################
								;####################################
								;####################################
								;(setq aux ponto2)
								;(setq ponto2 ponto1)
								;(setq ponto1 aux)
								
								(setq pontoMeio (polar ponto1 (angle ponto1 ponto2)  (/ (distance ponto1 ponto2) 2) ))
								(setq ponto3 (polar pontoMeio  (+ (angle ponto1 ponto2) (/ pi 2))   (* distancia12 2)   ))
								
								
								(setq x1 (rtos (car ponto2) 2 3))
								(setq y1 (rtos (cadr ponto2) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados2  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline2 (cons (list  (nth 0 ponto2 ) (nth 1 ponto2 ) ) listaPontoPolyline2) )
										(setq pontoVerificados2 (cons (list (strcat x1 y1) ) pontoVerificados2))
									)
								)
								
								(setq x1 (rtos (car ponto3) 2 3))
								(setq y1 (rtos (cadr ponto3) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados2  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline2 (cons (list  (nth 0 ponto3 ) (nth 1 ponto3 )  ) listaPontoPolyline2) )
										(setq pontoVerificados2 (cons (list (strcat x1 y1) ) pontoVerificados2))
									)
								)
								
								
								(setq x1 (rtos (car ponto1) 2 3))
								(setq y1 (rtos (cadr ponto1) 2 3))
								(setq procura5 (assoc (strcat x1 y1 ) pontoVerificados2  ))
								(if (= procura5 nil)
									(progn
										(setq listaPontoPolyline2 (cons (list  (nth 0 ponto1) (nth 1 ponto1 )  ) listaPontoPolyline2) )
										(setq pontoVerificados2 (cons (list (strcat x1 y1) ) pontoVerificados2))
									)
								)
								
								
								
								(command "layer" "m" "layer_temporaria1" "c" "cyan" "" "")
								(command "line" ponto2 ponto3 "")
								;(command "zoom" "c" ponto3 10)
								;(getstring "ldldl")
								(command "line" ponto3 ponto1 "")
								;(command "zoom" "c" ponto2 10)
								;(getstring "ldldl")
								
								
								
							)
						
						)
						
					)
				)
				(setq ponto2 ponto1)
				
				
				
			)
		)
		
		(setq contador (+ contador 1))
	)
	
	
	
	listaPontoPolyline
)


(defun procura_polyline(c1 c2)
	(setq angulo1 (angle c2 c1 ))
	(setq angulo2 (angle c1 c2 ))
	(setq procura nil)
	(setq distanciaRun 0)
	(while (= procura nil)
		(setq procura1 (faz_janela2 (polar c2 angulo1 distanciaRun) 0 "LWPOLYLINE" 0.3 8 "NET_UPGRADE"))
		(setq procura2 (faz_janela2 (polar c2 angulo2 distanciaRun) 0 "LWPOLYLINE" 0.3 8 "NET_UPGRADE"))
		(setq distanciaRun (+ distanciaRun 0.1))
		
		
		(if (/= procura1 nil)
			(progn
				(setq procura (ssname procura1 0))
			)
		)
		(if (/= procura2 nil)
			(progn
				(setq procura (ssname procura2 0))
			)
		)
	)
	
	procura
)

(defun procura_atualizacoes(c1 c2)
	(setq angulo (angle c1 c2))
	(setq procura nil)
	(setq distanciaRun 0)
	(while (= procura nil)
		(setq procura (faz_janela (polar c2 angulo distanciaRun) 2 "CX_CodUpgrade,CX_CodUpgradeCabo" 0.3))
		(setq distanciaRun (+ distanciaRun 0.01))
		
		(if (/= procura nil)
			(progn
				(setq procura (ssname procura 0))
			)
		)
	)
	
	procura
)

(defun faz_janela2(COORD TIPO PROPRIEDADE TAMANHO TIPO2 PROPRIEDADE2)
	 (command "zoom" "c" COORD 30)
	 (setq PONTO1 (polar COORD (/ pi 4) TAMANHO))
	 (setq PONTO2 (polar COORD (* (/ pi 4) 5) TAMANHO))

	 (command "zoom" "w" PONTO1 PONTO2)
	 (setq JANELA (ssget "C" PONTO1 PONTO2 (list (cons TIPO PROPRIEDADE) (cons TIPO2 PROPRIEDADE2) )))
	 JANELA
)

(defun faz_janela(COORD TIPO PROPRIEDADE TAMANHO)
	 (command "zoom" "c" COORD 30)
	 (setq PONTO1 (polar COORD (/ pi 4) TAMANHO))
	 (setq PONTO2 (polar COORD (* (/ pi 4) 5) TAMANHO))

	 (command "zoom" "w" PONTO1 PONTO2)
	 (setq JANELA (ssget "C" PONTO1 PONTO2 (list (cons TIPO PROPRIEDADE))))
	 JANELA
)




(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite 'rel'"))
	(princ)
)

(setq pathLisp "C:\\Users\\Samuel\\Desktop\\arcitech lisp verifica informações\\relaciona.lsp")
(setq basePath "C:\\Users\\Samuel\\Desktop\\arcitech lisp verifica informações\\")


(load "C:\\arcitech1\\funcoes.lsp")

(defun c:rel()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(setq resp (strcase (getstring "\nDo início? S/N")))
	(if (= resp "S")
		(progn
			(setq listaFeitos nil)
		)
	)
	
	(setq all3 (ssget "x" (List (cons 8 "layer_temporaria1,Erro_encontrado")  )))
	(command "erase" all3 "")
	
	
	(relaciona_leaders)
	
	
	(alert "Fim")
	(princ "\nFim")
	(princ)
)























