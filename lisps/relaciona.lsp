

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
						(setq coord1 nil)
						(setq coord2 nil)
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
						
						
						
						
						
						;Salva flechas conferidas, e não vai mais precisar conferir
						(setq x11 (rtos (car coord2) 2 3))
						(setq y11 (rtos (cadr coord2) 2 3))
						
						(setq objPolyline nil)
						(setq objInformacoes nil)
						
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
											
											(if (= (strcase (cdr (assoc 0 (entget obj2)))) "INSERT")
												(progn
													(setq layerName2 (strcase (cdr (assoc 8 (entget obj2)))))
													(setq blockName2 (strcase (cdr (assoc 2 (entget obj2)))))
													
													(if (or  (= layerName2 "NET-TAP")  (= blockName2 "DC") )
														(progn
															
															(setq achouObj 1)
															
															;(getstring "ldlddlld")
															
														)
													)
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
										
										
										(setq coordObj1  (nth 0 listaPontoPolyline2))
												
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
						)
						
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	
)






(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite 'rel'"))
	(princ)
)

(setq pathLisp "C:\\arcitech1\\relaciona_mais_recente.lsp")
(setq basePath "C:\\arcitech1\\")

(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")


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
	
	(setq all3 (ssget "x" (List (cons 8 "layer_temporaria1,Erro_encontrado"))))
	(command "erase" all3 "")
	
	(relaciona_leaders)
	
	(alert "Fim")
	(princ "\nFim")
	(princ)
)























