


(defun c:ssname()
	(vl-load-com)
	
	(setq selObj (ssget))
	(setq selObj (ssname selObj 0))
	(setq lista (entget selObj))
	
	(princ)
)


(defun c:deleteall( / layerName typeBlock qtd)
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(setq obj (ssget))
	(if (/= obj nil)
		(progn
			(setq obj (ssname obj 0))
			(setq valid_option nil)
			(while (= valid_option nil)
				
				(princ "\n### Comandos disponíveis ###")
				(princ "\nTem certeza que deseja excluir os itens?")
				(princ "\n[p] - Prosseguir")
				(princ "\n[c] - Cancelar")
				(princ "\n[s] - Sair")
				(princ "\nDigite a opção: ")
				
				(setq opcao (getstring))
				(setq valid_option (verifica_opcoes_escolha_string opcao (list "P" "C" "S")))
				(if (= valid_option nil )
					(progn
						(princ "\n### Opção inválida! ###")
					)
					(progn
						;Se for diferente de sair
						(if (and (/= (strcase opcao) "S" )(/= (strcase opcao) "C" ) )
							(progn
								
								(if (= (strcase opcao) "P" )
									(progn
										
										(setq layerName (cdr (assoc 8 (entget obj))))
										(setq typeBlock (cdr (assoc 0 (entget obj))))
										
										(setq SELECAO (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
										
										(if (/= SELECAO nil)
											(progn
												(setq qtd (sslength SELECAO))
												(sam_delete SELECAO)
												(princ (strcat "\nForam excluídos " (itoa qtd) " objetos."))
											)
											(progn
												(alert "Nenhum item foi excluído")
											)
										)
										
									)
								)
								
							)
							(progn
								(princ "\nSaindo da aplicação...")
							)
						)
					)
				)
			)
			
		)
		(progn
			(alert "Nenhum objeto selecionado")
		)
	)
	
	(princ)
)


(defun f_nao_configurado()
	(command "layer" "m" "Nao_Configurado" "c" "green" "" "")
	(command "circle" (list x y 0) 11)
	(command "circle" (list x y 0) 12)
)

(defun f_tudo_ok()
	(command "layer" "m" "Configurado_Ok" "c" "yellow" "" "")
	(command "circle" (list x y 0) 5)
	(command "circle" (list x y 0) 6)
)



(defun f_numeracao_dos_blocos_incompativeis()
	(command "layer" "m" "Divergencia_Valores_incopativeis" "c" "133" "" "")
	(command "circle" (list x y 0) 7)
	(command "circle" (list x y 0) 8)
)


(defun f_nome_dos_blocos_incompativeis()
	(command "layer" "m" "Nomes_dos_blocos_incopativeis" "c" "190" "" "")
	(command "circle" (list x y 0) 9)
	(command "circle" (list x y 0) 10)
)


(defun carrega_lista_posicao_tap()
	(setq all (ssget "x" (List (cons -4 "<OR") (cons 8 "NET-TAP")  (cons 8 "DC")  (cons -4 "OR>") )))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	;(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(setq lista_posicao_tap nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq lista_posicao_tap (cons (list (strcat x1 y1) obj  ) lista_posicao_tap))
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
)






(defun sam_delete(o)
	(command "erase" o "")
)



(defun configura_lista(lista)
	(setq comprimento_array (length lista))
	(setq lista1 nil)
	(while (> comprimento_array 0)
		
		(setq elem (nth (- comprimento_array 1)  lista ) )
		
		
		(setq lista1 (cons (list elem) lista1))
		
		(setq comprimento_array (- comprimento_array 1))
	)
	
	lista1
)

(defun verifica_opcoes_escolha_string(string_digitada opcoes)
	(setq string_digitada (strcase string_digitada))
	(setq opcoes (configura_lista opcoes))
	
	(setq check_value (assoc string_digitada opcoes))
	
	check_value
)




;:: ViewExtents  coordenada dos zooms da tela  ===> retorna uma lista de coordenadas
(defun ViewExtents (/ A B C D X)
  (setq B (getvar "VIEWSIZE")
        A (* B (/ (car (getvar "SCREENSIZE")) (cadr (getvar "SCREENSIZE"))))
        X (trans (getvar "VIEWCTR") 1 2)
        C (trans (list (- (car X) (/ A 2.0)) (+ (cadr X) (/ B 2.0))) 2 1)
        D (trans (list (+ (car X) (/ A 2.0)) (- (cadr X) (/ B 2.0))) 2 1)
  );setq
  (list C D)
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
		(setq procura (faz_janela (polar c2 angulo distanciaRun) 2 "CX_CODUPGRADE,CX_CodUpgrade,CX_CodUpgradeCabo" 0.3))
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


(defun sam_zoom(c tamanho)
	(if (= tamanho nil)
		(setq tamanho 10)
	)
	(command "zoom" "c" c tamanho)
)

(defun sam_rtos (numero_real casas_decimais)
	(if (= casas_decimais nil)
		(progn
			(setq casas_decimais 3)
		)
	)
	(rtos numero_real 2 casas_decimais)
	
)

