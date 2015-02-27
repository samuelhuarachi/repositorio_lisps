


(defun c:ssname()
	(vl-load-com)
	
	(setq selObj (ssget))
	(setq selObj (ssname selObj 0))
	(setq lista (entget selObj))
	
	(princ)
)


(defun parar()
	(getstring "\n======   Breakpoint     ========")
)

(defun gts(c)
	(getstring c)
)


(defun CriarLink (ent link codigo)
	(if (not (tblsearch "APPID" link)) (regapp link))
	(if (= (type codigo) 'STR)
		(setq tipo_dado 1000)
		(setq tipo_dado 1071)
	)
	(setq entl (entget ent))
	(setq xd (list (list -3 (list link (cons tipo_dado codigo)))))
	(setq new_entl (append entl xd))
	(entmod new_entl)
	(entupd (cdr (assoc -1 new_entl)))
)


(defun sparser (str delim / ptr lst)
	(while (setq ptr (vl-string-search delim str))
		(setq lst (cons (substr str 1 ptr) lst))
		(setq str (substr str (+ ptr 2)))
	)
	(reverse (cons str lst))
)

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


(defun GetId (entd nome_link / codigo nome links retorno)
 (setq retorno nil)
 (if entd
  (progn   
   (if (setq Links  (assoc -3 (entget entd '("*"))))
	(progn
	 (assoc codlog (cdr links))   
	 (if (assoc nome_link (cdr links))
	  ;(assoc "Samuel" (cdr links))
	  (setq retorno (cdr (car (cdr (assoc nome_link (cdr links))))))
	  ;(setq retorno (cdr (car (cdr (assoc "Samuel" (cdr links))))))
	 )
	)
   )
  )
 )
 retorno
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

(defun c:v ()
 
 
 (if (setq ent (car (entsel)))
   ;(entget ent '("*"))
   (assoc -3 (entget ent '("*")))
 )
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
	(if (/= o nil)
		(progn
			(command "erase" o "")
		)
	)
	
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

(defun retorna_pontos_base (all / qtd obj layerName coord x1 y1 contador2 obj_base)
	(setq obj_base nil)
	(setq contador2 0)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (= layerName "PONTO_BASE")
					(progn
						(setq contador2 (+ contador2 1))
						(setq obj_base obj)
					)
				)
				
				
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	(if (/= contador2 1)
		(progn
			(setq obj_base nil)
		)
	)
	obj_base
)



(defun procura_atualizacoes(c1 c2 / angulo  procura distanciaRun)
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

(defun faz_janela(COORD TIPO PROPRIEDADE TAMANHO / JANELA PONTO1 PONTO2)
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


;Pega o angulo e a distancia, vc entra com 2 pontos
(defun c:gad()
	(vl-load-com)
	
	(setq p1_ (getpoint "\nPONTO 1"))
	(setq p2_ (getpoint "\nPONTO 2"))
	
	
	(setq angulo (angle p1_ p2_))
	(setq ANGULOGRAUS (* (/ ANGULO pi) 180))
	(setq distancia (distance p1_ p2_))
	
	(princ (strcat "\nDistancia    " (rtos distancia 2 18)))
	(princ (strcat "\n" (rtos angulo 2 18)  "   ///   "   (rtos ANGULOGRAUS 2 18)))
	
	
	
	
	(princ)
)



(defun samcircle(p raios layername color)
	(if  (= layername nil)
		(progn
			(setq layername "layer_temporaria2")
		)
	)
	(if (= color nil)
		(progn
			(setq color "251")
		)
	)
	(if (= raios nil)
		(progn
			(setq raios 1)
		)
	)
	
	(command "layer" "m" layername "c" color "" "")
	(command "circle" p raios)
)

(defun samshow(v)
	(if (/= (type v) 'STR)
		(progn
			(setq v (rtos v 2 2))
		)
	)
	(princ (strcat "\nVariável: " v))
	(princ)
)


(defun verifica_duplicidade(layerName typeBlock / lista_coordenadas achouDuplicado  all obj coord x1 y1 chave procura1)
	
	(setq lista_coordenadas nil)
	(setq achouDuplicado 0)
	
	;(setq all (ssget "x" (List (cons 8 layer))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	(setq all (ssget "x" (List (cons -4 "<AND") (cons 0 typeBlock)   (cons 8 layerName)  (cons -4 "AND>")  )))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq chave (strcat x1 y1))
				(setq procura1 (assoc chave lista_coordenadas))
				
				(if (= procura1 nil)
					(progn
						(setq lista_coordenadas (cons (list chave ) lista_coordenadas))
					)
					(progn
						(command "layer" "m" "Item_Duplicado" "c" "red" "" "")
						(command "circle" coord 8)
						(command "circle" coord 9)
						(command "circle" coord 10)
						(setq achouDuplicado 1)
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	(if (= achouDuplicado 1)
		(progn
			(princ "\nOs objetos duplicados foram marcados na layer 'Item_Duplicado'. Layer do objeto: " layerName ", tipo do objeto: " typeBlock)
		)
		(progn
			(princ (strcat "\nNão foi encontrado objetos duplicados. Layer do objeto: " layerName ", tipo do objeto: " typeBlock) )
		)
	)
	(princ)
)
