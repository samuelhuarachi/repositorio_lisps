
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


(defun opcoes_nomerua(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Visualizar nome de rua pré-edição")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Visualizar nome de rua pré edição
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizaRua.lsp"))
								(c:vsrua)
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

(defun opcoes_etiqueta(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Mover etiqueta")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Mover etiqueta
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "mover.lsp"))
								(c:mm)
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

(defun opcoes_cabo_proposto_pre(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Visualizar rede proposto pré-edição")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar rede atual pré edição
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizarRedePropPre.lsp"))
								(c:vsredeprop)
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


(defun opcoes_cabo_atual_pre(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Visualizar rede atual pré-edição")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar rede atual pré edição
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizarRedeAtualPre.lsp"))
								(c:vsredeatual)
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

(defun opcoes_arvore_pre(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Visualizar árvore pré-edição")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar árvore
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizaArvore.lsp"))
								(c:vsarvore)
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

(defun opcoes_arvore(layerRede)
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Editar árvore")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar árvore
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdEditarArvore.lsp"))
								(c:editarArvore)
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



(defun opcoes_cliente_pre(layerRede)
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Visualizar cliente proposto")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar cliente
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizarClientePre.lsp"))
								(c:vscliente)
								
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

(defun opcoes_cliente(layerRede)
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Editar cliente")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar cliente
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdEditarCliente.lsp"))
								(c:editarCliente)
								
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


(defun opcoes_postes_pre(layerRede)
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Editar poste pré-edição")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar poste
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdVisualizaPoste.lsp"))
								(c:vsposte)
								
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

(defun opcoes_postes(layerRede)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Editar poste")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar poste
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "cmdPoste.lsp"))
								(c:selposte)
								
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

(defun opcoes_redes(layerRede)
	(setq valid_option nil)
	(while (= valid_option nil)
		(princ "\n[1] - Editar rede")
		(princ "\n[2] - Mover rede")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "2" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Editar rede
						(if (= (strcase opcao) "1" )
							(progn
								
								(if (= layerRede "CABO_PRIMARIO_PROPOSTO")
									(progn
										(load (strcat basePath "cmdVisualizaCaboPrimProposto.lsp"))
										(c:vscaboprimprop)
									)
								)
								(if (= layerRede "CABO_SECUNDARIO_ATUAL")
									(progn
										(load (strcat basePath "cmdVisualizaCaboSec.lsp"))
										(c:vscabosec)
									)
								)
								(if (= layerRede "CABO_SECUNDARIO_PROPOSTO")
									(progn
										(load (strcat basePath "cmdVisualizaCaboSecProposto.lsp"))
										(c:vscabosecprop)
									)
								)
								(if (= layerRede "CABO_PRIMARIO_ATUAL")
									(progn
										(load (strcat basePath "cmdVisualizaCaboPrim.lsp"))
										(c:vscaboprim)
									)
								)
								
								
							)
						)
						
						;Mover rede
						(if (= (strcase opcao) "2" )
							(progn
								(load (strcat basePath "moverRede.lsp"))
								(c:mmr)
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


(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite 'sel'"))
	(princ)
)

(setq pathLisp "C:\\Bandeirante_Gecad\\cmdSel.lsp")
(setq basePath "C:\\Bandeirante_Gecad\\")

(defun c:sel()
	(vl-load-com)
	
	(princ "\nSelecione um objeto no mapa")
	(setq selObj (ssget))
	(if (/= selObj nil )
		(progn
			
			(setq obj (ssname selObj 0))
			
			;pega a layer do objeto
			(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
			
			(if (or (= layerName "CABO_PRIMARIO_PROPOSTO") (= layerName "CABO_SECUNDARIO_ATUAL") (= layerName "CABO_SECUNDARIO_PROPOSTO") (= layerName "CABO_PRIMARIO_ATUAL") )
				(progn
					(opcoes_redes layerName)
				)
			)
			
			
			(if (= layerName "POSTES")
				(progn
					(opcoes_postes layerName) 
				)
			)
			(if (= layerName "POSTE")
				(progn
					(opcoes_postes_pre layerName) 
				)
			)
			
			
			(if (= layerName "CLIENTEPROPOSTO")
				(progn
					(opcoes_cliente layerName) 
				)
			)
			(if (= layerName "CLIENTE_PROPOSTO")
				(progn
					(opcoes_cliente_pre layerName) 
				)
			)
			
			(if (= layerName "PODA")
				(progn
					(opcoes_arvore layerName) 
				)
			)
			
			(if (= layerName "ARVORE")
				(progn
					(opcoes_arvore_pre layerName) 
				)
			)
			
			(if (= layerName "CABO_ATUAL")
				(progn
					(opcoes_cabo_atual_pre layerName) 
				)
			)
			
			(if (= layerName "CABO_PROPOSTO")
				(progn
					(opcoes_cabo_proposto_pre layerName) 
				)
			)
			
			
			(if (= layerName "GEO_ETIQUETA")
				(progn
					(opcoes_etiqueta layerName) 
				)
			)
			
			(if (= layerName "NOME_DE_RUA")
				(progn
					(opcoes_nomerua layerName) 
				)
			)
			
			
		)
		(progn
			
			
			(princ "\nNenhum objeto selecionado!")
		)
	)
	
	(princ)
)