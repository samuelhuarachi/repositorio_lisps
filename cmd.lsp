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


(defun opcoes_cmd()
	(setq valid_option nil)
	(while (= valid_option nil)
		
		(princ "\n### Comandos disponíveis ###")
		(princ "\n[1] - Converter mapa")
		(princ "\n[2] - Adicionar rede (pós conversão)")
		(princ "\n[3] - Adcionar dxf (pré conversão)")
		(princ "\n[4] - Inserir quadras")
		(princ "\n[5] - Extrair (pós conversão)")
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "1" "2" "3" "4" "5" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						;Converter mapa
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "ConverteMapa.fas"))
								(c:conv)
							)
						)
						;Adicionar rede
						(if (= (strcase opcao) "2" )
							(progn
								(load (strcat basePath "cmdAddRede.lsp"))
								(c:addRede)
							)
						)
						;Adicionar dxf
						(if (= (strcase opcao) "3" )
							(progn
								(load (strcat basePath "cmdInsereDxf.lsp"))
								(c:inseredxf)
							)
						)
						
						;Inserir quadras
						(if (= (strcase opcao) "4" )
							(progn
								(load (strcat basePath "insere_quadras.lsp"))
								(c:Quadras)
							)
						)
						
						;Extrair
						(if (= (strcase opcao) "5" )
							(progn
								(load (strcat basePath "extrai.lsp"))
								(c:extrai)
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
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite 'cmd'"))
	(princ)
)

(setq pathLisp "C:\\Bandeirante_Gecad\\cmd.lsp")
(setq basePath "C:\\Bandeirante_Gecad\\")


(defun c:cmd()
	(opcoes_cmd)
	
	(princ)
)