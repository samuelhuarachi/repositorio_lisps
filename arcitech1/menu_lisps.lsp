

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




(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite '" nomeComando "'"))
	(princ)
)

(setq basePath "C:\\arcitech1\\")
(setq nomeComando "loadl")
(setq pathLisp (strcat basePath "carrega_lisps.lsp"))

(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")

(defun c:loadl()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(setq valid_option nil)
	(while (= valid_option nil)
		
		(princ "\n### Comandos disponíveis ###")
		(princ "\n[0] - Ajustar layers")
		(princ "\n[1] - Parte 1 - Carregar posições do mapa original")
		(princ "\n[2] - Parte 2 - Verificando possíveis erros no mapa")
		(princ "\n[3] - Parte 3 - Verificar erros no mapa")
		(princ "\n[4] - Verifica erros individualmente")
		(princ "\n[5] - Números base")
		(princ "\n[6] - Relatório")
		(princ "\n[7] - Numera CX_CodUpgradeCabo")
		(princ "\n[8] - Verifica comprimento dos cabos")
		
		(princ "\n[s] - Sair")
		(princ "\nDigite a opção: ")
		
		(setq opcao (getstring))
		(setq valid_option (verifica_opcoes_escolha_string opcao (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "S")))
		(if (= valid_option nil )
			(progn
				(princ "\n### Opção inválida! ###")
			)
			(progn
				;Se for diferente de sair
				(if (/= (strcase opcao) "S" )
					(progn
						
						(if (= (strcase opcao) "0" )
							(progn
								(load (strcat basePath "lisps\\ajusta_layers.lsp"))
								(c:aj)
							)
						)
						(if (= (strcase opcao) "1" )
							(progn
								(load (strcat basePath "lisps\\parte1.lsp"))
								(c:dd)
							)
						)
						(if (= (strcase opcao) "2" )
							(progn
								(load (strcat basePath "lisps\\relaciona.lsp"))
								(c:rel)
							)
						)
						(if (= (strcase opcao) "3" )
							(progn
								(load (strcat basePath "lisps\\parte2.lsp"))
								(c:pp)
							)
						)
						(if (= (strcase opcao) "4" )
							(progn
								(load (strcat basePath "lisps\\parte2_individual.lsp"))
								(c:ppi)
							)
						)
						(if (= (strcase opcao) "5" )
							(progn
								(load (strcat basePath "lisps\\numeros_base.lsp"))
								(c:num1)
							)
						)
						
						(if (= (strcase opcao) "6" )
							(progn
								(load (strcat basePath "lisps\\preenche_planilha2.lsp"))
								(c:rtt)
							)
						)
						
						(if (= (strcase opcao) "7" )
							(progn
								(load (strcat basePath "lisps\\numera_cx.lsp"))
								(c:numera_cx)
							)
						)
						
						(if (= (strcase opcao) "8" )
							(progn
								(load (strcat basePath "lisps\\calcula_metragem_cabo.lsp"))
								(c:calcula_metragem_cabo)
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
	
	(princ "\nFim")
	(princ)
)