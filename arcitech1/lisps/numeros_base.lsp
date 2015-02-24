(defun carrega_arquivo2()
 
	(setq ARQUIVO_CSV (getfiled "Selecione o arquivo CSV" "c:" "csv" 0))
	(setq ARQUIVO_CSV (open ARQUIVO_CSV "r"))

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
				(setq numero2  (nth 2 LISTA_LINHA))
				
				
				(command "layer" "m" "numero_base" "c" "green" "" "")
				(command "text" "bc" (list x y 0) 3 0 numero2)
				
				
				
				
				(setq LINHA_CSV (read-line ARQUIVO_CSV))
				(setq LISTA_LINHA nil)
				
			)
			
			(close ARQUIVO_CSV)
			(setq ARQUIVO_CSV nil)
		)
	)
)

(defun c:lll()
	(load pathLisp)
	
	(princ (strcat "O arquivo '" pathLisp "', foi carregado. Para iniciar digite '" nomeComando "'"))
	(princ)
)


(setq basePath "C:\\arcitech1\\")
(setq nomeComando "num1")
(setq pathLisp (strcat basePath "numeros_base.lsp"))



(load "C:\\arcitech1\\lisps_aux\\funcoes.lsp")



(defun c:num1()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(carrega_arquivo2)
	
)