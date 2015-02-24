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


(defun extrai_posicao()
	;(setq all (ssget "X" '((8 . "LAYER"))))
	;(setq all (ssget "x" (List (cons 8 "NET-TAP"))))
	(setq all (ssget "x" '((-4 . "<OR")    (2 . "DC") (8 . "NET-TAP") (-4 . "OR>"))))
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq tipo  (strcase (cdr (assoc 0 (entget obj)))))
				(if (= tipo "INSERT")
					(progn
						(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
						(setq blockName (strcase (cdr (assoc 2 (entget obj)))))
						(setq coord (cdr (assoc 10 (entget obj))))
						(setq x1 (rtos (car coord) 2 3))
						(setq y1 (rtos (cadr coord) 2 3))
						
						(setq valor1 (retorna_attrib obj 1))
						
						;(getstring "lldldl")
						
						(if (/= valor1 nil)
							(progn
								(write-line (strcat x1 ";" y1 ";" valor1 ";" blockName) ARQUIVO_CSV)
								(command "layer" "m" "layer_teste" "c" "yellow" "" "")
								(command "circle" coord 10)
							)
						)
						
					)
				)	
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun abre_arq()
 (setq ARQUIVO_CSV "info_geral."
       ;ARQUIVO_CSV (open (strcat ARQUIVO_CSV "csv") "w"))
       ARQUIVO_CSV (open (getfiled (strcat "Escolha a Pasta") "" "csv" 1) "w")
))

(defun fecha()
	(close ARQUIVO_CSV)
)

(defun c:dd()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	(abre_arq)
	(extrai_posicao)
	(fecha)
	
	
	(princ "\nFim...")
	(princ)
)