
;----------------------------------------------------------------------------
;                            EXTRAI_ARQUIVO_CABOS
;----------------------------------------------------------------------------
(defun extrai_arquivo_CABOS()
   (SETQ CODIGO_UPGRADE_CABO NIL)
   (SETQ SERVICO NIL)
   (SETQ BITOLA NIL)
   (SETQ COMPRIMENTO NIL)
   (prompt "\nExtraindo Arquivo LANÇAMENTOS . . . Aguarde . . .")
   (setq LISTA_GRAFO (ssget "X" '((2 . "CX_CodUpgradeCabo"))))
   (setq QTDE_LISTA_GRAFO (- (sslength LISTA_GRAFO) 1))
   (setq ARQ_ARC (open C_NOME_DO_ARQUIVO "w"))
   (write-line (strcat "CODIGO_UPGRADE_CABO;SERVICO;BITOLA;COMPRIMENTO") ARQ_ARC)
   (while (>= QTDE_LISTA_GRAFO 0)

      (SETQ CODIGO_UPGRADE_CABO NIL)
      (SETQ SERVICO NIL)
      (SETQ BITOLA NIL)
      (SETQ COMPRIMENTO NIL)
     
      (setq ELEM_GRAFO (ssname LISTA_GRAFO QTDE_LISTA_GRAFO))
      (setq CODIGO_UPGRADE_CABO (itoa (atoi (cdr (assoc 1 (entget (entnext ELEM_GRAFO)))))))
      (setq SERVICO (cdr (assoc 1 (entget (entnext(entnext ELEM_GRAFO))))))
      (setq BITOLA (cdr (assoc 1 (entget (entnext(entnext(entnext ELEM_GRAFO)))))))
      (setq COMPRIMENTO (itoa (atoi (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))
      (write-line (strcat CODIGO_UPGRADE_CABO ";" SERVICO ";" BITOLA ";" COMPRIMENTO) ARQ_ARC)
     (setq QTDE_LISTA_GRAFO (- QTDE_LISTA_GRAFO 1))
   )
   (close ARQ_ARC)
)
;----------------------------------------------------------------------------
;                            EXTRAI_ARQUIVO_ALTERAÇÕES
;----------------------------------------------------------------------------
(defun extrai_arquivo_ALTERACOES()
   (SETQ CODIGO_UPGRADE NIL)
   (SETQ EQUIPAMENTO1 NIL)
   (SETQ ANTIGO1 NIL)
   (SETQ NOVO1 NIL)
   (SETQ EQUIPAMENTO2 NIL)
   (SETQ ANTIGO2 NIL)
   (SETQ NOVO2 NIL)
   (SETQ EQUIPAMENTO3 NIL)
   (SETQ ANTIGO3 NIL)
   (SETQ NOVO3 NIL)
   (SETQ EQUIPAMENTO4 NIL)
   (SETQ ANTIGO4 NIL)
   (SETQ NOVO4 NIL)
   (SETQ EQUIPAMENTO5 NIL)
   (SETQ ANTIGO5 NIL)
   (SETQ NOVO5 NIL)
   (SETQ EQUIPAMENTO6 NIL)
   (SETQ ANTIGO6 NIL)
   (SETQ NOVO6 NIL)
   (SETQ EQUIPAMENTO7 NIL)
   (SETQ ANTIGO7 NIL)
   (SETQ NOVO7 NIL)
   (SETQ EQUIPAMENTO8 NIL)
   (SETQ ANTIGO8 NIL)
   (SETQ NOVO8 NIL)
   (SETQ EQUIPAMENTO9 NIL)
   (SETQ ANTIGO9 NIL)
   (SETQ NOVO9 NIL)

   (prompt "\nExtraindo Arquivo ALTERAÇÕES . . . Aguarde . . .")
   (setq LISTA_GRAFO (ssget "X" '((2 . "CX_CodUpgrade"))))
   (setq QTDE_LISTA_GRAFO (- (sslength LISTA_GRAFO) 1))
   (setq ARQ_ARC (open C_NOME_DO_ARQUIVO "w"))
   (write-line (strcat "CODIGO_UPGRADE;EQUIPAMENTO1;ANTIGO1;NOVO1;EQUIPAMENTO2;ANTIGO2;NOVO2;EQUIPAMENTO3;ANTIGO3;NOVO3;EQUIPAMENTO4;ANTIGO4;NOVO4;EQUIPAMENTO5;ANTIGO5;NOVO5;EQUIPAMENTO6;ANTIGO6;NOVO6;EQUIPAMENTO7;ANTIGO7;NOVO7;EQUIPAMENTO8;ANTIGO8;NOVO8;EQUIPAMENTO9;ANTIGO9;NOVO9") ARQ_ARC)
   (while (>= QTDE_LISTA_GRAFO 0)

      (SETQ CODIGO_UPGRADE NIL)
      (SETQ EQUIPAMENTO1 NIL)
      (SETQ ANTIGO1 NIL)
      (SETQ NOVO1 NIL)
      (SETQ EQUIPAMENTO2 NIL)
      (SETQ ANTIGO2 NIL)
      (SETQ NOVO2 NIL)
      (SETQ EQUIPAMENTO3 NIL)
      (SETQ ANTIGO3 NIL)
      (SETQ NOVO3 NIL)
      (SETQ EQUIPAMENTO4 NIL)
      (SETQ ANTIGO4 NIL)
      (SETQ NOVO4 NIL)
      (SETQ EQUIPAMENTO5 NIL)
      (SETQ ANTIGO5 NIL)
      (SETQ NOVO5 NIL)
      (SETQ EQUIPAMENTO6 NIL)
      (SETQ ANTIGO6 NIL)
      (SETQ NOVO6 NIL)
      (SETQ EQUIPAMENTO7 NIL)
      (SETQ ANTIGO7 NIL)
      (SETQ NOVO7 NIL)
      (SETQ EQUIPAMENTO8 NIL)
      (SETQ ANTIGO8 NIL)
      (SETQ NOVO8 NIL)
      (SETQ EQUIPAMENTO9 NIL)
      (SETQ ANTIGO9 NIL)
      (SETQ NOVO9 NIL)

      (setq ELEM_GRAFO (ssname LISTA_GRAFO QTDE_LISTA_GRAFO))
      (setq CODIGO_UPGRADE (itoa (atoi (cdr (assoc 1 (entget (entnext ELEM_GRAFO)))))))
      (setq EQUIPAMENTO1 (cdr (assoc 1 (entget (entnext(entnext ELEM_GRAFO))))))
      (setq ANTIGO1 (cdr (assoc 1 (entget (entnext(entnext(entnext ELEM_GRAFO)))))))
      (setq NOVO1 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext ELEM_GRAFO))))))))
      (setq EQUIPAMENTO2 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))
      (setq ANTIGO2 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))
      (setq NOVO2 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))
      (setq EQUIPAMENTO3 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))
      (setq ANTIGO3 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))
      (setq NOVO3 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))
      (setq EQUIPAMENTO4 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))
      (setq ANTIGO4 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))
      (setq NOVO4 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))
      (setq EQUIPAMENTO5 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))
      (setq ANTIGO5 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))
      (setq NOVO5 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))
      (setq EQUIPAMENTO6 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))
      (setq ANTIGO6 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))
      (setq NOVO6 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))))
      (setq EQUIPAMENTO7 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))))
      (setq ANTIGO7 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))))))
      (setq NOVO7 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))))))
      (setq EQUIPAMENTO8 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))))))))
      (setq ANTIGO8 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))))))))
      (setq NOVO8 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))))))))))
      (setq EQUIPAMENTO9 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))))))))))
      (setq ANTIGO9 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO)))))))))))))))))))))))))))))))
      (setq NOVO9 (cdr (assoc 1 (entget (entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext(entnext ELEM_GRAFO))))))))))))))))))))))))))))))))
     
      (write-line (strcat CODIGO_UPGRADE";"EQUIPAMENTO1";"ANTIGO1";"NOVO1";"EQUIPAMENTO2";"ANTIGO2";"NOVO2";"EQUIPAMENTO3";"ANTIGO3";"NOVO3";"EQUIPAMENTO4";"ANTIGO4";"NOVO4";"EQUIPAMENTO5";"ANTIGO5";"NOVO5";"EQUIPAMENTO6";"ANTIGO6";"NOVO6";"EQUIPAMENTO7";"ANTIGO7";"NOVO7";"EQUIPAMENTO8";"ANTIGO8";"NOVO8";"EQUIPAMENTO9";"ANTIGO9";"NOVO9) ARQ_ARC)
      (setq QTDE_LISTA_GRAFO (- QTDE_LISTA_GRAFO 1))
   )
   (close ARQ_ARC)
)


;********************************************************************************************************
;********************************************************************************************************
;******************************************* PROGRAMA ***************************************************
;******************************************* PRINCIPAL **************************************************
;********************************************************************************************************
;********************************************************************************************************

(defun c:grafo()
    
     (setvar "cmdecho" 0)
     (command "osnap" "off")
     (setq CAM (getvar "dwgprefix"))
     (setq NOME (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4) ))

     (setq OPCAO "")
     (while (/= OPCAO "S")
 
          (command "_textscr")

          (setq OPCAO "")

          (prompt "\n")
          (prompt "\n       <C> - Extrair CABOS")
          (prompt "\n       <A> - Extrair ALTERAÇÕES")
          (prompt "\n")
          (prompt "\n        S  - SAIR")
          (prompt "\n")(prompt "\n")

          (initget 1 "C c A a S s")
	  (setq OPCAO (getkword "Selecione uma Opção : "))

          (command "_graphscr")

          (if(= OPCAO "c")(setq OPCAO "C"))
          (if(= OPCAO "a")(setq OPCAO "A"))
          (if(= OPCAO "s")(setq OPCAO "S"))

          (cond
 ;************************* EXTRAI ARQUIVO CABOS ***************************
               ((= OPCAO "C")
                  (progn                        
                     (setq C_NOME_DO_ARQUIVO (getfiled "Selecione o nome do arquivo CABOS a ser gerado" (strcat CAM NOME "_CABOS.CSV") "CSV" 1))
                     (extrai_arquivo_CABOS)
                  )
               )
 ;************************* EXTRAI ARQUIVO ARC ***************************
               ((= OPCAO "A")
                  (progn
                     (setq C_NOME_DO_ARQUIVO (getfiled "Selecione o nome do arquivo ALTERAÇÕES a ser gerado" (strcat CAM NOME "_ALTERACOES.CSV") "CSV" 1))
                     (extrai_arquivo_ALTERACOES)
                  )
               )

    

); Fim do PROGRAMA

(princ "\n\nPrograma GRAFO")
(princ "\n    Digite GRAFO para iniciar.")(princ)

       ))