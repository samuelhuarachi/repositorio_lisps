


(defun percorre_ponto_rede(obj / obj qtd contador  elemento coord1)
	(setq contador 1)
	(setq elemento "")
	(setq pontoAntigo nil)
	(while (/= elemento nil)
		(setq elemento (nth contador (entget obj)))
		
		(if (/= elemento nil)
			(progn
				(setq id1 (car elemento))
				(if (= id1 10)
					(progn
						(setq coord1 (cdr elemento))
						
						(if (/= pontoAntigo nil)
							(progn
								;(command "layer" "m" "layer_temporaria3" "c" "cyan" "" "")
								;(command "line" coord1 pontoAntigo "")
								
								(setq xNecessarios (/  (distance coord1 pontoAntigo)  2.3 ))
								
								(setq dist12 0)
								;Faz o xNecessarios
								(while (> xNecessarios 0)
									(setq ponto1Insert (polar pontoAntigo  (angle pontoAntigo coord1)  (* dist12 2.3)))
									
									
									
									(command "layer" "m" "ALTERAÇÃO" "c" "cyan" "" "")
									(command "insert" "c:\\arcitech1\\blocos\\EXCLUIR_CABOdwg.dwg" ponto1Insert "" "" (* (/ (angle pontoAntigo coord1) pi) 180) )
									
									(setq dist12 (+ dist12 1))
									(setq xNecessarios (- xNecessarios 1))
								)
								
								
								;(setq retorno_metragem (+ retorno_metragem (distance coord1 pontoAntigo) ))
							)
						)
						
						;samcircle_p_OOOraios_OOOlayername_OOOcolor
						(setq pontoAntigo coord1)
					)
				)
			)
		)
		
		(setq contador (+ contador 1))
	)
)


(defun c:excluir_rede()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(princ "\nSelecione a rede")
	(setq all (ssget '((-4 . "<AND") (8 . "NET-CBTR")(0 . "LWPOLYLINE")(-4 . "AND>"))))
	
	(if (/= all nil)
		(progn
			
			(setq objRede (ssname all 0))
			(percorre_ponto_rede objRede)
			
			
		)
	)
	
	(princ)
)