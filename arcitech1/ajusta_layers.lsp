
(defun ajusta_layers_arcitech1()
	(command "layer" "off" "*" "" "")
	
	(command "layer" "ON" "NET_UPGRADE" "" "")
	(command "layer" "ON" "NET-TAP" "" "")
	(command "layer" "ON" "NET-SPLT" "" "")
	(command "layer" "ON" "ELETRONICO" "" "")
	(command "layer" "ON" "QUADRA" "" "")
	(command "layer" "ON" "QUADRAS" "" "")
	(command "layer" "ON" "HC" "" "")
	(command "layer" "ON" "TERRA" "" "")
	(command "layer" "ON" "RUA" "" "")
	(command "layer" "ON" "LIMITE" "" "")
	(command "layer" "ON" "pular1" "" "")
	(command "layer" "ON" "NET-AMP" "" "")
	
)


(defun c:aj()
	(setvar "cmdecho" 0)
	(command "_osnap" "none")
	(vl-load-com)
	
	(ajusta_layers_arcitech1)
	
	(princ)
)