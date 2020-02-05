; item 1
(defstruct modelo marca designacao potencia cilindradas cor portas) ;Estrutura Modelo (item 1)

(setq modelo1 (make-modelo :marca "Rolls Royce" :designacao "Silver-Ghost" :potencia "300Cv" :cilindradas "20000cm^3" :cor "" :portas "")) ; Modelo1 criado da estrutura modelo
(setq modelo2 (make-modelo :marca "Toyota" :designacao "Silver-Ghost" :potencia "150Cv" :cilindradas "10000cm^3" :cor "Branco" :portas "6")) ;Modelo2 criado da estrutura modelo

; item 2
(defstruct veiculomd1 matricula ano (modelo modelo1)) ; Estrutura veiculo com modelo 1 (item 2)

; item 3
(setq veiculo1 (make-veiculomd1 :matricula "12-34-AB" :ano 1900 )) ; Veiculo1 com modelo1 (item 3)
(setq veiculo2 (make-veiculomd1 :matricula "12-68-AB" :ano 2000 )) ; Veiculo2 com modelo1

(defstruct veiculomd2 matricula ano (modelo modelo2)) ;Estrutura veiculo com modelo 2 (item 2)
(setq veiculo3 (make-veiculomd2 :matricula "12-25-AB" :ano 2010 )) ; Veiculo3 com modelo2

;(print modelo1)
;(print veiculo1)

(print "Digite um veiculo: (Ex: veiculo1, veiculo2, veiculo3)")
(setq veiculolido (read)) ; Recebendo o veiculo desejado, exemplo: digitar 'veiculo1' ou 'veiculo2' ou 'veiculo3'

;(Item 4)
(if (string-equal veiculolido "veiculo1") 
	(setq modeloCarro (veiculomd1-modelo veiculo1)) 
	(if (string-equal veiculolido "veiculo2") 
		(setq modeloCarro (veiculomd1-modelo veiculo2)) 
		(if (string-equal veiculolido "veiculo3")
			(setq modeloCarro (veiculomd2-modelo veiculo3)) 
			(print "Veiculo não encontrado")
		)
	)
)
																				
;(setq modeloCarro (veiculo-modelo veiculo1)) -- Pega modelo do carro
(setq cilindradas (modelo-cilindradas modeloCarro)) ; Pega a cilindrada do modelo que foi pego.
(print cilindradas)
