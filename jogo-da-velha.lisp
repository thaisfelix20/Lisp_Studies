    (defvar *jogador* 1) ;Definição da variável que representa o jogador 

    (defvar *pc* 10) ;Definição da variável que representa o pc 

    (defun cria-tabuleiro () ; Função que cria o tabuleiro 
    ;O tabuleiro é representado como uma lista que consiste no símbolo tabuleiro seguido por 9 zeros
      (list 'tabuleiro 0 0 0 0 0 0 0 0 0)) ;board =  tabuleiro

    (defun converte-para-letra (v) ;Converte o que for digitado nas respectivas letras: 0 em vazio, 1 em O e 10 em X
      (cond ((eql v 1) "O") ; 1 significa que a posição é preenchida por um O
            ((eql v 10) "X") ; 10 significa que a posição é preenchida por um X
            (t " "))) ;0 zero significa que a posição está vazia

    (defun imprime-linha (x y z) ; Imprime as linhas com o |
      (format t "~&  ~A | ~A | ~A"
              (converte-para-letra x)
              (converte-para-letra y)
              (converte-para-letra z)))

    (defun imprime-tabuleiro (tabuleiro) ;Função que imprime o tabuleiro colocando o tracejado entre as linhas
      (format t "~%")
      (imprime-linha (nth 1 tabuleiro) (nth 2 tabuleiro) (nth 3 tabuleiro)) ; Retorna o elemento nth da lista de acordo com o número.
      (format t "~& -----------")
      (imprime-linha (nth 4 tabuleiro) (nth 5 tabuleiro) (nth 6 tabuleiro)) ; Retorna o elemento nth da lista de acordo com o número.
      (format t "~& -----------")
      (imprime-linha (nth 7 tabuleiro) (nth 8 tabuleiro) (nth 9 tabuleiro)) ; Retorna o elemento nth da lista de acordo com o número.
      (format t "~%~%")) 

    (defun realiza-movimento (jogador pos tabuleiro) 
      (setf (nth pos tabuleiro) jogador)
      tabuleiro)

      ;Define as possibilidades de jogo, que nesse caso são 3 para ganhar
    (defvar *possibilidades*
      '((1 2 3) (4 5 6) (7 8 9) ; Horizontal
        (1 4 7) (2 5 8) (3 6 9) ; Vertical
        (1 5 9) (3 5 7)))       ; Diagonal

        ;Essa função retorna a soma dos números definidos acima
    (defun sum-possibilidades (tabuleiro triplet)
      (+ (nth (first triplet) tabuleiro)
         (nth (second triplet) tabuleiro)
         (nth (third triplet) tabuleiro)))

         ;Essa função retorna a lista de todas as oito somas
    (defun compute-sums (tabuleiro)
      (mapcar #'(lambda (triplet)
                  (sum-possibilidades tabuleiro triplet))
              *possibilidades*))

    (defun vencedor (tabuleiro) ;Função que define o ganhador
      (let ((sums (compute-sums tabuleiro)))
        (or (member (* 3 *jogador*) sums)
            (member (* 3 *pc*) sums))))

    (defun board-full-p (tabuleiro)
      (not (member 0 tabuleiro)))
    (format t "~&Sua Jogada: ")
    (defun read-a-legal-move (tabuleiro)
      (let ((pos (read)))
        (cond ((not (and (integerp pos)
                         (<= 1 pos 9)))
               (format t "~&Entrada Inválida.") ;Se o número não estiver entre 0 e 9 apresenta a mensagem
               (read-a-legal-move tabuleiro))
              ((not (zerop (nth pos tabuleiro)))
               (format t "~&A jogada já foi realizada.") ;Se tentar jogar onde já está ocupado apresenta a mensagem
               (read-a-legal-move tabuleiro))
              (t pos))))

              ;Função para escolha de uma posição aleatória válida
    (defun pega-posicao-vazia-aleatoria (tabuleiro)
      (let ((pos (+ 1 (random 9)))) ;Pega uma posição aleatória de 1 a 9
        (if (zerop (nth pos tabuleiro)) ;Se a posição estiver disponível faz o movimento
          pos(pega-posicao-vazia-aleatoria tabuleiro)))) ;Caso contrário, se chama de maneira recursiva para buscar outro número aleatório

          ;Função que encontra uma posição vazia
    (defun encontra-posicao-vazia (tabuleiro squares)
      (find-if #'(lambda (pos)
                   (zerop (nth pos tabuleiro)))
               squares))

    (defun ganha-ou-bloqueia (tabuleiro target-sum)
      (let ((triplet (find-if
                       #'(lambda (trip)
                           (equal (sum-possibilidades tabuleiro trip) target-sum))
                       *possibilidades*)))
        (when triplet
          (encontra-posicao-vazia tabuleiro triplet))))

    (defun strategia-movimento-aleatorio (tabuleiro)
      (list (pega-posicao-vazia-aleatoria tabuleiro)
            "Movimento aleatório"))

            ;Se houver dois XS em sequência, deve preencher o terceiro X para ganhar o jogo
            ;Retorna NIL se ele não puder encontrar um movimento que se encaixe em sua estratégia.
    (defun faz-tres-seguidas (tabuleiro)
      (let ((pos (ganha-ou-bloqueia tabuleiro (* 2 *pc*))))
        (and pos (list pos "Tente marcar as três"))))

        ;Se houver dois Os seguidos, ele deve colocar um X para impedir que o oponente ganhe.
        ;Também retorna NIL se ele não puder encontrar um movimento que se encaixe em sua estratégia.
    (defun block-player-win (tabuleiro)
      (let ((pos (ganha-ou-bloqueia tabuleiro (* 2 *jogador*))))
        (and pos (list pos "Blocking player win"))))

        ;Definição da função para escolha da melhor jogada 
        ;Precisamos desta função pois como foi descrito anteriormente para dar preferência a essas duas estratégias em relação
        ;à estratédia de movimento aleatório
    (defun escolhe-melhor-jogada (tabuleiro)
      (or (faz-tres-seguidas tabuleiro)
          (block-player-win tabuleiro)
          (strategia-movimento-aleatorio tabuleiro)))

    (defun movimento-pc (tabuleiro) movimento-pc
      "Placeholder"
      tabuleiro)

      ;Define o movimento do jogador
    (defun movimento-jogador (tabuleiro) 
      (format t "~&Sua Jogada: ")
      (let* ((pos (read-a-legal-move tabuleiro))
             (novo-tabuleiro (realiza-movimento *jogador* pos tabuleiro)))
        (imprime-tabuleiro novo-tabuleiro)
        (cond ((vencedor novo-tabuleiro) (format t "~&Você Ganhou!"))
              ((board-full-p novo-tabuleiro) (format t "~&Empate."))
              (t (movimento-pc novo-tabuleiro)))))

               ;Define o movimento do jogador
    (defun movimento-pc (tabuleiro)
      (let* ((best-move (escolhe-melhor-jogada tabuleiro))
             (pos (first best-move))
      
             (novo-tabuleiro (realiza-movimento *pc* pos tabuleiro)))
        (format t "~&Jogada do PC: ~S" pos)
   
        (imprime-tabuleiro novo-tabuleiro)
        (cond ((vencedor novo-tabuleiro) (format t "~&Computador Ganhou!"))
              ((board-full-p novo-tabuleiro) (format t "~&Empate."))
              (t (movimento-jogador novo-tabuleiro)))))

    (defun jogar ()   ;Definição da função jogar
      (if (y-or-n-p "Você gostaria de começar? ") ;Se digitar Y o jogador começa, se digitar N o pc começa
        (movimento-jogador (cria-tabuleiro))
        (movimento-pc (cria-tabuleiro))))

    (jogar);chama a função