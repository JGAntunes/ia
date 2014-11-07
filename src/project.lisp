
;;;;;;;;;;;;;EXEMPLOS;;;;;;;;;;;;;
(load "exemplos.fas")
;(load (compile-file "testes publicos/exemplos.lisp"))

(defstruct (restricao 
            (:constructor cria-restricao (variaveis funcao-validacao)))
	variaveis
 	funcao-validacao
)

(defstruct (psr 
            (:constructor cria-psr (variaveis-todas dominio restricoes)))
	variaveis-todas
	dominio
 	restricoes
  atribuicoes
)


; devolve valor do par cuja variavel e = a variavel de input
(defun valor-lista-pares (variavel lista-pares)
  	(cond ((null lista-pares) nil)
        ((equal variavel (car (first lista-pares))) (cdr (first lista-pares)))
        (t (valor-lista-pares variavel (rest lista-pares)))
	)
)

(defun not-lista-pares (variavel lista-pares)
    (cond ((null lista-pares) t)
        ((equal variavel (car (first lista-pares))) nil)
        (t (not-lista-pares variavel (rest lista-pares)))
  )
)

; verifica se a variavel pertence a lista
(defun pertence-lista (variavel lista)
  	(cond ((null lista) nil)
        ((equal variavel (first lista)) t)
        (t (pertence-lista variavel (rest lista)))
	)
)

(defun pertence-restricao (restricao lista)
  (cond ((null lista) nil)
      ((pertence-lista (first lista) (restricao-variaveis restricao)) t)
      (t (pertence-restricao restricao (rest lista)))
  )
)

;compara 2 listas atraves de um predicado. A lista devolvida e o resultado da aplicacao do predicado aos elementos da lista1
;sobre a lista2. Sempre que este falhe os elementos nao sao incluidos na lista de resultado
(defun compara-listas (lista1 lista2 predicado)
	(cond ((null lista1) nil)
    	((funcall predicado (first lista1) lista2) (append (list (first lista1)) (compara-listas (rest lista1) lista2 predicado)))
     	(t (compara-listas (rest lista1) lista2 predicado))
    )
)

;percorre duas listas em simultaneo devolvendo um valor da segunda sempre que encontrar a variavel na primeira
(defun mesmo-elemento (lista-teste lista-percorre variavel)
	(cond ((null lista-teste) nil)
    	((equal variavel (first lista-teste)) (first lista-percorre))
     	(t (mesmo-elemento (rest lista-teste) (rest lista-percorre) variavel))
    )  
)

;igual a de cima, com substituicao caso encontre
(defun adiciona-mesmo-elemento (lista-teste lista-percorre variavel valor)
	(cond ((null lista-teste) nil)
    	((equal variavel (first lista-teste)) (append (list valor) (rest lista-percorre)))
     	(t (append (list (first lista-percorre)) (adiciona-mesmo-elemento (rest lista-teste) (rest lista-percorre) variavel valor)))
    )  
)

;adiciona o par variavel valor a lista de pares, ou susbtitui caso a variavel ja esteja atribuida
(defun adiciona-lista-pares (variavel valor lista-pares)
  	(cond ((null lista-pares) (list (cons variavel valor)))
        ((equal variavel (car (first lista-pares))) (append (list (cons variavel valor)) (rest lista-pares)))
        (t (append (list (first lista-pares)) (adiciona-lista-pares variavel valor (rest lista-pares))))
	)
)

;remove o par cujo primeiro elemento e igual a variavel dada
(defun remove-lista-pares (variavel lista-pares)
  	(cond ((null lista-pares) nil)
        ((equal variavel (car (first lista-pares))) (rest lista-pares))
        (t (append (list (first lista-pares)) (remove-lista-pares variavel (rest lista-pares))))
	)
)

(defun aplica-restricoes (p lista-restricoes count)
    (cond ((null lista-restricoes) (values t count))
        ((funcall (restricao-funcao-validacao (first lista-restricoes)) p) (aplica-restricoes p (rest lista-restricoes) (incf count)))
        (t (values nil (incf count)))
    )
)

(defun psr-variaveis-nao-atribuidas (p)
  (compara-listas (psr-variaveis-todas p) (psr-atribuicoes p) #'not-lista-pares)
)

(defun psr-variavel-valor (p variavel)
	(valor-lista-pares variavel (psr-atribuicoes p))  
)

(defun psr-variavel-dominio (p variavel)
	(mesmo-elemento (psr-variaveis-todas p) (psr-dominio p) variavel)
)

(defun psr-variavel-restricoes (p variavel)
	(compara-listas (psr-restricoes p) (list variavel) #'pertence-restricao)
)

(defun psr-adiciona-atribuicao! (p variavel valor)
	(setf (psr-atribuicoes p) (adiciona-lista-pares variavel valor (psr-atribuicoes p)))  
)

(defun psr-remove-atribuicao! (p variavel)
	(setf (psr-atribuicoes p) (remove-lista-pares variavel (psr-atribuicoes p)))  
)

(defun psr-altera-dominio! (p variavel dominio)
	(setf (psr-dominio p) (adiciona-mesmo-elemento (psr-variaveis-todas p) (psr-dominio p) variavel dominio))  
)

(defun psr-completo-p (p)
	(not (psr-variaveis-nao-atribuidas p))  
)

(defun psr-consistente-p (p)
  (aplica-restricoes p (psr-restricoes p) 0)
)

(defun psr-variavel-consistente-p (p variavel)
  (aplica-restricoes p (psr-variavel-restricoes p variavel) 0)
)

(defun psr-atribuicao-consistente-p (p variavel valor)
    (let ((valor-original (psr-variavel-valor p variavel)))
          (psr-adiciona-atribuicao! p variavel valor)
          (multiple-value-bind (result-p result-n) (psr-variavel-consistente-p p variavel)
            (cond ((null valor-original) (psr-remove-atribuicao! p variavel))
                  ((not (equal valor  valor-original)) (psr-adiciona-atribuicao! p variavel valor-original)))
            (values result-p result-n)
          )
    )
)

;(defun psr-atribuicoes-consistentes-arco-p (p variavel1 valor1 variavel2 valor2) ())

;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;
;(load "testes publicos/test04/input")