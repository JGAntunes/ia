
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
  (cond ((null lista) t)
    ((pertence-lista (first lista) (restricao-variaveis restricao)) (and t (pertence-restricao restricao (rest lista))))
    (t nil)
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

(defun psr-atribuicoes-consistentes-arco-p (p variavel1 valor1 variavel2 valor2)
  (let ((restricoes (compara-listas (psr-restricoes p) (list variavel1 variavel2) #'pertence-restricao))
    (valor-original1 (psr-variavel-valor p variavel1))
    (valor-original2 (psr-variavel-valor p variavel2)))
    (psr-adiciona-atribuicao! p variavel1 valor1)
    (psr-adiciona-atribuicao! p variavel2 valor2)
    (multiple-value-bind (result-p result-n) (aplica-restricoes p restricoes 0)
      (cond ((null valor-original1) (psr-remove-atribuicao! p variavel1))
        ((not (equal valor1  valor-original1)) (psr-adiciona-atribuicao! p variavel1 valor-original1)))
      (cond ((null valor-original2) (psr-remove-atribuicao! p variavel2))
        ((not (equal valor2  valor-original2)) (psr-adiciona-atribuicao! p variavel2 valor-original2)))
      (values result-p result-n)
    )
  )
)

(defun cria-variavel (linha coluna)
  (concatenate 'string "l" (write-to-string linha) "c" (write-to-string coluna))
)

(defun cria-predicado (valor-restricao variaveis)
  (let ((max-num0 (- 9 valor-restricao))
    (max-num1 valor-restricao)
    (variaveis variaveis))
  #'(lambda (psr)
      (let ((num0 0)
        (num1 0)
        (valor nil))
        (dolist (variavel variaveis t)
          (setf valor (psr-variavel-valor psr variavel))
          (cond ((equal valor 0) (incf num0) (when (> num0 max-num0) (return nil)))
            ((equal valor 1) (incf num1) (when (> num1 max-num1) (return nil)))
          )
        )
      )
    )  
  )
)


(defun nova-restricao (linha coluna valor limite-linha limite-coluna)
  (let ((lista-vars ()))
    (do ((x  (- linha 1) (incf x)))
      ((> x (+ linha 1)) nil)
      (cond ((or (> linha  limite-linha) (< linha 0)) nil)
        (t (do ((y  (- coluna 1) (incf y)))
            ((> y (+ coluna 1)) nil)
            (cond ((or (> coluna  limite-coluna) (< coluna 0)) nil)
              (t (setf lista-vars (append lista-vars (list (cria-variavel x y)))))
            )
          )
        )
      ) 
    )
    (cria-restricao lista-vars (cria-predicado valor lista-vars))  
  )  
)

(defun fill-a-pix->psr (array)
  (let ((lista-vars ())
    (lista-restricoes ())
    (lista-dominios ()))
    (dotimes (x (array-dimension array 0) nil)
      (dotimes (y (array-dimension array 1) nil)
        (setf lista-vars (append lista-vars (list (cria-variavel x y))))
        (setf lista-dominios (append lista-dominios (list (list 0 1))))
        (when (aref array x y) (setf lista-restricoes (append lista-restricoes (list (nova-restricao x y (aref array x y) (array-dimension array 0) (array-dimension array 1))))))
      )
    )
    (cria-psr lista-vars lista-dominios lista-restricoes)
  )   
)

(defun psr->fill-a-pix (psr linhas colunas)
  (let ((array (make-array (list linhas colunas))))
    (dotimes (x linhas array)
      (dotimes (y colunas array)
        (setf (aref array x y) (psr-variavel-valor psr (cria-variavel x y)))
      )
    )
  )
)

;(defun retrocesso-simples (psr)
;  (let (()))
;  (cond ((psr-completo-p psr) RETURN STUFF)
;    (t (let ((variavel (first (psr-variaveis-nao-atribuidas psr))))
;      (dolist (valor (psr-variavel-dominio psr variavel) t)
;        (cond ((psr-atribuicao-consistente-p psr variavel valor) (psr-adiciona-atribuicao psr variavel valor) (retrocesso-simples psr)))
;      ))
;    )
;  )  
;)
;
;(defun procura-retrocesso-simples (psr)
;  
;)

;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;
;(load "testes publicos/test04/input")