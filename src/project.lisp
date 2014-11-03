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

(defun valor-lista-pares (variavel lista-pares)
  	(cond ((null lista-pares) nil)
        ((equal variavel (first (car lista-pares))) (first (cdr lista-pares)))
        (t (valor-lista-pares variavel (rest lista-pares)))
	)
)

(defun pertence-lista (variavel lista)
  	(cond ((null lista) nil)
        ((equal variavel (first lista)) t)
        (t (pertence-lista variavel (rest lista)))
	)
)

(defun compara-listas (lista1 lista2 predicado)
	(cond ((null lista1) nil)
    	((funcall predicado (first lista1) lista2) (append (first lista1) (compara-listas (rest lista1) lista2 predicado)))
     	(t (compara-listas (rest lista1) lista2 predicado))
    )
)

(defun mesmo-elemento (lista-teste lista-percorre variavel)
	(cond ((null lista-teste) nil)
    	((equal variavel (first lista-teste)) (first lista-percorre))
     	(t (mesmo-elemento (rest lista-teste) (rest lista-percorre) variavel))
    )  
)

;(defun adiciona-lista-pares (variavel valor lista-pares)
;  	(let ())
;  	(cond ((null lista-pares) (setf lista_pares (append lista_pares (list (cons variavel valor))))
;        ((equal variavel (first (car lista-pares))) (setf (first (cdr lista-pares)))
;        (t (valor-lista-pares variavel (rest lista-pares)))
;	)
;)

(defun psr-variaveis-nao-atribuidas (p)
  	(compara-listas (psr-variaveis-todas p) (psr-atribuicoes p) #'valor-lista-pares)
)

(defun psr-variavel-valor (p variavel)
	(valor-lista-pares variavel (psr-atribuicoes p))  
)

(defun psr-variavel-dominio (p variavel)
	(mesmo-elemento (psr-variaveis-todas p) (psr-dominio p) variavel)
)

(defun psr-variavel-restricoes (p variavel)
	(compara-listas (psr-restricoes p) (list variavel) #'pertence-lista)  
)

;(defun psr-adiciona-atribuicao! (p variavel valor)
;	()  
;)