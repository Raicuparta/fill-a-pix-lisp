;; Grupo 62: 73639 Ricardo Lopes, 75646 Nuno Gomes, 76031 Leonor Bandeira

(load "exemplos.fas")

(defstruct restriction variables pred)

(defun cria-restricao (vars p)
(make-restriction :variables vars :pred p)
)

(defun restricao-variaveis (r)
(restriction-variables r)
)

(defun restricao-funcao-validacao (r)
(restriction-pred r)
)



(defstruct psr variables domains restrictions atributionVar atributionValue)

(defun cria-psr (vars doms restricts)
(make-psr :variables vars :domains doms :restrictions restricts :atributionVar NIL :atributionValue NIL)
)

(defun psr-atribuicoes (psr)
	(mapcar #'cons (psr-atributionVar psr) (psr-atributionValue psr)))

		
(defun psr-variaveis-todas (psr)
(psr-variables psr)
)


(defun psr-variaveis-nao-atribuidas (psr)
	(let ((l1 NIL) 
		(l2 (psr-variaveis-todas psr)) 
		(l3 (psr-atributionVar psr)))
		(setf l1 (set-difference l2 l3 :test #'equal))
	)
)


(defun psr-variavel-valor (psr var)
	(let ((value NIL)
		(i 0)
		(l2 (psr-atributionValue psr)) 
		(l3 (psr-atributionVar psr)))
		(dolist (el l3)
			(cond ((string= var el) (setf value (nth i l2)) (return 0))
				(T (incf i)))) value))
				

(defun psr-variavel-dominio (psr var)
	(let ((d NIL)
		(i 0)
		(l2 (psr-variaveis-todas psr)) 
		(l3 (psr-domains psr)))
		(dolist (el l2)
			(cond ((string= var el) (setf d (nth i l3)) (return 0))
				(T (incf i)))) d))				

(defun psr-variavel-restricoes (psr var)
	(let ((r NIL)
		(l1 (psr-restrictions psr)))
		(dolist (e1 l1)
			(dolist (e2 (restricao-variaveis e1))
				(cond((string= var e2) (setf r(append r (list e1))))))) r))
			

(defun psr-adiciona-atribuicao! (psr var value)
	(let ( (l1 (psr-atributionVar psr))
		(i 0)
		(j NIL))
		(dolist (el l1)
				(cond ((string= var el) (setf (nth i (psr-atributionValue psr)) value) (setf j T) (return 0))
					(T (incf i))))
		(cond ((not j) (setf (psr-atributionVar psr) (append (psr-atributionVar psr) (list var))) 
						(setf (psr-atributionValue psr) (append (psr-atributionValue psr) (list value)))
						) ) NIL))
								
				
(defun psr-remove-atribuicao! (psr var)
	(let ( (l1 (psr-atributionVar psr))
			(i 0))
			(dolist (el l1)
				(cond ((string= var el) (setf (psr-atributionVar psr) (remove-nth i (psr-atributionVar psr))) 
										(setf (psr-atributionValue psr)(remove-nth i (psr-atributionValue psr))))
					(T (incf i)))) NIL))
					
; esta funcao vai remover um inteiro de uma lista no index n, juntando o resto da lista				  
(defun remove-nth (n l)
(loop for elt in l 
		for i from 0
		unless (= i n) collect elt))
		
		
(defun psr-altera-dominio! (psr var dom)
	(let ((i 0)
		(l1 (psr-variaveis-todas psr)))
		(dolist (el l1)
			(cond ((string= var el)(setf (nth i (psr-domains psr)) dom) (return 0))
				(T (incf i)))) NIL))
				

(defun psr-completo-p (psr)
	(cond((= (list-length (psr-variaveis-todas psr)) (list-length (psr-atributionVar psr))) T )
		(T NIL)))

(defun psr-consistente-p (psr)
	(let((l1 (psr-restrictions psr))
		(i 0)
		(j T)
		;(vars NIL)
		(valido NIL))
		(dolist (el l1)
			(setf valido (funcall(restricao-funcao-validacao el) psr))
			;(setf vars (restricao-variaveis el))
			;(dolist (var vars)
				;(cond ((null (psr-variavel-valor psr var)) (setf valido 1) (return 0))
				;		(T (setf valido (funcall(restricao-funcao-validacao el) psr)))))
			(cond( (eq valido 1))
				((eq valido T) (incf i))
				(T (setf j NIL) (incf i) (return (values j i)))))
		
		(values j i)))
		

(defun psr-variavel-consistente-p (psr var)
	(let ((l1 (psr-variavel-restricoes psr var))
		(i 0)
		(j T))
		;(vars NIL)
		;(valido NIL))
		(dolist (el l1)
		(incf i)
			;(setf vars (restricao-variaveis el))
			;(dolist (var vars)
			;	(cond ((null (psr-variavel-valor psr var)) (setf valido 1) (return 0) )
			;			(T (setf valido (funcall(restricao-funcao-validacao el) psr)))))
			;(cond((eq valido 1) (incf i) )
			;	((eq valido T) (incf i))
			;	(T (setf j NIL) (incf i) (return (values j i)))))
			
			(cond( (funcall(restricao-funcao-validacao el) psr))
					(T (setf j NIL) (return (values j i)))))
		
		(values j i)))
		
(defun psr-atribuicao-consistente-p (psr var val)
	(let ( (x NIL) (y NIL) (oldval (psr-variavel-valor psr var)))
		(psr-adiciona-atribuicao! psr var val)
		(setf x (values (psr-variavel-consistente-p psr var)))
		(setf y (nth-value 1 (psr-variavel-consistente-p psr var)))
		(cond ( (null oldval) (psr-remove-atribuicao! psr var) )
			(T (psr-adiciona-atribuicao! psr var oldval))) (values x y)))		  

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)
	(let ((l1 NIL) (x NIL) (y NIL) (newpsr NIL))
		(setf l1 (intersection (psr-variavel-restricoes psr var1) (psr-variavel-restricoes psr var2)))
		(setf newpsr (cria-psr (list var1 var2) (list (list val1) (list val2)) l1))
		(psr-adiciona-atribuicao! newpsr var1 val1)
		(psr-adiciona-atribuicao! newpsr var2 val2)
		(setf x (values (psr-consistente-p newpsr)))
		(setf y (nth-value 1 (psr-consistente-p newpsr)))
		(values x y)))

;recebe uma posicao e o tamanho do tabuleiro, devolve uma lista com todos os adjacentes
;o argumento posicao esta no formato "linha_coluna"
(defun lista-adjacencias (posicao linhas colunas)
	(let ((l (parse-integer (nth 0 (posicao-divide posicao))))
		  (c (parse-integer (nth 1 (posicao-divide posicao))))
		  (lista NIL)
		  (newl NIL)
		  (newc NIL))
		  (dotimes (i 3)
			(dotimes (j 3)
				(setf newl (+ l(1- i)))
				(setf newc (+ c(1- j)))
				(if (and (>= newl 0) (< newl linhas) (>= newc 0) (< newc colunas)) 
						(setf lista (append lista (list (concatenate 'string (write-to-string newl) "_" (write-to-string newc)))))))) lista ))

;recebe um psr e uma lista de adjacencias e conta as posicoes pintadas
(defun conta-pintados (psr l)
	(let ((i 0) (j 0) (value 0))
		(dolist (el l)
			(setf value (psr-variavel-valor psr el))
			(if (eq value NIL) (setf j(1+ j )) (setf i(+ i value))) 
			
		)
		(list i j)
	)
)		
		
		
		
		
		
(defun fill-a-pix->psr (arr)
	(let (
		(linhas (array-dimension arr 0))
		(colunas (array-dimension arr  1))
		(novo-psr NIL)
		(pos NIL)
		(vars NIL)
		(dom NIL)
		(pred NIL)
		(l1 NIL)
		(int 0)
		(restricoes NIL))
		(dotimes (l linhas)
			(dotimes (c colunas)
				(setf pos (concatenate 'string (write-to-string l) "_" (write-to-string c)))
				(setf dom (append dom (list (list 0 1))))
				(setf vars (append vars (list pos)))
				(setf l1 (lista-adjacencias pos linhas colunas))
				(setf int (aref arr l c))
				(setf pred (cria-predicado int l1))
				(setf restricoes 
					(append restricoes 
						(list(cria-restricao 
							(list pos)
							pred)
						)
					)
				)
			)
		)
		(setf novo-psr (cria-psr vars dom restricoes))
		novo-psr
	)
)


(defun cria-predicado (n l) 
	(let ((l1 l) (int n))
	#'(lambda (psr) (let ((x (conta-pintados psr l1)) (y int)) (if (null y) 1 (and (<= (- y (nth 0 x)) (nth 1 x)) (<= (nth 0 x) y))) ))
	)
)


(defun posicao-divide (string)
	(loop for start = 0 then (1+ finish)
        for finish = (position #\_ string :start start)
        collecting (subseq string start finish)
        until (null finish)
    )
)


(defun psr->fill-a-pix (psr linhas colunas)
	(let ((posicoes (psr-atributionVar psr))
		  (valores (psr-atributionValue psr))
		  (l NIL)
		  (c NIL)
		  (arr (make-array (list linhas colunas))))
		  (loop for pos in posicoes
				for val in valores do
				(setf l (parse-integer (nth 0 (posicao-divide pos))))
				(setf c (parse-integer (nth 1 (posicao-divide pos))))
				(setf (aref arr l c) val)) arr)) 

				
				
(defun procura-retrocesso-simples (psr)
	(let ((var NIL) (r-testes 0) (testes 0) (testes-atr NIL) (testes-psr NIL) (consistente-atr NIL) (consistente-psr NIL)(resultado NIL) (lista NIL))
	(cond ( (psr-completo-p psr) (return-from procura-retrocesso-simples (values psr testes)))
		  (T (setf var (first (psr-variaveis-nao-atribuidas psr)))))
	(dolist (val (psr-variavel-dominio psr var))
		(setf consistente-atr (values (psr-atribuicao-consistente-p psr var val)))
		(setf testes-atr (nth-value 1 (psr-atribuicao-consistente-p psr var val)))
		(setf testes (+ testes testes-atr))
		;(print testes) (prin1 "primeiro")
		(cond (consistente-atr 
					(psr-adiciona-atribuicao! psr var val)
					(setf lista (multiple-value-bind (resultado r-testes) (procura-retrocesso-simples psr)(list resultado r-testes)) )
					;(print (nth-value 1 resultado)) (prin1 "estupida")
					(setf resultado (nth 0 lista))
					(setf r-testes (nth 1 lista))
					(setf testes (+ testes r-testes))
					(cond ((null resultado) (setf consistente-psr NIL)(setf testes-psr 0)) 
						  (T (setf consistente-psr (values (psr-consistente-p resultado)))
							 (setf testes-psr (nth-value 1 (psr-consistente-p resultado)))))
					;(setf testes (+ testes testes-psr))
					;(print testes) (prin1 "segundo")
					(cond (consistente-psr (return-from procura-retrocesso-simples(values resultado testes)))
						  (T (psr-remove-atribuicao! psr var))
					))
		)
	)
	(values NIL testes)
	)
)