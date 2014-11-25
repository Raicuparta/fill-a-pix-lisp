;; Grupo 62: 73639 Ricardo Lopes, 75646 Nuno Gomes, 76031 Leonor Bandeira

(load "exemplos.fas")

;###########################################################################
;	RESTRICAO
;###########################################################################

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


;###########################################################################
;	PSR
;###########################################################################


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
	(let ((vars-nao-atr NIL) 
		(vars-todas (psr-variaveis-todas psr)) 
		(vars-atr (psr-atributionVar psr)))
		(setf vars-nao-atr (set-difference vars-todas vars-atr :test #'equal))
	)
)

(defun psr-variavel-valor (psr var)
	(let ((value NIL)
		(l-vals (psr-atributionValue psr)) 
		(l-vars (psr-atributionVar psr)))

	(loop for var-atr in l-vars
		for val-atr in l-vals do
		(cond ((string= var var-atr) (setf value val-atr) (return 0)))
	) value)
)
	
(defun psr-variavel-dominio (psr var)
	(let ((domain NIL)
		(l-vars (psr-variaveis-todas psr)) 
		(l-doms (psr-domains psr)))
		
		(loop for v in l-vars
		for dom in l-doms do
			(cond ((string= var v) (setf domain dom) (return 0)))
		) domain)
)				

(defun psr-variavel-restricoes (psr var)
	(let ((l-restric NIL)
		(l-todas-restric (psr-restrictions psr)))
		(dolist (restric l-todas-restric)
			(dolist (restric-var (restricao-variaveis restric))
				(cond((string= var restric-var) (setf l-restric (append l-restric (list restric))))))) l-restric))
			

(defun psr-adiciona-atribuicao! (psr var value)
	(let ( 
		(l-vars (psr-atributionVar psr))
		(l-vals (psr-atributionValue psr))
		(i 0)
		(j NIL))
		(dolist (v l-vars)
				(cond ((string= var v) (setf (nth i l-vals) value) (setf j T) (return 0))
					(T (incf i))))

		(cond ((not j) (setf (psr-atributionVar psr) (append (psr-atributionVar psr) (list var))) 
						(setf (psr-atributionValue psr) (append (psr-atributionValue psr) (list value)))
						) ) NIL)
)
												
(defun psr-remove-atribuicao! (psr var)
	(let ( (l-vars (psr-atributionVar psr))
			(i 0))
			(dolist (v l-vars)
				(cond ((string= var v) (setf (psr-atributionVar psr) (remove-nth i (psr-atributionVar psr))) 
										(setf (psr-atributionValue psr)(remove-nth i (psr-atributionValue psr))))
					(T (incf i)))) NIL))
							
(defun psr-altera-dominio! (psr var dom)
	(let ((i 0)
		(l-vars (psr-variaveis-todas psr))
		(l-doms (psr-domains psr)))

		(dolist (v l-vars)
				(cond ((string= var v) (setf (nth i l-doms) dom) (return 0))
					(T (incf i))))
	NIL)
)
				

(defun psr-completo-p (psr)
	(cond((= (list-length (psr-variaveis-todas psr)) (list-length (psr-atributionVar psr))) T )
		(T NIL)))

(defun psr-consistente-p (psr)
	(let((l-restric (psr-restrictions psr))
		(i 0)
		(j T)
		(valido NIL))
		(dolist (restric l-restric)
			(setf valido (funcall(restricao-funcao-validacao restric) psr))
			(cond( (eq valido 1))
				((eq valido T) (incf i))
				(T (setf j NIL) (incf i) (return (values j i)))))
		
		(values j i)))
		

(defun psr-variavel-consistente-p (psr var)
	(let ((l-restric (psr-variavel-restricoes psr var))
		(i 0)
		(j T))
		(dolist (restric l-restric)
		(incf i)
			(cond( (funcall(restricao-funcao-validacao restric) psr))
					(T (setf j NIL) (return (values j i)))))
		
		(values j i)))
		
(defun psr-atribuicao-consistente-p (psr var val)
	(let ( (i NIL) (j NIL) (oldval (psr-variavel-valor psr var)))
		(psr-adiciona-atribuicao! psr var val)
		(setf i (values (psr-variavel-consistente-p psr var)))
		(setf j (nth-value 1 (psr-variavel-consistente-p psr var)))
		(cond ( (null oldval) (psr-remove-atribuicao! psr var) )
			(T (psr-adiciona-atribuicao! psr var oldval))) (values i j)))		  

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)
	(let ((l-restric NIL) (i NIL) (j NIL) (newpsr NIL))
		(setf l-restric (intersection (psr-variavel-restricoes psr var1) (psr-variavel-restricoes psr var2)))
		(setf newpsr (cria-psr (list var1 var2) (list (list val1) (list val2)) l-restric))
		(psr-adiciona-atribuicao! newpsr var1 val1)
		(psr-adiciona-atribuicao! newpsr var2 val2)
		(setf i (values (psr-consistente-p newpsr)))
		(setf j (nth-value 1 (psr-consistente-p newpsr)))
		(values i j)))

		
;###########################################################################
;	FUNCOES DE CONVERSAO
;###########################################################################		
		
(defun fill-a-pix->psr (arr)
	(let (
		(linhas (array-dimension arr 0))
		(colunas (array-dimension arr  1))
		(novo-psr NIL)
		(pos NIL)
		(vars NIL)
		(dom NIL)
		(pred NIL)
		(l-adjacencias NIL)
		(para-pintar 0)
		(restricoes NIL))
		(dotimes (l linhas)
			(dotimes (c colunas)
				(setf pos (concatenate 'string (write-to-string l) "_" (write-to-string c)))
				(setf dom (append dom (list (list 0 1))))
				(setf vars (append vars (list pos)))
				(setf para-pintar (aref arr l c))

				(unless(null para-pintar) 
						(setf l-adjacencias (lista-adjacencias pos linhas colunas))
						(setf pred (cria-predicado para-pintar l-adjacencias))
						(setf restricoes 
							(append restricoes 
								(list(cria-restricao 
									l-adjacencias
									pred)
								)
							)
						)
				)
			)
		)
		(setf novo-psr (cria-psr vars dom restricoes))
		novo-psr
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
				
				
;###########################################################################
;	FUNCOES DE PROCURA
;###########################################################################					
			
(defun procura-retrocesso-simples (psr)
	;;(print (psr-variaveis-nao-atribuidas psr))
	;;(print (desenha-fill-a-pix (psr->fill-a-pix psr 3 3)))
	(let ((var NIL) (r-testes 0) (retorno-atr NIL)
	(testes 0) (testes-atr NIL) 
	(consistente-atr NIL) 
	(resultado NIL) (lista NIL))
	(cond ( (psr-completo-p psr) (return-from procura-retrocesso-simples (values psr testes)))
		  (T (setf var (first (psr-variaveis-nao-atribuidas psr)))))
	(dolist (val (psr-variavel-dominio psr var))
		(setf retorno-atr (multiple-value-bind (resultado testes) (psr-atribuicao-consistente-p psr var val) (list resultado testes)))
		(setf consistente-atr (nth 0 retorno-atr))
		(setf testes-atr (nth 1 retorno-atr))
		(setf testes (+ testes testes-atr))
		(cond (consistente-atr 
					(psr-adiciona-atribuicao! psr var val)
					(setf lista (multiple-value-bind (resultado r-testes) (procura-retrocesso-simples psr)(list resultado r-testes)) )
					(setf resultado (nth 0 lista))
					(setf r-testes (nth 1 lista))
					(setf testes (+ testes r-testes))
					(cond (resultado (return-from procura-retrocesso-simples(values resultado testes)))
						  (T (psr-remove-atribuicao! psr var))
					))
		)
	)
	(values NIL testes)
	)
)


(defun procura-retrocesso-grau (psr)

	(let ((var NIL) (r-testes 0) (retorno-atr NIL)
	(testes 0) (testes-atr NIL) 
	(consistente-atr NIL) 
	(resultado NIL) (lista NIL))
	(cond ( (psr-completo-p psr) (return-from procura-retrocesso-grau (values psr testes)))
		  (T (setf var (variavel-maior-grau psr))  ))
	;;(format T "var: ") (prin1 var)
	;;(print (psr-variaveis-nao-atribuidas psr))
	;;(print 0)
	;;(desenha-fill-a-pix (psr->fill-a-pix psr 3 3))
	
	;;(cond ( (null var) (print "oi") ))
		  		
	(dolist (val (psr-variavel-dominio psr var))
		(setf retorno-atr (multiple-value-bind (resultado testes) (psr-atribuicao-consistente-p psr var val) (list resultado testes)))
		
		(setf consistente-atr (nth 0 retorno-atr))
		(setf testes-atr (nth 1 retorno-atr))
		(setf testes (+ testes testes-atr))
				
		(cond (consistente-atr 
					(psr-adiciona-atribuicao! psr var val)
					(setf lista (multiple-value-bind (resultado r-testes) (procura-retrocesso-grau psr)(list resultado r-testes)) )
					(setf resultado (nth 0 lista))
					(setf r-testes (nth 1 lista))
					(setf testes (+ testes r-testes))
					(cond (resultado (return-from procura-retrocesso-grau(values resultado testes)))
						  (T (psr-remove-atribuicao! psr var))
					))
		)
	)

	(values NIL testes)
	)
)




(defun resolve-simples (arr)
	(let ( (psr NIL) (newpsr NIL) (newarr NIL))
		(setf psr (fill-a-pix->psr arr ))
		(setf newpsr (procura-retrocesso-simples psr))
		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))))

(defun resolve-grau (arr)
	(let ( (psr NIL) (newpsr NIL) (newarr NIL))
		(setf psr (fill-a-pix->psr arr ))
		(setf newpsr (procura-retrocesso-grau psr))
		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))))
		

;###########################################################################
;	FUNCOES AUXILIARES
;###########################################################################	

; esta funcao vai remover um inteiro de uma lista no index n, juntando o resto da lista				  
(defun remove-nth (n l)
(loop for elt in l 
		for i from 0
		unless (= i n) collect elt))
		
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



(defun variavel-maior-grau (psr)
	(let ((l NIL) (variavel NIL) (grau1 0) (grau2 0) (value NIL) (l_vars_res NIL) (l_atribs NIL) (l_restricoes NIL))
		(setf l (psr-variaveis-nao-atribuidas psr))
		(setf l_atribs (psr-atributionVar psr))
		(setf variavel (first (psr-variaveis-nao-atribuidas psr)))

		(dolist (var_na l)
			(setf grau1 0)
			(setf l_restricoes (psr-variavel-restricoes psr var_na))
			(dolist (res l_restricoes)
				(setf l_vars_res (restricao-variaveis res))
				(cond ( (null (set-difference l_vars_res l_atribs :test #'equal)))
						 (T (incf grau1))
				)
			)
			(cond ((> grau1 grau2) (setf variavel var_na) (setf grau2 grau1) ))
		)
		(values variavel grau2)
	)
)	
			
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
