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


(defstruct psr variables domains restrictions varsHash)

(defun cria-psr (vars doms restricts)
	(let ((dom_hash NIL) (var_hash NIL))
		(setf dom_hash (make-hash-table :test 'equal))
		(setf var_hash (make-hash-table :test 'equal))
		
		(loop 
			for var in vars
			for dom in doms
			do (setf (gethash var dom_hash) dom)
		)
	
		(make-psr 
			:variables vars
			:domains dom_hash
			:restrictions restricts
			:varsHash var_hash
		)
    )
)


(defun psr-atribuicoes (psr)
	;;(mapcar #'cons (psr-atributionVar psr) (psr-atributionValue psr))
	(let ((l_atribs NIL))
        (maphash #'(lambda (key val) 
            (setf l_atribs (cons (cons key val) l_atribs))) (psr-varsHash psr)
        )   
    l_atribs          
    )
)

		
(defun psr-variaveis-todas (psr)
	(psr-variables psr)
  	
)

(defun psr-variaveis-nao-atribuidas (psr)
	(let ((vars-nao-atr NIL) 
		(vars-todas (psr-variaveis-todas psr)) 
		(vars-atr NIL))
		(setf vars-atr (loop for key being the hash-keys of (psr-varsHash psr) collect key))
		(setf vars-nao-atr (set-difference vars-todas vars-atr :test #'equal))
	)

)

(defun psr-variavel-valor (psr var)
	;;(let ((value NIL)
	;;	(l-vals (psr-atributionValue psr)) 
	;;	(l-vars (psr-atributionVar psr)))

	;;(loop for var-atr in l-vars
	;;	for val-atr in l-vals do
	;;	(cond ((string= var var-atr) (setf value val-atr) (return 0)))
	;;) value)
	(nth-value 0 (gethash var (psr-varsHash psr)))
)
	
(defun psr-variavel-dominio (psr var)
	;(let ((domain NIL)
	;	(l-vars (psr-variaveis-todas psr)) 
	;	(l-doms (psr-domains psr)))
	;	
	;	(loop for v in l-vars
	;	for dom in l-doms do
	;		(cond ((string= var v) (setf domain dom) (return 0)))
	;	) domain)
	(nth-value 0 (gethash var (psr-domains psr)))
)								

(defun psr-variavel-restricoes (psr var)
	(let ((l-restric NIL)
		(l-todas-restric (psr-restrictions psr)))
		(dolist (restric l-todas-restric)
			(dolist (restric-var (restricao-variaveis restric))
				(cond((string= var restric-var) (setf l-restric (append l-restric (list restric))))))) l-restric))
			

(defun psr-adiciona-atribuicao! (psr var value)
	;;(let ( 
	;;	(l-vars (psr-atributionVar psr))
	;;	(l-vals (psr-atributionValue psr))
	;;	(i 0)
	;;	(j NIL))
	;;	(dolist (v l-vars)
	;;			(cond ((string= var v) (setf (nth i l-vals) value) (setf j T) (return 0))
	;;				(T (incf i))))
	;;
	;;	(cond ((not j) (setf (psr-atributionVar psr) (append (psr-atributionVar psr) (list var))) 
	;;					(setf (psr-atributionValue psr) (append (psr-atributionValue psr) (list value)))
	;;					) ) NIL)
	(setf (gethash var (psr-varsHash psr)) value)
	
)
												
(defun psr-remove-atribuicao! (psr var)
	;;(let ( (l-vars (psr-atributionVar psr))
	;;		(i 0))
	;;		(dolist (v l-vars)
	;;			(cond ((string= var v) (setf (psr-atributionVar psr) (remove-nth i (psr-atributionVar psr))) 
	;;									(setf (psr-atributionValue psr)(remove-nth i (psr-atributionValue psr))))
	;;				(T (incf i)))) NIL))

	(remhash var (psr-varsHash psr))

)
							
(defun psr-altera-dominio! (psr var dom)
	;(let ((i 0)
	;	(l-vars (psr-variaveis-todas psr))
	;	(l-doms (psr-domains psr)))
	;
	;	(dolist (v l-vars)
	;			(cond ((string= var v) (setf (nth i l-doms) dom) (return 0))
	;				(T (incf i))))
	;NIL)
	(setf (gethash var (psr-domains psr)) dom)
)				

(defun psr-completo-p (psr)
	;;(cond((= (list-length (psr-variaveis-todas psr)) (list-length (psr-atributionVar psr))) T )
	;;	(T NIL))

	(cond((= (list-length (psr-variaveis-todas psr)) (hash-table-count (psr-varsHash psr))) T )
		(T NIL))
)

(defun psr-consistente-p (psr)
	(let((l-restric (psr-restrictions psr))
		(i 0)
		(j T)
		(valido NIL))
		(dolist (restric l-restric)
			(setf valido (funcall(restricao-funcao-validacao restric) psr))
			;(cond( (eq valido 1))
				(cond ((eq valido T) (incf i))
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

(defun psr-atribuicoes-consistentes-arco-p (psr var1 valor1 var2 valor2)
	(let ((l-restric nil) (testes 0) (oldValue1 nil) (oldValue2 nil))
		
		(setf oldValue1 (psr-variavel-valor psr var1))
		(setf oldValue2 (psr-variavel-valor psr var2))
		(psr-adiciona-atribuicao! psr var1 valor1)
		(psr-adiciona-atribuicao! psr var2 valor2)
		
		(setf l-restric (psr-variavel-restricoes psr var1))
		
		(dolist (r l-restric)
			(cond ((member var2 (restricao-variaveis r) :test #'equal)
				(incf testes)
				(cond ( (not (funcall(restricao-funcao-validacao r) psr)) 
					(cond ((null oldValue1) (psr-remove-atribuicao! psr var1) )
					      (T (psr-adiciona-atribuicao! psr var1 oldValue1)))
					(cond ((null oldValue2) (psr-remove-atribuicao! psr var2) )
					      (T (psr-adiciona-atribuicao! psr var2 oldValue2)))
				
					(return-from psr-atribuicoes-consistentes-arco-p (values NIL testes))
				))
					
			      )
			
			)
			
		)
		(cond ((null oldValue1) (psr-remove-atribuicao! psr var1) )
			(T (psr-adiciona-atribuicao! psr var1 oldValue1)))
		(cond ((null oldValue2) (psr-remove-atribuicao! psr var2) )
			(T (psr-adiciona-atribuicao! psr var2 oldValue2)))
	
		(return-from psr-atribuicoes-consistentes-arco-p (values T testes))
		)
)

		
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
	(let ((posicoes NIL)
		  (l NIL)
		  (c NIL)
		  (arr (make-array (list linhas colunas))))
		  
		  (setf posicoes (mapcar #'car (psr-atribuicoes psr)))
		  (loop for pos in posicoes do
				(setf l (parse-integer (nth 0 (posicao-divide pos))))
				(setf c (parse-integer (nth 1 (posicao-divide pos))))
				(setf (aref arr l c) (psr-variavel-valor psr pos))) arr)) 
				
				
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


(defun procura-retrocesso-fc-mrv (psr)
	(let ((testes-totais 0) (var NIL) (dominio NIL) (testes 0) (testes2 0) (resultado NIL) (teste 0) (backup-dominio NIL) (inferencias NIL) (lista3 NIL) (consistente NIL) (lista2 NIL) (lista NIL))
		(cond ((psr-completo-p psr) (return-from procura-retrocesso-fc-mrv (values psr testes-totais))))
		(setf var (mrv psr))
		
		(setf dominio (psr-variavel-dominio psr var))
		(dolist (value dominio)

			(setf lista (multiple-value-bind (consistente teste) (psr-atribuicao-consistente-p psr var value)(list consistente teste)) )
			(setf consistente (nth 0 lista))
			(setf teste (nth 1 lista))
			;(cond ((null teste) (setf teste 0)))
			(setf testes-totais (+ testes-totais teste))
			(cond (consistente
					(psr-adiciona-atribuicao! psr var value)
					(setf lista2 (multiple-value-bind (inferencias testes) (forward-checking psr var)(list inferencias testes)) )
					(setf inferencias (nth 0 lista2))
					(setf testes (nth 1 lista2))
					(setf testes-totais (+ testes-totais testes))
					
					(cond ( (not (null inferencias))              ;(and (not (null inferencias)) (not (eq 0 (hash-table-count inferencias))))
						   (setf backup-dominio (copia-dominio psr inferencias))
						   (loop for key being the hash-keys of inferencias do
								(psr-altera-dominio! psr key (gethash key inferencias)))
						  
						   (setf lista3 (multiple-value-bind (resultado testes2) (procura-retrocesso-fc-mrv psr)(list resultado testes2)) )
						   (setf resultado (nth 0 lista3))
						   (setf testes2 (nth 1 lista3))
						   ;(cond ((null testes2) (setf testes2 0)))
						   (setf testes-totais (+ testes-totais testes2))
						   (cond (resultado
									(return-from procura-retrocesso-fc-mrv (values resultado testes-totais))))
							(loop for key being the hash-keys of backup-dominio do
											(psr-altera-dominio! psr key (gethash key backup-dominio)))))
					(psr-remove-atribuicao! psr var))))
		(values NIL testes-totais)
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
	(let ((l NIL) (variavel NIL) (grau1 0) (grau2 0) (l_vars_res NIL) (l_atribs NIL) (l_restricoes NIL))
		(setf l (psr-variaveis-nao-atribuidas psr))
		(setf l_atribs (mapcar #'car (psr-atribuicoes psr)))
		(setf variavel (first (psr-variaveis-nao-atribuidas psr)))

		(dolist (var_na l)
			(setf grau1 0)
			(setf l_restricoes (psr-variavel-restricoes psr var_na))
			(dolist (res l_restricoes)
				(setf l_vars_res (restricao-variaveis res))
				(cond ( (null (set-difference l_vars_res (cons var_na l_atribs) :test #'equal)))
						 (T (incf grau1))
				)
			)
			(cond ((> grau1 grau2) (setf variavel var_na) (setf grau2 grau1) ))
		)
		(values variavel grau2)
	)
)

(defun arcos-vizinhos-nao-atribuidos (psr var)
	(let ((lista-arcos NIL) (l_na NIL) (l_res NIL) (vars_res NIL))
		(setf l_res (psr-variavel-restricoes psr var))
		(setf l_na (psr-variaveis-nao-atribuidas psr))
		(dolist (var_na l_na)
			(cond ( (not(equal var var_na))
				(dolist (res l_res)
					(setf vars_res (restricao-variaveis res))
					(cond ( (member var_na vars_res :test #'equal) 
							(cond ((not(member (cons var_na var) lista-arcos :test #'equal))
								(setf lista-arcos (append lista-arcos (list (cons var_na var)))))
							
						)))))))
		lista-arcos
	)
)

(defun revise (psr var1 var2 inferencias)
	(let (
			(testes-totais 0)
			(valores NIL)
			(consistente NIL)
			(testes 0)
			(l_hash2 NIL)
			(foundConsistentValue NIL)
			(revised NIL)
			(dominio-var1 NIL)
			(var2-valor NIL)
			(dominio-var2 NIL)
			(novo-dominio-var1 NIL)
			(l_hash NIL)
		) 
		
		(setf l_hash (multiple-value-bind (value has-domain) (gethash var1 inferencias)(list value has-domain)) )
		
		(cond ((nth 1 l_hash) (setf dominio-var1 (nth 0 l_hash)))
			(T (setf dominio-var1 (psr-variavel-dominio psr var1))))
		
		(setf novo-dominio-var1 dominio-var1)
		(setf var2-valor (psr-variavel-valor psr var2))
		(cond (var2-valor (setf dominio-var2 (append dominio-var2 (list var2-valor))))
			  (T 
				(setf l_hash2 (multiple-value-bind (value has-domain) (gethash var2 inferencias)(list value has-domain)) )
				(cond ((nth 1 l_hash2) (setf dominio-var2 (nth 0 l_hash2)))
					  (T (setf dominio-var2 (psr-variavel-dominio psr var2)))))) 
		
		(dolist (d_value1 dominio-var1)
			(setf foundConsistentValue NIL)
			(dolist (d_value2 dominio-var2)
				(setf valores (multiple-value-bind (consistente testes) (psr-atribuicoes-consistentes-arco-p psr var1 d_value1 var2 d_value2)(list consistente testes)) )
				(setf consistente (nth 0 valores))
				(setf testes (nth 1 valores))
				;(cond ((null testes) (setf testes 0)))
				(setf testes-totais (+ testes-totais testes))
				(cond (consistente (setf foundConsistentValue T) (return 0))))
				
			(cond ((null foundConsistentValue) 
				(setf revised T) 
				(setf novo-dominio-var1 (remove d_value1 novo-dominio-var1 :test #'equal)))
			)
		)
		(cond (revised (setf (gethash var1 inferencias) novo-dominio-var1) ))
		(values revised testes-totais)
	)
)

(defun forward-checking (psr var)
	(let ((lista-arcos NIL) (dominio-v2 NIL) (testes-totais 0) (inferencias NIL) (testes 0) (revised NIL)(v1 NIL) (valores NIL)(v2 NIL) )
		(setf inferencias (make-hash-table :test 'equal))
		;(setf (gethash var inferencias) ())
		(setf lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
		(dolist (el lista-arcos)
			(setf v2 (car el))
			(setf v1 (cdr el))
			(setf valores (multiple-value-bind (revised testes) (revise psr v2 v1 inferencias)(list revised testes)) )
			(setf revised (nth 0 valores))
			(setf testes (nth 1 valores))
			;(cond ((null testes) (setf testes 0)))
			(setf testes-totais (+ testes-totais testes))
			(cond (revised
					(setf dominio-v2 (gethash v2 inferencias))
					(cond ((zerop (length dominio-v2)) (return-from forward-checking (values NIL testes-totais))))))
		)
		(values inferencias testes-totais)
	)
)

(defun copia-dominio (psr inferencias)
	(let ((backup NIL))
		(setf backup (make-hash-table :test 'equal))
		(loop for key being the hash-keys of inferencias do
			(setf (gethash key backup) (psr-variavel-dominio psr key)))
		backup
	)
)


(defun mrv (psr)
	(let ((l_vars NIL) (var NIL) (tamanho NIL) (v_tamanho NIL))
		(setf l_vars (psr-variaveis-nao-atribuidas psr))
		(setf var (first l_vars))
		(setf tamanho (list-length (psr-variavel-dominio psr var)))
		(dolist (v l_vars)
			(setf v_tamanho (list-length (psr-variavel-dominio psr v)))
			(cond ((> tamanho v_tamanho) (setf var v) (setf tamanho v_tamanho)))
		)
		var
	)
)





(defun procura-retrocesso-MAC-mrv (psr)
	(let ((testes-totais 0) (var NIL) (dominio NIL) (testes 0) (testes2 0) (resultado NIL) (teste 0) (backup-dominio NIL) (inferencias NIL) (lista3 NIL) (consistente NIL) (lista2 NIL) (lista NIL))
		(cond ((psr-completo-p psr) (return-from procura-retrocesso-MAC-mrv (values psr testes-totais))))
		(setf var (mrv psr))
		
		(setf dominio (psr-variavel-dominio psr var))
		(dolist (value dominio)

			(setf lista (multiple-value-bind (consistente teste) (psr-atribuicao-consistente-p psr var value)(list consistente teste)) )
			(setf consistente (nth 0 lista))
			(setf teste (nth 1 lista))
			;(cond ((null teste) (setf teste 0)))
			(setf testes-totais (+ testes-totais teste))
			(cond (consistente
					(psr-adiciona-atribuicao! psr var value)
					(setf lista2 (multiple-value-bind (inferencia testes) (mac psr var)(list inferencia testes)) )
					(setf inferencias (nth 0 lista2))
					(setf testes (nth 1 lista2))
					(setf testes-totais (+ testes-totais testes))
					
					(cond ( (not (null inferencias))              ;(and (not (null inferencias)) (not (eq 0 (hash-table-count inferencias))))
						   (setf backup-dominio (copia-dominio psr inferencias))
						   (loop for key being the hash-keys of inferencias do
								(psr-altera-dominio! psr key (gethash key inferencias)))
						   (setf lista3 (multiple-value-bind (resultado testes2) (procura-retrocesso-MAC-mrv psr)(list resultado testes2)) )
						   (setf resultado (nth 0 lista3))
						   (setf testes2 (nth 1 lista3))
						   (setf testes-totais (+ testes-totais testes2))
						   (cond (resultado
									(return-from procura-retrocesso-MAC-mrv (values resultado testes-totais))))
							(loop for key being the hash-keys of backup-dominio do
											(psr-altera-dominio! psr key (gethash key backup-dominio)))
							
							))
					(psr-remove-atribuicao! psr var))))
		(values NIL testes-totais)
	)
)




(defun mac (psr var)
	(let ((lista-arcos NIL) (testes-totais 0) (inferencias NIL)
			(testes 0) (cont NIL)
			;(v1 NIL) 
;; 			;(novos-arcos NIL)
			(valores NIL)
			)
			;(v2 NIL) 
			;(aux NIL)
			;(aux-testes 0)
			;(aux-revised NIL)
			;(l_hash NIL))
		(setf inferencias (make-hash-table :test 'equal))
		;(setf (gethash var inferencias) ())
		(setf lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
		;(setf valores (multiple-value-bind (revised testes) (iteracao psr lista-arcos inferencias)(list revised testes)) )
		;(setf revised (nth 0 valores))
		;(setf testes (nth 1 valores))
		;(cond ((null testes) (setf testes 0)))
		;(setf testes (iteracao psr lista-arcos inferencias))
		;(setf testes-totais (+ testes-totais testes))
		(loop do
			(setf valores (multiple-value-bind (inf revised testes) (iteracao psr lista-arcos inferencias)(list inf revised testes)) )
			(setf inferencias (nth 1 valores))
			(setf testes (nth 2 valores))
			(setf cont (nth 0 valores))
			(setf lista-arcos (nth 3 valores))
			(setf testes-totais (+ testes-totais testes))
			(cond ((not cont)
					(return-from mac (values NIL testes-totais))))
			while (not (null lista-arcos))
		)
;		(loop for el in lista-arcos do
;			(setf v2 (car el))
;			(setf v1 (cdr el))
;			(setf valores (multiple-value-bind (revised testes) (revise psr v2 v1 inferencias)(list revised testes)) )
;			(setf revised (nth 0 valores))
;			(setf testes (nth 1 valores))
;			;(cond ((null testes) (setf testes 0)))
;			(setf testes-totais (+ testes-totais testes))
;			(cond (revised
;					(setf l_hash (multiple-value-bind (value has-domain) (gethash v2 inferencias)(list value has-domain)))
;					(cond ((and (null (nth 0 l_hash)) (nth 1 l_hash)) (return-from mac (values NIL testes-totais))))
;		
;					(setf novos-arcos (arcos-vizinhos-nao-atribuidos psr v2))
;					(print "original") (prin1 novos-arcos) 
;					(print "vou remover") (prin1  (cons v1 v2))
;					(setf novos-arcos (remove (cons v1 v2) novos-arcos :test #'equal))
;					(print "pos remove") (prin1 novos-arcos) 
					;(setf lista-arcos (append lista-arcos novos-arcos))
					;(setf aux (multiple-value-bind (revised testes) (mac psr novos-arcos)(list revised testes)) )
					;(setf aux-revised (nth 0 valores))
					;(setf aux-testes (nth 1 valores))
					;(cond ((null testes) (setf testes 0)))
;					(setf testes-totais (+ testes-totais aux-testes))
;					(print "pos append") (prin1 lista-arcos) 
				
		
		(values inferencias testes-totais)
	)
)



(defun iteracao (psr lista-arcos inferencias)
	(let ((testes-totais 0) (testes 0) (revised NIL)(v1 NIL) 
			(novos-arcos NIL)
			(valores NIL)
			(v2 NIL) 
			;(aux 0)
			(arcos NIL)
			(lista lista-arcos)
			(inferencia inferencias)
			;(aux-testes 0)
			;(aux-revised NIL)
			(l_hash NIL))
		(dolist (el lista)
			(setf v2 (car el))
			(setf v1 (cdr el))
			(setf valores (multiple-value-bind (revised testes) (revise psr v2 v1 inferencia)(list revised testes)) )
			(setf revised (nth 0 valores))
			(setf testes (nth 1 valores))
			;(cond ((null testes) (setf testes 0)))
			(setf testes-totais (+ testes-totais testes))
			(cond (revised
				(setf l_hash (multiple-value-bind (value has-domain) (gethash v2 inferencia)(list value has-domain)))
				(cond ((and (null (nth 0 l_hash)) (nth 1 l_hash)) (return-from iteracao (values NIL inferencias testes-totais arcos))))
				(setf novos-arcos (arcos-vizinhos-nao-atribuidos psr v2))
;; 				(print "original") (prin1 novos-arcos) 
;; 				(print "vou remover") (prin1  (cons v1 v2))
				(setf novos-arcos (remove (cons v1 v2) novos-arcos :test 'equal))
;; 				(print "pos remove") (prin1 novos-arcos) 
				;(setf lista-arcos (append lista-arcos novos-arcos))
				(setf arcos (append arcos novos-arcos))
;; 				(print "pos append") (prin1 lista) )
			)
			
		))
				;(cond ((not(null novos-arcos))
				;(setf aux (iteracao psr novos-arcos inferencias) )
				;(cond ((null testes) (setf testes 0)))
				;(setf testes-totais (+ testes-totais aux))))
			(values T inferencias testes-totais arcos)
	)
)


;; (defun MAC(psr var)
;; 	(let ((testesTotais 0) 
;; 		   (inferencias (make-inferencia)) 
;; 		   (lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
;; 		   (aux NIL) 
;; 		   (repeat NIL)
;; 		 )
;; 		(loop do
;; 			(setf aux (multiple-value-list (ExpandsList psr lista-arcos inferencias)))
;; 			(setf repeat(nth 0 aux))
;; 			(setf testesTotais (+ testesTotais (nth 1 aux)))
;; 			(setf lista-arcos (nth 2 aux))
;; 			(setf inferencias (nth 3 aux))
;; 			(if (not repeat)
;; 				(return-from MAC (values nil testesTotais))
;; 			)
;; 			while(not (null lista-arcos))
;; 		) 
;; 		(values inferencias testesTotais)
;; 	)
;; )






















