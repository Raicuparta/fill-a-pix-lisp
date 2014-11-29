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


(defstruct psr variables domains restrictions varsHash hash-restricoes )

(defun cria-psr (vars doms restricoes)
	(let ((dom_hash NIL) (var_hash NIL) (hashrestricoes (make-hash-table :test 'equal)))
		(setf dom_hash (make-hash-table :test 'equal))
		(setf var_hash (make-hash-table :test 'equal))
				

		(loop 
			for var in vars
			for dom in doms
			do (setf (gethash var dom_hash) dom)
		)
		(dotimes (i (length restricoes)) 
			(dolist (el (restricao-variaveis (nth i restricoes)))
				(setf (gethash el hashrestricoes) (append (gethash el hashrestricoes) (list (nth i restricoes))))
			)
		)
	
		(make-psr 
			:variables vars
			:domains dom_hash
			:restrictions restricoes
			:varsHash var_hash
			:hash-restricoes hashrestricoes 
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

(defun psr-variavel-restricoes (p var)
	(first (list(gethash var (psr-hash-restricoes p))))
)			

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
		


		
(defun psr-atribuicao-consistente-p (psr var val)
	(let ( (i NIL) (j NIL) (oldval (psr-variavel-valor psr var)))
		(psr-adiciona-atribuicao! psr var val)
		(setf i (values (psr-variavel-consistente-p psr var)))
		(setf j (nth-value 1 (psr-variavel-consistente-p psr var)))
		(cond ( (null oldval) (psr-remove-atribuicao! psr var) )
			(T (psr-adiciona-atribuicao! psr var oldval))) (values i j)))		  

(defun psr-atribuicoes-consistentes-arco-p (p var1 val1 var2 val2)
		(let ((oldval1 (psr-variavel-valor p var1))
			  (oldval2 (psr-variavel-valor p var2))
			  (listaRestrvar1 (psr-variavel-restricoes p var1))
			  (listaRestrvar2 (psr-variavel-restricoes p var2))
			  (i 0) (bool T) (inter NIL))
			(setf inter (intersection listaRestrvar1 listaRestrvar2))
			(psr-adiciona-atribuicao! p var1 val1)
			(psr-adiciona-atribuicao! p var2 val2)
			(dotimes (el (length inter))
				(incf i)
				(cond ((not(funcall (restricao-funcao-validacao (nth el inter)) p))
						(setq bool nil) (return))
						(T (setq bool T))
				)
			)
			(if (null oldval1) (psr-remove-atribuicao! p var1)
				(psr-adiciona-atribuicao! p var1 oldval1)
			)
			(if (null oldval2) (psr-remove-atribuicao! p var2)
				(psr-adiciona-atribuicao! p var2 oldval2)
			)
			(values bool i)
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


;copia as inferencias para o psr1
(defun copia-dominios (p inferencias)
   (maphash #'(lambda (key value) (psr-altera-dominio! p key value))  inferencias)
)

;copia dom2 para dom1 caso n seja 0

(defun backup (p inferencias)
  (let ((backup (make-hash-table :test 'equal)))
    (maphash #'(lambda (key value) (setf value value) (setf (gethash key backup) (psr-variavel-dominio p key)))  inferencias)
    backup    
  )
)

(defun procura-retrocesso-fc-mrv(p)
    (let ((testesTotais 0) (var nil)(resultadoFinal nil)(backup-dominios nil))
      (cond ((psr-completo-p p) (setf resultadoFinal p))
            (T (setf var (mrv p))
              (dolist (valor (psr-variavel-dominio p var))
                (let ((consistente nil)(testes 0)(inferencias nil)(resultado nil))
                  (setf (values consistente testes)  (psr-atribuicao-consistente-p p var valor))
                  (setf testesTotais (+ testesTotais testes))
                  (cond (consistente (psr-adiciona-atribuicao! p var valor) ;(print "Add")(print var) (print valor)
                                    (setf (values inferencias testes)  (forward-checking p var))
                                    (setf testesTotais (+ testesTotais testes))
                                    (cond (inferencias 
                                          (setf backup-dominios (backup p inferencias))
                                          (copia-dominios p inferencias)
                                          (setf (values resultado testes)  (procura-retrocesso-fc-mrv p))
                                          (setf testesTotais (+ testesTotais testes))
                                          (cond (resultado (setf resultadoFinal resultado)(return)))
                                          (copia-dominios p backup-dominios)
                                          )
                                          
                                    )
                                    (psr-remove-atribuicao! p var)  ;(print "Remove")(print var) (print valor)
                      )
                  )
                ) 
              )
            )
          
      )
  (values resultadoFinal testesTotais)
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
	

(defun resolve-fc (arr)
	(let ( (psr NIL) (newpsr NIL) (newarr NIL))
		(setf psr (fill-a-pix->psr arr ))
		(setf newpsr (procura-retrocesso-fc-mrv psr))
		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))))	

(defun resolve-mac (arr)
	(let ( (psr NIL) (newpsr NIL) (newarr NIL))
		(setf psr (fill-a-pix->psr arr ))
		(setf newpsr (procura-retrocesso-MAC-mrv psr))
		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))))

;; (defun resolve-best (arr)
;; 	(let ( (psr NIL) (newpsr NIL) (newarr NIL))
;; 		(setf psr (fill-a-pix->psr arr ))
;; 		(setf newpsr (procura-retrocesso-MAC-mrv psr))
;; 		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))))
;; 		
		
		
		
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
	(let ((i 0) (j 0) (value 0) (l_nil NIL))
		(dolist (el l)
			(setf value (psr-variavel-valor psr el))
			(cond ((eq value NIL) 
					(setf j(1+ j )) (setf l_nil (append l_nil (list el))) )
				(T	(setf i(+ i value)))) 
			
		)
		(list i j l_nil)
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

(defun isIn (p var1 var2)
  (let((bool nil))
    (cond ((psr-variavel-valor p var2))
          (t (dolist (restricao (psr-variavel-restricoes p var1))
                (dolist (restrvar (restricao-variaveis restricao))
                  (cond((equal restrvar var2) (setf bool T)
                    (return T)))))
          )
    )
  bool
  )
)


(defun arcos-vizinhos-nao-atribuidos(p var)
  (let((lista-arcos nil) 
      ;(lista-restricoes-var (psr-variavel-restricoes p var))
      )
      
  (dolist (var-natribuida (psr-variaveis-nao-atribuidas p)) 
    (cond((not(equal var var-natribuida))
            (if (isIn p var var-natribuida)
              (setf lista-arcos (append lista-arcos (list (cons var-natribuida var))))
            )
            
          )
    )
  )
  
  lista-arcos
 )
)

(defun revise (psr x y inferencias)
  (let ((testesTotais 0)
        (revised nil)
        (dominio-x (gethash x inferencias))
        (dominio-y (list (psr-variavel-valor psr y)))
        (novo-dominio-x nil))
        (if(null dominio-x)
          (setf dominio-x (psr-variavel-dominio psr x))
          
        )
        (setf novo-dominio-x dominio-x)

        (cond((null (first dominio-y))
                (setf dominio-y (gethash y inferencias))
                (cond ((null dominio-y)
                  (setf dominio-y (psr-variavel-dominio psr y)) )
                )
              )
        )
        (dolist (valx dominio-x)
          (let ((foundConsistentValue nil) (consistente nil) (testes 0))
            (dolist (valy dominio-y)
              (setf (values consistente testes)  (psr-atribuicoes-consistentes-arco-p psr x valx y valy))
              (setf testesTotais (+ testesTotais testes))
              (cond (consistente (setf foundConsistentValue T) (return)))
            )
            (cond ((null foundConsistentValue) (setf revised T) (setf novo-dominio-x (remove valx novo-dominio-x :test #'equal))))
          )
        )
        (if revised (setf (gethash x inferencias) novo-dominio-x))
    (values revised testesTotais)  
  )
)

(defun forward-checking(p var)
  (let ((testesTotais 0) 
      (inferencias (make-hash-table :test 'equal)) 
      (lista-arcos (arcos-vizinhos-nao-atribuidos p var))
      (revised nil)
      (testes 0)
      (bool T))
      

      (dolist (arco lista-arcos)
        ;(print inferencias)
        (setf (values revised testes)  (revise p (car arco) (cdr arco) inferencias))
        (setf testesTotais (+ testesTotais testes))
        (cond (revised (cond ((= (length (gethash (car arco) inferencias)) 0)
                              (setf bool nil) (return)
                            )
                      )
              )
        )
      )
    (if bool (values inferencias testesTotais)
              (values nil testesTotais)
    )
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


(defun mrv (p)
  (let* ((var-n-atribuidas (psr-variaveis-nao-atribuidas p))
      (minvalue (length (psr-variavel-dominio p (first var-n-atribuidas))))
      (value 0)
      (minvar (first var-n-atribuidas))
      )
    (dolist (var var-n-atribuidas) 
        (setf value (length (psr-variavel-dominio p var)))
        (cond ((< value minvalue)(setf minvar var)(setf minvalue value))
        )
    )
  minvar 
  )
)


(defun procura-retrocesso-MAC-mrv (psr)
	(let ((testes-totais 0) (var NIL) (dominio NIL) (testes 0) (testes2 0) (resultado NIL) (teste 0) (backup-dominio NIL) (inferencias NIL) (lista3 NIL) (consistente NIL) (lista2 NIL) (lista NIL))
		(cond ((psr-completo-p psr) (return-from procura-retrocesso-MAC-mrv (values psr testes-totais))))
		(setf var (mrv psr))
		(print (psr->fill-a-pix psr 20 20))
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
			(testes 0) (revised NIL)(v1 NIL) (novos-arcos NIL)
			(valores NIL)(v2 NIL) (l_hash NIL))
		(setf inferencias (make-hash-table :test 'equal))
		;(setf (gethash var inferencias) ())
		(setf lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
		
		(loop while lista-arcos do
			(setf v2 (car (first lista-arcos)))
			(setf v1 (cdr (first lista-arcos)))
			(setf lista-arcos (rest lista-arcos))
			(setf valores (multiple-value-bind (revised testes) (revise psr v2 v1 inferencias)(list revised testes)) )
			(setf revised (nth 0 valores))
			(setf testes (nth 1 valores))
			;(cond ((null testes) (setf testes 0)))
			(setf testes-totais (+ testes-totais testes))
			(cond (revised
					(setf l_hash (multiple-value-bind (value has-domain) (gethash v2 inferencias)(list value has-domain)))
					(cond ((and (null (nth 0 l_hash)) (nth 1 l_hash)) (return-from mac (values NIL testes-totais))))
		
					(setf novos-arcos (arcos-vizinhos-nao-atribuidos psr v2))
					(setf novos-arcos (remove (cons v1 v2) novos-arcos :test #'equal))
					(setf lista-arcos (append lista-arcos novos-arcos))
				)
			)
		
		)
		
		(values inferencias testes-totais)
	)
)






(defun preenche-tudo (psr arr)
	(let ((l_na NIL) (linhas 0) (colunas 0) (l 0) (c 0) (posicao NIL) (nr_var NIL))
		(setf l_na (psr-variaveis-nao-atribuidas psr))
		(setf linhas (array-dimension arr 0))
		(setf colunas (array-dimension arr  1))	
		(dolist (var l_na)
			(setf posicao (posicao-divide var))
			(setf l (parse-integer (nth 0 posicao)))
			(setf c (parse-integer (nth 1 posicao)))
			(setf nr_var (aref arr l c))
			(cond ((eq nr_var 0) 
						(dolist (adj (lista-adjacencias var linhas colunas))
							(psr-adiciona-atribuicao! psr adj 0)
						)
						;(setf (aref arr l c) -1)
				  )
				  ((eq nr_var 9) 
						(dolist (adj (lista-adjacencias var linhas colunas))
							(psr-adiciona-atribuicao! psr adj 1)
						)
						;(setf (aref arr l c) -1)
				  )
				  ((eq nr_var 6) 
						(cond ((or (eq c 0) (eq l 0) (eq c (1- colunas)) (eq l (1- linhas)))
								(dolist (adj (lista-adjacencias var linhas colunas))
									(psr-adiciona-atribuicao! psr adj 1)
								)
								;(setf (aref arr l c) -1)
							)
						)
				  )
				  ((eq nr_var 4) 
						(cond ((or (and (eq c 0)(eq l 0)) (and (eq c (1- colunas)) (eq l (1- linhas))) (and (eq c 0)(eq l (1- linhas))) (and (eq c (1- colunas)) (eq l 0)))
								(dolist (adj (lista-adjacencias var linhas colunas))
									(psr-adiciona-atribuicao! psr adj 1)
								)
								;(setf (aref arr l c) -1)
							)
						)
				)
			)
		) 
		psr
	)
)
	
(defun preenche-resto (psr arr)
	(let ((l_vars NIL) (linhas 0) (colunas 0) (posicao 0) (l 0) (c 0) (nr_var NIL) (l_adj NIL) (pintados NIL) )
		(setf l_vars (psr-variaveis-todas psr))
		(setf linhas (array-dimension arr 0))
		(setf colunas (array-dimension arr  1))	
		(dolist (var l_vars)
			(setf posicao (posicao-divide var))
			(setf l (parse-integer (nth 0 posicao)))
			(setf c (parse-integer (nth 1 posicao)))
			(setf nr_var (aref arr l c))
			(cond ((and nr_var (not (eq nr_var 9)) (not (eq nr_var 0)))
					(setf l_adj (lista-adjacencias var linhas colunas))
					(setf pintados (conta-pintados psr l_adj))
					(cond ( (and (not (eq (nth 1 pintados) 0)) (eq (nth 0 pintados) nr_var))
							(dolist (v_nil (nth 2 pintados))
								(psr-adiciona-atribuicao! psr v_nil 0)
							)
							;(setf (aref arr l c) -1)
						  )
					)
					(cond ( (and (not(eq (nth 1 pintados) 0)) (= (+(nth 1 pintados)(nth 0 pintados)) nr_var) )
							(dolist (v_nil (nth 2 pintados))
								(psr-adiciona-atribuicao! psr v_nil 1)
							)
							;(setf (aref arr l c) -1)
						  )
					)
							
				 )
			)
		)
		psr
	)
)
			
		

			
(defun resolve-best (arr)
	(let ( (psr NIL)(newpsr NIL)(l_actual NIL) (l_antiga NIL) (newarr NIL))
		(setf psr (fill-a-pix->psr arr ))
	;	(setf psr (preenche-tudo psr arr))
		(setf l_actual (psr-atribuicoes psr))

;;    		(loop while (not (equal l_actual l_antiga)) do
;;   			(setf l_antiga (psr-atribuicoes psr))
;;   			(setf psr (preenche-resto psr arr))
;;   			(setf l_actual (psr-atribuicoes psr))
;;  			;;(print (psr->fill-a-pix psr (array-dimension arr 0)(array-dimension arr 1)))
;;  
;;  		)

 		(setf psr (preenche-resto psr arr))
;		(print (psr->fill-a-pix psr (array-dimension arr 0)(array-dimension arr 1)))
		(setf newpsr (procura-retrocesso-fc-mrv psr))
		(setf newarr (psr->fill-a-pix newpsr (array-dimension arr 0)(array-dimension arr 1)))
	)
)















