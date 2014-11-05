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
		 (j T))
		 (dolist (el l1)
			(incf i)
			(cond( (funcall(restricao-funcao-validacao el) psr))
				 (T (setf j NIL) (return (values j i)))))
		
		 (values j i)))
		 

(defun psr-variavel-consistente-p (psr var)
	(let ((l1 (psr-variavel-restricoes psr var))
		  (i 0)
		  (j T))
		  (dolist (el l1)
			(incf i)
			(cond( (funcall(restricao-funcao-validacao el) psr) )
				 (T (setf j NIL) (return (values j i)))))
		
		 (values j i)))
		
(defun psr-atribuicao-consistente-p (psr var val)
	(let ( (x NIL) (y NIL) (oldval (psr-variavel-valor psr var)))
		(psr-adiciona-atribuicao! psr var val)
		(setf x (values (psr-variavel-consistente-p psr var)))
		(setf y (nth-value 1 (psr-variavel-consistente-p psr var)))
		(cond ( (null oldval) (psr-remove-atribuicao! psr var) )
			  (T (psr-adiciona-atribuicao! psr var oldval))) (values x y)))		  

(defun psr-atribuicoes-consistentes-arco (psr var1 val1 var2 val2)
	(let ((l1 NIL) (x NIL) (y NIL) (newpsr NIL))
		  (setf l1 (intersection (psr-variavel-restricoes psr var1) (psr-variavel-restricoes psr var2)))
		  (setf newpsr (cria-psr (list var1 var2) (list (list val1) (list val2)) l1))
		  (psr-adiciona-atribuicao! newpsr var1 val1)
		  (psr-adiciona-atribuicao! newpsr var2 val2)
		  (setf x (values (psr-consistente-p newpsr)))
		  (setf y (nth-value 1 (psr-consistente-p newpsr)))
		  (values x y)))
		  
		  