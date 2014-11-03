;; Grupo 62: 73639 Ricardo Lopes, 75646 Nuno Gomes, 76031 Leonor Bandeira


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



(defstruct psr variables domains restrictions atributions)

(defun cria-psr (vars doms restricts)
  (make-psr :variables vars :domains doms :restrictions restricts :atributions NIL)
)

(defun psr-atribuicoes (psr)
  (psr-atributions psr)
)

(defun psr-variaveis-todas (psr)
  (psr-variables psr)
)

(defun psr-variaveis-nao-atribuidas (psr)
  (let ((l3 NIL) (l2 (psr-atribuicoes psr)) (l1 (psr-variaveis-todas psr))) 
    (if (l2 NIL)
	(l1)
	(dolist (e1 l1)
	  (dolist (e2 l2)
	   (unless (equal e1 (car(e2)))
	    (setf l3 append(list(e1)))
	   )
	  )
	)
    )
  )
)
	    