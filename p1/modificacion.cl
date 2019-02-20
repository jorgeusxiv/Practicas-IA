;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (fbf)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert
;;; Recibe una expresion y la convierte en una expresion con and's y or's
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF modificada
;;;

(defun convert (fbf)
  (cond ((or (literal-p fbf) (connector-p fbf)) fbf)
        ((bicond-connector-p (first fbf)) (convert (bicond fbf)))
        ((cond-connector-p (first fbf)) (convert (conditional fbf)))
        ((unary-connector-p (first fbf))
              (cond ((bicond-connector-p (first (first (rest fbf))))
                     (convert (neg-bicond (second fbf))))
                    ((cond-connector-p (first (first (rest fbf))))
                     (convert (neg-conditional (second fbf))))
                    ((eql +or+ (first (first (rest fbf))))
                     (convert (de-morgan-or (second fbf))))
                    ((eql +and+ (first (first (rest fbf))))
                     (convert (de-morgan-and (second fbf))))
                    ((unary-connector-p (first (first (rest fbf))))
                     (convert (double-negation (second fbf))))
                    (t fbf)))
        ((and (rest fbf) (or (n-ary-connector-p (first fbf)) (literal-p (first fbf))))
         (mapcar #'(lambda(x) (convert x)) fbf))
        ((and (listp (first fbf)) (first fbf))
         (if (rest fbf)
           (list (convert (first fbf)) (convert (rest fbf)))
           (convert (first fbf))))
        ((and (listp fbf) (atom (first fbf)))
         (first fbf)) ;;UN POCO REDUNDANTE Y RARO
        (t fbf)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; de-morgan-or
;;; Recibe una expresion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF modificada
;;;

(defun de-morgan-or (fbf)
  (cond ((null (rest fbf)) (list (list +not+ (first fbf))))
        ((n-ary-connector-p (first fbf)) (cons +and+ (de-morgan-or (rest fbf))))
        (t (append (list (list +not+ (first fbf))) (de-morgan-or (rest fbf))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; de-morgan-and
;;; Recibe una expresion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF modificada
;;;

(defun de-morgan-and (fbf)
(cond ((null (rest fbf)) (list (list +not+ (first fbf))))
      ((n-ary-connector-p (first fbf)) (cons +or+ (de-morgan-or (rest fbf))))
      (t (append (list (list +not+ (first fbf))) (de-morgan-or (rest fbf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg-conditional
;;; Recibe una expresion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF modificada
;;;

(defun neg-conditional(fbf)
  (list +and+ (second fbf) (list +not+ (third fbf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg-bicond
;;; Recibe una expresion con una doble condicion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF sin doble condicional
;;;

(defun neg-bicond(fbf)
  (list +or+ (list +and+ (second fbf) (list +not+ (third fbf)))
        (list +and+ (list +not+ (second fbf)) (third fbf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditional
;;; Recibe una expresion con una condicion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF sin condicional
;;;

(defun conditional(fbf)
  (list +or+ (list +not+ (second fbf)) (third fbf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bicond
;;; Recibe una expresion con una doble condicion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF sin doble condicional
;;;

(defun bicond(fbf)
  (list +or+ (list +and+ (second fbf) (third fbf))
        (list +and+ (list +not+ (second fbf)) (list +not+ (third fbf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; double-negation
;;; Recibe una expresion con una doble negacion y aplica la regla de derivacion adecuada
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : fbf-modified FBF sin doble condicional
;;;

(defun double-negation(fbf)
  (second fbf))
