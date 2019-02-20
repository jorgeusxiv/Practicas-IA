;; Javier Martinez Rubio javier.martinezrubio@estudiante.uam.es e357532
;; Jorge Santisteban Rivas jorge.santisteban@estudiante.uam.es e360104
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar-rec (x y)
;;; Calcula el producto escalar de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar entre x e y
;;;

(defun prod-esc-rec (x y)
  (if (or (null x) (null y))
    0
    (+ (* (first x) (first y))
       (prod-esc-rec (rest x) (rest y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
  (cond ((or (null x) (null y)) 0)
        ((= 0 (* (prod-esc-rec x x) (prod-esc-rec y y))) 0)
        (t (- 1 (/ (prod-esc-rec x y)
                   (* (sqrt(prod-esc-rec x x))
                      (sqrt(prod-esc-rec y y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar-mapcar (x y)
;;; Calcula el producto escalar usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar entre x e y
;;;

(defun prod-esc-mapcar (x y)
  (if (or (null x) (null y))
  0
  (apply #'+ (mapcar #'* x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;

(defun cosine-distance-mapcar (x y)
  (cond ((or (null x) (null y)) 0)
        ((= 0 (* (prod-esc-mapcar x x) (prod-esc-mapcar y y))) 0)
        (t (- 1 (/ (prod-esc-mapcar x y)
                   (* (sqrt(prod-esc-mapcar x x))
                      (sqrt(prod-esc-mapcar y y))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-lst-vectors
;;; Ordena las listas segun su nivel de confianza
;;; INPUT:  vector-ref: vector que representa a una categoria,
;;;                 representado como una lista
;;;         vector-insert: vector a insertar
;;;         ord-lst-of-vectors: nuevo vector de vectores ordenados
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;


(defun order-lst-vectors(vector-ref vector-insert ord-lst-of-vectors)
  (cond ((null  ord-lst-of-vectors) (cons vector-insert ord-lst-of-vectors))
        ((< (cosine-distance-mapcar vector-ref vector-insert)
            (cosine-distance-mapcar vector-ref (first ord-lst-of-vectors)))
         (cons vector-insert ord-lst-of-vectors))
        (t (cons (first ord-lst-of-vectors)
                 (order-lst-vectors vector-ref vector-insert (rest ord-lst-of-vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;


(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
  (if (>= (- 1 confidence-level)
         (cosine-distance-mapcar vector (first lst-of-vectors)))
    (if (null (rest lst-of-vectors))
      (order-lst-vectors vector (first lst-of-vectors) '())
      (order-lst-vectors vector (first lst-of-vectors) (order-vectors-cosine-distance
                                                      vector (rest lst-of-vectors) confidence-level)))
    (if (null (rest lst-of-vectors))
      nil
      (order-vectors-cosine-distance vector (rest lst-of-vectors) confidence-level))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun get-vectors-category (categories texts distance-measure)
  (if (or (null categories) (null texts))
      NIL
  (mapcar #'(lambda(x) (get-text-category categories x distance-measure (first categories))) texts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-text-category (categories text distance-measure min-category)
;;; A partir de un texto devolvemos el identificador de la categoria que lo aproxima y su distancia.
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         text:      vector, representado como una lista
;;;         distance-measure: funcion de distancia
;;;         min-category: la categoria minima para comenzar la iteracion (la primera categoria por defecto)
;;; OUTPUT: Par formado por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia


(defun get-text-category (categories text distance-measure min-category)
  (if (null categories)
     (list (first min-category) (funcall distance-measure (rest text) (rest min-category)))
    (if (< (funcall distance-measure (rest (first categories)) (rest text))
           (funcall distance-measure (rest min-category) (rest text)))
      (get-text-category (rest categories) text distance-measure (first categories))
      (get-text-category (rest categories) text distance-measure min-category))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
(defun newton (f df max-iter x0 &optional (tol 0.001))
  (if (= max-iter -1) NIL
  (if (< (abs (funcall f x0)) tol) x0
      (newton f df (- max-iter 1) (- x0 (/ (funcall f x0) (funcall df x0))) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (cond ((null semillas) nil)
          ((newton f df max-iter (first semillas) tol)
           (newton f df max-iter (first semillas) tol))
          (t (one-root-newton f df max-iter (rest semillas) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  (mapcar #'(lambda(x) (newton f df max-iter x tol)) semillas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas (sin nil)
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla
;;;

(defun list-not-nil-roots-newton (f df max-iter semillas &optional ( tol 0.001))
    (mapcan #'(lambda(x) (unless (null x) (list x))) (all-roots-newton f df max-iter semillas tol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
  (cond ((or (null elt) (null lst))
              nil)
        (t (mapcar #'(lambda(x) (list elt x)) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
  (cond ((or (null lst1) (null lst2))
         nil)
        (t (mapcan #'(lambda(x) (combine-elt-lst x lst2)) lst1))))

;;FUNCIONES AUXILIARES PARA EL 3.3;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-cons-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista (utilizando cons)
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista

(defun combine-cons-elt-lst (elt lst)
  (cond ((or (null elt) (null lst))
              nil)
        (t (mapcar #'(lambda(x) (cons elt x)) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-cons-lst-lst
;;; Calcula el producto cartesiano de dos listas (utilizando cons)
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas

(defun combine-cons-lst-lst (lst1 lst2)
  (cond ((or (null lst1) (null lst2))
         nil)
        (t (mapcan #'(lambda(x) (combine-cons-elt-lst x lst2)) lst1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos

(defun combine-list-of-lsts (lstolsts)
  (cond ((null lstolsts)
         (list nil))
        (t (combine-cons-lst-lst (first lstolsts)
                            (combine-list-of-lsts (rest lstolsts))))))


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
        ((n-ary-connector-p (first fbf))
         (mapcar #'(lambda(x) (convert x)) fbf))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  )

(defun shortest-path-improved (end queue net)
  )
