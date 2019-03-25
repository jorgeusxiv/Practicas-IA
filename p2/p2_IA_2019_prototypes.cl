;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Lab assignment 2: Search
;;
;; Javier Martinez Rubio javier.martinezrubio@estudiante.uam.es e357532
;; Jorge Santisteban Rivas jorge.santisteban@estudiante.uam.es e360104
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Problem definitions
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether
                       ; a state fulfils the goal
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state
  operators)           ; list of operators (references to functions) to
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Node in search tree
;;
(defstruct node
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Actions
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Search strategies
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cities* '(Calais Reims Paris Nancy Orleans
                                St-Malo Brest Nevers Limoges
                                Roenne Lyon Toulouse Avignon Marseille))

(defparameter *trains*
  '((Paris Calais (34.0 60.0))      (Calais Paris (34.0 60.0))
    (Reims Calais (35.0 70.0))      (Calais Reims (35.0 70.0))
    (Nancy Reims (35.0 55.0))       (Reims Nancy (35.0 55.0))
    (Paris Nancy (40.0 67.0))       (Nancy Paris (40.0 67.0))
    (Paris Nevers (48.0 75.0))      (Nevers Paris (48.0 75.0))
    (Paris Orleans (23.0 38.0))     (Orleans Paris (23.0 38.0))
    (Paris St-Malo (40.0 70.0))     (St-Malo Paris (40.0 70.0))
    (St-Malo Nantes (20.0 28.0))    (Nantes St-Malo (20.0 28.0))
    (St-Malo Brest (30.0 40.0))     (Brest St-Malo (30.0 40.0))
    (Nantes Brest (35.0 50.0))      (Brest Nantes (35.0 50.0))
    (Nantes Orleans (37.0 55.0))    (Orleans Nantes (37.0 55.0))
    (Nantes Toulouse (80.0 130.0))  (Toulouse Nantes (80.0 130.0))
    (Orleans Limoges (55.0 85.0))   (Limoges Orleans (55.0 85.0))
    (Limoges Nevers (42.0 60.0))    (Nevers Limoges (42.0 60.0))
    (Limoges Toulouse (25.0 35.0))  (Toulouse Limoges (25.0 35.0))
    (Toulouse Lyon (60.0 95.0))     (Lyon Toulouse (60.0 95.0))
    (Lyon Roenne (18.0 25.0))       (Roenne Lyon  (18.0 25.0))
    (Lyon Avignon (30.0 40.0))      (Avignon Lyon (30.0 40.0))
    (Avignon Marseille (16.0 25.0)) (Marseille Avignon (16.0 25.0))
    (Marseille Toulouse (65.0 120.0)) (Toulouse Marseille (65.0 120.0))))


(defparameter *canals*
  '((Reims Calais (75.0 15.0)) (Paris Reims (90.0 10.0))
    (Paris Nancy (80.0 10.0)) (Nancy reims (70.0 20.0))
    (Lyon Nancy (150.0 20.0)) (Nevers Paris (90.0 10.0))
    (Roenne Nevers (40.0 5.0)) (Lyon Roenne (40.0 5.0))
    (Lyon Avignon (50.0 20.0)) (Avignon Marseille (35.0 10.0))
    (Nantes St-Malo (40.0 15.0)) (St-Malo Brest (65.0 15.0))
    (Nantes Brest (75.0 15.0))))



(defparameter *estimate*
  '((Calais (0.0 0.0)) (Reims (25.0 0.0)) (Paris (30.0 0.0))
    (Nancy (50.0 0.0)) (Orleans (55.0 0.0)) (St-Malo (65.0 0.0))
    (Nantes (75.0 0.0)) (Brest (90.0 0.0)) (Nevers (70.0 0.0))
    (Limoges (100.0 0.0)) (Roenne (85.0 0.0)) (Lyon (105.0 0.0))
    (Toulouse (130.0 0.0)) (Avignon (135.0 0.0)) (Marseille (145.0 0.0))))

(defparameter *origin* 'Marseille)

(defparameter *destination* '(Calais))

(defparameter *forbidden*  '(Avignon))

(defparameter *mandatory* '(Paris))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 1 -- Evalucion de las heurísticas
;;
;; Devuelve el valor de las heurísticas de una determinada ciudad
;;
;;  Input:
;;    state: la ciudad en la que estamos
;;    sensors: una lista con heurísticas, que es una lista con pares
;;                (state (time-est cost-est) )
;;             donde el primer elemento es el nombre de la ciudad y el segundo
;;             un numero que estima el coste(temporal o de precio)
;;
;;  Returns:
;;    El coste (un numero) o NIL si la ciudad no esta en la lista de heurísticas
;;
;;  Es necesario definir dos funciones: la primera la cual estima el coste temporal
;;  y la segunda el coste economico (precio)

(defun f-h-time (state sensors)
  (first (second (assoc state sensors))))

(defun f-h-price (state sensors)
  (second (second (assoc state sensors))))

;;
;; END: Exercise 1 -- Evalucion de las heurísticas
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Operadores de navegacion
;;

;;Funciones auxiliares para obtener el estado inicio, el estado final y los costes

(defun start (edge)
  (first edge))

(defun end (edge)
  (second edge))

(defun cost-time (edge)
  (first (third edge)))

(defun cost-price (edge)
  (second (third edge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Funcion de navegacion general
;;
;;  Devuelve las acciones que se pueden realizar desde una ciudad
;;
;;  Input:
;;    state:      la ciudad desde la que queremos realizar la accion
;;    lst-edges:  lista de las aristas del grafo, cada elemento es de la
;;                forma: (source destination (cost1 cost2))
;;    c-fun:      funcion que extrae el coste correcto (tiempo o precio)
;;                del par que aparece en la arista
;;    name:       nombre dado a las acciones que se crean (ver la estructura de
;;                action)
;;    forbidden-cities:
;;                lista de las ciudades que no se pueden visitar en tren
;;
;;  Returns
;;    Una lista con estructuras action con el origen en la ciudad actual y el
;;    destino en las ciudades a las cuales el origen está conectado
;;
(defun navigate (state lst-edges cfun  name &optional forbidden )

  (remove nil (mapcar #'(lambda(x) (if (and (eql state (start x)) ;;Comprobamos que state sea el inicio
                           (NULL (find (end x) forbidden))) ;;Comprobamos que el destino no esté en forbidden
                           (make-action :name name
                                        :origin state
                                        :final (end x)
                                        :cost (funcall cfun x))
                         NIL)) lst-edges)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by canal
;;
;; Esto es una especializacion de la funcion de navegar general:dado una
;; ciudad y una lista de canales, devuelve una lista con acciones a las que navegar
;; desde la ciudad origen hasta las ciudades alcanzables desde el origen a traves de los
;; canales.
;;
(defun navigate-canal-time (state canals)
  (navigate state canals #'cost-time 'NAVIGATE-CANAL-TIME))

(defun navigate-canal-price (state canals)
  (navigate state canals #'cost-price 'NAVIGATE-CANAL-PRICE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by train
;;
;; Esto es una especializacion de la funcion de navegar general:dado una
;; ciudad y una lista de trenes, devuelve una lista con acciones a las que viajar
;; desde la ciudad origen hasta las ciudades alcanzables desde el origen a traves del
;; tren.
;; tener en cuenta que esta función utiliza una lista de ciudades prohibidas.
;;

(defun navigate-train-time (state trains forbidden)
  (navigate state trains #'cost-time 'NAVIGATE-TRAIN-TIME forbidden))

(defun navigate-train-price (state trains forbidden)
  (navigate state trains #'cost-price 'NAVIGATE-TRAIN-PRICE forbidden))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal path
;;
;;  Devuelve T or NIl dependiendo si el camino lleva a una ciudad final
;;
;;  Input:
;;    node:       nodo estructura que contiene, en la cadena de nodos padres,
;;                un camino empezando en la ciudad inicial
;;    mandatory:  lista con los nombres de las ciudades que son obligatorias visitar
;;
;;  Returns
;;    T: el camino es un camino correcto a la ciudad final
;;    NIL: camino erroneo: o la ciudad final no es un destino o alguna de las
;;         ciudades obligatorias no estan en el camino.



(defun f-goal-path (node mandatory)
  (if (NULL (node-parent node))
      (if (NULL (remove (node-state node) mandatory))
          t
          NIL)

      (if (find (node-state node) mandatory)
          (f-goal-path (node-parent node) (remove (node-state node) mandatory))
          (f-goal-path (node-parent node) mandatory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal test
;;
;;  Devuelve T or NIl dependiendo si el camino lleva a una ciudad final, comprobando si
;; es una ciudad destino y si lleva a una ciudad final (a partir de la función goal-path)
;;
;;  Input:
;;    node:       nodo estructura que contiene, en la cadena de nodos padres,
;;                un camino empezando en la ciudad inicial
;;    destinations: lista con los nombres de las ciudades destino
;;    mandatory:  lista con los nombres de las ciudades que son obligatorias visitar
;;
;;  Returns
;;    T: el camino es un camino correcto a la ciudad final
;;    NIL: camino erroneo: o la ciudad final no es un destino o alguna de las
;;         ciudades obligatorias no estan en el camino.

(defun f-goal-test (node destinations mandatory)
  (if (find (node-state node) destinations)
      (f-goal-path (node-parent node) mandatory)
      NIL))


;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 4 -- Equal predicate for search states
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Si ha visitado todas las ciudades obligatorias devolverá NIL, sino devolverá las ciudades
;; obligatorias restantes
;;
;,
;;  Input:
;;    node      : el nodo a estudiar
;;    mandatory:  lista con los nombres de las ciudades que es obligatorio visitar
;;
;;  Returns
;;    mandatory: la lista una vez visitados todos los nodos
;;


(defun f-search-path (node &optional mandatory)
  (if (NULL (node-parent node))
      mandatory
      (if (find (node-state node) mandatory)
          (f-search-path (node-parent node) (remove (node-state node) mandatory))
          (f-search-path (node-parent node) mandatory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Determina si dos nodos son iguales respecto a la solución del problema:
;; dos nodos son equivalentes si representan la misma ciudad y si el camino
;, contiene las mismas ciudades obligatorias
;;  Input:
;;    node-1, node-2: los dos nodos que estamos comparando, cada uno
;;                    define un camino a través del enlace de nodos padre
;;    mandatory:  lista con los nombres de las ciudades que es obligatorio visitar
;;
;;  Returns
;;    T: los dos nodos son equivalentes
;;    NIL: los dos nodos no son equivalentes
;;


(defun f-search-state-equal (node-1 node-2 &optional mandatory)
  (if (eql (node-state node-1) (node-state node-2) )
      (if (eql (f-search-path node-1 mandatory) (f-search-path node-2 mandatory))
          t
          NIL)
      NIL))


;;
;; END: Exercise 4 -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 5 -- Define the problem structure
;;
;;
;;  Notar que la conectividad de la red usando canales y trenes
;;  es implicita en los operadores: hay una lista de dos operadores,
;;  cada uno coge un solo parametro: el nombre de la ciudad, y devuelve
;;  una lista de acciones, indicando a que ciudades se puede mover y a
;;  coste. La lista de aristas son constantes en el segundo parametro de
;;  los operadores de navegacion
;;
;;  Hay dos problemas definidos: uno minimiza el tiempo, otro el coste


(defparameter *travel-cheap*
  (make-problem
   :states *cities*
   :initial-state *origin*
   :f-h #'(lambda (state) (f-h-price state *estimate*))
   :f-goal-test #'(lambda(node) (f-goal-test node *destination* *mandatory*))
   :f-search-state-equal #'(lambda(node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))
   :operators (list
              #'(lambda (node) (navigate-train-price (node-state node) *trains* *forbidden*))
              #'(lambda (node) (navigate-canal-price (node-state node) *canals*)))))

(defparameter *travel-fast*
  (make-problem
   :states *cities*
   :initial-state *origin*
   :f-h #'(lambda (state) (f-h-price state *estimate*))
   :f-goal-test #'(lambda(node) (f-goal-test node *destination* *mandatory*))
   :f-search-state-equal #'(lambda(node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))
   :operators (list
              #'(lambda (node) (navigate-train-time (node-state node) *trains* *forbidden*))
              #'(lambda (node) (navigate-canal-time (node-state node) *canals*)))))

;;
;;  END: Exercise 5 -- Define the problem structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 6: Expand node
;;
;; La función principal de esta sección es "expand-node", que recibe
;; a node estructura (el nodo a expandir) y una problem estructura.
;; La problem estructura tiene una lista de operadores de navigacion, y
;; nosotos estamos interesados en las ciudades a las que se puede llegar usando
;; alguno de ellos.
;;
;; Por lo que, en la función expand-node, iteramos (usando mapcar) sobre todos
;; los operadores del problema, y para cada uno de ellos, llamamos a
;; expand-node-operator, para determinar las ciudades a las que se puede llegar
;; usando ese operador.
;;
;; El operador nos devuelve una lista de actions. Iteramos de nuevo en
;; esta lista de actions y, para cada una, llamamos a expand-node-action
;; que crea una node estructura con el nodo al que se puede llegar a partir de
;; esa action
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Crea una lista con todas las acciones a los que se puede llegar usando unos
;; determinados operadores
;;
;;  Input:
;;    node:   la node estructura desde donde empezamos.
;;    problem: la problem estructura con la lista de operadores
;;
;;  Returns:
;;    Una lista (action_1,...,action_n) de acciones a los que se puede llegar desde
;;    el actual con los operadores
;;


(defun expand-node-operator (node problem)
  (mapcan #'(lambda(x) (funcall x node)) (problem-operators problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Crea una node estructura con el nodo al que se puede llegar a partir de
;; esa action (modificando la g y la h)
;;
;;  Input:
;;    node:   la node estructura desde donde empezamos.
;;    problem: la problem estructura con la lista de operadores
;;
;;  Returns:
;;   Una node estructura con el nodo al que se puede llegar a partir de
;;   esa action (modificando la g y la h)
;;

(defun expand-node-action (node action f-h)
  (let ((h (funcall f-h (action-final action)))
        (g (+ (node-g node) (action-cost action))))
    (make-node :state (action-final action)
               :parent node
               :action action
               :g g
               :h h
               :f (+ h g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Crea una lista con todos los nodos a los que se puede llegar desde el
;;  actual usando todos los operadores de un problema
;;
;;  Input:
;;    node:   la node estructura desde donde empezamos.
;;    problem: la problem estructura con la lista de operadores
;;
;;  Returns:
;;    Una lista (node_1,...,node_n) de nodos a los que se puede llegar desde
;;    el actual
;;

(defun expand-node (node problem)
  (mapcar #'(lambda(x) (expand-node-action node x (problem-f-h problem))) (expand-node-operator node problem)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 7 -- Node list management
;;;
;;;  Fusiona dos listas de nodos, una de ellas ordenada respecto a una
;;;  estrategia dada, manteniendo el resultado ordenado con respecto a una
;;;  misma estrategia.
;;;
;;;  Esta es la idea: supongamos que la manera de ordenar es simplemente
;;;  la manera de ordenar los numeros naturales. Tenemos una lista "base" que ya
;;;  está ordenada, por ejemplo:
;;;      lst1 --> '(3 6 8 10 13 15)
;;;
;;;  y una lista no necesariamente ordenada:
;;;
;;;      nord --> '(11 5 9 16)
;;;
;;;  la llamada a (insert-nodes nord lst1 #'<) producira
;;;
;;;    (3 5 6 8 9 10 11 13 15 16)
;;;
;;;  The functionality is divided in three functions. The first,
;;;  insert-node, inserts a node in a list keeping it ordered. The
;;;  second, insert-nodes, insert the nodes of the non-ordered list
;;;  into the ordered, one by one, so that the two lists are merged.
;;;  The last function, insert-node-strategy is a simple interface that
;;;  receives a strategy, extracts from it the comparison function,
;;;  and calls insert-nodes

(defun insert-node (node lst-nodes node-compare-p)
  (cond ((NULL lst-nodes) (list node))
        ((funcall node-compare-p node (first lst-nodes))
         (cons node lst-nodes))
        (t (cons (first lst-nodes) (insert-node node (rest lst-nodes) node-compare-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes
;;               are to be inserted
;;    node-compare-p: a function node x node --> 2 that returns T if the
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and
;;    those of the list "nodes@. The list is ordered with respect to the
;;   criterion node-compare-p.
;;
(defun insert-nodes (nodes lst-nodes node-compare-p)
  (if (NULL nodes)
      lst-nodes
      (insert-nodes (rest nodes) (insert-node (first nodes) lst-nodes node-compare-p) node-compare-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect the given strategy
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes
;;               are to be inserted
;;    strategy: the strategy that gives the criterion for node
;;              comparison
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and
;;    those of the list "nodes@. The list is ordered with respect to the
;;    criterion defined in te strategy.
;;
;; Note:
;;   You will note that this function is just an interface to
;;   insert-nodes: it allows to call using teh strategy as a
;;   parameter; all it does is to "extract" the compare function and
;;   use it to call insert-nodes.
;;

(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (insert-nodes nodes lst-nodes (strategy-node-compare-p strategy)))
  ;



;;
;;    END: Exercize 7 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 8 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell
;; us which nodes should be analyzed first. In the A* strategy, the first
;; node to be analyzed is the one with the smallest value of g+h
;;
(defun node-f-<= (node-1 node-2)
  (<= (node-f node-1)
      (node-f node-2)))

(defparameter *a-estrella*
  (make-strategy
                :name 'a-estrella
                :node-compare-p #'node-f-<=))

;;
;; END: Exercise 8 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 9: Search algorithm
;;;
;;;    Searches a path that solves a given problem using a given search
;;;    strategy. Here too we have two functions: one is a simple
;;;    interface that extracts the relevant information from the
;;;    problem and strategy structure, builds an initial open-nodes
;;;    list (which contains only the starting node defined by the
;;;    state), and initial closed node list (the empty list), and calls
;;;    the auxiliary function.
;;;
;;;    The auxiliary is a recursive function that extracts nodes from
;;;    the open list, expands them, inserts the neighbors in the
;;;    open-list, and the expanded node in the closed list. There is a
;;;    caveat: with this version of the algorithm, a node can be
;;;    inserted in the open list more than once. In this case, if we
;;;    extract a node in the open list and the following two condition old:
;;;
;;;     the node we extract is already in the closed list (it has
;;;     already been expanded)
;;;       and
;;;     the path estimation that we have is better than the one we
;;;     obtain from the node in the open list
;;;
;;;     then we ignore the node.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR QUE DEVUELVE EL ESTADO Y LA G;;;

(defun get-state-n-g (lst-nodes)
  (mapcar #'(lambda(x) (list (node-state x) (node-g x))) lst-nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Funcion que expande un nodo, modificando la lista de abiertos y la de cerrados;;;

(defun graph-search-aux2 (problem open-nodes closed-nodes strategy)
  (let ((new-open-nodes (insert-nodes-strategy (expand-node (first open-nodes) problem) (rest open-nodes) strategy))
        (new-closed-nodes (cons (first open-nodes) closed-nodes)));;QUIZA SEA CONS O APPEND
  (graph-search-aux problem new-open-nodes new-closed-nodes strategy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION CHECK-EQUAL-NODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-node-equal (node closed-nodes problem)
  (if (NULL closed-nodes)
      nil
      (if (funcall (problem-f-search-state-equal problem) node (first closed-nodes))
          t
          (check-node-equal node (rest closed-nodes) problem))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auxiliary search function (the one that actually does all the work
;;
;;  Input:
;;    problem: the problem structure from which we get the general
;;             information (goal testing function, action operatos, etc.
;;    open-nodes: the list of open nodes, nodes that are waiting to be
;;                visited
;;    closed-nodes: the list of closed nodes: nodes that have already
;;                  been visited
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;     Note that what is returned is quite a complex structure: the
;;     node contains in "parent" the node that comes before in the
;;     path, that contains another one in "parents" and so on until
;;     the initial one. So, what we have here is a rather complex
;;     nested structure that contains not only the final node but the
;;     whole path from the starting node to the final.
;;
(defun graph-search-aux (problem open-nodes closed-nodes strategy)
  (if (NULL open-nodes)
      NIL
      (if (funcall (problem-f-goal-test problem) (first open-nodes))
          (first open-nodes)
          (let ((state  (node-state (first open-nodes)))
                (state-n-g (get-state-n-g closed-nodes)))

          (if (NULL (assoc state state-n-g))
               (graph-search-aux2 problem open-nodes closed-nodes strategy)
              (if (check-node-equal (first open-nodes) closed-nodes problem)  ;; COMPROBAR QUE LOS NODOS SEAN IGUALES (FUNCION AUXILIAR)
                  (if (< (node-g (first open-nodes)) (second (assoc state state-n-g)))
                      (graph-search-aux2 problem open-nodes closed-nodes strategy)
                      (graph-search-aux problem (rest open-nodes) closed-nodes strategy)
                  )
                  (graph-search-aux2 problem open-nodes closed-nodes strategy)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface function for the graph search.
;;
;;  Input:
;;    problem: the problem structure from which we get the general
;;             information (goal testing function, action operatos,
;;             starting node, heuristic, etc.
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;    See the graph-search-aux for the complete structure of the
;;    returned node.
;;    This function simply prepares the data for the auxiliary
;;    function: creates an open list with a single node (the source)
;;    and an empty closed list.
;;
(defun graph-search (problem strategy)
  (graph-search-aux problem (list (make-node :state (problem-initial-state problem)
                                             :parent NIL
                                             :action NIL
                                             :g 0
                                             :h 0
                                             :f 0))
                    '() strategy)
  )

;
;  A* search is simply a function that solves a problem using the A* strategy
;
(defun a-star-search (problem)
  (graph-search problem *a-estrella*)
  )


  ; ;; TESTS
  ;
   (defparameter node-nevers
      (make-node :state 'Nevers) )
   (defparameter node-paris
      (make-node :state 'Paris :parent node-nevers))
   (defparameter node-nancy
      (make-node :state 'Nancy :parent node-paris))
   (defparameter node-reims
      (make-node :state 'Reims :parent node-nancy))
   (defparameter node-calais
      (make-node :state 'Calais :parent node-reims))
   (defparameter node-calais-2
      (make-node :state 'Calais :parent node-paris))
  (defparameter node-marseille-ex6
     (make-node :state 'Marseille :depth 12 :g 10 :f 20) )

  (defparameter lst-nodes-ex6
    (expand-node node-marseille-ex6 *travel-fast*))

  (defun node-g-<= (node-1 node-2)
    (<= (node-g node-1)
        (node-g node-2)))

  (defparameter *uniform-cost*
    (make-strategy
     :name 'uniform-cost
     :node-compare-p #'node-g-<=))

  (defparameter node-paris-ex7
    (make-node :state 'Paris :depth 0 :g 0 :f 0) )

  (defparameter node-nancy-ex7
    (make-node :state 'Nancy :depth 2 :g 50 :f 50) )

  (defparameter nodo-avignon
    (make-node :state 'Avignon :parent node-marseille-ex6))


  (defparameter sol-ex7 (insert-nodes-strategy (list node-paris-ex7 node-nancy-ex7)
                                               lst-nodes-ex6
                                               *uniform-cost*))

  (graph-search *travel-cheap* *a-estrella*)


;;
;; END: Exercise 9 -- Search algorithm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 10: Solution path
;;;
;*** solution-path ***

(defun solution-path (node)
  (if (NULL node)
       nil
       (if (null (node-parent node))
           (list (node-state node))
           (append (solution-path (node-parent node)) (list (node-state node))))))

;*** action-sequence ***
; Visualize sequence of actions

(defun action-sequence (node)
(if (null node)
    nil
    (if (null (node-parent (node-parent node)))
        (list (node-action node))
        (append (action-sequence (node-parent node)) (list (node-action node))))))

;;;
;;;    END Exercise 10: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    BEGIN Exercise 11: Otras estrategias de búsqueda
;;;

;;; Para realizar la búsqueda en profundidad los nodos recien explorados tienen
;;; mayor prioridad, por los que serán añadidos los primeros a la lista de abiertos,
;;; por lo que con poner un TRUE valdrá y añadirá el nodo el primero a la lista. De
;;; esta manera no expandiremos el árbol de izquierda a derecha sino de derecha a
;;; izquierda (siendo una manera igualmente válida)


(defun depth-first-node-compare-p (node-1 node-2)
  t)

;;;Definimos la estrategia de búsqueda en profundidad

(defparameter *depth-first*
  (make-strategy
    :name 'depth-first
    :node-compare-p #'depth-first-node-compare-p))

;;; Para realizar la búsqueda en anchura los nodos recien explorados serán los
;;; últimos  añadidos a la lista de abiertos,por lo que con poner un NIL valdrá
;;; y añadirá el nodo el último de la lista.

(defun breadth-first-node-compare-p (node-1 node-2)
  NIL)

;;;Definimos la estrategia de búsqueda en anchura

(defparameter *breadth-first*
  (make-strategy
    :name 'breadth-first
    :node-compare-p #'breadth-first-node-compare-p))

;;;
;;;    END Exercise 11: Otras estrategias de búsqueda
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
