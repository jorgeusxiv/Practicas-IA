;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Lab assignment 4: Connect4
;;
;; Javier Martinez Rubio javier.martinezrubio@estudiante.uam.es e357532
;; Jorge Santisteban Rivas jorge.santisteban@estudiante.uam.es e360104
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :2302_P01_6f367 ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias
(in-package 2302_P01_6f367)
(defvar *alias* '|CARLOSTRATADEARRANCARLO|) ; alias que aparece en el ranking

;; Funcion heuristica(estado)
;; Devuelve el valor de un determinado estado del tablero
;;
;;  Input:
;;    estado: situacion del tablero actual
;;
;;  Returns:
;;    El valor o puntuación asignada a dicho tablero
;;

(defun heuristica (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
   ;Definimos  nuestra ficha y la ficha del oponente
   (ficha-actual (estado-turno estado))
   (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
      ;Comprobamos si es un estado ganador
  (let ((ganador (ganador estado)))
    (cond ((not ganador) 0)
  	((eql ganador ficha-actual) +val-max+)
  	(t +val-min+)))
      (let ((puntuacion 0))
  ;Contamos las fichas en la columna central
  (loop for fila from 0 below (1- (tablero-alto tablero)) do
    (setf puntuacion
          (+ puntuacion
             (if (eql (obtener-ficha tablero 3 fila) ficha-actual)
                 3 0))))
  ;Contamos el numero de bloques horizontales ganadores
  (loop for fila from 0 below (1- (tablero-alto tablero)) do
    (loop for columna from 0 to 3 do
      (let* ((b1 (obtener-ficha tablero columna fila))
             (b2 (obtener-ficha tablero (+ columna 1) fila))
             (b3 (obtener-ficha tablero (+ columna 2) fila))
             (b4 (obtener-ficha tablero (+ columna 3) fila)))
        (setf puntuacion
              (+ puntuacion
                 (evaluarBloque b1 b2 b3 b4 ficha-actual ficha-oponente))))))
   ;Contamos el numero de bloques horizontales ganadores
   (loop for columna from 0 below (1- (tablero-ancho tablero)) do
     (loop for fila from 0 to 2 do
       (let* ((b1 (obtener-ficha tablero columna fila))
              (b2 (obtener-ficha tablero columna (+ 1 fila)))
              (b3 (obtener-ficha tablero columna (+ 2 fila)))
              (b4 (obtener-ficha tablero columna (+ 3 fila))))
         (setf puntuacion
               (+ puntuacion
                  (evaluarBloque b1 b2 b3 b4 ficha-actual ficha-oponente))))))
    ;Contamos el numero de diagonales ascendentes
    (loop for fila from 0 to 2 do
      (loop for columna from 0 to 3 do
        (let* ((b1 (obtener-ficha tablero columna fila))
               (b2 (obtener-ficha tablero (+ 1 columna) (+ 1 fila)))
               (b3 (obtener-ficha tablero (+ 2 columna) (+ 2 fila)))
               (b4 (obtener-ficha tablero (+ 3 columna) (+ 3 fila))))
         (setf puntuacion
               (+ puntuacion
                  (evaluarBloque b1 b2 b3 b4 ficha-actual ficha-oponente))))))

    ;Contamos el numero de diagonales descendentes
    (loop for fila from 3 to 5 do
      (loop for columna from 0 to 3 do
        (let* ((b1 (obtener-ficha tablero columna fila))
               (b2 (obtener-ficha tablero (+ 1 columna) (- fila 1)))
               (b3 (obtener-ficha tablero (+ 2 columna) (- fila 2)))
               (b4 (obtener-ficha tablero (+ 3 columna) (- fila 3))))
         (setf puntuacion
               (+ puntuacion
                  (evaluarBloque b1 b2 b3 b4 ficha-actual ficha-oponente))))))
        puntuacion))))


;; Funcion evaluarBloque(b1 b2 b3 b4 ficha-actual ficha-oponente)
;; A partir de las 4 fichas que conforman un bloque analizamos este
;;
;;  Input:
;;    b1: Primera ficha
;;    b2: Segunda ficha
;;    b3: Tercera ficha
;;    b4: Cuarta ficha
;;    ficha-actual: Define si el jugador actual es 1 o 0
;;    ficha-oponente: Define si el jugador rival es 1 o 0
;;
;;  Returns:
;;    La puntuacion asignada a ese bloque
;;

(defun evaluarBloque (b1 b2 b3 b4 ficha-actual ficha-oponente)
  (let ((fichasNuestras 0)
        (fichasVacias 0)
        (fichasRival 0)
        (score 0))
    ;Contamos nuestras fichas
    (setf fichasNuestras
          (+ fichasNuestras
             (cond ((eql b1 ficha-actual) 1)
                   (t 0))
             (cond ((eql b2 ficha-actual) 1)
                   (t 0))
             (cond ((eql b3 ficha-actual) 1)
                   (t 0))
             (cond ((eql b4 ficha-actual) 1)
                   (t 0))))
    ;Contamos las fichas del rival
     (setf fichasRival
           (+ fichasRival
              (cond ((eql b1 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b2 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b3 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b4 ficha-oponente) 1)
                    (t 0))))
    ;Contamos las fichas vacias
      (setf fichasVacias
            (+ fichasVacias
               (cond ((NULL b1) 1)
                     (t 0))
               (cond ((NULL b2) 1)
                     (t 0))
               (cond ((NULL b3) 1)
                     (t 0))
               (cond ((NULL b4) 1)
                     (t 0))))
    ;Según los contadores definimos la puntuacion del bloque
      (setf score
            (+ score
               (cond ((= fichasNuestras 4) 100)
                     ((and (= fichasNuestras 3) (= fichasVacias 1)) 40)
                     ((and (= fichasNuestras 2) (= fichasVacias 2)) 10)
                     (t 0))
               (if (and (= fichasRival 3) (= fichasVacias 1))
                    -10 0)))
    score))
