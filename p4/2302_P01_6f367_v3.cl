(defpackage :2302_P01_6f367 ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias
(in-package 2302_P01_6f367)
(defvar *alias* '|CARLOSTRATADEARRANCARLO|) ; alias que aparece en el ranking
(defun heuristica (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
    (loop for columna from 0 below (tablero-ancho tablero) do
        (loop for fila from 0 below (1- (altura-columna tablero columna)) do

                  (setf puntuacion-actual
                        (+ puntuacion-actual

                           (cond ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (= columna 3)
                                      (or (= fila 0) (= fila 1) (= fila 2))) 200)
                                 ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (or (= columna 2) (= columna 4))
                                      (or (= fila 0) (= fila 1) (= fila 2))) 100)
                                 ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (or (= columna 1) (= columna 5))
                                      (or (= fila 0) (= fila 1) (= fila 2) (= fila 3))) 50)
                                 ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (or (= columna 0) (= columna 6))
                                      (or (= fila 0) (= fila 1) (= fila 2) (= fila 3) (= fila 4))) 25)
                                 (t 0)))))

           (loop for altura from (altura-columna tablero columna) below (tablero-alto tablero) do

                   (let* (
                          (fila (1- altura))
                          (abajo (contar-abajo tablero ficha-actual columna fila))
                          (der (contar-derecha tablero ficha-actual columna fila))
                          (izq (contar-izquierda tablero ficha-actual columna fila))
                          (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
                          (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
                          (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
                          (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
                     (setf puntuacion-actual
                           (+ puntuacion-actual

                                   (if (or (>= abajo 3)
                                           (>= (+ der izq) 3)
                                           (>= (+ arriba-izq abajo-der) 3)
                                           (>= (+ arriba-der abajo-izq) 3))
                                        (if (= (mod fila 2) 0)
                                                500
                                                400)
                                        0)
                                     ; (cond ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                     ;            (= columna 3)
                                     ;            (or (= fila 0) (= fila 1) (= fila 2))) 200)
                                     ;       ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                     ;            (or (= columna 2) (= columna 4))
                                     ;            (or (= fila 0) (= fila 1) (= fila 2))) 100)
                                     ;       ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                     ;            (or (= columna 1) (= columna 5))
                                     ;            (or (= fila 0) (= fila 1) (= fila 2) (= fila 3))) 50)
                                     ;       ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                     ;            (or (= columna 0) (= columna 6))
                                     ;            (or (= fila 0) (= fila 1) (= fila 2) (= fila 3) (= fila 4))) 25)
                                     ;       (t 0))
                              )))
                   (let* (
                          (fila (1- altura))
                          (abajo (contar-abajo tablero ficha-oponente columna fila))
                          (der (contar-derecha tablero ficha-oponente columna fila))
                          (izq (contar-izquierda tablero ficha-oponente columna fila))
                          (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
                          (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
                          (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
                          (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
                     (setf puntuacion-oponente
                           (+ puntuacion-oponente

                           (cond ((>= abajo 3) 1000)
                                 ((>= (+ der izq) 3) 1000)
                                 ((>= (+ arriba-izq abajo-der) 3) 1000)
                                 ((>= (+ arriba-der abajo-izq) 3) 1000)
                                 (t 0))
                           ; (cond ((and (= columna 3) (fila 1) ) 10)
                           ;       (t 0))
                           ; (cond ((= fila 0) 3)
                           ;       ((= fila 2) 2)
                           ;       ((= fila 4) 1)
                           ;       (t 0))
                              )))))
        (- puntuacion-actual puntuacion-oponente)))))
