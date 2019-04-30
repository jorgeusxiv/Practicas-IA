(defpackage :2302_P01_1cf7b ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias
(in-package 2302_P01_1cf7b)
(defvar *alias* '|pacopaquerasV2|) ; alias que aparece en el ranking
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
	      (loop for altura from (altura-columna tablero columna) below (tablero-alto tablero) do

                (let* ((fila (1- altura))
                       (abajo (contar-abajo tablero ficha-actual columna fila))
                       (der (contar-derecha tablero ficha-actual columna fila))
                       (izq (contar-izquierda tablero ficha-actual columna fila))
                       (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
                       (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
                       (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
                       (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
                  (setf puntuacion-actual
                        (+ puntuacion-actual
                      (cond ((>= abajo 3) 1)
                            ((>= (+ der izq) 3) 1)
                            ((>= (+ arriba-izq abajo-der) 3) 1)
                            ((>= (+ arriba-der abajo-izq) 3) 1)
                            (t 0)))))
                (let* ((fila (1- altura))
                       (abajo (contar-abajo tablero ficha-oponente columna fila))
                       (der (contar-derecha tablero ficha-oponente columna fila))
                       (izq (contar-izquierda tablero ficha-oponente columna fila))
                       (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
                       (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
                       (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
                       (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
                  (setf puntuacion-oponente
                        (+ puntuacion-oponente
                      (cond ((>= abajo 3) 1)
                            ((>= (+ der izq) 3) 1)
                            ((>= (+ arriba-izq abajo-der) 3) 1)
                            ((>= (+ arriba-der abajo-izq) 3) 1)
                            (t 0)))))))
        (- puntuacion-actual puntuacion-oponente)))))
