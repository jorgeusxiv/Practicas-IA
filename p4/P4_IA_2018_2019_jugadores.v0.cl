(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
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
	      (let* ((altura (altura-columna tablero columna))
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
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
	      (let* ((altura (altura-columna tablero columna))
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
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))




  (defun heuristica1 (estado)
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
  	      (let* ((altura (altura-columna tablero columna))
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
  			 (cond ((= abajo 0) 0)
  			       ((= abajo 1) 20)
  			       ((= abajo 2) 300)
  			       ((= abajo 3) 2000))
  			 (cond ((= der 0) 0)
  			       ((= der 1) 20)
  			       ((= der 2) 300)
  			       ((= der 3) 2000))
  			 (cond ((= izq 0) 0)
  			       ((= izq 1) 20)
  			       ((= izq 2) 300)
  			       ((= izq 3) 2000))
  			 (cond ((= abajo-izq 0) 0)
  			       ((= abajo-izq 1) 20)
  			       ((= abajo-izq 2) 300)
  			       ((= abajo-izq 3) 2000))
         (cond ((= abajo-der 0) 0)
  			       ((= abajo-der 1) 20)
  			       ((= abajo-der 2) 300)
  			       ((= abajo-der 3) 2000))
         (cond ((= arriba-izq 0) 0)
  			       ((= arriba-izq 1) 20)
  			       ((= arriba-izq 2) 300)
  			       ((= arriba-izq 3) 2000))
         (cond ((= arriba-der 0) 0)
  			       ((= arriba-der 1) 20)
  			       ((= arriba-der 2) 300)
  			       ((= arriba-der 3) 2000)))))
  	      (let* ((altura (altura-columna tablero columna))
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
            (cond ((= abajo 0) 0)
       			       ((= abajo 1) 20)
       			       ((= abajo 2) 300)
       			       ((= abajo 3) 1500))
       			 (cond ((= der 0) 0)
       			       ((= der 1) 20)
       			       ((= der 2) 300)
       			       ((= der 3) 1500))
       			 (cond ((= izq 0) 0)
       			       ((= izq 1) 20)
       			       ((= izq 2) 300)
       			       ((= izq 3) 1500))
       			 (cond ((= abajo-izq 0) 0)
       			       ((= abajo-izq 1) 20)
       			       ((= abajo-izq 2) 300)
       			       ((= abajo-izq 3) 1500))
              (cond ((= abajo-der 0) 0)
       			       ((= abajo-der 1) 20)
       			       ((= abajo-der 2) 300)
       			       ((= abajo-der 3) 1500))
              (cond ((= arriba-izq 0) 0)
       			       ((= arriba-izq 1) 20)
       			       ((= arriba-izq 2) 300)
       			       ((= arriba-izq 3) 1500))
              (cond ((= arriba-der 0) 0)
       			       ((= arriba-der 1) 20)
       			       ((= arriba-der 2) 300)
       			       ((= arriba-der 3) 1500))))))
  	(- puntuacion-actual puntuacion-oponente)))))



    (defun heuristica2 (estado)
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
            (let* ((altura (altura-columna tablero columna))
             (fila (1- altura))
             (arriba (contar-arriba tablero ficha-actual columna fila))
             (abajo (contar-abajo tablero ficha-actual columna fila))
             (der (contar-derecha tablero ficha-actual columna fila))
             (izq (contar-izquierda tablero ficha-actual columna fila))
             (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
             (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
             (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
             (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
        (setf puntuacion-actual
              (+ puntuacion-actual

           (cond ((= abajo-izq 0) 0)
                 ((= abajo-izq 1) 10)
                 ((= abajo-izq 2) 100)
                 ((= abajo-izq 3) 1000))
           (cond ((= abajo-der 0) 0)
                 ((= abajo-der 1) 10)
                 ((= abajo-der 2) 100)
                 ((= abajo-der 3) 1000))
           (cond ((= arriba-izq 0) 0)
                 ((= arriba-izq 1) 10)
                 ((= arriba-izq 2) 100)
                 ((= arriba-izq 3) 1000))
           (cond ((= arriba-der 0) 0)
                 ((= arriba-der 1) 10)
                 ((= arriba-der 2) 100)
                 ((= arriba-der 3) 1000)))))
            (let* ((altura (altura-columna tablero columna))
             (fila (1- altura))
             (arriba (contar-arriba tablero ficha-oponente columna fila))
             (abajo (contar-abajo tablero ficha-oponente columna fila))
             (der (contar-derecha tablero ficha-oponente columna fila))
             (izq (contar-izquierda tablero ficha-oponente columna fila))
             (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
             (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
             (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
             (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
        (setf puntuacion-oponente
              (+ puntuacion-oponente

               (cond ((= abajo-izq 0) 0)
                     ((= abajo-izq 1) 10)
                     ((= abajo-izq 2) 100)
                     ((= abajo-izq 3) 1000))
                (cond ((= abajo-der 0) 0)
                     ((= abajo-der 1) 10)
                     ((= abajo-der 2) 100)
                     ((= abajo-der 3) 1000))
                (cond ((= arriba-izq 0) 0)
                     ((= arriba-izq 1) 10)
                     ((= arriba-izq 2) 100)
                     ((= arriba-izq 3) 1000))
                (cond ((= arriba-der 0) 0)
                     ((= arriba-der 1) 10)
                     ((= arriba-der 2) 100)
                     ((= arriba-der 3) 1000))))))
      (- puntuacion-actual puntuacion-oponente)))))

  (defun heuristica3 (estado)
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
          (let* ((altura (altura-columna tablero columna))
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
         (cond ((= abajo 0) 0)
               ((= abajo 1) 0)
               ((= abajo 2) 0)
               ((= abajo 3) 0))
         (cond ((= der 0) 0)
               ((= der 1) 0)
               ((= der 2) 0)
               ((= der 3) 0))
         (cond ((= izq 0) 0)
               ((= izq 1) 0)
               ((= izq 2) 0)
               ((= izq 3) 0))
         (cond ((= abajo-izq 0) 0)
               ((= abajo-izq 1) 0)
               ((= abajo-izq 2) 0)
               ((= abajo-izq 3) 0))
         (cond ((= abajo-der 0) 0)
               ((= abajo-der 1) 0)
               ((= abajo-der 2) 0)
               ((= abajo-der 3) 0))
         (cond ((= arriba-izq 0) 0)
               ((= arriba-izq 1) 0)
               ((= arriba-izq 2) 0)
               ((= arriba-izq 3) 0))
         (cond ((= arriba-der 0) 0)
               ((= arriba-der 1) 0)
               ((= arriba-der 2) 0)
               ((= arriba-der 3) 0)))))
          (let* ((altura (altura-columna tablero columna))
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
         (cond ((= abajo 0) 0)
               ((= abajo 1) 50)
               ((= abajo 2) 500)
               ((= abajo 3) 5000))
         (cond ((= der 0) 0)
               ((= der 1) 50)
               ((= der 2) 500)
               ((= der 3) 5000))
         (cond ((= izq 0) 0)
               ((= izq 1) 50)
               ((= izq 2) 500)
               ((= izq 3) 5000))
         (cond ((= abajo-izq 0) 0)
               ((= abajo-izq 1) 50)
               ((= abajo-izq 2) 500)
               ((= abajo-izq 3) 5000))
         (cond ((= abajo-der 0) 0)
               ((= abajo-der 1) 50)
               ((= abajo-der 2) 500)
               ((= abajo-der 3) 5000))
         (cond ((= arriba-izq 0) 0)
               ((= arriba-izq 1) 50)
               ((= arriba-izq 2) 500)
               ((= arriba-izq 3) 5000))
         (cond ((= arriba-der 0) 0)
               ((= arriba-der 1) 50)
               ((= arriba-der 2) 500)
               ((= arriba-der 3) 5000))))))
    (- puntuacion-actual puntuacion-oponente)))))

(defun heuristica4 (estado)
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

(defun heuristica5 (estado)
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
                              (t 0))
                         ; (cond ((and (= columna 3) (fila 1)) 10)
                         ;       (t 0))
                         (cond ((= fila 0) 3)
                               ((= fila 2) 2)
                               ((= fila 4) 1)
                               (t 0))
                           )))
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
                              (t 0))
                        ; (cond ((and (= columna 3) (fila 1) ) 10)
                        ;       (t 0))
                        (cond ((= fila 0) 3)
                              ((= fila 2) 2)
                              ((= fila 4) 1)
                              (t 0))
                           )))))
        (- puntuacion-actual puntuacion-oponente)))))

(defun heuristica6 (estado)
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
	(loop for columna from 0 below (1- (tablero-ancho tablero)) do
	      (loop for fila from 0 below (1- (tablero-alto tablero)) do

                (let* (
                       ;(fila (1- altura))
                       (abajo (contar-abajo tablero ficha-actual columna fila))
                       (der (contar-derecha tablero ficha-actual columna fila))
                       (izq (contar-izquierda tablero ficha-actual columna fila))
                       (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
                       (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
                       (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
                       (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
                  (setf puntuacion-actual
                        (+ puntuacion-actual
                        (if (NULL (obtener-ficha tablero columna fila))
                            (if (= (mod fila 2) 0)
                                (cond ((>= abajo 3) 30)
                                    ((>= (+ der izq) 3) 30)
                                    ((>= (+ arriba-izq abajo-der) 3) 30)
                                    ((>= (+ arriba-der abajo-izq) 3) 30)
                                    (t 0))
                                (cond ((>= abajo 3) 20)
                                    ((>= (+ der izq) 3) 20)
                                    ((>= (+ arriba-izq abajo-der) 3) 20)
                                    ((>= (+ arriba-der abajo-izq) 3) 20)
                                    (t 0))
                              )

                           (cond ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (= columna 3)) 100)
                                 ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (or (= columna 2) (= columna 4))) 50)
                                 ((and (eql (obtener-ficha tablero columna fila) ficha-actual)
                                      (or (= columna 1) (= columna 5))) 25)
                                 (t 0))))))
                (let* (
                       ;(fila (1- altura))
                       (abajo (contar-abajo tablero ficha-oponente columna fila))
                       (der (contar-derecha tablero ficha-oponente columna fila))
                       (izq (contar-izquierda tablero ficha-oponente columna fila))
                       (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
                       (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
                       (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
                       (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
                  (setf puntuacion-oponente
                        (+ puntuacion-oponente
                        (if (NULL (obtener-ficha tablero columna fila))

                        (cond ((>= abajo 3) 20)
                              ((>= (+ der izq) 3) 20)
                              ((>= (+ arriba-izq abajo-der) 3) 20)
                              ((>= (+ arriba-der abajo-izq) 3) 20)
                              (t 0))
                        ; (cond ((and (= columna 3) (fila 1) ) 10)
                        ;       (t 0))
                        ; (cond ((= fila 0) 3)
                        ;       ((= fila 2) 2)
                        ;       ((= fila 4) 1)
                        ;       (t 0))
                           0)
                           )))))
        (- puntuacion-actual puntuacion-oponente)))))

(defun heuristica7 (estado)
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

(defun heuristica8 (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
   (ficha-actual (estado-turno estado))
   (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
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

(defun evaluarBloque (b1 b2 b3 b4 ficha-actual ficha-oponente)
  (let ((fichasNuestras 0)
        (fichasVacias 0)
        (fichasRival 0)
        (score 0))
    (setf fichasNuestras
          (+ fichasNuestras
             (cond ((eql b1 ficha-actual) 1)
                   (t 0))
             (cond ((eql b2 ficha-actual) 1)
                   (t 0))
             (cond ((eql b3 ficha-actual) 1)
                   (t 0))
             (cond ((eql b4 ficha-actual) 1)
                   (t 0))
             ))
     (setf fichasRival
           (+ fichasRival
              (cond ((eql b1 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b2 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b3 ficha-oponente) 1)
                    (t 0))
              (cond ((eql b4 ficha-oponente) 1)
                    (t 0))
              ))
      (setf fichasVacias
            (+ fichasVacias
               (cond ((NULL b1) 1)
                     (t 0))
               (cond ((NULL b2) 1)
                     (t 0))
               (cond ((NULL b3) 1)
                     (t 0))
               (cond ((NULL b4) 1)
                     (t 0))
               ))
      (setf score
            (+ score
               (cond ((= fichasNuestras 4) 100)
                     ((and (= fichasNuestras 3) (= fichasVacias 1)) 5)
                     ((and (= fichasNuestras 2) (= fichasVacias 2)) 2)
                     (t 0))
               (if (and (= fichasRival 3) (= fichasVacias 1))
                    -4 0)))
    score))


;; -------------------------------------------------------------------------------
;; Jugadores
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))

(defvar *jugador1* (make-jugador :nombre 'Jugador1
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica1))

(defvar *jugador2* (make-jugador :nombre 'Jugador2
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica2))

(defvar *jugador3* (make-jugador :nombre 'Jugador3
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica3))

(defvar *jugador4* (make-jugador :nombre 'Jugador4
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica4))
(defvar *jugador5* (make-jugador :nombre 'Jugador5
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica5))
(defvar *jugador6* (make-jugador :nombre 'Jugador6
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica6))
(defvar *jugador7* (make-jugador :nombre 'Jugador7
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica7))
(defvar *jugador8* (make-jugador :nombre 'Jugador8
              :f-jugador #'f-jugador-negamax
				      :f-eval  #'heuristica8))

;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
;(print (partida *jugador-humano* *jugador5*))
;(print (partida *jugador-aleatorio* *jugador-humano*))
;(print (partida *jugador-bueno* *jugador4*))
;(print (partida *jugador4* *jugador-bueno*))
;(print (partida *jugador-bueno* *jugador5*))
;(print (partida *jugador4* *jugador6* ))
;(print (partida *jugador4* *jugador5*))
;(print (partida *jugador5* *jugador4*))
;(print (partida *jugador-bueno* *jugador3*))
(print (partida *jugador-humano* *jugador8* 5))
;(print (partida *jugador-bueno* *jugador2*))
;(print (partida *jugador2* *jugador-bueno*))
;;(load "P4_IA_2018_2019_jugadores.v0.cl")
;;
