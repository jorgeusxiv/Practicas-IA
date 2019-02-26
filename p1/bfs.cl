; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; B r e a d t h - f i r s t - s e a r c h in graphs
; ;;
( defun bfs ( end queue net )
  ( if ( null queue ) '()
    ( let* (( path ( first queue ))
      ( node ( first path )))
      ( if ( eql node end )
        ( reverse path )
        ( bfs end
          ( append (rest queue) ( new-paths path node net ))
        net )))))

( defun new-paths ( path node net )
  ( mapcar #'( lambda ( n ) (cons n path )) ( rest ( assoc node net ))))
; ;;


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
