(setq *Num-players 2
      *Max-dice 3
      *Board-size 2)

(setq *Board-hexnum (* *Board-size *Board-size))

(de gen-board ()
  (make
    (do *Board-hexnum
      (link (list (rand 1 *Num-players)
                  (rand 1 *Max-dice))))))

(de player-letter (N)
  (char (+ 96 N)))

(de draw-board (Board)
  (for I *Board-size
    (prin (pack (need (- *Board-size I) "  ")))
    (for J *Board-size
      (let (Hex (car (nth Board (+ (* *Board-size (- I 1)) J))))
        (prin (player-letter (car Hex)) "-" (cadr Hex) " ")))
    (prinl)))

(test "  a-3 a-2 ^Jb-3 b-1 ^J" (pipe (draw-board '((1 3) (1 2) (2 3) (2 1))) (till NIL T)))

(de game-tree (Board Player Spare-dice First-move)
  (list Player
        Board
        (add-passing-move Board
                          Player
                          Spare-dice
                          First-move
                          (attacking-moves Board Player Spare-dice))))

(de add-passing-move (Board Player Spare-dice First-move Moves)
  (if First-move
    Moves
    (cons (list NIL
                  (game-tree (add-new-dice Board Player (- Spare-dice 1))
                             (% Player *Num-players)
                             0
                             T))
            Moves)))

(de attacking-moves (Board Cur-player Spare-dice)
  (let (player '((Pos) (caar (nth Board Pos)))
        dice '((Pos) (cadar (nth Board Pos))))
    (mapcan '((Src) (when (= (player Src) Cur-player)
                      (mapcan '((Dst) (when (and (not (= (player Dst) Cur-player))
                                                 (> (dice Src) (dice Dst)))
                                        (list
                                          (list (list Src Dst)
                                                (game-tree (board-attack Board Cur-player Src Dst (dice Src))
                                                           Cur-player
                                                           (+ Spare-dice (dice Dst))
                                                           NIL)))))
                              (neighbors Src))))
            (range 1 *Board-hexnum))))

(de neighbors (Pos)
  (let (Up (- Pos *Board-size)
        Down (+ Pos *Board-size))
    (filter '((P) (and (> P 0) (<= P *Board-hexnum)))
            (append (list Up Down)
                    (unless (= 1 (% Pos *Board-size))
                      (list (- Up 1) (- Pos 1)))
                    (unless (= 1 (% (+ 1 Pos) *Board-size))
                      (list (+ 1 Pos) (+ 1 Down)))))))

(test '(1 4) (neighbors 3))

(de board-attack (Board Player Src Dst Dice)
  (make
    (for (Pos . Hex) Board
      (link (cond ((= Pos Src) (list Player 1))
                  ((= Pos Dst) (list Player (- Dice 1)))
                  (T Hex))))))

(test '((1 3) (1 1) (2 3) (1 2)) (board-attack '((1 3) (1 3) (2 3) (2 1)) 1 2 4 3))

(de add-new-dice (Board Player Spare-dice)
  (let (f '((Lst N)
             (cond ((= NIL Lst) NIL)
                   ((=0 N) Lst)
                   (T (let (Cur-player (caar Lst)
                            Cur-dice (cadar Lst))
                        (if (and (= Cur-player Player) (< Cur-dice *Max-dice))
                          (cons (list Cur-player (+ 1 Cur-dice))
                                (f (cdr Lst) (- N 1)))
                          (cons (car Lst)
                                (f (cdr Lst) N))))))))
    (f Board Spare-dice)))

(test '((1 2) (2 3) (1 3) (2 1)) (add-new-dice '((1 1) (2 3) (1 2) (2 1)) 1 2))

(test '(1 ((1 1) (2 1) (1 2) (2 1)) (((3 4) (1 ((1 1) (2 1) (1 1) (1 1)) ((NIL (1 ((1 1) (2 1) (1 1) (1 1)) NIL))))))) (game-tree '((1 1) (2 1) (1 2) (2 1)) 1 0 T))


