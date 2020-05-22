(load "curses.l")

(seed (time))

(setq
  *BoardX 20
  *BoardY 15)

(de snake-reset ()
  (list (cons (/ *BoardX 2) (/ *BoardY 2))))

(de rand-apple (Snake)
  (let Apple (car Snake)
    (while (member Apple Snake)
      (setq Apple (cons (rand 1 *BoardX)
                        (rand 1 *BoardY))))
    Apple))

(de dir-reset () 'Down)

(de score-reset () 0)

(de next-pos (Snake Dir)
  (let Head (car Snake)
    (case Dir
      (Down (cons (car Head) (inc (cdr Head))))
      (Up (cons (car Head) (dec (cdr Head))))
      (Right (cons (inc (car Head)) (cdr Head)))
      (Left (cons (dec (car Head)) (cdr Head))))))

(test (0 . 1) (next-pos '((0 . 0)) 'Down))
(test (0 . -1) (next-pos '((0 . 0)) 'Up))
(test (1 . 0) (next-pos '((0 . 0)) 'Right))
(test (-1 . 0) (next-pos '((0 . 0)) 'Left))

(de collide-body? (Snake Next)
  (member Next Snake))

(de collide-border? (Next)
  (let ((X . Y) Next)
    (or (= X 0) (= X (inc *BoardX))
        (= Y 0) (= Y (inc *BoardY)))))

(de eat-apple? (Apple Next)
  (= Apple Next))

(de update-snake (OldSnake OldApple Next)
  (let Snake (insert 1 OldSnake Next)
    (ifn (eat-apple? OldApple Next)
      (flip (cdr (flip Snake)))
      Snake)))

(test '((0 . 1)) (update-snake '((0 . 0)) '(3 . 3) '(0 . 1)))
(test '((0 . 1) (0 . 0)) (update-snake '((0 . 0)) '(0 . 1) '(0 . 1)))

(de game-loop ()
  (draw-board *Win *Snake *Apple *Score)
  (setq *Dir (get-user-input *Dir))
  (let (Next (next-pos *Snake *Dir)
        Ate (eat-apple? *Apple Next))
    (setq *Lost (or (collide-body? *Snake Next)
                    (collide-border? Next)))
    (setq *Snake (update-snake *Snake *Apple Next))
    (setq *Apple (if Ate (rand-apple *Snake) *Apple))
    (setq *Score (if Ate (inc *Score) *Score))))

(de snake-game ()
  (until *Lost
    (game-loop))
  (endwin)
  (bye))

(de draw-board (Win Snake Apple Score)
  (box Win 0 0)
  (wattron Win (color-pair 3))
  (for X *BoardX
    (for Y *BoardY
      (mvwaddch Win Y X (char " "))))
  (wattroff Win)
  (wattron Win (color-pair 2))
  (for S Snake
    (mvwaddch Win (cdr S) (car S) (char " ")))
  (wattroff Win)
  (wattron Win (color-pair 1))
  (mvwaddch Win (cdr Apple) (car Apple) (char " "))
  (wattroff Win)
  (wrefresh Win)
  (mvprintw 0 0 "Use arrow keys (or hjkl) to move the snake.\nPress 'q' to quit. Press 'r' to restart the game.")
  (mvprintw (dec (getmaxy *Stdscr)) 0 "Score: %3d" Score)
  (refresh))

(de init-graphics ()
  (let (Col (getmaxx *Stdscr)
        Row (getmaxy *Stdscr)
        W (+ 2 *BoardX)
        H (+ 2 *BoardY)
        Sx (/ (- Col W) 2)
        Sy (/ (- Row H) 2)
        Win (newwin H W Sy Sx))
    (clear)
    (noecho)
    (cbreak)
    (keypad *Stdscr T)
    (keypad Win T)
    (halfdelay 5)
    (start-color)
    (init-pair 1 cRed cRed) # apple
    (init-pair 2 cWhite cWhite) # snake
    (init-pair 3 cBlack cBlack) # background
    (wrefresh Win)
    Win))

(de n= (A B)
  (not (= A B)))

(de get-user-input (Dir)
  (let Ch (getch)
    (cond
      ((or (= Ch keyUp) (= (char Ch) "k"))
         (if (n= Dir 'Down) 'Up Dir))
      ((or (= Ch keyDown) (= (char Ch) "j"))
         (if (n= Dir 'Up) 'Down Dir))
      ((or (= Ch keyLeft) (= (char Ch) "h"))
         (if (n= Dir 'Right) 'Left Dir))
      ((or (= Ch keyRight) (= (char Ch) "l"))
         (if (n= Dir 'Left) 'Right 'Dir))
      ((= (char Ch) "q") (endwin) (bye))
      (T Dir))))

(setq *Snake (snake-reset))
(setq *Apple (rand-apple *Snake))
(setq *Dir (dir-reset))
(setq *Score (score-reset))
(setq *Lost NIL)
(setq *Stdscr (initscr))
(setq *Win (init-graphics))

(snake-game)
