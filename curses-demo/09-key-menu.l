(load "../curses.l")

(de key-demo ()
  (let (stdscr (initscr)
        col (getmaxx stdscr)
        row (getmaxy stdscr)
        w 20
        h 10
        sx (/ (- col w) 2)
        sy (/ (- row h) 2)
        choices '("Choice 1"
                  "Choice 2"
                  "Choice 3"
                  "Choice 4"
                  "Exit")
        n (length choices)
        hl 1
        ch 0
        win (newwin h w sy sx))
    (initscr)
    (clear)
    (noecho)
    (cbreak)
    (keypad win T)
    (mvprintw 0 0 "Use arrow keys to go up and down, Press enter to select a choice\n")
    (refresh)
    (print-menu win hl choices)
    (loop
      (let (c (wgetch win))
        (cond
          ((= c (char "k"))
           (if (= hl 1)
             (setq hl n)
             (setq hl (dec hl))))
          ((= c (char "j"))
           (if (= hl n)
             (setq hl 1)
             (setq hl (inc hl))))
          ((= c 10)
           (setq ch hl))
          (T (mvprintw 25 0 "Character pressed is = %3d Hopefully it can be printed as '%c'" c c)
             (refresh))))
      (print-menu win hl choices)
      (NIL (= ch 0) (mvprintw 23 0 "You chose choice %d with choice string %s\n" ch (nth choices ch T))))
    (clrtoeol)
    (refresh)
    (getch)
    (endwin)
    (bye)))

(de print-menu (win hl choices)
  (box win 0 0)
  (let (x 2
        y 2)
    (for (i . ch) choices
      (if (= hl i)
        (prog
          (wattron win aReverse)
          (mvwprintw win (+ y i) x ch)
          (wattroff win aReverse))
        (mvwprintw win (+ y i) x ch))))
  (wrefresh win))

(key-demo)
