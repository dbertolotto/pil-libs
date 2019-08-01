(load "../curses.l")

(de color-demo ()
  (let (stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr))
    (ifn (has-colors)
      (prog
        (printf "Your terminal does not support colors")
        (refresh))
      (prog
        (start-color)
        (init-pair 1 cRed cBlack)
        (attron (color-pair 1))
        (print-middle stdscr (/ row 2) 0 col "Viola !!! In color ...")
        (refresh)
        (attroff (color-pair 1)))))
  (getch)
  (endwin)
  (bye))

(de print-middle (win sy sx w s)
  (let (l (length s)
        tmp (/ (- w l) 2)
        x (+ sx tmp))
    (mvwprintw win sy x "%s" s)))

(color-demo)
