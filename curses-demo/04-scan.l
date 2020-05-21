(load "../curses.l")

(de scan-demo ()
  (let (msg "Enter a string: "
        stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr))
    (mvprintw (/ row 2) (/ (- col (length msg)) 2) "%s" msg)
    (mvprintw (- row 2) 0 "You Entered: %s" (getstr)))
  (getch)
  (endwin)
  (bye))

(scan-demo)
