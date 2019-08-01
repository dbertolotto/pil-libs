(load "../curses.l")

(de print-demo ()
  (let (msg "Just a string"
        stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr))
    (mvprintw (/ row 2) (/ (- col (length msg)) 2) "%s" msg)
    (mvprintw (- row 2) 0 "This screen has %d rows and %d columns\n" row col)
    (printw "Try resizing your window (if possible) and then run this program again"))
  (refresh)
  (getch)
  (endwin)
  (bye))

(print-demo)
