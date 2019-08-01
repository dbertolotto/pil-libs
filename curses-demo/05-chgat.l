(load "../curses.l")

(de chgat-demo ()
  (initscr)
  (start-color)
  (init_pair 1 cCyan cBlack)
  (printw "A Big string which I didn't care to type fully ")
  (mvchgat 0 0 -1 aBlink 1)
  (refresh)
  (getch)
  (endwin)
  (bye))

(chgat-demo)
