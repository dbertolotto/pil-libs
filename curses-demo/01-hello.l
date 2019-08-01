(load "../curses.l")

(de hello-demo ()
  (initscr)
  (printw "Hello world!")
  (refresh)
  (getch)
  (endwin)
  (bye))

(hello-demo)
