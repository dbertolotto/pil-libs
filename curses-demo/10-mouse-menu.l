(load "../curses.l")

(de mouse-demo ()
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
    (attron aReverse)
    (mvprintw 23 1 "Click on Exit to quit (Work best in a virtual console)")
    (refresh)
    (attroff aReverse)
    (print-menu win hl choices)
    (mousemask mAllEvents NIL)
    (loop
      (let (c (wgetch win))
        (cond
          ((! = c keyMouse)
           (let (event (getmouse)
                 ex (cadr event)
                 ey (caddr event)
                 emask (caddddr event))
             (if (= emask mButton1Pressed)
               (let ch (report-choice (+ ex 1) (+ ey 1) sx sy choices)
                 (if (= -1 ch)
                   (endwin)
                   (prog
                     (mvprintw 23 0 "You chose choice %d with choice string %s\n" ch (nth choices ch T))
                     (refresh))))))))))
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

(de report-choice (mx my sx sy choices)
  (let (i (+ sx 2)
        j (+ sy 3)
        pch -1)
    (for (n . ch) choices
      (if (and (= my (+ j ch))
               (>= mx i)
               (<= mx (+ i (length ch))))
        (ifn (= n (length choices))
          (setq pch (+ n 1)))))
    pch))

(mouse-demo)

