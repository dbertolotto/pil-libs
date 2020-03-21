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

(mouse-demo)

	/* Get all the mouse events */
	mousemask(ALL_MOUSE_EVENTS, NULL);

	while(1)
	{	c = wgetch(menu_win);
		switch(c)
		{	case KEY_MOUSE:
			if(getmouse(&event) == OK)
			{	/* When the user clicks left mouse button */
				if(event.bstate & BUTTON1_PRESSED)
				{	report_choice(event.x + 1, event.y + 1, &choice);
					if(choice == -1) //Exit chosen
						goto end;
					mvprintw(22, 1, "Choice made is : %d String Chosen is \"%10s\"", choice, choices[choice - 1]);
					refresh();
				}
			}
			print_menu(menu_win, choice);
			break;
		}
	}
end:
	endwin();
	return 0;
}

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
