#(symbols 'curses 'pico)

(local) paths
(setq paths '("libncursesw.so"
              "libncursesw.so.6"
              "libncursesw.so.5"
              "libncurses.so"
              "libncurses.so.6"
              "libncurses.so.5"
              "/data/data/com.termux/files/usr/lib/libncurses.so"))

(local) find-lib
(de find-lib ()
  (or (find '((path) (n0 (native "@" "dlopen" 'N path 257))) paths)
      (quit "No CURSES found")))

(local) curses lib
(de curses @
  (pass 'native (if (get lib) lib (find-lib))))

(local) keyF
(de keyF (X)
  (+ X 264))

(local) '(keyDown keyUp keyLeft keyRight)
(setq
  keyDown 402
  keyUp 403
  keyLeft 404
  keyRight 405)

(local) ncursesBits
(de ncursesBits (M S)
   (>> (- (+ S 8)) M) )

(local) '(aStandout aUnderline aReverse aBlink aDim aBold aAltcharset aInvis aProtect aHorizontal aLeft aLow aRight aTop aVertical aItalic)
(setq 
  aStandout (ncursesBits 1 8)
  aUnderline (ncursesBits 1 9)
  aReverse (ncursesBits 1 10)
  aBlink (ncursesBits 1 11)
  aDim (ncursesBits 1 12)
  aBold (ncursesBits 1 13)
  aAltcharset (ncursesBits 1 14)
  aInvis (ncursesBits 1 15)
  aProtect (ncursesBits 1 16)
  aHorizontal (ncursesBits 1 17)
  aLeft (ncursesBits 1 18)
  aLow (ncursesBits 1 19)
  aRight (ncursesBits 1 20)
  aTop (ncursesBits 1 21)
  aVertical (ncursesBits 1 22)
  aItalic (ncursesBits 1 23))

(local) '(cBlack cRed cGreen cYellow cBlue cMagenta cCyan cWhite)
(setq
  cBlack 0
  cRed 1
  cGreen 2
  cYellow 3
  cBlue 4
  cMagenta 5
  cCyan 6
  cWhite 7)

(local) color-pair
(de color-pair (n) (ncursesBits n 0))

(local) initscr
(de initscr () (curses "initscr" 'N))

(local) '(printw mvprintw wprintw mvwprintw)
(de printw @
  (pass 'curses "printw" NIL))
(de mvprintw (y x . @)
  (pass 'curses "mvprintw" NIL y x))
(de wprintw (win . @)
  (pass 'curses "wprintw" NIL win))
(de mvwprintw (y x win . @)
  (pass 'curses "mvwprintw" NIL win y x))

(local) '(refresh wrefresh)
(de refresh ()
  (curses "refresh" 'I))
(de wrefresh (win)
  (curses "wrefresh" 'I win))

(local) '(getch cgetch)
(de getch ()
  (curses "getch" 'I))
(de cgetch ()
  (char (getch)))

(local) endwin
(de endwin ()
  (curses "endwin" NIL))

(local) hello
(de hello ()
  (initscr)
  (printw "Hello world!")
  (refresh)
  (getch)
  (endwin)
  (bye))

(local) raw
(de raw ()
  (curses "raw" NIL))

(local) cbreak
(de cbreak ()
  (curses "cbreak" NIL))

(local) echo
(de echo ()
  (curses "echo" NIL))

(local) noecho
(de noecho ()
  (curses "noecho" NIL))

(local) keypad
(de keypad (win bl)
  (curses "keypad" NIL win bl))

(local) halfdelay
(de halfdelay (delay)
  (curses "halfdelay" NIL delay))

(local) attron
(de attron @
  (curses "attron" 'I (pass '|)))

(local) attroff
(de attroff @
  (curses "attroff" 'I (pass '|)))

(local) attrset
(de attrset @
  (curses "attrset" 'I (pass '|)))

(local) '(chgat mvchgat wchgat mvwchgat)
(de chgat (n attr pair)
  (curses "chgat" 'I n attr pair NIL))
(de mvchgat (y x n attr pair)
  (curses "mvchgat" 'I y x n attr pair NIL))
(de wchgat (win n attr pair)
  (curses "wchgat" 'I win n attr pair NIL))
(de mvwchgat (win y x n attr pair)
  (curses "chgat" 'I win y x n attr pair NIL))

(local) '(standend wstandend standout wstandout)
(de standend () (curses "standend" 'I))
(de wstandend (win) (curses "wstandend" 'I win))
(de standout () (curses "standout" 'I))
(de wstandout (win) (curses "wstandout" 'I win))

(local) newwin
(de newwin (nl nc y x)
  (curses "newwin" 'N nl nc y x))

(local) init-demo
(de init-demo ()
  (let (stdscr (initscr))
    (raw)
    (keypad stdscr T)
    (noecho))
  (printw "Type any character to see it in bold\n")
  (let (ch (cgetch))
    (if (= ch "q")
      (printw "q key pressed")
      (prog
        (printw "The pressed key is ")
        (attron aBold)
        (printw "%s" ch)
        (attroff aBold)
        )))
  (refresh)
  (getch)
  (endwin)
  (bye))

(local) '(addch mvaddch waddch mvwaddch)
(de addch (ch)
  (curses "addch" NIL ch))
(de mvaddch (row col ch)
  (curses "mvaddch" NIL row col ch))
(de waddch (win ch)
  (curses "waddch" NIL win ch))
(de mvwaddch (row col win ch)
  (curses "mvwaddch" NIL row col win ch))

(local) vwprintw
(de vwprintw (win . @)
  (pass 'curses "vwprintw" NIL win))

(local) '(getmaxy getmaxx)
(de getmaxy (win)
  (curses "getmaxy" 'I win))
(de getmaxx (win)
  (curses "getmaxx" 'I win))

(local) getmaxyx
(de getmaxyx (win row col)
  (curses "getmaxyx" win row col))

(local) print-demo
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

(local) '(addstr mvaddstr waddstr mvwaddstr)
(de addstr @
  (pass 'curses "addstr" NIL))
(de mvaddstr (y x . @)
  (pass 'curses "mvaddstr" NIL y x))
(de waddstr (win . @)
  (pass 'curses "waddstr" NIL win))
(de mvwaddstr (y x win . @)
  (pass 'curses "mvwaddstr" NIL y x win))

(local) '(scanw mvscanw wscanw mvwscanw)
(de scanw @
  (pass 'curses "scanw" NIL))
(de mvscanw (y x . @)
  (pass 'curses "mvscanw" NIL y x))
(de wscanw (win . @)
  (pass 'curses "wscanw" NIL win))
(de mvwscanw (y x win . @)
  (pass 'curses "mvwscanw" NIL y x win))

(local) vwscanw
(de vwscanw (win . @)
  (pass 'curses "vwscanw" NIL win))

(local) charar
(de charar (size)
  (list 's (cons size 'C size)))

(local) '(getstr mvgetstr wgetstr mvwgetstr)
(de getstr ()
  (let (s NIL)
    (curses "getstr" 'I (charar 100))
    (pack s)))
(de mvgetstr (y x . @)
  (pass 'curses "mvgetstr" 'N y x))
(de wgetstr (win . @)
  (pass 'curses "wgetstr" 'N win))
(de mvwgetstr (y x win . @)
  (pass 'curses "mvwgetstr" 'N y x win))

(local) scan-demo
(de scan-demo ()
  (let (msg "Enter a string: "
        stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr))
    (initscr)
    (mvprintw (/ row 2) (/ (- col (length msg)) 2) "%s" msg)
    (mvprintw (- row 2) 0 "You Entered: %s" (getstr)))
  (getch)
  (endwin)
  (bye))

(local) '(start_color init_pair)
(de start_color () (curses "start_color" 'I))
(de init_pair (pair f b) (curses "init_pair" 'I pair f b))

(local) chgat-demo
(de chgat-demo ()
  (initscr)
  (start_color)
  (init_pair 1 cCyan cBlack)
  (printw "A Big string which i didn't care to type fully ")
  (mvchgat 0 0 -1 aBlink 1)
  (refresh)
  (getch)
  (endwin)
  (bye))

(local) box
(de box (win verch horch)
  (curses "box" 'I win verch horch))

(local) delwin
(de delwin (win)
  (curses "delwin" 'I))

(local) wborder
#{
The parameters taken are
* 1. win: the window on which to operate
* 2. ls: char for the left side of the window
* 3. rs: char for the right side of the window
* 4. ts: char for the top side of the window
* 5. bs: char for the bottom side of the window
* 6. tl: char for the top left corner of the window
* 7. tr: char for the top right corner of the window
* 8. bl: char for the bottom left corner of the window
* 9. br: char for the bottom right corner of the window
}#
(de wborder (win ls rs ts bs tl tr bl br)
  (curses "wborder" 'I win ls rs ts bs tl tr bl br))

(local) '(winbox-demo _create-win _destroy-win)
(de winbox-demo ()
  (let (stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr)
        h 3
        w 10
        sy (/ (- row h) 2)
        sx (/ (- col w) 2)
        win (_create-win h w sy sx))
    (cbreak)
    (noecho)
    (keypad stdscr T)
    (printw "Press q to exit")
    (refresh)
    (let (ch (cgetch))
      (while (not (= ch "q"))
        (cond
          ((= ch "h")
           (_destroy-win win)
           (setq sx (- sx 1))
           (setq win (_create-win h w sy sx)))
          ((= ch "l") 
           (_destroy-win win)
           (setq sx (+ sx 1))
           (setq win (_create-win h w sy sx)))
          ((= ch "k")
           (_destroy-win win)
           (setq sy (- sy 1))
           (setq win (_create-win h w sy sx)))
          ((= ch "j")
           (_destroy-win win)
           (setq sy (+ sy 1))
           (setq win (_create-win h w sy sx))))
        (setq ch (cgetch))))
    (endwin)
    (bye)))

(de _create-win (h w sx sy)
  (let (win (newwin h w sx sy))
    (box win 0 0)
    (wrefresh win)
    win))

(de _destroy-win (win)
  (wborder win 32 32 32 32 32 32 32 32)
  (wrefresh win)
  (delwin win))

(local) '(hline mvhline whline mvwhline)
(de hline (ch n)
  (curses "hline" 'I ch n))
(de hline (y x ch n)
  (curses "mvhline" 'I y x ch n))
(de hline (win ch n)
  (curses "whline" 'I win ch n))
(de hline (win y x ch n)
  (curses "mvwhline" 'I win y x ch n))

(local) '(vline mvvline wvline mvwvline)
(de vline (ch n)
  (curses "vline" 'I ch n))
(de vline (y x ch n)
  (curses "mvvline" 'I y x ch n))
(de vline (win ch n)
  (curses "wvline" 'I win ch n))
(de vline (win y x ch n)
  (curses "mvwvline" 'I win y x ch n))

(local) '(border-demo +Border +Win create-box)
(de border-demo ()
  (let (stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr)
        winp (new '(+Win) row col)
        pair (init_pair 1 cCyan cBlack))
    (start_color)
    (attron (color-pair 1))
    (printw "press q to quit")
    (attroff (color-pair 1))
    (cbreak)
    (noecho)
    (keypad stdscr T))
  (endwin))

(clasd +Border)
(dm T ()
  (=: ls "|")
  (=: rs "|")
  (=: ts "-")
  (=: bs "-")
  (=: tl "+")
  (=: tr "+")
  (=: bl "+")
  (=: br "+"))

(class +Win +Border)
(dm T (row col)
  (super)
  (=: h 3)
  (=: w 10)
  (=: sy (/ (- row h) 2))
  (=: sx (/ (- col w) 2)))

(de create-box (winp bl)
  (let (x (; winp sx)
        y (; winp sy)
        w (; winp w)
        h (; winp h))
    (if bl
      (prog
        (mvaddch y x (; winp tl))
        (mvaddch y (+ x w) (; winp tr))
        (mvaddch (+ y h) x (; winp bl))
        (mvaddch (+ y h) (+ x w) (; winp br))
        (mvhline y (+ x 1) (; winp ts) (- w 1))
        (mvhline (+ y h) (+ x 1) (; winp bs) (- w 1))
        (mvvline (+ y 1) x (; winp ls) (- h 1))
        (mvvline (+ y 1) (+ x w) (; winp rs) (- h 1)))
      (for (j y (<= j (+ y h)) (inc j))
        (for (i x (<= i (+ x w) (inc i)))
	  (mvaddch j i 32)))))
  (refresh))
