#(symbols 'curses 'pico)

(local) Lib
(setq Lib "/data/data/com.termux/files/usr/lib/libncurses.so")

(local) curses
(de curses @
  (pass 'native Lib))

(local) keyF
(de keyF (X)
  (+ X 264))

(local) (keyDown keyUp keyLeft keyRight)
(setq
  keyDown 402
  keyUp 403
  keyLeft 404
  keyRight 405)

(local) ncursesBits
(de ncursesBits (M S)
   (>> (- (+ S 8)) M) )

(local) (aStandout aUnderline aReverse aBlink aDim aBold aAltcharset aInvis aProtect aHorizontal aLeft aLow aRight aTop aVertical aItalic)
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

(local) (cBlack cRed cGreen cYellow cBlue cMagenta cCyan cWhite)
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

(local) (printw mvprintw wprintw mvwprintw)
(de printw @
  (pass 'curses "printw" NIL))
(de mvprintw (y x . @)
  (pass 'curses "mvprintw" NIL y x))
(de wprintw (win . @)
  (pass 'curses "wprintw" NIL win))
(de mvwprintw (y x win . @)
  (pass 'curses "mvwprintw" NIL win y x))

(local) (refresh wrefresh)
(de refresh ()
  (curses "refresh" 'I))
(de wrefresh (win)
  (curses "wrefresh" 'I win))

(local) getch
(de getch ()
  (curses "getch" 'I))

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

(local) (chgat mvchgat wchgat mvwchgat)
(de chgat (n attr pair)
  (curses "chgat" 'I n attr pair NIL))
(de mvchgat (y x n attr pair)
  (curses "mvchgat" 'I y x n attr pair NIL))
(de wchgat (win n attr pair)
  (curses "wchgat" 'I win n attr pair NIL))
(de mvwchgat (win y x n attr pair)
  (curses "chgat" 'I win y x n attr pair NIL))

(local) (standend wstandend standout wstandout)
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
  (let (ch (getch))
    (if (= ch (keyF 1))
      (printw "F1 key pressed")
      (prog
        (printw "The pressed key is ")
        (attron aBold)
        (printw "%c" ch)
        (attroff aBold)
        )))
  (refresh)
  (getch)
  (endwin)
  (bye))

(local) (addch mvaddch waddch mvwaddch)
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

(local) (getmaxy getmaxx)
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

(local) (addstr mvaddstr waddstr mvwaddstr)
(de addstr @
  (pass 'curses "addstr" NIL))
(de mvaddstr (y x . @)
  (pass 'curses "mvaddstr" NIL y x))
(de waddstr (win . @)
  (pass 'curses "waddstr" NIL win))
(de mvwaddstr (y x win . @)
  (pass 'curses "mvwaddstr" NIL y x win))

(local) (scanw mvscanw wscanw mvwscanw)
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

(local) (getstr mvgetstr wgetstr mvwgetstr)
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
    #getmaxyx(stdscr,row,col)
    (mvprintw (/ row 2) (/ (- col (length msg)) 2) "%s" msg)
    (mvprintw (- row 2) 0 "You Entered: %s" (getstr)))
  (getch)
  (endwin)
  (bye))

(local) (start_color init_pair)
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
(de wborder (win ls rs ts bs tl tr bl br)
  (curses "wborder" 'I win ls rs ts bs tl tr bl br))

(local) (winbox-demo _create-newwin _destroy-win)
(de winbox-demo ()
  (let (stdscr (initscr)
        row (getmaxy stdscr)
        col (getmaxx stdscr)
        height 3
        width 10
        starty (/ (- row height) 2)
        startx (/ (- col width) 2)
        mywin (_create-newwin height width starty startx))
    (cbreak)
    (keypad stdscr T)
    (printw "Press F1 to exit")
    (refresh)
    (let (ch (getch))
      (loop
        (T (= ch (keyF 1)) NIL)
        (case ch
          (keyLeft (_destroy-win mywin)
                   (setq startx (- startx 1))
                   (_create-newwin height width starty startx))
          (keyRight (_destroy-win mywin)
                    (setq startx (+ startx 1))
                    (_create-newwin height width starty startx))
          (keyUp (_destroy-win mywin)
                 (setq starty (- starty 1))
                 (_create-newwin height width starty startx))
          (keyDown (_destroy-win mywin)
                   (setq starty (+ starty 1))
                   (_create-newwin height width starty startx)))
        (setq ch (getch))))
    (endwin)))

(de _create-newwin (h w sx sy)
  (let (win (newwin h w sx sy))
    (box win 0 0)
    (wrefresh win)
    win))

(de _destroy-win (win)
  (wborder win " " " " " " " " " " " " " " " ")
  (wrefresh win)
  (delwin win))
#{ 
The parameters taken are
* 1. win: the window on which to operate
* 2. ls: character to be used for the left side of the window
* 3. rs: character to be used for the right side of the window
* 4. ts: character to be used for the top side of the window
* 5. bs: character to be used for the bottom side of the window
* 6. tl: character to be used for the top left corner of the window
* 7. tr: character to be used for the top right corner of the window
* 8. bl: character to be used for the bottom left corner of the window
* 9. br: character to be used for the bottom right corner of the window
}#
