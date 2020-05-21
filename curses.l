#(symbols 'curses 'pico)

(setq *Curses-mouse-version 2)

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

(local) '(keyDown keyUp keyLeft keyRight keyMouse)
(setq
  keyDown 402
  keyUp 403
  keyLeft 404
  keyRight 405
  keyMouse 631)

(local) ncursesBits
(de ncursesBits (M S)
   (>> (- (+ S 8)) M) )

(local) ncursesMouseMask
(de ncursesMouseMask (M B)
  (let N (if (> *Curses-mouse-version 1) 5 6)
    (>> (* (- B 1) N) M)))

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

(setq
  mButtonReleased 1
  mButtonPressed 2
  mButtonClicked 4
  mDoubleClicked 10
  mTripleClicked 20
  mReservedEvent 40)

(setq
  mButton1Released (ncursesMouseMask 1 mButtonReleased)
  mButton1Pressed (ncursesMouseMask 1 mButtonPressed)
  mButton1Clicked (ncursesMouseMask 1 mButtonClicked)
  mButton1DoubleClicked (ncursesMouseMask 1 mDoubleClicked)
  mButton1TripleClicked (ncursesMouseMask 1 mTripleClicked)
  mButtonCtrl (ncursesMouseMask 5 1)
  mButtonShift (ncursesMouseMask 5 2)
  mButtonAlt (ncursesMouseMask 5 4)
  mReportMousePosition (ncursesMouseMask 5 10))

(setq mAllEvents (- mReportMousePosition 1))

(local) color-pair
(de color-pair (n) (ncursesBits n 0))

(local) '(start-color init-pair init-color has-colors can-change-color color-content pair-content)
(de start-color ()
  (curses "start_color" 'I))
(de init-pair (pair fg bg)
  (curses "init_pair" 'I pair fg bg))
(de init-color (color r g b)
  (curses "init_color" 'I color r g b))
(de has-colors ()
  (curses "has_colors" 'B))
(de can-change-color ()
  (curses "can_change_color" 'B))
(de color-content (color)
  (use (r g b)
    (curses "color_content" 'I color '(r (4 . B) 0) '(g (4 . B) 0) '(b (4 . B) 0))
    (list r g b)))
(de pair-content (pair)
  (use (fg bg)
    (curses "pair_content" 'I pair '(fg (4 . B) 0) '(bg (4 . B) 0))
    (cons fg bg)))

(local) initscr
(de initscr ()
  (curses "initscr" 'N))

(local) '(printw mvprintw wprintw mvwprintw)
(de printw @
  (pass 'curses "printw" 'I))
(de mvprintw (y x . @)
  (pass 'curses "mvprintw" 'I y x))
(de wprintw (win . @)
  (pass 'curses "wprintw" 'I win))
(de mvwprintw (win y x . @)
  (pass 'curses "mvwprintw" 'I win y x))

(local) '(refresh wrefresh)
(de refresh ()
  (curses "refresh" 'I))
(de wrefresh (win)
  (curses "wrefresh" 'I win))

(local) '(getch mvgetch wgetch mvwgetch)
(de getch ()
  (curses "getch" 'I))
(de mvgetch (y x)
  (curses "mvgetch" 'I y x))
(de wgetch (win)
  (curses "wgetch" 'I win))
(de mvwgetch (win y x)
  (curses "mvwgetch" 'I win y x))
(de has-key (ch)
  (curses "has_key" 'I ch))

(de getwch ()
  (use wint
    (curses "get_wch" 'I '(wint (4 . I)))
    wint))

(local) cgetch
(de cgetch ()
  (char (getch)))

(local) endwin
(de endwin ()
  (curses "endwin" NIL))

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

(local) '(attron wattron)
(de attron @
  (curses "attron" 'I (pass '|)))
(de wattron (win . @)
  (curses "wattron" 'I win (pass '|)))

(local) '(attroff wattroff)
(de attroff @
  (curses "attroff" 'I (pass '|)))
(de wattroff (win . @)
  (curses "wattroff" 'I win (pass '|)))

(local) '(attrset wattrset)
(de attrset @
  (curses "attrset" 'I (pass '|)))
(de wattrset (win . @)
  (curses "wattrset" 'I win (pass '|)))

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

(local) '(getcury getcurx getmaxy getmaxx)
(de getcury (win)
  (curses "getcury" 'I win))
(de getcurx (win)
  (curses "getcurx" 'I win))
(de getmaxy (win)
  (curses "getmaxy" 'I win))
(de getmaxx (win)
  (curses "getmaxx" 'I win))

(local) '(getyx getmaxyx)
(de getyx (win)
  (cons (getcury win) (getcurx win)))
(de getmaxyx (win)
  (cons (getmaxy win) (getmaxx win)))

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

(local) '(move wmove)
(de move (y x)
  (curses "move" 'I y x))
(de wmove (win y x)
  (curses "wmove" 'I win y x))

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

(local) '(hline mvhline whline mvwhline)
(de hline (ch n)
  (curses "hline" 'I ch n))
(de mvhline (y x ch n)
  (curses "mvhline" 'I y x ch n))
(de whline (win ch n)
  (curses "whline" 'I win ch n))
(de mvwhline (win y x ch n)
  (curses "mvwhline" 'I win y x ch n))

(local) '(vline mvvline wvline mvwvline)
(de vline (ch n)
  (curses "vline" 'I ch n))
(de mvvline (y x ch n)
  (curses "mvvline" 'I y x ch n))
(de wvline (win ch n)
  (curses "wvline" 'I win ch n))
(de mvwvline (win y x ch n)
  (curses "mvwvline" 'I win y x ch n))

(local) '(erase werase clear wclear clrtobot wclrtobot clrtoeol wclrtoeol)
(de erase ()
  (curses "erase" 'I))
(de werase (win)
  (curses "werase" 'I win))
(de clear ()
  (curses "clear" 'I))
(de wclear (win)
  (curses "wclear" 'I win))
(de clrtobot ()
  (curses "clrtobot" 'I))
(de wclrtobot (win)
  (curses "wclrtobot" 'I win))
(de clrtoeol ()
  (curses "clrtoeol" 'I))
(de wclrtoeol (win)
  (curses "wclrtoeol" 'I win))

(de has-mouse ()
  (curses "has_mouse" 'B))
(de getmouse ()
  (use mevent
    (curses "getmouse" 'I '(mevent (B I I I N)))
    mevent))
(de ungetmouse (mevent)
  (curses "ungetmouse" 'I mevent))
(de mousemask (newmask oldmask)
  (curses "mousemask" 'N newmask oldmask))
(de wenclose (win y x)
  (curses "wenclose" 'B win y x))
(de mouseinterval (interval)
  (curses "mouseinterval" 'I interval))
