'Othello Max by Stephane Edwardson (Lodovik)


'                MMBasic for Windows specifics
'--------------------------------------------------------
IF MM.DEVICE$ = "MMBasic for Windows" THEN

  CONST MMB4W = 1

  FONT 1,1        'Workaround for bug in MMB4W
  changepalette   'Workaround for bug in MMB4W
  RANDOMIZE TIMER 'MMB4W: seed the random number generator

END IF

'--------------------------------------------------------



MODE 12,8  '960x540, 256 colors
OPTION ANGLE DEGREES

CONST PROGNAME$ = "OTHELLO MAX"
CONST AUTHOR$ = "Stephane Edwardson"
CONST YEAR$ = "2022"
CONST VER$ = "1.0.4"
CONST BRDSQUARE = INT(MM.VRES-100)/8  'Size of a square on the board
CONST BRDPC = BRDSQUARE / 2.5           'Size of pieces
CONST BKCOL = MAP(241)                'Screen background color
CONST BRDCOL = MAP(8)                 'Board background color
CONST BRDFRM = MAP(68)                'Board frame color
CONST BRDCOR = MAP(108)                'Board coordinates color
CONST P1COL = RGB(WHITE)              'Player 1 pieces color
CONST P2COL = RGB(BLACK)              'Player 2 pieces color
CONST WINDBAR = RGB(BLACK)            'Windows title background color
CONST WINDTXT = RGB(WHITE)            'Window bar text color
CONST WINDOUT = MAP(247)              'Window outline color
CONST WINDCON = MAP(249)              'Window content text color
CONST WINDBAK = MAP(8)                'Window background
CONST MAXSRCHD = 20                   'Max search depth
CONST BRDX = BRDSQUARE                'X location of the board on the screen 
CONST BRDY = BRDSQUARE                'Y location of the board on the screen
CONST ANIMSPD = 15                    'ms between frames of animation
CONST DEBUG = 0 '1
CONST SHOWSD = 3                      'Search depth visualisation

DIM soundon = 1                       'Sound ON, 0 = OFF

DIM winx1(10),winy1(10)               'Windows inside coordinates for easier reference 
DIM winx2(10),winy2(10)               'Window 1 = score
                                      'Window 2 = black player infos
                                      'Window 3 = white player infos
                                      'Window 4 = status display infos
                                      'Window 5 = Opening book status
                                      'Window 9 = Help screen


DIM board(64)                          'Board positions storage, player pieces on each square 1 = W, 2=B, 0=empty

DIM srch(64,8,8)                       'Pre-computed related squares in 8 (0-7) directions for each square (position, dir, movelist)
                                       'For each square (position) srch(x,,),
                                       'srch(x,0,0)    = number of directions to look
                                       'srch(x,1,0)    = number of squares to look in direction 1
                                       'srch(x,1,1..y) = enumeration of the squares
                                       'srch(x,2,0)    = number of squares to look in direction 2
                                       'and so on...
                                       'srch(x,0,1)    = total number of related squares


DIM drct(7) = (-8,-7,1,9,8,7,-1,-9)    'Search vectors for each of the 8 directions

DIM vmtbl(2187,2)                      'Tables of valid moves ,1 = white; ,2 = black (precomputed nbr of moves)
                                       'Given a Base3 encoded value, returns # of flipped pieces, 0 if no valid move
                                       'Reduces the search in each direction to a simple addition and table lookup
                                       'In concert with srch() table, only valid direction are calculated  
                                        
DIM base3(7,2)                         'Base3 values for faster conversion of board positions


DIM movsort(61,61)                      'Sorted moves to improve AB searches cutoff by looking at good positions first
                                        'List has one useless center position to permit scan after 60 moves to check if game is over
                                        'For each move the list of free squares is updated to the next index, removing the played move
                                        'movsort(0,1...60) contains the list of initial free squares (before move #1)
                                        'movsort(1,1...59) contains the updated list after a move (after move #1)
                                        'movsort(0,0) contains the number of moves played

DIM movlist(MAXSRCHD,30,2)              'List of possible moves at specified depth
                                        'movlist(xxx,0,P)    = # of possible moves for player P
                                        'movlist(xxx,1..x,P) = list of possible moves

DIM movflip(MAXSRCHD,30,2)              'List of # of squares flipped for each possible move, depth 
                                        'movflip(xxx,0,P)    = # of pieces flipped for player P
                                        'movflip(xxx,1..x,P) = list of flipped pieces for move 1..x, player P

DIM moves(75,22)                        'List of moves (xx,20) played so far with pieces that were flipped (60,xx) for each move
                                        'moves(0,0) = number of moves played in the game
                                        'moves(xx,0) = for move xx, number of pieces flipped
                                        'moves(xx,21) = position played
                                        'moves(xx,22) = player color (1 or 2)
                                        'moves(xx,1..20) = list of the pieces that were flipped for move xx
DIM flndx(2) = (0,2,1)                  'Flip the pieces with index instead if IF THEN

                                        'plmenu$() contains the various possible players to select 
DIM plmenu$(10)
  plmenu$(1) = "HUMAN"
  plmenu$(2) = "COMPUTER LEVEL 1"       'Totally random
  plmenu$(3) = "COMPUTER LEVEL 2"       'Opening book + 1-ply evaluation + 8-plies  exhaustive endings
  plmenu$(4) = "COMPUTER LEVEL 3"       'Opening book + 2 plies ahead    + 8-plies  exhaustive endings
  plmenu$(5) = "COMPUTER LEVEL 4"       'Opening book + 4 plies ahead    + 10-plies exhaustive endings
  plmenu$(6) = "COMPUTER LEVEL 5"       'Opening book + 6 plies ahead    + 10-plies exhaustive endings



DIM plsel(3) = (6,1,2,1)                'plsel(0) = nb of players choices, plsel(1) = black selection, plsel(2) = white selection, plsel(3) = Opening Book

DIM sqval(64)                           'Squares values for non minimax computer players
DIM statinfo$(10)                       'Lines of text to display in the status box; last line should be ""
DIM srchinfo$(5)                        'Lines of text to display in the status box for search type and status index(0) = title
DIM lastmv                              'Last move played
DIM lastscore$                          'Last score displayed; will be used for animation when updating board
DIM depth                               'Depth of search for accessing corresponding array
DIM plyr                                'Current player color 1=W, 2=B 
DIM esc                                 'Esc key pressed? 0 = No, 1 = Yes.
DIM pcnt(2)                             'Pieces count
DIM mvctr

DIM sideval(27,2)  'Sides next to corners hints
DIM diagval(9,2)   'Diag next to corners hints
DIM base3b(2,2)    'Base3 precalculated powers
DIM nodes          'count of positions evaluated
DIM ctrl$          'Controller detected; "": nothing, "NUNCHUCK", "CLASSIC"
DIM lastjoy        'Last state of the joystick
DIM joys=0.5       'Joystick sensivity in % of max value; lower = more sensible
DIM ju,jd,jl,jr    'Limits of analog joystick based on joys
 

DIM openbookb,openbookw,openname$,opencont$ 'Opening book active (if enabled), variation name, continuation
DIM opencount                               'Number of opening lines
DIM obmenu$(4)                              'Opening book choices
  obmenu$(1) = "BLACK: OFF -  WHITE: OFF"
  obmenu$(2) = "BLACK: ON  -  WHITE: ON "
  obmenu$(3) = "BLACK: OFF -  WHITE: ON "
  obmenu$(4) = "BLACK: ON  -  WHITE: OFF"


DIM obsel(1) = (4,2)                        '(0)=# of choices, (1)=default selection




'--------------------MAIN---------------------------




changepalette

initbase3
initsearcharray
initmovetables
initmovesortlist
initboard
initpatterns
checkctrl
countopenings
IF MMB4W THEN ctrl$="MOUSE"


DO
  RESTORE StartPosition
  lastscore$ = "0000"
  initgame
  initsquarevalues
  movsort(0,0) = 0  'Initial list of possible moves
  plyr = 2        'Turn
  depth = 0
  showvalidmoves
  selectplayers

  x=0
  TIMER=0
  DO WHILE x<2

    plyr=2
    n=getmovelist(0)
    n=movlist(0,0,plyr)
    showvalidmoves
    IF debug = 0 THEN
      statinfo$(1)="BLACK'S TURN"
      statinfo$(2)=""
      statusprint
    END IF
    IF n>0 THEN
      x=0
      SELECT CASE plsel(2)
        CASE 1
          m = getmove(m,plyr)
        CASE 2
          m = getopening()
          IF m = 0 THEN m = cp_johnrandom()
        CASE 3
          m = getopening()
          IF m = 0 THEN m = cp_stratend()
        CASE 4
          m = getopening()
          IF m = 0 THEN m = cp_complete3()
        CASE 5
          m = getopening()
          IF m = 0 THEN m = cp_complete5()
        CASE 6
          m = getopening()
          IF m = 0 THEN m = cp_complete7()
      END SELECT
      f = playmove(m,plyr)
      fliplastmove
      updatesortlist(m)
      findopening
      PAUSE 500
    ELSE
      INC x
      statinfo$(1)="BLACK CANNOT PLAY"
      statinfo$(2)=""
      statusprint
      PAUSE 1000
    ENDIF


    plyr = 1
    n=getmovelist(0)
    n=movlist(0,0,plyr)
    showvalidmoves
    IF debug = 0 THEN  
      statinfo$(1)="WHITE'S TURN"
      statinfo$(2)=""
      statusprint
    ENDIF
    IF n>0 THEN
      x=0
      SELECT CASE plsel(1)
        CASE 1
          m = getmove(m,plyr)
        CASE 2
          m = getopening()
          IF m = 0 THEN m = cp_johnrandom()
        CASE 3
          m = getopening()
          IF m = 0 THEN m = cp_stratend()
        CASE 4
          m = getopening()
          IF m = 0 THEN m = cp_complete3()
        CASE 5
          m = getopening()
          IF m = 0 THEN m = cp_complete5()
        CASE 6
          m = getopening()
          IF m = 0 THEN m = cp_complete7()
        CASE 7
     END SELECT
      f = playmove(m,plyr)
      fliplastmove
      updatesortlist(m)
      findopening
      PAUSE 500
    ELSE
      INC x
      statinfo$(1)="WHITE CANNOT PLAY"
      statinfo$(2)=""
      statusprint
      PAUSE 1000  
    ENDIF  
  LOOP

  IF plsel(1) = 1 OR plsel(2) = 1 THEN
    statinfo$(1)="END OF GAME"
    statinfo$(2)=" "
    statinfo$(3)="PRESS [ENTER] OR BUTTON"
    statinfo$(4)="TO CONTINUE"
    statinfo$(5)=""
    statusprint

    k$=""
    emptykeybuf   'Empty keyboard buffer
    DO WHILE k$<>"enter"
      k$=getinput$()
    LOOP 
    statinfo$(1)=" "
    statinfo$(2)=""
    statusprint
  ELSE
    statinfo$(1)="END OF GAME"
    statinfo$(2)=""
    statusprint
    PAUSE 5000
  END IF
  clearboard

LOOP



'--------------------SUBS & FUNCS---------------------------

SUB initmovesortlist    'Init the list of free square, sorted for best AB pruning
  LOCAL z

  RESTORE MoveOrder
  FOR z = 1 TO 61
    READ movsort(0,z)
  NEXT z

END SUB



SUB updatesortlist(m)   'Remove a played move from the list of free quares

  nmv = movsort(0,0)
  c = 0

  FOR z = 1 TO 61-nmv
    IF movsort(nmv,z) <> m THEN
      INC c
      movsort(nmv+1,c) = movsort(nmv,z) 'Create new list
    END IF
  NEXT z 
  INC movsort(0,0)


END SUB






SUB undomove        'Undo a move (opponent move(s) and player move before
  LOCAL m,lm,p1,z

  m  = moves(0,0)       '# of moves played

  IF m > 1 THEN         'At least 2 moves played? 
    p1 = plyr           'Player currently undoing the move
    lm = 0
    FOR z = 1 TO m      'Searches for last move (could be the one just before)
      IF moves(z,22) = p1 THEN lm = z
    NEXT z
    IF lm > 0 THEN
      FOR z = m TO lm STEP -1
        bm = moves(z,21)                  'Most recent move first
        clearpiece(bm)                    'Remove piece
        PAUSE 200
        back1move                         'Unplay move and restore state
        drawallpieces                     'Redraw pieces on screen
        movsort(0,0) = movsort(0,0) - 1   'Update counter for sorted free square lists
        PAUSE 400
      NEXT z
    END IF
  END IF

END SUB




SUB clearpiece(p)            'Clears one piece with animation
  LOCAL cx,cy,z


  FOR z=BRDPC TO 1 STEP -1
    cx = SquareX(p) + BRDSQUARE / 2 - 1
    cy = SquareY(p) + BRDSQUARE / 2 - 1
    CIRCLE cx,cy,z,2,,BRDCOL
    PAUSE ANIMSPD 
  NEXT z
      
END SUB



SUB drawallpieces 'Draw all the player pieces at once with animation (for undo move)
  LOCAL x,y,z,c,p


  FOR z = 1 TO BRDPC
    FOR p = 1 TO 64
      x = SquareX(p) + BRDSQUARE / 2 - 1
      y = SquareY(p) + BRDSQUARE / 2 - 1
      c = board(p) 

      SELECT CASE c
      CASE 1
        IF z = BRDPC THEN
          CIRCLE x,y,z,,,P2COL,P1COL
        ELSE
          CIRCLE x,y,z,,,P1COL,P1COL
        END IF
      CASE 2
        IF z = BRDPC THEN
          CIRCLE x,y,z,,,P1COL,P2COL
        ELSE
          CIRCLE x,y,z,,,P2COL,P2COL
        END IF
      CASE ELSE
        CIRCLE x,y,z,,,BRDCOL,BRDCOL
      END SELECT  
    NEXT p
    PAUSE ANIMSPD/2
  NEXT z

END SUB





SUB showhelp              'Show help screen in the center of the board
  LOCAL k$,s,z,x,y
  LOCAL t$(400),nl        'Help text in t$(), nl = number of lines
  LOCAL x1,x2,y1,y2,ml    'Coordinates of help windows (inside), ml=max number of lines (based on font)
  LOCAL fh = MM.INFO(FONTHEIGHT) +2 'Font height plus spacing
  LOCAL bw,bh,el,pt,upd

  FONT 1,1
  COLOR WINDTXT,WINDBAK
 
  RESTORE HelpText
  DO WHILE k$ <> "END"
    READ k$
    INC nl
    t$(nl) = k$
  LOOP  
  nl = nl-1

  s = BRDSQUARE / 4
  PAGE COPY 0 TO 2,I
  windowanim squarex(10)-s, squarey(10)-s, squarex(64)+s, squarey(64)+s, "HELP - PROGRAM V"+VER$,9

  x1 = winx1(9)       'Coords if the usable Help Windows
  x2 = winx2(9)
  y1 = winy1(9)
  y2 = winy2(9)
  bw = x2-x1          'Width and height of the Help Windows
  bh = y2-y1
  ml = INT(bh/fh)-1   'Max number of lines
  el = nl-ml+1        'el=end of list, but always calculated relative to the top of the window 

  pt = 1              'Pointer to the first line to display at the top
  upd = 1             'Update flag
  emptykeybuf         'Empty keyboard buffer
  DO WHILE k$ <> "enter" AND k$ <> "f1"
    k$ = getinput$()

    IF k$ = "down" THEN
      pt = pt+1
      IF pt > el THEN pt = el
      upd = 1
    ELSE IF k$ = "up" THEN
      pt = pt - 1
      IF pt < 1 THEN pt = 1
      upd = 1
    ELSE IF k$ = "right" THEN
      pt = pt + 10
      IF pt > el THEN pt = el
      upd = 1
    ELSE IF k$ = "left" THEN
      pt = pt - 10
      IF pt < 1 THEN pt = 1
      upd = 1
    END IF


    IF upd = 1 THEN
      upd = 0
      FOR z=1 TO ml
        printline t$(z+pt-1),z,x1,y1,bh,bw,2
      NEXT z
    END IF
  LOOP

  playtone 4500,4500,3
  fadeout squarex(10)-s, squarey(10)-s, squarex(64)+s, squarey(64)+s,2,0
END SUB



SUB printline(tx$,li,bx,by,bh,bw,sp)
  LOCAL fw,fh,cx,cy,t$


  fh = MM.INFO(FONTHEIGHT) + sp
  fw = MM.INFO(FONTWIDTH)
  cx = INT(bw/fw)-2             'Maximum # of char per line at current font width 
  cy = INT(bh/fh)               'Maximum of lines
  t$ = LEFT$(tx$+SPACE$(cx),cx) 'Crop text

  TEXT bx+fw,by+li*fh,t$,"LT"


END SUB


SUB fadeout(x1,y1,x2,y2,ps,pd)  'Fade out by copying from page ps to page pd
  LOCAL z,w,h,s,stx,sty,xb1,yb1,xb2,yb2,wb,wh  

  w = x2-x1+1
  h = y2-y1+1
  s = 60      'step
  stx = INT(w/s)
  sty = INT(h/s)
  xb1 = x1
  yb1 = y1
  xb2 = x2
  yb2 = y2
  wb  = w
  wh  = h

  PAGE WRITE pd
  FOR z = 1 TO INT(w/stx/2)
    BLIT xb1,yb1,xb1,yb1,wb,sty,ps
    BLIT xb1,yb2-sty,xb1,yb2-sty,wb,sty,ps
    BLIT xb1,yb1,xb1,yb1,stx,wh,ps
    BLIT xb2-stx,yb1,xb2-stx,yb1,stx,wh,ps

    xb1 = xb1 + stx
    yb1 = yb1 + sty
    xb2 = xb2 - stx
    yb2 = yb2 - sty
    wb = wb - 2*stx
    wh = wh - 2*sty
    PAUSE ANIMSPD/2 
  NEXT z
  PAGE COPY ps TO pd,B
END SUB




SUB windowanim(x1,y1,x2,y2,t$,i)  'Draw output windows; t$ = title, i = index to store inside coordinates in winx1()...Winy2() array
  LOCAL lw,tbx,tby,tbw,tbh,wx,wy,ww,wh
  LOCAL stx,sty,z


  lw = 1 
  tbx = x1+lw         'Window title box x,y,width,height
  tby = y1+lw         
  tbw = x2-x1-2*lw-1
  tbh = 20

  wx = x1             'Window x,y,width,height
  wy = y1
  ww = x2-x1-lw
  wh = y2-y1-lw

  winx1(i) = x1+2       'Store windows inside printable area
  winy1(i) = tby+tbh
  winx2(i) = x2
  winy2(i) = y2

  stx = ww/60
  sty = wh/60
  cwx = wx + ww/2
  cwy = wy + wh/2
  
  FOR z = 1 TO 60
    RBOX cwx-z*stx/2,cwy-z*sty/2,z*stx,z*sty,10,WINDOUT,WINDBAK
    PAUSE ANIMSPD/3
  NEXT z

  RBOX wx,wy,ww,wh,10,WINDOUT,WINDBAK
  RBOX tbx,tby,tbw,tbh,10,WINDBAR,WINDBAR
  TEXT tbx+tbw/2,tby+tbh/2+1,t$,"CM",4,1,WINDTXT,WINDBAR 

END SUB





SUB playtone(l,r,d)   'Plat a tone in L & R channels, D duration

  IF soundon = 1 THEN
    PLAY TONE l,r,d
  END IF
END SUB




FUNCTION getopening()   'Will return a valid opening book move in one is found; 0 if not
  LOCAL ob,op,oplist$,nbmatch,o$,n$,z,mo$,i,r

  op = 0                'Matching opening next move (if found, 0 if no moves)
  ob = 1                'Opening book ON
  SELECT CASE obsel(1)  'Menu selection will turn it OFF if disabled
    CASE 1
      ob = 0
    CASE 3
      IF plyr = 2 THEN ob = 0
    CASE 4
      IF plyr = 1 THEN ob = 0
  END SELECT  

  IF ob = 1 THEN
    IF moves(0,0) = 0 THEN  'First move? 
      r = INT(RND * opencount) + 1
      RESTORE OpeningBook
      FOR z = 1 TO r
        READ o$,n$
      NEXT z
      op = coord2dec(LEFT$(o$,2))
    ELSE
      mo$ = makemovelist$() 'Get move list of current game    
      RESTORE OpeningBook

      FOR z = 1 TO opencount
        READ o$,n$
        i = INSTR(o$,mo$) 'Is current game moves part of opening?
        IF i = 1 AND LEN(o$) > LEN(mo$) THEN
          INC nbmatch
          oplist$ = oplist$ + MID$(o$,LEN(mo$)+1,2)  'Add this opening's next move to the string
        END IF
      NEXT z
      IF nbmatch > 0 THEN
        r = INT(RND * nbmatch) * 2 + 1
        'showopen(str$(nbmatch)+":"+o$)
        op = coord2dec(MID$(oplist$,r,2))
      ELSE
        op = 0
      END IF
    END IF
  END IF

  getopening = op
END FUNCTION





FUNCTION makemovelist$()
  LOCAL z,mo$

  FOR z = 1 TO moves(0,0)
    mo$ = mo$ + dec2coord$(moves(z,21))
  NEXT z

  makemovelist$ = mo$
END FUNCTION





SUB findopening()       'Looks at the current moves and display corresponding book opening if found 
  LOCAL mo$,z,op$,nm$,ol
  
  mo$ = makemovelist$()
  IF LEN(mo$) < 4 THEN openname$=""
  RESTORE OpeningBook
  FOR z = 1 TO opencount
    READ op$,nm$
    l = LEN(op$)
    IF LEFT$(mo$,l) = op$ THEN
      IF l > ol THEN  'Longest matching series of moves will be the current opening
        openname$ = nm$
        ol = l
      END IF
    END IF
  NEXT z   

  showopen(openname$)    

END SUB




SUB countopenings   'Counts openings (opencount) in the DATA statements for later scanning
  LOCAL l$,n$
  
  RESTORE OpeningBook
  opencount = -1
  DO
    READ l$,n$
    INC opencount
  LOOP UNTIL l$="END"
END SUB





SUB back1move                        'Back a move; faster than recopying the entire board, mv = 0 for a null move
  LOCAL m,po,co,z

  m = moves(0,0)                                    'Get move count
  IF m > 0 THEN 
    po = moves(m,21)                                'Get position played
    co = moves(m,22)
    pcnt(co) = pcnt(co) - moves(m,0) - 1            'Update pieces count for player
    co = flndx(co)                                  'Get player color and invert it
    pcnt(co) = pcnt(co) + moves(m,0)                'Update opponent piece count
    FOR z = 1 TO moves(m,0)                         'board(m,0) = number of pieces to flip
      board(moves(m,z)) = co
    NEXT z
    board(po) = 0                                   'Remove piece from board (0=empty)
    m = m-1                                         'Decrement moves played
    moves(0,0) = m                                  'Update move number
  ENDIF

END SUB






FUNCTION playmove(po,co)           'Plays a move, flip pieces (optimized, not on screen); po = position 1..64; co = color 1,2; returns # of flipped pieces
  LOCAL DIR,npc,pat,flip,m,fc,mv


  fc = 0                                        'Flipped pieces count
  IF po > 0 THEN                                'check for null move
    m = moves(0,0)                              'Get move count
    INC m                                       'Increment moves played

    FOR DIR = 1 TO srch(po,0,0)                 'For each valid direction
      pat = board(srch(po,DIR,1))

      FOR npc = 2 TO srch(po,DIR,0)             'Other squares to add to the pattern using pre-calculated search limits
        pat = pat + base3(npc,board(srch(po,DIR,npc)))
      NEXT npc 

      IF vmtbl(pat,co) <> 0 THEN
        FOR flip = 1 TO vmtbl(pat,co)           'Number of pieces flipped by player co
          mv = srch(po,DIR,flip)
          INC fc
          moves(m,fc) = mv                      'Store the flipped piece coordinate
          board(mv) = co                        'Flip the piece
        NEXT flip
      ENDIF
    NEXT DIR
    moves(0,0) = m                              'Update move number
    moves(m,0) = fc                             'Store number of flipped pieces
    moves(m,21) = po                            'Store the position played at the end of the flipped array dimension
    moves(m,22) = co                            'Store the player color played at the end of the flipped array dimension
    board(po) = co

    pcnt(co) = pcnt(co) + fc + 1                'Update player pieces count            
    pcnt(flndx(co)) = pcnt(flndx(co)) - fc      'Update opponent pieces count
  ENDIF
  playmove = fc
END FUNCTION



FUNCTION cp_stratend()
  LOCAL mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2) 'Empty squares
  IF ml > 8 THEN
    mv = cp_strategic()
    srchinfo$(1)="1-ply strategic"
  ELSE
    srchinfo$(1)="7-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_stratend = mv


END FUNCTION



FUNCTION cp_complete3()
  LOCAL mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  IF ml > 8 THEN
    srchinfo$(1)="2-plies depth"
    mv = midgame(3)
  ELSE
    srchinfo$(1)="8-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_complete3 = mv


END FUNCTION





FUNCTION cp_complete4()
  LOCAL mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  IF ml > 8 THEN
    srchinfo$(1)="3-plies depth"
    mv = midgame(4)
  ELSE
    srchinfo$(1)="8-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_complete4 = mv


END FUNCTION



FUNCTION cp_complete5()
  LOCAL mv,ml
  
  ml = 64 - pcnt(1) - pcnt(2)
  IF ml > 10 THEN
    srchinfo$(1)="4-plies depth"
    mv = midgame(5)
  ELSE
    srchinfo$(1)="10-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_complete5 = mv

END FUNCTION





FUNCTION cp_complete6()
  LOCAL mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  IF ml > 10 THEN
    srchinfo$(1)="5-plies depth"
    mv = midgame(6)
  ELSE
    srchinfo$(1)="10-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_complete6 = mv

END FUNCTION




FUNCTION cp_complete7()
  LOCAL mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  IF ml > 11 THEN
    srchinfo$(1)="6-plies depth"
    mv = midgame(7)
  ELSE
    srchinfo$(1)="11-plies exhaustive"
    mv = endgame()
  ENDIF
  cp_complete7 = mv

END FUNCTION




FUNCTION midgame(maxdp)       'Midgame; Alpha-Beta search and eval function entry and start, maxdp=depth of search
LOCAL p1,p2,dp,nnmoves,p1moves,m,bst,bmv,t,f,alpha,beta,mdp,nod2,nps


  p1 = plyr                   'P1 is current player
  p2 = flndx(p1)              'P2 is the other
  dp = 1                      'Depth of search
  mdp = maxdp
  bst = -9999
  bmv = 0
  alpha = -9999
  beta = 9999
  TIMER = 0
  nodes = 0

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1

  srchinfo$(0)="ANALYSING POSITION"
  srchinfo$(2)=""
  srchinfo$(3)=""
  srchinfo$(4)=""
  statinfo$(1)=" "
  statinfo$(2)=""
  statusprint

  FOR m = 1 TO p1moves
    f = playmove(movlist(dp,m,p1),p1)
    showsearch(dp)
    t = midgame_min(p2,dp+1,alpha,beta,mdp)
    IF t > bst THEN   'MAX
      bst = t
      bmv = movlist(dp,m,p1)
      alpha = t
    ENDIF      
    nps = INT(nodes/TIMER*1000)
    srchinfo$(2)=STR$(nodes)
    srchinfo$(3)=STR$(nps)
    srchinfo$(4)=dec2coord$(bmv) + ", "+STR$(bst)

    back1move
  NEXT m

  midgame = bmv   

END FUNCTION




FUNCTION midgame_min(player,depth,alpha,beta,maxdp)       'Endgame; Alpha-Beta minimum
  LOCAL p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b,mdp


  p1 = player                  'P1 is other player
  p2 = flndx(p1)                         'P2 is current
  a = alpha
  b = beta
  dp = depth
  mdp = maxdp
  bst = 9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  IF dp < mdp THEN
    IF p1moves > 0 THEN
      FOR m = 1 TO p1moves
        f = playmove(movlist(dp,m,p1),p1)
        IF dp <= SHOWSD THEN showsearch(dp)
        t = midgame_max(p2,dp+1,a,b,mdp)
        'en = t
        back1move
        IF t < bst THEN bst = t
        b = MIN(b,t)
        IF b <= a THEN   'MIN
          EXIT FOR
        ENDIF      
      NEXT m
      en = bst
    ELSEIF p2moves > 0 THEN
      en = midgame_max(p2,dp+1,a,b,mdp)
    ELSE
      en = (pcnt(p2) - pcnt(p1)) * 100
    ENDIF
  ELSE
    en = p2moves - p1moves + poseval(p2)
    INC nodes
  ENDIF
  midgame_min = en

END FUNCTION



FUNCTION midgame_max(player,depth,alpha,beta,maxdp)       'Endgame; Alpha-Beta maximum
  LOCAL p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b,mdp
  LOCAL tt

  p1 = player                  'P1 is player
  p2 = flndx(p1)               'P2 is other player
  a = alpha
  b = beta
  dp = depth
  mdp = maxdp
  bst = -9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  IF dp < mdp THEN
    IF p1moves > 0 THEN
      FOR m = 1 TO p1moves
        f = playmove(movlist(dp,m,p1),p1)
        IF dp <= SHOWSD THEN showsearch(dp)
        t = midgame_min(p2,dp+1,a,b,mdp)
        'en = t
        back1move
        IF t > bst THEN bst = t
        a = MAX(a,t)
        IF a >= b THEN   'MAX
          EXIT FOR
        ENDIF      
      NEXT m
      en = bst
    ELSEIF p2moves > 0 THEN
      en = midgame_min(p2,dp+1,a,b,mdp)
    ELSE
      en = (pcnt(p1) - pcnt(p2)) * 100
    ENDIF
  ELSE
    en = p1moves - p2moves + poseval(p1)
    INC nodes
  ENDIF
  midgame_max = en

END FUNCTION


FUNCTION poseval(p)
  LOCAL en,m,p1

  p1 = p

  m = base3b(0,board(1)) + base3b(1,board(2)) + base3b(2,board(3))
  en = sideval(m,p1)
  m = base3b(0,board(1)) + base3b(1,board(9)) + base3b(2,board(17))
  en = en + sideval(m,p1)
  m = base3b(0,board(8)) + base3b(1,board(7)) + base3b(2,board(6))
  en = en + sideval(m,p1)
  m = base3b(0,board(8)) + base3b(1,board(16)) + base3b(2,board(24))
  en = en + sideval(m,p1)
  m = base3b(0,board(64)) + base3b(1,board(56)) + base3b(2,board(48))
  en = en + sideval(m,p1)
  m = base3b(0,board(64)) + base3b(1,board(63)) + base3b(2,board(62))
  en = en + sideval(m,p1)
  m = base3b(0,board(57)) + base3b(1,board(58)) + base3b(2,board(59))
  en = en + sideval(m,p1)
  m = base3b(0,board(57)) + base3b(1,board(49)) + base3b(2,board(41))
  en = en + sideval(m,p1)
  m = base3b(0,board(1)) + base3b(1,board(10))
  en = en + diagval(m,p1)
  m = base3b(0,board(8)) + base3b(1,board(15))
  en = en + diagval(m,p1)
  m = base3b(0,board(64)) + base3b(1,board(55))
  en = en + diagval(m,p1)
  m = base3b(0,board(57)) + base3b(1,board(50))
  en = en + diagval(m,p1)

  poseval = en
END FUNCTION




FUNCTION endgame()       'Endgame; play all the possible moves until game is over and chose the best outcome
  LOCAL p1,p2,dp,nnmoves,p1moves,m,bst,bmv,t,f,alpha,beta


  p1 = plyr                   'P1 is current player
  p2 = flndx(p1)              'P2 is the other
  dp = 1                      'Depth of search
  bst = -9999
  bmv = 0
  alpha = -9999
  beta = 9999
  TIMER = 0
  nps = 0

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1

  srchinfo$(2)=""
  srchinfo$(3)=""
  srchinfo$(4)=""
  statinfo$(1)=" "
  statinfo$(2)=""
  statusprint

  FOR m = 1 TO p1moves
    f = playmove(movlist(dp,m,p1),p1)
    IF dp <= SHOWSD THEN showsearch(dp)
    t = endgame_min(p2,dp+1,alpha,beta)
    IF t > bst THEN   'MAX
      bst = t
      bmv = movlist(dp,m,p1)
      alpha = t
    ENDIF      
    nps = INT(nodes/TIMER*1000)
    srchinfo$(2)=STR$(nodes)
    srchinfo$(3)=STR$(nps)
    srchinfo$(4)=dec2coord$(bmv) + ", "+STR$(bst)

    back1move
  NEXT m

  endgame = bmv   

END FUNCTION




FUNCTION endgame_min(player,depth,alpha,beta)       'Endgame; play all the possible moves until game is over and chose the best outcome
  LOCAL p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b


  p1 = player                  'P1 is other player
  p2 = flndx(p1)                         'P2 is current
  a = alpha
  b = beta
  dp = depth
  bst = 9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  IF p1moves > 0 THEN
    FOR m = 1 TO p1moves
      f = playmove(movlist(dp,m,p1),p1)
      IF dp <= SHOWSD THEN showsearch(dp)
      t = endgame_max(p2,dp+1,a,b)
      back1move
      IF t < bst THEN bst = t
      b = MIN(b,t)
      IF b <= a THEN   'MIN
        EXIT FOR
      ENDIF      
    NEXT m
    en = bst
  ELSEIF p2moves > 0 THEN
    en = endgame_max(p2,dp+1,a,b)
  ELSE
    en = pcnt(p2) - pcnt(p1)
    INC nodes
  ENDIF
  endgame_min = en

END FUNCTION



FUNCTION endgame_max(player,depth,alpha,beta)       'Endgame; play all the possible moves until game is over and chose the best outcome
  LOCAL p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b


  p1 = player                  'P1 is other player
  p2 = flndx(p1)                         'P2 is current
  a = alpha
  b = beta
  dp = depth
  bst = -9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  IF p1moves > 0 THEN
    FOR m = 1 TO p1moves
      f = playmove(movlist(dp,m,p1),p1)
      IF dp <= SHOWSD THEN showsearch(dp)
      t = endgame_min(p2,dp+1,a,b)
      back1move
      IF t > bst THEN bst = t
      a = MAX(a,t)
      IF a >= b THEN   'MAX
        EXIT FOR
      ENDIF      
    NEXT m
    en = bst
  ELSEIF p2moves > 0 THEN
    en = endgame_min(p2,dp+1,a,b)
  ELSE
    en = pcnt(p1) - pcnt(p2)
    INC nodes
  ENDIF
  endgame_max = en

END FUNCTION




SUB showsearch(depth)
  LOCAL sq,px,py,sz,wd,tw,tx,ty,sx,sy

  sq = 7                        'Square size
  sz = 9+9*sq                   'Board size + space
  wd = sz*SHOWSD                'Total size of preview
  tw = winx2(4)-winx1(4)        'Windows width
  px = winx1(4)+(tw-wd)/2       'x coord of the first board
  py = winy1(4)+6+24            'y coord of the first board
  tx = (winx1(4)+winx2(4))/2    'Search title center X position
  ty = (py +winy1(4))/2         'Search title center Y position
  sx = px
  sy = py+sz
  miniboard(px,py,sq,depth-1)
  srchstatprint(tx,ty,sx,sy)

END SUB



SUB srchstatprint(tx,ty,sx,sy)          'Display search infos in the status box
                                        'sinfo$(0) = title; tx,ty = titlepos; sx,sy = status position
                                        'status lines in sinfo$(1)-sinfo$(4)
  LOCAL z,fh,sp,l,h,x,y,si$(4)
  
  fh = 8         'font height to calculate center
  sp = 2          'line spacing

  TEXT tx,ty,srchinfo$(0),"CM",4,,WINDTXT,WINDBAK

  x = sx
  y = sy

  si$(1)="Search type : "+srchinfo$(1)
  si$(2)="Evaluations : "+srchinfo$(2)
  si$(3)="Evals / sec : "+srchinfo$(3)
  si$(4)="Best, score : "+srchinfo$(4)

  FOR z=1 TO 4
    TEXT x,y,LEFT$(si$(z)+SPACE$(40),40),"LT",7,,WINDTXT,WINDBAK
    y = y + fh + sp
  NEXT z

END SUB





SUB miniboard(xx,yy,s,p)   'Display 1-8 miniature boards positions
                         'xx,yy = coordinates, s = inside square size, p = position (1,2,3) )

  LOCAL stp,cx,cy,x,y,col(2),mv,lm,px,py,sz

  col(0)=BRDCOL : col(2)=RGB(BLACK) : col(1)=RGB(WHITE)
  sz = 9+9*s
  px = xx+sz*p
  py = yy
  stp = s+1


  FOR x=0 TO 7
    FOR y=0 TO 7  
      cx=px+x*stp
      cy=py+y*stp
      mv=cr2mov(x,y)
      BOX cx,cy,s+2,s+2,,WINDOUT,col(board(mv))
    NEXT y
  NEXT x

  lm = moves(moves(0,0),21)
  x = pos2col(lm)
  y = pos2row(lm)  
  BOX px+x*stp,py+y*stp,s+2,s+2,,RGB(RED),col(board(lm))  'Draw a red square around last positon played

END SUB





FUNCTION cp_strategic()         'Computer player. Plays strategic squares without any depth analysis
  LOCAL z,best,s,r$,y,n,mv,m,v


  n=movlist(0,0,plyr)
  best = 0
  mv = movlist(0,1,plyr)
  FOR z = 1 TO n
    m = movlist(0,z,plyr)
    v = sqval(m) + movflip(0,z,plyr)
    IF v+RND*15 >= best+RND*10 THEN
      mv = m
      best = v
    END IF 
  NEXT z

  sv = sqval(mv)
  SELECT CASE mv 'Adjusting values after a corner is played
    CASE 1
      sqval(2)=sv
      sqval(9)=sv
      sqval(3)=sqval(3)+  sv/2
      sqval(17)=sqval(17)+sv/2
      sqval(4)=sqval(4)+  sv/3
      sqval(25)=sqval(25)+sv/3
      sqval(5)=sqval(5)+  sv/4
      sqval(33)=sqval(33)+sv/4
      sqval(6)=sqval(6)+  sv/5
      sqval(41)=sqval(41)+sv/6
      sqval(10)=sv/4
    CASE 8
      sqval(7)=sv
      sqval(16)=sv
      sqval(6)=sqval(6)+  sv/2
      sqval(24)=sqval(24)+sv/2
      sqval(5)=sqval(5)+  sv/3
      sqval(32)=sqval(32)+sv/3
      sqval(4)=sqval(4)+  sv/4
      sqval(40)=sqval(40)+sv/4
      sqval(3)=sqval(3)+  sv/5
      sqval(48)=sqval(48)+sv/6
      sqval(15)=sv/4
    CASE 57
      sqval(49)=sv
      sqval(58)=sv
      sqval(41)=sqval(41)+sv/2
      sqval(59)=sqval(59)+sv/2
      sqval(33)=sqval(33)+sv/3
      sqval(60)=sqval(60)+sv/3
      sqval(25)=sqval(25)+sv/4
      sqval(61)=sqval(61)+sv/4
      sqval(17)=sqval(17)+sv/5
      sqval(62)=sqval(62)+sv/6
      sqval(50)=sv/4
    CASE 64
      sqval(56)=sv
      sqval(63)=sv
      sqval(48)=sqval(48)+sv/2
      sqval(62)=sqval(62)+sv/2
      sqval(40)=sqval(40)+sv/3
      sqval(61)=sqval(61)+sv/3
      sqval(32)=sqval(32)+sv/4
      sqval(60)=sqval(60)+sv/4
      sqval(24)=sqval(24)+sv/5
      sqval(59)=sqval(59)+sv/6
      sqval(55)=sv/4
  END SELECT  

  cp_strategic = mv
END FUNCTION




FUNCTION cp_johnrandom()    'Plays a random move
  LOCAL m,n

  n=movlist(0,0,plyr)
  m = movlist(0,INT(RND*n)+1,plyr)
  cp_johnrandom = m
END FUNCTION




SUB initsquarevalues      'Init a board with the relative positional values of the squares; used for easy-level computer oponents
  LOCAL s,z,y,r$

  RESTORE PositionHint
  s = 0
  FOR z = 1 TO 8
    READ r$
    FOR y = 1 TO 8
      INC s
      sqval(s) = 10 * VAL(MID$(r$,y,1))
    NEXT y
  NEXT z

END SUB




SUB checkctrl


  ctrl$=""
  ON ERROR SKIP 1
  CONTROLLER NUNCHUK OPEN
  IF MM.ERRNO = 0 THEN
    ctrl$="UNKNOWN"
    IF NUNCHUK(T) = &HA4200000 THEN
      ctrl$="NUNCHUK"
      jl=NUNCHUK(JXC) + (NUNCHUK(JXL) - NUNCHUK(JXC)) * joys
      jr=NUNCHUK(JXC) + (NUNCHUK(JXR) - NUNCHUK(JXC)) * joys
      ju=NUNCHUK(JYC) + (NUNCHUK(JYT) - NUNCHUK(JYC)) * joys
      jd=NUNCHUK(JYC) + (NUNCHUK(JYB) - NUNCHUK(JYC)) * joys
    ELSE IF NUNCHUK(T) = &HA4200101 THEN 
      CONTROLLER NUNCHUK CLOSE
      ON ERROR SKIP 1
      CONTROLLER CLASSIC OPEN
      IF MM.ERRNO = 0 THEN
        IF CLASSIC(T) = &HA4200101 THEN ctrl$="CLASSIC"
      END IF
    END IF
  END IF

END SUB




SUB selectplayers 'Select players before game; opening book menu added and hacked in later
  LOCAL k$,sel,pt$,t1,t2,np,pcol,ng,cnt$
  LOCAL sx(2) = (0,2,1)

  statinfo$(1) = "["+CHR$(146)+CHR$(147)+CHR$(149)+CHR$(148)+"] TO SELECT OPTIONS"
  statinfo$(2) = "[ENTER] OR BUTTON TO START GAME"
  statinfo$(3) = "[ESC] TO QUIT PROGRAM"
  statinfo$(4) = "[F1] FOR HELP"
  statinfo$(5) = " "
  statinfo$(6) = " "
  IF ctrl$ <> "" THEN statinfo$(6) = ctrl$+" CONTROLLER DETECTED"
  statinfo$(7) = " "
  statinfo$(8) = ""
  
  statusprint

  cnt$=ctrl$
  k$ = ""
  pt$ = CHR$(148)+CHR$(149)
  sel = 2
  showplayersel 1,"  "
  showplayersel 2,pt$
  showplayersel 3,"  "
  ng = 11
  ct = 0

  emptykeybuf   'Empty keyboard buffer
  DO WHILE k$ <> "enter"
 
    IF cnt$ <> ctrl$ THEN
      cnt$=ctrl$
      statinfo$(5) = " "
      IF ctrl$ <> "" THEN statinfo$(5) = ctrl$+" CONTROLLER DETECTED"
      statusprint
    END IF

    k$ = getinput$()
    SELECT CASE k$
      CASE "up"
        sel = sel-1
        IF sel < 1 THEN
          sel = 1
        ELSE
          showplayersel sel+1," "
          showplayersel sel,pt$
          ng = 11
        END IF
        k$ = ""
      CASE "down"
        sel = sel+1
        IF sel > 3 THEN
          sel = 3
        ELSE
          showplayersel sel-1," "
          showplayersel sel,pt$
          ng = 11
        END IF
        k$ = ""
      CASE "left"
        IF sel < 3 THEN
          plsel(sx(sel)) = plsel(sx(sel))-1
          IF plsel(sx(sel)) < 1 THEN
            plsel(sx(sel)) = 1
          ELSE
            showplayersel sel,pt$
            ng = 11
          END IF
        ELSE
          obsel(1) = obsel(1) - 1
          IF obsel(1) < 1 THEN
            obsel(1) = 1
          ELSE
            showplayersel 3,pt$
          END IF
          ng = 11
        END IF
        k$ = ""
      CASE "right"
        IF sel <3 THEN
          plsel(sx(sel)) = plsel(sx(sel))+1
          IF plsel(sx(sel)) > plsel(0) THEN
            plsel(sx(sel)) = plsel(0)
          ELSE
            showplayersel sel,pt$
            ng = 11
          END IF
        ELSE
          obsel(1) = obsel(1) + 1
          IF obsel(1) > obsel(0) THEN
            obsel(1) = obsel(0)
          ELSE
            showplayersel 3,pt$
          END IF
          ng = 11
        END IF
        k$ = ""
      CASE "enter"
        showplayersel sel," "
      CASE "esc"   
        END
      CASE "f1"
        showhelp
        ng = 11
    END SELECT

    IF plsel(1) > 1 AND plsel(2) > 1 THEN
      IF ct=1 THEN
        IF TIMER - t1 > 1000 THEN
          ng = ng - 1
          t1 = TIMER
          statinfo$(7) = "AUTOPLAY IN "+STR$(ng)+" SEC"
          statusprint
          IF ng = 0 THEN k$="enter"
        END IF
      ELSE
        ct = 1
      END IF

    ELSE
      ng =1
      t1 = TIMER
      IF ct = 1 THEN
        statinfo$(7) = " "
        statusprint
        ct=0
      END IF
    END IF
  LOOP

  playtone 4500,4500,3
  showplayersel sel," "
  statinfo$(1) = UCASE$(plmenu$(plsel(2)))
  statinfo$(2) = "VS"
  statinfo$(3) = UCASE$(plmenu$(plsel(1)))
  IF statinfo$(1) = statinfo$(3) THEN statinfo$(3) = "ITSELF"
  statinfo$(4) = ""
  statusprint


END SUB




SUB showplayersel(p,hl$)   'Show player selection and arrows pointing to selected player; 2 spaces will erase arrows)
  LOCAL x1,y1,x2,y2,h,pl

  h = 16
  IF p = 1 THEN       'Black player (P1)
    x1 = winx1(2)+1
    y1 = winy1(2)
    x2 = winx2(2)-2
    y2 = winy2(2)-2
  ELSE IF p = 2 THEN  'White player (P2)
    x1 = winx1(3)+1
    y1 = winy1(3)
    x2 = winx2(3)-2
    y2 = winy2(3)-2
  ELSE IF p = 3 THEN  'Opening book selection
    x1 = winx1(5)+1
    y1 = winy1(5)
    x2 = winx2(5)-2
    y2 = winy2(5)-2
  END IF

  RBOX x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK 'Erase previous content

  x = x1+(x2-x1)/2    'Center text
  y = y1 + (y2-y1)/2

  IF p < 3 THEN
    pl = 2
    IF p = 2 THEN pl = 1
    TEXT x,y,plmenu$(plsel(pl)),"CM",4,,WINDCON,WINDBAK   
    TEXT x1-12,y,LEFT$(hl$,1),"CM",4,2,RGB(WHITE),BKCOL
    TEXT x2+14,y,RIGHT$(hl$,1),"CM",4,2,RGB(WHITE),BKCOL
  ELSE
    TEXT x,y,obmenu$(obsel(1)),"CM",4,,WINDCON,WINDBAK   
    TEXT x1-12,y,LEFT$(hl$,1),"CM",4,2,RGB(WHITE),BKCOL
    TEXT x2+14,y,RIGHT$(hl$,1),"CM",4,2,RGB(WHITE),BKCOL
  END IF
END SUB



SUB showopen(o$)   'Show opening status
  LOCAL x1,y1,x2,y2,x,y

  x1 = winx1(5)+1
  y1 = winy1(5)
  x2 = winx2(5)-2
  y2 = winy2(5)-2
  END IF

  RBOX x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK 'Erase previous content

  x = x1+(x2-x1)/2    'Center text
  y = y1 + (y2-y1)/2

  TEXT x,y,UCASE$(o$),"CM",1,,WINDCON,WINDBAK   

END SUB





SUB statusprint       'Display text in the status box
  LOCAL x1,y1,x2,y2,fh,sp,l,h,x,y
  
  fh = 16         'font height to calculate center
  sp = 3          'line spacing
  x1 = winx1(4)+3
  y1 = winy1(4)
  x2 = winx2(4)-5
  y2 = winy2(4)-5
  BOX x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK
  l = 10
  FOR z=10 TO 1 STEP -1
    IF statinfo$(z) = "" THEN l=z-1   'Find last line
  NEXT z
  h = fh*l + sp*(l-1)
  x = x1+(x2-x1)/2
  y = y1 + (y2-y1-h+fh)/2

  FOR z=1 TO l
    TEXT x,y,statinfo$(z),"CM",4,,WINDTXT,WINDBAK
    y = y + fh + sp
  NEXT z

END SUB





SUB dprint(s$,p)        'Scroll up and print a line of text on the INFO window, for debug purpose.
  LOCAL x1,y1,x2,y2
  
  IF debug = 1 THEN
    x1 = winx1(4)+6
    y1 = winy1(4)+6
    x2 = winx2(4)-6
    y2 = winy2(4)-6
    BLIT x1,y1+9,x1,y1,x2-x1,y2-y1-9
    TEXT x1,y2,STRING$((x2-x1)/6," "),"LB",7,,WINDTXT,WINDBAK
    TEXT x1,y2,s$,"LB",7,,WINDTXT,WINDBAK
    IF p > 0 THEN PAUSE p
  END IF
END SUB




SUB displayscore    'Display / update the current score
  LOCAL x,y,ns$,DIR(4),dpx(4),dx,dy,p1x,p1y,p2x,p2y,z
  LOCAL brx1,brx2,bry2,brw,brh,lastb,newb,incb

  dx = INT((winx2(1)-winx1(1)-128) / 4)
  dy = INT((winy2(1)-winy1(1)-50) / 2)
    

  p1x = winx1(1)+dx/2 'Top left x location of first score digit for each player
  p1y = winy1(1)+dy-8 '
  p2x = p1x+64+3*dx
  p2y = winy1(1)+dy-8
  brx1 = p1x          'Bar graph score start x
  brx2 = p2x+64       'Bar graph score end x
  brw = brx2-brx1     'Bar graph score width
  bry1 = p1y+50+dx/2  'Bar graph score start y
  brh = bry1+dx/2-bry1'Bar graph score height

  ns$ = RIGHT$(STR$(100+pcnt(2)),2) + RIGHT$(STR$(100+pcnt(1)),2)  

  IF lastscore$="0000" THEN
    TEXT p1x,p1y,"00","LT",6,,RGB(BLACK),WINDBAK 'WINDBAK
    TEXT p2x,p2y,"00","LT",6,,RGB(WHITE),WINDBAK 'WINDBAK
    BOX brx1,bry1,brw,brh,,RGB(WHITE),RGB(WHITE)
    BOX brx1,bry1,brw/2,brh,,RGB(BLACK),RGB(BLACK)

  END IF


  PAGE WRITE 1
  TEXT p1x,p1y,LEFT$(lastscore$,2),"LT",6,,RGB(BLACK),WINDBAK 'WINDBAK
  TEXT p2x,p2y,RIGHT$(lastscore$,2),"LT",6,,RGB(WHITE),WINDBAK 'WINDBAK
  TEXT p1x,p1y+50,LEFT$(ns$,2),"LT",6,,RGB(BLACK),WINDBAK 'WINDBAK
  TEXT p2x,p2y+50,RIGHT$(ns$,2),"LT",6,,RGB(WHITE),WINDBAK 'WINDBAK
  TEXT p1x,p1y-50,LEFT$(ns$,2),"LT",6,,RGB(BLACK),WINDBAK 'WINDBAK
  TEXT p2x,p2y-50,RIGHT$(ns$,2),"LT",6,,RGB(WHITE),WINDBAK 'WINDBAK

  FOR z = 1 TO 4                                        'Set digits rotation directions
    IF MID$(lastscore$,z,1) > MID$(ns$,z,1) THEN
      DIR(z) = -1
    ELSE IF MID$(lastscore$,z,1) < MID$(ns$,z,1) THEN
      DIR(z) = 1
    ELSE
      DIR(z) = 0
    END IF
  NEXT z
  
  dpx(1) = p1x
  dpx(2) = p1x+32
  dpx(3) = p2x
  dpx(4) = p2x+32


  
  lastb = brx1 + scorepct(lastscore$) * brw
  newb = brx1 + scorepct(ns$) * brw
  incb = (newb-lastb)/50


  PAGE WRITE 0
  
  FOR z = 1 TO 50
    FOR x = 1 TO 4
      BLIT dpx(x),p1y+DIR(x)*z,dpx(x),p1y,32,50,1   'Rotate the score digits
    NEXT x

    IF incb > 0 THEN
      BOX brx1,bry1,lastb-brx1+z*incb,brh,,RGB(BLACK),RGB(BLACK)    'Increase black's score bar
    ELSE
      BOX lastb+z*incb,bry1,brx2-lastb-z*incb,brh,,RGB(WHITE),RGB(WHITE)  'Increase white's score bar
    END IF

    PAUSE ANIMSPD/6
  NEXT z
    
  lastscore$ = ns$
  
END SUB




FUNCTION scorepct(sc$)
  LOCAL p1,p2,pct

  
  p1 = VAL(LEFT$(sc$,2))
  p2 = VAL(RIGHT$(sc$,2))
  IF p1 + p2 > 0 THEN
    pct = p1/(p1+p2)
  ELSE
    pct = .5
  END IF

  scorepct = pct 
END FUNCTION





SUB showvalidmoves    'Highlight valid moves for current player
  LOCAL mv,n,bs,z,x1,y1,s

  mv = getmovelist(0)
  n = movlist(0,0,plyr)
  bs = BRDSQUARE / 16

  col = P1COL
  IF plyr = 2 THEN col = P2COL

  FOR z = 1 TO 64
    IF board(z) = 0 THEN
      x1 = SquareX(z) + BRDSQUARE / 2 - 1
      y1 = SquareY(z) + BRDSQUARE / 2 - 1
      CIRCLE x1,y1,bs,,1,BRDCOL,BRDCOL
    END IF
  NEXT z  

  IF n >0 THEN
    FOR z=1 TO n
      s = movlist(0,z,plyr)
      x1 = SquareX(s) + BRDSQUARE / 2 - 1
      y1 = SquareY(s) + BRDSQUARE / 2 - 1
      CIRCLE x1,y1,bs,,1,col,col
    NEXT z      
  END IF

END SUB





SUB fliplastmove      'Flip the pieces of last move played (animated)
  LOCAL lm,pcol,z,m,w,col1,col2,x1,y1,p,po$

  m = moves(0,0)      'Get # of moves
  lm = moves(m,21)    'Get last move played
  pcol = moves(m,22)  'Get color of last move
  playpiece lm,pcol

  SELECT CASE pcol    'Select colors depending on player's turn
    CASE 1
      col1 = P2COL
      col2 = P1COL
    CASE 2
      col1 = P1COL
      col2 = P2COL
  END SELECT  


  FOR z = 1 TO 0 STEP -0.1
    PAGE WRITE 1
    drawsquare 1      
    x1 = SquareX(1) + BRDSQUARE / 2 - 1
    y1 = SquareY(1) + BRDSQUARE / 2 - 1
    CIRCLE x1,y1,BRDPC,,z,col2,col1
    PAGE WRITE 0

    FOR w = 1 TO moves(m,0)
      p = moves(m,w)
      IF debug = 1 THEN
        PAGE WRITE 1
        po$ = RIGHT$(STR$(p+100),2)
        TEXT SquareX(1)+1,SquareY(1)+1,po$,LT,7,1,MAP(12),BRDCOL
        PAGE WRITE 0
      END IF
      BLIT SquareX(1),SquareY(1),SquareX(p),SquareY(p),BRDSQUARE,BRDSQUARE,1
    NEXT w
    PAUSE ANIMSPD
  NEXT z  

  FOR z = 0 TO 1 STEP 0.1
    PAGE WRITE 1
    drawsquare 1      
    x1 = SquareX(1) + BRDSQUARE / 2 - 1
    y1 = SquareY(1) + BRDSQUARE / 2 - 1
    CIRCLE x1,y1,BRDPC,,z,col1,col2
    PAGE WRITE 0

    FOR w = 1 TO moves(m,0)
      p = moves(m,w)
      IF debug = 1 THEN
        PAGE WRITE 1
        po$ = RIGHT$(STR$(p+100),2)
        TEXT SquareX(1)+1,SquareY(1)+1,po$,LT,7,1,MAP(12),BRDCOL
        PAGE WRITE 0
      END IF
      BLIT SquareX(1),SquareY(1),SquareX(p),SquareY(p),BRDSQUARE,BRDSQUARE,1
    NEXT w
    PAUSE ANIMSPD
  NEXT z  

  displayscore
END SUB




SUB clearboard            'Clear the board with animation
  LOCAL cx,cy,x,y,z,s,ss,r,p

  r = INT(RND*100)

  SELECT CASE r
    CASE 0 TO 85
      FOR z=BRDPC TO 1 STEP -1
        FOR p = 1 TO 64
          cx = SquareX(p) + BRDSQUARE / 2 - 1
          cy = SquareY(p) + BRDSQUARE / 2 - 1
          CIRCLE cx,cy,z,2,,BRDCOL
        NEXT p   
        PAUSE ANIMSPD 
      NEXT z
      
    CASE ELSE
      s = 360 / 180
      FOR z=0 TO 360-s STEP s
        ss = z
        FOR p = 1 TO 64
          ss = ss + s
          cx = SquareX(p) + BRDSQUARE / 2 - 1
          cy = SquareY(p) + BRDSQUARE / 2 - 1
          x = cx + COS(ss) * BRDPC
          y = cy + SIN(ss) * BRDPC
          LINE cx,cy,x,y,,BRDCOL
          LINE cx+1,cy,x+1,y,,BRDCOL
          LINE cx,cy+1,x,y+1,,BRDCOL
        NEXT p   
        PAUSE ANIMSPD/3 
      NEXT z
  END SELECT
END SUB




SUB playpiece(p, c) 'Draw the player piece (p) on specified square. c = player color
  LOCAL x,y,z

  x = SquareX(p) + BRDSQUARE / 2 - 1
  y = SquareY(p) + BRDSQUARE / 2 - 1

  SELECT CASE c
    CASE 1
      playtone 4000,4000,1
      FOR z=1 TO BRDPC
        CIRCLE x,y,z,,,P2COL,P1COL
        PAUSE ANIMSPD
      NEXT z
    CASE 2
      playtone 3500,3500,1
      FOR z=1 TO BRDPC
        CIRCLE x,y,z,,,P1COL,P2COL
        PAUSE ANIMSPD
      NEXT z
    CASE ELSE
      drawsquare p      
  END SELECT  
END SUB





FUNCTION getmovelist(d)   'Return the number of possible moves, and the list of them in movlist(d, moves, player)
  LOCAL x,y,mvs1,mvs2,po,pat,npc,flip1,flip2,DIR,z,ms

  mvs1  = 0           'Number of possible moves from this position for P1
  mvs2  = 0           '....P2
  ms = movsort(0,0)   'Number of moves played (search not included) and index of non played square list

  FOR z = 1 TO 61-ms
    po = movsort(ms,z)
    IF board(po) = 0 THEN   'Check only empty squares
      flip1 = 0
      flip2 = 0

      FOR DIR = 1 TO srch(po,0,0)    'For each valid direction
        pat = board(srch(po,DIR,1))
        FOR npc = 2 TO srch(po,DIR,0)   'Other squares to add to the pattern using pre-calculated search limits
            pat = pat + base3(npc,board(srch(po,DIR,npc)))
        NEXT npc 
        flip1 = flip1 + vmtbl(pat,1)    'Number of pieces flipped by player 1
        flip2 = flip2 + vmtbl(pat,2)    'Number of pieces flipped by player 2
      NEXT DIR

      IF flip1 > 0 THEN    'At least a piece flipped for P1?
        INC mvs1           'One more square to add to the list of possible moves
        movlist(d,mvs1,1) = po
        movflip(d,mvs1,1) = flip1
      END IF

      IF flip2 > 0 THEN    'At least a piece flipped for P2?
        INC mvs2           'One more square to add to the list of possible moves
        movlist(d,mvs2,2) = po
        movflip(d,mvs2,2) = flip2
      END IF

    END IF

  NEXT z
  movlist(d,0,1) = mvs1         'store # of P1 moves into index 0 of movlist
  movlist(d,0,2) = mvs2         'store # of P2 moves into index 0 of movlist
  movlist(d,0,0) = mvs1 + mvs2  'Store # of total moves (if = 0 then game is over)
  getmovelist = movlist(d,0,0) 

END FUNCTION






FUNCTION getmovelistfast(p,d)   'p=player, d=depth; Faster player p, return the number of possible moves -> movlist(depth,moves)
  LOCAL x,y,mvs,po,pat,npc,flip

  ms = movsort(0,0)             'Number of moves played (search not included) and index of non played square list

  mvs = 0                       'Number of possible moves from this position
  FOR z = 1 TO 61-ms
    po = movsort(ms,z)
    IF board(po) = 0 THEN               'Check only empty squares
      FOR DIR = 1 TO srch(po,0,0)       'For each valid direction
        pat = board(srch(po,DIR,1))
        FOR npc = 2 TO srch(po,DIR,0)   'Other squares to add to the pattern using pre-calculated search limits
            pat = pat + base3(npc,board(srch(po,DIR,npc)))
        NEXT npc 
        IF vmtbl(pat,p) > 0 THEN
          INC mvs                       'One more square to add to the list of possible moves
          movlist(d,mvs,p) = po
          'movflip(d,mvs,p) = 1
          EXIT FOR
        END IF
      NEXT DIR
    END IF
  NEXT z
  movlist(d,0,p) = mvs    'store # of moves into index 0 of movlist
  getmovelistfast = mvs

END FUNCTION





SUB prtscr          'Make a screenshot
  LOCAL f$,d$,t$

  d$ = DATE$
  t$ = TIME$
  f$ = RIGHT$(d$,4)+MID$(d$,4,2)+LEFT$(d$,2)+"-"+LEFT$(t$,2)+MID$(t$,4,2)+RIGHT$(t$,2)
  PAGE WRITE 0
  SAVE IMAGE f$
  PAGE WRITE 1
  CLS RGB(WHITE)
  PAGE DISPLAY 1
  PAUSE 5
  PAGE DISPLAY 0
  PAGE WRITE 0  
END SUB




SUB initbase3     'Precalculated base 3 values
  LOCAL b,e,t

  t = 1
  FOR e = 1 TO 7 
    base3(e,1) = t
    base3(e,2) = t*2
    t = t*3
  NEXT e
END SUB




SUB initpatterns  'Init hint patterns and base3 coding
  LOCAL b,e,t,z,p$,v

  RESTORE PatternsDiag
  FOR z = 0 TO 8
    READ p$,v
    diagval(z,1) = v
    diagval(z,2) = -v
  NEXT z 

  RESTORE PatternsSide
  FOR z = 0 TO 26
    READ p$,v
    sideval(z,1) = v
    sideval(z,2) = -v
  NEXT z 

  t = 1
  FOR e = 0 TO 2 
    base3b(e,1) = t
    base3b(e,2) = t*2
    t = t*3
  NEXT e

END SUB



Function mousepos()       'Return the mouse position relative to the board squares; 0 if out of the board borders
                          'The returned value is the square (1...64) on the playing area
  LOCAL brx1,bry1,brx2,bry2,msx,msy,msl,s,px,py

  brx1 = SquareX(1)
  bry1 = SquareY(1)
  brx2 = SquareX(64) + BRDSQUARE
  bry2 = SquareY(64) + BRDSQUARE

  msx = MOUSE(x)
  msy = MOUSE(y)
  msl = MOUSE(l)
  IF msx > brx1 AND msx < brx2 AND msy > bry1 AND msy < bry2 THEN
    px = INT((msx-brx1)/BRDSQUARE)
    py = INT((msy-bry1)/BRDSQUARE)
    s = cr2mov(px,py)
  else
    s = 0
  end if
  
  mousepos = s
end function




FUNCTION getmove(lm,c)  'Player C inputs move, last move lm used as a start point on the grid
  LOCAL x,y,px,py,co,xit,mv,k$,b,boxcol
  LOCAL lastsq,s
  
  IF lm < 1 OR lm > 64 THEN
    mv = 1
  ELSE
    mv = lm
  END IF

  xit = 0
  IF c = 1 THEN
    co = P1COL
  ELSE
    co = P2COL
  END IF  
  
  emptykeybuf
  lastsq = 0     'Empty keyboard buffer
  DO
    x = SquareX(mv)
    y = SquareY(mv)
    if ctrl$ <> "MOUSE" then BLIT READ #1,x,y,BRDSQUARE, BRDSQUARE
    if ctrl$ <> "MOUSE" then BOX x+1,y+1,BRDSQUARE-2,BRDSQUARE-2,2,co
    
    px = pos2col(mv)
    py = pos2row(mv)
    k$ = ""

    DO WHILE k$ = ""
      k$ = getinput$()
      IF ctrl$="MOUSE" AND k$ <> "enter" and k$ <> "back" THEN
        s = mousepos()
        px = pos2col(s)
        py = pos2row(s)
        k$=""
        END IF
      END IF

      
      SELECT CASE k$
        CASE "up"
          py = py - 1
          IF py = -1 THEN py = 7
        CASE "down"
          py = py + 1
          IF py = 8 THEN py = 0 
        CASE "left"
          px = px - 1
          IF px = -1 THEN px = 7
        CASE "right"
          px = px + 1
          IF px = 8 THEN px = 0
        CASE "enter"
          IF moveisvalid(cr2mov(px,py),c) = 1 THEN xit = cr2mov(px,py) 
        CASE "back"
          playtone 5000,5000,1
          if ctrl$ <> "MOUSE" then BLIT WRITE #1,x,y
          undomove
          k$ = ""
          showvalidmoves
          if ctrl$ <> "MOUSE" then BLIT READ #1,x,y,BRDSQUARE, BRDSQUARE
          if ctrl$ <> "MOUSE" then BOX x+1,y+1,BRDSQUARE-2,BRDSQUARE-2,2,co
          displayscore
          emptykeybuf
      END SELECT
    LOOP
    k$ = ""
    mv = cr2mov(px,py)
    if ctrl$ <> "MOUSE" then BLIT WRITE #1,x,y
  LOOP UNTIL xit <> 0

  getmove = mv
END FUNCTION



SUB emptykeybuf
  LOCAL b$

  DO
    b$ = INKEY$    
  LOOP UNTIL b$=""

END SUB




FUNCTION getinput$()  'Get input from keyboard or pad and virtualize it
  LOCAL k,i$,j,jx,jy,msl,msr
  
  k = ASC(INKEY$)
  SELECT CASE k
    CASE 128
      i$ = "up"
    CASE 129
      i$ = "down"
    CASE 130
      i$ = "left"
    CASE 131
      i$ = "right"
    CASE 13, 32, 10
      i$ = "enter"
    CASE 27
      i$ = "esc"
    CASE 8
      i$ = "back"
    CASE 10 , 0
      i$ = ""
    CASE 145
      i$ = "f1"
    CASE 157
      prtscr
      i$=""
    'case 156        'Reset
    '  run
    CASE ELSE
      i$ = ""
  END SELECT
  

        
  IF  i$ = "" AND ctrl$ <> "" THEN
    IF ctrl$="CLASSIC" THEN
      j =  CLASSIC(B)
      IF j = lastjoy AND j>0 THEN
        PAUSE 200
        j = 0
      END IF
      IF j AND 15360 THEN i$ = "enter"
      IF j AND 32  THEN i$ = "down"
      IF j AND 64  THEN i$ = "right"
      IF j AND 128 THEN i$ = "up"
      IF j AND 256 THEN i$ = "left"
      lastjoy = j
      'if i$ <> "" then pause 200 
    ELSE IF ctrl$ = "NUNCHUK" THEN
      j = 0
      jx = NUNCHUK(JX)
      jy = NUNCHUK(JY)
      IF jx < jl THEN i$ = "left"  : j = j OR 256
      IF jx > jr THEN i$ = "right" : j = j OR 64
      IF jy < jd THEN i$ = "down"  : j = j OR 32
      IF jy > ju THEN i$ = "up"    : j = j OR 128
      IF NUNCHUK(z) = 1 THEN i$ = "enter" : j = j OR 1
      IF NUNCHUK(c) = 1 THEN i$ = "enter" : j = j OR 2
      
      IF j = lastjoy AND j>0 THEN
        PAUSE 200
        i$=""
        j = 0
      END IF
      lastjoy = j
    ELSE IF ctrl$ = "MOUSE" THEN
      msl = MOUSE(l)
      msr = mouse(r)
      IF msr = 1 THEN i$ = "back"
      IF msl = 1 and mousepos() > 0 THEN i$ = "enter"
    END IF
  END IF
  
  IF i$ <> "" THEN
    IF i$ <> "enter" and i$ <> "back" THEN
      playtone 5000,5000,1
    END IF
  END IF
  getinput$ = i$
END FUNCTION




FUNCTION moveisvalid(m,c) 'Check if a move is valid; m = square played (1..64), C = player (1,2)
  LOCAL z,v,n


  v = 0
  n = getmovelist(0)
  IF movlist(0,0,c) > 0 THEN
    FOR z = 1 TO movlist(0,0,c)
      IF movlist(0,z,c) = m THEN v = 1
    NEXT z
  END IF
  moveisvalid = v
END FUNCTION




FUNCTION cr2mov(c,r) 'Column (0-7), Row (0-7) to grid (1-64) conversion
  cr2mov = r*8 + c + 1
END FUNCTION




FUNCTION pos2col(p)      'Grid (1-64) to column position (0-7)
  pos2col = (p-1) MOD 8
END FUNCTION





FUNCTION pos2row(p)       'Grid (1-64) to row position (0-7)
  pos2row = INT((p-1) / 8)
END FUNCTION




SUB initgame          'Read starting position and set board values
  LOCAL b$,r$,y$,z,c

  pcnt(1) = 0
  pcnt(2) = 0
  b$=""
  FOR z = 1 TO 8
    READ r$
    b$ = b$ + r$
  NEXT
  FOR z = 1 TO 64
    y$ = MID$(b$,z,1)
    SELECT CASE y$
      CASE "O" 'White
        c = 1

      CASE "X" 'Black
        c = 2
      CASE ELSE
        c = 0  'Empty
    END SELECT
    board(z) = c
    INC pcnt(c)
  NEXT z  
  moves(0,0)=0

  
  displaypos 0 'Draw board level 0 (main)
  displayscore
END SUB





SUB displaypos(l) 'Draw board at level l
  LOCAL z

  IF debug = 1 THEN
    redrawboard
  ELSE
    FOR z = 1 TO 64
      playpiece z , board(z)
    NEXT z
  END IF

END SUB




SUB waitforkey

  DO WHILE INKEY$<>"":LOOP
  DO WHILE INKEY$="":LOOP
END SUB




SUB redrawboard
  LOCAL z

  FOR z = 1 TO 64
    drawpiece z,board(z)
  NEXT z
END SUB





SUB drawpiece(p, c) 'Draw the player piece (p) on specified square. c = player color
  LOCAL x,y,r,col1,col2

  x = SquareX(p) + BRDSQUARE / 2 - 1
  y = SquareY(p) + BRDSQUARE / 2 - 1

  SELECT CASE c
    CASE 1
      CIRCLE x,y,BRDPC,,,P2COL,P1COL  
    CASE 2
      CIRCLE x,y,BRDPC,,,P1COL,P2COL  
    CASE ELSE
      drawsquare p      
  END SELECT  
END SUB





SUB drawsquare(p)  'Draw an empty square

    LOCAL po$,x,y

    x = SquareX(p)
    y = SquareY(p)
    BOX x, y, BRDSQUARE, BRDSQUARE, 1, RGB(BLACK), BRDCOL
    IF debug = 1 THEN
      po$ = RIGHT$(STR$(p+100),2)
      TEXT x+1,y+1,po$,LT,7,1,MAP(12),BRDCOL
    END IF
END SUB



SUB changepalette
  LOCAL z

  FOR z = 1 TO 14
    MAP(239+z) = RGB(0,z*16,0)
  NEXT z
  MAP SET
END SUB



SUB showpalette 'Show current palette and wait for a key

  LOCAL x,y,c


  CLS
  FOR x = 0 TO 15
    FOR y = 0 TO 15
      c=x*16+y
      BOX x*30,y*30,30,30,1,MAP(c),MAP(c)
      TEXT x*30+15,y*30+15,STR$(c),"CM",7,,MAP(c XOR 255),MAP(c)
    NEXT y
  NEXT x
  waitforkey
END SUB



SUB initboard  'Draw the playing board with frame and square coordinates
  LOCAL x,y,p,w,t$,brdx,brdy,brdx2,brdy2,wsp

  CLS BKCOL

  x = SquareX(5)
  y = SquareY(1) - 20
  TEXT x,y,PROGNAME$,"CB",1,2,RGB(WHITE),BKCOL      'Title
  
  x = SquareX(1)
  brdx = x-20
  y = SquareY(1)
  brdy = y-20
  w = BRDSQUARE * 8 + 32
  brdx2 = brdx+w
  brdy2 = brdy+w
  BOX brdx,brdy,w,w,1,RGB(BLACK), BRDFRM         'Frame
  LINE x,y,x-20,y-20,,BLACK
  LINE x,y+w-41,x-20,y+w-21,,BLACK
  LINE x+w-41,y,x+w-21,y-20,,BLACK
  LINE x+w-41,y+w-41,x+w-21,y+w-21,,BLACK

  FOR p = 1 TO 8                                    'Squares
    x = SquareX(p) + BRDSQUARE / 2 - 4
    y = SquareY(1) - 3
    TEXT x,y,CHR$(64+p),"LB",,,BRDCOR,BRDFRM
    y = y + BRDSQUARE * 8 + 11
    TEXT x,y,CHR$(64+p),"LB",,,BRDCOR,BRDFRM

    x = SquareX(1) - 9
    y = SquareY(p*8) + BRDSQUARE / 2
    TEXT x,y,CHR$(48+p),"CM",,,BRDCOR,BRDFRM
    x = x + BRDSQUARE * 8 + 11
    TEXT x,y,CHR$(48+p),"CM",,,BRDCOR,BRDFRM
  NEXT p

  FOR p = 1 TO 64
    drawsquare p
    board(p) = 0
  NEXT p        

  wsp = 8  
  window brdx2+BRDSQUARE*2.5,brdy,MM.HRES-BRDSQUARE*2.5,BRDSQUARE*2.4, "SCORE",1
  window brdx2+BRDSQUARE,winy2(1)+wsp,MM.HRES-BRDSQUARE,winy2(1)+BRDSQUARE+wsp, "BLACK PLAYER",2
  window winx1(2),winy2(2)+wsp,winx2(2),winy2(2)+BRDSQUARE+wsp, "WHITE PLAYER",3
  window winx1(3),winy2(3)+wsp,winx2(3),winy2(3)+BRDSQUARE+wsp, "OPENING BOOK",5
  window winx1(5),winy2(5)+wsp,winx2(5),brdy+w, "STATUS / INFO",4
END SUB



SUB window(x1,y1,x2,y2,t$,i)  'Draw output windows; t$ = title, i = index to store inside coordinates in winx1()...Winy2() array
  LOCAL lw,bx,by,bw,bh

  lw = 1
  RBOX x1,y1,x2-x1-lw,y2-y1-lw,10,WINDOUT,WINDBAK

  bx = x1+lw
  by = y1+lw
  bw = x2-x1-2*lw-1
  bh = 20
  winx1(i) = x1
  winy1(i) = by+bh
  winx2(i) = x2
  winy2(i) = y2
  RBOX bx,by,bw,bh,10,WINDBAR,WINDBAR
  TEXT bx+bw/2,by+bh/2+1,t$,"CM",4,1,WINDTXT,WINDBAR 

END SUB



'Precalculate squares to be included in relation to the current position (s=1..64)
'For each square (position) srch(x,,),
'srch(x,0,0)    = number of directions to look
'srch(x,1,0)    = number of squares to look in direction 1
'srch(x,1,1..y) = enumeration of the squares
'srch(x,2,0)    = number of squares to look in direction 2
'and so on...

'srch(x,0,1)    = total number of related squares

SUB initsearcharray 'Init of search limits array
  LOCAL x,y,s,d,z,p,l
  
  FOR x = 0 TO 7
    FOR y = 0 TO 7
      s = y*8+x+1
      d = 0
  
      IF y > 1 THEN             '-8
        l = y
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s-8*z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF MIN(7-x,y) > 1 THEN    '-7
        l = MIN(7-x,y)
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s-7*z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF x < 6 THEN             '+1
        l = 7-x
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s+z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF MIN(7-x,7-y) > 1 THEN  '+9
        l = MIN(7-x,7-y)
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s+9*z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF y < 6 THEN             '+8
        l = 7-y
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s+8*z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF MIN(x,7-y) > 1 THEN    '+7
        l = MIN(x,7-y)
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s+7*z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF x > 1 THEN             '-1
        l = x
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s-z
          INC srch(s,0,1)
        NEXT z
      END IF

      IF MIN(x,y) > 1 THEN      '-9
        l = MIN(x,y)
        INC d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        FOR z = 1 TO l
          srch(s,d,z) = s-9*z
          INC srch(s,0,1)
        NEXT z
      END IF
    
    NEXT y
  NEXT x
END SUB




SUB initmovetables     'Init of valid moves tables
  LOCAL z,a$            '0,1,2 in base three system with 0=empty, 1=white, 2=black
                        'Returns the number of flipped pieces
  FOR z = 0 TO 2186
    a$=BASE$(3,z,7)

    'White valid position patterns
    IF RIGHT$(a$,2) = "12" THEN vmtbl(z,1) = 1
    IF RIGHT$(a$,3) = "122" THEN vmtbl(z,1) = 2
    IF RIGHT$(a$,4) = "1222" THEN vmtbl(z,1) = 3
    IF RIGHT$(a$,5) = "12222" THEN vmtbl(z,1) = 4
    IF RIGHT$(a$,6) = "122222" THEN vmtbl(z,1) = 5
    IF RIGHT$(a$,7) = "1222222" THEN vmtbl(z,1) = 6

    'Black valid position patterns
    IF RIGHT$(a$,2) = "21" THEN vmtbl(z,2) = 1
    IF RIGHT$(a$,3) = "211" THEN vmtbl(z,2) = 2
    IF RIGHT$(a$,4) = "2111" THEN vmtbl(z,2) = 3
    IF RIGHT$(a$,5) = "21111" THEN vmtbl(z,2) = 4
    IF RIGHT$(a$,6) = "211111" THEN vmtbl(z,2) = 5
    IF RIGHT$(a$,7) = "2111111" THEN vmtbl(z,2) = 6
  NEXT
END SUB



SUB cvtopenings
  LOCAL x,y,z,o$,n$
  
  OPEN "Openings.txt" FOR INPUT AS #1
  OPEN "book" FOR OUTPUT AS #2
  DO WHILE NOT EOF(#1)
    LINE INPUT #1,o$
    LINE INPUT #1,n$
    PRINT #2, "data ";CHR$(34);UCASE$(o$);CHR$(34);", ";CHR$(34);n$;CHR$(34)
  LOOP
  CLOSE #1
  CLOSE #2
END SUB





SUB pon
  PAGE WRITE 1
  FONT 7
  CLS
END SUB


SUB poff
  pup
  PAGE WRITE 0
  FONT 1
END SUB



SUB pup
  LOCAL dx,dy,w,h

  dx = SquareX(8) + 2 * BRDSQUARE
  dy = SquareY(8)
  w = BRDSQUARE * 7
  h = BRDSQUARE * 5
  
  PAGE WRITE 0
  BLIT 0,0,dx,dy,w,h,1 
  PAGE WRITE 1
END SUB




FUNCTION coord2dec(c$)

  coord2dec = ASC(LEFT$(c$,1))-64 + (VAL(RIGHT$(c$,1))-1)*8 
END FUNCTION





FUNCTION dec2coord$(p)
  LOCAL c  
  
  c = p - 1
  dec2coord$ = CHR$(65 + c MOD 8) + CHR$(49 + INT(c/8)) 
END FUNCTION




FUNCTION SquareX(s)
  
  SquareX = BRDX + (s -1) MOD 8 * (BRDSQUARE - 1)
END FUNCTION


FUNCTION SquareY(s)

  SquareY = BRDY + INT((s - 1)/8) * (BRDSQUARE -1)
END FUNCTION


SUB makepatterns
  LOCAL z

  OPEN "patterns.bas" FOR OUTPUT AS #2
  FOR z = 0 TO 8
    PRINT #2, "data ";CHR$(34);BASE$(3,z,2);CHR$(34);" ,000"
  NEXT z
  FOR z = 0 TO 26
    PRINT #2, "data ";CHR$(34);BASE$(3,z,3);CHR$(34);",000"
  NEXT z
  CLOSE #2

END SUB





'Normal start position
StartPosition:
DATA "........"
DATA "........"
DATA "........"
DATA "...OX..."
DATA "...XO..."
DATA "........"
DATA "........"
DATA "........"


TestPosition:
DATA "........"
DATA ".XXOOX.."
DATA "...O...."
DATA "..XOXX.."
DATA "..OXO..."
DATA ".OXOX..."
DATA "........"
DATA "........"


EndGame10:
DATA "X..XXOXX"
DATA "XXOOOOXX"
DATA "X.OOXXXX"
DATA "XOOXXOXX"
DATA ".XXXOX.X"
DATA ".XXOOXXX"
DATA "XO.X.XXX"
DATA "XO..XXXX"

EndGame8:
DATA "XX.XXOXX"
DATA "XXXOOOXX"
DATA "X.OXXXXX"
DATA "XOOXXOXX"
DATA ".XXXOOOX"
DATA ".XXOOXXX"
DATA "XO.X.XXX"
DATA "XO..XXXX"

EndGame6:
DATA "XX.XXOXX"
DATA "XXXXOOXX"
DATA "X.XXXXXX"
DATA "XXOXXOXX"
DATA "XXXXOOOX"
DATA ".XXOOXXX"
DATA "XO.O.XXX"
DATA "XO.OXXXX"





'Squares values for each position at the start of the game (very approximative; debutant level computer play)
PositionHint:
DATA "90233209"
DATA "00444400"
DATA "24555542"
DATA "34500543"
DATA "34500543"
DATA "24555542"
DATA "00444400"
DATA "90233209"




'Various openings to make games more entertainings when playing strong computer player 
OpeningBook:
DATA "C4C3", "Diagonal Opening"
DATA "C4C3D3C5B3", "Snake, Peasant"
DATA "C4C3D3C5B3F4B5B4C6D6F5", "Pyramid, Checkerboarding Peasant"
DATA "C4C3D3C5B4", "Heath, Tobidashi 'Jumping Out'"
DATA "C4C3D3C5B4D2C2F4D6C6F5E6F7", "Mimura variation II"
DATA "C4C3D3C5B4D2D6", "Heath-Bat"
DATA "C4C3D3C5B4D2E2", "Iwasaki variation"
DATA "C4C3D3C5B4E3", "Heath-Chimney, 'Mass-Turning'"
DATA "C4C3D3C5B5", "Raccoon Dog"
DATA "C4C3D3C5B6C6B5", "Hamilton"
DATA "C4C3D3C5B6E3", "Lollipop"
DATA "C4C3D3C5D6", "Cow"
DATA "C4C3D3C5D6E3", "Chimney"
DATA "C4C3D3C5D6F4B4", "Cow Bat, Bat, Cambridge"
DATA "C4C3D3C5D6F4B4B6B5C6B3", "Bat (Piau Continuation 2)"
DATA "C4C3D3C5D6F4B4B6B5C6F5", "Melnikov, Bat (Piau Continuation 1)"
DATA "C4C3D3C5D6F4B4C6B5B3B6E3C2A4A5A6D2", "Bat (Kling Continuation)"
DATA "C4C3D3C5D6F4B4E3B3", "Bat (Kling Alternative)"
DATA "C4C3D3C5D6F4F5", "Rose-v-Toth"
DATA "C4C3D3C5D6F4F5D2", "Tanida"
DATA "C4C3D3C5D6F4F5D2B5", "Aircraft, Feldborg"
DATA "C4C3D3C5D6F4F5D2G4D7", "Sailboat"
DATA "C4C3D3C5D6F4F5E6C6D7", "Maruoka"
DATA "C4C3D3C5D6F4F5E6F6", "Landau"
DATA "C4C3D3C5F6", "Buffalo, Kenichi Variation"
DATA "C4C3D3C5F6E2C6", "Maruoka Buffalo"
DATA "C4C3D3C5F6E3C6F5F4G5", "Tanida Buffalo"
DATA "C4C3D3C5F6F5", "Hokuriku Buffalo"
DATA "C4C3D3E3C2", "Snake, Peasant"
DATA "C4C3D3E3C2D6E2D2F3F4E6", "Pyramid, Checkerboarding Peasant"
DATA "C4C3D3E3D2", "Heath, Tobidashi 'Jumping Out'"
DATA "C4C3D3E3D2B4B3D6F4F3E6F5G6", "Mimura variation II"
DATA "C4C3D3E3D2B4B5", "Iwasaki variation"
DATA "C4C3D3E3D2B4F4", "Heath-Bat"
DATA "C4C3D3E3D2C5", "Heath-Chimney, 'Mass-Turning'"
DATA "C4C3D3E3E2", "Raccoon Dog"
DATA "C4C3D3E3F2C5", "Lollipop"
DATA "C4C3D3E3F2F3E2", "Hamilton"
DATA "C4C3D3E3F4", "Cow"
DATA "C4C3D3E3F4C5", "Chimney"
DATA "C4C3D3E3F4D6D2", "Cow Bat, Bat, Cambridge"
DATA "C4C3D3E3F4D6D2C5C2", "Bat (Kling Alternative)"
DATA "C4C3D3E3F4D6D2F2E2F3C2", "Bat (Piau Continuation 2)"
DATA "C4C3D3E3F4D6D2F2E2F3E6", "Melnikov, Bat (Piau Continuation 1)"
DATA "C4C3D3E3F4D6D2F3E2C2F2C5B3D1E1F1B4", "Bat (Kling Continuation)"
DATA "C4C3D3E3F4D6E6", "Rose-v-Toth"
DATA "C4C3D3E3F4D6E6B4", "Tanida"
DATA "C4C3D3E3F4D6E6B4D7G4", "Sailboat"
DATA "C4C3D3E3F4D6E6B4E2", "Aircraft, Feldborg"
DATA "C4C3D3E3F4D6E6F5F3G4", "Maruoka"
DATA "C4C3D3E3F4D6E6F5F6", "Landau"
DATA "C4C3D3E3F6", "Buffalo, Kenichi Variation"
DATA "C4C3D3E3F6B5F3", "Maruoka Buffalo"
DATA "C4C3D3E3F6C5F3E6D6E7", "Tanida Buffalo"
DATA "C4C3D3E3F6E6", "Hokuriku Buffalo"
DATA "C4C3E6C5", "Wing Variation"
DATA "C4C3F5C5", "Semi-Wing Variation"
DATA "C4C5", "Parallel Opening"
DATA "C4E3", "Perpendicular Opening"
DATA "C4E3F4C5D6E6", "Mimura"
DATA "C4E3F4C5D6F3C6", "Shaman, Danish"
DATA "C4E3F4C5D6F3D3", "Inoue"
DATA "C4E3F4C5D6F3D3C3", "IAGO"
DATA "C4E3F4C5D6F3E2", "Bhagat"
DATA "C4E3F4C5D6F3E6C3D3E2", "Rose"
DATA "C4E3F4C5D6F3E6C3D3E2B5", "Flat"
DATA "C4E3F4C5D6F3E6C3D3E2B5F5", "Rotating Flat"
DATA "C4E3F4C5D6F3E6C3D3E2B5F5B3", "Murakami Variation"
DATA "C4E3F4C5D6F3E6C3D3E2B5F5B4F6C2E7D2C7", "Rotating Flat (Kling Continuation)"
DATA "C4E3F4C5D6F3E6C3D3E2B6F5", "Rose-Birth"
DATA "C4E3F4C5D6F3E6C3D3E2B6F5B4F6G5D7", "Brightstein"
DATA "C4E3F4C5D6F3E6C3D3E2B6F5G5", "Rose-birdie, Rose-Tamenori"
DATA "C4E3F4C5D6F3E6C3D3E2B6F5G5F6", "Rose-Tamenori-Kling"
DATA "C4E3F4C5D6F3E6C3D3E2D2", "Greenberg, Dawg"
DATA "C4E3F4C5D6F3E6C6", "Ralle"
DATA "C4E3F4C5E6", "Horse"
DATA "C4E3F5B4", "No-Cat"
DATA "C4E3F5B4F3", "Swallow"
DATA "C4E3F5B4F3F4E2E6G5F6D6C6", "No-Cat (Continuation)"
DATA "C4E3F5E6D3", "Italian"
DATA "C4E3F5E6F4", "Cat"
DATA "C4E3F5E6F4C5D6C6F7F3", "Sakaguchi"
DATA "C4E3F5E6F4C5D6C6F7G5G6", "Berner"
DATA "C4E3F6B4", "Ganglion"
DATA "C4E3F6E6F5", "Tiger"
DATA "C4E3F6E6F5C5C3", "Stephenson"
DATA "C4E3F6E6F5C5C3B4", "No-Kung"
DATA "C4E3F6E6F5C5C3B4D6C6B5A6B6C7", "No-Kung (Continuation)"
DATA "C4E3F6E6F5C5C3C6", "COMP'OTH"
DATA "C4E3F6E6F5C5C3G5", "Kung"
DATA "C4E3F6E6F5C5D3", "Leader's Tiger"
DATA "C4E3F6E6F5C5D6", "Brightwell"
DATA "C4E3F6E6F5C5F4G5G4F3C6D3D6", "Ishii"
DATA "C4E3F6E6F5C5F4G5G4F3C6D3D6B3C3B4E2B6", "Mainline Tiger"
DATA "C4E3F6E6F5C5F4G6F7", "Rose-BILL"
DATA "C4E3F6E6F5C5F4G6F7D3", "Tamenori"
DATA "C4E3F6E6F5G6", "Aubrey, Tanaka"
DATA "D3C3", "Diagonal Opening"
DATA "D3C3C4C5B3", "Snake, Peasant"
DATA "D3C3C4C5B3F4B5B4C6D6F5", "Pyramid, Checkerboarding Peasant"
DATA "D3C3C4C5B4", "Heath, Tobidashi 'Jumping Out'"
DATA "D3C3C4C5B4D2C2F4D6C6F5E6F7", "Mimura variation II"
DATA "D3C3C4C5B4D2D6", "Heath-Bat"
DATA "D3C3C4C5B4D2E2", "Iwasaki variation"
DATA "D3C3C4C5B4E3", "Heath-Chimney, 'Mass-Turning'"
DATA "D3C3C4C5B5", "Raccoon Dog"
DATA "D3C3C4C5B6C6B5", "Hamilton"
DATA "D3C3C4C5B6E3", "Lollipop"
DATA "D3C3C4C5D6", "Cow"
DATA "D3C3C4C5D6E3", "Chimney"
DATA "D3C3C4C5D6F4B4", "Cow Bat, Bat, Cambridge"
DATA "D3C3C4C5D6F4B4B6B5C6B3", "Bat (Piau Continuation 2)"
DATA "D3C3C4C5D6F4B4B6B5C6F5", "Melnikov, Bat (Piau Continuation 1)"
DATA "D3C3C4C5D6F4B4C6B5B3B6E3C2A4A5A6D2", "Bat (Kling Continuation)"
DATA "D3C3C4C5D6F4B4E3B3", "Bat (Kling Alternative)"
DATA "D3C3C4C5D6F4F5", "Rose-v-Toth"
DATA "D3C3C4C5D6F4F5D2", "Tanida"
DATA "D3C3C4C5D6F4F5D2B5", "Aircraft, Feldborg"
DATA "D3C3C4C5D6F4F5D2G4D7", "Sailboat"
DATA "D3C3C4C5D6F4F5E6C6D7", "Maruoka"
DATA "D3C3C4C5D6F4F5E6F6", "Landau"
DATA "D3C3C4C5F6", "Buffalo, Kenichi Variation"
DATA "D3C3C4C5F6E2C6", "Maruoka Buffalo"
DATA "D3C3C4C5F6E3C6F5F4G5", "Tanida Buffalo"
DATA "D3C3C4C5F6F5", "Hokuriku Buffalo"
DATA "D3C3C4E3C2", "Snake, Peasant"
DATA "D3C3C4E3C2D6E2D2F3F4E6", "Pyramid, Checkerboarding Peasant"
DATA "D3C3C4E3D2", "Heath, Tobidashi 'Jumping Out'"
DATA "D3C3C4E3D2B4B3D6F4F3E6F5G6", "Mimura variation II"
DATA "D3C3C4E3D2B4B5", "Iwasaki variation"
DATA "D3C3C4E3D2B4F4", "Heath-Bat"
DATA "D3C3C4E3D2C5", "Heath-Chimney, 'Mass-Turning'"
DATA "D3C3C4E3E2", "Raccoon Dog"
DATA "D3C3C4E3F2C5", "Lollipop"
DATA "D3C3C4E3F2F3E2", "Hamilton"
DATA "D3C3C4E3F4", "Cow"
DATA "D3C3C4E3F4C5", "Chimney"
DATA "D3C3C4E3F4D6D2", "Cow Bat, Bat, Cambridge"
DATA "D3C3C4E3F4D6D2C5C2", "Bat (Kling Alternative)"
DATA "D3C3C4E3F4D6D2F2E2F3C2", "Bat (Piau Continuation 2)"
DATA "D3C3C4E3F4D6D2F2E2F3E6", "Melnikov, Bat (Piau Continuation 1)"
DATA "D3C3C4E3F4D6D2F3E2C2F2C5B3D1E1F1B4", "Bat (Kling Continuation)"
DATA "D3C3C4E3F4D6E6", "Rose-v-Toth"
DATA "D3C3C4E3F4D6E6B4", "Tanida"
DATA "D3C3C4E3F4D6E6B4D7G4", "Sailboat"
DATA "D3C3C4E3F4D6E6B4E2", "Aircraft, Feldborg"
DATA "D3C3C4E3F4D6E6F5F3G4", "Maruoka"
DATA "D3C3C4E3F4D6E6F5F6", "Landau"
DATA "D3C3C4E3F6", "Buffalo, Kenichi Variation"
DATA "D3C3C4E3F6B5F3", "Maruoka Buffalo"
DATA "D3C3C4E3F6C5F3E6D6E7", "Tanida Buffalo"
DATA "D3C3C4E3F6E6", "Hokuriku Buffalo"
DATA "D3C3E6E3", "Semi-Wing Variation"
DATA "D3C3F5E3", "Wing Variation"
DATA "D3C5", "Perpendicular Opening"
DATA "D3C5D6E3F4C6B5", "Bhagat"
DATA "D3C5D6E3F4C6C4", "Inoue"
DATA "D3C5D6E3F4C6C4C3", "IAGO"
DATA "D3C5D6E3F4C6F3", "Shaman, Danish"
DATA "D3C5D6E3F4C6F5C3C4B5", "Rose"
DATA "D3C5D6E3F4C6F5C3C4B5B4", "Greenberg, Dawg"
DATA "D3C5D6E3F4C6F5C3C4B5E2", "Flat"
DATA "D3C5D6E3F4C6F5C3C4B5E2E6", "Rotating Flat"
DATA "D3C5D6E3F4C6F5C3C4B5E2E6C2", "Murakami Variation"
DATA "D3C5D6E3F4C6F5C3C4B5E2E6D2F6B3G5B4G3", "Rotating Flat (Kling Continuation)"
DATA "D3C5D6E3F4C6F5C3C4B5F2E6", "Rose-Birth"
DATA "D3C5D6E3F4C6F5C3C4B5F2E6D2F6E7G4", "Brightstein"
DATA "D3C5D6E3F4C6F5C3C4B5F2E6E7", "Rose-birdie, Rose-Tamenori"
DATA "D3C5D6E3F4C6F5C3C4B5F2E6E7F6", "Rose-Tamenori-Kling"
DATA "D3C5D6E3F4C6F5F3", "Ralle"
DATA "D3C5D6E3F4F5", "Mimura"
DATA "D3C5D6E3F5", "Horse"
DATA "D3C5E6D2", "No-Cat"
DATA "D3C5E6D2C6", "Swallow"
DATA "D3C5E6D2C6D6B5F5E7F6F4F3", "No-Cat (Continuation)"
DATA "D3C5E6F5C4", "Italian"
DATA "D3C5E6F5D6", "Cat"
DATA "D3C5E6F5D6E3F4F3G6C6", "Sakaguchi"
DATA "D3C5E6F5D6E3F4F3G6E7F7", "Berner"
DATA "D3C5F6D2", "Ganglion"
DATA "D3C5F6F5E6", "Tiger"
DATA "D3C5F6F5E6E3C3", "Stephenson"
DATA "D3C5F6F5E6E3C3D2", "No-Kung"
DATA "D3C5F6F5E6E3C3D2F4F3E2F1F2G3", "No-Kung (Continuation)"
DATA "D3C5F6F5E6E3C3E7", "Kung"
DATA "D3C5F6F5E6E3C3F3", "COMP'OTH"
DATA "D3C5F6F5E6E3C4", "Leader's Tiger"
DATA "D3C5F6F5E6E3D6E7D7C6F3C4F4", "Ishii"
DATA "D3C5F6F5E6E3D6E7D7C6F3C4F4C2C3D2B5F2", "Mainline Tiger"
DATA "D3C5F6F5E6E3D6F7G6", "Rose-BILL"
DATA "D3C5F6F5E6E3D6F7G6C4", "Tamenori"
DATA "D3C5F6F5E6E3F4", "Brightwell"
DATA "D3C5F6F5E6F7", "Aubrey, Tanaka"
DATA "D3E3", "Parallel Opening"
DATA "E6D6", "Parallel Opening"
DATA "E6F4", "Perpendicular Opening"
DATA "E6F4C3C4D3", "Tiger"
DATA "E6F4C3C4D3C2", "Aubrey, Tanaka"
DATA "E6F4C3C4D3D6C5", "Brightwell"
DATA "E6F4C3C4D3D6E3C2B3", "Rose-BILL"
DATA "E6F4C3C4D3D6E3C2B3F5", "Tamenori"
DATA "E6F4C3C4D3D6E3D2E2F3C6F5C5", "Ishii"
DATA "E6F4C3C4D3D6E3D2E2F3C6F5C5F7F6E7G4C7", "Mainline Tiger"
DATA "E6F4C3C4D3D6F5", "Leader's Tiger"
DATA "E6F4C3C4D3D6F6", "Stephenson"
DATA "E6F4C3C4D3D6F6C6", "COMP'OTH"
DATA "E6F4C3C4D3D6F6D2", "Kung"
DATA "E6F4C3C4D3D6F6E7", "No-Kung"
DATA "E6F4C3C4D3D6F6E7C5C6D7C8C7B6", "No-Kung (Continuation)"
DATA "E6F4C3E7", "Ganglion"
DATA "E6F4D3C4E3", "Cat"
DATA "E6F4D3C4E3D6C5C6B3D2C2", "Berner"
DATA "E6F4D3C4E3D6C5C6B3F3", "Sakaguchi"
DATA "E6F4D3C4F5", "Italian"
DATA "E6F4D3E7", "No-Cat"
DATA "E6F4D3E7F3", "Swallow"
DATA "E6F4D3E7F3E3G4C4D2C3C5C6", "No-Cat (Continuation)"
DATA "E6F4E3D6C4", "Horse"
DATA "E6F4E3D6C5C4", "Mimura"
DATA "E6F4E3D6C5F3C4C6", "Ralle"
DATA "E6F4E3D6C5F3C4F6F5G4", "Rose"
DATA "E6F4E3D6C5F3C4F6F5G4C7D3", "Rose-Birth"
DATA "E6F4E3D6C5F3C4F6F5G4C7D3D2", "Rose-birdie, Rose-Tamenori"
DATA "E6F4E3D6C5F3C4F6F5G4C7D3D2C3", "Rose-Tamenori-Kling"
DATA "E6F4E3D6C5F3C4F6F5G4C7D3E7C3D2B5", "Brightstein"
DATA "E6F4E3D6C5F3C4F6F5G4D7", "Flat"
DATA "E6F4E3D6C5F3C4F6F5G4D7D3", "Rotating Flat"
DATA "E6F4E3D6C5F3C4F6F5G4D7D3E7C3G6B4G5B6", "Rotating Flat (Kling Continuation)"
DATA "E6F4E3D6C5F3C4F6F5G4D7D3F7", "Murakami Variation"
DATA "E6F4E3D6C5F3C4F6F5G4G5", "Greenberg, Dawg"
DATA "E6F4E3D6C5F3C6", "Shaman, Danish"
DATA "E6F4E3D6C5F3F5", "Inoue"
DATA "E6F4E3D6C5F3F5F6", "IAGO"
DATA "E6F4E3D6C5F3G4", "Bhagat"
DATA "E6F6", "Diagonal Opening"
DATA "E6F6C4D6", "Wing Variation"
DATA "E6F6D3D6", "Semi-Wing Variation"
DATA "E6F6F5D6C3", "Buffalo, Kenichi Variation"
DATA "E6F6F5D6C3D3", "Hokuriku Buffalo"
DATA "E6F6F5D6C3F4C6D3E3D2", "Tanida Buffalo"
DATA "E6F6F5D6C3G4C6", "Maruoka Buffalo"
DATA "E6F6F5D6C5", "Cow"
DATA "E6F6F5D6C5E3D3", "Rose-v-Toth"
DATA "E6F6F5D6C5E3D3C4C3", "Landau"
DATA "E6F6F5D6C5E3D3C4C6B5", "Maruoka"
DATA "E6F6F5D6C5E3D3G5", "Tanida"
DATA "E6F6F5D6C5E3D3G5D7", "Aircraft, Feldborg"
DATA "E6F6F5D6C5E3D3G5E2B5", "Sailboat"
DATA "E6F6F5D6C5E3E7", "Cow Bat, Bat, Cambridge"
DATA "E6F6F5D6C5E3E7C6D7F7C7F4G6E8D8C8G5", "Bat (Kling Continuation)"
DATA "E6F6F5D6C5E3E7C7D7C6D3", "Melnikov, Bat (Piau Continuation 1)"
DATA "E6F6F5D6C5E3E7C7D7C6F7", "Bat (Piau Continuation 2)"
DATA "E6F6F5D6C5E3E7F4F7", "Bat (Kling Alternative)"
DATA "E6F6F5D6C5F4", "Chimney"
DATA "E6F6F5D6C7C6D7", "Hamilton"
DATA "E6F6F5D6C7F4", "Lollipop"
DATA "E6F6F5D6D7", "Raccoon Dog"
DATA "E6F6F5D6E7", "Heath, Tobidashi 'Jumping Out'"
DATA "E6F6F5D6E7F4", "Heath-Chimney, 'Mass-Turning'"
DATA "E6F6F5D6E7G5C5", "Heath-Bat"
DATA "E6F6F5D6E7G5G4", "Iwasaki variation"
DATA "E6F6F5D6E7G5G6E3C5C6D3C4B3", "Mimura variation II"
DATA "E6F6F5D6F7", "Snake, Peasant"
DATA "E6F6F5D6F7E3D7E7C6C5D3", "Pyramid, Checkerboarding Peasant"
DATA "E6F6F5F4C3", "Buffalo, Kenichi Variation"
DATA "E6F6F5F4C3C4", "Hokuriku Buffalo"
DATA "E6F6F5F4C3D6F3C4C5B4", "Tanida Buffalo"
DATA "E6F6F5F4C3D7F3", "Maruoka Buffalo"
DATA "E6F6F5F4E3", "Cow"
DATA "E6F6F5F4E3C5C4", "Rose-v-Toth"
DATA "E6F6F5F4E3C5C4D3C3", "Landau"
DATA "E6F6F5F4E3C5C4D3F3E2", "Maruoka"
DATA "E6F6F5F4E3C5C4E7", "Tanida"
DATA "E6F6F5F4E3C5C4E7B5E2", "Sailboat"
DATA "E6F6F5F4E3C5C4E7G4", "Aircraft, Feldborg"
DATA "E6F6F5F4E3C5G5", "Cow Bat, Bat, Cambridge"
DATA "E6F6F5F4E3C5G5D6G6", "Bat (Kling Alternative)"
DATA "E6F6F5F4E3C5G5F3G4G6G3D6F7H5H4H3E7", "Bat (Kling Continuation)"
DATA "E6F6F5F4E3C5G5G3G4F3C4", "Melnikov, Bat (Piau Continuation 1)"
DATA "E6F6F5F4E3C5G5G3G4F3G6", "Bat (Piau Continuation 2)"
DATA "E6F6F5F4E3D6", "Chimney"
DATA "E6F6F5F4G3D6", "Lollipop"
DATA "E6F6F5F4G3F3G4", "Hamilton"
DATA "E6F6F5F4G4", "Raccoon Dog"
DATA "E6F6F5F4G5", "Heath, Tobidashi 'Jumping Out'"
DATA "E6F6F5F4G5D6", "Heath-Chimney, 'Mass-Turning'"
DATA "E6F6F5F4G5E7D7", "Iwasaki variation"
DATA "E6F6F5F4G5E7E3", "Heath-Bat"
DATA "E6F6F5F4G5E7F7C5E3F3C4D3C2", "Mimura variation II"
DATA "E6F6F5F4G6", "Snake, Peasant"
DATA "E6F6F5F4G6C5G4G5F3E3C4", "Pyramid, Checkerboarding Peasant"
DATA "F5D6", "Perpendicular Opening"
DATA "F5D6C3D3C4", "Tiger"
DATA "F5D6C3D3C4B3", "Aubrey, Tanaka"
DATA "F5D6C3D3C4F4C5B3C2", "Rose-BILL"
DATA "F5D6C3D3C4F4C5B3C2E6", "Tamenori"
DATA "F5D6C3D3C4F4C5B4B5C6F3E6E3", "Ishii"
DATA "F5D6C3D3C4F4C5B4B5C6F3E6E3G6F6G5D7G3", "Mainline Tiger"
DATA "F5D6C3D3C4F4E3", "Brightwell"
DATA "F5D6C3D3C4F4E6", "Leader's Tiger"
DATA "F5D6C3D3C4F4F6", "Stephenson"
DATA "F5D6C3D3C4F4F6B4", "Kung"
DATA "F5D6C3D3C4F4F6F3", "COMP'OTH"
DATA "F5D6C3D3C4F4F6G5", "No-Kung"
DATA "F5D6C3D3C4F4F6G5E3F3G4H3G3F2", "No-Kung (Continuation)"
DATA "F5D6C3G5", "Ganglion"
DATA "F5D6C4D3C5", "Cat"
DATA "F5D6C4D3C5F4E3F3C2B4B3", "Berner"
DATA "F5D6C4D3C5F4E3F3C2C6", "Sakaguchi"
DATA "F5D6C4D3E6", "Italian"
DATA "F5D6C4G5", "No-Cat"
DATA "F5D6C4G5C6", "Swallow"
DATA "F5D6C4G5C6C5D7D3B4C3E3F3", "No-Cat (Continuation)"
DATA "F5D6C5F4D3", "Horse"
DATA "F5D6C5F4E3C6D3F3", "Ralle"
DATA "F5D6C5F4E3C6D3F6E6D7", "Rose"
DATA "F5D6C5F4E3C6D3F6E6D7E7", "Greenberg, Dawg"
DATA "F5D6C5F4E3C6D3F6E6D7G3C4", "Rose-Birth"
DATA "F5D6C5F4E3C6D3F6E6D7G3C4B4", "Rose-birdie, Rose-Tamenori"
DATA "F5D6C5F4E3C6D3F6E6D7G3C4B4C3", "Rose-Tamenori-Kling"
DATA "F5D6C5F4E3C6D3F6E6D7G3C4G5C3B4E2", "Brightstein"
DATA "F5D6C5F4E3C6D3F6E6D7G4", "Flat"
DATA "F5D6C5F4E3C6D3F6E6D7G4C4", "Rotating Flat"
DATA "F5D6C5F4E3C6D3F6E6D7G4C4G5C3F7D2E7F2", "Rotating Flat (Kling Continuation)"
DATA "F5D6C5F4E3C6D3F6E6D7G4C4G6", "Murakami Variation"
DATA "F5D6C5F4E3C6D7", "Bhagat"
DATA "F5D6C5F4E3C6E6", "Inoue"
DATA "F5D6C5F4E3C6E6F6", "IAGO"
DATA "F5D6C5F4E3C6F3", "Shaman, Danish"
DATA "F5D6C5F4E3D3", "Mimura"
DATA "F5F4", "Parallel Opening"
DATA "F5F6", "Diagonal Opening"
DATA "F5F6C4F4", "Semi-Wing Variation"
DATA "F5F6D3F4", "Wing Variation"
DATA "F5F6E6D6C3", "Buffalo, Kenichi Variation"
DATA "F5F6E6D6C3D3", "Hokuriku Buffalo"
DATA "F5F6E6D6C3F4C6D3E3D2", "Tanida Buffalo"
DATA "F5F6E6D6C3G4C6", "Maruoka Buffalo"
DATA "F5F6E6D6C5", "Cow"
DATA "F5F6E6D6C5E3D3", "Rose-v-Toth"
DATA "F5F6E6D6C5E3D3C4C3", "Landau"
DATA "F5F6E6D6C5E3D3C4C6B5", "Maruoka"
DATA "F5F6E6D6C5E3D3G5", "Tanida"
DATA "F5F6E6D6C5E3D3G5D7", "Aircraft, Feldborg"
DATA "F5F6E6D6C5E3D3G5E2B5", "Sailboat"
DATA "F5F6E6D6C5E3E7", "Cow Bat, Bat, Cambridge"
DATA "F5F6E6D6C5E3E7C6D7F7C7F4G6E8D8C8G5", "Bat (Kling Continuation)"
DATA "F5F6E6D6C5E3E7C7D7C6D3", "Melnikov, Bat (Piau Continuation 1)"
DATA "F5F6E6D6C5E3E7C7D7C6F7", "Bat (Piau Continuation 2)"
DATA "F5F6E6D6C5E3E7F4F7", "Bat (Kling Alternative)"
DATA "F5F6E6D6C5F4", "Chimney"
DATA "F5F6E6D6C7C6D7", "Hamilton"
DATA "F5F6E6D6C7F4", "Lollipop"
DATA "F5F6E6D6D7", "Raccoon Dog"
DATA "F5F6E6D6E7", "Heath, Tobidashi 'Jumping Out'"
DATA "F5F6E6D6E7F4", "Heath-Chimney, 'Mass-Turning'"
DATA "F5F6E6D6E7G5C5", "Heath-Bat"
DATA "F5F6E6D6E7G5G4", "Iwasaki variation"
DATA "F5F6E6D6E7G5G6E3C5C6D3C4B3", "Mimura variation II"
DATA "F5F6E6D6F7", "Snake, Peasant"
DATA "F5F6E6D6F7E3D7E7C6C5D3", "Pyramid, Checkerboarding Peasant"
DATA "F5F6E6F4C3", "Buffalo, Kenichi Variation"
DATA "F5F6E6F4C3C4", "Hokuriku Buffalo"
DATA "F5F6E6F4C3D6F3C4C5B4", "Tanida Buffalo"
DATA "F5F6E6F4C3D7F3", "Maruoka Buffalo"
DATA "F5F6E6F4E3", "Cow"
DATA "F5F6E6F4E3C5C4", "Rose-v-Toth"
DATA "F5F6E6F4E3C5C4D3C3", "Landau"
DATA "F5F6E6F4E3C5C4D3F3E2", "Maruoka"
DATA "F5F6E6F4E3C5C4E7", "Tanida"
DATA "F5F6E6F4E3C5C4E7B5E2", "Sailboat"
DATA "F5F6E6F4E3C5C4E7G4", "Aircraft, Feldborg"
DATA "F5F6E6F4E3C5G5", "Cow Bat, Bat, Cambridge"
DATA "F5F6E6F4E3C5G5D6G6", "Bat (Kling Alternative)"
DATA "F5F6E6F4E3C5G5F3G4G6G3D6F7H5H4H3E7", "Bat (Kling Continuation)"
DATA "F5F6E6F4E3C5G5G3G4F3C4", "Melnikov, Bat (Piau Continuation 1)"
DATA "F5F6E6F4E3C5G5G3G4F3G6", "Bat (Piau Continuation 2)"
DATA "F5F6E6F4E3D6", "Chimney"
DATA "F5F6E6F4G3D6", "Lollipop"
DATA "F5F6E6F4G3F3G4", "Hamilton"
DATA "F5F6E6F4G4", "Raccoon Dog"
DATA "F5F6E6F4G5", "Heath, Tobidashi 'Jumping Out'"
DATA "F5F6E6F4G5D6", "Heath-Chimney, 'Mass-Turning'"
DATA "F5F6E6F4G5E7D7", "Iwasaki variation"
DATA "F5F6E6F4G5E7E3", "Heath-Bat"
DATA "F5F6E6F4G5E7F7C5E3F3C4D3C2", "Mimura variation II"
DATA "F5F6E6F4G6", "Snake, Peasant"
DATA "F5F6E6F4G6C5G4G5F3E3C4", "Pyramid, Checkerboarding Peasant"
DATA "END","END"



'Precalculated values for positions near the corners
'Diag XY : X = diagonal position near the corner, Y = corner
'Side XYZ: X = corner, Y side near corner, Z side near Y
'All in base3 format, 0=empty, 1=white, 2=black
'The base3 values are not used but are placed here for
'easy modification of their values (relative to white)
'Ex.: 001 = white has a corner
'
PatternsDiag:   'Values of diagonal corner patterns
DATA "00" ,0
DATA "01" ,400
DATA "02" ,-400
DATA "10" ,-500
DATA "11" ,200
DATA "12" ,-200
DATA "20" ,500
DATA "21" ,200
DATA "22" ,-200

PatternsSide:   'Values of side corner-adjacent patterns 
DATA "000",0
DATA "001",200
DATA "002",-200
DATA "010",-150
DATA "011",400
DATA "012",-550
DATA "020",150
DATA "021",550
DATA "022",-400
DATA "100",0
DATA "101",150
DATA "102",-200
DATA "110",-150
DATA "111",600
DATA "112",-300
DATA "120",550
DATA "121",200
DATA "122",-400
DATA "200",0
DATA "201",200
DATA "202",-150
DATA "210",-550
DATA "211",400
DATA "212",-200
DATA "220",150
DATA "221",200
DATA "222",-600


MoveOrder:    'Search for moves in this order to maximise AB cutting
DATA 01,08,57,64
DATA 03,06,17,24,41,48,59,62
DATA 19,22,43,46
DATA 04,05,25,32,33,40,60,61
DATA 20,21,27,30,35,38,44,45
DATA 12,13,26,31,34,39,52,53
DATA 11,14,18,23,42,47,51,54
DATA 02,07,09,16,49,56,58,63
DATA 10,15,50,55
DATA 28,29,36,37 'Starting position never played (for completeness)


HelpText:
DATA "-----------------------------------------"
DATA "OTHELLO MAX by Stephane Edwardson (2022) "
DATA "        (Lodovik on TheBackShed)         "
DATA "       Email: cybersed@hotmail.com       "
DATA "-----------------------------------------"
DATA "                                         "
DATA "[UP], [DOWN], [LEFT], [RIGHT] to scroll  "
DATA "[ENTER], [F1], button to exit            "
DATA "                                         "
DATA "Basics (from Wikipedia)                  "
DATA "-----------------------                  "
DATA "                                         "
DATA "There  are  64   identical  games  pieces"
DATA "called disks, which are light on one side"
DATA "and dark on the other. Players take turns"
DATA "placing  disks  on  the  board with their"
DATA "assigned color facing up.  During a play,"
DATA "any disks of the  opponent's  color  that"
DATA "are in a straight line and bounded by the"
DATA "disk just placed and  another disk of the"
DATA "current player's color are turned over to"
DATA "the current player's color. The objective"
DATA "of the game  is to have  the  majority of"
DATA "disks turned to display  one's color when"
DATA "the last playable empty square is filled."
DATA "                                         "
DATA "                                         "
DATA "This program                             "
DATA "------------                             "
DATA "                                         "
DATA "This program  was developed  on the Color"
DATA "Maximite II but it also works MMBasic for"
DATA "Windows  (MMB4W).  Be  sure  to  get  the"
DATA "latest  version,  as  MMB4W  is  still in"
DATA "development. It runs much  faster on a PC"
DATA "with even a moderate CPU (around 20X). It"
DATA "has  not  been  tested  on  other MMBasic"
DATA "machines. It requires a  lot  of  RAM  to"
DATA "store pre-calculated tables and use a lot"
DATA "of 2D accelerated functions.             "
DATA "                                         "
DATA "It's designed to be  played  on  a  wide-"
DATA "screen monitor at  960x540  resolution. I"
DATA "wanted to have a big  game board  with  a"
DATA "lot of space to  the  right  of  it.  The"
DATA "program will try to adjust if you  change"
DATA "the resolution but it  will  look  mostly"
DATA "ugly on lower-res and weird on 4:3.      "
DATA "                                         "
DATA "                                         "
DATA "The options                              "
DATA "-----------                              "
DATA "                                         "
DATA "When you start the program,  you  will be"
DATA "presented with the game options.  Use the"
DATA "direction arrows to change and select the"
DATA "available choices.                       "
DATA "                                         "
DATA "First, you need to  select  the  players."
DATA "For each color, you can select  human  or"
DATA "5 levels of computer strength:           "
DATA "                                         "
DATA "Level 1: computer  plays  randomly.   You"
DATA "         should be able to beat  it right"
DATA "         away  while  learning  the rules"
DATA "         and common strategies.          "
DATA "                                         "
DATA "Level 2: computer plays strategically but"
DATA "         without looking ahead at  future"
DATA "         moves. When 8 discs  of less are"
DATA "         left on the board, it  will look"
DATA "         ahead to the end  of the game to"
DATA "         find the most favorable moves.  "
DATA "                                         "
DATA "Level 3: computer looks 2 moves ahead and"
DATA "         also play  end of  games  with 8"
DATA "         moves ahead.  This  level  has a"
DATA "         much more advanced strategy. The"
DATA "         computer will  try  to  restrict"
DATA "         your moves, forcing you  to play"
DATA "         dangerous squares  that will put"
DATA "         it in position  gain the corners"
DATA "         and  then  extend its  pieces to"
DATA "         the borders. Level 3 will always"
DATA "         easily beat the  other  2  lower"
DATA "         levels.  This  level  will  play"
DATA "         fast: about 5 seconds  per  move"
DATA "         CMM2.                           "
DATA "                                         "
DATA "Level 4: same strategy as level 3  but it"
DATA "         looks 4 moves ahead in  mid-game"
DATA "         and 10 moves ahead  at  the  end"
DATA "         of the game. It is  hard to beat"
DATA "         and play  in  about  60-90s,  or"
DATA "         often less,  when it  finds  the"
DATA "         best move early in  its  search."
DATA "                                         "
DATA "Level 5: again  same   strategy  as   the"
DATA "         previous 2 but  will take  a LOT"
DATA "         of time on  anything but  a fast"
DATA "         computer  running   MMBasic  for"
DATA "         Windows.  It's  far too  slow on"
DATA "         CMM2 to be enjoyable. This level"
DATA "         looks at 6 moves ahead  during a"
DATA "         game and 11 at the end.         "
DATA "                                         "
DATA "You can select two humans as the players,"
DATA "in  which  case  each  player  will  play"
DATA "alternatively,  the  computer  then  used"
DATA "as an arbiter.  Note  that  opening  book"
DATA "selection  has no  effect  when two human"
DATA "players  play   together.  It  will  only"
DATA "report detected openings.                "
DATA "                                         "
DATA "You can also select two  computer players"
DATA "and they  will  play against  each  other"
DATA "countinuously,  with a 10  seconds  pause"
DATA "between each game.  This was  mostly used"
DATA "during development but  was left there as"
DATA "it can make entertaining games.          "
DATA "                                         "
DATA "Finally,  you can  select if  the opening"
DATA "book  will  be  active  for  the computer"
DATA "players. Othello Max has  a list of about"
DATA "90 openings  including  their symmetrical"
DATA "variations.  It   will   choose  randomly"
DATA "between  possible  openings at  start and"
DATA "then will begin  to compute its move when"
DATA "out of the opening sequence.             "
DATA "                                         "
DATA "It will  also display  which  opening  is"
DATA "presently  played,  regardless  of if the"
DATA "opening book is activated.               "
DATA "                                         "
DATA "                                         "
DATA "Input methods                            "
DATA "-------------                            "
DATA "                                         "
DATA "On the CMM2,  the  program  supports  WII"
DATA "controlers. It  will  detect Nunchuck and"
DATA "classic controllers.  It will  also works"
DATA "with the NES mini  pad  and  third  party"
DATA "devices, depending on compatibility.     "
DATA "                                         "
DATA "Keyboard and controllers  can be  used at"
DATA "the same  time,  which is  useful for two"
DATA "players games.                           "
DATA "                                         "
DATA "During  play,  you  can  undo a  move  by"
DATA "pressing  the [BACKSPACE]  key. Note that"
DATA "this will undo your  opponent's last move"
DATA "and then undo the move you  played before"
DATA "that. If  your  opponent  played  several"
DATA "consecutive moves, they will all be taken"
DATA "back so you  can replay  the move you did"
DATA "just before this sequence.               "
DATA "                                         "
DATA "In MMBasic for Windows, you use the mouse"
DATA "to play  and the  keybard  for  selecting"
DATA "the options. Use the left mouse button to"
DATA "select  your move  and  the  right  mouse"
DATA "button to undo a move.                   "
DATA "                                         "
DATA "                                         "
DATA "Development notes                        "
DATA "=================                        "
DATA "                                         "
DATA "I started  developing  this  program soon"
DATA "after I got  my CMM2,  in June 2021.  Now"
DATA "(July 2022),  more  than a  year  after I"
DATA "finally finished it and put all the stuff"
DATA "I wanted to put in.  I worked on it in my"
DATA "spare times,  with a long 5  months pause"
DATA "a the beginning of 2022.                 "
DATA "                                         "
DATA "I love the CMM2! It's the first ready-to-"
DATA "BASIC  computer  I  owned  that  is  fast"
DATA "enough to produce quality animations with"
DATA "ease.  Before, a  long  time  ago,  in  a"
DATA "distant basement far far away,  I learned"
DATA "BASIC on  the C64,  Amiga and  IBM PC.  I"
DATA "got back to programming in BASIC with the"
DATA "CMM2 and found that the experience was as"
DATA "immediate and fun as on the C64.         "
DATA "                                         "
DATA "From the  start,  I optimised  everything"
DATA "for speed.  I generated  a  lot  of  pre-"
DATA "computed  tables  to  speed up  the  move"
DATA "generator.  I used a  ternary  system  to"
DATA "code the  board  positions  with  0, 1, 2"
DATA "representing  empty,   white  and   black"
DATA "occupied  squares.  Almost  everything is"
DATA "reduced to additions and table lookups.  "
DATA "                                         "
DATA "I started by  programming a  random moves"
DATA "player and  then a strategic square moves"
DATA "opponent.  From the  start, I  could make"
DATA "them play each other,  which helped a lot"
DATA "finding bugs and enhancing play.         "
DATA "                                         "
DATA "But these first two levels were very easy"
DATA "to beat if you know  the basic strategies"
DATA "of the game. I was sure that the CMM2 was"
DATA "fast enough to create a good  opponent. I"
DATA "started  implementing the  minimax search"
DATA "but soon  discovered  that it takes a lot"
DATA "time to compute more  than 3 moves ahead."
DATA "                                         "
DATA "I then worked on the minimax algorithm to"
DATA "calculate  endings  with up  to 6  moves."
DATA "After  I   implemented    the  alpha-beta"
DATA "pruning,  I  saw a  spectacular  rise  in"
DATA "speed.  I could go up  to 10 moves in the"
DATA "same time as before in end game.         "
DATA "                                         "
DATA "Here  are  some  timed  searchs,  finding"
DATA "exactly the  same moves  but  much faster"
DATA "with alpha-beta pruning:                 "
DATA "                                         "
DATA "  Normal search (s)   Alpha-beta (s)     "
DATA "  -----------------   --------------     "
DATA "  16.8                1.3                "
DATA "  33.0                1.5                "
DATA "  47.0                1.8                "
DATA "  62.0                2.1                "
DATA "  76.7                2.3                "
DATA "  94.6                2.6                "
DATA "                                         "
DATA "During  development,  I  made  some  mini"
DATA "boards to  see how  search  progressed. I"
DATA "left them  in the  final program  as they"
DATA "are kind of  fun to  watch  and show that"
DATA "the program is  actually running  and not"
DATA "frozen.                                  "
DATA "                                         "
DATA "One of my little  challenges was  to make"
DATA "the  program   self-contained   with   no"
DATA "support files.  Everything  is  generated"
DATA "on the fly.  The CMM2 BASIC is FAST and I"
DATA "used  a  lot  of  2D   acceleration.  The"
DATA "blitter was  immensely  useful,  easy and"
DATA "fun to use.                              "
DATA "                                         "
DATA "The other  goal I  had  was  to  make  an"
DATA "opponent that at least could beat me. I'm"
DATA "an  average   Othello  /  Reversi  player"
DATA "but to  be enjoyable,  the program was to"
DATA "be at minimum a good player.  It can beat"
DATA "me at level 3 but I  often win because of"
DATA "its shortsighted  search.  At level 4, it"
DATA "wins most of the time.                   "
DATA "                                         "
DATA "The evaluation  function is  very simple:"
DATA "it just  check  for  3-squares   patterns"
DATA "around the corners and  add the scores of"
DATA "all of them  for all the corners.  A high"
DATA "score is obtained  of a good position but"
DATA "also on a bad position  of the  opponent."
DATA "These  patterns are  loaded at start from"
DATA "DATA statements  and are easily adjusted."
DATA "It also give points for mobility.        "
DATA "                                         "
DATA "At the beginning of the game, the level 3"
DATA "and up computer opponents will prioritise"
DATA "mobility.  They  will  often  fall a  lot"
DATA "behind on the  score but  will  have more"
DATA "moves to choose from which will also have"
DATA "the  effect  of  reducing  the   opponent"
DATA "mobility, a key factor to force bad moves"
DATA "and then regain disks at the end.        "
DATA "                                         "
DATA "If you notice a slight delay when you run"
DATA "the program, it's because the valid moves"
DATA "table  is   generated.   There  are  2187"
DATA "possible states of  a 7-squares-long line"
DATA "representing  the 7^3  possible positions"
DATA "next to a  played piece.  The  table will"
DATA "return the  number  of  pieces  that will"
DATA "change  color.  It saves  a lot  of  time"
DATA "compared to having the  position searched"
DATA "dynamically. There's also tables for each"
DATA "board position  telling how  many squares"
DATA "to look  in each  direction.  In essence,"
DATA "every possible  pieces configuration  for"
DATA "every  square  in 8  directions  is  pre-"
DATA "calculated  and put  on arrays.  All this"
DATA "because the  CMM2 has  a lot  of RAM  for"
DATA "variables.  I still  only  use 3%  of the"
DATA "available variable  memory  with  160K of"
DATA "tables.                                  "
DATA "                                         "
DATA "I worked a lot in refining and perfecting"
DATA "the interface.  I wanted it to be easy to"
DATA "operate and understand. The interface was"
DATA "getting priority at the  beginning and it"
DATA "later paid  off because  testing was made"
DATA "easier with a feature complete program.  "
DATA "                                         "
DATA "When  the  program  was  finished  on the"
DATA "CMM2, I was pleasantly  surprised that it"
DATA "also worked on  MMBasic for  Windows, but"
DATA "with  some  minor   glitches.  I  finally"
DATA "decided to support  Windows by correcting"
DATA "those small  problems  and  adding  mouse"
DATA "support. Also, I added a level 5 which is"
DATA "only playable on a  Windows  computer due"
DATA "to the time  it takes  to compute a  move"
DATA "two plies farther than level 4.          "
DATA "                                         "
DATA "So, I hope you enjoy this little game and"
DATA "learn a very unique  strategy game in the"
DATA "process. I had as much fun making it as I"
DATA "had playing it and watching  the autoplay"
DATA "mode, thinking about  how I could make it"
DATA "better.                                  "
DATA "END"
