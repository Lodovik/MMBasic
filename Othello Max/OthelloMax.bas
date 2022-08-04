'Othello Max by Stephane Edwardson (Lodovik)

font 1,1       'Workaround for bug in MMB4W
changepalette  'Workaround for bug in MMB4W

mode 12,8  '960x540, 256 colors
OPTION ANGLE DEGREES

CONST PROGNAME$ = "OTHELLO MAX"
CONST AUTHOR$ = "Stephane Edwardson"
CONST YEAR$ = "2022"
CONST VER$ = "1.0.2"
CONST BRDSQUARE = int(MM.VRES-100)/8  'Size of a square on the board
CONST BRDPC = BRDSQUARE / 2.5           'Size of pieces
CONST BKCOL = map(241)                'Screen background color
CONST BRDCOL = map(8)                 'Board background color
CONST BRDFRM = map(68)                'Board frame color
CONST BRDCOR = map(108)                'Board coordinates color
CONST P1COL = rgb(white)              'Player 1 pieces color
CONST P2COL = rgb(black)              'Player 2 pieces color
CONST WINDBAR = rgb(black)            'Windows title background color
CONST WINDTXT = rgb(white)            'Window bar text color
CONST WINDOUT = map(247)              'Window outline color
CONST WINDCON = map(249)              'Window content text color
CONST WINDBAK = map(8)                'Window background
CONST MAXSRCHD = 20                   'Max search depth
CONST BRDX = BRDSQUARE                'X location of the board on the screen 
CONST BRDY = BRDSQUARE                'Y location of the board on the screen
CONST ANIMSPD = 15                    'ms between frames of animation
CONST DEBUG = 0 '1
CONST SHOWSD = 3                      'Search depth visualisation

dim soundon = 1                       'Sound ON, 0 = OFF

dim winx1(10),winy1(10)               'Windows inside coordinates for easier reference 
dim winx2(10),winy2(10)               'Window 1 = score
                                      'Window 2 = black player infos
                                      'Window 3 = white player infos
                                      'Window 4 = status display infos
                                      'Window 5 = Opening book status
                                      'Window 9 = Help screen


dim board(64)                          'Board positions storage, player pieces on each square 1 = W, 2=B, 0=empty

dim srch(64,8,8)                       'Pre-computed related squares in 8 (0-7) directions for each square (position, dir, movelist)
                                       'For each square (position) srch(x,,),
                                       'srch(x,0,0)    = number of directions to look
                                       'srch(x,1,0)    = number of squares to look in direction 1
                                       'srch(x,1,1..y) = enumeration of the squares
                                       'srch(x,2,0)    = number of squares to look in direction 2
                                       'and so on...
                                       'srch(x,0,1)    = total number of related squares


dim drct(7) = (-8,-7,1,9,8,7,-1,-9)    'Search vectors for each of the 8 directions

dim vmtbl(2187,2)                      'Tables of valid moves ,1 = white; ,2 = black (precomputed nbr of moves)
                                       'Given a Base3 encoded value, returns # of flipped pieces, 0 if no valid move
                                       'Reduces the search in each direction to a simple addition and table lookup
                                       'In concert with srch() table, only valid direction are calculated  
                                        
dim base3(7,2)                         'Base3 values for faster conversion of board positions


dim movlist(MAXSRCHD,30,2)              'List of possible moves at specified depth
                                        'movlist(xxx,0,P)    = # of possible moves for player P
                                        'movlist(xxx,1..x,P) = list of possible moves

dim movflip(MAXSRCHD,30,2)              'List of # of squares flipped for each possible move, depth 
                                        'movflip(xxx,0,P)    = # of pieces flipped for player P
                                        'movflip(xxx,1..x,P) = list of flipped pieces for move 1..x, player P

dim moves(75,22)                        'List of moves (xx,20) played so far with pieces that were flipped (60,xx) for each move
                                        'moves(0,0) = number of moves played in the game
                                        'moves(xx,0) = for move xx, number of pieces flipped
                                        'moves(xx,21) = position played
                                        'moves(xx,22) = player color (1 or 2)
                                        'moves(xx,1..20) = list of the pieces that were flipped for move xx
dim flndx(2) = (0,2,1)                  'Flip the pieces with index instead if IF THEN

                                        'plmenu$() contains the various possible players to select 
dim plmenu$(10)
  plmenu$(1) = "HUMAN"
  plmenu$(2) = "COMPUTER LEVEL 1"       'Totally random
  plmenu$(3) = "COMPUTER LEVEL 2"       'Opening book + 1-ply evaluation + 8-plies  exhaustive endings
  plmenu$(4) = "COMPUTER LEVEL 3"       'Opening book + 2 plies ahead    + 8-plies  exhaustive endings
  plmenu$(5) = "COMPUTER LEVEL 4"       'Opening book + 4 plies ahead    + 10-plies exhaustive endings
  plmenu$(6) = "COMPUTER LEVEL 5"       'Opening book + 6 plies ahead    + 10-plies exhaustive endings



dim plsel(3) = (6,1,2,1)                'plsel(0) = nb of players choices, plsel(1) = black selection, plsel(2) = white selection, plsel(3) = Opening Book

dim sqval(64)                           'Squares values for non minimax computer players
dim statinfo$(10)                       'Lines of text to display in the status box; last line should be ""
dim srchinfo$(5)                        'Lines of text to display in the status box for search type and status index(0) = title
dim lastmv                              'Last move played
dim lastscore$                          'Last score displayed; will be used for animation when updating board
dim depth                               'Depth of search for accessing corresponding array
dim plyr                                'Current player color 1=W, 2=B 
dim esc                                 'Esc key pressed? 0 = No, 1 = Yes.
dim pcnt(2)                             'Pieces count
dim mvctr

dim sideval(27,2)  'Sides next to corners hints
dim diagval(9,2)   'Diag next to corners hints
dim base3b(2,2)    'Base3 precalculated powers
dim nodes          'count of positions evaluated
dim ctrl$          'Controller detected; "": nothing, "NUNCHUCK", "CLASSIC"
dim lastjoy        'Last state of the joystick
dim joys=0.5       'Joystick sensivity in % of max value; lower = more sensible
dim ju,jd,jl,jr    'Limits of analog joystick based on joys
 

dim openbookb,openbookw,openname$,opencont$ 'Opening book active (if enabled), variation name, continuation
dim opencount                               'Number of opening lines
dim obmenu$(4)                              'Opening book choices
  obmenu$(1) = "BLACK: OFF -  WHITE: OFF"
  obmenu$(2) = "BLACK: ON  -  WHITE: ON "
  obmenu$(3) = "BLACK: OFF -  WHITE: ON "
  obmenu$(4) = "BLACK: ON  -  WHITE: OFF"


dim obsel(1) = (4,2)                        '(0)=# of choices, (1)=default selection
'--------------------MAIN---------------------------

changepalette

initbase3
initsearcharray
initmovetables
initboard
initpatterns
checkctrl
countopenings


do
  restore StartPosition
  lastscore$ = "0000"
  initgame
  initsquarevalues
  plyr = 2        'Turn
  depth = 0
  showvalidmoves
  selectplayers

  x=0
  timer=0
  do while x<2

    plyr=2
    n=getmovelist(0)
    n=movlist(0,0,plyr)
    showvalidmoves
    if debug = 0 then
      statinfo$(1)="BLACK'S TURN"
      statinfo$(2)=""
      statusprint
    end if
    if n>0 then
      x=0
      select case plsel(2)
        case 1
          m = getmove(m,plyr)
        case 2
          m = getopening()
          if m = 0 then m = cp_johnrandom()
        case 3
          m = getopening()
          if m = 0 then m = cp_stratend()
        case 4
          m = getopening()
          if m = 0 then m = cp_complete3()
        case 5
          m = getopening()
          if m = 0 then m = cp_complete5()
        case 6
          m = getopening()
          if m = 0 then m = cp_complete7()
      end select
      f = playmove(m,plyr)
      fliplastmove
      findopening
      pause 500
    else
      inc x
      statinfo$(1)="BLACK CANNOT PLAY"
      statinfo$(2)=""
      statusprint
      pause 1000
    endif


    plyr = 1
    n=getmovelist(0)
    n=movlist(0,0,plyr)
    showvalidmoves
    if debug = 0 then  
      statinfo$(1)="WHITE'S TURN"
      statinfo$(2)=""
      statusprint
    endif
    if n>0 then
      x=0
      select case plsel(1)
        case 1
          m = getmove(m,plyr)
        case 2
          m = getopening()
          if m = 0 then m = cp_johnrandom()
        case 3
          m = getopening()
          if m = 0 then m = cp_stratend()
        case 4
          m = getopening()
          if m = 0 then m = cp_complete3()
        case 5
          m = getopening()
          if m = 0 then m = cp_complete5()
        case 6
          m = getopening()
          if m = 0 then m = cp_complete7()
        case 7
     end select
      f = playmove(m,plyr)
      fliplastmove
      findopening
      pause 500
    else
      inc x
      statinfo$(1)="WHITE CANNOT PLAY"
      statinfo$(2)=""
      statusprint
      pause 1000  
    endif  
  loop

  if plsel(1) = 1 or plsel(2) = 1 then
    statinfo$(1)="END OF GAME"
    statinfo$(2)=" "
    statinfo$(3)="PRESS [ENTER] OR BUTTON"
    statinfo$(4)="TO CONTINUE"
    statinfo$(5)=""
    statusprint

    k$=""
    emptykeybuf   'Empty keyboard buffer
    do while k$<>"enter"
      k$=getinput$()
    loop 
    statinfo$(1)=" "
    statinfo$(2)=""
    statusprint
  else
    statinfo$(1)="END OF GAME"
    statinfo$(2)=""
    statusprint
    pause 5000
  end if
  clearboard

loop



'--------------------SUBS & FUNCS---------------------------

sub undomove        'Undo a move (opponent move(s) and player move before
  local m,lm,p1,z

  m  = moves(0,0)    '# of moves played

  if m > 1 then        'At least 2 moves played? 
    p1 = plyr          'Player currently undoing the move
    lm = 0
    for z = 1 to m     'Searches for last move (could be the one just before)
      if moves(z,22) = p1 then lm = z
    next z
    if lm > 0 then
      for z = m to lm step -1
        bm = moves(z,21)
        clearpiece(bm)
        pause 500
        back1move
        drawallpieces
        pause 1000
      next z
    end if
  end if

end sub




sub clearpiece(p)            'Clears one piece with animation
  local cx,cy,z


  for z=BRDPC to 1 step -1
    cx = SquareX(p) + BRDSQUARE / 2 - 1
    cy = SquareY(p) + BRDSQUARE / 2 - 1
    circle cx,cy,z,2,,BRDCOL
    pause ANIMSPD 
  next z
      
end sub



sub drawallpieces 'Draw all the player pieces at once with animation (for undo move)
  local x,y,z,c,p


  for z = 1 to BRDPC
    for p = 1 to 64
      x = SquareX(p) + BRDSQUARE / 2 - 1
      y = SquareY(p) + BRDSQUARE / 2 - 1
      c = board(p) 

      select case c
      case 1
        if z = BRDPC then
          circle x,y,z,,,P2COL,P1COL
        else
          circle x,y,z,,,P1COL,P1COL
        end if
      case 2
        if z = BRDPC then
          circle x,y,z,,,P1COL,P2COL
        else
          circle x,y,z,,,P2COL,P2COL
        end if
      case else
        circle x,y,z,,,BRDCOL,BRDCOL
      end select  
    next p
    pause ANIMSPD/2
  next z

end sub





sub showhelp              'Show help screen in the center of the board
  local k$,s,z,x,y
  local t$(400),nl        'Help text in t$(), nl = number of lines
  local x1,x2,y1,y2,ml    'Coordinates of help windows (inside), ml=max number of lines (based on font)
  local fh = MM.INFO(FONTHEIGHT) +2 'Font height plus spacing
  local bw,bh,el,pt,upd

  font 1,1
  color WINDTXT,WINDBAK
 
  restore HelpText
  do while k$ <> "END"
    read k$
    inc nl
    t$(nl) = k$
  loop  
  nl = nl-1

  s = BRDSQUARE / 4
  page copy 0 to 2,I
  windowanim squarex(10)-s, squarey(10)-s, squarex(64)+s, squarey(64)+s, "HELP - PROGRAM V"+VER$,9

  x1 = winx1(9)       'Coords if the usable Help Windows
  x2 = winx2(9)
  y1 = winy1(9)
  y2 = winy2(9)
  bw = x2-x1          'Width and height of the Help Windows
  bh = y2-y1
  ml = int(bh/fh)-1   'Max number of lines
  el = nl-ml+1        'el=end of list, but always calculated relative to the top of the window 

  pt = 1              'Pointer to the first line to display at the top
  upd = 1             'Update flag
  emptykeybuf         'Empty keyboard buffer
  do while k$ <> "enter" and k$ <> "f1"
    k$ = getinput$()

    if k$ = "down" then
      pt = pt+1
      if pt > el then pt = el
      upd = 1
    else if k$ = "up" then
      pt = pt - 1
      if pt < 1 then pt = 1
      upd = 1
    else if k$ = "right" then
      pt = pt + 10
      if pt > el then pt = el
      upd = 1
    else if k$ = "left" then
      pt = pt - 10
      if pt < 1 then pt = 1
      upd = 1
    end if


    if upd = 1 then
      upd = 0
      for z=1 to ml
        printline t$(z+pt-1),z,x1,y1,bh,bw,2
      next z
    end if
  loop

  playtone 4500,4500,3
  fadeout squarex(10)-s, squarey(10)-s, squarex(64)+s, squarey(64)+s,2,0
end sub



sub printline(tx$,li,bx,by,bh,bw,sp)
  local fw,fh,cx,cy,t$


  fh = MM.INFO(FONTHEIGHT) + sp
  fw = MM.INFO(FONTWIDTH)
  cx = int(bw/fw)-2             'Maximum # of char per line at current font width 
  cy = int(bh/fh)               'Maximum of lines
  t$ = left$(tx$+space$(cx),cx) 'Crop text

  text bx+fw,by+li*fh,t$,"LT"


end sub


sub fadeout(x1,y1,x2,y2,ps,pd)  'Fade out by copying from page ps to page pd
  local z,w,h,s,stx,sty,xb1,yb1,xb2,yb2,wb,wh  

  w = x2-x1+1
  h = y2-y1+1
  s = 60      'step
  stx = int(w/s)
  sty = int(h/s)
  xb1 = x1
  yb1 = y1
  xb2 = x2
  yb2 = y2
  wb  = w
  wh  = h

  page write pd
  for z = 1 to int(w/stx/2)
    blit xb1,yb1,xb1,yb1,wb,sty,ps
    blit xb1,yb2-sty,xb1,yb2-sty,wb,sty,ps
    blit xb1,yb1,xb1,yb1,stx,wh,ps
    blit xb2-stx,yb1,xb2-stx,yb1,stx,wh,ps

    xb1 = xb1 + stx
    yb1 = yb1 + sty
    xb2 = xb2 - stx
    yb2 = yb2 - sty
    wb = wb - 2*stx
    wh = wh - 2*sty
    pause ANIMSPD/2 
  next z
  page copy ps to pd,B
end sub




sub windowanim(x1,y1,x2,y2,t$,i)  'Draw output windows; t$ = title, i = index to store inside coordinates in winx1()...Winy2() array
  local lw,tbx,tby,tbw,tbh,wx,wy,ww,wh
  local stx,sty,z


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
  
  for z = 1 to 60
    rbox cwx-z*stx/2,cwy-z*sty/2,z*stx,z*sty,10,WINDOUT,WINDBAK
    pause ANIMSPD/3
  next z

  rbox wx,wy,ww,wh,10,WINDOUT,WINDBAK
  rbox tbx,tby,tbw,tbh,10,WINDBAR,WINDBAR
  text tbx+tbw/2,tby+tbh/2+1,t$,"CM",4,1,WINDTXT,WINDBAR 

end sub





sub playtone(l,r,d)   'Plat a tone in L & R channels, D duration

  if soundon = 1 then
    play tone l,r,d
  end if
end sub




function getopening()   'Will return a valid opening book move in one is found; 0 if not
  local ob,op,oplist$,nbmatch,o$,n$,z,mo$,i,r

  op = 0                'Matching opening next move (if found, 0 if no moves)
  ob = 1                'Opening book ON
  select case obsel(1)  'Menu selection will turn it OFF if disabled
    case 1
      ob = 0
    case 3
      if plyr = 2 then ob = 0
    case 4
      if plyr = 1 then ob = 0
  end select  

  if ob = 1 then
    if moves(0,0) = 0 then  'First move? 
      r = int(rnd * opencount) + 1
      restore OpeningBook
      for z = 1 to r
        read o$,n$
      next z
      op = coord2dec(left$(o$,2))
    else
      mo$ = makemovelist$() 'Get move list of current game    
      restore OpeningBook

      for z = 1 to opencount
        read o$,n$
        i = instr(o$,mo$) 'Is current game moves part of opening?
        if i = 1 and len(o$) > len(mo$) then
          inc nbmatch
          oplist$ = oplist$ + mid$(o$,len(mo$)+1,2)  'Add this opening's next move to the string
        end if
      next z
      if nbmatch > 0 then
        r = int(rnd * nbmatch) * 2 + 1
        'showopen(str$(nbmatch)+":"+o$)
        op = coord2dec(mid$(oplist$,r,2))
      else
        op = 0
      end if
    end if
  end if

  getopening = op
end function





function makemovelist$()
  local z,mo$

  for z = 1 to moves(0,0)
    mo$ = mo$ + dec2coord$(moves(z,21))
  next z

  makemovelist$ = mo$
end function





sub findopening()       'Looks at the current moves and display corresponding book opening if found 
  local mo$,z,op$,nm$,ol
  
  mo$ = makemovelist$()
  if len(mo$) < 4 then openname$=""
  restore OpeningBook
  for z = 1 to opencount
    read op$,nm$
    l = len(op$)
    if left$(mo$,l) = op$ then
      if l > ol then  'Longest matching series of moves will be the current opening
        openname$ = nm$
        ol = l
      end if
    end if
  next z   

  showopen(openname$)    

end sub




sub countopenings   'Counts openings (opencount) in the DATA statements for later scanning
  local l$,n$
  
  restore OpeningBook
  opencount = -1
  do
    read l$,n$
    inc opencount
  loop until l$="END"
end sub





sub back1move                        'Back a move; faster than recopying the entire board, mv = 0 for a null move
  local m,po,co,z

  m = moves(0,0)                                    'Get move count
  if m > 0 then 
    po = moves(m,21)                                'Get position played
    co = moves(m,22)
    pcnt(co) = pcnt(co) - moves(m,0) - 1            'Update pieces count for player
    co = flndx(co)                                  'Get player color and invert it
    pcnt(co) = pcnt(co) + moves(m,0)                'Update opponent piece count
    for z = 1 to moves(m,0)                         'board(m,0) = number of pieces to flip
      board(moves(m,z)) = co
    next z
    board(po) = 0                                   'Remove piece from board (0=empty)
    m = m-1                                         'Decrement moves played
    moves(0,0) = m                                  'Update move number
  endif

end sub






function playmove(po,co)           'Plays a move, flip pieces (optimized, not on screen); po = position 1..64; co = color 1,2; returns # of flipped pieces
  local dir,npc,pat,flip,m,fc,mv


  fc = 0                                        'Flipped pieces count
  if po > 0 then                                'check for null move
    m = moves(0,0)                              'Get move count
    inc m                                       'Increment moves played

    for dir = 1 to srch(po,0,0)                 'For each valid direction
      pat = board(srch(po,dir,1))

      for npc = 2 to srch(po,dir,0)             'Other squares to add to the pattern using pre-calculated search limits
        pat = pat + base3(npc,board(srch(po,dir,npc)))
      next npc 

      if vmtbl(pat,co) <> 0 then
        for flip = 1 to vmtbl(pat,co)           'Number of pieces flipped by player co
          mv = srch(po,dir,flip)
          inc fc
          moves(m,fc) = mv                      'Store the flipped piece coordinate
          board(mv) = co                        'Flip the piece
        next flip
      endif
    next dir
    moves(0,0) = m                              'Update move number
    moves(m,0) = fc                             'Store number of flipped pieces
    moves(m,21) = po                            'Store the position played at the end of the flipped array dimension
    moves(m,22) = co                            'Store the player color played at the end of the flipped array dimension
    board(po) = co

    pcnt(co) = pcnt(co) + fc + 1                'Update player pieces count            
    pcnt(flndx(co)) = pcnt(flndx(co)) - fc      'Update opponent pieces count
  endif
  playmove = fc
end function



function cp_stratend()
  local mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2) 'Empty squares
  if ml > 8 then
    mv = cp_strategic()
    srchinfo$(1)="1-ply strategic"
  else
    srchinfo$(1)="7-plies exhaustive"
    mv = endgame()
  endif
  cp_stratend = mv


end function



function cp_complete3()
  local mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  if ml > 8 then
    srchinfo$(1)="2-plies depth"
    mv = midgame(3)
  else
    srchinfo$(1)="8-plies exhaustive"
    mv = endgame()
  endif
  cp_complete3 = mv


end function





function cp_complete4()
  local mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  if ml > 8 then
    srchinfo$(1)="3-plies depth"
    mv = midgame(4)
  else
    srchinfo$(1)="8-plies exhaustive"
    mv = endgame()
  endif
  cp_complete4 = mv


end function



function cp_complete5()
  local mv,ml
  
  ml = 64 - pcnt(1) - pcnt(2)
  if ml > 10 then
    srchinfo$(1)="4-plies depth"
    mv = midgame(5)
  else
    srchinfo$(1)="10-plies exhaustive"
    mv = endgame()
  endif
  cp_complete5 = mv

end function





function cp_complete6()
  local mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  if ml > 10 then
    srchinfo$(1)="5-plies depth"
    mv = midgame(6)
  else
    srchinfo$(1)="10-plies exhaustive"
    mv = endgame()
  endif
  cp_complete6 = mv

end function




function cp_complete7()
  local mv,ml

  
  ml = 64 - pcnt(1) - pcnt(2)
  if ml > 11 then
    srchinfo$(1)="6-plies depth"
    mv = midgame(7)
  else
    srchinfo$(1)="11-plies exhaustive"
    mv = endgame()
  endif
  cp_complete7 = mv

end function




function midgame(maxdp)       'Midgame; Alpha-Beta search and eval function entry and start, maxdp=depth of search
local p1,p2,dp,nnmoves,p1moves,m,bst,bmv,t,f,alpha,beta,mdp,nod2,nps


  p1 = plyr                   'P1 is current player
  p2 = flndx(p1)              'P2 is the other
  dp = 1                      'Depth of search
  mdp = maxdp
  bst = -9999
  bmv = 0
  alpha = -9999
  beta = 9999
  timer = 0
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

  for m = 1 to p1moves
    f = playmove(movlist(dp,m,p1),p1)
    showsearch(dp)
    t = midgame_min(p2,dp+1,alpha,beta,mdp)
    if t > bst then   'MAX
      bst = t
      bmv = movlist(dp,m,p1)
      alpha = t
    endif      
    nps = int(nodes/timer*1000)
    srchinfo$(2)=str$(nodes)
    srchinfo$(3)=str$(nps)
    srchinfo$(4)=dec2coord$(bmv) + ", "+str$(bst)

    back1move
  next m

  midgame = bmv   

end function




function midgame_min(player,depth,alpha,beta,maxdp)       'Endgame; Alpha-Beta minimum
  local p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b,mdp


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
  if dp < mdp then
    if p1moves > 0 then
      for m = 1 to p1moves
        f = playmove(movlist(dp,m,p1),p1)
        if dp <= SHOWSD then showsearch(dp)
        t = midgame_max(p2,dp+1,a,b,mdp)
        'en = t
        back1move
        if t < bst then bst = t
        b = min(b,t)
        if b <= a then   'MIN
          exit for
        endif      
      next m
      en = bst
    elseif p2moves > 0 then
      en = midgame_max(p2,dp+1,a,b,mdp)
    else
      en = (pcnt(p2) - pcnt(p1)) * 100
    endif
  else
    en = p2moves - p1moves + poseval(p2)
    inc nodes
  endif
  midgame_min = en

end function



function midgame_max(player,depth,alpha,beta,maxdp)       'Endgame; Alpha-Beta maximum
  local p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b,mdp
  local tt

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
  if dp < mdp then
    if p1moves > 0 then
      for m = 1 to p1moves
        f = playmove(movlist(dp,m,p1),p1)
        if dp <= SHOWSD then showsearch(dp)
        t = midgame_min(p2,dp+1,a,b,mdp)
        'en = t
        back1move
        if t > bst then bst = t
        a = max(a,t)
        if a >= b then   'MAX
          exit for
        endif      
      next m
      en = bst
    elseif p2moves > 0 then
      en = midgame_min(p2,dp+1,a,b,mdp)
    else
      en = (pcnt(p1) - pcnt(p2)) * 100
    endif
  else
    en = p1moves - p2moves + poseval(p1)
    inc nodes
  endif
  midgame_max = en

end function


function poseval(p)
  local en,m,p1

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
end function




function endgame()       'Endgame; play all the possible moves until game is over and chose the best outcome
  local p1,p2,dp,nnmoves,p1moves,m,bst,bmv,t,f,alpha,beta


  p1 = plyr                   'P1 is current player
  p2 = flndx(p1)              'P2 is the other
  dp = 1                      'Depth of search
  bst = -9999
  bmv = 0
  alpha = -9999
  beta = 9999
  timer = 0
  nps = 0

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1

  srchinfo$(2)=""
  srchinfo$(3)=""
  srchinfo$(4)=""
  statinfo$(1)=" "
  statinfo$(2)=""
  statusprint

  for m = 1 to p1moves
    f = playmove(movlist(dp,m,p1),p1)
    if dp <= SHOWSD then showsearch(dp)
    t = endgame_min(p2,dp+1,alpha,beta)
    if t > bst then   'MAX
      bst = t
      bmv = movlist(dp,m,p1)
      alpha = t
    endif      
    nps = int(nodes/timer*1000)
    srchinfo$(2)=str$(nodes)
    srchinfo$(3)=str$(nps)
    srchinfo$(4)=dec2coord$(bmv) + ", "+str$(bst)

    back1move
  next m

  endgame = bmv   

end function




function endgame_min(player,depth,alpha,beta)       'Endgame; play all the possible moves until game is over and chose the best outcome
  local p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b


  p1 = player                  'P1 is other player
  p2 = flndx(p1)                         'P2 is current
  a = alpha
  b = beta
  dp = depth
  bst = 9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  if p1moves > 0 then
    for m = 1 to p1moves
      f = playmove(movlist(dp,m,p1),p1)
      if dp <= SHOWSD then showsearch(dp)
      t = endgame_max(p2,dp+1,a,b)
      back1move
      if t < bst then bst = t
      b = min(b,t)
      if b <= a then   'MIN
        exit for
      endif      
    next m
    en = bst
  elseif p2moves > 0 then
    en = endgame_max(p2,dp+1,a,b)
  else
    en = pcnt(p2) - pcnt(p1)
    inc nodes
  endif
  endgame_min = en

end function



function endgame_max(player,depth,alpha,beta)       'Endgame; play all the possible moves until game is over and chose the best outcome
  local p1,p2,dp,nnmoves,p1moves,p2moves,m,bst,t,en,a,b


  p1 = player                  'P1 is other player
  p2 = flndx(p1)                         'P2 is current
  a = alpha
  b = beta
  dp = depth
  bst = -9999

  nnmoves = getmovelist(dp)     'Get number of moves for both B&W       
  p1moves = movlist(dp,0,p1)   'Number of moves for p1
  p2moves = movlist(dp,0,p2)   'Number of moves for p2
  if p1moves > 0 then
    for m = 1 to p1moves
      f = playmove(movlist(dp,m,p1),p1)
      if dp <= SHOWSD then showsearch(dp)
      t = endgame_min(p2,dp+1,a,b)
      back1move
      if t > bst then bst = t
      a = max(a,t)
      if a >= b then   'MAX
        exit for
      endif      
    next m
    en = bst
  elseif p2moves > 0 then
    en = endgame_min(p2,dp+1,a,b)
  else
    en = pcnt(p1) - pcnt(p2)
    inc nodes
  endif
  endgame_max = en

end function




sub showsearch(depth)
  local sq,px,py,sz,wd,tw,tx,ty,sx,sy

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

end sub



sub srchstatprint(tx,ty,sx,sy)          'Display search infos in the status box
                                        'sinfo$(0) = title; tx,ty = titlepos; sx,sy = status position
                                        'status lines in sinfo$(1)-sinfo$(4)
  local z,fh,sp,l,h,x,y,si$(4)
  
  fh = 8         'font height to calculate center
  sp = 2          'line spacing

  text tx,ty,srchinfo$(0),"CM",4,,WINDTXT,WINDBAK

  x = sx
  y = sy

  si$(1)="Search type : "+srchinfo$(1)
  si$(2)="Evaluations : "+srchinfo$(2)
  si$(3)="Evals / sec : "+srchinfo$(3)
  si$(4)="Best, score : "+srchinfo$(4)

  for z=1 to 4
    text x,y,left$(si$(z)+space$(40),40),"LT",7,,WINDTXT,WINDBAK
    y = y + fh + sp
  next z

end sub





sub miniboard(xx,yy,s,p)   'Display 1-8 miniature boards positions
                         'xx,yy = coordinates, s = inside square size, p = position (1,2,3) )

  local stp,cx,cy,x,y,col(2),mv,lm,px,py,sz

  col(0)=BRDCOL : col(2)=rgb(black) : col(1)=rgb(white)
  sz = 9+9*s
  px = xx+sz*p
  py = yy
  stp = s+1


  for x=0 to 7
    for y=0 to 7  
      cx=px+x*stp
      cy=py+y*stp
      mv=cr2mov(x,y)
      box cx,cy,s+2,s+2,,WINDOUT,col(board(mv))
    next y
  next x

  lm = moves(moves(0,0),21)
  x = pos2col(lm)
  y = pos2row(lm)  
  box px+x*stp,py+y*stp,s+2,s+2,,rgb(red),col(board(lm))  'Draw a red square around last positon played

end sub





function cp_strategic()         'Computer player. Plays strategic squares without any depth analysis
  local z,best,s,r$,y,n,mv,m,v


  n=movlist(0,0,plyr)
  best = 0
  mv = movlist(0,1,plyr)
  for z = 1 to n
    m = movlist(0,z,plyr)
    v = sqval(m) + movflip(0,z,plyr)
    if v+rnd*15 >= best+rnd*10 then
      mv = m
      best = v
    end if 
  next z

  sv = sqval(mv)
  select case mv 'Adjusting values after a corner is played
    case 1
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
    case 8
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
    case 57
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
    case 64
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
  end select  

  cp_strategic = mv
end function




function cp_johnrandom()    'Plays a random move
  local m,n

  n=movlist(0,0,plyr)
  m = movlist(0,int(rnd*n)+1,plyr)
  cp_johnrandom = m
end function




sub initsquarevalues      'Init a board with the relative positional values of the squares; used for easy-level computer oponents
  local s,z,y,r$

  restore PositionHint
  s = 0
  for z = 1 to 8
    read r$
    for y = 1 to 8
      inc s
      sqval(s) = 10 * val(mid$(r$,y,1))
    next y
  next z

end sub




sub checkctrl


  ctrl$=""
  on error skip 1
  controller Nunchuk open
  if MM.ERRNO = 0 then
    ctrl$="UNKNOWN"
    if nunchuk(T) = &HA4200000 then
      ctrl$="NUNCHUK"
      jl=nunchuk(JXC) + (nunchuk(JXL) - nunchuk(JXC)) * joys
      jr=nunchuk(JXC) + (nunchuk(JXR) - nunchuk(JXC)) * joys
      ju=nunchuk(JYC) + (nunchuk(JYT) - nunchuk(JYC)) * joys
      jd=nunchuk(JYC) + (nunchuk(JYB) - nunchuk(JYC)) * joys
    else if nunchuk(T) = &HA4200101 then 
      controller nunchuk close
      on error skip 1
      controller classic open
      if MM.ERRNO = 0 then
        if classic(T) = &HA4200101 then ctrl$="CLASSIC"
      end if
    end if
  end if

end sub




sub selectplayers 'Select players before game; opening book menu added and hacked in later
  local k$,sel,pt$,t1,t2,np,pcol,ng,cnt$
  local sx(2) = (0,2,1)

  statinfo$(1) = "["+chr$(146)+chr$(147)+chr$(149)+chr$(148)+"] TO SELECT OPTIONS"
  statinfo$(2) = "[ENTER] OR BUTTON TO START GAME"
  statinfo$(3) = "[ESC] TO QUIT PROGRAM"
  statinfo$(4) = "[F1] FOR HELP"
  statinfo$(5) = " "
  statinfo$(6) = " "
  if ctrl$ <> "" then statinfo$(6) = ctrl$+" CONTROLLER DETECTED"
  statinfo$(7) = " "
  statinfo$(8) = ""
  
  statusprint

  cnt$=ctrl$
  k$ = ""
  pt$ = chr$(148)+chr$(149)
  sel = 2
  showplayersel 1,"  "
  showplayersel 2,pt$
  showplayersel 3,"  "
  ng = 11
  ct = 0

  emptykeybuf   'Empty keyboard buffer
  do while k$ <> "enter"
 
    if cnt$ <> ctrl$ then
      cnt$=ctrl$
      statinfo$(5) = " "
      if ctrl$ <> "" then statinfo$(5) = ctrl$+" CONTROLLER DETECTED"
      statusprint
    end if

    k$ = getinput$()
    select case k$
      case "up"
        sel = sel-1
        if sel < 1 then
          sel = 1
        else
          showplayersel sel+1," "
          showplayersel sel,pt$
          ng = 11
        end if
        k$ = ""
      case "down"
        sel = sel+1
        if sel > 3 then
          sel = 3
        else
          showplayersel sel-1," "
          showplayersel sel,pt$
          ng = 11
        end if
        k$ = ""
      case "left"
        if sel < 3 then
          plsel(sx(sel)) = plsel(sx(sel))-1
          if plsel(sx(sel)) < 1 then
            plsel(sx(sel)) = 1
          else
            showplayersel sel,pt$
            ng = 11
          end if
        else
          obsel(1) = obsel(1) - 1
          if obsel(1) < 1 then
            obsel(1) = 1
          else
            showplayersel 3,pt$
          end if
          ng = 11
        end if
        k$ = ""
      case "right"
        if sel <3 then
          plsel(sx(sel)) = plsel(sx(sel))+1
          if plsel(sx(sel)) > plsel(0) then
            plsel(sx(sel)) = plsel(0)
          else
            showplayersel sel,pt$
            ng = 11
          end if
        else
          obsel(1) = obsel(1) + 1
          if obsel(1) > obsel(0) then
            obsel(1) = obsel(0)
          else
            showplayersel 3,pt$
          end if
          ng = 11
        end if
        k$ = ""
      case "enter"
        showplayersel sel," "
      case "esc"   
        end
      case "f1"
        showhelp
        ng = 11
    end select

    if plsel(1) > 1 and plsel(2) > 1 then
      if ct=1 then
        if timer - t1 > 1000 then
          ng = ng - 1
          t1 = timer
          statinfo$(7) = "AUTOPLAY IN "+str$(ng)+" SEC"
          statusprint
          if ng = 0 then k$="enter"
        end if
      else
        ct = 1
      end if

    else
      ng =1
      t1 = timer
      if ct = 1 then
        statinfo$(7) = " "
        statusprint
        ct=0
      end if
    end if
  loop

  playtone 4500,4500,3
  showplayersel sel," "
  statinfo$(1) = ucase$(plmenu$(plsel(2)))
  statinfo$(2) = "VS"
  statinfo$(3) = ucase$(plmenu$(plsel(1)))
  if statinfo$(1) = statinfo$(3) then statinfo$(3) = "ITSELF"
  statinfo$(4) = ""
  statusprint


end sub




sub showplayersel(p,hl$)   'Show player selection and arrows pointing to selected player; 2 spaces will erase arrows)
  local x1,y1,x2,y2,h,pl

  h = 16
  if p = 1 then       'Black player (P1)
    x1 = winx1(2)+1
    y1 = winy1(2)
    x2 = winx2(2)-2
    y2 = winy2(2)-2
  else if p = 2 then  'White player (P2)
    x1 = winx1(3)+1
    y1 = winy1(3)
    x2 = winx2(3)-2
    y2 = winy2(3)-2
  else if p = 3 then  'Opening book selection
    x1 = winx1(5)+1
    y1 = winy1(5)
    x2 = winx2(5)-2
    y2 = winy2(5)-2
  end if

  rbox x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK 'Erase previous content

  x = x1+(x2-x1)/2    'Center text
  y = y1 + (y2-y1)/2

  if p < 3 then
    pl = 2
    if p = 2 then pl = 1
    text x,y,plmenu$(plsel(pl)),"CM",4,,WINDCON,WINDBAK   
    text x1-12,y,left$(hl$,1),"CM",4,2,rgb(white),BKCOL
    text x2+14,y,right$(hl$,1),"CM",4,2,rgb(white),BKCOL
  else
    text x,y,obmenu$(obsel(1)),"CM",4,,WINDCON,WINDBAK   
    text x1-12,y,left$(hl$,1),"CM",4,2,rgb(white),BKCOL
    text x2+14,y,right$(hl$,1),"CM",4,2,rgb(white),BKCOL
  end if
end sub



sub showopen(o$)   'Show opening status
  local x1,y1,x2,y2,x,y

  x1 = winx1(5)+1
  y1 = winy1(5)
  x2 = winx2(5)-2
  y2 = winy2(5)-2
  end if

  rbox x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK 'Erase previous content

  x = x1+(x2-x1)/2    'Center text
  y = y1 + (y2-y1)/2

  text x,y,ucase$(o$),"CM",1,,WINDCON,WINDBAK   

end sub





sub statusprint       'Display text in the status box
  local x1,y1,x2,y2,fh,sp,l,h,x,y
  
  fh = 16         'font height to calculate center
  sp = 3          'line spacing
  x1 = winx1(4)+3
  y1 = winy1(4)
  x2 = winx2(4)-5
  y2 = winy2(4)-5
  box x1,y1,x2-x1,y2-y1,10,WINDBAK,WINDBAK
  l = 10
  for z=10 to 1 step -1
    if statinfo$(z) = "" then l=z-1   'Find last line
  next z
  h = fh*l + sp*(l-1)
  x = x1+(x2-x1)/2
  y = y1 + (y2-y1-h+fh)/2

  for z=1 to l
    text x,y,statinfo$(z),"CM",4,,WINDTXT,WINDBAK
    y = y + fh + sp
  next z

end sub





sub dprint(s$,p)        'Scroll up and print a line of text on the INFO window, for debug purpose.
  local x1,y1,x2,y2
  
  if debug = 1 then
    x1 = winx1(4)+6
    y1 = winy1(4)+6
    x2 = winx2(4)-6
    y2 = winy2(4)-6
    blit x1,y1+9,x1,y1,x2-x1,y2-y1-9
    text x1,y2,string$((x2-x1)/6," "),"LB",7,,WINDTXT,WINDBAK
    text x1,y2,s$,"LB",7,,WINDTXT,WINDBAK
    if p > 0 then pause p
  end if
end sub




sub displayscore    'Display / update the current score
  local x,y,ns$,dir(4),dpx(4),dx,dy,p1x,p1y,p2x,p2y,z
  local brx1,brx2,bry2,brw,brh,lastb,newb,incb

  dx = int((winx2(1)-winx1(1)-128) / 4)
  dy = int((winy2(1)-winy1(1)-50) / 2)
    

  p1x = winx1(1)+dx/2 'Top left x location of first score digit for each player
  p1y = winy1(1)+dy-8 '
  p2x = p1x+64+3*dx
  p2y = winy1(1)+dy-8
  brx1 = p1x          'Bar graph score start x
  brx2 = p2x+64       'Bar graph score end x
  brw = brx2-brx1     'Bar graph score width
  bry1 = p1y+50+dx/2  'Bar graph score start y
  brh = bry1+dx/2-bry1'Bar graph score height

  ns$ = right$(str$(100+pcnt(2)),2) + right$(str$(100+pcnt(1)),2)  

  if lastscore$="0000" then
    text p1x,p1y,"00","LT",6,,rgb(black),WINDBAK 'WINDBAK
    text p2x,p2y,"00","LT",6,,rgb(white),WINDBAK 'WINDBAK
    box brx1,bry1,brw,brh,,rgb(white),rgb(white)
    box brx1,bry1,brw/2,brh,,rgb(black),rgb(black)

  end if


  page write 1
  text p1x,p1y,left$(lastscore$,2),"LT",6,,rgb(black),WINDBAK 'WINDBAK
  text p2x,p2y,right$(lastscore$,2),"LT",6,,rgb(white),WINDBAK 'WINDBAK
  text p1x,p1y+50,left$(ns$,2),"LT",6,,rgb(black),WINDBAK 'WINDBAK
  text p2x,p2y+50,right$(ns$,2),"LT",6,,rgb(white),WINDBAK 'WINDBAK
  text p1x,p1y-50,left$(ns$,2),"LT",6,,rgb(black),WINDBAK 'WINDBAK
  text p2x,p2y-50,right$(ns$,2),"LT",6,,rgb(white),WINDBAK 'WINDBAK

  for z = 1 to 4                                        'Set digits rotation directions
    if mid$(lastscore$,z,1) > mid$(ns$,z,1) then
      dir(z) = -1
    else if mid$(lastscore$,z,1) < mid$(ns$,z,1) then
      dir(z) = 1
    else
      dir(z) = 0
    end if
  next z
  
  dpx(1) = p1x
  dpx(2) = p1x+32
  dpx(3) = p2x
  dpx(4) = p2x+32


  
  lastb = brx1 + scorepct(lastscore$) * brw
  newb = brx1 + scorepct(ns$) * brw
  incb = (newb-lastb)/50


  page write 0
  
  for z = 1 to 50
    for x = 1 to 4
      blit dpx(x),p1y+dir(x)*z,dpx(x),p1y,32,50,1   'Rotate the score digits
    next x

    if incb > 0 then
      box brx1,bry1,lastb-brx1+z*incb,brh,,rgb(black),rgb(black)    'Increase black's score bar
    else
      box lastb+z*incb,bry1,brx2-lastb-z*incb,brh,,rgb(white),rgb(white)  'Increase white's score bar
    end if

    pause ANIMSPD/6
  next z
    
  lastscore$ = ns$
  
end sub




function scorepct(sc$)
  local p1,p2,pct

  
  p1 = val(left$(sc$,2))
  p2 = val(right$(sc$,2))
  if p1 + p2 > 0 then
    pct = p1/(p1+p2)
  else
    pct = .5
  end if

  scorepct = pct 
end function





sub showvalidmoves    'Highlight valid moves for current player
  local mv,n,bs,z,x1,y1,s

  mv = getmovelist(0)
  n = movlist(0,0,plyr)
  bs = BRDSQUARE / 16

  col = P1COL
  if plyr = 2 then col = P2COL

  for z = 1 to 64
    if board(z) = 0 then
      x1 = SquareX(z) + BRDSQUARE / 2 - 1
      y1 = SquareY(z) + BRDSQUARE / 2 - 1
      circle x1,y1,bs,,1,BRDCOL,BRDCOL
    end if
  next z  

  if n >0 then
    for z=1 to n
      s = movlist(0,z,plyr)
      x1 = SquareX(s) + BRDSQUARE / 2 - 1
      y1 = SquareY(s) + BRDSQUARE / 2 - 1
      circle x1,y1,bs,,1,col,col
    next z      
  end if

end sub





sub fliplastmove      'Flip the pieces of last move played (animated)
  local lm,pcol,z,m,w,col1,col2,x1,y1,p,po$

  m = moves(0,0)      'Get # of moves
  lm = moves(m,21)    'Get last move played
  pcol = moves(m,22)  'Get color of last move
  playpiece lm,pcol

  select case pcol    'Select colors depending on player's turn
    case 1
      col1 = P2COL
      col2 = P1COL
    case 2
      col1 = P1COL
      col2 = P2COL
  end select  


  for z = 1 to 0 step -0.1
    page write 1
    drawsquare 1      
    x1 = SquareX(1) + BRDSQUARE / 2 - 1
    y1 = SquareY(1) + BRDSQUARE / 2 - 1
    circle x1,y1,BRDPC,,z,col2,col1
    page write 0

    for w = 1 to moves(m,0)
      p = moves(m,w)
      if debug = 1 then
        page write 1
        po$ = right$(str$(p+100),2)
        text SquareX(1)+1,SquareY(1)+1,po$,LT,7,1,map(12),BRDCOL
        page write 0
      end if
      blit SquareX(1),SquareY(1),SquareX(p),SquareY(p),BRDSQUARE,BRDSQUARE,1
    next w
    pause ANIMSPD
  next z  

  for z = 0 to 1 step 0.1
    page write 1
    drawsquare 1      
    x1 = SquareX(1) + BRDSQUARE / 2 - 1
    y1 = SquareY(1) + BRDSQUARE / 2 - 1
    circle x1,y1,BRDPC,,z,col1,col2
    page write 0

    for w = 1 to moves(m,0)
      p = moves(m,w)
      if debug = 1 then
        page write 1
        po$ = right$(str$(p+100),2)
        text SquareX(1)+1,SquareY(1)+1,po$,LT,7,1,map(12),BRDCOL
        page write 0
      end if
      blit SquareX(1),SquareY(1),SquareX(p),SquareY(p),BRDSQUARE,BRDSQUARE,1
    next w
    pause ANIMSPD
  next z  

  displayscore
end sub




sub clearboard            'Clear the board with animation
  local cx,cy,x,y,z,s,ss,r,p

  r = int(rnd*100)

  select case r
    case 0 to 85
      for z=BRDPC to 1 step -1
        for p = 1 to 64
          cx = SquareX(p) + BRDSQUARE / 2 - 1
          cy = SquareY(p) + BRDSQUARE / 2 - 1
          circle cx,cy,z,2,,BRDCOL
        next p   
        pause ANIMSPD 
      next z
      
    case else
      s = 360 / 180
      for z=0 to 360-s step s
        ss = z
        for p = 1 to 64
          ss = ss + s
          cx = SquareX(p) + BRDSQUARE / 2 - 1
          cy = SquareY(p) + BRDSQUARE / 2 - 1
          x = cx + cos(ss) * BRDPC
          y = cy + sin(ss) * BRDPC
          line cx,cy,x,y,,BRDCOL
          line cx+1,cy,x+1,y,,BRDCOL
          line cx,cy+1,x,y+1,,BRDCOL
        next p   
        pause ANIMSPD/3 
      next z
  end select
end sub




sub playpiece(p, c) 'Draw the player piece (p) on specified square. c = player color
  local x,y,z

  x = SquareX(p) + BRDSQUARE / 2 - 1
  y = SquareY(p) + BRDSQUARE / 2 - 1

  select case c
    case 1
      playtone 4000,4000,1
      for z=1 to BRDPC
        circle x,y,z,,,P2COL,P1COL
        pause ANIMSPD
      next z
    case 2
      playtone 3500,3500,1
      for z=1 to BRDPC
        circle x,y,z,,,P1COL,P2COL
        pause ANIMSPD
      next z
    case else
      drawsquare p      
  end select  
end sub





function getmovelist(d)   'Return the number of possible moves, and the list of them in movlist(d, moves, player)
  local x,y,mvs1,mvs2,po,pat,npc,flip1,flip2,dir

  mvs1  = 0   'Number of possible moves from this position for P1
  mvs2  = 0   '....P2
  for po = 1 to 64
    if board(po) = 0 then   'Check only empty squares
      flip1 = 0
      flip2 = 0

      for dir = 1 to srch(po,0,0)    'For each valid direction
        pat = board(srch(po,dir,1))
        for npc = 2 to srch(po,dir,0)   'Other squares to add to the pattern using pre-calculated search limits
            pat = pat + base3(npc,board(srch(po,dir,npc)))
        next npc 
        flip1 = flip1 + vmtbl(pat,1)    'Number of pieces flipped by player 1
        flip2 = flip2 + vmtbl(pat,2)    'Number of pieces flipped by player 1
      next dir

      if flip1 > 0 then    'At least a piece flipped for P1?
        inc mvs1           'One more square to add to the list of possible moves
        movlist(d,mvs1,1) = po
        movflip(d,mvs1,1) = flip1
      end if

      if flip2 > 0 then    'At least a piece flipped for P2?
        inc mvs2           'One more square to add to the list of possible moves
        movlist(d,mvs2,2) = po
        movflip(d,mvs2,2) = flip2
      end if

    end if

  next po
  movlist(d,0,1) = mvs1         'store # of P1 moves into index 0 of movlist
  movlist(d,0,2) = mvs2         'store # of P2 moves into index 0 of movlist
  movlist(d,0,0) = mvs1 + mvs2  'Store # of total moves (if = 0 then game is over)
  getmovelist = movlist(d,0,0) 

end function






function getmovelistfast(p,d)   'Faster(?) Return the number of possible moves, and the list of them in movlist(depth,moves)
  local x,y,mvs,po,pat,npc,flip

  mvs = 0   'Number of possible moves from this position
  for po = 1 to 64
    if board(po) = 0 then   'Check only empty squares
      flip = 0

      for dir = 1 to srch(po,0,0)    'For each valid direction
        pat = board(srch(po,dir,1))
        for npc = 2 to srch(po,dir,0)   'Other squares to add to the pattern using pre-calculated search limits
            pat = pat + base3(npc,board(srch(po,dir,npc)))
        next npc 
        flip = flip + vmtbl(pat,p)    'Number of pieces flipped
        if flip > 0 then
          inc mvs           'One more square to add to the list of possible moves
          movlist(d,mvs) = po
          movflip(d,mvs) = flip
          exit for
        end if
      next dir


    end if

  next po
  movlist(d,0) = mvs    'store # of moves into index 0 of movlist
  getmovelistfast = mvs

end function





sub prtscr          'Make a screenshot
  local f$,d$,t$

  d$ = date$
  t$ = time$
  f$ = right$(d$,4)+mid$(d$,4,2)+left$(d$,2)+"-"+left$(t$,2)+mid$(t$,4,2)+right$(t$,2)
  page write 0
  save image f$
  page write 1
  cls rgb(white)
  page display 1
  pause 5
  page display 0
  page write 0  
end sub




sub initbase3     'Precalculated base 3 values
  local b,e,t

  t = 1
  for e = 1 to 7 
    base3(e,1) = t
    base3(e,2) = t*2
    t = t*3
  next e
end sub




sub initpatterns  'Init hint patterns and base3 coding
  local b,e,t,z,p$,v

  restore PatternsDiag
  for z = 0 to 8
    read p$,v
    diagval(z,1) = v
    diagval(z,2) = -v
  next z 

  restore PatternsSide
  for z = 0 to 26
    read p$,v
    sideval(z,1) = v
    sideval(z,2) = -v
  next z 

  t = 1
  for e = 0 to 2 
    base3b(e,1) = t
    base3b(e,2) = t*2
    t = t*3
  next e

end sub



function getmove(lm,c)  'Player C inputs move, last move lm used as a start point on the grid
  local x,y,px,py,co,xit,mv,k$,b

  if lm < 1 or lm > 64 then
    mv = 1
  else
    mv = lm
  end if
  
  xit = 0
  if c = 1 then
    co = P1COL
  else
    co = P2COL
  end if  
  
  emptykeybuf     'Empty keyboard buffer
  do
    x = SquareX(mv)
    y = SquareY(mv)
    blit read #1,x,y,BRDSQUARE, BRDSQUARE
    box x+1,y+1,BRDSQUARE-2,BRDSQUARE-2,2,co
    
    px = pos2col(mv)
    py = pos2row(mv)
    k$ = ""

    do while k$ = ""
      k$ = getinput$()
      select case k$
        case "up"
          py = py - 1
          if py = -1 then py = 7
        case "down"
          py = py + 1
          if py = 8 then py = 0 
        case "left"
          px = px - 1
          if px = -1 then px = 7
        case "right"
          px = px + 1
          if px = 8 then px = 0
        case "enter"
          if moveisvalid(cr2mov(px,py),c) = 1 then xit = cr2mov(px,py) 
        case "back"
          blit write #1,x,y
          undomove
          k$ = ""
          showvalidmoves
          blit read #1,x,y,BRDSQUARE, BRDSQUARE
          displayscore
          box x+1,y+1,BRDSQUARE-2,BRDSQUARE-2,2,co
          emptykeybuf
      end select
    loop
    k$ = ""
    mv = cr2mov(px,py)
    blit write #1,x,y
  loop until xit <> 0

  getmove = mv
end function



sub emptykeybuf
  local b$

  do
    b$ = inkey$    
  loop until b$=""

end sub




function getinput$()  'Get input from keyboard and virtualize it
  local k,i$,j,jx,jy
  
  k = asc(inkey$)
  select case k
    case 128
      i$ = "up"
    case 129
      i$ = "down"
    case 130
      i$ = "left"
    case 131
      i$ = "right"
    case 13, 32, 10
      i$ = "enter"
    case 27
      i$ = "esc"
    case 8
      i$ = "back"
    case 10 , 0
      i$ = ""
    case 145
      i$ = "f1"
    case 157
      prtscr
      i$=""
    'case 156        'Reset
    '  run
    case else
      i$ = ""
  end select
  

  if  i$ = "" and ctrl$ <> "" then
    if ctrl$="CLASSIC" then
      j =  classic(B)
      if j = lastjoy and j>0 then
        pause 200
        j = 0
      end if
      if j and 15360 then i$ = "enter"
      if j and 32  then i$ = "down"
      if j and 64  then i$ = "right"
      if j and 128 then i$ = "up"
      if j and 256 then i$ = "left"
      lastjoy = j
      'if i$ <> "" then pause 200 
    elseif ctrl$ = "NUNCHUK" then
      j = 0
      jx = nunchuk(JX)
      jy = nunchuk(JY)
      if jx < jl then i$ = "left"  : j = j or 256
      if jx > jr then i$ = "right" : j = j or 64
      if jy < jd then i$ = "down"  : j = j or 32
      if jy > ju then i$ = "up"    : j = j or 128
      if nunchuk(z) = 1 then i$ = "enter" : j = j or 1
      if nunchuk(c) = 1 then i$ = "enter" : j = j or 2
      
      if j = lastjoy and j>0 then
        pause 200
        i$=""
        j = 0
      end if
      lastjoy = j
    end if
  end if
  
  if i$ <> "" then
    if i$ = "enter" then
    else
      playtone 5000,5000,1
    end if
  end if
  getinput$ = i$
end function




function moveisvalid(m,c) 'Check if a move is valid; m = square played (1..64), C = player (1,2)
  local z,v,n


  v = 0
  n = getmovelist(0)
  if movlist(0,0,c) > 0 then
    for z = 1 to movlist(0,0,c)
      if movlist(0,z,c) = m then v = 1
    next z
  end if
  moveisvalid = v
end function




function cr2mov(c,r) 'Column (0-7), Row (0-7) to grid (1-64) conversion
  cr2mov = r*8 + c + 1
end function




function pos2col(p)      'Grid (1-64) to column position (0-7)
  pos2col = (p-1) mod 8
end function





function pos2row(p)       'Grid (1-64) to row position (0-7)
  pos2row = int((p-1) / 8)
end function




sub initgame          'Read starting position and set board values
  local b$,r$,y$,z,c

  pcnt(1) = 0
  pcnt(2) = 0
  b$=""
  for z = 1 to 8
    read r$
    b$ = b$ + r$
  next
  for z = 1 to 64
    y$ = mid$(b$,z,1)
    select case y$
      case "O" 'White
        c = 1

      case "X" 'Black
        c = 2
      case else
        c = 0  'Empty
    end select
    board(z) = c
    inc pcnt(c)
  next z  
  moves(0,0)=0

  
  displaypos 0 'Draw board level 0 (main)
  displayscore
end sub





sub displaypos(l) 'Draw board at level l
  local z

  if debug = 1 then
    redrawboard
  else
    for z = 1 to 64
      playpiece z , board(z)
    next z
  end if

end sub




sub waitforkey

  do while inkey$<>"":loop
  do while inkey$="":loop
end sub




sub redrawboard
  local z

  for z = 1 to 64
    drawpiece z,board(z)
  next z
end sub





sub drawpiece(p, c) 'Draw the player piece (p) on specified square. c = player color
  local x,y,r,col1,col2

  x = SquareX(p) + BRDSQUARE / 2 - 1
  y = SquareY(p) + BRDSQUARE / 2 - 1

  select case c
    case 1
      circle x,y,BRDPC,,,P2COL,P1COL  
    case 2
      circle x,y,BRDPC,,,P1COL,P2COL  
    case else
      drawsquare p      
  end select  
end sub





sub drawsquare(p)  'Draw an empty square

    local po$,x,y

    x = SquareX(p)
    y = SquareY(p)
    box x, y, BRDSQUARE, BRDSQUARE, 1, rgb(black), BRDCOL
    if debug = 1 then
      po$ = right$(str$(p+100),2)
      text x+1,y+1,po$,LT,7,1,map(12),BRDCOL
    end if
end sub



sub changepalette
  local z

  for z = 1 to 14
    map(239+z) = rgb(0,z*16,0)
  next z
  map set
end sub



sub showpalette 'Show current palette and wait for a key

  local x,y,c


  cls
  for x = 0 to 15
    for y = 0 to 15
      c=x*16+y
      box x*30,y*30,30,30,1,map(c),map(c)
      text x*30+15,y*30+15,str$(c),"CM",7,,map(c xor 255),map(c)
    next y
  next x
  waitforkey
end sub



sub initboard  'Draw the playing board with frame and square coordinates
  local x,y,p,w,t$,brdx,brdy,brdx2,brdy2,wsp

  cls BKCOL

  x = SquareX(5)
  y = SquareY(1) - 20
  text x,y,PROGNAME$,"CB",1,2,rgb(white),BKCOL      'Title
  
  x = SquareX(1)
  brdx = x-20
  y = SquareY(1)
  brdy = y-20
  w = BRDSQUARE * 8 + 32
  brdx2 = brdx+w
  brdy2 = brdy+w
  box brdx,brdy,w,w,1,rgb(black), BRDFRM         'Frame
  line x,y,x-20,y-20,,black
  line x,y+w-41,x-20,y+w-21,,black
  line x+w-41,y,x+w-21,y-20,,black
  line x+w-41,y+w-41,x+w-21,y+w-21,,black

  for p = 1 to 8                                    'Squares
    x = SquareX(p) + BRDSQUARE / 2 - 4
    y = SquareY(1) - 3
    text x,y,chr$(64+p),"LB",,,BRDCOR,BRDFRM
    y = y + BRDSQUARE * 8 + 11
    text x,y,chr$(64+p),"LB",,,BRDCOR,BRDFRM

    x = SquareX(1) - 9
    y = SquareY(p*8) + BRDSQUARE / 2
    text x,y,chr$(48+p),"CM",,,BRDCOR,BRDFRM
    x = x + BRDSQUARE * 8 + 11
    text x,y,chr$(48+p),"CM",,,BRDCOR,BRDFRM
  next p

  for p = 1 to 64
    drawsquare p
    board(p) = 0
  next p        

  wsp = 8  
  window brdx2+BRDSQUARE*2.5,brdy,MM.HRES-BRDSQUARE*2.5,BRDSQUARE*2.4, "SCORE",1
  window brdx2+BRDSQUARE,winy2(1)+wsp,MM.HRES-BRDSQUARE,winy2(1)+BRDSQUARE+wsp, "BLACK PLAYER",2
  window winx1(2),winy2(2)+wsp,winx2(2),winy2(2)+BRDSQUARE+wsp, "WHITE PLAYER",3
  window winx1(3),winy2(3)+wsp,winx2(3),winy2(3)+BRDSQUARE+wsp, "OPENING BOOK",5
  window winx1(5),winy2(5)+wsp,winx2(5),brdy+w, "STATUS / INFO",4
end sub



sub window(x1,y1,x2,y2,t$,i)  'Draw output windows; t$ = title, i = index to store inside coordinates in winx1()...Winy2() array
  local lw,bx,by,bw,bh

  lw = 1
  rbox x1,y1,x2-x1-lw,y2-y1-lw,10,WINDOUT,WINDBAK

  bx = x1+lw
  by = y1+lw
  bw = x2-x1-2*lw-1
  bh = 20
  winx1(i) = x1
  winy1(i) = by+bh
  winx2(i) = x2
  winy2(i) = y2
  rbox bx,by,bw,bh,10,WINDBAR,WINDBAR
  text bx+bw/2,by+bh/2+1,t$,"CM",4,1,WINDTXT,WINDBAR 

end sub



'Precalculate squares to be included in relation to the current position (s=1..64)
'For each square (position) srch(x,,),
'srch(x,0,0)    = number of directions to look
'srch(x,1,0)    = number of squares to look in direction 1
'srch(x,1,1..y) = enumeration of the squares
'srch(x,2,0)    = number of squares to look in direction 2
'and so on...

'srch(x,0,1)    = total number of related squares

Sub initsearcharray 'Init of search limits array
  local x,y,s,d,z,p,l
  
  for x = 0 to 7
    for y = 0 to 7
      s = y*8+x+1
      d = 0
  
      if y > 1 then             '-8
        l = y
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s-8*z
          inc srch(s,0,1)
        next z
      end if

      if min(7-x,y) > 1 then    '-7
        l = min(7-x,y)
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s-7*z
          inc srch(s,0,1)
        next z
      end if

      if x < 6 then             '+1
        l = 7-x
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s+z
          inc srch(s,0,1)
        next z
      end if

      if min(7-x,7-y) > 1 then  '+9
        l = min(7-x,7-y)
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s+9*z
          inc srch(s,0,1)
        next z
      end if

      if y < 6 then             '+8
        l = 7-y
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s+8*z
          inc srch(s,0,1)
        next z
      end if

      if min(x,7-y) > 1 then    '+7
        l = min(x,7-y)
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s+7*z
          inc srch(s,0,1)
        next z
      end if

      if x > 1 then             '-1
        l = x
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s-z
          inc srch(s,0,1)
        next z
      end if

      if min(x,y) > 1 then      '-9
        l = min(x,y)
        inc d
        srch(s,0,0) = d
        srch(s,d,0) = l  
        for z = 1 to l
          srch(s,d,z) = s-9*z
          inc srch(s,0,1)
        next z
      end if
    
    next y
  next x
end sub




sub initmovetables     'Init of valid moves tables
  local z,a$            '0,1,2 in base three system with 0=empty, 1=white, 2=black
                        'Returns the number of flipped pieces
  for z = 0 to 2186
    a$=base$(3,z,7)

    'White valid position patterns
    if right$(a$,2) = "12" then vmtbl(z,1) = 1
    if right$(a$,3) = "122" then vmtbl(z,1) = 2
    if right$(a$,4) = "1222" then vmtbl(z,1) = 3
    if right$(a$,5) = "12222" then vmtbl(z,1) = 4
    if right$(a$,6) = "122222" then vmtbl(z,1) = 5
    if right$(a$,7) = "1222222" then vmtbl(z,1) = 6

    'Black valid position patterns
    if right$(a$,2) = "21" then vmtbl(z,2) = 1
    if right$(a$,3) = "211" then vmtbl(z,2) = 2
    if right$(a$,4) = "2111" then vmtbl(z,2) = 3
    if right$(a$,5) = "21111" then vmtbl(z,2) = 4
    if right$(a$,6) = "211111" then vmtbl(z,2) = 5
    if right$(a$,7) = "2111111" then vmtbl(z,2) = 6
  next
end sub




sub cvtopenings
  local x,y,z,o$,n$
  
  open "Openings.txt" for input as #1
  open "book" for output as #2
  do while not eof(#1)
    line input #1,o$
    line input #1,n$
    print #2, "data ";chr$(34);ucase$(o$);chr$(34);", ";chr$(34);n$;chr$(34)
  loop
  close #1
  close #2
end sub





sub pon
  page write 1
  font 7
  cls
end sub


sub poff
  pup
  page write 0
  font 1
end sub



sub pup
  local dx,dy,w,h

  dx = SquareX(8) + 2 * BRDSQUARE
  dy = SquareY(8)
  w = BRDSQUARE * 7
  h = BRDSQUARE * 5
  
  page write 0
  blit 0,0,dx,dy,w,h,1 
  page write 1
end sub




function coord2dec(c$)

  coord2dec = asc(left$(c$,1))-64 + (val(right$(c$,1))-1)*8 
end function





function dec2coord$(p)
  local c  
  
  c = p - 1
  dec2coord$ = chr$(65 + c mod 8) + chr$(49 + int(c/8)) 
end function




function SquareX(s)
  
  SquareX = BRDX + (s -1) mod 8 * (BRDSQUARE - 1)
end function


function SquareY(s)

  SquareY = BRDY + int((s - 1)/8) * (BRDSQUARE -1)
end function


sub makepatterns
  local z

  open "patterns.bas" for output as #2
  for z = 0 to 8
    print #2, "data ";chr$(34);base$(3,z,2);chr$(34);" ,000"
  next z
  for z = 0 to 26
    print #2, "data ";chr$(34);base$(3,z,3);chr$(34);",000"
  next z
  close #2

end sub





'Normal start position
StartPosition:
data "........"
data "........"
data "........"
data "...OX..."
data "...XO..."
data "........"
data "........"
data "........"


TestPosition:
data "........"
data ".XXOOX.."
data "...O...."
data "..XOXX.."
data "..OXO..."
data ".OXOX..."
data "........"
data "........"


EndGame10:
data "X..XXOXX"
data "XXOOOOXX"
data "X.OOXXXX"
data "XOOXXOXX"
data ".XXXOX.X"
data ".XXOOXXX"
data "XO.X.XXX"
data "XO..XXXX"

EndGame8:
data "XX.XXOXX"
data "XXXOOOXX"
data "X.OXXXXX"
data "XOOXXOXX"
data ".XXXOOOX"
data ".XXOOXXX"
data "XO.X.XXX"
data "XO..XXXX"

EndGame6:
data "XX.XXOXX"
data "XXXXOOXX"
data "X.XXXXXX"
data "XXOXXOXX"
data "XXXXOOOX"
data ".XXOOXXX"
data "XO.O.XXX"
data "XO.OXXXX"





'Squares values for each position at the start of the game (very approximative; debutant level computer play)
PositionHint:
data "90233209"
data "00444400"
data "24555542"
data "34500543"
data "34500543"
data "24555542"
data "00444400"
data "90233209"




'Various openings to make games more entertainings when playing strong computer player 
OpeningBook:
data "C4C3", "Diagonal Opening"
data "C4C3D3C5B3", "Snake, Peasant"
data "C4C3D3C5B3F4B5B4C6D6F5", "Pyramid, Checkerboarding Peasant"
data "C4C3D3C5B4", "Heath, Tobidashi 'Jumping Out'"
data "C4C3D3C5B4D2C2F4D6C6F5E6F7", "Mimura variation II"
data "C4C3D3C5B4D2D6", "Heath-Bat"
data "C4C3D3C5B4D2E2", "Iwasaki variation"
data "C4C3D3C5B4E3", "Heath-Chimney, 'Mass-Turning'"
data "C4C3D3C5B5", "Raccoon Dog"
data "C4C3D3C5B6C6B5", "Hamilton"
data "C4C3D3C5B6E3", "Lollipop"
data "C4C3D3C5D6", "Cow"
data "C4C3D3C5D6E3", "Chimney"
data "C4C3D3C5D6F4B4", "Cow Bat, Bat, Cambridge"
data "C4C3D3C5D6F4B4B6B5C6B3", "Bat (Piau Continuation 2)"
data "C4C3D3C5D6F4B4B6B5C6F5", "Melnikov, Bat (Piau Continuation 1)"
data "C4C3D3C5D6F4B4C6B5B3B6E3C2A4A5A6D2", "Bat (Kling Continuation)"
data "C4C3D3C5D6F4B4E3B3", "Bat (Kling Alternative)"
data "C4C3D3C5D6F4F5", "Rose-v-Toth"
data "C4C3D3C5D6F4F5D2", "Tanida"
data "C4C3D3C5D6F4F5D2B5", "Aircraft, Feldborg"
data "C4C3D3C5D6F4F5D2G4D7", "Sailboat"
data "C4C3D3C5D6F4F5E6C6D7", "Maruoka"
data "C4C3D3C5D6F4F5E6F6", "Landau"
data "C4C3D3C5F6", "Buffalo, Kenichi Variation"
data "C4C3D3C5F6E2C6", "Maruoka Buffalo"
data "C4C3D3C5F6E3C6F5F4G5", "Tanida Buffalo"
data "C4C3D3C5F6F5", "Hokuriku Buffalo"
data "C4C3D3E3C2", "Snake, Peasant"
data "C4C3D3E3C2D6E2D2F3F4E6", "Pyramid, Checkerboarding Peasant"
data "C4C3D3E3D2", "Heath, Tobidashi 'Jumping Out'"
data "C4C3D3E3D2B4B3D6F4F3E6F5G6", "Mimura variation II"
data "C4C3D3E3D2B4B5", "Iwasaki variation"
data "C4C3D3E3D2B4F4", "Heath-Bat"
data "C4C3D3E3D2C5", "Heath-Chimney, 'Mass-Turning'"
data "C4C3D3E3E2", "Raccoon Dog"
data "C4C3D3E3F2C5", "Lollipop"
data "C4C3D3E3F2F3E2", "Hamilton"
data "C4C3D3E3F4", "Cow"
data "C4C3D3E3F4C5", "Chimney"
data "C4C3D3E3F4D6D2", "Cow Bat, Bat, Cambridge"
data "C4C3D3E3F4D6D2C5C2", "Bat (Kling Alternative)"
data "C4C3D3E3F4D6D2F2E2F3C2", "Bat (Piau Continuation 2)"
data "C4C3D3E3F4D6D2F2E2F3E6", "Melnikov, Bat (Piau Continuation 1)"
data "C4C3D3E3F4D6D2F3E2C2F2C5B3D1E1F1B4", "Bat (Kling Continuation)"
data "C4C3D3E3F4D6E6", "Rose-v-Toth"
data "C4C3D3E3F4D6E6B4", "Tanida"
data "C4C3D3E3F4D6E6B4D7G4", "Sailboat"
data "C4C3D3E3F4D6E6B4E2", "Aircraft, Feldborg"
data "C4C3D3E3F4D6E6F5F3G4", "Maruoka"
data "C4C3D3E3F4D6E6F5F6", "Landau"
data "C4C3D3E3F6", "Buffalo, Kenichi Variation"
data "C4C3D3E3F6B5F3", "Maruoka Buffalo"
data "C4C3D3E3F6C5F3E6D6E7", "Tanida Buffalo"
data "C4C3D3E3F6E6", "Hokuriku Buffalo"
data "C4C3E6C5", "Wing Variation"
data "C4C3F5C5", "Semi-Wing Variation"
data "C4C5", "Parallel Opening"
data "C4E3", "Perpendicular Opening"
data "C4E3F4C5D6E6", "Mimura"
data "C4E3F4C5D6F3C6", "Shaman, Danish"
data "C4E3F4C5D6F3D3", "Inoue"
data "C4E3F4C5D6F3D3C3", "IAGO"
data "C4E3F4C5D6F3E2", "Bhagat"
data "C4E3F4C5D6F3E6C3D3E2", "Rose"
data "C4E3F4C5D6F3E6C3D3E2B5", "Flat"
data "C4E3F4C5D6F3E6C3D3E2B5F5", "Rotating Flat"
data "C4E3F4C5D6F3E6C3D3E2B5F5B3", "Murakami Variation"
data "C4E3F4C5D6F3E6C3D3E2B5F5B4F6C2E7D2C7", "Rotating Flat (Kling Continuation)"
data "C4E3F4C5D6F3E6C3D3E2B6F5", "Rose-Birth"
data "C4E3F4C5D6F3E6C3D3E2B6F5B4F6G5D7", "Brightstein"
data "C4E3F4C5D6F3E6C3D3E2B6F5G5", "Rose-birdie, Rose-Tamenori"
data "C4E3F4C5D6F3E6C3D3E2B6F5G5F6", "Rose-Tamenori-Kling"
data "C4E3F4C5D6F3E6C3D3E2D2", "Greenberg, Dawg"
data "C4E3F4C5D6F3E6C6", "Ralle"
data "C4E3F4C5E6", "Horse"
data "C4E3F5B4", "No-Cat"
data "C4E3F5B4F3", "Swallow"
data "C4E3F5B4F3F4E2E6G5F6D6C6", "No-Cat (Continuation)"
data "C4E3F5E6D3", "Italian"
data "C4E3F5E6F4", "Cat"
data "C4E3F5E6F4C5D6C6F7F3", "Sakaguchi"
data "C4E3F5E6F4C5D6C6F7G5G6", "Berner"
data "C4E3F6B4", "Ganglion"
data "C4E3F6E6F5", "Tiger"
data "C4E3F6E6F5C5C3", "Stephenson"
data "C4E3F6E6F5C5C3B4", "No-Kung"
data "C4E3F6E6F5C5C3B4D6C6B5A6B6C7", "No-Kung (Continuation)"
data "C4E3F6E6F5C5C3C6", "COMP'OTH"
data "C4E3F6E6F5C5C3G5", "Kung"
data "C4E3F6E6F5C5D3", "Leader's Tiger"
data "C4E3F6E6F5C5D6", "Brightwell"
data "C4E3F6E6F5C5F4G5G4F3C6D3D6", "Ishii"
data "C4E3F6E6F5C5F4G5G4F3C6D3D6B3C3B4E2B6", "Mainline Tiger"
data "C4E3F6E6F5C5F4G6F7", "Rose-BILL"
data "C4E3F6E6F5C5F4G6F7D3", "Tamenori"
data "C4E3F6E6F5G6", "Aubrey, Tanaka"
data "D3C3", "Diagonal Opening"
data "D3C3C4C5B3", "Snake, Peasant"
data "D3C3C4C5B3F4B5B4C6D6F5", "Pyramid, Checkerboarding Peasant"
data "D3C3C4C5B4", "Heath, Tobidashi 'Jumping Out'"
data "D3C3C4C5B4D2C2F4D6C6F5E6F7", "Mimura variation II"
data "D3C3C4C5B4D2D6", "Heath-Bat"
data "D3C3C4C5B4D2E2", "Iwasaki variation"
data "D3C3C4C5B4E3", "Heath-Chimney, 'Mass-Turning'"
data "D3C3C4C5B5", "Raccoon Dog"
data "D3C3C4C5B6C6B5", "Hamilton"
data "D3C3C4C5B6E3", "Lollipop"
data "D3C3C4C5D6", "Cow"
data "D3C3C4C5D6E3", "Chimney"
data "D3C3C4C5D6F4B4", "Cow Bat, Bat, Cambridge"
data "D3C3C4C5D6F4B4B6B5C6B3", "Bat (Piau Continuation 2)"
data "D3C3C4C5D6F4B4B6B5C6F5", "Melnikov, Bat (Piau Continuation 1)"
data "D3C3C4C5D6F4B4C6B5B3B6E3C2A4A5A6D2", "Bat (Kling Continuation)"
data "D3C3C4C5D6F4B4E3B3", "Bat (Kling Alternative)"
data "D3C3C4C5D6F4F5", "Rose-v-Toth"
data "D3C3C4C5D6F4F5D2", "Tanida"
data "D3C3C4C5D6F4F5D2B5", "Aircraft, Feldborg"
data "D3C3C4C5D6F4F5D2G4D7", "Sailboat"
data "D3C3C4C5D6F4F5E6C6D7", "Maruoka"
data "D3C3C4C5D6F4F5E6F6", "Landau"
data "D3C3C4C5F6", "Buffalo, Kenichi Variation"
data "D3C3C4C5F6E2C6", "Maruoka Buffalo"
data "D3C3C4C5F6E3C6F5F4G5", "Tanida Buffalo"
data "D3C3C4C5F6F5", "Hokuriku Buffalo"
data "D3C3C4E3C2", "Snake, Peasant"
data "D3C3C4E3C2D6E2D2F3F4E6", "Pyramid, Checkerboarding Peasant"
data "D3C3C4E3D2", "Heath, Tobidashi 'Jumping Out'"
data "D3C3C4E3D2B4B3D6F4F3E6F5G6", "Mimura variation II"
data "D3C3C4E3D2B4B5", "Iwasaki variation"
data "D3C3C4E3D2B4F4", "Heath-Bat"
data "D3C3C4E3D2C5", "Heath-Chimney, 'Mass-Turning'"
data "D3C3C4E3E2", "Raccoon Dog"
data "D3C3C4E3F2C5", "Lollipop"
data "D3C3C4E3F2F3E2", "Hamilton"
data "D3C3C4E3F4", "Cow"
data "D3C3C4E3F4C5", "Chimney"
data "D3C3C4E3F4D6D2", "Cow Bat, Bat, Cambridge"
data "D3C3C4E3F4D6D2C5C2", "Bat (Kling Alternative)"
data "D3C3C4E3F4D6D2F2E2F3C2", "Bat (Piau Continuation 2)"
data "D3C3C4E3F4D6D2F2E2F3E6", "Melnikov, Bat (Piau Continuation 1)"
data "D3C3C4E3F4D6D2F3E2C2F2C5B3D1E1F1B4", "Bat (Kling Continuation)"
data "D3C3C4E3F4D6E6", "Rose-v-Toth"
data "D3C3C4E3F4D6E6B4", "Tanida"
data "D3C3C4E3F4D6E6B4D7G4", "Sailboat"
data "D3C3C4E3F4D6E6B4E2", "Aircraft, Feldborg"
data "D3C3C4E3F4D6E6F5F3G4", "Maruoka"
data "D3C3C4E3F4D6E6F5F6", "Landau"
data "D3C3C4E3F6", "Buffalo, Kenichi Variation"
data "D3C3C4E3F6B5F3", "Maruoka Buffalo"
data "D3C3C4E3F6C5F3E6D6E7", "Tanida Buffalo"
data "D3C3C4E3F6E6", "Hokuriku Buffalo"
data "D3C3E6E3", "Semi-Wing Variation"
data "D3C3F5E3", "Wing Variation"
data "D3C5", "Perpendicular Opening"
data "D3C5D6E3F4C6B5", "Bhagat"
data "D3C5D6E3F4C6C4", "Inoue"
data "D3C5D6E3F4C6C4C3", "IAGO"
data "D3C5D6E3F4C6F3", "Shaman, Danish"
data "D3C5D6E3F4C6F5C3C4B5", "Rose"
data "D3C5D6E3F4C6F5C3C4B5B4", "Greenberg, Dawg"
data "D3C5D6E3F4C6F5C3C4B5E2", "Flat"
data "D3C5D6E3F4C6F5C3C4B5E2E6", "Rotating Flat"
data "D3C5D6E3F4C6F5C3C4B5E2E6C2", "Murakami Variation"
data "D3C5D6E3F4C6F5C3C4B5E2E6D2F6B3G5B4G3", "Rotating Flat (Kling Continuation)"
data "D3C5D6E3F4C6F5C3C4B5F2E6", "Rose-Birth"
data "D3C5D6E3F4C6F5C3C4B5F2E6D2F6E7G4", "Brightstein"
data "D3C5D6E3F4C6F5C3C4B5F2E6E7", "Rose-birdie, Rose-Tamenori"
data "D3C5D6E3F4C6F5C3C4B5F2E6E7F6", "Rose-Tamenori-Kling"
data "D3C5D6E3F4C6F5F3", "Ralle"
data "D3C5D6E3F4F5", "Mimura"
data "D3C5D6E3F5", "Horse"
data "D3C5E6D2", "No-Cat"
data "D3C5E6D2C6", "Swallow"
data "D3C5E6D2C6D6B5F5E7F6F4F3", "No-Cat (Continuation)"
data "D3C5E6F5C4", "Italian"
data "D3C5E6F5D6", "Cat"
data "D3C5E6F5D6E3F4F3G6C6", "Sakaguchi"
data "D3C5E6F5D6E3F4F3G6E7F7", "Berner"
data "D3C5F6D2", "Ganglion"
data "D3C5F6F5E6", "Tiger"
data "D3C5F6F5E6E3C3", "Stephenson"
data "D3C5F6F5E6E3C3D2", "No-Kung"
data "D3C5F6F5E6E3C3D2F4F3E2F1F2G3", "No-Kung (Continuation)"
data "D3C5F6F5E6E3C3E7", "Kung"
data "D3C5F6F5E6E3C3F3", "COMP'OTH"
data "D3C5F6F5E6E3C4", "Leader's Tiger"
data "D3C5F6F5E6E3D6E7D7C6F3C4F4", "Ishii"
data "D3C5F6F5E6E3D6E7D7C6F3C4F4C2C3D2B5F2", "Mainline Tiger"
data "D3C5F6F5E6E3D6F7G6", "Rose-BILL"
data "D3C5F6F5E6E3D6F7G6C4", "Tamenori"
data "D3C5F6F5E6E3F4", "Brightwell"
data "D3C5F6F5E6F7", "Aubrey, Tanaka"
data "D3E3", "Parallel Opening"
data "E6D6", "Parallel Opening"
data "E6F4", "Perpendicular Opening"
data "E6F4C3C4D3", "Tiger"
data "E6F4C3C4D3C2", "Aubrey, Tanaka"
data "E6F4C3C4D3D6C5", "Brightwell"
data "E6F4C3C4D3D6E3C2B3", "Rose-BILL"
data "E6F4C3C4D3D6E3C2B3F5", "Tamenori"
data "E6F4C3C4D3D6E3D2E2F3C6F5C5", "Ishii"
data "E6F4C3C4D3D6E3D2E2F3C6F5C5F7F6E7G4C7", "Mainline Tiger"
data "E6F4C3C4D3D6F5", "Leader's Tiger"
data "E6F4C3C4D3D6F6", "Stephenson"
data "E6F4C3C4D3D6F6C6", "COMP'OTH"
data "E6F4C3C4D3D6F6D2", "Kung"
data "E6F4C3C4D3D6F6E7", "No-Kung"
data "E6F4C3C4D3D6F6E7C5C6D7C8C7B6", "No-Kung (Continuation)"
data "E6F4C3E7", "Ganglion"
data "E6F4D3C4E3", "Cat"
data "E6F4D3C4E3D6C5C6B3D2C2", "Berner"
data "E6F4D3C4E3D6C5C6B3F3", "Sakaguchi"
data "E6F4D3C4F5", "Italian"
data "E6F4D3E7", "No-Cat"
data "E6F4D3E7F3", "Swallow"
data "E6F4D3E7F3E3G4C4D2C3C5C6", "No-Cat (Continuation)"
data "E6F4E3D6C4", "Horse"
data "E6F4E3D6C5C4", "Mimura"
data "E6F4E3D6C5F3C4C6", "Ralle"
data "E6F4E3D6C5F3C4F6F5G4", "Rose"
data "E6F4E3D6C5F3C4F6F5G4C7D3", "Rose-Birth"
data "E6F4E3D6C5F3C4F6F5G4C7D3D2", "Rose-birdie, Rose-Tamenori"
data "E6F4E3D6C5F3C4F6F5G4C7D3D2C3", "Rose-Tamenori-Kling"
data "E6F4E3D6C5F3C4F6F5G4C7D3E7C3D2B5", "Brightstein"
data "E6F4E3D6C5F3C4F6F5G4D7", "Flat"
data "E6F4E3D6C5F3C4F6F5G4D7D3", "Rotating Flat"
data "E6F4E3D6C5F3C4F6F5G4D7D3E7C3G6B4G5B6", "Rotating Flat (Kling Continuation)"
data "E6F4E3D6C5F3C4F6F5G4D7D3F7", "Murakami Variation"
data "E6F4E3D6C5F3C4F6F5G4G5", "Greenberg, Dawg"
data "E6F4E3D6C5F3C6", "Shaman, Danish"
data "E6F4E3D6C5F3F5", "Inoue"
data "E6F4E3D6C5F3F5F6", "IAGO"
data "E6F4E3D6C5F3G4", "Bhagat"
data "E6F6", "Diagonal Opening"
data "E6F6C4D6", "Wing Variation"
data "E6F6D3D6", "Semi-Wing Variation"
data "E6F6F5D6C3", "Buffalo, Kenichi Variation"
data "E6F6F5D6C3D3", "Hokuriku Buffalo"
data "E6F6F5D6C3F4C6D3E3D2", "Tanida Buffalo"
data "E6F6F5D6C3G4C6", "Maruoka Buffalo"
data "E6F6F5D6C5", "Cow"
data "E6F6F5D6C5E3D3", "Rose-v-Toth"
data "E6F6F5D6C5E3D3C4C3", "Landau"
data "E6F6F5D6C5E3D3C4C6B5", "Maruoka"
data "E6F6F5D6C5E3D3G5", "Tanida"
data "E6F6F5D6C5E3D3G5D7", "Aircraft, Feldborg"
data "E6F6F5D6C5E3D3G5E2B5", "Sailboat"
data "E6F6F5D6C5E3E7", "Cow Bat, Bat, Cambridge"
data "E6F6F5D6C5E3E7C6D7F7C7F4G6E8D8C8G5", "Bat (Kling Continuation)"
data "E6F6F5D6C5E3E7C7D7C6D3", "Melnikov, Bat (Piau Continuation 1)"
data "E6F6F5D6C5E3E7C7D7C6F7", "Bat (Piau Continuation 2)"
data "E6F6F5D6C5E3E7F4F7", "Bat (Kling Alternative)"
data "E6F6F5D6C5F4", "Chimney"
data "E6F6F5D6C7C6D7", "Hamilton"
data "E6F6F5D6C7F4", "Lollipop"
data "E6F6F5D6D7", "Raccoon Dog"
data "E6F6F5D6E7", "Heath, Tobidashi 'Jumping Out'"
data "E6F6F5D6E7F4", "Heath-Chimney, 'Mass-Turning'"
data "E6F6F5D6E7G5C5", "Heath-Bat"
data "E6F6F5D6E7G5G4", "Iwasaki variation"
data "E6F6F5D6E7G5G6E3C5C6D3C4B3", "Mimura variation II"
data "E6F6F5D6F7", "Snake, Peasant"
data "E6F6F5D6F7E3D7E7C6C5D3", "Pyramid, Checkerboarding Peasant"
data "E6F6F5F4C3", "Buffalo, Kenichi Variation"
data "E6F6F5F4C3C4", "Hokuriku Buffalo"
data "E6F6F5F4C3D6F3C4C5B4", "Tanida Buffalo"
data "E6F6F5F4C3D7F3", "Maruoka Buffalo"
data "E6F6F5F4E3", "Cow"
data "E6F6F5F4E3C5C4", "Rose-v-Toth"
data "E6F6F5F4E3C5C4D3C3", "Landau"
data "E6F6F5F4E3C5C4D3F3E2", "Maruoka"
data "E6F6F5F4E3C5C4E7", "Tanida"
data "E6F6F5F4E3C5C4E7B5E2", "Sailboat"
data "E6F6F5F4E3C5C4E7G4", "Aircraft, Feldborg"
data "E6F6F5F4E3C5G5", "Cow Bat, Bat, Cambridge"
data "E6F6F5F4E3C5G5D6G6", "Bat (Kling Alternative)"
data "E6F6F5F4E3C5G5F3G4G6G3D6F7H5H4H3E7", "Bat (Kling Continuation)"
data "E6F6F5F4E3C5G5G3G4F3C4", "Melnikov, Bat (Piau Continuation 1)"
data "E6F6F5F4E3C5G5G3G4F3G6", "Bat (Piau Continuation 2)"
data "E6F6F5F4E3D6", "Chimney"
data "E6F6F5F4G3D6", "Lollipop"
data "E6F6F5F4G3F3G4", "Hamilton"
data "E6F6F5F4G4", "Raccoon Dog"
data "E6F6F5F4G5", "Heath, Tobidashi 'Jumping Out'"
data "E6F6F5F4G5D6", "Heath-Chimney, 'Mass-Turning'"
data "E6F6F5F4G5E7D7", "Iwasaki variation"
data "E6F6F5F4G5E7E3", "Heath-Bat"
data "E6F6F5F4G5E7F7C5E3F3C4D3C2", "Mimura variation II"
data "E6F6F5F4G6", "Snake, Peasant"
data "E6F6F5F4G6C5G4G5F3E3C4", "Pyramid, Checkerboarding Peasant"
data "F5D6", "Perpendicular Opening"
data "F5D6C3D3C4", "Tiger"
data "F5D6C3D3C4B3", "Aubrey, Tanaka"
data "F5D6C3D3C4F4C5B3C2", "Rose-BILL"
data "F5D6C3D3C4F4C5B3C2E6", "Tamenori"
data "F5D6C3D3C4F4C5B4B5C6F3E6E3", "Ishii"
data "F5D6C3D3C4F4C5B4B5C6F3E6E3G6F6G5D7G3", "Mainline Tiger"
data "F5D6C3D3C4F4E3", "Brightwell"
data "F5D6C3D3C4F4E6", "Leader's Tiger"
data "F5D6C3D3C4F4F6", "Stephenson"
data "F5D6C3D3C4F4F6B4", "Kung"
data "F5D6C3D3C4F4F6F3", "COMP'OTH"
data "F5D6C3D3C4F4F6G5", "No-Kung"
data "F5D6C3D3C4F4F6G5E3F3G4H3G3F2", "No-Kung (Continuation)"
data "F5D6C3G5", "Ganglion"
data "F5D6C4D3C5", "Cat"
data "F5D6C4D3C5F4E3F3C2B4B3", "Berner"
data "F5D6C4D3C5F4E3F3C2C6", "Sakaguchi"
data "F5D6C4D3E6", "Italian"
data "F5D6C4G5", "No-Cat"
data "F5D6C4G5C6", "Swallow"
data "F5D6C4G5C6C5D7D3B4C3E3F3", "No-Cat (Continuation)"
data "F5D6C5F4D3", "Horse"
data "F5D6C5F4E3C6D3F3", "Ralle"
data "F5D6C5F4E3C6D3F6E6D7", "Rose"
data "F5D6C5F4E3C6D3F6E6D7E7", "Greenberg, Dawg"
data "F5D6C5F4E3C6D3F6E6D7G3C4", "Rose-Birth"
data "F5D6C5F4E3C6D3F6E6D7G3C4B4", "Rose-birdie, Rose-Tamenori"
data "F5D6C5F4E3C6D3F6E6D7G3C4B4C3", "Rose-Tamenori-Kling"
data "F5D6C5F4E3C6D3F6E6D7G3C4G5C3B4E2", "Brightstein"
data "F5D6C5F4E3C6D3F6E6D7G4", "Flat"
data "F5D6C5F4E3C6D3F6E6D7G4C4", "Rotating Flat"
data "F5D6C5F4E3C6D3F6E6D7G4C4G5C3F7D2E7F2", "Rotating Flat (Kling Continuation)"
data "F5D6C5F4E3C6D3F6E6D7G4C4G6", "Murakami Variation"
data "F5D6C5F4E3C6D7", "Bhagat"
data "F5D6C5F4E3C6E6", "Inoue"
data "F5D6C5F4E3C6E6F6", "IAGO"
data "F5D6C5F4E3C6F3", "Shaman, Danish"
data "F5D6C5F4E3D3", "Mimura"
data "F5F4", "Parallel Opening"
data "F5F6", "Diagonal Opening"
data "F5F6C4F4", "Semi-Wing Variation"
data "F5F6D3F4", "Wing Variation"
data "F5F6E6D6C3", "Buffalo, Kenichi Variation"
data "F5F6E6D6C3D3", "Hokuriku Buffalo"
data "F5F6E6D6C3F4C6D3E3D2", "Tanida Buffalo"
data "F5F6E6D6C3G4C6", "Maruoka Buffalo"
data "F5F6E6D6C5", "Cow"
data "F5F6E6D6C5E3D3", "Rose-v-Toth"
data "F5F6E6D6C5E3D3C4C3", "Landau"
data "F5F6E6D6C5E3D3C4C6B5", "Maruoka"
data "F5F6E6D6C5E3D3G5", "Tanida"
data "F5F6E6D6C5E3D3G5D7", "Aircraft, Feldborg"
data "F5F6E6D6C5E3D3G5E2B5", "Sailboat"
data "F5F6E6D6C5E3E7", "Cow Bat, Bat, Cambridge"
data "F5F6E6D6C5E3E7C6D7F7C7F4G6E8D8C8G5", "Bat (Kling Continuation)"
data "F5F6E6D6C5E3E7C7D7C6D3", "Melnikov, Bat (Piau Continuation 1)"
data "F5F6E6D6C5E3E7C7D7C6F7", "Bat (Piau Continuation 2)"
data "F5F6E6D6C5E3E7F4F7", "Bat (Kling Alternative)"
data "F5F6E6D6C5F4", "Chimney"
data "F5F6E6D6C7C6D7", "Hamilton"
data "F5F6E6D6C7F4", "Lollipop"
data "F5F6E6D6D7", "Raccoon Dog"
data "F5F6E6D6E7", "Heath, Tobidashi 'Jumping Out'"
data "F5F6E6D6E7F4", "Heath-Chimney, 'Mass-Turning'"
data "F5F6E6D6E7G5C5", "Heath-Bat"
data "F5F6E6D6E7G5G4", "Iwasaki variation"
data "F5F6E6D6E7G5G6E3C5C6D3C4B3", "Mimura variation II"
data "F5F6E6D6F7", "Snake, Peasant"
data "F5F6E6D6F7E3D7E7C6C5D3", "Pyramid, Checkerboarding Peasant"
data "F5F6E6F4C3", "Buffalo, Kenichi Variation"
data "F5F6E6F4C3C4", "Hokuriku Buffalo"
data "F5F6E6F4C3D6F3C4C5B4", "Tanida Buffalo"
data "F5F6E6F4C3D7F3", "Maruoka Buffalo"
data "F5F6E6F4E3", "Cow"
data "F5F6E6F4E3C5C4", "Rose-v-Toth"
data "F5F6E6F4E3C5C4D3C3", "Landau"
data "F5F6E6F4E3C5C4D3F3E2", "Maruoka"
data "F5F6E6F4E3C5C4E7", "Tanida"
data "F5F6E6F4E3C5C4E7B5E2", "Sailboat"
data "F5F6E6F4E3C5C4E7G4", "Aircraft, Feldborg"
data "F5F6E6F4E3C5G5", "Cow Bat, Bat, Cambridge"
data "F5F6E6F4E3C5G5D6G6", "Bat (Kling Alternative)"
data "F5F6E6F4E3C5G5F3G4G6G3D6F7H5H4H3E7", "Bat (Kling Continuation)"
data "F5F6E6F4E3C5G5G3G4F3C4", "Melnikov, Bat (Piau Continuation 1)"
data "F5F6E6F4E3C5G5G3G4F3G6", "Bat (Piau Continuation 2)"
data "F5F6E6F4E3D6", "Chimney"
data "F5F6E6F4G3D6", "Lollipop"
data "F5F6E6F4G3F3G4", "Hamilton"
data "F5F6E6F4G4", "Raccoon Dog"
data "F5F6E6F4G5", "Heath, Tobidashi 'Jumping Out'"
data "F5F6E6F4G5D6", "Heath-Chimney, 'Mass-Turning'"
data "F5F6E6F4G5E7D7", "Iwasaki variation"
data "F5F6E6F4G5E7E3", "Heath-Bat"
data "F5F6E6F4G5E7F7C5E3F3C4D3C2", "Mimura variation II"
data "F5F6E6F4G6", "Snake, Peasant"
data "F5F6E6F4G6C5G4G5F3E3C4", "Pyramid, Checkerboarding Peasant"
data "END","END"



'Precalculated values for positions near the corners
'Diag XY : X = diagonal position near the corner, Y = corner
'Side XYZ: X = corner, Y side near corner, Z side near Y
'All in base3 format, 0=empty, 1=white, 2=black
'The base3 values are not used but are placed here for
'easy modification of their values (relative to white)
'Ex.: 001 = white has a corner
'
PatternsDiag:   'Values of diagonal corner patterns
data "00" ,0
data "01" ,400
data "02" ,-400
data "10" ,-500
data "11" ,200
data "12" ,-200
data "20" ,500
data "21" ,200
data "22" ,-200

PatternsSide:   'Values of side corner-adjacent patterns 
data "000",0
data "001",200
data "002",-200
data "010",-150
data "011",400
data "012",-550
data "020",150
data "021",550
data "022",-400
data "100",0
data "101",150
data "102",-200
data "110",-150
data "111",600
data "112",-300
data "120",550
data "121",200
data "122",-400
data "200",0
data "201",200
data "202",-150
data "210",-550
data "211",400
data "212",-200
data "220",150
data "221",200
data "222",-600


HelpText:
data "-----------------------------------------"
data "OTHELLO MAX by Stephane Edwardson (2022) "
data "        (Lodovik on TheBackShed)         "
data "       Email: cybersed@hotmail.com       "
data "-----------------------------------------"
data "                                         "
data "[UP], [DOWN], [LEFT], [RIGHT] to scroll  "
data "[ENTER], [F1], button to exit            "
data "                                         "
data "Basics (from Wikipedia)                  "
data "-----------------------                  "
data "                                         "
data "There  are  64   identical  games  pieces"
data "called disks, which are light on one side"
data "and dark on the other. Players take turns"
data "placing  disks  on  the  board with their"
data "assigned color facing up.  During a play,"
data "any disks of the  opponent's  color  that"
data "are in a straight line and bounded by the"
data "disk just placed and  another disk of the"
data "current player's color are turned over to"
data "the current player's color. The objective"
data "of the game  is to have  the  majority of"
data "disks turned to display  one's color when"
data "the last playable empty square is filled."
data "                                         "
data "                                         "
data "This program                             "
data "------------                             "
data "                                         "
data "This program  was developed  on the Color"
data "Maximite II but it also works MMBasic for"
data "Windows  (MMB4W).  Be  sure  to  get  the"
data "latest  version,  as  MMB4W  is  still in"
data "development. It runs much  faster on a PC"
data "with even a moderate CPU (around 20X). It"
data "has  not  been  tested  on  other MMBasic"
data "machines. It requires a  lot  of  RAM  to"
data "store pre-calculated tables and use a lot"
data "of 2D accelerated functions.             "
data "                                         "
data "It's designed to be  played  on  a  wide-"
data "screen monitor at  960x540  resolution. I"
data "wanted to have a big  game board  with  a"
data "lot of space to  the  right  of  it.  The"
data "program will try to adjust if you  change"
data "the resolution but it  will  look  mostly"
data "ugly on lower-res and weird on 4:3.      "
data "                                         "
data "                                         "
data "The options                              "
data "-----------                              "
data "                                         "
data "When you start the program,  you  will be"
data "presented with the game options.  Use the"
data "direction arrows to change and select the"
data "available choices.                       "
data "                                         "
data "First, you need to  select  the  players."
data "For each color, you can select  human  or"
data "5 levels of computer strength:           "
data "                                         "
data "Level 1: computer  plays  randomly.   You"
data "         should be able to beat  it right"
data "         away  while  learning  the rules"
data "         and common strategies.          "
data "                                         "
data "Level 2: computer plays strategically but"
data "         without looking ahead at  future"
data "         moves. When 8 discs  of less are"
data "         left on the board, it  will look"
data "         ahead to the end  of the game to"
data "         find the most favorable moves.  "
data "                                         "
data "Level 3: computer looks 2 moves ahead and"
data "         also play  end of  games  with 8"
data "         moves ahead.  This  level  has a"
data "         much more advanced strategy. The"
data "         computer will  try  to  restrict"
data "         your moves, forcing you  to play"
data "         dangerous squares  that will put"
data "         it in position  gain the corners"
data "         and  then  extend its  pieces to"
data "         the borders. Level 3 will always"
data "         easily beat the  other  2  lower"
data "         levels.  This  level  will  play"
data "         fast: about 5 seconds  per  move"
data "         CMM2.                           "
data "                                         "
data "Level 4: same strategy as level 3  but it"
data "         looks 4 moves ahead in  mid-game"
data "         and 10 moves ahead  at  the  end"
data "         of the game. It is  hard to beat"
data "         and play  in  about  60-90s,  or"
data "         often less,  when it  finds  the"
data "         best move early in  its  search."
data "                                         "
data "Level 5: again  same   strategy  as   the"
data "         previous 2 but  will take  a LOT"
data "         of time on  anything but  a fast"
data "         computer  running   MMBasic  for"
data "         Windows.  It's  far too  slow on"
data "         CMM2 to be enjoyable. This level"
data "         looks at 6 moves ahead  during a"
data "         game and 11 at the end.         "
data "                                         "
data "You can select two humans as the players,"
data "in  which  case  each  player  will  play"
data "alternatively,  the  computer  then  used"
data "as an arbiter.  Note  that  opening  book"
data "selection  has no  effect  when two human"
data "players  play   together.  It  will  only"
data "report detected openings.                "
data "                                         "
data "You can also select two  computer players"
data "and they  will  play against  each  other"
data "countinuously,  with a 10  seconds  pause"
data "between each game.  This was  mostly used"
data "during development but  was left there as"
data "it can make entertaining games.          "
data "                                         "
data "Finally,  you can  select if  the opening"
data "book  will  be  active  for  the computer"
data "players. Othello Max has  a list of about"
data "90 openings  including  their symmetrical"
data "variations.  It   will   choose  randomly"
data "between  possible  openings at  start and"
data "then will begin  to compute its move when"
data "out of the opening sequence.             "
data "                                         "
data "It will  also display  which  opening  is"
data "presently  played,  regardless  of if the"
data "opening book is activated.               "
data "                                         "
data "                                         "
data "                                         "
data "Input methods                            "
data "-------------                            "
data "                                         "
data "On the CMM2,  the  program  supports  WII"
data "controlers. It  will  detect Nunchuck and"
data "classic controllers.  It will  also works"
data "with the NES mini  pad  and  third  party"
data "devices, depending on compatibility.     "
data "                                         "
data "Keyboard and controllers  can be  used at"
data "the same  time,  which is  useful for two"
data "players games.                           "
data "                                         "
data "During  play,  you  can  undo a  move  by"
data "pressing  the [BACKSPACE]  key. Note that"
data "this will undo your  opponent's last move"
data "and then undo the move you  played before"
data "that. If  your  opponent  played  several"
data "consecutive moves, they will all be taken"
data "back so you  can replay  the move you did"
data "just before this sequence.               "
data "                                         "
data "Development notes                        "
data "=================                        "
data "                                         "
data "I started  developing  this  program soon"
data "after I got  my CMM2,  in June 2021.  Now"
data "(July 2022),  more  than a  year  after I"
data "finally finished it and put all the stuff"
data "I wanted to put in.  I worked on it in my"
data "spare times,  with a long 5  months pause"
data "a the beginning of 2022.                 "
data "                                         "
data "I love the CMM2! It's the first ready-to-"
data "BASIC  computer  I  owned  that  is  fast"
data "enough to produce quality animations with"
data "ease.  Before, a  long  time  ago,  in  a"
data "distant basement far far away,  I learned"
data "BASIC on  the C64,  Amiga and  IBM PC.  I"
data "got back to programming in BASIC with the"
data "CMM2 and found that the experience was as"
data "immediate and fun as on the C64.         "
data "                                         "
data "From the  start,  I optimised  everything"
data "for speed.  I generated  a  lot  of  pre-"
data "computed  tables  to  speed up  the  move"
data "generator.  I used a  ternary  system  to"
data "code the  board  positions  with  0, 1, 2"
data "representing  empty,   white  and   black"
data "occupied  squares.  Almost  everything is"
data "reduced to additions and table lookups.  "
data "                                         "
data "I started by  programming a  random moves"
data "player and  then a strategic square moves"
data "opponent.  From the  start, I  could make"
data "them play each other,  which helped a lot"
data "finding bugs and enhancing play.         "
data "                                         "
data "But these first two levels were very easy"
data "to beat if you know  the basic strategies"
data "of the game. I was sure that the CMM2 was"
data "fast enough to create a good  opponent. I"
data "started  implementing the  minimax search"
data "but soon  discovered  that it takes a lot"
data "time to compute more  than 3 moves ahead."
data "                                         "
data "I then worked on the minimax algorithm to"
data "calculate  endings  with up  to 6  moves."
data "After  I   implemented    the  alpha-beta"
data "pruning,  I  saw a  spectacular  rise  in"
data "speed.  I could go up  to 10 moves in the"
data "same time as before in end game.         "
data "                                         "
data "Here  are  some  timed  searchs,  finding"
data "exactly the  same moves  but  much faster"
data "with alpha-beta pruning:                 "
data "                                         "
data "  Normal search (s)   Alpha-beta (s)     "
data "  -----------------   --------------     "
data "  16.8                1.3                "
data "  33.0                1.5                "
data "  47.0                1.8                "
data "  62.0                2.1                "
data "  76.7                2.3                "
data "  94.6                2.6                "
data "                                         "
data "During  development,  I  made  some  mini"
data "boards to  see how  search  progressed. I"
data "left them  in the  final program  as they"
data "are kind of  fun to  watch  and show that"
data "the program is  actually running  and not"
data "frozen.                                  "
data "                                         "
data "One of my little  challenges was  to make"
data "the  program   self-contained   with   no"
data "support files.  Everything  is  generated"
data "on the fly.  The CMM2 BASIC is FAST and I"
data "used  a  lot  of  2D   acceleration.  The"
data "blitter was  immensely  useful,  easy and"
data "fun to use.                              "
data "                                         "
data "The other  goal I  had  was  to  make  an"
data "opponent that at least could beat me. I'm"
data "an  average   Othello  /  Reversi  player"
data "but to  be enjoyable,  the program was to"
data "be at minimum a good player.  It can beat"
data "me at level 3 but I  often win because of"
data "its shortsighted  search.  At level 4, it"
data "wins most of the time.                   "
data "                                         "
data "The evaluation  function is  very simple:"
data "it just  check  for  3-squares   patterns"
data "around the corners and  add the scores of"
data "all of them  for all the corners.  A high"
data "score is obtained  of a good position but"
data "also on a bad position  of the  opponent."
data "These  patterns are  loaded at start from"
data "DATA statements  and are easily adjusted."
data "It also give points for mobility.        "
data "                                         "
data "At the beginning of the game, the level 3"
data "and up computer opponents will prioritise"
data "mobility.  They  will  often  fall a  lot"
data "behind on the  score but  will  have more"
data "moves to choose from which will also have"
data "the  effect  of  reducing  the   opponent"
data "mobility, a key factor to force bad moves"
data "and then regain disks at the end.        "
data "                                         "
data "If you notice a slight delay when you run"
data "the program, it's because the valid moves"
data "table  is   generated.   There  are  2187"
data "possible states of  a 7-squares-long line"
data "representing  the 7^3  possible positions"
data "next to a  played piece.  The  table will"
data "return the  number  of  pieces  that will"
data "change  color.  It saves  a lot  of  time"
data "compared to having the  position searched"
data "dynamically. There's also tables for each"
data "board position  telling how  many squares"
data "to look  in each  direction.  In essence,"
data "every possible  pieces configuration  for"
data "every  square  in 8  directions  is  pre-"
data "calculated  and put  on arrays.  All this"
data "because the  CMM2 has  a lot  of RAM  for"
data "variables.  I still  only  use 3%  of the"
data "available variable  memory  with  160K of"
data "tables.                                  "
data "                                         "
data "I worked a lot in refining and perfecting"
data "the interface.  I wanted it to be easy to"
data "operate and understand. The interface was"
data "getting priority at the  beginning and it"
data "later paid  off because  testing was made"
data "easier with a feature complete program.  "
data "                                         "
data "So, I hope you enjoy this little game and"
data "learn a very unique  strategy game in the"
data "process. I had as much fun making it as I"
data "had playing it and watching  the autoplay"
data "mode, thinking about  how I could make it"
data "better.                                  "
data "END"
