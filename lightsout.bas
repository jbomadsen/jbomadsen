locate 1,1
& "LightsOut v.1.4
& "jbomadsen 2024
& "Moves: WASD or arrow keys, toggle: space/enter, H: hint next move
& "Win condition: turn off all the squares
& : & : &

board_y = 6 : 'space for the cool header above
bwidth = 5
bheight = 5
bscale = 4
hborder$ = "  "
per_square_space = len(hborder$)

' nocur$ and yescur$ toggle reverse video on cursor:
nocur$ = chr$(27)+"[?25l" : yescur$   = chr$(27)+"[?25h"
def fncursordown$(n%) = chr$(27)+"["+str$(n%)+"B"
def fncursorleft$(n%) = chr$(27)+"["+str$(n%)+"D"

row_width% = (bwidth*(per_square_space+bscale*2))+per_square_space
board_x = (width - row_width%) / 2 -1 : ' center board horizontally
blank_row$ = string$(row_width%, " ") + fncursorleft$(row_width%) + fncursordown$(1)
tile$(0) = "55;55;55"    : ' unlit
tile$(1) = "255;225;100" : '   lit
tile$(2) = "235;15;110" : ' unlit, cursor
tile$(3) = "255;75;135" : '   lit, cursor
tile$(4) = "200;220;255" : ' unlit, hint
tile$(5) = "160;180;255" : '   lit, hint
tile$(6) = "155;155;255" : ' unlit, hint, cursor
tile$(7) = "175;75;255" : '   lit, hint, cursor
for ti = 0 to 7
  tile$(ti) = string$(bscale*2, th_sprintf$("%c[48;2;%sm %c[m",27,tile$(ti),27)) + hborder$
next ti

for redraw_gosub = 1 to 1 step 0
  is_lit% = 0 : 'tracks win condition
  & nocur$ ; : locate board_y, board_x
  for by = 1 to bheight
    row$ = hborder$
    for bx = 1 to bwidth
      is_lit% = is_lit% or board(bx,by)
      cursor = (x=bx and y=by) * 2
      if hint then if bx = path_x%(path_c% -1) then if (by=path_y%(path_c%-1)) then cursor = cursor or 4
      row$ = row$ + tile$(cursor or board(bx,by))
    next bx : ' draw each row (bscale) times:
    row$ = string$(bscale, row$ + fncursorleft$(row_width%) + fncursordown$(1))
    & row$ blank_row$ ;
  next by
  & yescur$ ; :
  if redraw_return then next redraw_return

for toggle_gosub = 1 to 1 step 0
  t_lit% = 0
  if toggle_return then for tx = x-(x>1) to x+(x<bwidth): board(tx,y) = board(tx,y) xor 1: t_lit% = t_lit% or board(tx,y): next tx
  if toggle_return then for ty = y-(y>1) to y+(y<bheight): board(x,ty) = board(x,ty) xor (ty<>y): t_lit% = t_lit% or board(x,ty): next ty
  if toggle_return then next toggle_return

' Need at least required_moves% successive moves that didn't turn off everything,
' to avoid generating too trivial boards:
required_moves% = 3
last$ = ""
for setup = 1 to 4 + int(rnd(10))
  dedup = int(rnd(2)) : ' choose which coordinate to ensure is different from previous:
  x = 1 + (int(rnd(bwidth -dedup) + dedup*x) mod bwidth)
  dedup = dedup xor 1
  y = 1 + (int(rnd(bheight-dedup) + dedup*y) mod bheight)
  path_x%(path_c%) = x : path_y%(path_c%) = y : path_c% = path_c% + 1
  for toggle_return = 1 to 2: if 1 = toggle_return then next toggle_gosub
  if not t_lit% then setup = setup - required_moves%
next setup
required_moves% = path_c% : ' required for us to make it, not necessarily to solve it that is

'Randomize "cursor" start position:
x = 1+int(rnd(bwidth)) : y = 1+int(rnd(bheight))
moves_used = 0
for moves = 1 to 1 step 0
  for redraw_return = 1 to 2: if 1 = redraw_return then next redraw_gosub
  if not is_lit% then & : & : & chr$(27) "[1m" "YOU WIN! You used "str$(moves_used)" moves."
  if not is_lit% then & "The computer needed " str$(required_moves%) " moves."
  if not is_lit% then & "Your score is " th_sprintf$("%.2f",required_moves%/moves_used*100) "% !"chr$(27)"[m" : END

  inp$ = inkey$(1) : masked% = 0 :
  if inp$ = "H" then hint = 1 : next moves
  hint = 0
  if asc(inp$) = 27 then inp$=inkey$(1) : if inp$="[" then inp$=inkey$(1): masked% = asc(inp$) and 63
  dotoggle = inp$=" " or (asc(inp$) = 13)

  cancels_last = (path_x%(path_c%-1) = x) and (path_y%(path_c%-1) = y)
  if dotoggle then path_x%(path_c%) = x: path_y%(path_c%) = y : path_c% = path_c% + 1 - 2*cancels_last
  if dotoggle then for toggle_return = 1 to 2: if 1 = toggle_return then next toggle_gosub
  if dotoggle then moves_used = moves_used + 1 : next moves

  ' WASD to arrows: A->4, D->3, W->1, S->2  :  4 - (vertical: 2)  -(direction: 1)
  if not masked% then masked% = 4 - (asc(inp$) and 16)/8 - ((asc(inp$) and 4)/4)
  ' arrow move: D< C> A^ Bv: 4, 3, 1, 2
  if masked% > 2 then x = x -1 + (masked% and 2) : x = (x -1) mod bwidth + 1
  if masked% <= 2 then y = y - 1 + (masked% and 2) : y = (y - 1) mod bheight + 1
next moves
