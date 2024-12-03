' should move the input bar to be at the left/right margins instead of in the bottom here
& chr$(27)+"[?1049h"+chr$(27)+"[?25l"+chr$(27)+"[?4h" ;
locate 1,1: & "INVADERS.BAS"; : m$="Move=Arrows, Insane=I, Shoot=Space, Quit=Q": locate 1,int((width-len(m$))/2): & m$;: r$="jbomadsen 2024": locate 1,int(width-len(r$)): & r$
for quit = 0 to 1: if quit then & fnscrollregion$(1,height)+ chr$(27)+"[?1047l": end:'return
def fnu$(s$) = chr$(th_sprintf$("%y", s$))
down$ = chr$(27)+"[T": up$ = chr$(27)+"[S"
def fnscrollregion$(top,bottom) = th_sprintf$("%c[%d;%dr",27,top,bottom)
insane_mult = 1

player_pos = width/2
for player = 0 to 0 step 0
  c$ = string$(not player_ret, "x")
  a$ = polkey$(0.0035+speed*insane_mult)
  if asc(a$) = 27 then b$=polkey$(0.01): if b$="[" then c$=polkey$(0.01): if gotesc then next gotesc
  if a$=" " then for sh=height-1 to 2 step -1: locate sh,player_pos:& th_sprintf$("%c[7m %c[m",27,27);: sleep 0.25/(height-4):next sh: for sh=height-1 to 2 step -1:locate sh,player_pos:&" ";:next sh
  if a$="i" then insane_mult = 0
  if a$="q" then next quit
  if player_ret then next player_ret
  for gotesc = 1 to 1 step 0
  movedir = 2*(-(c$="D") + (c$="C"))
  movemult=1 +((th_time - lastmove)< 0.4)*(sgn(movedir)=sgn(lastmovedir))*(1+sqrt(movemult))
  if movedir then lastmove = th_time: lastmovedir = movedir
  player_pos = int(player_pos + movemult*movedir)
  if player_pos < 3 then player_pos = 2
  if player_pos >= width then player_pos=width-1
  if c$ then locate height,(player_pos-1) : & chr$(27)+"[1K|"+c$+"|"+chr$(27)+"[0K" ;
  if player_ret then next player_ret

alien$ = fnu$("1F47E")
def fnins$(i) = th_sprintf$("%c[%d@",27,i) : 'insert chars to the right (doesn't move cursor)
def fndel$(i) = th_sprintf$("%c[%dP",27,i) : 'del chars left of cursor
aliens = int(height/4): aliens = aliens + (aliens=0)
for i = 2 to aliens+2-1
  thisrnd = int(RND(height/4))
  this = int((width-12)/4-thisrnd): this = this + (this=0)
  locate int(i),int((width-this*4)/2) : & string$(this, alien$+"  ")
next i
direction$ = down$
& fnscrollregion$(2,height-1) ;
for z = 1 to val("Inf")
  for i = 1 to height-2-1-aliens
    for x = ((z+i) mod 2) to height-3 step 2
      locate 2+x,3
      if (x) and 1 then & fnins$(2) ;
      if 0 = (x and 1) then & fndel$(2) ;
      speed = ((15/height)/sqrt(sqrt(z*2)))/height
      for player_ret = 1 to 2: if player_ret = 1 then next player
    next x
    & direction$ ;
    speed = 1/(sqrt(sqrt(z))*2+sqrt(z))
    for player_ret = 1 to 2: if player_ret = 1 then next player
  next i
  locate 1,2+len("INVADERS.BAS"): & "round="+str$(z)+" ";
  if direction$ = down$ then direction$ = up$: next z: direction$="finish"
  if direction$ = up$ then direction$ = down$: next z: direction$="finish"
next z
