' Cyberpunk 2077 hacking sim clone
' v.1.1 jbomadsen 2024
' from gradient.bas:

inactive = 235: inactivebg$=chr$(27)+"[48;5;"+str$(inactive)+"m"
active = inactive '3 (inactive: we are using underlines instead now)
inactivenext = inactive ' 247

h$(0)="e6b464":h$(1)="ddac5c":h$(2)="d4a454":h$(3)="cb9c4d":h$(4)="c29345":h$(5)="b98c3d":h$(6)="c29345":h$(7)="cb9c4d":h$(8)="d4a454":h$(9)="ddac5c":h$(10)="e6b464"
h$(11)="e6b464":h$(12)="efbc6c":h$(13)="f7c475":h$(14)="ffcc7d":h$(15)="ffd586":h$(16)="ffdd8e":h$(17)="ffd586":h$(18)="ffcc7d":h$(19)="f7c475":h$(20)="efbc6c"
h$(21)="e6b464":h$(22)="f1a962":h$(23)="e8a15a":h$(24)="df9953":h$(25)="d6914b":h$(26)="cd8944":h$(27)="c4823d":h$(28)="cd8944":h$(29)="d6914b":h$(30)="df9953"
h$(31)="e8a15a":h$(32)="f1a962":h$(33)="f1a962":h$(34)="fab16a":h$(35)="ffba73":h$(36)="ffc27b":h$(37)="ffca84":h$(38)="ffd38d":h$(39)="ffca84":h$(40)="ffc27b"
h$(41)="ffba73":h$(42)="fab16a":h$(43)="f1a962":h$(44)="fa9e64":h$(45)="f1965d":h$(46)="e88e55":h$(47)="df874e":h$(48)="d67f47":h$(49)="cd7740":h$(50)="d67f47"
h$(51)="df874e":h$(52)="e88e55":h$(53)="f1965d":h$(54)="fa9e64":h$(55)="fa9e64":h$(56)="ffa66c":h$(57)="ffaf75":h$(58)="ffb87e":h$(59)="ffc086":h$(60)="ffc98f"
h$(61)="ffc086":h$(62)="ffb87e":h$(63)="ffaf75":h$(64)="ffa66c":h$(65)="fa9e64":h$(66)="ff926a":h$(67)="f68a63":h$(68)="ed835c":h$(69)="e47b55":h$(70)="dc744e"
h$(71)="d36c47":h$(72)="dc744e":h$(73)="e47b55":h$(74)="ed835c":h$(75)="f68a63":h$(76)="ff926a":h$(77)="ff926a":h$(78)="ff9b73":h$(79)="ffa47b":h$(80)="ffac84"
h$(81)="ffb58d":h$(82)="ffbe95":h$(83)="ffb58d":h$(84)="ffac84":h$(85)="ffa47b":h$(86)="ff9b73":h$(87)="ff926a":h$(88)="ff8674":h$(89)="f67f6d":h$(90)="ee7766"
h$(91)="e5705f":h$(92)="dd6859":h$(93)="d46152":h$(94)="dd6859":h$(95)="e5705f":h$(96)="ee7766":h$(97)="f67f6d":h$(98)="ff8674":h$(99)="ff8674":h$(100)="ff8f7d"
h$(101)="ff9885":h$(102)="ffa18e":h$(103)="ffab97":h$(104)="ffb4a0":h$(105)="ffab97":h$(106)="ffa18e":h$(107)="ff9885":h$(108)="ff8f7d":h$(109)="ff8674"
h$(110)="ff7a81":h$(111)="f7737a":h$(112)="ee6b73":h$(113)="e6646d":h$(114)="de5d66":h$(115)="d65660":h$(116)="de5d66":h$(117)="e6646d":h$(118)="ee6b73"
h$(119)="f7737a":h$(120)="ff7a81":h$(121)="ff7a81":h$(122)="ff848a":h$(123)="ff8d93":h$(124)="ff979c":h$(125)="ffa0a5":h$(126)="ffaaae":h$(127)="ffa0a5"
h$(128)="ff979c":h$(129)="ff8d93":h$(130)="ff848a":h$(131)="ff7a81":h$(132)="ff7091":h$(133)="f7698a":h$(134)="ef6183":h$(135)="e75a7d":h$(136)="df5376"
h$(137)="d74b70":h$(138)="df5376":h$(139)="e75a7d":h$(140)="ef6183":h$(141)="f7698a":h$(142)="ff7091":h$(143)="ff7091":h$(144)="ff7a9a":h$(145)="ff84a3"
h$(146)="ff8eac":h$(147)="ff98b5":h$(148)="ffa2bf":h$(149)="ff98b5":h$(150)="ff8eac":h$(151)="ff84a3":h$(152)="ff7a9a":h$(153)="ff7091":h$(154)="ff68a2"
h$(155)="f7619b":h$(156)="ef5994":h$(157)="e7528e":h$(158)="df4a87":h$(159)="d74280":h$(160)="df4a87":h$(161)="e7528e":h$(162)="ef5994":h$(163)="f7619b"
h$(164)="ff68a2":h$(165)="ff68a2":h$(166)="ff72ab":h$(167)="ff7db4":h$(168)="ff87bd":h$(169)="ff91c7":h$(170)="ff9bd0":h$(171)="ff91c7":h$(172)="ff87bd"
h$(173)="ff7db4":h$(174)="ff72ab":h$(175)="ff68a2":h$(176)="fe64b5":h$(177)="f65cae":h$(178)="ee55a7":h$(179)="e64da0":h$(180)="df4599":h$(181)="d73d92"
h$(182)="df4599":h$(183)="e64da0":h$(184)="ee55a7":h$(185)="f65cae":h$(186)="fe64b5":h$(187)="fe64b5":h$(188)="ff6fbe":h$(189)="ff79c7":h$(190)="ff83d1"
h$(191)="ff8eda":h$(192)="ff98e4":h$(193)="ff8eda":h$(194)="ff83d1":h$(195)="ff79c7":h$(196)="ff6fbe":h$(197)="fe64b5":h$(198)="f064c9":h$(199)="e85dc2"
h$(200)="e155bb":h$(201)="d94eb4":h$(202)="d247ad":h$(203)="ca3fa6":h$(204)="d247ad":h$(205)="d94eb4":h$(206)="e155bb":h$(207)="e85dc2":h$(208)="f064c9"
h$(209)="f064c9":h$(210)="fa6fd2":h$(211)="ff79dc":h$(212)="ff83e5":h$(213)="ff8eef":h$(214)="ff98f8":h$(215)="ff8eef":h$(216)="ff83e5":h$(217)="ff79dc"
h$(218)="fa6fd2":h$(219)="f064c9":h$(218)="fa6fd2":h$(219)="f064c9":h%=220
g$(0)="e6f064":g$(1)="dbe65a":g$(2)="d0db50":g$(3)="c6d145":g$(4)="bbc73b":g$(5)="b0bd30":g$(6)="bbc73b":g$(7)="c6d145":g$(8)="d0db50":g$(9)="dbe65a":g$(10)="e6f064"
g$(11)="e6f064":g$(12)="edf76c":g$(13)="f4fd73":g$(14)="fbff7b":g$(15)="ffff82":g$(16)="ffff8a":g$(17)="ffff82":g$(18)="fbff7b":g$(19)="f4fd73":g$(20)="edf76c"
g$(21)="e6f064":g$(22)="cde364":g$(23)="c3d95b":g$(24)="b9d051":g$(25)="afc648":g$(26)="a5bc3f":g$(27)="9cb335":g$(28)="a5bc3f":g$(29)="afc648":g$(30)="b9d051"
g$(31)="c3d95b":g$(32)="cde364":g$(33)="cde364":g$(34)="d5ea6c":g$(35)="dcf174":g$(36)="e4f97c":g$(37)="ebff84":g$(38)="f3ff8c":g$(39)="ebff84":g$(40)="e4f97c"
g$(41)="dcf174":g$(42)="d5ea6c":g$(43)="cde364":g$(44)="b6d664":g$(45)="adcd5b":g$(46)="a4c453":g$(47)="9bbb4a":g$(48)="92b242":g$(49)="89a939":g$(50)="92b242"
g$(51)="9bbb4a":g$(52)="a4c453":g$(53)="adcd5b":g$(54)="b6d664":g$(55)="b6d664":g$(56)="bede6c":g$(57)="c6e574":g$(58)="ceed7d":g$(59)="d6f585":g$(60)="dffd8d"
g$(61)="d6f585":g$(62)="ceed7d":g$(63)="c6e574":g$(64)="bede6c":g$(65)="b6d664":g$(66)="a0c864":g$(67)="98c05c":g$(68)="8fb754":g$(69)="87af4d":g$(70)="7fa745"
g$(71)="779f3d":g$(72)="7fa745":g$(73)="87af4d":g$(74)="8fb754":g$(75)="98c05c":g$(76)="a0c864":g$(77)="a0c864":g$(78)="a8d06c":g$(79)="b1d875":g$(80)="bae17e"
g$(81)="c2e986":g$(82)="cbf18f":g$(83)="c2e986":g$(84)="bae17e":g$(85)="b1d875":g$(86)="a8d06c":g$(87)="a0c864":g$(81)="c2e986":g$(82)="cbf18f":g$(83)="c2e986":g$(84)="bae17e":g$(85)="b1d875":g$(86)="a8d06c":g$(87)="a0c864":g%=88
def fncol$(palette%,s$) = th_sprintf$("%c[38;5;%dm%s%c[m", 27, PALETTE%, s$, 27)
def fnbgcol$(palette%,s$) = th_sprintf$("%c[48;5;%dm%s%c[m", 27, PALETTE%, s$, 27)

1 locate 1,1 : & chr$(27)+"[2J"
erase sym$, solve$, grid%, usedsym, used, grid, chosen, solved
again$=""
stime = timer
maxsym = 7
solves = 3
for i = 0 to maxsym
  sym$(i) = th_sprintf$("%02X", RND(256))
  if usedsym(sym$(i)) then i = i-1: next i
  usedsym(sym$(i)) = 1
next i

maxx = 6: maxy = 6
for x = 1 to maxx
  for y = 1 to maxy
    grid(y,x) = int(RND(maxsym+1))
  next y
next x

def fnboundsx(x) = x >= 1 and x <= maxx
def fnboundsy(y) = y >= 1 and y <= maxy

' Generate the targets by picking a random place and walking in random directions.
' They are generated separately, so there's no guarantee there's one big chain
' that solves everything in < 8 moves. Room for improvement.
20 for s = 1 to solves
  loops = 0: xd = 0 : yd = 0
  y = int(1+RND(maxy))
  x = int(1+RND(maxx))
  solve$(s) = sym$(grid(y,x))
  used(s,y,x) = 1: used(y,x) = used(y,x) + 1
  lastxd = int(rnd(3))-1
  lastyd = (not lastxd)
  for si = 1 to INT(1+s/3+(RND(1.3)^2))
     loops = loops + 1
     if loops>100 then si=si+100: next break: 'avoid painting ourselves into a corner
     if lastxd then xd = 0: yd = int(rnd(3))-1: if yd = 0 or 1=not fnboundsy(y+yd) then si=si-1:next si
     if lastyd then yd = 0: xd = int(rnd(3))-1: if xd = 0 or 1=not fnboundsx(x+xd) then si=si-1:next si
     if (0 < used(s,y+yd,x+xd)) then si = si - 1: next si
     if xd < 0 then xd = xd * (int(rnd(x-1))+1):' & "-XD",xd
     if yd < 0 then yd = yd * (int(rnd(y-1))+1):' & "-YD",yd
     if xd > 0 then xd = xd * (int(rnd(maxx-x))+1):' & "+XD",xd
     if yd > 0 then yd = yd * (int(rnd(maxy-y))+1):' & "+YD",yd
     if (0 < used(s,y+yd, x+xd)) then si=si-1:next si
     y=y+yd: x=x+xd: used(s,y,x) = 1: used(y,x) = used(y,x) + 1
     solve$(s) = solve$(s) + ":" +sym$(grid(y,x))
     lastxd = xd : lastyd = yd
  for break = 0 to 0 step 0: next si
next s

ypos = 1: xpos = 1: vertical = 0: chosen$ = ""

locate 1,1:print chr$(27)+"[2J"+chr$(27)+"[?25l";

for gameloop = 0 to 0 step 0
  gosub 100 ' redraw
  ' design mistake: solved() is maintained inside the redraw routine at 100 :-(
  score=0: solved = 0: for s=1 to solves: solved = solved + solved(s): score=score+solved(s)*((len(solve$(s))+s)^1.5): next s: score=int(score+longestprefix)
  &  "SCORE:       "+fncol$(101,score)
  if solved =solves or len(chosen$) >= 2*8+7 then & xtab$+"Game over!": again$="x": & xtab$+"Play again? (Y/N) "
30 if again$ <>""then again$=polkey$(.10): on again$="y" goto 1: again$=chr$(asc(again$) or 64)
     if again$ = "n" then & th_sprintf$("%c]104;%c",27,7) : end: 'reset palette and quit
     if again$<> "" then : gosub 1000: goto 30
40 k$ = polkey$(0.10)
     if k$ = "" then gosub 1000: goto 40
   nypos = ypos: nxpos = xpos
50 v = 0 : 'keypress valid?  
  if vertical   then if k$ = "w" or k$="s" then nypos = nypos -(k$="w") +(k$="s"): on chosen(nypos,xpos) goto 50: v=1: if fnboundsy(nypos) then ypos=nypos
  if vertical=0 then if k$ = "a" or k$="d" then nxpos = nxpos -(k$="a") +(k$="d"): on chosen(ypos,nxpos) goto 50: v=1: if fnboundsx(nxpos) then xpos=nxpos
  if k$ = " " and (chosen(ypos,xpos) = 0) then v=1: vertical=vertical xor 1: chosen = 1: chosen(ypos,xpos) = 1: chosen$=chosen$+string$(chosen$<>"",":")+sym$(grid(ypos,xpos))
  ' try to find a free position to put the cursor in: (m: we assume maxy>=maxx here so we can do with one loop)
  if chosen then nypos = ypos: nxpos = xpos: for m = cint(maxy) to 1 step -1: for mp=1 to -1 step -2
  if chosen then ny=ypos+m*mp: ny = ny*(not chosen(ny,xpos))*fnboundsy(ny): if ny then nypos=ny
  if chosen then nx=xpos+m*mp: nx = nx*(not chosen(ypos,nx))*fnboundsx(nx): if nx then nxpos=nx
  if chosen then next mp: next m: ypos=ypos+(nypos-ypos)*vertical: xpos=xpos+(nxpos-xpos)*(not vertical): chosen = 0
  if v = 0 then 40 : 'don't redraw on invalid key press
next gameloop

100 'REDRAW subroutine
xoff = int((width-24)/2): xtab$=" "+chr$(27)+"["+str$(xoff-2)+"b"
h=int((height-20-solves)/2):h=h+(h<=1):locate h,xoff: & " Controls: WASD + Space": print
longestprefix = 0
for s = 1 to solves
  ms = th_re(solve$(s),":",1)
  prefix$ = th_re$(chosen$, ".*?("+th_sed$(solve$(s),":","(?::","g")+string$(ms, ")?")+")+$", 2)
  missing$ = mid$(solve$(s), len(prefix$)+1+(prefix$<>""))
  solved(s) = th_re(chosen$, solve$(s))
  if len(prefix$) > longestprefix and solved(s) = 0 then longestprefix = len(prefix$)
  & xtab$ + "       " ;
  if prefix$<>"" and missing$<>"" and solved(s) = 0 then & th_sprintf$("%c[38;5;118m%s%c[m",27,prefix$,27)+":";
  if solved(s) then & chr$(27)+"[38;5;118m" + solve$(s);
  if solved(s)=0 and sym$(grid(ypos,xpos))=left$(missing$,2) then missing$ = mid$(missing$,3): & fncol$(100,sym$(grid(ypos,xpos))) ;
  if solved(s)=0 then & chr$(27)+"[38;5;196m" + missing$ ;
  & chr$(27)+"[m"
next s

& ctl + xtab$ ;
if chosen$ then for i = 1 to int(len(chosen$)/2+(len(chosen$)-1)*(len(chosen$)>2)) step 3: & fncol$(100+(i mod 2),mid$(chosen$,i,3)) ; : next i
& ctl + xtab$+chr$(27)+"[38;5;"+str$(inactive)+"m"+chr$(9604)+chr$(27)+"[23b"+chr$(27)+"[m" + ctl + xtab$ ;
for y = 1 to maxy
  for x = 1 to maxx
    & inactivebg$ ;
    if     ((ypos=y) and (xpos=x)) then & chr$(8883) ;
    if not ((ypos=y) and (xpos=x)) then & " ";
    if (ypos=y and vertical=0) or (xpos=x and vertical) then & chr$(27)+"[7m"; ' inverse, bold
    if ((vertical=0) and (xpos=x)) or ((vertical=1) and (ypos=y)) then & chr$(27)+"[1m"; ' bold
    if (ypos=y) and (xpos=x) then & chr$(27)+"[21;5;6m" ; ' double underline, blink (cursor)
    & chr$(27)+"[40m" ; 'black background
    if chosen(y,x) then & chr$(27)+"[38;5;"+str$(inactive)+"m"+sym$(grid(y,x)) ;
    if not chosen(y,x) then & fncol$(100+(int((y+(stime mod (x+y+3))) mod 3)),sym$(grid(y,x))) ;
    & inactivebg$ + string$(not ((ypos=y) and (xpos=x))," ")+string$(((ypos=y) and (xpos=x)),chr$(8882))+ chr$(27)+"[m" ;
  next x: print
  if y < maxy then print xtab$+chr$(27)+"[38;5;"+str$(inactive)+"m"+chr$(129840)+chr$(27)+"[23b"+chr$(27)+"[m"
  print xtab$ ;
next y
print chr$(27)+"[30;48;5;"+str$(inactive)+"m"+chr$(9604)+chr$(27)+"[23b"+chr$(27)+"[m"+ctl+xtab$ ;
return

1000 ' cycle color gradients
hp% = hp% + 1.21 ' or 1 for correct gradient, but 1.21 experimentally seems to give funkier results
curh% = abs(h%*(1=((hp% / h%) mod 2))-(hp% mod h%)) : ' cycle 01234543210123, ..., probably a shorter way to accomplish this
curg% = abs(g%*(1=((hp% / g%) mod 2))-(hp% mod g%))
PRINT th_sprintf$("%c]4;%d;#%s%c", 27, 100, h$(curh%), 7)+th_sprintf$("%c]4;%d;#%s%c", 27, 101, h$((curh%+h%/2)mod h%), 7)+th_sprintf$("%c]4;%d;#%s%c", 27, 102, g$(curg%), 7) ;
return
