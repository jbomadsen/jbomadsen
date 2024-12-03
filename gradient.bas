' gradient.bas v 1.1
' CHANGELOG:
' - 1.1: add -pulsate
'
' https://github.com/ThomasDickey/xterm-snapshots/blob/master/vttests/palettes.pl
'
' sRGB -> XYZ -> LAB -> LCH -> transform -> LAB -> XYZ -> sRGB
' this has some useful code examples:
' https://www.w3.org/TR/css-color-4/#color-conversion-code

def fnrep$(ch$,w%) = string$(w%>=1, th_sprintf$("%s%c[%sb", ch$, 27, int(w%-1)))

PI = 3.141592653589793115997963468544185161590576171875
pi180 = pi/180
pi2 = pi * 2
fivediv12 = 5 /12
labxyzCBRT_EPSILON = 6.0 / 29.0
labxyzKAPPA = (29^3) / (3^3)
xyzlabEPSILON  = (6^3) / (29^3)
xyzlabKAPPA    = labxyzKAPPA

def fnfinv(v) = ((v>labxyzCBRT_EPSILON)*(v^3)) + ((v<=labxyzCBRT_EPSILON)*((v * 116 - 16) / labxyzKAPPA))

for i = 1 to argc% : if argv$(i)="-pulsate" then pulsate=1: for n = i to argc%: argv$(n)=argv$(n+1): next n: argc%=argc%-1
next
if argv$(1) = "steps" then 7000
if argv$(1) <> "demo" and argv$(1)<>"prof" then & "usage: gradient [steps | demo | prof]": end

srgb(0) = 0
srgb(1) = 255
srgb(2) = 0
'srgb(0) = 1
'srgb(1) = 0
'srgb(2) = 0
'print srgb(0), srgb(1), srgb(2)

  gosub 100 ' srgb->xyz
    gosub 200 ' xyz->lab
      gosub 300
'& srgb(0), srgb(1), srgb(2), lch(2)

lumenbump = -5
contrastbump = 0
stored(0) = lch(0): stored(1)=lch(1): stored(2)=lch(2)
winc = 360 / width : 'two pages for full rainbow
hinc = 360 / height
for yyi = 1 to height + ((argv$(1)="demo")*50*height)
lch(0)=stored(0): lch(1)=stored(1): lch(2)=stored(2)
for xxi = 1 to width
      t(0) = t(0) + 1
      gosub 400
      'if xxi = 1 then & "LAB:", lab(0), lab(1), lab(2)
    gosub 500 'lab->xyz
    'if xxi = 1 then & "XYZ", xyz(0), xyz(1), xyz(2)
  gosub 600   'xyz->srgb
  'if xxi = 1 then & "SRGB", srgb(0), srgb(1), srgb(2)
  lch(2) = lch(2) + winc : REM when printing one char per color
  rem lch(2) = lch(2) + hinc  : REM when printing one line per color
  ' one line per color:
  'l$ = l$ + chr$(27)+th_sprintf$("[48;2;%d;%d;%dm",srgb(0),srgb(1),srgb(2))+fnrep$(" ",width)
  ' one char per color:
  l$ = l$ + th_sprintf$("%c[48;2;%d;%d;%dm ",27,srgb(0),srgb(1),srgb(2))
  '& l$ ; : l$=""
next xxi
& l$ ; : l$=""
& chr$(27)+"[m" : '& ""
'& srgb(0),srgb(1),srgb(2), lch(2)
stored(0) = stored(0) + lumenbump : ' fade luminosity to black
stored(1) = stored(1) + contrastbump: 'bump contrast
if stored(0) < 0 or stored(0)>=100 then lumenbump = lumenbump * -1
if stored(1) < 0 or stored(1)>150 then contrastbump = contrastbump * -1
if (yyi mod height) =0 then contrastbump = contrastbump+ lumenbump
'stored(2) = stored(2)+(360/height)
next yyi
& t(0), t(1), t(2), t(3)
& t(0), t(1)/t(0), t(2)/t(0), t(3)/t(0)

'100 sRGB->XYZ
'200 XYZ->LAB
'300 LAB->LCH
'400 LCH->LAB
'500 LAB->XYZ
'600 XYZ->sRGB
'6000 atan2(y,x)
end
100 'sRGB -> XYZ
'fn xyz_from_srgb(srgb: [u8; 3]) -> [f32; 3] {
   for i = 0 to 2
     if srgb(i) <= 10 then rgb(i) = srgb(i) / 3294.6
     if srgb(i) >  10 then rgb(i) = ((srgb(i) + 14.025) / 269.025) ^ (2.4)
   next i
   xyz(0) = rgb(0)*0.4124108464885388   + rgb(1)*0.3575845678529519 + rgb(2)*0.18045380393360833
   xyz(1) = rgb(0)*0.21264934272065283  + rgb(1)*0.7151691357059038 + rgb(2)*0.07218152157344333
   xyz(2) = rgb(0)*0.019331758429150258 + rgb(1)*0.11919485595098397 + rgb(2)*0.9503900340503373
   return
200 'XYZ -> LAB
'fn lab_from_xyz(xyz: [f32; 3]) -> [f32; 3] {
    xyzlab(0) = 0.9504492182750991
    xyzlab(1) = 1
    xyzlab(2) = 1.0889166484304715
    for i = 0 to 2
        v = xyz(i) / xyzlab(i)
        if v >  xyzlabEPSILON then f(i) = v ^ (1/3)
        if v <= xyzlabEPSILON then f(i) = (v * xyzlabKAPPA +16)/116
    next i
    lab(0) = 116 * f(1) - 16
    lab(1) = 500 * (f(0)-f(1))
    lab(2) = 200 * (f(1)-f(2))
    return

300 'LAB -> LCH ' Lab_to_LCH(Lab)
   atan2y = lab(2)
   atan2x = lab(1)
   gosub 6000 ' Math.atan2(lab(2),lab(1))   & "atan2 of ",lab(2),lab(1),"=>",ret
   lch(0) = lab(0) ' L = L
   lch(1) = sqrt((lab(1)^2) + (lab(2)^2)) ' C = sqrt(a^2 + b^2)
   lch(2) = r2d(ret)
   ' TODO if we manipulated hue in radians instead of degrees we could skip the overhead of converting here
   return
400 'LCH -> LAB ' LCH_to_Lab(LCH)
   s = th_time
   lab(0) = lch(0)
   lab(1) = lch(1) * cos(d2r(lch(2))): ' a = C cos(H) , TODO radians or degrees here?
   lab(2) = lch(1) * sin(d2r(lch(2))): ' a = C cos(H) 
   e = th_time
   t(1) = t(1) + e-s
   return
500 'LAB -> XYZ
   'fn xyz_from_lab(lab: [f32; 3]) -> [f32; 3] {
    s = th_time
    fy = (lab(0) + 16) / 116
    fx = (lab(1) / 500) + fy
    fz = fy - (lab(2) / 200)

    xyz(0) = fnfinv(fx) * 0.9504492182750991
    if lab(0) > 8 then xyz(1) = fy^(3)
    if lab(0) <=8 then xyz(1) = lab(0) / labxyzKAPPA
    xyz(2) = fnfinv(fz) * 1.0889166484304715
    e = th_time
    t(2) = t(2) + e-s
    return
600 'XYZ->sRGB    fn srgb_from_xyz(xyz: [f32; 3]) -> [u8; 3] {
    rem // Gamma compression from linear [0, 1] value to 8-bit integer.
    s = th_time
    rgb(0) = xyz(0)* 3.240812398895283    - xyz(1)*1.5373084456298136  - xyz(2)*0.4985865229069666
    rgb(1) = xyz(0)*-0.9692430170086407   + xyz(1)*1.8759663029085742  + xyz(2)*0.04155503085668564
    rgb(2) = xyz(0)* 0.055638398436112804 - xyz(1)*0.20400746093241362 + xyz(2)*1.0571295702861434
    for i = 0 to 2
      if rgb(i) <= 0.00313066844250060782371 then v = cint(3294.6 * rgb(i))
      if rgb(i) >  0.00313066844250060782371 then v = cint(269.025 * (rgb(i)^fivediv12) - 14.025)
      srgb(i) = v * (v>0)
      if v > 255 then srgb(i) = 255
    next i
    e = th_time
    t(3) = t(3) + e-s
    return
6000 ' atan2(y,x) -> ret in radians
    on (atan2x>0) + (atan2x<=0)*(atan2y>0)*2 + 3*((atan2x<=0)*(atan2y<=0)) goto  6002,6003,6004
    stop
6002 ret = atn(atan2y/atan2x): ret = ret + (ret<0)*pi2: return
6003 ret =   pi/2  - atn(atan2x/atan2y): ret = ret + (ret<0)*pi2: return
6004 ret = -(pi/2) - atn(atan2x/(atan2y+(atan2y=0)*(2^(-50))))
     ret = ret + (ret<0)*pi2: return

7000 ' argv$(1)=STEPS 
erase gradient$
gradientc% = 0
if argc% <> 9 then & "usage: gradient steps STEPS  redA greenA blueA  redB greenB blueB [-pulsate]"
if argc% <> 9 then & "ex: steps 10   255 0 0   0 100 255 -pulsate": end
howmany = int(argv$(2)) -1 : if howmany < 1 then & "at least 2 steps": end
for coli = 3 to 0 step -3
  for i = 0 to 2:  srgb(i) = int(argv$(3+i+coli)):  next i
  gosub 100: gosub 200: gosub 300
    lch(coli,0) = lch(0):   lch(coli,1) = lch(1):   lch(coli,2) = lch(2)
  gosub 400: gosub 500: gosub 600
next coli
for ci = 0 to howmany
  for i = 0 to 1: lch(i) = lch(0,i) + (lch(3,i)-lch(0,i)) / howmany * ci: next i
  ' the hue is a circle, so we can pick two paths (around the circle); we try to pick the
  ' shortest path (try, not sure I got this logic right):
  diff = lch(3,2) - lch(0,2): if abs(diff) >  180 then diff = ((( diff +180) mod 360))-180
  lch(2) = lch(0,2) + diff / howmany * ci
  gosub 400: gosub 500: gosub 600 : 'lch -> lab -> xyz -> srgb
  rgb$=th_sprintf$("%d;%d;%d",srgb(0),srgb(1),srgb(2))
  rgbh$=th_sprintf$("%02x%02x%02x", srgb(0),srgb(1),srgb(2))
  & th_sprintf$("STEP %3d:  %12s   %s   %s",ci,rgb$, rgbh$) ;
  ' calculate some contrast color by flipping hue 180 degrees, toning down chromacity, and flipping luminosity:
  lch(2) = (lch(2) - 180) mod 360: lch(1)=(lch(1) /2 ) mod 100: lch(0) = (lch(0)-50) mod 100
  gosub 400: gosub 500: gosub 600
  invrgb$=th_sprintf$("%d;%d;%d",srgb(0),srgb(1),srgb(2))
  & th_sprintf$("%c[48;2;%sm  %c[38;2;%smstep %3d  %c[m",27,rgb$,27,invrgb$,ci, 27) ; 
  if pulsate then gosub 8000
  & ""
next ci

if gradientc% = 0 then end

' now start the light show we precomputed in 8000 PULSATE
print
o$ = "": bl = 10
for g = 0 to gradientc%-1
  x$ = th_sprintf$(":h$(%d)=%c%s%c", g, 34, gradienthex$(g), 34)
  if len(x$)+len(o$) > width-4 then print : o$ = ""
  print th_sprintf$("%c[38;2;%sm%s%c[m", 27, gradient$(g), mid$(x$,1+(o$="")), 27) ;
  o$ = o$ + x$
next g: print o$+":h%="+str$(gradientc%)
print "PALETTE% = 100: REM change palette of PALETTE% (ie PALETTE%=100)"
print th_sprintf$("PRINT th_sprintf$(%c%%c[38;5;%%dm%%s%%c[m%c, 27, PALETTE%%, %cHello, world!%c, 27)",34, 34,34, 34)
print "FOR i = 0 TO h% - 1"
print th_sprintf$("  PRINT th_sprintf$(%c%%c]4;%%d;#%%s%%c%c, 27, PALETTE%%, h$(i), 7) ; ",34,34)
PRINT "  SLEEP 0.2"
PRINT "  NEXT I"
PRINT "PRINT "+th_sprintf$("th_sprintf$(%c%%c]104;%%d%%c%c, 27, PALETTE%%, 7) ; ' reset palette",34,34)
print
print th_sprintf$("%c[48;5;100m%s%c[m",27,string$(width," "),27)
print
print th_sprintf$("%c[38;5;100m%s",27)+"Well here we are: some very quick, very cyberpunky chromed-up fox jumped over the high-tech, low-life dog"
print "So now we can all look at this glittering text stuff, I guess"
print th_sprintf$("%c[m",27)
print
idir = 1
for r = 0 to gradientc% * 50
 for i = 0 to gradientc%
   print th_sprintf$("%c]4;%d;#%s%c",27,100,gradienthex$(i),7);
   sleep 0.15
 next i
 for i = gradientc% to 0 step -1
   print th_sprintf$("%c]4;%d;#%s%c",27,100,gradienthex$(i),7);
   sleep 0.15
 next i  
next r
end

8000 ' pulsate
pulsteps = 5
for i = 0 to 2
  srgb(i) = int(th_re$(rgb$, "(?<=^|;)\d++",1+i))
  o(i) = srgb(i)
next i
gosub 100: gosub 200: gosub 300
'& lch(0), lch(1), lch(2)
pfrom(0)=lch(0): pfrom(1)=lch(1): pfrom(2)=lch(2)
pto(-3) = 0: pto(-2) = lch(1) : pto(-1) = 0 : ' black
pto(0)=lch(0):pto(1)=lch(1):pto(2)=lch(2)
pto(3) = 150:pto(4) = lch(1)*.8 : pto(5) = 180: ' white
pmfst = -3 ' -3 or 0, -3 dims first
pmoff = -(pmfst/3) + 1
for pm = pmfst to 0 step 3
  if pm=0 then & th_sprintf$("%c[38;2;%d;%d;%dmO%c[m",27, o(0),o(1),o(2), 27);
  pulsescale = 25
  if pmoff = 2 and pm = -3 then pulsescale = 50
  for pp = 0 to pulsteps
    if pm = 0 then for i = 0 to 1: lch(i) = pto(i+pm) + (pto(i+3+pm)-pto(i+pm)) / pulsescale * (pp): next i
    if pm = -3 then  for i = 0 to 1: lch(i) = pto(i+pm) + (pto(i+3+pm)-pto(i+pm)) / pulsescale * ((pulsescale-pp)-pp): next i
    lch(2) = pfrom(2)
    'diff = pto(2+3*pm) - pfrom(2): if abs(diff) >  180 then diff = (((( diff +180) mod 360))-180) mod 360
    'oz = lch(2)
    ' lch(2) = pfrom(2) + diff / pulsescale * pp
    gosub 400: gosub 500: gosub 600 : 'lch -> lab -> xyz -> srgb
    grgb$ = th_sprintf$("%d;%d;%d", srgb(0), srgb(1), srgb(2))
    grgbh$ = th_sprintf$("%02x%02x%02x", srgb(0), srgb(1), srgb(2))
    & th_sprintf$("%c[38;2;%smX%c[m",27, grgb$, 27) ;
    gradient$(gradientc%+pp) = grgb$
    gradient$(gradientc% + pmoff*pulsteps -pp)=grgb$
    gradienthex$(gradientc%+pp) = grgbh$
    gradienthex$(gradientc% + (pmoff*pulsteps) -pp) = grgbh$
  next pp: gradientc% = gradientc% + (pmoff * pulsteps) + 1
next pm
return
