' SUN demo
  center_y = height/2 : center_x = width/2 : radius = height /5 : radius2 = radius^2
  for iter = 10 to 100 step 10
  for y = 1 to height-1 : o$ = "" :  ycy2 = (y-center_y)^2
    for x = 1 to width :
      dist = (x-center_x)^2/4 + ycy2
      g=iter+(155/(1+(radius2/(1+dist))^1.5))
      if dist <= radius2 then o$=o$+th_sprintf$("%c[48;2;%d;%d;0m ",27,255,g): next x
      if dist -13 <= radius2 then o$ = o$ + th_sprintf$("%c[48;2;%d;%d;%dm ",27,255,int(g*1.1),0,27): next x
      r = int(255*sin(y/center_y)) mod 256
      o$ = o$ + th_sprintf$("%c[48;2;%d;%d;%dm ",27,abs(200-r+(iter-10)*0.5) mod 256,(iter-10)/2,r/2-256/(1+abs(r*sin(360/dist/x))) mod 256,27)
    next x : print o$
  next y : print chr$(27) "[m";
  if iter<> 100 then sleep 1: print chr$(27)+"[1;1H";
  next iter
  return
