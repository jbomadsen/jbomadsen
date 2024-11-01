' https://en.wikipedia.org/wiki/Josephus_problem
' use arrows + space to select a number and see if you survive :-)
' jbomadsen 2024
for josephus_status = 0 to 1
if josephus_status then h$ = th_sprintf$(" n=%d k=%d dead=%d c=%d ",points, k, dead,c): locate mid_y, mid_x - int(len(h$)/2): & h$ ;
if josephus_status then for i = 1 to points
if josephus_status then   locate int(mid_y + joseph_r*sin(d2r(360/points*i))), int(mid_x + 2*joseph_r*cos(d2r(360/points*i)))
if josephus_status then   & string$(choice=i,chr$(27)+"[7m")+string$(i=c,chr$(27)+"[5m")+th_sprintf$("%c[3%dm %s %c[m",27,status(i),i,27) ;
if josephus_status then next i
if josephus_status then josephus_status=0: next josephus_ret

rl_nocur$ = chr$(27)+"[?25l" : rl_cur$   = chr$(27)+"[?25h": & rl_nocur$;
mid_x = int(width / 2): mid_y = int(height / 2)
joseph_r = (3+sqr(mid_x)+sqr(mid_y))
points = 3+RND(50): for i = 1 to points: status(i) = 6: next i
k = INT(RND(points/2-1)*2+2)
choice = 1
for josephus_ret = 0 to 1: if not josephus_ret then next josephus_status
  ch$ = inkey$(0): if ch$=" " or asc(ch$)=13 then ch$=" "
  if asc(ch$)=27 then ch$=polkey$(0.1): if ch$="[" then ch$=polkey$(0.1)
  pt = (360/points*choice+360) mod 360
  if ch$<>" " then if ch$<>"A" and ch$<>"B" and ch$<>"D" and ch$<>"C" then josephus_ret=-1: next josephus_ret
  if ch$<>" " then if ch$ = "A" then m2= ((pt<270) and (pt>90)) - ((pt>270) or (pt<90))
  if ch$<>" " then if ch$ = "B" then m2= -((pt<270) and (pt>90)) + ((pt>270) or (pt<90))
  if ch$<>" " then if ch$ = "D" then m2= ((pt<180) - (pt>180))
  if ch$<>" " then if ch$ = "C" then m2= ((pt>180) - (pt<180))
  if ch$<>" " then m=m2: 'if m2 then m=m2: 'ensure we progress in some direction
  if ch$<>" " then choice = (choice+m) mod (points+1): if choice = 0 then choice= (choice + m) mod (points+1)
  if ch$<>" " then josephus_ret=-1: next josephus_ret
dead = 0: ck = k
for c = 1 to 1 step 0: if status(c) = 1 then c=(c mod points) +1: next c
  ck = ck -1: if ck then c=(c mod points)+1: next c
  ck = k: status(c) = 1: dead = dead + 1
  if c = choice then lasted = dead
  if lasted then h$=th_sprintf$("DEAD! Lasted %d turns", lasted-1): locate mid_y+1, mid_x - int(len(h$)/2): & h$ ;
  for josephus_ret = 0 to 1: if not josephus_ret then next josephus_status
  sleep 1
  if (points - dead) > 1 then c=(c mod points)+1: next c
if lasted=0 then h$=th_sprintf$("%c[7mYOU SURVIVE!%c[m",27,27): locate mid_y+1, mid_x - int(len(h$)/2): & h$ ;
& rl_cur$
