'visualize binary search tree for given charset, jbomadsen 2024
pattern$ = argV$(1)
if ""=pattern$ then & "usage: bintree.bas"," [a-zA-Z0-9]"
if ""=pattern$ then & "prints the binary search tree of the matching single-char regex"
if ""=pattern$ then pattern$ = "[a-zA-Z0-9]"

' Well sue me, stupid but it works for single chars:
for i = 0 to 255
  if th_re(chr$(i), pattern$, 1) then opts%(opts_c%) = i: opts_c% = opts_c% + 1
next i

q_r(1) = opts_c% - 1  :  stk = 1
for bfs = 0 to 0 step 0
  m = int((q_l(stk) + q_r(stk))/2)
  parent(m) = q_m(stk) : depth = q_depth(stk)
  chars(depth,chars_c%(depth)) = m : chars_c%(depth) = chars_c%(depth) + 1
  s_l = q_l(stk) : q_l(stk) = m + 1
  if (q_l(stk) <= q_r(stk)) then q_m(stk)=m: q_depth(stk)=depth+1: stk = stk + 1
  q_r(stk) = m - 1
  if (s_l <= q_r(stk)) then q_m(stk)=m: q_l(stk)=s_l: q_depth(stk)=depth+1: stk = stk + 1
  stk = stk - 1 : if stk then next bfs

'Walk the sparse tree as measured in the BFS above and draw it:
maxlvl = int(th_sprintf$("%f",log(opts_c%) * (2/log(2)) / 2)) : ' <- floor(log2(opts_c%)), sprintf handles log(8)
for level = 0 to maxlvl
s$ = string$(2*int((2^(maxlvl - level))/2)-1," ")
padmid$ = string$(int(2^(maxlvl + 1 - level)/2)-1, " ")
if level = maxlvl then & chr$(27) "[7m";
for x = 0 to chars_c%(level)-1
  i = chars(level, x)
  missing=((index(parent(i))*2-1) -(i<parent(i)) - (drawn-1))
  if (x > 0) then pipe_p = len(padmid$)>= 1  or (parent(i)<>parent(chars(level,x-1)))
  if (x > 0) then s$=th_sprintf$("%s%c[2m%c%c[22m%s", padmid$, 27, 32 + pipe_p * 9442, 27, padmid$)
  & s$ string$(missing,"  ") chr$(opts%(i)) ; : drawn=drawn+missing+1: index(i) = drawn
next x : & : if drawn < opts_c% then next level
& chr$(27) "[27m" ;
