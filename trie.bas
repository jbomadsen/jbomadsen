'version 3
' changelog:
'   3: now memoizing MGET lookups, results are renamed to trie_values$, trie_values_c% to prevent ns clashes
'      the readline demo was rewritten to support more keys, allow inline tab completion, and run fewer lines per key press
'trie-like data structure for e.g. tab completion
'the data structure is backed by two hash maps, one emulates trie nodes and the other emulate leaves.
'the node map is only relevant for prefix iteration.
'USAGE:
'  O(1)  get a single value: fntrieget$(key$)
'  O(1)  membership test: fntriemember(key$) -> bool
'  O(1)  do keys exist under prefix$: fntrieprefix(prefix$) -> bool
'  O(1)  upsert: key$="abc": value$="def" : gosub 6000
'O(n+m)  retrieve everything under a prefix (lexicographic order), n=len(key$), m=trie_values_c%
'        key$ = "a" : if not trie_mget_canary%(trie_ns$, key$) then gosub 7000
'        for i = 0 to trie_values_c% - 1: print fntriekey$(trie_values$(i)), " => ", fntrievalue$(trie_values$(i)): next i
'        REM trie_mget_canary%(trie_ns$, key$) = 1 when trie_values$ is cached
'TODO: given key$, find longest matching prefix X% that satisfies predicate fntrieprefix$(left$(key$,X%))
'jbomadsen 2024
dim trie_n$(1)  : 'trie_ns$ , prefix => next chars
dim trie_kv$(1) : 'trie_ns$ , key => value map

' leaves are tuples of K,V where K length is encoded as a wide char:
def fntrieleaf$(k$,v$) = "L" + chr$(th_sprintf$("%y",hex$(len(k$)))) + k$ + v$
def fntriekey$(l$) = mid$(l$, 3, asc(mid$(l$,2,1)))
def fntrievalue$(l$) = mid$(l$, 3 + asc(mid$(l$,2,41)))
def fntrieget$(k$) = fntrievalue$(trie_kv$(trie_ns$, k$))
def fntriemember(k$) = "" <> fntriekey$(trie_kv$(trie_ns$, k$))
def fntrieprefix(k$) = "" <> trie_n$(trie_ns$, k$)
goto 9000

6000 'add: key$ -> value$ -> ()
  'Invalidate mget cache:
  ERASE trie_mget_canary%
  trie_leaf$ = fntrieleaf$(key$, value$)
  if trie_kv$(trie_ns$, key$) then trie_kv$(trie_ns$, key$) = trie_leaf$ : return : 'overwrite, there is already a path to this.
  trie_kv$(trie_ns$, key$) = trie_leaf$ : 'add the new value
  ' add the nodes for the prefix path - could do exponential search since we index trie_n$ by full prefix,
  ' unlike implementations backing trie_n$ by pointers instead of hash tables. another day.
  for trie_i = 0 to len(key$)
    trie_prefix$ = mid$(key$, 1, trie_i) : 'first prefix is the empty string "", intentionally, so we can list everything
    trie_ch$ = mid$(key$, 1 + trie_i, 1)
    trie_node$ = trie_n$(trie_ns$, trie_prefix$)
    if pos(trie_node$, trie_ch$) then next trie_i : return
    for trie_off% = 1 to len(trie_node$)
      if mid$(trie_node$, trie_off%, 1) > trie_ch$ then trie_n$(trie_ns$, trie_prefix$) = MID$(trie_node$, 1, trie_off%-1) + trie_ch$ + MID$(trie_node$, trie_off%): next trie_i: return
    next trie_off%
    trie_n$(trie_ns$, trie_prefix$) = trie_node$ + trie_ch$
  next trie_i
  return

7000 'mget: key$ -> trie_values$(trie_values_c%)
  if trie_mget_canary%(trie_ns$, key$) then return : ' result was memoized
  trie_values_c% = 0 : ERASE trie_values$, trie_dfs, trie_mget_canary%, trie_keys$
  trie_stack% = 1 : trie_ran% = 0 : trie_keys$(1) = key$ : trie_key$ = key$
  trie_mget_canary%(trie_ns$, key$) = 1
  'reentrant dfs search (GOSUB is emulated to avoid overflowing the call stack)
  for trie_goto_leave_node = 0 to 0 step 0
  if trie_ran% + trie_stack% > 2 then next trie_goto_resume_parent
  if trie_ran% then return : 'ERASE trie_dfs, trie_keys$: return
  trie_ran% = 1
  for trie_goto_node = 0 to 0 step 0
  trie_values$(trie_values_c%) = trie_kv$(trie_ns$, trie_keys$(trie_stack%))
  if trie_values$(trie_values_c%) then trie_values_c% = trie_values_c% + 1
  if trie_n$(trie_ns$, trie_key$) = "" then next trie_goto_leave_node
  trie_stack% = trie_stack% + 1 : for trie_goto_resume_parent= 0 to 0 step 0 : trie_stack% = trie_stack% -1
  trie_key$ = trie_keys$(trie_stack%)
  if 1 + trie_dfs(trie_stack%) > len(trie_n$(trie_ns$, trie_key$)) then next trie_goto_leave_node : 'resume do..while without extra iteration at end
  for trie_dfs(trie_stack%) = 1 + trie_dfs(trie_stack%) to len(trie_n$(trie_ns$, trie_key$))
    trie_keys$(trie_stack%+1) = trie_key$ + mid$(trie_n$(trie_ns$, trie_key$), trie_dfs(trie_stack%) ,1)
    IF trie_stop$ <> RIGHT$(trie_keys$(trie_stack%+1),1) THEN trie_stack% = trie_stack% + 1 : trie_dfs(trie_stack%) = 0 : next trie_goto_node
  next trie_dfs(trie_stack%)
  next trie_goto_leave_node

9000 'tests and examples
print "===== TESTS"
key$ = "ip addr": value$ = "1": gosub 6000
key$ = "ip route" : value$ ="2" : gosub 6000
key$ = "ip" : value$ = "3" : gosub 6000
key$ = "ifconfig" : value$ = "4" : gosub 6000
key$ = "whoami" : value$ = "5": gosub 6000
key$ = "sudo whoami" : value$ = "6" : gosub 6000
key$ = "sudo reboot" : value$ = "7" : gosub 6000
key$ = "sudo ls /root" : value$ = "8" : gosub 6000
key$ = "sudo ls" : value$ = "9" : gosub 6000
key$ = "sudo" : value$ = "10" : gosub 6000
print "inserted. now fetching."

print "all keys - count: "; 
key$ = "" : trie_stop$ = "" : gosub 7000 : print trie_values_c%
for i = 0 to trie_values_c%-1
  print "value found: ", fntriekey$(trie_values$(i)), " => ", fntrievalue$(trie_values$(i))
next i
print "keys before space: " ;
key$ = "" : trie_stop$ = " " : gosub 7000 : print trie_values_c%
for i = 0 to trie_values_c%-1
  print "", fntriekey$(trie_values$(i)), " => ", fntrievalue$(trie_values$(i))
next i


print "=== TAB COMPLETION TEST (press [tab] twice to complete, further to cycle)"
print "... single [tab] completes when there's only one option."
print "... this rather limited readline clone supports backspace, tab,"
print "... left, right, ctrl-left, ctrl-right, meta-backspace, ctrl-w, meta-d, ctrl-k"
print "... ctrl-y, meta-y, ctrl-u, Home, End, ctrl-a, ctrl-e, meta-\, and nothing else."
print "# ";

' Reset internal state callbacks, this is necessary if you want to use READLINE recursively.
' TODO document which internal state needs saving and write an example of reentrant use.
readline_tick_return = 0 : readline_reset = 0

' Example of a TICK callback that runs every readline_poll interval (currently hardcoded to 0.3 seconds) TODO
for readline_tick_callback = 1 to 1 step 0
if readline_tick_return then & chr$(27) "7" ;:locate 1,width-20:& th_time;: & chr$(27) "8";: next readline_tick_return

'Example of hook for completed lines:
for readline_submission = 1 to 1 step 0
if readline_reset then print "SUBMISSION:"+rl_cycle$ : next readline_reset

def fnhilight$(s$) = chr$(27)+"[7m" + s$ + chr$(27)+"[27m"
def fnhilightat$(s$,p%) = left$(s$,p%-1) + fnhilight$(mid$(s$,p%,1)) + mid$(s$,p%+1)
def fnrl_wordf$(s$) = th_re$(mid$(s$, 1 + rl_cur%),"^( *[^ ]+)", 2)
def fnrl_wordb$(s$) = th_re$(left$(s$,rl_cur%), "(^| )([^ ]* *$)", 3)
rl_nocur$ = chr$(27)+"[?25l" : rl_cur$   = chr$(27)+"[?25h" : rl_k$ = chr$(27)+"[K"
print rl_nocur$;

for readline_reset = 1 to 1 step 0
rl_cycle$ = ""
rl_buf$ = "" : rl_cur% = 0 : ' 0 is after 0th char, before 1st (in a 1-indexed string)
ch$ = "" : ch_asc = 0

for readline_handle_esc = 0 to Inf step 0
ch2$ = "": ch3$="": ch4$ = "" : ch5$ = "" : ch6$ = ""
if 27 = ch_asc then ch2$ = inkey$(0) : if asc(ch2$) = 91 then ch3$ = inkey$(0)
if ch3$ = "D" then if rl_cur% > 0 then rl_cur% = rl_cur% - 1 : if readline_loop then next readline_loop: 'left arrow
if ch3$ = "C" then if rl_cur% < rl_cyclelen% then rl_cur% = rl_cur% + 1 : if readline_loop then next readline_loop: 'right arrow
if ch2$="\" then rl_buf$=th_sed$(left$(rl_cycle$,rl_cur%)," +$","") :rl_cycle$=rl_buf$+th_sed$(mid$(rl_cycle$,1+rl_cur%),"^ +",""):rl_cur%=len(rl_buf$): rl_buf$=rl_cycle$: next readline_loop : ' meta-\, collapse whitespace
'meta-backspace: delete word left (like ctrl-w but no yanking), could probably collapse the two:
if 127 = asc(ch2$) then rl_buf$=left$(rl_cycle$, rl_cur%-len(fnrl_wordb$(rl_cycle$)))+mid$(rl_cycle$,1+rl_cur%): rl_cur%=rl_cur%-rl_cyclelen%+len(rl_buf$): rl_cycle$=rl_buf$: next readline_loop
if ch3$ = ";" then print "unknown modifier pressed"
if ch2$ = "y" then rl_kill_c% : rl_yank_len%=len(rl_kill$(rl_yank%)) : if (mid$(rl_cycle$,1+rl_cur%,rl_yank_len%) = rl_kill$(rl_yank%)) then rl_yank% = (rl_yank% -1) mod (1+rl_kill_c%+(""<>rl_kill$(19))*(19-rl_kill_c%)) : rl_buf$ = left$(rl_cycle$, rl_cur%) + rl_kill$(rl_yank%) + mid$(rl_cycle$, 1+rl_cur%+rl_yank_len%): rl_cycle$=rl_buf$
if (ch2$ = "y") and readline_loop then next readline_loop : ' this RETURN also handles when meta-yank is invalid
'meta-d: kill word forwards (TODO should it set buf=cycle as it does below?)
if ch2$ = "d" then rl_kill_c% = (rl_kill_c% +1)mod 20: rl_kill$(rl_kill_c%) = fnrl_wordf$(rl_cycle$) : rl_buf$=mid$(rl_cycle$,1,rl_cur%)+mid$(rl_cycle$,1+len(rl_kill$(rl_kill_c%))+rl_cur%): rl_cycle$=rl_buf$:if readline_loop then next readline_loop : ' meta-D
if ch3$ = "1" then if ";" =inkey$(0) then ch5$=inkey$(0): ch6$=inkey$(1)
if ch3$ = "3" then ch4$ = inkey$(0)
if ch6$ = "D" then rl_cur% = rl_cur% - len(fnrl_wordb$(rl_cycle$)) : if readline_loop then next readline_loop : 'ctrl-left jump word
if ch6$ = "C" then rl_cur% = rl_cur% + len(fnrl_wordf$(rl_cycle$)) : if readline_loop then next readline_loop : 'ctrl-right jump word
' Home can also be [1~  [7~  [OH
if ch3$ = "H" then rl_cur% = 0 : if readline_loop then next readline_loop : ' Home
'delete is 27 [ 3 ~
if (ch3$ = "3") and (ch4$ = "~") then if rl_cur% < rl_cyclelen% then rl_buf$ = left$(rl_cycle$,rl_cur%)+mid$(rl_cycle$,2+rl_cur%): rl_cycle$ = rl_buf$ : if readline_loop then next readline_loop
if ch3$ = "5" then ch4$ = inkey$(0) : if readline_loop then next readline_loop: 'page-up
if ch3$ = "6" then ch4$ = inkey$(0) : 'page-down
' End can also be [4~  [8~ [OF
if ch3$ = "F" then rl_cur% = rl_cyclelen% : if readline_loop then next readline_loop ' End
if ch3$ = "A" then print "up arrow" : 'print chr$(27)+"[S";
if ch3$ = "B" then print "down arrow" : 'print chr$(27)+"[D";
readline_poll = 0.001
for readline_loop = 1 to 1 step 0

 rl_buflen% = len(rl_buf$)
 rl_cyclelen% = len(rl_cycle$)
 'rl_buf is the prefix of rl_cycle$ that the user themselves keyed in
 rl_typed$     = left$(rl_cycle$, rl_buflen%)
 rl_completed$ = right$(rl_cycle$, rl_cyclelen% - rl_buflen%)+" "
 if rl_cur% < rl_buflen% then rl_typed$ = fnhilightat$(rl_typed$,1+rl_cur%)
 if rl_cur% >= rl_buflen% then rl_completed$ = fnhilightat$(rl_completed$, 1+rl_cur%-rl_buflen%)
 ' underline the part that the user typed:
 rl_pbuf$ = th_sprintf$("%c[4m%s%c[24m%s", 27, rl_typed$, 27, rl_completed$)
 ' color buffer green when it's an exact match of something in the trie, red if not:
 rl_pbuf$ = th_sprintf$("%c[3%dm%s%c[0m", 27, 1 + fntriemember(rl_cycle$), rl_pbuf$, 27)
 print chr$(13) +"# "+ rl_pbuf$ + rl_k$;
 'print chr$(27)+"[S";
 for readline_tick = 0 to 0 step 0
 ' this is where we would insert our timer logic
 ch$ = polkey$(readline_poll) : readline_poll = .3
 for readline_tick_return = 1 to 2 : if (1=readline_tick_return) and readline_tick_callback then next readline_tick_callback
 if ch$ = "" then next readline_tick : ' wait for next char
 ch_asc = asc(ch$)
 skip = (ch_asc < 32) or (ch_asc = 127)
 'literal character add to buffer:
 if not skip then rl_buf$ = left$(rl_cycle$,rl_cur%) + ch$ + mid$(rl_cycle$,1+rl_cur%): rl_cycle$=rl_buf$: rl_tabs = 0 : rl_cur% = rl_cur% + 1 : next readline_loop
 if ch_asc <> 9 then rl_tabs = 0
 '127 => backspace:
 if ch_asc = 127 then if rl_cur% then rl_buf$ = left$(rl_buf$,rl_cur%-1)+mid$(rl_buf$,rl_cur%+1) : rl_cycle$=rl_buf$: rl_cur% = rl_cur%-1:next readline_loop
 'below here everything is ctrl char handling:
 if (ch_asc = 13) then next readline_submission
 ' 1 => ctrl-a
 if readline_not_ctrl_char and (ch_asc = 9) then next readline_not_ctrl_char
 ' above we fast-track tab key presses, except for first loop iteration.
 ' ord('u')-ord('a')+1
 if  1 = ch_asc then rl_cur% = 0 : next readline_loop : 'ctrl-A
 if  5 = ch_asc then rl_cur% = len(rl_cycle$) : next readline_loop : 'ctrl-E
 ' ctrl-k: kill after cursor:
 if 11 = ch_asc then rl_kill_c% = (rl_kill_c%  + 1) mod 20: rl_kill$(rl_kill_c%) = mid$(rl_cycle$, 1+rl_cur%) : rl_cycle$=mid$(rl_cycle$, 1, rl_cur%): rl_buf$=left$(rl_buf$,len(rl_cycle$)): next readline_loop
 ' ctrl-u: kill until cursor:
 if 21 = ch_asc then rl_kill_c% = (rl_kill_c%  + 1) mod 20: rl_kill$(rl_kill_c%) = left$(rl_cycle$, rl_cur%) : rl_cycle$=mid$(rl_cycle$,1+rl_cur%):rl_buf$=mid$(rl_cycle$,1,rl_cur%):rl_cur%=0:next readline_loop
 ' ctrl-w: kill word:
 if 23 = ch_asc then rl_kill_c% = (rl_kill_c%  + 1) mod 20: rl_kill$(rl_kill_c%) = fnrl_wordb$(rl_cycle$): rl_buf$ = mid$(rl_cycle$,1,rl_cur%-len(rl_kill$(rl_kill_c%))) + mid$(rl_cycle$, 1+rl_cur%): rl_cycle$=rl_buf$ : rl_cur% = rl_cur% - len(rl_kill$(rl_kill_c%)): next readline_loop
 ' ctrl-y: yank from kill ring:
 if 25 = ch_asc then rl_buf$ = left$(rl_cycle$, rl_cur%) + rl_kill$(rl_kill_c%) + mid$(rl_cycle$, 1+rl_cur%): rl_cycle$ = rl_buf$ : rl_yank% = rl_kill_c% : next readline_loop
 ' 26 => ctrl-z
 if 27 = ch_asc then next readline_handle_esc
 if 127 = ch_asc then next readline_loop : 'backspace on leftmost cursor pos
 for readline_not_ctrl_char = 1 to 1 step 0
 if ch_asc <> 9 then PRINT "unhandled ctrl:",ch_asc, "ch:"+ch$ : next readline_loop
 ' Below here -- tab handling. { ch_asc = 9 }
 trie_stop$ = " "
 ' Tab-completing something that isn't a prefix is futile:
 key$ = left$(rl_buf$, rl_cur%)
 if not fntrieprefix(key$) then next readline_loop : ' trie_values_c% >= 1 if we continue:
 gosub 7000 : ' trie mget
 rl_tabs = rl_tabs + 1

 ' If there's only one possible option and rl_tabs=1:
 rl_tkey$ = fntriekey$(trie_values$(0))
 if (trie_values_c% = 1) and (rl_tabs=1) then rl_buf$ = rl_tkey$ +mid$(rl_buf$,1+rl_cur%) : rl_cur% = len(rl_tkey$): rl_cycle$=rl_buf$: next readline_loop
 '   When tab is pressed a second time, check if we can complete another limb after space/trie_stop$:
 if (trie_values_c% = 1) and (rl_tabs=2) and fntrieprefix(rl_tkey$+trie_stop$) then rl_tabs=0: rl_buf$ =rl_tkey$+trie_stop$+mid$(rl_buf$,1+rl_cur%): rl_cur% =len(rl_tkey$)+1: rl_cycle$=rl_buf$: next readline_loop
 ' tab is a no-op for rl_tabs>=3 with one completion value:
 if (trie_values_c% = 1) then next readline_loop

 ' First tab for multi values, printing options:
 if rl_tabs = 1 then for i = 0 to trie_values_c% -1 : print fntriekey$(trie_values$(i)) + "  "; : next i : print rl_k$ : next readline_loop

 ' we preserve text in rl_cycle$ to the right of the previous option (if any); { rl_tabs >= 2}
 ' that way "nets[tab] | grep hello" can complete to "netstat | grep hello" etc:
 rl_cycle$ = right$(rl_cycle$, len(rl_cycle$) - rl_cur%*(rl_tabs=2) - (rl_tabs>2)*len(fntriekey$(trie_values$((rl_tabs-1) mod trie_values_c%))))
 rl_cycle$ = fntriekey$(trie_values$(rl_tabs mod trie_values_c%)) + rl_cycle$

next readline_loop
print "end"
