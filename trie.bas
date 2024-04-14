'trie-like data structure for e.g. tab completion
'the data structure is backed by two hash maps, one emulates trie nodes and the other emulate leaves.
'the node map is only relevant for prefix iteration.
'USAGE:
'  O(1)  get a single value: fntrieget(key$)
'  O(1)  membership test: fntriemember(key$)
'  O(1)  upsert: key$="abc": value$="def" : gosub 6000
'        retrieve everything under a prefix (lexicographic order):
'        key$ = "a" : gosub 7000
'        for i = 0 to values_c% - 1: print fntriekey$(values$(i)), " => ", fntrievalue$(values$(i)): next i
'
'jbomadsen 2024

dim trie_n$(1)  : 'prefix => next chars
dim trie_kv$(1) : 'key => value map

' leaves are tuples of K,V where K length is encoded as a wide char:
def fntrieleaf$(k$,v$) = "L" + chr$(th_sprintf$("%y",hex$(len(k$)))) + k$ + v$
def fntriekey$(l$) = mid$(l$, 3, asc(mid$(l$,2,1)))
def fntrievalue$(l$) = mid$(l$, 3 + asc(mid$(l$,2,1)))
def fntrieget$(k$) = fntrievalue$(trie_kv$(k$))
def fntriemember(k$) = "" <> fntriekey$(trie_kv$(k$))

goto 9000

6000 'add: key$ -> value$ -> ()
  trie_leaf$ = fntrieleaf$(key$, value$)
  if trie_kv$(key$) then trie_kv$(key$) = trie_leaf$ : return : 'overwrite, there is already a path to this.
  trie_kv$(key$) = trie_leaf$ : 'add the new value
  ' add the nodes for the prefix path - could do exponential search since we index trie_n$ by full prefix,
  ' unlike implementations backing trie_n$ by pointers instead of hash tables. another day.
  for trie_i = 0 to len(key$)
    trie_prefix$ = mid$(key$, 1, trie_i) : 'first prefix is the empty string "", intentionally, so we can list everything
    trie_ch$ = mid$(key$, 1 + trie_i, 1)
    trie_node$ = trie_n$(trie_prefix$)
    if pos(trie_node$, trie_ch$) then next trie_i : return
    for trie_off% = 1 to len(trie_node$)
      if mid$(trie_node$, trie_off%, 1) > trie_ch$ then trie_n$(trie_prefix$) = MID$(trie_node$, 1, trie_off%-1) + trie_ch$ + MID$(trie_node$, trie_off%): next trie_i: return
    next trie_off%
    trie_n$(trie_prefix$) = trie_node$ + trie_ch$
  next trie_i
  return

7000 'mget: key$ -> values$(values_c%)
  values_c% = 0 : erase values$, trie_dfs
  trie_stack% = 1 : trie_ran% = 0 : trie_keys$(1) = key$
  'reentrant dfs search (GOSUB is emulated to avoid overflowing the call stack)
  for trie_goto_leave_node = 0 to Inf step 0
  if trie_ran% + trie_stack% > 2 then next trie_goto_resume_parent
  if trie_ran% then return : 'erase trie_dfs, trie_keys$: return
  trie_ran% = 1

  for trie_goto_node = 0 to Inf step 0
  if trie_kv$(trie_keys$(trie_stack%)) then values$(values_c%) = trie_kv$(trie_keys$(trie_stack%)) : values_c% = values_c% + 1
  if trie_n$(trie_keys$(trie_stack%)) = "" then next trie_goto_leave_node
  trie_stack% = trie_stack% + 1 : for trie_goto_resume_parent= 0 to Inf step 0 : trie_stack% = trie_stack% -1
  if 1 + trie_dfs(trie_stack%) > len(trie_n$(trie_keys$(trie_stack%))) then next trie_goto_leave_node : 'resume do..while without extra iteration at end
  for trie_dfs(trie_stack%) = 1 + trie_dfs(trie_stack%) to len(trie_n$(trie_keys$(trie_stack%)))
    trie_keys$(trie_stack%+1) = trie_keys$(trie_stack%) + mid$(trie_n$(trie_keys$(trie_stack%)), trie_dfs(trie_stack%) ,1)
    trie_stack% = trie_stack% + 1 : trie_dfs(trie_stack%) = 0
    next trie_goto_node
  next trie_dfs(trie_stack%)
  next trie_goto_leave_node

9000 'tests and examples
print "===== TESTS"
key$ = "ip addr": value$ = "1": gosub 6000
key$ = "ip route" : value$ ="2" : gosub 6000
key$ = "ifconfig" : value$ = "3" : gosub 6000
key$ = "id" : value$ = "4" : gosub 6000
key$ = "whoami" : value$ = "5": gosub 6000
key$ = "sudo whoami" : value$ = "6" : gosub 6000
key$ = "sudo reboot" : value$ = "7" : gosub 6000
print "inserted. now fetching."

print "all keys - count: "; 
key$ = "" : gosub 7000 : print values_c%
for i = 0 to values_c%-1
  print "value found: ", fntriekey$(values$(i)), " => ", fntrievalue$(values$(i))
next i

print "=== TAB COMPLETION TEST (press [tab] twice to complete, further to cycle)"
print "... this rather limited readline clone supports backspace, tab, and nothing else."
print "# ";
buf$ = ""
for test = 0 to Inf step 0
 key$ = inkey$(0)
 skip = 0
 if asc(key$) = 127 then buf$ = left$(buf$,-1) : skip=1 : tabs=0
 if asc(key$) = 9 then skip =1 : tabs=tabs+1: key$=buf$ : gosub 7000 : for i = 0 to values_c% -1 : print fntriekey$(values$(i)) + "  "; : next i : print
 if not skip then buf$ = cycle$ + key$ : tabs = 0
 if tabs < 2 then cycle$ = buf$
 if (tabs > 1) and (values_c% > 1) then cycle$ = fntriekey$(values$(tabs mod values_c%))
 ' color buffer green when it's an exact match of something in the trie:
 pbuf$ = th_sprintf$("%c[4m%s%c[24m%s", 27, left$(cycle$,len(buf$)), 27, right$(cycle$,len(cycle$)-len(buf$)))
 pbuf$ = th_sprintf$("%c[3%dm%s%c[0m", 27, 1 + fntriemember(cycle$), pbuf$, 27)
 print chr$(13) +"# "+ pbuf$ +string$(40-len(cycle$), " ");
next test
print "end"