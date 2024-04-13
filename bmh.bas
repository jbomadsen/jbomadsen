'Boyer-Moore-Horspool string search
' User defines:
'  - bmh_p$   : string to search for
'  - def fnbmh_peek(idx) -> ascii value at idx
'  - bmh_off% : start at this index
'  - bmh_top% : stop at this index
'jbomadsen 2024

10 bmh_p$ = "564"
30 bmh_test$ = "123546abc78906564564xkls" : bmh_top% = len(bmh_test$) : ' set top length
40 def fnbmh_peek(bmh_peek_idx%) = asc(mid$(bmh_test$, bmh_peek_idx% + 1, 1))
45 gosub 2000 : 'execute the search
46 print "found? ", bmh_i% : 'stop
60 if bmh_i% = -1 then print "NOT FOUND :-(": END
70 print "search done, found at:", bmh_i%
90 END

1000 ' pre-process. slightly different than usual BMH setup stage: our table uses negative indices.
1010 bmh_m% = len(bmh_p$) : bmh_k% = bmh_m%-1 : bmh_minus_k% = -bmh_k%
1020 for bmh_i% = 0 to 255 : bmh_dic%(bmh_i%) = bmh_k% : next bmh_i%
1030 for bmh_i% = 0 to bmh_m% - 2 : 'all but last char of pattern bmh_p$
1040   bmh_p%(-bmh_i%) = asc(mid$(bmh_p$, 1+bmh_i%, 1))
1050   bmh_dic%( bmh_p%(-bmh_i%) ) = bmh_m% -1 - bmh_i% -1  : 'NEXT will bump this (implicit STEP 1), so "0" means "advance one character", hence last -1
1060 next bmh_i% : bmh_p%(-bmh_i%) = asc(mid$(bmh_p$, 1+bmh_i%, 1)) : 'entry for the last character too
1070 return


2000 'search loop
2010 gosub 1000 : 'pre-process
2020 for bmh_i% = bmh_off% to bmh_top% - bmh_m%
2030   for bmh_kn% = bmh_minus_k% to 0: bmh_cur = fnbmh_peek(bmh_i%-bmh_kn%) : on bmh_p%(bmh_kn%) <> bmh_cur goto 2040 : next bmh_kn% : bmh_off% = 0 : return
2040   bmh_i% = bmh_i% + bmh_dic%( bmh_cur ) : next bmh_i%
2050  bmh_i% = -1: bhm_off% = 0 : return
