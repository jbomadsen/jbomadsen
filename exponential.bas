' benchmark of linear and exponential search (with `EOF`) to find the line count
' of a file.
' Linear search at line 145
' Exponential search at line 250
' jbomadsen 2024

' linear: 9998   bsearch:9998    27            bsearch_time: 0.0349068641662598 linear_time:31.6094369888306
10 maxlines=100 : ' 1000 is painfully slow

20 open "db.db", as #1
25 ' start by writing a big file
30 for x = 1 to maxlines : print# 1,x : next x
40 close #1

45 ' then iteratively truncate it to simulate having written
46 ' a file with maxlines-1-i lines
47 ' (writing new files takes too long)
50 for i=1 to maxlines-1
60 open "db.db", as #1
70 PRINT #1,maxlines-i;END
80 close 1
90 mkfilend=th_time

95 ' reopen the file and do binary search for the ending: 
100 open "db.db", as #1
110 searchfile=th_time
120 FD=1 : gosub 250
122 print# 1,999
125 read #1,probe; x$ : print x$,probe
130 searchfileend=th_time
140 close 1

145 ' do the linear search from the examples:
150 OPEN "db.db", AS #1
160 searchlinear=th_time
165 linearlines=0
170 IF EOF(1) THEN GOTO 200
180 INPUT# 1, DUMP$ : linearlines=linearlines+1
190 goTO 170
200 searchlinearend=th_time : CLOSE #1

205 ' print results:
210 print "linear: "+str$(linearlines),"bsearch:"+str$(last(0)),debugheight,"bsearch_time: "+str$(searchfileend-searchfile), " linear_time:"+str$(searchlinearend-searchlinear)
215 ' the line counts are hopefully equivalent:
220 if linearlines<>last(0) then print linearlines,lastgood: stop
230 next i

240 end 

250 probe=2 : dim last(2) : last(0)=0 : last(-1)=2^52-1 : '(0) is last good line , (-1) is last bad (initialized to max int)
260 read #1, probe : 'read record at index (probe) from file #1, but not into any variables. this is faster than input#.
270 last(eof(1)) = probe : 'store the probed offset as either good or bad, depending on whether we hit EOF or not.
275 x = probe*2 : 'Exponential search for EOF
280 y=last(0) + int((last(-1)-last(0))/2) : 'halfway between last known good and last EOF (binary search)
290 probe=y xor ((x xor y) and -(x < y)) : ' probe = min(x,y); picks binary search when probe overshoots
295 if probe > last(0) goto 260 : ' probe>good implies (bad-good)/2>=1 implies EOF may lie beyond good+1; keep probing.
299 return 'when EOF is at mult+1, return probe.
