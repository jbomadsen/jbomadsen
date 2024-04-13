    5  'undialed.bas: dial every number in a file that you haven't dialed before.
    6  'JBOMADSEN 2024-02-17
    7 goto 400
   10  c(0)=9:c(1)=32:c(2)=35:c(3)=40:c(4)=45:c(5)=48:c(6)=93:c(7)=100:c(8)=109:c(9)=130:c(10)=154
   20  c(11)=158:c(12)=160:c(13)=162:c(14)=197:c(15)=214:c(16)=219:c(17)=221:c(18)=226:c(19)=228
   30  col$=chr$(27)+"[38;5;":blue$=col$+"32m":reset$=chr$(27)+"[0m"
   40  n$=col$+"197m"+chr$(120587):forall$=blue$+chr$(8704):ne$=blue$+chr$(8716):mpl$=chr$(8594)
   50  dial$=chr$(7429)+chr$(618)+chr$(7424)+chr$(671)
   60  diallog$=col$+"35m"+dial$+" /"+chr$(671)+chr$(7439)+chr$(610)
   70  print string$(20," ")+col$+"160m"+forall$+n$+reset$+" : "+diallog$+" "+ne$+" "+n$+reset$+" "+mpl$+" ";
   80  print col$+"93m"+dial$+chr$(10088)+n$+col$+"93m"+chr$(10089)+reset$
   90  th_exec "dial /log|cut -c 15-|uniq", log$
  100  log$=th_sed$(log$, " +[^ ]+\n",chr$(10),"g")
  110  'log$=th_sed$(log$, "(^|\n)[^ ]+ +",chr$(10),"g")
  120  'log$=th_sed$(log$, "[\(\) \r-]+","","g")
  122  off%=2
  130  nexto = instr(log$,chr$(10), off%) : if nexto = -1 then 180
  140  num$=th_sed$(mid$(log$,off%,nexto-off%+1), "[\(\) \r-]+", "","g")
  145 'print num$
  150  off%=nexto+2
  160  nums(num$)=1
  170  goto 130
  180  had=0: nohad=0
  185  if infiles_c% <= 0 then end
  190  fame$ = infiles$(infiles_c%-1) : print infiles_c%,"opening",fame$ : open fame$, as #1
  200  if eof(1) then goto 300
  210  input# 1, num$
  220  if nums(num$) then had=had+1: print chr$(13)+"not dialing", num$,"","",had,nohad;: goto 200
  230  print num$
  240  nohad = nohad + 1
  250  th_exec "dial "+num$
  290  goto 200
  300  print : 'end
  310  close 1
  320  infiles_c% = infiles_c% - 1: goto 185
  400  argvptr = argvptr + 1 : if argv$(argvptr) = "" then 10
  410  th_exec "ls "+argv$(argvptr), ls$
  420  for fi = 1 to th_re(ls$, "[^ \t\r\n]+", 1)
  430    infiles$(infiles_c%) = th_re$(ls$, "[^ \t\r\n]+", fi)
  440    infiles_c% = infiles_c% + 1
  450  next fi
  460  goto 400
