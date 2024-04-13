   05 ' CHIP-8 disassembler based on https://en.wikipedia.org/wiki/CHIP-8#Opcode_table
   06 ' the binary file reading lines 40-130 is broken because 'cat' rewrites newlines to \r\n.
   07 ' JBOMADSEN 2024-02-17
   10  if argv$(1)="" then argv$(1) = "c8test.c8"
   20  goto 150
   30  'STOP
   40  'read binary file
   50  th_exec "rand", rand_out$ : rand_out$ = left$(rand_out$,8)
   60  rand_tmpfile$ = "c8." + rand_out$ + ".txt"
   70  th_exec "cp "+rand_filename$+" "+rand_tmpfile$, ret$
   80  if pos(ret$, "not found") then stop
   90  th_exec "cat "+rand_tmpfile$, ret$
  100  th_exec "rm "+rand_tmpfile$ : 'scratch rand_tmpfile$
  110  ret$=left$(ret$,-2) : 'strip 0d 0a that cat helpfully adds
  120  'print len(ret$), asc(right$(ret$,1))
  130  return : stop
  131  hashmask%=2^47-1 : '>>> for x in range(0,256): r=((float(2**(47))-1)*33+x); print(x,r,(r-1==r) or (r+1 ==r))

  132  hash%=5381 : hashoff%=1 : 'floating-point friendly djb2: inp$ -> hash%
  133 if len(inp$)=0 then return
  134 for hashoff%=1 to len(inp$)
  136 hash% = (hash%*33 + asc(mid$(inp$,hashoff%,1))) and hashmask%
  138 next hashoff%
  139 return : stop: 'if hashch%<>0 then hash%=(hash%*33 + hashch%) and hashmask%: hashoff%=hashoff%+1 : goto 133
  140 inp=(a2% * 256) or b%
  141 'colored by hash: inp -> ret$
  142 c(0)=9:c(1)=32:c(2)=35:c(3)=40:c(4)=45:c(5)=48:c(6)=93:c(7)=100:c(8)=109:c(9)=130:c(10)=154
  143 c(11)=158:c(12)=160:c(13)=162:c(14)=197:c(15)=214:c(16)=219:c(17)=221:c(18)=226:c(19)=228
  144  inp$ = th_sprintf$("%03x",inp)
  145  gosub 131
  146  ret$ = th_sprintf$("%c[48;5;%dm%s%c[0m",27,c(hash% mod 20),inp$,27)
  147  return
  150  dim ops$(1) : ' ops$() was for the assembler that this didn't turn into yet.
  160  ops$("clear")="00e0"
  170  ops$("return")="00ee"
  180  ops$("goto")="1NNN"
  190  ops$("gosub")="2NNN"
  200  ops$("skipif")="3NNN"
  210  ops$("skipifn")="4NNN"
  220  ops$("")="x"
  221  def fnV$(iV%) = th_sprintf$("%c[48;5;%dmV%01x%c[0m",27, c(iV%), iV%, 27)
  230  rand_filename$ = argv$(1) : gosub 50 : buf$=ret$
  240  for i=1 to len(buf$) step 2
  250    o$ = mid$(buf$,i,2)
  251    a% = asc(o$) : b% = asc(right$(o$,1))
  252    a1% = int(a% / 16) : a2% = a% and 15
  253    b1% = int(b% / 16) : b2% = b% and 15
  254    a2b% = (a2% * 256) or b%
  260    ohex$ = th_sprintf$("%02x%02x", a%, b%)
  262    inp=512+i-1 : gosub 141 : 'color -> ret$
  263    print th_sprintf$("%s "+chr$(9), ret$);
  264    inp=a2b% : gosub 141: a2b$=ret$
  265    'print ohex$
  270    on 1+a1% gosub 1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1092, 1100, 1110, 1120, 1130, 1140, 1150
  280  next i
  290 END
  1000 '
  1001 if a%=0 and b%=224 then print th_sprintf$("clear_display; %s", ohex$): RETURN : ' 0x00 E0
  1002 if a%=0 and b%=238 then print th_sprintf$("RETURN      ; %s", ohex$): RETURN : '0x00 EE
  1003 if a%=0 and b%=253 then print th_sprintf$("END         ; %s", ohex$): RETURN : '0x00 FD
  1004 if a%=0 and b%=0   then print th_sprintf$("NOP         ; %s", ohex$): RETURN : '0x00 00
  1005 print th_sprintf$("CALL 0x%s  ; %s", a2b$, ohex$): RETURN : ' 0x0_ __
  1006 stop
  1010 print th_sprintf$("GOTO 0x%s  ; %s ", a2b$, ohex$): RETURN : '0x1_ __
  1020 print th_sprintf$("gosub 0x%s ; %s", a2b$, ohex$): RETURN : '0x2_ __
  1030 print th_sprintf$("if (%s == 0x%02x) ; %s", fnV$(a2%), b%, ohex$): RETURN : '0x3_ __
  1040 print th_sprintf$("if (V%01x != 0x%02x) ; %s", a2%, b%, ohex$): RETURN : '0x4_ __
  1050 if b2% <> 0 then stop
  1051 print th_sprintf$("if (V%01x == V%01x) ; %s", a2%, b1%, ohex$): return : '0x5_ __
  1060 print th_sprintf$("%s := 0x%02x  ; %s", fnV$(a2%), b%, ohex$): return : '0x6_ __
  1070 print th_sprintf$("%s += 0x%02x  ; %s", fnV$(a2%), b%, ohex$): return : '0x7_ __
  1080 ' 0x8_ __
  1081 on 1 + b2% goto 1082, 1083, 1084, 1085, 1086, 1087, 1088 : if b2%=14 then 1090
  1082 print th_sprintf$("%s := %s   ; %s", fnV$(a2%), fnV$(b1%), ohex$): return : '0x8_ _0
  1083 print th_sprintf$("%s |= %s   ; %s", fnV$(a2%), fnV$(b1%), ohex$): return : '0x8_ _1
  1084 print th_sprintf$("%s ^= %s   ; %s", fnV$(a2%), fnV$(b1%), ohex$): return : '0x8_ _2
  1085 print th_sprintf$("V%01x ^= V%01x   ; %s", a2%, b1%, ohex$): return : '0x8_ _3
  1086 print th_sprintf$("%s += %s   ; %s", fnV$(a2%), fnV$(b1%), ohex$): return : '0x8_ _4
  1087 print th_sprintf$("V%01x -= V%01x   ; %s", a2%, b1%, ohex$): return : '0x8_ _5
  1088 print th_sprintf$("V%01x >>= 1   ; %s", a2%, ohex$): return : '0x8_ _6
  1089 print th_sprintf$("V%01x := V%01x - V%01x ; %s", a2%, b1%, a1%, ohex$): return : '0x8_ _7
  1090 print th_sprintf$("V%01x <<= V%01x  ; %s", a2%, b1%, ohex$): return : '0x8_ _E
  1091 stop
  1092 if b2% <> 0 then stop
  1093 print th_sprintf$("if (%s != %s); %s", fnV$(a2%), fnV$(b1%), ohex$): return : '0x9_ __
  1100 gosub 140:print th_sprintf$("I := 0x%s  ; %s",ret$, ohex$) :return : '0xA_ __
  1110 gosub 140:print th_sprintf$("GOTO V0 + 0x%s; %s", ret$, ohex$): return : '0xB_ __
  1120 print th_sprintf$("%s = RND() & 0x%02x ; %s", fnV$(a2%), b%, ohex$): return : '0xC_ __
  1130 print th_sprintf$("draw(x:%s, y:%s, 0x%01x); %s", fnV$(a2%), fnV$(b1%), b2%, ohex$): return : '0xD_ __
  1140 ' 0xE_ __
  1145  if b%=161 then print th_sprintf$("if key() != V%01x; %s", fnV$(a2%), ohex$): return : '0xE_ A1
  1146  if b%=158 then print th_sprintf$("if key() == %s; %s", fnV$(a2%), ohex$): return : '0xE_ 9E
  1147  print a1%, a2%, b1%, b2% : stop
  1150 '0xFXXX
  1151 if b% =  7 then print th_sprintf$("V%01x := get_delay(); %s", a2%, ohex$):return:'0xF_ 07
  1152 if b% = 10 then print th_sprintf$("V%01x := get_key(); %s", a2%, ohex$):return:'0xF_ 0A
  1153 if b% = 21 then print th_sprintf$("delay(%s); %s", fnV$(a2%), ohex$):return:'0xF_ 15
  1154 if b% = 24 then print th_sprintf$("sound(%s); %s", fnV$(a2%), ohex$):return:'0xF_ 18
  1155 if b% = 30 then print th_sprintf$("I += %s", fnV$(a2%), ohex$):return:'0xF_ 1E
  1156 if b% = 41 then print th_sprintf$("I := sprite_addr[%s]", fnV$(a2%), ohex$):return:'0xF_29
  1160 if b% = 51 then print th_sprintf$("set_BCD(V%1x), *(I):=BCD(3), *(I+1):=BCD(2), *(I+2) :=BCD(1); %s", a% and 15, ohex$): return '0xF_33
  1162 if b% = 85 then print th_sprintf$("reg_dump(V%01x, &I); %s", a2%, ohex$): return:'0xF_ 55
  1170 if b% = 101 then print th_sprintf$("reg_load(V%01x, &I); %s", a2%, ohex$):return:'0xF_ 65
  1180 if b% = 41 then print th_sprintf$("I := sprite_addr[V%01x]; %s", a2%, ohex$):return:' 0xF_ 29
  print th_sprintf$("; %s", ohex$): return : '0xF
  
