1 ' https://deramp.com/downloads/mostek/AID-80F/manuals/ANSI_BASIC_Version_5.3_Mar1980.pdf
csi$ = chr$(27)+"["
bccolor$ = csi$+"48;5;18;38;5;222m"
bccolor$ = csi$+"48;5;18;38;5;222m"
'ESC[38;5;58;48;5;193m
'c: ESC[38;5;198;48;5;0m
'/dump                                                                           c: ESC[38;5;173;48;5;52m

'print csi$+"38;5;90;48;5;111m"
'print csi$+"48;5;96;38;5;111m"
'print csi$+"48;5;18;38;5;222m"
'for arithmetic we can use the FORTRAN I algorithm:
'  ^    -> )^(
'  +, - -> )))+((( , )))-(((
'  *, / -> ))*((, ))/((
'  begin, before each ( -> ((((
'  end, before each ) -> ))))
' note that this is not a problem for the - a ^ 2 example producing (-a)^(2) because that's
' how dartmouth basic does it :D

print bccolor$; : cls
quot$ = chr$(34)

print "ByteCodeBASIC (c) 2024"

type_int%       = asc("%") : type2str$(type_int%) = "int"
type_str%       = asc("$") : type2str$(type_str%) = "str"
type_push%      = asc("(") : type2str$(type_push%) = "("
type_pop%       = asc(")") : type2str$(type_pop%)  = ")"
type_opcode%    = asc("!") : type2str$(type_opcode%) = "op"
type_ptr%       = asc("&") : type2str$(type_ptr%) = "ptr"

def fntlv$(fnpascal_typ%, fnpascal_arg$) = chr$(fnpascal_typ%) + chr$(len(fnpascal_arg$)) + fnpascal_arg$
def fnlen%(fnlen_tape$, fnlen_off%) = asc(mid$(fnlen_tape$, fnlen_off% + 1, 1))
def fntyp%(fntyp_tape$, fntyp_off%) = asc(mid$(fntyp_tape$, fntyp_off%    , 1))
def fnval$(fnval_tape$, fnval_off%) = mid$(fnval_tape$, fnval_off%+2, fnlen%(fnval_tape$, fnval_off%))
def fnid$(fnid_tape$, fnid_off%) = mid$(fnid_tape$, fnid_off%, 2) + fnval$(fnid_tape$, fnid_off%)
def fn_next_off%(fn_next_off_tape$, fn_next_off_off%) = fn_next_off_off% + fnlen%(fn_next_off_tape$, fn_next_off_off%)

dim op2str$(1)
str2op$("DATA")  = "DATA" : op2str$("DATA") = "DATA"
str2op$("READ")  = "READ" : op2str$("READ") = "READ" : opmin%("READ") = 1
str2op$("PRINT") = "&"
str2op$("&") = "&" : op2str$("&") = "PRINT"
str2op$("+") = "+"
str2op$("SLEEP") = "SLEEP" : op2str$("SLEEP") = "SLEEP" : opmin%("SLEEP") = 1 : opmax%("SLEEP") = 1
str2op$("=") = "="
str2op$("GOTO") = ":"  : op2str$(":") = "GOTO" : opmin%(":") = 1 : opmax%(":") = 1
str2op$("GOSUB") = "|" : op2str$("|") = "GOSUB" : opmin%("|") = 1 : opmax%("|") = 1
str2op$("RETURN") = ";"
str2op$("END")    = "END"
str2op$("-")      = "-"
str2op$("*")      = "*" : op2str$("*") = "*"
str2op$("/")      = "/"
str2op$("^")      = "^"
str2op$("XOR")    = "XOR"
str2op$("IF")     = "IF"
str2op$("THEN")   = "THEN"
str2op$("TRON")   = "TRON"
str2op$("TROFF")  = "TROFF"
str2op$("SIN")    = "SIN" : op2str$("SIN") = "SIN" : opmax%("SIN") = 1
str2op$("COS")    = "COS" : op2str$("COS") = "COS" : opmax%("COS") = 1
str2op$("ATN")    = "ATN" : op2str$("ATN") = "ATN" : opmax%("ATN") = 2 :' TODO
str2op$("ABS")    = "ABS" : op2str$("ABS") = "ABS" : opmax%("ABS") = 1
str2op$("BIN$")   = "BIN$": op2str$("BIN$") = "BIN$" : opmax%("BIN$") = 1
str2op$("UPS$")   = "UPS$": op2str$("UPS$")= "UPS$" : opmax%("UPS$") = 1
str2op$("NOT")    = "NOT" : op2str$("NOT") = "NOT"
str2op$("AND")    = "AND"
str2op$("OR")     = "OR" : op2str$("OR") = "OR"
str2op$("==")     = "=="
str2op$("<")      = "<"  : op2str$("<") = "<"
str2op$(">")      = ">"  : op2str$(">") = ">"
str2op$("MOD")    = "%"  : op2str$("%") = "MOD"
str2op$("VAL")    = "VAL": op2str$("VAL") = "VAL"   : opmax%("VAL")  = 1
str2op$("INT")    = "INT": op2str$("INT") = "INT"   : opmax%("INT")  = 1
str2op$("CINT")   = "CINT":op2str$("CINT") = "CINT" : opmax%("CINT") = 1
str2op$("CSNG")   = "CSNG":op2str$("CSNG") = "CSNG" : opmax%("CSNG") = 1
str2op$("LEN")    = "LEN" :op2str$("LEN") = "LEN"   : opmax%("LEN") = 1
str2op$("MID$")   = "MID$":op2str$("MID$") = "MID$" : opmax%("MID$") = 3
op2str$("+")    = "+"
op2str$("=")    = ":="
op2str$("#")    = "LINE"
op2str$("SWAP") = "SWAP"
op2str$("END")  = "END"
op2str$(";")    = "RETURN"
op2str$("-")    = "-"
op2str$("/")    = "/"
op2str$("^")    = "POW"
op2str$("XOR")  = "XOR"
op2str$("IF")   = "IF"
op2str$("THEN") = "THEN"
op2str$("TRON") = "TRON"
op2str$("TROFF")= "TROFF"
op2str$("CHR$") = "CHR$"
op2str$("ASC")  = "ASC"
op2str$("~-")   = "~-" : 'unary minus
op2str$("ABS")  = "ABS" : 'absolute int value
op2str$("BIN$") = "BIN$" : 'int to binary string
op2str$("==")   = "==" : 'boolean comparison
op2str$("<>")   = "<>" : 'boolean comparison
op2str$("%")    = "MOD"
op2str$(">")    = ">"
op2str$("<=")   = "<="
op2str$(">=")   = ">="
op2str$("AND")  = "AND"
op2str$("EQV")  = "EQV"
op2str$("IMP")  = "IMP"
op2off("&")     = 1 : 'PRINT
op2off("+")     = 2 : 'addition
op2off("=")     = 3 : 'ASSIGNMENT
op2off("#")     = 4 : 'LABEL/LINE
op2off(":")     = 5 : 'GOTO
op2off("SLEEP") = 6
op2off("|")     = 7 : 'GOSUB
op2off(";")     = 8 : 'RETURN
op2off("SWAP")  = 9 : 'SWAP
op2off("END")   = 10 : 'END
op2off("-")     = 11 : 'subtraction
op2off("*")     = 12 : 'multiply
op2off("/")     = 13 : 'divide
op2off("^")     = 14 : 'exponentiate
op2off("XOR")   = 15 : 'xor
op2off("IF")    = 16 : '(IF) .. THEN
op2off("THEN")  = 17 : 'IF .. (THEN)
op2off("TRON")  = 18 : 'TRace ON
op2off("TROFF") = 19 : 'TRace OFF
op2off("SIN")   = 20 : 'SINus (degrees)
op2off("COS")   = 21 : 'CO-Sinus (degrees)
op2off("ATN")   = 22 : 'ArcTaNgent
op2off("LEN")   = 23 : 'string length
op2off("CHR$")  = 24 : 'make str singleton from ascii value
op2off("MID$")  = 25 : 'MID$ section of string
op2off("ASC")   = 26 : 'ASCii index of first char in str operand, or 0
op2off("~-")    = 27 : 'unary minus
op2off("ABS")   = 28 : 'absolute int value
op2off("BIN$")  = 29 : 'int to binary string
op2off("==")    = 30 : 'boolean comparison, evaluates to 1 when both operands are equal, 0 otherwise
op2off("<>")    = 31 : 'boolean comparison, evaluates to 0 when both operands are equal, 1 otherwise
op2off("<")     = 32
op2off(">")     = 33
op2off("NOT")   = 34 : 'boolean not
op2off("<=")    = 35 :' less than or equal, int and str
op2off(">=")    = 36 : 'greater than or equal, int and str
op2off("IMP")   = 37 :' IMP
op2off("EQV")   = 38 : 'EQV, bitwise equality, a ^ ~b
op2off("OR")    = 39 : 'bitwise OR
op2off("AND")   = 40 : 'bitwise AND
op2off("MOD")   = 41 : 'integer modulo, remainder
op2off("VAL")   = 42 : 'float of int/string
op2off("INT")   = 43 : 'truncated floored float of int/string
op2off("CINT")  = 44 : 'rounded up/down version of INT
op2off("CSNG")  = 45 : 'CSNG
op2off("READ")  = 46 : 'read from DATA
'ranked from strongest (high) to lowest (1) binding:
precedence%("~-")   = 10
precedence%("ABS")  = 9 : precedence%("ASC")  = 9 : precedence%("ATN") = 9 : precedence%("BIN$") = 9
precedence%("CHR$") = 9 : precedence%("CINT") = 9 : precedence%("COS") = 9 : precedence%("CSNG") = 9
precedence%("D2R")  = 9 : precedence%("DATE$") = 9 : precedence%("EOF") = 9 : precedence%("EXP")  = 9
precedence%("HEX$") = 9 : precedence%("INKEY$")= 9 : precedence%("INPUT$") = 9: precedence%("INSTR") = 9
precedence%("INT") = 9 : precedence%("ITM") = 9 : precedence%("LEFT$") = 9: precedence%("LEN") = 9
precedence%("LG") = 9
precedence%("MID$") = 9
precedence%("SIN") = 9 : precedence%("VAL") = 9
precedence%("^") = 7
precedence%("*") = 6
precedence%("/") = 6
precedence%("%") = 5.5 :' MOD
precedence%("+") = 5
precedence%("-") = 5
precedence%("==")  = 4.8 : ' boolean equal =, TODO precedence test
precedence%("<>")  = 4.7 : ' boolean inequal <>, TODO precedence test
precedence%("<")   = 4.6
precedence%(">")   = 4.5
precedence%("<=")  = 4.4
precedence%("=<")  = precedence%("<=")
precedence%("=>")  = 4.3
precedence%(">=")  = precedence%("=>")
precedence%("NOT") = 4.2
precedence%("AND") = 4.1
precedence%("OR")  = 4
precedence%("XOR") = 3
precedence%("EQV") = 2.5
precedence%("IMP") = 2
precedence%("(") = 1
'inp$="1 a$ = "+chr$(34)+"Hello"+chr$(34)+"+"+chr$(34)+"World"+chr$(34)+" : PRINT a$ : SLEEP 2: x = x + 1 : IF x = 5 THEN END : 10 GOTO 1"
'if user$ = "jbomadsen" then inp$ = "TRON :10 thirty%=(2+2*5*3-1-1): goto thirty%: 20 print "+quot$+"skip"+quot$+": 30   PRINT 1,(3+  4/2-0-(0-1)-1-3)  , ((thirty%) ),(  ((4  )) ) : PRINT "+chr$(34)+"Hello World!"+chr$(34)+"  :  'from BcBASIC"
'if user$ = "jbomadsen" then inp$="print 1,(2),((3)),(((4)))" : '1234
 'if user$ = "jbomadsen" then inp$ = "10 goto 30: 20 print 2 : goto 40 : 30 print 1 : goto 20: 40 print 3"
'if user$ = "jbomadsen" then inp$ = "tron: 10 gosub 300: 20 print "+quot$+"gosub works"+quot$+": END : 300 print "+quot$+"sub."+quot$+":return"
'if user$ = "jbomadsen" then inp$ = "  ab%   = 666 : goto ab% : print "+quot$+"skip"+quot$+":  666 print 1,ab%,3"
'if user$ = "jbomadsen" then inp$ = "ab$ = (10-200)-3000: print ab$" : '-3190 : 'TODO actually should be a type error due to $
'if user$ = "jbomadsen" then inp$ = "ab% = (10 - 30 + 40) : print ab%" ' 20
'if user$ = "jbomadsen" then inp$ = "tron:ab% = (10 -30 +40)" : '20
'if user$ = "jbomadsen" then inp$ = "print (((1-20)+300)-4000)" : '-3719
'if user$ = "jbomadsen" then inp$ = "print ((1-3)-4-100)" : ' -106
'if user$ = "jbomadsen" then inp$ = "print (3 + 4 * 2 / (1 -5) ^ 2 ^ 3)" : '3, roundabout
'if user$ = "jbomadsen" then inp$ = "print (3+ 4 * 2 / (1 -5) ^ 2 ^ 3)" : ' 3.002
'if user$ = "jbomadsen" then inp$ = "print 3+ 4 * 2 / (1 -5) ^ 2 ^ 3" : '3.002
'if user$ = "jbomadsen" then inp$ = "ab$ = 123 : print (ab$ + 2 - 100 -3 -20)" : '2
'if user$ = "jbomadsen" then inp$ = "ab$=((2+123) XOR (123+1))" : '1
'if user$ = "jbomadsen" then inp$ = "ab$=(2 + 123 XOR 123 + 1)" : '1
'if user$ = "jbomadsen" then inp$ = "ab$=(2 + (123 XOR 123) + 1)" : '3
'if user$ = "jbomadsen" then inp$ = "print (1-20*300-4000)" : '-9999: 1 - (20*300) - 4000
'if user$ = "jbomadsen" then inp$ = "print 1-20*300-4000" : '-9999
'if user$ = "jbomadsen" then inp$ = "print 1-20*300-4000,10-20" : '-9999 -10
'if user$ = "jbomadsen" then inp$ = "print ((1-20*300)-4000)" : '-9999
'if user$ = "jbomadsen" then inp$ = "print (1-(20*300-4000))" : '-1999
'if user$ = "jbomadsen" then inp$ = "print (1-20*(300-4000))"  : '74001
'if user$ = "jbomadsen" then inp$ = "print 1^10*2" : '2
'if user$ = "jbomadsen" then inp$ = "print 1^10^4 xOr 2^3" : '9
'if user$ = "jbomadsen" then inp$ = "print (1^10 * 2)" : '2
'if user$ = "jbomadsen" then inp$ = "a$=10^2 : b$=2^2 : c$ = b$ - a$^3 : print c$" '-999996
'if user$ = "jbomadsen" then inp$ = "a$=10^2 : b$=2^2 : c$ = b$ - a$^3 : d$=123 :print c$"
'if user$ = "jbomadsen" then inp$ = "print "+quot$+"abc"+quot$+",1,2,"+quot$+"def"+quot$ : 'abc 1 2 def
'if user$ = "jbomadsen" then inp$ = "a%=10^2 : b%=2^2 : c% = b% - a%^3 : d%=0: print c%,"+quot$+"x"+quot$+",c%" : '-999996       x               -999996
'if user$ = "jbomadsen" then inp$ = "a$ = 2 3"   : 'fail
'if user$ = "jbomadsen" then inp$ = "a$ = (2 3 )" : 'should fail
'if user$ = "jbomadsen" then inp$ = "tron:a$ = (2* 3 ^) 4" : 'should fail
'if user$ = "jbomadsen" then inp$ = "tron:a$ = 2 + 3" : 'good, 5
'if user$ = "jbomadsen" then inp$ = "tron:a% = 1 + (2+2) + 2" : '7
'if user$ = "jbomadsen" then inp$ = "a$=b$ 2" :'"if 1 then print 2"
'if user$ = "jbomadsen" then inp$ = "tron:a=10 : b=a*2" : ' b=20
'if user$ = "jbomadsen" then inp$ = "print -1, --5" : 'should be -1, 5
'if user$ = "jbomadsen" then inp$ = "print -(--(3--7))" : 'should be -10
'if user$ = "jbomadsen" then inp$ = "tron:print mid$("+quot$+"hello"+quot$+",asc(chr$(1)),--4)" :'"hell"
'if user$ = "jbomadsen" then inp$ = "tron:print asc("+quot$+"A"+quot$+")^4" : '17850625
'if user$ = "jbomadsen" then inp$ = "tron:print -asc("+quot$+"A"+quot$+")^4": '17850625 because - binds stronger than ^
'if user$ = "jbomadsen" then inp$ = "tron:print abs(2),abs(4-9)" : '2,5
'if user$ = "jbomadsen" then inp$ = "tron:print bin$(123)"
'if user$ = "jbomadsen" then inp$ = "tron: if 1+(0) then print 2: print 3"
'if user$ = "jbomadsen" then inp$ = "tron: if (2-2) then print 2: 666 print 999"
'if user$ = "jbomadsen" then inp$ = "tron: if 0+0 then print 111: 666 print 999"
'if user$ = "jbomadsen" then inp$ = "tron: print (2=3)"
'if user$ = "jbomadsen" then inp$ = "tron: if then print 123: 666 print 999" : 'should fail
'if user$ = "jbomadsen" then inp$ = "print (1 <> 2-1)" : 'should be 0
'if user$ = "jbomadsen" then inp$ = "print (1 xor 1 <> 1 xor 0)" : 'should be 1
'if user$ = "jbomadsen" then inp$ = "print 4**3" : ' 4^3 = 4*4*4 = 64
'if user$ = "jbomadsen" then inp$ = "tron:print 4 < 4" : '0
'if user$ = "jbomadsen" then inp$ = "tron:print 1 < 4" : '1
'if user$ = "jbomadsen" then inp$ = "tron:print 1 + not 0"
'if user$ = "jbomadsen" then inp$ = "tron: print not 0 + (not 0)" : ' 0; not (0 + (not 0))
'if user$ = "jbomadsen" then inp$ = "tron: print not 0 + (0)" : ' 1; not (0 + (0))
'if user$ = "jbomadsen" then inp$ = "tron: print not 0 + 1" : '0; not (0 + 1)
'if user$ = "jbomadsen" then inp$ = "tron: print not 0 + not 0" : '0; not (0 + not 0)
if user$ = "jbomadsen" then inp$ = "print 1 < 2 > 0, 1 > 2 < 0" :' 1,1           (1 <   2) > 0 , 1 >  (2 < 0)
if user$ = "jbomadsen" then inp$ = "tron: print 0 <= 2 > 0, 0 >= 2 < 0" : ' 1,1:  0 <= (2  > 0), 0 >= (2 < 0)
if user$ = "jbomadsen" then inp$ = "tron: print 3 IMP 4" : '-4
if user$ = "jbomadsen" then inp$ = "tron: print 3 EQV 4" : '-8
if user$ = "jbomadsen" then inp$ = "tron: print 1 or 8" : '9
if user$ = "jbomadsen" then inp$ = "tron: print 7 and 11" : '3
if user$ = "jbomadsen" then inp$ = "tron: print 7 mod 3" : '1
if user$ = "jbomadsen" then inp$ = "tron: print csng(-1),csng(0),csng(1),csng("+quot$+"-1"+quot$+")
print "Supported statements: ","X=Y  END  IF..THEN  GOSUB  GOTO  GO TO  PRINT  RETURN  SLEEP  TROFF  TRON"
print "NOT supported statements: CLEAR, CLOSE, CLS, COLOR, DATA, DEF, DIM, FOR, HOME, OPEN, INPUT, ..."
print "Supported functions: ABS(d)  ATN(d)  ASC(s)  BIN$(d)  CHR$(d)  CINT(sd)  COS(d)  CSNG(sd)  INT(sd), LEN(s)  MID$(s,d)  MID$(s,d,d)  SIN(d)  VAL(sd)"
print "NOT supported functions: D2R, EOF, EXP, HEX$, INKEY$, INPUT$, INSTR, ITM, LEFT$, LG, LIN,"
print "                         LN, LOG, LOG10, NINT, NUM, OCT$, PEEK, POLKEY$, POS, R2D, REC, RIGHT$, RND,"
print "                         SGN, SPACE$, SPC$, SQR, SQRT, STR$, STRING$, TAB, TAB$, TAN, TH_*, TIM, TIME$, TIMER, TYP, UPS$"
print "Supported operators: ","+  *  /  -  ** ^  =  <>  <  >  <= >= AND NOT  OR  XOR  IMP  EQV MOD"
if user$ <> "xjbomadsen" and inp$ <> "" then print "Example program: "+ inp$
'if user$ = "jbomadsen" then inp$ = ""
10 if (user$<> "jbomadsen") or (inp$ = "") then input "Type your program: ", inp$
print "INPUT PROGRAM:"
print csi$+"48;5;11;38;5;18m"+inp$ : print bccolor$;
gosub 6000 : ' lex & parse

if bytecode$ <> "" then tape$ = bytecode$

print "bytecode tape len: ", len(tape$)

trace% = 0
dim callstack$(1)
callstack_frame% = 0
dim operands$(1)
tape_scanned = 0
dim offset_next_line%(1)
offset_next_line%(0) = 1
700 'in order for GOTO/GOSUB to work we need to prescan the tape for labels
if trace% or (not tape_scanned) then print string$(width, "*")
operands_c% = 0
actual_line% = 0
for tape_off% = 1 to len(tape$) step 2
  if trace% or (not tape_scanned) then print tape_off%, type2str$(fntyp%(tape$, tape_off%)), fnlen%(tape$, tape_off%), fnval$(tape$, tape_off%)+"/"+op2str$(fnval$(tape$, tape_off%)), operands_c% ;
  if trace% or (not tape_scanned) then for pxi = 1 to pstate_i% : print pstack%(pstate_i%); : next pxi: print
    if fntyp%(tape$, tape_off%) <> type_ptr% then 750
      operands$(operands_c%) = mem$( fnval$(tape$,tape_off%) )
      ' Undefined variables default to 0 or "" depending on their type:
      if (operands$(operands_c%) = "") and (right$(fnval$(tape$, tape_off%),1)="$") then operands$(operands_c%) = fntlv$(type_str%, "")
      if operands$(operands_c%) = "" then operands$(operands_c%) = fntlv$(type_int%, 0)
      operands_c% = operands_c% + 1
750 if fntyp%(tape$, tape_off%) = type_str% then operands$(operands_c%) = fnid$(tape$, tape_off%) : operands_c% = operands_c% + 1
    if fntyp%(tape$, tape_off%) = type_int% then operands$(operands_c%) = fnid$(tape$, tape_off%) : operands_c% = operands_c% + 1
    if not (fntyp%(tape$, tape_off%) = type_opcode%) then 800
    if not op2off(fnval$(tape$, tape_off%)) then print "Unknown operator: "+fnval$(tape$, tape_off%) : stop
    if (operands_c% - opmin%(fnval$(tape$, tape_off%)) - (9=precedence%(fnval$(tape$, tape_off%)))) < 0 then print "Arity underflow:", operands_c%, op2str$(fnval$(tape$, tape_off%))+"/"+str$(opmin%(fnval$(tape$, tape_off%))):stop
    if (opmax%(fnval$(tape$, tape_off%))) and (9<>precedence%(fnval$(tape$, tape_off%))) and (operands_c% - opmax%(fnval$(tape$, tape_off%))) > 0 then print "Arity overflow:", operands_c%, op2str$(fnval$(tape$, tape_off%))+"/"+str$(opmax%(fnval$(tape$, tape_off%))):stop
    if (tape_scanned = 0) and (fnval$(tape$, tape_off%)="#") then gosub 1300 : offset_next_line%(actual_line) = fn_next_off%(tape$, tape_off%)-1 : print "next from",actual_line%,offset_next_line%(actual_line%): actual_line% = actual_line% + 1 :goto 800
    if tape_scanned = 0 then goto 800
    on op2off(fnval$(tape$, tape_off%)) gosub 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 4950, 5000, 5050, 5100, 5150, 5200
800  tape_off% = fn_next_off%(tape$, tape_off%)
next tape_off%
if tape_scanned = 0 then tape_scanned = 1 : goto 700

print "PROGRAM FINISHED."
inp$ = ""
goto 10
end
900 ' dereference pointer
     stop
     return
1000 ' PRINT
1002 if operands_c% = 0 then print: return
     print csi$+"48;5;200;37;1m";
     for p% = 0 to operands_c% -1 'to 0 step -1
       print string$(p%>0,chr$(9))+fnval$(operands$(p%),1) ;
     next p%
     print csi$+"m"+bccolor$
     operands_c% = 0
     return
1100 ' +
    if operands_c% < 2 then STOP : 'arity error
    a% = operands_c% - 2 : 'print "a=",operands$(a%)
    b% = operands_c% - 1 : 'print "b=",operands$(b%)
    if trace% then print "PLUS", "a:"+fnval$(operands$(a%),1),type2str$(fntyp%(operands$(a%), 1)), "b"+fnval$(operands$(b%),1), type2str$(fntyp%(operands$(b%), 1)), operands_c%
    if fntyp%(operands$(a%), 1) <> fntyp%(operands$(b%), 1) then STOP : ' type error
    if fntyp%(operands$(a%), 1) = type_str% then operands$(a%) = fntlv$( type_str%, fnval$(operands$(a%),1) + fnval$(operands$(b%),1))
    if fntyp%(operands$(a%), 1) = type_int% then operands$(a%) = fntlv$( type_int%, val(fnval$(operands$(a%),1)) + val(fnval$(operands$(b%), 1)))
    if trace% then print "PLUS => ",fnval$(operands$(a%),1)
    operands_c% = operands_c% - 1
    return
1200 'assignment =
   if operands_c% < 2 then STOP : 'arity error
   lhs% = operands_c% - 2
   rhs% = operands_c% - 1
   'if fntyp%(operands$(lhs%), 1) 
   'if fntyp%(operands$(lhs%), 1) <> type_str% then STOP : 'type error
   if trace% then print "ASSIGNMENT",fnval$(operands$(lhs%),1),":=",fnval$(operands$(rhs%),1)
   mem$( fnval$(operands$(lhs%),1) ) = operands$(rhs%)
   operands_c% = operands_c% -2
   return
1300 'line number /label (for goto, gosub, ...)
   if operands_c% < 1 then STOP : 'arity error
   ' point to the label itself, that way GOTO etc can mutate tape_off% and return
   mem$( "line", fnval$(operands$(operands_c%-1),1)) = fntlv$(type_int%, tape_off%) : 'fn_next_off%(tape$, tape_off%)+2
   operands_c% = operands_c% - 1
   return
1400 'GOTO. NB in TH BASIC 8000.00003 denotes 4th line after 8000.00000, -0.99999 is the first line of unnumbered program
   tape_off% = val( fnval$( mem$( "line",fnval$(operands$(operands_c%-1),1)   )  ,1))
   operands_c% = operands_c% - 1
   return
1500 'SLEEP
   if fntyp%(operands$(operands_c%-1),1) <> type_int% then STOP : 'type error
   if trace% then print "SLEEP opcnt:", operands_c%
   SLEEP fnval$(operands$(operands_c%-1),1)
   operands_c% = operands_c% - 1
   return
1600 'GOSUB
   ' subtract 1 (we are pointing to the GOSUB instruction, which is 1 byte long):
   callstack$( callstack_frame% ) = fntlv$(type_int%, fn_next_off%(tape$, tape_off%)-1)
   callstack_frame% = callstack_frame% + 1
   tape_off% = val( fnval$( mem$( "line",fnval$(operands$(operands_c%-1),1)   )  ,1))
   if trace% then print "GOSUB", tape_off%
   operands_c% = operands_c% - 1
   return
1700 'RETURN
   if callstack_frame% < 1 then STOP
   callstack_frame% = callstack_frame% - 1
   tape_off% = val( fnval$( callstack$(callstack_frame%), 1  ) )
   if trace% then print "returning to", tape_off%-2
   return
1800 'SWAP
     if operands_c% < 2 then STOP
     operands$(operands_c%) = operands$(operands_c%-2)
     operands$(operands_c% - 2) = operands$(operands_c%-1)
     operands$(operands_c% - 1) = operands$(operands_c%)
     return
1900 ' END
     if trace% then print "END."
     tape_off% = len(tape$)
     return
2000 'subtraction
    if operands_c% < 2 then STOP : 'arity error
    a% = operands_c% - 2 : 'print "a=",operands$(a%)
    b% = operands_c% - 1 : 'print "b=",operands$(b%)
    if trace% then print "MINUS", fnval$(operands$(a%),1), fnval$(operands$(b%),1)
    if fntyp%(operands$(a%), 1) <> fntyp%(operands$(b%), 1) then STOP : ' type error
    if fntyp%(operands$(a%), 1) <> type_int% then STOP : 'type error
    operands$(operands_c%-2) = fntlv$( type_int%, val(fnval$(operands$(a%),1)) - val(fnval$(operands$(b%), 1)))
    if trace% then print "MINUS => ",fnval$(operands$(a%),1)
    operands_c% = operands_c% - 1
    return
2100 ' multiply
     if operands_c% < 2 then STOP : 'arity error
     a% = operands_c% - 2
     b% = operands_c% - 1
     if fntyp%(operands$(a%),1) <> type_int% then print "multiplication: first operand not int": stop
     if fntyp%(operands$(b%),1) <> type_int% then print "multiplication: second operand not int": stop
     operands$(operands_c% -2) = fntlv$(type_int%, val(fnval$(operands$(a%),1)) * val(fnval$(operands$(b%),1)))
     operands_c% = operands_c% - 1
     RETURN
2200 ' divide
     if operands_c% < 2 then STOP : 'arity error
     a% = operands_c% - 2
     b% = operands_c% - 1
     if fntyp%(operands$(a%),1) <> type_int% then print "division: first operand not int": stop
     if fntyp%(operands$(b%),1) <> type_int% then print "division: second operand not int": stop
     if trace% then print "div:", val(fnval$(operands$(a%),1)), "/", val(fnval$(operands$(b%),1))
     operands$(operands_c% -2) = fntlv$(type_int%, val(fnval$(operands$(a%),1)) / val(fnval$(operands$(b%),1)))
     operands_c% = operands_c% - 1
     RETURN
2300 ' exponentiation
     if operands_c% < 2 then STOP : 'arity error
     a% = operands_c% - 2
     b% = operands_c% - 1
     ' TODO check they are both int
     if fntyp%(operands$(a%),1) <> type_int% then print "POW: first operand not int": stop
     if fntyp%(operands$(b%),1) <> type_int% then print "POW: second operand not int": stop
     if trace% then print "pow:",val(fnval$(operands$(a%),1)), "^", val(fnval$(operands$(b%),1)), "=>", val(fnval$(operands$(a%),1)) ^ val(fnval$(operands$(b%),1))
     operands$(operands_c% -2) = fntlv$(type_int%, val(fnval$(operands$(a%),1)) ^ val(fnval$(operands$(b%),1)))
     operands_c% = operands_c% - 1
     RETURN
2400 'XOR
     if operands_c% < 2 then STOP : 'arity error
     a% = operands_c% - 2
     b% = operands_c% - 1
     if fntyp%(operands$(a%),1) <> type_int% then print "xor: first operand not int": stop
     if fntyp%(operands$(b%),1) <> type_int% then print "xor: second operand not int": stop
     if trace% then print "xor:",val(fnval$(operands$(a%),1)), "^", val(fnval$(operands$(b%),1)), "=>", val(fnval$(operands$(a%),1)) XOR val(fnval$(operands$(b%),1))
     operands$(operands_c% -2) = fntlv$(type_int%, val(fnval$(operands$(a%),1)) XOR val(fnval$(operands$(b%),1)))
     operands_c% = operands_c% - 1
     RETURN
2500 'IF
     if operands_c% < 1 then STOP : 'arity error
     cond_val% = val(fnval$(operands$(operands_c%-1),1))
     if trace% then print "if: 0=",cond_val%,"then goto",actual_line,offset_next_line%(actual_line)
     operands_c% = operands_c% - 1
     if cond_val% then return
     if 1 = offset_next_line%(actual_line) then tape_off% = len(tape$) : return : 'last line
     tape_off% = offset_next_line%(actual_line)
     RETURN
2600 'THEN
     STOP
2700 'TRON
     trace% = 1
     RETURN
2800 'TROFF
     trace% = 0
     RETURN
2900 'SIN
     a% = operands_c% - 2
     a_val% = val(fnval$(operands$(a%),1))
     if trace% then print "SIN", a_val%, SIN(a_val%)
     operands_c% = operands_c% - 1
     operands$(a%) = fntlv$(type_int%, SIN(a_val%))
     RETURN
3000 'COSinus
    a% = operands_c% - 2
    a_val% = val(fnval$(operands$(a%),1))
    if trace% then print "COS", a_val%, COS(a_val%)
    operands_c% = operands_c% - 1
    operands$(a%) = fntlv$(type_int%, COS(a_val%))
    RETURN
3100 'ArcTaNgent
    a% = operands_c% - 2
    a_val% = val(fnval$(operands$(a%),1))
    if trace% then print "ATN", a_val%, ATN(a_val%)
    operands_c% = operands_c% - 1
    operands$(a%) = fntlv$(type_int%, ATN(a_val%))
    RETURN
3200 ' String LENgth
    a% = operands_c% - 2
    if fntyp%(operands$(a%), 1) <> type_str% then stop : 'type error
    a_val$ = fnval$(operands$(a%), 1)
    if trace% then print "LEN", a_val$, len(a_val$)
    operands_c% = operands_c% - 1
    operands$(a%) = fntlv$(type_int%, LEN(a_val$))
    RETURN
3300 'CHR$, singleton str
    a% = operands_c% - 2
    if fntyp%(operands$(a%), 1) <> type_int% then print operands$(a%) : stop : 'type error
    a_val% = int(fnval$(operands$(a%), 1))
    if trace% then print "CHR$", a_val%, chr$(a_val%)
    operands_c% = operands_c% - 1
    operands$(a%) = fntlv$(type_str%, chr$(a_val%))
    return
3400 'MID$, mid section of string
    if operands_c% < 2 then STOP : 'arity error, note that 3 is also ok
    midargc% = int(fnval$(operands$(operands_c% - 1),1)) : 'int, argc
    if (midargc% < 1) or (2 < midargc%) then print "midargc%", midargc% : stop : 'arity error
    a% = operands_c% - 1 - midargc% - 1 : 'the str
    if fntyp%(operands$(a%), 1) <> type_str% then print operands$(a%) : stop : 'type error
    b% = operands_c% - 1 - midargc% : ' offset
    print "b is ",b%, operands$(b%)
    b% = a% + 1
    a_val$ = fnval$(operands$(a%), 1)
    b_val% = int(fnval$(operands$(b%), 1))
    c_val% = len(a_val$)
    if midargc% = 2 then c_val% = int(fnval$(operands$(operands_c%-midargc%), 1))
    if trace% then print "MID$", "a:",a_val$,"b:",b_val%,"c:",c_val%, mid$(a_val$, b_val%, c_val%)
    operands_c% = operands_c% - midargc% - 1
    operands$(a%) = fntlv$(type_str%, mid$(a_val$, b_val%, c_val%))
    return
3500 'ASCii of first operand
    if operands_c% < 2 then STOP : 'arity error
    a% = operands_c% - 2
    if fntyp%(operands$(a%),1) <> type_str% then print a%, operands$(a%), "not str",fntyp%(operands(a%),1): stop
    a_val$ = fnval$(operands$(a%), 1)
    if trace% then print "ASC", mid$(a_val$,1,1), asc(mid$(a_val$,1,1))
    operands_c% = operands_c% - 1
    operands$(a%) = fntlv$(type_int%, asc(a_val$))
    return
3600 'unary minus
    if operands_c% < 1 then STOP : 'arity error
    a% = operands_c% - 1
    a_val% = val(fnval$(operands$(a%),1))
    print "unary minus", operands$(a%), a_val%, "=>", -a_val%
    operands$(a%) = fntlv$(type_int%, -a_val%)
    return
3700 'abs
    if operands_c% < 2 then STOP : 'arity error
    if fnval$(operands$(operands_c%-1),1) <> "0" then stop : 'arity error
    if fntyp%(operands$(operands_c%-2),1) <> type_int% then stop : 'type error
    if trace% then print "ABS",operands_c%,operands$(operands_c%-1)
    operands$(operands_c%-2) = fntlv$(type_int%, abs(val(fnval$(operands$(operands_c%-2),1))))
    operands_c% = operands_c% - 1
    return
3800 'bin$ : d -> s
    if operands_c% < 2 then stop
    a_val% = val(fnval$(operands$(operands_c%-2),1))
    operands$(operands_c%-2) = fntlv$(type_str, bin$(a_val%))
    operands_c% = operands_c% - 1
    return
3900 'boolean equal
    if operands_c% < 2 then stop : 'arity error
    a_val$ = fnval$(operands$(operands_c%-1),1)
    b_val$ = fnval$(operands$(operands_c%-2),1)
    if trace% then print "= comparison:",a_val$,"=",b_val$,"=>",a_val$ = b_val$
    operands_c% = operands_c% - 1
    operands$(operands_c%-1) = fntlv$(type_int%, a_val$ = b_val$)
    return
4000 'boolean inequal
    if operands_c% < 2 then stop : 'arity error
    a_val$ = fnval$(operands$(operands_c%-1),1)
    b_val$ = fnval$(operands$(operands_c%-2),1)
    if trace% then print "<> comparison:",a_val$,"<>",b_val$,"=>",a_val$ <> b_val$
    operands_c% = operands_c% - 1
    operands$(operands_c%-1) = fntlv$(type_int%, a_val$ <> b_val$)
    return
4100 'boolean less than <
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print fntyp%(a$, 1), fntyp%(b$, 1) : stop : 'type error
    if fntyp%(a$, 1) = type_int% then res$ = fntlv$(type_int%, val(fnval$(a$,1)) < val(fnval$(b$,1)))
    if fntyp%(a$, 1) = type_str% then res$ = fntlv$(type_int%, fnval$(a$,1) < fnval$(b$,1))
    if trace% then PRINT "< comparison", type2str$(fntyp%(a$,1)), a$, type2str$(fntyp%(b$,1)), b$, "=>", res$
    if res$ = "" then print "type error" : stop
    operands$(operands_c%-2) = res$
    operands_c% = operands_c% - 1
    return
4200 'boolean greater than >
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print fntyp%(a$, 1), fntyp%(b$, 1) : stop : 'type error
    if fntyp%(a$, 1) = type_int% then res$ = fntlv$(type_int%, val(fnval$(a$,1)) > val(fnval$(b$,1)))
    if fntyp%(a$, 1) = type_str% then res$ = fntlv$(type_int%, fnval$(a$,1) > fnval$(b$,1))
    if trace% then PRINT "> comparison", type2str$(fntyp%(a$,1)), a$, type2str$(fntyp%(b$,1)), b$, "=>", res$
    if res$ = "" then print "type error" : stop
    operands$(operands_c%-2) = res$
    operands_c% = operands_c% - 1
    return
4300 'boolean NOT
    if operands_c% < 1 then stop : 'arity error
    a$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) = type_int% then res$ = fntlv$(type_int%, NOT VAL(fnval$(a$,1)))
    if fntyp%(a$, 1) = type_str% then res$ = fntlv$(type_int%, NOT fnval$(a$,1))
     if trace% then PRINT "NOT: ", type2str$(fntyp%(a$,1)), a$ "=>", res$
     if res$ = "" then print "type error" : stop
     operands$(operands_c%-1) = res$
     return
4400 'less-than-or-equal
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print fntyp%(a$, 1), fntyp%(b$, 1) : stop : 'type error
    if fntyp%(a$, 1) = type_int% then res$ = fntlv$(type_int%, val(fnval$(a$,1)) <= val(fnval$(b$,1)))
    if fntyp%(a$, 1) = type_str% then res$ = fntlv$(type_int%, fnval$(a$,1) <= fnval$(b$,1))
    if trace% then PRINT "<= comparison", type2str$(fntyp%(a$,1)), a$, type2str$(fntyp%(b$,1)), b$, "=>", res
    if res$ = "" then print "type error" : stop
    operands$(operands_c%-2) = res$
    operands_c% = operands_c% - 1
    return
4500 'less-than-or-equal
   if operands_c% < 2 then stop : 'arity error
   a$ = operands$(operands_c%-2)
   b$ = operands$(operands_c%-1)
   if fntyp%(a$, 1) <> fntyp%(b$, 1) then print fntyp%(a$, 1), fntyp%(b$, 1) : stop : 'type error
   if fntyp%(a$, 1) = type_int% then res$ = fntlv$(type_int%, val(fnval$(a$,1)) >= val(fnval$(b$,1)))
   if fntyp%(a$, 1) = type_str% then res$ = fntlv$(type_int%, fnval$(a$,1) >= fnval$(b$,1))
   if trace% then PRINT ">= comparison", type2str$(fntyp%(a$,1)), a$, type2str$(fntyp%(b$,1)), b$, "=>", res
   if res$ = "" then print "type error" : stop
   operands$(operands_c%-2) = res$
   operands_c% = operands_c% - 1
   return
4600 'IMP
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print "TYPE ERROR", type2str$(fntyp%(a$, 1)), type2str$(fntyp%(b$, 1)) : stop
    if fntyp%(a$, 1) <> type_int% then print "TYPE ERROR", fnval$(a,1) : stop
    a_val% = val(fnval$(a$,1)) : b_val% = val(fnval$(b$,1))
    if trace% then PRINT "IMP:", a_val%, b_val%, "=>", a_val% imp b_val%
    operands$(operands_c%-2) = fntlv$(type_int%, a_val% IMP b_val%)
    operands_c% = operands_c% - 1
    return
4700 'EQV, bitwise equality
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print "TYPE ERROR", type2str$(fntyp%(a$, 1)), type2str$(fntyp%(b$, 1)) : stop
    if fntyp%(a$, 1) <> type_int% then print "TYPE ERROR", fnval$(a,1) : stop
    a_val% = val(fnval$(a$,1)) : b_val% = val(fnval$(b$,1))
    if trace% then PRINT "EQV:", a_val%, b_val%, "=>", a_val% EQV b_val%
    operands$(operands_c%-2) = fntlv$(type_int%, a_val% EQV b_val%)
    operands_c% = operands_c% - 1
    return
4800 'OR, bitwise OR
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print "TYPE ERROR", type2str$(fntyp%(a$, 1)), type2str$(fntyp%(b$, 1)) : stop
    if fntyp%(a$, 1) <> type_int% then print "TYPE ERROR", fnval$(a,1) : stop
    a_val% = val(fnval$(a$,1)) : b_val% = val(fnval$(b$,1))
    if trace% then PRINT "OR:", a_val%, b_val%, "=>", a_val% OR b_val%
    operands$(operands_c%-2) = fntlv$(type_int%, a_val% OR b_val%)
    operands_c% = operands_c% - 1
    return
4900 'AND, bitwise AND
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print "TYPE ERROR", type2str$(fntyp%(a$, 1)), type2str$(fntyp%(b$, 1)) : stop
    if fntyp%(a$, 1) <> type_int% then print "TYPE ERROR", fnval$(a,1) : stop
    a_val% = val(fnval$(a$,1)) : b_val% = val(fnval$(b$,1))
    if trace% then PRINT "AND:", a_val%, b_val%, "=>", a_val% AND b_val%
    operands$(operands_c%-2) = fntlv$(type_int%, a_val% AND b_val%)
    operands_c% = operands_c% - 1
    return
4950 'MOD, modulo
    if operands_c% < 2 then stop : 'arity error
    a$ = operands$(operands_c%-2)
    b$ = operands$(operands_c%-1)
    if fntyp%(a$, 1) <> fntyp%(b$, 1) then print "TYPE ERROR", type2str$(fntyp%(a$, 1)), type2str$(fntyp%(b$, 1)) : stop
    if fntyp%(a$, 1) <> type_int% then print "TYPE ERROR", fnval$(a,1) : stop
    a_val% = val(fnval$(a$,1)) : b_val% = val(fnval$(b$,1))
    if b_val% = 0 then print "VALUE ERROR: division by zero": stop
    if trace% then PRINT "MOD:", a_val%, b_val%, "=>", a_val% MOD b_val%
    operands$(operands_c%-2) = fntlv$(type_int%, a_val% MOD b_val%)
    operands_c% = operands_c% - 1
    return
5000 'VAL, float of int/str
    if operands_c% < 2 then STOP : 'arity error
    if fnval$(operands$(operands_c% -1),1) <> "0" then print "VAL: arity error" : stop
    a$ = operands$(operands_c%-2)
    if not ((fntyp%(a$,1) = type_int%) or (fntyp%(a$,1)=type_str%)) then print "type error, VAL operand must be str/int",a$: STOP
    if trace% then print "VAL", type2str$(fntyp%(a$,1)), fnval$(a$,1), "=>", VAL(fnval$(a$,1))
    operands$(operands_c%-2) = fntlv$(type_int%, VAL(fnval$(a$,1)))
    operands_c% = operands_c% - 1
    return
5050 'INT, truncated float of int/str
    if operands_c% < 2 then STOP : 'arity error
    if fnval$(operands$(operands_c% -1),1) <> "0" then print "INT: arity error", : stop
    a$ = operands$(operands_c%-2)
    if not ((fntyp%(a$,1) = type_int%) or (fntyp%(a$,1)=type_str%)) then print "type error, INT operand must be str/int",a$: STOP
    if trace% then print "INT", type2str$(fntyp%(a$,1)), fnval$(a$,1), "=>", INT(fnval$(a$,1))
    operands$(operands_c%-2) = fntlv$(type_int%, INT(fnval$(a$,1)))
    operands_c% = operands_c% - 1
    return
5100 'CINT/1, round up/down
   if operands_c% < 2 then STOP : 'arity error
   if fnval$(operands$(operands_c% -1),1) <> "0" then print "CINT: arity error", : stop
   a$ = operands$(operands_c%-2)
   if not ((fntyp%(a$,1) = type_int%) or (fntyp%(a$,1)=type_str%)) then print "type error, INT operand must be str/int",a$: STOP
   if trace% then print "INT", type2str$(fntyp%(a$,1)), fnval$(a$,1), "=>", CINT(fnval$(a$,1))
   operands$(operands_c%-2) = fntlv$(type_int%, CINT(fnval$(a$,1)))
   operands_c% = operands_c% - 1
   return
5150 'CSNG/1, -1/0/1 depending on sign of operand
   if operands_c% < 2 then STOP : 'arity error
   if fnval$(operands$(operands_c% -1),1) <> "0" then print "CSNG: arity error", : stop
   if trace% then print "CSNG", fnval$(operands$(operands_c%-2),1), "=>", CSNG(fnval$(operands$(operands_c%-2),1))
   operands$(operands_c% - 2) = fntlv$(type_int%, CSNG(fnval$(operands$(operands_c%-2),1)))
   operands_c% = operands_c% - 1
   return
5200 'READ (from DATA)
   col_data = 0
   row_data = val(mem$("data_row")) + 1
   for var = 0 to operands_c% -1
     if trace% then print "READ: operands:",operands_c%,fnval$(operands$(var),1)
     'if val(mem$("data_line")) >= no_read% then stop : ' out of bounds read (row)
     'if val(mem$("data_row")) >= col_read% then stop : 'out of bounds read (column)
     mem$( fnval$(operands$(var),1) ) = mem$( "data", row_data, col_data )
     col_data = col_data + 1
   next var
   mem$("data_row") = str$(row_data)
   operands_c% = 0
   return
6000 print string$(width/5*2, "=")+" Lexing & parsing"
     bytecode$ = ""
     def fnwhitespace%(fnws_i%) = th_re(mid$(inp$, fnws_i%, 1), "[ \t]", 1)
     def fnnum%(fnnum_i%) = th_re(mid$(inp$, fnnum_i%, 1), "\d", 1)
     dim pstate%(1)
     pstate%("label")     = 1 : state2str$(1) = "label"
     pstate%("statement") = 2 : state2str$(2) = "statement"
     pstate%("expr")      = 3 : state2str$(3) = "expr"
     pstate%("string")    = 4 : state2str$(4) = "string"
     'pstate%("swap")      = 5
     pstate%("assignment") = 5 : state2str$(5) = "assignment"
     pstate%("identifier") = 6 : state2str$(6) = "identifier"
     pstate%("infix")      = 7 : state2str$(7) = "infix"
     pstate%("data")       = 8 : state2str$(8) = "data"
     pstate%("ptr2ref")    = 9 : state2str$(9) = "ptr2ref"
     dim pstack%(1) : 'parent stack
     pstack_i% = 1
     pstack%(pstack_i%) = pstate%("label")

     string_builder% = 0

     no_data% = 0
     compiled_data$(1, 0) = "" : ' ERASE compiled_data$

     dim compiled$(1) : 'stack of compiled tokens (for RPN compilation)
     compiled_c% = 0

     dim expr_expect_stack%(1)
     expr_expect_c% = 0

     for ci% = 1 to len(inp$)
       ch$ = mid$(inp$,ci%,1)
       print th_sprintf$("=> ci=%-4d pstack_i=%-3d exprEXP(%d-1)=%s ", ci%, pstack_i%, expr_expect_c%, type2str$(expr_expect_stack%(expr_expect_c%-1)));
       for pstki=1 to pstack_i% : print state2str$(pstack%(pstki))+" "; : next pstki : print "ch="+ch$
       if (pstack%(pstack_i%)=pstate%("expr")) and (mid$(inp$,ci%,1) = ":") then print "expr at end of statement, draining op stack then compiling": gosub 10900 : gosub 7400 : goto 7000
       if (pstack_i% = 2) and (mid$(inp$,ci%,1) = ":") then gosub 7400 : goto 7000
       if (pstack%(pstack_i%-1) = pstate%("statement")) and (mid$(inp$,ci%,1) = ":") then gosub 7400 : goto 7000
       if (pstack%(pstack_i%) < 1) or (pstack%(pstack_i%) > 8) then print pstack%(pstack_i%) : stop
       on pstack%(pstack_i%) gosub 7500, 8000, 10000, 11000, 9000, 12000, 10800, 13000
       if (pstack%(pstack_i%)=pstate%("expr")) and (ci%=len(inp$)) then print "expr at EOF", operator_c% : gosub 10900
       print th_sprintf$("<= ci=%-4d pstack_i=%-3d exprEXP(%d-1)=%s ",ci%,pstack_i%, expr_expect_c%, type2str$(expr_expect_stack%(expr_expect_c%-1)));
       for pstki=1 to pstack_i% : print state2str$(pstack%(pstki))+" "; : next pstki : print

7000  next ci%
     gosub 7400
     if no_data% = 0 then RETURN : 'end of parsing
     for di% = 1 to no_data%
       print "COMPILED DATA:", di%, dr%, "=>", compiled_data$(di%, dr%)
       dr% = 0
       this$ = ""
7050   if compiled_data$(di%, dr%) = "" then 7100
       this$ = fntlv$(type_str%, compiled_data$(di%, dr%)) + this$
       dr% = dr% + 1
       goto 7050
7100 bytecode$ = this$ + fntlv$(type_int%, dr%) + fntlv$(type_opcode%, "DATA") + bytecode$
     next di%
     RETURN : ' end of parsing (with DATA)

7400 'emit compile stack at end of statement parsing
     'if pstack%(pstack_i%-1) <> pstate%("swap") then 7430
     'compiled$(compiled_c%) = compiled$(compiled_c%-2)
     'compiled$(compiled_c%-2) = fntlv$(type_opcode%, "SWAP")
     'compiled_c% = compiled_c% + 1
     'TODO
     'pstack_i% = pstack_i%-1
     'goto 7400
     print "DEBUG: expr_expect_c%", expr_expect_c%
7430 if (pstack_i%=3) and (pstack%(3)=pstate%("expr")) and (pstack%(2)=pstate%("statement") or pstack%(2)=pstate%("assignment")) then print "PSTACK", pstack_i%, pstack%(2), pstack%(3) : goto 7440 : 'unclosed parent
     if not ((pstack_i% > 2) and (compiled_c% > 0) and (fntyp%(compiled$(0),1)=type_opcode%) and (fnval$(compiled$(0),1)="IF")) then 7435
       print "\\\\\\\\\\ IF: compiled (cond) ; IF ; statement2; ... ; until [label]"
       for ps=1 to pstack_i%:print state2str$(pstack%(ps)):next ps
       for c = 0 to compiled_c%-1
          print c, fnval$(compiled$(c),1)
       next
       print type2str$(fntyp%(compiled$(0),1)),"FNVAL:",fnval$(compiled$(0),1)
       stop
7435 if not ((pstack_i%=3) and (pstack%(3)=pstate%("data"))) then 7438
        print "\\\\\ END OF DATA"
        pstack_i% = pstack_i% - 1

7438 if not ((pstack_i%=4) and pstack%(3)=pstate%("ptr2ref")) then 7439
     ' TODO this is some hacky shit right here, rewriting ptr to str.
     ' presently used in READ, but same problem exists in ASSIGNMENT, INPUT, etc where we want the literal ptr
     for c = 0 to compiled_c%
       if fntyp%(compiled$(c),1) = type_ptr% then compiled$(c) = fntlv$(type_str%, fnval$(compiled$(c),1))
     next
     'print "PTR2REF",compiled_c%
     goto 7440
7439 if pstack_i% <> 2 then print "PSTACK<>2",pstack_i% :for ps=1 to pstack_i%:print state2str$(pstack%(ps)):next ps: stop
7440 if compiled_c% <= 1 then 7460
     print string$(40,"_")+"EMITTING COMPILED",compiled_c%
     for compile_i% = 1 to compiled_c%-1
       print "infix_stack", infix_c%
       print "","compiled:",fnval$(compiled$(compile_i%),1)
       bytecode$ = bytecode$ + compiled$(compile_i%)
       'if not infix_c% then 7460
       'if not infix_at%(compile_i%) then 7460
       'print infix_at%(compile_i%)-1, infix_stack$(0)
       'bytecode$ = bytecode$ + infix_stack$(infix_at%(compile_i%)-1)
       'infix_c% = infix_c% -1
     next compile_i%
7460 bytecode$ = bytecode$ + compiled$(0)
     compiled_c% = 0
     pstack_i% = 1
     pstack%(pstack_i%) = pstate%("label")
     RETURN

7500 'label parsing
     if fnwhitespace%(ci%) then RETURN
     if expr_expect_c% then print "WHY ARE WE EXPECTING things" : STOP
     label$ = ""
7520 if fnnum%(ci%) then label$ = label$ + mid$(inp$, ci%, 1) : ci% = ci% + 1 : goto 7520
     if label$ <> "" then bytecode$ = bytecode$ + fntlv$(type_int%, label$) + fntlv$(type_opcode%, "#")
     if label$ = "" then ci% = ci% -1 : ' we didn't find a label, so ci% point(ed) at first char of statement.
     pstack_i% = pstack_i% + 1
     pstack%(pstack_i%) = pstate%("statement")
     return
8000 'statement parsing
     if fnwhitespace%(ci%) then RETURN
     lookahead$ = mid$(inp$, ci%, 10)
     'TODO in line with TH Basic and no doubt Dartmouth basic, we don't do look-ahead to determine
     'if found$ is at a word boundary (remoulade$ = 1 gets parsed as REM ....)
     found$ = th_re$(lookahead$, "^(DATA|END|&|PRINT|SLEEP|GO\s*TO|GOSUB|RETURN|REM|READ|'|IF|TRON|TROFF)", 1, "i")
     ' if it's not a statement keyword, it's either a syntax error or an assignment:
     if found$ <> "" then 8500
       compiled$(compiled_c%) = fntlv$(type_opcode%, "=")
       compiled_c% = compiled_c + 1
       pstack%(pstack_i%) = pstate%("assignment") : ' look for =
       pstack_i% = pstack_i%+1
       pstack%(pstack_i%) = pstate%("identifier") : 'look for LHS
       ci%=ci%-1
       return
8500 found$ = UPS$(found$)
     if (found$ = "REM") or (found$="'") then ci%=len(inp$) : RETURN : 'comment to the end of the line TODO
     print "statement:", ci%, found$
     opname$ = str2op$(th_sed$(found$, "\s+", "", "g")) : 'go to -> GOTO
     if opname$ = "" then STOP : 'didn't find this operator
     ci% = ci% + len(found$) -1
     
     if opname$ <> "DATA" then 8700
       ' DATA gets pushed in front of the program
       no_data% = no_data% + 1 : rec_data% = 0 : compiled_data$(no_data%, rec_data%) = ""
       pstack_i% = pstack_i% + 1
       pstack%(pstack_i%) = pstate%("data")
       RETURN
8700 print "STATEMENT-COMPILED_C%",compiled_c%
     compiled$(compiled_c%) = fntlv$(type_opcode%, opname$)
     compiled_c% = compiled_c% + 1
     pstack_i% = pstack_i% + 1
     pstack%(pstack_i%) = pstate%("expr")
     if opname$ = "READ" then pstack%(pstack_i%)=pstate%("ptr2ref"):pstack_i%=pstack_i%+1:pstack%(pstack_i%)=pstate%("expr"):pstack_i%=pstack_i%+1:pstack%(pstack_i%) = pstate%("identifier")
     expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
     RETURN
9000 'assignment
     if fnwhitespace%(ci%) then RETURN
     if mid$(inp$, ci%, 1) <> "=" then STOP : 'expect = here
     'LHS has just been compiled to a ptr%, but we are not dereferencing it when assigning,
     'so we need to fix that:
     if fntyp%(compiled$(compiled_c%-1),1) <> type_ptr% then STOP : 'expected an identifier ptr
     identifier$ = fnval$(compiled$(compiled_c%-1),1)
     compiled$(compiled_c%-1) = fntlv$(type_str%, identifier$)
     'now find RHS:
     pstack%(pstack_i%) = pstate%("expr") : ' TODO add to expr_expect stack
     print "assignLHS", right$(identifier$,1)
     expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
     if right$(identifier$,1) = "$" then expr_expect_stack%(expr_expect_c%-1) = type_str%
     return
10000 'expr
      if fnwhitespace%(ci%) then RETURN
      dim expr_stack$(1)
      expr_stack_c% = 0
      ch$ = mid$(inp$, ci%, 1)
      print "expr:"+ch$
      if ch$ <> "(" then 10028
        print "pushing ( to operator stack"
        comma_stack%(operator_c%) = 0
        operator_stack$(operator_c%) = "(": operator_c% = operator_c% + 1
        expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
        RETURN
10028 if (ch$ <> ")") then 10050 : 'unbalanced )
      print "close when expecting:",type2str$(expr_expect_stack%(expr_expect_c% -1))
      if expr_expect_stack%(expr_expect_c% -1) <> type_opcode% then print "close) when expecting value" : stop
10030 if not operator_c% then STOP : 'unbalanced right-)
      ' case: ch$ is a ) closing bracket
           print "first)", operator_c%, operator_stack$(operator_c% - 1)
           if operator_stack$(operator_c%-1) = "(" then 10040
           if 9 <> precedence%(operator_stack$(operator_c%-1)) then 10035
              print "prec 9 so output arity operand: ",comma_stack%(operator_c%-1)
              compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1)): compiled_c% = compiled_c% + 1
              comma_stack%(operator_c%-1) = 0
10035      print "output:"+operator_stack$(operator_c% - 1)
           compiled$(compiled_c%) = fntlv$(type_opcode%, operator_stack$(operator_c%-1)) : compiled_c% = compiled_c% + 1
           operator_c% = operator_c% - 1
           goto 10030
10040      print "in )",operator_c%, operator_stack$(operator_c%-1)
           if operator_stack$(operator_c%-1)  <> "(" then stop : 'unbalanced right )
           operator_c% = operator_c% - 1 : 'pop left (
           'if (not operator_c%) and (expr_expect_c% <> 1) then print "TODO can this happen, no operator_c in nested expr?": stop
           if not operator_c%  then return
           expr_expect_c% = expr_expect_c% - 1 :' TODO should this be before the above return?
           expr_expect_stack%(expr_expect_c% - 1) = type_opcode%
           if operator_stack$(operator_c%-1) = "(" then return
           'pop first token:
           'print "   popping first token:"+operator_stack$(operator_c%-1)
           'compiled$(compiled_c%) = fntlv$(type_opcode%, operator_stack$(operator_c%-1))
           'operator_c% = operator_c% - 1
           'compiled_c% = compiled_c% + 1
           return
10050      if not ((ch$ = ",")) then 10100
        ' comma encountered while parsing an expression. this could be starting
        ' a new expression in an operand list, or a syntax error
      if pstack%(pstack_i%-1) = pstate%("expr") then print "COMMA INSIDE EXPRESSION" : stop : 'that's a syntax error
      if pstack%(pstack_i%-1) = pstate%("assignment") then print "COMMA AFTER ASSIGNMENT" : stop
      if pstack%(pstack_i%-1) = pstate%("ptr2ref") then print "COMMA IN ptr2ref",compiled$(compiled_c%-1):compiled$(compiled_c%)=fntlv$(type_str%,fnval$(compiled$(compiled_c%-1),1)):gosub 10900:return
      if pstack%(pstack_i%-1) <> pstate%("statement") then stop
      print "comma in statement, draining stack"
      gosub 10900
      expr_expect_stack%(0)=type_int%:expr_expect_c% = 1
      return
      
10100 ' TODO check that () isn't an empty expr
      print "not () expr:"+ch$,"op stack height:",operator_c%
      'TODO if this we have not emitted anything, an infix operator here is a syntax error.
      'TODO if the last thing we emitted was a value, another value is a syntax error.
      if (ch$ = ")") then stop :' pstack_i% = pstack_i% - 1 : RETURN : 'close expr
      if ch$ = quot$ then pstack_i% = pstack_i% + 1 : pstack%(pstack_i%) = pstate%("string") : string_builder% = ci%+1 : RETURN
      ' it might be an int:
      lookahead$ = ups$(mid$(inp$, ci%, 20))
      found$ = th_re$(lookahead$, "^\d+\.?\d*", 1)
      if found$ = "" then 10200
         ' found an int, put it on compile stack
         if expr_expect_stack%(expr_expect_c%-1) <> type_int% then print "NOT EXPECTING VAL ", found$, expr_expect_stack%(expr_expect_c%-1), expr_expect_c%:STOP : 'not expecting value
         'if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "expecting operator got int", expr_expect_c%: stop
         expr_expect_stack%(expr_expect_c%-1) = type_opcode%
         print "output int:"+found$
         compiled$(compiled_c%) = fntlv$(type_int%, found$)
         compiled_c% = compiled_c% + 1
         ci%=ci%+len(found$)-1
         RETURN
      'it's not a string, it's not an int, it COULD BE infix operator OR identifier:
10200 operator_re$ = "^ *(AND|OR|XOR|MOD|NOT|IMP|EQV|<>|<=|=<|>=|=>|\*\*|[=+/*^<>-])"
      found$ = th_re$(lookahead$, operator_re$, 2)
      if found$ = "" then 10500
      found_len% = len(th_re$(lookahead$, operator_re$, 1))
      if found$ = "=" then found$ = "==" : 'disambiguate comparison from assignment
      if found$ = "**" then found$ = "^" : 'alias for ^
      ci% = ci% + found_len% - 1
      print "OP found:"+found$+".", found_len%
      ' operator found:
      ' - while ( there is an operator on the stack that is not (
      '         ( AND ( (o2 has greater precedence than o1)
      '         (        or (o1 and o2 have same precedence AND o1 is left-associative))
      ' - pop o2 from operator stack into output queue:
10250 goto 10300 : 'if not (operator_c% and (operator_stack$(operator_c%-1) <> "(")) then print "skip 10250",operator_c% : goto 10300
      top$ = operator_stack$(operator_c%-1)
      print th_sprintf$("op stack height:%-3d  found=%s  top=%s", operator_c%, found$, top$)
      print th_sprintf$("todo compare %s(%d) and %s(%d)",found$,precedence%(found$),top$,precedence%(top$))
      'operator_c% = operator_c% -1
      'compiled$(compiled_c%) = fntlv$(type_opcode%, top$) : compiled_c% = compiled_c% + 1
      print "compiled o2:"+top$+" o1:"+found$
      operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1
      'compiled$(compiled_c%) = fntlv$(type_opcode%, found$) : compiled_c% = compiled_c% + 1
'operator_c% = operator_c% -1
'compiled$(compiled_c%) = fntlv$(type_opcode%, top$) : compiled_c% = compiled_c% + 1

      'print "opstack pushing "+found$
      'operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1
      return : 'stop
      goto 10250
10300 if operator_c% then 10400
      print "no opers on stack but found and push:"+found$, expr_expect_stack%(expr_expect_c%-1), expr_expect_c%
      if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then 10350
        if found$ = "-" then operator_stack$(operator_c%) = "~-" : operator_c% = operator_c% + 1: return
        if found$ = "NOT" then operator_stack$(operator_c%) = "NOT" : operator_c% = operator_c% + 1 : return
        print "expected value got operator:"+found$
        stop
10350 operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      return
10400 top$ = operator_stack$(operator_c%-1)
      print th_sprintf$("OP IS AN OPERATOR:[%d]:%s:(%d)  stack top[%d]:%s:(%d)", len(found$), found$,precedence%(found$), len(top$), top$,precedence%(top$))
      ' TODO this here is a mess, should use lookup tables instead of hardcoding the operators:
      dopop = 0
      if precedence%(found$) = 0 then stop
      if precedence%(top$)   = 0 then stop
      if precedence%(found$) <= precedence%(top$) then dopop = 1
      if precedence%(found$) > precedence%(top$) then dopop = -1
      if (dopop = 0) and (found$ <> "^") then dopop = 1 : print "equal precedence level, "+found$+" is left-associative."
      if dopop then print "precedence: ",found$+string$(dopop=1," <= ")+string$(dopop=-1, " > ")+top$
      if found$ = "NOT" then print "GOTNOT dopop <- -1, dopop=", dopop : dopop=-1
      if (found$ = "-") and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then print "FOUNDMINUS ----------- expecting int%":operator_stack$(operator_c%)="~-":operator_c%=operator_c%+1:return
      if top$ = "(" then dopop = -1
      'if top$ = "^" then dopop = -1 : print "NOT POPPING ^ BECAUSE IT IS RIGHT-ASSOCIATIVE"
      if dopop = 0 then stop
      if (dopop=1) and (top$ <> "(") and (9 = precedence%(top$)) then compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1)): compiled_c% = compiled_c% + 1 : comma_stack%(operator_c%-1) =0
      if (dopop=1) and (top$ <> "(") then print "   emitting "+top$ :compiled$(compiled_c%) = fntlv$(type_opcode%, top$) : compiled_c% = compiled_c% + 1 : operator_c% = operator_c% - 1 : goto 10300
      'TODO gosub 10300
      if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then 10450
        if found$ = "-" then print "UNARY MINUS": operator_stack$(operator_c%) = "~-" : operator_c% = operator_c% + 1: return
        if found$ = "NOT" then print "UNARY NOT": operator_stack$(operator_c%) = "NOT" : operator_c% = operator_c% + 1: return
        print "expect value got operator",found$ :stop
10450 operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      print "op stack pushed:"+found$
      return

10500 'it's not a string, and it's not an integer. could be a identifier or a colon or a THEN (if we are in an IF)
      if operator_c% = 0 then print "OPERATOR STACK EMPTY, if "+quot$+found$+quot$+"is a keyword then terminate expr, expr_expect:",expr_expect_stack%(expr_expect_c%-1), expr_expect_c%
      lookahead$ = ups$(mid$(inp$, ci%, 20))
      func$ = th_re$(lookahead$, "^(SIN|COS|ABS|ATN|ASC|BIN\$|CHR\$|CINT|CSNG|INT|ITM|LEFT\$|LEN|MID\$|FN[A-Z0-9_]+[!$%]|RIGHT\$|VAL)\(", 2)
      print "INFIX STACK", infix_c%, ch$, found$, func$
      if (func$ <> "") and expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "expected infix operator, got function:"+func$:stop
      if func$ <> "" then print "is a function", func$: operator_stack$(operator_c%)=func$:operator_c% = operator_c% + 1 : ci% = ci% + len(func$)-1: return
      if left$(lookahead$,4) <> "THEN" then 10560
        print "THEN ENCOUNTERED", pstack_i%, operator_c%, infix_c%, pstack%(pstack_i%)
        if expr_expect_c% and (expr_expect_stack%(expr_expect_c%-1) <> type_opcode%) then print expr_expect_stack%(expr_expect_c%-1): STOP
        if infix_c% <> 0 then print "INFIX_C% <> 0": stop : 'what
        gosub 10900 : 'drain operator stack of the cond expr
        gosub 7400  : 'compile the cond expr and the IF operator
        ci% = ci% -1 + len("THEN")
        return
10560  if not ((ch$ = ":") and (0 < infix_c%)) then 10600
         print "Piling up infix", operator_stack$(operator_c%-1)
         'compiled$(compiled_c%) = infix_stack$(infix_c%-1)
         'infix_c% = infix_c% - 1
         'compiled_c% = compiled_c% + 1
         ci% = ci% - 1
         pstack_i% = pstack_i% - 1
         return
10600 ' TODO could also be a function call
      pstack_i% = pstack_i% + 1
      pstack%(pstack_i%) = pstate%("identifier")
      print "EXPR LOOKING FOR IDENTIFIER",ci%,quot$+mid$(inp$,ci%,1)+quot$
      'if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "looking for operator got identifier?": stop
      if expr_expect_stack%(expr_expect_c%-1) <> type_int% then print "  EXPR EXPECT",expr_expect_c%,expr_expect_stack%(expr_expect_c%-1)
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      ci% = ci% - 1
      RETURN
10800 'infix
      PRINT "fixing INFIX TODO", operator_c%
      stop
      return
10900 if (not operator_c%) then expr_expect_c% = 0 : RETURN
      top$ = operator_stack$(operator_c%-1)
      if top$ <> "(" then 10950
      ' precedence =9 is for functions:
      pretop$ = operator_stack$(operator_c%-2)
      if (operator_c% < 1) or (9<>precedence%(pretop$)) then 10940
      ' TODO tron: print mid$("a",1,2,3)
      comma_stack%(operator_c%-2) = comma_stack%(operator_c%-2) + 1
      if opmax%(pretop$) and (opmax%(pretop$) <= comma_stack%(operator_c%-2)) then print pretop$+"/"+str$(opmax%(pretop$))+": too many arguments": STOP
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      print "at comma, before ( on op stack is",pretop$,"which is a function. commas:",comma_stack%(operator_c%-2)
      return
10940 for i = operator_c%-1 to 0 step -1
        print operator_stack$(i)
      next i
      print "UNMATCHED '('" : STOP
10950 print "end opstack ",operator_c%,top$
      if expr_expect_c% and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then print "at end of expr looking for int":stop
      if expr_expect_c% then expr_expect_c% = expr_expect_c% - 1
      if expr_expect_c% then print "last expr expect", expr_expect_c%
      if 9 = precedence%(top$) then print "WAS FUNC ",top$," with COMMA STACK ",comma_stack%(operator_c%-1)
      if 9 = precedence%(top$) then compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1))
      if 9 = precedence%(top$) then compiled_c% = compiled_c% + 1 : comma_stack%(operator_c%-1) = 0
      compiled$(compiled_c%) = fntlv$(type_opcode%, top$)
      compiled_c% = compiled_c% + 1 : operator_c% = operator_c% - 1
      GOTO 10900
11000 'string
      if ch$ <> quot$ then RETURN : 'next char
      string_builder$ = mid$(inp$, string_builder%, ci% - string_builder%)
      pstack_i% = pstack_i% - 1
      if pstack%(pstack_i%) <> pstate%("expr") then goto 11200
        compiled$(compiled_c%) = fntlv$(type_str%, string_builder$)
        compiled_c% = compiled_c% + 1
        expr_expect_stack%(expr_expect_c%-1) = type_opcode%
        RETURN
11200 if pstack%(pstack_i%) <> pstate%("data") then STOP : 'how did we end up here?
        print "END OF STR PARSING FOR DATA no:",no_data%,"rec:",rec_data%,"payload:",ci%-string_builder%,string_builder$
        compiled_data$(no_data%, rec_data%) = fntlv$(type_str%, string_builder$)
        compiled_data$(no_data%, rec_data%+1) = ""
      RETURN
12000 'identifier
      if fnwhitespace%(ci%) then RETURN
      print "IDENTIFIER LOOKING"
      ch$ = mid$(inp$, ci%, 1)
      if not th_re(ch$, "^[A-Za-z_]", 1) then print quot$+ch$+quot$: STOP : 'not valid identifier start char
      for id% = ci%+1 to len(inp$) : ' len(tape$)
        ch$ = mid$(inp$, id%, 1)
        if not th_re(ch$, "^[a-z_0-9$%!]", 1) then done : goto 12200 : 'end of plain identifier
      next id%
12200 id$ = mid$(inp$,ci%, id%-ci%)
      ci% = id%-1
      'print "id="+id$+". next=" + mid$(inp$,ci%,1)
      ' TODO no support for arrays yet
      if expr_expect_c% and (expr_expect_stack%(expr_expect_c%-1) = type_opcode%) then print "got identifier but looked for opcode":stop
      if expr_expect_c% and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then expr_expect_stack%(expr_expect_c%-1) = type_opcode%
      compiled$(compiled_c%) = fntlv$(type_ptr%, id$)
      compiled_c% = compiled_c% + 1
      pstack_i% = pstack_i% - 1
      return
13000 ' DATA
      print ch$,found$,"DATA",ci%
      if fnwhitespace%(ci%) then RETURN
      if ch$ = quot$ then print "woop" : pstack_i% = pstack_i%  + 1 : pstack%(pstack_i%) = pstate%("string"): string_builder% = ci%+1 : return
      if ch$ = "," then print "data col comma at",no_data%,rec_data%: rec_data% = rec_data% + 1 : return
      compiled_data$(no_data%, rec_data%) = compiled_data$(no_data%, rec_data%) + ch$
      compiled_data$(no_data%, rec_data% + 1) = ""
      print "DATA read until : or ,  - rec:", rec_data%, ch$, ch$=","
      return
