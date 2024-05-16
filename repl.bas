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

'TODO it's a problem if the types share names with operators/functions:
type_int%       = asc("%") : type2str$(type_int%) = "int"
type_str%       = asc("$") : type2str$(type_str%) = "str"
type_push%      = asc("(") : type2str$(type_push%) = "("
type_pop%       = asc(")") : type2str$(type_pop%)  = ")"
type_opcode%    = asc("!") : type2str$(type_opcode%) = "op"
type_ptr%       = asc("x") : type2str$(type_ptr%) = "ptr"
type_ptrkey%   =  asc("&") : type2str$(type_ptrkey%) = "ptrkey"
def fntlv$(fnpascal_typ%, fnpascal_arg$) = chr$(fnpascal_typ%) + chr$(len(fnpascal_arg$)) + fnpascal_arg$
def fnlen%(fnlen_tape$, fnlen_off%) = asc(mid$(fnlen_tape$, fnlen_off% + 1, 1))
def fntyp%(fntyp_tape$, fntyp_off%) = asc(mid$(fntyp_tape$, fntyp_off%    , 1))
def fnval$(fnval_tape$, fnval_off%) = mid$(fnval_tape$, fnval_off%+2, fnlen%(fnval_tape$, fnval_off%))
def fnid$(fnid_tape$, fnid_off%) = mid$(fnid_tape$, fnid_off%, 2) + fnval$(fnid_tape$, fnid_off%)
def fn_next_off%(fn_next_off_tape$, fn_next_off_off%) = fn_next_off_off% + fnlen%(fn_next_off_tape$, fn_next_off_off%)
def fn_has_arity(op$) = (9 = precedence%(op$)) or fntyp%(op$,1)=type_ptr%

' interpreter tables:
'   statement(op): true if op is a statement and should clear operands_c%
dim op2str$(1)
str2op$("DATA")  = "DATA" : op2str$("DATA") = "DATA": statement("DATA") = 1
str2op$("READ")  = "READ" : op2str$("READ") = "READ" : opmin%("READ") = 1: statement("READ") = 1
str2op$("PRINT") = "&" : statement("&") = 1
str2op$("&") = "&" : op2str$("&") = "PRINT"
str2op$("+") = "+" : op2str$("+") = "+"
str2op$("SLEEP") = "SLEEP" : op2str$("SLEEP") = "SLEEP" : opmin%("SLEEP") = 1 : opmax%("SLEEP") = 1: statement("SLEEP")=1
str2op$("=") = "=" : statement("=") = 1
str2op$("GOTO") = ":"  : op2str$(":") = "GOTO" : opmin%(":") = 1 : opmax%(":") = 1: statement(":") = 1
str2op$("GOSUB") = "|" : op2str$("|") = "GOSUB" : opmin%("|") = 1 : opmax%("|") = 1: statement("|") = 1
str2op$("RETURN") = ";": op2str$(";") = "RETURN": statement(";") = 1
str2op$("END")    = "END": op2str$("END") = "END": statement("END") = 1
str2op$("ON")     = "ON" : op2str$("ON")  = "ON" : opmin%("ON") = 2: statement("ON") = 1
str2op$("-")      = "-" : op2str$("-") = "-"
str2op$("*")      = "*" : op2str$("*") = "*"
str2op$("/")      = "/" : op2str$("/") = "/"
str2op$("^")      = "^"
str2op$("XOR")    = "XOR"
str2op$("IF")     = "IF" : op2str$("IF")   = "IF" : statement("IF") = 1
str2op$("THEN")   = "THEN": op2str$("THEN") = "THEN"
str2op$("FOR")    = "FOR" : op2str$("FOR") = "FOR" : statement("FOR") = 1
str2op$("NEXT")   = "NEXT": op2str$("NEXT")= "NEXT" : statement("NEXT") = 1
str2op$("TO")     = "TO"  : op2str$("TO")  = "TO": statement("TO") = 1
str2op$("TRON")   = "TRON": op2str$("TRON")="TRON"  : statement("TRON") = 1
str2op$("TROFF")  = "TROFF":op2str$("TROFF")="TROFF": statement("TROFF")= 1
str2op$("SIN")    = "SIN" : op2str$("SIN") = "SIN" : opmax%("SIN") = 1
str2op$("COS")    = "COS" : op2str$("COS") = "COS" : opmax%("COS") = 1
str2op$("ATN")    = "ATN" : op2str$("ATN") = "ATN" : opmax%("ATN") = 2 :' TODO
str2op$("ABS")    = "ABS" : op2str$("ABS") = "ABS" : opmax%("ABS") = 1
str2op$("BIN$")   = "BIN$": op2str$("BIN$") = "BIN$" : opmax%("BIN$") = 1
str2op$("UPS$")   = "UPS$": op2str$("UPS$")= "UPS$" : opmax%("UPS$") = 1
str2op$("NOT")    = "NOT" : op2str$("NOT") = "NOT"
str2op$("AND")    = "AND"
str2op$("OR")     = "OR" : op2str$("OR") = "OR"
str2op$("==")     = "==" : op2str$("==") = "=": 'boolean comparisong
str2op$("<")      = "<"  : op2str$("<") = "<"
str2op$(">")      = ">"  : op2str$(">") = ">"
str2op$("MOD")    = "%"  : op2str$("%") = "MOD"
str2op$("VAL")    = "VAL": op2str$("VAL") = "VAL"   : opmax%("VAL")  = 1
str2op$("INT")    = "INT": op2str$("INT") = "INT"   : opmax%("INT")  = 1
str2op$("CINT")   = "CINT":op2str$("CINT") = "CINT" : opmax%("CINT") = 1
str2op$("CSNG")   = "CSNG":op2str$("CSNG") = "CSNG" : opmax%("CSNG") = 1
str2op$("LEN")    = "LEN" :op2str$("LEN") = "LEN"   : opmax%("LEN") = 1
str2op$("MID$")   = "MID$":op2str$("MID$") = "MID$" : opmax%("MID$") = 3
str2op$("RND")    = "RND" :op2str$("RND")  = "RND"  : opmax%("RND") = 1
op2str$("=")    = ":="
op2str$("#")    = "LINE": statement("#") = 1
op2str$("SWAP") = "SWAP"
op2str$("^")    = "POW"
op2str$("XOR")  = "XOR"
op2str$("CHR$") = "CHR$"
op2str$("ASC")  = "ASC"
op2str$("~-")   = "~-" : 'unary minus
op2str$("ABS")  = "ABS" : 'absolute int value
op2str$("BIN$") = "BIN$" : 'int to binary string
op2str$("<>")   = "<>" : 'boolean comparison
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
op2off("DATA")  = 47 : 'push DATA at program load
op2off("ON")    = 48 : 'ON expr GOTO line1, line2, ...
op2off("RND")   = 49 : 'RND/1(upper) random float from [0;upper(
op2off("NEXT")  = 50 : 'NEXT/0, NEXT/1
'ranked from strongest (high) to lowest (1) binding:
precedence%("~-")   = 10
precedence%("ABS")  = 9 : precedence%("ASC")  = 9 : precedence%("ATN") = 9 : precedence%("BIN$") = 9
precedence%("CHR$") = 9 : precedence%("CINT") = 9 : precedence%("COS") = 9 : precedence%("CSNG") = 9
precedence%("D2R")  = 9 : precedence%("DATE$") = 9 : precedence%("EOF") = 9 : precedence%("EXP")  = 9
precedence%("HEX$") = 9 : precedence%("INKEY$")= 9 : precedence%("INPUT$") = 9: precedence%("INSTR") = 9
precedence%("INT") = 9 : precedence%("ITM") = 9 : precedence%("LEFT$") = 9: precedence%("LEN") = 9
precedence%("LG") = 9
precedence%("MID$") = 9 : precedence%("RND") = 9
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
if user$ = "jbomadsen" then inp$ = "tron:10 i=i+1: on (i<>0)+1 goto 1000,2000: 1000 & 1000:END:2000 &2000:END"
if user$ = "jbomadsen" then inp$ = "tron:on int(rnd(3)) goto 10,20:& "+quot$+"no rnd jmp"+quot$+":10 & 10:20 &20"
' you can inspect/alter the stopvar of the FOR loop:
if user$ = "jbomadsen" then inp$ = "for i=3 to 4: & i: sleep 1: next i: & varstops(chr$(36)+chr$(1)+chr$(105))"
'if user$ = "jbomadsen" then inp$ = "tron:10 i=i+1:& 123"
print "Supported statements: ","X=Y  DATA  END  FOR..NEXT  IF..THEN  GOSUB  GOTO  GO TO  ON..GOTO PRINT  RETURN  SLEEP  TROFF  TRON"
print "NOT supported statements: CLEAR, CLOSE, CLS, COLOR, DEF, DIM, FOR, HOME, INPUT, LOCATE, OPEN, ..."
print "Supported functions: ABS(d)  ATN(d)  ASC(s)  BIN$(d)  CHR$(d)  CINT(sd)  COS(d)  CSNG(sd)  INT(sd), LEN(s)  MID$(s,d)  MID$(s,d,d)  SIN(d)  VAL(sd)"
print "NOT supported functions: D2R, EOF, EXP, HEX$, INKEY$, INPUT$, INSTR, ITM, LEFT$, LG, LIN,"
print "                         LN, LOG, LOG10, NINT, NUM, OCT$, PEEK, POLKEY$, POS, R2D, REC, RIGHT$, RND,"
print "                         SGN, SPACE$, SPC$, SQR, SQRT, STR$, STRING$, TAB, TAB$, TAN, TH_*, TIM, TIME$, TIMER, TYP, UPS$"
print "Supported operators: ","+  *  /  -  ** ^  =  <>  <  >  <= >= AND NOT  OR  XOR  IMP  EQV MOD"
print "Supported literals:  1 (float64),  1.2 (float64), "+quot$+"string"+quot$+",  &2200 &1B (hex) "
if user$ <> "xjbomadsen" and inp$ <> "" then print "Example program: "+ inp$
10 if (user$<> "jbomadsen") or (inp$ = "") then input "Type your program: ", inp$
if inp$="chain" then goto 60000: 'TODO special demo
print "INPUT PROGRAM:"
print csi$+"48;5;11;38;5;18m"+inp$ : print bccolor$;
gosub 6000 : ' lex & parse
gosub 600  : ' interpret
goto 10    : ' read another line

600 '  INTERPRETER

if bytecode$ <> "" then tape$ = bytecode$

print "bytecode tape len: ", len(tape$)

erase mem$
trace% = 0
erase callstack$
callstack_frame% = 0
erase operands$
tape_scanned = 0
erase offset_next_line%
offset_next_line%(0) = 1
700 'in order for GOTO/GOSUB to work we need to prescan the tape for labels
if trace% or (not tape_scanned) then print string$(width, "*")
operands_c% = 0
actual_line% = 0
print "off","typ","len","val/t","opc","STACKSTATES"
for tape_off% = 1 to len(tape$) step 2
  if trace% or (not tape_scanned) then print tape_off%, type2str$(fntyp%(tape$, tape_off%)), fnlen%(tape$, tape_off%), fnval$(tape$, tape_off%)+"/"+op2str$(fnval$(tape$, tape_off%)), operands_c% ;
  if trace% or (not tape_scanned) then for pxi = 1 to pstate_i% : print pstack%(pstate_i%); : next pxi: print
    oval$ = fnval$(tape$, tape_off%)
    if (tape_scanned=0) or ((fntyp%(tape$, tape_off%) <> type_ptr%) and (fntyp%(tape$, tape_off%) <> type_ptrkey%)) then 750
      arity = val(fnval$(operands$(operands_c%-1),1))
      arrkey$ = fntlv$(type_str%, fnval$(tape$, tape_off%))
      if arity then for i = operands_c%-1-arity to operands_c%-2 : arrkey$=arrkey$+fntlv$(type_str%, fnval$(operands$(i),1)) : next i
      if trace% then & "POINTER DEREF: ", arrkey$, "->", mem$( arrkey$ ), len(mem$(arrkey$))
      operands_c% = operands_c% - 1 - arity + 1
      if operands_c% < 1 then stop
      if (fntyp%(tape$, tape_off%) = type_ptrkey%) then operands$(operands_c%-1) = fntlv$(type_str%, arrkey$): goto 750
      operands$(operands_c%-1) = mem$( arrkey$ )
      ' Undefined variables default to 0 or "" depending on their type:
      if (operands$(operands_c%-1) = "") and (right$(fnval$(tape$, tape_off%),1)="$") then operands$(operands_c%-1) = fntlv$(type_str%, "")
      if (operands$(operands_c%-1) = "") then operands$(operands_c%-1) = fntlv$(type_int%, 0)
750 if fntyp%(tape$, tape_off%) = type_str% then operands$(operands_c%) = fnid$(tape$, tape_off%) : operands_c% = operands_c% + 1
    if fntyp%(tape$, tape_off%) = type_int% then operands$(operands_c%) = fnid$(tape$, tape_off%) : operands_c% = operands_c% + 1
    if not (fntyp%(tape$, tape_off%) = type_opcode%) then 800
    if not op2off(fnval$(tape$, tape_off%)) then print "Unknown operator: "+fnval$(tape$, tape_off%) : stop
    if (operands_c% - opmin%(fnval$(tape$, tape_off%)) - (9=precedence%(fnval$(tape$, tape_off%)))) < 0 then print "Arity underflow:", operands_c%, op2str$(fnval$(tape$, tape_off%))+"/"+str$(opmin%(fnval$(tape$, tape_off%))):stop
    if (opmax%(fnval$(tape$, tape_off%))) and (9<>precedence%(fnval$(tape$, tape_off%))) and (operands_c% - opmax%(fnval$(tape$, tape_off%))) > 0 then print "Arity overflow:", operands_c%, op2str$(fnval$(tape$, tape_off%))+"/"+str$(opmax%(fnval$(tape$, tape_off%))):stop
    if (tape_scanned = 0) and (fnval$(tape$, tape_off%)="#") then gosub 1300 : offset_next_line%(actual_line) = fn_next_off%(tape$, tape_off%)-1 : print "next from",actual_line%,offset_next_line%(actual_line%): actual_line% = actual_line% + 1 :goto 800
    if tape_scanned = 0 then goto 800
    on op2off(fnval$(tape$, tape_off%)) gosub 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 4950, 5000, 5050, 5100, 5150, 5200, 5250, 5300, 5350, 5400
800  tape_off% = fn_next_off%(tape$, tape_off%)
    if statement(oval$) then operands_c% = 0 : 'clear operands if op was a statement
next tape_off%
if tape_scanned = 0 then tape_scanned = 1 : goto 700

print "PROGRAM FINISHED."
inp$ = ""
return

900 ' dereference pointer
     stop
     return
1000 ' PRINT
     if operands_c% = 0 then print: return
     print csi$+"48;5;200;37;1m";
     for p% = 0 to operands_c% -1 'to 0 step -1
       print string$(p%>0,chr$(9))+fnval$(operands$(p%),1) ;
     next p%
     print csi$+"m"+bccolor$
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
   if operands_c% < 3 then STOP : 'arity error
  ' TODO we should now be able to use type_ptrkey% to retrieve lhs_key$ instead of having to do it here:
   arity = val(fnval$(operands$(operands_c%-3),1))
   if trace% then & "assignment lhs arity:", arity, operands_c%, operands$(operands_c%-2)
   lhs_key$ = fntlv$(type_str%, fnval$(operands$(operands_c%-2),1))
   if arity then for lhs_i% = 0 to operands_c%-4 : lhs_key$ = lhs_key$ + fntlv$(type_str%, fnval$(operands$(lhs_i%),1)): next lhs_i%
   rhs% = operands_c% - 1
   'if fntyp%(operands$(lhs%), 1) 
   'if fntyp%(operands$(lhs%), 1) <> type_str% then STOP : 'type error
   if trace% then print "ASSIGNMENT",lhs_key$,th_b64e$(lhs_key$),":=",fnval$(operands$(rhs%),1)
   ' TODO vars used to use plain key, now they are fntlv(type_str$, previous):
   mem$( lhs_key$ ) = operands$(rhs%)
   if trace% then & "assignment set", lhs_key$, " to ", mem$( lhs_key$) 
   return
1300 'line number /label (for goto, gosub, ...)
   ' since this is a VM instruction, it gets evaluated/updated each time this instruction is executed,
   ' which means that "10 & 1: 10 & 2: goto 10" will print  1 2 2 2 ..., as opposed to "2 2 2" in
   ' regular basic variants where the last 10 would override the first.
   ' we utilize this in FOR looparr(x%)= .. NEXT where we can't evaluate x% statically
   if (tape_scanned=0) and (operands_c% > 1) then print "scanning of dynamic labels skips": return
   if operands_c% > 1 then & operands$(operands_c%-2): stop : 'why does the line have more than one arg?
   if operands_c% < 1 then STOP : 'arity error
   ' point to the label itself, that way GOTO etc can mutate tape_off% and return
   mem$( "line", fnval$(operands$(operands_c%-1),1)) = fntlv$(type_int%, tape_off%) : 'fn_next_off%(tape$, tape_off%)+2
   operands_c% = 0
   return
1400 'GOTO. NB in TH BASIC 8000.00003 denotes 4th line after 8000.00000, -0.99999 is the first line of unnumbered program
   target$ = mem$( "line",fnval$(operands$(operands_c%-1),1))
   if target$ = "" then & "no such line:"+fnval$(operands$(operands_c%-1),1): stop
   tape_off% = val( fnval$(target$,1)  )
   return
1500 'SLEEP
   if fntyp%(operands$(operands_c%-1),1) <> type_int% then STOP : 'type error
   if trace% then print "SLEEP opcnt:", operands_c%
   SLEEP fnval$(operands$(operands_c%-1),1)
   return
1600 'GOSUB
   ' subtract 1 (we are pointing to the GOSUB instruction, which is 1 byte long):
   callstack$( callstack_frame% ) = fntlv$(type_int%, fn_next_off%(tape$, tape_off%)-1)
   callstack_frame% = callstack_frame% + 1
   tape_off% = val( fnval$( mem$( "line",fnval$(operands$(operands_c%-1),1)   )  ,1))
   if trace% then print "GOSUB", tape_off%
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
    if fntyp%(operands$(a%), 1) <> type_str% then print type2str$(fntyp%(operands$(a%), 1)) : stop : 'type error
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
    if trace% then print "unary minus", operands$(a%), a_val%, "=>", -a_val%
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
   row_data = val(mem$("data_row"))
   for var = 0 to operands_c% -1
     if trace% then print "READ: operands:",operands_c%,fnval$(operands$(var),1),"row_data:"+str$(row_data),"col_data:"+str$(col_data)
     if val(mem$("data_line")) <= no_read% then & val(mem$("data_line")), no_read%:stop : ' out of bounds read (row)
     'if val(mem$("data_row")) >= col_read% then stop : 'out of bounds read (column)
     this$ = mem$( "data", row_data, col_data )
     ' Cast whatever to string if it assignee with $, otherwise cast to float:
     ' TODO this detection strategy does presumably not work for arrays
     this_should_be_str = right$(fnval$(operands$(var),1),1) = "$"
     if this_should_be_str then this$ = fntlv$(type_str%, fnval$(this$,1))
     if not this_should_be_str then this$ = fntlv$(type_int%, val(fnval$(this$,1)))
     mem$( operands$(var) ) = this$ : ' TODO used to be fnval()
     if trace% then & fnval$(operands$(var),1) + " := " + this$
     col_data = col_data + 1
   next var
   mem$("data_row") = str$(row_data + 1)
   return
5250 'DATA (push DATA array at program load)
   if operands_c% < 1 then STOP :'arity error
   numargs% = val(fnval$(operands$(operands_c% -1),1))
   if numargs% = 0 then operands_c% = 0 : return
   for a% = 0 to numargs% - 1
     mem$( "data", val(mem$("data_line")), a%) = operands$(operands_c%-2-a%)
   next a%
   mem$("data_line") = str$(val(mem$("data_line")) + 1)
   return
5300 ' ON .. GOTO
   selector = val(fnval$(operands$(0),1))
   if (selector = 0) or (selector > operands_c%) then print "ON..GOTO oob": return
   & operands_c%, selector, operands$(selector)
   operands$(0) = operands$(selector) : operands_c% = 1
   goto 1400 : ' actual GOTO
5350 ' RND
   operands_c% = operands_c% - 1
   operands$(operands_c%-1) = fntlv$(type_int%, RND(val(fnval$(operands$(operands_c%-1),1))))
   return
5400 ' NEXT
   ' we have the mem reference in operands$(0)
   ' - we need to bump it
   loopvar% = val( fnval$(mem$(fnval$(operands$(0),1)),1) )
   loopvar% = loopvar% + 1
   mem$( fnval$(operands$(0),1) ) = fntlv$(type_int%, loopvar%)
   ' then retriev stopvar: TODOTODO
   ptrstop$ = fntlv$(type_str%, "varstops") + operands$(0)
   stopvar% = val(fnval$(mem$(ptrstop$),1))
   'stopvar% = val(fnval$( fntlv$(type_str%, "varstops", mem$(operands$(0)))  ,1))
   ' - then compare it
   if loopvar% > stopvar% then return
   ' - then goto 1400 (real GOTO):
   '& "next operands", operands$(0), operands_c%
   goto 1400
   stop : 'not implemented
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
     pstate%("data")       = 7 : state2str$(7) = "data"
     pstate%("on")         = 8 : state2str$(8) = "on" : 'kicks in after parsing "ON" (expr) "GOTO/GOSUB"
     pstate%("for")        = 9 : state2str$(9) = "for": 'kicks in after TO

     pstate%("ptr2ref")    =99 : state2str$(99) = "ptr2ref" : ' TODO unsure if this still gets used for anything
     erase pstack% : 'parent stack
     pstack_i% = 1
     pstack%(pstack_i%) = pstate%("label")

     string_builder% = 0

     no_data% = 0
     erase compiled_data$

     erase compiled$ : 'stack of compiled tokens (for RPN compilation)
     compiled_c% = 0

     erase expr_expect_stack%
     expr_expect_c% = 0

     for ci% = 1 to len(inp$)
       ch$ = mid$(inp$,ci%,1)
       print th_sprintf$("=> ci=%-4d pstack_i=%-3d exprEXP(%d-1)=%s ", ci%, pstack_i%, expr_expect_c%, type2str$(expr_expect_stack%(expr_expect_c%-1)));
       for pstki=1 to pstack_i% : print state2str$(pstack%(pstki))+" "; : next pstki : print "ch="+ch$
       'if (pstack%(pstack_i%)=pstate%("expr")) and (mid$(inp$,ci%,1) = ":") then print "expr at end of statement, draining op stack then compiling"
       if (pstack%(pstack_i%)=pstate%("expr")) and (mid$(inp$,ci%,1) = ":") then gosub 10900 : gosub 7400 : goto 7000
       if (pstack_i% = 2) and (mid$(inp$,ci%,1) = ":") then gosub 7400 : goto 7000
       if (pstack%(pstack_i%-1) = pstate%("statement")) and (mid$(inp$,ci%,1) = ":") then gosub 7400 : goto 7000
       if (pstack%(pstack_i%) < 1) or (pstack%(pstack_i%) > 9) then print pstack%(pstack_i%) : stop
       on pstack%(pstack_i%) gosub 7500, 8000, 10000, 11000, 9000, 12000, 13000, 14000, 15000
       if (pstack%(pstack_i%)=pstate%("expr")) and (ci%=len(inp$)) then print "expr at EOF", operator_c% : gosub 10900
       print th_sprintf$("<= ci=%-4d pstack_i=%-3d exprEXP(%d-1)=%s ",ci%,pstack_i%, expr_expect_c%, type2str$(expr_expect_stack%(expr_expect_c%-1)));
       for pstki=1 to pstack_i% : print state2str$(pstack%(pstki))+" "; : next pstki : print

7000  next ci%
     & "done with ci loop, about to hit last 7400 drain", compiled_c%
     gosub 7400
     bytecode$ = bytecode$ + fntlv$(type_int%,"EOL") + fntlv$(type_opcode%, "#")
     if no_data% = 0 then RETURN : 'end of parsing
     print "bytecode with data is now", bytecode$
     for di% = no_data% to 1 step -1
       print "COMPILED DATA:", di%, dr%, "=>", compiled_data$(di%, dr%)
       dr% = 0
       this$ = ""
       for loop_7050 = 0 to 0 step 0
       if compiled_data$(di%, dr%) <> "" then this$ = compiled_data$(di%, dr%) + this$ : dr% = dr% + 1 : next loop_7050
     bytecode$ = this$ + fntlv$(type_int%, dr%) + fntlv$(type_opcode%, "DATA") + bytecode$
     print "di%",di%,"bytecode",bytecode$
     next di%
     RETURN : ' end of parsing (with DATA)

7400 'emit compile stack at end of statement parsing
     if (compiled_c%>1) and (compiled$(0) = fntlv$(type_opcode%,"NEXT")) and (fntyp%(compiled$(compiled_c%-1),1)=type_ptr%) then compiled$(compiled_c%-1)=fntlv$(type_ptrkey%,fnval$(compiled$(compiled_c%-1),1))
7430 if (pstack_i%=3) and (pstack%(3)=pstate%("expr")) and (pstack%(2)=pstate%("statement") or pstack%(2)=pstate%("assignment")) then print "PSTACK", pstack_i%, pstack%(2), pstack%(3) : goto 7440 : 'unclosed parent
     if not ((pstack_i% > 2) and (compiled_c% > 0) and (fntyp%(compiled$(0),1)=type_opcode%) and (fnval$(compiled$(0),1)="IF")) then 7435
       'print "\\\\\\\\\\ IF: compiled (cond) ; IF ; statement2; ... ; until [label]"
       for ps=1 to pstack_i%:print state2str$(pstack%(ps)):next ps
       for c = 0 to compiled_c%-1
          print c, fnval$(compiled$(c),1)
       next
       'print type2str$(fntyp%(compiled$(0),1)),"FNVAL:",fnval$(compiled$(0),1)
       stop
7435 if (pstack_i%=3) and (pstack%(3)=pstate%("data")) then pstack_i% = pstack_i% - 1: return : 'end of a DATA statement
     if not ((pstack_i%=4) and pstack%(3)=pstate%("ptr2ref")) then 7440
     ' TODO this is some hacky shit right here, rewriting ptr to str.
     ' presently used in READ, but same problem exists in ASSIGNMENT, INPUT, etc where we want the literal ptr
     for c = 0 to compiled_c%
       if fntyp%(compiled$(c),1) = type_ptr% then compiled$(c) = fntlv$(type_str%, fnval$(compiled$(c),1))
     next
     'print "PTR2REF",compiled_c%
7440 if compiled_c% < 2 then 7460
     print string$(40,"_")+"EMITTING COMPILED",compiled_c%
     for compile_i% = 1 to compiled_c%-1
       print "","compiled:",fnval$(compiled$(compile_i%),1), "ie:", compiled$(compile_i%)
       bytecode$ = bytecode$ + compiled$(compile_i%)
     next compile_i%
7460 if compiled_c% then bytecode$ = bytecode$ + compiled$(0)
     compiled_c% = 0
     pstack_i% = 1
     pstack%(pstack_i%) = pstate%("label")
     RETURN
7465 'reloc from 7439 TODOTODO
     if (pstack_i% = 5) and (pstack%(pstack_i%-2)=pstate%("on")) then & "its on"
     if pstack_i% <> 2 then print "PSTACK<>2",pstack_i% :for ps=1 to pstack_i%:print ps,state2str$(pstack%(ps)):next ps: stop
7500 'label parsing
     if fnwhitespace%(ci%) then RETURN
     if expr_expect_c% then print "WHY ARE WE EXPECTING things" : STOP
     label$ = ""
     for labelagain = 0 to 0 step 0: if fnnum%(ci%) then label$ = label$ + mid$(inp$, ci%, 1) : ci% = ci% + 1 : next labelagain
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
     'TODO it would be nice to build this regex from our table of statements:
     found$ = th_re$(lookahead$, "^(DATA|END|&|PRINT|SLEEP|GO\s*TO|GOSUB|TO|NEXT|RETURN|REM|READ|'|IF|ON|FOR|TRON|TROFF)", 1, "i")
     ' if it's not a statement keyword, it's either a syntax error or an assignment:
     if found$ = "" then compiled$(compiled_c%) = fntlv$(type_opcode%, "=") : compiled_c% = compiled_c + 1
     if found$ = "" then pstack%(pstack_i%) = pstate%("assignment") : ' look for =
     if found$ = "" then pstack_i% = pstack_i%+1 : pstack%(pstack_i%) = pstate%("expr") : 'look for LHS
     'if found$ = "" then pstack_i% = pstack_i%+1 : pstack%(pstack_i%) = pstate%("identifier") : 'look for LHS
     if found$ = "" then ci%=ci%-1 : return
     found$ = UPS$(found$) : 'helps with RE being case-insensitive, is that needed?
     if (found$ = "REM") or (found$="'") then ci%=len(inp$) : RETURN : 'comment to the end of the line TODO
     print "statement:", ci%, found$
     opname$ = str2op$(th_sed$(found$, "\s+", "", "g")) : 'go to -> GOTO
     if not statement(opname$) then print "not a statement: "+found$,opname$: stop
     if opname$ = "" then STOP : 'didn't find this operator
     ci% = ci% + len(found$) -1
     
     'FOR parses: "FOR" [assignment] "TO" [expr1] ?STEP expr2?
     'to AST: LABEL [assignment.lhs_key$] [assignment]
     'and registers "NEXT" [lhs_key$] as lhs_key$=lhs_key$+expr2: ON (lhs_key$ <= [expr]) GOTO [lhs_key$]
     'that means we need to implement ON..GOTO first, TODO
     REM assignment:
     REM compiled$() = fntlv$(type_str%, lhs_key%) : ' textual lines?
     REM compiled$() = fntlv$(type_opcode%, "#")
     REM ...
     REM compiled$() = fntlv$(type_int%, ) : 'arity of assignment
     REM compiled$() = fntlv$(type_opcode%, ":=")
     REM
     REM compiled$() = lhs_key$ :' this needs to be the ptr to the lhs_key$, might be easier to just generate a deterministic id for the FOR based on tape offset or w/ever
     REM compiled$() = expr
     REM compiled$() = fntlv$(type_opcode%, "<=")
     REM lhs_key$ :'todo need to make sure this works for strings
     REM ON
     if opname$ = "FOR" then & "parsing FOR, letting it fall through to TO"
     if opname$ = "FOR" then compiled$(compiled_c%) = fntlv$(type_opcode%, "=") : compiled_c% = compiled_c + 1
     if opname$ = "FOR" then pstack_i%=pstack_i%+3:pstack%(pstack_i%-2)=pstate%("for"):pstack%(pstack_i%-1)=pstate%("assignment"):pstack%(pstack_i%)=pstate%("expr"):return
     ' DATA gets pushed in front of the program:
     if opname$ = "DATA" then no_data% = no_data% + 1 : rec_data% = 0 : compiled_data$(no_data%, rec_data%) = ""
     if opname$ = "DATA" then pstack_i% = pstack_i% + 1 : pstack%(pstack_i%) = pstate%("data") : RETURN
     if opname$ = "ON" then pstack_i% = pstack_i% + 1: pstack%(pstack_i%) = pstate%("on")
     print "STATEMENT-COMPILED_C%",compiled_c%
     compiled$(compiled_c%) = fntlv$(type_opcode%, opname$)
     compiled_c% = compiled_c% + 1
     pstack_i% = pstack_i% + 1
     pstack%(pstack_i%) = pstate%("expr")
     if opname$ = "READ" then pstack%(pstack_i%)=pstate%("ptr2ref"):pstack_i%=pstack_i%+1:pstack%(pstack_i%)=pstate%("expr"):pstack_i%=pstack_i%+1:pstack%(pstack_i%) = pstate%("identifier")
     expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
     RETURN
9000 'assignment
     if fnwhitespace%(ci%) then RETURN
     ' There's a grammar conflict here where
     'a(1)=2 ends up as [a] [(1)==2]
     'if ch$ = "(" then expr_expect_stack%(expr_expect_c%) = type_int%: expr_expect_c% = expr_expect_c% + 1
     'if ch$ = "(" then pstack_i%=pstack_i%+1:pstack%(pstack_i%) = pstate%("expr"): ci%=ci%-1:RETURN
     'going to tack it onto the identifer parser instead
     if ch$ <> "=" then STOP : 'expect = here
     'LHS has just been compiled to a ptr%, but we are not dereferencing it when assigning,
     'so we need to fix that:
     & "assignment opc", operator_c%
     if compiled$(0) <> fntlv$(type_opcode%, "=" ) then print "assignment without =", compiled$(0), compiled$(1), compiled_c%:stop
     if compiled_c% < 2 then stop : ' UNREACH, assignment arity is missing
     arity = val(fnval$(compiled$(compiled_c%-2),1))
     & "in assignment opc:"+str$(operator_c%) +" compiled_c%:"+str$(compiled_c%)
     'if compiled_c% > 4 then arity=arity: compiled$(compiled_c%-2) = fntlv$(type_int%, arity)
     last_assignment_offset = compiled_c% ' this gets reused for for-loops, which is kind of hacky.
     & "assignment =>", "arity:"+str$(arity),"compiled:"+str$(compiled_c%), "assoff:"+str$(last_assignment_offset)
     for c = 0 to compiled_c%-1
        & "assignment:: ", c, compiled$(c)
     next c
9050 if fntyp%(compiled$(compiled_c%-1),1) <> type_ptr% then & compiled_c%, type2str$(fntyp%(compiled$(2),1)) : STOP : 'expected an identifier ptr
     identifier$ = fnval$(compiled$(compiled_c%-1),1)
     compiled$(compiled_c%-1) = fntlv$(type_str%, identifier$)
     ' 0: =
     ' 1: arity, 0=variable, 1<=array
     ' 2: base name of array/variable
     ' need to compile this stuff in reverse
     'now find RHS:
     pstack%(pstack_i%) = pstate%("expr")
     print "assignLHS", right$(identifier$,1)
     expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
     if right$(identifier$,1) = "$" then expr_expect_stack%(expr_expect_c%-1) = type_str%
     return
10000 'expr
      if fnwhitespace%(ci%) then return
      expr_stack_c% = 0
10010 ch$ = mid$(inp$, ci%, 1)
      'print "expr:"+ch$
      'if ch$ = "(" then print "pushing ( to operator stack"
        if ch$ = "(" then & "open bracket when last was", operator_c%, operator_stack$(operator_c%-1)
        if ch$ = "(" then comma_stack%(operator_c%) = 0
        if ch$ = "(" then operator_stack$(operator_c%) = "(": operator_c% = operator_c% + 1
        ' if previous token was a ptr% then this is an array: fntyp%(operator_stack$(operator_c%-2),1) = type_ptr%
        if ch$ = "(" then expr_expect_stack%(expr_expect_c%) = type_int% : expr_expect_c% = expr_expect_c% + 1
        if ch$ = "(" then RETURN
      if (ch$ <> ")") then 10050 : 'unbalanced )
      if ch$ = ")" then if expr_expect_stack%(expr_expect_c% -1) <> type_opcode% then print "close-')' when expecting:",type2str$(expr_expect_stack%(expr_expect_c% -1)): stop
      for loop_10030 = 0 to 0 step 0
      if not operator_c% then print "unbalanced right-')'",operator_c% : STOP
      ' case: ch$ is a ) closing bracket
           'print "first)", operator_c%, operator_stack$(operator_c% - 1)
           lastop$ = operator_stack$(operator_c%-1)
           if lastop$ = "(" then 10040
           if lastop$ = "" then stop
           'if 9 = precedence%(operator_stack$(operator_c%-1)) then print "prec 9 so output arity operand: ",comma_stack%(operator_c%-1)
           if fn_has_arity(lastop$) then compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1))
           if fn_has_arity(lastop$) then & "lastop has arity, compiling:", compiled$(compiled_c%)
           if fn_has_arity(lastop$) then compiled_c% = compiled_c% + 1: comma_stack%(operator_c%-1) = 0
           if fntyp%(lastop$,1)  = type_ptr% then & "  compiling ptr",lastop$,len(lastop$) : compiled$(compiled_c%) = lastop$
           if fntyp%(lastop$,1) <> type_ptr% then & "  compiling op",lastop$:compiled$(compiled_c%) = fntlv$(type_opcode%, lastop$)
           print "output:"+compiled$(compiled_c%), type2str$(fntyp%(lastop$,1)), "witharity:"+str$(fn_has_arity(lastop$))
           compiled_c% = compiled_c% + 1
           operator_c% = operator_c% - 1
           if (operator_c% = 0) and fntyp%(lastop$,1) = type_ptr% then & "TODOTODO this is a hack: early return when ptr met on close-), why??": return
           next loop_10030
10040      print "in )",operator_c%, operator_stack$(operator_c%-1), operator_stack$(operator_c%-2)
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
           ' TODO stop here?
           return
10050      if not ((ch$ = ",")) then 10100
        ' comma encountered while parsing an expression. this could be starting
        ' a new expression in an operand list, or a syntax error
      for drain_after_10050 = 0 to 1
      if drain_after_10050 then gosub 10900: expr_expect_stack%(0) = type_int% : expr_expect_c% = 1: return
      if pstack%(pstack_i%-1) = pstate%("expr") then print "COMMA INSIDE EXPRESSION" : stop : 'that's a syntax error
      if (operator_c% = 0) and (pstack%(pstack_i%-1) = pstate%("assignment")) then print "COMMA AFTER ASSIGNMENT", operator_c% : stop
      if pstack%(pstack_i%-1) = pstate%("assignment") then & "10050:comma in assignment": goto 10070
      if pstack%(pstack_i%-1) = pstate%("ptr2ref") then print "COMMA IN ptr2ref",compiled$(compiled_c%-1):compiled$(compiled_c%)=fntlv$(type_str%,fnval$(compiled$(compiled_c%-1),1)):gosub 10900:return
      if pstack%(pstack_i%-1) = pstate%("on") then print "comma in ON..GO(TO|SUB)": next drain_after_10050
      if pstack%(pstack_i%-1) <> pstate%("statement") then print pstack_i%, pstack%(pstack_i%-1): stop
      next drain_after_10050

10070 'comma encountered in assignment -> expr
      gosub 10900
      & "assignment compiled_c%",compiled_c%, "comma_stack(0):", comma_stack%(0)
      expr_expect_c% = 1
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      return
      
10100 ' TODO check that () isn't an empty expr
      'print "not () expr:"+ch$,"op stack height:",operator_c%
      'TODO if this we have not emitted anything, an infix operator here is a syntax error.
      'TODO if the last thing we emitted was a value, another value is a syntax error.
      if (ch$ = ")") then stop :' pstack_i% = pstack_i% - 1 : RETURN : 'close expr
      if ch$ = quot$ then pstack_i% = pstack_i% + 1 : pstack%(pstack_i%) = pstate%("string") : string_builder% = ci%+1 : RETURN
      ' it might be an int:
      lookahead$ = ups$(mid$(inp$, ci%, 20))
      foundhex$ = th_re$(lookahead$, "^&([0-9A-F]+)", 2) : 'GW-BASIC style hex number
      found$ = th_re$(lookahead$, "^\d+\.?\d*", 1)
      if found$+foundhex$ = "" then 10200
         ' found an int, put it on compile stack
         if expr_expect_stack%(expr_expect_c%-1) <> type_int% then print "NOT EXPECTING VAL ", found$, expr_expect_stack%(expr_expect_c%-1), expr_expect_c%:STOP : 'not expecting value
         'if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "expecting operator got int", expr_expect_c%: stop
         expr_expect_stack%(expr_expect_c%-1) = type_opcode%
         if foundhex$ <> "" then found$ = th_sprintf$("%y", foundhex$) : ci% = ci% + 1
         ci% = ci% + len(found$) -1
         'print "output int:"+found$, string$(foundhex$<>"", "(&HEX)")
         compiled$(compiled_c%) = fntlv$(type_int%, found$)
         compiled_c% = compiled_c% + 1
         RETURN
      'it's not a string, it's not an int, it COULD BE infix operator OR identifier:
10200 operator_re$ = "^ *(AND|OR|XOR|MOD|NOT|IMP|EQV|<>|<=|=<|>=|=>|\*\*|[=+/*^<>-])"
      found$ = th_re$(lookahead$, operator_re$, 2)
      if found$ = "" then 10500
      found_len% = len(th_re$(lookahead$, operator_re$, 1))
      if (found$ = "=") and (pstack%(pstack_i%-1)=pstate%("assignment")) and (expr_expect_c%=0) then & "pretty SURE THIS IS :=", expr_expect_c%, operator_c%: pstack_i%=pstack_i%-1:ci%=ci%-1:gosub 10900:return
      if found$ = "=" then found$ = "==" : 'disambiguate comparison from assignment
      if found$ = "**" then found$ = "^" : 'alias for ^
      ci% = ci% + found_len% - 1
      print "OP found:"+found$+".", found_len%
      ' operator found:
      ' - while ( there is an operator on the stack that is not (
      '         ( AND ( (o2 has greater precedence than o1)
      '         (        or (o1 and o2 have same precedence AND o1 is left-associative))
      ' - pop o2 from operator stack into output queue:
      for opstack_empty_10300 = 0 to 0 step 0
      if operator_c% then 10400
      if found$="==" and expr_expect_c%=0 then pstack_i%=pstack_i%-1: ci%=ci%-1:return : 'plain assignment ? TODO
      if found$="==" and (expr_expect_c%=1 and operator_stack$(operator_c%)="(") then pstack_i%=pstack_i%-1:ci%=ci%-1:return : 'plain assignment?
      'print "no opers on stack but found and push:"+found$, expr_expect_stack%(expr_expect_c%-1), expr_expect_c%
      if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1
      if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then expr_expect_stack%(expr_expect_c%-1) = type_int%: return
        if found$ = "-" then operator_stack$(operator_c%) = "~-" : operator_c% = operator_c% + 1: return
        if found$ = "NOT" then operator_stack$(operator_c%) = "NOT" : operator_c% = operator_c% + 1 : return
        print "expected value got operator:"+found$ : stop

10400 top$ = operator_stack$(operator_c%-1)
      print th_sprintf$("OP IS AN OPERATOR:[%d]:%s:(%d)  stack top[%d]:%s:(%d)", len(found$), found$,precedence%(found$), len(top$), top$,precedence%(top$))
      ' TODO this here is a mess, should use lookup tables instead of hardcoding the operators:
      dopop = 0
      ' 9: is the precedence of functions:
      precedence_found% = precedence%(found$) + 9*(fntyp%(found$,1) = type_ptr%)
      precedence_top%   = precedence%(top$)   + 9*(fntyp%(top$,1)   = type_ptr%)
      if precedence_found% = 0 then stop : ' TOOD can found$ ever be a ptr%? then we have to fixup precedence for that too
      if precedence_top%   = 0 then stop
      if precedence_found% <= precedence_top% then dopop = 1
      if precedence_found% > precedence_top% then dopop = -1
      if (dopop = 0) and (found$ <> "^") then dopop = 1 : 'print "equal precedence level, "+found$+" is left-associative."
      'if dopop then print "precedence: ",found$+string$(dopop=1," <= ")+string$(dopop=-1, " > ")+top$
      if found$ = "NOT" then print "GOTNOT dopop <- -1, dopop=", dopop : dopop=-1
      if (found$ = "-") and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then operator_stack$(operator_c%)="~-":operator_c%=operator_c%+1:return
      if top$ = "(" then dopop = -1
      'if top$ = "^" then dopop = -1 : print "NOT POPPING ^ BECAUSE IT IS RIGHT-ASSOCIATIVE"
      if dopop = 0 then stop
      if (dopop=1) and (top$ <> "(") and fn_has_arity(top$) then &"out1":compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1)): compiled_c% = compiled_c% + 1 : comma_stack%(operator_c%-1) =0
      'if (dopop=1) and (top$ <> "(") then print "   emitting "+top$
      if (dopop=1) and (top$ <> "(") then if fntyp%(top$,1)<>type_ptr% then & "  dopop(f):"top$:compiled$(compiled_c%) = fntlv$(type_opcode%, top$)
      if (dopop=1) and (top$ <> "(") then if fntyp%(top$,1)=type_ptr% then & "  dopop(a):"top$:compiled$(compiled_c%) = top$
      if (dopop=1) and (top$ <> "(") then compiled_c% = compiled_c% + 1 : operator_c% = operator_c% - 1 : next opstack_empty_10300
      if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then operator_stack$(operator_c%) = found$ : operator_c% = operator_c% + 1 : expr_expect_stack%(expr_expect_c%-1) = type_int% : return
      if found$ = "-" then operator_stack$(operator_c%) = "~-" : operator_c% = operator_c% + 1: return : 'unary MINUS
      if found$ = "NOT" then operator_stack$(operator_c%) = "NOT" : operator_c% = operator_c% + 1: return : 'unary NOT
      if (found$ = "==") and (pstack%(pstack_i%-1)=pstate%("assignment")) then & "looks like assignment from inside expr": return
      print "expect value got operator",found$ :stop

10500 'it's not a string, and it's not an integer. could be a identifier or a colon or a THEN (if we are in an IF)
      'if operator_c% = 0 then print "OPERATOR STACK EMPTY, if "+quot$+found$+quot$+"is a keyword then terminate expr, expr_expect:",expr_expect_stack%(expr_expect_c%-1), expr_expect_c%
      lookahead$ = ups$(mid$(inp$, ci%, 20))
      func$ = th_re$(lookahead$, "^(SIN|COS|ABS|ATN|ASC|BIN\$|CHR\$|CINT|CSNG|INT|ITM|LEFT\$|LEN|MID\$|FN[A-Z0-9_]+[!$%]|RIGHT\$|RND|VAL)\(", 2)
      'print "INFIX STACK", infix_c%, ch$, found$, func$
      if (func$ <> "") and expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "expected infix operator, got function:"+func$:stop
      'if func$ <> "" then print "is a function", func$
      if func$ <> "" then operator_stack$(operator_c%)=func$:operator_c% = operator_c% + 1 : ci% = ci% + len(func$)-1: return
      if left$(lookahead$,4) <> "THEN" then 10560
        'print "THEN ENCOUNTERED", pstack_i%, operator_c%, infix_c%, pstack%(pstack_i%)
        if (expr_expect_c%<>0) and (expr_expect_stack%(expr_expect_c%-1) <> type_opcode%) then print expr_expect_stack%(expr_expect_c%-1): STOP
        if infix_c% <> 0 then print "INFIX_C% <> 0": stop : 'what
        gosub 10900 : 'drain operator stack of the cond expr
        gosub 7400  : 'compile the cond expr and the IF operator
        ci% = ci% -1 + len("THEN")
        return
10560  if (ch$ = ":") and (0 < infix_c%) then print "Piling up infix", operator_stack$(operator_c%-1) : ci% = ci% - 1 : pstack_i% = pstack_i% - 1 : return
      ' TODO could also be a function call?
      pstack_i% = pstack_i% + 1
      pstack%(pstack_i%) = pstate%("identifier")
      'print "EXPR LOOKING FOR IDENTIFIER",ci%,quot$+mid$(inp$,ci%,1)+quot$
      'if expr_expect_stack%(expr_expect_c%-1) = type_opcode% then print "looking for operator got identifier?": stop
      'if expr_expect_stack%(expr_expect_c%-1) <> type_int% then print "  EXPR EXPECT",expr_expect_c%,expr_expect_stack%(expr_expect_c%-1)
      expr_expect_stack%(expr_expect_c%-1) = type_int%
      ci% = ci% - 1
      RETURN
10900 if (not operator_c%) and (pstack%(pstack_i%-1)=pstate%("for")) and (comma_stack%(0)=1) then & "for in pstack",compiled_c%,comma_stack%(0):for i=0 to compiled_c%:& i,compiled$(compiled_c%)::goto 15000
      if (not operator_c%) then & "10900: opc empty":expr_expect_c% = 0 : RETURN
      top$ = operator_stack$(operator_c%-1)
      & "at 10900 with top" top$
      if top$ <> "(" then & "top is" top$,"opc:"+str$(operator_c%):goto 10950
      ' precedence =9 is for functions:
      pretop$ = operator_stack$(operator_c%-2)
      if (operator_c% < 1) or (not fn_has_arity(pretop$)) then for i = operator_c%-1 to 0 step -1: PRINT operator_stack$(i): next i
      if (operator_c% < 1) or (not fn_has_arity(pretop$)) then print "UNMATCHED '('": STOP
      & "trying to fix up comma_stack in opc-2",operator_c%-2,str$(comma_stack%(operator_c%-2))+"+=1"
      comma_stack%(operator_c%-2) = comma_stack%(operator_c%-2) + 1
      if opmax%(pretop$) and (opmax%(pretop$) <= comma_stack%(operator_c%-2)) then print pretop$+"/"+str$(opmax%(pretop$))+": too many arguments": STOP
      if expr_expect_c% then expr_expect_stack%(expr_expect_c%-1) = type_int%
      'print "at comma, before ( on op stack is",pretop$,"which is a function. commas:",comma_stack%(operator_c%-2)
      return
10950 print "end opstack ",operator_c%,top$
      if (expr_expect_c%<>0) and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then print "at end of expr looking for int":stop
      expr_expect_c% = expr_expect_c% - (expr_expect_c% <>0 )
      'if expr_expect_c% then print "last expr expect", expr_expect_c%
      'if 9 = precedence%(top$) then print "WAS FUNC ",top$," with COMMA STACK ",comma_stack%(operator_c%-1)
      if fn_has_arity(top$) then compiled$(compiled_c%) = fntlv$(type_int%, comma_stack%(operator_c%-1))
      if fn_has_arity(top$) then compiled_c% = compiled_c% + 1 : comma_stack%(operator_c%-1) = 0
      if fntyp%(top$,1) = type_ptr% then & "    opstack end ptr:" top$: compiled$(compiled_c%) = top$
      if fntyp%(top$,1)<> type_ptr% then & "    opstack end func" top$: compiled$(compiled_c%) = fntlv$(type_opcode%, top$)
      compiled_c% = compiled_c% + 1 : operator_c% = operator_c% - 1
      GOTO 10900
11000 'string
      if ch$ <> quot$ then RETURN : 'next char
      string_builder$ = mid$(inp$, string_builder%, ci% - string_builder%)
      pstack_i% = pstack_i% - 1
      if pstack%(pstack_i%) = pstate%("data") then compiled_data$(no_data%, rec_data%) = fntlv$(type_str%, string_builder$)
      if pstack%(pstack_i%) = pstate%("data") then compiled_data$(no_data%, rec_data%+1) = "": RETURN
      if pstack%(pstack_i%) <> pstate%("expr") then STOP : ' end of string, but not parsing string/DATA
        compiled$(compiled_c%) = fntlv$(type_str%, string_builder$)
        compiled_c% = compiled_c% + 1
        expr_expect_stack%(expr_expect_c%-1) = type_opcode%
        RETURN
12000 'identifier
      if fnwhitespace%(ci%) then RETURN
      'print "IDENTIFIER LOOKING"
      ch$ = mid$(inp$, ci%, 1)
      if not th_re(ch$, "^[A-Za-z_]", 1) then print quot$+ch$+quot$: STOP : 'not valid identifier start char
      lastid% = ci%+1
      for id% = ci%+1 to len(inp$)
        ch$ = mid$(inp$, id%, 1)
        if th_re(ch$, "^[a-z_0-9$%!]", 1) then next id%
      id$ = mid$(inp$,ci%, id%-ci%)
      ci% = id%-1
      'here we need to skip whitespace such that chnext$="(" or chnext$="=", otherwise
      'a   (1) =2  will have array arity 0
      for i = ci% to len(inp$)
      chnext$ = mid$(inp$,i+1,1) : if chnext$=" " then next i
      ci% = i
      print "id="+id$+". next=" + mid$(inp$,ci%+1,1),  expr_expect_stack%(expr_expect_c%-1) 

      'TODO we could conceivably end up here inside an (invalid) expr like ON ((((1=GOTO)+2 GOTO where we should
      'TODO emit a syntax error, but for now we are going to accept identifiers with reserved names, ie they are not reserved.
      'TODO we could state IF statement(id$) THEN & "RESERVED KEYWORD": STOP but not sure if we rely on id parsing elsewhere:
      if pstack%(pstack_i%-2) = pstate%("on") then if id$="GO" then stop ' TODO need to figure out if it's GO SUB or GO TO
      di$ = id$ : ' work around PED bug where it shows id$ as mid$, 2024-05-14
      di$ = ups$(di$) : 'uppercase it because hash table lookups are case-sensitive
      ' Add a fake comma and 
      isgoing = (pstack%(pstack_i%-2) = pstate%("on")) and (di$="GOTO" or di$="GOSUB") and (expr_expect_c%<=2)
      & "isgoing exprc opc compc di$ pstack-2", isgoing, expr_expect_c%, operator_c%, compiled_c%, di$, pstack%(pstate_i%-2), pstate%("on")
      if (pstack%(pstack_i%-2) = pstate%("on")) and (expr_expect_c%>0) and (operator_c%>1) then if not isgoing then & "expr_expect_c%",expr_expect_c%: stop
      if isgoing then & "if we already have a goto/gosub then this is a syntax error"
      'if isgoing then & "ongoto/gosub: add fake comma and continue with exprs": stop
      'if isgoing and (operator_c%<=0) then stop
      if isgoing and di$="GOSUB" then stop :' on..gosub not implemented yet TODOTODO, should compile di$ here
      if isgoing then expr_expect_stack%(expr_expect_c%-1)=type_opcode%: gosub 10900 : 'comma_stack(0)=1
      if isgoing and (expr_expect_c%>0) then stop : 'not a bug per se, but wasn't sure if this could happen
      if isgoing and (expr_expect_c%=0) then & "no EXPR_EXPECT_C":expr_expect_c% = 1:expr_expect_stack%(0) = type_int% : ' TODO can it ever be > 0 ?
      'if isgoing then & "setting expr_expect_s at",expr_expect_c%-1,"to int": expr_expect_stack%(expr_expect_c%-1) = type_int%:& expr_expect_stack%(expr_expect_c%-1)
      if isgoing then pstack_i% = pstack_i% -1 : ci%=ci%-1 : return

      if (di$ = "TO") then & "TODOTODO saw to, going to assume we are in a FOR .. statement", compiled$(compiled_c%-4), compiled$(0)
      if (di$="TO") then & "okay so compiled$(1..compiled_c%-1) holds the initial FOR assignment and top of opstack would be ==", operator_stack$(operator_c%-1)
      if (di$="TO") then & "we should probably have used the assignment parser for this"
      if di$="TO" then & "pop two levels off pstack to get down to FOR pstate"
      if di$="TO" and operator_c%>0 then & "TO WITH OPSTACK", operator_c%, operator_stack$(operator_c%-1): expr_expect_stack%(expr_expect_c%-1)=type_opcode%:gosub 10900
      if di$="TO" then last_to_offset% = compiled_c%
      if di$="TO" then comma_stack%(0)=1:expr_expect_c% = 1:expr_expect_stack%(expr_expect_c%-1) = type_int%
      if di$="TO" then pstack_i%=pstack_i%-1: ci%=ci%-1:return
      if di$="TO" then & "should act like a comma inside for": stop
      if di$="TO" then pstack_i% = pstack_i% - 2 : ci% = ci%-1: return
      if di$="TO" then stop

      if statement(di$) then & di$ + " is a statement keyword, TODO should tis be allowed?": stop
      & "statement keyword?", di$, statement(ups$(di$))
      if (expr_expect_c%<>0) and (expr_expect_stack%(expr_expect_c%-1) = type_opcode%) then print "got identifier but looked for opcode":stop
      if (expr_expect_c%<>0) and (expr_expect_stack%(expr_expect_c%-1) = type_int%) then expr_expect_stack%(expr_expect_c%-1) = type_opcode%
      if (expr_expect_c%<>0) and expr_expect_stack%(expr_expect_c%-1)<>type_opcode% then print "wtf expr expect after identifier", : stop

      operator_stack$(operator_c%) = fntlv$(type_ptr%, id$)
      comma_stack%(operator_c%) = ( chnext$="(" )
      operator_c% = operator_c% + 1
      pstack_i% = pstack_i% - 1
      return
13000 ' DATA
      if fnwhitespace%(ci%) then RETURN
      if ch$ = quot$ then pstack_i% = pstack_i%  + 1 : pstack%(pstack_i%) = pstate%("string"): string_builder% = ci%+1 : return
      if ch$ = "," then rec_data% = rec_data% + 1 : return
      this$ = compiled_data$(no_data%, rec_data%)
      if this$ = "" then this$ = fntlv$(type_int%, "")
      compiled_data$(no_data%, rec_data%) = fntlv$(fntyp%(this$,1), fnval$(this$,1) + ch$)
      compiled_data$(no_data%, rec_data% + 1) = ""
      return
14000 '"ON" (expr) ("GOTO"|"GOSUB") 1,2,3,...
      & "ON .. GOTO/GOSUB is a stub", asc(ch$)
      'gosub 10900
      '& "back from compiling ON premise"
      operator_c% = 0
      if th_re(ch$, "^[ 0-9,]",1) then return
      if not ((ch$ = ":") or (ch$="'") or (asc(ch$)=0)) then stop : REM not next statement, not REM, not EOF; what is it?
      ' arguably we could have used the expression parser here instead and permitted variables in ON .. GO .. x,y,z
      ' which would be cleaner (for us) but less compatible with regular basic
      list$ = mid$(inp$, cion%, ci%-cion%)
      & list$
      stop
15000 'FOR = [here] TO [stopvar]
     lhs_keybytecode$ = ""
    ' for i = 0 to compiled_c%-1
    '    & "FOR: ",i,compiled$(i)
    ' next i
     ' we have an assignment starting with =/:= in compiled$(0)
     ' capture the loop var index as bytecode:
     lhs_keybytecode$ = ""
     for i = 1 to last_assignment_offset - 2 : ' set in the pstate%("assignment") handler
       lhs_keybytecode$ = lhs_keybytecode$ + compiled$(i)
     next i
     ' the final element is the name of the lhs, which we sometimes need as a type_ptr% and sometimes as type_str$
     ' the loop var name is stored at compiled$(i) now, i=last_assignment_offset-1
     lhs_ptrbytecode$ = lhs_keybytecode$ + fntlv$(type_ptr%, fnval$(compiled$(i),1))
     lhs_ptrkeybytecode$ = lhs_keybytecode$ + fntlv$(type_ptrkey%,fnval$(compiled$(i),1))
     lhs_strbytecode$ = lhs_keybytecode$ + compiled$(i)
     ' the initial value is whatever is in last_assignment_offset..last_to_offset%
     if compiled_c% - last_assignment_offset <= 0 then stop 'no initial value
     rhs$ = ""
     for xx = last_assignment_offset to last_to_offset%-1
       'print "FOR rhs", xx, compiled$(xx)
       rhs$ = rhs$ + compiled$(xx)
     next xx
     varstop$ = ""
     for xx = last_to_offset% to compiled_c% -1
       'print "FOR stopvar", xx, compiled$(xx)
       varstop$ = varstop$  + compiled$(xx)
     next xx
     ' varstop will need to be looked up wherever we see NEXT,
     ' but we can't compute lhs_ptrkeybytecode$ statically,
     ' and it can very well look different: FOR i(1+2)=7 to 9: NEXT i(3)
     ' so TODOTODO for now we mock it with (0):
     varstop$(0) = varstop$
     ' capture that, then output:
     ' - initial assignment: lhs := rhs
     ' - varstops$(ptrkey) = ptrkey   (used by NEXT later)
     ' - loopvar = initial ; label [ptr]
     compiled$(0) = lhs_strbytecode$ + rhs$ + fntlv$(type_opcode%, "=")
     compiled$(0) = compiled$(0) + lhs_ptrkeybytecode$ + fntlv$(type_int%, 1) + fntlv$(type_str%, "varstops") + varstop$ + fntlv$(type_opcode%, "=")
     compiled$(0) = compiled$(0) + lhs_ptrkeybytecode$ + fntlv$(type_opcode%, "#") : ' LINE/LABEL
     compiled_c% = 1
     expr_expect_c%=0: operator_c%=0 : ' TODO unsure if this is needed
     gosub 7400
     compiled_c% = 0
     return
60000 '
print "chain yo"
open "demo.bcb", as #1
erase bc$
for toneof = 0 to val("Inf") step 1
if not eof(1) then read#1; inp$ : c$(toneof)=inp$: gosub 6000: bc$(toneof)=bytecode$: next toneof
bytecode$ = ""
for i = 0 to toneof-1
  & c$(i) spc$(30-len(c$(i))) "=>", bc$(i)
  bytecode$ = bytecode$ + bc$(i)
next i
& "... sleeping 15 ..."
sleep 15
gosub 600
print "Executed DEMO.BCB without crashing, what a day"
goto 10
