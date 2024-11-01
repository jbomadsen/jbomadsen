'Heap's algorithm for generating all permutations of array A of length N > 1
'this version tries to not use line numbers, so as to make it easier to copy-paste
'jbomadsen 2024

N = 5
dim A(n)
A(0) = 0 : A(1) = 1 : A(2) = 2 : A(3) = 3 : A(4) = 4

for ret("n200") = 0 to 1: if ret("n200") then end

'entry point: ret("n200")
for i = 0 to N-1: c(i) = 0: next i
for n100 = 1 to 1 step 0
for y = 0 to N-1: print A(y) ; : next y : print : i = 0 : if ret("n100") then next ret("n100")

for ret("n250") = 1 to 1 step 0
if c(i) >= i then c(i) = 0 : i = i + 1 : next ret("n2"+mid$("05",1+(i<>N),1)+"0")
    z = (i and 1) * c(i)
    A(z) = A(z) xor A(i) : A(i) = A(i) xor A(z) : A(z) = A(z) xor A(i)
    c(i) = c(i) + 1
    for ret("n100")=1 to 2: if ret("n100")=1 then next n100
    next ret("n250")

