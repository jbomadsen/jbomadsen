'Heap's algorithm for generating all permutations of array A of length N > 1
'jbomadsen 2024

N = 5
dim A(n)
A(0) = 0 : A(1) = 1 : A(2) = 2 : A(3) = 3 : A(4) = 4

GOSUB 200

END

100 ' yield a combination:
    for y = 0 to N-1
        print A(y) ;
    next y : print
    i = 0 : ' reset index
    RETURN

200 for i = 0 to N-1
        c(i) = 0
    next i

    gosub 100 : ' yield first permutation (the original A)

250 if c(i) >= i then c(i) = 0 : i = i + 1 : on i <> N goto 250 : RETURN
    z = (i and 1) * c(i) : 'z <- { c(i) when i is odd; 0 when i is even }
    A(z) = A(z) xor A(i) : '|\
    A(i) = A(i) xor A(z) : '| swap A(z), A(i)
    A(z) = A(z) xor A(i) : '|/
    c(i) = c(i) + 1
    gosub 100 : ' yield permutation
    goto 250

