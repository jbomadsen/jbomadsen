'djb2 hash, modulo 2^52 to avoid float64 overflow
'jbomadsen 2024

300  hash%=5381 : hashoff%=1 : hashmask%=2^52-1 : 'floating-point friendly djb2: inp$ -> hash%
310  hashch%=asc(mid$(inp$,hashoff%,1)) : if hashch% then hash%=(hash%*33 + hashch%) and hashmask%: hashoff%=hashoff%+1 : goto 310
320  return
