I am a user of [Telehack](https://telehack.com), an online simulation of ARPANET of the 80's, a blend of retrocomputing museum and computer game.
The programs here are written in Telehack's TH BASIC, a reimagination of the original Dartmouth University "TeleBasic" (~1965).

```
10 PRINT "HELLO WORLD"
20 GOTO 10
```

# Here
- [BMH.BAS](./bmh.bas), implementation of [Boyer-Moore-Horspool string search](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm)
- [CHIP8ASM.BAS](./chip8asm.bas), [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) disassembler (that could use a little bit more love)
- [EXPONENTIAL.BAS](./exponential.bas), benchmark of [linear](https://en.wikipedia.org/wiki/Linear_search) & [exponential](https://en.wikipedia.org/wiki/Exponential_search) search algorithms for counting lines in a file
- [HASH.BAS](./hash.bas), djb2 hash adjusted for ieee754 `float64`
- [HEAP.BAS](./heap.bas), implementation of [Heap's Algorithm](https://en.wikipedia.org/wiki/Heap%27s_Algorithm)
- [LZ4.BAS](./lz4.bas), mostly working [LZ4 decompression](https://github.com/lz4/lz4/)
- [REPL.BAS](./repl.bas), WiP P-code interpreter and a TH BASIC compiler for the interpreter.
- [TRIE.BAS](./trie.bas), a hash table-backed prefix tree to support tab completion. Uses `FOR..NEXT` instead of `GOTO`/`GOSUB` to make integration easier.
- [UNDIALED.BAS](./undialed.bas), given a set of files with phone numbers, dial the numbers not in your `dial /log`

