' very bare-bones LZ4 decompression, with no xxHash32 checksum computation at all,
' and no cross-block sliding window (only independent blocks supported)
' jbomadsen 2024


'https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md
' test case generation:
'echo -n '123123123123123' | lz4 -c|base64
'lz4_inp$ = th_b64d$("BCJNGGRApwEAAIAzAAAAAMRz7Jw=") :' one "3"
'lz4_inp$ = th_b64d$("BCJNGGRApwoAAAATMwEAUDMzMzMzAAAAAKIKck0=") : '13x "3", smallest thing LZ4 will actually compress
'lz4_inp$ = th_b64d$("BCJNGGRApwsAAAAjMTICAFAyMTIxMgAAAAD9Lwv1") : '12121212121212
'lz4_inp$ = th_b64d$("BCJNGGRApwwAAAAzMTIzAwBQMjMxMjMAAAAA4YZW+g==") : '123123123123123
lz4_inp$ = th_b64d$("BCJNGGRApyEAAAD/B0hlcmUncyBhIGxvdCBvZiBBJ3M6IGECADpQb2theS4AAAAAwLVbFA==")
debug = 0
def fnlz4_mid$(idx%, size%) = mid$(lz4_inp$, 1 + idx%, size%)
for lz4_callback = 0 to Inf step 0
if lz4_callback then & "got callback at ",lz4_pos,len(lz4_decompressed$),"ratio:"+str$(len(lz4_inp$)/len(lz4_decompressed$)*100)+"%" : & lz4_decompressed$: next lz4_callback_return
gosub 1000
& "return from lz4 decode, lz4_err:",lz4_err,"lz4_pos:",lz4_pos

END

1000 ' LZ4 decompressor
' TODO for the actual copying we cheat and use MID$() in the current implementation,
' so all this abstraction fluff of only implementing 
' user defines:
' def fnlz4_mid$(idx%, size%)   = mid$(lz4_inp$, 1 + idx%, size%)
' for lz4_callback = 0 to Inf step 0: ' this is the callback for block data.
'   lz4_callback is set to non-zero when there is data to treat.
'   the callback receives lz4_decompressed$
'   at the end of the callback, they do NEXT lz4_callback_return
def fnlz4_peek8$(idx%) = fnlz4_mid$(idx%, 1)
def fnlz4_peek8%(idx%) = asc(fnlz4_mid$(idx%,1))
def fnlz4_peek32$(idx%) = fnlz4_mid$(idx%, 4)
def fnlz4_32%(b$) = asc(left$(b$,1)) + 256*asc(right$(b$,3)) + 256^2*asc(right$(b$,2)) + 256^3*asc(right$(b$,1))
def fnlz4_peek32%(idx%) = fnlz4_32%(fnlz4_peek32$(idx%))

lz4_pos = 0 : lz4_err = 0
'https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md
'magic number
' 0x184D2204
' stored in little endian, eg:
' 04 22 4D 18
lz4_magic% = fnlz4_peek32%(lz4_pos)
if lz4_magic% <> 407708164 then lz4_err = 1 : return
lz4_pos = lz4_pos + 4 : ' ignore magic

'frame descriptor
'  FLG byte (7-6: Version ; 5: B.Indep ; 4: B.Checksum ; 3: C.Size ; 2: C.Checksum ; 1: Reserved ; 0: DictID
lz4_flg% = fnlz4_peek8%(lz4_pos)
'    Version should be 01, ie 64
lz4_version% = int((lz4_flg% and 192) / 64)
if lz4_version% <> 1 then lz4_err = 2 : return
'    Block independence, ie 32
lz4_block_independence_flag% = lz4_flg% and 32
if lz4_block_independence_flag% = 0 then stop : ' TODO window not implemented
'    Block checksum flag, ie 16
lz4_block_checksum_flag% = lz4_flg% and 16
'    Content size flag, ie 8 (without this, skip Content Size
lz4_content_size_flag% = lz4_flg% and 8
'    Content checksum flag, ie 4 governs where there's a checksum after EndMark
lz4_content_checksum_flag% = lz4_flg% and 4
'    DictID, ie 1
lz4_DictID_flag% = lz4_flg% and 1
'  BD  byte (7: Reserved ; 6-4: Block MaxSize ; 3-0: Reserved)
'  content size (optional, uint64_t, 8 bytes, little endian)
'  Dictionary ID (optional, 4 bytes, little endian)
lz4_pos = lz4_pos + 1
lz4_block_maxsize% = int(fnlz4_peek8%(lz4_pos) / 16)
lz4_pos = lz4_pos + 1
if debug then & "header flg:",lz4_flg%,"b.I:",lz4_block_independence_flag%,"b.C:",lz4_block_checksum_flag%,"c.S:",lz4_content_size_flag%,"";
if debug then & "c.C:", lz4_content_checksum_flag%, "dictid:", lz4_DictID_flag%
if debug then & "BD block maxsize:",lz4_block_maxsize%

if lz4_DictID_flag% then lz4_dictid% = fnlz4_peek32%(lz4_pos) : lz4_pos = lz4_pos + 4
'  HC  byte  header checksum (including optional bytes), (xxh32()>>8) & 0xff
lz4_header_checksum% = fnlz4_peek8%(lz4_pos) : lz4_pos = lz4_pos + 1
if debug then & "TODO calculate header checksum == ", lz4_header_checksum%

' check block checksum should be here, and ignored on first step

for lz4_loop_data_block = 0 to Inf step 0
'data blocks or skippable frames
'  skippable frame: magic number 0x184d2a5[0-f], ie 5[0-f] d2 4d 18
   lz4_block_size% = fnlz4_peek32%(lz4_pos) : lz4_pos = lz4_pos + 4
   if debug then & "=== block size:", lz4_block_size%, "at",lz4_pos-4,lz4_pos

   ' these are user frames, of application data. we don't need that, so we just skip:
   lz4_frame_size% = 0
   if 407710288 = (lz4_block_size% and 407710288) then lz4_frame_size% = fnlz4_peek32(lz4_pos) : lz4_pos = lz4_pos + 4
   if lz4_frame_size% then lz4_pos = lz4_pos + lz4_frame_size% : next lz4_loop_data_block

'  data block
'    block size, little-endian, uint32_t, 4 bytes
   ' if all four bytes are 0, this is an EndMark
'     content checksum, xxHash-32 of plaintext. only there if the frame header warrants it with a bit.
      if (lz4_block_size% = 0) then if lz4_content_checksum_flag% then lz4_content_checksum% = fnlz4_peek32%(lz4_pos) : lz4_pos = lz4_pos + 4 : if debug then  & "TODO check lz4_content_checksum% = ", lz4_content_checksum%
      if lz4_block_size% = 0 then return

'      highest bit set: block is uncompressed
   lz4_compressed_size% = (lz4_block_size% and (2147483648-1))
   lz4_uncompressed_size% = 0

  'if Block_checksum is set: xx32(raw undecoded block), little-endian, 4 bytes
  '  note that here we compute that up front, before decompressing.
  '  that means we need to fix up the lz4_pos increase accordingly (+4 bytes if there's a checksum)
  if lz4_block_checksum_flag% then lz4_block_checksum% = fnlz4_peek%(lz4_pos + lz4_compressed_size%) :' lz4_pos = lz4_pos + 4   
  if lz4_block_checksum_flag% then if debug then & "TODO lz4_block_checksum% = ", lz4_block_checksum%

   if 2147483648 = (lz4_block_size% and 2147483648) then lz4_uncompressed_size% = lz4_compressed_size%
   if lz4_uncompressed_size% then lz4_decompressed$=fnlz4_mid$(lz4_pos, lz4_uncompressed_size%) : for lz4_callback_return = 0 to 1 : if not lz4_callback_return then lz4_callback = 1 : next lz4_callback
   if lz4_uncompressed_size% then lz4_pos = lz4_pos + lz4_uncompressed_size% + 4*(0<>lz4_block_checksum_flag%) : lz4_decompressed$ = "" : next lz4_loop_data_block
'      highest bit not set: lz4 block format specification
'      uncompressed blocks of size 0 are legal
   lz4_compressed_end% = lz4_pos + lz4_compressed_size%
   for lz4_blockpos = lz4_pos to lz4_compressed_end% - 10
       lz4_token% = fnlz4_peek8%(lz4_pos)
       lz4_literal_len% = int(lz4_token% / 16)
       lz4_matchlength% = (lz4_token% and 15) + 4
       if lz4_literal_len% = 15 then for lz4_pos = lz4_pos+1 to val("Inf") step 1: lz4_extlen = fnlz4_peek8%(lz4_pos) : lz4_literal_len% = lz4_literal_len% + lz4_extlen: if lz4_extlen = 255 then next lz4_pos
       lz4_pos = lz4_pos + 1 : 'bump from either lz4_token% or extended literal length
       if lz4_literal_len% then lz4_decompressed$ = lz4_decompressed$ + fnlz4_mid$(lz4_pos, lz4_literal_len%)
       lz4_pos = lz4_pos + lz4_literal_len%
       lz4_offset% = fnlz4_peek8%(lz4_pos) + 256*fnlz4_peek8%(lz4_pos+1)
       if lz4_offset% > lz4_literal_len% + len(lz4_decompressed$) then stop
       lz4_pos = lz4_pos + 2
       if lz4_offset% = 0 then lz4_err = 3: return
       if lz4_matchlength% = 19 then for lz4_pos = lz4_pos to val("Inf"): lz4_extlen = fnlz4_peek8%(lz4_pos) : lz4_matchlength% = lz4_matchlength% + lz4_extlen: if lz4_extlen = 255 then next lz4_pos
       lz4_pos = lz4_pos + (lz4_matchlength% >= 19) : ' fix up pos for last byte (not incremented by NEXT lz4_pos)
       if lz4_offset% > LEN(lz4_decompressed$) then lz4_err = 4: return: ' TODO implement 64K window
       lz4_decompressed$ = lz4_decompressed$ + string$(INT(lz4_matchlength%/lz4_offset%), right$(lz4_decompressed$,lz4_offset%)) + left$(right$(lz4_decompressed$, lz4_offset%),lz4_matchlength% mod lz4_offset%)
       lz4_blockpos = lz4_pos
       next lz4_blockpos : 'while block is not empty
    lz4_pos = lz4_pos + 1
    lz4_decompressed$ = lz4_decompressed$ + fnlz4_mid$(lz4_pos, (lz4_compressed_end% - lz4_pos))
    for lz4_callback_return = 0 to 1 : if not lz4_callback_return then lz4_callback = 1 : next lz4_callback
    lz4_decompressed$ = ""
    lz4_pos = lz4_pos + (lz4_compressed_end% - lz4_pos)
    ' see https://github.com/lz4/lz4/blob/dev/doc/lz4_Block_format.md
    ' TODO here the block checksum is blatantly ignored:
    lz4_pos% = lz4_pos% + 4*(0<>lz4_block_checksum_flag%)
next lz4_loop_data_block
