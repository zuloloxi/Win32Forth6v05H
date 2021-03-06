CREATE SoundexTable
    ," 01230120022455012623010202" 
CREATE SoundexInitial
    ," ABKDEFGHEGKLMNOPQRSTOVVKIS"
0 [if]
CODE str++ ( s c -- s++ cnt-- )
    or  ebx, ebx
    jz  short @@1
    inc [esp]
    dec ebx
@@1:
    next c;
[then]
: str++ ( str cnt -- str++ cnt-- ) ?DUP 0<> IF 1- SWAP 1+ SWAP THEN ;
: char>soundex ( char -- code ) UPC 64 - 1 MAX 27 MIN SoundexTable + C@ ;
: soundex-first ( str cnt -- str' cnt' soundex code )
    DUP 0= IF 48 48 ELSE 
        OVER C@ UPC 64 - SoundexInitial + C@ >R \ save initial code
        OVER C@ char>soundex >R str++ R> R> 
    THEN ;
: soundex-next ( str cnt previous -- str' cnt' code )
    >R  \ keep previous for comparison
    BEGIN
        DUP 0>
    WHILE
        OVER C@ \ prev str cnt char
        char>soundex
        DUP 48 <> IF  \ not a 'zero'
            DUP R@ <> IF
                \ it's non-zero and unique. adjust string
                R>DROP >R str++ R> EXIT
            THEN
        THEN
        DROP str++    \ drop char and bump string
    REPEAT
    \ we ran off the end of the string without a match:
    R>DROP 48
;
: soundex ( str cnt -- soundex )
    soundex-first 24 lshift >R
    soundex-next DUP 16 LSHIFT R> OR >R 
    soundex-next DUP 8 LSHIFT R> OR >R
    soundex-next R> OR >R
    2DROP R>
;
