\ $ld: assert.f 1.1  1994/07/12 15:45:00 andrew Exp $

cr .( Loading Assert Wordset...)

0 value assert?

: ASSERT(       ( -<words>- )
                assert? 0=
                if      postpone (
                then    ; immediate

: ?assert       ( flag nfa -- )
                swap
                if      drop
                else    cr ." Assertion failed in " nfa-count type
                        cr ." Enter to continue, ESC to abort"
                        key 0x1B = if abort then cr
                then    ;

: )             ( -- )
                ?comp
                last @ postpone literal
                postpone ?assert ; immediate

\s

true to assert?

: atest         ( -- )
                10 0
                do      i .
                        assert( i 5 < )
                loop    ;


