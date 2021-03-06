\ $Id: args.f 1.1 1994/04/01 07:52:01 andrew Exp $

cr .( Loading Local Variables...)

((
Named input parameters and local variables.  This implementation is
copied from the Macintosh Forth system Yerkes (formerly Neon).

Note: The ANSI Standard "Local" is similiar to what are called "args"
in this documentation, except for reversed order.

Syntax:

: wordname  { arg1 arg2 \ loc1 loc2 loc3$ -- result }
        arg1            \ returns value of 1st argument
        0  TO loc1      \ stores a value to local 1
        35 TO arg1      \ ok to change value of an arg
        1  +TO loc2     \ increment local 2
        &LOCAL loc1     \ address of the local following
        ... ;

You can have 0 or more args or locals up to a total of 8. The -- and }
are required parts of the syntax. Anything between -- and } is treated
as a comment and does not do anything. The following are all valid
argument declarations:

  { -- }                        \ does nothing

  { arg -- }                    \ one input argument

  { \ local -- }                \ one local (not initialized!)

  { arg \ local -- }            \ one arg and one local

  { arg1 arg2 -- result }       \ two args


The args and locals are similiar to "values", in the sense that the
value of the current arg or local is pushed on the stack during execution
of a definition when the name is encountered.  The content of an arg or
local may be changed by using TO or +TO .

Restrictions:

  1.  Left brace '{' must be first word if used in a :M method definition.
  2.  Total #args + #locals limited to 8.
  3.  No double precision.
  4.  Locals are not initialized.
  5.  Names cannot begin with '\' '-' or '}'
  6.  Entire declaration must fit on one source line.
  7.  A definition may have only one occurrence of the use of {
      or LOCALS| .  If one is used in a definition, the other may NOT
      be used!

Implementation:

  At the start of the word, locals are allocated and arguments are poped
  from the data stack and pushed onto the return stack.  They are
  referenced relative to the user variable 'mp'.  When you use exit or ;
  the arguments are deallocated.  It is ok to use do..loop and >r r> at
  the same time as locals.

  Since there is some runtime overhead, you should only use args and
  locals when it would clear up the code or when you find yourself
  doing a lot of stack juggling.  Do not abuse this feature!

                { a b \ c d -- }

        oldbp -> +--------+
                 | retadr |
                 +--------+
                 | old lp |
           lp -> +--------+
                 | arg 0  |   d
                 +--------+
                 | arg 1  |   c
                 +--------+
                 | arg 2  |   b
                 +--------+
                 | arg 3  |   a
           bp -> +--------+


Additional notes:
  The LOCALS| word, as specified in the LOCAL-EXT extension in the ANS
  Standard has now been implemented.  The following is an example:

        : FOO
                LOCALS|  A1 A2 |
                2 +TO A2
                A1 A2 ;

  The sequence  7 8 FOO will return 8 and 9 on the stack, with the 9
  on top.

))

0 value ?:M

10 constant #-locals

create local-ptrs ' LOCAL0 , ' LOCAL1 , ' LOCAL2 , ' LOCAL3 , ' LOCAL4 ,
                  ' LOCAL5 , ' LOCAL6 , ' LOCAL7 , ' LOCAL8 , ' LOCAL9 ,

: >loc  ( n -- cfa )  cells local-ptrs + @ ;


\ -------------------- Parameter Name List --------------------

\ Names in Win32For are limited in length to name-max-chars characters.
\ Parameter names are stored as name-max-chars byte counted string.

create ParmList
        #-locals 1+ name-max-chars 1+ * allot   \ list of parameter names
                                        \ first slot is temp for current name

0 value Parms   \ number of parameters

: ToParm        ( addr cnt -- )   \ store string in first slot of parmlist
                name-max-chars min parmList place ;

: AddParm       ( addr cnt -- )         \ add string to end of parmlist
                parms #-locals 1- > abort" Too many parameters"
                1 +to parms
                2dup ToParm
                name-max-chars min parmList parms
                name-max-chars 1+ * + place ;


\ -------------------- Parameter Compiler --------------------

0 value inParms         \ number of input paramters
0 value locFlg          \ 1 = compiling args, 0 = compiling locals

: parms,  ( -- )   \ compile runtime to push parameters
        ?:M                             \ in method?
        if      cell negate allot       \ then deallocate the cell layed down
        else    postpone init-locals    \ else this is a normal local def
        then
        inParms parms over - c, ( #locals ) c, ( #args )
        0 c, 0 c,  ( unused filler bytes for cell alignment )   ;

: firstChr  ( addr cnt -- addr cnt chr )  over c@ ;

: (LOCAL)       ( addr cnt -- )
        dup 0=
        IF      2drop
        ELSE
                addParm
                inParms locFlg + to inparms
        THEN
        ;

: {  ( -- )
\       ?comp
        0 to parms
        0 to inparms
        1 to locFlg

        BEGIN
                bl word ?uppercase count        \ tjz per 'smb' March 6th, 1996
                firstChr [char] - <>
        WHILE
                firstChr [char] \ =
                IF
                        0 to LocFlg 2drop
                ELSE
                        firstChr [char] } = abort" Args missing --"
                        (LOCAL)
                THEN
        REPEAT
        2drop
        parms IF  parms,  ( compile runtime code )  THEN

        BEGIN
                bl word dup c@ 0= abort" Args missing }"
                count firstChr [char] } = nip nip
        UNTIL
 ; immediate

: REVERSEARGS   ( -- )
        inParms 0= ?EXIT
        inParms 2/ 0
        ?DO     ParmList I 1+ name-max-chars 1+ * +
                ParmList name-max-chars 1+ move
                ParmList inParms I - name-max-chars 1+ * +
                ParmList I 1+ name-max-chars 1+ * +
                name-max-chars 1+ move
                Parmlist
                ParmList inParms I - name-max-chars 1+ * +
                name-max-chars 1+ move
        LOOP ;

: LOCALS|       ( -- )
                parms abort" Locals may be defined only once per definition."
                0 to parms
                0 to inparms
                1 to locFlg
                BEGIN
                        bl word count 2dup upper
                        firstChr [char] | <>
                WHILE
                        (LOCAL)
                REPEAT  2drop
                parms IF  parms,  ( compile runtime code )  THEN
                reverseargs
                ; IMMEDIATE

: &LOCAL        ( -<name>- a1 )
                r> dup cell+ >r
                abs>rel @ dup @ dolocal <>
                abort" Must be followed by a local variable"
                cell+ @ lp @ abs>rel + ;

also environment definitions

: LOCALS        TRUE ;

: LOCALS-EXT    TRUE ;

#-locals constant #LOCALS

previous definitions



\ -------------------- Local Dictionary Search --------------------

: D= d- or 0= ;

: (pfind)   ( addr -- cfa t | addr f )
        false parms
        IF      over count ToParm
                parms 1+ 1
                DO      ParmList i name-max-chars 1+ * + count
                        ParmList count name-max-chars min
                        compare 0=                      \ true if matched
                        IF      2drop  parms  i  -  >loc TRUE
                                LEAVE
                        THEN
                LOOP
        THEN ;

: pfind state @ if (pfind) else false then ;

: parmFind  ( addr -- addr 0 | cfa -1 | cfa 1 )
        pfind ?dup 0= if (find)  then ;


\ -------------------- New Colon Compiler --------------------

warning @ warning off   nostack1

: Parms-init    ( -- )
                0 to Parms ;

: _parms:       ( -- )
                false to ?:M
                Parms-init _: ;

: EXIT  ( -- )
        ?:M     ( -- f1 )
        false to ?:M
                ( -- f1 ) abort" Can't use EXIT in a Method !"
        Parms
        if      postpone exitp
        else    postpone exit
        then    ; immediate

\ : EXITM   ( -- )
\        ?:M 0= abort" Can use EXITM only in a Method !"
\        Parms
\        IF      postpone unparms
\        THEN
\        postpone exitm ; immediate

: ?exit ( f1 -- )
        ?:M     ( -- f1 )
        false to ?:M
                ( -- f1 ) abort" Can't use ?EXIT in a Method !"
        ?comp
        compile ?branch >mark
        POSTPONE  exit
        POSTPONE _then
        >resolve ; immediate

: ;     ( -- )
        ?:M     ( -- f1 )
        false to ?:M
                ( -- f1 ) abort" Methods must END in ;M !"
        ?csp reveal
        parms if  postpone unnestp  else  postpone unnest  then
        postpone [
        0 to Parms
        semicolon-chain do-chain ; immediate

: DOES>  ( -- )
        ?:M     ( -- f1 )
        false to ?:M
                ( -- f1 ) abort" Can't use DOES> in a Method !"
        Parms
        if
                postpone unparms
                postpone Parms-init
        then
        postpone does>
        Parms-init ; immediate

: localAlloc:   ( n1 -<name>- )   \ allocate a local n1 byte buffer to local "name"
                compile localAlloc postpone to ; immediate

assembler defined ;CODE nip #IF  forth

in-system

: _;CODEP   ( -- )
        parms
        IF      postpone unparms
        THEN    [ assembler asm-hidden ] _;code [ forth ]
        ; immediate

in-application

#THEN

forth

warning !

\ -------------------- Enable Locals --------------------

: locals-on
                ['] parmFind is find
                ['] _parms:  is :
\+ _;codep      ['] _;codep is ;code
                0 to Parms ;

: locals-off
        ['] (find) is find ;

locals-on

