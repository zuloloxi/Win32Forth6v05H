\ Help.F        HPO 2019 Mar 02        Display a help file window

\ convert character position to pixel position
: c>p-h ( c l - x y )   char-height-help * swap  char-width-main * swap ;
: p>c-h ( x y - c l )   char-height-help / swap  char-width-main / swap ;

variable MyTextBuffer 2000 allot


defer Htype \ x y a n Htype

: (Htype)  2swap  p>c-h  gotoXY type ;
: norm   ['] (Htype) is Htype ;

: emit+ ( c )   \ display certain control codes as printable characters

   dup 128 and if  drop  191  then \ 128 - 255

   dup case
      32 of  drop  183  endof   \ space
      13 of  drop  162  endof   \ cr
      10 of  drop  164  endof   \ lf
      09 of  drop  187  endof   \ tab
   endcase

   emit
;

: ShowHelp   page
    cr   ."               Cweed : C source and header file weeder"
    cr   ."               ======================================= "
    cr   ."                    1. Select the File "
    cr   ."                    2. Press 'Weed'  "
    cr   ."     3. Edit the file using your favourite editor to shorten long lines.... "
    cr   ."                    4. Go to 2 until 'Pass' "
    cr
    cr   ."     Click on 'Show info' to re-load the file after editing"
    cr   ."     Floating Pointers means 't_struct* fred' instead of 't_struct *fred'"
    cr   ."     Press 'ctrlR' to convert a Block file to a text file "
    cr   ."     PageUp, PageDown, Home, End and arrows show the file with or"
    cr   ."     without comments, as selected by Show File + or - "
    cr   ."     Spaces, carriage returns, linefeeds and tabs are shown as follows :"
    cr   ."     space ( 32 ) = "    32 emit+ ."     cr ( 13 ) = "   13 emit+
         ."     linefeed ( 10 ) = " 10 emit+ ."     tab ( 09 ) = "  09 emit+
    cr   ."     ctrlF toggles the full line info mode on/off (please see .LINE in FILES.F)"
    cr
    cr   ."     Press 'R' to create a folder report file for *.c*, *.h* and *.ini files, F for *.c and *.h files"
    cr   ."     DISCLAIMER : this program changes the selected file."
    cr   ."     Cweed is believed to work correctly, but please take a backup"
    cr
    cr   ."     ***  Restore  will put back the original file  *** "
    cr
    cr   ."     Note : Fix Indents  lines up function arguments to simple indent rules,"
    cr   ."     these may not be your rules, so it not been included in the Weed function."
    cr   ."     #ifdef , #ifndef , #else and #endif lines containing a double underscore __ are not indented"
    cr
    cr   ."              Howerd Oakford       www.inventio.co.uk"
;


\ *********************** Dialog and Monologue boxes **************************

variable my_temp$ 128 allot

: Monologue ( $message $title )     \ presents some information

   temp$ place
   temp$ +NULL

   my_temp$ place
   my_temp$ +NULL

   MB_TASKMODAL
   MB_ICONWARNING or   \ add a big "!"

   my_temp$ 1+ rel>abs
   temp$ 1+ rel>abs

   GetHandleOfMainWindow

   Call MessageBox drop
;

: Dialogue  ( $message $title - result ) \ presents some information and offers a choice
   temp$ place
   temp$ +NULL

   my_temp$ place
   my_temp$ +NULL

   MB_TASKMODAL
   MB_OKCANCEL or  \ Ok or cancel?
   MB_ICONQUESTION or  \ addd a big "?"

   my_temp$ 1+ rel>abs

   temp$ 1+ rel>abs

   GetHandleOfMainWindow

   Call MessageBox         \ returns 1 for OK, 2 for Cancel
;

\ : TTD  s" Title bar" s" Message" Dialogue ;

\ ***** program end *****
