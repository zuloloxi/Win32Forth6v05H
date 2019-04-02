// $Id: term.h 1.1 1994/04/01 07:51:49 andrew Exp $

#define BL 32
#define BELL 7
#define BS 8
#define CR 13
#define LF 10

#define function_mask  0x10000
#define special_mask   0x20000
#define control_mask   0x40000
#define shift_mask     0x80000
#define alt_mask       0x100000
#define mouse_mask     0x200000
#define menu_mask      0x400000
#define proc_mask      0x800000
#define double_mask    0x1000000
#define down_mask      0x2000000
#define up_mask        0x4000000

extern int *image;                          // this is where forth is
extern char *screen;                        // address of screen buffer

ENTRY init (HANDLE, HANDLE, int);           //
ENTRY key (void);                           // 2
ENTRY Accept (char *, int);                 // 3
ENTRY emit (char);                          // 4
ENTRY type (char*, int);                    // 5
ENTRY cr (void);                            // 6
ENTRY cls (void);                           // 7
ENTRY spaces (int);                         //
ENTRY keyq (void);                          // 8
ENTRY qcr (int);                            // 9
ENTRY gotoxy (int, int);                    // 19
ENTRY getxy (void);                         // 20
ENTRY getcolrow (void);                     // 23
ENTRY setfgbg (int, int);                   // 27
ENTRY fpushkey (long);                      // 28
ENTRY thescreen (void);                     // 29
ENTRY charwh (void);                        // 30
ENTRY shiftmask (void);                     // 31
ENTRY resize (int, int);                    // 32
ENTRY cursorshape (int );                   // 35
ENTRY getcursor (void);                     // 36
ENTRY havemenu (int );                      // 37
ENTRY printdlg (long);                      // 39
ENTRY startprint (void);                    // 40
ENTRY pageprint (void);                     // 41
ENTRY endprint (void);                      // 42
ENTRY initprint (int);                      // 43
ENTRY closeprint (void);                    // 44
ENTRY wscroll (int );                       // 51
ENTRY rowoffset (void);                     // 52
ENTRY maxcolrow (void);                     // 53
ENTRY setmaxcolrow (int, int);              // 54
ENTRY qualityprint (void);                  // 57
ENTRY startapage (void);                    // 58
ENTRY endapage (void);                      // 59
ENTRY printcopies (void);                   // 60
ENTRY printfrompages (void);                // 61
ENTRY printtopages (void);                  // 62
ENTRY orientation (int);                    // 67
ENTRY setcharwh (int, int);                 // 71
ENTRY mark (long, long, long, long);        // 72
ENTRY getfg (void);                         // 73
ENTRY getbg (void);                         // 74
ENTRY initprint2 (int, int, int, int);      // 77
ENTRY printflags (void);                    // 78

#define typez(s)    type (s, strlen(s));
#define space()     emit(32)

extern BOOL doingdll;
