/******************************************************************************\
*                            SYMBOLIC CONSTANTS
\******************************************************************************/

#define IDM_LOADLIBRARY   100 /* menu identifiers */
#define IDM_FREELIBRARY   101
#define IDM_DLLFORTHENTRY 102
#define IDM_ABOUT         108
#define IDC_FORTHEDT	  110

typedef int (__stdcall *PFNDLL)();

HWND ghwndMain;

PFNDLL gpfnDLLForthEntry = NULL;

LRESULT CALLBACK MainWndProc  (HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK AboutDlgProc (HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK ForthDlgProc (HWND, UINT, WPARAM, LPARAM);

void FixMenu    (UINT, HMENU);
