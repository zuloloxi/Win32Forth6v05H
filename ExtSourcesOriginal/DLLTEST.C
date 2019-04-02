
/******************************************************************************\
*
*  PROGRAM:     LOADTEST.C
*
*  PURPOSE:     A simple demonstration of LoadLibrary()-ing a DLL at
*               runtime and obtaining function addresses within the DLL.
*
*  FUNTIONS:    WinMain     - initialization, create window, msg loop
*               MainWndProc - processes main window msgs
*               ThreadProc  - makes a single call into "FORTHDLL.DLL"
*               AboutDlgProc- processes about dialog messages
*
\******************************************************************************/

#include <windows.h>
#include "dlltest.h"
#include "forthdll.h"
#include "messages.h"

char commandline[260];

/******************************************************************************\
*
*  FUNCTION:    WinMain (standard WinMain INPUTS/RETURNS)
*
*  GLOBAL VARS: ghwndMain - handle of main app window
*
*  LOCAL VARS:  msg - msg to get/dispatch
*
\******************************************************************************/

int PASCAL WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpCmdLine, int nCmdShow)
{
  MSG msg;

  if (!hPrevInstance)
  {
    WNDCLASS wc;

    wc.style         = 0;
    wc.lpfnWndProc   = (WNDPROC)MainWndProc;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hInstance     = hInstance;
    wc.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor       = LoadCursor (NULL, IDC_ARROW);
    wc.hbrBackground = GetStockObject (WHITE_BRUSH);
    wc.lpszMenuName  = (LPCTSTR) "Menu";
    wc.lpszClassName = (LPCTSTR) "LOADTEST";

    if (!RegisterClass (&wc))
    {
      MessageBox (NULL, (LPCTSTR) "WinMain(): RegisterClass() failed",
                  (LPCTSTR) "Err! - LOADTEST", MB_OK | MB_ICONEXCLAMATION);
      return(FALSE);
    }
  }

  if (!(ghwndMain = CreateWindow ("LOADTEST", "LOADTEST Sample Application",
                                  WS_OVERLAPPEDWINDOW,
                                  CW_USEDEFAULT, CW_USEDEFAULT,
                                  CW_USEDEFAULT, CW_USEDEFAULT,
                                  NULL, NULL, hInstance, NULL)))
    return (0);

  ShowWindow (ghwndMain, nCmdShow);

  while (GetMessage (&msg, NULL, 0, 0))
  {
    TranslateMessage (&msg);
    DispatchMessage  (&msg);
  }

  return (msg.wParam);
}



/******************************************************************************\
*
*  FUNCTION:    MainWndProc (standard window procedure INPUTS/RETURNS)
*
*  LOCAL VARS:  hLib     - handle of FORTHDLL.DLL
*
\******************************************************************************/

LRESULT CALLBACK MainWndProc (HWND hwnd, UINT message, WPARAM wParam,
                              LPARAM lParam)
{ static HANDLE hLib = NULL;
  static HMENU  hSubMenu;

  switch (message)
  {
    case WM_CREATE:
    {
      HMENU hMenu = GetMenu (hwnd);

      hSubMenu = GetSubMenu (hMenu, 0);
      FixMenu (IDM_FREELIBRARY, hSubMenu);
      break;
    }

    case WM_COMMAND:

      switch (LOWORD(wParam))
      {
        case IDM_LOADLIBRARY:

          if (!hLib)
          {
            if (!(hLib = LoadLibrary ("FORTHDLL.DLL")))

              MessageBox (hwnd,
                          (LPCTSTR) "Failed to load FORTHDLL.DLL",
                          (LPCTSTR) "Win32Forth DLL Text",
                          MB_OK | MB_ICONEXCLAMATION);
            else
            {
              gpfnDLLForthEntry = (PFNDLL) GetProcAddress (hLib,"DLLForthEntry");
              FixMenu (IDM_LOADLIBRARY, hSubMenu);
            }
          }
          break;

        case IDM_FREELIBRARY:

          if (hLib)
          {
            FreeLibrary (hLib);
            gpfnDLLForthEntry = NULL;
            FixMenu (IDM_FREELIBRARY, hSubMenu);
            hLib = NULL;
          }
          break;

        case IDM_DLLFORTHENTRY:

                        DialogBox (GetModuleHandle (NULL), (LPCTSTR)"FORTH", hwnd, (DLGPROC) ForthDlgProc);
                        break;

        case IDM_ABOUT:

          DialogBox (GetModuleHandle (NULL), (LPCTSTR)"ABOUT", hwnd, (DLGPROC) AboutDlgProc);
          break;

        default:

          break;
      }
      break;

    case WM_DESTROY:

      PostQuitMessage(0);
      break;

    default:

      return (DefWindowProc(hwnd, message, wParam, lParam));
  }
  return (0);
}



/******************************************************************************\
*
*  FUNCTION:    FixMenu
*
*  INPUTS:      choice   - IDM_LOADLIBRARY or IBM_FREELIBRARY
*               hSubMenu - handle of submenu, the items of which we'll
*                          enable/disable
*
*  LOCAL VARS:  i - loop variable
*
*  COMMENTS:    Enables/disables menuitems depending on whether FORTHDLL
*               is loaded/unloaded.
*
\******************************************************************************/

void FixMenu (UINT choice, HMENU hSubMenu)
{
  UINT i;

  if (choice == IDM_LOADLIBRARY)
  {
    EnableMenuItem (hSubMenu, IDM_LOADLIBRARY, MF_DISABLED | MF_GRAYED |
                                               MF_BYCOMMAND);

    for (i = IDM_FREELIBRARY; i <= IDM_DLLFORTHENTRY; i++)

      EnableMenuItem (hSubMenu, i, MF_ENABLED | MF_BYCOMMAND);
  }
  else /* choice == IDM_FREELIBRARY */
  {
    EnableMenuItem (hSubMenu, IDM_LOADLIBRARY, MF_ENABLED | MF_BYCOMMAND);

    for (i = IDM_FREELIBRARY; i <= IDM_DLLFORTHENTRY; i++)

      EnableMenuItem (hSubMenu, i, MF_DISABLED | MF_GRAYED |MF_BYCOMMAND);

  }
}


BOOL CALLBACK AboutDlgProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ switch (message)
  {
    case WM_COMMAND:

      if (LOWORD(wParam) == IDOK)
      {
        EndDialog(hwnd, TRUE);
        return (TRUE);
      }
      return (TRUE);
  }
  return (FALSE);
}

BOOL CALLBACK ForthDlgProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ switch (message)
  {
    case WM_COMMAND:

      if (LOWORD(wParam) == IDOK)
      {
                GetDlgItemText (hwnd, IDC_FORTHEDT, commandline, 255);
                        if (gpfnDLLForthEntry)
                    (gpfnDLLForthEntry) (F_INTERPRET, NULL, NULL, commandline);
//                              (gpfnDLLForthEntry) (F_WORDS, NULL, NULL, NULL);        
                return (TRUE);
      }
      if (LOWORD(wParam) == IDCANCEL)
      {
        EndDialog(hwnd, TRUE);
        return (TRUE);
      }
      return (TRUE);
  }
  return (FALSE);
}
