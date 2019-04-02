Attribute VB_Name = "GCQDEFS"
Option Explicit

Declare Function DLLForthEntry Lib "FORTHDLL.DLL" (ByVal func As Long, ByVal Message As Long, ByVal wParam As Long, lParam As Any) As Long

Sub InterpretForth(InterpretLine As String)
        DLLForthEntry DLL_INTERPRET, 0, 0, InterpretLine    'no error checking on returned result
'       The following version of the above DLL call, raises an error code if the forth function fails.
'       If (DLLForthEntry(DLL_INTERPRET, 0, 0, InterpretLine) = 0) Then Err.Raise 1
End Sub

