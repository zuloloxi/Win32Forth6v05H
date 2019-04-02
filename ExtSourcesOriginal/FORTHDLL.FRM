VERSION 4.00
Begin VB.Form Form1 
   Caption         =   "FORTHDLL"
   ClientHeight    =   1440
   ClientLeft      =   2820
   ClientTop       =   1785
   ClientWidth     =   4635
   Height          =   1845
   Left            =   2760
   LinkTopic       =   "Form1"
   ScaleHeight     =   1440
   ScaleWidth      =   4635
   Top             =   1440
   Width           =   4755
   Begin VB.CommandButton Command1 
      Caption         =   "Interpret Forth Command Line"
      Height          =   375
      Left            =   360
      TabIndex        =   1
      Top             =   720
      Width           =   3975
   End
   Begin VB.TextBox CommandLine 
      Height          =   285
      Left            =   360
      TabIndex        =   0
      Top             =   240
      Width           =   3975
   End
End
Attribute VB_Name = "Form1"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Private Sub Command1_Click()
    InterpretForth CommandLine.Text
End Sub

