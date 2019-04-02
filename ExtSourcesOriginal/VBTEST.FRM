VERSION 2.00
Begin Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   5850
   ClientLeft      =   3705
   ClientTop       =   2625
   ClientWidth     =   7365
   Height          =   6255
   Left            =   3645
   LinkTopic       =   "Form1"
   ScaleHeight     =   5850
   ScaleWidth      =   7365
   Top             =   2280
   Width           =   7485
   Begin CommandButton Command3 
      Caption         =   "Hex"
      Height          =   375
      Left            =   5760
      TabIndex        =   10
      Top             =   3000
      Width           =   1095
   End
   Begin CommandButton Command2 
      Caption         =   "Decimal"
      Height          =   375
      Left            =   5760
      TabIndex        =   9
      Top             =   2400
      Width           =   1095
   End
   Begin CommandButton Command1 
      Caption         =   "Get base"
      Height          =   495
      Left            =   720
      TabIndex        =   8
      Top             =   2640
      Width           =   1335
   End
   Begin CommandButton Base 
      Caption         =   "Set Base"
      Height          =   495
      Left            =   2640
      TabIndex        =   7
      Top             =   2640
      Width           =   1335
   End
   Begin TextBox Text3 
      Height          =   375
      Left            =   4680
      TabIndex        =   6
      Text            =   "10"
      Top             =   2640
      Width           =   615
   End
   Begin TextBox Text2 
      Height          =   375
      Left            =   1680
      TabIndex        =   5
      Text            =   "0"
      Top             =   1560
      Width           =   495
   End
   Begin CommandButton Add 
      Caption         =   "Add"
      Height          =   375
      Left            =   2640
      TabIndex        =   3
      Top             =   1560
      Width           =   735
   End
   Begin TextBox first 
      Height          =   375
      Left            =   720
      TabIndex        =   2
      Text            =   "0"
      Top             =   1560
      Width           =   615
   End
   Begin TextBox Text1 
      Height          =   615
      Left            =   720
      TabIndex        =   1
      Top             =   480
      Width           =   3015
   End
   Begin CommandButton Evaluate 
      Caption         =   "Evaluate"
      Height          =   615
      Left            =   4680
      TabIndex        =   0
      Top             =   480
      Width           =   1575
   End
   Begin Label Result 
      Caption         =   "0"
      Height          =   375
      Left            =   3720
      TabIndex        =   4
      Top             =   1680
      Width           =   615
   End
End
Dim Forth As Object




Sub Add_Click ()
    Dim n1, n2 As Integer
    n1 = first.Text
    n2 = text2.Text
    result.Caption = Forth.Add(n1, n2)
End Sub

Sub Base_Click ()
    Forth.Base = text3.Text
End Sub

Sub Command1_Click ()
    text3.Text = Forth.Base
End Sub

Sub Command2_Click ()
    Forth.Base = 10
    text3.Text = Forth.Base
End Sub

Sub Command3_Click ()
    Forth.Base = 16
    text3.Text = Forth.Base
End Sub

Sub Evaluate_Click ()
    Forth.Interpret (text1.Text)
    text1.Text = ""
End Sub

Sub Form_Load ()
    Set Forth = CreateObject("Forth.Application")
End Sub

