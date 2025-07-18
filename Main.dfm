object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pMain: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 37
    Align = alTop
    TabOrder = 0
    object lbNum: TLabel
      Left = 1
      Top = 1
      Width = 91
      Height = 15
      Align = alLeft
      Alignment = taCenter
      Caption = #1063#1080#1089#1083#1086' '#1076#1083#1103' '#1074#1074#1086#1076#1072
    end
    object edNum: TEdit
      Left = 97
      Top = 1
      Width = 121
      Height = 35
      Align = alLeft
      NumbersOnly = True
      TabOrder = 0
      OnKeyPress = edNumKeyPress
      ExplicitLeft = 112
      ExplicitTop = 8
      ExplicitHeight = 23
    end
    object btnStart: TButton
      Left = 236
      Top = 6
      Width = 75
      Height = 25
      Caption = #1047#1072#1087#1091#1089#1082
      TabOrder = 1
      OnClick = btnStartClick
    end
  end
  object pThread1: TPanel
    Left = 0
    Top = 37
    Width = 185
    Height = 404
    Align = alLeft
    Caption = 'pThread1'
    TabOrder = 1
    ExplicitLeft = 156
    ExplicitTop = 128
    ExplicitHeight = 41
    object lbThread1: TLabel
      Left = 1
      Top = 1
      Width = 79
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = #1055#1077#1088#1074#1099#1081' '#1087#1086#1090#1086#1082
    end
    object mThread1: TMemo
      Left = 1
      Top = 16
      Width = 183
      Height = 387
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pThread2: TPanel
    Left = 185
    Top = 37
    Width = 185
    Height = 404
    Align = alLeft
    Caption = 'pThread2'
    TabOrder = 2
    ExplicitLeft = 220
    ExplicitTop = 136
    ExplicitHeight = 41
    object lbThread2: TLabel
      Left = 1
      Top = 1
      Width = 75
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = #1042#1090#1086#1088#1086#1081' '#1087#1086#1090#1086#1082
    end
    object mThread2: TMemo
      Left = 1
      Top = 16
      Width = 183
      Height = 387
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pResult: TPanel
    Left = 370
    Top = 37
    Width = 254
    Height = 404
    Align = alClient
    Caption = 'pResult'
    TabOrder = 3
    ExplicitLeft = 412
    ExplicitTop = 132
    ExplicitWidth = 185
    ExplicitHeight = 41
    object lbResult: TLabel
      Left = 1
      Top = 1
      Width = 53
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090
    end
    object mResult: TMemo
      Left = 1
      Top = 16
      Width = 252
      Height = 387
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
