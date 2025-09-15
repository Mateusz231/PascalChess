object Chess: TChess
  Left = 0
  Top = 0
  Caption = 'Chess'
  ClientHeight = 688
  ClientWidth = 1064
  Color = clGrayText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object YourLogin: TLabel
    Left = 232
    Top = 424
    Width = 3
    Height = 15
  end
  object YourRanking: TLabel
    Left = 232
    Top = 456
    Width = 3
    Height = 15
  end
  object OppLogin: TLabel
    Left = 232
    Top = 477
    Width = 3
    Height = 15
  end
  object OppRanking: TLabel
    Left = 232
    Top = 520
    Width = 3
    Height = 15
  end
  object lblWhiteTime: TLabel
    Left = 168
    Top = 432
    Width = 70
    Height = 15
    Caption = 'lblWhiteTime'
  end
  object lblBlackTime: TLabel
    Left = 168
    Top = 456
    Width = 67
    Height = 15
    Caption = 'lblBlackTime'
  end
  object Panel: TPanel
    Left = 384
    Top = 312
    Width = 185
    Height = 41
    Caption = 'Panel'
    ParentBackground = False
    TabOrder = 0
    OnClick = PanelClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 536
    Top = 392
  end
  object FDQuery1: TFDQuery
    Left = 800
    Top = 256
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=127.0.0.1'
      'Database=pascalchess'
      'User_Name=root'
      'DriverID=MySQL')
    LoginPrompt = False
    Left = 800
    Top = 344
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 808
    Top = 440
  end
  object tmrWhite: TTimer
    Enabled = False
    OnTimer = tmrWhiteTimer
    Left = 320
    Top = 528
  end
  object tmrBlack: TTimer
    Enabled = False
    OnTimer = tmrBlackTimer
    Left = 416
    Top = 544
  end
end
