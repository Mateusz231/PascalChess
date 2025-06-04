object LeaderboardForm: TLeaderboardForm
  Left = 0
  Top = 0
  Caption = 'Leaderboard'
  ClientHeight = 619
  ClientWidth = 1116
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object LblPageInfo: TLabel
    Left = 86
    Top = 264
    Width = 37
    Height = 13
    Caption = 'Page: 1'
    Transparent = True
  end
  object GridLeaderboard: TStringGrid
    Left = 168
    Top = 120
    Width = 320
    Height = 120
    Color = clWhite
    ColCount = 4
    DefaultColWidth = 120
    FixedColor = clGray
    RowCount = 16
    TabOrder = 0
  end
  object cboGameType: TComboBox
    Left = 192
    Top = 48
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cboGameTypeChange
  end
  object BtnPrevPage: TButton
    Left = 48
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 2
    OnClick = BtnPrevPageClick
  end
  object BtnNextPage: TButton
    Left = 64
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 3
    OnClick = BtnNextPageClick
  end
  object BackButton: TButton
    Left = 296
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 4
    OnClick = BackButtonClick
  end
  object FDQuery1: TFDQuery
    Left = 880
    Top = 328
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=127.0.0.1'
      'Database=pascalchess'
      'User_Name=root'
      'DriverID=MySQL')
    Left = 880
    Top = 392
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = 
      'C:\Users\Mateusz\Documents\Embarcadero\Studio\Projects\libmysql.' +
      'dll'
    Left = 880
    Top = 464
  end
end
