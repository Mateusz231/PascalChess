object MatchHistoryForm: TMatchHistoryForm
  Left = 0
  Top = 0
  Caption = 'Match History'
  ClientHeight = 845
  ClientWidth = 1125
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object LblPageInfo: TLabel
    Left = 320
    Top = 416
    Width = 63
    Height = 15
    Caption = 'LblPageInfo'
  end
  object BtnPrevPage: TButton
    Left = 320
    Top = 480
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 0
    OnClick = BtnPrevPageClick
  end
  object BtnNextPage: TButton
    Left = 320
    Top = 511
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 1
    OnClick = BtnNextPageClick
  end
  object BackButton: TButton
    Left = 320
    Top = 542
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 2
    OnClick = BackButtonClick
  end
  object tsgrid: TStringGrid
    Left = 416
    Top = 200
    Width = 320
    Height = 120
    TabOrder = 3
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
    LoginPrompt = False
    Left = 880
    Top = 392
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 880
    Top = 464
  end
end
