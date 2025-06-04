object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Serwer'
  ClientHeight = 844
  ClientWidth = 1123
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object MemoLog: TMemo
    Left = 48
    Top = 176
    Width = 425
    Height = 377
    Lines.Strings = (
      'MemoLog')
    TabOrder = 0
  end
  object sgPlayers: TStringGrid
    Left = 488
    Top = 176
    Width = 273
    Height = 377
    ColCount = 2
    RowCount = 1
    FixedRows = 0
    TabOrder = 1
    ColWidths = (
      64
      64)
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    OnExecute = IdTCPServer1Execute
    Left = 168
    Top = 32
  end
  object FDQuery1: TFDQuery
    Left = 48
    Top = 32
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=127.0.0.1'
      'Database=pascalchess'
      'User_Name=root'
      'DriverID=MySQL')
    Left = 88
    Top = 32
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = 
      'C:\Users\Mateusz\Documents\Embarcadero\Studio\Projects\libmysql.' +
      'dll'
    Left = 128
    Top = 32
  end
end
