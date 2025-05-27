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
    Width = 633
    Height = 449
    Lines.Strings = (
      'MemoLog')
    TabOrder = 0
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    OnExecute = IdTCPServer1Execute
    Left = 736
    Top = 520
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
    Left = 800
    Top = 344
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    VendorLib = 
      'C:\Users\Mateusz\Documents\Embarcadero\Studio\Projects\libmysql.' +
      'dll'
    Left = 800
    Top = 416
  end
end
