object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Kalkulator'
  ClientHeight = 712
  ClientWidth = 1069
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 417
    Top = 237
    Width = 41
    Height = 23
    Caption = 'Wynik'
  end
  object Button1: TButton
    Left = 464
    Top = 296
    Width = 49
    Height = 49
    Caption = '+'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 551
    Top = 296
    Width = 50
    Height = 49
    Caption = '-'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 464
    Top = 367
    Width = 49
    Height = 49
    Caption = '/'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 551
    Top = 367
    Width = 50
    Height = 49
    Caption = '*'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 384
    Top = 208
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 551
    Top = 208
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object Edit3: TEdit
    Left = 464
    Top = 237
    Width = 121
    Height = 23
    TabOrder = 6
  end
end
