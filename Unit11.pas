unit Unit11;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TGameType = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

  private
    procedure ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
  public
    { Public declarations }
  end;

var
  GameType: TGameType;

implementation

{$R *.dfm}

uses unit3, unit4;

procedure TGameType.Button1Click(Sender: TObject);
begin
  Self.Hide;  // chowasz GameType
  // Tworzysz now¹ instancjê gry
  Chess := TChess.Create(nil);
  // Ustawiasz typ gry
  Chess.SetGameType(1);
  // Pod³¹czasz handler, który po zamkniêciu gry przywróci menu g³ówne
  Chess.OnClose := ChessCloseHandler;
  // Poka¿ formê gry
  Chess.Show;
end;

procedure TGameType.Button2Click(Sender: TObject);
begin
 Self.Hide;  // chowasz GameType
  // Tworzysz now¹ instancjê gry
  Chess := TChess.Create(nil);
  // Ustawiasz typ gry
  Chess.SetGameType(2);
  // Pod³¹czasz handler, który po zamkniêciu gry przywróci menu g³ówne
  Chess.OnClose := ChessCloseHandler;
  // Poka¿ formê gry
  Chess.Show;
end;

procedure TGameType.Button3Click(Sender: TObject);
begin
 Self.Hide;  // chowasz GameType
  // Tworzysz now¹ instancjê gry
  Chess := TChess.Create(nil);
  // Ustawiasz typ gry
  Chess.SetGameType(3);
  // Pod³¹czasz handler, który po zamkniêciu gry przywróci menu g³ówne
  Chess.OnClose := ChessCloseHandler;
  // Poka¿ formê gry
  Chess.Show;
end;

procedure TGameType.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Application.Terminate;
  Self.Hide;
  Application.MainForm.Show;
end;

procedure TGameType.ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
  // 1) Zamykasz i zwalniasz formularz gry
  Action := caFree;

  // 2) Poka¿ menu g³ówne
  MainMenu.Show;

  // 3) (Opcjonalnie) przywróæ ten formularz wyboru typu
  //    albo go zwolnij, je¿eli nie chcesz go trzymaæ w tle:
  // Self.Show;
  // albo
  // Action := caFree;  // ¿eby te¿ TGameType znik³
end;


end.
