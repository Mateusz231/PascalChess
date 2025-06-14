unit GameTypeMenu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.ExtCtrls,
  Vcl.StdCtrls, System.Generics.Collections, Vcl.ComCtrls, IdException, IdStack, System.Math;

type
  TGameType = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PlayerList: TScrollBox; // <--- dodaj



    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);


  private
  ActivePlayers: TDictionary<string, boolean>;
  procedure ChessCloseHandler(Sender: TObject; var Action: TCloseAction);

  public
  procedure RefreshPlayerList;
  end;

var
  GameType: TGameType;

implementation

{$R *.dfm}

uses ChessGame, Main, UserSession;

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
 Self.Hide;

  Chess := TChess.Create(nil);

  Chess.SetGameType(2);

  Chess.OnClose := ChessCloseHandler;

  Chess.Show;
end;

procedure TGameType.Button3Click(Sender: TObject);
begin
 Self.Hide;

  Chess := TChess.Create(nil);

  Chess.SetGameType(3);

  Chess.OnClose := ChessCloseHandler;

  Chess.Show;
end;

procedure TGameType.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Action:= caFree;
  Application.MainForm.Show;
end;

procedure TGameType.FormCreate(Sender: TObject);
begin

  Image1.Align := alClient;
  Image1.SendToBack;


  // Dodaj ScrollBox po prawej
 // PlayerList := TScrollBox.Create(Self);
  PlayerList.Parent := Self;
  PlayerList.Align := alRight;
  PlayerList.Width := 250;
  PlayerList.BorderStyle := bsNone;
  PlayerList.VertScrollBar.Tracking := True;


  ActivePlayers := TDictionary<string, Boolean>.Create;
 // Resize;

end;

procedure TGameType.FormResize(Sender: TObject);
const
  Spacing = 16;
var
  TotalH, StartY, BtnX, LeftAreaWidth: Integer;
begin
  LeftAreaWidth := ClientWidth - PlayerList.Width;

  TotalH := Button1.Height + Button2.Height + Button3.Height + 2*Spacing;
  StartY := (ClientHeight - TotalH) div 2;

  BtnX := (LeftAreaWidth - Button1.Width) div 2;

  Button1.SetBounds(BtnX, StartY, Button1.Width, Button1.Height);
  Button2.SetBounds(BtnX, StartY + Button1.Height + Spacing,
                    Button2.Width, Button2.Height);
  Button3.SetBounds(BtnX, StartY + Button1.Height + Spacing + Button2.Height + Spacing,
                    Button3.Width, Button3.Height);
  RefreshPlayerList;

end;


procedure TGameType.FormShow(Sender: TObject);
begin
Resize;

  // UserSession.IdTCPClient1.IOHandler.WriteLn('GET_PLAYERS');

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

  //
end;



procedure TGameType.RefreshPlayerList;
var
  i, PosY: Integer;
  Panel: TPanel;
  Btn: TButton;
  Lbl: TLabel;
  PlayerName: string;
  InGame: Boolean;

    Line: string;
  Parts: TArray<string>;
  login: string;
  flag: string;
 // inGame: Boolean;
begin

    ActivePlayers.Clear;

  // 1) Wyœlij ¿¹danie
  if not Assigned(UserSession.IdTCPClient1)
    or not UserSession.IdTCPClient1.Connected then
  begin
    ShowMessage('Brak po³¹czenia z serwerem.');
    Exit;
  end;

  try
    UserSession.IdTCPClient1.IOHandler.WriteLn('GET_PLAYERS');
  except
    on E: EIdConnClosedGracefully do Exit;
    on E: Exception do
      raise Exception.Create('B³¹d wys³ania GET_PLAYERS: ' + E.Message);
  end;

  // 2) Odczyt odpowiedzi — zak³adamy, ¿e serwer oddaje X linii zakoñczonych pust¹
  repeat
    try
      Line := UserSession.IdTCPClient1.IOHandler.ReadLn();
    except
      on E: EIdConnClosedGracefully do Break;
      on E: Exception do
        raise Exception.Create('B³¹d odczytu listy graczy: ' + E.Message);
    end;

    // jeœli pusty wiersz, koniec
    if Line = '' then
      Break;

    // 3) Parsujemy liniê PLAYER:login:flag
    if Line.StartsWith('PLAYER:') then
    begin
      Parts := Line.Split([':'], 3);
      if Length(Parts) = 3 then
      begin
        login := Parts[1];
        flag  := Parts[2];
        inGame := (flag = '1');
        ActivePlayers.AddOrSetValue(login, inGame);
      end;
    end;
  until False;





  for i := PlayerList.ControlCount - 1 downto 0 do
    PlayerList.Controls[i].Free;

  PosY := 10;

  for PlayerName in ActivePlayers.Keys do
  begin
    InGame := ActivePlayers.Items[PlayerName];

    Panel := TPanel.Create(Self);
    Panel.Parent := PlayerList;
    Panel.Width := PlayerList.ClientWidth - 20;
    Panel.Height := 40;
    Panel.Top := PosY;
    Panel.Left := 10;
    Panel.BevelOuter := bvNone;

    Lbl := TLabel.Create(Self);
    Lbl.Parent := Panel;
    Lbl.Caption := PlayerName;
    Lbl.Left := 10;
    Lbl.Top := 10;

    Btn := TButton.Create(Self);
    Btn.Parent := Panel;


     If InGame Then
     begin
        Btn.Caption :='InGame';
     end
     else
     begin
     Btn.Caption:='Invite';
     end;

    Btn.Enabled := not InGame;
    Btn.Top := 5;
    Btn.Left := Panel.Width - Btn.Width - 10;
    Btn.Tag := Integer(Pointer(PChar(PlayerName))); // do ewentualnej obs³ugi klikniêcia
    // Btn.OnClick := @OnInviteClick; // Mo¿na dodaæ osobn¹ procedurê

    Inc(PosY, Panel.Height + 5);
  end;
end;





end.
