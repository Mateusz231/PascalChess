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
    PlayerList: TScrollBox;
    Button4: TButton;



    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure InviteClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);

  private
  ActivePlayers: TDictionary<string, boolean>;
  procedure ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
  function SelectGameMode(out Mode: Integer): Boolean;
  function ShowDifficultyDialog: Integer;

  public
  procedure RefreshPlayerList;
  end;

var
  GameType: TGameType;

implementation

{$R *.dfm}

uses ChessGame, Main, UserSession, Unit3;

procedure TGameType.Button1Click(Sender: TObject);
begin

if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
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
end;


procedure TGameType.Button2Click(Sender: TObject);
begin


if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
begin
 Self.Hide;

  Chess := TChess.Create(nil);

  Chess.SetGameType(2);

  Chess.OnClose := ChessCloseHandler;

  Chess.Show;
end;



end;


procedure TGameType.Button3Click(Sender: TObject);
begin

if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
begin
 Self.Hide;

  Chess := TChess.Create(nil);

  Chess.SetGameType(3);

  Chess.OnClose := ChessCloseHandler;

  Chess.Show;
end;
end;





procedure TGameType.Button4Click(Sender: TObject);
begin

var difficulty: Integer;

 Self.Hide;

  difficulty := ShowDifficultyDialog;

  Chess := TChess.Create(nil);

  Chess.SetGameType(4);

  Chess.SetAILevel(difficulty);

  Chess.OnClose := ChessCloseHandler;

  Chess.Show;

end;

procedure TGameType.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //UserSession.IdTCPClient1.IOHandler.InputBuffer.Clear;
  Action:= caFree;
  GameType := nil;
  Application.MainForm.Show;
end;

procedure TGameType.FormCreate(Sender: TObject);
begin

  Image1.Align := alClient;
  Image1.SendToBack;

  PlayerList.Parent := Self;
  PlayerList.Align := alRight;
  PlayerList.Width := 250;
  PlayerList.BorderStyle := bsNone;
  PlayerList.VertScrollBar.Tracking := True;

  PlayerList.Color:= clDkGray;

  ActivePlayers := TDictionary<string, Boolean>.Create;

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
  Button2.SetBounds(BtnX, StartY + Button1.Height + Spacing, Button2.Width, Button2.Height);
  Button3.SetBounds(BtnX, StartY + Button1.Height + Spacing + Button2.Height + Spacing, Button3.Width, Button3.Height);
  Button4.SetBounds(BtnX, StartY + Button1.Height + Spacing + Button2.Height + Spacing + Button3.Height + Spacing, Button4.Width, Button4.Height);

end;


procedure TGameType.FormShow(Sender: TObject);
begin
Resize;
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

function TGameType.SelectGameMode(out Mode: Integer): Boolean;
var
  Dlg: TForm;
  BtnRapid, BtnBlitz, BtnBullet: TButton;
begin
  Result := False;
  Mode := 0;

  Dlg := CreateMessageDialog('Select game mode to invite:', mtInformation, []);
  try
    Dlg.Caption := 'Choose Game Mode';

    // RAPID
    BtnRapid := TButton.Create(Dlg);
    BtnRapid.Parent := Dlg;
    BtnRapid.Caption := 'Rapid';
    BtnRapid.ModalResult := 101;
    BtnRapid.Left := 18;
    BtnRapid.Top := 70;
    BtnRapid.Width := 75;

    // BLITZ
    BtnBlitz := TButton.Create(Dlg);
    BtnBlitz.Parent := Dlg;
    BtnBlitz.Caption := 'Blitz';
    BtnBlitz.ModalResult := 102;
    BtnBlitz.Left := BtnRapid.Left + BtnRapid.Width;
    BtnBlitz.Top := 70;
    BtnBlitz.Width := 75;

    // BULLET
    BtnBullet := TButton.Create(Dlg);
    BtnBullet.Parent := Dlg;
    BtnBullet.Caption := 'Bullet';
    BtnBullet.ModalResult := 103;
    BtnBullet.Left := BtnBlitz.Left + BtnBlitz.Width;
    BtnBullet.Top := 70;
    BtnBullet.Width := 75;

    Dlg.Height := 150;
    Dlg.Width := 280;
    Dlg.Position := poScreenCenter;

    case Dlg.ShowModal of
      101: begin Mode := 1; Result := True; end;
      102: begin Mode := 2; Result := True; end;
      103: begin Mode := 3; Result := True; end;
    end;
  finally
    Dlg.Free;
  end;
end;


function TGameType.ShowDifficultyDialog: Integer;
const
  Labels: array[0..3] of string = (
    'Easy (1)',
    'Medium (5)',
    'Hard (10)',
    'Master (20)'
  );
  Values: array[0..3] of Integer = (1, 5, 10, 20);
var
  dlg: TForm;
  rg: TRadioGroup;
  btnOk, btnCancel: TButton;
  i: Integer;
begin
  Result := 5; // domyœlny poziom
  dlg := TForm.Create(nil);
  try
    dlg.Caption := 'Wybierz poziom trudnoœci AI';
    dlg.BorderStyle := bsDialog;
    dlg.Position := poScreenCenter;
    dlg.ClientWidth := 360;
    dlg.ClientHeight := 180;

    rg := TRadioGroup.Create(dlg);
    rg.Parent := dlg;
    rg.Left := 10;
    rg.Top := 10;
    rg.Width := dlg.ClientWidth - 20;
    rg.Height := 100;
    rg.Items.Clear;
    for i := 0 to High(Labels) do
      rg.Items.Add(Labels[i]);
    rg.ItemIndex := 1; // domyœlnie "Œredni"

    btnOk := TButton.Create(dlg);
    btnOk.Parent := dlg;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.SetBounds(dlg.ClientWidth div 2 - 90, dlg.ClientHeight - 50, 80, 28);

    btnCancel := TButton.Create(dlg);
    btnCancel.Parent := dlg;
    btnCancel.Caption := 'Anuluj';
    btnCancel.ModalResult := mrCancel;
    btnCancel.SetBounds(dlg.ClientWidth div 2 + 10, dlg.ClientHeight - 50, 80, 28);

    if dlg.ShowModal = mrOk then
    begin
      i := rg.ItemIndex;
      if (i >= 0) and (i <= High(Values)) then
        Result := Values[i];
    end;
  finally
    dlg.Free;
  end;
end;



procedure TGameType.InviteClick(Sender: TObject);
var
  Btn: TButton;
  Login: string;
  Mode: Integer;
begin
  Btn := Sender as TButton;
  Login := Btn.Hint;

  if not SelectGameMode(Mode) then
    Exit;

  try
    UserSession.IdTCPClient1.IOHandler.WriteLn('INVITE:' + Login + ':' + IntToStr(Mode));
    ShowMessage('Invitation sent to ' + Login);
  except
    on E: Exception do
      ShowMessage('Error sending invite: ' + E.Message);
  end;
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

begin

    ActivePlayers.Clear;

  // 1) Wyœlij ¿¹danie
  if not Assigned(UserSession.IdTCPClient1)
    or not UserSession.IdTCPClient1.Connected then
  begin
    //ShowMessage('Brak po³¹czenia z serwerem.');
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

    lbl := TLabel.Create(Self);
    lbl.Parent := Panel;
    lbl.Caption := PlayerName;
    lbl.SetBounds(0, 0, Panel.Width div 2, 24);
    lbl.Font.Name := 'Segoe UI';
    lbl.Font.Size := 20;
    lbl.Font.Color := clNavy;

    if PlayerName = UserSession.LoggedUserLogin then
    begin

    end
    else
    begin
        btn := TButton.Create(Self);
    btn.Parent := Panel;

    If InGame Then
     begin
        Btn.Caption :='InGame';
     end
     else
     begin
     Btn.Caption:='Invite';
     end;



    btn.SetBounds(Panel.Width - 72 - 8, 4, 72, 32);
    btn.Font.Color := clWhite;
    Btn.Enabled := not InGame;
    Btn.Top := 5;
    Btn.Left := Panel.Width - Btn.Width - 10;
    Btn.Hint:= PlayerName;
    Btn.Tag := Integer(Pointer(PChar(PlayerName)));
    Btn.OnClick :=InviteClick;















    end;
    Inc(PosY, Panel.Height + 5);

  end;
end;













end.
