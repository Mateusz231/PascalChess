unit GameTypeMenu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, Vcl.StdCtrls, System.Generics.Collections, Vcl.ComCtrls,
  IdException, IdStack, System.Math, System.StrUtils;

type
  TGameType = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PlayerList: TScrollBox;
    Button4: TButton;
    Timer1: TTimer;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure InviteClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);




  private
    ActivePlayers: TDictionary<string, Boolean>;
    CurrentOffset: Integer;
    PageSize: Integer;
    EditSearch: TEdit;
    BtnPrevPage: TButton;
    BtnNextPage: TButton;
    PanelHeader: TPanel;

    procedure UpdateButtonsState(ReceivedCount: Integer);
    procedure HandleServerLine(const Line: string);
    procedure ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
    procedure EditSearchChange(Sender: TObject);
    procedure BtnPrevPageClick(Sender: TObject);
    procedure BtnNextPageClick(Sender: TObject);
    function SelectGameMode(out Mode: Integer): Boolean;
    function ShowDifficultyDialog: Integer;
    procedure ShowInviteDialog(const FromUser: string; const FromUserId: string; Gamemode: Integer);
  public
    procedure LoadPlayers;
  end;

var
  GameType: TGameType;

implementation

{$R *.dfm}

uses ChessGame, Main, UserSession, Unit3;

procedure TGameType.Button1Click(Sender: TObject);
begin
  if not UserSession.Logged then
    ShowMessage('Currently you are not logged')
  else
  begin
    Self.Hide;
    Timer1.Enabled:=false;
    Chess := TChess.Create(nil);
    Chess.SetInvited(false);
    Chess.SetGameType(1);
    Chess.OnClose := ChessCloseHandler;
    Chess.Show;
  end;
end;

procedure TGameType.Button2Click(Sender: TObject);
begin
  if not UserSession.Logged then
    ShowMessage('Currently you are not logged')
  else
  begin
    Self.Hide;
    Timer1.Enabled:=false;
    Chess := TChess.Create(nil);
    Chess.SetInvited(false);
    Chess.SetGameType(2);
    Chess.OnClose := ChessCloseHandler;
    Chess.Show;
  end;
end;

procedure TGameType.Button3Click(Sender: TObject);
begin
  if not UserSession.Logged then
    ShowMessage('Currently you are not logged')
  else
  begin
    Self.Hide;
    Timer1.Enabled:=false;
    Chess := TChess.Create(nil);
    Chess.SetInvited(false);
    Chess.SetGameType(3);
    Chess.OnClose := ChessCloseHandler;
    Chess.Show;
  end;
end;

procedure TGameType.Button4Click(Sender: TObject);
var
  difficulty: Integer;
begin
  Self.Hide;
  Timer1.Enabled:=false;
  difficulty := ShowDifficultyDialog;
  Chess := TChess.Create(nil);
  Chess.SetGameType(4);
  Chess.SetAILevel(difficulty);
  Chess.OnClose := ChessCloseHandler;
  Chess.Show;
end;

procedure TGameType.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(ActivePlayers) then
    ActivePlayers.Free;
    Timer1.Enabled:=false;
  Action := caFree;
  GameType := nil;
  Application.MainForm.Show;
end;

procedure TGameType.FormCreate(Sender: TObject);
var
  PanelNav: TPanel;
begin
  Image1.Align := alClient;
  Image1.SendToBack;

  PlayerList.Parent := Self;
  PlayerList.Align := alRight;
  PlayerList.Width := 250;
  PlayerList.BorderStyle := bsNone;
  PlayerList.VertScrollBar.Tracking := True;
  PlayerList.Color := clDkGray;

  ActivePlayers := TDictionary<string, Boolean>.Create;
  PageSize := 10;
  CurrentOffset := 0;

  PanelHeader := TPanel.Create(Self);
  PanelHeader.Parent := PlayerList;
  PanelHeader.Align := alTop;
  PanelHeader.Height := 72;
  PanelHeader.BevelOuter := bvNone;

  EditSearch := TEdit.Create(Self);
  EditSearch.Parent := PanelHeader;
  EditSearch.Align := alTop;
  EditSearch.Height := 28;
  EditSearch.TextHint := 'Search player...';
  EditSearch.OnChange := EditSearchChange;

  PanelNav := TPanel.Create(Self);
  PanelNav.Parent := PanelHeader;
  PanelNav.Align := alTop;
  PanelNav.Height := 36;
  PanelNav.BevelOuter := bvNone;

  BtnPrevPage := TButton.Create(Self);
  BtnPrevPage.Parent := PanelNav;
  BtnPrevPage.Caption := 'Prev';
  BtnPrevPage.SetBounds(6, 4, 70, 28);
  BtnPrevPage.OnClick := BtnPrevPageClick;

  BtnNextPage := TButton.Create(Self);
  BtnNextPage.Parent := PanelNav;
  BtnNextPage.Caption := 'Next';
  BtnNextPage.SetBounds(PanelNav.ClientWidth - 76, 4, 70, 28);
  BtnNextPage.Anchors := [akRight, akTop];
  BtnNextPage.OnClick := BtnNextPageClick;
end;

procedure TGameType.FormResize(Sender: TObject);
const
  Spacing = 16;
  Margin = 16;
var
  TotalH, StartY, BtnX, LeftAreaWidth: Integer;
begin
  LeftAreaWidth := ClientWidth - PlayerList.Width;

  TotalH := Button1.Height + Button2.Height + Button3.Height + 2 * Spacing;
  StartY := (ClientHeight - TotalH) div 2 - 60;

  BtnX := (LeftAreaWidth - Button1.Width) div 2;
  Button1.SetBounds(BtnX, StartY, Button1.Width, Button1.Height);
  Button2.SetBounds(BtnX, StartY + Button1.Height + Spacing, Button2.Width, Button2.Height);
  Button3.SetBounds(BtnX, StartY + Button1.Height + Spacing + Button2.Height + Spacing,
    Button3.Width, Button3.Height);
  Button4.SetBounds(BtnX,
    StartY + Button1.Height + Spacing + Button2.Height + Spacing + Button3.Height + Spacing,
    Button4.Width, Button4.Height);
end;

procedure TGameType.FormShow(Sender: TObject);
begin
  Timer1.Enabled:=true;
  CurrentOffset := 0;
  LoadPlayers;
end;

procedure TGameType.ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  MainMenu.Show;
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

    BtnRapid := TButton.Create(Dlg);
    BtnRapid.Parent := Dlg;
    BtnRapid.Caption := 'Rapid';
    BtnRapid.ModalResult := 101;
    BtnRapid.Left := 18;
    BtnRapid.Top := 70;
    BtnRapid.Width := 75;

    BtnBlitz := TButton.Create(Dlg);
    BtnBlitz.Parent := Dlg;
    BtnBlitz.Caption := 'Blitz';
    BtnBlitz.ModalResult := 102;
    BtnBlitz.Left := BtnRapid.Left + BtnRapid.Width;
    BtnBlitz.Top := 70;
    BtnBlitz.Width := 75;

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
  Result := 5;
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
    rg.ItemIndex := 1;

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

procedure TGameType.ShowInviteDialog(const FromUser: string; const FromUserId: string; Gamemode: Integer);
var
  GameModeName: String;
begin
  case GameMode of
    1: GameModeName := 'Rapid';
    2: GameModeName := 'Blitz';
    3: GameModeName := 'Bullet';
  else
    GameModeName := 'Rapid';
  end;

  if MessageDlg(Format('%s invites you to play (%s). Accept?', [FromUser, GameModeName]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if not UserSession.Logged then
    begin
      ShowMessage('Currently you are not logged');
      Exit;
    end;

    UserSession.IdTCPClient1.IOHandler.WriteLn(
      Format('INVITE_ACCEPT:%s:%d:%d:%s:%d',
        [ FromUser,                   // zapraszaj¹cy
          StrToInt(FromUserId),
          Gamemode,                   // tryb gry
          UserSession.LoggedUserLogin,// login akceptuj¹cego
          UserSession.LoggedUserID ]) // ID akceptuj¹cego
    );

  end
  else
  begin
    UserSession.IdTCPClient1.IOHandler.WriteLn(
      Format('INVITE_DECLINE:%s:%d', [FromUser, GameMode])
    );
  end;
end;

procedure TGameType.HandleServerLine(const Line: string);
var
  Parts: TArray<string>;
  Cmd: string;
begin
  if Line.Trim = '' then Exit;

  Parts := Line.Split([':']);
  Cmd   := UpperCase(Parts[0]);

  if Cmd = 'INVITED_BY' then
  begin
    if Length(Parts) >= 4 then
      ShowInviteDialog(Parts[1],Parts[3],StrToIntDef(Parts[2], 1));
  end
  else if Cmd = 'INVITE_ACCEPTED' then
  begin
    if Length(Parts) >= 3 then
    begin

    Timer1.Enabled:=false;
    Self.Hide;
    Chess := TChess.Create(nil);
    Chess.SetInvited(True);
    Chess.SetGameType(StrToInt(Parts[2]));
    Chess.OnClose := ChessCloseHandler;
    Chess.Show;

    end;
  end
  else if Cmd = 'INVITE_DECLINED' then
  begin
    if Length(Parts) >= 2 then
      ShowMessage('User ' + Parts[1] + ' declined your invitation.');
  end
 
end;


procedure TGameType.Timer1Timer(Sender: TObject);

var
  Line: string;
begin
  if UserSession.IdTCPClient1.Connected
     and UserSession.IdTCPClient1.IOHandler.InputBufferIsEmpty = False then
  begin
    try
      Line := UserSession.IdTCPClient1.IOHandler.ReadLn('', 1);
    except
      Exit;
    end;

    if Line <> '' then
      HandleServerLine(Line);
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
     UserSession.IdTCPClient1.IOHandler.WriteLn(
    'INVITE:' +
    Login + ':' +                // login przeciwnika
    IntToStr(Mode) + ':' +       // tryb gry
    UserSession.LoggedUserLogin + ':' +    // login zapraszaj¹cego
    IntToStr(UserSession.LoggedUserID) // ID zapraszaj¹cego
  );

  except
    on E: Exception do
      ShowMessage('Error sending invite: ' + E.Message);
  end;
end;

procedure TGameType.UpdateButtonsState(ReceivedCount: Integer);
begin
  BtnPrevPage.Enabled := CurrentOffset > 0;
  BtnNextPage.Enabled := ReceivedCount = PageSize;
end;

procedure TGameType.EditSearchChange(Sender: TObject);
begin
  CurrentOffset := 0;
  LoadPlayers;
end;

procedure TGameType.BtnPrevPageClick(Sender: TObject);
begin
  if CurrentOffset >= PageSize then
  begin
    Dec(CurrentOffset, PageSize);
    LoadPlayers;
  end;
end;

procedure TGameType.BtnNextPageClick(Sender: TObject);
begin
  Inc(CurrentOffset, PageSize);
  LoadPlayers;
end;

procedure TGameType.LoadPlayers;
var
  Msg, Line, login, flag: string;
  Parts: TArray<string>;
  ReceivedCount: Integer;
  InGame: Boolean;
  i: Integer;
  P: TControl;
begin
  ActivePlayers.Clear;

  if (not Assigned(UserSession.IdTCPClient1)) or
     (not UserSession.IdTCPClient1.Connected) then Exit;

  if Trim(EditSearch.Text) <> '' then
    Msg := Format('GET_PLAYERS:%d:%d:%s', [CurrentOffset, PageSize, Trim(EditSearch.Text)])
  else
    Msg := Format('GET_PLAYERS:%d:%d', [CurrentOffset, PageSize]);

  try
    UserSession.IdTCPClient1.IOHandler.WriteLn(Msg);
  except
    on E: Exception do
    begin
      ShowMessage('Error sending GET_PLAYERS: ' + E.Message);
      Exit;
    end;
  end;

  ReceivedCount := 0;
  try
    repeat
      Line := UserSession.IdTCPClient1.IOHandler.ReadLn();
      if Line = '' then Break;

      if Line.StartsWith('PLAYER:') then
      begin
        Parts := Line.Split([':']);
        if Length(Parts) = 3 then
        begin
          login := Parts[1];
          flag  := Parts[2];
          InGame := (flag = '1');
          ActivePlayers.AddOrSetValue(login, InGame);
          Inc(ReceivedCount);
        end;
      end;
    until False;
  except
    on E: Exception do
    begin
      ShowMessage('Error reading GET_PLAYERS response: ' + E.Message);
      Exit;
    end;
  end;

  PlayerList.DisableAlign;
  try
    for i := PlayerList.ControlCount - 1 downto 0 do
    begin
      P := PlayerList.Controls[i];
      if P <> PanelHeader then
        P.Free;
    end;
  finally
    PlayerList.EnableAlign;
  end;

  for login in ActivePlayers.Keys do
  begin
    var Panel := TPanel.Create(Self);
    Panel.Parent := PlayerList;
    Panel.Align := alTop;
    Panel.Height := 40;
    Panel.BevelOuter := bvNone;
    Panel.Margins.Left := 8;
    Panel.Margins.Right := 8;

    var Lbl := TLabel.Create(Self);
    Lbl.Parent := Panel;
    Lbl.Caption := login;
    Lbl.Font.Size := 12;
    Lbl.Align := alLeft;
    Lbl.Margins.Left := 6;

    if login <> UserSession.LoggedUserLogin then
    begin
      var Btn := TButton.Create(Self);
      Btn.Parent := Panel;
      Btn.Width := 72;
      Btn.Height := 28;
      Btn.Align := alRight;
      Btn.Caption := IfThen(ActivePlayers[login], 'Ingame', 'Invite');
      Btn.Enabled := not ActivePlayers[login];
      Btn.Hint := login;
      Btn.OnClick := InviteClick;
    end;
  end;

  UpdateButtonsState(ReceivedCount);
end;

end.
