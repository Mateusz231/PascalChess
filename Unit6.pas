unit Unit6;

interface



uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, System.Hash, UserSession, Unit4,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;



type
  TLogin = class(TForm)
    LoginLabel: TLabel;
    PasswordLabel: TLabel;
    LoginButton: TButton;
    BackButton: TButton;
    Password: TEdit;
    Login: TEdit;
    Image1: TImage;
    CheckBox1: TCheckBox;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    IdTCPClient1: TIdTCPClient;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure LoginButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);



  private
    function HashPassword(const Password: string): string;
    function ServerIsLogged(const Username: string): Boolean;
    function EnsureConnected: Boolean;
    function SendServerCommand(const Cmd: string): string;
    procedure ServerAddPlayer(const Username: string);
    procedure ServerRemovePlayer(const Username: string);
  public
    { Public declarations }
  end;

var
  Login: TLogin;

implementation



{$R *.dfm}




procedure TLogin.BackButtonClick(Sender: TObject);
begin
Self.Hide;
Application.MainForm.Show;
end;

procedure TLogin.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  Password.PasswordChar := #0 // poka¿ has³o
else
  Password.PasswordChar := '*'; // ukryj
end;



procedure TLogin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Application.terminate;
end;

procedure TLogin.FormCreate(Sender: TObject);
begin
Application.Title:='Log in';
Image1.SendToBack;
FormResize(self);
end;

procedure TLogin.FormKeyPress(Sender: TObject; var Key: Char);
begin
 MessageDlg(Key + ' has been pressed', mtInformation, [mbOK], 0)
end;

function TLogin.HashPassword(const Password: string): string;
begin
  Result := THashSHA2.GetHashString(Password);
end;

procedure TLogin.FormResize(Sender: TObject);
var
  centerX, spacing, fieldHeight: Integer;
begin
  spacing := 15;
  fieldHeight := 40;
  centerX := ClientWidth div 2;
  CheckBox1.Font.Color:= clWhite;
  CheckBox1.Font.Style := [fsBold];  // pogrubienie

  // Styl etykiet
  LoginLabel.Font.Color := clWhite;
  PasswordLabel.Font.Color := clWhite;
  LoginLabel.Font.Size := 14;
  PasswordLabel.Font.Size := 14;
  LoginLabel.Font.Style := [fsBold];
  PasswordLabel.Font.Style := [fsBold];

  // Login Label
  LoginLabel.Left := (ClientWidth - LoginLabel.Width) div 2;
  LoginLabel.Top := 50;

  // Login Edit
  Login.SetBounds(centerX - 150, LoginLabel.Top + LoginLabel.Height + spacing div 2, 300, fieldHeight);

  // Password Label (nad polem has³a)
  PasswordLabel.Top := Login.Top + fieldHeight + spacing;
  PasswordLabel.Left := (ClientWidth - PasswordLabel.Width) div 2;

  // Password Edit
  Password.SetBounds(centerX - 150, PasswordLabel.Top + PasswordLabel.Height + spacing div 2, 300, fieldHeight);

  // Buttons
  LoginButton.SetBounds(centerX - 150, Password.Top + fieldHeight + spacing * 2, 140, fieldHeight);
  BackButton.SetBounds(centerX + 10, LoginButton.Top, 140, fieldHeight);

end;

function TLogin.SendServerCommand(const Cmd: string): string;
begin
  if not EnsureConnected then
    Exit('');
  // bez kodowania
  IdTCPClient1.IOHandler.WriteLn(Cmd);
    Result := IdTCPClient1.IOHandler.ReadLn('', 1500);
end;



procedure TLogin.ServerRemovePlayer(const Username: string);
begin
  if IdTCPClient1.Connected then
    SendServerCommand('REMOVEPLAYER:' + Username);
end;





procedure TLogin.ServerAddPlayer(const Username: string);
begin
  SendServerCommand('ADDPLAYER:' + Username);
end;



function TLogin.ServerIsLogged(const Username: string): Boolean;
begin
  Result := SameText(SendServerCommand('ISLOGGED:' + Username), 'YES');
end;



function TLogin.EnsureConnected: Boolean;
begin
  Result := True;
  if not IdTCPClient1.Connected then
    try
      IdTCPClient1.Host := '127.0.0.1';
      IdTCPClient1.Port := 5000;
      IdTCPClient1.Connect;
      UserSession.IdTCPClient1:= Self.IdTCPClient1;
    except
      on E: Exception do
      begin
        ShowMessage('Nie uda³o siê po³¹czyæ z serwerem: ' + E.Message);
        Result := False;
      end;
    end;
end;



procedure TLogin.LoginButtonClick(Sender: TObject);

var
  query: TFDQuery;
  enteredLogin, enteredPassword, hashedPassword: string;

begin
  enteredLogin := Trim(Login.Text);
  enteredPassword := Trim(Password.Text);
  hashedPassword := HashPassword(enteredPassword);

  if (enteredLogin = '') or (enteredPassword = '') then
  begin
    ShowMessage('WprowadŸ login i has³o.');
    Exit;
  end;

  query := TFDQuery.Create(nil);
  try
    query.Connection := FDConnection1; // podmieñ na swoj¹ nazwê po³¹czenia
    query.SQL.Text := 'SELECT userid, login FROM users WHERE login = :login AND pass = :password';
    query.ParamByName('login').AsString := enteredLogin;
    query.ParamByName('password').AsString := hashedPassword;
    query.Open;

    if not query.IsEmpty then
    begin
        if not EnsureConnected then Exit;
        if ServerIsLogged(enteredLogin) then
        begin
        ShowMessage('Ten u¿ytkownik jest ju¿ zalogowany na serwerze.');
        Exit;
        end;

  // 3) dodaj na serwerze
       ServerAddPlayer(enteredLogin);


      UserSession.LoggedUserLogin:= enteredLogin;
      UserSession.Logged:= true;
      UserSession.LoggedUserID:= query.FieldByName('userid').AsInteger;
      MainMenu.Button6.Visible:= false;
      ShowMessage('Zalogowano pomyœlnie!');
      MainMenu.ShowLogoutButton;
      Self.Hide;
      Application.MainForm.Show; // lub cokolwiek chcesz pokazaæ
    end
    else
    begin
      ShowMessage('Nieprawid³owy login lub has³o.');
    end;
  finally
    query.Free;
  end;
end;





end.
