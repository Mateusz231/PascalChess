unit RegisterUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.MySQLDef, FireDAC.Phys.MySQL,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, System.Hash, UserSession;

type
  TRegisterMenu = class(TForm)
    RegisterButton: TButton;
    BackButton: TButton;
    Login: TEdit;
    Password: TEdit;
    LoginLabel: TLabel;
    PasswordLabel: TLabel;
    Image1: TImage;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDQuery1: TFDQuery;
    procedure BackButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RegisterButtonClick(Sender: TObject);


  private
      function HashPassword(const Password: string): string;
  public
    { Public declarations }
  end;

var
  RegisterMenu: TRegisterMenu;

implementation

{$R *.dfm}

procedure TRegisterMenu.BackButtonClick(Sender: TObject);
begin
Self.Hide;
Application.MainForm.Show;

end;

procedure TRegisterMenu.FormClose(Sender: TObject; var Action: TCloseAction);

begin
if UserSession.Logged then
begin
UserSession.Logout;
Application.terminate;
end

else
begin
Application.terminate;
end;


end;

procedure TRegisterMenu.FormCreate(Sender: TObject);
begin
FDPhysMySQLDriverLink1.VendorLib := ExtractFilePath(Application.ExeName) + 'libmysql.dll';
Image1.SendToBack;
FormResize(self);
end;




function TRegisterMenu.HashPassword(const Password: string): string;
begin
  Result := THashSHA2.GetHashString(Password); // Domyœlnie SHA-256
end;



procedure TRegisterMenu.FormResize(Sender: TObject);


var
  formCenterX, spacing, fieldHeight, labelHeight, buttonWidth, buttonHeight, startY: Integer;
begin
  spacing := 15;
  fieldHeight := 40;
  labelHeight := 20;
  buttonWidth := 130;
  buttonHeight := 40;
  formCenterX := ClientWidth div 2;

  // Start Y offset
  startY := 60;

  // Login label & edit
  LoginLabel.Top := startY;
  LoginLabel.Left := formCenterX - LoginLabel.Width div 2;

  Login.Top := LoginLabel.Top + labelHeight + 5;
  Login.Left := formCenterX - 150;
  Login.Width := 300;
  Login.Height := fieldHeight;

  // Password label & edit
  PasswordLabel.Top := Login.Top + fieldHeight + spacing;
  PasswordLabel.Left := formCenterX - PasswordLabel.Width div 2;

  PasswordLabel.Font.Color := clWhite;
  LoginLabel.Font.Color := clWhite;

  PasswordLabel.Font.Size := 14;
  LoginLabel.Font.Size := 14;
  PasswordLabel.Font.Style := [fsBold];
  LoginLabel.Font.Style := [fsBold];


  Password.Top := PasswordLabel.Top + labelHeight + 5;
  Password.Left := formCenterX - 150;
  Password.Width := 300;
  Password.Height := fieldHeight;

  // Buttons
  RegisterButton.Top := Password.Top + fieldHeight + spacing * 2;
  RegisterButton.Left := formCenterX - buttonWidth - spacing div 2;
  RegisterButton.Width := buttonWidth;
  RegisterButton.Height := buttonHeight;

  BackButton.Top := RegisterButton.Top;
  BackButton.Left := formCenterX + spacing div 2;
  BackButton.Width := buttonWidth;
  BackButton.Height := buttonHeight;


end;

procedure TRegisterMenu.RegisterButtonClick(Sender: TObject);
var
  HashedPass, LoginStr: string;
  TempID: Integer;
  j: Integer;
begin
  LoginStr := Trim(Login.Text);
  HashedPass := HashPassword(Trim((Password.Text)));

  if (LoginStr = '') or (Password.Text = '') then
  begin
    ShowMessage('Uzupe³nij wszystkie pola.');
    Exit;
  end;

  if Length(Password.Text) < 6 then
begin
  ShowMessage('Has³o musi mieæ co najmniej 6 znaków.');
  Exit;
end;

if Pos(' ', LoginStr) > 0 then
begin
  ShowMessage('Login nie mo¿e zawieraæ spacji.');
  Exit;
end;

  // SprawdŸ czy login ju¿ istnieje
  FDQuery1.SQL.Clear;
  FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM users WHERE login = :user';
  FDQuery1.ParamByName('user').AsString := LoginStr;
  FDQuery1.Open;

  if FDQuery1.Fields[0].AsInteger > 0 then
  begin
    ShowMessage('Taki login ju¿ istnieje. Wybierz inny.');
    Exit;
  end;

  FDQuery1.Close;

  // Jeœli nie istnieje — wstaw nowego u¿ytkownika
  FDQuery1.SQL.Text := 'INSERT INTO users (login, pass, creation_date, last_modified) VALUES (:login, :pass, :cdate, :lmodified)';
  FDQuery1.ParamByName('login').AsString := LoginStr;
  FDQuery1.ParamByName('pass').AsString := HashedPass;
  FDQuery1.ParamByName('cdate').AsDate := Date;
  FDQuery1.ParamByName('lmodified').AsDateTime := Now;





  try
    FDQuery1.ExecSQL;

    FDQuery1.SQL.Text := 'SELECT LAST_INSERT_ID() AS NewID';
    FDQuery1.Open;
    TempID := FDQuery1.FieldByName('NewID').AsInteger;
    FDQuery1.SQL.Text := 'UPDATE users SET who_modified = :who WHERE userid = :userid';
    FDQuery1.ParamByName('who').AsInteger := TempID;
    FDQuery1.ParamByName('userid').AsInteger := TempID;
    FDQuery1.ExecSQL;
    FDQuery1.Close;


    for j := 1 to 3 do
    begin
    FdQuery1.SQL.Text:= 'INSERT INTO rankings (users_userid, game_type_id) VALUES (:users_userid, :game_type_id)';
    FDQuery1.ParamByName('users_userid').AsInteger:= TempID;
    FDQuery1.ParamByName('game_type_id').AsInteger:= j;
    FDQuery1.ExecSQL;
    end;

    ShowMessage('Zarejestrowano pomyœlnie!');
    Self.Hide;
    Application.MainForm.Show;
  except
    on E: Exception do
      ShowMessage('B³¹d rejestracji: ' + E.Message);
  end;
end;


end.
