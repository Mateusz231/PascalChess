unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Unit3, System.ImageList,
  Vcl.ImgList, Vcl.ExtCtrls, Vcl.Imaging.jpeg, Unit5, UserSession,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;


type
  TMainMenu = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    LogoutButton: TButton;
    Image1: TImage;
    procedure ButtonPlay1Click(Sender: TObject);
    procedure ButtonExit5Click(Sender: TObject);
    procedure ButtonProfile2Click(Sender: TObject);
    procedure ButtonHistory3Click(Sender: TObject);
    procedure ButtonLeaderboard4Click(Sender: TObject);
    procedure ButtonLogin6Click(Sender: TObject);
    procedure ButtonRegister7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LogoutButtonClick(Sender: TObject);


  private
  public
     procedure ShowLogoutButton;
  end;

var
  MainMenu: TMainMenu;

implementation

{$R *.dfm}

uses Unit6, Unit7, Unit8, Unit9, Unit11;



procedure TMainMenu.ButtonPlay1Click(Sender: TObject);
begin

if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
begin
Self.Hide;
GameType.Show;
end;

end;


procedure TMainMenu.ButtonProfile2Click(Sender: TObject);
begin
if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
begin
Self.Hide;
Profile.Show;
end;

end;

procedure TMainMenu.ButtonHistory3Click(Sender: TObject);
begin

if not UserSession.Logged then
begin
ShowMessage('Currently you are not logged');
end
else
begin
Self.Hide;
MatchHistoryForm.Show;
end;


end;

procedure TMainMenu.ButtonLeaderboard4Click(Sender: TObject);
begin
Self.Hide;
LeaderboardForm.Show;
end;

procedure TMainMenu.ButtonExit5Click(Sender: TObject);
begin
UserSession.Logout;
Application.Terminate;
end;

procedure TMainMenu.ButtonLogin6Click(Sender: TObject);
begin
Self.Hide;
Login.Show;
end;

procedure TMainMenu.ButtonRegister7Click(Sender: TObject);
begin
Self.Hide;
RegisterMenu.Show;
end;


procedure TMainMenu.ShowLogoutButton;
begin
  LogoutButton.Visible := True;
end;



procedure TMainMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
UserSession.Logout;
Application.terminate;
end;

procedure TMainMenu.FormCreate(Sender: TObject);
begin
Image1.SendToBack;
FormResize(self);
end;

procedure TMainMenu.FormResize(Sender: TObject);

var
  spacing, btnWidth, btnHeight, totalHeight, startY, centerX: Integer;
begin
  // Ustawienia przycisków
  spacing := 10;
  btnWidth := 240;
  btnHeight := 80;
  totalHeight := (btnHeight + spacing) * 4 + btnHeight + 40; // 4 rzêdy + Exit + label

  centerX := ClientWidth div 2;

  // Ustaw label na œrodku
  Label1.Left := (ClientWidth - Label1.Width) div 2;
  Label1.Top := 40;

  // Przycisk Play
  Button1.SetBounds(centerX - btnWidth - spacing div 2, Label1.Top + 60, btnWidth, btnHeight);
  Button6.SetBounds(centerX + spacing div 2, Label1.Top + 60, btnWidth, btnHeight);

  // Your Profile & Register
  Button2.SetBounds(Button1.Left, Button1.Top + btnHeight + spacing, btnWidth, btnHeight);
  Button7.SetBounds(Button6.Left, Button2.Top, btnWidth, btnHeight);

  // Leaderboards & History
  Button4.SetBounds(Button1.Left, Button2.Top + btnHeight + spacing, btnWidth, btnHeight);
  Button3.SetBounds(Button6.Left, Button4.Top, btnWidth, btnHeight);

  // Exit — na œrodku
  Button5.SetBounds(centerX - btnWidth div 2, Button4.Top + btnHeight + spacing + 10, btnWidth, btnHeight);
end;



procedure TMainMenu.LogoutButtonClick(Sender: TObject);
begin
UserSession.Logout;
ShowMessage('Wylogowano');
LogoutButton.Visible:= false;
Button6.Visible:= true;
end;

end.
