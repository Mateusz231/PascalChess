program Project4;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Chess},
  Unit4 in 'Unit4.pas' {Menu},
  Unit5 in 'Unit5.pas' {RegisterMenu},
  Unit6 in 'Unit6.pas' {Login},
  UserSession in 'UserSession.pas',
  Unit7 in 'Unit7.pas' {Profile},
  Unit8 in 'Unit8.pas' {Form8},
  Unit9 in 'Unit9.pas' {Form9},
  Unit11 in 'Unit11.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainMenu, MainMenu);
  Application.CreateForm(TChess, Chess);
  Application.CreateForm(TRegisterMenu, RegisterMenu);
  Application.CreateForm(TLogin, Login);
  Application.CreateForm(TProfile, Profile);
  Application.CreateForm(TLeaderboardForm, LeaderboardForm);
  Application.CreateForm(TMatchHistoryForm, MatchHistoryForm);
  Application.CreateForm(TGameType, GameType);
  Application.Run;
end.
