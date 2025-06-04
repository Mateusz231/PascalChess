program ChessArena;

uses
  Vcl.Forms,
  ChessGame in 'ChessGame.pas' {Chess},
  Main in 'Main.pas' {Menu},
  RegisterUser in 'RegisterUser.pas' {RegisterMenu},
  LoginUser in 'LoginUser.pas' {Login},
  UserSession in 'UserSession.pas',
  ProfileMenu in 'ProfileMenu.pas' {Profile},
  RankingsLeaderboard in 'RankingsLeaderboard.pas' {Form8},
  MatchHistory in 'MatchHistory.pas' {Form9},
  GameTypeMenu in 'GameTypeMenu.pas' {Form11};

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
