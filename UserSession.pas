unit UserSession;

interface

uses IdTCPClient;

procedure Logout;

var
  LoggedUserID: Integer;
  LoggedUserLogin: string;
  Logged: Boolean;
  IdTCPClient1: TIdTCPClient;
  ShutDown: Boolean;

implementation

procedure Logout;
begin


  If IdTCPClient1.Connected then
  begin
  IdTCPClient1.IOHandler.WriteLn('REMOVEPLAYER:'+ LoggedUserLogin);
  IdTCPClient1.Disconnect;
  end;


  LoggedUserID := -1;
  LoggedUserLogin := '';
  Logged := False;





end;

initialization
  IdTCPClient1 := TIdTCPClient.Create(nil); // Tworzony na starcie aplikacji


finalization
IdTCPClient1.Free;





end.
