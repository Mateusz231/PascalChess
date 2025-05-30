unit UserSession;

interface

uses IdTCPClient;

procedure Logout;

var
  LoggedUserID: Integer;
  LoggedUserLogin: string;
  Logged: Boolean;
  IdTCPClient1: TIdTCPClient;

implementation

procedure Logout;
begin


  If IdTCPClient1.Connected then
  begin
  IdTCPClient1.IOHandler.WriteLn('REMOVEPLAYER:'+ LoggedUserLogin);
  end;


  LoggedUserID := -1;
  LoggedUserLogin := '';
  Logged := False;



end;

end.
