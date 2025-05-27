unit UserSession;

interface

procedure Logout;

var
  LoggedUserID: Integer;
  LoggedUserLogin: string;
  Logged: Boolean;

implementation

procedure Logout;
begin
  LoggedUserID := -1;
  LoggedUserLogin := '';
  Logged := False;
end;

end.
