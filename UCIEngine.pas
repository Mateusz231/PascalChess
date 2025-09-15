unit UCIEngine;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes;

type
  TUciEngine = class
  private
    FProcessInfo: TProcessInformation;
    FStdInWrite: THandle;
    FStdOutRead: THandle;
    procedure StartProcess(const ExeName: string);
    procedure StopProcess;
  public
    constructor Create(const ExeName: string);
    destructor Destroy; override;
    procedure SendCommand(const Cmd: string);
    function ReadOutput: string;
  end;

implementation

{ TUciEngine }

constructor TUciEngine.Create(const ExeName: string);
begin
  StartProcess(ExeName);
end;

destructor TUciEngine.Destroy;
begin
  StopProcess;
  inherited;
end;

procedure TUciEngine.StartProcess(const ExeName: string);
var
  SI: TStartupInfo;
  SecAttr: TSecurityAttributes;
  StdInRead, StdOutWrite: THandle;
begin
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.bInheritHandle := True;
  SecAttr.lpSecurityDescriptor := nil;

  // Pipes
  CreatePipe(FStdOutRead, StdOutWrite, @SecAttr, 0);
  SetHandleInformation(FStdOutRead, HANDLE_FLAG_INHERIT, 0);

  CreatePipe(StdInRead, FStdInWrite, @SecAttr, 0);
  SetHandleInformation(FStdInWrite, HANDLE_FLAG_INHERIT, 0);

  ZeroMemory(@SI, SizeOf(SI));
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  SI.hStdInput := StdInRead;
  SI.hStdOutput := StdOutWrite;
  SI.hStdError := StdOutWrite;

  if not CreateProcess(nil, PChar(ExeName), nil, nil, True,
    CREATE_NO_WINDOW, nil, nil, SI, FProcessInfo) then
    raise Exception.Create('Nie uda³o siê uruchomiæ silnika: ' + ExeName);
end;

procedure TUciEngine.StopProcess;
begin
  if FProcessInfo.hProcess <> 0 then
  begin
    TerminateProcess(FProcessInfo.hProcess, 0);
    CloseHandle(FProcessInfo.hProcess);
    CloseHandle(FProcessInfo.hThread);
  end;
  if FStdInWrite <> 0 then CloseHandle(FStdInWrite);
  if FStdOutRead <> 0 then CloseHandle(FStdOutRead);
end;

procedure TUciEngine.SendCommand(const Cmd: string);
var
  BytesWritten: DWORD;
  S: AnsiString;
begin
  S := AnsiString(Cmd + #13#10);
  WriteFile(FStdInWrite, PAnsiChar(S)^, Length(S), BytesWritten, nil);
end;

function TUciEngine.ReadOutput: string;
var
  Buffer: array[0..1023] of AnsiChar;
  BytesRead: DWORD;
begin
  Result := '';
  while True do
  begin
    if not PeekNamedPipe(FStdOutRead, nil, 0, nil, @BytesRead, nil) then Break;
    if BytesRead = 0 then Break;
    if ReadFile(FStdOutRead, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
    begin
      Buffer[BytesRead] := #0;
      Result := Result + string(Buffer);
    end
    else
      Break;
  end;
end;

end.
