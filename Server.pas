unit Server;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext,
  System.Generics.Collections, SyncObjs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, Vcl.Grids, System.Math, EncdDecd, System.Threading;

type
  /// Informacje o graczu oczekującym na parę
  TPlayerInfo = record
    Context: TIdContext;
    Login: string;
    ID: Integer;
  end;

  /// Para graczy: biały i czarny
  TPlayerPair = record
    WhitePlayer, BlackPlayer: TIdContext;
    WhiteLogin, BlackLogin: string;
    WhiteID, BlackID: Integer;
    WhiteSeconds: Integer;
    BlackSeconds: Integer;
    IncrementSeconds: Integer;
    WhiteRanking, BlackRanking: Integer;
    GameTypeId: Byte;
    Result: String;
  end;

  TForm10 = class(TForm)
    IdTCPServer1: TIdTCPServer;
    MemoLog: TMemo;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    sgPlayers: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Connect(AContext: TIdContext);
  private
    WaitingRapid, WaitingBlitz, WaitingBullet: TList<TPlayerInfo>;
    ActivePairs: TList<TPlayerPair>;
    ListLock: TCriticalSection;
    ActivePlayers: TDictionary<string, boolean>;
    ContextToLogin: TDictionary<TIdContext,string>;

    procedure TryPairRapid;
    procedure TryPairBlitz;
    procedure TryPairBullet;
    function FindPairWithPlayer(Player: TIdContext): TPlayerPair;
    function IsPlayerPaired(Player: TIdContext): Boolean;
    function FindPairIndex(Player: TIdContext): Integer;
    procedure BroadcastToOpponent(SenderClient: TIdContext; const Msg: string);
    procedure BroadcastPromotion(SenderClient: TIdContext; const Msg: string);
    procedure Log(const Msg: string);
    procedure BroadcastEndgame(SenderClient: TIdContext; const Msg: string);
    procedure BroadcastChat(SenderClient: TIdContext; const Text: string);
    procedure BroadcastSan(SenderClient: TIdContext; const Msg: string);
    procedure RefreshPlayerGrid;
    procedure OnPlayerLeft(AContext: TidContext);
    procedure IdTCPServer1Disconnect(AContexT: TIdContext);
    procedure QueueLeft(AContext: TIdContext);
    procedure BroadcastDrawOffer(SenderClient: TIdContext);
    procedure BroadcastCustom(SenderClient: TIdContext; const Msg: string);
    procedure SendSQL(SenderClient: TIdContext; const SQL: string; OpString: string);
    procedure HandleRanking(SenderClient: TIdContext; const Ranking: string);
    procedure UpdateRanking(SenderClient: TIdContext; const Msg: string);
    procedure SaveRankingToDB(const Pair: TPlayerPair);
    function FindContextByLogin(const Login: string): TIdContext;
    function GetContextUserID(AContext: TIdContext): Integer;
    procedure StartManualGame(InviterCtx, AccepterCtx: TIdContext;
    GameTypeId, InviterID, AccepterID: Integer);
    procedure RemovePair(AContext: TIdContext);



  public

  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}


const
  RapidTime       = 600; // 10 min
  BlitzTime       = 180; // 3 min
  BulletTime      = 60;  // 1 min
  RapidIncrement  = 0;
  BlitzIncrement  = 2;
  BulletIncrement = 1;


procedure TForm10.Log(const Msg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoLog.Lines.Add(FormatDatetime('hh:nn:ss.zzz', Now) + ' - ' + Msg);
    end
  );
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
   FDPhysMySQLDriverLink1.VendorLib := ExtractFilePath(Application.ExeName) + 'libmysql.dll';
  Randomize;

  Application.title:='Server';

  sgPlayers.ColCount   := 2;
  sgPlayers.RowCount   := 2;
  sgPlayers.FixedCols  := 0;
  sgPlayers.FixedRows  := 1;
  sgPlayers.Options    := sgPlayers.Options + [goRowSelect, goColSizing];
  sgPlayers.Cells[0,0] := 'Login';
  sgPlayers.Cells[1,0] := 'InGame';
  sgPlayers.ColWidths[0] := 150;
  sgPlayers.ColWidths[1] := 60;

  WaitingRapid  := TList<TPlayerInfo>.Create;
  WaitingBlitz  := TList<TPlayerInfo>.Create;
  WaitingBullet := TList<TPlayerInfo>.Create;
  ActivePairs    := TList<TPlayerPair>.Create;
  ListLock       := TCriticalSection.Create;
  ActivePlayers:= TDictionary<string, boolean>.Create();
  ContextToLogin:= TDictionary<TidContext, string>.Create();

  MemoLog.Clear;
  Log('Server startuje...');

  IdTCPServer1.DefaultPort := 5000;
  IdTCPServer1.OnConnect    := IdTCPServer1Connect;
  IdTCPServer1.OnDisconnect := IdTCPServer1Disconnect;
  IdTCPServer1.Active      := True;

  Log('Serwer nasłuchuje na porcie 5000');
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
  IdTCPServer1.Active := False;
  ListLock.Free;
  WaitingRapid.Free;
  WaitingBlitz.Free;
  WaitingBullet.Free;
  ActivePairs.Free;
  ActivePlayers.Free;
  ContextToLogin.Free;
  Log('Serwer zatrzymany');
end;

procedure TForm10.IdTCPServer1Connect(AContext: TIdContext);
begin
  Log('Nowe połączenie z ' + AContext.Binding.PeerIP + ':' + AContext.Binding.PeerPort.ToString);
end;



procedure TForm10.IdTCPServer1Disconnect(AContext: TIdContext);
var
Login: string;

begin
  Log('Rozłączono ' + AContext.Binding.PeerIP + ':' + AContext.Binding.PeerPort.ToString);

  if ContextToLogin.TryGetValue(AContext, Login) then
  begin
    Log('Gracz rozłączony: ' + Login);

    ContextToLogin.Remove(AContext);
    ActivePlayers.Remove(Login);
  end

  else
  begin
    Log('Nieznany kontekst rozłączony (brak loginu)');
  end;


  RefreshPlayerGrid;

end;



procedure TForm10.OnPlayerLeft(AContext: TIdContext);
 var
  Pair: TPlayerPair;
  Opponent: TIdContext;
  i: Integer;
begin
  ListLock.Acquire;

  try

    for i := ActivePairs.Count - 1 downto 0 do
    begin
      Pair := ActivePairs[i];

      if (Pair.WhitePlayer = AContext) or (Pair.BlackPlayer = AContext) then
      begin
        if Pair.WhitePlayer = AContext then
        begin
        Opponent := Pair.BlackPlayer;
        if ActivePlayers.ContainsKey(Pair.WhiteLogin) then
        begin
          ActivePlayers[Pair.WhiteLogin]:=false;
          ActivePlayers[Pair.BlackLogin]:=false;
        end;

        end

        else
        begin
        Opponent := Pair.WhitePlayer;

          if ActivePlayers.ContainsKey(Pair.BlackLogin) then
        begin
          ActivePlayers[Pair.BlackLogin]:=false;
          ActivePlayers[Pair.WhiteLogin]:=false;
        end;


        end;

        ActivePairs.Delete(i);

        if (Opponent <> nil) and Opponent.Connection.Connected then
        begin
          Opponent.Connection.IOHandler.WriteLn('OPPONENT_LEFT');
          Log('Wysłano do przeciwnika: OPPONENT_LEFT');
        end;

        Break;
      end;
    end;




  finally
    ListLock.Release;
  end;

  RefreshPlayerGrid;


end;

procedure TForm10.RemovePair(AContext: TIdContext);
var
  i: Integer;
  Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    for i := ActivePairs.Count - 1 downto 0 do
    begin
      Pair := ActivePairs[i];

      if (Pair.WhitePlayer = AContext) or (Pair.BlackPlayer = AContext) then
      begin
        ActivePlayers[Pair.WhiteLogin] := False;
        ActivePlayers[Pair.BlackLogin] := False;
        ActivePairs.Delete(i);
        Break;
      end;
    end;
  finally
    ListLock.Release;
  end;

  RefreshPlayerGrid;

end;




procedure TForm10.QueueLeft(AContext: TIdContext);
  var
  i: Integer;

begin
   ListLock.Acquire;
  try
    for i := WaitingRapid.Count-1 downto 0 do
      if WaitingRapid[i].Context = AContext then
        WaitingRapid.Delete(i);
    for i := WaitingBlitz.Count-1 downto 0 do
      if WaitingBlitz[i].Context = AContext then
        WaitingBlitz.Delete(i);
    for i := WaitingBullet.Count-1 downto 0 do
      if WaitingBullet[i].Context = AContext then
        WaitingBullet.Delete(i);
  finally
    ListLock.Release;
  end;


  RefreshPlayerGrid;

end;



procedure TForm10.SendSQL(SenderClient: TIdContext; const SQL: string; OpString: string);
var
Pair: TPlayerPair;
begin

Pair := FindPairWithPlayer(SenderClient);
FDQuery1.Connection := FDConnection1;
FDQuery1.SQL.Text := SQL;
FDQuery1.ParamByName('result').AsString := Pair.Result;
FDQuery1.ParamByName('blackid').AsInteger := Pair.BlackID;
FDQuery1.ParamByName('whiteid').AsInteger := Pair.WhiteID;
FDQuery1.ParamByName('pgn').AsString := OpString;
FDQuery1.ExecSQL;

end;

procedure TForm10.SaveRankingToDB(const Pair: TPlayerPair);
begin

  var LocalPair := Pair;

  TTask.Run(procedure
  var
    Conn: TFDConnection;
    Q: TFDQuery;
    addWin, addLose, addDraw: Integer;
  begin
    try
      Conn := TFDConnection.Create(nil);
      Q := nil;
      try
        Conn.Params.Assign(FDConnection1.Params);
        Conn.DriverName := FDConnection1.DriverName;
        Conn.LoginPrompt := False;
        try
          Conn.Connected := True;
        except
          on E: Exception do
          begin
            Log('DB connect error in SaveRankingToDBAsync: ' + E.ClassName + ': ' + E.Message);
            Exit;
          end;
        end;

        Q := TFDQuery.Create(nil);
        Q.Connection := Conn;

        // --- BIAŁY ---
        addWin  := Ord(LocalPair.Result = 'WHITE');
        addLose := Ord(LocalPair.Result = 'BLACK');
        addDraw := Ord(LocalPair.Result = 'DRAW');

        Q.SQL.Text :=
          'UPDATE rankings SET ' +
          'rating      = :newRating, ' +
          'gamesplayed = gamesplayed + 1, ' +
          'wins        = wins  + :addWin, ' +
          'loses       = loses + :addLose, ' +
          'draws       = draws + :addDraw ' +
          'WHERE users_userid = :userID AND game_type_id = :gameType';

        Q.ParamByName('newRating').AsInteger := LocalPair.WhiteRanking;
        Q.ParamByName('addWin').AsInteger    := addWin;
        Q.ParamByName('addLose').AsInteger   := addLose;
        Q.ParamByName('addDraw').AsInteger   := addDraw;
        Q.ParamByName('userID').AsInteger    := LocalPair.WhiteID;
        Q.ParamByName('gameType').AsInteger  := LocalPair.GameTypeId;

        try
          Q.ExecSQL;
        except
          on E: Exception do
          begin
            Log('DB exec error (white) SaveRankingToDBAsync: ' + E.ClassName + ': ' + E.Message);
          end;
        end;

        // --- CZARNY ---
        addWin  := Ord(LocalPair.Result = 'BLACK');
        addLose := Ord(LocalPair.Result = 'WHITE');
        addDraw := Ord(LocalPair.Result = 'DRAW');

        Q.ParamByName('newRating').AsInteger := LocalPair.BlackRanking;
        Q.ParamByName('addWin').AsInteger    := addWin;
        Q.ParamByName('addLose').AsInteger   := addLose;
        Q.ParamByName('addDraw').AsInteger   := addDraw;
        Q.ParamByName('userID').AsInteger    := LocalPair.BlackID;
        Q.ParamByName('gameType').AsInteger  := LocalPair.GameTypeId;

        try
          Q.ExecSQL;
        except
          on E: Exception do
            Log('DB exec error (black) SaveRankingToDBAsync: ' + E.ClassName + ': ' + E.Message);
        end;

        Log(Format('Saved ranking async: W:%d B:%d (userids %d/%d)',
          [LocalPair.WhiteRanking, LocalPair.BlackRanking, LocalPair.WhiteID, LocalPair.BlackID]));
      finally
        Q.Free;
        Conn.Free;
      end;
    except
      on E: Exception do
        Log('Unhandled exception in SaveRankingToDBAsync task: ' + E.ClassName + ': ' + E.Message);
    end;
  end);
end;



procedure TForm10.UpdateRanking(SenderClient: TIdContext; const Msg: string);
var
  idx       : Integer;
  Pair      : TPlayerPair;
  K         : Integer;
  Ewhite,
  Eblack,
  Swhite,
  Sblack    : Double;
  NewWhite,
  NewBlack  : Integer;
begin
  idx := FindPairIndex(SenderClient);
  if idx = -1 then Exit;

  ListLock.Acquire;
  try
    Pair := ActivePairs[idx];

    // --- obliczenia Elo ---
    K := 32;

    Ewhite := 1 / (1 + Power(10, (Pair.BlackRanking - Pair.WhiteRanking) / 400));
    Eblack := 1 / (1 + Power(10, (Pair.WhiteRanking - Pair.BlackRanking) / 400));

    if Pair.Result = 'WHITE' then
    begin
      Swhite := 1.0;
      Sblack := 0.0;
    end
    else if Pair.Result = 'BLACK' then
    begin
      Swhite := 0.0;
      Sblack := 1.0;
    end
    else // 'DRAW'
    begin
      Swhite := 0.5;
      Sblack := 0.5;
    end;

    NewWhite := Round(Pair.WhiteRanking + K * (Swhite - Ewhite));
    NewBlack := Round(Pair.BlackRanking + K * (Sblack - Eblack));

    Pair.WhiteRanking := NewWhite;
    Pair.BlackRanking := NewBlack;

    ActivePairs[idx] := Pair;
  finally
    ListLock.Release;
  end;

 SaveRankingToDB(Pair);

end;


function TForm10.FindContextByLogin(const Login: string): TIdContext;
var
  ctx: TIdContext;
begin
  Result := nil;
  for ctx in ContextToLogin.Keys do
    if ContextToLogin[ctx] = Login then
      Exit(ctx);
end;


function TForm10.GetContextUserID(AContext: TIdContext): Integer;
var
  i: Integer;
begin
  Result := 0;
  ListLock.Acquire;
  try
    for i := 0 to WaitingRapid.Count - 1 do
      if WaitingRapid[i].Context = AContext then
        Exit(WaitingRapid[i].ID);
    for i := 0 to WaitingBlitz.Count - 1 do
      if WaitingBlitz[i].Context = AContext then
        Exit(WaitingBlitz[i].ID);
    for i := 0 to WaitingBullet.Count - 1 do
      if WaitingBullet[i].Context = AContext then
        Exit(WaitingBullet[i].ID);
  finally
    ListLock.Release;
  end;
end;


 procedure TForm10.StartManualGame(InviterCtx, AccepterCtx: TIdContext;
  GameTypeId, InviterID, AccepterID: Integer);
var
  Pair: TPlayerPair;
  InviterLogin, AccepterLogin: string;
begin
  if (InviterCtx = nil) or (AccepterCtx = nil) then Exit;

  if not ContextToLogin.TryGetValue(InviterCtx, InviterLogin) then Exit;
  if not ContextToLogin.TryGetValue(AccepterCtx, AccepterLogin) then Exit;

  ListLock.Acquire;
  try
    QueueLeft(InviterCtx);
    QueueLeft(AccepterCtx);

    if Random(2) = 0 then
    begin
      Pair.WhitePlayer := InviterCtx;
      Pair.WhiteLogin  := InviterLogin;
      Pair.WhiteID     := InviterID;

      Pair.BlackPlayer := AccepterCtx;
      Pair.BlackLogin  := AccepterLogin;
      Pair.BlackID     := AccepterID;
    end
    else
    begin
      Pair.WhitePlayer := AccepterCtx;
      Pair.WhiteLogin  := AccepterLogin;
      Pair.WhiteID     := AccepterID;

      Pair.BlackPlayer := InviterCtx;
      Pair.BlackLogin  := InviterLogin;
      Pair.BlackID     := InviterID;
    end;

    case GameTypeId of
      1: begin Pair.WhiteSeconds := RapidTime; Pair.BlackSeconds := RapidTime; Pair.IncrementSeconds := RapidIncrement; end;
      2: begin Pair.WhiteSeconds := BlitzTime; Pair.BlackSeconds := BlitzTime; Pair.IncrementSeconds := BlitzIncrement; end;
      3: begin Pair.WhiteSeconds := BulletTime; Pair.BlackSeconds := BulletTime; Pair.IncrementSeconds := BulletIncrement; end;
    else
      begin Pair.WhiteSeconds := RapidTime; Pair.BlackSeconds := RapidTime; Pair.IncrementSeconds := RapidIncrement; end;
    end;

    Pair.WhiteRanking := 0;
    Pair.BlackRanking := 0;
    Pair.GameTypeId   := GameTypeId;

    ActivePlayers[Pair.WhiteLogin] := True;
    ActivePlayers[Pair.BlackLogin] := True;

    ActivePairs.Add(Pair);
  finally
    ListLock.Release;
  end;

  Pair.WhitePlayer.Connection.IOHandler.WriteLn('START');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('COLOR:WHITE');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.BlackLogin);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('ID:'+Pair.BlackID.ToString);

  Pair.BlackPlayer.Connection.IOHandler.WriteLn('START');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('COLOR:BLACK');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.WhiteLogin);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('ID:'+Pair.WhiteID.ToString);

  Pair.WhitePlayer.Connection.IOHandler.WriteLn('RANKING:');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('RANKING:');

  RefreshPlayerGrid;

end;




procedure TForm10.TryPairRapid;
var
  P1, P2: TPlayerInfo;
  Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    if WaitingRapid.Count < 2 then Exit;

    if P1.ID = P2.ID then
    begin
    WaitingRapid.Add(P2);
    Exit;
     end;

    P1 := WaitingRapid[0];
    P2 := WaitingRapid[1];
    WaitingRapid.Delete(1);
    WaitingRapid.Delete(0);

    if Random(2)=0 then
    begin
      Pair.WhitePlayer := P1.Context; Pair.WhiteLogin := P1.Login; Pair.WhiteID := P1.ID;
      Pair.BlackPlayer := P2.Context; Pair.BlackLogin := P2.Login; Pair.BlackID := P2.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end else
    begin
      Pair.WhitePlayer := P2.Context; Pair.WhiteLogin := P2.Login; Pair.WhiteID := P2.ID;
      Pair.BlackPlayer := P1.Context; Pair.BlackLogin := P1.Login; Pair.BlackID := P1.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end;
    Pair.WhiteSeconds     := RapidTime;
    Pair.BlackSeconds     := RapidTime;
    Pair.IncrementSeconds := RapidIncrement;
    Pair.BlackRanking:=0;
    Pair.WhiteRanking:=0;
    Pair.GameTypeId:=1;

    ActivePairs.Add(Pair);


    RefreshPlayerGrid;



  finally
    ListLock.Release;
  end;

  Log(Format('RAPID: %s vs %s', [Pair.WhiteLogin, Pair.BlackLogin]));
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('START');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('COLOR:WHITE');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.BlackLogin);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('ID:'+Pair.BlackID.ToString);

  Pair.BlackPlayer.Connection.IOHandler.WriteLn('START');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('COLOR:BLACK');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.WhiteLogin);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('ID:'+Pair.WhiteID.ToString);

  Pair.WhitePlayer.Connection.IOHandler.WriteLn('RANKING:');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('RANKING:');

end;







procedure TForm10.TryPairBlitz;
var P1,P2: TPlayerInfo; Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    if WaitingBlitz.Count < 2 then Exit;


   if P1.ID = P2.ID then
   begin
    WaitingBlitz.Add(P2);
    Exit;
   end;


    P1 := WaitingBlitz[0]; P2 := WaitingBlitz[1];
    WaitingBlitz.Delete(1); WaitingBlitz.Delete(0);
    if Random(2)=0 then
    begin
      Pair.WhitePlayer := P1.Context; Pair.WhiteLogin := P1.Login; Pair.WhiteID := P1.ID;
      Pair.BlackPlayer := P2.Context; Pair.BlackLogin := P2.Login; Pair.BlackID := P2.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end else
    begin
      Pair.WhitePlayer := P2.Context; Pair.WhiteLogin := P2.Login; Pair.WhiteID := P2.ID;
      Pair.BlackPlayer := P1.Context; Pair.BlackLogin := P1.Login; Pair.BlackID := P1.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end;

    Pair.WhiteSeconds     := BlitzTime;
    Pair.BlackSeconds     := BlitzTime;
    Pair.IncrementSeconds := BlitzIncrement;

    RefreshPlayerGrid;
    Pair.BlackRanking:=0;
    Pair.WhiteRanking:=0;
    Pair.GameTypeId:=2;


    ActivePairs.Add(Pair);





  finally
    ListLock.Release;
  end;
  Log(Format('BLITZ: %s vs %s', [Pair.WhiteLogin, Pair.BlackLogin]));
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('START');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('COLOR:WHITE');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.BlackLogin);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('ID:'+Pair.BlackID.ToString);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('START');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('COLOR:BLACK');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.WhiteLogin);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('ID:'+Pair.WhiteID.ToString);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('RANKING:');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('RANKING:');
end;



procedure TForm10.TryPairBullet;
var P1,P2: TPlayerInfo; Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    if WaitingBullet.Count < 2 then Exit;

    if P1.ID = P2.ID then
    begin
    WaitingBullet.Add(P2);
    Exit;
   end;


    P1 := WaitingBullet[0]; P2 := WaitingBullet[1];
    WaitingBullet.Delete(1); WaitingBullet.Delete(0);
    if Random(2)=0 then
    begin
      Pair.WhitePlayer := P1.Context; Pair.WhiteLogin := P1.Login; Pair.WhiteID := P1.ID;
      Pair.BlackPlayer := P2.Context; Pair.BlackLogin := P2.Login; Pair.BlackID := P2.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end else
    begin
      Pair.WhitePlayer := P2.Context; Pair.WhiteLogin := P2.Login; Pair.WhiteID := P2.ID;
      Pair.BlackPlayer := P1.Context; Pair.BlackLogin := P1.Login; Pair.BlackID := P1.ID;
      ActivePlayers[P1.Login] := True;
      ActivePlayers[P2.Login] := True;
    end;

    RefreshPlayerGrid;

    Pair.WhiteSeconds     := BulletTime;
    Pair.BlackSeconds     := BulletTime;
    Pair.IncrementSeconds := BulletIncrement;
    Pair.BlackRanking:=0;
    Pair.WhiteRanking:=0;
    Pair.GameTypeId:=3;

    ActivePairs.Add(Pair);





  finally
    ListLock.Release;
  end;
  Log(Format('BULLET: %s vs %s', [Pair.WhiteLogin, Pair.BlackLogin]));
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('START');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('COLOR:WHITE');
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.BlackLogin);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('ID:'+Pair.BlackID.ToString);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('START');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('COLOR:BLACK');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('OPPONENT:'+Pair.WhiteLogin);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('ID:'+Pair.WhiteID.ToString);
  Pair.WhitePlayer.Connection.IOHandler.WriteLn('RANKING:');
  Pair.BlackPlayer.Connection.IOHandler.WriteLn('RANKING:');
end;

/// Sprawdza, czy dany kontekst jest w jakiejś aktywnej parze
function TForm10.IsPlayerPaired(Player: TIdContext): Boolean;
var
  Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    for Pair in ActivePairs do
      if (Pair.WhitePlayer = Player) or (Pair.BlackPlayer = Player) then
        Exit(True);
  finally
    ListLock.Release;
  end;
  Result := False;
end;

/// Znajduje strukturę pary zawierającą danego gracza
function TForm10.FindPairWithPlayer(Player: TIdContext): TPlayerPair;
var
  Pair: TPlayerPair;
begin
  ListLock.Acquire;
  try
    for Pair in ActivePairs do
      if (Pair.WhitePlayer = Player) or (Pair.BlackPlayer = Player) then
        Exit(Pair);
  finally
    ListLock.Release;
  end;
  Result.WhitePlayer := nil;
  Result.BlackPlayer := nil;
end;


function TForm10.FindPairIndex(Player: TIdContext): Integer;
begin
  ListLock.Acquire;
  try
    for Result := 0 to ActivePairs.Count - 1 do
      if (ActivePairs[Result].WhitePlayer = Player) or
         (ActivePairs[Result].BlackPlayer = Player) then
        Exit;
    Result := -1;
  finally
    ListLock.Release;
  end;
end;

procedure TForm10.BroadcastToOpponent(SenderClient: TIdContext; const Msg: string);
var
  Pair: TPlayerPair;
  Forwarded: string;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if Pair.WhitePlayer = nil then
    Exit;

  Forwarded := 'OPPONENT_MOVE:' + Msg;
  Log('Forwarduję ruch: ' + Forwarded);

  if SenderClient = Pair.WhitePlayer then
    Pair.BlackPlayer.Connection.IOHandler.WriteLn(Forwarded)
  else
    Pair.WhitePlayer.Connection.IOHandler.WriteLn(Forwarded);
end;



procedure TForm10.BroadcastPromotion(SenderClient: TIdContext; const Msg: string);

var
  Pair: TPlayerPair;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if Pair.WhitePlayer = nil then Exit;

  if SenderClient = Pair.WhitePlayer then
    Pair.BlackPlayer.Connection.IOHandler.WriteLn('OPPONENT_PROMO:' + Msg)
  else
    Pair.WhitePlayer.Connection.IOHandler.WriteLn('OPPONENT_PROMO:' + Msg);

  Log('Forwarduję promocję: ' + Msg);

end;


procedure TForm10.BroadcastSan(SenderClient: TIdContext; const Msg: string);

var
  Pair: TPlayerPair;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if Pair.WhitePlayer = nil then Exit;


  if SenderClient = Pair.WhitePlayer then
    Pair.BlackPlayer.Connection.IOHandler.WriteLn('SAN:' + Msg)
  else
    Pair.WhitePlayer.Connection.IOHandler.WriteLn('SAN:' + Msg);

  Log('Forwarduję SAN: ' + Msg);

end;






procedure TForm10.BroadcastChat(SenderClient: TIdContext; const Text: string);
var
  Pair: TPlayerPair;
  SenderName, OutMsg: string;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if Pair.WhitePlayer = nil then Exit;

  if SenderClient = Pair.WhitePlayer then
    SenderName := Pair.WhiteLogin
  else
    SenderName := Pair.BlackLogin;

  OutMsg := 'CHAT:' + SenderName + ': ' + Text;

  Pair.WhitePlayer.Connection.IOHandler.WriteLn(OutMsg);
  Pair.BlackPlayer.Connection.IOHandler.WriteLn(OutMsg);
end;


procedure TForm10.BroadcastDrawOffer(SenderClient: TIdContext);
begin
var
  Pair: TPlayerPair;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if (Pair.WhitePlayer = nil) or (Pair.BlackPlayer = nil) then Exit;

  if SenderClient = Pair.WhitePlayer then
    Pair.BlackPlayer.Connection.IOHandler.WriteLn('DRAW:OFFER')
  else
    Pair.WhitePlayer.Connection.IOHandler.WriteLn('DRAW:OFFER');

  end;



end;




procedure TForm10.BroadcastCustom(SenderClient: TidContext; const Msg: string);
begin
var
  Pair: TPlayerPair;
begin
  Pair := FindPairWithPlayer(SenderClient);
  if (Pair.WhitePlayer = nil) or (Pair.BlackPlayer = nil) then Exit;

  if SenderClient = Pair.WhitePlayer then
    Pair.BlackPlayer.Connection.IOHandler.WriteLn(Msg)
  else
    Pair.WhitePlayer.Connection.IOHandler.WriteLn(Msg);

  end;



end;



procedure TForm10.BroadcastEndgame(SenderClient: TIdContext; const Msg: string);
var
  idx : Integer;
  Pair: TPlayerPair;
begin
  idx := FindPairIndex(SenderClient);
  if idx = -1 then Exit;

  ListLock.Acquire;
  try
    Pair := ActivePairs[idx];

    if Msg = 'WIN' then
    begin
      if SenderClient = Pair.WhitePlayer then
      begin
        Pair.BlackPlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
        Pair.BlackPlayer.Connection.IOHandler.WriteLn('PGN:');
        Pair.Result := 'BLACK';
      end
      else
      begin
        Pair.WhitePlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
        Pair.WhitePlayer.Connection.IOHandler.WriteLn('PGN:');
        Pair.Result := 'WHITE';
      end;
    end
    else if Msg = 'LOSE' then
    begin
      if SenderClient = Pair.WhitePlayer then
      begin
        Pair.WhitePlayer.Connection.IOHandler.WriteLn('PGN:');
        Pair.BlackPlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
        Pair.Result := 'WHITE';
      end
      else
      begin
        Pair.BlackPlayer.Connection.IOHandler.WriteLn('PGN:');
        Pair.WhitePlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
        Pair.Result := 'BLACK';
      end;
    end
    else if Msg = 'DRAW' then
    begin
      Pair.BlackPlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
      Pair.WhitePlayer.Connection.IOHandler.WriteLn('ENDGAME:' + Msg);
      Pair.WhitePlayer.Connection.IOHandler.WriteLn('PGN:');
      Pair.Result := 'DRAW';
    end;

    ActivePlayers[Pair.BlackLogin] := False;
    ActivePlayers[Pair.WhiteLogin] := False;

    ActivePairs[idx] := Pair;
  finally
    ListLock.Release;
  end;

  RefreshPlayerGrid;
end;




procedure TForm10.HandleRanking(SenderClient: TIdContext; const Ranking: String);
var
  idx: Integer;
  Pair: TPlayerPair;
begin
  idx := FindPairIndex(SenderClient);
  if idx = -1 then Exit;

  ListLock.Acquire;
  try
    Pair := ActivePairs[idx];
    if SenderClient = Pair.WhitePlayer then
      Pair.BlackRanking := StrToInt(Ranking)
    else
      Pair.WhiteRanking := StrToInt(Ranking);
    ActivePairs[idx] := Pair;
  finally
    ListLock.Release;
  end;
end;


procedure TForm10.IdTCPServer1Execute(AContext: TIdContext);
var
  Msg: string;
  Info: TPlayerInfo;
  mode: string;
  temp: string;
  PairRec: TPlayerPair;
  IsWhite: Boolean;
  Elapsed: Integer;
  tcp: string;
begin

  Msg := AContext.Connection.IOHandler.ReadLn;
  Log('Odebrano od ' + AContext.Binding.PeerIP + ': ' + Msg);



    if Msg.StartsWith('TIME:') then
  begin
    var i: Integer;

    Elapsed := StrToIntDef(Msg.Substring(5), 0);

    for i := 0 to ActivePairs.Count-1 do
      with ActivePairs[i] do
        if (WhitePlayer = AContext) or (BlackPlayer = AContext) then
        begin
          PairRec := ActivePairs[i];
          IsWhite := (AContext = WhitePlayer);

          if IsWhite then
            PairRec.WhiteSeconds := PairRec.WhiteSeconds - Elapsed+PairRec.IncrementSeconds
          else
            PairRec.BlackSeconds := PairRec.BlackSeconds - Elapsed+PairRec.IncrementSeconds;

          ActivePairs[i] := PairRec;
          Break;
        end;

    tcp := Format('TIME_UPDATE:WHITE=%d|BLACK=%d',
                  [PairRec.WhiteSeconds, PairRec.BlackSeconds]);
    PairRec.WhitePlayer.Connection.IOHandler.WriteLn(tcp);
    PairRec.BlackPlayer.Connection.IOHandler.WriteLn(tcp);

    Exit;
  end;


  if Msg.StartsWith('CHAT:') then
  begin
    BroadcastChat(AContext, Msg.Substring(5).Trim);
    Exit;
  end;




  if Msg.StartsWith('GET_PLAYERS') then
begin
  var Parts := Msg.Split([':']);
  var Offset := 0;
  var Limit  := 10;
  var Filter := '';
  var Flag:='';


  if Length(Parts) >= 3 then
  begin
    Offset := StrToIntDef(Parts[1], 0);
    Limit  := StrToIntDef(Parts[2], 10);
  end;
  if Length(Parts) >= 4 then
    Filter := LowerCase(Parts[3]);

  var AllLogins := ActivePlayers.Keys.ToArray;
  var CountSent := 0;
  var Index     := 0;

  TArray.Sort<string>(AllLogins);

  for var Login in AllLogins do
  begin

    if (Filter <> '') and (Pos(Filter, LowerCase(Login)) = 0) then
      Continue;

    if Index < Offset then
    begin
      Inc(Index);
      Continue;
    end;

    if CountSent >= Limit then
      Break;

    if ActivePlayers[Login] then
    begin
    Flag:='1';
    end
    else
    begin
    Flag:='0';
    end;

    AContext.Connection.IOHandler.WriteLn('PLAYER:' + Login + ':' + Flag);

    Inc(CountSent);
    Inc(Index);
  end;

  AContext.Connection.IOHandler.WriteLn('');
  Exit;
end;


 if Msg.StartsWith('INVITE:') then
begin
  var Parts := Msg.Split([':']);
  if Length(Parts) >= 5 then
  begin
    var TargetLogin := Parts[1];
    var Mode2       := StrToIntDef(Parts[2], 1);
    var SourceLogin := Parts[3];
    var SourceID    := StrToIntDef(Parts[4], 0);

    ListLock.Acquire;
    try
      var TargetCtx := FindContextByLogin(TargetLogin);
      var IsIdle: Boolean;
      if (TargetCtx <> nil) and ActivePlayers.TryGetValue(TargetLogin, IsIdle) and (IsIdle = False) then
      begin
        QueueLeft(AContext);
        QueueLeft(TargetCtx);

        TargetCtx.Connection.IOHandler.WriteLn(
          'INVITED_BY:' + SourceLogin + ':' + IntToStr(Mode2) + ':' + IntToStr(SourceID)
        );

      end
      else
      begin

      end;
    finally
      ListLock.Release;
    end;

    RefreshPlayerGrid;
  end;
  Exit;
end;


if Msg.StartsWith('INVITE_ACCEPT:') then
begin
  var Parts := Msg.Split([':']);
  if Length(Parts) >= 6 then
  begin
    var InviterLogin := Parts[1];
    var InviterID    := StrToIntDef(Parts[2], 0);
    var Mode2        := StrToIntDef(Parts[3], 1);
    var AccepterLogin:= Parts[4];
    var AccepterID   := StrToIntDef(Parts[5], 0);

    var InviterCtx := FindContextByLogin(InviterLogin);
    if Assigned(InviterCtx) then
    begin

      InviterCtx.Connection.IOHandler.WriteLn(
        'INVITE_ACCEPTED:' + AccepterLogin + ':' + IntToStr(Mode2)
      );
      AContext.Connection.IOHandler.WriteLn('INVITE_ACCEPTED:' + InviterLogin + ':' + IntToStr(Mode2));
      StartManualGame(InviterCtx, AContext, Mode2, InviterID, AccepterID);
    end
    else
    begin
      AContext.Connection.IOHandler.WriteLn('INVITE_FAIL:' + InviterLogin);
    end;
  end;
  Exit;
end;


if Msg.StartsWith('INVITE_DECLINE:') then
begin
  var Parts := Msg.Split([':']);
  if Length(Parts) >= 2 then
  begin
    var SourceLogin := Parts[1];
    var SourceCtx := FindContextByLogin(SourceLogin);
    if Assigned(SourceCtx) then
      SourceCtx.Connection.IOHandler.WriteLn(
        'INVITE_DECLINED:' + ContextToLogin[AContext]);

    ListLock.Acquire;
    try
      var declLogin := '';
      if ContextToLogin.TryGetValue(AContext, declLogin) then
        ActivePlayers[declLogin] := False;
      if Assigned(SourceCtx) then
      begin
        var srcLogin := '';
        if ContextToLogin.TryGetValue(SourceCtx, srcLogin) then
          ActivePlayers[srcLogin] := False;
      end;
    finally
      ListLock.Release;
    end;

    RefreshPlayerGrid;
  end;
  Exit;
end;

  if Msg.StartsWith('ADDPLAYER:') then
  begin
  var user := Msg.Substring(10);
  if not ActivePlayers.ContainsKey(user) then
  ActivePlayers.Add(user, False);
  ContextToLogin.Add(Acontext,user);
  RefreshPlayerGrid;
  Exit;

  end;

  if Msg.StartsWith('QUEUELEFT:') then
  begin
  ActivePlayers[(Msg.Substring(10))]:=false;
  QueueLeft(AContext);
  Exit;
  end;

  if Msg.StartsWith('REMOVEPLAYER:') then
  begin
  ActivePlayers.Remove(Msg.Substring(13));

  RefreshPlayerGrid;

  Exit;
  end;

  if Msg.StartsWith('ISLOGGED:') then
  begin
  if ActivePlayers.ContainsKey(Msg.Substring(9)) then
    AContext.Connection.IOHandler.WriteLn('YES')
  else
    AContext.Connection.IOHandler.WriteLn('NO');



  Exit;
   end;



  if Msg.StartsWith('LEFT') then
  begin
  OnPlayerLeft(AContext);
  Exit;
  end;

  if Msg.StartsWith('LOGIN:') then
  begin
    Info.Context := AContext;
    Info.Login   := Msg.Substring(6);

    Msg := AContext.Connection.IOHandler.ReadLn;
    Log('Odebrano: ' + Msg);
    if not Msg.StartsWith('ID:') then
    begin
      Log('Błędny protokół – oczekiwano ID:, dostałem: ' + Msg);
      Exit;
    end;
    Info.ID := StrToIntDef(Msg.Substring(3), 0);

    Msg := AContext.Connection.IOHandler.ReadLn;
    Log('Odebrano: ' + Msg);
    if not Msg.StartsWith('MODE:') then
    begin
      Log('Błędny protokół – oczekiwano MODE:, dostałem: ' + Msg);
      Exit;
    end;
    mode := UpperCase(Msg.Substring(5));

    ListLock.Acquire;
    try
      if mode = '1' then
      begin

       var found := False;
      for var i := 0 to WaitingRapid.Count - 1 do
        if WaitingRapid[i].ID = Info.ID then
        begin
          found := True;
          Break;
        end;
      if not found then
        WaitingRapid.Add(Info);


    end

      else if mode = '2' then
      begin

      var found := False;
      for var i := 0 to WaitingBlitz.Count - 1 do
        if WaitingBlitz[i].ID = Info.ID then
        begin
          found := True;
          Break;
        end;
      if not found then
        WaitingBlitz.Add(Info);


    end

    else if mode = '3' then
     begin
     var found := False;
      for var i := 0 to WaitingBullet.Count - 1 do
        if WaitingBullet[i].ID = Info.ID then
        begin
          found := True;
          Break;
        end;
      if not found then
        WaitingBullet.Add(Info);


    end



    else

        Log('Nieznany tryb gry: ' + mode);
    finally
      ListLock.Release;
    end;

    if mode = '1' then
      TryPairRapid
    else if mode = '2' then
      TryPairBlitz
    else if mode = '3' then
      TryPairBullet;

    Exit;
  end;

  if not IsPlayerPaired(AContext) then
  Exit;
  Msg := Msg.Trim;
  if Msg.StartsWith('PROMO:') then
    BroadcastPromotion(AContext, Msg.Substring(6))

  else if Msg.StartsWith('RANKING:') then
  begin
  var temp2:= Msg.Substring(8);
  HandleRanking(Acontext, temp2);
  end

  else if Msg.StartsWith('ENDGAME:') then

  begin
  temp:= Msg.Substring(8);


  if temp='WIN' then

  begin
  BroadcastEndgame(AContext, 'LOSE');
  EXIT
  end

  else if temp='LOSE' then

  begin
   BroadcastEndgame(AContext, 'WIN');
   EXIT
  end

  else if temp='DRAW' then
  begin
   BroadcastEndgame(AContext, 'DRAW');
   EXIT
  end;


  end


  else if Msg.StartsWith('SAN:') then
  begin
  BroadcastSAN(AContext, Msg.Substring(4).Trim);
  Exit;
  end

  else if Msg.StartsWith('DRAW:OFFER') then
  begin
  BroadcastDrawOffer(AContext);
  EXIT;
  end


  else if Msg.StartsWith('DRAW:ACCEPT') then
  begin
  BroadcastCustom(AContext,'DRAW:ACCEPT');
  BroadcastEndgame(Acontext,'DRAW');
  end

  else if Msg.StartsWith('DRAW:DECLINE') then
  begin
  BroadcastCustom(AContext,'DRAW:DECLINE');
  end

  else if Msg.StartsWith('PGN:') then
  begin
  temp:=Msg.Substring(4);
  temp:=StringReplace(Msg.Substring(4), '\n', sLineBreak, [rfReplaceAll]);
  SendSQL(AContext,   'INSERT INTO games (result, blackplayerid, whiteplayerid, date, pgn) '+
  'VALUES (:result, :blackid, :whiteid, NOW(), :pgn)', temp);
  UpdateRanking(AContext,'');
  RemovePair(AContext);
  end

  else
    BroadcastToOpponent(AContext, Msg);

end;




procedure TForm10.RefreshPlayerGrid;
var
  row: Integer;
  keys: TArray<string>;
begin
  if not TThread.CheckTerminated then
    TThread.Queue(nil,
      procedure
      begin
        ListLock.Acquire;
        try

          keys := ActivePlayers.Keys.ToArray;
          sgPlayers.RowCount := Length(keys) + 1;
          for var i := 0 to High(keys) do
          begin
            row := i + 1;
            sgPlayers.Cells[0, row] := keys[i];

            if ActivePlayers[keys[i]] then
            begin
            sgPlayers.Cells[1,row]:='YES';
            end
            else
            begin
            sgPlayers.Cells[1,row]:='NO';
            end;

          end;
        finally
          ListLock.Release;
        end;
      end
    );
end;




end.

