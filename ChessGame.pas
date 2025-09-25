unit ChessGame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, Vcl.StdCtrls, System.Types,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, System.Generics.Collections, System.Math, DateUtils, UCIEngine, EncdDecd;

// Typy figur
type
  TPiece = (
    ptNone,
    ptWPawn, ptWRook, ptWKnight, ptWBishop, ptWQueen, ptWKing,
    ptBPawn, ptBRook, ptBKnight, ptBBishop, ptBQueen, ptBKing
  );

  TChess = class(TForm)
    Panel: TPanel;
    Timer1: TTimer;
    YourLogin: TLabel;
    YourRanking: TLabel;
    OppLogin: TLabel;
    OppRanking: TLabel;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    tmrWhite: TTimer;
    tmrBlack: TTimer;
    lblWhiteTime: TLabel;
    lblBlackTime: TLabel;

    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tmrWhiteTimer(Sender: TObject);
    procedure tmrBlackTimer(Sender: TObject);
    procedure PanelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);



    private
    OriginalPos: array[0..7,0..7] of TPoint;
    LoginName: string;
    BoardPanels: array[0..7,0..7] of TPanel;
    BoardState: array[0..7,0..7] of TPiece;
    promotionSelection: Integer;
    SelectedSrc: TPoint;
    MyColor: string;
    OpponentName: string;
    OppId: Integer;
    SelectedPanel: TPanel;
    LastSrc, LastDst: TPoint;
    pnlChat: TPanel;
    btnSavePGN: TButton;
    btnDraw: TButton;
    btnResign: TButton;
    memChat: TMemo;
    edtChat: TEdit;
    btnSend: TButton;
    PromotionFlag: Boolean;
    promotionChar: string;
    HasWhiteKingMoved: Boolean;
    HasWhiteRookA1Moved: Boolean;
    HasWhiteRookH1Moved: Boolean;
    HasBlackKingMoved: Boolean;
    HasBlackRookA8Moved: Boolean;
    HasBlackRookH8Moved: Boolean;
    Lost: Boolean;
    Win: Boolean;
    Draw: Boolean;
    GameType: Integer;
    lstMoves: TListBox;
    MovesList: TStringList;
    InGame: Boolean;
    Uci: TUciEngine;
    IsMyTurn: Boolean;
    FMoveHistory: string;
    AILevel: Integer;


    WhiteSeconds: Integer;
    BlackSeconds: Integer;
    IncrementSeconds: Integer;

    CurrentTurn: string;

    TurnStartTime: TDateTime;



    procedure CreateBoard;
    procedure InitializeState;
    procedure SendMove(const Move: string);
    procedure ReceiveMessages;
    procedure RotateBoardForBlack;
    procedure SetupAfterColor;
    procedure ApplyMove(srcRow, srcCol, dstRow, dstCol: Integer; promotionPiece: TPiece);
    procedure UpdateBoardColors;
    procedure PromoButtonClick(Sender: TObject);
    procedure CheckEndGame;
    procedure UpdateClockLabels;
    procedure UpdateTimers;
    procedure DisableBoard;
    procedure UpdateOpponentLabelsPosition;
    procedure UpdateYourLabelsPosition;
    procedure SetupCoordinatesLeftAndBottom;
    procedure UpdateClockLabelsPosition;
    procedure CreateChatControls;
    procedure btnSendClick(Sender: TObject);
    procedure edtChatKeyPress(Sender: TObject; var Key: Char);
    procedure CreateMovesList;
    procedure CreateButtons;
    procedure AddMoveToList(const Move: string);
    procedure btnSavePGNClick(Sender: TObject);
    procedure btnResignClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);



  /////



    function IsPathClear(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
    function IsValidMove(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
    function FindRankingSQL(userid: Integer; gtype: Integer): String;
    function PieceToChar(P: TPiece): string;
    function OwnsPiece(P: TPiece): Boolean;
    function PromotePawnDialog(): string;
    function IsSquareAttacked(r, c: Integer; const byColor: string): Boolean;
    function OpponentColor: string;
    function IsInCheck(color: string): Boolean;
    function IsLegalMove(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
    function HasAnyLegalMove(color: string): Boolean;
    function OppositeColor(const color: string): string;
    function MoveToSAN(Piece: TPiece; const Src, Dst: TPoint; Capture: Boolean; Promotion: TPiece ): string;
    function CanReach(const From, To2: TPoint; Piece: TPiece): Boolean;
    function CodeToPiece(const promoCode: string): TPiece;
    function SanHelper(Promotion: TPiece): string;
    function UciToCoords(const Move: string; out sr, sc, tr, tc: Integer): Boolean;
    function ExtractBestMove(const Output: string): string;
    function InsufficientMaterial: Boolean;
    function MoveListToPGN: string;


  public


  procedure SetGameType(gType: Integer);
  procedure SetAILevel(AI: Integer);
  end;

var
  Chess: TChess;

implementation

{$R *.dfm}
uses
  UserSession;

const
  // Offsets dla skoczka
  KnightOffsets: array[0..7] of TPoint = (
    (X:  1; Y:  2), (X:  2; Y:  1), (X:  2; Y: -1), (X:  1; Y: -2),
    (X: -1; Y: -2), (X: -2; Y: -1), (X: -2; Y:  1), (X: -1; Y:  2)
  );
  // Kierunki dla wieży/hetmana
  RookDirs: array[0..3] of TPoint = (
    (X:  1; Y:  0), (X: -1; Y:  0),
    (X:  0; Y:  1), (X:  0; Y: -1)
  );
  // Kierunki dla gońca/hetmana
  BishopDirs: array[0..3] of TPoint = (
    (X:  1; Y:  1), (X:  1; Y: -1),
    (X: -1; Y:  1), (X: -1; Y: -1)
  );


  const
  RapidTime       = 600; // 10 min
  BlitzTime       = 180; // 3 min
  BulletTime      = 60;  // 1 min
  BlitzIncrement  = 2;   // 2s increment
  RapidIncrement  = 0;
  BulletIncrement = 1;





procedure TChess.SetGameType(gType: Integer);
begin
  GameType:= gType;
end;

procedure TChess.SetAILevel(AI: Integer);
begin
AILevel:=AI;
end;


procedure TChess.FormShow(Sender: TObject);
begin


  if GameType = 4  then
  begin

  UCI := TUciEngine.Create(ExtractFilePath(Application.ExeName) + 'stockfish.exe');
  Uci.SendCommand('uci');
  Uci.SendCommand('isready');
  Uci.SendCommand('setoption name Skill Level value ' + IntToStr(AILevel));
  Uci.SendCommand('ucinewgame');

  Randomize;

  if Random(2) = 0 then
  begin
  MyColor:='WHITE';
  end

  else

  begin
  MyColor:='BLACK';
  ;
  end;



  LoginName := UserSession.LoggedUserLogin;
  OppLogin.Caption:= 'Stockfish';
  Application.Title:='Chess';
  CreateBoard;
  CurrentTurn:= 'WHITE';
  IsMyTurn:=true;

  if Mycolor='BLACK' then

  begin

    RotateBoardForBlack;
  end;


  end





  else
  begin
  LoginName := UserSession.LoggedUserLogin;
  Application.Title:='Chess';

    if not UserSession.IdTCPClient1.Connected then
    begin
    ShowMessage('Błąd połączenia z serwerem.');
    Self.Close;
    Exit;
    end;

    UserSession.IdTCPClient1.IOHandler.WriteLn('LOGIN:' + LoginName);
    UserSession.IdTCPClient1.IOHandler.WriteLn('ID:' + IntToStr(UserSession.LoggedUserID));
    UserSession.IdTCPClient1.IOHandler.WriteLn('MODE:'+IntToStr(GameType));

    // Debug – poczekaj na odpowiedź serwera
    while not UserSession.IdTCPClient1.IOHandler.InputBufferIsEmpty do
    begin
      var msg := Trim(UserSession.IdTCPClient1.IOHandler.ReadLn);
      ShowMessage('Odpowiedź od serwera: ' + msg);
    end;


     CreateBoard;
     UpdateClockLabelsPosition;

     tmrWhite.Interval := 1000;
     tmrWhite.Enabled  := False;
     tmrBlack.Interval := 1000;
     tmrBlack.Enabled  := False;
     CurrentTurn:= 'WHITE';
     UpdateClockLabels;

     Timer1.Enabled := True;
     Timer1.Interval := 200;

  end;




end;


procedure TChess.FormCreate(Sender: TObject);
begin
if not GameType = 4 then
begin
FDPhysMySQLDriverLink1.VendorLib := ExtractFilePath(Application.ExeName) + 'libmysql.dll';
end;

end;

procedure TChess.FormDestroy(Sender: TObject);
begin

  if InGame and not Win then
  begin

  SendMove('LEFT');
  SendMove('ENDGAME:LOSE');
  //FreeMemory;
  end

  else
  begin
  SendMove('QUEUELEFT:'+UserSession.LoggedUserLogin);
 // FreeMemory;
  end;

  if GameType = 4 then
  begin
  Uci.StopProcess;
  end;


end;

procedure TChess.FormResize(Sender: TObject);
const
  ScalePercent = 80;   // plansza zajmie 80% szerokości/wysokości
  ChatHeightPercent = 20; // chat będzie miał 60% wysokości planszy
  ChatWidth         = 250; // szerokość w px
    MovesPct       = 50;  // ruchy = 50% wysokości planszy
  SideMargin     = 10;
var
  avail, boardSize, cellSize, boardLeft, boardTop: Integer;
  movesLeft, movesTop, movesWidth, movesHeight: Integer;
  i, j: Integer;
  chatHeight: Integer;
begin


  if GameType = 4 then
  begin

  avail := Min(ClientWidth, ClientHeight);
  boardSize := (avail * ScalePercent) div 100;
  cellSize  := boardSize div 8;
  boardSize := cellSize * 8;
  boardLeft := 60;
  boardTop  := (ClientHeight - boardSize) div 2;
  Panel.SetBounds(boardLeft, boardTop, boardSize, boardSize);

  for i := 0 to 7 do
    for j := 0 to 7 do
      BoardPanels[i,j].SetBounds(j*cellSize, i*cellSize, cellSize, cellSize);



       if MyColor = 'BLACK' then
  begin
    for i := 0 to 7 do
      for j := 0 to 7 do
        // nowe położenie: zamieniamy i<->j i lustrujemy indeksy 7-i,7-j
        BoardPanels[i,j].SetBounds(
          (7-j)*cellSize,
          (7-i)*cellSize,
          cellSize, cellSize
        );
  end;

  movesLeft   := Panel.Left + Panel.Width + SideMargin;
  movesTop    := Panel.Top;
  movesWidth  := 200;  // stała szerokość listy ruchów
  movesHeight := Panel.Height * MovesPct div 100;
  lstMoves.SetBounds(movesLeft, movesTop, movesWidth, movesHeight);

  btnResign.Left := lstMoves.Left;
  btnResign.Top := lstMoves.Top + lstMoves.Height + 10;


  btnSavePGN.Left := btnResign.Left + btnResign.Width;
  btnSavePGN.Top := btnResign.Top;




    SetupCoordinatesLeftAndBottom;
    UpdateYourLabelsPosition;
    UpdateOpponentLabelsPosition;
  end
  else
  begin


  // 1) Ile mamy miejsca – bierzemy mniejszy wymiar okna
  avail := Min(ClientWidth, ClientHeight);

  // 2) Skalujemy do zadanej procentowo wartości
  boardSize := (avail * ScalePercent) div 100;

  // 3) Dzielimy na 8 pól i wyrównujemy do wielokrotności 8
  cellSize  := boardSize div 8;
  boardSize := cellSize * 8;

  boardLeft := 60;
  boardTop  := (ClientHeight - boardSize) div 2;


  // szerokość stała albo procentowa


  // wysokość 60% planszy
  chatHeight := Panel.Height * ChatHeightPercent div 100;




  // 5) Ustawiamy Panel (planszę)
  Panel.SetBounds(boardLeft, boardTop, boardSize, boardSize);

  // 6) Przesuwamy / skalujemy istniejące pola
  for i := 0 to 7 do
    for j := 0 to 7 do
      BoardPanels[i,j].SetBounds(j*cellSize, i*cellSize, cellSize, cellSize);



       if MyColor = 'BLACK' then
  begin
    for i := 0 to 7 do
      for j := 0 to 7 do
        // nowe położenie: zamieniamy i<->j i lustrujemy indeksy 7-i,7-j
        BoardPanels[i,j].SetBounds(
          (7-j)*cellSize,
          (7-i)*cellSize,
          cellSize, cellSize
        );
  end;


  movesLeft   := Panel.Left + Panel.Width + SideMargin;
  movesTop    := Panel.Top;
  movesWidth  := 200;  // stała szerokość listy ruchów
  movesHeight := Panel.Height * MovesPct div 100;
  lstMoves.SetBounds(movesLeft, movesTop, movesWidth, movesHeight);



     pnlChat.SetBounds(
  Panel.Left + Panel.Width + 10,
  (Panel.Top + Panel.Height) - chatHeight,
  ClientWidth - (Panel.Left + Panel.Width + 20),
  chatHeight
);


  btnResign.Left := LstMoves.Left;
  btnResign.Top := LstMoves.Top + LstMoves.Height + 10;

  btnDraw.Left := btnResign.Left + btnResign.Width + 10;
  btnDraw.Top := btnResign.Top;

  btnSavePGN.Left := btnDraw.Left + btnDraw.Width + 10;
  btnSavePGN.Top := btnDraw.Top;


  SetupCoordinatesLeftAndBottom;
  UpdateClockLabelsPosition;
  UpdateOpponentLabelsPosition;
  UpdateYourLabelsPosition;



  end;



end;





procedure TChess.CreateBoard;
const
  WhiteChars: array[0..7] of string = ('♖','♘','♗','♕','♔','♗','♘','♖');
  BlackChars: array[0..7] of string = ('♜','♞','♝','♛','♚','♝','♞','♜');
var
  i, j: Integer;
  cellSize, boardLeft, boardTop: Integer;
  square: TPanel;
begin
  // 1) Oblicz rozmiar komórki i pozycję planszy
  cellSize  := 60;             // lub np. Panel.Width div 8, jeśli Panel już ma Align=alClient
  boardLeft := 50;
  boardTop  := 50;
  Panel.SetBounds(boardLeft, boardTop, cellSize*8, cellSize*8);

  // 2) Tworzymy pola
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      square := TPanel.Create(Self);
      square.Parent := Panel;
      square.SetBounds(j*cellSize, i*cellSize, cellSize, cellSize);
      square.BevelOuter       := bvNone;
      square.Tag              := i*8 + j;
      square.OnClick          := PanelClick;
      square.Alignment        := taCenter;
      square.Font.Size        := cellSize div 2;          // proporcjonalnie do cellSize
      square.ParentBackground := False;
      if (i + j) mod 2 = 0 then
        square.Color := RGB(198,169,115)
      else
        square.Color := RGB(118,79,43);

      BoardPanels[i,j]    := square;
      OriginalPos[i,j]    := Point(j*cellSize, i*cellSize);
    end;

  if GameType = 4 then
  begin
  YourLogin.Caption := UserSession.LoggedUserLogin;
  CreateMovesList;
  CreateButtons;
  UpdateYourLabelsPosition;
  SetupCoordinatesLeftAndBottom;
  InitializeState;

  end
  else
  begin

  YourLogin.Caption := UserSession.LoggedUserLogin;
  YourRanking.Caption := FindRankingSQL(UserSession.LoggedUserID, GameType);

  CreateMovesList;
  CreateChatControls;
  UpdateYourLabelsPosition;
  CreateButtons;
  SetupCoordinatesLeftAndBottom;
  InitializeState;

  end;


end;


function TChess.ExtractBestMove(const Output: string): string;
var
  Lines: TArray<string>;
  Line: string;
  Parts: TArray<string>;
begin
  Result := '';
  Lines := Output.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);
  for Line in Lines do
  begin
    if Line.StartsWith('bestmove') then
    begin
      Parts := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) >= 2 then
        Exit(Parts[1]); // np. "e2e4"
    end;
  end;
end;


function TChess.UciToCoords(const Move: string; out sr, sc, tr, tc: Integer): Boolean;
begin
  Result := False;
  if Length(Move) < 4 then Exit;

  try
    // źródło
    sc := Ord(Move[1]) - Ord('a');       // kolumna 'a'..'h' → 0..7
    sr := 8 - StrToIntDef(Move[2], 0);   // wiersz '1'..'8' → 7..0

    // cel
    tc := Ord(Move[3]) - Ord('a');
    tr := 8 - StrToIntDef(Move[4], 0);


    if Length(Move) = 5 then
    begin
    PromotionFlag:=true;

        if MyColor = 'BLACK' then

        begin

    case Move[5] of
    'q': promotionChar := 'QW';
    'r': promotionChar := 'RW';
    'b': promotionChar := 'BW';
    'n': promotionChar := 'KW';


    end;

        end

       else
       begin

       case Move[5] of
       'q': promotionChar := 'QB';
       'r': promotionChar := 'RB';
       'b': promotionChar := 'BB';
       'n': promotionChar := 'KB';



       end;




       end;






    end;
















    Result := True;
  except
    Result := False;
  end;
end;


function TChess.FindRankingSQL(userid: Integer; gtype: Integer): string;
var
query: TFDQuery;

begin

  query := TFDQuery.Create(nil);
  try
    query.Connection := FDConnection1;
    query.SQL.Text := 'SELECT rating FROM rankings WHERE users_userid = :users_userid AND game_type_id = :game_type_id';
    query.ParamByName('users_userid').AsInteger:= userid;
    query.ParamByName('game_type_id').AsInteger := gtype;
    query.Open;

    if not query.IsEmpty then
    begin
    result:= query.FieldByName('rating').AsString;
    end
    else
    begin
    result:= '';
    end;
  finally
    query.Free;
  end;





end;






procedure TChess.UpdateClockLabels;
begin
  lblWhiteTime.Caption := Format('White: %d:%2.2d', [WhiteSeconds div 60, WhiteSeconds mod 60]);
  lblBlackTime.Caption := Format('Black: %d:%2.2d', [BlackSeconds div 60, BlackSeconds mod 60]);
end;


procedure TChess.tmrWhiteTimer(Sender: TObject);
begin
  Dec(WhiteSeconds);
  UpdateClockLabels;
  if WhiteSeconds <= 0 then
  begin
    tmrWhite.Enabled := False;
    tmrBlack.Enabled := False;
    if MyColor = 'WHITE' then
    begin
    SendMove('ENDGAME:LOSE');
    ShowMessage('You lost on time!');
    Lost:=true;
    BtnDraw.Visible:=false;
    BtnResign.Visible:=false;
    BtnSavePGN.Visible:=true;
    InGame:=false;
    DisableBoard;
    end
    else
    begin
    ShowMessage('White lost on time, you win!');
    Win:=true;
    BtnDraw.Visible:=false;
    BtnResign.Visible:=false;
    InGame:=false;
    DisableBoard;
    end;

  end;
end;


procedure TChess.tmrBlackTimer(Sender: TObject);
begin
  Dec(BlackSeconds);
  UpdateClockLabels;
  if BlackSeconds <= 0 then
  begin

    tmrWhite.Enabled := False;
    tmrBlack.Enabled := False;

    if MyColor = 'BLACK' then
    begin
    SendMove('ENDGAME:LOSE');
    ShowMessage('You lost on time!');
    BtnDraw.Visible:=false;
    BtnResign.Visible:=false;
    BtnSavePGN.Visible:=true;
    Lost:=true;
    InGame:=false;
    DisableBoard;
    end
    else
    begin
    ShowMessage('Black lost on time, you win!');
    Win:=true;
    BtnDraw.Visible:=false;
    BtnResign.Visible:=false;
    InGame:=false;
    DisableBoard;
    end;

  end;
end;






procedure TChess.InitializeState;
var i, j: Integer;
  output, move: string;
  parts: TArray<string>;
  sr, sc, tr, tc: Integer;
  San: String;
  promo: Tpiece;
begin

    HasWhiteKingMoved:= false;
    HasWhiteRookA1Moved:= false;
    HasWhiteRookH1Moved:= false;
    HasBlackKingMoved:= false;
    HasBlackRookA8Moved:= false;
    HasBlackRookH8Moved:= false;

  // Wyzeruj stan
  for i := 0 to 7 do
    for j := 0 to 7 do
      BoardState[i,j] := ptNone;

  // Białe figury
  BoardState[7,0] := ptWRook; BoardState[7,1] := ptWKnight;
  BoardState[7,2] := ptWBishop; BoardState[7,3] := ptWQueen;
  BoardState[7,4] := ptWKing; BoardState[7,5] := ptWBishop;
  BoardState[7,6] := ptWKnight;BoardState[7,7] := ptWRook;
  for j := 0 to 7 do BoardState[6,j] := ptWPawn;

  // Czarne
  BoardState[0,0] := ptBRook; BoardState[0,1] := ptBKnight;
  BoardState[0,2] := ptBBishop;BoardState[0,3] := ptBQueen;
  BoardState[0,4] := ptBKing;  BoardState[0,5] := ptBBishop;
  BoardState[0,6] := ptBKnight;BoardState[0,7] := ptBRook;
  for j := 0 to 7 do BoardState[1,j] := ptBPawn;

  // Ustaw UI
  for i := 0 to 7 do
    for j := 0 to 7 do
      BoardPanels[i,j].Caption := PieceToChar(BoardState[i,j]);

  SelectedSrc := Point(-1,-1);
  LastSrc := Point(-1,-1);
  LastDst := Point(-1,-1);
  IsMyTurn := False;
  Resize;

  if GameType = 4 then
  begin

  If MyColor='BLACK' then
  begin



  // 1. Aktualizuj pozycję (np. przechowuj historię ruchów w FMoveHistory)
  Uci.SendCommand('position startpos moves ' + FMoveHistory);

  // 2. Poproś o najlepszy ruch
  Uci.SendCommand('go depth 12');

  // 3. Odbierz wynik
  repeat
    output := Uci.ReadOutput;
  until output.Contains('bestmove');

  // 4. Parsuj "bestmove e2e4"

  var move2 := ExtractBestMove(output);


  if (move2 <> '') and UciToCoords(move2, sr, sc, tr, tc) then
  begin


    var wasCapture := BoardState[tr, tc] <> ptNone;
    var   p := BoardState[sr, sc];      // ← zapamiętujemy przed apply

      san := MoveToSAN(
      p,
      Point(sc, sr),
      Point(tc, tr),
      wasCapture,
      promo
      );



  ApplyMove(sr, sc, tr, tc, ptNone);


        if PromotionFlag then
      begin
        // PromotionChar ustawiasz gdzie indziej jako 'Q','N' itd.
       promo := CodeToPiece(PromotionChar);

        // (napisz własną funkcję mapującą)
      end
      else
        promo := ptNone;

      san:= san+SanHelper(promo);



      AddMoveToList(san);



  FMoveHistory := FMoveHistory + ' ' + move2;
  IsMyTurn:=True;
  end




  end;




  end;


end;

procedure TChess.PanelClick(Sender: TObject);
var
  idx, vRow, vCol: Integer;
  row, col: Integer;
  square: TPanel;
  Elapsed: Integer;
  san: String;
  promo: TPiece;
  wasCapture: Boolean;

begin


  if not IsMyTurn then Exit;

  square := Sender as TPanel;
  idx := square.Tag;
  vRow := idx div 8; // wizualny rząd
  vCol := idx mod 8; // wizualna kolumna

  // MAPUJEMY na logiczne współrzędne:
  if MyColor = 'BLACK' then
  begin
    row := 7 - vRow;
    col := 7 - vCol;
  end
  else
  begin
    row := vRow;
    col := vCol;
  end;



  // Teraz zamiast BoardState[vRow,vCol] robimy BoardState[row,col]:
  if (SelectedSrc.X < 0) and OwnsPiece(BoardState[row,col]) then
  begin
    // Zaznaczenie źródła:
    SelectedSrc := Point(col, row);
    SelectedPanel := square;
    square.Color := clYellow;
    Exit;
  end;

  if SelectedSrc.X >= 0 then
  begin
    var srcRow := SelectedSrc.Y;
    var srcCol := SelectedSrc.X;
    var dstRow := row;
    var dstCol := col;

    if IsLegalMove(srcRow, srcCol, dstRow, dstCol) then
    begin
      wasCapture := BoardState[dstRow, dstCol] <> ptNone;
      var  p := BoardState[srcRow, srcCol];      // ← zapamiętujemy przed apply

      san := MoveToSAN(
      p,
      Point(srcCol, srcRow),
      Point(dstCol, dstRow),
      wasCapture,
      promo
      );


      ApplyMove(srcRow, srcCol, dstRow, dstCol, ptNone);


          if PromotionFlag then
      begin
        // PromotionChar ustawiasz gdzie indziej jako 'Q','N' itd.
       promo := CodeToPiece(PromotionChar);

        // (napisz własną funkcję mapującą)
      end
      else
        promo := ptNone;

      san:= san+SanHelper(promo);



      AddMoveToList(san);
      SendMove('SAN:'+san);

      CheckEndGame;
      SendMove( Format('%d,%d->%d,%d',[srcRow,srcCol,dstRow,dstCol]) );


    if (GameType = 4) then
    begin

    var temppromo: string;

      if PromotionFlag then
        begin
         temppromo:=PromotionChar;
         PromotionChar:='';
         PromotionFlag:= false;
        end;


      if Draw then EXIT;
      if Win then EXIT;
       

      IsMyTurn := False;

       // 1. Zamień ruch człowieka na UCI
     var srcUci := Chr(Ord('a') + srcCol) + IntToStr(8 - srcRow);
     var dstUci := Chr(Ord('a') + dstCol) + IntToStr(8 - dstRow);
     var moveUci := srcUci + dstUci;
     var sr,sc,tr,tc: Integer;


     if (temppromo = 'QW') or (temppromo ='QB') then moveUci:= moveUci+'q';
     if (temppromo = 'RW') or (temppromo ='RB') then moveUci:= moveUci+'r';
     if (temppromo = 'BW') or (temppromo ='BB') then moveUci:= moveUci+'b';
     if (temppromo = 'KW') or (temppromo ='KB') then moveUci:= moveUci+'n';

      // 2. Dodaj do historii
      if FMoveHistory <> '' then
        FMoveHistory := FMoveHistory + ' ' + moveUci
      else
        FMoveHistory := moveUci;

      // 3. Zaktualizuj pozycję w Stockfish

      Uci.SendCommand('position startpos moves ' + FMoveHistory);
      Uci.SendCommand('go depth 2');

      // 4. Poczekaj na odpowiedź
      var output: string;
      repeat
        output := Uci.ReadOutput;
      until output.Contains('bestmove');

      // 5. Parsuj odpowiedź
      var parts := ExtractBestMove(output);
      if (Length(parts) >= 2) and UciToCoords(parts, sr, sc, tr, tc) then
      begin

         wasCapture := BoardState[tr, tc] <> ptNone;
         p := BoardState[sr, sc];      // ← zapamiętujemy przed apply

      san := MoveToSAN(
      p,
      Point(sc, sr),
      Point(tc, tr),
      wasCapture,
      promo
      );

        ApplyMove(sr, sc, tr, tc, ptNone);

        LastSrc := Point(sc, sr);
        LastDst := Point(tc, tr);


          if PromotionFlag then
      begin
        // PromotionChar ustawiasz gdzie indziej jako 'Q','N' itd.
       promo := CodeToPiece(PromotionChar);
      BoardState[tr, tc] := promo;
      BoardPanels[tr, tc].Caption := PieceToChar(promo);
      UpdateBoardColors;


      end
      else
        promo := ptNone;






      san:= san+SanHelper(promo);
      AddMoveToList(san);

      if PromotionFlag then
        begin
         PromotionChar:='';
         PromotionFlag:= false;
        end;


        CheckEndgame;
        FMoveHistory := FMoveHistory + ' ' + parts;
        IsMyTurn := True; // teraz znów kolej człowieka
      end;


    end
    else


    begin

    UpdateClockLabels;
    Elapsed := SecondsBetween(Now, TurnStartTime);
    if UserSession.IdTCPClient1.Connected then
    UserSession.IdTCPClient1.IOHandler.WriteLn('TIME:' + IntToStr(Elapsed));


    tmrWhite.Enabled := False;
    tmrBlack.Enabled := False;


       if PromotionFlag then
        begin
         SendMove('PROMO:'+ PromotionChar);
         PromotionChar:='';
         PromotionFlag:= false;
        end;

        IsMyTurn := False;

    LastSrc := Point(srcCol, srcRow);
    LastDst := Point(dstCol, dstRow);

    end;







    end;

    // odznaczamy panel źródłowy:
    if Assigned(SelectedPanel) then
    begin
      // oryginalny kolor po (vRow+vCol) mod 2:
      if (vRow + vCol) mod 2 = 0 then
        SelectedPanel.Color := RGB(198,169,115)
      else
        SelectedPanel.Color := RGB(118,79,43);
    end;

    SelectedSrc := Point(-1,-1);
    SelectedPanel := nil;
    UpdateBoardColors;
  end;














end;

function TChess.PieceToChar(P: TPiece): string;
begin
  case P of
    ptWPawn: Result := '♙'; ptWRook: Result := '♖';
    ptWKnight: Result := '♘'; ptWBishop: Result := '♗';
    ptWQueen: Result := '♕'; ptWKing: Result := '♔';
    ptBPawn: Result := '♟'; ptBRook: Result := '♜';
    ptBKnight: Result := '♞'; ptBBishop: Result := '♝';
    ptBQueen: Result := '♛'; ptBKing: Result := '♚';
  else Result := '';
  end;
end;

function TChess.OwnsPiece(P: TPiece): Boolean;
begin
  if MyColor = 'WHITE' then
    Result := Ord(P) in [Ord(ptWPawn)..Ord(ptWKing)]
  else
    Result := Ord(P) in [Ord(ptBPawn)..Ord(ptBKing)];
end;

procedure TChess.SendMove(const Move: string);
begin
  if UserSession.IdTCPClient1.Connected then
    UserSession.IdTCPClient1.IOHandler.WriteLn(Move);
end;


   //
procedure TChess.ReceiveMessages;
var
  Msg, FollowUp, payload: string;
  parts: TArray<string>;
  wPart, bPart: string;
  sr, sc, tr, tc: Integer;
begin
  if not UserSession.IdTCPClient1.Connected then Exit;

  // 1) Sprawdź, czy są dane
  if UserSession.IdTCPClient1.IOHandler.InputBufferIsEmpty then
    UserSession.IdTCPClient1.IOHandler.CheckForDataOnSource(1);
  if UserSession.IdTCPClient1.IOHandler.InputBufferIsEmpty then Exit;

  // 2) Odczytujemy pierwszy pakiet
  Msg := Trim(UserSession.IdTCPClient1.IOHandler.ReadLn('', 50));
  if Msg = '' then Exit;

  // 3) Obsługa TIME_UPDATE
  if Msg.StartsWith('TIME_UPDATE:') then
  begin
    // synchronizacja czasów
    parts := Msg.Substring(12).Split(['|']);
    wPart := parts[0].Split(['='])[1];
    bPart := parts[1].Split(['='])[1];
    WhiteSeconds := StrToIntDef(wPart, WhiteSeconds);
    BlackSeconds := StrToIntDef(bPart, BlackSeconds);
    UpdateClockLabels;
    TurnStartTime := Now;

    UpdateTimers;

    //


    // 4) Od razu próbujemy odczytać drugi pakiet
    if not UserSession.IdTCPClient1.IOHandler.InputBufferIsEmpty then
    begin
      FollowUp := Trim(UserSession.IdTCPClient1.IOHandler.ReadLn('', 50));
      if FollowUp <> '' then
      begin
        // sprawdzamy kolejno możliwe typy
        if FollowUp.StartsWith('OPPONENT_MOVE:') then
        begin
          payload := FollowUp.Substring(14).Trim;
          parts := payload.Split(['-', '>', ','], TStringSplitOptions.ExcludeEmpty);
          if Length(parts) = 4 then
            try

              sr := StrToInt(parts[0]); sc := StrToInt(parts[1]);
              tr := StrToInt(parts[2]); tc := StrToInt(parts[3]);
              ApplyMove(sr, sc, tr, tc, ptNone);

              CheckEndGame;
              LastSrc := Point(sc, sr);
              LastDst := Point(tc, tr);

              IsMyTurn := True;
              TurnStartTime := Now;
              UpdateTimers;


            except on E: EConvertError do
              ShowMessage('Błąd parsowania ruchu przeciwnika: ' + payload);
            end
          else
            ShowMessage('Niepoprawny ruch: ' + payload)
        end
        else if FollowUp.StartsWith('OPPONENT_PROMO:') then
        begin

    var promoCode := FollowUp.Substring(15).Trim;
    var promotionPiece: TPiece := ptNone;
    if promoCode = 'QW' then promotionPiece := ptWQueen
    else if promoCode = 'RW' then promotionPiece := ptWRook
    else if promoCode = 'BW' then promotionPiece := ptWBishop
    else if promoCode = 'KW' then promotionPiece := ptWKnight
    else if promoCode = 'QB' then promotionPiece := ptBQueen
    else if promoCode = 'RB' then promotionPiece := ptBRook
    else if promoCode = 'BB' then promotionPiece := ptBBishop
    else if promoCode = 'KB' then promotionPiece := ptBKnight;
    BoardState[LastDst.Y, LastDst.X] := promotionPiece;
    BoardPanels[LastDst.Y, LastDst.X].Caption := PieceToChar(promotionPiece);
    UpdateBoardColors;



        end
        else if FollowUp.StartsWith('ENDGAME:') then
        begin

          var mess := FollowUp.Substring(8);
          if mess = 'LOSE' then
          begin
            Lost:=true;
            ShowMessage('You lost by checkmate!');
            DisableBoard;
            tmrWhite.Enabled := False;
            tmrBlack.Enabled := False;
            BtnSavePGN.Visible:= true;
            btnResign.Visible:=false;
            btnDraw.Visible:=false;
            InGame:= false;
          end
          else if mess = 'WIN' then
          begin
            Win:=true;
            DisableBoard;
            tmrWhite.Enabled := False;
            tmrBlack.Enabled := False;
            BtnSavePGN.Visible:= true;
            btnResign.Visible:=false;
            btnDraw.Visible:=false;
            InGame:= false;
          end


          else if mess = 'DRAW' then
          begin
          Draw:=true;
          DisableBoard;
          tmrWhite.Enabled := False;
          tmrBlack.Enabled := False;
          BtnSavePGN.Visible:= true;
          btnResign.Visible:=false;
          btnDraw.Visible:=false;
          InGame:= false;

          end;

        end
        else if FollowUp.StartsWith('COLOR:') then
        begin
          MyColor := FollowUp.Substring(6);
          SetupAfterColor;
        end
        else if FollowUp.StartsWith('OPPONENT:') then
          OpponentName := FollowUp.Substring(9)

        else if FollowUp.StartsWith('DRAW:OFFER') then
        begin

         if MessageDlg('Przeciwnik proponuje remis. Przyjąć?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            // zaakceptowano
            SendMove('DRAW:ACCEPT');
          end
          else
          begin
            // odrzucono
            SendMove('DRAW:DECLINE');
          end;

        end


        else if FollowUp.StartsWith('DRAW:ACCEPT') then
        begin
        memChat.Lines.Add('Remis poprzez zgodę obu stron');
        end

        else if FollowUp.StartsWith('DRAW:DECLINE') then
        begin
        memChat.Lines.Add('Przeciwnik odrzucil propozycje remis');
        BtnDraw.Visible:=true;
        end


       else if FollowUp.StartsWith('RANKING:') then
       begin
       var temp:= FindRankingSQL(OppId,GameType);
       SendMove('RANKING:'+temp);
       end


       else if FollowUp.StartsWith('PGN:') then
       begin
       var s := StringReplace(MoveListToPGN, sLineBreak, '\n', [rfReplaceAll]);
       SendMove('PGN:' + s);
       end;


      end;





    end;

    Exit;
  end;

  // 5) Jeśli pierwszy pakiet był od razu OPPONENT_MOVE
  if Msg.StartsWith('OPPONENT_MOVE:') then
  begin
    payload := Msg.Substring(14).Trim;
    parts := payload.Split(['-', '>', ','], TStringSplitOptions.ExcludeEmpty);
    if Length(parts) = 4 then
      try
        sr := StrToInt(parts[0]); sc := StrToInt(parts[1]);
        tr := StrToInt(parts[2]); tc := StrToInt(parts[3]);
        ApplyMove(sr, sc, tr, tc, ptNone);

        CheckEndGame;
        LastSrc := Point(sc, sr);
        LastDst := Point(tc, tr);

        IsMyTurn := True;
        TurnStartTime := Now;
        UpdateTimers;


      except on E: EConvertError do
        ShowMessage('Błąd parsowania ruchu przeciwnika: ' + payload);
      end
    else
      ShowMessage('Niepoprawny ruch: ' + payload);
    Exit;
  end;

  // 6) Pozostałe komunikaty
  if Msg = 'START' then
  begin
   ShowMessage('Gra rozpoczęta!');
   btnResign.Visible:=true;
   btnDraw.Visible:=true;
   InGame:= True;
  end

  else if Msg.StartsWith('COLOR:') then
  begin
    MyColor := Msg.Substring(6);
    SetupAfterColor;
  end
  else if Msg.StartsWith('OPPONENT:') then
  begin
    OpponentName := Msg.Substring(9);
    OppLogin.Caption:= OpponentName;
    UpdateOpponentLabelsPosition;
  end
  else if Msg.StartsWith('ID:') then
  begin
  OppId := StrToInt(Msg.Substring(3));
  OppRanking.Caption:= FindRankingSQL(OppId,GameType);
  UpdateOpponentLabelsPosition;

  end

  else if Msg.StartsWith('OPPONENT_LEFT') then
  begin
  ShowMessage('Your opponent left, you win!');
  Win:=true;
  DisableBoard;
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;
  BtnSavePGN.Visible:= true;
  btnResign.Visible:=false;
  btnDraw.Visible:=false;
  InGame:= false;
  end
  else if Msg.StartsWith('OPPONENT_PROMO:') then

  begin
    var promoCode := Msg.Substring(15);
    var promotionPiece: TPiece := ptNone;
    if promoCode = 'QW' then promotionPiece := ptWQueen
    else if promoCode = 'RW' then promotionPiece := ptWRook
    else if promoCode = 'BW' then promotionPiece := ptWBishop
    else if promoCode = 'KW' then promotionPiece := ptWKnight
    else if promoCode = 'QB' then promotionPiece := ptBQueen
    else if promoCode = 'RB' then promotionPiece := ptBRook
    else if promoCode = 'BB' then promotionPiece := ptBBishop
    else if promoCode = 'KB' then promotionPiece := ptBKnight;
    BoardState[LastDst.Y, LastDst.X] := promotionPiece;
    BoardPanels[LastDst.Y, LastDst.X].Caption := PieceToChar(promotionPiece);
    UpdateBoardColors;


  end





  else if Msg.StartsWith('ENDGAME:') then
  begin

  var mess := Msg.Substring(8);
  if mess = 'LOSE' then
  begin
  Lost:=true;
  ShowMessage('You lost by checkmate!');
  DisableBoard;
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;
  BtnSavePGN.Visible:= true;
  btnResign.Visible:=false;
  btnDraw.Visible:=false;
  InGame:= false;
  end


  else if mess = 'WIN' then
  begin
  Win:=true;
  DisableBoard;
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;
  BtnSavePGN.Visible:= true;
  btnResign.Visible:=false;
  btnDraw.Visible:=false;
  InGame:= false;

  end


  else if mess = 'DRAW' then
  begin
  Draw:=true;
  DisableBoard;
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;
  BtnSavePGN.Visible:= true;
  btnResign.Visible:=false;
  btnDraw.Visible:=false;
  InGame:= false;


  end;

  end


 else if Msg.StartsWith('CHAT:') then
begin
  memChat.Lines.Add(Msg.Substring(5).Trim);
  Exit;
end


  else if Msg.StartsWith('SAN:') then
   begin
  // Msg = 'SAN:e4' albo 'SAN:Nf3' albo 'SAN:exd5' itd.
     AddMoveToList(Msg.Substring(4).Trim);
     Exit;
   end


   else if Msg.StartsWith('DRAW:OFFER') then
   begin

    if MessageDlg('Przeciwnik proponuje remis. Przyjąć?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
    // zaakceptowano
    SendMove('DRAW:ACCEPT');
    end
    else
    begin
    // odrzucono
     SendMove('DRAW:DECLINE');
    end;


   end


   else if Msg.StartsWith('DRAW:DECLINE') then
   begin
   memChat.Lines.Add('Przeciwnik odrzucil propozycje remis');
   BtnDraw.Visible:=true;
   end

   else if Msg.StartsWith('DRAW:ACCEPT') then
   begin
   memChat.Lines.Add('Remis poprzez zgodę obu stron');
   end

   else if Msg.StartsWith('PGN:') then
   begin
   var s := StringReplace(MoveListToPGN, sLineBreak, '\n', [rfReplaceAll]);
   SendMove('PGN:' + s);
   end

   else if Msg.StartsWith('RANKING:') then
   begin
   var temp:= FindRankingSQL(OppId,GameType);
   SendMove('RANKING:'+temp);
   end;




end;
















procedure TChess.SetupAfterColor;
begin
  IsMyTurn := MyColor = 'WHITE';
  TurnStartTime := Now;

  case GameType of
    1: begin WhiteSeconds := RapidTime;  BlackSeconds := RapidTime;  IncrementSeconds := RapidIncrement; end;
    2: begin WhiteSeconds := BlitzTime;  BlackSeconds := BlitzTime;  IncrementSeconds := BlitzIncrement; end;
    3: begin WhiteSeconds := BulletTime; BlackSeconds := BulletTime; IncrementSeconds := BulletIncrement; end;
  end;
  UpdateClockLabels;

  // włącz zegar gracza, wyłącz zegar przeciwnika
  tmrWhite.Enabled := true;
  tmrBlack.Enabled := false;

  if MyColor = 'BLACK' then
    RotateBoardForBlack;
    SetupCoordinatesLeftAndBottom;
    UpdateClockLabelsPosition;

    TurnStartTime := Now;


end;

procedure TChess.Timer1Timer(Sender: TObject);
begin
  ReceiveMessages;
end;



procedure TChess.RotateBoardForBlack;
var i, j, sizeCell: Integer;
begin
  sizeCell := BoardPanels[0,0].Width;
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      BoardPanels[i,j].SetBounds(
        (7 - j) * sizeCell,
        (7 - i) * sizeCell,
        sizeCell,
        sizeCell
      );
      BoardPanels[i,j].Tag := (7 - i) * 8 + (7 - j); // aktualizacja tagu!
    end;
end;





function TChess.IsPathClear(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
var
  dRow, dCol, r, c: Integer;
begin
  Result := True;
  dRow := Sign(dstRow - srcRow);
  dCol := Sign(dstCol - srcCol);
  r := srcRow + dRow;
  c := srcCol + dCol;
  while (r <> dstRow) or (c <> dstCol) do
  begin
    if BoardState[r][c] <> ptNone then
    begin
      Result := False;
      Exit;
    end;
    Inc(r, dRow);
    Inc(c, dCol);
  end;
end;

function TChess.IsValidMove(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
var
  Piece: TPiece;
  dx, dy: Integer;
  TargetPiece: TPiece;
begin
  Result := False;
  Piece := BoardState[srcRow][srcCol];
  TargetPiece := BoardState[dstRow][dstCol];

  if (Piece = ptNone) then Exit;

// Sprawdź, czy próbujesz zbić własną figurę
if ((Ord(Piece) in [Ord(ptWPawn)..Ord(ptWKing)]) and (Ord(TargetPiece) in [Ord(ptWPawn)..Ord(ptWKing)]))
   or
   ((Ord(Piece) in [Ord(ptBPawn)..Ord(ptBKing)]) and (Ord(TargetPiece) in [Ord(ptBPawn)..Ord(ptBKing)]))
then Exit;


  dx := dstCol - srcCol;
  dy := dstRow - srcRow;

  case Piece of
    ptWPawn:
      begin

        // ruch do przodu
        if (dx = 0) and ((dy = -1) or ((dy = -2) and (srcRow = 6) and (BoardState[5,srcCol]=ptNone))) then
          Result := (TargetPiece = ptNone) and ((dy = -1) or ((dy = -2) and (BoardState[5, srcCol] = ptNone)))
        // bicie normalne
        else if (Abs(dx)=1) and (dy=-1) and (TargetPiece in [ptBPawn..ptBKing]) then
          Result := True
        // en passant
        else if (Abs(dx)=1) and (dy=-1) and (TargetPiece = ptNone) then

        begin
          // uniwersalny test na dwupolowy ruch przeciwnika:
          if (Abs(LastDst.Y - LastSrc.Y) = 2)
             // i ten ruch zakończył się dokładnie tam, gdzie stoi pion przeciwnika:
             and (LastDst.Y = srcRow)
             and (LastDst.X = dstCol) then
          begin
            Result := True;
            Exit;
          end;
        end;

      end;
    ptBPawn:
      begin

        if (dx = 0) and ((dy = 1) or ((dy = 2) and (srcRow = 1) and (BoardState[2,srcCol]=ptNone))) then
          Result := (TargetPiece = ptNone) and ((dy=1) or ((dy=2) and (BoardState[2,srcCol] = ptNone)))
        else if (Abs(dx)=1) and (dy=1) and (TargetPiece in [ptWPawn..ptWKing]) then
          Result := True
        else if (Abs(dx)=1) and (dy=1) and (TargetPiece = ptNone) then

        begin
          if (Abs(LastDst.Y - LastSrc.Y) = 2)
             and (LastDst.Y = srcRow)
             and (LastDst.X = dstCol) then
          begin
            Result := True;
            Exit;
          end;
        end;


      end;
    ptWRook, ptBRook:
      begin
        if (dx = 0) or (dy = 0) then
          Result := IsPathClear(srcRow, srcCol, dstRow, dstCol);
      end;
    ptWBishop, ptBBishop:
      begin
        if Abs(dx) = Abs(dy) then
          Result := IsPathClear(srcRow, srcCol, dstRow, dstCol);
      end;
    ptWQueen, ptBQueen:
      begin
        if ((dx = 0) or (dy = 0) or (Abs(dx) = Abs(dy))) then
          Result := IsPathClear(srcRow, srcCol, dstRow, dstCol);
      end;
    ptWKnight, ptBKnight:
      begin
        if (Abs(dx) = 1) and (Abs(dy) = 2) or (Abs(dx) = 2) and (Abs(dy) = 1) then
          Result := True;
      end;
    ptWKing, ptBKing:
  begin
    // normalny ruch króla o jedno pole
    if (Abs(dx) <= 1) and (Abs(dy) <= 1) then
      Result := True
    // roszada: ruch o 2 pola w poziomie
    else if (dy = 0) and (Abs(dx) = 2) then
    begin
      // przygotujemy warunki:
      // 1) król i odpowiadająca wieża nie ruszone
      // 2) między szachownicą puste pola
      // 3) żadne z trzech pól (start, przez, cel) nie jest atakowane

      // biała czy czarna?
      if Piece = ptWKing then
      begin
        // krótka O-O
        if (dx =  2)
           and not HasWhiteKingMoved
           and not HasWhiteRookH1Moved
           and (BoardState[srcRow,5] = ptNone)
           and (BoardState[srcRow,6] = ptNone)
           and not IsSquareAttacked(srcRow,4, OpponentColor)
           and not IsSquareAttacked(srcRow,5, OpponentColor)
           and not IsSquareAttacked(srcRow,6, OpponentColor)
        then Result := True
        // długa O-O-O
        else if (dx = -2)
           and not HasWhiteKingMoved
           and not HasWhiteRookA1Moved
           and (BoardState[srcRow,1] = ptNone)
           and (BoardState[srcRow,2] = ptNone)
           and (BoardState[srcRow,3] = ptNone)
           and not IsSquareAttacked(srcRow,4, OpponentColor)
           and not IsSquareAttacked(srcRow,3, OpponentColor)
           and not IsSquareAttacked(srcRow,2, OpponentColor)
        then Result := True;
      end
      else
      begin
        // analogicznie dla czarnego
        if (dx =  2)
           and not HasBlackKingMoved
           and not HasBlackRookH8Moved
           and (BoardState[srcRow,5] = ptNone)
           and (BoardState[srcRow,6] = ptNone)
           and not IsSquareAttacked(srcRow,4, OpponentColor)
           and not IsSquareAttacked(srcRow,5, OpponentColor)
           and not IsSquareAttacked(srcRow,6, OpponentColor)
        then Result := True
        else if (dx = -2)
           and not HasBlackKingMoved
           and not HasBlackRookA8Moved
           and (BoardState[srcRow,1] = ptNone)
           and (BoardState[srcRow,2] = ptNone)
           and (BoardState[srcRow,3] = ptNone)
           and not IsSquareAttacked(srcRow,4, OpponentColor)
           and not IsSquareAttacked(srcRow,3, OpponentColor)
           and not IsSquareAttacked(srcRow,2, OpponentColor)
        then Result := True;
      end;
    end;
  end;

  end;
end;

procedure TChess.ApplyMove(srcRow, srcCol, dstRow, dstCol: Integer; promotionPiece: TPiece);
var
  P: TPiece;
  capRow, capCol: Integer;
  promotedChar: string;
begin
  P := BoardState[srcRow, srcCol];

  // en passant: bijemy pionek 'po skosie' na pustym polu
  if ((P = ptWPawn) or (P = ptBPawn)) and (srcCol <> dstCol)
     and (BoardState[dstRow, dstCol] = ptNone) then
  begin
    // wylicz pole, skąd bijemy:
    if P = ptWPawn then
      capRow := dstRow + 1
    else
      capRow := dstRow - 1;
    capCol := dstCol;

    BoardState[capRow, capCol] := ptNone;
    BoardPanels[capRow, capCol].Caption := '';
  end;



    if BoardState[srcRow,srcCol] in [ptWKing, ptBKing] then
  begin
    if BoardState[srcRow,srcCol] = ptWKing then
      HasWhiteKingMoved := True
    else
      HasBlackKingMoved := True;
  end;


   if BoardState[srcRow,srcCol] = ptWRook then
  begin
    // biała wieża a1 to srcRow=7,srcCol=0
    if (srcRow=7) and (srcCol=0) then HasWhiteRookA1Moved := True;
    // biała wieża h1 to srcRow=7,srcCol=7
    if (srcRow=7) and (srcCol=7) then HasWhiteRookH1Moved := True;
  end
  else if BoardState[srcRow,srcCol] = ptBRook then
  begin
    if (srcRow=0) and (srcCol=0) then HasBlackRookA8Moved := True;
    if (srcRow=0) and (srcCol=7) then HasBlackRookH8Moved := True;
  end;




   if (P in [ptWKing, ptBKing]) and (Abs(dstCol - srcCol) = 2) then
begin
  // krótkie O-O
  if dstCol = srcCol + 2 then
  begin
    // przesuń wieżę z h-file na f-file
    BoardState[srcRow,7] := ptNone;
    BoardPanels[srcRow,7].Caption := '';
    if P = ptWKing then
  BoardState[srcRow,5] := ptWRook
   else
  BoardState[srcRow,5] := ptBRook;
    BoardPanels[srcRow,5].Caption := PieceToChar(BoardState[srcRow,5]);
  end
  // długie O-O-O
  else if dstCol = srcCol - 2 then
  begin
    // przesuń wieżę z a-file na d-file
    BoardState[srcRow,0] := ptNone;
    BoardPanels[srcRow,0].Caption := '';
   // BoardState[srcRow,3] := IfThen(P=ptWKing, ptWRook, ptBRook);
    if P = ptWKing then
  BoardState[srcRow,3] := ptWRook
  else
  BoardState[srcRow,3] := ptBRook;
    BoardPanels[srcRow,3].Caption := PieceToChar(BoardState[srcRow,3]);
  end;
end;














    if ((P = ptWPawn) and (dstRow = 0)) or ((P = ptBPawn) and (dstRow = 7)) then
  begin

      if promotionPiece <> ptNone then
    P := promotionPiece
      else if IsMyTurn then
  begin
    promotedChar := PromotePawnDialog;
    PromotionFlag:= true;
    if promotedChar = '♕' then
    begin
    P := ptWQueen;
    promotionChar:='QW'
    end

    else if promotedChar = '♖' then
    begin
    P := ptWRook;
    promotionChar:='RW'
    end

    else if promotedChar = '♗' then
    begin
    P := ptWBishop;
    promotionChar:='BW'
    end

    else if promotedChar = '♘' then
    begin
    P := ptWKnight;
    promotionChar:='KW'

    end

    else if promotedChar = '♛' then
    begin
     P := ptBQueen;
     promotionChar:='QB'
    end

    else if promotedChar = '♜' then
    begin
    P := ptBRook;
    promotionChar:='RB'
    end

    else if promotedChar = '♝' then
    begin
    P := ptBBishop;
    promotionChar:='BB'
    end

    else if promotedChar = '♞' then
    begin
    P := ptBKnight;
    promotionChar:='KB'
    end;




  end;


  end;
  // normalny ruch i bicie
  BoardState[dstRow, dstCol] := P;
  BoardState[srcRow, srcCol] := ptNone;
  BoardPanels[srcRow, srcCol].Caption := '';
  BoardPanels[dstRow, dstCol].Caption := PieceToChar(P);

  // odśwież kolory i pamięć ostatniego ruchu
  UpdateBoardColors;

end;



procedure TChess.UpdateBoardColors;
var
  i, j: Integer;
begin
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if (i + j) mod 2 = 0 then
        BoardPanels[i, j].Color := RGB(198,169,115)
      else
        BoardPanels[i, j].Color := RGB(118,79,43);
    end;
end;






function TChess.PromotePawnDialog(): string;
const
  // kolejność: Hetman, Wieża, Goniec, Skoczek
  WhitePieces: array[0..3] of string = ('♕','♖','♗','♘');
  BlackPieces: array[0..3] of string = ('♛','♜','♝','♞');
var
  dlg: TForm;
  btn: TButton;
  i, btnWidth: Integer;
  arr: array of string;
begin
  // wybierz tablicę figur
  if MyColor = 'WHITE' then
  begin
    SetLength(arr, Length(WhitePieces));
    for i := 0 to High(WhitePieces) do arr[i] := WhitePieces[i];
  end
  else
  begin
    SetLength(arr, Length(BlackPieces));
    for i := 0 to High(BlackPieces) do arr[i] := BlackPieces[i];
  end;

  // utwórz dialog
  dlg := TForm.Create(nil);
  try
    dlg.Caption := 'Promocja pionka';
    dlg.BorderStyle := bsDialog;
    dlg.Position := poScreenCenter;
    dlg.ClientWidth := 100 * Length(arr) + 20;
    dlg.ClientHeight := 100;

    promotionSelection := 0; // domyślne: hetman
    btnWidth := (dlg.ClientWidth - 20) div Length(arr);

    for i := 0 to High(arr) do
    begin
      btn := TButton.Create(dlg);
      btn.Parent := dlg;
      btn.Caption := arr[i];
      btn.SetBounds(10 + i * btnWidth, 10, btnWidth - 5, 40);
      btn.Tag := i;
      btn.OnClick := PromoButtonClick;
    end;

    if dlg.ShowModal = mrOk then
      Result := arr[promotionSelection]
    else
      Result := arr[0]; // hetman jako domyślna
  finally
    dlg.Free;
  end;
end;



procedure TChess.PromoButtonClick(Sender: TObject);
var
  btn: TButton;
begin
  btn := Sender as TButton;
  promotionSelection := btn.Tag;
  (btn.Parent as TForm).ModalResult := mrOk;
end;





function TChess.IsSquareAttacked(r, c: Integer; const byColor: string): Boolean;
var
  i, idx: Integer;
  dr, dc, nr, nc: Integer;
  attacker: TPiece;
begin
  Result := False;



  if byColor = 'WHITE' then
  begin
    dr := +1;  // Białe piony atakują w dół (rosnący indeks rzędu)
    for i := 0 to 1 do
    begin
      dc := IfThen(i = 0, -1, 1);
      nr := r + dr;  nc := c + dc;
      if InRange(nr, 0, 7) and InRange(nc, 0, 7) and
         (BoardState[nr, nc] = ptWPawn) then
        Exit(True);
    end;
  end
  else
  begin
    dr := -1;  // Czarne piony atakują w górę (malejący indeks rzędu)
    for i := 0 to 1 do
    begin
      dc := IfThen(i = 0, -1, 1);
      nr := r + dr;  nc := c + dc;
      if InRange(nr, 0, 7) and InRange(nc, 0, 7) and
         (BoardState[nr, nc] = ptBPawn) then
        Exit(True);
    end;
  end;



  // 2) Ataki skoczkiem
  for i := 0 to High(KnightOffsets) do
  begin
    nr := r + KnightOffsets[i].Y;
    nc := c + KnightOffsets[i].X;
    if InRange(nr, 0, 7) and InRange(nc, 0, 7) then
    begin
      attacker := BoardState[nr, nc];
      if ((byColor = 'WHITE') and (attacker = ptWKnight)) or
         ((byColor = 'BLACK') and (attacker = ptBKnight)) then
        Exit(True);
    end;
  end;

  // 3) Ataki liniowe – wieża i hetman
  for idx := 0 to High(RookDirs) do
  begin
    dr := RookDirs[idx].Y;
    dc := RookDirs[idx].X;
    nr := r + dr;
    nc := c + dc;
    while InRange(nr, 0, 7) and InRange(nc, 0, 7) do
    begin
      attacker := BoardState[nr, nc];
      if attacker <> ptNone then
      begin
        if ((byColor = 'WHITE') and ((attacker = ptWRook) or (attacker = ptWQueen))) or
           ((byColor = 'BLACK') and ((attacker = ptBRook) or (attacker = ptBQueen))) then
          Exit(True)
        else
          Break;
      end;
      Inc(nr, dr);
      Inc(nc, dc);
    end;
  end;

  // 4) Ataki diagonalne – gońce i hetman
  for idx := 0 to High(BishopDirs) do
  begin
    dr := BishopDirs[idx].Y;
    dc := BishopDirs[idx].X;
    nr := r + dr;
    nc := c + dc;
    while InRange(nr, 0, 7) and InRange(nc, 0, 7) do
    begin
      attacker := BoardState[nr, nc];
      if attacker <> ptNone then
      begin
        if ((byColor = 'WHITE') and ((attacker = ptWBishop) or (attacker = ptWQueen))) or
           ((byColor = 'BLACK') and ((attacker = ptBBishop) or (attacker = ptBQueen))) then
          Exit(True)
        else
          Break;
      end;
      Inc(nr, dr);
      Inc(nc, dc);
    end;
  end;

  // 5) Ataki od króla (sąsiednie pola)
  for dr := -1 to 1 do
    for dc := -1 to 1 do
      if not ((dr = 0) and (dc = 0)) then
      begin
        nr := r + dr;
        nc := c + dc;
        if InRange(nr, 0, 7) and InRange(nc, 0, 7) then
        begin
          attacker := BoardState[nr, nc];
          if ((byColor = 'WHITE') and (attacker = ptWKing)) or
             ((byColor = 'BLACK') and (attacker = ptBKing)) then
            Exit(True);
        end;
      end;
end;


function TChess.OpponentColor: string;
begin
  if MyColor='WHITE' then Result:='BLACK' else Result:='WHITE';
end;

function TChess.OppositeColor(const color: string): string;
begin
  if color = 'WHITE' then
    Result := 'BLACK'
  else
    Result := 'WHITE';
end;





function TChess.IsLegalMove(srcRow, srcCol, dstRow, dstCol: Integer): Boolean;
var
  savedSrc, savedDst: TPiece;
  moveColor: string;
begin
  // 1) Ruch musi być poprawny wg zasad danej figury
  if not IsValidMove(srcRow, srcCol, dstRow, dstCol) then
    Exit(False);

  // 2) Ustalamy, czy to ruch Białych czy Czarnych
  if Ord(BoardState[srcRow, srcCol]) in [Ord(ptWPawn)..Ord(ptWKing)] then
    moveColor := 'WHITE'
  else
    moveColor := 'BLACK';

  // 3) Symulujemy ruch
  savedSrc := BoardState[srcRow, srcCol];
  savedDst := BoardState[dstRow, dstCol];
  BoardState[srcRow, srcCol] := ptNone;
  BoardState[dstRow, dstCol] := savedSrc;

  // 4) Sprawdzamy tylko: czy PO ruchu król tej strony jest w szachu?
  Result := not IsInCheck(moveColor);

  // 5) Przywracamy planszę
  BoardState[srcRow, srcCol] := savedSrc;
  BoardState[dstRow, dstCol] := savedDst;

end;



function TChess.IsInCheck(color: string): Boolean;
var
  kr, kc, i, j: Integer;
begin
  // 1) Znajdź króla danego koloru
  kr := -1; kc := -1;
  for i := 0 to 7 do
    for j := 0 to 7 do
      if ((color = 'WHITE') and (BoardState[i,j] = ptWKing))
      or  ((color = 'BLACK') and (BoardState[i,j] = ptBKing)) then
      begin
        kr := i; kc := j;
        Break;
      end;

  if (kr < 0) or (kc < 0) then
    Exit(False);

  // 2) Sprawdź, czy ten król jest atakowany przez figury przeciwnego koloru
  Result := IsSquareAttacked(kr, kc, OppositeColor(color));
end;



function TChess.HasAnyLegalMove(color: string): Boolean;
var
  sr, sc, tr, tc: Integer;
  foundAny: Boolean;
  buf: string;
begin
  foundAny := False;
  buf := 'Possible moves for ' + color + ':' + sLineBreak;

  for sr := 0 to 7 do
    for sc := 0 to 7 do
      if ((color = 'WHITE') and (Ord(BoardState[sr,sc]) in [Ord(ptWPawn)..Ord(ptWKing)]))
      or  ((color = 'BLACK') and (Ord(BoardState[sr,sc]) in [Ord(ptBPawn)..Ord(ptBKing)])) then
      begin
        for tr := 0 to 7 do
          for tc := 0 to 7 do
            if IsLegalMove(sr, sc, tr, tc) then
            begin
              foundAny := True;
              buf := buf + Format('  %d,%d -> %d,%d', [sr, sc, tr, tc]) + sLineBreak;
            end;
      end;

  if foundAny then
  begin
    Result := True;
  end
  else
    Result := False;
end;

procedure TChess.CheckEndGame;
var
  opp: string;
  inCheck, anyMove: Boolean;
begin

  if GameType = 4 then
  begin

  opp:= MyColor;
  inCheck:= IsInCheck(opp);
  anyMove:= HasAnyLegalMove(opp);


  if InsufficientMaterial then
  begin
  ShowMessage('Remis – brak materiału do mata');
  DisableBoard;
  Draw := True;
  BtnSavePGN.Visible:= true;
  Exit;
  end;



  if inCheck and not anyMove then

  begin
  ShowMessage('Przegrałeś przez mata!');
  Disableboard;
  Lost:=true;
  BtnSavePGN.Visible:= true;
  Exit;
  end;


  if not inCheck and not anyMove then
  begin
  ShowMessage('Remis poprzez pata');
  Draw:=True;
  Disableboard;
  BtnSavePGN.Visible:= true;
  Exit;
  end;


  end;

  opp := OpponentColor;
  inCheck := IsInCheck(opp);
  anyMove := HasAnyLegalMove(opp);


  if InsufficientMaterial then
  begin
  SendMove('ENDGAME:DRAW');
  ShowMessage('Remis – brak materiału do mata');
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;
  DisableBoard;
  Draw := True;
   BtnSavePGN.Visible:= true;
   BtnDraw.Visible:=false;
   BtnResign.Visible:=false;
  Exit;
  end;

  if inCheck and not anyMove then
  begin
    SendMove('ENDGAME:WIN');
    ShowMessage('Mat! przeciwnik nie ma ruchu.');
    tmrWhite.Enabled := False;
    tmrBlack.Enabled := False;
    DisableBoard;
    Win:=true;
     BtnSavePGN.Visible:= true;
     BtnDraw.Visible:=false;
     BtnResign.Visible:=false;
    Exit;
  end;

  if not inCheck and not anyMove then
  begin
    SendMove('ENDGAME:DRAW');
    ShowMessage('Pat!');
    tmrWhite.Enabled := False;
    tmrBlack.Enabled := False;
    DisableBoard;
    Draw:=true;
    BtnSavePGN.Visible:= true;
    BtnDraw.Visible:=false;
    BtnResign.Visible:=false;
    Exit;
  end;




end;






procedure TChess.UpdateTimers;
begin
  tmrWhite.Enabled := False;
  tmrBlack.Enabled := False;


  if Win or LOST or DRAW then
  begin
  EXIT;
  end;


  if IsMyTurn then
  begin
    if MyColor = 'WHITE' then
      tmrWhite.Enabled := True
    else
      tmrBlack.Enabled := True;
  end
  else
  begin
    if MyColor = 'WHITE' then
      tmrBlack.Enabled := True
    else
      tmrWhite.Enabled := True;
  end;
end;


procedure TChess.DisableBoard;
var
  r, c: Integer;
begin
  for r := 0 to 7 do
    for c := 0 to 7 do
      BoardPanels[r, c].Enabled := False;
end;


procedure TChess.SetupCoordinatesLeftAndBottom;
const
  Letters: array[1..8] of string = ('A','B','C','D','E','F','G','H');
  MarginBelowBoard = 5;   // przerwa tuż pod planszą
var
  i, cellSize, coordRowHeight, x, y: Integer;
  lbl: TLabel;
begin
  // 1) Usuń stare etykiety z Tag=1234
  for i := ComponentCount - 1 downto 0 do
    if (Components[i] is TLabel) and (TLabel(Components[i]).Tag = 1234) then
      FreeAndNil(Components[i]);

  cellSize       := Panel.Width div 8;
  coordRowHeight := cellSize div 4;






  if MyColor = 'BLACK' then
  begin



  for i := 1 to 8 do
  begin
    lbl := TLabel.Create(Self);
    lbl.Parent    := Self;
    lbl.Tag       := 1234;
    lbl.Caption   := IntToStr(i);
    lbl.Alignment := taCenter;
    lbl.Font.Size := Max(8, cellSize div 6);
    lbl.SetBounds(
      Panel.Left - cellSize div 4,
      Panel.Top + (i-1)*cellSize,
      cellSize div 4,
      cellSize
    );
  end;


  // 3) litery A–H tuż pod planszą, nad zegarem
  y := Panel.Top + Panel.Height + MarginBelowBoard;
  x := Panel.Left;
  for i := 1 to 8 do
  begin
    lbl := TLabel.Create(Self);
    lbl.Parent    := Self;
    lbl.Tag       := 1234;
    lbl.Caption   := Letters[9-i];
    lbl.Alignment := taCenter;
    lbl.Font.Size := Max(8, cellSize div 6);
    lbl.SetBounds(
      x + (i-1)*cellSize,  // kolejne kolumny
      y,                   // wspólna wartość Y
      cellSize,
      coordRowHeight
    );
  end;






  end




  else

  begin



  for i := 1 to 8 do
  begin
    lbl := TLabel.Create(Self);
    lbl.Parent    := Self;
    lbl.Tag       := 1234;
    lbl.Caption   := IntToStr(9 - i);
    lbl.Alignment := taCenter;
    lbl.Font.Size := Max(8, cellSize div 6);
    lbl.SetBounds(
      Panel.Left - cellSize div 4,
      Panel.Top + (i-1)*cellSize,
      cellSize div 4,
      cellSize
    );
  end;


  // 3) litery A–H tuż pod planszą, nad zegarem
  y := Panel.Top + Panel.Height + MarginBelowBoard;
  x := Panel.Left;
  for i := 1 to 8 do
  begin
    lbl := TLabel.Create(Self);
    lbl.Parent    := Self;
    lbl.Tag       := 1234;
    lbl.Caption   := Letters[i];
    lbl.Alignment := taCenter;
    lbl.Font.Size := Max(8, cellSize div 6);
    lbl.SetBounds(
      x + (i-1)*cellSize,  // kolejne kolumny
      y,                   // wspólna wartość Y
      cellSize,
      coordRowHeight
    );
  end;





  end;















end;




procedure TChess.UpdateClockLabelsPosition;
const
  MarginTop    = 5;  // odstęp od planszy
  MarginBottom = 5;  // odstęp od koordynatów
var
  cellSize, coordRowHeight: Integer;
begin
  cellSize        := Panel.Width div 8;
  coordRowHeight  := cellSize div 4;  // zgodnie z SetupCoordinates



  if MyColor = 'BLACK' then
  begin

    // — Biały zegar — bez zmian —
  lblWhiteTime.Font.Size := Max(14, cellSize div 4);
  lblWhiteTime.SetBounds(
    Panel.Left + Panel.Width - lblWhiteTime.Width,
    Panel.Top  - lblWhiteTime.Height - MarginTop,
    lblWhiteTime.Width,
    lblWhiteTime.Height
  );
  lblWhiteTime.Alignment := taRightJustify;
  lblWhiteTime.BringToFront;

  // — Czarny zegar — teraz pod koordynatami —
  lblBlackTime.Font.Size := Max(14, cellSize div 4);
  lblBlackTime.SetBounds(
    Panel.Left + Panel.Width - lblBlackTime.Width,
    // Panel.Top + Panel.Height → dół planszy
    // + coordRowHeight       → wysokość rzędu liter
    // + MarginBottom         → odstęp
    Panel.Top  + Panel.Height + coordRowHeight + MarginBottom,
    lblBlackTime.Width,
    lblBlackTime.Height
  );
  lblBlackTime.Alignment := taRightJustify;
  lblBlackTime.BringToFront;


  end

  else
  begin

    // — Biały zegar — bez zmian —
  lblBlackTime.Font.Size := Max(14, cellSize div 4);
  lblBlackTime.SetBounds(
    Panel.Left + Panel.Width - lblBlackTime.Width,
    Panel.Top  - lblWhiteTime.Height - MarginTop,
    lblBlackTime.Width,
    lblBlackTime.Height
  );
  lblBlackTime.Alignment := taRightJustify;
  lblBlackTime.BringToFront;

  // — Czarny zegar — teraz pod koordynatami —
  lblWhiteTime.Font.Size := Max(14, cellSize div 4);
  lblWhiteTime.SetBounds(
    Panel.Left + Panel.Width - lblWhiteTime.Width,
    // Panel.Top + Panel.Height → dół planszy
    // + coordRowHeight       → wysokość rzędu liter
    // + MarginBottom         → odstęp
    Panel.Top  + Panel.Height + coordRowHeight + MarginBottom,
    lblWhiteTime.Width,
    lblWhiteTime.Height
  );
  lblWhiteTime.Alignment := taRightJustify;
  lblWhiteTime.BringToFront;




  end;



end;



procedure TChess.UpdateOpponentLabelsPosition;
const
  OffsetY = 5;
  SpacingX = 20;
begin


  if GameType = 4  then
  begin

  OppLogin.Top    := Panel.Top - OppLogin.Height - OffsetY;
  OppLogin.Left   := Panel.Left;
  OppLogin.Font.Size:= 18;
  OppLogin.BringToFront;


  end



  else
  begin

    // OppLogin/OppRanking nad planszą
  OppLogin.Top    := Panel.Top - OppLogin.Height - OffsetY;
  OppLogin.Left   := Panel.Left;
  OppRanking.Top  := OppLogin.Top;
  OppRanking.Left := OppLogin.Left + OppLogin.Width + SpacingX;
  OppRanking.Font.Size:= 14;
  OppLogin.Font.Size:= 18;


  OppLogin.BringToFront;
  OppRanking.BringToFront;


  end;


end;


procedure TChess.UpdateYourLabelsPosition;
const
  SpacingX = 20;
  LowerLabelsOffset = 20; // już zadeklarowane wyżej
var
  coordRowHeight: Integer;
begin

  if GameType = 4  then
  begin
  YourLogin.Font.Size   := 18;   // proporcjonalne
  YourLogin.Top  := Panel.Top + Panel.Height + coordRowHeight + LowerLabelsOffset;
  YourLogin.Left := Panel.Left;
  YourLogin.BringToFront;

  end



  else
  begin
    // Obliczamy wysokość rzędu z literami (koordynatami)
  coordRowHeight := (Panel.Width div 8) div 4;
  // bo w SetupCoordinates robiliśmy: cellSize div 4 dla wysokości liter

  // Twoje labele -- poniżej planszy + koordynaty + dodatkowy offset
  YourLogin.Font.Size   := 18;   // proporcjonalne
  YourRanking.Font.Size := 14;
  YourLogin.Top  := Panel.Top + Panel.Height + coordRowHeight + LowerLabelsOffset;
  YourLogin.Left := Panel.Left;

  YourRanking.Top  := YourLogin.Top;
  YourRanking.Left := YourLogin.Left + YourLogin.Width + SpacingX;

  YourLogin.BringToFront;
  YourRanking.BringToFront;
  end;


end;




procedure TChess.CreateChatControls;
begin
  // 1) Panel chatu
  pnlChat := TPanel.Create(Self);
  pnlChat.Parent := Self;
  pnlChat.BevelOuter := bvNone;
  // Pozycję i rozmiar ustawimy w FormResize
  pnlChat.Align := alNone;

  // 2) Memo historii
  memChat := TMemo.Create(Self);
  memChat.Parent := pnlChat;
  memChat.Align := alClient;
  memChat.ReadOnly := True;
  memChat.ScrollBars := ssVertical;

  // 3) Edit pola wejścia
  edtChat := TEdit.Create(Self);
  edtChat.Parent := pnlChat;
  edtChat.Align := alBottom;
  edtChat.Height := 24;
  edtChat.OnKeyPress := edtChatKeyPress;

  // 4) Przycisk Wyślij
  btnSend := TButton.Create(Self);
  btnSend.Parent := pnlChat;
  btnSend.Align := alRight;
  btnSend.Caption := 'Wyślij';
  btnSend.Width := 60;
  btnSend.OnClick := btnSendClick;
end;


procedure TChess.btnSendClick(Sender: TObject);
var
  text: string;
begin
  text := Trim(edtChat.Text);
  if text = '' then Exit;
  if UserSession.IdTCPClient1.Connected then
    UserSession.IdTCPClient1.IOHandler.WriteLn('CHAT:' + text);
  edtChat.Clear;
  edtChat.SetFocus;
end;

procedure TChess.edtChatKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnSendClick(Sender);
  end;
end;


procedure TChess.CreateMovesList;
begin
  MovesList := TStringList.Create;

  lstMoves := TListBox.Create(Self);
  lstMoves.Parent      := Self;
  lstMoves.Align       := alNone;   // ustawimy w FormResize
  lstMoves.ItemHeight  := 24;
  lstMoves.Font.Size   := 16;
end;


procedure TChess.CreateButtons;
begin

  btnSavePGN := TButton.Create(Self);
  btnSavePGN.Parent := Self;
  btnSavePGN.Caption := 'Save PGN';
  btnSavePGN.Width := 80;
  btnSavePGN.Height := 30;
  btnSavePGN.Visible:= false;
  btnSavePGN.OnClick := btnSavePGNClick;

  btnResign := TButton.Create(Self);
  btnResign.Parent := Self;
  btnResign.Caption := 'Resign';
  btnResign.Width := 80;
  btnResign.Height := 30;
  btnResign.onClick:= btnResignClick;
  btnResign.Visible:= false;

  btnDraw := TButton.Create(Self);
  btnDraw.Parent := Self;
  btnDraw.Caption := 'Draw';
  btnDraw.Width := 80;
  btnDraw.Height := 30;
  btnDraw.Visible:= false;
  btnDraw.OnClick:= btnDrawClick;


  if GameType = 4 then
  begin

  btnResign.Left := lstMoves.Left;
  btnResign.Top := lstMoves.Top + lstMoves.Height + 10;


  btnSavePGN.Left := btnResign.Left + btnResign.Width;
  btnSavePGN.Top := btnResign.Top;
  end
  else
  begin


  btnResign.Left := edtChat.Left;
  btnResign.Top := edtChat.Top + edtChat.Height + 10;


  btnDraw.Left := btnResign.Left + btnResign.Width + 10;
  btnDraw.Top := btnResign.Top;


  btnSavePGN.Left := btnDraw.Left + btnDraw.Width + 10;
  btnSavePGN.Top := btnDraw.Top;

  end;







end;


procedure TChess.AddMoveToList(const Move: string);
var
  idx: Integer;
  line: string;
begin
  idx := MovesList.Count;
  MovesList.Add(Move);

  if (idx mod 2) = 0 then
  begin
    // pierwszy ruch pary (biały) ⇒ nowa linia "1. E2-E4"
    line := Format('%d. %s', [idx div 2 + 1, Move]);
    lstMoves.Items.Add(line);
  end
  else
  begin
    // drugi ruch pary (czarny) ⇒ dopisz do ostatniej linii
    line := lstMoves.Items[lstMoves.Items.Count-1] +
            '   ' + Move;
    lstMoves.Items[lstMoves.Items.Count-1] := line;
  end;

  // przewiń do dołu
  lstMoves.ItemIndex := lstMoves.Items.Count - 1;
end;



function TChess.CanReach(const From, To2: TPoint; Piece: TPiece): Boolean;
begin
  // zakładam, że masz metodę IsLegalMove(srcRow, srcCol, dstRow, dstCol)
  Result := IsLegalMove(From.Y, From.X, To2.Y, To2.X);
end;






function TChess.MoveToSAN(
  Piece: TPiece;
  const Src, Dst: TPoint;
  Capture: Boolean;
  Promotion: TPiece
): string;
var
  prefix, action, dest, disambig: string;
  isPawn: Boolean;
  r, c: Integer;
  needFile: Boolean;
  candidates: TList<TPoint>;
begin
  // 0) remis
  if Draw then
    Exit('½-½');

  // 1) roszada
  if Piece in [ptWKing, ptBKing] then
  begin
    if (Src.X = 4) and (Dst.X = 6) then Exit('O-O');
    if (Src.X = 4) and (Dst.X = 2) then Exit('O-O-O');
  end;

  // 2) figura (prefix)
  isPawn := Piece in [ptWPawn, ptBPawn];
  case Piece of
    ptWPawn, ptBPawn: prefix := '';
    ptWKing, ptBKing: prefix := 'K';
    ptWQueen, ptBQueen: prefix := 'Q';
    ptWRook, ptBRook: prefix := 'R';
    ptWBishop, ptBBishop: prefix := 'B';
    ptWKnight, ptBKnight: prefix := 'N';
  end;


  disambig := '';
if prefix <> '' then
begin
   candidates := TList<TPoint>.Create;
  try
    // dodaj oryginalną pozycję
    candidates.Add(Src);

    // znajdź inne figury tego samego typu
    for  r := 0 to 7 do
      for  c := 0 to 7 do
        if (r <> Src.Y) or (c <> Src.X) then
          if BoardState[r, c] = Piece then
            // używamy Twojej CanReach (czyli IsLegalMove)
            if CanReach(Point(c, r), Dst, Piece) then
              candidates.Add(Point(c, r));

    if candidates.Count > 1 then
    begin
      // czy wskazać plikę (kolumnę)?
       needFile := False;
      for var i := 0 to candidates.Count - 1 do
        if candidates[i].X <> Src.X then
        begin
          needFile := True;
          Break;
        end;

      if needFile then
        disambig := Chr(Ord('a') + Src.X)
      else
        disambig := IntToStr(8 - Src.Y);
    end;
  finally
    candidates.Free;
  end;
end;




  // 5) bicie
  if Capture then
    if isPawn then
      action := Chr(Ord('a') + Src.X) + 'x'
    else
      action := 'x'
  else
    action := '';

  // 6) dest
  dest := Chr(Ord('a') + Dst.X) + IntToStr(8 - Dst.Y);

  // 8) składanie
  Result := prefix + disambig + action + dest;


end;






function TChess.SanHelper(Promotion: TPiece): string;
var
isCheck, isMate: Boolean;
promoPart: string;

begin

 // 7) promocja




  case Promotion of
    ptWQueen, ptBQueen:   promoPart := '=Q';
    ptWRook, ptBRook:     promoPart := '=R';
    ptWBishop, ptBBishop: promoPart := '=B';
    ptWKnight, ptBKnight: promoPart := '=N';
  else
    promoPart := '';
  end;

  // 8) składanie
  Result :=  promoPart;

  // 9) szach/mat
  isCheck := IsInCheck(OpponentColor);
  isMate  := isCheck and not HasAnyLegalMove(OpponentColor);

  if isMate then
    Result := Result + '#'
  else if isCheck then
    Result := Result + '+';

  If (Gametype = 4) and not IsMyTurn then
  begin
  isCheck := IsInCheck(MyColor);
  isMate := IsCheck and not HasAnyLegalMove(Mycolor);

  if isMate then
  Result := Result + '#'
  else if isCheck then
  Result := Result + '+';



  end;



end;








function TChess.CodeToPiece(const promoCode: string): TPiece;
begin
  if promoCode = 'QW' then Exit(ptWQueen)
  else if promoCode = 'RW' then Exit(ptWRook)
  else if promoCode = 'BW' then Exit(ptWBishop)
  else if promoCode = 'KW' then Exit(ptWKnight)
  else if promoCode = 'QB' then Exit(ptBQueen)
  else if promoCode = 'RB' then Exit(ptBRook)
  else if promoCode = 'BB' then Exit(ptBBishop)
  else if promoCode = 'KB' then Exit(ptBKnight)
  else Exit(ptNone);
end;





function TChess.InsufficientMaterial: Boolean;
var
  r, c: Integer;
  piece: TPiece;
  others: array of TPiece;
begin
  SetLength(others, 0);

  for r := 0 to 7 do
    for c := 0 to 7 do
    begin
      piece := BoardState[r, c];
      if (piece <> ptNone) and (piece <> ptWKing) and (piece <> ptBKing) then
      begin
        SetLength(others, Length(others) + 1);
        others[High(others)] := piece;
      end;
    end;

  // brak innych figur niż króle
  if Length(others) = 0 then
    Exit(True);

  // tylko jedna lekka figura
  if (Length(others) = 1) and
     (others[0] in [ptWBishop, ptBBishop, ptWKnight, ptBKnight]) then
    Exit(True);

  // TODO: opcjonalnie obsłuż przypadek goniec vs goniec na tym samym kolorze

  Result := False;
end;


procedure TChess.btnSavePGNClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  PGNText: string;
  PGNFile: TextFile;
begin
  SaveDlg := TSaveDialog.Create(Self);
  try
    SaveDlg.Title := 'Zapisz partię jako PGN';
    SaveDlg.Filter := 'PGN files (*.pgn)|*.pgn|All files (*.*)|*.*';
    SaveDlg.DefaultExt := 'pgn';

    if SaveDlg.Execute then
    begin
      PGNText := MoveListToPGN; // ← generujesz PGN ze swojej listy ruchów

      AssignFile(PGNFile, SaveDlg.FileName);
      Rewrite(PGNFile);
      Write(PGNFile, PGNText);
      CloseFile(PGNFile);

      ShowMessage('Partia zapisana do ' + SaveDlg.FileName);
    end;
  finally
    SaveDlg.Free;
  end;
end;



function TChess.MoveListToPGN: string;
var
  i: Integer;
  ResultTag: string;
begin
  Result := '';

  // Nagłówki PGN
  Result := Result + '[Event "Chess Game"]' + sLineBreak;
  Result := Result + '[Site "Local"]' + sLineBreak;
  Result := Result + '[Date "' + FormatDateTime('yyyy.MM.dd', Now) + '"]' + sLineBreak;
  Result := Result + '[Round "-"]' + sLineBreak;


  if Gametype = 4 then
  begin

  if MyColor = 'WHITE' then
  begin
    Result := Result + '[White "' + UserSession.LoggedUserLogin + '"]' + sLineBreak;
    Result := Result + '[Black "Stockfish"]' + sLineBreak;
  end
  else
  begin
    Result := Result + '[White "Stockfish"]' + sLineBreak;
    Result := Result + '[Black "' + UserSession.LoggedUserLogin + '"]' + sLineBreak;
  end;
  end
  else
  begin

   if MyColor = 'WHITE' then
  begin
    Result := Result + '[White "' + UserSession.LoggedUserLogin + '"]' + sLineBreak;
    Result := Result + '[Black "' + OpponentName + '"]' + sLineBreak;
  end
  else
  begin
    Result := Result + '[White "' + OpponentName + '"]' + sLineBreak;
    Result := Result + '[Black "' + UserSession.LoggedUserLogin + '"]' + sLineBreak;
  end;






  end;




  // Wynik w tagu
  if Win and (MyColor = 'WHITE') then
    ResultTag := '1-0'
  else if Win and (MyColor = 'BLACK') then
    ResultTag := '0-1'
  else if Draw then
    ResultTag := '1/2-1/2';


  Result := Result + '[Result "' + ResultTag + '"]' + sLineBreak + sLineBreak;

  // Ruchy partii
  for i := 0 to MovesList.Count - 1 do
  begin
    if i mod 2 = 0 then
      Result := Result + IntToStr(i div 2 + 1) + '. ' + MovesList[i] + ' '
    else
      Result := Result + MovesList[i] + ' ';
  end;

  // Wynik na końcu
  Result := Result + ResultTag;
end;


procedure TChess.btnResignClick(Sender: TObject);
begin
SendMove('ENDGAME:LOSE');
DisableBoard;
tmrWhite.Enabled := False;
tmrBlack.Enabled := False;
InGame:=false;
btnSavePGN.Visible:=true;
btnResign.Visible:=false;
btnDraw.Visible:=false;
end;



procedure TChess.btnDrawClick(Sender: TObject);
begin
SendMove('DRAW:OFFER');
BtnDraw.Visible:=false;
end;





end.
