unit MatchHistory;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  Vcl.StdCtrls, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait;

type
  TGameInfo = class
  public GameId: Integer;
  Opponent: string;
  end;

  TMatchHistoryForm = class(TForm)
    BtnPrevPage: TButton;
    BtnNextPage: TButton;
    LblPageInfo: TLabel;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    BackButton: TButton;
    tsgrid: TStringGrid;

    procedure FormCreate(Sender: TObject);
    procedure BtnPrevPageClick(Sender: TObject);
    procedure BtnNextPageClick(Sender: TObject);
    procedure tsgridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormResize(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tsgridClick(Sender: TObject);
    procedure ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
  private
    FPage: Integer;
    const
      RecordsPerPage = 15;
    procedure LoadMatchHistory;
    procedure UpdatePageInfo;
    procedure SetupGrid;
    procedure ResizeGridColumns;
  public
    { Public declarations }
  end;

var
  MatchHistoryForm: TMatchHistoryForm;

implementation

{$R *.dfm}

Uses UserSession, ChessGame;

procedure TMatchHistoryForm.FormCreate(Sender: TObject);
Begin
 FDPhysMySQLDriverLink1.VendorLib := ExtractFilePath(Application.ExeName) + 'libmysql.dll';
 FDConnection1.Connected := False;
  Color := clBlack;
  Font.Color := clWhite;
  Font.Size := 10;




  tsgrid.Parent := Self;
  tsgrid.Color := clBlack;
  tsgrid.Font.Color := clWhite;
  tsgrid.FixedColor := clGray;
  tsgrid.Options := tsgrid.Options;
  tsgrid.DefaultRowHeight := 28;
  tsgrid.FixedRows := 1;
  tsgrid.OnDrawCell := tsgridDrawCell;

  BtnPrevPage.Parent := Self;
  BtnPrevPage.Caption := 'Previous';

  BtnNextPage.Parent := Self;
  BtnNextPage.Caption := 'Next';

  LblPageInfo.Parent := Self;
  LblPageInfo.Font.Color := clWhite;
  LblPageInfo.Caption := 'Page: 1';

  FPage := 0;

  SetupGrid;
  LoadMatchHistory;
end;

procedure TMatchHistoryForm.SetupGrid;
begin
  tsgrid.RowCount := RecordsPerPage + 1;
  tsgrid.ColCount := 5;

  tsgrid.Cells[0, 0] := 'Lp.';
  tsgrid.Cells[1, 0] := 'Opponent';
  tsgrid.Cells[2, 0] := 'Result';
  tsgrid.Cells[3, 0] := 'Date';
  tsgrid.Cells[4,0] := 'Analyze';
end;

procedure TMatchHistoryForm.LoadMatchHistory;
var
  Offset, i: Integer;
begin
  Offset := FPage * RecordsPerPage;

  FDQuery1.Connection := FDConnection1;
  FDQuery1.Close;
    FDQuery1.SQL.Text :=
    'SELECT ' +
    '  CASE ' +
    '    WHEN :userid = m.whiteplayerid THEN u_black.login ' +
    '    ELSE u_white.login ' +
    '  END AS opponent_login, ' +
    '  m.result, ' +
    '  m.date, ' +
    '  m.gameid,'+
    '  CASE ' +
    '    WHEN :userid = m.whiteplayerid AND m.result = ''white'' THEN ''Win'' ' +
    '    WHEN :userid = m.blackplayerid AND m.result = ''black'' THEN ''Win'' ' +
    '    WHEN m.result = ''draw'' THEN ''Draw'' ' +
    '    ELSE ''Loss'' ' +
    '  END AS outcome ' +
    'FROM games m ' +
    'JOIN users u_white ON m.whiteplayerid = u_white.userid ' +
    'JOIN users u_black ON m.blackplayerid = u_black.userid ' +
    'WHERE :userid IN (m.whiteplayerid, m.blackplayerid) ' +
    'ORDER BY m.date DESC ' +
    'LIMIT :limit OFFSET :offset';
  FDQuery1.ParamByName('userid').AsInteger := UserSession.LoggedUserID;
  FDQuery1.ParamByName('limit').AsInteger := RecordsPerPage;
  FDQuery1.ParamByName('offset').AsInteger := Offset;
  FDQuery1.Open;



  if FDQuery1.IsEmpty then
  begin
    if FPage > 0 then
    begin
      Dec(FPage);
      LoadMatchHistory;
    end;
    Exit;
  end;

  // Czyść stare dane
  for i := 1 to tsgrid.RowCount - 1 do
  begin

  if Assigned(tsgrid.Objects[4, i]) then
  begin
    TGameInfo(tsgrid.Objects[4, i]).Free;
    tsgrid.Objects[4, i] := nil;
  end;
  tsgrid.Rows[i].Clear;
  end;


  i := 1;
  while not FDQuery1.Eof do
  begin
    tsgrid.Cells[0, i] := IntToStr(Offset + i);
    tsgrid.Cells[1, i] := FDQuery1.FieldByName('opponent_login').AsString;
    tsgrid.Cells[2, i] := FDQuery1.FieldByName('outcome').AsString;
    tsgrid.Cells[3, i] := FDQuery1.FieldByName('date').AsString;
   // tsgrid.Objects[4,i]:= TObject(FDQuery1.FieldByName('gameid').AsInteger);
    var GameInfo:=TGameInfo.Create;
    GameInfo.GameId:= FdQuery1.FieldByName('gameid').AsInteger;
    GameInfo.Opponent:= FDQuery1.FieldByName('opponent_login').AsString;
    tsgrid.Objects[4,i]:=GameInfo;
    tsgrid.Cells[4,i]:='Analyze';
    Inc(i);
    FDQuery1.Next;
  end;

  UpdatePageInfo;
end;






procedure TMatchHistoryForm.BtnPrevPageClick(Sender: TObject);
begin
  if FPage > 0 then
  begin
    Dec(FPage);
    LoadMatchHistory;
  end;
end;

procedure TMatchHistoryForm.BtnNextPageClick(Sender: TObject);
begin
  Inc(FPage);
  LoadMatchHistory;
end;

procedure TMatchHistoryForm.UpdatePageInfo;
begin
  LblPageInfo.Caption := Format('Page: %d', [FPage + 1]);
end;


procedure TMatchHistoryForm.ChessCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Application.MainForm.Show;
end;



procedure TMatchHistoryForm.tsgridClick(Sender: TObject);
var
  col, row: Integer;
  GameInfo: TGameInfo;
begin
  col := tsgrid.Col;
  row := tsgrid.Row;



  if (row > 0) and (col = 4) then
  begin
    if Assigned(tsgrid.Objects[col, row]) then
    begin
      GameInfo := TGameInfo(tsgrid.Objects[col, row]);

      // teraz masz oba
     // ShowMessage(Format('GameID=%d, Opponent=%s',
       // [GameInfo.GameID, GameInfo.Opponent]));


        Self.Hide;
        Chess := TChess.Create(nil);
        Chess.SetGameType(5);
        Chess.OnClose := ChessCloseHandler;
        Chess.LoadGameFromDB(GameInfo.GameID, GameInfo.Opponent);
        Chess.Show;


    end;
  end;
end;




procedure TMatchHistoryForm.tsgridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  with (Sender as TStringGrid).Canvas do
  begin
    if ARow = 0 then
    begin
      Brush.Color := clGray;
      Font.Color := clBlack;
    end
    else
    begin
      Brush.Color := clBlack;
      Font.Color := clWhite;
    end;
    FillRect(Rect);
    TextRect(Rect, Rect.Left + 4, Rect.Top + 4, tsgrid.Cells[ACol, ARow]);
  end;
end;

procedure TMatchHistoryForm.FormResize(Sender: TObject);
var
  nonClient: Integer;
begin
  tsgrid.DefaultRowHeight := 28;
  tsgrid.RowCount := RecordsPerPage + 1; // 15 danych + nagłówek

  // oblicz non-client (ramki)
  nonClient := tsgrid.Height - tsgrid.ClientHeight;
  if nonClient < 0 then
    nonClient := 0;

  // ustaw dokładną wysokość: wszystkie wiersze * wysokość + ramka
  tsgrid.Height := (tsgrid.RowCount * tsgrid.DefaultRowHeight) + nonClient;

  // pozycjonowanie reszty
  tsgrid.Left := 20;
  tsgrid.Top := 50;
  tsgrid.Width := ClientWidth - 40;

  BtnPrevPage.Left := 20;
  BtnPrevPage.Top := tsgrid.Top + tsgrid.Height + 20;
  BtnNextPage.Left := BtnPrevPage.Left + BtnPrevPage.Width + 10;
  BtnNextPage.Top := BtnPrevPage.Top;
  BackButton.Top:= BtnNextPage.Top + BtnPrevPage.Height + 20;
  BackButton.Left:= 20;
  LblPageInfo.Left := ClientWidth - 120;
  LblPageInfo.Top := 20;




  ResizeGridColumns;
end;

procedure TMatchHistoryForm.FormShow(Sender: TObject);
begin
  LoadMatchHistory;
  Resize;
end;

procedure TMatchHistoryForm.ResizeGridColumns;
var
  TotalWidth: Integer;
begin
  if tsgrid.ColCount = 5 then
  begin
    TotalWidth := tsgrid.ClientWidth;
    tsgrid.ColWidths[0] := TotalWidth div 10;           // Lp.
    tsgrid.ColWidths[1] := (TotalWidth * 4) div 10;     // Opponent
    tsgrid.ColWidths[2] := (TotalWidth * 2) div 10;     // Result  s
    tsgrid.ColWidths[3] := (TotalWidth * 2) div 10;
    tsgrid.ColWidths[4] := (TotalWidth * 1) div 10;
  end;
end;



procedure TMatchHistoryForm.BackButtonClick(Sender: TObject);
begin
  Self.Hide;
  Application.MainForm.Show;
end;

procedure TMatchHistoryForm.FormClose(Sender: TObject; var Action: TCloseAction);

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

end.
