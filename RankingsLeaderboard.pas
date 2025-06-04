unit RankingsLeaderboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  FireDAC.Comp.Client, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, UserSession;

type
  TLeaderboardForm = class(TForm)
    cboGameType: TComboBox;
    GridLeaderboard: TStringGrid;
    BtnPrevPage: TButton;
    BtnNextPage: TButton;
    LblPageInfo: TLabel;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    BackButton: TButton;

    procedure FormCreate(Sender: TObject);
    procedure cboGameTypeChange(Sender: TObject);
    procedure BtnPrevPageClick(Sender: TObject);
    procedure BtnNextPageClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GridLeaderboardDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormResize(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
  private
    FPage: Integer;
    const
      RecordsPerPage = 15;
    procedure LoadLeaderboard;
    function GetSelectedGameTypeID: Integer;
    procedure UpdatePageInfo;
    procedure SetupGrid;
    procedure ResizeGridColumns;
  public
    { Public declarations }
  end;

var
  LeaderboardForm: TLeaderboardForm;

implementation

{$R *.dfm}

procedure TLeaderboardForm.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TLeaderboardForm.FormCreate(Sender: TObject);
begin
    Color := clBlack;
  Font.Color := clWhite;
  Font.Size := 10;

  cboGameType.Parent := Self;
  cboGameType.Style := csDropDownList;
  cboGameType.Color := clBlack;
  cboGameType.Font.Color := clWhite;

  cboGameType.Items.Add('Rapid');
  cboGameType.Items.Add('Blitz');
  cboGameType.Items.Add('Bullet');
  cboGameType.ItemIndex := 0;


  GridLeaderboard.Parent := Self;
  GridLeaderboard.Color := clBlack;
  GridLeaderboard.Font.Color := clWhite;
  GridLeaderboard.FixedColor := clGray;
  GridLeaderboard.Options := GridLeaderboard.Options + [goRowSelect];
  GridLeaderboard.DefaultRowHeight := 28;
  GridLeaderboard.FixedRows := 1;
  GridLeaderboard.OnDrawCell := GridLeaderboardDrawCell;

  BtnPrevPage.Parent := Self;
  BtnPrevPage.Caption := 'Previous';

  BtnNextPage.Parent := Self;
  BtnNextPage.Caption := 'Next';

  LblPageInfo.Parent := Self;
  LblPageInfo.Font.Color := clWhite;
  LblPageInfo.Caption := 'Page: 1';

  FPage := 0;

  SetupGrid;
  LoadLeaderboard;
end;

procedure TLeaderboardForm.SetupGrid;
begin
  GridLeaderboard.RowCount := RecordsPerPage + 1;
  GridLeaderboard.ColCount := 4;
  GridLeaderboard.Cells[0, 0] := 'Lp.';
  GridLeaderboard.Cells[1, 0] := 'Login';
  GridLeaderboard.Cells[2, 0] := 'Country';
  GridLeaderboard.Cells[3, 0] := 'Rating';
end;





procedure TLeaderboardForm.LoadLeaderboard;
var
  Offset: Integer;
  i: Integer;
begin
  Offset := FPage * RecordsPerPage;

  FDQuery1.Connection := FDConnection1;
  FDQuery1.Close;
  FDQuery1.SQL.Text :=
    'SELECT u.login, u.country, r.rating ' +
    'FROM users u ' +
    'JOIN rankings r ON u.userid = r.users_userid ' +
    'WHERE r.game_type_id = :game_type_id ' +
    'ORDER BY r.rating DESC ' +
    'LIMIT :limit OFFSET :offset';

  FDQuery1.ParamByName('game_type_id').AsInteger := GetSelectedGameTypeID;
  FDQuery1.ParamByName('limit').AsInteger := RecordsPerPage;
  FDQuery1.ParamByName('offset').AsInteger := Offset;
  FDQuery1.Open;

  // Jeœli nic nie znaleziono
  if FDQuery1.IsEmpty then
  begin

    if FPage > 0 then
    begin
      Dec(FPage);
      LoadLeaderboard;  // spróbuj jeszcze raz
    end;
    Exit;
  end;

  // Wyczyœæ stare dane
  for i := 1 to GridLeaderboard.RowCount - 1 do
    GridLeaderboard.Rows[i].Clear;

  i := 1;
  while not FDQuery1.Eof do
  begin
    GridLeaderboard.Cells[0, i] := IntToStr(Offset + i);
    GridLeaderboard.Cells[1, i] := FDQuery1.FieldByName('login').AsString;
    GridLeaderboard.Cells[2, i] := FDQuery1.FieldByName('country').AsString;
    GridLeaderboard.Cells[3, i] := FDQuery1.FieldByName('rating').AsString;

    Inc(i);
    FDQuery1.Next;
  end;

  UpdatePageInfo;
end;











procedure TLeaderboardForm.cboGameTypeChange(Sender: TObject);
begin
  FPage := 0;
  LoadLeaderboard;
end;

procedure TLeaderboardForm.BtnPrevPageClick(Sender: TObject);
begin
  if FPage > 0 then
  begin
    Dec(FPage);
    LoadLeaderboard;
  end;
end;

procedure TLeaderboardForm.BackButtonClick(Sender: TObject);
begin
Self.Hide;
Application.MainForm.Show;
end;

procedure TLeaderboardForm.BtnNextPageClick(Sender: TObject);
begin
  Inc(FPage);
  LoadLeaderboard;
end;

procedure TLeaderboardForm.UpdatePageInfo;
begin
  LblPageInfo.Caption := Format('Page: %d', [FPage + 1]);
end;

function TLeaderboardForm.GetSelectedGameTypeID: Integer;
begin
  case cboGameType.ItemIndex of
    0: Result := 1; // Rapid
    1: Result := 2; // Blitz
    2: Result := 3; // Bullet
  else
    Result := 1;
  end;
end;

procedure TLeaderboardForm.GridLeaderboardDrawCell(Sender: TObject; ACol, ARow: Integer;
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
    TextRect(Rect, Rect.Left + 4, Rect.Top + 4, GridLeaderboard.Cells[ACol, ARow]);
  end;
end;

procedure TLeaderboardForm.FormResize(Sender: TObject);
begin
  GridLeaderboard.Left := 20;
  GridLeaderboard.Top := 50;
  GridLeaderboard.Width := ClientWidth - 40;
  GridLeaderboard.DefaultRowHeight := 28;
  GridLeaderboard.RowCount := RecordsPerPage + 1;
  GridLeaderboard.Height := (RecordsPerPage + 1) * GridLeaderboard.DefaultRowHeight;

  cboGameType.Left := 20;
  cboGameType.Top := GridLeaderboard.Top + GridLeaderboard.Height + 20;
  cboGameType.Width := 150;

  BtnPrevPage.Left := 20;
  BtnPrevPage.Top := cboGameType.Top + cboGameType.Height + 20;

  BtnNextPage.Left := BtnPrevPage.Left + BtnPrevPage.Width + 10;
  BtnNextPage.Top := BtnPrevPage.Top;

  BackButton.Top:= BtnNextPage.Top + BtnPrevPage.Height + 20;
  BackButton.Left:= 20;

  LblPageInfo.Left := ClientWidth - 120;
  LblPageInfo.Top := 20;

  ResizeGridColumns;
end;

procedure TLeaderboardForm.ResizeGridColumns;
var
  TotalWidth: Integer;
begin
  if GridLeaderboard.ColCount = 4 then
  begin
    TotalWidth := GridLeaderboard.ClientWidth;

    GridLeaderboard.ColWidths[0] := TotalWidth div 10;  // Lp.
    GridLeaderboard.ColWidths[1] := (TotalWidth * 5) div 10; // Login
    GridLeaderboard.ColWidths[2] := (TotalWidth * 2) div 10; // Country
    GridLeaderboard.ColWidths[3] := (TotalWidth * 2) div 10; // Rating
  end;
end;

end.
