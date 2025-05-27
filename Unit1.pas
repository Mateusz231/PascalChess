unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var

liczba1, liczba2, wynik : Double;

begin

liczba1:= StrToFloat(Edit1.Text);
liczba2:= StrToFloat(Edit2.Text);

wynik:= liczba1+liczba2;

Edit3.Text:= FloatToStr(wynik);


end;







procedure TForm1.Button2Click(Sender: TObject);


begin


Edit3.Text:= FloatToStr(StrToFloat(Edit1.Text)-StrToFloat(Edit2.Text));


end;

procedure TForm1.Button3Click(Sender: TObject);

var liczba1,liczba2,wynik : Double;

begin

liczba1:= StrToFloat(Edit1.Text);
liczba2:= StrToFloat(Edit2.Text);


if liczba2 = 0 then
begin
  ShowMessage('Nie mo¿na dzieliæ przez 0');
  Exit;
end
else
begin
  wynik := liczba1 / liczba2;
  Edit3.Text := FloatToStr(wynik);
end;


end;

procedure TForm1.Button4Click(Sender: TObject);
begin


Edit3.Text:= FloatToStr(StrToFloat(Edit1.Text)*StrToFloat(Edit2.Text));

end;

end.
