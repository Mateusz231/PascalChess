program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;


var
  liczba1, liczba2, wynik: Double;
  operacja: Char;



begin
  Writeln('Prosty kalkulator CLI w Object Pascal');
  Writeln('Dostêpne operacje: +, -, *, /');

  Write('Podaj pierwsz¹ liczbê: ');
  Readln(liczba1);

  Write('Podaj operator (+, -, *, /): ');
  Readln(operacja);

  Write('Podaj drug¹ liczbê: ');
  Readln(liczba2);

  case operacja of
    '+': wynik := liczba1 + liczba2;
    '-': wynik := liczba1 - liczba2;
    '*': wynik := liczba1 * liczba2;
    '/':
      if liczba2 <> 0 then
        wynik := liczba1 / liczba2
      else
      begin
        Writeln('B³¹d: Dzielenie przez zero!');
        Halt(1);  // koñczy program z kodem b³êdu
      end;
  else
    begin
      Writeln('Nieznany operator!');
      Halt(1);
    end;
  end;

  Writeln('Wynik: ', wynik:0:2);
  Readln; // czeka na Enter, ¿eby okno siê nie zamknê³o od razu
end.


