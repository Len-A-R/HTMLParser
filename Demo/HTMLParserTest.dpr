program HTMLParserTest;

uses
  Vcl.Forms,
  uTest in 'uTest.pas' {Form1},
  HTMLParser in '..\HTMLParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
