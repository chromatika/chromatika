program chromatika;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  haldclut in 'haldclut.pas',
  helpers in 'helpers.pas',
  mobile in 'mobile.pas',
  log in 'log.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
