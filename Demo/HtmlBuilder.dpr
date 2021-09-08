program HtmlBuilder;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  HtmlEmail in '..\HtmlEmail.pas',
  HtmlEmail.Elements in '..\HtmlEmail.Elements.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
