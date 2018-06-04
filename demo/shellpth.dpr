
(********************************************************)
(*                                                      *)
(*  Codebot Shell Controls Demo                         *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

program shellpth;

uses
  Forms,
  Main in 'Main.pas' {ShellDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Shell Path';
  Application.CreateForm(TShellDemoForm, ShellDemoForm);
  Application.Run;
end.
