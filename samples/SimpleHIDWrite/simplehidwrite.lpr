program simplehidwrite;

uses
  {$IFNDEF FPC}
  Forms,
  {$ELSE}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF }
  Forms,
  Interfaces,
  {$ENDIF }
  DevReader;

{$R *.res}

begin
  {$ifdef FPC}
  RequireDerivedFormResource := True;
  {$endif}
  Application.Initialize;
  {$ifdef MSWINDOWS}
  Application.MainFormOnTaskbar{%H-}:=True;
  {$endif}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
