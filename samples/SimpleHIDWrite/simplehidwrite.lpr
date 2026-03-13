program simplehidwrite;

uses
  {$IFNDEF FPC}
  {$ELSE}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF }
  Interfaces,
  {$ENDIF }
  Forms,
  DevReader in 'DevReader.pas' {MainForm};

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
