program FpcUsbHid;

{$IFDEF LINUX}
  {$IFDEF FPC_CROSSCOMPILING}
    {$linklib libc_nonshared.a}
  {$ENDIF}
{$ENDIF}

{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, Main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

