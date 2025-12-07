unit main;

{$ifdef FPC}
{$mode Delphi}{$H+}
{$endif}

interface

uses
  {$ifdef MSWindows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  usbbase;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnEnableUSB: TButton;
    Memo1: TMemo;
    procedure btnEnableUSBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure UpdateUSB(Sender: TObject;datacarrier:integer);
  public
    UsbCounter       : TDataDevice;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  UsbCounter:=TDataDevice.Create(Application.ExeName);
  UsbCounter.OnDeviceChange:=UpdateUSB;
end;

procedure TForm1.btnEnableUSBClick(Sender: TObject);
begin
  if (NOT UsbCounter.Enabled) then UsbCounter.Enabled:=True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(UsbCounter);
end;

procedure TForm1.UpdateUSB(Sender: TObject;datacarrier:integer);
begin
  if datacarrier>0 then
  begin
    Memo1.Lines.Append('Data acquisitionboard '+InttoStr(datacarrier)+' added');
  end
  else
  if datacarrier<0 then
  begin
    Memo1.Lines.Append('Data acquisitionboard '+InttoStr(-1*datacarrier)+' removed');
  end
  else
  begin
    Memo1.Lines.Append('Unknown data acquisitionboard error');
  end;
end;

end.

