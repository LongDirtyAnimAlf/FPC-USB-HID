unit Main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  usb;

type

  { TForm1 }

  TMyUSB = class(TUSB)
  public
    function CheckVendorProduct(const VID,PID:word):boolean;override;
  end;

  TForm1 = class(TForm)
    btnHIDCreate: TButton;
    btnHIDEnable: TButton;
    btnInfo: TButton;
    Memo1: TMemo;
    procedure btnHIDCreateClick(Sender: TObject);
    procedure btnHIDEnableClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    NewUSB:TUSB;
    procedure UpdateUSBDevice(Sender: TObject;datacarrier:integer);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function TMyUSB.CheckVendorProduct(const VID,PID:word):boolean;
begin
  // Allow all devices for testing !!
  result:=true;
end;

{ TForm1 }

procedure TForm1.btnHIDCreateClick(Sender: TObject);
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Created.');
  NewUSB:=TMyUSB.Create;
  Memo1.Lines.Append('Ready.');
  btnHIDEnable.Enabled:=True;
end;

procedure TForm1.btnHIDEnableClick(Sender: TObject);
var
  S:string;
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Enabled.');
  NewUSB.OnUSBDeviceChange:=UpdateUSBDevice;
  NewUSB.Enabled:=True;
  Memo1.Lines.Append('Ready.');
  btnInfo.Enabled:=True;
end;

procedure TForm1.btnInfoClick(Sender: TObject);
var
  S:string;
begin
  S:=NewUSB.Info;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('INFO:');
    Memo1.Lines.Append(S);
  end else Memo1.Lines.Append('No new USB info.');

  S:=NewUSB.Errors;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('ERRORS:');
    Memo1.Lines.Append(S);
  end else Memo1.Lines.Append('No new USB errors.');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(NewUSB) then
  begin
    NewUSB.Destroy;
  end;
end;

procedure TForm1.UpdateUSBDevice(Sender: TObject;datacarrier:integer);
var
  LocalDevice:TUSBController;
  DeviceIndex:integer;
begin
  Memo1.Lines.Append('***************');
  Memo1.Lines.Append('Device change !');

  DeviceIndex:=Abs(datacarrier);
  LocalDevice:=TUSBController(NewUSB.USBList[DeviceIndex]);
  if (datacarrier>0) then
  begin
    with LocalDevice.HidCtrl do
    begin
      Memo1.Lines.Append('Found correct HID device.');
      Memo1.Lines.Append('HID device index: '+InttoStr(DeviceIndex));
      Memo1.Lines.Append('VID: '+InttoHex(Attributes.VendorID,4)+'. PID: '+InttoHex(Attributes.ProductID,4)+'.');
      Memo1.Lines.Append('Name: '+ProductName+'. Vendor: '+VendorName+'.');
      Memo1.Lines.Append('Serial: '+SerialNumber+'.');
      Memo1.Lines.Append('Length output report: '+InttoStr(Caps.OutputReportByteLength)+'.');
      Memo1.Lines.Append('Length input report: '+InttoStr(Caps.InputReportByteLength)+'.');
      Memo1.Lines.Append('DeviceDescription: '+PnPInfo.DeviceDescr+'.');
      Memo1.Lines.Append('Device Path: '+PnPInfo.DevicePath+'.');
      Memo1.Lines.Append('Friendly Name: '+PnPInfo.FriendlyName+'.');
    end;
  end;
  if (datacarrier<0) then
  begin
    if (LocalDevice.BoardNumber=Abs(datacarrier)) then
    begin
      with LocalDevice.HidCtrl do
      begin
        Memo1.Lines.Append('Removed HID device.');
      end;
    end;
  end;
end;

end.

