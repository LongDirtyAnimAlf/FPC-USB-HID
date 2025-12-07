program FpcUsbHidConsole;

{$mode objfpc}{$H+}

uses
  {$ifdef Windows}
  Windows,
  {$endif}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  usb;

const
  ProductVID                       = $0665;
  ProductPID                       = $5161;

type
  TMyUSB = class(TUSB)
  private
    FDevice:TUSBController;
  public
    function CheckVendorProduct(const VID,PID:word):boolean;override;
    procedure UpdateUSBDevice(Sender: TObject;LocalDevice:TUSBController);
    property Device:TUSBController read FDevice;
    constructor Create;
  end;

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    USBDevice          : TMyUSB;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyUSB.Create;
begin
  inherited;
  FDevice:=nil;
end;

function TMyUSB.CheckVendorProduct(const VID,PID:word):boolean;
begin
  //result:=((VID=ProductVID) AND (PID=ProductPID));
  result:=true;
end;

procedure TMyUSB.UpdateUSBDevice(Sender: TObject;LocalDevice:TUSBController);
begin
  writeln('***************');
  writeln('CheckVendorProduct(const VID,PID:word) has returned true.');
  writeln('Device change !');

  with LocalDevice.HidCtrl do
  begin
    if IsPluggedIn then
      writeln('Arrival of correct HID device.')
    else
      writeln('Removal of correct HID device.');
  end;

  (*
  if Length(LocalDevice.ProductSerial)=0 then
    writeln('Removal of correct HID device.')
  else
    writeln('Arrival of correct HID device.');
  *)

  with LocalDevice.HidCtrl do
  begin
    writeln('VID: '+InttoHex(Attributes.VendorID,4)+'. PID: '+InttoHex(Attributes.ProductID,4)+'.');
    writeln('Name: '+ProductName+'. Vendor: '+VendorName+'.');
    writeln('Serial: '+SerialNumber+'.');
    writeln('Length output report: '+InttoStr(Caps.OutputReportByteLength)+'.');
    writeln('Length input report: '+InttoStr(Caps.InputReportByteLength)+'.');
    writeln('DeviceDescription: '+PnPInfo.DeviceDescr+'.');
    writeln('Device Path: '+PnPInfo.DevicePath+'.');
    writeln('Friendly Name: '+PnPInfo.FriendlyName+'.');
  end;
  if (NOT Assigned(FDevice)) then
  begin
    FDevice:=LocalDevice;
  end;
end;

{ TMyApplication }

procedure TMyApplication.DoRun;
{$ifdef Windows}
var
  Msg:TMsg;
{$endif}
begin
  USBDevice.OnUSBDeviceChange:=@USBDevice.UpdateUSBDevice;
  USBDevice.Enabled:=true;

  writeln('Program loop started');

  while true do
  begin
    {$ifdef Windows}
    if PeekMessage({%H-}Msg,0,0,0,0) then
    begin
      GetMessage(Msg,0,0,0);
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
    {$endif}

    if Assigned(USBDevice.Device) then
    begin
      // Do something with the connected device
    end;

    // Give some rest ... ;-)
    sleep(500);
  end;

  writeln('Ready');

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  USBDevice:=TMyUSB.Create;
end;

destructor TMyApplication.Destroy;
begin
  USBDevice.Free;
  inherited Destroy;
end;

var
  Application: TMyApplication;
  {$ifdef Windows}
  ServiceWnd       : HWND;
  ServiceWndClass  : Windows.TWndClass;
  {$endif}

{$ifdef Windows}
function ServiceWndProc(HWindow: HWnd; Message: UINT; WParam: WPARAM; LParam: LPARAM): Longint;stdcall;
var
  MyMsg: TMessage;
begin
  Result:=0;

  if (Message=WM_DESTROY) then
  begin
    PostQuitMessage(0);
    exit;
  end
  else
  begin
    MyMsg.msg:=Message;
    MyMsg.wParam:=WParam;
    MyMsg.lParam:=LParam;
    if Assigned(Application.USBDevice) then
    begin
      Application.USBDevice.USBMasterController.EventPipeExternal(MyMsg,HWindow);
      Result:=MyMsg.Result;
    end
    else
      Result:=DefWindowProc(HWindow,Message,WParam,LParam);
  end;
end;
{$endif}

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';

  {$ifdef Windows}
  ServiceWndClass.lpfnWndProc     := @ServiceWndProc;
  ServiceWndClass.hInstance       := HInstance;
  ServiceWndClass.lpszClassName   := 'HIDClass';
  Windows.RegisterClass(ServiceWndClass);

  ServiceWnd := CreateWindowEx(WS_EX_TOOLWINDOW, PChar('HIDClass'),
      'HID', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if (ServiceWnd=0) then Exit;
  {$endif}

  Application.Run;

  {$ifdef Windows}
  DestroyWindow(ServiceWnd);
  {$endif}

  Application.Free;
end.

