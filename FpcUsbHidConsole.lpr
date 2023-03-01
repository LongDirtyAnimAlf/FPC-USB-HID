program FpcUsbHidConsole;

uses Windows, SysUtils, usb2;

type
  TMyUSB = class(TUSB)
  public
    function CheckVendorProduct(const VID,PID:word):boolean;override;
  end;

function TMyUSB.CheckVendorProduct(const VID,PID:word):boolean;
begin
  result:=true;
end;

var
  active          : boolean;
  Msg             : TMsg;
  ServiceWnd      : HWND;
  ServiceWndClass : Windows.TWndClass;
  NewUSB          : TMyUSB;
  M               : TMethod;

function ServiceWndProc(HWindow: HWnd; Message: UINT; WParam: WPARAM; LParam: LPARAM): Longint;stdcall;
var
  Msg: TMessage;
begin
  Result:=0;

  if (Message=WM_DESTROY) then
  begin
    active:=false;
    PostQuitMessage(0);
    exit;
  end
  else
  begin
    Msg.msg:=Message;
    Msg.wParam:=WParam;
    Msg.lParam:=LParam;
    if Assigned(NewUSB) then
    begin
      NewUSB.Controller.EventPipeExternal(Msg,HWindow);
      Result:=Msg.Result;
    end
    else
      Result:=DefWindowProc(HWindow,Message,WParam,LParam);
  end;
end;

procedure UpdateUSBDevice(Sender: TObject;datacarrier:integer);
var
  S:string;
begin
  if (datacarrier>0) then WriteLn('HID device index: '+InttoStr(datacarrier));
  S:=NewUSB.Info;
  if Length(S)>0 then
  begin
    WriteLn('INFO:');
    WriteLn(S);
  end;
  S:=NewUSB.Errors;
  if Length(S)>0 then
  begin
    WriteLn('ERRORS:');
    WriteLn(S);
  end;
end;

begin
  ServiceWndClass.lpfnWndProc     := @ServiceWndProc;
  ServiceWndClass.hInstance       := HInstance;
  ServiceWndClass.lpszClassName   := 'HIDClass';
  Windows.RegisterClass(ServiceWndClass);

  ServiceWnd := CreateWindowEx(WS_EX_TOOLWINDOW, PChar('HIDClass'),
      'HID', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if (ServiceWnd=0) then Exit;

  NewUSB:=TMyUSB.Create;

  M.Code:=@UpdateUSBDevice;
  M.Data:=@NewUSB;

  NewUSB.OnUSBDeviceChange:=TUSBEvent(M);
  NewUSB.Enabled:=true;

  active:=true;
  (*
  while active do
  begin
    if PeekMessage({%H-}Msg,0,0,0,0) then
    begin
      GetMessage(Msg,0,0,0);
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
    Sleep(10);
  end;
  *)

  while (active AND GetMessage({%H-}msg, 0, 0, 0)) do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;

  NewUSB.Free;
  DestroyWindow(ServiceWnd);
end.

