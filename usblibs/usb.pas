unit usb;

{$ifdef FPC}
{$mode DELPHI}
{$endif}

interface

uses
  SysUtils, Classes, SyncObjs,
  {$ifdef MSWINDOWS}
  JvHidControllerClass;
  {$else}
  usbcontroller;
  {$endif}

const
  USBSERIALREMOVEME ='I_AM_UNPLUGGED_AND_REMOVED';

type
  TReport = packed record
    ReportID: byte;
    Data:    {packed?} array [0..15] of byte; // <-- this needs to be adapted to the report size
  end;
  PReport = ^TReport;

  TUSBController = class
  strict private
    FHidCtrl       : TJvHidDevice;
    FProductSerial : ansistring;
    //FaultCounter   : word;
    procedure SetDataEvent(const DataEvent: TJvHidDataEvent);
    function  GetDataEvent:TJvHidDataEvent;
    procedure ShowRead({%H-}HidDev: TJvHidDevice; ReportID: Byte;const Data: Pointer; {%H-}Size: Word);
  private
    LocalDataTimer : TEvent;
    property  OnData         : TJvHidDataEvent read GetDataEvent write SetDataEvent;
  public
    LocalData         : TReport;
    Accepted          : boolean;
    FaultCounter      : word;
    ControllerData    : pointer;
    constructor Create(HidDev: TJvHidDevice; SN:ansistring='');
    destructor Destroy;override;
    procedure EnableShowReadThreading;
    procedure DisableShowReadThreading;
    property  HidCtrl        : TJvHidDevice read FHidCtrl;
    property  ProductSerial  : ansistring read FProductSerial;
  end;

  TRemoveUSBController = class(TUSBController)
  end;

  TUSBChangeEvent  = procedure(Sender: TObject;datacarrier:TUSBController) of object;

  TUSB=class
  strict private
    FHidCtl    : TJvHidDeviceController;

    FErrors    : TStringList;
    FInfo      : TStringList;
    FEmulation : boolean;

    FEnabled   : Boolean;
    FWaitEx    : Boolean;

    FOnUSBDeviceChange: TUSBChangeEvent;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;
    procedure SetEnabled(Value: Boolean);

    function  SendDevice(HidDev: TJvHidDevice; const LocalSerial: ansistring = ''):boolean;
    function  HidCtlEnumerate(HidDev: TJvHidDevice;const {%H-}Idx: Integer): Boolean;
    function  GetHidCtl:TJvHidDeviceController;

    function  Enumerate:integer;
  protected
    procedure DeviceArrival(HidDev: TJvHidDevice);
    procedure DeviceRemoval(HidDev: TJvHidDevice);
  public
    constructor Create;
    destructor Destroy;override;

    function  CheckVendorProduct(const {%H-}VID,{%H-}PID:word):boolean;virtual;
    function  CheckHIDDevice(const {%H-}HidDev: TJvHidDevice):boolean;virtual;

    function  HidReadWrite(Ctrl: TUSBController; WriteOnly:boolean):boolean;

    property  Emulation:boolean read FEmulation;

    property  Errors:String read GetErrors write AddErrors;
    property  Info:String read GetInfo write AddInfo;

    property  Enabled: Boolean read FEnabled write SetEnabled;
    property  WaitEx: Boolean read FWaitEx write FWaitEx;

    property  OnUSBDeviceChange: TUSBChangeEvent read FOnUSBDeviceChange write FOnUSBDeviceChange;

    property  USBMasterController:TJvHidDeviceController read GetHidCtl;
  end;

implementation

uses
  {$ifdef MSWINDOWS}
  Windows;
  {$else}
  Unix,
  BaseUnix;
  {$endif}

const
  //DeviceDelay                   = 20;
  {$ifdef win64}
  USBTimeout                    = 500;
  {$else}
  USBTimeout                    = 150;
  {$endif}

function UTF16ToUTF8(const s: UnicodeString): string;
begin
  {$IFDEF UNICODE}
  Result:=s;
  {$ELSE}
    if s='' then exit('');
    Result:=UTF8Encode(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
  {$ENDIF}
end;

constructor TUSBController.Create(HidDev: TJvHidDevice; SN:ansistring);
begin
  Inherited Create;
  ControllerData:=nil;
  LocalDataTimer:=nil;
  FaultCounter:=0;
  Accepted:=false;
  FProductSerial:=SN;
  FHidCtrl:=HidDev;
end;

destructor TUSBController.Destroy;
begin
  if Assigned(LocalDataTimer) then LocalDataTimer.Destroy;
  inherited Destroy;
end;

procedure TUSBController.EnableShowReadThreading;
begin
  if Assigned(HidCtrl) then
  begin
    HidCtrl.FlushQueue;
    // Start reading thread and direct data to ShowRead
    OnData:=ShowRead;
    // Sleep a bit to give threads some time to startup
    Sleep(25);
  end;
end;

procedure TUSBController.DisableShowReadThreading;
begin
  OnData:=nil;
end;

procedure TUSBController.ShowRead(HidDev: TJvHidDevice; ReportID: Byte;const Data: Pointer; Size: Word);
var
  x: Integer;
  P: PByte;
begin
  LocalData.ReportID:=ReportID;
  P := PByte(Data);
  for x := Low(LocalData.Data) to High(LocalData.Data) do
  begin
    LocalData.Data[x]:=P^;
    Inc(P);
  end;
  if Assigned(LocalDataTimer) then LocalDataTimer.SetEvent;
end;

procedure TUSBController.SetDataEvent(const DataEvent: TJvHidDataEvent);
begin
  if Assigned(HidCtrl) then
  begin
    HidCtrl.OnData:=DataEvent;
    if Assigned(HidCtrl.OnData) then
    begin
      LocalDataTimer:=TEvent.Create(nil, true, false, '');
      LocalDataTimer.ResetEvent;
    end;
  end;
  if Assigned(LocalDataTimer) then
  begin
    if ((NOT Assigned(HidCtrl)) OR (NOT Assigned(DataEvent))) then
    begin
     LocalDataTimer.Destroy;
     LocalDataTimer:=nil;
    end;
  end;
end;

function TUSBController.GetDataEvent: TJvHidDataEvent;
begin
  if Assigned(HidCtrl) then
    result:=HidCtrl.OnData
  else
    result:=nil;
end;

constructor TUSB.Create;
begin
  inherited Create;

  FErrors       := TStringList.Create;
  FInfo         := TStringList.Create;
  FEmulation    := True;
  FEnabled      := False;
  FWaitEx       := False;

  if (NOT Assigned(FHidCtl)) then
  begin
   FHidCtl:=TJvHidDeviceController.Create(nil);

   //FHidCtl:=HidCtl;
   //USBMasterController.DevThreadSleepTime:=USBTimeout;
   //USBMasterController.DevThreadSleepTime:=10;
   USBMasterController.OnArrival:= nil;
   USBMasterController.OnRemoval:= nil;
   USBMasterController.OnDeviceChange:=nil;
   USBMasterController.OnEnumerate:=HidCtlEnumerate;
  end;
end;

destructor TUSB.Destroy;
begin
  if Assigned(FHidCtl) then
  begin
   FHidCtl.OnArrival:= nil;
   FHidCtl.OnRemoval:= nil;
   FHidCtl.OnDeviceChange:=nil;
   FHidCtl.OnEnumerate:=nil;
   FHidCtl.Destroy;
   FHidCtl:=nil;
  end;
  FErrors.Free;
  FInfo.Free;
  inherited Destroy;
end;

function TUSB.SendDevice(HidDev: TJvHidDevice; const LocalSerial: ansistring):boolean;
var
  NewUSBController:TUSBController;
begin
  result:=false;
  if Assigned(FOnUSBDeviceChange) then
  begin
    // Create controller with serial
    // Will be freed by the boss, if accepted
    NewUSBController:=TUSBController.Create(HidDev,LocalSerial);
    FOnUSBDeviceChange(Self,NewUSBController);
    result:=NewUSBController.Accepted;
    // if the controller is not accepted by the boss, we need to destroy it ourselves !
    if (NOT result) then NewUSBController.Destroy;
  end;
end;

procedure TUSB.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    USBMasterController.Enabled:=FEnabled;
    if FEnabled then
    begin
      USBMasterController.OnArrival:= DeviceArrival;
      USBMasterController.OnRemoval:= DeviceRemoval;
      // Get and process the connected devices
      Enumerate;
    end
    else
    begin
      USBMasterController.OnArrival:= nil;
      USBMasterController.OnRemoval:= nil;
    end;
  end;
end;

function TUSB.HidCtlEnumerate(HidDev: TJvHidDevice; const Idx: Integer): Boolean;
begin
  result:=True;
  DeviceArrival(HidDev);
end;

function TUSB.Enumerate:integer;
begin
  result:=USBMasterController.Enumerate;
end;

function TUSB.HidReadWrite(Ctrl: TUSBController; WriteOnly:boolean):boolean;
var
  error:boolean;
  Written,TotalWritten:DWORD;
  Err:DWORD;
begin
  error:=False;

  if ((NOT Assigned(Ctrl)) OR (NOT Assigned(Ctrl.HidCtrl))) then
  begin
    result:=False;
    exit;
  end;

  if Assigned(Ctrl.HidCtrl) then
  begin
    if (NOT WriteOnly) then
    begin
      Ctrl.HidCtrl.FlushQueue;
      if Assigned(Ctrl.OnData) then Ctrl.LocalDataTimer.ResetEvent;
    end;
    TotalWritten:=0;
    while true do
    begin
      Written:=0;
      error:=(NOT Ctrl.HidCtrl.WriteFile(Ctrl.LocalData, Ctrl.HidCtrl.Caps.OutputReportByteLength, Written));
      error:=(error OR (Written=0));
      if (error) then
      begin
        {$ifdef MSWINDOWS}
        Err := GetLastError;
        {$else}
        Err := fpgeterrno;
        {$endif}
        AddErrors(Format('USB normal write error: %s (%x)', [SysErrorMessage(Err), Err]));
        break;
      end;
      Inc(TotalWritten,Written);
      if (TotalWritten>=SizeOf(Ctrl.LocalData)) then break;
      break;
    end;

    if (NOT error) AND (NOT WriteOnly) then
    begin
      if Assigned(Ctrl.OnData) then
      begin
       error:=True;
       if (WaitEx {OR True}) then
       begin
         Err:=0;
         repeat
           if (MainThreadID=GetCurrentThreadID) then CheckSynchronize(USBTimeout DIV 10);
           error:=(Ctrl.LocalDataTimer.WaitFor(USBTimeout DIV 10)<>wrSignaled);
           Inc(Err);
         until ((NOT error) OR (Err>10));
       end
       else
       begin
         if (MainThreadID=GetCurrentThreadID) then CheckSynchronize(USBTimeout);
         error:=(Ctrl.LocalDataTimer.WaitFor(USBTimeout)<>wrSignaled);
       end;
       if error then
       begin
         FillChar(Ctrl.LocalData, SizeOf(Ctrl.LocalData), 0);
         AddErrors('USB thread read timeout !!');
       end;
      end
      else
      begin
        TotalWritten:=0;
        while true do
        begin
          FillChar(Ctrl.LocalData, SizeOf(Ctrl.LocalData), 0);
          Written:=0;
          error:=(NOT Ctrl.HidCtrl.ReadFileTimeOut(Ctrl.LocalData, Ctrl.HidCtrl.Caps.InputReportByteLength, Written, USBTimeout));
          if (error) then
          begin
            //windows.beep(1000,100);
            FillChar(Ctrl.LocalData, SizeOf(Ctrl.LocalData), 0);
            if (Ctrl.HidCtrl.Err<>ERROR_SUCCESS) then
              AddErrors(Format('USB normal read error: %s (%x)', [SysErrorMessage(Ctrl.HidCtrl.Err), Ctrl.HidCtrl.Err]));
            break;
          end
          else
          begin
            Inc(TotalWritten,Written);
            if (TotalWritten>=SizeOf(Ctrl.LocalData)) then break;
            break;
          end;
        end;
      end;
    end;
  end;

  result:=error;
end;

procedure TUSB.DeviceRemoval(HidDev: TJvHidDevice);
begin
  if (CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) AND CheckHIDDevice(HidDev)) then
  begin
    AddInfo('Correct device removal. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
    if (NOT SendDevice(HidDev)) then
    begin
      // The device is not accepted by the boss, we might need to checkin !
      if HidDev.IsCheckedOut then USBMasterController.CheckIn(HidDev);
    end;
  end;
  FEmulation:=(USBMasterController.NumCheckedOutDevices=0);
  //if (MainThreadID=GetCurrentThreadID) then CheckSynchronize;
end;

procedure TUSB.DeviceArrival(HidDev: TJvHidDevice);
var
  LocalSerial      : string;
begin
  if (CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) AND CheckHIDDevice(HidDev)) then
  begin
    AddInfo('Correct device arrival. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');

    LocalSerial:=string(HidDev.SerialNumber);
    if (LocalSerial='') then LocalSerial:=InttoStr(HidDev.Attributes.VendorID)+'_'+InttoStr(HidDev.Attributes.ProductID)+'_'+InttoStr(HidDev.PnPInfo.DeviceID);

    if (NOT HidDev.IsCheckedOut) then
    begin
      if HidDev.CheckOut then
      begin
        // Send device to boss
        if (NOT SendDevice(HidDev,LocalSerial)) then
        begin
          // The device is not accepted by the boss, we need to checkin !
          USBMasterController.CheckIn(HidDev);
        end;
      end;
    end;
  end
  else
  begin
    if Assigned(FOnUSBDeviceChange) then
    begin
      // We might send some info about other devices not in our list of accepted devices
      FOnUSBDeviceChange(Self,nil);
    end;
  end;
  FEmulation:=(USBMasterController.NumCheckedOutDevices=0);
  //if (MainThreadID=GetCurrentThreadID) then CheckSynchronize;
end;

procedure TUSB.AddErrors(data:string);
begin
  if length(data)>0 then
  begin
   while (FErrors.Count>1000) do FErrors.Delete(0);
   FErrors.Append(DateTimeToStr(Now)+'; USB error: '+data);
   //FErrors.Append('USB error: '+data);
  end;
end;

function TUSB.GetErrors:String;
begin
  if FErrors.Count>0 then
  begin
    result:=FErrors.CommaText;
    FErrors.Clear;
  end else result:='';
end;

procedure TUSB.AddInfo(data:string);
begin
  if Length(data)>0 then
  begin
    while FInfo.Count>1000 do FInfo.Delete(0);
    FInfo.Append(data);
  end;
end;

function TUSB.GetInfo:String;
begin
  {$ifdef UNIX}
  AddInfo(USBMasterController.DebugInfo);
  {$endif}
  if FInfo.Count>0 then
  begin
    result:=FInfo.Text;
    FInfo.Clear;
  end else result:='';
end;

function TUSB.CheckVendorProduct(const VID,PID:word):boolean;
begin
  result:=true;
end;

function TUSB.CheckHIDDevice(const HidDev: TJvHidDevice):boolean;
begin
  result:=true;
end;

function TUSB.GetHidCtl:TJvHidDeviceController;
begin
  result:=FHidCtl;
end;

end.
