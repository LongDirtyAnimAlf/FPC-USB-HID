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

type
  TReport = packed record
    ReportID: byte;
    Data:    {packed?} array [0..15] of byte; // <-- this needs to be adapted to the report size
  end;

  TUSBController = class
  strict private
    FHidCtrl       : TJvHidDevice;
    FProductSerial : ansistring;
    FaultCounter   : word;
    procedure SetDataEvent(const DataEvent: TJvHidDataEvent);
    function  GetDataEvent:TJvHidDataEvent;
    procedure ShowRead({%H-}HidDev: TJvHidDevice; ReportID: Byte;const Data: Pointer; {%H-}Size: Word);
  private
    LocalDataTimer : TEvent;
    property  OnData         : TJvHidDataEvent read GetDataEvent write SetDataEvent;
  public
    LocalData : TReport;
    Accepted  : boolean;
    constructor Create(HidDev: TJvHidDevice; SN:ansistring='');
    destructor Destroy;override;
    procedure EnableShowReadThreading;
    procedure DisableShowReadThreading;
    property  HidCtrl        : TJvHidDevice read FHidCtrl;
    property  ProductSerial  : ansistring read FProductSerial;
  end;

  TUSBControllerChangeEvent  = procedure(Sender: TObject;datacarrier:TUSBController) of object;

  TUSB=class
  private
    FHidCtl    : TJvHidDeviceController;

    FErrors    : TStringList;
    FInfo      : TStringList;
    FEmulation : boolean;

    FEnabled   : Boolean;
    FWaitEx    : Boolean;

    FOnUSBDeviceChange: TUSBControllerChangeEvent;

    function HidCtlEnumerate(HidDev: TJvHidDevice;const Idx: Integer): Boolean;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;
    procedure SetEnabled(Value: Boolean);

    procedure DeviceChange({%H-}Sender:TObject);

    function  CheckParameters(board:word):boolean;overload;

    function  GetHidCtl:TJvHidDeviceController;

  protected
    procedure DeviceArrival(HidDev: TJvHidDevice);
    procedure DeviceRemoval(HidDev: TJvHidDevice);
  public
    constructor Create;
    destructor Destroy;override;

    function  CheckVendorProduct(const {%H-}VID,{%H-}PID:word):boolean;virtual;
    function  CheckHIDDevice(const {%H-}HidDev: TJvHidDevice):boolean;virtual;

    function  Enumerate:integer;

    function  HidReadWrite(Ctrl: TUSBController; WriteOnly:boolean):boolean;

    property  Emulation:boolean read FEmulation;

    property  Errors:String read GetErrors write AddErrors;
    property  Info:String read GetInfo write AddInfo;

    property  Enabled: Boolean read FEnabled write SetEnabled;
    property  WaitEx: Boolean read FWaitEx write FWaitEx;

    property  OnUSBDeviceChange: TUSBControllerChangeEvent read FOnUSBDeviceChange write FOnUSBDeviceChange;

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
  begin
    result:=HidCtrl.OnData;
  end else result:=nil;
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

procedure TUSB.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    USBMasterController.Enabled:=FEnabled;
    if FEnabled then
    begin
      // Get and process the connected devices
      Enumerate;
      // either enable this, or the other two, to detect USB device changes
      USBMasterController.OnDeviceChange:=DeviceChange;
      //USBMasterController.OnArrival:= DeviceArrival;
      //USBMasterController.OnRemoval:= DeviceRemoval;
    end
    else
    begin
      USBMasterController.OnArrival:= nil;
      USBMasterController.OnRemoval:= nil;
      USBMasterController.OnDeviceChange:=nil;
    end;
  end;
end;

function TUSB.HidCtlEnumerate(HidDev: TJvHidDevice; const Idx: Integer): Boolean;
begin
  result:=True;
  //AddInfo('Enumerate device #'+InttoStr(Idx)+'. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'. Name: '+HidDev.PnPInfo.FriendlyName+'. Product: '+HidDev.ProductName);
  DeviceArrival(HidDev);
  if (MainThreadID=GetCurrentThreadID) then CheckSynchronize;
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
var
  aController:TUSBController;
begin
  //AddInfo('Device removal. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
  if (CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) AND CheckHIDDevice(HidDev)) then
  begin
    //AddInfo('Correct device removal. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
    if HidDev.IsCheckedOut then
    begin
      if Assigned(FOnUSBDeviceChange) then
      begin
        // A bit tricky
        // Create controller without serial to indicate removal
        aController:=TUSBController.Create(HidDev);
        try
          // Signal the boss about the removal
          FOnUSBDeviceChange(Self,aController);
          // Checkin device
          USBMasterController.CheckIn(HidDev);
        finally
          aController.Destroy;
        end;
      end
      else
      begin
        // Juts perform simple checkin
        USBMasterController.CheckIn(HidDev);
      end;
    end;
    if USBMasterController.NumCheckedOutDevices=0 then FEmulation:=True;
  end;
end;

procedure TUSB.DeviceArrival(HidDev: TJvHidDevice);
var
  NewUSBController : TUSBController;
  LocalSerial      : ansistring;
begin
  //AddInfo('Device arrival. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');

  if (CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) AND CheckHIDDevice(HidDev)) then
  begin
   //AddInfo('Correct Device arrival. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');

   FEmulation:=False;

   //AddInfo('Device type: '+HidDev.SerialNumber);

   //AddInfo('I1: '+HidDev.DeviceStrings[1]);
   //AddInfo('I2: '+HidDev.DeviceStrings[2]);
   //AddInfo('I3: '+HidDev.DeviceStrings[3]);
   //AddInfo('I4: '+HidDev.DeviceStrings[4]);

   //AddInfo('Input length: '+InttoStr(HidDev.Caps.InputReportByteLength));
   //AddInfo('Output length: '+InttoStr(HidDev.Caps.OutputReportByteLength));

   LocalSerial:='';
   if ((LocalSerial='') OR (Length(LocalSerial)<>29)) then LocalSerial:=HidDev.DeviceStrings[6];
   if ((LocalSerial='') OR (Length(LocalSerial)<>29)) then LocalSerial:=HidDev.DeviceStrings[5];
   if ((LocalSerial='') OR (Length(LocalSerial)<>29)) then LocalSerial:=HidDev.DeviceStrings[4];
   if ((LocalSerial='') OR (Length(LocalSerial)<>29)) then LocalSerial:=HidDev.SerialNumber;
   // Last resort serial
   if ((LocalSerial='') OR (Length(LocalSerial)<>29)) then LocalSerial:=InttoStr(HidDev.Attributes.VendorID)+'_'+InttoStr(HidDev.Attributes.ProductID)+'_'+InttoStr(HidDev.PnPInfo.DeviceID);

   if LocalSerial='' then
   begin
     AddInfo('Severe error while receiving serial number of controller !!!!');
     AddInfo('Therefor: ignoring the USB device !!');
     exit;
   end;

   if (NOT HidDev.IsCheckedOut) then
   begin
     if HidDev.CheckOut then
     begin
       if Assigned(FOnUSBDeviceChange) then
       begin
         // Create controller with serial to indicate arrival
         // Will be freed by the boss
         NewUSBController:=TUSBController.Create(HidDev,LocalSerial);
         FOnUSBDeviceChange(Self,NewUSBController);
         if (NOT NewUSBController.Accepted) then NewUSBController.Destroy;
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
end;

procedure TUSB.DeviceChange(Sender:TObject);
var
  i:integer;
  Device:TJvHidDevice;
  LocalController:TJvHidDeviceController;
  {$ifdef debug}
  s:string;
  {$endif debug}
begin
  //LocalController:=(Sender AS TJvHidDeviceController);
  LocalController:=USBMasterController;
  AddInfo('Devices change !!');
  i:=0;
  while (i<LocalController.HidDevices.Count) do
  begin
    Device:=TJvHidDevice(LocalController.HidDevices[i]);

    {$ifdef debug}
    s:='HID';
    {$ifdef Unix}
    if Pos('hidraw',Device.PhysicalDescriptor)>0 then s:='HIDraw';
    if Pos('hiddev',Device.PhysicalDescriptor)>0 then s:='HIDdev';
    {$endif}
    AddInfo(s+'-device #'+InttoStr(i)+'. VID: '+InttoStr(Device.Attributes.VendorID)+'. PID: '+InttoStr(Device.Attributes.ProductID)+'.');
    {$ifdef MSWINDOWS}
    AddInfo('Mfg: '+Device.PnPInfo.Mfg+'. Name UTF8: '+UTF16ToUTF8(Device.ProductName)+'. Vendor: '+UTF16ToUTF8(Device.VendorName)+'.');
    {$else}
    AddInfo('Mfg: '+HidDev.PnPInfo.Mfg+'. Name: '+HidDev.ProductName+'. Vendor: '+HidDev.VendorName+'.');
    {$endif}
    {$endif debug}

    if (CheckVendorProduct(Device.Attributes.VendorID,Device.Attributes.ProductID) AND CheckHIDDevice(Device)) then
    begin
      if Device.IsPluggedIn AND NOT Device.IsCheckedOut then
      begin
        AddInfo('New device that has not been checked out.');
        DeviceArrival(Device);
      end;
      if NOT Device.IsPluggedIn AND Device.IsCheckedOut then
      begin
        AddInfo('Checkedout device that has been unplugged.');
        DeviceRemoval(Device);
      end;
    end;
    Inc(i);
  end;
end;

function TUSB.CheckParameters(board:word):boolean;
begin
  result:=FEmulation;
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
  //result:=HidCtl;
  result:=FHidCtl;
end;

end.
