unit usb2;

interface

{$define USEHASHLIST}

uses
  SysUtils, Classes, SyncObjs
  {$ifdef usegenerics}
  ,fgl
  {$endif}
  {$ifdef MSWINDOWS}
  ,JvHidControllerClass
  {$else}
  ,usbcontroller
  {$endif}
  ;

const
  INIFILENAME = 'boards.ini';

type
  TReport = packed record
    ReportID: byte;
    Data:    array [0..15] of byte; // <-- this needs to be adapted to your report size
  end;

  TUSBController = class
  strict private
    FHidCtrl       : TJvHidDevice;
  private
    FaultCounter   : word;
    LocalDataTimer : TEvent;
    procedure SetDataEvent(const DataEvent: TJvHidDataEvent);
    function  GetDataEvent:TJvHidDataEvent;
    procedure ShowRead({%H-}HidDev: TJvHidDevice; ReportID: Byte;const Data: Pointer; Size: Word);
  public
    LocalData      : TReport;
    Serial         : string;
    constructor Create(HidDev: TJvHidDevice);
    destructor  Destroy;override;
    property  OnData  : TJvHidDataEvent read GetDataEvent write SetDataEvent;
    property  HidCtrl : TJvHidDevice read FHidCtrl;
  end;


  {$IFDEF usegenerics}
  TUSBList = specialize TFPGList<TUSBController>;
  {$ELSE}
  TUSBList = TList;
  {$ENDIF}

  TUSBEvent  = procedure(Sender: TObject;datacarrier:integer) of object;

  TUSB=class
  private
    HidCtl:TJvHidDeviceController;

    FUSBList   : TUSBList;

    FErrors    : TStringList;
    FInfo      : TStringList;
    FEmulation : boolean;

    FEnabled   : Boolean;

    FOnUSBDeviceChange: TUSBEvent;

    FIniFileFullPath:string;

    function HidCtlEnumerate(HidDev: TJvHidDevice;const Idx: Integer): Boolean;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;
    procedure SetEnabled(Value: Boolean);

    procedure DeviceArrival(HidDev: TJvHidDevice);
    procedure DeviceRemoval(HidDev: TJvHidDevice);
    procedure DeviceChange(Sender:TObject);

    function  CheckAddressNewer(Ctrl: TUSBController):integer;
    function  CheckParameters(board:word):boolean;overload;

    function  FGetSerial(board:word):string;
  public
    constructor Create;
    destructor Destroy;override;

    function  CheckVendorProduct(const VID,PID:word):boolean;virtual;

    function  Enumerate:integer;

    function  HidReadWrite(Ctrl: TUSBController; ReadOnly:boolean):boolean;

    property  Emulation:boolean read FEmulation;

    property  Errors:String read GetErrors;
    property  Info:String read GetInfo write AddInfo;

    property  Enabled: Boolean read FEnabled write SetEnabled;

    property  OnUSBDeviceChange: TUSBEvent read FOnUSBDeviceChange write FOnUSBDeviceChange;

    property  GetSerial[board: word]: string read FGetSerial;

    property  Controller:TJvHidDeviceController read HidCtl;

    property  USBList : TUSBList read FUSBList;
  end;


implementation

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  Unix,
  BaseUnix,
  {$endif}
  IniFiles,
  StrUtils;

const
  VENDORID_BASE                 = $04D8;
  PRODUCTID_BASE                = $003F;
  VENDORID_ALT                  = $ABCD;
  PRODUCTID_ALT                 = $1234;

  ErrorDelay                    = 100;
  USBTimeout                    = 200;

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

constructor TUSBController.Create(HidDev: TJvHidDevice);
begin
  Inherited Create;
  FHidCtrl:=HidDev;
  if HidCtrl<>nil then
  begin
    OnData:=nil;
    // enable this for non-blocking read of USB !!!
    //OnData:=ShowRead;
  end;
end;

destructor TUSBController.Destroy;
begin
  OnData:=nil;
  Inherited Destroy;
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

  FIniFileFullPath:=INIFILENAME;

  FErrors:=TStringList.Create;
  FInfo:=TStringList.Create;

  FUSBList := TUSBList.Create;

  FEmulation    := True;

  HidCtl:=TJvHidDeviceController.Create(nil);
  HidCtl.OnArrival:= nil;
  HidCtl.OnRemoval:= nil;
  HidCtl.OnDeviceChange:=nil;
  HidCtl.OnEnumerate:=HidCtlEnumerate;
end;

destructor TUSB.Destroy;
var
  board:word;
  aController:TUSBController;
  aHidCtrl:TJvHidDevice;
begin
  HidCtl.OnArrival:= nil;
  HidCtl.OnRemoval:= nil;
  HidCtl.OnDeviceChange:=nil;
  HidCtl.OnEnumerate:=nil;

  if FUSBList.Count>0 then
  begin
    for board:=Pred(FUSBList.Count) downto 0  do
    begin
      if Assigned(FUSBList.Items[board]) then
      begin
        aController:={$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[board]);
        aHidCtrl:=aController.HidCtrl;
        //DeviceRemoval(aHidCtrl);
        aController.Destroy;
        if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
      end;
      FUSBList.Delete(board);
    end;
  end;

  FUSBList.Free;

  HidCtl.Destroy;
  HidCtl:=nil;

  FErrors.Free;
  FInfo.Free;
  inherited Destroy;
end;

procedure TUSB.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      // either enable this, or the other two, to detect USB device changes
      HidCtl.OnDeviceChange:=DeviceChange;
      //HidCtl.OnArrival:= DeviceArrival;
      //HidCtl.OnRemoval:= DeviceRemoval;
    end
    else
    begin
      HidCtl.OnArrival:= nil;
      HidCtl.OnRemoval:= nil;
      HidCtl.OnDeviceChange:=nil;
    end;
    HidCtl.Enabled:=FEnabled;
  end;
end;

function TUSB.HidCtlEnumerate(HidDev: TJvHidDevice; const Idx: Integer): Boolean;
begin
  AddInfo('Device #'+InttoStr(Idx)+' arrival. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
  if CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) then
  begin
    if HidDev.IsCheckedOut then
    begin
      AddInfo('I1: '+HidDev.DeviceStrings[1]);
      AddInfo('I2: '+HidDev.DeviceStrings[2]);
      AddInfo('I3: '+HidDev.DeviceStrings[3]);
      AddInfo('I4: '+HidDev.DeviceStrings[4]);

      AddInfo('Input length: '+InttoStr(HidDev.Caps.InputReportByteLength));
      AddInfo('Output length: '+InttoStr(HidDev.Caps.OutputReportByteLength));
    end;
  end;
  result:=True;
end;

function TUSB.Enumerate:integer;
begin
  result:=HidCtl.Enumerate;
end;

function TUSB.HidReadWrite(Ctrl: TUSBController; ReadOnly:boolean):boolean;
var
  error:boolean;
  Written,TotalWritten:DWORD;
  Err:DWORD;
begin
  error:=False;

  if NOT Assigned(Ctrl.HidCtrl) then
  begin
    result:=False;
    exit;
  end;

  if Assigned(Ctrl.HidCtrl) then
  begin
    //Ctrl.HidCtrl.FlushQueue;
    if (NOT ReadOnly) then
    begin
      if Assigned(Ctrl.OnData) then Ctrl.LocalDataTimer.ResetEvent;
    end;
    TotalWritten:=0;
    while true do
    begin
      error:=(NOT Ctrl.HidCtrl.WriteFile(Ctrl.LocalData, Ctrl.HidCtrl.Caps.OutputReportByteLength, Written));
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
    end;

    if (NOT error) AND (NOT ReadOnly) then
    begin
      error:=True;
      if Assigned(Ctrl.OnData) then
      begin
        if Ctrl.LocalDataTimer.WaitFor(USBTimeout) = wrSignaled
           then error:=False
           else
           begin
             FillChar(Ctrl.LocalData, SizeOf(Ctrl.LocalData), 0);
             AddErrors('USB thread read timeout !!');
           end;
      end
      else
      begin
        error:=(NOT Ctrl.HidCtrl.ReadFile(Ctrl.LocalData, Ctrl.HidCtrl.Caps.InputReportByteLength, Written));
        if error then
        begin
          FillChar(Ctrl.LocalData, SizeOf(Ctrl.LocalData), 0);
          {$ifdef MSWINDOWS}
          Err := GetLastError;
          {$else}
          Err := fpgeterrno;
          {$endif}
          AddErrors(Format('USB normal read error: %s (%x)', [SysErrorMessage(Err), Err]));
        end;
      end;
    end;
  end;

  result:=error;

end;


procedure TUSB.DeviceRemoval(HidDev: TJvHidDevice);
var
  board:integer;
  aController:TUSBController;
  aHidCtrl:TJvHidDevice;
begin
  AddInfo('Device removal. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
  if CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) then
  begin
    for board:=Pred(FUSBList.Count) downto 0 do
    begin
      if Assigned(FUSBList.Items[board]) then
      begin
        aController:={$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[board]);
        aHidCtrl:=aController.HidCtrl;
        //if ((Assigned(aHidCtrl)) and (NOT aHidCtrl.IsPluggedIn)) then
        if ((Assigned(aHidCtrl)) and (aHidCtrl=HidDev)) then
        begin
          if Assigned(FOnUSBDeviceChange) then
          begin
            FOnUSBDeviceChange(Self,-board);
          end;
          aController.Destroy;
          if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
          FUSBList.Items[board]:=nil;
          break;
        end;
      end;
    end;
    if HidCtl.NumCheckedOutDevices=0 then FEmulation:=True;
  end;
end;

procedure TUSB.DeviceArrival(HidDev: TJvHidDevice);
var
  newboard:integer;
  NewUSBController : TUSBController;
  aHidCtrl:TJvHidDevice;
begin

  AddInfo('Device arrival. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');

  if CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) then
  begin

    if HidDev.CheckOut then
    begin

      FEmulation:=False;

      AddInfo('I1: '+HidDev.DeviceStrings[1]);
      AddInfo('I2: '+HidDev.DeviceStrings[2]);
      AddInfo('I3: '+HidDev.DeviceStrings[3]);
      AddInfo('I4: '+HidDev.DeviceStrings[4]);

      AddInfo('Input length: '+InttoStr(HidDev.Caps.InputReportByteLength));
      AddInfo('Output length: '+InttoStr(HidDev.Caps.OutputReportByteLength));

      NewUSBController := TUSBController.Create(HidDev);

      Sleep(200);

      with NewUSBController do
      begin
        Serial:=HidDev.DeviceStrings[4];
        if Serial='' then Serial:=HidDev.SerialNumber;
        if Serial='' then
        begin
          Serial:=InttoStr(HidDev.Attributes.VendorID)+'_'+InttoStr(HidDev.Attributes.ProductID)+'_'+InttoStr(HidDev.PnPInfo.DeviceID);
        end;
        FaultCounter:=0;
      end;

      Sleep(200);

      if NewUSBController.Serial='' then
      begin
        AddInfo('Severe error while receiving serial number of controller !!!!');
        AddInfo('Therefor: destroying the USB controller !!');
        NewUSBController.Destroy;
        HidCtl.CheckIn(HidDev);
        exit;
      end;

      newboard:=CheckAddressNewer(NewUSBController);

      AddInfo('S/N of board '+InttoStr(newboard)+': '+NewUSBController.Serial);


      if FUSBList.Count<(newboard+1) then
      begin
        FUSBList.Count:=newboard+1;
      end;

      if FUSBList.Items[newboard]<>nil then
      begin
        AddInfo('Strange error: list-position of board already taken !!');
        AddInfo('Therefor: destroying the USB controller !!');

        // there are two possibilities to handle this: destroy new arrival or destroy previous contents of list
        // both are here just to prevent any memory leaks, because this should never happen !!

        // destroy new arrival
        //NewUSBController.Destroy;
        //HidCtl.CheckIn(HidDev);
        //exit;

        // destroy previous contents of list at position of board
        aHidCtrl:={$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[newboard]).HidCtrl;
        {$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[newboard]).Destroy;
        if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
      end;

      FUSBList.Items[newboard]:=NewUSBController;

      if Assigned(FOnUSBDeviceChange) then
      begin
        FOnUSBDeviceChange(Self,newboard);
      end;

    end;
  end
  else
  begin
    if Assigned(FOnUSBDeviceChange) then
    begin
      FOnUSBDeviceChange(Self,0);
    end;
  end;
end;

procedure TUSB.DeviceChange(Sender:TObject);
var
  i:integer;
  HidDev:TJvHidDevice;
  s:string;
begin
  AddInfo('Devices change !!');
  i:=0;
  while (i<HidCtl.HidDevices.Count) do
  begin
    HidDev:=TJvHidDevice(HidCtl.HidDevices[i]);
    s:='HID';
    {$ifdef Unix}
    if Pos('hidraw',HidDev.PhysicalDescriptor)>0 then s:='HIDraw';
    if Pos('hiddev',HidDev.PhysicalDescriptor)>0 then s:='HIDdev';
    {$endif}

    {$ifdef debug}
    AddInfo(s+'-device #'+InttoStr(i)+'. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
    {$ifdef MSWINDOWS}
    AddInfo('Mfg: '+HidDev.PnPInfo.Mfg+'. Name UTF8: '+UTF16ToUTF8(HidDev.ProductName)+'. Vendor: '+UTF16ToUTF8(HidDev.VendorName)+'.');
    {$else}
    AddInfo('Mfg: '+HidDev.PnPInfo.Mfg+'. Name: '+HidDev.ProductName+'. Vendor: '+HidDev.VendorName+'.');
    {$endif}
    {$endif debug}

    if CheckVendorProduct(HidDev.Attributes.VendorID,HidDev.Attributes.ProductID) then
    begin
      if HidDev.IsPluggedIn AND NOT HidDev.IsCheckedOut then
      begin
        AddInfo('New device that has not been checked out.');
        DeviceArrival(HidDev);
      end;
      if NOT HidDev.IsPluggedIn AND HidDev.IsCheckedOut then
      begin
        AddInfo('Checkedout device that has been unplugged.');
        DeviceRemoval(HidDev);
      end;
    end;
    Inc(i);
  end;
end;

function TUSB.CheckAddressNewer(Ctrl: TUSBController):integer;
var
  x,y: integer;
  newboardnumber:word;
  found:boolean;
  RegValueNames: TStringList;
  dataline:string;
  error:boolean;
  Ini: TIniFile;
begin

  result:=0;

  error:=False;

  if (NOT error)  then
  begin
    found:=false;

    newboardnumber:=0;

    Ini := TIniFile.Create(FIniFileFullPath);

    RegValueNames:=TStringList.Create;
    try
      ini.ReadSection('USBLocations',RegValueNames);
      if RegValueNames.Count>0 then
      begin
        for x:=1 to RegValueNames.Count do
        begin
          If Pos('Controller',RegValueNames.Strings[x-1])>-1 then
          begin
            y:=StrToIntDef(RightStr(RegValueNames.Strings[x-1],2),0);
            dataline:=ini.ReadString('USBLocations',RegValueNames.Strings[x-1],'');
            if (dataline=Ctrl.Serial) AND (y>0) then
            begin
              found:=true;
              newboardnumber:=y;
              break;
            end;
          end;
        end;
      end;
      if (NOT found) then
      begin
        y:=1;
        while ini.ValueExists('USBLocations','Controller '+InttoStr(y)) do Inc(y);
        ini.WriteString('USBLocations','Controller '+InttoStr(y),Ctrl.Serial);
        newboardnumber:=y;
      end;
    finally
      RegValueNames.Free;
      Ini.UpdateFile;
      Ini.Free;
    end;

    result:=newboardnumber;
  end;

end;

function TUSB.CheckParameters(board:word):boolean;
begin
  result:=true;
  if  FEmulation then exit;
  result:=NOT ( (board<FUSBList.Count) AND (Assigned({$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[board]).HidCtrl)) );
end;

function TUSB.GetErrors:String;
begin
  if FErrors.Count>0 then
  begin
    result:=FErrors.Text;
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
  AddInfo(HidCtl.DebugInfo);
  {$endif}
  if FInfo.Count>0 then
  begin
    result:=FInfo.Text;
    FInfo.Clear;
  end else result:='';
end;

procedure TUSB.AddErrors(data:string);
begin
  if length(data)>0 then
  begin
   while FErrors.Count>1000 do FErrors.Delete(0);
   FErrors.Append(DateTimeToStr(Now)+': '+data);
  end;
end;

function TUSB.FGetSerial(board:word):string;
begin
  result:='';
  if FEmulation then exit;
  if FUSBList.Count=0 then exit;
  if board>FUSBList.Count then exit;
  result:={$ifndef usegenerics}TUSBController{$endif}(FUSBList.Items[board]).Serial;
end;

function TUSB.CheckVendorProduct(const VID,PID:word):boolean;
begin
  result:=
  (
  ( (VENDORID_BASE=VID) AND (PRODUCTID_BASE=PID) )
  OR
  ( (VENDORID_ALT=VID) AND (PRODUCTID_ALT=PID) )
  );
end;

end.
