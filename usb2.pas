unit usb2;

interface

uses
  SysUtils, Classes, SyncObjs
  {$IFDEF usegenerics}
  ,fgl
  {$ENDIF}
  {$ifdef Unix}
  ,usbcontroller
  {$else}
  ,JvHidControllerClass
  {$endif}
  ;

const
  INIFILENAME = 'settings.ini';

type
  TReport = packed record
    ReportID: byte;
    Data:    array [0..15] of byte; // <-- this needs to be adapted to your report size
  end;

  TUSBController = class
    HidCtrl       : TJvHidDevice;
    FaultCounter  : word;
    Serial        : string;
    LocalDataTimer: TEvent;
    LocalData     : TReport;
    procedure   SetDataEvent(const DataEvent: TJvHidDataEvent);
    function    GetDataEvent:TJvHidDataEvent;
    procedure   ShowRead(HidDev: TJvHidDevice; ReportID: Byte;const Data: Pointer; Size: Word);
  public
    constructor Create(HidDev: TJvHidDevice);
    destructor  Destroy;override;
    property    OnData: TJvHidDataEvent read GetDataEvent write SetDataEvent;
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

    AUSBList   : TUSBList;

    FErrors    : TStringList;
    FInfo      : TStringList;
    FEmulation : boolean;

    FEnabled   : Boolean;

    MaxErrors  : word;

    FOnUSBDeviceChange: TUSBEvent;

    FIniFileFullPath:string;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;
    procedure SetEnabled(Value: Boolean);

    function  HidReadWrite(Ctrl: TUSBController; ReadOnly:boolean):boolean;

    procedure DeviceArrival(HidDev: TJvHidDevice);
    procedure DeviceRemoval(HidDev: TJvHidDevice);
    procedure DeviceChange(Sender:TObject);

    function  CheckAddressNewer(Ctrl: TUSBController):integer;
    function  CheckParameters(board:word):boolean;overload;

    procedure HandleCRCError(HidCtrl: TJvHidDevice);overload;

    function  FGetSerial(board:word):string;
  public
    constructor Create;
    destructor Destroy;override;

    property  Emulation:boolean read FEmulation;

    property  Errors:String read GetErrors;
    property  Info:String read GetInfo;

    property  Enabled: Boolean read FEnabled write SetEnabled;

    property  OnUSBDeviceChange: TUSBEvent read FOnUSBDeviceChange write FOnUSBDeviceChange;

    property  GetSerial[board: word]: string read FGetSerial;

    property Controller:TJvHidDeviceController read HidCtl;
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
  Vendor                        = $045E;
  Product                       = $0916;

  ErrorDelay                    = 100;
  USBTimeout                    = 200;

constructor TUSBController.Create(HidDev: TJvHidDevice);
begin
  Inherited Create;
  HidCtrl:=HidDev;
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
begin
  LocalData.ReportID:=ReportID;
  for x := Low(LocalData.Data) to High(LocalData.Data) do
  begin
    LocalData.Data[x]:=byte(PByte(Data)[x]);
  end;
  LocalDataTimer.SetEvent;
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
    end
    else
    begin
      if Assigned(LocalDataTimer) then LocalDataTimer.Free;
    end;
  end
  else if Assigned(LocalDataTimer) then LocalDataTimer.Free;
end;

function TUSBController.GetDataEvent: TJvHidDataEvent;
begin
  if Assigned(HidCtrl) then
  begin
    result:=HidCtrl.OnData;
  end else result:=nil;
end;


constructor TUSB.Create;
var
  Ini: TIniFile;
begin
  inherited Create;

  FIniFileFullPath:=INIFILENAME;

  FErrors:=TStringList.Create;
  FInfo:=TStringList.Create;

  AUSBList      := TUSBList.Create;

  FEmulation    := True;

  MaxErrors     := 1;

  Ini           := TIniFile.Create(FIniFileFullPath);
  try
    MaxErrors   := Ini.ReadInteger( 'General', 'NumError', MaxErrors );
  finally
    Ini.Free;
  end;

  HidCtl:=TJvHidDeviceController.Create(nil);
  // either enable this, or the other two, to detect USB device changes
  HidCtl.OnDeviceChange:=DeviceChange;
  //HidCtl.OnArrival:= DeviceArrival;
  //HidCtl.OnRemoval:= DeviceRemoval;
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

  if AUSBList.Count>0 then
  begin
    for board:=Pred(AUSBList.Count) downto 0  do
    begin
      if Assigned(AUSBList.Items[board]) then
      begin
        aController:={$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[board]);
        aHidCtrl:=aController.HidCtrl;
        aController.Destroy;
        if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
      end;
      AUSBList.Delete(board);
    end;
  end;

  AUSBList.Free;

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
    HidCtl.Enabled:=FEnabled;
  end;
end;

function TUSB.HidReadWrite(Ctrl: TUSBController; ReadOnly:boolean):boolean;
var
  error:boolean;
  Written:DWORD;
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
    error:=(NOT Ctrl.HidCtrl.WriteFile(Ctrl.LocalData, Ctrl.HidCtrl.Caps.OutputReportByteLength, Written));
    if (error) then
    begin
      {$ifdef MSWINDOWS}
      Err := GetLastError;
      {$else}
      Err := fpgeterrno;
      {$endif}
      AddErrors(Format('USB normal write error: %s (%x)', [SysErrorMessage(Err), Err]));
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
  if ((HidDev.Attributes.VendorID = Vendor) AND
      (HidDev.Attributes.ProductID = Product) ) then
  begin
    for board:=Pred(AUSBList.Count) downto 0 do
    begin
      if Assigned(AUSBList.Items[board]) then
      begin
        aController:={$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[board]);
        aHidCtrl:=aController.HidCtrl;
        //if ((Assigned(aHidCtrl)) and (NOT aHidCtrl.IsPluggedIn)) then
        if ((Assigned(aHidCtrl)) and (aHidCtrl=HidDev)) then
        begin
          if Assigned(FOnUSBDeviceChange) then
          begin
            FOnUSBDeviceChange(Self,-1*board);
          end;
          aController.Destroy;
          if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
          AUSBList.Items[board]:=nil;
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

  if ( (HidDev.Attributes.VendorID = Vendor) AND
       (HidDev.Attributes.ProductID = Product) ) then
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
        Serial:=HidDev.SerialNumber;
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

      if AUSBList.Count<(newboard+1) then
      begin
        AUSBList.Count:=newboard+1;
      end;

      if AUSBList.Items[newboard]<>nil then
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
        aHidCtrl:={$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[newboard]).HidCtrl;
        {$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[newboard]).Destroy;
        if Assigned(aHidCtrl) then HidCtl.CheckIn(aHidCtrl);
      end;


      AUSBList.Items[newboard]:=NewUSBController;

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
  while i<HidCtl.HidDevices.Count do
  begin
    HidDev:=TJvHidDevice(HidCtl.HidDevices[i]);
    s:='HID';
    {$ifdef Unix}
    if Pos('hidraw',HidDev.PhysicalDescriptor)>0 then s:='HIDraw';
    if Pos('hiddev',HidDev.PhysicalDescriptor)>0 then s:='HIDdev';
    {$endif}
    AddInfo(s+'-device#'+InttoStr(i)+'. VID: '+InttoStr(HidDev.Attributes.VendorID)+'. PID: '+InttoStr(HidDev.Attributes.ProductID)+'.');
    AddInfo('Name: '+HidDev.ProductName+'. Vendor: '+HidDev.VendorName+'.');
    if ( (HidDev.Attributes.VendorID = Vendor) AND
       (HidDev.Attributes.ProductID = Product) ) then
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
  if Assigned(FOnUSBDeviceChange) then
  begin
    FOnUSBDeviceChange(Self,0);
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
  ErrorCounter:word;

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

procedure TUSB.HandleCRCError(HidCtrl: TJvHidDevice);
begin
  begin
    //if Assigned(HidCtrl) then HidCtrl.FlushQueue;
    //if Assigned(HidCtrl) then HidCtrl.CloseFileEx(omhRead);
    //if Assigned(HidCtrl) then HidCtrl.CloseFileEx(omhWrite);
  end;
end;

function TUSB.CheckParameters(board:word):boolean;
begin
  result:=true;
  if  FEmulation then exit;
  result:=NOT ( (board<AUSBList.Count) AND (Assigned({$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[board]).HidCtrl)) );
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
  if AUSBList.Count=0 then exit;
  if board>AUSBList.Count then exit;
  result:={$ifndef usegenerics}TUSBController{$endif}(AUSBList.Items[board]).Serial;
end;

end.
