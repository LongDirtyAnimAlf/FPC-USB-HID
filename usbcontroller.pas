unit usbcontroller;

{
  TJvHidDeviceController USB HID framework linux replacement for the free pascal compiler.
  Target: Linux.
  Copyright (C) 2015 DonAlfredo
  longdirtyanimalf@gmail.com

  The Original Code is: JvHidControllerClass.PAS, released on 2001-02-28.

  The Initial Developer of the Original Code is Robert Marquardt. May he rest in peace.
  Portions created by Robert Marquardt are Copyright (C) 1999-2003 Robert Marquardt.
  All Rights Reserved.

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of this code is Alfred Glänzer.

  Portions created by the Initial Developer are Copyright (C) 2014
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****
}

{$mode objfpc}{$H+}
{$PACKRECORDS C}

{.$DEFINE udevstatic}
{.$DEFINE usegenerics}
{.$DEFINE debug}

{.$DEFINE hidraw}
{$DEFINE hiddev}

{$ifdef udevstatic}
{$LINKLIB udev}
{$endif}

{$IFDEF hiddev}
{.$DEFINE oldkernel}
{$ENDIF}

{$Q-}
{$H+}

interface

uses
  Classes,
  baseunix, unix
  {$IFDEF usegenerics}
  ,fgl
  {$ENDIF}
  ;

const
  sOK = 0;
  sErr = integer(-1);

  INVALID_HANDLE_VALUE    = THandle(-1);

  EPERM                   = 1;
  ENOENT                  = 2;
  EACCES                  = 13;

  HID_REPORT_ID_UNKNOWN   = $ffffffff;
  HID_REPORT_ID_FIRST     = $00000100;
  HID_REPORT_ID_NEXT      = $00000200;
  HID_REPORT_ID_MASK      = $000000ff;
  HID_REPORT_ID_MAX       = $000000ff;

  HID_REPORT_TYPE_INPUT   = 1;
  HID_REPORT_TYPE_OUTPUT  = 2;
  HID_REPORT_TYPE_FEATURE = 3;
  HID_REPORT_TYPE_MIN     = 1;
  HID_REPORT_TYPE_MAX     = 3;

  HID_STRING_SIZE         = 256;
  HID_MAX_MULTI_USAGES    = 1024;



type
  Pudev_handle = ^udev_handle;
  udev_handle = record
    {undefined structure}
  end;

  Pudev_device_handle = ^udev_device_handle;
  udev_device_handle = record
    {undefined structure}
  end;

  Pudev_monitor_handle = ^udev_monitor_handle;
  udev_monitor_handle = record
    {undefined structure}
  end;

  Pudev_enumerate_handle = ^udev_enumerate_handle;
  udev_enumerate_handle = record
    {undefined structure}
  end;

  Pudev_list_entry_handle = ^udev_list_entry_handle;
  udev_list_entry_handle = record
    {undefined structure}
  end;

  Phiddev_string_descriptor = ^hiddev_string_descriptor;
   hiddev_string_descriptor = record
      index:cint32;
      value:array[0..HID_STRING_SIZE-1] of char;
   end;

  Phiddev_devinfo = ^hiddev_devinfo;
   hiddev_devinfo = record
      bustype:cuint32;
      busnum:cuint32;
      devnum:cuint32;
      ifnum:cuint32;
      vendor:cint16;
      product:cint16;
      version:cint16;
      num_applications:cuint32;
   end;

  Phiddev_usage_ref = ^hiddev_usage_ref;
   hiddev_usage_ref = record
     report_type:cuint32;
     report_id:cuint32;
     field_index:cuint32;
     usage_index:cuint32;
     usage_code:cuint32;
     value:cint32;
   end;

  Phiddev_report_info = ^hiddev_report_info;
   hiddev_report_info = record
     report_type:cuint32;
     report_id:cuint32;
     num_fields:cuint32;
   end;

  Phiddev_field_info = ^hiddev_field_info;
   hiddev_field_info = record
     report_type:cuint32;
     report_id:cuint32;
     field_index:cuint32;
     maxusage:cuint32;
     flags:cuint32;
     physical:cuint32;
     logical:cuint32;
     application:cuint32;
     logical_minimum:cint32;
     logical_maximum:cint32;
     physical_minimum:cint32;
     physical_maximum:cint32;
     unit_exponent:cuint32;
     newunit:cuint32;
   end;

  Phiddev_usage_ref_multi = ^hiddev_usage_ref_multi;
   hiddev_usage_ref_multi = record
     uref:hiddev_usage_ref;
     num_values:cuint32;
     values: array[0..HID_MAX_MULTI_USAGES-1] of cint32;
   end;

  Phiddev_event = ^hiddev_event;
   hiddev_event = record
     hid:cuint32;
     value:cint32;
   end;

const

  _IO                   = 0;
  _IOW                  = 1;
  _IOR                  = 2;
  _IOWR                 = 3;

  IOCPARM_MASK = $7f;
  IOC_VOID = $20000000;
  IOC_OUT = $40000000;
  IOC_IN = $80000000;
  IOC_INOUT = IOC_IN or IOC_OUT;
  FIONREAD =cardinal( IOC_OUT or ((4 and IOCPARM_MASK) shl 16) or (102 shl 8) or 127);



  HIDIOCSUSAGE          = cuint32((_IOW shl 30) + (sizeof(hiddev_usage_ref) shl 16) + (Ord('H') shl 8) + $0C);
  HIDIOCSREPORT         = cuint32((_IOW shl 30) + (sizeof(hiddev_report_info) shl 16) + (Ord('H') shl 8) + $08);
  HIDIOCGUCODE          = cuint32((_IOWR shl 30) + (sizeof(hiddev_usage_ref) shl 16) + (Ord('H') shl 8) + $0D);
  HIDIOCGUSAGE          = cuint32((_IOWR shl 30) + (sizeof(hiddev_usage_ref) shl 16) + (Ord('H') shl 8) + $0B);
  HIDIOCGREPORTINFO     = cuint32((_IOWR shl 30) + (sizeof(hiddev_report_info) shl 16) + (Ord('H') shl 8) + $09);
  HIDIOCGFIELDINFO      = cuint32((_IOWR shl 30) + (sizeof(hiddev_field_info) shl 16) + (Ord('H') shl 8) + $0A);
  HIDIOCGDEVINFO        = cuint32((_IOR shl 30) + (sizeof(hiddev_devinfo) shl 16) + (Ord('H') shl 8) + $03);
  HIDIOCGSTRING         = cuint32((_IOR shl 30) + (sizeof(hiddev_string_descriptor) shl 16) + (Ord('H') shl 8) + $04);
  HIDIOCGREPORT         = cuint32((_IOW shl 30) + (sizeof(hiddev_report_info) shl 16) + (Ord('H') shl 8) + $07);
  HIDIOCGUSAGES         = cuint32((_IOWR shl 30) + (sizeof(hiddev_usage_ref_multi) shl 16) + (Ord('H') shl 8) + $13);
  HIDIOCSUSAGES         = cuint32((_IOW shl 30) + (sizeof(hiddev_usage_ref_multi) shl 16) + (Ord('H') shl 8) + $14);

  //HIDIOCSFEATURE(len)    = cuint32((_IOW shl 30) + ((len) shl 16) + (Ord('H') shl 8) + $06);
  //HIDIOCGFEATURE(len)    = cuint32((_IOW shl 30) + ((len) shl 16) + (Ord('H') shl 8) + $07);

type
  TJvHidDeviceController = class; // forward declaration
  TJvHidDevice = class; // forward declaration

  TJvHidPlugEvent = procedure(HidDev: TJvHidDevice) of object;
  TJvHidUnplugEvent = TJvHidPlugEvent;

  TJvHidEnumerateEvent = function(HidDev: TJvHidDevice;
    const Idx: Integer): Boolean of object;

  TJvHidDataEvent = procedure(HidDev: TJvHidDevice; ReportID: Byte;
    const Data: Pointer; Size: Word) of object;

  TJvHidDataErrorEvent = procedure(HidDev: TJvHidDevice; Error: DWORD) of object;

  // check out test function
  TJvHidCheckCallback = function(HidDev: TJvHidDevice): Boolean; stdcall;

  TJvHidPnPInfo = class(TObject)
  private
    FDeviceID: DWORD;
    FHidPath: string;
    FUSBPath: string;
    FCapabilities: DWORD;
    FClassDescr: string;
    FClassGUID: string;
    FConfigFlags: DWORD;
    FDeviceDescr: string;
    FDriver: string;
    FProductID: DWORD;
    FVendorID: DWORD;
    FFriendlyName: string;
    FMfg: string;
    FAddress: string;
    FBusNumber: DWORD;
    FBusType: string;
    FCharacteristics: string;
    FDevType: DWORD;
    FEnumeratorName: string;
    FExclusive: DWORD;
    FLegacyBusType: DWORD;
    FLocationInfo: string;
    FPhysDevObjName: string;
    FSecuritySDS: string;
    FService: string;
    FUINumber: DWORD;
    FUINumberFormat: string;
  public
    property DeviceID: DWORD read FDeviceID write FDeviceID;
    property HidPath: string read FHidPath;
    property USBPath: string read FUSBPath;

    // registry values
    property Capabilities: DWORD read FCapabilities;
    property ClassDescr: string read FClassDescr;
    property ClassGUID: string read FClassGUID;
    property ConfigFlags: DWORD read FConfigFlags;
    property DeviceDescr: string read FDeviceDescr;
    property Driver: string read FDriver;
    property VendorID: DWORD read FVendorID;
    property ProductID: DWORD read FProductID;
    property FriendlyName: string read FFriendlyName;
    property Mfg: string read FMfg;
    property Address: string read FAddress;
    property BusNumber: DWORD read FBusNumber;
    property BusType: string read FBusType;
    property Characteristics: string read FCharacteristics;
    property DevType: DWORD read FDevType;
    property EnumeratorName: string read FEnumeratorName;
    property Exclusive: DWORD read FExclusive;
    property LegacyBusType: DWORD read FLegacyBusType;
    property LocationInfo: string read FLocationInfo;
    property PhysDevObjName: string read FPhysDevObjName;
    property SecuritySDS: string read FSecuritySDS;
    property Service: string read FService;
    property UINumber: DWORD read FUINumber;
    property UINumberFormat: string read FUINumberFormat;
    constructor Create(ADeviceId:DWORD; AHidDevicePath: String; AUSBDevice:Pudev_device_handle; AUSBDevicePath: String);
    destructor Destroy; override;
  end;


  TJvHidDeviceReadThread = class(TThread)
  private
    FErr: DWORD;
    procedure DoData;
    procedure DoDataError;
    {%H-}constructor CtlCreate(const Dev: TJvHidDevice);
  protected
    procedure Execute; override;
  public
    Device: TJvHidDevice;
    NumBytesRead: longword;
    Report: array of Byte;
    constructor Create({%H-}CreateSuspended: Boolean);
  end;

  THIDPCAPS = record
    InputReportByteLength:     Word;
    OutputReportByteLength:    Word;
    FeatureReportByteLength:   Word;
  end;

  THIDDAttributes = record
    VendorID:      Word;
    ProductID:     Word;
    VersionNumber: Word;
  end;

  TJvHidDevice = class(TObject)
  private
    FHidFileHandle : THandle;
    FUSBFileHandle : THandle;
    FMyController: TJvHidDeviceController;
    FIsPluggedIn: Boolean;
    FIsCheckedOut: Boolean;
    FIsEnumerated: Boolean;
    fCaps:THIDPCaps;
    FAttributes: THIDDAttributes;
    FPnPInfo: TJvHidPnPInfo;
    FVendorName: String;
    FProductName: String;
    FSerialNumber: String;
    FPhysicalDescriptor: string;
    FLanguageStrings: TStringList;
    FPollingDelayTime: Integer;
    FThreadSleepTime: Integer;
    FData: TJvHidDataEvent;
    FDataError: TJvHidDataErrorEvent;
    FUnplug: TJvHidUnplugEvent;
    FDataThread: TJvHidDeviceReadThread;
    FTag: Integer;
    FDebugInfo: TStringList;
    function IsAccessible: Boolean;
    function GetDeviceString(Idx: Byte): String;
    function GetCaps: THIDPCaps;
    function GetAttributes: THIDDAttributes;
    function GetVendorName: String;
    function GetProductName: String;
    function GetSerialNumber: String;
    function GetPhysicalDescriptor: string;
    function GetLanguageStrings: TStrings;
    function GetDebugInfo: string;
    procedure SetDebugInfo(value:String);
    procedure SetDataEvent(const DataEvent: TJvHidDataEvent);
    procedure SetPollingDelayTime(const DelayTime: Integer);
    procedure SetThreadSleepTime(const SleepTime: Integer);
    procedure StartThread;
    procedure StopThread;
    function CanRead(Timeout: Integer): Boolean;
    function CanWrite(Timeout: Integer): Boolean;
    {%H-}constructor CtlCreate(const APnPInfo: TJvHidPnPInfo; const LocalController: TJvHidDeviceController);
  protected
    // internal event implementor
    procedure DoUnplug;
  public
    // dummy constructor
    constructor Create;
    destructor Destroy; override;
    // methods
    procedure CloseFile;
    function OpenFile: Boolean;
    function ReadFile(var Report; ToRead: DWORD; var BytesRead: DWORD): Boolean;
    function WriteFile(const Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
    function CheckOut: Boolean;
    procedure ShowReports(report_type:word);
    property HidFileHandle:THandle read FHidFileHandle;
    property USBFileHandle:THandle read FUSBFileHandle;
    property Attributes: THIDDAttributes read GetAttributes;
    property Caps: THIDPCaps read GetCaps;
    property IsCheckedOut: Boolean read FIsCheckedOut;
    property IsPluggedIn: Boolean read FIsPluggedIn;
    property LanguageStrings: TStrings read GetLanguageStrings;
    property DebugInfo: String read GetDebugInfo write SetDebugInfo;
    property PhysicalDescriptor: string read GetPhysicalDescriptor;
    property PnPInfo: TJvHidPnPInfo read FPnPInfo;
    property VendorName: String read GetVendorName;
    property ProductName: String read GetProductName;
    property SerialNumber: String read GetSerialNumber;
    property Tag: Integer read FTag write FTag;
    property PollingDelayTime: Integer read FPollingDelayTime write SetPollingDelayTime;
    property ThreadSleepTime: Integer read FThreadSleepTime write SetThreadSleepTime;
    property DeviceStrings[Idx: Byte]: string read GetDeviceString;
    property OnData: TJvHidDataEvent read FData write SetDataEvent;
    property OnDataError: TJvHidDataErrorEvent read FDataError write FDataError;
    property OnUnplug: TJvHidUnplugEvent read FUnplug write FUnplug;
  end;


  TJvHidDeviceControllerMonitorThread = class(TThread)
  private
    FUSBController: TJvHidDeviceController;
    fNode:string;
  protected
    procedure   Execute; override;
  public
    constructor CreateUSBChangeThread(USBController: TJvHidDeviceController);
  end;

  {$IFDEF usegenerics}
  THidDevList = specialize TFPGList<TJvHidDevice>;
  {$ELSE}
  THidDevList = TList;
  {$ENDIF}

  TJvHidDeviceController = class(TComponent)
  private
    FPriority: TThreadPriority;
    FArrivalEvent: TJvHidPlugEvent;
    FEnumerateEvent: TJvHidEnumerateEvent;
    FDeviceChangeEvent: TNotifyEvent;
    FDeviceDataError: TJvHidDataErrorEvent;
    FDevUnplugEvent: TJvHidUnplugEvent;
    FRemovalEvent: TJvHidUnplugEvent;
    FControllerMonitorThread:TJvHidDeviceControllerMonitorThread;
    FDevPollingDelayTime: Integer;
    FDevThreadSleepTime: Integer;
    FEnabled: Boolean;
    FEventPipe: TFilDes;
    FDevDataEvent: TJvHidDataEvent;
    FList: THidDevList;
    FNumCheckedInDevices: Integer;
    FNumCheckedOutDevices: Integer;
    FNumUnpluggedDevices: Integer;
    FInDeviceChange: Boolean;
    FDebugInfo: TStringList;
    function    CheckThisOut(var HidDev: TJvHidDevice; Idx: Integer; Check: Boolean): Boolean;
    procedure   SetDevPollingDelayTime(const DevTime: Integer);
    procedure   SetDevThreadSleepTime(const DevTime: Integer);
    procedure   SetEnabled(Value: Boolean );
    procedure   SetDevData(const DataEvent: TJvHidDataEvent);
    procedure   SetDeviceDataError(const DataErrorEvent: TJvHidDataErrorEvent);
    procedure   SetDeviceChangeEvent(const Notifier: TNotifyEvent);
    procedure   SetEnumerateEvent(const EnumerateEvent: TJvHidEnumerateEvent);
    procedure   SetDevUnplug(const Unplugger: TJvHidUnplugEvent);
    function    GetDebugInfo: String;
    procedure   SetDebugInfo(value:String);
  protected
    procedure   DoArrival(HidDev: TJvHidDevice);
    procedure   DoRemoval(HidDev: TJvHidDevice);
    procedure   DoDeviceChange;
    function    DoEnumerate(HidDev: TJvHidDevice; Idx: Integer): Boolean;
    procedure   StartControllerThread;
    procedure   StopControllerThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CheckIn(var HidDev: TJvHidDevice);
    function    CheckOut(var HidDev: TJvHidDevice): Boolean;
    function    CheckOutByID(var HidDev: TJvHidDevice; const Vid, Pid: Integer): Boolean;
    function    CheckOutByIndex(var HidDev: TJvHidDevice; const Idx: Integer): Boolean;
    function    CheckOutByProductName(var HidDev: TJvHidDevice; const ProductName: String): Boolean;
    function    CheckOutByVendorName(var HidDev: TJvHidDevice; const VendorName: String): Boolean;
    function    CheckOutByCallback(var HidDev: TJvHidDevice; Check: TJvHidCheckCallback): Boolean;
    // methods to count HID device objects
    function    CountByID(const Vid, Pid: Integer): Integer;
    function    CountByProductName(const ProductName: String): Integer;
    function    CountByVendorName(const VendorName: String): Integer;
    function    CountByCallback(Check: TJvHidCheckCallback): Integer;
    // iterate over the HID devices
    function    Enumerate: Integer;
    property    DebugInfo: String read GetDebugInfo write SetDebugInfo;
    property    HidDevices:THidDevList read FList;
    property    NumCheckedInDevices: Integer read FNumCheckedInDevices;
    property    NumCheckedOutDevices: Integer read FNumCheckedOutDevices;
    property    NumUnpluggedDevices: Integer read FNumUnpluggedDevices;
  published
    property    Enabled: Boolean read FEnabled write SetEnabled;
    property    DevPollingDelayTime: Integer read FDevPollingDelayTime write SetDevPollingDelayTime default 0;
    property    DevThreadSleepTime: Integer read FDevThreadSleepTime write SetDevThreadSleepTime default 100;
    property    ThreadPriority: TThreadPriority read FPriority write FPriority default tpNormal;
    property    OnDeviceData: TJvHidDataEvent read FDevDataEvent write SetDevData;
    property    OnDeviceDataError: TJvHidDataErrorEvent read FDeviceDataError write SetDeviceDataError;
    property    OnArrival: TJvHidPlugEvent read FArrivalEvent write FArrivalEvent;
    property    OnEnumerate: TJvHidEnumerateEvent read FEnumerateEvent write SetEnumerateEvent;
    property    OnDeviceChange: TNotifyEvent read FDeviceChangeEvent write SetDeviceChangeEvent;
    property    OnDeviceUnplug: TJvHidUnplugEvent read FDevUnplugEvent write SetDevUnplug;
    property    OnRemoval: TJvHidUnplugEvent read FRemovalEvent write FRemovalEvent;
    procedure   DeviceChange;
  end;


{$ifndef udevstatic}
var
{$endif}
  {$ifdef udevstatic}function {$endif}udev_new{$ifndef udevstatic}: function{$endif}:Pudev_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_unref{$ifndef udevstatic}: function{$endif}(udev:Pudev_handle):Pudev_handle;cdecl;{$ifdef udevstatic}external;{$endif}

  {$ifdef udevstatic}function {$endif}udev_enumerate_new{$ifndef udevstatic}: function{$endif}(udev:Pudev_handle):Pudev_enumerate_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_enumerate_unref{$ifndef udevstatic}: function{$endif}(udev_enumerate:Pudev_enumerate_handle):Pudev_enumerate_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_enumerate_add_match_subsystem{$ifndef udevstatic}: function{$endif}(udev_enumerate:Pudev_enumerate_handle;const subsystem:Pansichar):integer;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_enumerate_add_match_sysattr{$ifndef udevstatic}: function{$endif}(udev_enumerate:Pudev_enumerate_handle;const subsystem:Pansichar;const devtype:Pansichar):integer;cdecl;{$ifdef udevstatic}external;{$endif}

  {$ifdef udevstatic}function {$endif}udev_enumerate_scan_devices{$ifndef udevstatic}: function{$endif}(udev_enumerate:Pudev_enumerate_handle):integer;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_enumerate_get_list_entry{$ifndef udevstatic}: function{$endif}(udev_enumerate:Pudev_enumerate_handle):Pudev_list_entry_handle;cdecl;{$ifdef udevstatic}external;{$endif}

  {$ifdef udevstatic}function {$endif}udev_list_entry_get_name{$ifndef udevstatic}: function{$endif}(list_entry:Pudev_list_entry_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_list_entry_get_next{$ifndef udevstatic}: function{$endif}(list_entry:Pudev_list_entry_handle):Pudev_list_entry_handle;cdecl;{$ifdef udevstatic}external;{$endif}

  {$ifdef udevstatic}function {$endif}udev_monitor_new_from_netlink{$ifndef udevstatic}: function{$endif}(udev:Pudev_handle;const name:Pansichar):Pudev_monitor_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_monitor_receive_device{$ifndef udevstatic}: function{$endif}(mon:Pudev_monitor_handle):Pudev_device_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_monitor_filter_add_match_subsystem_devtype{$ifndef udevstatic}: function{$endif}(mon:Pudev_monitor_handle;const subsystem:Pansichar;const devtype:Pansichar):integer;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_monitor_enable_receiving{$ifndef udevstatic}: function{$endif}(mon:Pudev_monitor_handle):integer;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_monitor_get_fd{$ifndef udevstatic}: function{$endif}(mon:Pudev_monitor_handle):integer;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_monitor_unref{$ifndef udevstatic}: function{$endif}(mon:Pudev_monitor_handle):Pudev_monitor_handle;cdecl;{$ifdef udevstatic}external;{$endif}

  {$ifdef udevstatic}function {$endif}udev_device_unref{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pudev_device_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_sysattr_value{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle;const sysattr:Pansichar):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_action{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_devnode{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_devtype{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_devpath{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_syspath{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_new_from_syspath{$ifndef udevstatic}: function{$endif}(udev:Pudev_handle;const syspath:Pansichar):Pudev_device_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_parent_with_subsystem_devtype{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle;const subsystem:Pansichar;const devtype:Pansichar):Pudev_device_handle;cdecl;{$ifdef udevstatic}external;{$endif}
  {$ifdef udevstatic}function {$endif}udev_device_get_property_value{$ifndef udevstatic}: function{$endif}(dev:Pudev_device_handle;const key:Pansichar):Pansichar;cdecl;{$ifdef udevstatic}external;{$endif}

implementation

uses
  SysUtils,DynLibs;

var
  // UdevOk could be used in whole unit ... not for now ... will fail already during init when no udev-lib found .. lazy ...
  UdevOk: Boolean = True;
  {$ifndef udevstatic}
  libudev: TLibHandle = NilHandle;
  {$endif}

{$ifndef udevstatic}
procedure LoadUdevLibrary;
var
  e:Exception;
begin
  libudev:= LoadLibrary('libudev.'+SharedSuffix+'.1');
  if libudev = NilHandle then libudev:= LoadLibrary('libudev.'+SharedSuffix+'.0');
  UdevOk:= (libudev <> NilHandle);
  if UdevOk then
  try
    pointer(udev_new):= GetProcAddress(libudev, 'udev_new');
    pointer(udev_unref):= GetProcedureAddress(libudev, 'udev_unref');

    pointer(udev_enumerate_new):= GetProcedureAddress(libudev, 'udev_enumerate_new');
    pointer(udev_enumerate_unref):= GetProcedureAddress(libudev, 'udev_enumerate_unref');
    pointer(udev_enumerate_add_match_subsystem):= GetProcedureAddress(libudev, 'udev_enumerate_add_match_subsystem');
    pointer(udev_enumerate_add_match_sysattr):= GetProcedureAddress(libudev, 'udev_enumerate_add_match_sysattr');

    pointer(udev_enumerate_scan_devices):= GetProcedureAddress(libudev, 'udev_enumerate_scan_devices');
    pointer(udev_enumerate_get_list_entry):= GetProcedureAddress(libudev, 'udev_enumerate_get_list_entry');

    pointer(udev_list_entry_get_name):= GetProcedureAddress(libudev, 'udev_list_entry_get_name');
    pointer(udev_list_entry_get_next):= GetProcedureAddress(libudev, 'udev_list_entry_get_next');

    pointer(udev_monitor_new_from_netlink):= GetProcedureAddress(libudev, 'udev_monitor_new_from_netlink');
    pointer(udev_monitor_receive_device):= GetProcedureAddress(libudev, 'udev_monitor_receive_device');
    pointer(udev_monitor_filter_add_match_subsystem_devtype):= GetProcedureAddress(libudev, 'udev_monitor_filter_add_match_subsystem_devtype');
    pointer(udev_monitor_enable_receiving):= GetProcedureAddress(libudev, 'udev_monitor_enable_receiving');
    pointer(udev_monitor_get_fd):= GetProcedureAddress(libudev, 'udev_monitor_get_fd');
    pointer(udev_monitor_unref):= GetProcedureAddress(libudev, 'udev_monitor_unref');

    pointer(udev_device_unref):= GetProcedureAddress(libudev, 'udev_device_unref');
    pointer(udev_device_get_sysattr_value):= GetProcedureAddress(libudev, 'udev_device_get_sysattr_value');
    pointer(udev_device_get_action):= GetProcedureAddress(libudev, 'udev_device_get_action');
    pointer(udev_device_get_devnode):= GetProcedureAddress(libudev, 'udev_device_get_devnode');
    pointer(udev_device_get_devtype):= GetProcedureAddress(libudev, 'udev_device_get_devtype');
    pointer(udev_device_get_devpath):= GetProcedureAddress(libudev, 'udev_device_get_devpath');
    pointer(udev_device_get_syspath):= GetProcedureAddress(libudev, 'udev_device_get_syspath');
    pointer(udev_device_new_from_syspath):= GetProcedureAddress(libudev, 'udev_device_new_from_syspath');
    pointer(udev_device_get_parent_with_subsystem_devtype):= GetProcedureAddress(libudev, 'udev_device_get_parent_with_subsystem_devtype');
    pointer(udev_device_get_property_value):= GetProcedureAddress(libudev, 'udev_device_get_property_value');

  except
    on E: Exception do
    begin
      UdevOk:= False;
      UnloadLibrary(libudev);
      e:=Exception.Create('No udev library found');
      raise e;
    end;
  end;
end;

procedure FreeUdevLibrary;
begin
  if UdevOk then UnloadLibrary(libudev);
end;
{$endif}

procedure InitWithoutHint(out x);
begin
{$PUSH}
{$HINTS OFF}
  FillChar(x,sizeof(x),0);
  //ZeroMemory(@x, SizeOf(x));
{$POP}
end;


//=== { TJvHidPnPInfo } ======================================================
constructor TJvHidPnPInfo.Create(ADeviceId:DWORD; AHidDevicePath: String; AUSBDevice:Pudev_device_handle; AUSBDevicePath: String);
function HexToInt(Hex : String) : Integer;
const
  HexSymbols : String = '0123456789ABCDEF';
var
  I,J : Integer;
begin
  Hex := UpperCase(Hex);
  Result := 0;
  J := Length(Hex);
  For I := 1 to J do
    Result := Result+((Pos(Hex[J-I+1],HexSymbols)-1) shl ((I-1)*4));
end;
{
var
  device_info:hiddev_devinfo;
}
begin
  inherited Create;

  FHidPath      := AHidDevicePath;
  FUSBPath      := AUSBDevicePath;
  FDeviceID     := ADeviceId;

  FFriendlyName := udev_device_get_sysattr_value(AUSBDevice,'product');
  FMfg          := udev_device_get_sysattr_value(AUSBDevice,'manufacturer');

  FVendorID     := HexToInt(udev_device_get_sysattr_value(AUSBDevice,'idVendor'));
  FProductID    := HexToInt(udev_device_get_sysattr_value(AUSBDevice,'idProduct'));

  {
  fpioctl(cint(HidFileHandle), HIDIOCGDEVINFO, @device_info);
  FDeviceID:=device_info.devnum;
  FLegacyBusType:=device_info.bustype;
  FBusType:=InttoStr(device_info.bustype);
  FBusNumber:=device_info.busnum;
  FUINumber:=device_info.ifnum;
  }
end;

destructor TJvHidPnPInfo.Destroy;
begin
  inherited Destroy;
end;


{ TJvHidDeviceControllerMonitorThread }

constructor TJvHidDeviceControllerMonitorThread.CreateUSBChangeThread(USBController: TJvHidDeviceController);
begin
  inherited Create(True);
  FUSBController := USBController;
  FreeOnTerminate := False;
end;

procedure TJvHidDeviceControllerMonitorThread.Execute;
var
  readSet: TFDSet;
  localudev:Pudev_handle;
  localudev_device:Pudev_device_handle;
  localudev_monitor:Pudev_monitor_handle;
  buf: PChar = nil;
  fd:cint;
  fd_monitor:cint;
  {$IFDEF debug}
  Action:string;
  {$endif}
  //selectTimeout: TTimeVal;
begin
  localudev:=udev_new();
  if (localudev = nil) then
  begin
    //FUSBController.DebugInfo.append('Fatal error while creating new udev structure.');
    exit;
  end;
  //else FUSBController.DebugInfo.append('Creating new udev structure success.');

  localudev_monitor := udev_monitor_new_from_netlink(localudev, 'udev');
  if (localudev_monitor = nil) then
  begin
    {$IFDEF debug}FUSBController.DebugInfo:='Enum: fatal error while creating new udev monitor structure.';{$ENDIF}
    exit;
  end{$IFDEF debug} else FUSBController.DebugInfo:='Enum :creating new udev monitor structure success.'{$ENDIF};

  if ( (FUSBController.FEventPipe[0] = -1) OR (FUSBController.FEventPipe[1] = -1) )  then
  begin
    {$IFDEF debug}FUSBController.DebugInfo:='Enum: fatal error event pipe.';{$ENDIF}
    Exit;
  end{$IFDEF debug} else FUSBController.DebugInfo:='Enum: event pipe ok.'{$ENDIF};

  {$IFDEF hidraw}
  udev_monitor_filter_add_match_subsystem_devtype(localudev_monitor, 'hidraw', nil);
  {$ENDIF}
  {$IFDEF hiddev}
  udev_monitor_filter_add_match_subsystem_devtype(localudev_monitor, 'usbmisc', nil);
  {$IFDEF oldkernel}
  // add 'usb' for older kernels where hiddev is created under subsystem usb
  udev_monitor_filter_add_match_subsystem_devtype(localudev_monitor, 'usb', nil);
  //udev_monitor_filter_add_match_subsystem_devtype(localudev_monitor, 'usb', 'usb_device');
  {$ENDIF}
  {$ENDIF}

  udev_monitor_enable_receiving(localudev_monitor);

  fd_monitor :=cint(udev_monitor_get_fd(localudev_monitor));

  fd := FUSBController.FEventPipe[0] + 1;
  if fd_monitor >= fd
     then fd := fd_monitor + 1;

  while NOT Terminated do
  begin
    fpFD_ZERO(readSet);
    fpFD_SET(fd_monitor, readSet);
    fpFD_SET(FUSBController.FEventPipe[0], readSet);

    //selectTimeout.tv_sec:=1;
    //selectTimeout.tv_usec:=0;

    //if (fpSelect(fd + 1, @readSet, NIL, NIL, @selectTimeout) > 0) then
    if (fpSelect(fd + 1, @readSet, NIL, NIL, NIL) > 0) then
    begin
      if fpFD_ISSET(FUSBController.FEventPipe[0], readSet) = 1 then
      begin
        while FpRead(FUSBController.FEventPipe[0], buf, 1) <> -1 do;
      end;

      if fpFD_ISSET(fd_monitor, readSet) = 0 then continue;

      localudev_device := udev_monitor_receive_device(localudev_monitor);
      if(localudev_device<>nil) then
      begin
        {$IFDEF debug}
        Action:=udev_device_get_action(localudev_device);
        FUSBController.DebugInfo:='Enum action: '+Action;
        {$ENDIF}
        fNode:=udev_device_get_devnode(localudev_device);
        if not FUSBController.FInDeviceChange then
        begin
          FUSBController.FInDeviceChange := True;

          // choose one of the following
          FUSBController.DeviceChange;
          //Synchronize(@FUSBController.DeviceChange);
          FUSBController.FInDeviceChange := False;
        end;
        {$IFDEF debug}
        FUSBController.DebugInfo:='Enum devpath: '+ udev_device_get_devpath(localudev_device);
        FUSBController.DebugInfo:='Enum syspath: '+ udev_device_get_syspath(localudev_device);
        {$ENDIF}
        udev_device_unref(localudev_device);
      end
      else
      begin
        {$IFDEF debug}
        FUSBController.DebugInfo:='Enum: something went very wrong while getting new usb device !!';
        {$ENDIF}
      end;
    end;
  end;
  udev_monitor_unref(localudev_monitor);
  localudev_monitor:=nil;
end;

{ TJvHidDeviceController }

constructor TJvHidDeviceController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPriority := tpNormal;

  FDeviceChangeEvent := nil;
  FDevUnplugEvent := nil;
  FArrivalEvent := nil;
  FRemovalEvent := nil;
  FDevDataEvent := nil;

  FControllerMonitorThread:=nil;

  FNumCheckedInDevices := 0;
  FNumCheckedOutDevices := 0;
  FNumUnpluggedDevices := 0;
  DevPollingDelayTime := 0;
  FDevThreadSleepTime := 100;

  FInDeviceChange := False;

  fEnabled:=False;

  FDebugInfo := TStringList.Create;

  FList := THidDevList.Create;

  // read part of pipe
  FEventPipe[0] := -1;
  // write part of pipe
  FEventPipe[1] := -1;

  if FpPipe(FEventPipe) = 0 then
  begin
    // set read part of pipe to non-blocking
    FpFcntl(FEventPipe[0], F_SetFl, FpFcntl(FEventPipe[0], F_GetFl) or O_NONBLOCK);
    // set write part of pipe to non-blocking
    FpFcntl(FEventPipe[1], F_SetFl, FpFcntl(FEventPipe[1], F_GetFl) or O_NONBLOCK);
  end;
end;

destructor TJvHidDeviceController.Destroy;
var
  I: Integer;
  HidDev: TJvHidDevice;
begin
  StopControllerThread;

  // close read part of pipe
  if FEventPipe[0] <> -1 then
  begin
    FpClose(FEventPipe[0]);
    FEventPipe[0] := -1;
  end;

  // close write part of pipe
  if FEventPipe[1] <> -1 then
  begin
    FpClose(FEventPipe[1]);
    FEventPipe[1] := -1;
  end;

  FDeviceChangeEvent := nil;
  FDevUnplugEvent := nil;

  for I := 0 to FList.Count - 1 do
  begin
    HidDev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
    if Assigned(HidDev) then
    begin
      with HidDev do
      begin
        // set to uncontrolled
        FMyController := nil;
        if IsCheckedOut then
          DoUnplug; // pull the plug for checked out TJvHidDevices
        //else
        Free; // kill TJvHidDevices which are not checked out
      end;
    end;
  end;
  FList.Free;

  FDebugInfo.Free;

  inherited Destroy;
end;

procedure TJvHidDeviceController.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      DeviceChange;
      StartControllerThread;
    end
    else
    begin
      StopControllerThread;
    end;
  end;
end;

procedure TJvHidDeviceController.StartControllerThread;
begin
  if not (csDesigning in ComponentState) then
  begin
    if NOT Assigned(FControllerMonitorThread) then
    begin
      FControllerMonitorThread:=TJvHidDeviceControllerMonitorThread.CreateUSBChangeThread(Self);
      FControllerMonitorThread.Priority := FPriority;
      FControllerMonitorThread.Start;
    end;
  end;
end;

procedure TJvHidDeviceController.StopControllerThread;
var
  buf: Char;
begin
  if Assigned(FControllerMonitorThread) then FControllerMonitorThread.Terminate;

  // signal thread !!
  buf := #0;
  FpWrite(FEventPipe[1], buf, 1);

  if Assigned(FControllerMonitorThread) then
  begin
    FControllerMonitorThread.WaitFor;
    FControllerMonitorThread.Destroy;
    FControllerMonitorThread:=nil;
  end;
end;

procedure TJvHidDeviceController.SetDevData(const DataEvent: TJvHidDataEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if @DataEvent <> @FDevDataEvent then
  begin
    // change all OnData events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
      if @Dev.OnData = @FDevDataEvent then
        Dev.OnData := DataEvent;
    end;
    FDevDataEvent := DataEvent;
  end;
end;

procedure TJvHidDeviceController.SetEnumerateEvent(const EnumerateEvent: TJvHidEnumerateEvent);
begin
  if @FEnumerateEvent <> @EnumerateEvent then
  begin
    FEnumerateEvent := EnumerateEvent;
    {
    if not (csLoading in ComponentState) then
      DeviceChange;
    }
  end;
end;

procedure TJvHidDeviceController.SetDeviceChangeEvent(const Notifier: TNotifyEvent);
begin
  if @FDeviceChangeEvent <> @Notifier then
  begin
    FDeviceChangeEvent := Notifier;
    {
    if not (csLoading in ComponentState) then
      DeviceChange;
    }
  end;
end;

function TJvHidDeviceController.DoEnumerate(HidDev: TJvHidDevice; Idx: Integer): Boolean;
begin
  Result := False;
  if Assigned(FEnumerateEvent) then
  begin
    HidDev.FIsEnumerated := True;
    Result := FEnumerateEvent(HidDev, Idx);
    HidDev.FIsEnumerated := False;
    if not HidDev.IsCheckedOut then HidDev.CloseFile;
  end;
end;

procedure TJvHidDeviceController.DoDeviceChange;
begin
  if Assigned(FDeviceChangeEvent) then
    FDeviceChangeEvent(Self);
end;


procedure TJvHidDeviceController.DoArrival(HidDev: TJvHidDevice);
begin
  if Assigned(FArrivalEvent) then
  begin
    HidDev.FIsEnumerated := True;
    FArrivalEvent(HidDev);
    HidDev.FIsEnumerated := False;
  end;
end;

procedure TJvHidDeviceController.DoRemoval(HidDev: TJvHidDevice);
begin
  if Assigned(FRemovalEvent) then
  begin
    HidDev.FIsEnumerated := True;
    FRemovalEvent(HidDev);
    HidDev.FIsEnumerated := False;
  end;
end;

procedure TJvHidDeviceController.SetDeviceDataError(const DataErrorEvent: TJvHidDataErrorEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if (TMethod(DataErrorEvent).Code <> TMethod(FDeviceDataError).Code) or
     (TMethod(DataErrorEvent).Data <> TMethod(FDeviceDataError).Data) then
  begin
    // change all OnDataError events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
      if (TMethod(Dev.OnDataError).Code = TMethod(FDeviceDataError).Code) and
         (TMethod(Dev.OnDataError).Data = TMethod(FDeviceDataError).Data) then
        Dev.OnDataError := DataErrorEvent;
    end;
    FDeviceDataError := DataErrorEvent;
  end;
end;


procedure TJvHidDeviceController.SetDevUnplug(const Unplugger: TJvHidUnplugEvent);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if @Unplugger <> @FDevUnplugEvent then
  begin
    // change all OnUnplug events with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
      if @Dev.OnUnplug = @FDevUnplugEvent then
        Dev.OnUnplug := Unplugger;
    end;
    FDevUnplugEvent := Unplugger;
  end;
end;

function TJvHidDeviceController.Enumerate: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TJvHidDevice(FList[I]).IsPluggedIn then
    begin
      Inc(Result);
      if not DoEnumerate(TJvHidDevice(FList[I]), I) then
        Break;
    end;
end;

function TJvHidDeviceController.CheckThisOut(var HidDev: TJvHidDevice; Idx: Integer; Check: Boolean): Boolean;
begin
  Result := Check and not {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[Idx]).IsCheckedOut;
  if Result then
  begin
    HidDev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList[Idx]);
    HidDev.FIsCheckedOut := True;
    Inc(FNumCheckedOutDevices);
    Dec(FNumCheckedInDevices);
    HidDev.StartThread;
  end;
end;

// method CheckOutByProductName hands out the first HidDevice with a matching ProductName

function TJvHidDeviceController.CheckOutByProductName(var HidDev: TJvHidDevice;
  const ProductName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  if ProductName <> '' then
    for I := 0 to FList.Count - 1 do
    begin
      Result := CheckThisOut(HidDev, I, ProductName = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).ProductName);
      if Result then
        Break;
    end;
end;

// method CheckOutByVendorName hands out the first HidDevice with a matching VendorName

function TJvHidDeviceController.CheckOutByVendorName(var HidDev: TJvHidDevice;
  const VendorName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  if VendorName <> '' then
    for I := 0 to FList.Count - 1 do
    begin
      Result := CheckThisOut(HidDev, I, VendorName = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).VendorName);
      if Result then
        Break;
    end;
end;

// method CheckOutByCallback hands out the first HidDevice which is accepted by the Check function
// only checked in devices are presented to the Check function
// the device object is usable like during Enumerate


function TJvHidDeviceController.CheckOutByCallback(var HidDev: TJvHidDevice;
  Check: TJvHidCheckCallback): Boolean;
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]);
    if not Dev.IsCheckedOut then
    begin
      Dev.FIsEnumerated := True;
      Result := CheckThisOut(HidDev, I, Check(Dev));
      Dev.FIsEnumerated := False;
      if not Result then
      begin
        Dev.CloseFile;
      end;
      if Result then
        Break;
    end;
  end;
end;


// method CheckOutByID hands out the first HidDevice with a matching VendorID and ProductID
// Pid = -1 matches all ProductIDs

function TJvHidDeviceController.CheckOutByID(var HidDev: TJvHidDevice;
  const Vid, Pid: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Result := CheckThisOut(HidDev, I, (Vid = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).Attributes.VendorID) and
      ((Pid = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).Attributes.ProductID) or (Pid = -1)));
    if Result then
      Break;
  end;
end;

// method CheckOutByIndex hands out the HidDevice in the list with the named index
// this is mainly for check out during OnEnumerate

function TJvHidDeviceController.CheckOutByIndex(var HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
begin
  Result := False;
  HidDev := nil;
  if (Idx >= 0) and (Idx < FList.Count) then
    Result := CheckThisOut(HidDev, Idx, True);
end;

function TJvHidDeviceController.CountByID(const Vid, Pid: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).IsPluggedIn and
      (Vid = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).Attributes.VendorID) and
      ((Pid = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).Attributes.ProductID) or (Pid = -1)) then
      Inc(Result);
end;

function TJvHidDeviceController.CountByProductName(const ProductName: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).IsPluggedIn and
      (ProductName = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).ProductName) then
      Inc(Result);
end;

function TJvHidDeviceController.CountByVendorName(const VendorName: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).IsPluggedIn and
      (VendorName = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).VendorName) then
      Inc(Result);
end;

function TJvHidDeviceController.CountByCallback(Check: TJvHidCheckCallback): Integer;
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
  begin
    if {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]).IsPluggedIn then
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList[I]);
      Dev.FIsEnumerated := True;
      if Check(Dev) then
        Inc(Result);
      Dev.FIsEnumerated := False;
      if not Dev.IsCheckedOut then
      begin
        Dev.CloseFile;
      end;
    end;
  end;
end;


// method CheckOut simply hands out the first available HidDevice in the list

function TJvHidDeviceController.CheckOut(var HidDev: TJvHidDevice): Boolean;
var
  I: Integer;
begin
  Result := False;
  HidDev := nil;
  for I := 0 to FList.Count - 1 do
  begin
    Result := CheckThisOut(HidDev, I, True);
    if Result then
      Break;
  end;
end;


procedure TJvHidDeviceController.SetDevPollingDelayTime(const DevTime: Integer);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if DevTime <> FDevPollingDelayTime then
  begin
    // change all DevPollingDelayTime with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
      if Dev.PollingDelayTime = FDevPollingDelayTime then
        Dev.PollingDelayTime := DevTime;
    end;
    FDevPollingDelayTime := DevTime;
  end;
end;


procedure TJvHidDeviceController.SetDevThreadSleepTime(const DevTime: Integer);
var
  I: Integer;
  Dev: TJvHidDevice;
begin
  if DevTime <> FDevThreadSleepTime then
  begin
    // change all DevThreadSleepTime with the same old value
    for I := 0 to FList.Count - 1 do
    begin
      Dev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
      if Dev.ThreadSleepTime = FDevThreadSleepTime then
        Dev.ThreadSleepTime := DevTime;
    end;
    FDevThreadSleepTime := DevTime;
  end;
end;



procedure TJvHidDeviceController.DeviceChange;
var
  I: Integer;
  J: Integer;
  HidDev: TJvHidDevice;
  Changed: Boolean;
  NewList: THidDevList;

  // internal worker function to find all HID devices and create their objects

  procedure FillInList;
  var
    Devn: Integer;
    HidDev: TJvHidDevice;
    path:Pchar;
    LocalRawNode,LocalUSBNode:string;
    LocalDeviceId:DWORD;
    localudev:Pudev_handle;
    localudev_enumerate:Pudev_enumerate_handle;
    localudev_list_entry:Pudev_list_entry_handle;
    localudev_rawdevice:Pudev_device_handle;
    localudev_intfdevice:Pudev_device_handle;
    localudev_usbdevice:Pudev_device_handle;
    localudev_hiddevice:Pudev_device_handle;
    {$IFDEF debug}
    LocalHidNode,LocalInterfaceNode:string;
    uevent:tstringlist;
    ueventcounter:integer;
    {$ENDIF}
    //Info : TSearchRec;
    //SysPath,InterfacePath:string;
    PnPInfo: TJvHidPnPInfo;
  begin
    // Get a handle for the Plug and Play node and request currently active HID devices

    LocalRawNode:='Empty';
    LocalUSBNode:='Empty';

    {$IFDEF debug}
    LocalHidNode:='Empty';
    LocalInterfaceNode:='Empty';
    {$ENDIF}

    localudev:=udev_new();
    if (localudev = nil) then Exit;
    Devn := 0;

    localudev_enumerate := udev_enumerate_new(localudev);

    {$IFDEF hidraw}
    udev_enumerate_add_match_subsystem(localudev_enumerate, 'hidraw');
    {$ENDIF}

    {$IFDEF hiddev}
    udev_enumerate_add_match_subsystem(localudev_enumerate, 'usbmisc');
    {$IFDEF oldkernel}
    // 'usb' added for old kernels
    udev_enumerate_add_match_subsystem(localudev_enumerate, 'usb');
    {$ENDIF}
    {$ENDIF}

    udev_enumerate_scan_devices(localudev_enumerate);

    localudev_list_entry := udev_enumerate_get_list_entry(localudev_enumerate);

    while localudev_list_entry<>nil do
    begin
      path := udev_list_entry_get_name(localudev_list_entry);
      localudev_rawdevice := udev_device_new_from_syspath(localudev, path);

      if(localudev_rawdevice<>nil) then
      begin

        LocalRawNode:=udev_device_get_devnode(localudev_rawdevice);

        {$IFDEF debug}
        DebugInfo:='Found raw node: '+LocalRawNode;
        DebugInfo:='HID_PHYS_1: '+udev_device_get_property_value(localudev_rawdevice, 'HID_PHYS');
        DebugInfo:='HID_ID_1: '+udev_device_get_property_value(localudev_rawdevice, 'HID_ID');
        {$ENDIF}
        localudev_hiddevice := udev_device_get_parent_with_subsystem_devtype(
        		       localudev_rawdevice,
        		       'hid',
        		       nil);
        if(localudev_hiddevice<>nil) then
        begin
          {$IFDEF debug}
          LocalHidNode:=udev_device_get_devnode(localudev_hiddevice);
          DebugInfo:='Found hid node: '+LocalHidNode;
          DebugInfo:='HID_PHYS_2: '+udev_device_get_property_value(localudev_hiddevice, 'HID_PHYS');
          DebugInfo:='HID_ID_2: '+udev_device_get_property_value(localudev_hiddevice, 'HID_ID');
          {$ENDIF}

          {$IFDEF debug}
          uevent:=tstringlist.Create;
          uevent.Text:=udev_device_get_sysattr_value(localudev_hiddevice, 'uevent');
          for ueventcounter:=0 to uevent.Count-1 do
          begin
            DebugInfo:='Hid node uevent'+InttoStr(ueventcounter)+': '+uevent[ueventcounter];
          end;
          uevent.Free;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF debug}
          DebugInfo:='Found no hid node !!';
          {$ENDIF}
          localudev_hiddevice:=localudev_rawdevice;
        end;

        localudev_intfdevice := udev_device_get_parent_with_subsystem_devtype(
                      		localudev_hiddevice,
        		        'usb',
        		        'usb_interface');

        if(localudev_intfdevice<>nil) then
        begin
          {$IFDEF debug}
          LocalInterfaceNode:=udev_device_get_devnode(localudev_intfdevice);
          DebugInfo:='Found interface node: '+LocalInterfaceNode;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF debug}
          DebugInfo:='Found no interface node !!';
          {$ENDIF}
          localudev_intfdevice:=localudev_hiddevice;
        end;
        localudev_usbdevice := udev_device_get_parent_with_subsystem_devtype(
                             localudev_intfdevice,
      	                     'usb',
          		     'usb_device');
        if(localudev_usbdevice<>nil) then
        begin

          LocalUSBNode:=udev_device_get_devnode(localudev_usbdevice);
          {$IFDEF debug}
          DebugInfo:='Found USB node: '+LocalUSBNode;
          DebugInfo:='Inside VendorID: '+udev_device_get_sysattr_value(localudev_usbdevice,'idVendor');
          DebugInfo:='Inside ProductID: '+udev_device_get_sysattr_value(localudev_usbdevice,'idProduct');
          DebugInfo:='Inside manufacturer: '+udev_device_get_sysattr_value(localudev_usbdevice,'manufacturer');
          DebugInfo:='Inside product: '+udev_device_get_sysattr_value(localudev_usbdevice,'product');
          DebugInfo:='Inside serial: '+udev_device_get_sysattr_value(localudev_usbdevice,'serial');
          DebugInfo:='Inside bcdDevice: '+udev_device_get_sysattr_value(localudev_usbdevice,'bcdDevice');
          DebugInfo:='Inside devnum: '+udev_device_get_sysattr_value(localudev_usbdevice,'devnum');
          DebugInfo:='Inside dev: '+udev_device_get_sysattr_value(localudev_usbdevice,'dev');
          DebugInfo:='Inside urbnum: '+udev_device_get_sysattr_value(localudev_usbdevice,'urbnum');
          DebugInfo:='Class: '+udev_device_get_sysattr_value(localudev_usbdevice,'bInterfaceClass');
          DebugInfo:='Syspath: '+udev_device_get_syspath(localudev_usbdevice);
          {$ENDIF}

          LocalDeviceId:=(StrToIntDef(udev_device_get_sysattr_value(localudev_usbdevice,'busnum'),random(255)) SHL 8)
                         +
                         (StrToIntDef(udev_device_get_sysattr_value(localudev_usbdevice,'devnum'),random(255)));
          {
          InterfacePath:='';
          SysPath:=udev_device_get_syspath(localudev_usbdevice);
          if FindFirst (SysPath+'/*',faDirectory,Info)=0 then
          begin
            Repeat
              if (Info.Name[1] in ['0'..'9']) AND ((Info.Attr and faDirectory) = faDirectory) then
              begin
                if FileExists(SysPath+'/'+Info.Name+'/bInterfaceClass') then
                begin
                  InterfacePath:=SysPath+'/'+Info.Name;
                  DebugInfo:='Found: '+InterfacePath+'/bInterfaceClass';
                  break;
                end;
              end;
            Until FindNext(info)<>0;
          end;
          FindClose(Info);

          if InterfacePath<>'' then
          begin
            localudev_intfdevice := udev_device_new_from_syspath(localudev, pchar(InterfacePath));
            DebugInfo:='Interface Class: '+udev_device_get_sysattr_value(localudev_intfdevice, 'bInterfaceClass');
          end;
          }
        end;

        {$IFDEF hidraw}
        if Pos('hidraw',LocalRawNode)>0 then
        {$ENDIF}
        {$IFDEF hiddev}
        if Pos('hiddev',LocalRawNode)>0 then
        {$ENDIF}
        begin
          {$IFDEF debug}
          DebugInfo:='Adding correct hid device !!';
          {$ENDIF}
          PnPInfo := TJvHidPnPInfo.Create(LocalDeviceId,LocalRawNode,localudev_usbdevice,LocalUSBNode);
          HidDev := TJvHidDevice.CtlCreate(PnPInfo, Self);
          //HidDev.FVendorName   := udev_device_get_sysattr_value(localudev_usbdevice,'manufacturer');
          //HidDev.FProductName  := udev_device_get_sysattr_value(localudev_usbdevice,'product');
          //HidDev.FSerialNumber := udev_device_get_sysattr_value(localudev_usbdevice,'serial');
          NewList.Add(HidDev);
          Inc(Devn);
        end;

      end;

      localudev_list_entry := udev_list_entry_get_next(localudev_list_entry);

    end;
    udev_enumerate_unref(localudev_enumerate);
    udev_unref(localudev);
  end;

begin
  Changed:=False;
  // get new device list
  NewList := THidDevList.Create;

  FillInList;

  // unplug devices in FList which are not in NewList
  for I := FList.Count - 1 downto 0 do
  begin
    HidDev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
    for J := NewList.Count - 1 downto 0 do
      if (TJvHidDevice(NewList.Items[J]).PnPInfo.DeviceID = HidDev.PnPInfo.DeviceID) and
        HidDev.IsPluggedIn then
      begin
        HidDev := nil;
        Break;
      end;
    if HidDev <> nil then
    begin
      HidDev.DoUnplug;
      DoRemoval(HidDev);
      // delete from list
      if not HidDev.IsCheckedOut then
      begin
        FList.Delete(I);
        HidDev.Free;
      end;
      Changed := True;
    end;
  end;

  // delete devices from NewList which are in FList
  for I := 0 to NewList.Count - 1 do
  begin
    HidDev:={$ifndef usegenerics}TJvHidDevice{$endif}(NewList[I]);
    for J := 0 to FList.Count - 1 do
      if (HidDev.PnPInfo.DeviceID = {$ifndef usegenerics}TJvHidDevice{$endif}(FList[J]).PnPInfo.DeviceID) and
        {$ifndef usegenerics}TJvHidDevice{$endif}(FList[J]).IsPluggedIn then
      begin
        HidDev.FMyController := nil; // prevent Free/Destroy from accessing this controller
        HidDev.Free;
        HidDev:=nil;
        NewList[I]:=nil;
        Break;
      end;
  end;

  // add the remains in NewList to FList
  for I := 0 to NewList.Count - 1 do
  begin
    if NewList[I] <> nil then
    begin
      HidDev:=TJvHidDevice(NewList[I]);
      FList.Add(HidDev);
      Changed := True;
      DoArrival(HidDev);
    end;
  end;

  // throw away helper list
  NewList.Free;

  // recount the devices
  FNumCheckedInDevices := 0;
  FNumCheckedOutDevices := 0;
  FNumUnpluggedDevices := 0;
  for I := 0 to FList.Count - 1 do
  begin
    HidDev := {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]);
    Inc(FNumCheckedInDevices, Ord(not HidDev.IsCheckedOut));
    Inc(FNumCheckedOutDevices, Ord(HidDev.IsCheckedOut));
    Inc(FNumUnpluggedDevices, Ord(not HidDev.IsPluggedIn));
  end;
  FNumCheckedOutDevices := FNumCheckedOutDevices - FNumUnpluggedDevices;

  if Changed then
    DoDeviceChange;
end;


procedure TJvHidDeviceController.CheckIn(var HidDev: TJvHidDevice);
begin
  if HidDev <> nil then
  begin
    HidDev.StopThread;
    HidDev.CloseFile;

    if HidDev.IsPluggedIn then
    begin
      HidDev.FIsCheckedOut := False;
      Dec(FNumCheckedOutDevices);
      Inc(FNumCheckedInDevices);
    end
    else
      HidDev.Free;
    HidDev := nil;
  end;
end;

function TJvHidDeviceController.GetDebugInfo: string;
begin
  Result := FDebugInfo.Text;
  FDebugInfo.Clear;
end;

procedure TJvHidDeviceController.SetDebugInfo(value:string);
begin
  FDebugInfo.Append(value);
end;


constructor TJvHidDeviceReadThread.CtlCreate(const Dev: TJvHidDevice);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  Device := Dev;
  NumBytesRead := 0;
  Finalize(Report);
  if (Device.Caps.InputReportByteLength>0) then
  begin
  SetLength(Report, Device.Caps.InputReportByteLength);
  {
  //Empty readbuffer
  while Device.CanRead(Device.ThreadSleepTime) do
  begin
    Device.ReadFile(Report,Device.Caps.InputReportByteLength,{%H-}BytesRead);
  end;
}
  FillChar(Report[0], Device.Caps.InputReportByteLength, #0);
  end else Terminate;
  Start;
end;

constructor TJvHidDeviceReadThread.Create(CreateSuspended: Boolean);
begin
  // direct creation of thread not allowed !!
end;

procedure TJvHidDeviceReadThread.DoData;
begin
  Device.OnData(Device, Report[0], @Report[1], NumBytesRead);
end;

procedure TJvHidDeviceReadThread.DoDataError;
begin
  if Assigned(Device.FDataError) then
     Device.FDataError(Device, FErr);
end;

procedure TJvHidDeviceReadThread.Execute;
var
  readSet: TFDSet;
  receiveBuffer: array[0..63] of hiddev_event;
  fd,ret:cint;
  i:word;
  selectTimeout: TTimeVal;
begin
  if (Device.HidFileHandle=INVALID_HANDLE_VALUE)  then exit;

  fd :=cint(Device.HidFileHandle);

  selectTimeout.tv_sec:= (Device.ThreadSleepTime DIV 1000);
  selectTimeout.tv_usec:= ((Device.ThreadSleepTime MOD 1000) * 1000);

  try
    while not Terminated do
    begin
      fpFD_ZERO(readSet);
      fpFD_SET(fd, readSet);

      IF fpSelect(fd+1, @readSet, NIL, NIL, @selectTimeout) > 0 THEN
      begin
        if fpFD_ISSET(fd, readSet) = 0 then continue; // we had a timeout : go on with wait for data !

        InitWithoutHint(receiveBuffer);
        FillChar(Report[1], Device.Caps.InputReportByteLength-1, #0);

        ret:= fpRead(cint(Device.HidFileHandle), receiveBuffer, sizeof(hiddev_event)*(Device.Caps.InputReportByteLength-1));

        if ret>0 then
        begin

          if not Terminated then
          begin

            NumBytesRead := (ret DIV sizeof(hiddev_event));
            // the below should not be necessary, but just to be absolutely sure !!
            if (NumBytesRead>(Device.Caps.InputReportByteLength-1)) then NumBytesRead:=(Device.Caps.InputReportByteLength-1);

            if (NumBytesRead > 0) then
            begin
              // copy data bytes
              for i := 0 to (NumBytesRead-1) do Report[i+1]:=receiveBuffer[i].value;
              // choose one of the below to signal the availability of data
              DoData;
              //Synchronize(@DoData);
              //Queue(@DoData);
            end;

            //to prevent CPU burning ... for compatibility with JvHidControllerClass
            if Device.PollingDelayTime > 0 then  // Throttle device polling
              SysUtils.Sleep(Device.PollingDelayTime);

          end;
        end
        else
        begin
          if (Not Terminated) AND (ret<0) then
          begin
            FErr := fpGetErrno;
            if (FErr<>ESysEAGAIN) AND (FErr<>ESysEINPROGRESS) then
            begin
              DoDataError;
              //Synchronize(@DoDataError);
              //Queue(@DoDataError);
              SysUtils.Sleep(Device.ThreadSleepTime);
            end;
          end;
        end;
      end;
    end;
  finally
    Finalize(Report);
  end;
end;

procedure TJvHidDevice.SetPollingDelayTime(const DelayTime: Integer);
begin
  // limit to 0 sec .. 10 sec
  if (DelayTime >= 0) and (DelayTime <= 10000) then
    FPollingDelayTime := DelayTime;
end;

procedure TJvHidDevice.SetThreadSleepTime(const SleepTime: Integer);
begin
  // limit to 10 msec .. 10 sec
  if (SleepTime >= 10) and (SleepTime <= 10000) then
    FThreadSleepTime := SleepTime;
end;


function TJvHidDevice.GetDeviceString(Idx: Byte): string;
var
  Hstring1:hiddev_string_descriptor;
function ArrayToString(const a: array of Char): string;
var
  i:word;
begin
  if Length(a)>0 then
  begin
    i:=0;
    while ((a[i]<>chr(0)) AND (i<Length(a))) do Inc(i);
    SetString(Result, PChar(@a[0]), i)
  end
  else
    Result := '';
end;
begin
  Result := '';
  if Idx > 0 then if OpenFile then
  begin
    Hstring1.index:=Idx;
    Hstring1.value[0]:=chr(0);
    fpioctl(cint(HidFileHandle), HIDIOCGSTRING, @Hstring1);
    Result:=ArrayToString(Hstring1.value);
  end;
end;

function TJvHidDevice.GetVendorName: String;
begin
  if FVendorName = '' then
  begin
    FVendorName := GetDeviceString(1);
  end;
  Result := FVendorName;
end;

function TJvHidDevice.GetProductName: String;
begin
  if FProductName = '' then
  begin
    FProductName := GetDeviceString(2);
  end;
  Result := FProductName;
end;

function TJvHidDevice.GetSerialNumber: String;
begin
  if FSerialNumber = '' then
  begin
    FSerialNumber:=GetDeviceString(3);
  end;
  Result := FSerialNumber;
end;

function TJvHidDevice.GetPhysicalDescriptor: string;
begin
  if Length(FPhysicalDescriptor) = 0 then
  begin
    FPhysicalDescriptor:=Self.PnPInfo.HidPath;
  end;
  Result := FPhysicalDescriptor;
end;

function TJvHidDevice.GetLanguageStrings: TStrings;
begin
  if FLanguageStrings.Count = 0 then
  begin

  end;
  Result := FLanguageStrings;
end;

function TJvHidDevice.GetDebugInfo: string;
begin
  Result := FDebugInfo.Text;
  FDebugInfo.Clear;
end;

procedure TJvHidDevice.SetDebugInfo(value:string);
begin
  fDebugInfo.Append(value);
end;


procedure TJvHidDevice.SetDataEvent(const DataEvent: TJvHidDataEvent);
begin
  // this assignment is a bit tricky because a thread may be running
  // kill the thread with the old event still in effect
  if not Assigned(DataEvent) then
    StopThread;
  // assign the new event and start the thread if needed
  FData := DataEvent;
  StartThread;
end;

procedure TJvHidDevice.StartThread;
begin
  if Assigned(FData) and IsPluggedIn and IsCheckedOut and
     not Assigned(FDataThread) then
  begin
    FDataThread := TJvHidDeviceReadThread.CtlCreate(Self);
  end;
end;

procedure TJvHidDevice.StopThread;
begin
  if Assigned(FDataThread) then
  begin
    FDataThread.Terminate;
    FDataThread.WaitFor;
    FDataThread.Free;
    FDataThread := nil;
  end;
end;

constructor TJvHidDevice.CtlCreate(const APnPInfo: TJvHidPnPInfo; const LocalController: TJvHidDeviceController);
begin
  inherited Create;

  FHidFileHandle := INVALID_HANDLE_VALUE;
  FUSBFileHandle := INVALID_HANDLE_VALUE;

  FPnPInfo := APnPInfo;
  FMyController := LocalController;
  FIsPluggedIn := True;
  FIsCheckedOut := False;
  FIsEnumerated := False;
  FVendorName := '';
  FProductName := '';
  FPhysicalDescriptor:='';
  FSerialNumber := '';
  FLanguageStrings := TStringList.Create;
  FDebugInfo := TStringList.Create;
  FThreadSleepTime := 100;
  FDataThread := nil;

  FillChar(fCaps, SizeOf(THIDPCaps), #0);

  FillChar(FAttributes, SizeOf(THIDDAttributes), #0);
  FAttributes.VendorID:=FPnPInfo.VendorID;
  FAttributes.ProductID:=FPnPInfo.ProductID;

  OnData := FMyController.OnDeviceData;
  OnUnplug := FMyController.OnDeviceUnplug;
  OnDataError := FMyController.OnDeviceDataError;

  OpenFile;
  CloseFile;
end;


// dummy constructor to catch invalid Create calls
constructor TJvHidDevice.Create;
begin
  inherited Create;
  FHidFileHandle := INVALID_HANDLE_VALUE;
  FUSBFileHandle := INVALID_HANDLE_VALUE;
  //raise EControllerError.CreateRes(@RsEDirectHidDeviceCreationNotAllowed);
end;

destructor TJvHidDevice.Destroy;
var
  I: Integer;
  TmpOnData: TJvHidDataEvent;
  TmpOnUnplug: TJvHidUnplugEvent;
  Dev: TJvHidDevice;
begin
  // if we need to clone the object
  TmpOnData := OnData;
  TmpOnUnplug := OnUnplug;
  // to prevent strange problems
  OnData := nil;
  OnUnplug := nil;

  // free the data which needs special handling
  CloseFile;

  FDebugInfo.Free;
  FLanguageStrings.Free;

  if FMyController <> nil then
    with FMyController do
    begin
      // delete device from controller list
      for I := 0 to FList.Count - 1 do
        if  {$ifndef usegenerics}TJvHidDevice{$endif}(FList.Items[I]) = Self then
        begin
          // if device is plugged in create a checked in copy
          if IsPluggedIn then
          begin
            Dev := nil;
            try
              Dev := TJvHidDevice.CtlCreate(FPnPInfo, FMyController);
              // make it a complete clone
              Dev.OnData := TmpOnData;
              Dev.OnUnplug := TmpOnUnplug;
              Dev.PollingDelayTime := PollingDelayTime;
              Dev.ThreadSleepTime := ThreadSleepTime;
              FList.Items[I] := Dev;
              // the FPnPInfo has been handed over to the new object
              FPnPInfo := nil;
              if IsCheckedOut then
              begin
                Dec(FNumCheckedOutDevices);
                Inc(FNumCheckedInDevices);
              end;
            except
              //on E:EControllerError do
              begin
                FList.Delete(I);
                Dev.Free;
                Dec(FNumUnpluggedDevices);
              end;
            end;
          end
          else
          begin
            FList.Delete(I);
            Dec(FNumUnpluggedDevices);
          end;
          Break;
        end;
    end;
  FPnPInfo.Free;
  inherited Destroy;
end;


function TJvHidDevice.IsAccessible: Boolean;
begin
  Result := IsPluggedIn and (IsCheckedOut or FIsEnumerated);
end;


function TJvHidDevice.GetCaps: THIDPCaps;
var
  finfo_out:hiddev_field_info;
begin
  if Openfile then
  begin

    if fCaps.OutputReportByteLength=0 then
    begin
      finfo_out.report_type := HID_REPORT_TYPE_OUTPUT;
      finfo_out.report_id   := HID_REPORT_ID_FIRST;
      finfo_out.field_index := 0;
      if ((fpioctl(cint(HidFileHandle), HIDIOCGFIELDINFO, @finfo_out))>=0)
         then fCaps.OutputReportByteLength:=finfo_out.maxusage+1;
    end;

    if fCaps.InputReportByteLength=0 then
    begin
      finfo_out.report_type := HID_REPORT_TYPE_INPUT;
      finfo_out.report_id   := HID_REPORT_ID_FIRST;
      finfo_out.field_index := 0;
      if ((fpioctl(cint(HidFileHandle), HIDIOCGFIELDINFO, @finfo_out))>=0)
         then fCaps.InputReportByteLength:=finfo_out.maxusage+1;
    end;

    if fCaps.FeatureReportByteLength=0 then
    begin
      finfo_out.report_type := HID_REPORT_TYPE_FEATURE;
      finfo_out.report_id   := HID_REPORT_ID_FIRST;
      finfo_out.field_index := 0;
      if ((fpioctl(cint(HidFileHandle), HIDIOCGFIELDINFO, @finfo_out))>=0)
         then fCaps.FeatureReportByteLength:=finfo_out.maxusage+1;
    end;

  end;
  Result:=fCaps;
end;

function TJvHidDevice.GetAttributes: THIDDAttributes;
var
  device_info:hiddev_devinfo;
begin
  if Openfile then
  begin
    fpioctl(cint(HidFileHandle), HIDIOCGDEVINFO, @device_info);
    if FAttributes.VendorID=0 then FAttributes.VendorID:=device_info.vendor;
    if FAttributes.ProductID=0 then FAttributes.ProductID:=device_info.product;
    if FAttributes.VersionNumber=0 then FAttributes.VersionNumber:=device_info.version;
  end;
  Result:=FAttributes;
end;

function TJvHidDevice.CheckOut: Boolean;
begin
  Result := Assigned(FMyController) and IsPluggedIn and not IsCheckedOut;
  if Result then
  begin
    FIsCheckedOut := True;
    Inc(FMyController.FNumCheckedOutDevices);
    Dec(FMyController.FNumCheckedInDevices);
    StartThread;
  end;
end;


function TJvHidDevice.OpenFile: Boolean;
//var
//  device_info:hiddev_devinfo;
begin
  if PnPInfo.HidPath='' then
  begin
    FHidFileHandle := INVALID_HANDLE_VALUE;
    Result:=False;
    {$IFDEF debug}
    DebugInfo:='HidPath empty';
    {$ENDIF}
    exit;
  end;

  // check if open allowed (propagates this state)
  if IsAccessible then
    if HidFileHandle = INVALID_HANDLE_VALUE then // if not already opened
    begin
      fHidFileHandle := THandle(fpOpen(PnPInfo.HidPath, O_RDWR)); //O_NONBLOCK
      if HidFileHandle = INVALID_HANDLE_VALUE then
         fHidFileHandle := THandle(fpOpen(PnPInfo.HidPath, O_RDONLY));
      if HidFileHandle <> INVALID_HANDLE_VALUE then
      begin
        //fpioctl(cint(HidFileHandle), HIDIOCGDEVINFO, @device_info);
        //PNPInfo.DeviceID:=device_info.devnum;
        //FNumInputBuffers := 0;
      end
      else
      begin
       {$IFDEF debug}
        DebugInfo:='fHidFileHandle error : ' + InttoStr(fpgeterrno);
        if fpgeterrno=13 then DebugInfo:='Check ' + PnPInfo.HidPath + ' permission !!!';
        {$ENDIF}
      end;
    end;
  Result := HidFileHandle <> INVALID_HANDLE_VALUE;
end;


procedure TJvHidDevice.CloseFile;
begin
  if HidFileHandle <> INVALID_HANDLE_VALUE then
     fpClose(cint(HidFileHandle));
  //FNumInputBuffers := 0;
  FHidFileHandle := INVALID_HANDLE_VALUE;
end;

function TJvHidDevice.CanRead(Timeout: Integer): Boolean;
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  FDSet: TFDSet;
  fd:cint;
begin
  result:=false;

  if (HidFileHandle=INVALID_HANDLE_VALUE)  then exit;

  fd :=cint(HidFileHandle);

  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then TimeVal := NIL;

  fpFD_ZERO(FDSet);
  fpFD_SET(fd, FDSet);

  if fpSelect(fd+1, @FDSet, NIL, NIL, @TimeVal) > 0 THEN
  begin
    result:=(fpFD_ISSET(fd, FDSet) <> 0);
  end;
end;

function TJvHidDevice.CanWrite(Timeout: Integer): Boolean;
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  FDSet: TFDSet;
  fd:cint;
begin
  result:=false;

  if (HidFileHandle=INVALID_HANDLE_VALUE)  then exit;

  fd :=cint(HidFileHandle);

  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then TimeVal := NIL;

  fpFD_ZERO(FDSet);
  fpFD_SET(fd, FDSet);

  if fpSelect(fd+1, NIL, @FDSet, NIL, @TimeVal) > 0 THEN
  begin
    result:=(fpFD_ISSET(fd, FDSet) <> 0);
  end;
end;

function TJvHidDevice.ReadFile(var Report; ToRead: DWORD; var BytesRead: DWORD): Boolean;
var
  ret:cint;
  readBuffer: array[0..64] of hiddev_event;
  readBufferByte: array[0..64] of byte;
  i:integer;
begin
  if (HidFileHandle=INVALID_HANDLE_VALUE)  then exit(false);

  ret:=-1;

  BytesRead:=0;

  if OpenFile then
  begin
    if CanRead(ThreadSleepTime) then
    begin
      InitWithoutHint(readBuffer);
      InitWithoutHint(readBufferByte);
      ret := FpRead( cint(HidFileHandle), readBuffer, sizeof(readBuffer[0])*(ToRead));
      if ret>=0 then
      begin
        BytesRead := ret DIV sizeof(readBuffer[0]);
        for i:=0 to BytesRead-1 do readBufferByte[i+1]:=readBuffer[i].value;
        Move(Report,readBufferByte,1);
        Move(readBufferByte,Report,BytesRead);
      end;
    end;
  end;

  Result :=(ret>=0);
end;


function TJvHidDevice.WriteFile(const Report; ToWrite: DWORD; var BytesWritten: DWORD): Boolean;
var
  ref_multi:hiddev_usage_ref_multi;
  rinfo_out:hiddev_report_info;
  writeBufferByte: array[0..64] of byte;
  i:integer;
  ret:cint;
begin
  if (HidFileHandle=INVALID_HANDLE_VALUE)  then exit(false);

  ret:=-1;

  BytesWritten:=0;

  if OpenFile then
  begin
    InitWithoutHint(writeBufferByte);
    Move(Report,writeBufferByte,ToWrite);
    ref_multi.uref.report_type := HID_REPORT_TYPE_OUTPUT;
    ref_multi.uref.report_id := writeBufferByte[0];
    //ref_multi.uref.report_id := HID_REPORT_ID_FIRST;
    ref_multi.uref.field_index := 0;
    ref_multi.uref.usage_index := 0;
    ref_multi.num_values := ToWrite-1;
    for i := 0 to ToWrite-1 do ref_multi.values[i]:=writeBufferByte[i+1];
    ret:=fpioctl(cint(HidFileHandle), HIDIOCSUSAGES, @ref_multi);
    if (ret>=0) then
    begin
      rinfo_out.report_type := HID_REPORT_TYPE_OUTPUT;
      rinfo_out.report_id   := writeBufferByte[0];
      //rinfo_out.report_id   := HID_REPORT_ID_FIRST;
      rinfo_out.num_fields  := 1;
      ret:=fpioctl(cint(HidFileHandle),HIDIOCSREPORT,@rinfo_out);
      if (ret>=0) then
      begin
        BytesWritten:=ToWrite;
      end;
    end;
  end;
  result :=(ret>=0);
end;

procedure TJvHidDevice.DoUnplug;
begin
  CloseFile;
  FIsPluggedIn := False;
  // event even for checked in devices
  if Assigned(FUnplug) then
    FUnplug(Self);
  // guarantees that event is only called once
  OnUnplug := nil;
end;

procedure TJvHidDevice.ShowReports(report_type:word);
function controlName(usage_code:integer):string;
var
  hi:integer;
  //lo:integer;
begin
  hi := (usage_code SHR 16) AND $FFFF;
  //lo := usage_code AND $FFFF;
  case (hi) of
    $80: Result:='(USB Monitor usage page)';
    $81: Result:='(USB Enumerated Values usage page)';
    $83: Result:='(Reserved usage page)';
  else
    Result:='(unknown usage page)';
  end;
end;
var
  rinfo:hiddev_report_info;
  finfo:hiddev_field_info;
  uref:hiddev_usage_ref;
  i,j,ret:integer;
begin

  if Openfile then
  begin
    rinfo.report_type := report_type;
    rinfo.report_id := HID_REPORT_ID_FIRST;
    ret := fpioctl(cint(HidFileHandle), HIDIOCGREPORTINFO, @rinfo);
    while (ret >= 0) do
      begin
      DebugInfo:=Format('HIDIOCGREPORTINFO: report_id=0x%X (%U fields)',[rinfo.report_id, rinfo.num_fields]);
      for i := 0 to rinfo.num_fields-1 do
      begin
        finfo.report_type := rinfo.report_type;
        finfo.report_id   := rinfo.report_id;
        finfo.field_index := i;
        fpioctl(cint(HidFileHandle), HIDIOCGFIELDINFO, @finfo);

        DebugInfo:=Format('HIDIOCGFIELDINFO: field_index=%U maxusage=%U flags=0x%X',
                       [finfo.field_index, finfo.maxusage, finfo.flags]);
        DebugInfo:=Format('physical=0x%X logical=0x%X application=0x%X',
                       [finfo.physical, finfo.logical, finfo.application]);
        DebugInfo:=Format('logical_minimum=%D,maximum=%D physical_minimum=%D,maximum=%D',
                       [finfo.logical_minimum,  finfo.logical_maximum,
		       finfo.physical_minimum, finfo.physical_maximum]);

        for j := 0 to finfo.maxusage-1 do
        begin
          uref.report_type := finfo.report_type;
	  uref.report_id   := finfo.report_id;
	  uref.field_index := i;
	  uref.usage_index := j;
	  fpioctl(cint(HidFileHandle), HIDIOCGUCODE, @uref);
	  fpioctl(cint(HidFileHandle), HIDIOCGUSAGE, @uref);
          DebugInfo:=Format(' >> usage_index=%U usage_code=0x%X (%S) value=%D',
                         [uref.usage_index,uref.usage_code,controlName(uref.usage_code),uref.value]);
        end;
      end;
      rinfo.report_id := rinfo.report_id OR HID_REPORT_ID_NEXT;
      ret := fpioctl(cint(HidFileHandle), HIDIOCGREPORTINFO, @rinfo);
    end;
  end;
end;

{$ifndef udevstatic}
initialization
  LoadUdevLibrary;

finalization
  FreeUdevLibrary;
{$endif}
end.

