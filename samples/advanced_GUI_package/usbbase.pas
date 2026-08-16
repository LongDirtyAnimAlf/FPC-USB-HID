unit usbbase;

interface

{$ifdef FPC}
{$mode Delphi}
{$endif}

uses
  {$ifdef MSWindows}
  Windows,
  {$endif}
  SysUtils, Classes,
  Contnrs,
  usb;

type
  EUSBException = class(Exception);

  TDeviceEvent    = procedure(Sender: TObject;datacarrier:integer) of object;

  TDataDevice=class;

  TMyUSB = class(TUSB)
  public
    function CheckVendorProduct(const VID,PID:word):boolean;override;
  end;

  TDataDevice=class
  strict private
    FDataSource    : TMyUSB;
    FEmulation     : boolean;
    FEnabled       : boolean;
  private
    FOnDeviceChange: TDeviceEvent;

    AppName:string;

    function  GetErrors:String;
    procedure AddErrors(data:string);

    function  GetInfo:String;
    procedure AddInfo(data:string);

    procedure UpdateUSBDevice(Sender: TObject;Board:TUSBController);

    procedure SetEnabled(Value: Boolean);
  public
    MaxErrors:word;

    ControllerBoards : TObjectList;

    constructor Create(aName:string);
    destructor Destroy;override;

    function  CheckParameters(board:word):boolean;

    property  OnDeviceChange: TDeviceEvent read FOnDeviceChange write FOnDeviceChange;

    property  DataSource:TMyUSB read FDataSource;
    property  Emulation:boolean read FEmulation;
    property  Enabled: Boolean read FEnabled write SetEnabled;

    property  Errors:String read GetErrors;
    property  Info:String read GetInfo;
  end;

implementation

function TMyUSB.CheckVendorProduct(const VID,PID:word):boolean;
const
  VENDORID_BASE                 = $04D8;
  PRODUCTID_BASE                = $003F;
  VENDORID_ALT                  = $ABCD;
  PRODUCTID_ALT                 = $1234;
begin
  result:=
  (
  ( (VENDORID_BASE=VID) AND (PRODUCTID_BASE=PID) )
  OR
  ( (VENDORID_ALT=VID) AND (PRODUCTID_ALT=PID) )
  );
end;

constructor TDataDevice.Create(aName:string);
begin
  AppName:=aName;
  MaxErrors  := 2;

  ControllerBoards:=TObjectList.Create;
  ControllerBoards.OwnsObjects:=False;

  FDataSource:=TMyUSB.Create;
end;

destructor TDataDevice.Destroy;
var
  Ctrl:TUSBController;
  I: integer;
begin
  for I := 1 to ControllerBoards.Count - 1 do
  begin
    if Assigned(ControllerBoards.Items[I]) then
    begin
      Ctrl := (ControllerBoards.Items[I] AS TUSBController);
      Ctrl.Destroy;
    end;
    ControllerBoards.Items[I] := nil;
  end;

  FDataSource.Destroy;

  ControllerBoards.Destroy;
end;

procedure TDataDevice.SetEnabled(Value: Boolean);
begin
  if (Value <> FEnabled) then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      DataSource.OnUSBDeviceChange:=UpdateUSBDevice;
    end
    else
    begin
      DataSource.OnUSBDeviceChange:=nil;
    end;
    DataSource.Enabled:=FEnabled;
  end;
end;

procedure TDataDevice.UpdateUSBDevice(Sender: TObject;Board:TUSBController);
var
  error:boolean;
  localboard:integer;
  Ctrl:TUSBController;
begin
  localboard:=1;
  error:=false;

  //if (Assigned(Board) AND Assigned(Board.HidCtrl)) then
  if Assigned(Board) then
  begin
    //Board.DisableReadThreading;
    // We might enable threaded reception of data
    //Board.EnableReadThreading;
    //Board.HidCtrl.ThreadSleepTime:=150; // 500 for Win64

    if Board.HidCtrl.IsPluggedIn then
    begin
      // Arrival
      // Board arrival

      // Find a free position, if any.
      while ControllerBoards.Count>localboard do
      begin
        if (NOT Assigned(ControllerBoards.Items[localboard])) then break;
        Inc(localboard);
      end;

      // Add a new free position if needed.
      if (ControllerBoards.Count<=localboard) then
      begin
        while ControllerBoards.Count<=localboard do ControllerBoards.Add(nil);
        localboard:=(ControllerBoards.Count-1);
      end;

      if (NOT Assigned(ControllerBoards.Items[localboard])) then
      begin
        AddInfo('Board ['+InttoStr(localboard)+'] accepted.');

        // If we accept the board, its now ours
        // So we are responsible for its lifetime also !!
        Board.Accepted:=True;

        // We might enable threaded reception of data
        //Board.EnableShowReadThreading;

        // Add databoard to the list of ControllerBoards
        ControllerBoards.Items[localboard]:=Board;
      end
      else
      begin
        // In theory, we should never get her, but anyhow.
        raise EUSBException.Create('Databoard already assigned. Should never happen. Please check code !');
      end;
      if (Board.Accepted AND Assigned(FOnDeviceChange)) then FOnDeviceChange(Self,localboard);
    end
    else
    begin
      // Removal
      localboard:=ControllerBoards.Count;
      // Find the board with the right HID device
      while localboard>0 do
      begin
        Dec(localboard);
        Ctrl:=(ControllerBoards.Items[localboard] AS TUSBController);
        if NOT Assigned(Ctrl) then continue;
        if NOT Assigned(Ctrl.HidCtrl) then continue;
        if (Ctrl.HidCtrl=Board.HidCtrl) then
        begin
          // Got you !!
          Ctrl.Destroy;
          Ctrl:=nil;

          // Delete controller from list by setting nil
          ControllerBoards.Items[localboard]:=nil;

          AddInfo('Board [#'+InttoStr(localboard)+'] removed.');

          if Assigned(FOnDeviceChange) then FOnDeviceChange(Self,-1*localboard);

          break;
        end;
      end;

      if (localboard=0) then
      begin
        // In theory, we should never get here, but anyhow.
        raise EUSBException.Create('Databoard to be removed does not exist. Should never happen. Please check code !');
      end;
    end;

    if Assigned(Board) then
    begin
      if (Board.Accepted) then
        AddInfo('Correct device accepted. VID: '+InttoStr(Board.HidCtrl.Attributes.VendorID)+'. PID: '+InttoStr(Board.HidCtrl.Attributes.ProductID)+'.')
      else
        AddInfo('Correct device NOT accepted. VID: '+InttoStr(Board.HidCtrl.Attributes.VendorID)+'. PID: '+InttoStr(Board.HidCtrl.Attributes.ProductID)+'.');
    end;

    AddInfo('Done.');
  end;
end;

function TDataDevice.CheckParameters(board:word):boolean;
var
  Ctrl:TUSBController;
begin
  result:=true;
  if FEmulation then exit;
  if (ControllerBoards.Count=0) then exit;
  if (board>=ControllerBoards.Count) then exit;
  Ctrl:=TUSBController(ControllerBoards.Items[board]);
  if (NOT Assigned(Ctrl)) then exit;
  result:=(NOT Assigned(Ctrl.HidCtrl));
end;

function TDataDevice.GetErrors:String;
begin
  result:=DataSource.Errors;
  DataSource.Errors:='';
end;

function TDataDevice.GetInfo:String;
begin
  result:=DataSource.Info;
  DataSource.Info:='';
end;

procedure TDataDevice.AddInfo(data:string);
begin
  if Length(data)>0 then
  begin
    DataSource.Info:=data;
  end;
end;

procedure TDataDevice.AddErrors(data:string);
begin
  if Length(data)>0 then
  begin
    DataSource.Errors:=data;
  end;
end;

end.
