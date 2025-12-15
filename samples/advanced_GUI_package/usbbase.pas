unit usbbase;

interface

{$ifdef FPC}
{$mode Delphi}
{$endif}

uses
  SysUtils, Classes,
  usb;

type
  EUSBException = class(Exception);

  TDeviceEvent    = procedure(Sender: TObject;datacarrier:integer) of object;

  TDataDevice=class;

  TMyUSB = class(TUSB)
  strict private
    FMaxBoards:word;
    FMaxErrors:word;
  public
    constructor Create;
    function CheckVendorProduct(const VID,PID:word):boolean;override;
    property MaxBoards:word write FMaxBoards;
    property MaxErrors:word write FMaxErrors;
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

    ControllerBoards : Classes.TList;

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

constructor TMyUSB.Create;
begin
  inherited;
  //{$ifdef LINUX}
  //WaitEx:=True;
  //{$endif}
end;

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
var
  BoardCount:word;
begin
  AppName:=aName;

  MaxErrors  := 2;
  BoardCount := 10;

  ControllerBoards:=TList.Create;
  Inc(BoardCount); // we do not use board zero.
  ControllerBoards.Count:=BoardCount;
  FDataSource:=TMyUSB.Create;
  DataSource.MaxBoards:=BoardCount;
  DataSource.MaxErrors:=MaxErrors;
end;

destructor TDataDevice.Destroy;
var
  i:word;
  Ctrl:TUSBController;
begin
  for i:=0 to Pred(ControllerBoards.Count) do
  begin
    Ctrl:=TUSBController(ControllerBoards.Items[i]);
    if Assigned(Ctrl) then Ctrl.Destroy;
  end;
  ControllerBoards.Free;
  FDataSource.Free;
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
  localboard:integer;
  Ctrl:TUSBController;
begin
  // Start at number 1
  localboard:=1;

  if Assigned(Board) then
  begin
    if Board.HidCtrl.IsPluggedIn then
    begin
      // Board arrival
      while Assigned(ControllerBoards.Items[localboard]) do Inc(localboard);

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
      // Board removal

      localboard:=ControllerBoards.Count;
      while localboard>0 do
      begin
        Dec(localboard);
        Ctrl:=TUSBController(ControllerBoards.Items[localboard]);
        if NOT Assigned(Ctrl) then continue;
        if NOT Assigned(Ctrl.HidCtrl) then continue;
        if (Ctrl.HidCtrl=Board.HidCtrl) then
        begin
          // Got you !!
          AddInfo('Board [#'+InttoStr(localboard)+'] removed.');
          Ctrl.Destroy;
          ControllerBoards.Items[localboard]:=nil;
          if Assigned(FOnDeviceChange) then FOnDeviceChange(Self,-1*localboard);
          break;
        end;
      end;

      if (localboard=0) then
      begin
        // In theory, we should never get her, but anyhow.
        raise EUSBException.Create('Databoard to be removed does not exist. Should never happen. Please check code !');
      end;
    end;

    if (Board.Accepted) then
      AddInfo('Correct device accepted. VID: '+InttoStr(Board.HidCtrl.Attributes.VendorID)+'. PID: '+InttoStr(Board.HidCtrl.Attributes.ProductID)+'.')
    else
      AddInfo('Correct device NOT accepted. VID: '+InttoStr(Board.HidCtrl.Attributes.VendorID)+'. PID: '+InttoStr(Board.HidCtrl.Attributes.ProductID)+'.');
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
