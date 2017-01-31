unit Main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  usb2;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnHIDCreate: TButton;
    btnHIDEnable: TButton;
    btnInfo: TButton;
    Memo1: TMemo;
    procedure btnHIDCreateClick(Sender: TObject);
    procedure btnHIDEnableClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

{ TForm1 }

procedure TForm1.btnHIDCreateClick(Sender: TObject);
var
  S:string;
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Created.');
  NewUSB:=TUSB.Create;
  NewUSB.OnUSBDeviceChange:=UpdateUSBDevice;
  Memo1.Lines.Append('Ready.');
  S:=NewUSB.Info;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('INFO:');
    Memo1.Lines.Append(S);
  end;
  S:=NewUSB.Errors;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('ERRORS:');
    Memo1.Lines.Append(S);
  end;
  {$ifndef Windows}
  // on Windows, HID is enabled by default !
  btnHIDEnable.Enabled:=True;
  {$endif}
  btnInfo.Enabled:=True;
end;

procedure TForm1.btnHIDEnableClick(Sender: TObject);
var
  S:string;
begin
  TButton(Sender).Enabled:=False;
  Memo1.Lines.Append('HID Enabled.');
  NewUSB.Enabled:=True;
  Memo1.Lines.Append('Ready.');
  S:=NewUSB.Info;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('INFO:');
    Memo1.Lines.Append(S);
  end;
  S:=NewUSB.Errors;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('ERRORS:');
    Memo1.Lines.Append(S);
  end;
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
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef Windows}
  // on Windows, HID is enabled by default !
  btnHIDEnable.Enabled:=False;
  {$endif}
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
  S:string;
begin
  Memo1.Lines.Append('HID device index: '+InttoStr(datacarrier));
  S:=NewUSB.Info;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('INFO:');
    Memo1.Lines.Append(S);
  end;
  S:=NewUSB.Errors;
  if Length(S)>0 then
  begin
    Memo1.Lines.Append('ERRORS:');
    Memo1.Lines.Append(S);
  end;
end;

end.

