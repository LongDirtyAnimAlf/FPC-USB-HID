unit Info;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvHidControllerClass;

type
  TInfoForm = class(TForm)
    DevStrings: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Vid: TLabel;
    Pid: TLabel;
    Vers: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    InputLen: TLabel;
    OutputLen: TLabel;
    FeatureLen: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    VendorName: TLabel;
    ProductName: TLabel;
    Label11: TLabel;
    SerialNo: TLabel;
    LangStrings: TListBox;
    Label12: TLabel;
    procedure FormShow(Sender: TObject);
  public
    Dev: TJvHidDevice;
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

procedure TInfoForm.FormShow(Sender: TObject);
var
  I: Integer;
begin
  VendorName.Caption := '"' + Dev.VendorName + '"';
  ProductName.Caption := '"' + Dev.ProductName + '"';
  SerialNo.Caption := '"' + Dev.SerialNumber + '"';
  Vid.Caption  := IntToHex(Dev.Attributes.VendorID, 4);
  Pid.Caption  := IntToHex(Dev.Attributes.ProductID, 4);
  Vers.Caption := IntToHex(Dev.Attributes.VersionNumber, 4);
  InputLen.Caption   := IntToStr(Dev.Caps.InputReportByteLength) + ' Bytes';
  OutputLen.Caption  := IntToStr(Dev.Caps.OutputReportByteLength) + ' Bytes';
  FeatureLen.Caption := IntToStr(Dev.Caps.FeatureReportByteLength) + ' Bytes';
  for I := 1 to 255 do
    if Dev.DeviceStrings[I] <> '' then
      DevStrings.Items.Add(Format('%3d) %s',[I, Dev.DeviceStrings[I]]));
  for I := 0 to Dev.LanguageStrings.Count - 1 do
    LangStrings.Items.Add(Dev.LanguageStrings[I]);
end;

end.
