unit HGI.View.Item;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item;

type
  TFramePackageItem = class(TFrame)
    RectangleBG: TRectangle;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    RectangleImage: TRectangle;
    ButtonDownload: TButton;
    LabelTitle: TLabel;
    LabelDesc: TLabel;
    LabelInfo: TLabel;
    LayoutOS: TLayout;
    PathWindows: TPath;
    PathIOS: TPath;
    PathAndroid: TPath;
    PathLinux: TPath;
    PathMacOS: TPath;
    LabelLicense: TLabel;
    procedure RectangleBGClick(Sender: TObject);
  private
    FItem: TGetItPackage;
    function FormatSize(const Value: string): string;
  public
    procedure Fill(Item: TGetItPackage);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Math, HGI.View.ItemFull;

{$R *.fmx}

{ TFramePackageItem }

constructor TFramePackageItem.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FItem := nil;
end;

destructor TFramePackageItem.Destroy;
begin
  if Assigned(FItem) then
    FItem.Free;
  inherited;
end;

function TFramePackageItem.FormatSize(const Value: string): string;
begin
  var F: Single;
  if TryStrToFloat(Value.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator), F) then
    Result := ' ' + Max(1, Round(F)).ToString + 'MB'
  else
    Result := '';
end;

procedure TFramePackageItem.Fill(Item: TGetItPackage);
begin
  FItem := Item;
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  LabelInfo.Text := Item.Version + #13#10 + 'by ' + Item.Vendor;
  LabelLicense.Text := Item.LibLicenseName;
  ButtonDownload.Text := 'DOWNLOAD' + FormatSize(Item.LibSize);
  PathWindows.Visible := False;
  PathLinux.Visible := False;
  PathMacOS.Visible := False;
  PathAndroid.Visible := False;
  PathIOS.Visible := False;
  for var OS in Item.LibOSes do
  begin
    if OS.Id = '1' then
      PathWindows.Visible := True
    else if OS.Id = '2' then
      PathMacOs.Visible := True
    else if OS.Id = '3' then
      PathIOS.Visible := True
    else if OS.Id = '4' then
      PathAndroid.Visible := True
    else if OS.Id = '5' then
      PathLinux.Visible := True;
  end;

  RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(Item.Image, False,
    procedure(Bitmap: TBitmap)
    begin
      RectangleImage.Fill.Kind := TBrushKind.Bitmap;
      RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    end);
end;

procedure TFramePackageItem.RectangleBGClick(Sender: TObject);
begin
  var Frame := TFramePackageItemFull.Create(Self);
  Frame.Parent := Application.MainForm;
  Frame.Fill(FItem);
end;

end.

