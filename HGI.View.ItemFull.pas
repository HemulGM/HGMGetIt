unit HGI.View.ItemFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item;

type
  TFramePackageItemFull = class(TFrame)
    Rectangle1: TRectangle;
    RectangleBG: TRectangle;
    Layout1: TLayout;
    RectangleImage: TRectangle;
    LabelInfo: TLabel;
    Layout3: TLayout;
    LayoutOS: TLayout;
    PathWindows: TPath;
    PathIOS: TPath;
    PathAndroid: TPath;
    PathLinux: TPath;
    PathMacOS: TPath;
    LabelLicense: TLabel;
    Layout4: TLayout;
    ButtonDownload: TButton;
    ButtonWebSite: TButton;
    Layout2: TLayout;
    LabelTitle: TLabel;
    VertScrollBoxText: TVertScrollBox;
    LabelDesc: TLabel;
    Label1: TLabel;
    LabelDate: TLabel;
    Label2: TLabel;
    LabelTags: TLabel;
    ButtonBuy: TButton;
    Path1: TPath;
    procedure RectangleBGClick(Sender: TObject);
    procedure Rectangle1Click(Sender: TObject);
  private
    function FormatSize(const Value: string): string;
    function ParseDate(const Value: string): string;
  public
    procedure Fill(Item: TGetItPackage);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Math, FMX.Ani;

{$R *.fmx}

{ TFramePackageItem }

constructor TFramePackageItemFull.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  VertScrollBoxText.AniCalculations.Animation := True;
  Opacity := 0;
end;

destructor TFramePackageItemFull.Destroy;
begin
  inherited;
end;

function TFramePackageItemFull.FormatSize(const Value: string): string;
begin
  var F: Single;
  if TryStrToFloat(Value.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator), F) then
    Result := ' ' + Max(1, Round(F)).ToString + 'MB'
  else
    Result := '';
end;

function TFramePackageItemFull.ParseDate(const Value: string): string;
begin
  var FormatDate: TFormatSettings;
  var Date: TDateTime;
  FormatDate.DateSeparator := '-';
  FormatDate.ShortDateFormat := 'YYYY-MM-DD';
  if (not Value.IsEmpty) and TryStrToDateTime(Value, Date, FormatDate) then
    Result := FormatDateTime('DD.MM.YYYY', Date)
  else
    Result := 'Unkonwn';
end;

procedure TFramePackageItemFull.Fill(Item: TGetItPackage);
begin
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  LabelInfo.Text := Item.Version + #13#10 + 'by ' + Item.Vendor + #13#10 + Item.VendorUrl;
  LabelLicense.Text := Item.LibLicenseName;
  ButtonDownload.Text := 'DOWNLOAD' + FormatSize(Item.LibSize);
  PathWindows.Visible := False;
  PathLinux.Visible := False;
  PathMacOS.Visible := False;
  PathAndroid.Visible := False;
  PathIOS.Visible := False;
  ButtonDownload.Visible := not Item.LibUrl.IsEmpty;
  ButtonDownload.TagString := Item.LibUrl;
  ButtonWebSite.Visible := not Item.VendorUrl.IsEmpty;
  ButtonWebSite.TagString := Item.VendorUrl;
  ButtonBuy.Visible := not Item.PurchaseUrl.IsEmpty;
  ButtonBuy.TagString := Item.PurchaseUrl;
  LabelTags.Text := Item.Tags;
  if LabelTags.Text.IsEmpty then
    LabelTags.Text := 'Empty';
  LabelDate.Text := 'Updated ' + ParseDate(Item.Modified);
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
  TAnimator.AnimateFloat(Self, 'Opacity', 1);
end;

procedure TFramePackageItemFull.Rectangle1Click(Sender: TObject);
begin
  Free;
end;

procedure TFramePackageItemFull.RectangleBGClick(Sender: TObject);
begin
  //
end;

end.

