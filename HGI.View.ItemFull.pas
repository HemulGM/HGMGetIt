unit HGI.View.ItemFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item,
  FMX.Filter.Effects, FMX.Edit, HGI.View.Item;

type
  TFramePackageItemFull = class(TFrame)
    RectangleShd: TRectangle;
    RectangleBG: TRectangle;
    Layout1: TLayout;
    RectangleImage: TRectangle;
    LabelInfo: TLabel;
    Layout3: TLayout;
    LayoutOS: TLayout;
    LabelLicense: TLabel;
    Layout4: TLayout;
    ButtonInstall: TButton;
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
    Label3: TLabel;
    FlowLayoutPlatforms: TFlowLayout;
    LayoutPfWin: TLayout;
    PathAndroid: TPath;
    PathIOS: TPath;
    PathLinux: TPath;
    PathMacOS: TPath;
    PathWindows: TPath;
    LayoutPfLin: TLayout;
    LayoutPfAndr: TLayout;
    LayoutPfIOS: TLayout;
    LayoutPfMac: TLayout;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ButtonClose: TButton;
    Path2: TPath;
    Label10: TLabel;
    EditId: TEdit;
    ButtonDownload: TButton;
    Path3: TPath;
    Label9: TLabel;
    EditLibUrl: TEdit;
    procedure FlowLayoutPlatformsResize(Sender: TObject);
    procedure RectangleBGResize(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonWebSiteClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
  private
    FLibUrl: string;
    FLibProjectUrl: string;
    FLibId: string;
    FLibSize: string;
    FPurchaseUrl: string;
    FOnAction: TOnItemAction;
    FIsInstalled: Boolean;
    function FormatSize(const Value: string): string;
    procedure SetOnAction(const Value: TOnItemAction);
    procedure SetIsInstalled(const Value: Boolean);
  public
    procedure Fill(Item: TGetItPackage; Installed: Boolean);
    property OnAction: TOnItemAction read FOnAction write SetOnAction;
    procedure Close;
    constructor Create(AOwner: TComponent); override;
    property IsInstalled: Boolean read FIsInstalled write SetIsInstalled;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Math, FMX.Ani, HGI.GetItAPI;

{$R *.fmx}

{ TFramePackageItem }

procedure TFramePackageItemFull.ButtonWebSiteClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    FOnAction(Self, FLibProjectUrl, TItemAction.OpenUrl);
end;

procedure TFramePackageItemFull.Close;
begin
  Release;
end;

procedure TFramePackageItemFull.ButtonBuyClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    FOnAction(Self, FPurchaseUrl, TItemAction.OpenUrl);
end;

procedure TFramePackageItemFull.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFramePackageItemFull.ButtonDownloadClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    FOnAction(Self, FLibUrl, TItemAction.OpenUrl);
end;

procedure TFramePackageItemFull.ButtonInstallClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    if FIsInstalled then
      FOnAction(Self, FLibId, TItemAction.Uninstall)
    else
      FOnAction(Self, FLibId, TItemAction.Install);
end;

constructor TFramePackageItemFull.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  VertScrollBoxText.AniCalculations.Animation := True;
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

procedure TFramePackageItemFull.Fill(Item: TGetItPackage; Installed: Boolean);
begin
  IsInstalled := Installed;
  FLibUrl := Item.LibUrl;
  FLibProjectUrl := Item.LibProjectUrl;
  FLibId := Item.Id;
  FLibSize := Item.LibSize;
  FPurchaseUrl := Item.PurchaseUrl;

  EditId.Text := Item.Id;
  EditLibUrl.Text := Item.LibUrl;
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  LabelInfo.Text := Item.Version + #13#10 + 'by ' + Item.Vendor + #13#10 + Item.VendorUrl;
  LabelLicense.Text := Item.LibLicenseName;

  LayoutPfWin.Visible := False;
  LayoutPfLin.Visible := False;
  LayoutPfMac.Visible := False;
  LayoutPfAndr.Visible := False;
  LayoutPfIOS.Visible := False;
  ButtonDownload.Visible := not Item.LibUrl.IsEmpty;
  ButtonWebSite.Visible := not Item.LibProjectUrl.IsEmpty;
  ButtonBuy.Visible := not Item.PurchaseUrl.IsEmpty;
  LabelTags.Text := Item.Tags;
  if LabelTags.Text.IsEmpty then
    LabelTags.Text := 'Empty';
  LabelDate.Text := 'Updated ' + TGetIt.ParseDate(Item.Modified);
  for var OS in Item.LibOSes do
  begin
    if OS.Id = '1' then
      LayoutPfWin.Visible := True
    else if OS.Id = '2' then
      LayoutPfMac.Visible := True
    else if OS.Id = '3' then
      LayoutPfIOS.Visible := True
    else if OS.Id = '4' then
      LayoutPfAndr.Visible := True
    else if OS.Id = '5' then
      LayoutPfLin.Visible := True;
  end;

  RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(RectangleImage, Item.Image, False,
    procedure(Success: Boolean)
    begin
      if Success then
      begin
        RectangleImage.Fill.Kind := TBrushKind.Bitmap;
        RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      end;
    end);
  Opacity := 0;
  TAnimator.AnimateFloat(Self, 'Opacity', 1);
  FlowLayoutPlatformsResize(nil);
end;

procedure TFramePackageItemFull.FlowLayoutPlatformsResize(Sender: TObject);
begin
  var H: Single := 0;
  for var Control in FlowLayoutPlatforms.Controls do
    H := Max(H, Control.Position.Y + Control.Height);
  FlowLayoutPlatforms.Height := H;
end;

procedure TFramePackageItemFull.RectangleBGResize(Sender: TObject);
begin
  if (Width < 600) or (Height < 600) then
  begin
    RectangleBG.Align := TAlignLayout.Client;
    RectangleBG.Margins.Rect := TRectF.Create(0, 0, 0, 0);
    RectangleBG.Corners := [];
  end
  else
  begin
    if RectangleBG.Align <> TAlignLayout.Fit then
    begin
      RectangleBG.Align := TAlignLayout.None;
      RectangleBG.Margins.Rect := TRectF.Create(50, 50, 50, 50);
      RectangleBG.Corners := AllCorners;
      RectangleBG.Width := 513;
      RectangleBG.Height := 634;
      RectangleBG.Align := TAlignLayout.Fit;
    end;
  end;
end;

procedure TFramePackageItemFull.SetIsInstalled(const Value: Boolean);
begin
  FIsInstalled := Value;
  if not FIsInstalled then
    ButtonInstall.Text := 'INSTALL' + FormatSize(FLibSize)
  else
    ButtonInstall.Text := 'UNINSTALL';
end;

procedure TFramePackageItemFull.SetOnAction(const Value: TOnItemAction);
begin
  FOnAction := Value;
end;

end.

