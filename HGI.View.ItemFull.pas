unit HGI.View.ItemFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item,
  FMX.Filter.Effects, FMX.Edit, HGI.View.Item, System.Actions, FMX.ActnList,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFramePackageItemFull = class(TFrame)
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
    Path1: TPathLabel;
    Label3: TLabel;
    FlowLayoutPlatforms: TFlowLayout;
    LayoutPfWin: TLayout;
    PathAndroid: TPathLabel;
    PathIOS: TPathLabel;
    PathLinux: TPathLabel;
    PathMacOS: TPathLabel;
    PathWindows: TPathLabel;
    LayoutPfLin: TLayout;
    LayoutPfAndr: TLayout;
    LayoutPfIOS: TLayout;
    LayoutPfMac: TLayout;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    EditId: TEdit;
    ButtonDownload: TButton;
    Label9: TLabel;
    EditLibUrl: TMemo;
    CircleGitHub: TCircle;
    PathGutHub: TPathLabel;
    Label11: TLabel;
    LabelDependencies: TLabel;
    CircleSite: TCircle;
    PathLabelSite: TPathLabel;
    PanelMenuDiv: TPanel;
    procedure FlowLayoutPlatformsResize(Sender: TObject);
    procedure ButtonWebSiteClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure VertScrollBoxTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
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
    procedure Fill(Item: TGetItPackage; Installed: Boolean; Versions: TArray<TGetItPackage>);
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
  if Value.Contains('.') then
  begin
    var F: Single;
    if TryStrToFloat(Value.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator), F) then
    begin
      if F > 1024 then
        Result := ' ' + Max(1, Round(F / 1024)).ToString + ' GB'
      else
        Result := ' ' + Max(1, Round(F)).ToString + ' MB';
    end
    else
      Result := '';
  end
  else
  begin
    var I: Integer;
    if TryStrToInt(Value, I) then
      Result := ' ' + Max(1, Round(I / 1024)).ToString + ' MB'
    else
      Result := '';
  end;
end;

procedure TFramePackageItemFull.Fill(Item: TGetItPackage; Installed: Boolean; Versions: TArray<TGetItPackage>);
begin
  FLibUrl := Item.LibUrl;
  FLibProjectUrl := Item.LibProjectUrl;
  FLibId := Item.Id;
  FLibSize := Item.LibSize;
  FPurchaseUrl := Item.PurchaseUrl;

  IsInstalled := Installed;

  EditId.Text := Item.Id;
  EditLibUrl.Text := Item.LibUrl;
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  if LabelDesc.Text.IsEmpty then
    LabelDesc.Text := 'None';
  LabelInfo.Text := Item.Version + #13#10 + Item.Vendor + #13#10 + Item.VendorUrl;
  LabelLicense.Text := Item.LibLicenseName;
  if LabelLicense.Text.IsEmpty then
    LabelLicense.Text := 'The license is not defined';

  ButtonDownload.Visible := not Item.LibUrl.IsEmpty;
  ButtonWebSite.Visible := not Item.LibProjectUrl.IsEmpty;
  ButtonWebSite.Hint := Item.LibProjectUrl;
  CircleGitHub.Visible := Item.LibProjectUrl.ToLower.Contains('github.com') or Item.LibProjectUrl.ToLower.Contains('gitlab.com');
  if CircleGitHub.Visible then
    if Item.LibProjectUrl.ToLower.Contains('github.com') then
      ButtonWebSite.Text := 'GitHub'
    else
      ButtonWebSite.Text := 'GitLab'
  else
    ButtonWebSite.Text := 'Web Site';
  CircleSite.Visible := not CircleGitHub.Visible;
  ButtonBuy.Visible := not Item.PurchaseUrl.IsEmpty;
  LabelTags.Text := Item.Tags;
  if LabelTags.Text.IsEmpty then
    LabelTags.Text := 'None';
  LabelDate.Text := 'Updated ' + TGetIt.ParseDate(Item.Modified);

  LayoutPfWin.Visible := False;
  LayoutPfLin.Visible := False;
  LayoutPfMac.Visible := False;
  LayoutPfAndr.Visible := False;
  LayoutPfIOS.Visible := False;
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

  var Dependencies: TArray<string>;
  for var Dependence in Item.Dependencies do
    Dependencies := Dependencies + [Dependence.Id + ' (' + Dependence.Size + ' Mb)'];

  if Length(Dependencies) > 0 then
    LabelDependencies.Text := string.Join(', ', Dependencies)
  else
    LabelDependencies.Text := 'None';

  RectangleImage.Fill.Kind := TBrushKind.Solid;
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
    if Control.Visible then
      H := Max(H, Control.Position.Y + Control.Height);
  FlowLayoutPlatforms.Height := H;
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

procedure TFramePackageItemFull.VertScrollBoxTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  PanelMenuDiv.Visible := NewViewportPosition.Y <> 0;
end;

end.

