unit HGI.View.ItemFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item,
  FMX.Filter.Effects, FMX.Edit, HGI.View.Item, System.Actions, FMX.ActnList,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, HGM.LineStorage, FMX.TabControl,
  FMX.ListBox, FMX.SearchBox;

type
  TFramePackageItemFull = class(TFrame)
    RectangleBG: TRectangle;
    LayoutHead: TLayout;
    RectangleImage: TRectangle;
    LabelInfo: TLabel;
    LayoutDetail: TLayout;
    LayoutLicense: TLayout;
    LabelLicense: TLabel;
    LayoutActions: TLayout;
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
    AniIndicatorLic: TAniIndicator;
    ButtonPFWin: TButton;
    ButtonPFLinux: TButton;
    ButtonPFMacOS: TButton;
    ButtonPFAndroid: TButton;
    ButtonPFiOS: TButton;
    Layout1: TLayout;
    Layout3: TLayout;
    ButtonSwitchTab: TButton;
    TabControlGeneral: TTabControl;
    TabItemDetails: TTabItem;
    TabItemVersions: TTabItem;
    ListBoxVersions: TListBox;
    ListBoxItem1: TListBoxItem;
    SearchBoxVers: TSearchBox;
    procedure FlowLayoutPlatformsResize(Sender: TObject);
    procedure ButtonWebSiteClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure VertScrollBoxTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure LabelLicenseClick(Sender: TObject);
    procedure ButtonSwitchTabClick(Sender: TObject);
    procedure TabControlGeneralChange(Sender: TObject);
  private
    FLibUrl: string;
    FLibProjectUrl: string;
    FLibId: string;
    FLibSize: string;
    FPurchaseUrl: string;
    FLicenseUrl: string;
    FLicense: string;
    FOnAction: TOnItemAction;
    FIsInstalled: Boolean;
    FLicenseHolder: TComponent;
    FOnShowVersion: TOnShowVersion;
    FVersions: TArray<TGetItPackage>;
    function FormatSize(const Value: string): string;
    procedure SetOnAction(const Value: TOnItemAction);
    procedure SetIsInstalled(const Value: Boolean);
    procedure ShowLicense;
    procedure FillVersions(Versions: TArray<TGetItPackage>);
    procedure SetOnShowVersion(const Value: TOnShowVersion);
    procedure FOpenVersion(Sender: TObject);
  public
    procedure Fill(Item: TGetItPackage; Installed: Boolean; Versions: TArray<TGetItPackage>);
    property OnAction: TOnItemAction read FOnAction write SetOnAction;
    property OnShowVersion: TOnShowVersion read FOnShowVersion write SetOnShowVersion;
    procedure Close;
    constructor Create(AOwner: TComponent); override;
    property IsInstalled: Boolean read FIsInstalled write SetIsInstalled;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Threading, HGM.ObjectHolder, System.Rtti,
  HGM.Common.Download, System.Math, FMX.Ani, HGI.GetItAPI, WinUI3.Dialogs,
  HGI.DM.Store;

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

procedure TFramePackageItemFull.ButtonSwitchTabClick(Sender: TObject);
begin
  if TabControlGeneral.ActiveTab = TabItemDetails then
    TabControlGeneral.ActiveTab := TabItemVersions
  else
    TabControlGeneral.ActiveTab := TabItemDetails;
end;

constructor TFramePackageItemFull.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  VertScrollBoxText.AniCalculations.Animation := True;
  ButtonPFWin.StylesData['icon.Data.Data'] := DataModuleStore.LineStoragePlatforms.GetByName('win');
  ButtonPFWin.StylesData['icon.Visible'] := True;

  ButtonPFLinux.StylesData['icon.Data.Data'] := DataModuleStore.LineStoragePlatforms.GetByName('linux');
  ButtonPFLinux.StylesData['icon.Visible'] := True;

  ButtonPFMacOS.StylesData['icon.Data.Data'] := DataModuleStore.LineStoragePlatforms.GetByName('macos');
  ButtonPFMacOS.StylesData['icon.Visible'] := True;

  ButtonPFAndroid.StylesData['icon.Data.Data'] := DataModuleStore.LineStoragePlatforms.GetByName('android');
  ButtonPFAndroid.StylesData['icon.Visible'] := True;

  ButtonPFiOS.StylesData['icon.Data.Data'] := DataModuleStore.LineStoragePlatforms.GetByName('ios');
  ButtonPFiOS.StylesData['icon.Visible'] := True;

  TabControlGeneral.TabPosition := TTabPosition.None;
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
        Result := Max(1, Round(F / 1024)).ToString + ' GB'
      else
        Result := Max(1, Round(F)).ToString + ' MB';
    end
    else
      Result := '';
  end
  else
  begin
    var I: Integer;
    if TryStrToInt(Value, I) then
      Result := Max(1, Round(I / 1024)).ToString + ' MB'
    else
      Result := '';
  end;
end;

procedure TFramePackageItemFull.ShowLicense;
begin
  if FLicenseUrl.IsEmpty then
    Exit;
  if not FLicense.IsEmpty then
  begin
    ShowUIMessage(TForm(Application.MainForm), 'License', FLicense);
    Exit;
  end;
  AniIndicatorLic.Visible := True;
  var LicUrl := FLicenseUrl;
  TaskRun(FLicenseHolder,
    procedure(Holder: IComponentHolder)
    begin
      if not Holder.IsLive then
        Exit;
      var LicenseText: string;
      try
        LicenseText := TDownload.GetText(LicUrl);
      except
        LicenseText := '';
      end;
      if not Holder.IsLive then
        Exit;
      Queue(
        procedure
        begin
          if not Holder.IsLive then
            Exit;
          FLicense := LicenseText;
          AniIndicatorLic.Visible := False;
          ShowUIMessage(TForm(Application.MainForm), 'License', LicenseText);
        end);
    end);
end;

procedure TFramePackageItemFull.TabControlGeneralChange(Sender: TObject);
begin
  if TabControlGeneral.ActiveTab = TabItemDetails then
  begin
    ButtonSwitchTab.Text := 'Versions';
  end
  else
  begin
    ButtonSwitchTab.Text := 'Back';
  end;
end;

procedure TFramePackageItemFull.LabelLicenseClick(Sender: TObject);
begin
  ShowLicense;
end;

procedure TFramePackageItemFull.Fill(Item: TGetItPackage; Installed: Boolean; Versions: TArray<TGetItPackage>);
begin
  TabControlGeneral.ActiveTab := TabItemDetails;
  FVersions := Versions;
  ButtonSwitchTab.Visible := Length(Versions) > 0;
  FLibUrl := Item.LibUrl;
  FLibProjectUrl := Item.LibProjectUrl;
  if FLibProjectUrl.IsEmpty then
    FLibProjectUrl := Item.VendorUrl;
  FLibId := Item.Id;
  FLibSize := Item.LibSize;
  FPurchaseUrl := Item.PurchaseUrl;
  FLicenseUrl := Item.LibLicense;
  FLicense := '';
  FLicenseHolder.Free;
  FLicenseHolder := nil;
  AniIndicatorLic.Visible := False;

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
  begin
    LabelLicense.Text := 'The license is not defined';
    LabelLicense.HitTest := False;
  end
  else
  begin
    LabelLicense.HitTest := True;
    FLicenseHolder := TComponent.Create(Self);
  end;

  ButtonDownload.Visible := not Item.LibUrl.IsEmpty;
  ButtonWebSite.Enabled := not FLibProjectUrl.IsEmpty;
  ButtonWebSite.Hint := FLibProjectUrl;
  CircleGitHub.Visible := FLibProjectUrl.ToLower.Contains('github.com') or FLibProjectUrl.ToLower.Contains('gitlab.com');
  if CircleGitHub.Visible then
    if FLibProjectUrl.ToLower.Contains('github.com') then
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
  LabelDate.Text := TGetIt.ParseDate(Item.Modified);

  ButtonPFWin.Visible := False;
  ButtonPFLinux.Visible := False;
  ButtonPFMacOS.Visible := False;
  ButtonPFAndroid.Visible := False;
  ButtonPFiOS.Visible := False;
  for var OS in Item.LibOSes do
  begin
    if OS.Id = '1' then
      ButtonPFWin.Visible := True
    else if OS.Id = '2' then
      ButtonPFMacOS.Visible := True
    else if OS.Id = '3' then
      ButtonPFiOS.Visible := True
    else if OS.Id = '4' then
      ButtonPFAndroid.Visible := True
    else if OS.Id = '5' then
      ButtonPFLinux.Visible := True;
  end;

  var Dependencies: TArray<string>;
  for var Dependence in Item.Dependencies do
    Dependencies := Dependencies + [Dependence.Id + ' (' + FormatSize(Dependence.Size) + ')'];

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
  FillVersions(Versions);

  FlowLayoutPlatformsResize(nil);

  Opacity := 0;
  TAnimator.AnimateFloat(Self, 'Opacity', 1);
end;

procedure TFramePackageItemFull.FillVersions(Versions: TArray<TGetItPackage>);
begin
  ListBoxVersions.BeginUpdate;
  try
    ListBoxVersions.Clear;
    for var Ver in Versions do
    begin
      var ListItem := TListBoxItem.Create(ListBoxVersions);
      ListItem.Text := Ver.Version + ' (' + FormatSize(Ver.LibSize) + ')';
      ListItem.TagString := Ver.Id;
      //ListItem.ItemData.Accessory := TListBoxItemData.TAccessory.aMore;
      ListItem.ItemData.Detail := Ver.Id;
      ListItem.StylesData['action.OnClick'] := TValue.From<TNotifyEvent>(FOpenVersion);
      ListItem.StylesData['action.TagString'] := Ver.Id;
      ListBoxVersions.AddObject(ListItem);
    end;
  finally
    ListBoxVersions.EndUpdate;
  end;
end;

procedure TFramePackageItemFull.FlowLayoutPlatformsResize(Sender: TObject);
begin
  var H: Single := 0;
  for var Control in FlowLayoutPlatforms.Controls do
    if Control.Visible then
      H := Max(H, Control.Position.Y + Control.Height);
  FlowLayoutPlatforms.Height := H;
end;

procedure TFramePackageItemFull.FOpenVersion(Sender: TObject);
begin
  if Sender is not TButton then
    Exit;
  var Id := TButton(Sender).TagString;
  for var Item in FVersions do
    if Item.Id = Id then
    begin
      if Assigned(FOnShowVersion) then
        FOnShowVersion(Self, Item);
    end;
end;

procedure TFramePackageItemFull.SetIsInstalled(const Value: Boolean);
begin
  FIsInstalled := Value;
  if not FIsInstalled then
    ButtonInstall.Text := 'INSTALL ' + FormatSize(FLibSize)
  else
    ButtonInstall.Text := 'UNINSTALL';
end;

procedure TFramePackageItemFull.SetOnAction(const Value: TOnItemAction);
begin
  FOnAction := Value;
end;

procedure TFramePackageItemFull.SetOnShowVersion(const Value: TOnShowVersion);
begin
  FOnShowVersion := Value;
end;

procedure TFramePackageItemFull.VertScrollBoxTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  PanelMenuDiv.Visible := NewViewportPosition.Y <> 0;
end;

end.

