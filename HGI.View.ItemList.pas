unit HGI.View.ItemList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item,
  FMX.Filter.Effects, FMX.Effects, FMX.Menus, FMX.Ani;

{$SCOPEDENUMS ON}

type
  TFramePackageItemList = class(TFrame)
    RectangleBG: TRectangle;
    LayoutContent: TLayout;
    LayoutHead: TLayout;
    RectangleImage: TRectangle;
    ButtonInstall: TButton;
    LabelTitle: TLabel;
    LabelDesc: TLabel;
    LabelInfo: TLabel;
    LayoutIndicates: TLayout;
    PathWindows: TPathLabel;
    PathIOS: TPathLabel;
    PathAndroid: TPathLabel;
    PathLinux: TPathLabel;
    PathMacOS: TPathLabel;
    LabelLicense: TLabel;
    ImagePers: TImage;
    Line1: TLine;
    PopupMenuOpt: TPopupMenu;
    MenuItemDownload: TMenuItem;
    MenuItemWebSite: TMenuItem;
    MenuItemShowCommand: TMenuItem;
    PathGutHub: TPathLabel;
    ColorAnimationOver: TColorAnimation;
    LayoutSelect: TLayout;
    CheckBoxSelected: TCheckBox;
    LabelVersions: TLabel;
    ButtonBG: TButton;
    procedure RectangleBGClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure ButtonInstallOptClick(Sender: TObject);
    procedure MenuItemDownloadClick(Sender: TObject);
    procedure MenuItemWebSiteClick(Sender: TObject);
    procedure MenuItemShowCommandClick(Sender: TObject);
    procedure CheckBoxSelectedChange(Sender: TObject);
  private
    FItem: TGetItPackage;
    FOnAction: TOnItemAction;
    FIsInstalled: Boolean;
    FOnChangeCheck: TNotifyEvent;
    FVersions: TArray<TGetItPackage>;
    FOnItemClick: TNotifyEvent;
    function FormatSize(const Value: string): string;
    procedure SetOnAction(const Value: TOnItemAction);
    function GetId: string;
    procedure SetIsInstalled(const Value: Boolean);
    function GetIsChecked: Boolean;
    procedure SetIsChecked(const Value: Boolean);
    procedure SetOnChangeCheck(const Value: TNotifyEvent);
    procedure UpdateVersions;
    procedure SetOnItemClick(const Value: TNotifyEvent);
  public
    procedure Fill(Item: TGetItPackage; Installed: Boolean);
    property OnAction: TOnItemAction read FOnAction write SetOnAction;
    constructor Create(AOwner: TComponent); override;
    property Id: string read GetId;
    property IsInstalled: Boolean read FIsInstalled write SetIsInstalled;
    property Item: TGetItPackage read FItem;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
    property OnChangeCheck: TNotifyEvent read FOnChangeCheck write SetOnChangeCheck;
    property OnItemClick: TNotifyEvent read FOnItemClick write SetOnItemClick;
    procedure AddVersion(Item: TGetItPackage);
    property Versions: TArray<TGetItPackage> read FVersions;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Math, HGI.View.ItemFull, System.Rtti, HGI.GetItAPI;

{$R *.fmx}

{ TFramePackageItem }

procedure TFramePackageItemList.AddVersion(Item: TGetItPackage);
begin
  FVersions := FVersions + [Item];
  UpdateVersions;
end;

procedure TFramePackageItemList.UpdateVersions;
begin
  LabelVersions.Visible := Length(FVersions) > 0;
  LabelVersions.Text := 'Multiple versions (' + Length(FVersions).ToString + ')';
end;

procedure TFramePackageItemList.ButtonInstallClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    if FIsInstalled then
      FOnAction(Self, FItem.Id, TItemAction.Uninstall)
    else
      FOnAction(Self, FItem.Id, TItemAction.Install);
end;

procedure TFramePackageItemList.ButtonInstallOptClick(Sender: TObject);
begin
  var Pt := Application.MainForm.ClientToScreen(ButtonInstall.AbsoluteRect.TopLeft);
  PopupMenuOpt.Popup(Pt.X, Pt.Y + ButtonInstall.Height);
end;

procedure TFramePackageItemList.CheckBoxSelectedChange(Sender: TObject);
begin
  case CheckBoxSelected.IsChecked of
    True:
      begin
        RectangleBG.Stroke.Kind := TBrushKind.Solid;
      end;
    False:
      begin
        RectangleBG.Stroke.Kind := TBrushKind.None;
      end;
  end;
  if Assigned(FOnChangeCheck) then
    FOnChangeCheck(Self);
end;

constructor TFramePackageItemList.Create(AOwner: TComponent);
begin
  inherited;
  RectangleBG.Stroke.Kind := TBrushKind.None;
  Name := '';
  FItem := nil;
end;

destructor TFramePackageItemList.Destroy;
begin
  if Assigned(FItem) then
    FItem.Free;
  for var Item in FVersions do
    Item.Free;
  inherited;
end;

function TFramePackageItemList.FormatSize(const Value: string): string;
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

function TFramePackageItemList.GetId: string;
begin
  if Assigned(FItem) then
    Result := Fitem.Id
  else
    Result := '';
end;

function TFramePackageItemList.GetIsChecked: Boolean;
begin
  Result := CheckBoxSelected.IsChecked;
end;

procedure TFramePackageItemList.MenuItemDownloadClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.Id, TItemAction.Download);
end;

procedure TFramePackageItemList.MenuItemShowCommandClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.Id, TItemAction.CommandLine);
end;

procedure TFramePackageItemList.MenuItemWebSiteClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.LibProjectUrl, TItemAction.OpenUrl);
end;

procedure TFramePackageItemList.Fill(Item: TGetItPackage; Installed: Boolean);
begin
  if Assigned(FItem) then
    FItem.Free;
  FItem := Item;
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  LabelInfo.Text := Item.Version + ' (' + TGetIt.ParseDate(Item.Modified) + ')';
  LabelLicense.Text := Item.LibLicenseName;
  ButtonInstall.TagString := Item.LibUrl;
  IsInstalled := Installed;
  ButtonInstall.StylesData['arrow.OnClick'] := TValue.From<TNotifyEvent>(ButtonInstallOptClick);
  MenuItemDownload.Enabled := not Item.LibUrl.IsEmpty;
  MenuItemWebSite.Enabled := not Item.LibProjectUrl.IsEmpty;
  PathGutHub.Visible := Item.LibProjectUrl.ToLower.Contains('github.com') or Item.LibProjectUrl.ToLower.Contains('gitlab.com');
  //1 delphi
  //5 all
  //2 cpp
  //6 ?
  if (Item.LibCode = '1') then
  begin
    ImagePers.Bitmap.LoadFromResource('delphi');
    ImagePers.Hint := 'Delphi only';
  end
  else if Item.LibCode = '2' then
  begin
    ImagePers.Bitmap.LoadFromResource('cpp');
    ImagePers.Hint := 'C++ only';
  end
  else
  begin
    ImagePers.Bitmap.LoadFromResource('rad');
    ImagePers.Hint := 'For RAD Studio';
  end;
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

  RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(RectangleImage, Item.Image, False,
    procedure(Success: Boolean)
    begin
      if Success then
      begin
        RectangleImage.Fill.Kind := TBrushKind.Bitmap;
        RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      end;
    end);
  UpdateVersions;
end;

procedure TFramePackageItemList.RectangleBGClick(Sender: TObject);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self);
end;

procedure TFramePackageItemList.SetIsChecked(const Value: Boolean);
begin
  CheckBoxSelected.IsChecked := Value;
end;

procedure TFramePackageItemList.SetIsInstalled(const Value: Boolean);
begin
  FIsInstalled := Value;
  if not FIsInstalled then
    ButtonInstall.Text := 'INSTALL' + FormatSize(FItem.LibSize)
  else
    ButtonInstall.Text := 'UNINSTALL';
end;

procedure TFramePackageItemList.SetOnAction(const Value: TOnItemAction);
begin
  FOnAction := Value;
end;

procedure TFramePackageItemList.SetOnChangeCheck(const Value: TNotifyEvent);
begin
  FOnChangeCheck := Value;
end;

procedure TFramePackageItemList.SetOnItemClick(const Value: TNotifyEvent);
begin
  FOnItemClick := Value;
end;

end.

