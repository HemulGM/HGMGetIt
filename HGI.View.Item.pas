unit HGI.View.Item;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, HGI.Item,
  FMX.Filter.Effects, FMX.Effects, FMX.Menus, FMX.Ani;

type
  TFramePackageItem = class(TFrame)
    RectangleBG: TRectangle;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    RectangleImage: TRectangle;
    ButtonInstall: TButton;
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
    ButtonInstallOpt: TButton;
    ImagePers: TImage;
    Line1: TLine;
    PopupMenuOpt: TPopupMenu;
    MenuItemDownload: TMenuItem;
    MenuItemWebSite: TMenuItem;
    MenuItemShowCommand: TMenuItem;
    PathGutHub: TPath;
    ColorAnimation1: TColorAnimation;
    LabelVersions: TLabel;
    procedure RectangleBGClick(Sender: TObject);
    procedure ButtonInstallClick(Sender: TObject);
    procedure ButtonInstallOptClick(Sender: TObject);
    procedure MenuItemDownloadClick(Sender: TObject);
    procedure MenuItemWebSiteClick(Sender: TObject);
    procedure MenuItemShowCommandClick(Sender: TObject);
  private
    FItem: TGetItPackage;
    FOnAction: TOnItemAction;
    FIsInstalled: Boolean;
    FVersions: TArray<TGetItPackage>;
    function FormatSize(const Value: string): string;
    procedure SetOnAction(const Value: TOnItemAction);
    function GetId: string;
    procedure SetIsInstalled(const Value: Boolean);
  public
    procedure Fill(Item: TGetItPackage; Installed: Boolean);
    property OnAction: TOnItemAction read FOnAction write SetOnAction;
    constructor Create(AOwner: TComponent); override;
    property Id: string read GetId;
    property IsInstalled: Boolean read FIsInstalled write SetIsInstalled;
    property Item: TGetItPackage read FItem;
    procedure AddVersion(Item: TGetItPackage);
    procedure UpdateVersions;
    destructor Destroy; override;
  end;

implementation

uses
  HGM.FMX.Image, System.Math, HGI.View.ItemFull, HGI.GetItAPI;

{$R *.fmx}

{ TFramePackageItem }

procedure TFramePackageItem.AddVersion(Item: TGetItPackage);
begin
  FVersions := FVersions + [Item];
  UpdateVersions;
end;

procedure TFramePackageItem.ButtonInstallClick(Sender: TObject);
begin
  if Assigned(FOnAction) then
    if FIsInstalled then
      FOnAction(Self, FItem.Id, TItemAction.Uninstall)
    else
      FOnAction(Self, FItem.Id, TItemAction.Install);
end;

procedure TFramePackageItem.ButtonInstallOptClick(Sender: TObject);
begin
  var Pt := Application.MainForm.ClientToScreen(ButtonInstall.AbsoluteRect.TopLeft);
  PopupMenuOpt.Popup(Pt.X, Pt.Y + ButtonInstall.Height);
end;

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
  for var Item in FVersions do
    Item.Free;
  inherited;
end;

function TFramePackageItem.FormatSize(const Value: string): string;
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

function TFramePackageItem.GetId: string;
begin
  if Assigned(FItem) then
    Result := Fitem.Id
  else
    Result := '';
end;

procedure TFramePackageItem.MenuItemDownloadClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.Id, TItemAction.Download);
end;

procedure TFramePackageItem.MenuItemShowCommandClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.Id, TItemAction.CommandLine);
end;

procedure TFramePackageItem.MenuItemWebSiteClick(Sender: TObject);
begin
  if Assigned(FItem) and Assigned(FOnAction) then
    FOnAction(Self, FItem.LibProjectUrl, TItemAction.OpenUrl);
end;

procedure TFramePackageItem.Fill(Item: TGetItPackage; Installed: Boolean);
begin
  if Assigned(FItem) then
    FItem.Free;
  FItem := Item;
  LabelTitle.Text := Item.Name;
  LabelDesc.Text := Item.Description;
  LabelInfo.Text := Item.Version + ' (' + TGetIt.ParseDate(Item.Modified) + ')' + #13#10 + 'by ' + Item.Vendor;
  LabelLicense.Text := Item.LibLicenseName;
  ButtonInstall.TagString := Item.LibUrl;
  IsInstalled := Installed;
  ButtonInstall.StylesData['bg.Padding.Right'] := ButtonInstallOpt.Width + 5;
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

procedure TFramePackageItem.RectangleBGClick(Sender: TObject);
begin
  var Frame := TFramePackageItemFull.Create(Self);
  Frame.Parent := Application.MainForm;
  Frame.Fill(FItem, FIsInstalled, FVersions);
  Frame.RecalcSize;
  Frame.OnAction := OnAction;
end;

procedure TFramePackageItem.SetIsInstalled(const Value: Boolean);
begin
  FIsInstalled := Value;
  if not FIsInstalled then
    ButtonInstall.Text := 'INSTALL' + FormatSize(FItem.LibSize)
  else
    ButtonInstall.Text := 'UNINSTALL';
end;

procedure TFramePackageItem.SetOnAction(const Value: TOnItemAction);
begin
  FOnAction := Value;
end;

procedure TFramePackageItem.UpdateVersions;
begin
  LabelVersions.Visible := Length(FVersions) > 0;
  LabelVersions.Text := 'Несколько версий (' + Length(FVersions).ToString + ')';
end;

end.

