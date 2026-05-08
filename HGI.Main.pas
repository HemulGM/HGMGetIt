unit HGI.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, HGI.View.Item,
  HGI.GetItAPI, FMX.Ani, FMX.Filter.Effects, FMX.Menus, HGI.Item,
  System.Threading, HGI.GetItCmd, System.ImageList, FMX.ImgList,
  HGI.View.ItemList, FMX.Effects, HGM.LineStorage;

{$SCOPEDENUMS ON}

type
  TViewStyle = (Card, List);

  TFormMain = class(TForm)
    LayoutHead: TLayout;
    LayoutMenu: TLayout;
    LayoutClient: TLayout;
    Rectangle2: TRectangle;
    StyleBook: TStyleBook;
    RadioButtonComponents: TRadioButton;
    RadioButtonAll: TRadioButton;
    RadioButtonLibs: TRadioButton;
    Layout3: TLayout;
    LabelCaption: TLabel;
    Path1: TPath;
    RadioButtonTrial: TRadioButton;
    RadioButtonIT: TRadioButton;
    RadioButtonIDPlugins: TRadioButton;
    RadioButtonStyles: TRadioButton;
    RadioButtonTools: TRadioButton;
    RadioButtonSampleProjects: TRadioButton;
    RadioButtonPatchesFixes: TRadioButton;
    VertScrollBoxCats: TVertScrollBox;
    VertScrollBoxContent: TVertScrollBox;
    LayoutDesc: TLayout;
    Layout1: TLayout;
    FlowLayoutItems: TFlowLayout;
    FramePackageItem3: TFramePackageItem;
    FramePackageItem4: TFramePackageItem;
    FramePackageItem5: TFramePackageItem;
    FramePackageItem6: TFramePackageItem;
    Circle1: TCircle;
    PathCurrentCat: TPath;
    LabelCurrentCatTitle: TLabel;
    LabelCurrentCatDesc: TLabel;
    TimerSearch: TTimer;
    RadioButtonPython: TRadioButton;
    LayoutLoading: TLayout;
    RectangleShd: TRectangle;
    FloatAnimationShd: TFloatAnimation;
    AniIndicator1: TAniIndicator;
    LayoutInfo: TLayout;
    LabelInfo: TLabel;
    LayoutMore: TLayout;
    ButtonMore: TButton;
    Layout2: TLayout;
    Layout4: TLayout;
    RadioButtonNew: TRadioButton;
    Line1: TLine;
    RadioButtonPromoted: TRadioButton;
    ButtonServer: TButton;
    ButtonServerList: TButton;
    PopupMenuServerList: TPopupMenu;
    MenuItemD11: TMenuItem;
    MenuItemD104: TMenuItem;
    MenuItemD103: TMenuItem;
    RadioButtonInstalled: TRadioButton;
    ImageList16: TImageList;
    LineStoragePath: TLineStorage;
    RadioButtonPlatforms: TRadioButton;
    Rectangle1: TRectangle;
    EditSearch: TEdit;
    ClearEditButtonSearch: TClearEditButton;
    ButtonPers: TButton;
    ButtonPersList: TButton;
    PopupMenuPersList: TPopupMenu;
    MenuItemPersDelphi: TMenuItem;
    MenuItemPersCPP: TMenuItem;
    LayoutActions: TLayout;
    ButtonOptions: TButton;
    Path2: TPath;
    PopupOptions: TPopup;
    Label1: TLabel;
    PopupMenuOrderList: TPopupMenu;
    MenuItemOrderName: TMenuItem;
    MenuItemOrderDate: TMenuItem;
    RadioButtonOrderName: TRadioButton;
    RadioButtonOrderDate: TRadioButton;
    Line2: TLine;
    RadioButtonTeeChart: TRadioButton;
    RadioButtonFonts: TRadioButton;
    RadioButtonHelp: TRadioButton;
    RadioButtonDUnit: TRadioButton;
    RadioButtonSamples: TRadioButton;
    Line3: TLine;
    RadioButtonInterbase: TRadioButton;
    RadioButtonIoT: TRadioButton;
    Label2: TLabel;
    RadioButtonViewCard: TRadioButton;
    RadioButtonViewList: TRadioButton;
    LayoutClientOver: TLayout;
    LayoutSelect: TLayout;
    RectangleSelect: TRectangle;
    ShadowEffect1: TShadowEffect;
    CheckBoxSelectAll: TCheckBox;
    ButtonInstall: TButton;
    ButtonUninstall: TButton;
    ButtonDownload: TButton;
    RadioButtonBooks: TRadioButton;
    procedure EditSearchChangeTracking(Sender: TObject);
    procedure LayoutHeadResized(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FlowLayoutItemsResized(Sender: TObject);
    procedure RadioButtonAllChange(Sender: TObject);
    procedure TimerSearchTimer(Sender: TObject);
    procedure FloatAnimationShdFinish(Sender: TObject);
    procedure ButtonMoreClick(Sender: TObject);
    procedure RadioButtonNewChange(Sender: TObject);
    procedure MenuItemD103Click(Sender: TObject);
    procedure ButtonServerListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemShowCommandClick(Sender: TObject);
    procedure ButtonPersListClick(Sender: TObject);
    procedure MenuItemPersClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure MenuItemOrderClick(Sender: TObject);
    procedure RadioButtonOrderNameChange(Sender: TObject);
    procedure RadioButtonViewCardChange(Sender: TObject);
    procedure CheckBoxSelectAllChange(Sender: TObject);
  private
    FInited: Boolean;
    FCategory: string;
    FOffset: Integer;
    FOrder: Integer;
    FIDEList: TArray<TIDEEntity>;
    FCurrentIDE: TIDEEntity;
    FIsNew: Boolean;
    FLastSearch: string;
    FGetItCmd: TGetItCmd;
    FPers: string;
    FGlobalOrder: Integer;
    FViewStyle: TViewStyle;
    FLoading: Boolean;
    FManChangeCheck: Boolean;
    function GetPers: string;
    procedure LoadPackages(More: Boolean);
    procedure ClearItems;
    procedure LoadingBegin;
    procedure LoadingEnd;
    procedure NeedMore(Value: Boolean);
    procedure SetCurrentIDE(const Version: string);
    function IsInstalled(const Id: string): Boolean;
    procedure FOnItemAction(Sender: TObject; const ItemId: string; Action: TItemAction);
    procedure AddToInstall(const ItemId: string);
    procedure AddToUninstall(const ItemId: string);
    procedure RemoveInstalled(const ItemId: string);
    procedure AddInstalled(const ItemId: string);
    function ExistsItem(const ItemId: string): Boolean;
    procedure SetItemInstallState(const ItemId: string; State: Boolean);
    procedure DownloadItem(const ItemId: string);
    function GetItemById(const ItemId: string; out Item: TGetItPackage): Boolean;
    procedure ShowCommandLine(const ItemId: string);
    procedure SetPers(const Value: string);
    procedure SetOrder(const Value: Integer);
    procedure LoadCustomServers;
    procedure SetViewStyle(const Value: TViewStyle);
    procedure RefillList(Items: TArray<TGetItPackage>; More: Boolean);
    procedure FOnItemChangeCheck(Sender: TObject);
    procedure CalcSelected;
    procedure HideSelectPanel;
    procedure ShowSelectPanel;
    function ExistsItemByName(const ItemName: string; var Item: TFrame): Boolean;
    procedure AddAsVersion(Frame: TFrame; Item: TGetItPackage);
    procedure UpdateCategories(Items: TCategories);
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle;
  end;

const
  CardMinW = 320;
  PageSize = 50;

var
  FormMain: TFormMain;

procedure OpenUrl(const URL: string);

implementation

uses
  System.Math, System.IniFiles, System.IOUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, DarkModeApi.FMX, Winapi.ShellAPI, FMX.Platform.Win,
  {$ENDIF}
  HGM.ObjectHolder;

{$R *.fmx}

procedure OpenUrl(const URL: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(ApplicationHWND, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}
end;

procedure TFormMain.EditSearchChangeTracking(Sender: TObject);
begin
  ClearEditButtonSearch.Visible := not EditSearch.Text.IsEmpty;
  TimerSearch.Enabled := False;
  TimerSearch.Enabled := True;
end;

procedure TFormMain.LoadingBegin;
begin
  LabelInfo.Text := 'Loading ...';
  LayoutInfo.Visible := True;
  LayoutLoading.Visible := True;
  RectangleShd.Opacity := 0;
  FloatAnimationShd.Enabled := False;
  FloatAnimationShd.Inverse := False;
  FloatAnimationShd.StartFromCurrent := True;
  FloatAnimationShd.Start;
end;

procedure TFormMain.LoadingEnd;
begin
  FloatAnimationShd.Enabled := False;
  FloatAnimationShd.Inverse := True;
  FloatAnimationShd.StartFromCurrent := True;
  FloatAnimationShd.Start;
end;

procedure TFormMain.ButtonMoreClick(Sender: TObject);
begin
  LoadPackages(True);
end;

procedure TFormMain.ButtonOptionsClick(Sender: TObject);
begin
  PopupOptions.PlacementTarget := ButtonOptions;
  var Pt: TPointF := ClientToScreen(ButtonOptions.AbsoluteRect.TopLeft);
  Pt.Offset(0, ButtonOptions.Height);
  PopupOptions.Placement := TPlacement.Left;
  //PopupOptions.PlacementRectangle.Rect := TRectF.Create(Pt, PopupOptions.Width, PopupOptions.Height);
  PopupOptions.Popup;
end;

procedure TFormMain.ButtonPersListClick(Sender: TObject);
begin
  PopupMenuPersList.PopupComponent := ButtonPers;
  var Pt: TPointF := ClientToScreen(ButtonPers.AbsoluteRect.TopLeft);
  Pt.Offset(0, ButtonPers.Height);
  PopupMenuPersList.Popup(Pt.X, Pt.Y);
end;

procedure TFormMain.ButtonServerListClick(Sender: TObject);
begin
  PopupMenuServerList.PopupComponent := ButtonServer;
  var Pt: TPointF := ClientToScreen(ButtonServer.AbsoluteRect.TopLeft);
  Pt.Offset(0, ButtonServer.Height);
  PopupMenuServerList.Popup(Pt.X, Pt.Y);
end;

procedure TFormMain.CheckBoxSelectAllChange(Sender: TObject);
begin
  if FManChangeCheck then
    Exit;
  FManChangeCheck := True;
  for var Control in FlowLayoutItems.Controls do
    if Control is TFramePackageItemList then
      TFramePackageItemList(Control).IsChecked := CheckBoxSelectAll.IsChecked;
  FManChangeCheck := False;
  CalcSelected;
end;

procedure TFormMain.FOnItemChangeCheck(Sender: TObject);
begin
  if FManChangeCheck then
    Exit;
  CalcSelected;
end;

procedure TFormMain.CalcSelected;
begin
  var SelCnt := 0;
  var ItemCnt := 0;
  var Installed := 0;
  for var Control in FlowLayoutItems.Controls do
    if Control is TFramePackageItemList then
    begin
      if TFramePackageItemList(Control).IsInstalled then
        Inc(Installed);
      Inc(ItemCnt);
      if TFramePackageItemList(Control).IsChecked then
        Inc(SelCnt);
    end;
  ButtonUninstall.Enabled := ItemCnt = Installed;
  ButtonInstall.Enabled := Installed = 0;
  if SelCnt > 0 then
    ShowSelectPanel
  else
    HideSelectPanel;
  FManChangeCheck := True;
  CheckBoxSelectAll.IsChecked := ItemCnt = SelCnt;
  FManChangeCheck := False;
end;

procedure TFormMain.HideSelectPanel;
begin
  LayoutSelect.Visible := False;
end;

procedure TFormMain.ShowSelectPanel;
begin
  LayoutSelect.Visible := True;
end;

procedure TFormMain.ClearItems;
begin
  FlowLayoutItems.BeginUpdate;
  try
    while FlowLayoutItems.ControlsCount > 0 do
      FlowLayoutItems.Controls[0].Free;
  finally
    FlowLayoutItems.EndUpdate;
  end;
  FlowLayoutItems.RecalcSize;
end;

procedure TFormMain.NeedMore(Value: Boolean);
begin
  LayoutMore.Visible := Value;
end;

function TFormMain.IsInstalled(const Id: string): Boolean;
begin
  for var Item in FCurrentIDE.Elements do
    if Item = Id then
      Exit(True);
  Result := False;
end;

function TFormMain.ExistsItem(const ItemId: string): Boolean;
begin
  for var Control in FlowLayoutItems.Controls do
  begin
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
        Exit(True);
    if Control is TFramePackageItemList then
      if TFramePackageItemList(Control).Id = ItemId then
        Exit(True);
  end;
  Result := False;
end;

function TFormMain.ExistsItemByName(const ItemName: string; var Item: TFrame): Boolean;
begin
  for var Control in FlowLayoutItems.Controls do
  begin
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Item.Name = ItemName then
      begin
        Item := TFramePackageItem(Control);
        Exit(True);
      end;
    if Control is TFramePackageItemList then
      if TFramePackageItemList(Control).Item.Name = ItemName then
      begin
        Item := TFramePackageItemList(Control);
        Exit(True);
      end;
  end;
  Result := False;
end;

function TFormMain.GetItemById(const ItemId: string; out Item: TGetItPackage): Boolean;
begin
  for var Control in FlowLayoutItems.Controls do
  begin
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
      begin
        Item := TFramePackageItem(Control).Item;
        Exit(Assigned(Item));
      end;
    if Control is TFramePackageItemList then
      if TFramePackageItemList(Control).Id = ItemId then
      begin
        Item := TFramePackageItemList(Control).Item;
        Exit(Assigned(Item));
      end;
  end;
  Result := False;
end;

function TFormMain.GetPers: string;
begin
  Result := FPers;
end;

procedure TFormMain.SetItemInstallState(const ItemId: string; State: Boolean);
begin
  for var Control in FlowLayoutItems.Controls do
  begin
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
        TFramePackageItem(Control).IsInstalled := State;
    if Control is TFramePackageItemList then
      if TFramePackageItemList(Control).Id = ItemId then
        TFramePackageItemList(Control).IsInstalled := State;
  end;
end;

procedure TFormMain.LoadPackages(More: Boolean);
begin
  if not FInited then
    Exit;

  FLoading := True;
  if not More then
    FOffset := 0
  else
    Inc(FOffset, PageSize);
  NeedMore(False);
  if FIsNew then
    FOrder := 2
  else
    FOrder := FGlobalOrder;
  LoadingBegin;
  var Pers := GetPers;
  var Search := EditSearch.Text;
  var Category := FCategory;
  var Order := FOrder;
  var Offset := FOffset;
  TaskRun(Self,
    procedure(Holder: IComponentHolder)
    begin
      try
        var Items: TPackages := nil;
        var Categories: TCategories := nil;
        try
          if not More then
          begin
            TGetIt.Categories(Categories, Pers);
          end;

          if Category <> '-1000' then
            TGetIt.Get(Items, Category, Order, Pers, Search, PageSize, Offset)
          else
            FCurrentIDE.LoadInstalled(Items, Search);
          Sync(
            procedure
            begin
              if not Holder.IsLive then
                Exit;
              if Assigned(Items) then
              begin
                var List := Items.Items;
                Items.Items := [];
                RefillList(List, More);
              end
              else
                RefillList([], More);
              if Assigned(Categories) then
              begin
                UpdateCategories(Categories);
              end;
            end);
        finally
          Items.Free;
          Categories.Free;
        end;
      except
        Queue(
          procedure
          begin
            if not Holder.IsLive then
              Exit;
            LabelInfo.Text := 'Error';
            LayoutInfo.Visible := True;
            NeedMore(More);
          end);
      end;
    end);
end;

procedure TFormMain.AddAsVersion(Frame: TFrame; Item: TGetItPackage);
begin
  if Frame is TFramePackageItem then
    TFramePackageItem(Frame).AddVersion(Item)
  else if Frame is TFramePackageItemList then
    TFramePackageItemList(Frame).AddVersion(Item);
end;

procedure TFormMain.RefillList(Items: TArray<TGetItPackage>; More: Boolean);
begin
  FLoading := False;
  if not More then
    ClearItems;
  FlowLayoutItems.BeginUpdate;
  try
    for var Item in Items do
    begin
      if not Assigned(Item) then
        Continue;
      if ExistsItem(Item.Id) then
      begin
        Item.Free;
        Continue;
      end;
      var ItemFrame: TFrame := nil;
      if ExistsItemByName(Item.Name, ItemFrame) then
      begin
        AddAsVersion(ItemFrame, Item);
        Continue;
      end;
      case ViewStyle of
        TViewStyle.Card:
          begin
            var Frame := TFramePackageItem.Create(FlowLayoutItems);
            Frame.Fill(Item, IsInstalled(Item.Id));
            Frame.Parent := FlowLayoutItems;
            Frame.OnAction := FOnItemAction;
          end;
        TViewStyle.List:
          begin
            var Frame := TFramePackageItemList.Create(FlowLayoutItems);
            Frame.Fill(Item, IsInstalled(Item.Id));
            Frame.Parent := FlowLayoutItems;
            Frame.Align := TAlignLayout.Top;
            Frame.OnAction := FOnItemAction;
            Frame.OnChangeCheck := FOnItemChangeCheck;
          end;
      end;
    end;
  finally
    FlowLayoutItems.EndUpdate;
  end;
  FlowLayoutItems.RecalcSize;
  if not More then
    VertScrollBoxContent.ViewportPosition := TPointF.Create(0, 0);
  if FlowLayoutItems.ControlsCount <= 0 then
  begin
    LabelInfo.Text := 'No results';
    LayoutInfo.Visible := True;
  end
  else
    LayoutInfo.Visible := False;
  NeedMore((Length(Items) >= PageSize) and (FCategory <> '-1000'));

  LoadingEnd;
  CalcSelected;
end;

procedure TFormMain.MenuItemD103Click(Sender: TObject);
var
  Item: TMenuItem absolute Sender;
begin
  SetCurrentIDE(Item.TagString);
  LoadPackages(False);
end;

procedure TFormMain.MenuItemOrderClick(Sender: TObject);
var
  Item: TMenuItem absolute Sender;
begin
  SetOrder(Item.Tag);
  LoadPackages(False);
end;

procedure TFormMain.MenuItemPersClick(Sender: TObject);
var
  Item: TMenuItem absolute Sender;
begin
  SetPers(Item.Tag.ToString);
  LoadPackages(False);
end;

procedure TFormMain.MenuItemShowCommandClick(Sender: TObject);
begin
  //
end;

procedure TFormMain.FloatAnimationShdFinish(Sender: TObject);
begin
  if FloatAnimationShd.Inverse then
    LayoutLoading.Visible := False;
end;

procedure TFormMain.FlowLayoutItemsResized(Sender: TObject);
begin
  if FLoading then
    Exit;
  case ViewStyle of
    TViewStyle.Card:
      begin
        var Cnt := Trunc((FlowLayoutItems.Width) / (CardMinW + 20));
        var CardW := Max(CardMinW, (FlowLayoutItems.Width) / Cnt);
        if CardW = Infinity then
          CardW := CardMinW;
        var H: Single := 0;
        for var Control in FlowLayoutItems.Controls do
        begin
          Control.Width := CardW - 20;
          H := Max(H, Control.Position.Y + Control.Height);
        end;
        FlowLayoutItems.Height := H;
      end;
    TViewStyle.List:
      begin
        var H: Single := 0;
        for var Control in FlowLayoutItems.Controls do
        begin
          Control.Width := FlowLayoutItems.Width - 20;
          H := Max(H, Control.Position.Y + Control.Height);
        end;
        FlowLayoutItems.Height := H;
      end;
  end;
end;

procedure TFormMain.AddToInstall(const ItemId: string);
begin
  if FCurrentIDE.IsCustom then
  begin
    ShowMessage('Installation or deinstallation is not possible with a custom server');
    Exit;
  end;
  if FGetItCmd.Execute(FCurrentIDE, TGetItCommand.Create.Install([ItemId]).Config(UseOnline).AcceptEULAs) then
    AddInstalled(ItemId);
end;

procedure TFormMain.RemoveInstalled(const ItemId: string);
begin
  for var i := Low(FCurrentIDE.Elements) to High(FCurrentIDE.Elements) do
    if FCurrentIDE.Elements[i] = ItemId then
    begin
      Delete(FCurrentIDE.Elements, i, 1);
      SetItemInstallState(ItemId, False);
      Exit;
    end;
end;

procedure TFormMain.AddInstalled(const ItemId: string);
begin
  if IsInstalled(ItemId) then
    Exit;
  SetLength(FCurrentIDE.Elements, Length(FCurrentIDE.Elements) + 1);
  FCurrentIDE.Elements[High(FCurrentIDE.Elements)] := ItemId;
  SetItemInstallState(ItemId, True);
end;

procedure TFormMain.AddToUninstall(const ItemId: string);
begin
  if FCurrentIDE.IsCustom then
  begin
    ShowMessage('Installation or deinstallation is not possible with a custom server');
    Exit;
  end;
  if FGetItCmd.Execute(FCurrentIDE, TGetItCommand.Create.Uninstall([ItemId])) then
    RemoveInstalled(ItemId);
end;

procedure TFormMain.ShowCommandLine(const ItemId: string);
begin
  ShowMessage(FCurrentIDE.GetPathGetItCmd + ' ' + TGetItCommand.Create.Install([ItemId]).Config(UseOnline).AcceptEULAs.Build);
end;

procedure TFormMain.DownloadItem(const ItemId: string);
begin
  var Item: TGetItPackage;
  if GetItemById(ItemId, Item) then
    OpenUrl(Item.LibUrl);
end;

procedure TFormMain.FOnItemAction(Sender: TObject; const ItemId: string; Action: TItemAction);
begin
  case Action of
    TItemAction.Install:
      AddToInstall(ItemId);
    TItemAction.Download:
      DownloadItem(ItemId);
    TItemAction.Uninstall:
      AddToUninstall(ItemId);
    TItemAction.OpenUrl:
      OpenUrl(ItemId);
    TItemAction.CommandLine:
      ShowCommandLine(ItemId);
  end;
end;

procedure TFormMain.SetCurrentIDE(const Version: string);
begin
  if Length(FIDEList) = 0 then
  begin
    FCurrentIDE.Version := '';
    FCurrentIDE.RootDir := '';
    FCurrentIDE.Personalities := 'Default (Olympus)';
    FCurrentIDE.ServiceUrl := 'https://getit-olympus.embarcadero.com';
    FCurrentIDE.Elements := [];
  end
  else
  begin
    var Found: Boolean := False;
    for var IDE in FIDEList do
    begin
      if (not IDE.Version.IsEmpty) and (IDE.Version = Version) then
      begin
        FCurrentIDE := IDE;
        Found := True;
        Break;
      end;
    end;
    if not Found then
      FCurrentIDE := FIDEList[High(FIDEList)];
  end;

  ButtonServer.Text := FCurrentIDE.Personalities;
  TGetIt.Url := FCurrentIDE.ServiceUrl;
  TGetIt.Version := FCurrentIDE.Version;
end;

procedure TFormMain.SetPers(const Value: string);
begin
  FPers := Value;
  for var i := 0 to PopupMenuPersList.ItemsCount - 1 do
    if PopupMenuPersList.Items[i].Tag.ToString = Value then
    begin
      ButtonPers.Text := PopupMenuPersList.Items[i].Text;
      Break;
    end;
end;

procedure TFormMain.SetViewStyle(const Value: TViewStyle);
begin
  if FViewStyle = Value then
    Exit;
  FViewStyle := Value;
  case FViewStyle of
    TViewStyle.Card:
      begin
        FlowLayoutItems.VerticalGap := 20;
        FlowLayoutItems.HorizontalGap := 20;
      end;
    TViewStyle.List:
      begin
        FlowLayoutItems.VerticalGap := 5;
        FlowLayoutItems.HorizontalGap := 20;
      end;
  end;
  LoadPackages(False);
end;

procedure TFormMain.SetOrder(const Value: Integer);
begin
  FGlobalOrder := Value;
  if RadioButtonOrderName.IsChecked <> (RadioButtonOrderName.Tag = Value) then
    RadioButtonOrderName.IsChecked := RadioButtonOrderName.Tag = Value;
  if RadioButtonOrderDate.IsChecked <> (RadioButtonOrderDate.Tag = Value) then
    RadioButtonOrderDate.IsChecked := RadioButtonOrderDate.Tag = Value;
end;

procedure TFormMain.LoadCustomServers;
begin
  try
    if TFile.Exists(TPath.Combine(ExtractFilePath(ParamStr(0)), 'servers.ini')) then
    begin
      var Ini := TIniFile.Create(TPath.Combine(ExtractFilePath(ParamStr(0)), 'servers.ini'));
      try
        var Sections := TStringList.Create;
        try
          Ini.ReadSections(Sections);
          for var Section in Sections do
          begin
            var Url := Ini.ReadString(Section, 'url', '');
            if Url.IsEmpty then
              Continue;
            var Version := Ini.ReadString(Section, 'version', '');
            if Version.IsEmpty then
              Continue;

            var IDE: TIDEEntity;
            IDE.ServiceUrl := Url;
            IDE.Version := Version;
            IDE.Personalities := Section;
            IDE.IsCustom := True;
            FIDEList := [IDE] + FIDEList;
          end;
        finally
          Sections.Free;
        end;
      finally
        Ini.Free;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Error of reading servers.ini:' + #13#10 + E.Message);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  try
    SetWindowColorModeAsSystem;
  except
    // never mind
  end;
  {$ENDIF}
  FGetItCmd := TGetItCmd.Create;
  FLoading := False;
  ClearItems;
  PopupMenuServerList.Clear;
  FIDEList := TIDEList.List;
  LoadCustomServers;
  var IsHaveCustom := False;
  for var IDE in FIDEList do
    if IDE.IsCustom then
    begin
      var Item := TMenuItem.Create(PopupMenuServerList);
      Item.Text := IDE.Personalities + ' (' + IDE.Version + ')';
      Item.TagString := IDE.Version;
      Item.OnClick := MenuItemD103Click;
      Item.ImageIndex := 0;
      PopupMenuServerList.AddObject(Item);
      IsHaveCustom := True;
    end;
  if IsHaveCustom then
  begin
    var Item := TMenuItem.Create(PopupMenuServerList);
    Item.Text := '-';
    PopupMenuServerList.AddObject(Item);
  end;
  for var IDE in FIDEList do
    if not IDE.IsCustom then
    begin
      var Item := TMenuItem.Create(PopupMenuServerList);
      Item.Text := IDE.Personalities + ' (' + IDE.Version + ')';
      Item.TagString := IDE.Version;
      Item.OnClick := MenuItemD103Click;
      Item.ImageIndex := 0;
      PopupMenuServerList.AddObject(Item);
    end;
  LayoutSelect.Visible := False;
  EditSearch.DisableDisappear := True;
  SetViewStyle(TViewStyle.Card);
  SetOrder(0);
  SetPers('1');
  SetCurrentIDE('');
  FOffset := 0;
  LabelInfo.Text := 'Loading ...';
  VertScrollBoxCats.AniCalculations.Animation := True;
  VertScrollBoxContent.AniCalculations.Animation := True;

  RadioButtonNew.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('new');
  RadioButtonNew.TagString := '98; 35';
  RadioButtonPromoted.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('promoted');
  RadioButtonPromoted.TagString := '998';
  RadioButtonAll.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('all');
  RadioButtonAll.TagString := '';
  RadioButtonInstalled.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('installed');
  RadioButtonInstalled.TagString := '-1000';
  RadioButtonLibs.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('lib');
  RadioButtonLibs.TagString := '1';
  RadioButtonComponents.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('comps');
  RadioButtonComponents.TagString := '2';
  RadioButtonTrial.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('trial');
  RadioButtonTrial.TagString := '33';
  RadioButtonTools.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('tools');
  RadioButtonTools.TagString := '46';
  RadioButtonStyles.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('styles');
  RadioButtonStyles.TagString := '38';
  RadioButtonPatchesFixes.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('patches');
  RadioButtonPatchesFixes.TagString := '999';
  RadioButtonIDPlugins.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('plugins');
  RadioButtonIDPlugins.TagString := '37';
  RadioButtonSamples.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('samples');
  RadioButtonSamples.TagString := '16';
  RadioButtonSampleProjects.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('samples');
  RadioButtonSampleProjects.TagString := '99; 40';
  RadioButtonIT.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('industry');
  RadioButtonIT.TagString := '36';
  RadioButtonIoT.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('iot');
  RadioButtonIoT.TagString := '4';
  RadioButtonPlatforms.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('platforms');
  RadioButtonPlatforms.TagString := '15';
  RadioButtonTeeChart.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('teechart');
  RadioButtonTeeChart.TagString := '21';
  RadioButtonDUnit.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('dunit');
  RadioButtonDUnit.TagString := '22';
  RadioButtonInterbase.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('interbase');
  RadioButtonInterbase.TagString := '23; 42';
  RadioButtonHelp.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('help');
  RadioButtonHelp.TagString := '17';
  RadioButtonFonts.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('fonts');
  RadioButtonFonts.TagString := '14';
  RadioButtonPython.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('python');
  RadioButtonPython.TagString := '47';
  RadioButtonBooks.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('books');
  RadioButtonBooks.TagString := '1008';

  RadioButtonNew.IsChecked := True;
  LayoutMore.Visible := False;
  FInited := True;
  RadioButtonNewChange(RadioButtonNew);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FGetItCmd.Free;
end;

procedure TFormMain.LayoutHeadResized(Sender: TObject);
begin
  EditSearch.Width := Min(460, LayoutHead.Width) - 20;
  EditSearch.Position.X := LayoutHead.Width / 2 - EditSearch.Width / 2;
  if EditSearch.Width > (LayoutActions.Position.X - EditSearch.Position.X - 20) then
  begin
    EditSearch.Width := Min(460, LayoutActions.Position.X) - 20;
    EditSearch.Position.X := LayoutActions.Position.X / 2 - EditSearch.Width / 2;
  end;
end;

procedure TFormMain.RadioButtonAllChange(Sender: TObject);
var
  Button: TRadioButton absolute Sender;
begin
  if Button.IsChecked then
  begin
    PathCurrentCat.Data.Data := Button.StylesData['icon.Data.Data'].AsString;
    LabelCurrentCatTitle.Text := Button.Text;
    LabelCurrentCatDesc.Text := Button.Hint;
    FLastSearch := '';
    EditSearch.Text := '';
    FIsNew := False;
    FCategory := Button.TagString;
    LoadPackages(False);
  end;
end;

procedure TFormMain.RadioButtonNewChange(Sender: TObject);
var
  Button: TRadioButton absolute Sender;
begin
  if Button.IsChecked then
  begin
    PathCurrentCat.Data.Data := Button.StylesData['icon.Data.Data'].AsString;
    LabelCurrentCatTitle.Text := Button.Text;
    LabelCurrentCatDesc.Text := Button.Hint;
    FCategory := '-1';
    FIsNew := True;
    FLastSearch := '';
    EditSearch.Text := '';
    LoadPackages(False);
  end;
end;

procedure TFormMain.RadioButtonOrderNameChange(Sender: TObject);
begin
  if RadioButtonOrderName.IsChecked then
  begin
    if FGlobalOrder = 0 then
      Exit;
    SetOrder(0);
  end
  else if RadioButtonOrderDate.IsChecked then
  begin
    if FGlobalOrder = 2 then
      Exit;
    SetOrder(2)
  end
  else
  begin
    if FGlobalOrder = 0 then
      Exit;
    SetOrder(0);
  end;
  LoadPackages(False);
end;

procedure TFormMain.RadioButtonViewCardChange(Sender: TObject);
begin
  if RadioButtonViewCard.IsChecked then
  begin
    ViewStyle := TViewStyle.Card;
  end
  else if RadioButtonViewList.IsChecked then
  begin
    ViewStyle := TViewStyle.List;
  end
  else
  begin
    ViewStyle := TViewStyle.Card;
  end;
end;

procedure TFormMain.TimerSearchTimer(Sender: TObject);
begin
  TimerSearch.Enabled := False;
  if FLastSearch <> EditSearch.Text then
  begin
    FLastSearch := EditSearch.Text;
    LoadPackages(False);
  end;
end;

procedure TFormMain.UpdateCategories(Items: TCategories);
begin
  for var Control in VertScrollBoxCats.Content.Controls do
    if Control is TRadioButton then
    begin
      var Button := TRadioButton(Control);
      var IDs := Button.TagString.Replace(' ', '', [rfReplaceAll]).Split([';']);
      var Founded := False;
      for var Item in Items.Items do
      begin
        for var Id in IDs do
          if Item.Id = Id then
          begin
            if Id = '35' then
              Button.StylesData['num'] := '●'
            else
              Button.StylesData['num'] := Item.NumElements;
            Founded := True;
            Break;
          end;
        if Founded then
          Break;
      end;
    end;
end;

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.

