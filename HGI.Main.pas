unit HGI.Main;

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls,
  HGI.View.Item, HGI.GetItAPI, FMX.Ani, FMX.Filter.Effects, FMX.Menus, HGI.Item,
  System.Threading, HGI.GetItCmd, System.ImageList, FMX.ImgList, HGM.LineStorage;

type
  TFormMain = class(TForm)
    LayoutHead: TLayout;
    LayoutMenu: TLayout;
    LayoutClient: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    EditSearch: TEdit;
    StyleBook: TStyleBook;
    ClearEditButtonSearch: TClearEditButton;
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
    RadioButtonSamples: TRadioButton;
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
  private
    FInited: Boolean;
    FCategory: Integer;
    FOffset: Integer;
    FOrder: Integer;
    FIDEList: TArray<TIDEEntity>;
    FCurrentIDE: TIDEEntity;
    FPool: TTHreadPool;
    FLastSearch: string;
    FGetItCmd: TGetItCmd;
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
  end;

const
  CardMinW = 320;
  PageSize = 50;

var
  FormMain: TFormMain;

procedure OpenUrl(const URL: string);

implementation

uses
  System.Math, System.IOUtils, DarkModeApi.FMX, Winapi.ShellAPI,
  FMX.Platform.Win;

{$R *.fmx}

procedure OpenUrl(const URL: string);
begin
  ShellExecute(ApplicationHWND, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
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

procedure TFormMain.ButtonServerListClick(Sender: TObject);
begin
  PopupMenuServerList.PopupComponent := ButtonServer;
  var Pt: TPointF := ClientToScreen(ButtonServer.AbsoluteRect.TopLeft);
  Pt.Offset(0, ButtonServer.Height);
  PopupMenuServerList.Popup(Pt.X, Pt.Y);
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
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
        Exit(True);
  Result := False;
end;

function TFormMain.GetItemById(const ItemId: string; out Item: TGetItPackage): Boolean;
begin
  for var Control in FlowLayoutItems.Controls do
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
      begin
        Item := TFramePackageItem(Control).Item;
        Exit(Assigned(Item));
      end;
  Result := False;
end;

procedure TFormMain.SetItemInstallState(const ItemId: string; State: Boolean);
begin
  for var Control in FlowLayoutItems.Controls do
    if Control is TFramePackageItem then
      if TFramePackageItem(Control).Id = ItemId then
        TFramePackageItem(Control).IsInstalled := State;
end;

procedure TFormMain.LoadPackages(More: Boolean);
begin
  if not FInited then
    Exit;
  if not More then
    FOffset := 0
  else
    Inc(FOffset, PageSize);
  NeedMore(False);
  LoadingBegin;
  TTask.Run(
    procedure
    var
      Items: TPackages;
    begin
      try
        Items := nil;
        try
          if FCategory <> -1000 then
            TGetIt.Get(Items, FCategory, FOrder, EditSearch.Text, PageSize, FOffset)
          else
            FCurrentIDE.LoadInstalled(Items, EditSearch.Text);
        finally
          TThread.Queue(nil,
            procedure
            begin
              if not More then
                ClearItems;
              if Assigned(Items) then
              try
                for var Item in Items.Items do
                begin
                  if ExistsItem(Item.Id) then
                  begin
                    Item.Free;
                    Continue;
                  end;
                  var Frame := TFramePackageItem.Create(FlowLayoutItems);
                  Frame.Fill(Item, IsInstalled(Item.Id));
                  Frame.Parent := FlowLayoutItems;
                  Frame.OnAction := FOnItemAction;
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
                NeedMore((Length(Items.Items) >= PageSize) and (FCategory <> -1000));
              finally
                Items.Items := [];
                Items.Free
              end
              else
              begin
                LabelInfo.Text := 'Error';
                LayoutInfo.Visible := True;
                NeedMore(More);
              end;
              LoadingEnd;
            end);
        end;
      except
        TThread.Queue(nil,
          procedure
          begin
            LabelInfo.Text := 'Error';
            LayoutInfo.Visible := True;
            NeedMore(More);
          end);
      end;
    end, FPool);
end;

procedure TFormMain.MenuItemD103Click(Sender: TObject);
var
  Item: TMenuItem absolute Sender;
begin
  SetCurrentIDE(Item.TagString);
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

procedure TFormMain.AddToInstall(const ItemId: string);
begin
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
      if IDE.Version = Version then
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  try
    SetWindowColorModeAsSystem;
  except
    // never mind
  end;
  FPool := TThreadPool.Create;
  FGetItCmd := TGetItCmd.Create;
  ClearItems;
  PopupMenuServerList.Clear;
  FIDEList := TIDEList.List;
  for var IDE in FIDEList do
  begin
    var Item := TMenuItem.Create(PopupMenuServerList);
    Item.Text := IDE.Personalities + ' (' + IDE.Version + ')';
    Item.TagString := IDE.Version;
    Item.OnClick := MenuItemD103Click;
    Item.ImageIndex := 0;
    PopupMenuServerList.AddObject(Item);
  end;
  SetCurrentIDE('');
  FOffset := 0;
  LabelInfo.Text := 'Loading ...';
  VertScrollBoxCats.AniCalculations.Animation := True;
  VertScrollBoxContent.AniCalculations.Animation := True;
  RadioButtonAll.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('all');
  RadioButtonLibs.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('lib');
  RadioButtonComponents.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('comps');
  RadioButtonTrial.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('trial');
  RadioButtonTools.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('tools');
  RadioButtonStyles.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('styles');
  RadioButtonPatchesFixes.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('patches');
  RadioButtonIDPlugins.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('plugins');
  RadioButtonSamples.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('samples');
  RadioButtonIT.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('industry');
  RadioButtonPython.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('python');
  RadioButtonNew.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('new');
  RadioButtonPromoted.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('promoted');
  RadioButtonInstalled.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('installed');
  RadioButtonPlatforms.StylesData['icon.Data.Data'] := LineStoragePath.GetByName('platforms');
  RadioButtonNew.IsChecked := True;
  LayoutMore.Visible := False;
  FInited := True;
  RadioButtonNewChange(RadioButtonNew);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FPool.Free;
  FGetItCmd.Free;
end;

procedure TFormMain.LayoutHeadResized(Sender: TObject);
begin
  EditSearch.Width := Min(460, LayoutHead.Width) - 20;
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
    FCategory := Button.Tag;
    FOrder := 0;
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
    FCategory := -1;
    FOrder := 2;
    FLastSearch := '';
    EditSearch.Text := '';
    LoadPackages(False);
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

initialization
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

end.

