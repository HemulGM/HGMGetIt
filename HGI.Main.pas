unit HGI.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, HGI.View.Item,
  HGI.GetItAPI, FMX.Ani;

type
  TFormMain = class(TForm)
    LayoutHead: TLayout;
    LayoutMenu: TLayout;
    LayoutClient: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    EditSearch: TEdit;
    StyleBook: TStyleBook;
    ClearEditButtonЫSearch: TClearEditButton;
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
    RadioButtonIoT: TRadioButton;
    VertScrollBoxCats: TVertScrollBox;
    VertScrollBoxContent: TVertScrollBox;
    LayoutDesc: TLayout;
    Layout1: TLayout;
    FlowLayoutItems: TFlowLayout;
    FramePackageItem1: TFramePackageItem;
    FramePackageItem2: TFramePackageItem;
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
    procedure EditSearchChangeTracking(Sender: TObject);
    procedure LayoutHeadResized(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FlowLayoutItemsResized(Sender: TObject);
    procedure RadioButtonAllChange(Sender: TObject);
    procedure TimerSearchTimer(Sender: TObject);
    procedure FloatAnimationShdFinish(Sender: TObject);
    procedure ButtonMoreClick(Sender: TObject);
  private
    FInited: Boolean;
    FCategory: Integer;
    FOffset: Integer;
    procedure LoadPackages(More: Boolean);
    procedure ClearItems;
    procedure LoadingBegin;
    procedure LoadingEnd;
    procedure NeedMore(Value: Boolean);
  public
    { Public declarations }
  end;

const
  CardMinW = 320;
  PageSize = 50;

var
  FormMain: TFormMain;

implementation

uses
  System.Math, System.Threading, HGI.Item;

  //1 - libs
  //2 - components
  //3 - empty
  //4 - iot
  //5-14 - empty
  //15 - builders
  //16 - samples
  //17 - help
  //18-19 - empty
  //20 - IntraWeb
  //21 - teechart
  //22 - dunit
  //23 - InterBase
  //24 - empty
  //25 - AndroidSDK-NDK
  //26 - jdk
  //27 - AndroidSDK-NDK
  //28 - AdoptOpenJDK
  //29 - AndroidSDK
  //30-32 - empty
  //33 - trial
  //34-35 - empty
  //36 - Industry templates
  //37 - IDE Plugins
  //38 - styles
  //39-41 - empty
  //42 - InterBase
  //43
  //46 - tools
  //47 - python

{$R *.fmx}

procedure TFormMain.EditSearchChangeTracking(Sender: TObject);
begin
  ClearEditButtonЫSearch.Visible := not EditSearch.Text.IsEmpty;
  TimerSearch.Enabled := False;
  TimerSearch.Enabled := True;
end;

procedure TFormMain.LoadingBegin;
begin
  LabelInfo.Text := 'Загрузка...';
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
          TGetIt.Get(Items, FCategory, EditSearch.Text, PageSize, FOffset);
        finally
          TThread.Synchronize(nil,
            procedure
            begin
              if not More then
                ClearItems;
              if Assigned(Items) then
              try
                for var Item in Items.Items do
                begin
                  var Frame := TFramePackageItem.Create(FlowLayoutItems);
                  Frame.Fill(Item);
                  Frame.Parent := FlowLayoutItems;
                end;
                FlowLayoutItems.RecalcSize;
                if not More then
                  VertScrollBoxContent.ViewportPosition := TPointF.Create(0, 0);
                if FlowLayoutItems.ControlsCount <= 0 then
                begin
                  LabelInfo.Text := 'Нет результатов';
                  LayoutInfo.Visible := True;
                end
                else
                  LayoutInfo.Visible := False;
                NeedMore(Length(Items.Items) >= PageSize);
              finally
                Items.Items := [];
                Items.Free
              end
              else
              begin
                LabelInfo.Text := 'Ошибка';
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
            LabelInfo.Text := 'Ошибка';
            LayoutInfo.Visible := True;
            NeedMore(More);
          end);
      end;
    end);
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ClearItems;
  FOffset := 0;
  LabelInfo.Text := 'Загрузка...';
  VertScrollBoxCats.AniCalculations.Animation := True;
  VertScrollBoxContent.AniCalculations.Animation := True;
  RadioButtonAll.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_all.inc};
  RadioButtonLibs.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_lib.inc};
  RadioButtonComponents.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_comps.inc};
  RadioButtonTrial.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_trial.inc};
  RadioButtonTools.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_tools.inc};
  RadioButtonStyles.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_styles.inc};
  RadioButtonIoT.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_iot.inc};
  RadioButtonIDPlugins.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_plugins.inc};
  RadioButtonSamples.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_samples.inc};
  RadioButtonIT.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_industry.inc};
  RadioButtonPython.StylesData['icon.Data.Data'] := {$INCLUDE icons/path_python.inc};
  RadioButtonLibs.IsChecked := True;
  FCategory := 1;
  LayoutMore.Visible := False;
  FInited := True;
  RadioButtonAllChange(RadioButtonLibs);
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
    FCategory := Button.Tag;
    LoadPackages(False);
  end;
end;

procedure TFormMain.TimerSearchTimer(Sender: TObject);
begin
  TimerSearch.Enabled := False;
  LoadPackages(False);
end;

end.

