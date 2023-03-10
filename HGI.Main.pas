unit HGI.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls;

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
    VertScrollBox1: TVertScrollBox;
    LayoutDesc: TLayout;
    Path2: TPath;
    Layout1: TLayout;
    procedure EditSearchChangeTracking(Sender: TObject);
    procedure LayoutHeadResized(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Math;

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



{$R *.fmx}

procedure TFormMain.EditSearchChangeTracking(Sender: TObject);
begin
  ClearEditButtonЫSearch.Visible := not EditSearch.Text.IsEmpty;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  VertScrollBoxCats.AniCalculations.Animation := True;
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
end;

procedure TFormMain.LayoutHeadResized(Sender: TObject);
begin
  EditSearch.Width := Min(460, LayoutHead.Width) - 20;
end;

end.

