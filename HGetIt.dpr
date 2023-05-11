program HGetIt;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  HGI.Main in 'HGI.Main.pas' {FormMain},
  HGI.Item in 'HGI.Item.pas',
  HGI.View.Item in 'HGI.View.Item.pas' {FramePackageItem: TFrame},
  HGI.GetItAPI in 'HGI.GetItAPI.pas',
  HGI.View.ItemFull in 'HGI.View.ItemFull.pas' {FramePackageItemFull},
  FMX.StyleChanger in '..\FMXChangeStyle\Source\FMX.StyleChanger.pas',
  DarkModeApi.Consts in '..\WindowDarkMode\DarkModeApi.Consts.pas',
  DarkModeApi.FMX in '..\WindowDarkMode\DarkModeApi.FMX.pas',
  DarkModeApi in '..\WindowDarkMode\DarkModeApi.pas',
  DarkModeApi.Types in '..\WindowDarkMode\DarkModeApi.Types.pas',
  HGI.GetItCmd in 'HGI.GetItCmd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
