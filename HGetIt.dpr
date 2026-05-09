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
  HGI.GetItCmd in 'HGI.GetItCmd.pas',
  HGM.LineStorage in 'LineStorage\HGM.LineStorage.pas',
  HGM.FMX.Image in 'Components\HGM.FMX.Image.pas',
  HGI.View.ItemList in 'HGI.View.ItemList.pas' {FramePackageItemList},
  HGM.ObjectHolder in 'AsyncObjectHolder\HGM.ObjectHolder.pas',
  FMX.Windows.Hints in 'DelphiWinUI3\FMXWindowsHint\FMX.Windows.Hints.pas',
  FMX.Menus in 'DelphiWinUI3\Fixes\D13\FMX.Menus.pas',
  FMX.Platform.Win in 'DelphiWinUI3\Fixes\D13\FMX.Platform.Win.pas',
  FMX.StyledContextMenu in 'DelphiWinUI3\Fixes\D13\FMX.StyledContextMenu.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
