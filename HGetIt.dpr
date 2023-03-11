program HGetIt;

uses
  System.StartUpCopy,
  FMX.Forms,
  HGI.Main in 'HGI.Main.pas' {FormMain},
  HGI.Item in 'HGI.Item.pas',
  HGI.View.Item in 'HGI.View.Item.pas' {FramePackageItem: TFrame},
  HGI.GetItAPI in 'HGI.GetItAPI.pas',
  HGI.View.ItemFull in 'HGI.View.ItemFull.pas' {FramePackageItemFull};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
