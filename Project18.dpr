program Project18;

uses
  System.StartUpCopy,
  FMX.Forms,
  HGI.Main in 'HGI.Main.pas' {FormMain},
  HGI.Item in 'HGI.Item.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
