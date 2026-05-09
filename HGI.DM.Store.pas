unit HGI.DM.Store;

interface

uses
  System.SysUtils, System.Classes, HGM.LineStorage;

type
  TDataModuleStore = class(TDataModule)
    LineStoragePath: TLineStorage;
    LineStoragePlatforms: TLineStorage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModuleStore: TDataModuleStore;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
