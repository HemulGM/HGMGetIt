unit HGI.GetItCmd;

interface

uses
  Winapi.Windows, Winapi.ShellAPI, System.Classes, System.SysUtils, HGI.GetItAPI;

type
  TFutureId = record
  public
    const
      Delphi = 'delphi';
      DelphiWindows = 'delphi_windows';
      DelphiMacOS = 'delphi_macos';
      DelphiLinux = 'delphi_linux';
      DelphiIOS = 'delphi_ios';
      DelphiAndroid = 'delphi_android';
      CBuilder = 'cbuilder';
      CBuilderWindows = 'cbuilder_windows';
      CBuilderIOS = 'cbuilder_ios';
      CBuilderAndroid = 'cbuilder_android';
      French = 'french';
      German = 'german';
      Japanese = 'japanese';
      Samples = 'samples';
      Help = 'help';
      TeeChart = 'teechart';
      DUnit = 'dunit';
      InterbaseExpress = 'interbase_express';
      Interbase2020 = 'interbase_2020';
      OpenJDK = 'openjdk';
      AndroidSDK = 'android_sdk';
  end;

  TGetItCommand = class;

  TGetItCmd = class
    function Execute(const IDE: TIDEEntity; Command: TGetItCommand): Boolean;
  end;

  TGetItConfig = (UseOnline, UseOffline);

  TGetItCommand = class
  protected
    FParams: TStringList;
  public
    /// <summary>
    /// Install Item
    /// </summary>
    function Install(const ItemId: TArray<string>): TGetItCommand;
    /// <summary>
    /// Install Feature Available ID in TFutureId
    /// </summary>
    function InstallFeatures(const FeatureId: TArray<string>): TGetItCommand;
    /// <summary>
    /// Install deferred packages
    /// </summary>
    function InstallDeferred(const ItemId: TArray<string>): TGetItCommand;
    /// <summary>
    /// Uninstall Item
    /// </summary>
    function Uninstall(const ItemId: TArray<string>): TGetItCommand;
    /// <summary>
    /// Download a file
    /// </summary>
    function Download(const FileUrl: string): TGetItCommand;
    /// <summary>
    /// List all available items. Result set depends on sort and filter commands.
    /// </summary>
    function List(const SearchBy: string): TGetItCommand;
    /// <summary>
    /// Specifies the sort parameter for the list command. Default is "name". (name/vedor/date)
    /// </summary>
    function Sort(const Value: string): TGetItCommand;
    /// <summary>
    /// Specifies the filter parameter for the list command. Default is "installed". (all/installed)
    /// </summary>
    function Filter(const Value: string): TGetItCommand;
    /// <summary>
    /// Run an application before exit.
    /// </summary>
    function RunApp(const AppPath: string): TGetItCommand;
    /// <summary>
    /// Set up a GetIt system. (useonline/useoffline)
    /// </summary>
    function Config(const Value: TGetItConfig): TGetItCommand;
    /// <summary>
    /// User name for proxies with required authentication.
    /// </summary>
    function UserName(const Value: string): TGetItCommand;
    /// <summary>
    /// Password for proxies with required authentication.
    /// </summary>
    function Password(const Value: string): TGetItCommand;
    /// <summary>
    /// Specifies the verbose level for console output messages. (quiet/minimal/normal/detailed)
    /// </summary>
    function Verb(const Value: string): TGetItCommand;
    /// <summary>
    /// The user accepts EULA[s] of downloaded package[s].
    /// </summary>
    function AcceptEULAs: TGetItCommand;
    /// <summary>
    /// Do not set environment variables.
    /// </summary>
    function DoNotSetEnvFile: TGetItCommand;
    /// <summary>
    /// Print all available commands.
    /// </summary>
    function Help: TGetItCommand;
    ///Build line
    function Build(AutoFree: Boolean = True): string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, FMX.Dialogs;

{ TGetItCommand }

function TGetItCommand.AcceptEULAs: TGetItCommand;
begin
  Result := Self;
  FParams.Add('--accepteulas');
end;

function TGetItCommand.Build(AutoFree: Boolean): string;
begin
  FParams.Delimiter := ' ';
  FParams.QuoteChar := #0;
  Result := FParams.DelimitedText;
  if AutoFree then
    Free;
end;

function TGetItCommand.Config(const Value: TGetItConfig): TGetItCommand;
begin
  Result := Self;
  case Value of
    UseOnline:
      FParams.Add('--c=useonline');
    UseOffline:
      FParams.Add('--c=useoffline');
  end;
end;

constructor TGetItCommand.Create;
begin
  inherited;
  FParams := TStringList.Create;
end;

destructor TGetItCommand.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TGetItCommand.DoNotSetEnvFile: TGetItCommand;
begin
  Result := Self;
  FParams.Add('--donotsetenvfile');
end;

function TGetItCommand.Download(const FileUrl: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--download=' + FileUrl);
end;

function TGetItCommand.Filter(const Value: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--filter=' + Value);
end;

function TGetItCommand.Help: TGetItCommand;
begin
  Result := Self;
  FParams.Add('--help');
end;

function TGetItCommand.Install(const ItemId: TArray<string>): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--install="' + string.Join(';', ItemId) + '"');
end;

function TGetItCommand.InstallDeferred(const ItemId: TArray<string>): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--installdeferred="' + string.Join(';', ItemId) + '"');
end;

function TGetItCommand.InstallFeatures(const FeatureId: TArray<string>): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--install_features="' + string.Join(';', FeatureId) + '"');
end;

function TGetItCommand.List(const SearchBy: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--list=' + SearchBy);
end;

function TGetItCommand.Password(const Value: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--password=' + Value);
end;

function TGetItCommand.RunApp(const AppPath: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--runapp="' + AppPath + '"');
end;

function TGetItCommand.Sort(const Value: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--sort=' + Value);
end;

function TGetItCommand.Uninstall(const ItemId: TArray<string>): TGetItCommand;
begin
  Result := Self;
  FParams.Add('-u="' + string.Join(';', ItemId) + '"');
end;

function TGetItCommand.UserName(const Value: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--username=' + Value);
end;

function TGetItCommand.Verb(const Value: string): TGetItCommand;
begin
  Result := Self;
  FParams.Add('--verb=' + Value);
end;

{ TGetItCmd }

function TGetItCmd.Execute(const IDE: TIDEEntity; Command: TGetItCommand): Boolean;
begin
  Result := ShellExecute(0, 'open',
    PChar(IDE.GetPathGetItCmd),
    PChar(Command.Build),
    PChar(IDE.GetPathBin),
    SW_NORMAL) > SE_ERR_DLLNOTFOUND;
end;

end.

