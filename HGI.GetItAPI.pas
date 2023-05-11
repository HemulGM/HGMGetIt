unit HGI.GetItAPI;

interface

uses
  HGI.Item, System.SysUtils;

type
  TGetIt = class
    class var
      Url, Version: string;
    class function Get(out Items: TPackages; const Category, Order: Integer; const Search: string = ''; const Count: Integer = 0; const Offset: Integer = 0): Boolean;
    class function ParseDate(const Value: string): string; static;
  end;

  TIDEEntity = record
    Version: string;
    RootDir: string;
    Personalities: string;
    ServiceUrl: string;
    RegPath: string;
    Elements: TArray<string>;
    function GetPathBin: string;
    function GetPathGetItCmd: string;
    procedure LoadInstalled(out Items: TPackages; const Search: string);
  end;

  TIDEList = class
    class function List: TArray<TIDEEntity>;
  end;

const
  GetItCmdPath = 'GetItCmd.exe';

implementation

uses
  Winapi.Windows, System.Net.HttpClient, System.Net.Mime, System.Classes,
  Rest.Json, System.Win.Registry, System.IOUtils;

{ TGetIt }

class function TGetIt.ParseDate(const Value: string): string;
begin
  var
    FormatDate: TFormatSettings;
  var
    Date: TDateTime;
  FormatDate.DateSeparator := '-';
  FormatDate.ShortDateFormat := 'YYYY-MM-DD';
  if (not Value.IsEmpty) and TryStrToDateTime(Value, Date, FormatDate) then
    Result := FormatDateTime('DD.MM.YYYY', Date)
  else
    Result := 'Unkonwn';
end;

class function TGetIt.Get(out Items: TPackages; const Category, Order: Integer; const Search: string; const Count, Offset: Integer): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  Items := nil;
  HTTP := THTTPClient.Create;
  try
    var
      Body := TMultipartFormData.Create;
    var
      Response := TStringStream.Create('', TEncoding.UTF8);
    try
      if Category > 0 then
        Body.AddField('Categories', Category.ToString);
      if not Search.IsEmpty then
        Body.AddField('Search', Search);
      if Offset > 0 then
        Body.AddField('Start', Offset.ToString);
      if Count > 0 then
        Body.AddField('End', (Offset + Count).ToString);
      Body.AddField('Order', Order.ToString);
      Body.AddField('Language', '0');
      Body.AddField('CatalogVersion', '5');
      Body.AddField('Personalities', '1'); // delphi
      Body.AddField('Identity', 'DELPHI');
      Body.AddField('Version', Version);
      // Body.AddField('BuildNumber', '28.0.47991.2819');

      // https://getit-104.embarcadero.com
      // https://getit-1032.embarcadero.com
      // https://getit-olympus.embarcadero.com
      if HTTP.Post(Url + '/catalog/info', Body, Response).StatusCode = 200 then
      begin
        Items := TJson.JsonToObject<TPackages>
          ('{ "items": ' + Response.DataString + '}');
        Result := True;
      end;
    finally
      Body.Free;
      Response.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

// Categories

// 4 - iot
// 20 - IntraWeb
// 25 - AndroidSDK-NDK
// 26 - jdk
// 27 - AndroidSDK-NDK
// 28 - AdoptOpenJDK
// 29 - AndroidSDK


// 1 - Libraries
// 2 - Components
// 3-13 - empty
// 14 - Fonts
// 15 - Platforms
// 16 - Samples
// 17 - Help
// 18-19 - empty
// 20 - empty (IntraWeb)
// 21 - TeeChart Standard
// 22 - DUnit Unit Testing Frameworks
// 23 - InterBase Express (IBX) Components
// 24-32 - empty
// 33 - Trial
// 34-35 - empty
// 36 - Industry Templates
// 37 - IDE Plugins
// 38 - Styles
// 39 - empty
// 40 - Introductory Samples
// 41 - empty
// 42 - InterBase 2020 Developer Edition
// 43-45 - empty
// 46 - Tools
// 47 - Python
// 48-97 - empty
// 98 - New
// 99 - Sample Projects
// 998 - Promoted
// 999 - Patches and Hotfixes
// 1001 - Tools

{ TIDEList }

class function TIDEList.List: TArray<TIDEEntity>;

  function ReadSection(Reg: TRegistry; const Section: string; out Entity: TIDEEntity): Boolean;
  begin
    Result := False;
    Reg.CloseKey;
    if not Reg.OpenKeyReadOnly('Software\Embarcadero\BDS\' + Section) then
      Exit;
    if not Reg.KeyExists('CatalogRepository') then
      Exit;
    try
      Entity.Version := Section;
      Entity.RootDir := Reg.ReadString('RootDir');
      if not Reg.OpenKeyReadOnly('Personalities') then
        Exit;
      Entity.Personalities := Reg.ReadString('');
      Reg.CloseKey;
      if not Reg.OpenKeyReadOnly('Software\Embarcadero\BDS\' + Section +
        '\CatalogRepository') then
        Exit;
      Entity.ServiceUrl := Reg.ReadString('ServiceUrl');
      if Reg.OpenKeyReadOnly('Elements') then
      begin
        var
          Elements := TStringList.Create;
        try
          Reg.GetKeyNames(Elements);
          Entity.Elements := Elements.ToStringArray;
        finally
          Elements.Free;
        end;
      end;
      Result := True;
    except
      Exit;
    end;
  end;

begin
  Result := [];
  var
    Reg := TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKeyReadOnly('Software\Embarcadero\BDS') then
      Exit;
    var
      Sections := TStringList.Create;
    try
      Reg.GetKeyNames(Sections);
      var
        Entity: TIDEEntity;
      for var Section in Sections do
        if ReadSection(Reg, Section, Entity) then
        begin
          Entity.RegPath := 'Software\Embarcadero\BDS\' + Section;
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Entity;
        end;
    finally
      Sections.Free;
    end;
  finally
    Reg.Free;
  end;
end;

{ TIDEEntity }

function TIDEEntity.GetPathBin: string;
begin
  Result := TPath.Combine(RootDir, 'Bin');
end;

function TIDEEntity.GetPathGetItCmd: string;
begin
  Result := TPath.Combine(GetPathBin, GetItCmdPath);
end;

procedure TIDEEntity.LoadInstalled(out Items: TPackages; const Search: string);
var
  Reg: TRegistry;
  SearchStr: string;

  function ReadItem(Item: TGetItPackage): Boolean;
  begin
    Result := True;
    Item.Name := Reg.ReadString('Name');
    Item.Tags := Reg.ReadString('Tags');
    Item.Description := Reg.ReadString('Description');
    if not SearchStr.IsEmpty then
    begin
      if not Item.Name.ToLower.Contains(SearchStr) then
        if not Item.Tags.ToLower.Contains(SearchStr) then
          if not Item.Description.ToLower.Contains(SearchStr) then
            Exit(False);
    end;

    Item.LibCode := Reg.ReadInteger('Code').ToString;
    Item.LibCodeName := Reg.ReadString('CodeName');
    Item.AllUsers := Reg.ReadInteger('AllUsers').ToString;
    Item.Compatibility := Reg.ReadInteger('Compatibility').ToString;
    Item.Icon := Reg.ReadString('Icon');
    Item.Image := Reg.ReadString('Image');
    Item.LibGenerateTmpUrl := Reg.ReadInteger('LibGenerateTmpUrl').ToString;
    Item.LibLicenseName := Reg.ReadString('LicenseName');
    Item.LicenseState := Reg.ReadInteger('LibGenerateTmpUrl').ToString;
    Item.Modified := FormatDateTime('YYYY-MM-DD', Reg.ReadDateTime('Modified'));
    Item.PurchaseUrl := Reg.ReadString('PurchaseUrl');
    Item.RequireElevation := Reg.ReadInteger('RequireElevation').ToString;
    Item.State := Reg.ReadInteger('State').ToString;
    Item.TargetPath := Reg.ReadString('TargetPath');
    Item.TypeDescription := Reg.ReadString('TypeDescription');
    Item.TypeId := Reg.ReadInteger('TypeId').ToString;
    Item.Vendor := Reg.ReadString('Vendor');
    Item.VendorUrl := Reg.ReadString('VendorUrl');
    Item.Version := Reg.ReadString('Version');
    Item.LibUrl := Reg.ReadString('Url');
    var
      List := TStringList.Create;
    try
      var
        Tmp := Reg.ReadString('OSes');
      List.Delimiter := ';';
      List.DelimitedText := Tmp;
      var
        LibOSes: TArray<TLibOS>;
      try
        for var SItem in List do
        begin
          var
            Arr := SItem.Split(['=']);
          if Length(Arr) > 1 then
          begin
            var
              OS := TLibOS.Create;
            OS.Id := Arr[0];
            OS.Name := Arr[1];
            SetLength(LibOSes, Length(LibOSes) + 1);
            LibOSes[High(LibOSes)] := OS;
          end;
        end;
      finally
        Item.LibOSes := LibOSes;
      end;

      Tmp := Reg.ReadString('Platforms');
      List.Delimiter := ';';
      List.DelimitedText := Tmp;
      var
        Platforms: TArray<TLibPlatform>;
      try
        for var SItem in List do
        begin
          var
            Arr := SItem.Split(['=']);
          if Length(Arr) > 1 then
          begin
            var
              LibPlatform := TLibPlatform.Create;
            LibPlatform.Id := Arr[0];
            LibPlatform.Name := Arr[1];
            SetLength(Platforms, Length(Platforms) + 1);
            Platforms[High(Platforms)] := LibPlatform;
          end;
        end;
      finally
        Item.LibPlatforms := Platforms;
      end;
    finally
      List.Free;
    end;
  end;

begin
  Items := TPackages.Create;
  SearchStr := Search.ToLower;
  var
    FItems: TArray<TGetItPackage>;
  try
    Reg := TRegistry.Create(KEY_QUERY_VALUE);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      var
        RegRoot := RegPath + '\CatalogRepository\Elements';
      if not Reg.OpenKeyReadOnly(RegRoot) then
        Exit;
      var
        List := TStringList.Create;
      try
        Reg.GetKeyNames(List);
        for var Key in List do
        begin
          Reg.CloseKey;
          if Reg.OpenKeyReadOnly(RegRoot + '\' + Key) then
          begin
            var
              Item: TGetItPackage;
            Item := TGetItPackage.Create;
            if not ReadItem(Item) then
            begin
              Item.Free;
              Continue;
            end;
            Item.Id := Key;
            SetLength(FItems, Length(FItems) + 1);
            FItems[High(FItems)] := Item;
          end;
        end;
      finally
        List.Free;
      end;
    finally
      Reg.Free;
    end;
    Items.Items := FItems;
  except
    Items.Items := FItems;
    Items.Free;
  end;
end;

end.

