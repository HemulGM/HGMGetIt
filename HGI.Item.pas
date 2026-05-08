unit HGI.Item;

interface

uses
  Generics.Collections, Rest.Json, Rest.Json.Types;

{$SCOPEDENUMS ON}

type
  TItemAction = (Install, Download, Uninstall, OpenUrl, CommandLine);

  TOnItemAction = procedure(Sender: TObject; const ItemId: string; Action: TItemAction) of object;

  TBasicItem = class
  private
    FId: string;
    FName: string;
  public
    property Id: string read FId write FId;
    property Name: string read FName write FName;
  end;

  TActionParameter = class
  private
    FParameter: string;
  public
    property Parameter: string read FParameter write FParameter;
  end;

  TPackageAction = class
  private
    FActionId: string;
    FActionName: string;
    FDescription: string;
    FId: string;
    FParameter: TArray<TActionParameter>;
    FRequireElevation: string;
    FType: string;
  public
    property ActionId: string read FActionId write FActionId;
    property ActionName: string read FActionName write FActionName;
    property Description: string read FDescription write FDescription;
    property Id: string read FId write FId;
    property Parameter: TArray<TActionParameter> read FParameter write FParameter;
    property RequireElevation: string read FRequireElevation write FRequireElevation;
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TProductVersion = class(TBasicItem);

  TLibOS = class(TBasicItem);

  TLibPlatform = class(TBasicItem);

  TCategory = class(TBasicItem)
  private
    FNumElements: string;
  public
    property NumElements: string read FNumElements write FNumElements;
  end;

  TPackageDependence = class
    Id: string;
    Size: string;
    InflateSize: string;
  end;

  TGetItPackage = class
  private
    FActions: TArray<TPackageAction>;
    FAllUsers: string;
    FCategories: TArray<TCategory>;
    FCompatibility: string;
    FDescription: string;
    FIcon: string;
    FId: string;
    FImage: string;
    FLibAkamaiUrl: string;
    FLibCloudfrontUrl: string;
    FLibCode: string;
    FLibCodeName: string;
    FLibGenerateTmpUrl: string;
    FLibLicense: string;
    FLibLicenseName: string;
    FLibOSes: TArray<TLibOS>;
    FLibPlatforms: TArray<TLibPlatform>;
    FLibProjectUrl: string;
    FLibSignAkamaiUrl: string;
    FLibSignCloudfrontUrl: string;
    FLibSize: string;
    FLibUrl: string;
    FLibUrlProvider: string;
    FLicenseState: string;
    FModified: string;
    FName: string;
    FProductVersions: TArray<TProductVersion>;
    FPurchaseUrl: string;
    FRequireElevation: string;
    FState: string;
    FTags: string;
    FTargetPath: string;
    FTypeDescription: string;
    FTypeId: string;
    FVendor: string;
    FVendorUrl: string;
    FVersion: string;
    FDependencies: TArray<TPackageDependence>;
    FLibInflateSize: string;
    FVersionTimestamp: string;
    FTryNow: string;
    FLangCode: string;
    FMinClientVersion: string;
    FLibMD5: string;
    FExternalUrl: string;
    FSubscription: string;
  public
    property Actions: TArray<TPackageAction> read FActions write FActions;
    property AllUsers: string read FAllUsers write FAllUsers; // 0/1
    property Categories: TArray<TCategory> read FCategories write FCategories;
    property Compatibility: string read FCompatibility write FCompatibility; // 0
    property Description: string read FDescription write FDescription; // text
    property Dependencies: TArray<TPackageDependence> read FDependencies write FDependencies;
    property ExternalUrl: string read FExternalUrl write FExternalUrl; // url ?
    property Icon: string read FIcon write FIcon; // url
    property Id: string read FId write FId; // text
    property Image: string read FImage write FImage; // url
    property LangCode: string read FLangCode write FLangCode; // text EN
    property LibAkamaiUrl: string read FLibAkamaiUrl write FLibAkamaiUrl;
    property LibCloudfrontUrl: string read FLibCloudfrontUrl write FLibCloudfrontUrl;
    property LibCode: string read FLibCode write FLibCode; // int
    property LibCodeName: string read FLibCodeName write FLibCodeName; // text
    property LibGenerateTmpUrl: string read FLibGenerateTmpUrl write FLibGenerateTmpUrl; // 0
    property LibInflateSize: string read FLibInflateSize write FLibInflateSize; // 0
    property LibLicense: string read FLibLicense write FLibLicense; // url
    property LibLicenseName: string read FLibLicenseName write FLibLicenseName; // text
    property LibMD5: string read FLibMD5 write FLibMD5; // text ?
    property LibOSes: TArray<TLibOS> read FLibOSes write FLibOSes;
    property LibPlatforms: TArray<TLibPlatform> read FLibPlatforms write FLibPlatforms;
    property LibProjectUrl: string read FLibProjectUrl write FLibProjectUrl;
    property LibSignAkamaiUrl: string read FLibSignAkamaiUrl write FLibSignAkamaiUrl;
    property LibSignCloudfrontUrl: string read FLibSignCloudfrontUrl write FLibSignCloudfrontUrl;
    property LibSize: string read FLibSize write FLibSize; // float 0.0
    property LibUrl: string read FLibUrl write FLibUrl; // url
    property LibUrlProvider: string read FLibUrlProvider write FLibUrlProvider; // url
    property LicenseState: string read FLicenseState write FLicenseState; // 0
    property MinClientVersion: string read FMinClientVersion write FMinClientVersion; // text ?
    property Modified: string read FModified write FModified; //2024-01-30 13:02:33
    property Name: string read FName write FName; // text
    property ProductVersions: TArray<TProductVersion> read FProductVersions write FProductVersions;
    property PurchaseUrl: string read FPurchaseUrl write FPurchaseUrl; // url
    property RequireElevation: string read FRequireElevation write FRequireElevation; // 0/1
    property State: string read FState write FState; // 1
    property Subscription: string read FSubscription write FSubscription; // 0
    property Tags: string read FTags write FTags; // string array
    property TargetPath: string read FTargetPath write FTargetPath; // ?
    property TryNow: string read FTryNow write FTryNow; // 0
    property TypeDescription: string read FTypeDescription write FTypeDescription; // text
    property TypeId: string read FTypeId write FTypeId; // 1
    property Vendor: string read FVendor write FVendor; // text
    property VendorUrl: string read FVendorUrl write FVendorUrl; // url
    property Version: string read FVersion write FVersion; // 1.0
    property VersionTimestamp: string read FVersionTimestamp write FVersionTimestamp; // 2022-10-20T10:00:00Z
    destructor Destroy; override;
  end;

  TPackages = class
  private
    FItems: TArray<TGetItPackage>;
  public
    property Items: TArray<TGetItPackage> read FItems write FItems;
    destructor Destroy; override;
  end;

  TCategories = class
  private
    [JsonNameAttribute('items')]
    FItems: TArray<TCategory>;
  public
    property Items: TArray<TCategory> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

{ TPackageAction }

destructor TPackageAction.Destroy;
begin
  for var Item in FParameter do
    Item.Free;
  inherited;
end;

{ TGetItPackage }

destructor TGetItPackage.Destroy;
begin
  for var Item in FCategories do
    Item.Free;
  for var Item in FLibPlatforms do
    Item.Free;
  for var Item in FLibOSes do
    Item.Free;
  for var Item in FProductVersions do
    Item.Free;
  for var Item in FActions do
    Item.Free;
  for var Item in FDependencies do
    Item.Free;
  inherited;
end;

{ TPackages }

destructor TPackages.Destroy;
begin
  for var Item in FItems do
    Item.Free;
  inherited;
end;

{ TCategories }

destructor TCategories.Destroy;
begin
  for var Item in FItems do
    Item.Free;
  inherited;
end;

end.

