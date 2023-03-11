unit HGI.Item;

interface

uses
  Generics.Collections, Rest.Json;

type
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

  TCategory = class(TBasicItem);

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
  public
    property Actions: TArray<TPackageAction> read FActions write FActions;
    property AllUsers: string read FAllUsers write FAllUsers;
    property Categories: TArray<TCategory> read FCategories write FCategories;
    property Compatibility: string read FCompatibility write FCompatibility;
    property Description: string read FDescription write FDescription;
    property Icon: string read FIcon write FIcon;
    property Id: string read FId write FId;
    property Image: string read FImage write FImage;
    property LibAkamaiUrl: string read FLibAkamaiUrl write FLibAkamaiUrl;
    property LibCloudfrontUrl: string read FLibCloudfrontUrl write FLibCloudfrontUrl;
    property LibCode: string read FLibCode write FLibCode;
    property LibCodeName: string read FLibCodeName write FLibCodeName;
    property LibGenerateTmpUrl: string read FLibGenerateTmpUrl write FLibGenerateTmpUrl;
    property LibLicense: string read FLibLicense write FLibLicense;
    property LibLicenseName: string read FLibLicenseName write FLibLicenseName;
    property LibOSes: TArray<TLibOS> read FLibOSes write FLibOSes;
    property LibPlatforms: TArray<TLibPlatform> read FLibPlatforms write FLibPlatforms;
    property LibProjectUrl: string read FLibProjectUrl write FLibProjectUrl;
    property LibSignAkamaiUrl: string read FLibSignAkamaiUrl write FLibSignAkamaiUrl;
    property LibSignCloudfrontUrl: string read FLibSignCloudfrontUrl write FLibSignCloudfrontUrl;
    property LibSize: string read FLibSize write FLibSize;
    property LibUrl: string read FLibUrl write FLibUrl;
    property LibUrlProvider: string read FLibUrlProvider write FLibUrlProvider;
    property LicenseState: string read FLicenseState write FLicenseState;
    property Modified: string read FModified write FModified;
    property Name: string read FName write FName;
    property ProductVersions: TArray<TProductVersion> read FProductVersions write FProductVersions;
    property PurchaseUrl: string read FPurchaseUrl write FPurchaseUrl;
    property RequireElevation: string read FRequireElevation write FRequireElevation;
    property State: string read FState write FState;
    property Tags: string read FTags write FTags;
    property TargetPath: string read FTargetPath write FTargetPath;
    property TypeDescription: string read FTypeDescription write FTypeDescription;
    property TypeId: string read FTypeId write FTypeId;
    property Vendor: string read FVendor write FVendor;
    property VendorUrl: string read FVendorUrl write FVendorUrl;
    property Version: string read FVersion write FVersion;
    destructor Destroy; override;
  end;

  TPackages = class
  private
    FItems: TArray<TGetItPackage>;
  public
    property Items: TArray<TGetItPackage> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

{ TPackageAction }

destructor TPackageAction.Destroy;
var
  LParameterItem: TActionParameter;
begin

  for LParameterItem in FParameter do
    LParameterItem.free;

  inherited;
end;

{ TGetItPackage }

destructor TGetItPackage.Destroy;
var
  LCategoriesItem: TCategory;
  LLibPlatformsItem: TLibPlatform;
  LLibOSesItem: TLibOS;
  LProductVersionsItem: TProductVersion;
  LActionsItem: TPackageAction;
begin

  for LCategoriesItem in FCategories do
    LCategoriesItem.free;
  for LLibPlatformsItem in FLibPlatforms do
    LLibPlatformsItem.free;
  for LLibOSesItem in FLibOSes do
    LLibOSesItem.free;
  for LProductVersionsItem in FProductVersions do
    LProductVersionsItem.free;
  for LActionsItem in FActions do
    LActionsItem.free;

  inherited;
end;

{ TPackages }

destructor TPackages.Destroy;
begin
  for var Item in FItems do
    Item.Free;
  inherited;
end;

end.

