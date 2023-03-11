unit HGI.GetItAPI;

interface

uses
  HGI.Item;

type
  TGetIt = class
    class function Get(out Items: TPackages; const Category: Integer = 0; const Search: string = ''; const Count: Integer = 0; const Offset: Integer = 0): Boolean;
  end;

implementation

uses
  System.Net.HttpClient, System.Net.Mime, System.SysUtils, System.Classes,
  Rest.Json;

{ TGetIt }

class function TGetIt.Get(out Items: TPackages; const Category: Integer; const Search: string; const Count, Offset: Integer): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  Items := nil;
  HTTP := THTTPClient.Create;
  try
    var Body := TMultipartFormData.Create;
    var Response := TStringStream.Create('', TEncoding.UTF8);
    try
      if Category > 0 then
        Body.AddField('Categories', Category.ToString);
      if not Search.IsEmpty then
        Body.AddField('Search', Search);
      if Offset > 0 then
        Body.AddField('Start', Offset.ToString);
      if Count > 0 then
        Body.AddField('End', (Offset + Count).ToString);
      Body.AddField('Product', 'delphi');
      Body.AddField('Sortby', 'name');
      Body.AddField('Sub', 'all');
      if HTTP.Post('https://getit.embarcadero.com/catalog/info', Body, Response).StatusCode = 200 then
      begin
        Items := TJson.JsonToObject<TPackages>('{ "items": ' + Response.DataString + '}');
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

end.

