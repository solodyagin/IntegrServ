unit uglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, httpdefs, httproute, base64, fpjson, odbcconn, SQLDB, DB;

const
  // Коды ошибок
  E_NoError = 0;
  E_NoAuth = 1; // ENoAuthException
  E_Simple = 2; // ESimpleException
  E_Unknown = 3; // Exception

type
  ENoAuthException = class(Exception);
  ESimpleException = class(Exception);

type
  TMyRouteObject = class(TRouteObject)
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  API_Auth: Boolean = True;
  API_UserName: String = 'admin';
  API_Password: String = 'gate';
  API_Port: Integer = 9080;
  ODBC_Driver: String = 'Microsoft Access Driver (*.mdb, *.accdb)';
  ODBC_DatabaseName: String = 'Gate';
  ODBC_UserName: String = 'master';
  ODBC_Password: String = 'masterio0';

  procedure ValidateRequest(ARequest: TRequest);
  procedure JSONResponse(AResponse: TResponse; JSON: TJSONObject; HttpCode: Integer);

implementation

constructor TMyRouteObject.Create;
begin
  inherited;
  Connection := TODBCConnection.Create(nil);
  Connection.Driver := ODBC_Driver;
  Connection.DatabaseName := ODBC_DatabaseName;
  Connection.UserName := ODBC_UserName;
  Connection.Password := ODBC_Password;
  //Connection.Params.Add('Locale Identifier=1251');
  //Connection.Params.Add('ExtendedAnsiSQL=1');
  //Connection.Params.Add('CHARSET=ansi');
  //Connection.KeepConnection := True;
  Transaction := TSQLTransaction.Create(nil);
  Transaction.DataBase := Connection;
  Transaction.Action := caCommit;
  Query := TSQLQuery.Create(nil);
  Query.DataBase := Connection;
  Query.UsePrimaryKeyAsKey := False;
end;

destructor TMyRouteObject.Destroy;
begin
  Connection.Free;
  Transaction.Free;
  Query.Free;
  inherited;
end;

procedure ValidateRequest(ARequest: TRequest);
var
  headerValue, b64decoded, username, password: String;
begin
  if not API_Auth then
    Exit;
  headerValue := ARequest.Authorization;
  if Length(headerValue) = 0 then
    raise ENoAuthException.Create('This endpoint requires authentication');
  if ExtractWord(1, headerValue, [' ']) <> 'Basic' then
    raise ENoAuthException.Create('Only Basic Authentication is supported');
  b64decoded := DecodeStringBase64(ExtractWord(2, headerValue, [' ']));
  username := ExtractWord(1, b64decoded, [':']);
  password := ExtractWord(2, b64decoded, [':']);
  if (username <> API_UserName) or (password <> API_Password) then
    raise ENoAuthException.Create('Invalid API credentials');
end;

procedure JSONResponse(AResponse: TResponse; JSON: TJSONObject; HttpCode: Integer);
begin
  AResponse.Code := HttpCode;
  AResponse.ContentType := 'application/json; charset=utf-8';
  AResponse.Content := JSON.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0);
  AResponse.ContentLength := Length(AResponse.Content);
  AResponse.SendContent;
end;

end.

