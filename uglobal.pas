unit uglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, httpdefs, httproute, base64, fpjson, odbcconn, SQLDB, DB;

const
  // Коды ошибок
  E_Unknown = 1; // Неизвестная ошибка (500 Internal Server Error)
  E_Unauthorized = 2; // Не авторизован (401 Unauthorized)
  E_BadRequest = 3; // Неправильный запрос (400 Bad Request)
  E_NotFound = 4; // Не найден (404 Not Found)
  E_AlreadyExists = 5; // Уже существует (409 Conflict)

type
  EUnauthorizedException = class(Exception);
  EBadRequestException = class(Exception);
  ENotFoundException = class(Exception);
  EAlreadyExistsException = class(Exception);

type
  TMyRouteObject = class(TRouteObject)
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

var
  API_IsAuthNeeded: Boolean = True;
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

{ TMyRouteObject }

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

procedure TMyRouteObject.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  jError: TJSONObject;
  HttpCode: Integer = 200;
begin
  JSON := TJSONObject.Create;
  jError := TJSONObject.Create;
  try
    ValidateRequest(ARequest);
    ExecuteRequest(ARequest, JSON, HttpCode);
  except
    on E: EDatabaseError do
    begin
      jError.Add('errNo', E_Unknown);
      jError.Add('errStr', 'Database error');
      HttpCode := 500;
      Writeln('Database error: ' + E.Message);
    end;
    on E: EUnauthorizedException do
    begin
      jError.Add('errNo', E_Unauthorized);
      jError.Add('errStr', E.Message);
      HttpCode := 401;
    end;
    on E: EBadRequestException do
    begin
      jError.Add('errNo', E_BadRequest);
      jError.Add('errStr', E.Message);
      HttpCode := 400;
    end;
    on E: ENotFoundException do
    begin
      jError.Add('errNo', E_NotFound);
      jError.Add('errStr', E.Message);
      HttpCode := 404;
    end;
    on E: EAlreadyExistsException do
    begin
      jError.Add('errNo', E_AlreadyExists);
      jError.Add('errStr', E.Message);
      HttpCode := 409;
    end;
    on E: Exception do
    begin
      jError.Add('errNo', E_Unknown);
      jError.Add('errStr', E.Message);
      HttpCode := 500;
      Writeln(E.Message);
    end;
  end;
  if jError.Get('errNo', 0) <> 0 then
    JSONResponse(AResponse, jError, HttpCode)
  else
    JSONResponse(AResponse, JSON, HttpCode);
  jError.Free;
  JSON.Free;
end;

procedure ValidateRequest(ARequest: TRequest);
var
  headerValue, b64decoded, username, password: String;
begin
  if not API_IsAuthNeeded then
    Exit;
  headerValue := ARequest.Authorization;
  if Length(headerValue) = 0 then
    raise EUnauthorizedException.Create('This endpoint requires authentication');
  if ExtractWord(1, headerValue, [' ']) <> 'Basic' then
    raise EUnauthorizedException.Create('Only Basic Authentication is supported');
  b64decoded := DecodeStringBase64(ExtractWord(2, headerValue, [' ']));
  username := ExtractWord(1, b64decoded, [':']);
  password := ExtractWord(2, b64decoded, [':']);
  if (username <> API_UserName) or (password <> API_Password) then
    raise EUnauthorizedException.Create('Invalid API credentials');
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

