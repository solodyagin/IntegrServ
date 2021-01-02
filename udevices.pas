unit udevices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, Math, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteDeviceGetCount = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteDeviceSearch = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  {TRouteDeviceCreate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

  TRouteDeviceRead = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  {TRouteDeviceUpdate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

  {TRouteDeviceDelete = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

implementation

procedure TRouteDeviceGetCount.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Devices';
  Query.ExecSQL;
  Query.Open;
  Query.First;
  JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
end;

procedure TRouteDeviceSearch.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  searchString: String;
  sSkip: String;
  sLimit: String;
  skip: Integer;
  limit: Integer;
  sql: String;
  jArray: TJSONArray;
  jObject: TJSONObject;
begin
  searchString := ARequest.QueryFields.Values['searchString'];
  SetCodePage(Rawbytestring(searchString), 1251, True);
  sSkip := ARequest.QueryFields.Values['skip'];
  sLimit := ARequest.QueryFields.Values['limit'];
  if sSkip <> '' then
  begin
    if not TryStrToInt(sSkip, skip) then
      raise EBadRequestException.Create('Parameter "skip" must be a number');
    if skip < 0 then
      raise EBadRequestException.Create('Parameter "skip" must be greater than 0');
  end;
  if sLimit <> '' then
  begin
    if not TryStrToInt(sLimit, limit) then
      raise EBadRequestException.Create('Parameter "limit" must be a number');
    if not InRange(limit, 0, 50) then
      raise EBadRequestException.Create('Parameter "limit" must be in range 0..50');
  end;
  if sSkip = '' then
  begin
    if sLimit = '' then
      sql := 'SELECT *' + LineEnding +
        'FROM Devices' + LineEnding +
        'WHERE Name LIKE ''%' + searchString + '%''' + LineEnding +
        'ORDER BY Name;'
    else if limit > 0 then
      sql := 'SELECT TOP ' + sLimit + ' *' + LineEnding +
        'FROM Devices' + LineEnding +
        'WHERE Name LIKE ''%' + searchString + '%''' + LineEnding +
        'ORDER BY Name;'
  end
  else
  begin
    if sLimit = '' then
      limit := 50;
    sql := 'SELECT *' + LineEnding +
      'FROM (' + LineEnding +
      '  SELECT TOP ' + limit.ToString + ' * ' + LineEnding +
      '  FROM (' + LineEnding +
      '    SELECT TOP ' + IntToStr(skip + limit) + ' * ' + LineEnding +
      '    FROM Devices' + LineEnding +
      '    WHERE Name LIKE ''%' + searchString + '%''' + LineEnding +
      '    ORDER BY Name' + LineEnding +
      '  ) sub' + LineEnding +
      '  ORDER BY sub.Name DESC' + LineEnding +
      ') subOrdered' + LineEnding +
      'ORDER BY Name;';
  end;
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := sql;
  Query.ExecSQL;
  Query.Open;
  jArray := TJSONArray.Create;
  try
    while not Query.EOF do
    begin
      jObject := TJSONObject.Create;
      try
        jObject.Add('id', Query.FieldByName('DevPtr').AsLargeInt); // Идентификатор
        jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Название
        jObject.Add('keyType', Query.FieldByName('KeyType').AsInteger); // Тип ключей
        jObject.Add('devMode', Query.FieldByName('DevMode').AsInteger); // Режим работы
        jArray.Add(jObject.Clone);
      finally
        FreeAndNil(jObject);
      end;
      Query.Next;
    end;
    JSON.Add('result', jArray.Clone);
  finally
    FreeAndNil(jArray);
  end;
end;

{procedure TRouteDeviceCreate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

procedure TRouteDeviceRead.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jObject: TJSONObject;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT * FROM Devices WHERE DevPtr = :DevPtr;';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('DevPtr').Value := Id;
  Query.ExecSQL;
  Query.Open;
  if Query.EOF then
    raise ENotFoundException.CreateFmt('Device "%s" is not exists', [Id]);
  Query.First;
  jObject := TJSONObject.Create;
  try
    jObject.Add('id', Query.FieldByName('DevPtr').AsLargeInt); // Идентификатор
    jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Название
    jObject.Add('keyType', Query.FieldByName('KeyType').AsInteger); // Тип ключей
    jObject.Add('devMode', Query.FieldByName('DevMode').AsInteger); // Режим работы
    JSON.Add('result', jObject.Clone);
  finally
    FreeAndNil(jObject);
  end;
end;

{procedure TRouteDeviceUpdate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

{procedure TRouteDeviceDelete.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

end.

