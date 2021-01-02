unit ukeytypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, Math, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteKeyTypeGetCount = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteKeyTypeSearch = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  {TRouteKeyTypeCreate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

  TRouteKeyTypeRead = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  {TRouteKeyTypeUpdate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

  {TRouteKeyTypeDelete = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;}

implementation

procedure TRouteKeyTypeGetCount.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM KeyTypes';
  Query.ExecSQL;
  Query.Open;
  Query.First;
  JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
end;

procedure TRouteKeyTypeSearch.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
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
        'FROM KeyTypes' + LineEnding +
        'WHERE KeyTypeName LIKE ''%' + searchString + '%'''+ LineEnding +
        '  OR ShortName LIKE ''%' + searchString + '%'''+ LineEnding +
        'ORDER BY KeyTypeName;'
    else if limit > 0 then
      sql := 'SELECT TOP ' + sLimit + ' *' + LineEnding +
        'FROM KeyTypes' + LineEnding +
        'WHERE KeyTypeName LIKE ''%' + searchString + '%'''+ LineEnding +
        '  OR ShortName LIKE ''%' + searchString + '%'''+ LineEnding +
        'ORDER BY KeyTypeName;'
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
      '    FROM KeyType' + LineEnding +
      '    WHERE KeyTypeName LIKE ''%' + searchString + '%''' + LineEnding +
      '    ORDER BY KeyTypeName' + LineEnding +
      '  ) sub' + LineEnding +
      '  ORDER BY sub.KeyTypeName DESC' + LineEnding +
      ') subOrdered' + LineEnding +
      'ORDER BY KeyTypeName;';
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
        jObject.Add('id', Query.FieldByName('KeyType').AsLargeInt); // Идентификатор
        jObject.Add('name', Query.FieldByName('KeyTypeName').AsUTF8String); // Название
        jObject.Add('shortName', Query.FieldByName('ShortName').AsUTF8String);
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

{procedure TRouteKeyTypeCreate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

procedure TRouteKeyTypeRead.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jObject: TJSONObject;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT * FROM KeyTypes WHERE KeyType = :KeyType;';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('KeyType').Value := Id;
  Query.ExecSQL;
  Query.Open;
  if Query.EOF then
    raise ENotFoundException.CreateFmt('KeyType "%s" is not exists', [Id]);
  Query.First;
  jObject := TJSONObject.Create;
  try
    jObject.Add('id', Query.FieldByName('KeyType').AsLargeInt); // Идентификатор
    jObject.Add('name', Query.FieldByName('KeyTypeName').AsUTF8String); // Название
    jObject.Add('shortName', Query.FieldByName('ShortName').AsUTF8String);
    JSON.Add('result', jObject.Clone);
  finally
    FreeAndNil(jObject);
  end;
end;

{procedure TRouteKeyTypeUpdate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

{procedure TRouteKeyTypeDelete.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin

end;}

end.

