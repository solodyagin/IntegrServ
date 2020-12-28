unit ugrouproutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, Math, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteGroupGetCount = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteGroupSearch = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteGroupCreate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteGroupRead = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteGroupUpdate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteGroupDelete = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

implementation

procedure TRouteGroupGetCount.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Groups';
  Query.ExecSQL;
  Query.Open;
  Query.First;
  JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
end;

procedure TRouteGroupSearch.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  searchString: String;
  sSkip: String;
  sLimit: String;
  skip: Integer;
  limit: Integer;
  sql: String;
  jArray: TJSONArray;
  jObject: TJSONObject;
  //Fields: TStringList;
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
        'FROM Groups' + LineEnding +
        'WHERE Name LIKE ''%' + searchString + '%'''
    else if limit > 0 then
      sql := 'SELECT TOP ' + sLimit + ' *' + LineEnding +
        'FROM Groups' + LineEnding +
        'WHERE Name LIKE ''%' + searchString + '%''';
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
      '    FROM Groups' + LineEnding +
      '    WHERE Name LIKE ''%' + searchString + '%''' + LineEnding +
      '    ORDER BY GroupPtr' + LineEnding +
      '  ) sub' + LineEnding +
      '  ORDER BY sub.GroupPtr DESC' + LineEnding +
      ') subOrdered' + LineEnding +
      'ORDER BY GroupPtr';
  end;
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := sql;
  Query.ExecSQL;
  Query.Open;
  // Поля таблицы
  //Fields := TStringList.Create;
  //Query.GetFieldNames(Fields);
  //Writeln(Fields.CommaText);
  //Fields.Free;
  jArray := TJSONArray.Create;
  try
    while not Query.EOF do
    begin
      jObject := TJSONObject.Create;
      try
        jObject.Add('id', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
        jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Имя группы
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

procedure TRouteGroupCreate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  jParser: TJSONParser;
  jObject: TJSONObject;
  Name: String;
begin
  jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
  try
    jObject := jParser.Parse as TJSONObject;
    Name := jObject.Get('name', '');
  finally
    FreeAndNil(jObject);
    FreeAndNil(jParser);
  end;
  if Name = '' then
    raise EBadRequestException.Create('Parameter "name" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  // Ищем группу по имени
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Groups WHERE Name = :name';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('name').Value := Name;
  Query.ExecSQL;
  Query.Open;
  Query.First;
  if Query.FieldByName('cnt').AsLargeInt > 0 then
    raise EAlreadyExistsException.CreateFmt('Group "%s" is already exists', [Name]);
  Query.Close;
  // Вставляем новую группу
  Query.SQL.Text := 'INSERT INTO Groups (Name) VALUES (:name)';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('name').Value := Name;
  Query.ExecSQL;
  // Получаем идентификатор
  //Query.SQL.Text := 'SELECT @@IDENTITY AS LastID';
  //Query.Open;
  //Query.First;
  //Writeln(Query.FieldByName('LastID').AsLargeInt);
  //Query.Close;
  HttpCode := 201;
end;

procedure TRouteGroupRead.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jObject: TJSONObject;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT * FROM Groups WHERE GroupPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  Query.Open;
  if Query.EOF then
    raise ENotFoundException.CreateFmt('Group "%s" is not exists', [Id]);
  Query.First;
  jObject := TJSONObject.Create;
  try
    jObject.Add('id', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
    jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Имя группы
    JSON.Add('result', jObject.Clone);
  finally
    FreeAndNil(jObject);
  end;
end;

procedure TRouteGroupUpdate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jParser: TJSONParser;
  jObject: TJSONObject;
  Name: String;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
  try
    jObject := jParser.Parse as TJSONObject;
    Name := jObject.Strings['name'];
  finally
    FreeAndNil(jObject);
    FreeAndNil(jParser);
  end;
  if Name = '' then
    raise EBadRequestException.Create('Parameter "name" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'UPDATE Groups SET Name = :name WHERE GroupPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('name').Value := Name;
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  if Query.RowsAffected = 0 then
    raise EAlreadyExistsException.CreateFmt('Group "%s" is not exists', [Id]);
end;

procedure TRouteGroupDelete.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'DELETE FROM Groups WHERE GroupPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  if Query.RowsAffected = 0 then
    raise ENotFoundException.CreateFmt('Group "%s" is not exists', [Id]);
end;

end.

