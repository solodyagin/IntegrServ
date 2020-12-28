unit uuserroutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, Math, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteUserGetCount = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserSearch = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserCreate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserRead = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserUpdate = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserDelete = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

implementation

procedure TRouteUserGetCount.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users';
  Query.ExecSQL;
  Query.Open;
  Query.First;
  JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
end;

procedure TRouteUserSearch.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
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
        'FROM Users' + LineEnding +
        'WHERE LastName LIKE ''%' + searchString + '%''' + LineEnding +
        '  OR FirstName LIKE ''%' + searchString + '%''' + LineEnding +
        '  OR FatherName LIKE ''%' + searchString + '%'''
    else if limit > 0 then
      sql := 'SELECT TOP ' + sLimit + ' *' + LineEnding +
        'FROM Users' + LineEnding +
        'WHERE LastName LIKE ''%' + searchString + '%''' + LineEnding +
        '  OR FirstName LIKE ''%' + searchString + '%''' + LineEnding +
        '  OR FatherName LIKE ''%' + searchString + '%''';
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
      '    FROM Users' + LineEnding +
      '    WHERE LastName LIKE ''%' + searchString + '%''' + LineEnding +
      '      OR FirstName LIKE ''%' + searchString + '%''' + LineEnding +
      '      OR FatherName LIKE ''%' + searchString + '%''' + LineEnding +
      '    ORDER BY UserPtr' + LineEnding +
      '  ) sub' + LineEnding +
      '  ORDER BY sub.UserPtr DESC' + LineEnding +
      ') subOrdered' + LineEnding +
      'ORDER BY UserPtr';
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
        jObject.Add('id', Query.FieldByName('UserPtr').AsLargeInt); // Идентификатор пользователя
        jObject.Add('number', Query.FieldByName('Number').AsUTF8String); // Номер карты
        jObject.Add('lastName', Query.FieldByName('LastName').AsUTF8String); // Фамилия
        jObject.Add('firstName', Query.FieldByName('FirstName').AsUTF8String);// Имя
        jObject.Add('fatherName', Query.FieldByName('FatherName').AsUTF8String); // Отчество
        jObject.Add('groupId', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
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

// todo: Проверять группу на существование (GroupPtr)
procedure TRouteUserCreate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  jParser: TJSONParser;
  jObject: TJSONObject;
  number: String;
  lastName: String;
  firstName: String;
  fatherName: String;
  groupId: Int64;
begin
  jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
  try
    jObject := jParser.Parse as TJSONObject;
    number := jObject.Get('number', '');
    lastName := jObject.Get('lastName', '');
    firstName := jObject.Get('firstName', '');
    fatherName := jObject.Get('fatherName', '');
    groupId := jObject.Get('groupId', 0);
  finally
    FreeAndNil(jObject);
    FreeAndNil(jParser);
  end;
  if number = '' then
    raise EBadRequestException.Create('Parameter "number" is empty');
  if lastName = '' then
    raise EBadRequestException.Create('Parameter "lastName" is empty');
  if firstName = '' then
    raise EBadRequestException.Create('Parameter "firstName" is empty');
  if fatherName = '' then
    raise EBadRequestException.Create('Parameter "fatherName" is empty');
  if groupId = 0 then
    raise EBadRequestException.Create('Parameter "groupId" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  // Проверяем используется ли карта
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users WHERE Number = :number';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('number').Value := number;
  Query.ExecSQL;
  Query.Open;
  Query.First;
  if Query.FieldByName('cnt').AsLargeInt > 0 then
    raise EAlreadyExistsException.CreateFmt('Card number "%s" used', [number]);
  Query.Close;
  // Вставляем нового пользователя
  Query.SQL.Text := 'INSERT INTO Users (Numer, LastName, FirstName, FatherName, GroupPtr)' + LineEnding +
    'VALUES (:number, :lastName, :firstName, :fatherName, :groupId)';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('number').Value := number;
  Query.ParamByName('lastName').Value := lastName;
  Query.ParamByName('firstName').Value := firstName;
  Query.ParamByName('fatherName').Value := fatherName;
  Query.ParamByName('groupId').Value := groupId;
  Query.ExecSQL;
  // Получаем идентификатор
  //Query.SQL.Text := 'SELECT @@IDENTITY AS LastID';
  //Query.Open;
  //Query.First;
  //Writeln(Query.FieldByName('LastID').AsLargeInt);
  //Query.Close;
  HttpCode := 201;
end;

procedure TRouteUserRead.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jObject: TJSONObject;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT * FROM Users WHERE UserPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  Query.Open;
  if Query.EOF then
    raise ENotFoundException.CreateFmt('User "%s" is not exists', [Id]);
  Query.First;
  jObject := TJSONObject.Create;
  try
    jObject.Add('id', Query.FieldByName('UserPtr').AsLargeInt); // Идентификатор пользователя
    jObject.Add('number', Query.FieldByName('Number').AsUTF8String); // Номер карты
    jObject.Add('lastName', Query.FieldByName('LastName').AsUTF8String); // Фамилия
    jObject.Add('firstName', Query.FieldByName('FirstName').AsUTF8String); // Имя
    jObject.Add('fatherName', Query.FieldByName('FatherName').AsUTF8String); // Отчество
    jObject.Add('groupId', Query.FieldByName('GroupId').AsLargeInt); // Идентификатор группы
    JSON.Add('result', jObject.Clone);
  finally
    FreeAndNil(jObject);
  end;
end;

procedure TRouteUserUpdate.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
  jParser: TJSONParser;
  jObject: TJSONObject;
  number: String;
  lastName: String;
  firstName: String;
  fatherName: String;
  groupId: Int64;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
  try
    jObject := jParser.Parse as TJSONObject;
    number := jObject.Get('number', '');
    lastName := jObject.Get('lastName', '');
    firstName := jObject.Get('firstName', '');
    fatherName := jObject.Get('fatherName', '');
    groupId := jObject.Get('groupId', 0);
  finally
    FreeAndNil(jObject);
    FreeAndNil(jParser);
  end;
  if number = '' then
    raise EBadRequestException.Create('Parameter "number" is empty');
  if lastName = '' then
    raise EBadRequestException.Create('Parameter "lastName" is empty');
  if firstName = '' then
    raise EBadRequestException.Create('Parameter "firstName" is empty');
  if fatherName = '' then
    raise EBadRequestException.Create('Parameter "fatherName" is empty');
  if groupId = 0 then
    raise EBadRequestException.Create('Parameter "groupId" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  // Проверяем используется ли карта
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users WHERE Number = :number';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('number').Value := number;
  Query.ExecSQL;
  Query.Open;
  Query.First;
  if Query.FieldByName('cnt').AsLargeInt > 0 then
    raise EAlreadyExistsException.CreateFmt('Card number "%s" used', [number]);
  Query.Close;
  // Обновляем пользователя
  Query.SQL.Text := 'UPDATE Users' + LineEnding +
    'SET Number = :number,' + LineEnding +
    '  LastName = :lastName,' + LineEnding +
    '  FirstName = :firstName,' + LineEnding +
    '  FatherName = :fatherName,' + LineEnding +
    '  groupId = :groupId' + LineEnding +
    'WHERE UserPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('number').Value := number;
  Query.ParamByName('lastName').Value := lastName;
  Query.ParamByName('firstName').Value := firstName;
  Query.ParamByName('fatherName').Value := fatherName;
  Query.ParamByName('groupId').Value := groupId;
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  if Query.RowsAffected = 0 then
    raise EAlreadyExistsException.CreateFmt('User "%s" is not exists', [Id]);
end;

procedure TRouteUserDelete.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  Id: String;
begin
  Id := ARequest.RouteParams['id'];
  if Id = '' then
    raise EBadRequestException.Create('Parameter "id" is empty');
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'DELETE FROM Users WHERE UserPtr = :id';
  Query.Params.ParseSQL(Query.SQL.Text, True);
  Query.ParamByName('id').Value := Id;
  Query.ExecSQL;
  if Query.RowsAffected = 0 then
    raise ENotFoundException.CreateFmt('User "%s" is not exists', [Id]);
end;

end.

