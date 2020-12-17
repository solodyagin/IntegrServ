program IntegrServ;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
{$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  cmem,
{$ENDIF}
  SysUtils, StrUtils, Classes, IniFiles, fphttpapp, httpdefs, httproute, fpwebfile, fpjson, jsonparser, jsonscanner,
  base64, odbcconn, SQLDB, DB;

const
  // Коды ошибок
  E_NoError = 0;
  E_NoAuth = 1; // ENoAuthException
  E_Simple = 2; // ESimpleException
  E_Unknown = 3; // Exception

type
  ENoAuthException = class(Exception);
  ESimpleException = class(Exception);

var
  API_Auth: Boolean = True;
  API_UserName: String = 'admin';
  API_Password: String = 'gate';
  API_Port: Integer = 9080;
  ODBC_Driver: String = 'Microsoft Access Driver (*.mdb, *.accdb)';
  ODBC_DatabaseName: String = 'Gate';
  ODBC_UserName: String = 'master';
  ODBC_Password: String = 'masterio0';

  procedure CreateConnection(var Connection: TODBCConnection; var Transaction: TSQLTransaction; var Query: TSQLQuery);
  begin
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

  procedure GetUsersCount(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users';
      Query.ExecSQL;
      Query.Open;
      Query.First;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure User_GetAll(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    jArray: TJSONArray;
    jObject: TJSONObject;
    HttpCode: Integer;
    //Fields: TStringList;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'SELECT * FROM Users';
      Query.ExecSQL;
      Query.Open;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
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
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure GetGroupsCount(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Groups';
      Query.ExecSQL;
      Query.Open;
      Query.First;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure Group_GetAll(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    jArray: TJSONArray;
    jObject: TJSONObject;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'SELECT * FROM Groups';
      Query.ExecSQL;
      Query.Open;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
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
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure Group_Create(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    jParser: TJSONParser;
    jObject: TJSONObject;
    Name: String;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
      try
        jObject := jParser.Parse as TJSONObject;
        Name := jObject.Strings['name'];
      finally
        FreeAndNil(jObject);
        FreeAndNil(jParser);
      end;
      if Name = '' then
        raise ESimpleException.Create('Parameter "name" is empty');
      Connection.Connected := True;
      Transaction.Active := True;
      // Ищем группу по имени
      Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Groups WHERE name=:name';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('name').Value := Name;
      Query.ExecSQL;
      Query.Open;
      Query.First;
      if Query.FieldByName('cnt').AsLargeInt > 0 then
        raise ESimpleException.CreateFmt('Group "%s" is already exists', [Name]);
      Query.Close;
      // Вставляем новую группу
      Query.SQL.Text := 'INSERT INTO Groups (name) VALUES (:name)';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('name').Value := Name;
      Query.ExecSQL;
      // Получаем идентификатор
      //Query.SQL.Text := 'SELECT @@IDENTITY AS LastID';
      //Query.Open;
      //Query.First;
      //Writeln(Query.FieldByName('LastID').AsLargeInt);
      //Query.Close;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      JSON.Add('result', '');
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: ESimpleException do
      begin
        JSON.Add('errNo', E_Simple);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 200;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure Group_Read(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    Id: String;
    jObject: TJSONObject;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Id := ARequest.RouteParams['id'];
      if Id = '' then
        raise ESimpleException.Create('Parameter "id" is empty');
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'SELECT * FROM Groups WHERE GroupPtr=:id';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('id').Value := Id;
      Query.ExecSQL;
      Query.Open;
      if Query.EOF then
        raise ESimpleException.CreateFmt('Group "%s" is not exists', [Id]);
      Query.First;
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      jObject := TJSONObject.Create;
      try
        jObject.Add('id', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
        jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Имя группы
        JSON.Add('result', jObject.Clone);
      finally
        FreeAndNil(jObject);
      end;
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: ESimpleException do
      begin
        JSON.Add('errNo', E_Simple);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 200;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure Group_Update(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    Id: String;
    jParser: TJSONParser;
    jObject: TJSONObject;
    Name: String;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Id := ARequest.RouteParams['id'];
      if Id = '' then
        raise ESimpleException.Create('Parameter "id" is empty');
      jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
      try
        jObject := jParser.Parse as TJSONObject;
        Name := jObject.Strings['name'];
      finally
        FreeAndNil(jObject);
        FreeAndNil(jParser);
      end;
      if Name = '' then
        raise ESimpleException.Create('Parameter "name" is empty');
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'UPDATE Groups SET Name=:name WHERE GroupPtr=:id';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('name').Value := Name;
      Query.ParamByName('id').Value := Id;
      Query.ExecSQL;
      if Query.RowsAffected = 0 then
        raise ESimpleException.CreateFmt('Group "%s" is not exists', [Id]);
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      JSON.Add('result', '');
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: ESimpleException do
      begin
        JSON.Add('errNo', E_Simple);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 200;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure Group_Delete(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Id: String;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    CreateConnection(Connection, Transaction, Query);
    try
      ValidateRequest(ARequest);
      Id := ARequest.RouteParams['id'];
      if Id = '' then
        raise ESimpleException.Create('Parameter "id" is empty');
      Connection.Connected := True;
      Transaction.Active := True;
      Query.SQL.Text := 'DELETE FROM Groups WHERE GroupPtr=:id';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('id').Value := Id;
      Query.ExecSQL;
      if Query.RowsAffected = 0 then
        raise ESimpleException.CreateFmt('Group "%s" is not exists', [Id]);
      JSON.Add('errNo', E_NoError);
      JSON.Add('errStr', '');
      JSON.Add('result', '');
      HttpCode := 200;
    except
      on E: ENoAuthException do
      begin
        JSON.Add('errNo', E_NoAuth);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 401;
      end;
      on E: ESimpleException do
      begin
        JSON.Add('errNo', E_Simple);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 200;
      end;
      on E: Exception do
      begin
        JSON.Add('errNo', E_Unknown);
        JSON.Add('errStr', E.Message);
        JSON.Add('result', '');
        HttpCode := 500;
        Writeln(E.Message);
      end;
    end;
    Connection.Free;
    Transaction.Free;
    Query.Free;
    JSONResponse(AResponse, JSON, HttpCode);
    JSON.Free;
  end;

  procedure CatchAll(ARequest: TRequest; AResponse: TResponse);
  begin
    AResponse.Content := 'This endpoint is not available';
    AResponse.Code := 404;
    AResponse.ContentType := 'text/plain';
    AResponse.ContentLength := Length(AResponse.Content);
    AResponse.SendContent;
  end;

  procedure ReRouteRoot(ARequest: TRequest; AResponse: TResponse);
  begin
    AResponse.Code := 301;
    AResponse.SetCustomHeader('Location', '/app/index.html');
    AResponse.SendContent;
  end;

var
  IniFileName: String;

{$R *.res}

begin
  // Загружаем настройки
  IniFileName := ChangeFileExt(Application.ExeName, '.ini');
  Writeln('Read settings from "' + IniFileName + '"');
  with TIniFile.Create(IniFileName, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]) do
  begin
    API_Auth := ReadBool('API', 'Auth', API_Auth);
    API_UserName := ReadString('API', 'UserName', API_UserName);
    API_Password := ReadString('API', 'Password', API_Password);
    API_Port := ReadInteger('API', 'Port', API_Port);
    ODBC_Driver := ReadString('ODBC', 'Driver', ODBC_Driver);
    ODBC_DatabaseName := ReadString('ODBC', 'DatabaseName', ODBC_DatabaseName);
    ODBC_UserName := ReadString('ODBC', 'UserName', ODBC_UserName);
    ODBC_Password := ReadString('ODBC', 'Password', ODBC_Password);
    Free;
  end;

  (*// Сохраняем настройки
  with TIniFile.Create(IniFileName, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]) do
  begin
    WriteBool('API', 'Auth', API_Auth);
    WriteString('API', 'UserName', API_UserName);
    WriteString('API', 'Password', API_Password);
    WriteInteger('API', 'Port', API_Port);
    WriteString('ODBC', 'Driver', ODBC_Driver);
    WriteString('ODBC', 'DatabaseName', ODBC_DatabaseName);
    WriteString('ODBC', 'UserName', ODBC_UserName);
    WriteString('ODBC', 'Password', ODBC_Password);
    Free;
  end;*)

  Writeln('API Auth: ' + BoolToStr(API_Auth, True));

  // Маршруты
  HTTPRouter.RegisterRoute('/catchall', rmAll, @CatchAll, True);
  HTTPRouter.RegisterRoute('/api/v1/getUsersCount', rmGet, @GetUsersCount); // Количество пользователей
  HTTPRouter.RegisterRoute('/api/v1/user', rmGet, @User_GetAll); // Все пользователи
  HTTPRouter.RegisterRoute('/api/v1/getGroupsCount', rmGet, @GetGroupsCount); // Количество групп
  HTTPRouter.RegisterRoute('/api/v1/group', rmGet, @Group_GetAll); // Все группы
  HTTPRouter.RegisterRoute('/api/v1/group', rmPost, @Group_Create); // Создание группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmGet, @Group_Read); // Получение группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmPut, @Group_Update); // Редактирование группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmDelete, @Group_Delete); // Удаление группы

  // Статика
  HTTPRouter.RegisterRoute('/', @ReRouteRoot);
  MimeTypesFile := ExtractFileDir(Application.ExeName) + PathDelim + 'mime.types';
  RegisterFileLocation('app', 'public_html');

  Application.Title := 'Gate IntegrServ';
  Application.Port := API_Port;
  Application.Threaded := True;
  Application.Initialize;
  Writeln('Server is ready at localhost:' + IntToStr(Application.Port));
  Application.Run;
end.

