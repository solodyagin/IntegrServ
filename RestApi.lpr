program RestApi;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  cmem, {$ENDIF}
  SysUtils,
  StrUtils,
  Classes,
  IniFiles,
  fphttpapp,
  httpdefs,
  httproute,
  fpwebfile,
  fpjson,
  jsonparser,
  jsonscanner,
  base64,
  odbcconn,
  SQLDB,
  DB;

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
  var
    headerValue, b64decoded, username, password: String;
  begin
    if not API_Auth then
      Exit;
    headerValue := ARequest.Authorization;
    if Length(headerValue) = 0 then
      raise Exception.Create('This endpoint requires authentication');
    if ExtractWord(1, headerValue, [' ']) <> 'Basic' then
      raise Exception.Create('Only Basic Authentication is supported');
    b64decoded := DecodeStringBase64(ExtractWord(2, headerValue, [' ']));
    username := ExtractWord(1, b64decoded, [':']);
    password := ExtractWord(2, b64decoded, [':']);
    if (username <> API_UserName) or (password <> API_Password) then
      raise Exception.Create('Invalid API credentials');
  end;

  procedure JSONResponse(AResponse: TResponse; JSON: TJSONObject; HttpCode: Integer);
  begin
    AResponse.Code := HttpCode;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.Content := JSON.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0);
    AResponse.ContentLength := Length(AResponse.Content);
    AResponse.SendContent;
  end;

  procedure User_GetAll(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    bCount: Boolean;
    jArray: TJSONArray;
    jObject: TJSONObject;
    HttpCode: Integer;
    //Fields: TStringList;
  begin
    JSON := TJSONObject.Create;
    try
      ValidateRequest(ARequest);
      bCount := ARequest.QueryFields.IndexOf('count') > -1;
      Connection := TODBCConnection.Create(nil);
      Connection.Driver := ODBC_Driver;
      Connection.DatabaseName := ODBC_DatabaseName;
      Connection.UserName := ODBC_UserName;
      Connection.Password := ODBC_Password;
      //Connection.Params.Add('Locale Identifier=1251');
      //Connection.Params.Add('ExtendedAnsiSQL=1');
      //Connection.Params.Add('CHARSET=ansi');
      Connection.Connected := True;
      //Connection.KeepConnection := True;
      Transaction := TSQLTransaction.Create(nil);
      Transaction.DataBase := Connection;
      Transaction.Action := caCommit;
      Transaction.Active := True;
      Query := TSQLQuery.Create(nil);
      Query.DataBase := Connection;
      Query.UsePrimaryKeyAsKey := False;
      if bCount then
        Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users'
      else
        Query.SQL.Text := 'SELECT * FROM Users';
      Query.ExecSQL;
      Query.Open;
      JSON.Add('success', True);
      // Поля таблицы
      //Fields := TStringList.Create;
      //Query.GetFieldNames(Fields);
      //Writeln(Fields.CommaText);
      //Fields.Free;
      if bCount then
      begin
        Query.First;
        JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
      end
      else
      begin
        jArray := TJSONArray.Create;
        while not Query.EOF do
        begin
          jObject := TJSONObject.Create;
          jObject.Add('id', Query.FieldByName('UserPtr').AsLargeInt); // Идентификатор пользователя
          jObject.Add('number', Query.FieldByName('Number').AsUTF8String); // Номер карты
          jObject.Add('lastName', Query.FieldByName('LastName').AsUTF8String); // Фамилия
          jObject.Add('firstName', Query.FieldByName('FirstName').AsUTF8String);// Имя
          jObject.Add('fatherName', Query.FieldByName('FatherName').AsUTF8String); // Отчество
          jObject.Add('groupId', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
          jArray.Add(jObject.Clone);
          FreeAndNil(jObject);
          Query.Next;
        end;
        JSON.Add('result', jArray.Clone);
        FreeAndNil(jArray);
      end;
      Query.Close;
      Connection.Free;
      Transaction.Free;
      Query.Free;
      HttpCode := 200;
    except
      on E: Exception do
      begin
        JSON.Add('success', False);
        JSON.Add('reason', E.Message);
        HttpCode := 401;
      end;
    end;
    JSONResponse(AResponse, JSON, HttpCode);
    FreeAndNil(JSON);
  end;

  procedure Group_GetAll(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    bCount: Boolean;
    jArray: TJSONArray;
    jObject: TJSONObject;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    try
      ValidateRequest(ARequest);
      bCount := ARequest.QueryFields.IndexOf('count') > -1;
      Connection := TODBCConnection.Create(nil);
      Connection.Driver := ODBC_Driver;
      Connection.DatabaseName := ODBC_DatabaseName;
      Connection.UserName := ODBC_UserName;
      Connection.Password := ODBC_Password;
      Connection.Connected := True;
      Transaction := TSQLTransaction.Create(nil);
      Transaction.DataBase := Connection;
      Transaction.Action := caCommit;
      Transaction.Active := True;
      Query := TSQLQuery.Create(nil);
      Query.DataBase := Connection;
      Query.UsePrimaryKeyAsKey := False;
      if bCount then
        Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Groups'
      else
        Query.SQL.Text := 'SELECT * FROM Groups';
      Query.ExecSQL;
      Query.Open;
      JSON.Add('success', True);
      if bCount then
      begin
        Query.First;
        JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
      end
      else
      begin
        jArray := TJSONArray.Create;
        while not Query.EOF do
        begin
          jObject := TJSONObject.Create;
          jObject.Add('id', Query.FieldByName('GroupPtr').AsLargeInt); // Идентификатор группы
          jObject.Add('name', Query.FieldByName('Name').AsUTF8String); // Имя группы
          jArray.Add(jObject.Clone);
          FreeAndNil(jObject);
          Query.Next;
        end;
        JSON.Add('result', jArray.Clone);
        FreeAndNil(jArray);
      end;
      Query.Close;
      Connection.Free;
      Transaction.Free;
      Query.Free;
      HttpCode := 200;
    except
      on E: Exception do
      begin
        JSON.Add('success', False);
        JSON.Add('reason', E.Message);
        HttpCode := 401;
      end;
    end;
    JSONResponse(AResponse, JSON, HttpCode);
    FreeAndNil(JSON);
  end;

  procedure Group_Create(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    Name: String;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    jParser: TJSONParser;
    jObject: TJSONObject;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    try
      ValidateRequest(ARequest);
      //if ARequest.ContentType <> 'application/json' then
      //  raise Exception.Create('ContentType must be "application/json"');
      jParser := TJSONParser.Create(ARequest.Content, DefaultOptions);
      jObject := jParser.Parse as TJSONObject;
      Name := jObject.Strings['name'];
      FreeAndNil(jObject);
      FreeAndNil(jParser);
      if Name = '' then
        raise Exception.Create('Parameter "name" is empty');
      Connection := TODBCConnection.Create(nil);
      Connection.Driver := ODBC_Driver;
      Connection.DatabaseName := ODBC_DatabaseName;
      Connection.UserName := ODBC_UserName;
      Connection.Password := ODBC_Password;
      Connection.Connected := True;
      Transaction := TSQLTransaction.Create(nil);
      Transaction.DataBase := Connection;
      Transaction.Action := caCommit;
      Transaction.Active := True;
      Query := TSQLQuery.Create(nil);
      Query.DataBase := Connection;
      Query.UsePrimaryKeyAsKey := False;
      Query.SQL.Text := 'INSERT INTO Groups (name) VALUES (:name)';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('name').Value := Name;
      Query.ExecSQL;
      //Query.SQL.Text := 'SELECT @@IDENTITY AS LastID';
      //Query.Open;
      //Query.First;
      //Writeln(Query.FieldByName('LastID').AsLargeInt);
      JSON.Add('success', True);
      Query.Close;
      Connection.Free;
      Transaction.Free;
      Query.Free;
      HttpCode := 200;
    except
      on E: Exception do
      begin
        JSON.Add('success', False);
        JSON.Add('reason', E.Message);
        Writeln(E.Message);
        HttpCode := 401;
      end;
    end;
    JSONResponse(AResponse, JSON, HttpCode);
    FreeAndNil(JSON);
  end;

  procedure Group_Delete(ARequest: TRequest; AResponse: TResponse);
  var
    JSON: TJSONObject;
    ID: String;
    Connection: TODBCConnection;
    Transaction: TSQLTransaction;
    Query: TSQLQuery;
    HttpCode: Integer;
  begin
    JSON := TJSONObject.Create;
    try
      ValidateRequest(ARequest);
      ID := ARequest.RouteParams['id'];
      if ID = '' then
        raise Exception.Create('Parameter "id" is empty');
      Connection := TODBCConnection.Create(nil);
      Connection.Driver := ODBC_Driver;
      Connection.DatabaseName := ODBC_DatabaseName;
      Connection.UserName := ODBC_UserName;
      Connection.Password := ODBC_Password;
      Connection.Connected := True;
      Transaction := TSQLTransaction.Create(nil);
      Transaction.DataBase := Connection;
      Transaction.Action := caCommit;
      Transaction.Active := True;
      Query := TSQLQuery.Create(nil);
      Query.DataBase := Connection;
      Query.UsePrimaryKeyAsKey := False;
      Query.SQL.Text := 'DELETE FROM Groups WHERE GroupPtr=:id';
      Query.Params.ParseSQL(Query.SQL.Text, True);
      Query.ParamByName('id').Value := ID;
      Query.ExecSQL;
      JSON.Add('success', True);
      Connection.Free;
      Transaction.Free;
      Query.Free;
      HttpCode := 200;
    except
      on E: Exception do
      begin
        JSON.Add('success', False);
        JSON.Add('reason', E.Message);
        Writeln(E.Message);
        HttpCode := 401;
      end;
    end;
    JSONResponse(AResponse, JSON, HttpCode);
    FreeAndNil(JSON);
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
  HTTPRouter.RegisterRoute('/api/v1/user', rmGet, @User_GetAll);
  HTTPRouter.RegisterRoute('/api/v1/group', rmGet, @Group_GetAll);
  HTTPRouter.RegisterRoute('/api/v1/group', rmPost, @Group_Create);
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmDelete, @Group_Delete);

  // Статика
  HTTPRouter.RegisterRoute('/', @ReRouteRoot);
  MimeTypesFile := ExtractFileDir(Application.ExeName) + PathDelim + 'mime.types';
  RegisterFileLocation('app', 'public_html');

  Application.Title := 'Gate RestAPI';
  Application.Port := API_Port;
  Application.Threaded := True;
  Application.Initialize;
  Writeln('Server is ready at localhost:' + IntToStr(Application.Port));
  Application.Run;
end.

