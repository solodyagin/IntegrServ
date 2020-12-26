unit ugrouproutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteGetGroupsCount = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteGroupGetAll = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteGroupCreate = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteGroupRead = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteGroupUpdate = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteGroupDelete = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

procedure TRouteGetGroupsCount.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteGroupGetAll.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  jArray: TJSONArray;
  jObject: TJSONObject;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteGroupCreate.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  jParser: TJSONParser;
  jObject: TJSONObject;
  Name: String;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteGroupRead.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  Id: String;
  jObject: TJSONObject;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteGroupUpdate.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  Id: String;
  jParser: TJSONParser;
  jObject: TJSONObject;
  Name: String;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteGroupDelete.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  Id: String;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

end.

