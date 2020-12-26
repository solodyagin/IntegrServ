unit uuserroutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteGetUsersCount = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TRouteUserGetAll = class(TMyRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

procedure TRouteGetUsersCount.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  HttpCode: Integer;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

procedure TRouteUserGetAll.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  JSON: TJSONObject;
  jArray: TJSONArray;
  jObject: TJSONObject;
  HttpCode: Integer;
  //Fields: TStringList;
begin
  JSON := TJSONObject.Create;
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
  JSONResponse(AResponse, JSON, HttpCode);
  JSON.Free;
end;

end.

