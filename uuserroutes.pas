unit uuserroutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fpjson, jsonparser, jsonscanner, odbcconn, SQLDB, DB, uglobal;

type
  TRouteGetUsersCount = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

  TRouteUserGetAll = class(TMyRouteObject)
  public
    procedure ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer); override;
  end;

implementation

procedure TRouteGetUsersCount.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT COUNT(*) AS "cnt" FROM Users';
  Query.ExecSQL;
  Query.Open;
  Query.First;
  JSON.Add('result', Query.FieldByName('cnt').AsLargeInt);
end;

procedure TRouteUserGetAll.ExecuteRequest(ARequest: TRequest; var JSON: TJSONObject; var HttpCode: Integer);
var
  jArray: TJSONArray;
  jObject: TJSONObject;
  //Fields: TStringList;
begin
  Connection.Connected := True;
  Transaction.Active := True;
  Query.SQL.Text := 'SELECT * FROM Users';
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

end.

