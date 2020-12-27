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
  SysUtils, Classes, IniFiles, fphttpapp, httpdefs, httproute, fpwebfile, uglobal, uuserroutes, ugrouproutes;

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
  IniFileName := ChangeFileExt(Application.ExeName, '.ini');
  if not FileExists(IniFileName) then
  begin
    // Сохраняем настройки по-умолчанию
    with TIniFile.Create(IniFileName, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]) do
    begin
      WriteBool('API', 'IsAuthNeeded', API_IsAuthNeeded);
      WriteString('API', 'UserName', API_UserName);
      WriteString('API', 'Password', API_Password);
      WriteInteger('API', 'Port', API_Port);
      WriteString('ODBC', 'Driver', ODBC_Driver);
      WriteString('ODBC', 'DatabaseName', ODBC_DatabaseName);
      WriteString('ODBC', 'UserName', ODBC_UserName);
      WriteString('ODBC', 'Password', ODBC_Password);
      Free;
    end;
  end;

  // Загружаем настройки
  Writeln('Read settings from "' + IniFileName + '"');
  with TIniFile.Create(IniFileName, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]) do
  begin
    API_IsAuthNeeded := ReadBool('API', 'IsAuthNeeded', API_IsAuthNeeded);
    API_UserName := ReadString('API', 'UserName', API_UserName);
    API_Password := ReadString('API', 'Password', API_Password);
    API_Port := ReadInteger('API', 'Port', API_Port);
    ODBC_Driver := ReadString('ODBC', 'Driver', ODBC_Driver);
    ODBC_DatabaseName := ReadString('ODBC', 'DatabaseName', ODBC_DatabaseName);
    ODBC_UserName := ReadString('ODBC', 'UserName', ODBC_UserName);
    ODBC_Password := ReadString('ODBC', 'Password', ODBC_Password);
    Free;
  end;

  // Маршруты
  HTTPRouter.RegisterRoute('/catchall', rmAll, @CatchAll, True);
  HTTPRouter.RegisterRoute('/api/v1/getUsersCount', rmGet, TRouteGetUsersCount); // Количество пользователей
  HTTPRouter.RegisterRoute('/api/v1/user', rmGet, TRouteUserGetAll); // Все пользователи
  HTTPRouter.RegisterRoute('/api/v1/getGroupsCount', rmGet, TRouteGetGroupsCount); // Количество групп
  HTTPRouter.RegisterRoute('/api/v1/group', rmGet, TRouteGroupGetAll); // Все группы
  HTTPRouter.RegisterRoute('/api/v1/group', rmPost, TRouteGroupCreate); // Создание группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmGet, TRouteGroupRead); // Получение группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmPut, TRouteGroupUpdate); // Редактирование группы
  HTTPRouter.RegisterRoute('/api/v1/group/:id', rmDelete, TRouteGroupDelete); // Удаление группы

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

