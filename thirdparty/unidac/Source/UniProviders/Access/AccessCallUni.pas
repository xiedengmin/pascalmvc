
//////////////////////////////////////////////////
//  MS Access Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I AccessDac.inc}
unit AccessCallUni;

interface

uses
  Windows,
  CLRClasses,
  CRTypes;

const
  ODBCPP32 = 'odbccp32.dll';

  mdbDriver = 'Microsoft Access Driver (*.mdb)';
  accdbDriver = 'Microsoft Access Driver (*.mdb, *.accdb)';

type
  TSQLConfigDataSource =
    function(hwndParent: Integer;
    fRequest: Integer;
    lpszDriverString: PAnsiChar;
    lpszAttributes: PAnsiChar): Integer; stdcall;

  procedure CreateNewDataBase(const DBName: string; accdbFormat: Boolean);

implementation

uses
  SysUtils;

const
  ODBC_ADD_DSN = 1;


procedure CreateNewDataBase(const DBName: string; accdbFormat: Boolean);
var
  hLib: HMODULE;
  SQLConfigDataSource: TSQLConfigDataSource;
  CreationResult: Integer;
begin
  if FileExists(DBName) then Exit;

  hLib := LoadLibraryExA(PAnsiChar(ODBCPP32), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  try
    if NativeUInt(hLib) = 0 then
      raise Exception.Create('Cannot load library: ' + ODBCPP32);

    SQLConfigDataSource := GetProcAddress(hLib, PChar('SQLConfigDataSource'));
    if @SQLConfigDataSource = nil then
      raise Exception.Create('SQLConfigDataSource function is not linked');

    if accdbFormat then
      CreationResult := SQLConfigDataSource(0, ODBC_ADD_DSN, PAnsiChar(accdbDriver), PAnsiChar('CREATE_DBV12="' + AnsiString(DBName) +'"'))
    else
      CreationResult := SQLConfigDataSource(0, ODBC_ADD_DSN, PAnsiChar(mdbDriver), PAnsiChar('CREATE_DB="' + AnsiString(DBName) +'"'));

    if CreationResult <> 1 then
      raise Exception.Create('Cannot create ODBC alias');
  finally
    FreeLibrary(hLib);
  end;
end;

end.
