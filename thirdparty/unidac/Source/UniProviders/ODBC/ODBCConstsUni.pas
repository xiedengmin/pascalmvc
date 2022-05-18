
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright (c) 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCConstsUni;

interface

const
{$IFDEF MSWINDOWS}
  ODBCDLLName = 'odbc32.dll';
{$ELSE}
{$IFDEF MACOS}
  ODBCDLLName = 'libiodbc.dylib';
{$ELSE}
  ODBCDLLName = 'libodbc.so.2';
{$ENDIF}
{$ENDIF}

{$IFDEF USE_UTF8_DRIVER}
  ODBCCharSize = 4;
{$ELSE}
  ODBCCharSize = SizeOf(Char);
{$ENDIF}

resourcestring
  SWrongDatabaseName      = 'Changing database name to default value is not allowed';

implementation

end.
