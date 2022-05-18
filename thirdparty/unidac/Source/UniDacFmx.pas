
{$I UniDac.inc}

{$DEFINE FMX}

unit UniDacFmx;

interface

uses
  SysUtils, Classes, DB, 
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  CRParser, CRAccess, CRServerEnumerator, DBAccess, DacFmx,
  UniConnectFormFmx, UniProvider, Uni;

{$I UniDacGui.inc}

