
{$I Dac.inc}

{$DEFINE FMX}

unit DacFmx;

interface

uses
  Classes, SysUtils, DB, TypInfo,
  System.UITypes, FMX.Types, FMX.Platform, FMX.Forms, FMX.Controls, FMX.Memo,
  {$IFDEF VER18P} FMX.StdCtrls,{$ENDIF}
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Registry,
{$ENDIF}
  MemData, DBAccess, DASQLMonitor;

{$I DacGui.inc}

