
{$I Dac.inc}

unit DacVcl;

interface

uses
  Classes, TypInfo, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, Forms, DB,
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  MemData, DBAccess, DASQLMonitor;

{$I DacGui.inc}

