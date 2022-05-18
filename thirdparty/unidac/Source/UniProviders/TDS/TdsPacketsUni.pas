//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access in Direct mode
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsPacketsUni;
interface
{$IFDEF NEXTGEN}
uses
CRTypes;
{$ENDIF}
type
OQ0QOOCOQ0=^OC0QOOCOQ0;
OC0QOOCOQ0=packed record
O0OQOOCOQ0:Byte;
OOOQOOCOQ0:Byte;
OQOQOOCOQ0:Word;
OCOQOOCOQ0:Word;
O0QQOOCOQ0:Byte;
OOQQOOCOQ0:Byte;
end;
const
OQQQOOCOQ0=0;
OCQQOOCOQ0=1;
O0CQOOCOQ0=2;
OOCQOOCOQ0=8;
OQCQOOCOQ0=$10;
OCCQOOCOQ0=1;
O00COOCOQ0=2;
OO0COOCOQ0=4;
OQ0COOCOQ0=8;
OC0COOCOQ0=$10;
O0OCOOCOQ0=$20;
OOOCOOCOQ0=0;
OQOCOOCOQ0=1;
OCOCOOCOQ0=2;
O0QCOOCOQ0=3;
OOQCOOCOQ0=4;
OQQCOOCOQ0=5;
OCQCOOCOQ0=$FF;
const
O0CCOOCOQ0=$0000;
OOCCOOCOQ0=$0001;
OQCCOOCOQ0=$0002;
OCCCOOCOQ0=$0004;
O000OOCOQ0=$0008;
OO00OOCOQ0=$0010;
OQ00OOCOQ0=$0020;
OC00OOCOQ0=$0040;
O0O0OOCOQ0=$0100;
implementation
end.
