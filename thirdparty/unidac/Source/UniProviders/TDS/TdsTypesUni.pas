//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access in Direct mode
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsTypesUni;
interface
uses
SysUtils,CRTypes,MemData,
{$IFNDEF UNIDACPRO}
TdsConsts;
{$ELSE}
TdsConstsUni;
{$ENDIF}
type
OQ0OCQCOQ0=^OC0OCQCOQ0;
OC0OCQCOQ0=packed record
O0OOCQCOQ0:Cardinal;
OOOOCQCOQ0:Cardinal;
end;
OQOOCQCOQ0=^OCOOCQCOQ0;
OCOOCQCOQ0=packed record
O0QOCQCOQ0:Cardinal;
end;
OOQOCQCOQ0=^OQQOCQCOQ0;
OQQOCQCOQ0=packed record
OCQOCQCOQ0:Integer;
O0COCQCOQ0:Integer;
end;
OOCOCQCOQ0=packed record
OQCOCQCOQ0:array[0..7]of Byte;
end;
OCCOCQCOQ0=packed record
O00QQQCOQ0:array[0..9]of Byte;
end;
OO0QQQCOQ0=^OQ0QQQCOQ0;
OQ0QQQCOQ0=packed record
OC0QQQCOQ0:Word;
O0OQQQCOQ0:Word;
end;
OOOQQQCOQ0=^OQOQQQCOQ0;
OQOQQQCOQ0=packed record
OCOQQQCOQ0:Cardinal;
O0QQQQCOQ0:Word;
OOQQQQCOQ0:Word;
OQQQQQCOQ0:array[0..7]of Byte;
end;
OCQQQQCOQ0=packed record
case Integer of
0:(O0CQQQCOQ0:array[0..7]of Byte);
1:(OOCQQQCOQ0:array[0..3]of Word);
2:(OQCQQQCOQ0:array[0..1]of Cardinal);
3:(OCCQQQCOQ0:UInt64);
end;
O00CQQCOQ0=packed record
OO0CQQCOQ0:Word;
OQ0CQQCOQ0:Word;
OC0CQQCOQ0:Byte;
end;
O0OCQQCOQ0=array of TBytes;
O0QCQQCOQ0=(OOOCQQCOQ0,OQOCQQCOQ0,OCOCQQCOQ0);
OOQCQQCOQ0=record
OQQCQQCOQ0:string;
OCQCQQCOQ0:string;
O0CCQQCOQ0:string;
OOCCQQCOQ0:string;
end;
OQCCQQCOQ0=record
OCCCQQCOQ0:string;
O000QQCOQ0:string;
OO00QQCOQ0:string;
end;
OQ00QQCOQ0=record
OC00QQCOQ0:Boolean;
O0O0QQCOQ0:Boolean;
OOO0QQCOQ0:TBytes;
end;
OQO0QQCOQ0=array of OQ00QQCOQ0;
OCO0QQCOQ0=^O0Q0QQCOQ0;
O0Q0QQCOQ0=record
OOQ0QQCOQ0:string;
OQQ0QQCOQ0:string;
OCQ0QQCOQ0:Cardinal;
O0C0QQCOQ0,OOC0QQCOQ0:Word;
OQC0QQCOQ0:Word;
OCC0QQCOQ0:Byte;
O00OQQCOQ0:Word;
OO0OQQCOQ0:string;
OQ0OQQCOQ0:TFieldDescKind;
OC0OQQCOQ0:Byte;
O0OOQQCOQ0:Cardinal;
OOOOQQCOQ0:O00CCQCOQ0;
OQOOQQCOQ0:Cardinal;
OCOOQQCOQ0:Boolean;
O0QOQQCOQ0:Boolean;
OOQOQQCOQ0:Boolean;
OQQOQQCOQ0:Boolean;
OCQOQQCOQ0:Boolean;
O0COQQCOQ0:OOQCQQCOQ0;
OOCOQQCOQ0:OQCCQQCOQ0;
OQCOQQCOQ0:O00CQQCOQ0;
OCCOQQCOQ0:Cardinal;
O00QOOCOQ0:OQ00QQCOQ0;
end;
OO0QOOCOQ0=array of O0Q0QQCOQ0;
implementation
end.
