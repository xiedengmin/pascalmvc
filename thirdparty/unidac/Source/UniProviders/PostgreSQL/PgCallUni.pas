
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgCallUni;

{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  CRTypes, CRFunctions, MemData;


const
  DACProductName = 'PgDAC';

{ PostgerSQL datatypes }

  SQL_PG_BIGINT = 20;
  SQL_PG_BIT = 1560;
  SQL_PG_BOOLEAN = 16;
  SQL_PG_BOX = 603;
  SQL_PG_BYTEA = 17;
  SQL_PG_CHAR = 1042;
  SQL_PG_CIRCLE = 718;
  SQL_PG_DATE = 1082;
  SQL_PG_DOUBLE = 701;
  SQL_PG_INT = 23;
  SQL_PG_INTERVAL = 1186;
  SQL_PG_LINE = 628;
  SQL_PG_LSEG = 601;
  SQL_PG_MONEY = 790;
  SQL_PG_NUMERIC = 1700;
  SQL_PG_PATH = 602;
  SQL_PG_POINT = 600;
  SQL_PG_POLYGON = 604;
  SQL_PG_REAL = 700;
  SQL_PG_SMALLINT = 21;
  SQL_PG_TEXT = 25;
  SQL_PG_TIME = 1083;
  SQL_PG_TIMESTAMP = 1114;
  SQL_PG_TIMESTAMPTZ = 1184;
  SQL_PG_TIMETZ = 1266;
  SQL_PG_VARBIT = 1562;
  SQL_PG_VARCHAR = 1043;
  SQL_PG_ROW = 1;
  SQL_PG_REFCURSOR = 1790;
  SQL_PG_INT_ARRAY = 1007;
  SQL_PG_JSONB = 3802;

{ PostgerSQL internal datatypes }

  SQL_PG_OID = 26;
  SQL_PG_CID = 29;
  SQL_PG_CIDR = 650;
  SQL_PG_OIDVECTOR = 30;
  SQL_PG_INT2VECTOR = 22;
  SQL_PG_NAME = 19;
  SQL_PG_MACADDR = 829;
  SQL_PG_INET = 869;
  SQL_PG_FLOAT4 = 188;
  SQL_PG_CHARACTER = 18;
  SQL_PG_INTERVAL12 = 10000;
  SQL_PG_OIDVECTOR801 = 10001;
  SQL_PG_UNKNOWN = 705;
  SQL_PG_VOID = 2278;
  SQL_PG_UUID = 2950;
  PG_TYPE_RELTYPE_OID = 71;
  PG_ATTRIBUTE_RELTYPE_OID = 75;
  PG_PROC_RELTYPE_OID = 81;
  PG_CLASS_RELTYPE_OID = 83;

  { charsets }

  PG_WIN866 = 866;
  PG_WIN874 = 874;
  PG_GBK = 936;
  PG_UHC = 949;
  PG_BIG5 = 950;
  PG_JOHAB = 1361;
  PG_WIN1250 = 1250;
  PG_WIN1251 = 1251;
  PG_WIN1252 = 1252;
  PG_WIN1253 = 1253;
  PG_WIN1254 = 1254;
  PG_WIN1255 = 1255;
  PG_WIN1256 = 1256;
  PG_WIN1257 = 1257;
  PG_WIN1258 = 1258;
  PG_KOI8R = 20866;
  PG_KOI8U = 21866;
  PG_ISO_8859_5 = 28595;
  PG_ISO_8859_6 = 28596;
  PG_ISO_8859_7 = 28597;
  PG_ISO_8859_8 = 38598;
  PG_LATIN1 = 28591;
  PG_LATIN2 = 28592;
  PG_LATIN3 = 28593;
  PG_LATIN4 = 28594;
  PG_LATIN5 = 28599;
  PG_LATIN6 = 28600;
  PG_LATIN7 = 28603;
  PG_LATIN8 = 28604;
  PG_LATIN9 = 28605;
  PG_GB18030 = 54936;
  PG_SJIS = 932;
  PG_EUC_JIS_2004 = 20932;
  PG_EUC_JP = 51932;
  PG_EUC_CN = 51936;
  PG_EUC_KR = 51949;
  PG_EUC_TW = 51950;
  PG_UTF8 = 65001;

type
  TPgCodePageRec = record
    CodePage: Cardinal;
    Name: string;
  end;

const
  CodePagesCount = 41;
  CodePages: array[0..CodePagesCount - 1] of TPgCodePageRec =
    (
      (CodePage: PG_WIN866;       Name: 'WIN866'),
      (CodePage: PG_WIN874;       Name: 'WIN874'),
      (CodePage: PG_GBK;          Name: 'GBK'),
      (CodePage: PG_UHC;          Name: 'UHC'),
      (CodePage: PG_BIG5;         Name: 'BIG5'),
      (CodePage: PG_JOHAB;        Name: 'JOHAB'),
      (CodePage: PG_WIN1250;      Name: 'WIN1250'),
      (CodePage: PG_WIN1251;      Name: 'WIN1251'),
      (CodePage: PG_WIN1252;      Name: 'WIN1252'),
      (CodePage: PG_WIN1253;      Name: 'WIN1253'),
      (CodePage: PG_WIN1254;      Name: 'WIN1254'),
      (CodePage: PG_WIN1255;      Name: 'WIN1255'),
      (CodePage: PG_WIN1256;      Name: 'WIN1256'),
      (CodePage: PG_WIN1257;      Name: 'WIN1257'),
      (CodePage: PG_WIN1258;      Name: 'WIN1258'),
      (CodePage: PG_KOI8R;        Name: 'KOI8R'),
      (CodePage: PG_KOI8U;        Name: 'KOI8U'),
      (CodePage: PG_ISO_8859_5;   Name: 'ISO_8859_5'),
      (CodePage: PG_ISO_8859_5;   Name: 'ISO_8859_6'),
      (CodePage: PG_ISO_8859_5;   Name: 'ISO_8859_7'),
      (CodePage: PG_ISO_8859_5;   Name: 'ISO_8859_8'),
      (CodePage: PG_LATIN1;       Name: 'LATIN1'),
      (CodePage: PG_LATIN2;       Name: 'LATIN2'),
      (CodePage: PG_LATIN3;       Name: 'LATIN3'),
      (CodePage: PG_LATIN4;       Name: 'LATIN4'),
      (CodePage: PG_LATIN5;       Name: 'LATIN5'),
      (CodePage: PG_LATIN6;       Name: 'LATIN6'),
      (CodePage: PG_LATIN7;       Name: 'LATIN7'),
      (CodePage: PG_LATIN8;       Name: 'LATIN8'),
      (CodePage: PG_LATIN9;       Name: 'LATIN9'),
      (CodePage: PG_GB18030;      Name: 'GB18030'),
      (CodePage: PG_SJIS;         Name: 'SJIS'),
      (CodePage: PG_EUC_JIS_2004; Name: 'EUC_JIS_2004'),
      (CodePage: 50220;           Name: 'EUC_JIS_2004'),
      (CodePage: 50221;           Name: 'EUC_JIS_2004'),
      (CodePage: 50222;           Name: 'EUC_JIS_2004'),
      (CodePage: PG_EUC_JP;       Name: 'EUC_JP'),
      (CodePage: PG_EUC_CN;       Name: 'EUC_CN'),
      (CodePage: PG_EUC_KR;       Name: 'EUC_KR'),
      (CodePage: PG_EUC_TW;       Name: 'EUC_TW'),
      (CodePage: PG_UTF8;         Name: 'UTF8')
  );

type
  OID = integer;
  LODescriptor = integer;


  TPgSQLItemDesc = record
    FieldName: string;
    TableOid: integer;
    TableCol: Smallint;
    TypeOid: integer;
    TypeSize: Smallint;
    TypeModifier: integer;
    FormatCode: Smallint;
    Described: boolean;
    DataType: Word;
    SubDataType: Word;
    Length: integer;
    Scale: integer;
    Size: integer;
    Fixed: boolean;
    ObjectType: TObjectType;
  end;

  TPgSQLItemDescs = array of TPgSQLItemDesc;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  DAConsts;


initialization

finalization

end.
