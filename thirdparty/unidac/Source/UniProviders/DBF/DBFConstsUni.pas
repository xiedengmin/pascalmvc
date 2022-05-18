
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFConstsUni;

interface

uses
  Classes,
  CRTypes,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMap,{$ENDIF}
  DBFDataTypeMap;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMapUni,{$ENDIF}
  DBFDataTypeMapUni;
{$ENDIF}

type
  TDBFFormat = (dfAuto, dfdBaseIII, dfdBaseIV, dfdBaseV, dfdBaseVII, dfFoxPro2, dfVisualFoxPro, dfHiPerSix, dfCodebase, dfClipper);
  TDBFConnectMode = (cmShared, cmExclusive, cmUnsafe);
  TDBFIndexKind = (ikNative, ikLocal);
  TDBFIdentifierCase = (icOriginal, icLower, icUpper);

const
  SInvalidPath = '"%s" is not a valid path';
  SDirectNotSupported    = 'Direct Mode is not supported';
  SNonDirectNotSupported = 'Non-Direct Mode is not supported';
  STableExists = 'Table "%s" already exists';
  STableNotFound = 'Table "%s" not found';
  SMemoOpenError = 'Error opening memo file'#13#10'%s';
  SEmptyIndexExpression = 'Index expression is empty';
  SInvalidTableHeader = 'Invalid table header';
  SDuplicateIndexValue = 'Attemp to insert duplicate values to a unique index';

  DBF_EXT = '.DBF';
  DBT_EXT = '.DBT';
  FPT_EXT = '.FPT';
  SMT_EXT = '.SMT';
  MDX_EXT = '.MDX';
  CDX_EXT = '.CDX';

  {DBF_VER_FOXBASE         = $02; // FoxBASE
  DBF_VER_FOXBASEP        = $03; // FoxBASE+/Dbase III plus, no memo
  DBF_VER_DBASE7          = $04; // dBASE VII
  DBF_VER_VFP             = $30; // Visual FoxPro
  DBF_VER_VFP_AUTOINC     = $31; // Visual FoxPro, autoincrement enabled
  DBF_VER_VFP_VARCHAR     = $32; // Visual FoxPro with field type Varchar or Varbinary = VFP 9
  DBF_VER_DBASE4_TABLE    = $43; // dBASE IV SQL table files, no memo
  DBF_VER_DBASE4_SYSTEM   = $63; // dBASE IV SQL system files, no memo
  DBF_VER_DBASE3P         = $83; // FoxBASE+/dBASE III PLUS, with memo
  DBF_VER_DBASE4          = $8B; // dBASE IV with memo = dBaseForWin, VisualDBase
  DBF_VER_DBASE4_MEMO     = $CB; // dBASE IV SQL table files, with memo
  DBF_VER_FOXPRO2         = $F5; // FoxPro 2.x (or earlier) with memo
  DBF_VER_HIPERSIX        = $E5; // HiPer-Six format with SMT memo file
  DBF_VER_FOXBASEP_MEMO   = $FB; // FoxBASE}
  // Bit 3 and bit 7 indicate presence of a dBASE IV or dBASE for Windows memo file;
  // bits 4-6 indicate the presence of a dBASE IV SQL table;
  // bit 7 indicates the presence of any .DBT memo file (either a dBASE III PLUS type or a dBASE IV or dBASE for Windows memo file)

  DBF_DBASE4_DBT          = 8;
  DBF_HAS_SQLTABLE        = $70;
  DBF_HAS_DBT             = $80;

  DBF_FLAG_HAS_MDX        = $01; // file has a structural cdx/mdx
  DBF_FLAG_HAS_MEMO       = $02; // file has a Memo field
  DBF_FLAG_IS_DBC         = $04; // file is a database (.dbc)

  ANSI_SPACE              = $20;

  CHAR_TRUE               = 'T';
  CHAR_FALSE              = 'F';

  DBF_FIELDFLAG_SYSTEM    = $01; // System Column (not visible to user)
  DBF_FIELDFLAG_NULLABLE  = $02; // Column can store null values
  DBF_FIELDFLAG_BINARY    = $04; // Binary column (for CHAR and MEMO only): Any Character data that you do not want translated across code pages
    //0x06   (0x02+0x04) When a field is NULL and binary (Integer, Currency, and Character/Memo fields)
  DBF_FIELDFLAG_AUTOINC   = $08;
    //0x0C   Column is autoincrementing

  DBF_RECORD_BEGIN        = $20;
  DBF_RECORD_DELETED      = $2A;
  DBF_SECTION_END         = $0D;
  DBF_RECORD_EOF: AnsiChar = {$IFNDEF NEXTGEN}#$1A{$ELSE}$1A{$ENDIF};

  DBF_TYPE_CHAR           = {$IFDEF NEXTGEN}Ord{$ENDIF}('C'); // 1 to 254
  DBF_TYPE_CURRENCY       = {$IFDEF NEXTGEN}Ord{$ENDIF}('Y');
  DBF_TYPE_NUMERIC        = {$IFDEF NEXTGEN}Ord{$ENDIF}('N'); // 1 to 32
  DBF_TYPE_FLOAT          = {$IFDEF NEXTGEN}Ord{$ENDIF}('F');
  DBF_TYPE_DATE           = {$IFDEF NEXTGEN}Ord{$ENDIF}('D'); // string YYYYMMDD like 20021201
  DBF_TYPE_TIME           = {$IFDEF NEXTGEN}Ord{$ENDIF}('T'); // DateTime, binary
  DBF_TYPE_TIMESTAMP      = {$IFDEF NEXTGEN}Ord{$ENDIF}('@');
  DBF_TYPE_BINARY         = {$IFDEF NEXTGEN}Ord{$ENDIF}('B'); // Binary: dBASE 5; // Double: MS Visual FoxPro
  DBF_TYPE_INTEGER        = {$IFDEF NEXTGEN}Ord{$ENDIF}('I'); // big-endian
  DBF_TYPE_LOGICAL        = {$IFDEF NEXTGEN}Ord{$ENDIF}('L');
  DBF_TYPE_MEMO           = {$IFDEF NEXTGEN}Ord{$ENDIF}('M'); // Memo
  DBF_TYPE_GENERAL        = {$IFDEF NEXTGEN}Ord{$ENDIF}('G'); // General (Reference to an OLE object)
  DBF_TYPE_PICTURE        = {$IFDEF NEXTGEN}Ord{$ENDIF}('P'); // Pictupe, FoxPro
  DBF_TYPE_AUTOINC        = {$IFDEF NEXTGEN}Ord{$ENDIF}('+');
  DBF_DB7_TYPE_DOUBLE     = {$IFDEF NEXTGEN}Ord{$ENDIF}('O'); // dBASE 7
  DBF_VFP_TYPE_DOUBLE     = {$IFDEF NEXTGEN}Ord{$ENDIF}('B'); // VFP
  DBF_TYPE_OLEBLOB        = {$IFDEF NEXTGEN}Ord{$ENDIF}('O'); // dBASE for Win, VisDBase
  DBF_TYPE_BLOB           = {$IFDEF NEXTGEN}Ord{$ENDIF}('W'); // Blob: MS Visual FoxPro
  DBF_TYPE_NULLFLAGS      = {$IFDEF NEXTGEN}Ord{$ENDIF}('0'); // hidden, bitmask
  // FoxPro 9
  DBF_TYPE_VARBINARY      = {$IFDEF NEXTGEN}Ord{$ENDIF}('Q'); // Varbinary: MS Visual FoxPro, 1 to 254
  DBF_TYPE_VARCHAR        = {$IFDEF NEXTGEN}Ord{$ENDIF}('V'); // Varchar: MS Visual FoxPro, 1 to 254

  DBF_CHARNULL            = $80;

  DBT4_BLOCK_SIGN         = $0008FFFF;

  MDX_MAX_INDEX_COUNT     = $30;
  MDX_MAX_INDEX_NO        = MDX_MAX_INDEX_COUNT - 1;

  INDEX_KEY_FORMAT_CALCULATED = $00;
  INDEX_KEY_FORMAT_DATA       = $10;

  { dBaseVII }

  DBF7_PROP_REQUIRED      = 1;
  DBF7_PROP_MIN           = 2;
  DBF7_PROP_MAX           = 3;
  DBF7_PROP_DEFAULT       = 4;
  DBF7_PROP_DATABASE      = 6;

  DBF7_PROPTYPE_NONE      = 0;    // 00 No type - constraint
  DBF7_PROPTYPE_CHAR      = 1;    // 01 Char
  DBF7_PROPTYPE_NUMERIC   = 2;    // 02 Numeric
  DBF7_PROPTYPE_MEMO      = 3;    // 03 Memo
  DBF7_PROPTYPE_LOGICAL   = 4;    // 04 Logical
  DBF7_PROPTYPE_DATE      = 5;    // 05 Date
  DBF7_PROPTYPE_FLOAT     = 6;    // 06 Float
  DBF7_PROPTYPE_OLE       = 8;    // 08 OLE
  DBF7_PROPTYPE_BINARY    = 9;    // 09 Binary
  DBF7_PROPTYPE_LONG      = $B;   // 11 Long
  DBF7_PROPTYPE_TIMESTAMP = $C;   // 12 Timestamp
  DBF7_PROPTYPE_DOUBLE    = $D;   // 13 Double
  DBF7_PROPTYPE_AUTOINC   = $E;   // 14 AutoIncrement (not settable from the Inspector)

  { FoxPro }

  CDX_NODETYPE_INDEX      = 0;
  CDX_NODETYPE_ROOT       = 1;
  CDX_NODETYPE_LEAF       = 2;

  { VisualFoxPro }

  VFP_NULLFLAGS_NAME      = '_NullFlags';
  VFP_NULLFLAGS_TYPE      = 'NULLFLAGS';


const
  DefLanguageDriverName = 'DBWINUS0';

type
  TDBFCodePage = (
    dpDefault,
    dpUnitedStatesOEM,            { $01 - 437 }
    dpGreekDOS437G,               { $6A - 737 }
    dpWesternEuropeanDOS,         { $02 - 850 }
    dpCentralEuropeanDOS,         { $64 - 852 }
    dpTurkishDOS,                 { $6B - 857 }
    dpPortugueseDOS,              { $24 - 860 }
    dpIcelandicDOS,               { $67 - 861 }
    dpFrenchCanadianDOS,          { $6C - 863 }
    dpNordicDOS,                  { $66 - 865 }
    dpCyrillicDOS,                { $65 - 866 }
    dpThai,                       { $7C - 874 }
    dpJapanese,                   { $7B - 932 }
    dpChineseSimplified,          { $7A - 936 }
    dpChineseTraditional,         { $78 - 950 }
    dpKorean,                     { $79 - 949 }
    dpCentralEuropeanANSI,        { $C8 - 1250 }
    dpCyrillicANSI,               { $C9 - 1251 }
    dpWindowsANSILatin1,          { $03 - 1252 }
    dpGreekANSI,                  { $CB - 1253 }
    dpTurkishANSI,                { $CA - 1254 }
    dpHebrewANSI,                 { $7D - 1255 }
    dpArabicANSI,                 { $7E - 1256 }
    dpBalticANSI,                 { $CC - 1257 }
    dpStandartMacintosh,          { $04 - 10000 }
    dpDanishDOS,                  { $08 - 865 }
    dpDutchDOS,                   { $09 - 437 }
    dpDutchDOSInternational,      { $0A - 850 }
    dpFinnishDOS,                 { $0B - 437 }
    dpFrenchDOS,                  { $0D - 437 }
    dpFrenchDOSInternational,     { $0E - 850 }
    dpGermanDOS,                  { $0F - 437 }
    dpGermanDOSInternational,     { $10 - 850 }
    dpItalianDOS,                 { $11 - 437 }
    dpItalianDOSInternational,    { $12 - 850 }
    dpJapaneseShiftJIS,           { $13 - 932 }
    dpSpanishDOSInternational,    { $14 - 850 }
    dpSwedishDOS,                 { $15 - 437 }
    dpSwedishDOSInternational,    { $16 - 850 }
    dpNorwegianDOS,               { $17 - 865 }
    dpSpanishDOS,                 { $18 - 437 }
    dpEnglishDOSGreatBritain,     { $19 - 437 }
    dpEnglishDOSGreatBritainInt,  { $1A - 850 }
    dpEnglishDOSUnitedStates,     { $1B - 437 }
    dpFrenchDOSCanada,            { $1C - 863 }
    dpFrenchDOSCanadaInt,         { $1D - 850 }
    dpCzechDOS,                   { $1F - 852 }
    dpHungarianDOS,               { $22 - 852 }
    dpPolishDOS,                  { $23 - 852 }
    dpPortugueseDOSInternational, { $25 - 850 }
    dpRussianDOS,                 { $26 - 866 }
    dpEnglishDOSUnitedStatesInt,  { $37 - 850 }
    dpRomanianDOS,                { $40 - 852 }
    dpChineseGBK,                 { $4D - 936 }
    dpKoreanANSI,                 { $4E - 949 }
    dpChineseBig5,                { $4F - 950 }
    dpThaiANSI,                   { $50 - 874 }
    dpWesternEuropeanANSI,        { $58 - 1252 }
    dpSpanishANSI,                { $59 - 1252 }
    dpKamenickyCzechDOS,          { $68 - 895 }
    dpMazoviaPolishDOS,           { $69 - 620 }
    dpGreekDOS,                   { $86 - 737 }
    dpSlovenianDOS,               { $87 - 852 }
    dpTurkishOEM,                 { $88 - 857 }
    dpRussianMacintosh,           { $96 - 10007 }
    dpEasternEuropianMacintosh,   { $97 - 10029 }
    dpGreekMacintosh              { $98 - 10006 }
  );

const
  LanguageDriverID: array[TDBFCodePage] of byte = (
    0,   { dpDefault }
    $01, { dpUnitedStatesOEM }
    $6A, { dpGreekDOS }
    $02, { dpWesternEuropeanDOS }
    $64, { dpCentralEuropeanDOS }
    $6B, { dpTurkishDOS }
    $24, { dpPortugueseDOS }
    $67, { dpIcelandicDOS }
    $6C, { dpFrenchCanadianDOS }
    $66, { dpNordicDOS }
    $65, { dpCyrillicDOS }
    $7C, { dpThai }
    $7B, { dpJapanese }
    $7A, { dpChineseSimplified }
    $78, { dpChineseTraditional }
    $79, { dpKorean }
    $C8, { dpCentralEuropeanANSI }
    $C9, { dpCyrillicANSI }
    $03, { dpWesternEuropeanANSI }
    $CB, { dpGreekANSI }
    $CA, { dpTurkishANSI }
    $7D, { dpHebrewANSI }
    $7E, { dpArabicANSI }
    $CC, { dpBalticANSI }
    $04, { dpStandartMacintosh }
    $08, { dpDanishDOS }
    $09, { dpDutchDOS }
    $0A, { dpDutchDOSInternational }
    $0B, { dpFinnishDOS }
    $0D, { dpFrenchDOS }
    $0E, { dpFrenchDOSInternational }
    $0F, { dpGermanDOS }
    $10, { dpGermanDOSInternational }
    $11, { dpItalianDOS }
    $12, { dpItalianDOSInternational }
    $13, { dpJapaneseShiftJIS }
    $14, { dpSpanishDOSInternational }
    $15, { dpSwedishDOS }
    $16, { dpSwedishDOSInternational }
    $17, { dpNorwegianDOS }
    $18, { dpSpanishDOS }
    $19, { dpEnglishDOSGreatBritain }
    $1A, { dpEnglishDOSGreatBritainInt }
    $1B, { dpEnglishDOSUnitedStates }
    $1C, { dpFrenchDOSCanada }
    $1D, { dpFrenchDOSCanadaInt }
    $1F, { dpCzechDOS }
    $22, { dpHungarianDOS }
    $23, { dpPolishDOS }
    $25, { dpPortugueseDOSInternational }
    $26, { dpRussianDOS }
    $37, { dpEnglishDOSUnitedStatesInt }
    $40, { dpRomanianDOS }
    $4D, { dpChineseGBK }
    $4E, { dpKoreanANSI }
    $4F, { dpChineseBig5 }
    $50, { dpThaiANSI }
    $58, { dpWesternEuropeanANSI }
    $59, { dpSpanishANSI }
    $68, { dpKamenickyCzechDOS }
    $69, { dpMazoviaPolishDOS }
    $86, { dpGreekDOS }
    $87, { dpSlovenianDOS }
    $88, { dpTurkishOEM }
    $96, { dpRussianMacintosh }
    $97, { dpEasternEuropianMacintosh }
    $98  { dpGreekMacintosh }
  );

  CodePageLocale: array[TDBFCodePage] of Cardinal = (
    0,     { dpDefault }
    437,   { dpUnitedStatesOEM }
    737,   { dpGreekDOS }
    850,   { dpWesternEuropeanDOS }
    852,   { dpCentralEuropeanDOS }
    857,   { dpTurkishDOS }
    860,   { dpPortugueseDOS }
    861,   { dpIcelandicDOS }
    863,   { dpFrenchCanadianDOS }
    865,   { dpNordicDOS }
    866,   { dpCyrillicDOS }
    874,   { dpThai }
    932,   { dpJapanese }
    936,   { dpChineseSimplified }
    950,   { dpChineseTraditional }
    949,   { dpKorean }
    1250,  { dpCentralEuropeanANSI }
    1251,  { dpCyrillicANSI }
    1252,  { dpWesternEuropeanANSI }
    1253,  { dpGreekANSI }
    1254,  { dpTurkishANSI }
    1255,  { dpHebrewANSI }
    1256,  { dpArabicANSI }
    1257,  { dpBalticANSI }
    10000, { dpStandartMacintosh }
    865,   { dpDanishDOS }
    437,   { dpDutchDOS }
    850,   { dpDutchDOSInternational }
    437,   { dpFinnishDOS }
    437,   { dpFrenchDOS }
    850,   { dpFrenchDOSInternational }
    437,   { dpGermanDOS }
    850,   { dpGermanDOSInternational }
    437,   { dpItalianDOS }
    850,   { dpItalianDOSInternational }
    932,   { dpJapaneseShiftJIS }
    850,   { dpSpanishDOSInternational }
    437,   { dpSwedishDOS }
    850,   { dpSwedishDOSInternational }
    865,   { dpNorwegianDOS }
    437,   { dpSpanishDOS }
    437,   { dpEnglishDOSGreatBritain }
    850,   { dpEnglishDOSGreatBritainInt }
    437,   { dpEnglishDOSUnitedStates }
    863,   { dpFrenchDOSCanada }
    850,   { dpFrenchDOSCanadaInt }
    852,   { dpCzechDOS }
    852,   { dpHungarianDOS }
    852,   { dpPolishDOS }
    850,   { dpPortugueseDOSInternational }
    866,   { dpRussianDOS }
    850,   { dpEnglishDOSUnitedStatesInt }
    852,   { dpRomanianDOS }
    936,   { dpChineseGBK }
    949,   { dpKoreanANSI }
    950,   { dpChineseBig5 }
    874,   { dpThaiANSI }
    1252,  { dpWesternEuropeanANSI }
    1252,  { dpSpanishANSI }
    895,   { dpKamenickyCzechDOS }
    620,   { dpMazoviaPolishDOS }
    737,   { dpGreekDOS }
    852,   { dpSlovenianDOS }
    857,   { dpTurkishOEM }
    10007, { dpRussianMacintosh }
    10029, { dpEasternEuropianMacintosh }
    10006  { dpGreekMacintosh }
  );

  CodePageString: array[TDBFCodePage] of string = (
    '',    { dpDefault }
    'US0', { dpUnitedStatesOEM }
    'GR0', { dpGreekDOS }
    'UK0', { dpWesternEuropeanDOS }
    'CZ0', { dpCentralEuropeanDOS }
    'TR0', { dpTurkishDOS }
    'PT0', { dpPortugueseDOS }
    'IC0', { dpIcelandicDOS }
    '',    { dpFrenchCanadianDOS }
    'NO',  { dpNordicDOS }
    'RU0', { dpCyrillicDOS }
    'TH0', { dpThai }
    'JP0', { dpJapanese }
    'CS0', { dpChineseSimplified }
    'CH0', { dpChineseTraditional }
    'KO0', { dpKorean }
    'CZ0', { dpCentralEuropeanANSI }
    'RU0', { dpCyrillicANSI }
    'UK0', { dpWesternEuropeanANSI }
    'GR0', { dpGreekANSI }
    'TR0', { dpTurkishANSI }
    'REW', { dpHebrewANSI }
    'AR0', { dpArabicANSI }
    '',    { dpBalticANSI }
    'UK0', { dpStandartMacintosh }
    'DA0', { dpDanishDOS }
    'NL0', { dpDutchDOS }
    'NL0', { dpDutchDOSInternational }
    'FI0', { dpFinnishDOS }
    'FR0', { dpFrenchDOS }
    'FR0', { dpFrenchDOSInternational }
    'DE0', { dpGermanDOS }
    'DE0', { dpGermanDOSInternational }
    'IT0', { dpItalianDOS }
    'IT1', { dpItalianDOSInternational }
    'JP0', { dpJapaneseShiftJIS }
    'ES0', { dpSpanishDOSInternational }
    'SV0', { dpSwedishDOS }
    'SV1', { dpSwedishDOSInternational }
    'NO0', { dpNorwegianDOS }
    'ES1', { dpSpanishDOS }
    'UK0', { dpEnglishDOSGreatBritain }
    'UK0', { dpEnglishDOSGreatBritainInt }
    'US0', { dpEnglishDOSUnitedStates }
    'CF1', { dpFrenchDOSCanada }
    'CF1', { dpFrenchDOSCanadaInt }
    'CZ0', { dpCzechDOS }
    'HDC', { dpHungarianDOS }
    'PO0', { dpPolishDOS }
    'PT0', { dpPortugueseDOSInternational }
    'RU0', { dpRussianDOS }
    'US0', { dpEnglishDOSUnitedStatesInt }
    '',    { dpRomanianDOS }
    'CN0', { dpChineseGBK }
    'KO0', { dpKoreanANSI }
    'TW0', { dpChineseBig5 }
    'TH0', { dpThaiANSI }
    'WE0', { dpWesternEuropeanANSI }
    'ES0', { dpSpanishANSI }
    'CZ0', { dpKamenickyCzechDOS }
    'PO1', { dpMazoviaPolishDOS }
    'GR0', { dpGreekDOS }
    'SL0', { dpSlovenianDOS }
    'TR0', { dpTurkishOEM }
    'RU0', { dpRussianMacintosh }
    'CZ0', { dpEasternEuropianMacintosh }
    'GR0'  { dpGreekMacintosh }
  );

var
  DbfPropTypeToDbfType: array[DBF7_PROPTYPE_NONE .. DBF7_PROPTYPE_AUTOINC] of AnsiChar;
  DbfFieldTypeToFieldLen, DbfIndexTypeToLen: array[0 .. 255] of integer;
  Dbf4FieldTypeToIndexType, Dbf7FieldTypeToIndexType, VfpFieldTypeToIndexType: array[0 .. 255] of AnsiChar;
  SqlTypeToDbfType: TIntValueStringList;

  SuppressIndexOpenErrors: boolean = False;

implementation

initialization

  FillChar(DbfPropTypeToDbfType, SizeOf(DbfPropTypeToDbfType), {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF});
  DbfPropTypeToDbfType[DBF7_PROPTYPE_NONE]      := {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF};    // 00 No type - constraint
  DbfPropTypeToDbfType[DBF7_PROPTYPE_CHAR]      := DBF_TYPE_CHAR;    // 01 Char
  DbfPropTypeToDbfType[DBF7_PROPTYPE_NUMERIC]   := DBF_TYPE_NUMERIC;    // 02 Numeric
  DbfPropTypeToDbfType[DBF7_PROPTYPE_MEMO]      := DBF_TYPE_MEMO;    // 03 Memo
  DbfPropTypeToDbfType[DBF7_PROPTYPE_LOGICAL]   := DBF_TYPE_LOGICAL;    // 04 Logical
  DbfPropTypeToDbfType[DBF7_PROPTYPE_DATE]      := DBF_TYPE_DATE;    // 05 Date
  DbfPropTypeToDbfType[DBF7_PROPTYPE_FLOAT]     := DBF_TYPE_FLOAT;    // 06 Float
  DbfPropTypeToDbfType[DBF7_PROPTYPE_OLE]       := DBF_TYPE_OLEBLOB;    // 08 OLE
  DbfPropTypeToDbfType[DBF7_PROPTYPE_BINARY]    := DBF_TYPE_BINARY;    // 09 Binary
  DbfPropTypeToDbfType[DBF7_PROPTYPE_LONG]      := DBF_TYPE_INTEGER;   // 11 Long
  DbfPropTypeToDbfType[DBF7_PROPTYPE_TIMESTAMP] := DBF_TYPE_TIMESTAMP;   // 12 Timestamp
  DbfPropTypeToDbfType[DBF7_PROPTYPE_DOUBLE]    := DBF_TYPE_OLEBLOB;   // 13 Double  ???
  DbfPropTypeToDbfType[DBF7_PROPTYPE_AUTOINC]   := DBF_TYPE_AUTOINC;   // 14 AutoIncrement (not settable from the Inspector)

  // constant fields length
  // Float, Numeric: has decimals
  FillChar(DbfFieldTypeToFieldLen, SizeOf(DbfFieldTypeToFieldLen), 0);
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_NUMERIC)] := 20;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_FLOAT)] := 20;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_MEMO)] := 10; // dBase as string, VFP as Integer (4)
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_LOGICAL)] := 1;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_DATE)] := 8;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_GENERAL)] := 10; // dBase as string, VFP as Integer (4)
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_BINARY)] := 10;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_TIMESTAMP)] := 8;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_INTEGER)] := 4;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_AUTOINC)] := 4;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_OLEBLOB)] := 8; // double dBaseVII
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_CURRENCY)] := 8;
  DbfFieldTypeToFieldLen[Byte(DBF_TYPE_TIME)] := 8;
  DbfFieldTypeToFieldLen[Byte(DBF_DB7_TYPE_DOUBLE)] := 8;

  FillChar(DbfIndexTypeToLen, SizeOf(DbfIndexTypeToLen), 0);
  DbfIndexTypeToLen[Byte(DBF_TYPE_NUMERIC)] := 12;
  DbfIndexTypeToLen[Byte(DBF_TYPE_FLOAT)] := 12;
  DbfIndexTypeToLen[Byte(DBF_TYPE_DATE)] := 8;
  DbfIndexTypeToLen[Byte(DBF_TYPE_TIMESTAMP)] := 8;
  DbfIndexTypeToLen[Byte(DBF_TYPE_INTEGER)] := 4;
  DbfIndexTypeToLen[Byte(DBF_TYPE_AUTOINC)] := 4;
  DbfIndexTypeToLen[Byte(DBF_TYPE_OLEBLOB)] := 8; // double dBaseVII
  DbfIndexTypeToLen[Byte(DBF_VFP_TYPE_DOUBLE)] := 8;
  DbfIndexTypeToLen[Byte(DBF_TYPE_FLOAT)] := 4;

  FillChar(Dbf4FieldTypeToIndexType, SizeOf(Dbf4FieldTypeToIndexType), 0);
  Dbf4FieldTypeToIndexType[Byte(DBF_TYPE_CHAR)] := DBF_TYPE_CHAR;
  Dbf4FieldTypeToIndexType[Byte(DBF_TYPE_NUMERIC)] := DBF_TYPE_NUMERIC;
  Dbf4FieldTypeToIndexType[Byte(DBF_TYPE_DATE)] := DBF_TYPE_DATE;

  FillChar(Dbf7FieldTypeToIndexType, SizeOf(Dbf7FieldTypeToIndexType), 0);
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_CHAR)] := DBF_TYPE_CHAR;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_NUMERIC)] := DBF_TYPE_NUMERIC;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_FLOAT)] := DBF_TYPE_NUMERIC;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_DATE)] := DBF_TYPE_DATE;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_TIMESTAMP)] := DBF_TYPE_TIMESTAMP;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_INTEGER)] := DBF_TYPE_INTEGER;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_AUTOINC)] := DBF_TYPE_AUTOINC;
  Dbf7FieldTypeToIndexType[Byte(DBF_TYPE_OLEBLOB)] := DBF_TYPE_OLEBLOB; // double

  FillChar(VfpFieldTypeToIndexType, SizeOf(VfpFieldTypeToIndexType), 0);
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_CHAR)] := DBF_TYPE_CHAR;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_NUMERIC)] := DBF_TYPE_NUMERIC; // data stored as DBF_VFP_TYPE_DOUBLE in index
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_DATE)] := DBF_TYPE_DATE;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_CURRENCY)] := DBF_TYPE_CURRENCY;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_FLOAT)] := DBF_TYPE_FLOAT;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_TIME)] := DBF_TYPE_TIME;
  VfpFieldTypeToIndexType[Byte(DBF_VFP_TYPE_DOUBLE)] := DBF_VFP_TYPE_DOUBLE;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_INTEGER)] := DBF_TYPE_INTEGER;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_GENERAL)] := DBF_TYPE_INTEGER;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_PICTURE)] := DBF_TYPE_INTEGER;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_BLOB)] := DBF_TYPE_INTEGER;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_VARBINARY)] := DBF_TYPE_VARBINARY;
  VfpFieldTypeToIndexType[Byte(DBF_TYPE_VARCHAR)] := DBF_TYPE_VARCHAR;

  SqlTypeToDbfType := TIntValueStringList.Create;
  SqlTypeToDbfType.Add('CHAR', byte(DBF_TYPE_CHAR));
  SqlTypeToDbfType.Add('CURRENCY', byte(DBF_TYPE_CURRENCY)); // FoxPro
  SqlTypeToDbfType.Add('NUMBER', byte(DBF_TYPE_NUMERIC));
  SqlTypeToDbfType.Add('NUMERIC', byte(DBF_TYPE_NUMERIC));
  SqlTypeToDbfType.Add('FLOAT', byte(DBF_TYPE_FLOAT));
  SqlTypeToDbfType.Add('DATE', byte(DBF_TYPE_DATE));
  SqlTypeToDbfType.Add('TIME', byte(DBF_TYPE_TIME)); // FoxPro
  SqlTypeToDbfType.Add('DATETIME', byte(DBF_TYPE_TIME)); // FoxPro
  SqlTypeToDbfType.Add('TIMESTAMP', byte(DBF_TYPE_TIMESTAMP));
  SqlTypeToDbfType.Add('BINARY', byte(DBF_TYPE_BINARY));
  SqlTypeToDbfType.Add('INT', byte(DBF_TYPE_INTEGER));
  SqlTypeToDbfType.Add('INTEGER', byte(DBF_TYPE_INTEGER));
  SqlTypeToDbfType.Add('LOGICAL', byte(DBF_TYPE_LOGICAL));
  SqlTypeToDbfType.Add('MEMO', byte(DBF_TYPE_MEMO));
  SqlTypeToDbfType.Add('GENERAL', byte(DBF_TYPE_GENERAL));
  SqlTypeToDbfType.Add('PICTURE', byte(DBF_TYPE_PICTURE));
  SqlTypeToDbfType.Add('AUTOINC', byte(DBF_TYPE_AUTOINC));
  SqlTypeToDbfType.Add('DOUBLE', byte(DBF_TYPE_OLEBLOB)); //DBF_TYPE_DOUBLE = 'O'; // dBASE 7, VFP Double is DBF_TYPE_BINARY
  SqlTypeToDbfType.Add('BLOB', byte(DBF_TYPE_BLOB));
  SqlTypeToDbfType.Add('VARBINARY', byte(DBF_TYPE_VARBINARY));
  SqlTypeToDbfType.Add('VARCHAR', byte(DBF_TYPE_VARCHAR));
  SqlTypeToDbfType.Add(VFP_NULLFLAGS_TYPE, byte(DBF_TYPE_NULLFLAGS));

finalization

  SqlTypeToDbfType.Free;

end.
