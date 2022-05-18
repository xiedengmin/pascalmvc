
//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I NexusDac.inc}
unit NexusDataTypeMapUni;
{$ENDIF}

interface

uses
  CRDataTypeMap{$IFNDEF DUMMY}, nxsdTypes{$ENDIF},
  MemData;

const
  nxBase           = 900;

  nxBoolean       = nxBase + 1;
  nxChar          = nxBase + 2;
  nxWideChar      = nxBase + 3;
  nxByte          = nxBase + 4;
  nxWord16        = nxBase + 5;
  nxWord32        = nxBase + 6;
  nxInt8          = nxBase + 7;
  nxInt16         = nxBase + 8;
  nxInt32         = nxBase + 9;
  nxInt64         = nxBase + 10;
  nxAutoInc       = nxBase + 11;
  nxSingle        = nxBase + 12;
  nxDouble        = nxBase + 13;
  nxExtended      = nxBase + 14;
  nxCurrency      = nxBase + 15;
  nxDate          = nxBase + 16;
  nxTime          = nxBase + 17;
  nxDateTime      = nxBase + 18;
  nxInterval      = nxBase + 19;
  nxBlob          = nxBase + 20;
  nxBlobMemo      = nxBase + 21;
  nxBlobGraphic   = nxBase + 22;
  nxByteArray     = nxBase + 23;
  nxShortString   = nxBase + 24;
  nxNullString    = nxBase + 25;
  nxWideString    = nxBase + 26;
  nxRecRev        = nxBase + 27;
  nxGuid          = nxBase + 28;
  nxBCD           = nxBase + 29;
  nxBlobWideMemo  = nxBase + 30;
  nxFmtBCD        = nxBase + 31;

type
  TNexusMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TNexusConverterManager = class(TConverterManager)
  public
    constructor Create;

    class function GetDBProvider: Word; override;
  {$IFNDEF DUMMY}
    class function GetDBType(nxFieldType: TnxFieldType): Word;
  {$ENDIF}
  end;

implementation

var
  NxConverterManager: TNexusConverterManager;

{ Functions }

procedure InitNexusTypes;
begin
  DBTypeInfos.Add(nxBoolean,       'Boolean',      False, False);
  DBTypeInfos.Add(nxChar,          'Char',         True,  False);
  DBTypeInfos.Add(nxWideChar,      'WideChar',     True,  False);
  DBTypeInfos.Add(nxByte,          'Byte',         False, False);
  DBTypeInfos.Add(nxWord16,        'Word16',       False, False);
  DBTypeInfos.Add(nxWord32,        'Word32',       False, False);
  DBTypeInfos.Add(nxInt8,          'Int8',         False, False);
  DBTypeInfos.Add(nxInt16,         'Int16',        False, False);
  DBTypeInfos.Add(nxInt32,         'Int32',        False, False);
  DBTypeInfos.Add(nxInt64,         'Int64',        False, False);
  DBTypeInfos.Add(nxAutoInc,       'AutoInc',      False, False);
  DBTypeInfos.Add(nxSingle,        'Single',       False, False);
  DBTypeInfos.Add(nxDouble,        'Double',       False, False);
  DBTypeInfos.Add(nxExtended,      'Extended',     False, False);
  DBTypeInfos.Add(nxCurrency,      'Currency',     False, False);
  DBTypeInfos.Add(nxDate,          'Date',         False, False);
  DBTypeInfos.Add(nxTime,          'Time',         False, False);
  DBTypeInfos.Add(nxDateTime,      'DateTime',     False, False);
  DBTypeInfos.Add(nxInterval,      'Interval',     False, False);
  DBTypeInfos.Add(nxBlob,          'Blob',         True,  False);
  DBTypeInfos.Add(nxBlobMemo,      'Memo',         True,  False);
  DBTypeInfos.Add(nxBlobGraphic,   'Graphic',      True,  False);
  DBTypeInfos.Add(nxByteArray,     'ByteArray',    True,  False);
  DBTypeInfos.Add(nxShortString,   'ShortString',  True,  False);
  DBTypeInfos.Add(nxNullString,    'NullString',   True,  False);
  DBTypeInfos.Add(nxWideString,    'WideString',   True,  False);
  DBTypeInfos.Add(nxRecRev,        'RecRev',       False, False);
  DBTypeInfos.Add(nxGuid,          'Guid',         False, False);
  DBTypeInfos.Add(nxBCD,           'BCD',          True,  True);
  DBTypeInfos.Add(nxBlobWideMemo,  'WideMemo',     True,  False);
  DBTypeInfos.Add(nxFmtBCD,        'FmtBCD',       True,  True);
end;

{ TNexusMapRules }

class function TNexusMapRules.GetConverterManager: TConverterManager;
begin
  Result := NxConverterManager;
end;

{ TNexusConverterManager }

constructor TNexusConverterManager.Create;
begin
  inherited;

  AddFetchConverter(nxByte, dtSmallInt);
  AddFetchConverter(nxWord16, dtInteger);
  AddFetchConverter(nxWord32, dtInt64);

  AddFetchConverter(nxAutoInc, dtInteger);
  AddFetchConverter(nxRecRev, dtInteger);
  AddFetchConverter(nxAutoInc, dtInt64);
  AddFetchConverter(nxRecRev, dtInt64);
end;

class function TNexusConverterManager.GetDBProvider: Word;
begin
  Result := nxBase;
end;

{$IFNDEF DUMMY}
class function TNexusConverterManager.GetDBType(nxFieldType: TnxFieldType): Word;
begin
  case nxFieldType of
    nxtBoolean:
      Result := nxBoolean;
    nxtChar:
      Result := nxChar;
    nxtWideChar:
      Result := nxWideChar;
    nxtByte:
      Result := nxByte;
    nxtWord16:
      Result := nxWord16;
    nxtWord32:
      Result := nxWord32;
    nxtInt8:
      Result := nxInt8;
    nxtInt16:
      Result := nxInt16;
    nxtInt32:
      Result := nxInt32;
    nxtInt64:
      Result := nxInt64;
    nxtAutoInc:
      Result := nxAutoInc;
    nxtSingle:
      Result := nxSingle;
    nxtDouble:
      Result := nxDouble;
    nxtExtended:
      Result := nxExtended;
    nxtCurrency:
      Result := nxCurrency;
    nxtDate:
      Result := nxDate;
    nxtTime:
      Result := nxTime;
    nxtDateTime:
      Result := nxDateTime;
    nxtInterval:
      Result := nxInterval;
    nxtBlob:
      Result := nxBlob;
    nxtBlobMemo:
      Result := nxBlobMemo;
    nxtBlobGraphic:
      Result := nxBlobGraphic;
    nxtByteArray:
      Result := nxByteArray;
    nxtShortString:
      Result := nxShortString;
    nxtNullString:
      Result := nxNullString;
    nxtWideString:
      Result := nxWideString;
    nxtRecRev:
      Result := nxRecRev;
    nxtGuid:
      Result := nxGuid;
    nxtBCD:
      Result := nxBCD;
    nxtBlobWideMemo:
      Result := nxBlobWideMemo;
    nxtFmtBCD:
      Result := nxFmtBCD;
  else
    Result := 0;
  end;
end;
{$ENDIF}

initialization
  InitNexusTypes;
  NxConverterManager := TNexusConverterManager.Create;

finalization
  NxConverterManager.Free;

end.
