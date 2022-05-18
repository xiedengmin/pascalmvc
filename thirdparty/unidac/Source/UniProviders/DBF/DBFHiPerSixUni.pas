
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFHiPerSixUni;

interface

{$IFDEF DBFENGINE}

uses
  SysUtils, Classes, Variants, DateUtils,
{$IFDEF LOG_PACKETS}
  LogHandler,
{$ENDIF}
  FMTBcd,
  CRTypes, CRFunctions, CRAccess, MemData,
  LiteClassesVirtual,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs, DBFMemos, DBFIndexes, DBFDBase;
{$ELSE}
  DBFConstsUni, DBFStructsUni, DBFMemosUni, DBFIndexesUni, DBFDBaseUni;
{$ENDIF}

type
  TDBFHiPerSix = class(TDBFDBase)
  protected
    procedure WriteMemoIndex(FieldNo, IdxOffset, Len: integer); override;
    procedure SetHasMemo(Value: boolean); override;
    procedure InternalOpenIndexFile; override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    class function GetMemoClass: TDBFMemoClass; override;
    class function GetIndexClass: TDBFIndexClass; override;
    class function GetMemoExt: string; override;
    class function GetIndexExt: string; override;

    function GetFieldNull(FieldNo: integer): boolean; override;
    function GetFieldValue(FieldNo: integer): variant; override;
  end;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

{ TDBFHiPerSix }

constructor TDBFHiPerSix.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfHiPerSix;
end;

class function TDBFHiPerSix.GetMemoClass: TDBFMemoClass;
begin
  Result := TDBFHiPerSixMemo;
end;

class function TDBFHiPerSix.GetIndexClass: TDBFIndexClass;
begin
  Result := inherited GetIndexClass; // SMT not yet implemented
end;

class function TDBFHiPerSix.GetMemoExt: string;
begin
  Result := SMT_EXT;
end;

class function TDBFHiPerSix.GetIndexExt: string;
begin
  Result := ''; // SNX, todo?
end;

procedure TDBFHiPerSix.WriteMemoIndex(FieldNo, IdxOffset, Len: integer);
var
  pLink: PSMTMemoLink;
begin
  pLink := @FCurrentRecordBuffer[FOffsets[FieldNo]];
  if Len <= 0 then
    FillChar(pLink^, SizeOf(TSMTMemoLink), $20)
  else begin
    pLink.Signature := 1;
    pLink.Len := Len;
    pLink.Offset := IdxOffset;
  end;
end;

procedure TDBFHiPerSix.SetHasMemo(Value: boolean);
begin
  if Value then
    FHeader.DB3.version := FHeader.DB3.version or DBF_HAS_DBT or DBF_DBASE4_DBT //?
  else
    FHeader.DB3.version := FHeader.DB3.version and not(DBF_HAS_DBT or DBF_DBASE4_DBT); //?
end;

procedure TDBFHiPerSix.InternalOpenIndexFile;
begin
  { todo NSX? }
end;

function TDBFHiPerSix.GetFieldNull(FieldNo: integer): boolean;
begin
  if GetFieldType(FieldNo) = DBF_TYPE_MEMO then
    Result := PSMTMemoLink(@FCurrentRecordBuffer[FOffsets[FieldNo]]).Signature <> 1 // = $2021
  else
    Result := inherited GetFieldNull(FieldNo);
end;

function TDBFHiPerSix.GetFieldValue(FieldNo: integer): variant;
var
  fType: AnsiChar;
  MemoIdx, len: integer;
  Blob: TBlob;
  pSmtLink: PSMTMemoLink;
begin
  Result := Unassigned;

  fType := GetFieldType(FieldNo);

  case fType of
    // todo: more types?
    DBF_TYPE_MEMO: begin
      len := 0;
      Assert(FLengths[FieldNo] = 10);
      pSmtLink := @FCurrentRecordBuffer[FOffsets[FieldNo]];
      if pSmtLink.Signature <> 1 then // $01 00 exists, $20 20 if empty, other?
        MemoIdx := 0
      else begin
        MemoIdx := pSmtLink.Offset;
        len := pSmtLink.Len;
      end;

      if MemoIdx > 0 then begin
        Blob := TBlob.Create;
        TVarData(Result).VType := varSharedObject;
        TVarData(Result).VPointer := Blob;
        FMemo.Read(Blob, MemoIdx, len);
      end;
    end;
  else
    Result := inherited GetFieldValue(FieldNo);
  end;
end;

{$ENDIF}

end.
