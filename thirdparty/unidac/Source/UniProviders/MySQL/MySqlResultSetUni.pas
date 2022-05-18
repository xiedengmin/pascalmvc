
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MySqlResultSetUni;

interface

uses
  Variants, SysUtils, CLRClasses, CRTypes,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlBind, MySqlSession;
{$ELSE}
  MyCallUni, MySqlBindUni, MySqlSessionUni;
{$ENDIF}

type
  TMySqlResultSet = class
  protected
    ffieldCount: integer;
    frowBuffer: TValueArr;
    ffields: TMySqlFieldInfos;
    fsession: TMySqlSession;
    feof: boolean;
    FBinds: TMySqlBinds;
    FIsPrepared: boolean;

    FGCHandleData: IntPtr; // frowBuffer
    procedure ClearGCHandleData;

    procedure FetchPreparedFields(var resultBinds: TMySqlBinds);
    procedure FetchFields(var resultBinds: TMySqlBinds);
    procedure ReceiveRows(Count: integer);
    function ReceiveRow: boolean;
    procedure ReadStatus;

  public
    constructor Create(session: TMySqlSession); overload;
    constructor Create(session: TMySqlSession; const fields: TMySqlFieldInfos; fieldCount: integer); overload;
    destructor Destroy; override;

    procedure ClearBuffer;
    procedure SkipRows(Count: integer);
    function Fetch(var resultBinds: TMySqlBinds): boolean;
    function ReadRow(var resultBinds: TMySqlBinds; Index: integer): boolean;
    function ReadFieldsInfo(fieldCount: integer): TMySqlFieldInfos;
    function GCHandleData: IntPtr;

    property Session: TMySqlSession read fsession write fsession;
    property FieldCount: integer read ffieldCount;
    property Fields: TMySqlFieldInfos read ffields;
    property Eof: boolean read feof;
    property IsPrepared: boolean read FIsPrepared write FIsPrepared;
    property Binds: TMySqlBinds read FBinds;
  end;

implementation

uses
  MemUtils, CRFunctions,
{$IFNDEF UNIDACPRO}
  MySqlNet, MySqlErrors;
{$ELSE}
  MySqlNetUni, MySqlErrorsUni;
{$ENDIF}

constructor TMySqlResultSet.Create(session: TMySqlSession);
begin
  Create(session, session.fields, session.fieldCount);
end;

constructor TMySqlResultSet.Create(session: TMySqlSession; const fields: TMySqlFieldInfos; fieldCount: integer);
begin
  inherited Create;
  Assert(session <> nil);
  fsession := session;
  ffields := fields;
  ffieldCount := fieldCount;

  FBinds := TMySqlBinds.Create(fieldCount);
end;

destructor TMySqlResultSet.Destroy;
begin
  FBinds.Free;

  inherited;
end;

procedure TMySqlResultSet.ClearGCHandleData;
begin
  if FGCHandleData = nil then
    Exit;

  FreeGCHandle(FGCHandleData);
  FGCHandleData := nil;
end;

function TMySqlResultSet.GCHandleData: IntPtr;
begin
  Assert(frowBuffer <> nil);
  if FGCHandleData = nil then
    FGCHandleData := AllocGCHandle(frowBuffer, True);
  Result := FGCHandleData;
end;

procedure TMySqlResultSet.ClearBuffer;
begin
  ClearGCHandleData;

  frowBuffer := nil;
  ffields := nil;

  if fsession <> nil then
    fsession.FreeQuery;
  fsession := nil;
end;

procedure TMySqlResultSet.ReadStatus;
var
  net: TMySqlNet;
  OldPosition: integer;
begin
  net := fsession.net;

  if net.Length > 1 then begin // MySQL 4.1 protocol
    OldPosition := net.Position;
    try
      // mysql->warning_count= uint2korr(net->read_pos+1);
      net.Position := OldPosition + 1;
      fsession.warningCount := net.ReadUInt16;

      // mysql->server_status= uint2korr(net->read_pos+3);
      net.Position := OldPosition + 3; /// d5 bug
      fsession.serverStatus := net.ReadUInt16;
    finally
      net.Position := OldPosition;
    end;
  end;
end;

procedure TMySqlResultSet.SkipRows(Count: integer);
begin
  ReceiveRows(Count);
end;

procedure TMySqlResultSet.ReceiveRows(Count: integer);
var
  net: TMySqlNet;
begin
  net := fsession.net;

  net.RowCount := Count + 1; // rows count + Status row
  net.ReceiveRow;
  while (net.Length > 8) or (Byte(net.Row[0]) <> 254) do begin
    Dec(Count);
    net.ReceiveRow;
  end;
  ReadStatus;
  net.SetEof;

  if Count <> 0 then
    raise EMySqlException.Create(CR_UNKNOWN_ERROR);
end;

function TMySqlResultSet.ReceiveRow: boolean; // client.c: read_one_row
var
  net: TMySqlNet;
  row: TValueArr;
begin
  ClearGCHandleData;

  net := fsession.net;
  net.ReceiveRow;
  row := net.Row;
  if (net.Length > 8) or (net.GetPacketType <> ptEOF) then begin
    frowBuffer := row;
    Result := true;
  end
  else begin
    ReadStatus;
    net.SetEof;
    fsession.status := msReady;
    frowBuffer := nil;
    feof := true;
    Result := false;
  end;
end;

procedure TMySqlResultSet.FetchPreparedFields(var resultBinds: TMySqlBinds);
var
  offset, bitMask, nullOfs, length: integer;
  i: integer;
  bind: TMySqlBind;
begin
  resultBinds := FBinds;
  offset := (ffieldCount + 9) div 8 + 1;
  bitMask := 4;
  nullOfs := 1;

  for i := 0 to ffieldCount - 1 do begin
    bind := resultBinds.Items[i];

    if (Byte(frowBuffer[nullOfs]) and bitMask) <> 0 then
      bind.IsNull := true
    else begin
      bind.IsNull := false;

      case ffields[i].MyType of
        FIELD_TYPE_TINY:
          length := 1;
        FIELD_TYPE_SHORT,
        FIELD_TYPE_YEAR:
          length := 2;
        FIELD_TYPE_INT24,
        FIELD_TYPE_LONG:
          length := 4;
        FIELD_TYPE_LONGLONG:
          length := 8;
        FIELD_TYPE_FLOAT:
          length := 4;
        FIELD_TYPE_DOUBLE:
          length := 8;
        else begin
          length := Byte(frowBuffer[offset]);
          Inc(offset);
          case length of
            251: begin
              bind.IsNull := true;
              length := 0;
            end;
            252: begin
              length := BitConverter.ToUInt16(frowBuffer, offset);
              Inc(offset, 2);
            end;
            253: begin
              length := BitConverter.ToInt32(frowBuffer, offset) and $FFFFFF;
              Inc(offset, 3);
            end;
            254: begin
              length := BitConverter.ToInt32(frowBuffer, offset);
              Inc(offset, 8);
            end;
            255:
              raise ArgumentException.Create;
          end;
        end;
      end;

      bind.Length := length;
      bind.Offset := offset;
      Inc(offset, length);
    end;

    bitMask := bitMask shl 1;
    if (bitMask and 255) = 0 then begin
      bitMask := 1;
      Inc(nullOfs);
    end;
  end;
end;

procedure TMySqlResultSet.FetchFields(var resultBinds: TMySqlBinds);
var
  offset, length: integer;
  i: integer;
  bind: TMySqlBind;
begin
  if resultBinds = nil then
    raise EMySqlException.Create(NULL_POINTER);

  offset := 0;
  for i := 0 to ffieldCount - 1 do begin
    bind := resultBinds.Items[i];

    length := Byte(frowBuffer[offset]);
    Inc(offset);

    case length of
      251: begin
        bind.Length := 0;
        bind.Offset := 0;
        bind.IsNull := true;
        continue;
      end;
      252: begin
        length := BitConverter.ToUInt16(frowBuffer, offset);
        Inc(offset, 2);
      end;
      253: begin
        length := BitConverter.ToInt32(frowBuffer, offset) and $FFFFFF;
        Inc(offset, 3);
      end;
      254: begin
        length := BitConverter.ToInt32(frowBuffer, offset);
        Inc(offset, 8);
      end;
      255:
        raise ArgumentException.Create;
    end;

    bind.Length := length;
    bind.Offset := offset;
    bind.IsNull := false;
    Inc(offset, length);
  end;
end;

function TMySqlResultSet.Fetch(var resultBinds: TMySqlBinds): boolean;
begin
  if feof then begin
    Result := False;
    Exit;
  end;

  if ReceiveRow then begin
    if FIsPrepared then
      FetchPreparedFields(resultBinds)
    else
      FetchFields(resultBinds);

    resultBinds.Buffer := frowBuffer;
    Result := True;
  end
  else
    Result := False;
end;

function TMySqlResultSet.ReadRow(var resultBinds: TMySqlBinds; Index: integer): boolean;
begin
  frowBuffer := @fsession.net.Rows[Index][0];

  if FIsPrepared then
    FetchPreparedFields(resultBinds)
  else
    FetchFields(resultBinds);

  resultBinds.Buffer := @frowBuffer[0];
  Result := True;
end;

function TMySqlResultSet.ReadFieldsInfo(fieldCount: integer): TMySqlFieldInfos;
var
  bind: TMySqlBinds;

  // Copied for MySql.Bind
  function GetString(Offset, Length: integer): AnsiString;
  begin
    if Length > 0 then
      Result := Marshal.PtrToStringAnsi(@bind.Buffer[Offset], Length)
    else
      Result := '';
  end;

  function GetByte(Offset: integer): Byte;
  begin
    Result := Byte(bind.Buffer[Offset]);
  end;

  function GetInt16(Offset: integer): Int16;
  begin
    Result := Int16(
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8);
  end;

  function GetInt24(Offset: integer): integer;
  begin
    Result :=
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8 +
      Integer(bind.Buffer[Offset + 2]) shl 16;
  end;

  function GetInt32(Offset: integer): integer;
  begin
    Result :=
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8 +
      Integer(bind.Buffer[Offset + 2]) shl 16 +
      Integer(bind.Buffer[Offset + 3]) shl 24;
  end;

var
  i: integer;
  {catalog, }database, table, orgTable, name, orgName, defaultValue: AnsiString;
  charsetnr: integer;
  length: cardinal;
  _type: TMySqlFieldType;
  flags, scale: integer;
  maxLength: integer;
begin
  ReceiveRows(fieldCount);
  try
    bind := nil;
    try
      SetLength(Result, fieldCount);
      if fsession.protocol41 then begin
        bind := TMySqlBinds.Create(7);
        for i := 0 to fieldCount - 1 do begin
          ReadRow(bind, i);
          // catalog := GetString(bind.Items[0].Offset, bind.Items[0].Length);
          database := GetString(bind.Items[1].Offset, bind.Items[1].Length);
          table := GetString(bind.Items[2].Offset, bind.Items[2].Length);
          orgTable := GetString(bind.Items[3].Offset, bind.Items[3].Length);
          name := GetString(bind.Items[4].Offset, bind.Items[4].Length);
          orgName := GetString(bind.Items[5].Offset, bind.Items[5].Length);
          charsetnr := GetInt16(bind.Items[6].Offset);
          length := cardinal(GetInt32(bind.Items[6].Offset + 2));
          _type := TMySqlFieldType(GetByte(bind.Items[6].Offset + 6));
          flags := GetInt16(bind.Items[6].Offset + 7);
          scale := GetInt16(bind.Items[6].Offset + 9);
          defaultValue := '';
          maxLength := 0;
          Result[i] := MySqlFieldInfo(name, orgName, table, orgTable, database, defaultValue, length, scale, maxLength, flags, charsetnr, _type);
        end;
      end
      else begin
        bind := TMySqlBinds.Create(5);
        for i := 0 to fieldCount - 1 do begin
          ReadRow(bind, i);
          orgTable := GetString(bind.Items[0].Offset, bind.Items[0].Length);
          name := GetString(bind.Items[1].Offset, bind.Items[1].Length);
          length := cardinal(GetInt24(bind.Items[2].Offset));
          _type := TMySqlFieldType(GetByte(bind.Items[3].Offset));
          if (fsession.serverCapabilities and CLIENT_LONG_FLAG) <> 0 then begin
            flags := GetInt16(bind.Items[4].Offset);
            scale := GetByte(bind.Items[4].Offset + 2);
          end
          else begin
            flags := GetByte(bind.Items[4].Offset);
            scale := GetByte(bind.Items[4].Offset + 1);
          end;
          defaultValue := '';
          maxLength := 0;
          Result[i] := MySqlFieldInfo(name, '', orgTable, orgTable, '', defaultValue, length, scale, maxLength, flags, 0, _type);
        end;
      end;
    finally
      bind.Free;
    end;
  finally
    fsession.net.RowCount := 1;
  end;
end;

end.
