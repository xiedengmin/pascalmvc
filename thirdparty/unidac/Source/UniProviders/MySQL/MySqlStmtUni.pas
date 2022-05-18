
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MySqlStmtUni;

interface

uses
  Classes, SysUtils,
  CLRClasses, CRTypes, CRAccess, CRFunctions,
{$IFNDEF UNIDACPRO}
  MySqlSession, MySqlBind, MyCall, MySqlResultSet;
{$ELSE}
  MySqlSessionUni, MySqlBindUni, MyCallUni, MySqlResultSetUni;
{$ENDIF}

type
  TMySqlFieldTypes = array of TMySqlFieldType;

  TMySqlStmt = class
  protected
    Ferrno: cardinal;
    Ferror: string;
    FIsServerError: boolean;

    fsession: TMySqlSession;
    fresult: TMySqlResultSet;
    fstmtId: integer;
    fstate: TPrepStmtState;
    fparamCount: integer;
    ffieldCount: integer;
    faffectedRows: Int64;
    FSendedTypes: TMySqlFieldTypes;

    procedure SetResult(Value: TMySqlResultSet);

  public
    constructor Create(session: TMySqlSession);
    destructor Destroy; override;

    procedure Close;
    procedure FreeResult;
    procedure Prepare(const query: TValueArr; length: integer);
    procedure BindResult;
    procedure Execute(Command: TCRCommand; const Unicode: boolean);
    function Fetch(var binds: TMySqlBinds): integer;
    procedure SendLongData(ParamNumber: cardinal; const Data: PAnsiChar; Count: cardinal);

    property errno: cardinal read Ferrno write Ferrno;
    property error: string read Ferror write Ferror;
    property IsServerError: boolean read FIsServerError write FIsServerError;

    property AffectedRows: Int64 read faffectedRows;
    property Session: TMySqlSession read fsession;
    property Result: TMySqlResultSet read fresult write SetResult;
    property ParamCount: integer read fparamCount;
    property FieldCount: integer read ffieldCount;
  end;

implementation

uses
  Variants, FMTBcd, DateUtils, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  CRTimeStamp, MemData, MemUtils,
{$IFNDEF UNIDACPRO}
  MySqlNet, MySqlErrors, MySqlApi, MyClasses;
{$ELSE}
  MySqlNetUni, MySqlErrorsUni, MySqlApiUni, MyClassesUni;
{$ENDIF}

constructor TMySqlStmt.Create(session: TMySqlSession);
begin
  inherited Create;
  fsession := session;
  fstate := psUnknown;
end;

destructor TMySqlStmt.Destroy;
begin
  fresult.Free;
  fresult := nil;
  Close;
  fsession := nil;
  inherited;
end;

procedure TMySqlStmt.SetResult(Value: TMySqlResultSet);
begin
  if fresult <> nil then
    fresult.Free;
  fresult := Value;
end;

procedure TMySqlStmt.Close;
var
  net: TMySqlNet;
begin
  session.FreeQuery;
  if (fstate <> psUnknown) and (session.net <> nil) then begin
    session.WriteCommand(scCloseStmt);
    net := session.net;
    net.WriteInt32(fstmtId);
    net.Send;
    fstate := psUnknown;
    fstmtId := 0;
  end;
  ffieldCount := 0;
  fparamCount := 0;
  if fresult <> nil then begin
    fresult.ClearBuffer;
    fresult.Free;
    fresult := nil;
  end;
end;

procedure TMySqlStmt.FreeResult;
begin
  if session <> nil then
    session.FreeQuery;
  if fresult <> nil then begin
    fresult.ClearBuffer;
    fresult.Free;
    fresult := nil;
  end;
end;

procedure TMySqlStmt.Prepare(const query: TValueArr; length: integer);
var
  net: TMySqlNet;
  fieldsResult: TMySqlResultSet;
  paramsResult: TMySqlResultSet;
  fields: TMySqlFieldInfos;
begin
  session.SimpleCommand(scPrepare, query, length, true);

  net := session.net;
  net.ReceiveRow;
  if net.ReadByte = 255 then
    raise EMySqlException.Create(CR_SERVER_LOST);
  fstmtId := net.ReadInt32;
  ffieldCount := net.ReadUInt16;
  fparamCount := net.ReadUInt16;
  Result := nil;

  if paramCount > 0 then begin
    paramsResult := TMySqlResultSet.Create(session, nil, 5);
    try
      paramsResult.SkipRows(paramCount);
    finally
      paramsResult.Free;
    end;
  end;

  if ffieldCount <> 0 then begin
    if (session.serverStatus and SERVER_STATUS_AUTOCOMMIT) = 0 then
      session.serverStatus := session.serverStatus and SERVER_STATUS_IN_TRANS;
    //if net.Length > net.Position then           // don't used and generates error
    //  session.extraInfo := net.ReadFieldLength;
    if session.protocol41 then
      fieldsResult := TMySqlResultSet.Create(session, nil, 7)
    else
      fieldsResult := TMySqlResultSet.Create(session, nil, 5);
    try
      SetLength(fields, 0);
      fields := fieldsResult.ReadFieldsInfo(ffieldCount);
      Result := TMySqlResultSet.Create(session, fields, ffieldCount);
    finally
      fieldsResult.Free;
    end;
  end;

  fstate := psPrepare;
end;

procedure TMySqlStmt.BindResult;
begin
  if fstate = psUnknown then
    raise EMySqlException.Create(NO_PREPARE_STMT);

  if (ffieldCount = 0) or (fresult.FieldCount <> ffieldCount) then
    raise EMySqlException.Create(CR_UNKNOWN_ERROR);

  fresult.IsPrepared := True;
end;

{$IFDEF NEXTGEN}
  {$DEFINE USE_WIDESTRING}
{$ENDIF}

procedure TMySqlStmt.Execute(Command: TCRCommand; const Unicode: boolean);
var
  net: TMySqlNet;
{$IFDEF USE_WIDESTRING}
  b: TBytes;
{$ELSE}
  ws: WideString;
{$ENDIF}
  u: AnsiString;
  l: Integer;
  s: string;

  OwnerCommand: TCRCommand;
  OwnerParamsCount: Integer;

  function GetParam(Index: integer; out ItemIndex: integer): TParamDesc;
  begin
    if OwnerCommand = nil then begin
      Result := Command.Params[Index];
      ItemIndex := 0;
    end
    else begin
      Result := OwnerCommand.ParamsInfo[Index mod OwnerParamsCount].ParamRef;
      ItemIndex := TMySQLCommand(Command).BatchOffset + Index div OwnerParamsCount;
    end;
  end;

  procedure SaveParamValue(Param: TParamDesc; ItemIndex: integer);

    procedure ProcessAnsiBlob(Blob: TBlob);
    begin
      {$IFDEF USE_WIDESTRING}
        s := Blob.AsString;
        b := Encoding.UTF8.GetBytes(s);
        l := Length(b);
        net.WriteFieldLength(l);
        net.WriteBytes(b);
      {$ELSE}
        u := Blob.AsAnsiString;
        u := AnsiToUtf8(u);
        l := Length(u);
        net.WriteFieldLength(l);
        net.WriteBytes(PAnsiChar(u), 0, l);
      {$ENDIF}
    end;

    procedure ProcessWideBlob(Blob: TBlob);
    begin
      u := CRFunctions.UTF8Encode(Blob.AsWideString);
      l := Length(u);
      net.WriteFieldLength(l);
      net.WriteBytes(PAnsiChar(u), 0, l);
    end;

    procedure ProcessBlob(Blob: TBlob; InternalType: word);
    var
      Piece: PPieceHeader;
      UseUnicode, IsBytesByRef: boolean;
    begin
      IsBytesByRef := InternalType in [dtBlob, dtBytes, dtVarBytes];
      if IsBytesByRef then
        UseUnicode := False   // Unicode not used in any case. See TMySQLConnection.AppendValueToSQL
      else
        UseUnicode := Unicode;

      if UseUnicode then
        if Blob.IsUnicode then
          ProcessWideBlob(Blob)
        else
          ProcessAnsiBlob(Blob)
      else begin
        if Blob.IsUnicode then
          if IsBytesByRef then
            ProcessWideBlob(Blob)
          else
            ProcessAnsiBlob(Blob)
        else begin
          net.WriteFieldLength(Integer(Blob.Size));
          Piece := Blob.FirstPiece;
          while Piece <> nil do begin
            net.WriteBytes(PAnsiChar(PtrOffset(Piece, sizeof(TPieceHeader))), 0, Piece.Used);
            Piece := Piece.Next;
          end;
        end;
      end;
    end;

  var
    InternalType: word;
    ParamValuePtr: PVariant;
    ParamVarType: TVarType;
    ParamArrPtr: IntPtr;
    lw: cardinal;
    i64: Int64;
    Blob: TBlob;
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  begin
    InternalType := Param.GetDataType;
    ParamValuePtr := Param.GetItemPtr(ItemIndex);
    ParamVarType := TVarData(ParamValuePtr^).VType;

    case InternalType of
      dtBoolean: begin
        //net.WriteFieldLength(sizeof(byte));
        net.WriteByte(Byte(WordBool(Boolean(ParamValuePtr^)))); // Convert to boolean is useful to bypass Delphi bug
      end;
      dtInt8: begin
        //net.WriteFieldLength(sizeof(byte));
        net.WriteByte(Byte(ParamValuePtr^));
      end;
      dtUInt8: begin
        //net.WriteFieldLength(sizeof(smallint));
        net.WriteInt16(Byte(ParamValuePtr^));
      end;
      dtInt16: begin
        //net.WriteFieldLength(sizeof(smallint));
        net.WriteInt16(SmallInt(ParamValuePtr^));
      end;
      dtUInt16: begin
        //net.WriteFieldLength(sizeof(word));
        net.WriteInt32(Word(ParamValuePtr^));
      end;
      dtInt32: begin
        //net.WriteFieldLength(sizeof(integer));
        net.WriteInt32(Integer(ParamValuePtr^));
      end;
      dtUInt32: begin
        //net.WriteFieldLength(sizeof(cardinal));
        lw := ParamValuePtr^;
        net.WriteInt64(lw);
      end;
      dtInt64: begin
        //net.WriteFieldLength(sizeof(Int64));
        i64 := ParamValuePtr^; // Explicit Convert!
        net.WriteInt64(i64);
      end;

      // Float fields
      dtSingle, dtFloat, dtCurrency: begin
        //net.WriteFieldLength(sizeof(double));
        net.WriteInt64(BitConverter.DoubleToInt64Bits(Double(ParamValuePtr^)));
      end;

      // Long fields
      dtBlob, dtBytes, dtVarBytes, // BytesByRef
      dtMemo, dtWideMemo, dtString, dtWideString: begin // CharsByRef
        if ParamVarType = varArray + varByte then begin
          ParamArrPtr := TVarData(ParamValuePtr^).VArray.Data;
          l := TVarData(ParamValuePtr^).VArray.Bounds[0].ElementCount;
          net.WriteFieldLength(l);
          net.WriteBytes(PAnsiChar(ParamArrPtr), 0, l);
        end
        else
        if ParamVarType = varSharedObject then begin
          Assert(TVarData(ParamValuePtr^).VPointer <> nil);
          // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
          Blob := TVarData(ParamValuePtr^).VPointer;
        {$IFDEF HAVE_COMPRESS}
          if Blob is TCompressedBlob then
            TCompressedBlob(Blob).Compressed := False; // may be non-optimal
        {$ENDIF}

          ProcessBlob(Blob, InternalType);
        end
        else
          if Unicode and (InternalType in [dtMemo, dtWideMemo, dtString, dtWideString]) then begin
          {$IFDEF USE_WIDESTRING}
            s := ParamValuePtr^;
            b := Encoding.UTF8.GetBytes(s);
            l := Length(b);
            net.WriteFieldLength(l);
            net.WriteBytes(b);
          {$ELSE}
            if InternalType = dtWideString then begin
              ws := ParamValuePtr^;
              u := CRFunctions.UTF8Encode(ws);
            end
            else begin
              s := ParamValuePtr^;
              u := CRFunctions.AnsiToUTF8(AnsiString(s));
            end;
            l := Length(u);
            net.WriteFieldLength(l);
            net.WriteBytes(PAnsiChar(u), 0, l);
          {$ENDIF}
          end
          else begin
          // CharsByRef Input parameter
          {$IFDEF USE_WIDESTRING}
            s := ParamValuePtr^;
            if InternalType in [dtMemo, dtWideMemo, dtString, dtWideString] then
              l := Length(s)
            else
              l := Integer(Param.GetSize);
            net.WriteFieldLength(l);
            b := Encoding.Default.GetBytes(s);
            net.WriteBytes(b);
          {$ELSE}
            ParamArrPtr := TVarData(ParamValuePtr^).VPointer;
            if (InternalType in [dtMemo, dtWideMemo, dtString, dtWideString]) then
              if ParamArrPtr <> nil then begin
                if (ParamVarType = varOleStr) {$IFDEF VER12P}or (ParamVarType = varUString){$ENDIF} then begin // WideString
                  ParamArrPtr := PAnsiChar(AnsiString(ParamValuePtr^));
                  l := Length(ParamValuePtr^);
                end
                else // Pascal string
                  l := {$IFDEF VER18P}{$IFNDEF NEXTGEN}AnsiStrings.{$ENDIF}{$ENDIF}StrLen(PAChar(ParamArrPtr));
              end
              else
                l := 0
            else
              l := Integer(Param.GetSize);
            net.WriteFieldLength(l);
            net.WriteBytes(PAnsiChar(ParamArrPtr), 0, l);
          {$ENDIF}
          end;
      end;

      // DateTime fields
      dtDate: begin
        net.WriteFieldLength(4);

        DecodeDate(ParamValuePtr^, AYear, AMonth, ADay);
        net.WriteInt16(AYear);
        net.WriteByte(AMonth);
        net.WriteByte(ADay);
      end;

      dtTime: begin
        net.WriteFieldLength(12);

        DecodeTime(ParamValuePtr^, AHour, AMinute, ASecond, AMilliSecond);
        net.WriteByte(0); // MYSQL_TIME.neg
        net.WriteInt32(0);
        net.WriteByte(AHour);
        net.WriteByte(AMinute);
        net.WriteByte(ASecond);
        net.WriteInt32(AMilliSecond * 1000);
      end;

      dtDateTime: begin
        net.WriteFieldLength(11);

        DecodeDateTime(ParamValuePtr^, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        net.WriteInt16(AYear);
        net.WriteByte(AMonth);
        net.WriteByte(ADay);
        net.WriteByte(AHour);
        net.WriteByte(AMinute);
        net.WriteByte(ASecond);
        net.WriteInt32(AMilliSecond * 1000);
      end;


      dtBCD, dtFMTBCD: begin
        s := BCDParamDescToStr(ParamValuePtr^, InternalType);
        l := Length(s);
        net.WriteFieldLength(l);
        net.WriteBytes(PAnsiChar(AnsiString(s)), 0, l);
      end;
      else
        Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
    end;
  end;

var
  i, j: integer;
  nilCount, nilOfs: integer;
  Param: TParamDesc;
  ParamOffset: Integer;
  ItemIndex: Integer;
  sendTypes: boolean;
  TypesToSend: TMySqlFieldTypes;
begin
  OwnerCommand := TMySQLCommand(Command).BatchOwner;

  if OwnerCommand <> nil then
    OwnerParamsCount := OwnerCommand.ParamsInfo.Count;

  if (Command.Params.Count > 0) and (Command.Params[0].GetParamType = pdResult) then
    ParamOffset := 1
  else
    ParamOffset := 0;

  session.WriteCommand(scExecute);
  net := session.net;
  net.WriteInt32(fstmtId);

  net.WriteByte(0); // no flags
  net.WriteInt32(1); // iteration count

  if (fparamCount <> 0) and (fparamCount = Command.Params.Count - ParamOffset) then begin
    SetLength(TypesToSend, fparamCount);

    // Process Param.IsNull
    nilCount := (fparamCount + 7) div 8;
    nilOfs := net.Position;
    net.Fill(0, nilCount);
    for i := 0 to fparamCount - 1 do begin
      Param := GetParam(i + ParamOffset, ItemIndex);

      if Param.ItemNull[ItemIndex] then begin
        j := nilOfs + i div 8;
        Byte(net.Row[j]) := Byte(net.Row[j]) or Byte(1 shl (i and 7));
      end;

      TypesToSend[i] := ConvertInternalTypeMySQLFormat(Param.GetDataType);
    end;

    sendTypes := Length(TypesToSend) <> Length(FSendedTypes);
    if not sendTypes then
      for i := 0 to fparamCount - 1 do
        if TypesToSend[i] <> FSendedTypes[i] then begin
          sendTypes := True;
          Break;
        end;

    net.WriteBool(sendTypes);
    if sendTypes then begin
      for i := 0 to fparamCount - 1 do
        net.WriteInt16(smallint(TypesToSend[i]));
      FSendedTypes := TypesToSend;
    end;

    // Process parameter values
    for i := 0 to fparamCount - 1 do begin
      Param := GetParam(i + ParamOffset, ItemIndex);
      if not Param.ItemNull[ItemIndex] then
        SaveParamValue(Param, ItemIndex);
    end;
  end;
  net.Send;
  session.ReadQueryResult;
  if (session.fieldCount <> 0) and (session.fields <> nil) then begin
    ffieldCount := session.fieldCount;
    Result := TMySqlResultSet.Create(session, session.fields, session.fieldCount);
    session.fieldCount := 0;
    SetLength(fsession.fields, 0);
  end;
  if ffieldCount > 0 then
    session.status := msUseResult;
  faffectedRows := session.affectedRows;
end;

function TMySqlStmt.Fetch(var binds: TMySqlBinds): integer;
var
  row0: byte;
begin
  binds := nil;
  if fresult.Fetch(binds) then begin
    Result := 0;
    Exit;
  end
  else begin
    Result := MYSQL_NO_DATA;

    // On executing of prepared StoredProc it is necessary to skip the last data block
    if session.SkipPacket and (session.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0) then begin
      session.net.SkipRow(row0);
      session.SkipPacket := False;
    end;
  end;
end;

procedure TMySqlStmt.SendLongData(ParamNumber: cardinal; const Data: PAnsiChar; Count: cardinal);
var
  net: TMySqlNet;
begin
  net := session.net;
  session.WriteCommand(scLongData);
  net.WriteInt32(fstmtId);
  net.WriteInt16(ParamNumber);
  net.WriteBytes(Data, 0, Count);
  net.Send;
end;

end.
