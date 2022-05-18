
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEServicesUni;

interface

uses
  SysUtils, Classes, Variants, DB, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CRTypes, CRParser, MemData,
  DBAccess, DAScript, {$IFNDEF FPC}MemDS,{$ELSE}MemDataSet,{$ENDIF}
  {$IFNDEF STD}DADump,{$ENDIF}
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCServices,{$ENDIF}
  {$IFNDEF STD}SqlClasses,{$ENDIF}
  ASEParser, ASEConnection, ASESQLGenerator;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCServicesUni,{$ENDIF}
  {$IFNDEF STD}SqlClassesUni,{$ENDIF}
  ASEParserUni, ASEConnectionUni, ASESQLGeneratorUni;
{$ENDIF}

type

{  TCustomASEDataSetUpdater }

  TCustomASEDataSetUpdater = class({$IFDEF ODBC_PROVIDER}TCustomODBCDataSetUpdater{$ELSE}TDADataSetUpdater{$ENDIF})
  protected
    function GetSQLSeparator: string; override;
    function RefreshByLockAllowed: boolean; override;
    function SavepointAllowed: boolean; override;
    function BatchUpdateAllowed: boolean; override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomASEDataSetService }

  TCustomASEDataSetService = class({$IFDEF ODBC_PROVIDER}TCustomODBCDataSetService{$ELSE}TDADataSetService{$ENDIF})
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
  {$IFDEF ODBC_PROVIDER}
    function KeysFromSpecialColumns: boolean; override;
  {$ENDIF}

    function DetectCanModify: boolean; override;
    function GetRecCount: integer; override;
  public
    function OpenNext: boolean; override;
    function GetDBKeyList(const TableName, IndexName: string): string; override;
  end;

{$IFNDEF ODBC_PROVIDER}

{ TCustomASEFieldTypeMap }

  TCustomASEFieldTypeMap = class(TDAFieldTypeMap)
  public
    class function GetDataType(FieldType: TFieldType; SubDataType: Word = 0): integer; override;
  end;

{$ENDIF}

{ TASEScriptProcessor }

  TASEScriptProcessor = class ({$IFDEF ODBC_PROVIDER}TODBCScriptProcessor{$ELSE}TDAScriptProcessor{$ENDIF})
  private
    FPrevLexem: integer;

  protected
    function ExecuteNext: boolean; override;
    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean); override;
    function GetReady(Code: integer): boolean; override;
  end;

{$IFNDEF STD}

{ TCustomASEDumpProcessor }

  TCustomASEDumpProcessor = class(TDADumpProcessor)
  protected
    function CreateQuery: TCustomDADataSet; override;
    function GetFieldValueForDump(Field: TField): string; override;
  end;

{$ENDIF}

implementation

uses
  CRProps, CRFunctions, CRAccess,
  DAConsts, DASQLMonitor;

{ TCustomASEDataSetUpdater }

function TCustomASEDataSetUpdater.GetSQLSeparator: string;
begin
  Result := '';
end;

function TCustomASEDataSetUpdater.RefreshByLockAllowed: boolean;
var
  Connection: TASEConnection;
begin
  Connection := TASEConnection(TDBAccessUtils.GetIConnection(UsedConnection));

  if Connection.Direct and
     (Connection.GetConnector <> nil) and
     ((Connection.GetConnector.ServerMajorVersion >= 16) or
      (Connection.GetConnector.ServerMajorVersion = 15) and (Connection.GetConnector.ServerMinorVersion >= 7))
  then
    Result := True
  else
    Result := inherited RefreshByLockAllowed;
end;

function TCustomASEDataSetUpdater.SavepointAllowed: boolean;
var
  Connection: TASEConnection;
begin
  Connection := TASEConnection(TDBAccessUtils.GetIConnection(UsedConnection));

  if Connection.Direct then
    Result := True
  else
    Result := inherited SavepointAllowed;
end;

function TCustomASEDataSetUpdater.BatchUpdateAllowed: boolean;
var
  Connection: TASEConnection;
begin
  Connection := TASEConnection(TDBAccessUtils.GetIConnection(UsedConnection));

  if Connection.Direct then
    Result := True
  else
    Result := inherited BatchUpdateAllowed;
end;

function TCustomASEDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT @@IDENTITY');
  Result := True;
end;

{ TCustomASEDataSetService }

procedure TCustomASEDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomASEDataSetUpdater.Create(Self));
end;

procedure TCustomASEDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomASESQLGenerator.Create(TDASQLGeneratorService));
end;

{$IFDEF ODBC_PROVIDER}

function TCustomASEDataSetService.KeysFromSpecialColumns: boolean;
begin
  Result := True;
end;

{$ENDIF}

function TCustomASEDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TCustomASEDataSetService.GetRecCount: integer;
var
  St: string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
begin
  Result := 0;
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TASEParser);
  St := 'SELECT count(*) FROM (' + DALineSeparator + St + DALineSeparator + ') t';

  TCustomASEDataSetUpdater(Updater).CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(TCustomASEDataSetUpdater(Updater).UpdateQuery);
  UpdateQuery.SQL.Text := St;
  UpdateQuery.Params.Assign(FDataSet.Params);

  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

  UpdateQuery.Open;
  if not UpdateQuery.EOF then
    Result := UpdateQuery.Fields[0].AsInteger;

  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
end;

function TCustomASEDataSetService.OpenNext: boolean;
var
  Connection: TASEConnection;
begin
  Connection := TASEConnection(TDBAccessUtils.GetIConnection(UsedConnection));

  if not Connection.Direct then
    Result := inherited OpenNext
{$IFNDEF STD}
  else if not FDataSet.Active then begin
    FDataSet.Open;
    Result := True;
  end
  else begin
    GetIRecordSet.SetProp(prOpenNext, True);
    TDBAccessUtils.SetLockDebug(FDataSet, True);
    try
      FDataSet.Close;

      Result := TSqlRecordSet(GetIRecordSet).CheckNextResult;

      if Result then begin
        FDataSet.FieldDefs.Updated := False;
        FDataSet.Open;
      end;
    finally
      TDBAccessUtils.SetLockDebug(FDataSet, False);
      GetIRecordSet.SetProp(prOpenNext, False);
    end;
  end;
{$ELSE}
  else
    Result := False;
{$ENDIF}
end;

function TCustomASEDataSetService.GetDBKeyList(const TableName, IndexName: string): string;
const
  fmtTableKeySQL = 'EXEC %ssp_odbc_getversioncolumns %s,%s,%s,''R'',''C'',''U'',3';
var
  SQL: string;
  db: string;
  Info: TSQLObjectInfo;
  Connection: TASEConnection;
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  FieldDesc: TFieldDesc;
  v: Variant;
begin
  Connection := TASEConnection(TDBAccessUtils.GetIConnection(UsedConnection));

  if Connection.Direct and (IndexName = '') then begin
    BeginConnection; // GetDBKeyList requires an active connection
    try
      RecordSet := Connection.GetRecordSet;
      try
        Connection.SQLInfo.SplitObjectName(TableName, Info);

        if Info.Catalog = '' then
          Info.Catalog := Connection.Database;
        if Info.Catalog <> '' then begin
          db := '[' + Info.Catalog + '].dbo.';
          Info.Catalog := 'N''' + Info.Catalog + '''';
        end
        else begin
          db := '';
          Info.Catalog := 'NULL';
        end;

        if Info.Schema = '' then
          Info.Schema := GetCurrentSchema;
        if Info.Schema <> '' then
          Info.Schema := 'N''' + Info.Schema + ''''
        else
          Info.Schema := 'NULL';

        Info.Name := 'N''' + Info.Name + '''';

        SQL := Format(fmtTableKeySQL, [db, Info.Name, Info.Schema, Info.Catalog]);

        RecordSet.SetSQL(SQL);
        RecordSet.Open;
        RecordSet.FetchAll;
        RecordSet.SetToBegin;

        FieldDesc := RecordSet.Fields[1];
        RecordSet.AllocRecBuf(RecBuf);
        try
          Result := '';
          while True do begin
            RecordSet.GetNextRecord(RecBuf);
            if RecordSet.Eof then
              Break;

            RecordSet.GetFieldAsVariant(FieldDesc, RecBuf, v);
            if Result <> '' then
              Result := Result + ',';
            Result := Result + VarToStr(v);
          end;
        finally
          RecordSet.FreeRecBuf(RecBuf);
        end;
      finally
        Connection.ReleaseRecordSet(RecordSet);
      end;
    finally
      EndConnection;
    end
  end
  else
    Result := inherited GetDBKeyList(TableName, IndexName);
end;

{$IFNDEF ODBC_PROVIDER}

{ TCustomASEFieldTypeMap }

class function TCustomASEFieldTypeMap.GetDataType(FieldType: TFieldType; SubDataType: Word = 0): integer;
begin
  if FieldType = ftFixedChar then
    Result := dtFixedChar
  else if Integer(FieldType) = Integer(ftFixedWideChar) then
    Result := dtFixedWideChar
  else
    Result := inherited GetDataType(FieldType, SubDataType);
end;

{$ENDIF}

{ TASEScriptProcessor }

function TASEScriptProcessor.ExecuteNext: boolean;
begin
  FPrevLexem := 0;

  Result := inherited ExecuteNext;
end;

function TASEScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

procedure TASEScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if (FPrevLexem = lxBEGIN) and (Code <> lxTRANSACTION) then
    StatementType := ST_SPECIFIC_SQL;

  FPrevLexem := Code;
end;

function TASEScriptProcessor.GetReady(Code: integer): boolean;
begin
  Result := Code = lxGO;
end;

{$IFNDEF STD}

{ TCustomASEDumpProcessor }

function TCustomASEDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetICommand(Result).SetProp(prEnableBCD, True);
  TDBAccessUtils.GetICommand(Result).SetProp(prEnableFmtBCD, True);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

function TCustomASEDumpProcessor.GetFieldValueForDump(Field: TField): string;
var
  i: integer;
  dt: TDateTime;
  hour, minutes, seconds, msec: Word;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
  fmt: string;
{$ELSE}
  s: string;
{$ENDIF}
{$IFDEF VER12P}
  b: TBytes;
{$ELSE}
  v: Variant;
{$ENDIF}
begin
  if Field.IsNull then
    Result := 'NULL'
  else begin
    case Field.DataType of
      ftBytes, ftVarBytes: begin  // ftBlob ?
      {$IFDEF VER12P}
        b := Field.AsBytes;
        if Length(b) > 0 then begin
          Result := '0x';
          // SetLength(Result, TVarData(v).VArray.Bounds[0].ElementCount + 2); optimize later if needed
          for i := 0 to High(b) do
            Result := Result + IntToHex(b[i], 2);
        end
      {$ELSE}
        v := Field.Value;
        Assert(TVarData(v).VType = varArray + varByte, 'GetFieldValueForDump: ftBytes not an array');
        if TVarData(v).VArray.Bounds[0].ElementCount > 0 then begin
          Result := '0x';
          // SetLength(Result, TVarData(v).VArray.Bounds[0].ElementCount + 2); optimize later if needed
          for i := 0 to TVarData(v).VArray.Bounds[0].ElementCount - 1 do
            Result := Result + IntToHex(PByteArray(TVarData(v).VArray.Data)[i], 2);
        end
      {$ENDIF}
        else
          Result := '''''';
      end;
      ftTime: begin
        dt := Field.AsDateTime;
        DecodeTime(dt, hour, minutes, seconds, msec);
      {$IFDEF VER7P}
        FmtSet.TimeSeparator := ':';
        FmtSet.DecimalSeparator := '.';
        fmt := 'hh:nn:ss';
        if msec <> 0 then
          fmt := fmt + '.z';
        Result := '''' + FormatDateTime(fmt, dt, FmtSet) + '''';
      {$ELSE}
        Result := FormatDateTime('hh:nn:ss', dt);
        if TimeSeparator <> ':' then
          Result := StringReplace(Result, TimeSeparator, ':', [rfReplaceAll]);
        if msec <> 0 then
          Result := Result + '.' + IntToStr(msec);
        Result := '''' + Result + '''';
      {$ENDIF}
      end;
      ftDateTime: begin
        dt := Field.AsDateTime;
        DecodeTime(dt, hour, minutes, seconds, msec);
      {$IFDEF VER7P}
        FmtSet.DateSeparator := '-';
        FmtSet.TimeSeparator := ':';
        FmtSet.DecimalSeparator := '.';
        fmt := 'yyyy-mm-dd hh:nn:ss';
        if msec <> 0 then
          fmt := fmt + '.z';
        Result := '''' + FormatDateTime(fmt, dt, FmtSet) + '''';
      {$ELSE}
        Result := FormatDateTime('yyyy-mm-dd', dt);
        if DateSeparator <> '-' then
          Result := StringReplace(Result, DateSeparator, '-', [rfReplaceAll]);
        s := FormatDateTime('hh:nn:ss', dt);
        if TimeSeparator <> ':' then
          s := StringReplace(s, TimeSeparator, ':', [rfReplaceAll]);
        if msec <> 0 then
          s := s + '.' + IntToStr(msec);
        Result := '''' + Result + ' ' + s + '''';
      {$ENDIF}
      end;
    else
      Result := inherited GetFieldValueForDump(Field);
    end;
  end;
end;

{$ENDIF}

end.
