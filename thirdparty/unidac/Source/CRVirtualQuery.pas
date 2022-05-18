
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit CRVirtualQuery;

interface

uses
  SysUtils, Classes,
{$IFDEF DEBUG_OUTPUT}
  Windows,
{$ENDIF}
  CRTypes, CRParser, CRAccess, CRDataTypeMap, MemData,
  CRVirtualData,
  LiteCallVirtual, LiteClassesVirtual;

type
  PIntPtr = ^IntPtr;
  TSQLiteArgs = array of string;

  TTableCreationMode = (cmAll, cmOnDemand);

  TSQLiteModule = class;
  TSQLiteVirtualTable = class;
  TSQLiteVirtualTableCursor = class;
  TCRVirtualConnection = class;
  TCRVirtualCommand = class;

  psqlite3_module = ^sqlite3_module;
  psqlite3_vtab = ^sqlite3_vtab;
  psqlite3_vtab_cursor = ^sqlite3_vtab_cursor;
  psqlite3_index_constraint = ^sqlite3_index_constraint;
  psqlite3_index_info = ^sqlite3_index_info;
  psqlite3_index_orderby = ^sqlite3_index_orderby;
  psqlite3_index_constraint_usage = ^sqlite3_index_constraint_usage;
  psqlite3_value = IntPtr;
  ppsqlite3_value = ^psqlite3_value;

  Tsqlite3_func = procedure(pContext: pSQLite3Context; nargs: integer; args: ppsqlite3_value); cdecl;

  sqlite3_module = record
    iVersion: integer;
    xCreate: function(pSQLite: pSQLite3; pAux: IntPtr; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer; cdecl;
    xConnect: function(pSQLite: pSQLite3; pAux: IntPtr; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer; cdecl;
    xBestIndex: function(pVTab: psqlite3_vtab; pIndexInfo: psqlite3_index_info): integer; cdecl;
    xDisconnect: function(pVTab: psqlite3_vtab): integer; cdecl;
    xDestroy: function(pVTab: psqlite3_vtab): integer; cdecl;
    xOpen: function(pVTab: psqlite3_vtab; var ppCursor: psqlite3_vtab_cursor): integer; cdecl;
    xClose: function(pCursor: psqlite3_vtab_cursor): integer; cdecl;
    xFilter: function(pCursor: psqlite3_vtab_cursor; idxNum: integer; idxStr: IntPtr; argc: integer; argv: ppsqlite3_value): integer; cdecl;
    xNext: function(pCursor: psqlite3_vtab_cursor): integer; cdecl;
    xEof: function(pCursor: psqlite3_vtab_cursor): integer; cdecl;
    xColumn: function(pCursor: psqlite3_vtab_cursor; pContext: pSQLite3Context; index: integer): integer; cdecl;
    xRowid: function(pCursor: psqlite3_vtab_cursor; var pRowid: Int64): integer; cdecl;
    xUpdate: function(pVTab: psqlite3_vtab; argc: Integer; var argv: ppsqlite3_value; var pRowid: Int64): integer; cdecl;
    xBegin: function(pVTab: psqlite3_vtab): integer; cdecl;
    xSync: function(pVTab: psqlite3_vtab): integer; cdecl;
    xCommit: function(pVTab: psqlite3_vtab): integer; cdecl;
    xRollback: function(pVTab: psqlite3_vtab): integer; cdecl;
    xFindFunction: function(pVTab: psqlite3_vtab; nArg: integer; zName: IntPtr; var pxFunc: Tsqlite3_func; var ppArg: IntPtr): integer; cdecl;
    xRename: function(pVTab: psqlite3_vtab; zNew: IntPtr): integer; cdecl;
    // The methods above are in version 1 of the sqlite_module object.
    // Those below are for version 2 and greater.
    xSavepoint: function(pVTab: psqlite3_vtab; n: integer): integer; cdecl;
    xRelease: function(pVTab: psqlite3_vtab; r: integer): integer; cdecl;
    xRollbackTo: function(pVTab: psqlite3_vtab; r: integer): integer; cdecl;
  end;

  sqlite3_vtab = record
    pModule: psqlite3_module; // The module for this virtual table */
    nRef: integer;            // Number of open cursors */
    zErrMsg: IntPtr;          // Error message from sqlite3_mprintf() */
    // Virtual table implementations will typically add additional fields
    pSQLiteVirtualTable: TSQLiteVirtualTable;
  end;

  sqlite3_vtab_cursor = record
    pVtab: psqlite3_vtab;      // Virtual table of this cursor */
    // Virtual table implementations will typically add additional fields
    pSQLiteVirtualTableCursor: TSQLiteVirtualTableCursor;
  end;

  sqlite3_index_constraint = record
    iColumn: integer;     // Column on left-hand side of constraint */
    op: byte;             // Constraint operator */
    usable: byte;         // True if this constraint is usable */
    iTermOffset: integer; // Used internally - xBestIndex should ignore */
  end;

  sqlite3_index_orderby = record
    iColumn: integer; // Column number */
    desc: byte;       // True for DESC.  False for ASC. */
  end;

  sqlite3_index_constraint_usage = record
    argvIndex: integer; // if >0, constraint is part of argv to xFilter */
    omit: byte;         // Do not code a test for this constraint */
  end;

  sqlite3_index_info = record
    // Inputs */
    nConstraint: integer;                              // Number of entries in aConstraint */
    aConstraint: psqlite3_index_constraint;            // Table of WHERE clause constraints */
    nOrderBy: integer;                                 // Number of terms in the ORDER BY clause */
    aOrderBy: psqlite3_index_orderby;                  // The ORDER BY clause */
    // Outputs */
    aConstraintUsage: psqlite3_index_constraint_usage;
    idxNum: integer;                                   // Number used to identify the index */
    idxStr: IntPtr;                                    // String, possibly obtained from sqlite3_malloc */
    needToFreeIdxStr: integer;                         // Free idxStr using sqlite3_free() if true */
    orderByConsumed: integer;                          // True if output is already ordered */
    estimatedCost: Double;                             // Estimated cost of using this index */
    // Fields below are only available in SQLite 3.8.2 and later */
    estimatedRows: Int64;                              // Estimated number of rows returned */
    // Fields below are only available in SQLite 3.9.0 and later */
    idxFlags: integer;                                 // Mask of SQLITE_INDEX_SCAN_* flags */
    // Fields below are only available in SQLite 3.10.0 and later */
    colUsed: UInt64;                                   // Input: Mask of columns used by statement */
  end;

  TSQLiteVirtualTableCursor = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FTable: TSQLiteVirtualTable;
    FInternalCursor: sqlite3_vtab_cursor;
    FFilter: TVirtualConstraint;
    FBookmark: TVirtualBookmark;

    procedure InternalCreateCursor;
  public
    constructor Create(Table: TSQLiteVirtualTable);

    function Filter(idxNum: integer; idxStr: IntPtr; argc: integer; argv: ppsqlite3_value): integer;
    procedure Open;
    procedure Close;
    procedure Next;
    function Eof: boolean;

    function GetRowId(var pRowid: Int64): integer;

    function GetFieldValue(pContext: pSQLite3Context; index: integer): integer;
  end;

  TSQLiteVirtualTable = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FModule: TSQLiteModule;
    FInternalTable: sqlite3_vtab;
    FSchemaName,
    FTableName: string;
    FData: TVirtualData;
    FConstraintCount: integer;
    FConstraints: TVirtualConstraints;
//    FOrderBy: array of sqlite3_index_orderby;
    FCursors: TCRObjectList;

    procedure InternalCreateTable;
    procedure InternalDropTable;
    procedure ReadValue(const Value: PVirtualValue; var ppValue: ppsqlite3_value);
    procedure AddLastError(const Error: string);

    // module callback functions
    function StartTransaction: integer;
    function CommitTransaction: integer;
    function RollbackTransaction: integer;
    function BestIndex(pIndexInfo: psqlite3_index_info): integer;
    function OpenCursor(var ppCursor: psqlite3_vtab_cursor): integer;
    function CloseCursor(Cursor: TSQLiteVirtualTableCursor): integer;
    function Update(argc: integer; argv: ppsqlite3_value; var pRowid: Int64): integer;
  public
    constructor Create(Module: TSQLiteModule; const SchemaName, TableName: string; const Data: TVirtualData);
    destructor Destroy; override;

    property SchemaName: string read FSchemaName;
    property TableName: string read FTableName;
    property Data: TVirtualData read FData;
  end;

  TVirtualTableClass = class of TSQLiteVirtualTable;

  TSQLiteVirtualTables = class(TCRObjectList)
  public
    function Find(const SchemaName, TableName: string): TSQLiteVirtualTable;
  end;

  TSQLiteModule = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRVirtualConnection;
    FTables: TSQLiteVirtualTables;
    FInternalModule: sqlite3_module;
    FName: string;

    procedure InternalCreateModule;

    function DoCreateTable(const Args: TSQLiteArgs): TSQLiteVirtualTable;
    function DoConnectTable(const Args: TSQLiteArgs): TSQLiteVirtualTable;

    // module callback functions
    function CreateTable(PerformCreate: boolean; pSQLite: pSQLite3; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer;
  public
    constructor Create(Connection: TCRVirtualConnection; const ModuleName: string);
    destructor Destroy; override;
  end;

  TCRVirtualTransaction = class(TSQLiteTransaction)
  end;

  TCRVirtualMetaData = class(TSQLiteMetaData)
  end;

  TCRVirtualConnection = class(TSQLiteConnection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TObject;
    FModule: TSQLiteModule;
    FSchemas: TStringList;
    FSpecificTypes: TSpecificTypes;
    FTableCreationMode: TTableCreationMode;
    FTablesCreated: integer;
    FLockCreateTables,
    FLockDropTables: boolean;

    function GetFieldDataType(const DataTypeName: string; var DataType: word; var Len, Scale: integer): boolean;
    function GetTables: TSQLiteVirtualTables;
  protected
    class function GetVirtualTableClass: TVirtualTableClass; virtual;
  public
    constructor Create; overload; override;
    constructor Create(const Owner: TObject); reintroduce; overload;
    destructor Destroy; override;

    class function GetMapRulesClass: TCRMapRulesClass; override;
    function GetCommandClass: TCRCommandClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    function FindVirtualData(const SchemaName, TableName: string): TVirtualData;
    procedure RegisterVirtualData(const SchemaName, TableName: string; const Reader: TVirtualData);

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    procedure CheckSchema(const SchemaName, DatabaseName: string);
    procedure DoCreateTable(const SchemaName, TableName: string);
    procedure DoDropTable(const SchemaName, TableName: string);

    property Tables: TSQLiteVirtualTables read GetTables;
    property SpecificTypes: TSpecificTypes read FSpecificTypes;
  end;

  TCRVirtualSQLInfo = class(TSQLiteInfo)
  private
    FParserClass: TSQLParserClass;
  public
    constructor Create(ParserClass: TSQLParserClass); override;

    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    procedure ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo); override;
  end;

  TCRVirtualCommand = class(TSQLiteCommand)
  protected
    FTablesInfo: TCRTablesInfo;

    procedure InternalCreateTablesInfo; virtual;
    procedure InternalCreateTables;
    procedure InternalDropTables;

    procedure InternalExecute; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;
  end;

  TCRVirtualRecordSet = class(TSQLiteRecordSet)
  private
    FLockDisableControls: boolean;
  protected
    procedure CreateCommand; override;

    procedure InternalCreateTables;
    procedure InternalDropTables;

    procedure InternalPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

    procedure DoBeforeFetch(out Cancel: boolean); override;
    procedure DoAfterFetch; override;

    function GetDBType(const SQLTypeName: string; var Len, Scale: integer): word; overload; override;
    function GetDBType(SQLType: integer): word; overload; override;
    function GetDBUnknown: word; override;
    function IsFixedDBType(DBType: word): boolean; override;
    function GetDataType(DBType: word): word; override;
  public
    procedure Reopen; override;

    procedure SetSQL(const Value: string); override;
  end;

implementation

uses
{$IFDEF OFS}
  Debug,
{$ENDIF}
  Variants, FmtBcd,
  CRFunctions, CRProps, CLRClasses,
  CRVirtualConsts, VirtualDataTypeMap,
  LitePropsVirtual, LiteErrorVirtual, LiteParserVirtual;

{$IFDEF DEBUG_OUTPUT}
{$IFNDEF OFS}
procedure OFS(s: string; sl: TStrings = nil);
begin
  OutputDebugString(PChar(s));
end;
{$ENDIF}
{$ENDIF}

{ sqlite3_module methods }

function sqlite3ModuleCreate(pSQLite: pSQLite3; pAux: IntPtr; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer; cdecl;
var
  Module: TSQLiteModule;
begin
  Module := TSQLiteModule(pAux);
  Result := Module.CreateTable(True, pSQLite, argc, argv, ppVTab, pzErrMsg);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleCreate Module=$%s: $%s', [IntToHex(Integer(pAux), 8), IntToHex(Integer(ppVTab^.pSQLiteVirtualTable), 8)]));
{$ENDIF}
end;

function sqlite3ModuleConnect(pSQLite: pSQLite3; pAux: IntPtr; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer; cdecl;
var
  Module: TSQLiteModule;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleConnect Module=$%s', [IntToHex(Integer(pAux), 8)]));
{$ENDIF}
  Module := TSQLiteModule(pAux);
  Result := Module.CreateTable(False, pSQLite, argc, argv, ppVTab, pzErrMsg);
end;

function sqlite3ModuleBestIndex(pVTab: psqlite3_vtab; pIndexInfo: psqlite3_index_info): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
  Table := pVTab^.pSQLiteVirtualTable;
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleBestIndex: %s ($%s)', [Table.FTableName, IntToHex(Integer(Table), 8)]));
{$ENDIF}
  Result := Table.BestIndex(pIndexInfo);
end;

function sqlite3ModuleDisconnect(pVTab: psqlite3_vtab): integer; cdecl;
begin
  Result := SQLITE_OK;
end;

function sqlite3ModuleDestroy(pVTab: psqlite3_vtab): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
  Table := pVTab^.pSQLiteVirtualTable;
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleDestroy: %s ($%s)', [Table.FTableName, IntToHex(Integer(Table), 8)]));
{$ENDIF}
  Table.InternalDropTable;

  Result := SQLITE_OK;
end;

function sqlite3ModuleOpen(pVTab: psqlite3_vtab; var ppCursor: psqlite3_vtab_cursor): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
  Table := pVTab^.pSQLiteVirtualTable;
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleOpen: %s ($%s)', [Table.FTableName, IntToHex(Integer(Table), 8)]));
{$ENDIF}
  Result := Table.OpenCursor(ppCursor);
end;

function sqlite3ModuleClose(pCursor: psqlite3_vtab_cursor): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleClose: %s ($%s)', [Cursor.FTable.FTableName, IntToHex(Integer(Cursor.FTable), 8)]));
{$ENDIF}
  Result := Cursor.FTable.CloseCursor(Cursor);
end;

function sqlite3ModuleFilter(pCursor: psqlite3_vtab_cursor; idxNum: integer; idxStr: IntPtr; argc: integer; argv: ppsqlite3_value): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
  Result := Cursor.Filter(idxNum, idxStr, argc, argv);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleFilter: %s ($%s), argc = %d, Result = %d', [Cursor.FTable.FTableName, IntToHex(Integer(Cursor.FTable), 8), argc, Result]));
{$ENDIF}
end;

function sqlite3ModuleNext(pCursor: psqlite3_vtab_cursor): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
  Cursor.Next;
  Result := SQLITE_OK;
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleNext: %s ($%s), Result = %d', [Cursor.FTable.FTableName, IntToHex(Integer(Cursor.FTable), 8), Result]));
{$ENDIF}
end;

function sqlite3ModuleEof(pCursor: psqlite3_vtab_cursor): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
  Result := integer(Cursor.Eof);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleEof: %s ($%s), Result = %d', [Cursor.FTable.FTableName, IntToHex(Integer(Cursor.FTable), 8), Result]));
{$ENDIF}
end;

function sqlite3ModuleColumn(pCursor: psqlite3_vtab_cursor; pContext: pSQLite3Context; index: integer): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
  Result := Cursor.GetFieldValue(pContext, index);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleColumn: %s ($%s), Index = %d, Result = %d', [Cursor.FTable.FTableName, IntToHex(Integer(Cursor.FTable), 8), index, Result]));
{$ENDIF}
end;

function sqlite3ModuleRowid(pCursor: psqlite3_vtab_cursor; var pRowid: Int64): integer; cdecl;
var
  Cursor: TSQLiteVirtualTableCursor;
begin
  Cursor := pCursor^.pSQLiteVirtualTableCursor;
  Result := Cursor.GetRowId(pRowid);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleRowid: RowId = $%s', [IntToHex(pRowid, 8)]));
{$ENDIF}
end;

function sqlite3ModuleUpdate(pVTab: psqlite3_vtab; argc: integer; argv: ppsqlite3_value; var pRowid: Int64): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
  Table := pVTab^.pSQLiteVirtualTable;
  Result := Table.Update(argc, argv, pRowid);
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleUpdate: %s ($%s), Result = %d', [Table.FTableName, IntToHex(Integer(Table), 8), Result]));
{$ENDIF}
end;

function sqlite3ModuleBegin(pVTab: psqlite3_vtab): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleBegin', []));
{$ENDIF}

  Table := pVTab^.pSQLiteVirtualTable;
  Result := Table.StartTransaction;
end;

function sqlite3ModuleSync(pVTab: psqlite3_vtab): integer; cdecl;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleSync', []));
{$ENDIF}
  Result := SQLITE_OK;
end;

function sqlite3ModuleCommit(pVTab: psqlite3_vtab): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleCommit', []));
{$ENDIF}

  Table := pVTab^.pSQLiteVirtualTable;
  Result := Table.CommitTransaction;
end;

function sqlite3ModuleRollback(pVTab: psqlite3_vtab): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleRollback', []));
{$ENDIF}

  Table := pVTab^.pSQLiteVirtualTable;
  Result := Table.RollbackTransaction;
end;

function sqlite3ModuleFindFunction(pVTab: psqlite3_vtab; nArg: integer; zName: IntPtr; var pxFunc: Tsqlite3_func; var ppArg: IntPtr): integer; cdecl;
var
  Table: TSQLiteVirtualTable;
  FunctionName: string;
begin
  Table := pVTab^.pSQLiteVirtualTable;
  FunctionName := Table.FModule.FConnection.DecodeString(zName);

  // TODO: MATCH is used in the full text search only
  // GLOB is the same as LIKE except wildcards: * and ? are used instead of % and _
  // REGEXP ca be used in Delphi since ...
  if LowerCase(FunctionName) = 'glob' then begin

  end
  else if LowerCase(FunctionName) = 'regexp' then begin

  end;

  Result := SQLITE_OK;
end;

function sqlite3ModuleRename(pVTab: psqlite3_vtab; zNew: IntPtr): integer; cdecl;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleRename', []));
{$ENDIF}
  Result := SQLITE_OK;
end;

function sqlite3ModuleSavepoint(pVTab: psqlite3_vtab; n: integer): integer; cdecl;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleSavepoint', []));
{$ENDIF}
  Result := SQLITE_OK;
end;

function sqlite3ModuleRelease(pVTab: psqlite3_vtab; r: integer): integer; cdecl;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleRelease', []));
{$ENDIF}
  Result := SQLITE_OK;
end;

function sqlite3ModuleRollbackTo(pVTab: psqlite3_vtab; r: integer): integer; cdecl;
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('sqlite3ModuleRollbackTo', []));
{$ENDIF}
  Result := SQLITE_OK;
end;

{ TSQLiteVirtualTableCursor }

constructor TSQLiteVirtualTableCursor.Create(Table: TSQLiteVirtualTable);
begin
  inherited Create;

  FTable := Table;
  FBookmark := -1;
end;

procedure TSQLiteVirtualTableCursor.InternalCreateCursor;
begin
  FInternalCursor.pVtab := @FTable.FInternalTable;
  FInternalCursor.pSQLiteVirtualTableCursor := Self;
end;

function TSQLiteVirtualTableCursor.Filter(idxNum: integer; idxStr: IntPtr; argc: integer; argv: ppsqlite3_value): integer;
var
  i, n, c: integer;
  pValue: ppsqlite3_value;
begin
  Result := SQLITE_OK;

  try
    pValue := argv;

    SetLength(FFilter.Items, argc);
    FFilter.CurrentItem := -1;
    if idxNum > 0 then begin
      FFilter.LocalIndex := FTable.FConstraints[idxNum - 1].LocalIndex;
      FFilter.SortItemIndex := FTable.FConstraints[idxNum - 1].SortItemIndex;

      n := 1;
      i := 0;
      c := High(FTable.FConstraints[idxNum - 1].Items);
      while n <= argc do begin
        while (i <= c) and (FTable.FConstraints[idxNum - 1].Items[i].FieldIndex = -100) do
          Inc(i);
        if (i <= c) and (FTable.FConstraints[idxNum - 1].Items[i].ArgIndex = n) then begin
          FFilter.Items[n - 1] := FTable.FConstraints[idxNum - 1].Items[i];
          FTable.ReadValue(@FFilter.Items[n - 1].Value, pValue);
        end
        else
          Break;

        Inc(i);
        Inc(n);
      end;
    end
    else
      FFilter.LocalIndex := -1;

    if FFilter.SortItemIndex >= Length(FFilter.Items) then
      FFilter.SortItemIndex := 0;

    Open;
  except
    on E: Exception do begin
      if (E is EVariantTypeCastError) then
        Result := SQLITE_CONSTRAINT
      else begin
        Result := SQLITE_ERROR;
        FTable.AddLastError(E.Message);
      end;
    end;
  end;
end;

procedure TSQLiteVirtualTableCursor.Open;
begin
  if FFilter.LocalIndex >= 0 then
    FTable.FData.PrepareFilter(@FFilter);

  FBookmark := FTable.FData.Open(@FFilter);
end;

procedure TSQLiteVirtualTableCursor.Close;
begin
  // do nothing
end;

procedure TSQLiteVirtualTableCursor.Next;
begin
  FBookmark := FTable.FData.Next(FBookmark, @FFilter);
end;

function TSQLiteVirtualTableCursor.Eof: boolean;
begin
  Result := FTable.FData.Eof(FBookmark);
end;

function TSQLiteVirtualTableCursor.GetRowId(var pRowid: Int64): integer;
begin
  pRowid := Int64(FBookmark);

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTableCursor.GetFieldValue(pContext: pSQLite3Context; index: integer): integer;
var
  Value: variant;
  DataType: integer;
  AStr: AnsiString;
  BlobValue: IntPtr;
  DestrType: IntPtr;
  ValInt64: Int64;
  ValFloat: double;
  FieldNull: boolean;
begin
  Value := FTable.FData.GetFieldValue(index, FieldNull);
  if FieldNull then
    FTable.FModule.FConnection.API.sqlite3_result_null(pContext)
  else begin
    DataType := FTable.FData.GetFieldType(index);
  {$IFDEF DEBUG_OUTPUT}
    OFS(Format('GetFieldValue(%d): %s', [index, VarToStr(Value)]));
  {$ENDIF}
    case DataType of
      dtUInt8:
        FTable.FModule.FConnection.API.sqlite3_result_int(pContext, byte(Value));
      dtInt8, dtInt16:
        FTable.FModule.FConnection.API.sqlite3_result_int(pContext, SmallInt(Value));
      dtUInt16:
        FTable.FModule.FConnection.API.sqlite3_result_int(pContext, Word(Value));
      dtInt32:
        FTable.FModule.FConnection.API.sqlite3_result_int(pContext, integer(Value));
      dtBoolean:
        FTable.FModule.FConnection.API.sqlite3_result_int64(pContext, Int64(boolean(Value)));
      dtInt64, dtUInt64, dtUInt32: begin
        ValInt64 := Value;
        FTable.FModule.FConnection.API.sqlite3_result_int64(pContext, ValInt64);
      end;
      dtSingle, dtFloat, dtCurrency, dtExtended:
        FTable.FModule.FConnection.API.sqlite3_result_double(pContext, Value);
      dtBCD, dtFMTBCD: begin
        ValFloat := Value;
        FTable.FModule.FConnection.API.sqlite3_result_double(pContext, ValFloat);
      end;
      dtTime,
      dtDate,
      dtDateTime,
      dtSQLTimeStamp,
      dtSQLTimeStampOffset: begin
        AStr := FTable.FModule.FConnection.GetTypes.ConvertToText(DataType, FTable.FData.GetFieldScale(index), Value);
        FTable.FModule.FConnection.API.sqlite3_result_text(pContext, PAnsiChar(AStr), LengthA(AStr), SQLITE_TRANSIENT);
      end;
      dtGuid,
      dtString,
      dtExtString,
      dtFixedChar,
      dtWideString,
      dtExtWideString,
      dtFixedWideChar: begin
        AStr := FTable.FModule.FConnection.EncodeString(VarToStr(Value));
        FTable.FModule.FConnection.API.sqlite3_result_text(pContext, PAnsiChar(AStr), LengthA(AStr), SQLITE_TRANSIENT);
      end;
      dtMemo,
      dtWideMemo: begin
        AStr := FTable.FModule.FConnection.GetTypes.ConvertMemo(Value);
        FTable.FModule.FConnection.API.sqlite3_result_blob(pContext, PAnsiChar(AStr), LengthA(AStr), SQLITE_TRANSIENT);
        FTable.FData.FreeFieldValue(Value);
      end;
      dtBlob,
      dtBytes,
      dtVarBytes,
      dtExtVarBytes: begin
        BlobValue := FTable.FModule.FConnection.GetTypes.ConvertBlob(Value, DestrType);
        FTable.FModule.FConnection.API.sqlite3_result_blob(pContext, BlobValue, Length(TBytes(BlobValue)), SQLITE_TRANSIENT);
        FTable.FData.FreeFieldValue(Value);
      end;
    else
      FTable.FModule.FConnection.API.sqlite3_result_null(pContext);
    end;
  end;

  Result := SQLITE_OK;
end;

{ TSQLiteVirtualTable }

constructor TSQLiteVirtualTable.Create(Module: TSQLiteModule; const SchemaName, TableName: string; const Data: TVirtualData);
begin
  inherited Create;

  FModule := Module;
  FSchemaName := SchemaName;
  FTableName := TableName;
  FData := Data;

  FConstraintCount := 0;
  FCursors := TCRObjectList.Create;
end;

destructor TSQLiteVirtualTable.Destroy;
begin
  FCursors.Clear;
  FCursors.Free;
  FData.Free;

  inherited;
end;

procedure TSQLiteVirtualTable.InternalCreateTable;
var
  SQL: string;
//{$IFDEF DEBUG_OUTPUT}
//  i: integer;
//{$ENDIF}
begin
{$IFDEF DEBUG_OUTPUT}
  OFS('TSQLiteVirtualTable.InternalCreateTable: ' + TableName);
{$ENDIF}
  FData.DescribeFields(FModule.FConnection.FSpecificTypes);

  if Length(FData.Fields) = 0 then begin
    SetLength(FData.FFields, 1);

    FData.Fields[0].Name := '_id';
    FData.Fields[0].DataType := dtWideString;
    FData.Fields[0].Length := 24;
    FData.Fields[0].Scale := 0;
    FData.Fields[0].IsKey := True;
    FData.Fields[0].IsAutoIncrement := False;
    FData.Fields[0].Required := False;
    FData.Fields[0].ReadOnly := False;
    FData.Fields[0].ActualIndex := 0;
  end;

{$IFDEF DEBUG_OUTPUT}
//  OFS('FData.DescribeFields: ' + TableName);
//  for i := 0 to High(FData.Fields) do
//    OFS(Format('%d. %s = %d', [i + 1, FData.Fields[i].Name, FData.Fields[i].DataType]));
{$ENDIF}
  SQL := FData.GetCreateSQL;

  if SQL <> '' then begin
    FModule.FConnection.Check(FModule.FConnection.API.sqlite3_declare_vtab(FModule.FConnection.API.SQLite, PAnsiChar(FModule.FConnection.EncodeString(SQL))));
    FModule.FConnection.Check(FModule.FConnection.API.sqlite3_vtab_config(FModule.FConnection.API.SQLite, SQLITE_VTAB_CONSTRAINT_SUPPORT, 1));
  end;
end;

procedure TSQLiteVirtualTable.InternalDropTable;
begin
  FData.Close;
end;

procedure TSQLiteVirtualTable.ReadValue(const Value: PVirtualValue; var ppValue: ppsqlite3_value);
var
  ValueSize: Integer;
  pValue: psqlite3_value;
  blobValue: IntPtr;
begin
  pValue := Marshal.ReadIntPtr(ppValue);

  case FModule.FConnection.API.sqlite3_value_type(pValue) of
    SQLITE_INTEGER: Value^.ValueType := vrInteger;
    SQLITE_FLOAT: Value^.ValueType := vrFloat;
    SQLITE_TEXT: Value^.ValueType := vrString;
    SQLITE_BLOB: Value^.ValueType := vrBlob;
  else
    Value^.ValueType := vrNull;
  end;

  case Value^.ValueType of
    vrInteger: begin
      Value^.IntValue := FModule.FConnection.API.sqlite3_value_int64(pValue);
      Value^.Value := Value^.IntValue;
    end;
    vrFloat: begin
      Value^.FloatValue := FModule.FConnection.API.sqlite3_value_double(pValue);
      Value^.Value := Value^.FloatValue;
    end;
    vrString: begin
      if FModule.FConnection.IsUnicodeDataBase then begin
        if FModule.FConnection.UseUnicode then begin
          ValueSize := FModule.FConnection.API.sqlite3_value_bytes16(pValue);
          Value^.ValueType := vrWideString;
          Value^.WideStrValue := Marshal.PtrToStringUni(FModule.FConnection.API.sqlite3_value_text16(pValue), ValueSize shr 1);
          Value^.Value := Value^.WideStrValue;
        end
        else begin
          Value^.ValueType := vrAnsiString;
          Value^.AnsiStrValue := FModule.FConnection.DecodeStringA(FModule.FConnection.API.sqlite3_value_text(pValue), FModule.FConnection.IsUnicodeDataBase);
          Value^.Value := Value^.AnsiStrValue;
        end;
      end
      else begin
        ValueSize := FModule.FConnection.API.sqlite3_value_bytes(pValue);
        Value^.ValueType := vrAnsiString;
        Value^.AnsiStrValue := Marshal.PtrToStringAnsi(FModule.FConnection.API.sqlite3_value_text(pValue), ValueSize);
        Value^.Value := Value^.AnsiStrValue;
      end;
    end;
    vrBlob: begin
      ValueSize := FModule.FConnection.API.sqlite3_value_bytes(pValue);
      if ValueSize > 0 then begin
        blobValue := FModule.FConnection.API.sqlite3_value_blob(pValue);
        Value^.ValueType := vrBlob;
        Value^.Value := VarArrayCreate([0, ValueSize - 1], varByte);;
        Move(blobValue^, TVarData(Value^.Value).VArray.Data^, ValueSize);
      end
      else
        Value^.ValueType := vrNull;
    end;
  end;

  ppValue := PtrOffset(ppValue, SizeOf(psqlite3_value));
end;

procedure TSQLiteVirtualTable.AddLastError(const Error: string);
var
  TmpMsg: AnsiString;
begin
  TmpMsg := FModule.FConnection.EncodeString(Error);
  FInternalTable.zErrMsg := FModule.FConnection.API.sqlite3_malloc(LengthA(TmpMsg) + 1);
  Move(PAnsiChar(TmpMsg)^, FInternalTable.zErrMsg^, LengthA(TmpMsg));
  PByte(PtrOffset(FInternalTable.zErrMsg, LengthA(TmpMsg)))^ := 0;
end;

function TSQLiteVirtualTable.StartTransaction: integer;
begin
  FData.StartTransaction;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.CommitTransaction: integer;
begin
  FData.Commit;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.RollbackTransaction: integer;
begin
  FData.Rollback;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.BestIndex(pIndexInfo: psqlite3_index_info): integer;
var
  Ptr: IntPtr;
  i, Cost, ArgIndex: integer;
  Indexed: boolean;
begin
  FData.CheckActive;

  SetLength(FConstraints, FConstraintCount + 1);

  if Length(FConstraints[FConstraintCount].Items) < pIndexInfo^.nConstraint then
    SetLength(FConstraints[FConstraintCount].Items, pIndexInfo^.nConstraint);

  ArgIndex := 1;
  Indexed := False;

  Ptr := pIndexInfo^.aConstraint;
  for i := 0 to pIndexInfo^.nConstraint - 1 do begin
    if (psqlite3_index_constraint(Ptr)^.usable = 1) and
       (psqlite3_index_constraint(Ptr)^.op in [SQLITE_INDEX_CONSTRAINT_EQ, SQLITE_INDEX_CONSTRAINT_GT, SQLITE_INDEX_CONSTRAINT_LE, SQLITE_INDEX_CONSTRAINT_LT, SQLITE_INDEX_CONSTRAINT_GE])
    then begin
      FConstraints[FConstraintCount].Items[i].FieldIndex := psqlite3_index_constraint(Ptr)^.iColumn;
      if FConstraints[FConstraintCount].Items[i].FieldIndex >= 0 then begin
        FConstraints[FConstraintCount].Items[i].ArgIndex := ArgIndex;
        case psqlite3_index_constraint(Ptr)^.op of
          SQLITE_INDEX_CONSTRAINT_EQ: FConstraints[FConstraintCount].Items[i].Operation := ntEqual;
          SQLITE_INDEX_CONSTRAINT_GT: FConstraints[FConstraintCount].Items[i].Operation := ntMore;
          SQLITE_INDEX_CONSTRAINT_LE: FConstraints[FConstraintCount].Items[i].Operation := ntLessEqual;
          SQLITE_INDEX_CONSTRAINT_LT: FConstraints[FConstraintCount].Items[i].Operation := ntLess;
          SQLITE_INDEX_CONSTRAINT_GE: FConstraints[FConstraintCount].Items[i].Operation := ntMoreEqual;
          SQLITE_INDEX_CONSTRAINT_MATCH: FConstraints[FConstraintCount].Items[i].Operation := ntMatch;
          SQLITE_INDEX_CONSTRAINT_LIKE: FConstraints[FConstraintCount].Items[i].Operation := ntLike;
          SQLITE_INDEX_CONSTRAINT_GLOB: FConstraints[FConstraintCount].Items[i].Operation := ntGlob;
          // TODO: regext support
          SQLITE_INDEX_CONSTRAINT_REGEXP: FConstraints[FConstraintCount].Items[i].Operation := ntRegExp;
        end;
        FConstraints[FConstraintCount].Items[i].SimpleCompare := FData.IsSimpleFieldType(FConstraints[FConstraintCount].Items[i].FieldIndex);
        FConstraints[FConstraintCount].Items[i].Value.ValueType := vrNull;
        VarClear(FConstraints[FConstraintCount].Items[i].Value.Value);
        Inc(ArgIndex);
        Indexed := True;
      end
      else begin
        FConstraints[FConstraintCount].Items[i].FieldIndex := -100;
        FConstraints[FConstraintCount].Items[i].ArgIndex := 0;
      end;
    end
    else begin
      FConstraints[FConstraintCount].Items[i].FieldIndex := -100;
      FConstraints[FConstraintCount].Items[i].ArgIndex := 0;
    end;

    Ptr := PtrOffset(Ptr, SizeOf(sqlite3_index_constraint));
  end;

  if Indexed then begin
    FData.PrepareConstraints(@FConstraints[FConstraintCount], Cost);

    FConstraints[FConstraintCount].SortItemIndex := -1;
    if FData.CanUseLocalIndex then
      for i := 0 to Length(FConstraints[FConstraintCount].Items) - 1 do
        if (FConstraints[FConstraintCount].Items[i].FieldIndex <> -100) and
           FConstraints[FConstraintCount].Items[i].SimpleCompare and
           (FConstraints[FConstraintCount].Items[i].Operation in [ntEqual, ntMore, ntLess, ntMoreEqual, ntLessEqual])
        then begin
          FConstraints[FConstraintCount].SortItemIndex := i;
          Break;
        end;

    if FConstraints[FConstraintCount].SortItemIndex >= 0 then
      FConstraints[FConstraintCount].LocalIndex := FData.GetLocalIndex(@FConstraints[FConstraintCount])
    else
      FConstraints[FConstraintCount].LocalIndex := -1;

    Ptr := pIndexInfo^.aConstraintUsage;
    for i := 0 to pIndexInfo^.nConstraint - 1 do begin
      psqlite3_index_constraint_usage(Ptr)^.argvIndex := FConstraints[FConstraintCount].Items[i].ArgIndex;

      if FConstraints[FConstraintCount].Items[i].FieldIndex <> -100 then
        psqlite3_index_constraint_usage(Ptr)^.omit := 1
      else
        psqlite3_index_constraint_usage(Ptr)^.omit := 0;

      Ptr := PtrOffset(Ptr, SizeOf(sqlite3_index_constraint_usage));
    end;

    if FConstraints[FConstraintCount].LocalIndex = -1 then
      Cost := Cost * 100;
    pIndexInfo^.estimatedCost := Cost;

    if pIndexInfo^.nConstraint > 0 then
      pIndexInfo^.idxNum := FConstraintCount + 1;
  end;

  Inc(FConstraintCount);

//  SetLength(FOrderBy, pIndexInfo^.nOrderBy);
//
//  Ptr := pIndexInfo^.aOrderBy;
//  for i := 0 to Length(FOrderBy) - 1 do begin
//    FOrderBy[i].iColumn := psqlite3_index_orderby(Ptr)^.iColumn;
//    FOrderBy[i].desc := psqlite3_index_orderby(Ptr)^.desc;
//
//    Ptr := PtrOffset(Ptr, SizeOf(sqlite3_index_orderby));
//  end;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.OpenCursor(var ppCursor: psqlite3_vtab_cursor): integer;
var
  TmpCursor: TSQLiteVirtualTableCursor;
begin
  TmpCursor := TSQLiteVirtualTableCursor.Create(Self);
  TmpCursor.InternalCreateCursor;
  FCursors.Add(TmpCursor);
  ppCursor := @TmpCursor.FInternalCursor;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.CloseCursor(Cursor: TSQLiteVirtualTableCursor): integer;
begin
  Cursor.Close;
  FCursors.Delete(FCursors.IndexOf(Cursor));

  if FCursors.Count = 0 then
    if FData.AutoClose then
      FData.Close
    else
      FData.Reset;

  Result := SQLITE_OK;
end;

function TSQLiteVirtualTable.Update(argc: integer; argv: ppsqlite3_value; var pRowid: Int64): integer;
var
  Rowid,
  NewRowid: TVirtualValue;
  i: integer;
  pValue: ppsqlite3_value;
  Values: TVirtualValues;
begin
  Result := SQLITE_OK;

  if argc < 1 then begin
    Result := SQLITE_MISUSE;
    Exit;
  end;

  try
    pValue := argv;
    ReadValue(@Rowid, pValue);
    if Rowid.ValueType <> vrNull then
      FData.GotoBookmark(TVirtualBookmark(Rowid.IntValue));

    if argc = 1 then begin
      FData.DeleteRecord;
      pRowid := FData.GetBookmark;
    end
    else begin
      ReadValue(@NewRowid, pValue);

      SetLength(Values, argc - 2);
      for i := 0 to argc - 3 do
        ReadValue(@Values[i], pValue);

    {$IFDEF DEBUG_OUTPUT}
      OFS('TSQLiteVirtualTable.Update: ' + TableName);
      for i := 0 to High(Values) do
        OFS(Format('(%d) %s', [i + 1, VarToStr(Values[i].Value)]));
    {$ENDIF}

      if Rowid.ValueType = vrNull then begin
        if not FData.Active then
          FData.Open(nil);
        FData.InsertRecord(Values);
        FData.ClearLocalIndexes;
        pRowid := FData.GetBookmark;
      end
      else begin
        if not FData.Active then
          FData.Open(nil);
        FData.EditRecord(Values);
        FData.ClearLocalIndexes;
      end;
    end;

    if FData.AutoUnwind and (FCursors.Count = 0) then
      FData.Close;
  except
    on e: Exception do begin
      Result := SQLITE_ERROR;
      AddLastError(e.Message);
    end;
  end;
end;

{ TTableDescs }

function TSQLiteVirtualTables.Find(const SchemaName, TableName: string): TSQLiteVirtualTable;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := TSQLiteVirtualTable(Items[i]);
    if ((SchemaName = '') or SameText(Result.FSchemaName, SchemaName)) and (SameText(Result.FTableName, TableName)) then
      Exit;
  end;

  Result := nil;
end;

{ TSQLiteModule }

constructor TSQLiteModule.Create(Connection: TCRVirtualConnection; const ModuleName: string);
begin
{$IFDEF DEBUG_OUTPUT}
  OFS(Format('TSQLiteModule.Create: ModuleName = %s', [ModuleName]));
{$ENDIF}

  inherited Create;

  FConnection := Connection;
  FName := ModuleName;

  FTables := TSQLiteVirtualTables.Create;
end;

destructor TSQLiteModule.Destroy;
begin
  FTables.Free;

  inherited;
end;

procedure TSQLiteModule.InternalCreateModule;
begin
  FInternalModule.xCreate := @sqlite3ModuleCreate;
  FInternalModule.xConnect := @sqlite3ModuleConnect;
  FInternalModule.xBestIndex := @sqlite3ModuleBestIndex;
  FInternalModule.xDisconnect := @sqlite3ModuleDisconnect;
  FInternalModule.xDestroy := @sqlite3ModuleDestroy;
  FInternalModule.xOpen := @sqlite3ModuleOpen;
  FInternalModule.xClose := @sqlite3ModuleClose;
  FInternalModule.xFilter := @sqlite3ModuleFilter;
  FInternalModule.xNext := @sqlite3ModuleNext;
  FInternalModule.xEof := @sqlite3ModuleEof;
  FInternalModule.xColumn := @sqlite3ModuleColumn;
  FInternalModule.xRowid := @sqlite3ModuleRowid;
  FInternalModule.xUpdate := @sqlite3ModuleUpdate;
  FInternalModule.xBegin := @sqlite3ModuleBegin;
  FInternalModule.xSync := @sqlite3ModuleSync;
  FInternalModule.xCommit := @sqlite3ModuleCommit;
  FInternalModule.xRollback := @sqlite3ModuleRollback;
  FInternalModule.xFindFunction := @sqlite3ModuleFindFunction;
  FInternalModule.xRename := @sqlite3ModuleRename;
  FInternalModule.xSavepoint := @sqlite3ModuleSavepoint;
  FInternalModule.xRelease := @sqlite3ModuleRelease;
  FInternalModule.xRollbackTo := @sqlite3ModuleRollbackTo;

  FConnection.Check(FConnection.API.sqlite3_create_module(FConnection.API.SQLite, PAnsiChar(FConnection.EncodeString(FName)), @FInternalModule, Self));
end;

function TSQLiteModule.DoCreateTable(const Args: TSQLiteArgs): TSQLiteVirtualTable;
var
  Schema: string;
begin
  if LowerCase(Args[1]) = 'main' then
    Schema := ''
  else
    Schema := Args[1];
  Result := FTables.Find(FConnection.NormalizeTableName(Schema), FConnection.NormalizeTableName(Args[2]));

  if Result <> nil then begin
    Result.FInternalTable.pSQLiteVirtualTable := Result;
    Result.InternalCreateTable;
  end;
end;

function TSQLiteModule.DoConnectTable(const Args: TSQLiteArgs): TSQLiteVirtualTable;
begin
  Result := DoCreateTable(Args);
end;

function TSQLiteModule.CreateTable(PerformCreate: boolean; pSQLite: pSQLite3; argc: integer; argv: PIntPtr; var ppVTab: psqlite3_vtab; var pzErrMsg: IntPtr): integer;
var
  TmpTable: TSQLiteVirtualTable;
  Args: TSQLiteArgs;
  i: integer;
  TmpMsg: AnsiString;
begin
  ppVTab := nil;
  pzErrMsg := nil;
  Result := SQLITE_ERROR;

  try
    SetLength(Args, argc);
    for i := 0 to argc - 1 do begin
      Args[i] := FConnection.DecodeString(argv^);
      argv := PtrOffset(argv, SizeOf(IntPtr));
    end;

    if PerformCreate then
      TmpTable := DoCreateTable(Args)
    else
      TmpTable := DoConnectTable(Args);

    if TmpTable <> nil then begin
      ppVTab := @TmpTable.FInternalTable;
      Result := SQLITE_OK;
    end;
  except
    on E: Exception do begin
      if E is ESQLiteError then
        Result := ESQLiteError(E).ErrorCode
      else begin
        TmpMsg := FConnection.EncodeString(E.Message);
        pzErrMsg := FConnection.API.sqlite3_malloc(LengthA(TmpMsg) + 1);
        Move(PAnsiChar(TmpMsg)^, pzErrMsg^, LengthA(TmpMsg));
        PByte(PtrOffset(pzErrMsg, LengthA(TmpMsg)))^ := 0;
      end;
    end;
  end;
end;

{ TSQLiteVirtualConnection }

constructor TCRVirtualConnection.Create;
begin
  inherited;

  FSpecificTypes := TSpecificTypes.Create{$IFDEF VER12P}(True){$ENDIF};
  FSpecificTypes.Sorted := True;

{$IFNDEF NOSTATIC}
  SetProp(prStaticLibrary, True);
{$ELSE}
  SetProp(prStaticLibrary, False);
{$ENDIF}
  SetProp(prEnableSharedCache, False);
  SetProp(prNativeDate, False);
  SetProp(prDatabase, ':memory:');
  SetProp(prTableCreationMode, cmAll);

  FModule := TSQLiteModule.Create(Self, 'LiteModule');
  FSchemas := TStringList.Create;
  FSchemas.Sorted := True;
end;

constructor TCRVirtualConnection.Create(const Owner: TObject);
begin
  Create;

  FOwner := Owner;
end;

destructor TCRVirtualConnection.Destroy;
{$IFNDEF VER12P}
var
  i: integer;
{$ENDIF}
begin
  inherited;

  FModule.Free;
  FSchemas.Free;

{$IFNDEF VER12P}
  for i := 0 to FSpecificTypes.Count - 1 do
    TSpecificTypeDesc(FSpecificTypes.Objects[i]).Free;
{$ENDIF}
  FSpecificTypes.Free;
end;

function TCRVirtualConnection.GetFieldDataType(const DataTypeName: string; var DataType: word; var Len, Scale: integer): boolean;
var
  n: integer;
  TypeDesc: TSpecificTypeDesc;
begin
  Result := False;

  n := FSpecificTypes.IndexOf(LowerCase(DataTypeName));
  if n >= 0 then begin
    TypeDesc := TSpecificTypeDesc(FSpecificTypes.Objects[n]);

    DataType := TypeDesc.DataType;
    Len := TypeDesc.Length;
    Scale := TypeDesc.Scale;

    Result := True;
  end;
end;

function TCRVirtualConnection.GetTables: TSQLiteVirtualTables;
begin
  if FModule <> nil then
    Result := FModule.FTables
  else
    Result := nil;
end;

class function TCRVirtualConnection.GetVirtualTableClass: TVirtualTableClass;
begin
  Result := TSQLiteVirtualTable;
end;

class function TCRVirtualConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TCRVirtualCommand.GetMapRulesClass;
end;

function TCRVirtualConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TCRVirtualCommand;
end;

function TCRVirtualConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prTableCreationMode:
      FTableCreationMode := TTableCreationMode(integer(Value));
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TCRVirtualConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prTableCreationMode:
      Value := integer(FTableCreationMode);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TCRVirtualConnection.FindVirtualData(const SchemaName, TableName: string): TVirtualData;
var
  Table: TSQLiteVirtualTable;
begin
  Table := FModule.FTables.Find(NormalizeTableName(SchemaName), NormalizeTableName(TableName));
  if Table <> nil then
    Result := Table.FData
  else
    Result := nil;
end;

procedure TCRVirtualConnection.RegisterVirtualData(const SchemaName, TableName: string; const Reader: TVirtualData);
var
  Schema, NormalizedTable, VirtualTableName: string;
begin
  if LowerCase(SchemaName) = 'main' then
    Schema := ''
  else
    Schema := NormalizeTableName(SchemaName);

  if TableName = '' then
    raise Exception.Create(STableNameIsEmpty);

  NormalizedTable := NormalizeTableName(TableName);

  if FModule.FTables.Find(Schema, NormalizedTable) = nil then
    FModule.FTables.Add(GetVirtualTableClass.Create(FModule, Schema, NormalizedTable, Reader))
  else begin
    if Schema <> '' then
      VirtualTableName := Schema + '.' + TableName
    else
      VirtualTableName := TableName;

    raise EVirtualQueryError.CreateFmt(STableRegistered, [VirtualTableName]);
  end;
end;

procedure TCRVirtualConnection.Connect(const ConnectString: string);
begin
  try
    inherited Connect('');

    FModule.InternalCreateModule;
  except
    Disconnect;
    raise;
  end;
end;

procedure TCRVirtualConnection.Disconnect;
var
  i: integer;
begin
  if (FTableCreationMode = cmAll) and (FModule <> nil) then begin
    for i := FModule.FTables.Count - 1 downto 0 do
      DoDropTable(TSQLiteVirtualTable(FModule.FTables[i]).FSchemaName, TSQLiteVirtualTable(FModule.FTables[i]).FTableName);

    FLockCreateTables := False;
    FTablesCreated := 0;
  end;

  if FModule <> nil then
    FModule.FTables.Clear;

  if FSchemas <> nil then
    FSchemas.Clear;

  inherited;
end;

procedure TCRVirtualConnection.CheckSchema(const SchemaName, DatabaseName: string);
begin
  if (SchemaName <> '') and (FSchemas.IndexOf(SchemaName) < 0) then begin
    ExecuteSQL('ATTACH DATABASE '':memory:'' AS ' + DatabaseName);
    FSchemas.Add(SchemaName);
  end;
end;

procedure TCRVirtualConnection.DoCreateTable(const SchemaName, TableName: string);
var
  Schema, NormalizedTable, VirtualTableName: string;
  Table: TSQLiteVirtualTable;
begin
  if LowerCase(SchemaName) = 'main' then
    Schema := ''
  else
    Schema := NormalizeTableName(SchemaName);

  NormalizedTable := NormalizeTableName(TableName);

  if Schema <> '' then
    VirtualTableName := Schema + '.' + NormalizedTable
  else
    VirtualTableName := NormalizedTable;

  Table := FModule.FTables.Find(Schema, NormalizedTable);
  if Table = nil then
    raise EVirtualQueryError.CreateFmt(STableNotFound, [VirtualTableName]);

  CheckSchema(Schema, SchemaName);

  ExecuteSQL('CREATE VIRTUAL TABLE ' + VirtualTableName + ' USING ' + FModule.FName);
  Inc(FTablesCreated);
end;

procedure TCRVirtualConnection.DoDropTable(const SchemaName, TableName: string);
var
  Schema, NormalizedTable, VirtualTableName: string;
  Table: TSQLiteVirtualTable;
begin
  if LowerCase(SchemaName) = 'main' then
    Schema := ''
  else
    Schema := NormalizeTableName(SchemaName);

  NormalizedTable := NormalizeTableName(TableName);

  if Schema <> '' then
    VirtualTableName := Schema + '.' + NormalizedTable
  else
    VirtualTableName := NormalizedTable;

  Table := FModule.FTables.Find(Schema, NormalizedTable);
  if Table <> nil then begin
    ExecuteSQL('DROP TABLE IF EXISTS ' + VirtualTableName);
    FModule.FTables.Delete(FModule.FTables.IndexOf(Table));
    if FTablesCreated > 0 then
      Dec(FTablesCreated);
  end;
end;

{ TCRVirtualSQLInfo }

constructor TCRVirtualSQLInfo.Create(ParserClass: TSQLParserClass);
begin
  inherited;

  FParserClass := ParserClass;
end;

function TCRVirtualSQLInfo.LeftQuote: Char;
begin
  Result := '"';
end;

function TCRVirtualSQLInfo.RightQuote: Char;
begin
  Result := '"';
end;

procedure TCRVirtualSQLInfo.ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo);

  // find close bracket and skip all inside
  procedure ToEndBracket(Parser: TSQLParser; var CodeLexem: integer; var StLex: string);
  var
    BracketCount: integer;
  begin
    BracketCount := 1;
    repeat
      CodeLexem := Parser.GetNext(StLex);

      if CodeLexem = lxLeftBracket then
        Inc(BracketCount)
      else if CodeLexem = lxRightBracket then
        Dec(BracketCount);
    until (BracketCount = 0) or (CodeLexem = lcEnd);
  end;

  procedure ParseTableName(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; InBrackets: boolean);
  var
    Name, Alias: string;
    TableInfo: TCRTableInfo;
  begin
    repeat
      // exit on WHERE and other clause lexems
      if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
        exit
      // bypass end bracket
      else if CodeLexem = lxRightBracket then
        exit
      // bypass join
      else if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL, lxOUTER] then
        CodeLexem := Parser.GetNext(StLex)
      // bypass comma
      else if CodeLexem = lxComma then
        CodeLexem := Parser.GetNext(StLex)
      //parse table name
      else begin
        // parse brackets recursive
        if CodeLexem = lxLeftBracket then begin
          CodeLexem := Parser.GetNext(StLex);

          // Oracle supports WITH lexem in subqueries
          if not (CodeLexem in [lxWITH, lxSELECT]) then
            ParseTableName(Parser, CodeLexem, StLex, True)
          else // skip subquery
            ToEndBracket(Parser, CodeLexem, StLex);

          if CodeLexem = lcEnd then
            Exit;

          // skip end bracket
          if CodeLexem = lxRightBracket then
            CodeLexem := Parser.GetNext(StLex);
        end
        else begin
          Name := '';

          // PostgreSQL can containt ONLY lexeme before table name, skip it
          if HasOnlyLexem then
            if CodeLexem = lxONLY then
              CodeLexem := Parser.GetNext(StLex);

          while (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) or (CodeLexem = lxPoint) do begin
              Name := Name + StLex;
              if CodeLexem <> lxPoint then begin
                CodeLexem := Parser.GetNext(StLex);
                if CodeLexem = lxPoint then
                  Name := Name + '.'
                else
                  break;
              end;
              CodeLexem := Parser.GetNext(StLex);
          end;

          if Name <> '' then begin
            if HasAsLexem then
              if CodeLexem = lxAS then
                CodeLexem := Parser.GetNext(StLex);

            // Oracle extended table info
            ParseExtTableInfo(Parser, CodeLexem, StLex, Name);

            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) and
               (CodeLexem <> lxJOIN) and (CodeLexem <> lxINNER) and
               (CodeLexem <> lxLEFT) and (CodeLexem <> lxRIGHT) and
               (CodeLexem <> lxFULL) and (CodeLexem <> lxON) and
               not Parser.IsClauseLexem(CodeLexem)
            then begin
              Alias := StLex;
              CodeLexem := Parser.GetNext(StLex);
            end
            else
              Alias := '';

            if CodeLexem <> lxLeftBracket then begin // skip stored functions
              Name := NormalizeName(Name);
              Alias := NormalizeName(Alias);
              TableInfo := TablesInfo.FindByNameAndAlias(Name, Alias);
              if TableInfo = nil then begin
                TableInfo := TablesInfo.Add;
                TableInfo.TableName := Name;
                TableInfo.TableAlias := Alias;
              end;
            end;
          end;
        end;

        if CodeLexem = lcEnd then
          Exit;

        // bypass subqueries, function calls, ON clause of join
        repeat
          if CodeLexem = lxLeftBracket then begin
            ToEndBracket(Parser, CodeLexem, StLex);
            if CodeLexem <> lcEnd then
              CodeLexem := Parser.GetNext(StLex);
          end;

          if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL] then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end
          else if CodeLexem = lxComma then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end
          else if CodeLexem = lxRightBracket then
            exit
          else if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
            Exit;

          CodeLexem := Parser.GetNext(StLex);
        until (CodeLexem = lcEnd);
      end;
    until CodeLexem = lcEnd;
  end;

var
  Parser: TSQLParser;
  StLex: string;
  CodeLexem: integer;
  i: integer;
  ObjectInfo: TSQLObjectInfo;
begin
  Assert(FParserClass <> nil);
  Parser := FParserClass.Create(SQL);
  TablesInfo.BeginUpdate;
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    repeat
      if Parser.ToLexem(lxSELECT) <> lcEnd then begin
        CodeLexem := Parser.ToLexem(lxFROM, True);
        if CodeLexem <> lcEnd then begin
          CodeLexem := Parser.GetNext(StLex);
          ParseTableName(Parser, CodeLexem, StLex, False);
        end;
      end
      else
        Break;
    until False;

    for i := 0 to TablesInfo.Count - 1 do begin
      TSQLiteTableInfo(TablesInfo[i]).TableNameFull := TablesInfo[i].TableName;
      SplitObjectName(TablesInfo[i].TableNameFull, ObjectInfo);
      TablesInfo[i].TableName := ObjectInfo.Name;
    end;
  finally
    TablesInfo.EndUpdate;
    Parser.Free;
  end;
end;

{ TCRVirtualCommand }

constructor TCRVirtualCommand.Create;
begin
  inherited;

  FTablesInfo := TCRTablesInfo.Create(GetTableInfoClass);
end;

destructor TCRVirtualCommand.Destroy;
begin
  FTablesInfo.Free;

  inherited;
end;

procedure TCRVirtualCommand.InternalCreateTablesInfo;
begin
  FTablesInfo.CaseSensitive := False;
  SQLInfo.ParseTablesInfo(FSQL, FTablesInfo);
end;

procedure TCRVirtualCommand.InternalCreateTables;
var
  i: integer;
  ObjectInfo: TSQLObjectInfo;
  Conn: TCRVirtualConnection;
begin
  Conn := TCRVirtualConnection(GetConnection);

  if not Conn.FLockCreateTables or (Conn.FTablesCreated < (Conn.FModule.FTables.Count - 1)) then begin

    if Conn.FTableCreationMode = cmOnDemand then begin
      InternalCreateTablesInfo;
      for i := 0 to FTablesInfo.Count - 1 do begin
        SQLInfo.SplitObjectName(FTablesInfo[i].TableNameFull, ObjectInfo);
        Conn.DoCreateTable(ObjectInfo.Catalog, ObjectInfo.Name);
      end;
    end
    else
      for i := Conn.FTablesCreated to Conn.FModule.FTables.Count - 1 do
        Conn.DoCreateTable(TSQLiteVirtualTable(Conn.FModule.FTables[i]).FSchemaName, TSQLiteVirtualTable(Conn.FModule.FTables[i]).FTableName);

    Conn.FLockCreateTables := True;
  end;
end;

procedure TCRVirtualCommand.InternalDropTables;
var
  i: integer;
  ObjectInfo: TSQLObjectInfo;
  Conn: TCRVirtualConnection;
begin
  Conn := TCRVirtualConnection(GetConnection);

  if (Conn.FTableCreationMode = cmOnDemand) and not Conn.FLockDropTables then begin
    inherited;

    for i := 0 to FTablesInfo.Count - 1 do begin
      SQLInfo.SplitObjectName(FTablesInfo[i].TableNameFull, ObjectInfo);
      TCRVirtualConnection(GetConnection).DoDropTable(ObjectInfo.Catalog, ObjectInfo.Name);
    end;

    Conn.FLockCreateTables := False;
    Conn.FTablesCreated := 0;
  end;
end;

procedure TCRVirtualCommand.InternalExecute;
begin
  InternalCreateTables;

  inherited;
end;

class function TCRVirtualCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TCRVirtualSQLInfo;
end;

class function TCRVirtualCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TVirtualMapRules;
end;

{ TCRVirtualRecordSet }

procedure TCRVirtualRecordSet.InternalCreateTables;
begin
  TCRVirtualCommand(FCommand).InternalCreateTables;
end;

procedure TCRVirtualRecordSet.InternalDropTables;
begin
  TCRVirtualCommand(FCommand).InternalDropTables;
end;

procedure TCRVirtualRecordSet.CreateCommand;
begin
  SetCommand(TCRVirtualCommand.Create);
end;

procedure TCRVirtualRecordSet.InternalPrepare;
begin
  try
    InternalCreateTables;

    inherited;
  except
    on E: Exception do begin
      if TCRVirtualConnection(GetConnection).FTablesCreated > 0 then
        InternalDropTables;

      raise;
    end;
  end;
end;

procedure TCRVirtualRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    if not DisableInitFields then
      InternalCreateTables;

    inherited;
  except
    on E: Exception do begin
      if TCRVirtualConnection(GetConnection).FTablesCreated > 0 then
        InternalDropTables;

      raise;
    end;
  end;
end;

procedure TCRVirtualRecordSet.InternalClose;
begin
  inherited;

  InternalDropTables;
end;

procedure TCRVirtualRecordSet.DoBeforeFetch(out Cancel: boolean);
var
  i: integer;
  Conn: TCRVirtualConnection;
begin
  inherited;

  if not FLockDisableControls then begin
    Conn := TCRVirtualConnection(GetConnection);
    for i := 0 to Conn.FModule.FTables.Count - 1 do
      TSQLiteVirtualTable(Conn.FModule.FTables[i]).FData.DisableControls(False);
  end;
end;

procedure TCRVirtualRecordSet.DoAfterFetch;
var
  i: integer;
  Conn: TCRVirtualConnection;
begin
  inherited;

  Conn := TCRVirtualConnection(GetConnection);
  for i := 0 to Conn.FModule.FTables.Count - 1 do
    TSQLiteVirtualTable(Conn.FModule.FTables[i]).FData.EnableControls;
end;

function TCRVirtualRecordSet.GetDBType(const SQLTypeName: string; var Len, Scale: integer): word;
var
  TempDataType: word;
  TempLen, TempScale: integer;
begin
  Result := TVirtualConverterManager.GetDBType(SQLTypeName);

  if ((Result = vqTime) or
      (Result = vqDateTime) or
      (Result = vqSQLTimeStamp)) and
     (Scale = -1)
  then begin
    Scale := Len;
    Len := -1;
  end;

  if Result = vqUnknown then begin
    if TCRVirtualConnection(GetConnection).GetFieldDataType(SQLTypeName, TempDataType, TempLen, TempScale) then begin
      Result := TVirtualConverterManager.DataTypeToDBType(TempDataType);
      Len := TempLen;
      Scale := TempScale;
    end;
  end;
end;

function TCRVirtualRecordSet.GetDBType(SQLType: integer): word;
begin
  Result := TVirtualConverterManager.GetDBType(SQLType);
end;

function TCRVirtualRecordSet.GetDBUnknown: word;
begin
  Result := vqUnknown;
end;

function TCRVirtualRecordSet.IsFixedDBType(DBType: word): boolean;
begin
  Result := (DBType = vqFixedChar) or (DBType = vqFixedWideChar);
end;

function TCRVirtualRecordSet.GetDataType(DBType: word): word;
begin
  if DBType = vqNumeric then begin
    if TCRVirtualCommand(FCommand).EnableFMTBCD or TCRVirtualConnection(GetConnection).EnableFMTBCD then begin
      Result := dtFMTBCD;
    end
    else
    if TCRVirtualCommand(FCommand).EnableBCD or TCRVirtualConnection(GetConnection).EnableBCD then begin
      Result := dtBCD;
    end
    else
      Result := dtFloat;
  end
  else
    Result := TVirtualConverterManager.GetDataType(DBType);
end;

procedure TCRVirtualRecordSet.Reopen;
var
  i: integer;
  Conn: TCRVirtualConnection;
begin
  Conn := TCRVirtualConnection(GetConnection);
  for i := 0 to Conn.Tables.Count - 1 do begin
    TSQLiteVirtualTable(Conn.Tables[i]).FData.DisableControls(True);
    TSQLiteVirtualTable(Conn.Tables[i]).FData.ClearLocalIndexes;
  end;

  FLockDisableControls := True;
  try
    inherited;
  finally
    FLockDisableControls := False;
  end;
end;

procedure TCRVirtualRecordSet.SetSQL(const Value: string);
begin
  inherited;

  GetCommand.CommandType := ctUnknown;
end;

{$IFDEF OFS}
initialization
  OFSFileName := 'D:\VirtualQuery.log';
{$ENDIF}

end.
