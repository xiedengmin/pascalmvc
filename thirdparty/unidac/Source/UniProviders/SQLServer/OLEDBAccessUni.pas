
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access via OLE DB
//////////////////////////////////////////////////


{$I Sdac.inc}

unit OLEDBAccessUni;

interface

{$IFNDEF MSWINDOWS}
  Error // unit shouldn't be used
{$ENDIF}

uses
  CLRClasses,
  Windows, Classes,
{$IFNDEF FPC}
  DBConsts,
{$ENDIF}
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
  FMTBcd, DAConsts, SysUtils, ActiveX,
{$IFNDEF BCB}{$IFNDEF FPC}
  {$NOINCLUDE ActiveX}
{$ENDIF}{$ENDIF}
  SyncObjs, CRTypes, CRProps, CRNumeric, CRParser, CRAccess, MemData,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OLEDBC, OLEDBIntf, SqlClasses, MSConsts, MSClasses,
{$ELSE}
  OLEDBCUni, OLEDBIntfUni, SqlClassesUni, MSConstsUni, MSClassesUni,
{$ENDIF}
{$IFNDEF LITE}
  MTSCall,
{$IFNDEF UNIDACPRO}MSUDT{$ELSE}MSUDTUni{$ENDIF},
{$ENDIF}
{$IFNDEF BCB}{$IFNDEF FPC}
  {$NOINCLUDE ComObj}
{$ENDIF}{$ENDIF}
  ComObj, Contnrs;

{$IFDEF VER24P}
  {$HPPEMIT '#include <WinAPI.OleDB.hpp>'}
{$ENDIF}
{$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
{$HPPEMIT '#undef MANAGED_INTERFACE_OPERATORS'}
{$HPPEMIT '#endif'}

const
  OLE_DB_INDICATOR_SIZE = sizeof(DBSTATUS);

type
{ TOLEDBConnector }

  EOLEDBError = class;
  EMSError = class;

  // ----------
  // Must be declared before(!) TOLEDBConnector declaration to prevent CBuilder compiler bug (pas -> hpp)

  {
  AccessorBlock types:
    1. Ordinary. May contain many ordinary fields and only one BLOB field
      1.1. For DefaultResultSet or for CursorUpdate = False. All fields in block must be not ReadOnly
      1.2. In other cases ReadOnly and not ReadOnly fields may contains in one AccessorBlock
    2. ReadOnly. Used only for KeySet and Dynamic if CursorUpdate is True. All fields in block must be ReadOnly
    3. BLOB. May contain only one BLOB field
    4. FetchBlock. Used for dtVariant and dtMemo (not BLOB) fields

    RecordSet may contain only one each of accessor types, and some BLOB accessors if need
  }

  TAccessorBlockType = (abOrdinary, abReadOnly, abBLOB, abFetchBlock);
  TAccessorBlock = record
    BlockType: TAccessorBlockType;
    hAcc: HACCESSOR; // OLE DB accessor
    BlobField: TFieldDesc; // nil, if BLOB field not avaible. This member useful to fetch BLOB streams
    Fields: array of TFieldDesc;
  end;

  TOLEDBRecordSet = class;
  TRestrictions = array of OleVariant;

  TAnalyzeMethod = function(const Status: HRESULT; const Arg: IntPtr = nil): WideString of object;

  TOLEDBConnector = class(TCRConnector)
  private
    function GetConnection: TMSSQLConnection;
  protected
  { DataSource }
    FIDBInitialize: IDBInitialize;
    FIDBProperties: IDBProperties;
    FIDBCreateSession: IDBCreateSession;

  { Session }
    FISessionProperties: ISessionProperties;
  {$IFNDEF LITE}
    FITransactionJoin: ITransactionJoin;
  {$ENDIF}

    FProviderVer: string;
    FProviderMajorVer: integer;
    FProviderFriendlyName: string;
    FProviderId: TGuid;
    FIsOutputLOBSupported: boolean;

    FColumnsMetaInfo: TOLEDBRecordSet;

    procedure ReleaseInterfaces;
    function GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
    procedure GetConnectionProperties;
    procedure SetConnectionProperties;
    procedure SetSessionProperties;
    procedure SetDataSourceProperties;

    function CheckColumnsMetaInfo: TOLEDBRecordSet;

    procedure CreateCompactDatabase;

    procedure Check(const Status: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
    procedure OLEDBError(const ErrorCode: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);

    function IsOutputLOBSupported: boolean;

  public
    procedure Connect; override;
    procedure Disconnect; override;

    procedure SetDatabase(const Value: string); override;

    class function CloneException(E: Exception): Exception; override;

    function GetClientVersion: string; override;
    function GetClientVersionFull: string; override;
    function GetClientMajorVersion: integer; override;

  {$IFNDEF LITE}
    procedure Enlist(Transaction: TMTSTransaction); override;
    procedure UnEnlist(Transaction: TMTSTransaction); override;
  {$ENDIF}
  end;

{ TOLEDBCommand }

  TOLEDBStream = class;

  TIntPtrDynArray = array of IntPtr;

  TParamsAccessorData = record
    Accessor: IAccessor;
    ExecuteParams: DBPARAMS;
    rgBindings: TDBBindingArray;
  end;

  TOLEDBCommand = class(TSqlCommand)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TMSSQLConnection;

    TmpPiece: PPieceHeader;
    FStoredProcName: string;

    procedure DescribeSPParams(MasterDatabase: boolean; out OriginalDatabase: string);
    function GetConnector: TOLEDBConnector;

  protected
    FQueryIntCnt: integer; // Quantity of calls to QueryInterfaces
    FRPCCall: boolean;
    FPrepared: boolean;

    // If statement executed w/o errors and warnings then FLastExecWarning is False
    // If statement executed with warning then FLastExecWarning is True
    // If statement executed with error then raising exception
    // Used to analyze CursorType changes in RecordSet
    FLastExecWarning: boolean;

  { Output Stream }
    FOutputStream: TStream;
    FOLEDBStream: TOLEDBStream;
    FOutputEncoding: TMSOutputEncoding;

  { Query Notification }
    FNotification: boolean;
    FDelayedSubsciption: boolean;
    FNotificationMessage: string;
    FNotificationService: string;
    FNotificationTimeout: integer;

  { NonBlocking}
    FISSAsynchStatus: ISSAsynchStatus;

  { Command }
    FICommandText: ICommandText;
    FICommandPrepare: ICommandPrepare;
    FICommandProperties: ICommandProperties;

  { Rowset }
    FRequestMultipleResults: boolean; // True for ctDefaultResultSet only
    FIUnknown: IUnknown; // If requested then must be setted to a nil as it possible
    FIUnknownNext: IUnknown;
    FIMultipleResults: IMultipleResults;
    FIAbort: ISSAbort;

  { Params }
    FParamsAccessorData: TParamsAccessorData;
    FParamsAccessorDataAvaible: boolean;

  { Rows }
    FRowsAffected: NativeInt;
    FRowsAffectedNext: NativeInt;

    FNextResultRequested: boolean; // FIUnknownNext getted from OLE DB

    procedure DoExecuteException(Sender: TObject; E: Exception; var Fail: boolean); override;
    procedure WaitAsynchCompletion; override;
    procedure CancelCommand; override;

    procedure EndExecute(E: Exception); override;

    procedure Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil);
    procedure QueryInterfaces(const QueryPrepare: boolean); // QueryPrepare must be True to request IID_ICommandPrepare
    procedure ReleaseInterfaces;
    procedure ClearIMultipleResults;

    procedure SetCommandProp;
    procedure SetParameterInfo;
    procedure GetParameterInfo;

    procedure SetOutputStream(Stream: TStream);
    function GetUseOutputStream: boolean;

  { Params }
    procedure CreateAndFillParamAccs;
    procedure RequestAndFreeParamAccs;
    procedure RequestParamsIfPossible; // Call RequestAndFreeParamAccs if interfaces is cleared

    procedure ConvertStreamToBlob(pValue: IntPtr; Size: integer; Blob: TBlob; IsUnicodeField: Boolean; OmitXMLPreamble: boolean = False);

    function Analyze(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
    procedure GetNextResult(out ResultSet: IUnknown; out RowsAffected: NativeInt);

    function GetRowsAffected: NativeInt; override;
    procedure SetRowsAffected(Value: NativeInt); override;

    class function InternalGetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
      ParamsInfo: TDAParamsInfo; Iters: integer): string; override;
    function NeedPrepareOnBatch: boolean; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    procedure SetConnection(Value: TCRConnection); override;
    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;
    procedure Execute; override;
    procedure Close; override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

{ TOLEDBRecordSet }

  TFetchAccessorData = record
    Accessor: IAccessor;
    AccessorBlocks: array of TAccessorBlock;
  end;

  TOLEDBRecordSet = class(TSqlRecordSet)
  private
    FUniqueRecords: Boolean;
    FHideSystemUniqueFields: Boolean;
    FWideStrings: Boolean;
    FWideMemos: Boolean;
    FDisableMultipleResults: Boolean;
    FCursorType: TMSCursorType;
    FBaseTableName: string;

  {$IFNDEF LITE}
    FFldCatalogNameIdx, FFldSchemaNameIdx, FFldTableNameIdx, FFldColumnNameIdx,
    FFldFieldNameIdx, {FFldActualFieldNameIdx,} FFldPrecisionIdx, FFldScaleIdx, FFldGuidIdx: integer;
    FFldColumnNumberIdx, FFldIsAutoIncIdx, FFldTypeIdx, FFldFlagsIdx, FFldColumnSizeIdx, FFldComputeModeIdx: integer;
    FFldXMLSchemaCollCatalogNameIdx, FFldXMLSchemaCollSchemaNameIdx, FFldXMLSchemaCollNameIdx: integer;
    FFldAssemblyTypenameIdx, FFldUDTNameIdx, FFldUDTSchemanameIdx, FFldUDTCatalognameIdx: integer;
  {$ENDIF}

    FClientMajorVer: integer;
    FProvider: TMSProvider;

    FRowArray: THROWArray;
    FPRowArray: PHROWArray;
    FRowsObtained: NativeUInt;
    FCursorTypeChanged: TCursorTypeChangedProc;

    function GetClientMajorVer: integer;
    function GetProvider: TMSProvider;
  {$IFDEF LITE}
    function GetBCDPrecision: integer;
    function GetBCDScale: integer;
  {$ENDIF}

    function ConvertDBCOLUMNINFOToFieldDesc(
      const FieldName: string;
      const ActualFieldName: string;
      const FieldNo: DBORDINAL;
      const OLEDBType: DBTYPE;
      const dwFlags: DBCOLUMNFLAGS;
      const IsAutoIncrement: boolean;
      const Precision: integer;
      const Scale: integer;
      const ColumnSize: DBLENGTH;
      Field: TMSFieldDesc
      ): boolean;
  {$IFNDEF LITE}
    procedure CheckColumnsMetaInfoIdx;
    procedure CreateFieldDescsByRowset(IUnk: IUnknown);
  {$ENDIF}
    procedure CreateFieldDescsByInfo(IUnk: IUnknown);

  protected
    FNativeRowset: boolean; // For non-native rowsets output parameters not supported

    FCommand: TOLEDBCommand;
    FFetchAccessorData: TFetchAccessorData;
    FHRowAccessible: boolean; // True, if FHRow is setted to valid value

  { Rowset }
    FIRowset: IRowset;
    FIRowsetLocate: IRowsetLocate;
    FIRowsetUpdate: IRowsetChange;

    FRequestSQLObjects: boolean;

    // Bookmarks
    FBookmarkField: TMSFieldDesc;

    FLastFetchOK: boolean; // True, if previous Fetch was called succesfulity (Result is True)
    FLastFetchEnd: boolean; // True, If previous FIRowset.GetNextRows has return DB_S_ENDOFROWSET

    FSchema: TGUID;
    FRestrictions: TRestrictions;

    FPopulatingKeyInfo: boolean;
    FBulkExecuting: boolean;
    FIUnknownIsAssigned: boolean; // True, if FIUnknown was assigned with AssignToNextResult

    class function ExceptionIsCanceled(E: Exception): boolean; override;

    function GetWideMemos: boolean;
    function CursorTypeForFetching: TMSCursorType;
    function IsServerCursor: boolean; override;
    function IsStaticCursor: boolean; override;
    function IsDynamicCursor: boolean; override;
    function IsWideField(Field: TFieldDesc): Boolean; override;

    procedure ClearHRow;

    function UseExtNumeric: boolean; virtual;
    function UseExtTimeStamp: boolean; virtual;
    procedure Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil); virtual;
    procedure CheckBCDOverflow(Field: TFieldDesc; RecBuf: IntPtr);
    function AnalyzeFieldsStatus(const Status: HRESULT; const Arg: IntPtr = nil): WideString;

    procedure QueryCommandInterfaces(const QueryPrepare: boolean); // Create ConnectionSwap if need. Call FCommand.QueryInterfaces.
    procedure ReleaseCommandInterfaces;
    procedure QueryRecordSetInterfaces; // Reqests IRowset, IRowsetLocate, IRowsetUpdate
    procedure ReleaseRecordSetInterfaces;
    procedure ReleaseAllInterfaces;

    function GetCommandClass: TSqlCommandClass; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open / Close }
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fields }
    procedure CreateFieldDescs; override;
    function GetIndicatorItemSize: Integer; override;

  { Fetch }
    function IsNeedFetchBlock(FieldDesc: TFieldDesc): boolean;
    procedure IncFetchBlockOffset(var FetchBlockOffset: integer; Field: TFieldDesc);
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); override;
    function ReadNextRows(FetchRows: integer; FetchBack: boolean): NativeUInt;
    function FetchingAccessible(FetchBack: boolean): boolean; override;
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    procedure ProcessNoResult(FetchBack: boolean); override;
    function ProcessFetchedException(E: Exception): boolean; override;
    procedure AllocFetchBuffer; override;
    procedure FreeFetchBuffer; override;

  { Modify }
    procedure RowsetUpdateCommit;
    procedure RowsetUpdateRollback;
    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;
    procedure InternalAppendOrUpdate(RecBuf: IntPtr; IsAppend: boolean);
    function FieldModified(FieldDesc: TFieldDesc; OldRecBuf, NewRecBuf: IntPtr): boolean;

    procedure SetCommandProp;

    procedure RequestParamsIfPossible; // Call FCommand.RequestAndFreeParamAccs if interfaces is cleared

    //PreCached FConection properties
    property ClientMajorVer: integer read GetClientMajorVer;
    property Provider: TMSProvider read GetProvider;
  public
    constructor Create; override;
    destructor Destroy; override;

  { Open / Close }
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    function GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
    procedure Disconnect; override;

  { Fetch }
    function CanDisconnect: boolean; override;

  { Fields }
    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
    function GetStatus(Field: TFieldDesc; RecBuf: IntPtr): DWORD;
    procedure SetStatus(Field: TFieldDesc; RecBuf: IntPtr; Value: DWORD);
    procedure GetField(Field: TFieldDesc; RecBuf: IntPtr; Dest: IntPtr; out DestLen: Word; NeedConvert: boolean; out IsBlank: boolean); override;
    procedure GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False); override;

    function RowsReturn: boolean; override;

    function IsMetaInfo: boolean; override;
    function GetIRowset: IRowset;
    procedure SetIRowset(Rowset: IRowset);

    procedure AssignToNextResult(Dest: TSqlRecordSet); override;
    function HasNextResult: boolean; override;
    function CheckNextResult: boolean; override;

    function GetFieldDescType: TFieldDescClass; override;
    function NeedGetRecordAfterGotoBookmark: boolean; override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property CursorTypeChanged: TCursorTypeChangedProc read FCursorTypeChanged write FCursorTypeChanged;
    property NativeRowset: boolean read FNativeRowset;
  end;

  TOLEDBTableTypeRecordSet = class(TOLEDBRecordSet)
  protected
    FTableTypeName: string;

    function UseExtNumeric: boolean; override;
    function UseExtTimeStamp: boolean; override;
    procedure OpenTVPRowset;
    function GetFilledRowset: IRowset;

    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalPrepare; override;
    function FetchingAccessible(FetchBack: boolean): boolean; override;

  {$IFNDEF LITE}
    procedure GetFieldDescsByRowset(IUnk: IUnknown);
  {$ENDIF}
    procedure CreateFieldDescs; override;
  public
    function IsFullReopen: boolean; override;
    procedure ExplicitInitFields; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

{ TOLEDBTransaction }

  TOLEDBTransaction = class(TCRTransaction)
  private
    FITransactionLocal: ITransactionLocal;

    procedure ReleaseInterfaces;

  public
    destructor Destroy; override;

    procedure AssignConnect(Source: TCRTransaction); override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;

    procedure Check(const Status: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil); virtual;
  end;

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

{$IFNDEF LITE}
{ TOLEDBMetaData }

  TOLEDBMetaData = class (TMSSQLMetaData)
  private
    procedure CopyTablesData(Restrictions: TStrings);
    procedure CopyColumnsData(Restrictions: TStrings);
    procedure CopyProcedureParametersData(Restrictions: TStrings);
    procedure CopyIndexColumnsData(Restrictions: TStrings);
    procedure CopyConstraintsData(Restrictions: TStrings);
    procedure CopyDatabasesData(Restrictions: TStrings);

  protected
    function CreateRecordSet: TCRRecordSet; override;
    function RequestIRowset(const MetaDataType: TMSMetaDataType; Restrictions: TStrings): TData; override;

    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
    function GetDatabases(Restrictions: TStrings): TData; override;
  end;

{$ENDIF}

{ TOLEDBLoader }

  TDBIDAccessor = class
  protected
    FPDBID: PDBID;
    function GeteKind: DBKIND;
    procedure SeteKind(Value: DBKIND);
    function GetpwszName: IntPtr;
    procedure SetpwszName(Value: IntPtr);
    procedure SetPDBID(Value: PDBID);
  public
    constructor Create(APDBID: PDBID);
    property eKind: DBKIND read GeteKind write SeteKind;
    property pwszName: IntPtr read GetpwszName write SetpwszName;
  end;

  TOLEDBLoader = class(TCRLoader)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TMSSQLConnection;

    FIOpenRowset: IOpenRowset;
    FIRowsetFastLoad: IRowsetFastLoad;
    FParamsAccessorData: TParamsAccessorData;
    FParamDescs: TParamDescs;
    FBlobList: TObjectList;

    FKeepIdentity: boolean;
    FKeepNulls: boolean;
    FRowsPerBatch: integer;
    FKilobytesPerBatch: integer;
    FLockTable: boolean;
    FCheckConstraints: boolean;
    FFireTrigger: boolean;

  protected
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;

    procedure LoadRow;
    procedure DoAfterLoadRow;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Prepare; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TOLEDBPropertiesSet }

  TOLEDBPropertiesSet = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnector: TOLEDBConnector;
    FPropSet: PDBPropSet;

    function GetPropCount: UINT; virtual;
    procedure SetPropCount(Value: UINT); virtual;

    procedure Check(const Status: HRESULT);
    function GetInitPropSetStatus: string;
    function GetDBPropPtr(Index: UINT): PDBProp; virtual;
    function InitProp(const dwPropertyID: DBPROPID; const Required: boolean = False): PDBProp;

    procedure Init(const GuidPropertySet: TGUID); virtual;
    procedure Clear; virtual;

  public
    constructor Create(Connector: TOLEDBConnector; const GuidPropertySet: TGUID);
    destructor Destroy; override;

    procedure AddPropSmallInt(const dwPropertyID: DBPROPID; const Value: smallint);
    procedure AddPropInt(const dwPropertyID: DBPROPID; const Value: integer);
    procedure AddPropUInt(const dwPropertyID: DBPROPID; const Value: Cardinal);
    procedure AddPropBool(const dwPropertyID: DBPROPID; const Value: boolean; const Required: boolean = False);
    procedure AddPropStr(const dwPropertyID: DBPROPID; const Value: string; const Required: boolean = False);
    procedure AddPropIntf(const dwPropertyID: DBPROPID; const Value: TInterfacedObject; const Required: boolean = False);

    procedure SetProperties(Obj: IDBProperties); overload;
    procedure SetProperties(Obj: ISessionProperties); overload;
    procedure SetProperties(Obj: ICommandProperties); overload;

    property InitPropSet: PDBPropSet read FPropSet;
    property PropCount: UINT read GetPropCount write SetPropCount;
  end;

{ TOLEDBParamPropertiesSet }

  TOLEDBParamPropertiesSet = class(TOLEDBPropertiesSet)
  protected
    FDBProp: PDBPropArray;
    FSSParamProp: PSSParamPropsArray;
    FSSParamsCount: UINT;

    function GetPropCount: UINT; override;
    procedure SetPropCount(Value: UINT); override;

    function GetDBPropPtr(Index: UINT): PDBProp; override;
    procedure Init(const GuidPropertySet: TGUID); override;
    procedure Clear; override;

  public
    procedure AddProp(const dwPropertyID: DBPROPID; const Value: OleVariant; const Ordinal: DBORDINAL);
    function SetParameterProperties(Obj: ISSCommandWithParameters): HResult;
  end;

{ TOLEDBPropertiesGet }

  TPropValues = array of Variant;

  TOLEDBPropertiesGet = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnector: TOLEDBConnector;
    FPropSet: PDBPropSet;
    FPropIds: array of DBPROPID;
    FPropIdsGC: IntPtr;

    procedure Check(const Status: HRESULT);
    function GetDBPropPtr(rgProperties: PDBPropArray; Index: UINT): PDBProp;

    procedure PrepareToGet;
    procedure ProcessResult(rgPropertySets: PDBPropSet; out PropValues: TPropValues);
    procedure ClearResult(rgPropertySets: PDBPropSet);

  public
    constructor Create(Connector: TOLEDBConnector; const GuidPropertySet: TGUID);
    destructor Destroy; override;

    procedure AddPropId(Id: DBPROPID);

    procedure GetProperties(Obj: IDBProperties; out PropValues: TPropValues); overload;
    procedure GetProperties(Obj: IRowsetInfo; out PropValues: TPropValues); overload;
    procedure GetProperties(Obj: ICommandProperties; out PropValues: TPropValues); overload;
  end;

{ TOLEDBErrors }

  TOLEDBErrors = class
  protected
    FList: TList;

    function GetCount: integer;
    function GetError(Index: Integer): EOLEDBError;

    procedure Assign(Source: TOLEDBErrors);
    procedure Add(Source: TOLEDBErrors);
    procedure Clear;

  public
    constructor Create;
    destructor Destroy; override;

    property Count: integer read GetCount;
    property Errors[Index: Integer]: EOLEDBError read GetError; default;
  end;

{ EOLEDBError }

{$IFDEF NODBACCESS}
  EOLEDBError = class(ECRError)
{$ELSE}
  EOLEDBError = class(EDAError)
{$ENDIF}
  protected
    FErrors: TOLEDBErrors;
    FOLEDBErrorCode: integer;

    // GetBasicErrorInfo - ERRORINFO struct
    // FhrError: HResult; - equal to FOLEDBErrorCode
    // FMinor: UINT; - only for EMSError and equal to FMSSQLErrorCode
    // Fclsid: TGUID; - always CLSID_SQLOLEDB
    Fiid: TGUID;
    // Fdispid: Integer; - always 0

    // GetErrorInfo - IErrorInfo interface
    // Fguid: TGUID; - same as Fiid
    // FSource: WideString; - always "Microsoft OLE DB Provider for SQL Server"
    // FDescription: WideString; - equal to Message
    // FHelpFile: WideString; - not used by sqloledb
    // FHelpContext: Integer; - not used by sqloledb

    FMessageWide: WideString;

    function GetErrorCount: integer;
    function GetError(Index: Integer): EOLEDBError;

    procedure Assign(Source: EOLEDBError); virtual;

  protected
    property iid: TGUID read Fiid;

  public
    constructor Create(ErrorCode: integer; const Msg: WideString);
    destructor Destroy; override;

    property ErrorCount: integer read GetErrorCount;
    property Errors[Index: Integer]: EOLEDBError read GetError; default;

    property OLEDBErrorCode: integer read FOLEDBErrorCode;
    property MessageWide: WideString read FMessageWide;
    // property hrError: HResult read FhrError;
    // property Minor: UINT read FMinor;
    // property clsid: TGUID read Fclsid;
    // property iid: TGUID read Fiid; - protected
    // property dispid: Integer read Fdispid;
    // property Source: WideString read FSource;
    // property Description: WideString read FDescription;
  end;

{ EMSError }

  EMSError = class(EOLEDBError)
  protected
    FMSSQLErrorCode: integer;
    FServerName: string;
    FProcName: string;
    FState: BYTE;
    FSeverityClass: BYTE;
    FLineNumber: WORD;
    FLastMessage: string;

    procedure Assign(Source: EOLEDBError); override;

  public
    constructor Create(
      const pServerErrorInfo: SSERRORINFO;
      OLEDBErrorCode: integer;
      Msg: WideString); overload;

  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}
    property MSSQLErrorCode: integer read FMSSQLErrorCode;
    property ServerName: string read FServerName;
    property ProcName: string read FProcName;
    property State: BYTE read FState;
    property SeverityClass: BYTE read FSeverityClass;
    property LineNumber: WORD read FLineNumber;

    property LastMessage: string read FLastMessage;
  end;

  TStorageType = (stBlob, stStream);

  TOLEDBStream = class(TInterfacedObject, ISequentialStream)
  protected
    FSize: UINT;
    FBlob: TBlob;
    FStream: TStream;
    FStorageType: TStorageType;
    FPosInBlob: cardinal;
    FStreamList: TList;
    procedure InitStream(StreamList: TList);
  public
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT; stdcall;
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT; stdcall;
  {$ENDIF}
  public
    constructor Create(Blob: TBlob; StreamList: TList); overload;
    constructor Create(Stream: TStream); overload;
    destructor Destroy; override;

    property Size: UINT read FSize; // Return size of stream in bytes
  end;

  function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

  procedure QueryIntf(Source: IUnknown; const IID: TGuid; out Intf); // OLE QueryInterface analog

{$IFDEF DEBUG}
var
  StreamCnt: integer = 0;
{$ENDIF}

var
  __UseRPCCallStyle: boolean; // temporary
  ParamsInfoOldBehavior: boolean; // do not remove (cr-s23142)
  UniqueFieldNamePrefix: string = '__COLUMN';

//procedure AddInfoToErr(var S: WideString; const FormatStr: string; const Args: array of const); overload; // Add additional info to exception message
procedure AddInfoToErr(var E: Exception; const FormatStr: string; const Args: array of const); overload; // Add additional info to exception message

{$IFDEF FPC}
const
  OLEDLLName = 'oleaut32.dll';
var
  hOleLib: HMODULE = 0;
  DispGetParam: function(const dispparams: TDispParams; position: Integer;
    vtTarg: TVarType; var varResult: TVarData; var puArgErr: Integer): HResult; {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
{$ENDIF}

type
  TOpenSqlFilestream = function (
    FilestreamPath: PWideChar;
    DesiredAccess: TOleEnum; //SQL_FILESTREAM_DESIRED_ACCESS;
    OpenOptions: ULONG;
    FilestreamTransactionContext: PBYTE;
    FilestreamTransactionContextLength: DWORD; //SIZE_T
    AllocationSize: Pointer //PInt64
  ): HResult; stdcall;

var
  OpenSqlFilestream: TOpenSqlFilestream;

procedure LoadNCLib;
function LocaleIdentifierToStr(LCID: Cardinal): string;
function LocaleIdentifierToCardinal(const LCID: string): Cardinal;
procedure GetLocaleIdentifierList(List: TStrings);

implementation

uses
  Math, {$IFDEF VER17P}Types,{$ENDIF}
  DateUtils, {$IFNDEF FPC}SqlTimSt,{$ENDIF} Variants,
{$IFDEF FPC}
  VarUtils,
{$ENDIF}
  CRFunctions, MemUtils,
{$IFNDEF UNIDACPRO}
  MSProps, MSParser{$IFNDEF LITE}, MSDataTypeMap{$ENDIF};
{$ELSE}
  MSPropsUni, MSParserUni, MSDataTypeMapUni;
{$ENDIF}

const
  sqlncli10 = 'sqlncli10.dll';
  sqlncli11 = 'sqlncli11.dll';
var
  hSqlncliLib: HMODULE = 0;

{$IFDEF FPC}
function OleNotLink: integer;
begin
  raise Exception.Create('Ole function is not linked');
  Result := 0;
end;

procedure LoadOleLib;
  procedure AssignProc(var Proc: pointer; const Name: string);
  begin
  {$IFDEF MSWINDOWS}
    Proc := GetProcAddress(hOleLib, PChar(Name));
  {$ELSE}
    Proc := dlsym(hOleLib, PAnsiChar(AnsiString(Name)));
  {$ENDIF}
    if Proc = nil then
      Proc := @OleNotLink;
  end;
begin
  hOleLib := LoadLibrary(OLEDLLName);
  if hOleLib = 0 then
    raise Exception.Create('Cannot load ' + OLEDLLName);
  AssignProc(@DispGetParam, 'DispGetParam');
end;

procedure FreeOleLib;
begin
  if hOleLib <> 0 then begin
    FreeLibrary(hOleLib);
    hOleLib := 0;
  end;
end;
{$ENDIF}

function NCNotLink: integer;
begin
  raise Exception.Create(SMSSQLNotFound);
{$IFNDEF VER16P}
  Result := 0;
{$ENDIF}
end;

procedure LoadNCLib;
  function GetProc(hModule: HMODULE; ProcName: string): FARPROC;
  begin
    Result := GetProcAddress(hModule, PChar(ProcName));
    if Result = nil then
      Result := @NCNotLink;
  end;

begin
  if hSqlncliLib = 0 then begin
    hSqlncliLib := LoadLibrary(PChar(sqlncli11));
    if hSqlncliLib = 0 then
      hSqlncliLib := LoadLibrary(PChar(sqlncli10));
    if hSqlncliLib > 0 then
      OpenSqlFilestream := GetProc(hSqlncliLib, 'OpenSqlFilestream');
  end;
end;

procedure FreeNCLib;
begin
  if hSqlncliLib > 0 then begin
    FreeLibrary(hSqlncliLib);
    hSqlncliLib := 0;
  end;
end;

var
  dstSmallint: IntPtr;
  dstInt: IntPtr;
  dstReal: IntPtr;
  dstFloat: IntPtr;
  dstMoney: IntPtr;
  dstDateTime: IntPtr;
  dstNVarChar: IntPtr;
  dstNVarCharMax: IntPtr;
  dstVarChar: IntPtr;
  dstVarCharMax: IntPtr;

  dstBit: IntPtr;
  dstTinyInt: IntPtr;
  dstBigint: IntPtr;
  dstSql_variant: IntPtr;
  dstImage: IntPtr;
  dstBinary: IntPtr;
  dstVarBinary: IntPtr;
  dstGuid: IntPtr;
  dstXML: IntPtr;
  dstTable: IntPtr;

var
  IsWindowsVista: boolean;
  GlobaIMalloc: IMalloc;
  LocaleIdentifierList: TStringList = nil;
  LocalesCallbackDelegate: {$IFDEF FPC}TFarProc{$ELSE}TFNLocaleEnumProc{$ENDIF};

function GetLocaleData(ID: LCID; Flag: DWORD): string;
var
  BufSize: Integer;
begin
  BufSize := GetLocaleInfo(ID, Flag, nil, 0);
  SetLength(Result, BufSize);
  GetLocaleInfo(ID, Flag, PChar(Result), BufSize);
  SetLength(Result, BufSize - 1);
end;

function LocalesEnumProc(Locale: PChar): integer; stdcall;
var
  LCID: Cardinal;
begin
  LCID := StrToInt('$' + Copy(Locale, 5, 4));
  Assert(LocaleIdentifierList <> nil);
  LocaleIdentifierList.AddObject(GetLocaleData(LCID, LOCALE_SLANGUAGE), TObject(LCID));
  Result := 1;
end;

procedure CheckLocaleIdentifierList;
begin
  if LocaleIdentifierList = nil then begin
    LocaleIdentifierList := TStringList.Create;
    LocaleIdentifierList.CaseSensitive := False;
    LocaleIdentifierList.Duplicates := dupIgnore;
    LocaleIdentifierList.Sorted := True;
    LocalesCallbackDelegate := @LocalesEnumProc;
    EnumSystemLocales(LocalesCallbackDelegate, LCID_INSTALLED);
  end;
end;

function LocaleIdentifierToStr(LCID: Cardinal): string;
var
  k: integer;
begin
  CheckLocaleIdentifierList;
  k := LocaleIdentifierList.IndexOfObject(TObject(LCID));
  if k <> -1 then
    Result := LocaleIdentifierList[k]
  else
    raise Exception.Create(SLocaleIdentifierUnknown);
end;

function LocaleIdentifierToCardinal(const LCID: string): Cardinal;
var
  e, k: Integer;
  _lcid: Cardinal;
begin
  CheckLocaleIdentifierList;
  Val(LCID, _lcid, e);
  if e = 0 then
    Result := _lcid
  else begin
    k := LocaleIdentifierList.IndexOf(LCID);
    if k = -1 then
      raise Exception.Create(SLocaleIdentifierUnknown)
    else
      Result := Cardinal(LocaleIdentifierList.Objects[k]);
  end;
end;

procedure GetLocaleIdentifierList(List: TStrings);
begin
  CheckLocaleIdentifierList;
  List.Assign(LocaleIdentifierList);
end;

procedure FreeCoMem(ptr: IntPtr);
begin
  if GlobaIMalloc = nil then
    CoGetMalloc(1, GlobaIMalloc);
  GlobaIMalloc.Free(ptr);
end;

function VarAsBooleanType(const Value: Boolean): Variant;
begin
  Result := VarAsType(Value, VT_BOOL);
{$IFDEF FPC}
  if Value then
    TVardata(Result).vsmallint := -1;
{$ENDIF}
end;

procedure AddInfoToErr(var S: WideString; const FormatStr: string; const Args: array of const); overload; // Add additional info to exception message
var
  s1: WideString;
begin
  s1 := WideString(Format(FormatStr, Args));
  if s1 <> '' then
    S := S + #10 + s1;
end;

procedure AddInfoToErr(var E: Exception; const FormatStr: string; const Args: array of const); overload; // Add additional info to exception message
var
  S: WideString;
  ENew: Exception;
begin
  if IsClass(E, EOLEDBError) then
    S := EOLEDBError(E).MessageWide
  else
    S := WideString(E.Message);
  AddInfoToErr(S, FormatStr, Args);

  if IsClass(E, EMSError) then begin
    ENew := EMSError.Create(EMSError(E).ErrorCode, S);
    EMSError(ENew).Assign(EMSError(E));
  end
  else
  if IsClass(E, EOLEDBError) then begin
    ENew := EOLEDBError.Create(EOLEDBError(E).ErrorCode, S);
    EOLEDBError(ENew).Assign(EOLEDBError(E));
  end
  else
{$IFNDEF NODBACCESS}
  if IsClass(E, EDAError) then
    ENew := EDAError.Create(EDAError(E).ErrorCode, string(S))
{$ELSE}
  if IsClass(E, ECRError) then
    ENew := ECRError.Create(ECRError(E).ErrorCode, string(S))
{$ENDIF}
  else
    ENew := Exception.Create(string(S));
  E := ENew;
end;

procedure AddInfoToErr(var E: EOLEDBError; const PrevError: EOLEDBError); overload;
var
  Msg: WideString;
  ErrorCode: integer;
  ENew: EOLEDBError;
begin
  Assert(E <> nil);

  if PrevError <> nil then begin
    Msg := E.MessageWide + WideString(#$D#$A) + PrevError.MessageWide;
    ErrorCode := PrevError.ErrorCode;
  end
  else begin
    Msg := E.MessageWide;
    ErrorCode := E.ErrorCode;
  end;

  if IsClass(E, EMSError) or IsClass(PrevError, EMSError) then
    ENew := EMSError.Create(ErrorCode, Msg)
  else
    ENew := EOLEDBError.Create(ErrorCode, Msg);

  if PrevError <> nil then begin
    ENew.Assign(PrevError);
    ENew.FErrors.Assign(E.FErrors); // E.Errors must be first
    ENew.FErrors.Add(PrevError.FErrors); // PrevError.Errors must be last
  end
  else
    ENew.Assign(E);

  E := ENew;
end;

{$WARNINGS OFF}
function ConvertInternalTypeToOLEDB(const InternalType, SubDataType: word; Provider: TMSProvider): word;
begin
  case InternalType of
    // Integer fields
    dtBoolean:
      Result := DBTYPE_BOOL;
    dtInt8:
      if Provider = prCompact then
        Result := DBTYPE_UI1
      else
        Result := DBTYPE_I1;
    dtUInt8:
      Result := DBTYPE_UI1;
    dtInt16:
      Result := DBTYPE_I2;
    dtWord:
      if Provider = prCompact then
        Result := DBTYPE_UI2
      else
        Result := DBTYPE_I4;
    dtInt32:
      Result := DBTYPE_I4;
    dtUInt32:
      if Provider = prCompact then
        Result := DBTYPE_UI4
      else
        Result := DBTYPE_I8;
    dtInt64:
      Result := DBTYPE_I8;
    dtUInt64:
      Result := DBTYPE_UI8;

    // Float fields
    dtSingle:
      Result := DBTYPE_R4;
    dtFloat: begin
      if (Provider = prCompact) and (SubDataType = sdtNumeric) then
        Result := DBTYPE_NUMERIC
      else
        Result := DBTYPE_R8;
    end;
    dtCurrency:
      if Provider = prCompact then
        Result := DBTYPE_CY
      else
        Result := DBTYPE_R8; // Currency type cannot be used over TCurrencyField uses double to store
    dtBCD:
      if Provider = prCompact then
        Result := DBTYPE_NUMERIC
      else
        Result := DBTYPE_CY;
    dtFmtBCD, dtExtended:
      Result := DBTYPE_NUMERIC;

    // Multibyte fields
    dtDateTime:
      if Provider = prCompact then
        Result := DBTYPE_DBTIMESTAMP
      else
        Result := DBTYPE_DATE;
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Result := DBTYPE_DBTIMESTAMP;
  {$ENDIF}
    dtDate, dtTime:
      Result := DBTYPE_DATE;
    dtString, dtExtString:
      Result := DBTYPE_STR;
    dtWideString, dtExtWideString:
      Result := DBTYPE_WSTR;
    dtBytes, dtVarBytes, dtExtVarBytes:
      Result := DBTYPE_BYTES;
    dtMemo, dtWideMemo, dtBlob, dtXML:
      Result := DBTYPE_IUNKNOWN;
    dtGuid:
      Result := DBTYPE_GUID;
    dtVariant:
      Result := DBTYPE_VARIANT;
    dtTable:
      Result := DBTYPE_TABLE;
    dtUnknown:
      Result := DBTYPE_VARIANT;
    else
      Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
  end;
end;
{$WARNINGS ON}

function ConvertCRParamTypeToOLEDB(const InternalType: TParamDirection): word;
begin
  case InternalType of
    pdUnknown:
      Result := DBPARAMIO_INPUT;
    pdInput:
      Result := DBPARAMIO_INPUT;
    pdOutput, pdResult:
      Result := DBPARAMIO_OUTPUT;
    pdInputOutput:
      Result := DBPARAMIO_INPUT + DBPARAMIO_OUTPUT;
    else
      Result := DBPARAMIO_INPUT;
  end;
end;

{$WARNINGS OFF}
function ConvertIsolationLevelToOLEDBIsoLevel(const Value: TCRIsolationLevel):Integer;
begin
  case Value of
    ilReadCommitted:
      Result := ISOLATIONLEVEL_READCOMMITTED;
    ilReadUnCommitted:
      Result := ISOLATIONLEVEL_READUNCOMMITTED;
    ilRepeatableRead:
      Result := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      Result := ISOLATIONLEVEL_ISOLATED;
    ilSnapshot:
      Result := ISOLATIONLEVEL_SNAPSHOT;
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;
end;
{$WARNINGS ON}

function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean;
begin
  Result := TMSFieldDesc(FieldDesc).IsLong; {dtBlob, dtMemo, dtWideMemo, dtXML}
end;

function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean;
begin
  Result := ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo];
end;

function IsOutputLOB(ParamDesc: TParamDesc): boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo]) and
    (ParamDesc.GetParamType in [pdResult, pdOutput, pdInputOutput]);
end;

function ProviderIsNativeClient(const ProviderId: TGuid): boolean;
begin
  Result := (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI)) or
  (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI10)) or
  (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI11));
end;

function ProviderIsMSOLEDBDriver(const ProviderId: TGuid): boolean;
begin
  Result := (GUIDToString(ProviderId) = GUIDToString(CLSID_MSOLEDBSQL));
end;

procedure QueryIntf(Source: IUnknown; const IID: TGuid; out Intf); // OLE QueryInterface analog
begin
  Assert(Source <> nil);
  if Source.QueryInterface(IID, Intf) <> S_OK then
    raise EOLEDBError.Create(E_NOINTERFACE, 'QueryInterface failed');
end;

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{ TOLEDBStream }

constructor TOLEDBStream.Create(Blob: TBlob {to avoid data copy}; StreamList: TList);
begin
  inherited Create;

  FBlob := Blob;
  FStorageType := stBlob;
  FPosInBlob := 0;
  InitStream(StreamList);
end;

constructor TOLEDBStream.Create(Stream: TStream);
begin
  inherited Create;

  FStream := Stream;
  FStorageType := stStream;
  InitStream(nil);
end;

procedure TOLEDBStream.InitStream(StreamList: TList);
begin
{$IFDEF DEBUG}
  Inc(StreamCnt);
{$ENDIF}

  case FStorageType of
    stBlob:
      FSize := FBlob.Size;
    stStream:
      FSize := FStream.Size;
    else
      Assert(False);
  end;

  FStreamList := StreamList;

  if FStreamList <> nil then
    FStreamList.Add(Self);
end;

destructor TOLEDBStream.Destroy;
begin
  if FStreamList <> nil then
    FStreamList.Remove(Self);

  inherited;
{$IFDEF DEBUG}
  Dec(StreamCnt);
{$ENDIF}
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TOLEDBStream.Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
{$ELSE}
function TOLEDBStream.Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TOLEDBStream.Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT;
{$ENDIF}
var
  cbSrcReadBytes: integer;
  cbDstWriteBytes: integer;
begin
  try
    case FStorageType of
      stBlob: begin
        cbSrcReadBytes := Min(Integer(FSize - FPosInBlob), cb);
        cbDstWriteBytes := FBlob.Read(FPosInBlob, cbSrcReadBytes, pv);
        Inc(FPosInBlob, cbDstWriteBytes);
      end;
      stStream: begin
        cbSrcReadBytes := Min(Integer(Integer(FSize) - FStream.Position), cb);
        cbDstWriteBytes := FStream.Read(pv^, cbSrcReadBytes);
      end;
      else
        Assert(False);
        cbDstWriteBytes := 0; // To prevent compiler warning
    end;

    if pcbRead <> nil then
      Marshal.WriteInt32(pcbRead, cbDstWriteBytes);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TOLEDBStream.Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
{$ELSE}
function TOLEDBStream.Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TOLEDBStream.Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT;
{$ENDIF}
begin
  try
    case FStorageType of
      stBlob: begin
        FBlob.Write(FPosInBlob, cb, pv);
        Inc(FPosInBlob, cb);
      end;
      stStream: begin
        FStream.Write(pv^, cb);
      end;
      else
        Assert(False);
    end;
    Inc(FSize, cb);

    if pcbWritten <> nil then
      Marshal.WriteInt32(pcbWritten, cb);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{ TOLEDBConnector }

function TOLEDBConnector.GetConnection: TMSSQLConnection;
begin
  Result := TMSSQLConnection(Connection);
end;

procedure TOLEDBConnector.ReleaseInterfaces;
begin
  TOLEDBTransaction(FOwner.GetInternalTransaction).ReleaseInterfaces;

  FISessionProperties := nil;
  if FIDBInitialize <> nil then
    FIDBInitialize.Uninitialize; // check not need
  FIDBInitialize := nil;
  FIDBProperties := nil;
  FIDBCreateSession := nil;
end;

procedure TOLEDBConnector.Check(const Status: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
begin
  if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) and
     ((GetConnection.Provider <> prCompact) or (Status <> DB_S_MULTIPLECHANGES))
  then
    OLEDBError(Status, Component, AnalyzeMethod, Arg);
end;

procedure TOLEDBConnector.OLEDBError(const ErrorCode: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);

  function MarshalErrInfo(Err: PSSERRORINFO): SSERRORINFO;
  begin
    Result.pwszMessage := Err.pwszMessage;
    Result.pwszServer := Err.pwszServer;
    Result.pwszProcedure := Err.pwszProcedure;
    Result.lNative := Err.lNative;
    Result.bState := Err.bState;
    Result.bClass := Err.bClass;
    Result.wLineNumber := Err.wLineNumber;
  end;

  procedure ObtainErrorParameters(var Msg: WideString; var pIErrorRecords: IErrorRecords; RecNo: integer);

    function GotoBegin(out Position: integer): boolean;
    var
      i, n: integer;
    begin
      Position := -1;
      Result := False;
      Trim(Msg);
      n := length(Msg);
      if Msg[n] = ']' then     // find  [,,,,,] section
        for i := n downto 1 do
          if msg[i] = '[' then begin
            Position := i;
            Result := True;
            Break;
          end;
    end;

    function GotoNextParamPos(var Position: integer): Boolean;
    begin
      Result := True;
      if Msg[Position] = ']' then
        Result := False
      else begin
        inc(Position);
        while((Msg[Position] <> ',') and (Msg[Position] <> ']')) do
          inc(Position);
      end;
    end;

    procedure InsertParam(const param: WideString; var msg: WideString; var Position: integer);
    var
      n: integer;
    begin
      n := length(param);
      if n > 0 then begin
        case msg[Position - 1] of
          '[': begin
            Insert(param, msg, Position);
            inc(Position, n);
          end;
          ',': begin
            Insert(' ' + param, msg, Position);
            inc(Position, n + 1);
          end;
          else begin
            Insert(': ' + param, msg, Position);
            inc(Position, n + 2)
          end;
        end;
      end;
    end;

  var
    i: integer;
    idp: DISPPARAMS;
    ov: OleVariant;
    ae: integer;
    Position: integer;
  begin
    if not GotoBegin(Position) then
      Exit;

    if (pIErrorRecords.GetErrorParameters (RecNo, idp) = S_OK) then begin
      VariantInit({$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF});
      for i := pred(idp.cArgs) downto 0  do begin
        if DispGetParam(idp, i, VT_BSTR, {$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF}, ae) <> S_OK then
          VariantClear({$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF});
        if GotoNextParamPos(Position) then
          InsertParam(ov, msg, Position)
      end;
    end;
  end;

  function GetErrMessage(const ErrInfo: PSSERRORINFO): WideString;
  begin
  {$IFNDEF LITE}
    Result := ErrInfo.pwszMessage;
  {$ELSE}
    Result := 'SQL State: ' + IntToStr(ErrInfo.bState) +
      ', SQL Error Code: ' + IntToStr(ErrInfo.lNative) +
      ', Level: ' + IntToStr(ErrInfo.bClass) +
      ', Procedure: ' + ErrInfo.pwszProcedure +
      ', Line: ' + IntToStr(ErrInfo.wLineNumber) +
      WideString(#$D#$A) + ErrInfo.pwszMessage;
  {$ENDIF};
  end;

var
  pIErrorInfoAll: IErrorInfo;
  pIErrorRecords: IErrorRecords;
  pISQLServerErrorInfo: ISQLServerErrorInfo;
  pssErrInfo: PSSERRORINFO;
  ssErrInfo: SSERRORINFO;
  pStrBuf: IntPtr; // pWideChar;
  Msg: WideString;
  s1: WideString;
  Analysis: string;
  ErrInfo: ERRORINFO;
  iu: IUnknown;
  ssErrInfoBuf: SSERRORINFO;
  i, RecCount: cardinal;
  Err, OldErr: Exception;
  LastErr: EOLEDBError;
  Fail: boolean;
begin
  if (ErrorCode = 0) and not Assigned(GetConnection.OnInfoMessage) then
    Exit;

  s1 := '';
  RecCount := 0;
  Err := nil;

  if GetErrorInfo(0, pIErrorInfoAll) = S_OK then begin
    pIErrorInfoAll.GetDescription(Msg);
    pIErrorInfoAll.QueryInterface(IID_IErrorRecords, pIErrorRecords);
    if pIErrorRecords <> nil then
      pIErrorRecords.GetRecordCount(RecCount);

    if RecCount > 0 then begin
      for i := RecCount - 1 downto 0 do begin
        if (pIErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, iu) = S_OK) then
          pISQLServerErrorInfo := ISQLServerErrorInfo(iu);
          iu := nil;
          if (pISQLServerErrorInfo <> nil) and
            (pISQLServerErrorInfo.GetErrorInfo(pssErrInfo, pStrBuf) = S_OK) and
            (pssErrInfo <> nil) then begin
            if s1 = '' then begin
              ssErrInfoBuf := MarshalErrInfo(pssErrInfo);
              s1 := GetErrMessage(pssErrInfo);
            end
            else
              s1 := GetErrMessage(pssErrInfo) + WideString(#$D#$A) + s1;

            FreeCoMem(pStrBuf);
            FreeCoMem(pssErrInfo);
          end;
      end;
      if s1 <> '' then
        Err := EMSError.Create(ssErrInfoBuf, ErrorCode, s1);
    end;
  end;

  if Err = nil then begin // this is OLE DB error. As example - repeated Connection.Rollback or non-convergence types of field and parameter
    if ErrorCode = 0 then
      Exit; // No error and no message

    if Msg = '' then
      Msg := WideString(Format(SOLEDBError, [ErrorCode]));
    if ErrorCode = CO_E_NOTINITIALIZED then
      Msg := Msg + '.'#$D#$A'CoInitialize has not been called.';

    Err := EOLEDBError.Create(ErrorCode, Msg);
  end;

  if Assigned(AnalyzeMethod) then begin
    Analysis := string(AnalyzeMethod(ErrorCode, Arg));
    if Analysis <> '' then begin
      OldErr := Err;
      AddInfoToErr(Err, Analysis, []);
      if OldErr <> Err then
        OldErr.Free;
    end;
  end;

{$IFNDEF NODBACCESS}
  EOLEDBError(Err).Component := Component;
{$ENDIF}
  EOLEDBError(Err).FOLEDBErrorCode := ErrorCode;
  if RecCount > 0 then
    for i := 0 to RecCount - 1 do begin
      if pIErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, iu) = S_OK then begin
        pISQLServerErrorInfo := ISQLServerErrorInfo(iu);
        if (pISQLServerErrorInfo <> nil) and
          (pISQLServerErrorInfo.GetErrorInfo(pssErrInfo, pStrBuf) = S_OK) and
          (pssErrInfo <> nil) then begin
          // EMSError
          ssErrInfo := MarshalErrInfo(pssErrInfo);
          LastErr := EMSError.Create(ssErrInfo, 0, ssErrInfo.pwszMessage);

          FreeCoMem(pStrBuf);
          FreeCoMem(pssErrInfo);
        end
        else begin
          if GetConnection.Provider = prCompact then begin
            ObtainErrorParameters(Msg, pIErrorRecords, i);
            Err.Message := string(Msg);
          end;
          LastErr := EOLEDBError.Create(ErrorCode, Msg);
        end;
        EOLEDBError(Err).FErrors.FList.Add(LastErr);

        if pIErrorRecords.GetBasicErrorInfo(i, ErrInfo) = S_OK then begin
          LastErr.FOLEDBErrorCode := ErrInfo.hrError;
          // LastErr.FMinor := ErrInfo.dwMinor;
          // LastErr.Fclsid := ErrInfo.clsid;
          LastErr.Fiid := ErrInfo.iid;
          // LastErr.Fdispid := ErrInfo.dispid;
        end;

        (*if pIErrorRecords.GetErrorInfo(i, GetUserDefaultLCID(), pIErrorInfo) = S_OK then begin
          // GetErrorInfo - IErrorInfo interface
          // pIErrorInfo.GetGUID(Fguid); - same as Fiid
          pIErrorInfo.GetSource(FSource);
          pIErrorInfo.GetDescription(FDescription);
          // pIErrorInfo.GetHelpFile(FHelpFile); - not used by sqloledb
          // pIErrorInfo.GetHelpContext(FdwHelpContext); - not used by sqloledb
        end;*)
      end;
    end;

  if (ErrorCode = 0) and Assigned(GetConnection.OnInfoMessage) then begin
    Assert(Err <> nil);
    try
      GetConnection.OnInfoMessage(Err);
    finally
      Err.Free;
    end;
  end
  else begin
    Fail := True;
    try
      if Assigned(FOwner.OnError) then
        FOwner.DoError(Err{$IFNDEF NODBACCESS} as EDAError{$ENDIF}, Fail);
      if Fail then
        raise Err
      else
        Abort;
    finally
      if not Fail then
        Err.Free;
    end;
  end;
end;

procedure TOLEDBConnector.GetConnectionProperties;
var
  PropValues: TPropValues;
  i, j: integer;
begin
  if (FOwner.Database = '') and (GetConnection.Provider <> prCompact) then begin
    with TOLEDBPropertiesGet.Create(Self, DBPROPSET_DATASOURCE) do
      try
        AddPropId(DBPROP_CURRENTCATALOG);
        GetProperties(FIDBProperties, PropValues);
      finally
        Free;
      end;
    FOwner.Database := PropValues[0];
  end;

  with TOLEDBPropertiesGet.Create(Self, DBPROPSET_DATASOURCEINFO) do
    try
      AddPropId(DBPROP_DBMSNAME);
      AddPropId(DBPROP_DBMSVER);
      AddPropId(DBPROP_PROVIDERFRIENDLYNAME);
      AddPropId(DBPROP_PROVIDERVER);
      if GetConnection.Provider = prCompact then
        AddPropId(DBPROP_PROVIDERNAME);
      GetProperties(FIDBProperties, PropValues);
    finally
      Free;
    end;

  FServerName := PropValues[0];
  FServerVersion := PropValues[1];

  i := Pos('.', FServerVersion);
  FServerMajorVersion := StrToInt(Copy(FServerVersion, 1, i - 1));

  j := i + 1;
  while (j <= Length(FServerVersion)) and (FServerVersion[j] >= '0') and (FServerVersion[j] <= '9') do
    Inc(j);
  FServerMinorVersion := StrToInt(Copy(FServerVersion, i + 1, j - i - 1));

  FProviderFriendlyName := PropValues[2];
  FProviderVer := PropValues[3];
  FProviderMajorVer := StrToInt(Copy(FProviderVer, 1, Pos('.', FProviderVer) - 1));
  if (FProviderFriendlyName = SSQLOLEDBProviderName) and (FProviderMajorVer > 8) then
    FProviderMajorVer := 8;

  if (GetConnection.Provider = prCompact) and (FProviderFriendlyName = '') then begin
    FProviderFriendlyName := PropValues[4];
    if Pos('.dll', LowerCase(FProviderFriendlyName)) > 0 then
      SetLength(FProviderFriendlyName, Length(FProviderFriendlyName) - 4);
  end;

  FIsOutputLOBSupported := (FServerMajorVersion >= 9) and (FProviderMajorVer >= 9);

  if GetConnection.Provider = prCompact then
    if not IsCompactEdition(FProviderMajorVer) then
      DatabaseError(SBadProviderName);
end;

procedure TOLEDBConnector.SetConnectionProperties;

  procedure SetCompactConnectionProperties;
  begin
  {$IFDEF CPUX64}
    SetExceptionMask([exOverflow, exPrecision]);
  {$ENDIF}
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_DBINIT) do
      try
        AddPropStr(DBPROP_INIT_DATASOURCE, FOwner.Database, True);
        if GetConnection.Options.InitMode <> imReadWrite then
          case GetConnection.Options.InitMode of
            imReadOnly:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_READ);
            imReadWrite:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_READWRITE);
            imExclusive:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_SHARE_EXCLUSIVE);
            imShareRead:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_SHARE_DENY_READ);
            else
              Assert(False);
          end;
        if GetConnection.Options.LocaleIdentifier <> GetSystemDefaultLCID then
          AddPropUInt(DBPROP_INIT_LCID, GetConnection.Options.LocaleIdentifier);
        try
          SetProperties(FIDBProperties);
        except
          on E: Exception do begin
            AddInfoToErr(E, SBadDatabaseFile, []);
            raise E;
          end;
        end;
      finally
        Free;
      end;

      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SSCE_DBINIT) do
        try
          if FOwner.GetPassword <> '' then
            AddPropStr(DBPROP_SSCE_DBPASSWORD, FOwner.GetPassword);
          if GetConnection.Options.MaxDatabaseSize <> DefaultMaxDatabaseSize then
            AddPropInt(DBPROP_SSCE_MAX_DATABASE_SIZE, GetConnection.Options.MaxDatabaseSize);
          if GetConnection.Options.MaxBufferSize <> DefaultMaxBufferSize then
            AddPropInt(DBPROP_SSCE_MAXBUFFERSIZE, GetConnection.Options.MaxBufferSize);
          if GetConnection.Options.TempFileDirectory <> '' then
            AddPropStr(DBPROP_SSCE_TEMPFILE_DIRECTORY, GetConnection.Options.TempFileDirectory);
          if GetConnection.Options.TempFileMaxSize <> DefaultTempFileMaxSize then
            AddPropInt(DBPROP_SSCE_TEMPFILE_MAX_SIZE, GetConnection.Options.TempFileMaxSize);
          if GetConnection.Options.DefaultLockEscalation <> DefaultDefaultLockEscalation then
            AddPropInt(DBPROP_SSCE_DEFAULT_LOCK_ESCALATION, GetConnection.Options.DefaultLockEscalation);
          if GetConnection.Options.DefaultLockTimeout <> DefaultDefaultLockTimeout then
            AddPropInt(DBPROP_SSCE_DEFAULT_LOCK_TIMEOUT, GetConnection.Options.DefaultLockTimeout);
          if GetConnection.Options.AutoShrinkThreshold <> DefaultAutoShrinkThreshold then
            AddPropInt(DBPROP_SSCE_AUTO_SHRINK_THRESHOLD, GetConnection.Options.AutoShrinkThreshold);
          if GetConnection.Options.FlushInterval <> DefaultFlushInterval then
            AddPropInt(DBPROP_SSCE_FLUSH_INTERVAL, GetConnection.Options.FlushInterval);
          SetProperties(FIDBProperties);
        finally
          Free;
        end;
  end;

var
  AppName: array[0..MAX_PATH + 100] of Char;
  Port: integer;
begin
  // Set initialization properties
  if GetConnection.Provider = prCompact then
    SetCompactConnectionProperties
  else begin
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_DBINIT) do
      try
        // Auth props
        Port := GetConnection.Port;
        if Port = 0 then
          Port := DefaultSDACPort;
        if FOwner.GetServer <> '' then begin
          if (Port = DefaultSDACPort) or (Pos(',', FOwner.GetServer) > 0) then
            AddPropStr(DBPROP_INIT_DATASOURCE, FOwner.GetServer)
          else
            AddPropStr(DBPROP_INIT_DATASOURCE, FOwner.GetServer + ',' + IntToStr(Port));
        end
        else begin
          if GetConnection.Port = DefaultSDACPort then
            AddPropStr(DBPROP_INIT_DATASOURCE, '(local)')
          else
            AddPropStr(DBPROP_INIT_DATASOURCE, '(local),' + IntToStr(Port));
        end;
        AddPropStr(DBPROP_INIT_CATALOG, FOwner.Database);
        AddPropInt(DBPROP_INIT_TIMEOUT, GetConnection.Options.ConnectionTimeout);

        case GetConnection.Options.Authentication of
          auWindows:
            AddPropStr(DBPROP_AUTH_INTEGRATED, '');
          auServer:
            if (FOwner.GetUserName = '') and (FOwner.GetPassword = '') then
             AddPropStr(DBPROP_AUTH_USERID, 'sa')
           else begin
             AddPropStr(DBPROP_AUTH_USERID, FOwner.GetUserName);
             AddPropStr(DBPROP_AUTH_PASSWORD, FOwner.GetPassword);
           end;
        end;

        if GetConnection.Options.PersistSecurityInfo then
          AddPropBool(DBPROP_AUTH_PERSIST_SENSITIVE_AUTHINFO, GetConnection.Options.PersistSecurityInfo);

        // Prompt props
        AddPropSmallInt(DBPROP_INIT_PROMPT, DBPROMPT_NOPROMPT);
        AddPropInt(DBPROP_INIT_HWND, 0);

        SetProperties(FIDBProperties);
      finally
        Free;
      end;

    // Set common SQL Server properties
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
      try
        if GetConnection.Options.WorkstationID <> '' then
          AddPropStr(SSPROP_INIT_WSID, GetConnection.Options.WorkstationID);

        if GetConnection.Options.ApplicationName <> '' then
          AddPropStr(SSPROP_INIT_APPNAME, GetConnection.Options.ApplicationName)
        else begin
          GetModuleFileName(0, AppName, sizeof(AppName));
          AddPropStr(SSPROP_INIT_APPNAME, ExtractFileName(AppName));
        end;

        if GetConnection.Options.Language <> '' then
          AddPropStr(SSPROP_INIT_CURRENTLANGUAGE, GetConnection.Options.Language, True);

        AddPropBool(SSPROP_INIT_AUTOTRANSLATE, GetConnection.Options.AutoTranslate, True);

        if ProviderIsNativeClient(FProviderId) or ProviderIsMSOLEDBDriver(FProviderId) then begin
          if GetConnection.Options.MultipleActiveResultSets then
            AddPropBool(SSPROP_INIT_MARSCONNECTION, GetConnection.Options.MultipleActiveResultSets);
          if GetConnection.Options.TrustServerCertificate then
            AddPropBool(SSPROP_INIT_TRUST_SERVER_CERTIFICATE, GetConnection.Options.TrustServerCertificate);
          if GetConnection.Options.OldPassword <> '' then
            AddPropStr(SSPROP_AUTH_OLD_PASSWORD, GetConnection.Options.OldPassword);

          case GetConnection.Options.ApplicationIntent of
            aiReadWrite:
              AddPropStr(SSPROP_INIT_APPLICATIONINTENT, 'ReadWrite');
            aiReadOnly:
              AddPropStr(SSPROP_INIT_APPLICATIONINTENT, 'ReadOnly');
          end;

          if ProviderIsMSOLEDBDriver(FProviderId) and GetConnection.Options.MultiSubnetFailover then
            AddPropBool(SSPROP_INIT_MULTISUBNETFAILOVER, GetConnection.Options.MultiSubnetFailover);
        end
        else
          if GetConnection.Options.OldPassword <> '' then
            raise Exception.Create(SSQLNCLINeedsChangePwd);

        if GetConnection.Options.InitialFileName <> '' then
          AddPropStr(SSPROP_INIT_FILENAME, GetConnection.Options.InitialFileName);

        if GetConnection.Options.FailoverPartner <> '' then
          AddPropStr(SSPROP_INIT_FAILOVERPARTNER, GetConnection.Options.FailoverPartner);

        SetProperties(FIDBProperties);
      finally
        Free;
      end;

    // Isolated for easy error detection
    if GetConnection.Options.Encrypt then // Set only if FEncrypt = True. This needs for prevent troubles with 7.xxx clients
      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
        try
          AddPropBool(SSPROP_INIT_ENCRYPT, GetConnection.Options.Encrypt);
          try
            SetProperties(FIDBProperties);
          except
            on E: Exception do
            begin
              AddInfoToErr(E, SBadEncrypt, []);
              raise E;
            end;
          end;
        finally
          Free;
        end;

    // Isolated for easy error detection
    if GetConnection.Options.NetworkLibrary <> '' then
      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
        try
          AddPropStr(SSPROP_INIT_NETWORKLIBRARY, GetConnection.Options.NetworkLibrary, True);
          try
            SetProperties(FIDBProperties);
          except
            on E: Exception do begin
              AddInfoToErr(E, SBadNetworkLibrary, []);
              raise E;
            end;
          end;
        finally
          Free;
        end;

    // Isolated for easy error detection
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
      try
        AddPropInt(SSPROP_INIT_PACKETSIZE, GetConnection.Options.PacketSize);
        try
          SetProperties(FIDBProperties);
        except
          on E: Exception do begin
            AddInfoToErr(E, SBadPacketSize, []);
            raise E;
          end;
        end;
      finally
        Free;
      end;
  end;
end;

procedure TOLEDBConnector.SetSessionProperties;
var
  IsolationLevel: Variant;
  OLEDBProperties: TOLEDBPropertiesSet;

  procedure SetCompactSessionProperties;
  begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_SSCE_SESSION);
    try
      if GetConnection.Options.LockTimeout <> DefaultDefaultLockTimeout then
        OLEDBProperties.AddPropInt(DBPROP_SSCE_LOCK_TIMEOUT, GetConnection.Options.LockTimeout);
      if GetConnection.Options.LockEscalation <> DefaultDefaultLockEscalation then
        OLEDBProperties.AddPropInt(DBPROP_SSCE_LOCK_ESCALATION, GetConnection.Options.LockEscalation);
      case GetConnection.Options.TransactionCommitMode of
        cmAsynchCommit:
          OLEDBProperties.AddPropInt(DBPROP_SSCE_TRANSACTION_COMMIT_MODE, DBPROPVAL_SSCE_TCM_DEFAULT);
        cmSynchCommit:
          OLEDBProperties.AddPropInt(DBPROP_SSCE_TRANSACTION_COMMIT_MODE, DBPROPVAL_SSCE_TCM_FLUSH);
        else
          Assert(False);
      end;
      OLEDBProperties.SetProperties(FISessionProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;

begin
  if GetConnection.Provider = prCompact then
    SetCompactSessionProperties
  else begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_SESSION);
    try
      // Set transaction properties
      Assert(FOwner.GetInternalTransaction <> nil);
      FOwner.GetInternalTransaction.GetProp(prIsolationLevel, IsolationLevel);
      OLEDBProperties.AddPropInt(DBPROP_SESS_AUTOCOMMITISOLEVELS, ConvertIsolationLevelToOLEDBIsoLevel(TCRIsolationLevel(IsolationLevel)));
      OLEDBProperties.SetProperties(FISessionProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
end;

procedure TOLEDBConnector.SetDataSourceProperties;
var
  OLEDBProperties: TOLEDBPropertiesSet;
begin
  if GetConnection.Provider <> prCompact then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_DATASOURCE);
    try
      // Set transaction properties
      OLEDBProperties.AddPropBool(DBPROP_MULTIPLECONNECTIONS, GetConnection.Options.MultipleConnections);
      OLEDBProperties.SetProperties(FIDBProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
end;

function TOLEDBConnector.CheckColumnsMetaInfo: TOLEDBRecordSet;
begin
  if FColumnsMetaInfo <> nil then
    Result := FColumnsMetaInfo
  else begin
    Result := TOLEDBRecordSet.Create;
    FColumnsMetaInfo := Result;

    Result.SetConnection(FOwner);
    Result.SetProp(prFetchAll, False);
    Result.SetProp(prFetchRows, 1);
    Result.SetProp(prUniDirectional, True);
  {$IFNDEF LITE}
    Result.DataTypeMap.Enabled := False;
    Result.FFldFieldNameIdx := -1;
  {$ENDIF}
  end;
end;

procedure TOLEDBConnector.CreateCompactDatabase;
const
  SSCEPropCount = 10;
var
  pIDBDataSourceAdmin: IDBDataSourceAdmin;
  pIUnknownSession: IUnknown;

  PPropertySet: IntPtr;
  PropertySet: PDBPropSet;
  PDBProperty: IntPtr;
  DBProperty: PDBProp;
  PSSCEProperty: IntPtr;
  SSCEProperty: PDBPROP;
begin
  PPropertySet := nil;
  PDBProperty := nil;
  PSSCEProperty := nil;
  try
    Check(CoCreateInstance(FProviderId, nil, CLSCTX_INPROC_SERVER,
      IID_IDBDataSourceAdmin, pIDBDataSourceAdmin), FOwner.Component);

    PDBProperty := Marshal.AllocHGlobal(SizeOf_DBProp);
    FillChar(PDBProperty, SizeOf_DBProp, $00);
    DBProperty := PDBProperty;
    DBProperty.dwPropertyID := DBPROP_INIT_DATASOURCE;
    DBProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    DBProperty.vValue := FOwner.Database;

    PSSCEProperty := Marshal.AllocHGlobal(SizeOf_DBProp * SSCEPropCount);
    FillChar(PSSCEProperty, SizeOf_DBProp * SSCEPropCount, $00);

    SSCEProperty := PSSCEProperty;
    SSCEProperty.dwPropertyID := DBPROP_SSCE_ENCRYPTDATABASE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsBooleanType(GetConnection.Options.Encrypt);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DBPASSWORD;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := FOwner.GetPassword;

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 2);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_MAX_DATABASE_SIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.MaxDatabaseSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 3);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_MAXBUFFERSIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.MaxBufferSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 4);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_TEMPFILE_DIRECTORY;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := GetConnection.Options.TempFileDirectory;

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 5);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_TEMPFILE_MAX_SIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.TempFileMaxSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 6);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DEFAULT_LOCK_ESCALATION;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.DefaultLockEscalation, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 7);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DEFAULT_LOCK_TIMEOUT;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.DefaultLockTimeout, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 8);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_AUTO_SHRINK_THRESHOLD;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.AutoShrinkThreshold, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOf_DBProp * 9);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_FLUSH_INTERVAL;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(GetConnection.Options.FlushInterval, VT_I4);

    PPropertySet := Marshal.AllocHGlobal(SizeOf(DBPROPSET) * 2);
    FillChar(PPropertySet, SizeOf(DBPROPSET) * 2, $00);
    PropertySet := PPropertySet;
    PropertySet.guidPropertySet := DBPROPSET_DBINIT;
    PropertySet.rgProperties := PDBProperty;
    PropertySet.cProperties := 1;

    PropertySet := PtrOffset(PPropertySet, SizeOf(DBPROPSET));
    PropertySet.guidPropertySet := DBPROPSET_SSCE_DBINIT;
    PropertySet.rgProperties := PSSCEProperty;
    PropertySet.cProperties := SSCEPropCount;

    Check(pIDBDataSourceAdmin.CreateDataSource(2, PPropertySet, nil,
      IID_IUnknown, pIUnknownSession), FOwner.Component);
  finally
    pIUnknownSession := nil;
    if PPropertySet <> nil then
      Marshal.FreeHGlobal(PPropertySet);
    if PSSCEProperty <> nil then
      Marshal.FreeHGlobal(PSSCEProperty);
    if PDBProperty <> nil then
      Marshal.FreeHGlobal(PDBProperty);
  end;
end;

function TOLEDBConnector.GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
var
  i: integer;
  iu: IUnknown;
  DBSchemaRowset: IDBSchemaRowset;
  rgRestrictionsPtr: IntPtr;
begin
  for i := Low(rgRestrictions) to High(rgRestrictions) do
    if (VarType(rgRestrictions[i]) = varOleStr) and (string(rgRestrictions[i]) = '') then
      rgRestrictions[i] := Null;
      // rgRestrictions[i] := Unassigned;
      // TVarData(rgRestrictions[i]).VType := varNull;

  QueryIntf(FISessionProperties, IID_IDBSchemaRowset, DBSchemaRowset);
  rgRestrictionsPtr := @rgRestrictions[0];
  Check(DBSchemaRowset.GetRowset(nil, Schema, Length(rgRestrictions), rgRestrictionsPtr,
    IID_IRowset, 0, nil, iu), FOwner.Component);
  Result := IRowset(iu);
  iu := nil;
end;

procedure TOLEDBConnector.Connect;

  function OpenProvider(const clsid: array of TGuid): HRESULT;
  var
    i: integer;
  begin
    Result := 0;
    for i := Low(clsid) to High(clsid) do begin
      Result := CoCreateInstance(clsid[i],
        nil,
        CLSCTX_INPROC_SERVER,
        IID_IDBInitialize,
        FIDBInitialize);
      if Result <> REGDB_E_CLASSNOTREG then begin
        FProviderId := clsid[i];
        Exit;
      end;
    end;
  end;

  // if database file exists, this method tries to determine its version
  // do nothing if db file does not exists or signature is not recognized
  // if signature is recognized, CompactVer will be corrected if FCompactVersion = prAuto
  // raise error if actual file version was determined successfully, and it differs from provided FCompactVersion
  // signature is 4 bytes at offset 0x10 (16) in the database file
  procedure CheckCompactDBFile(var CompactVer: TCompactVersion);
  var
    fs: TFileStream;
    w: cardinal;
    Determined: boolean;
  begin
    Determined := False;
    if not FileExists(FOwner.Database) then
      Exit;

    try // hide exception if access to the file denied (it may be open by another connection)
      fs := TFileStream.Create(FOwner.Database, fmOpenRead or fmShareDenyNone);

      try
        if fs.Size < 20 then
          Exit;
        fs.Seek(16, soFromBeginning);
        fs.ReadBuffer(w, 4);

        case w of
          4000000: begin
            CompactVer := cv40;
            Determined := True;
          end;
          3505053: begin
            CompactVer := cv35;
            Determined := True;
          end;
          3004180: begin
            CompactVer := cv30;
            Determined := True;
          end;
        end;

      finally
        fs.Free;
      end;

    except
    end;

    if Determined and (GetConnection.Options.CompactVersion <> cvAuto) and (GetConnection.Options.CompactVersion <> CompactVer) then
      DatabaseError(SDBVerAndCompactVerDiffer);
  end;

var
  hr: HRESULT;
  iu: IUnknown;
  TmpCompactVer: TCompactVersion;
begin
  if FIDBInitialize <> nil then
    ReleaseInterfaces;

  try
    hr := 0;
    case GetConnection.Provider of
      prAuto:
        hr := OpenProvider([CLSID_SQLNCLI11, CLSID_SQLNCLI10, CLSID_SQLNCLI, CLSID_MSOLEDBSQL, CLSID_SQLOLEDB]);
      prSQL:
        hr := OpenProvider([CLSID_SQLOLEDB]);
      prMSOLEDB:
        hr := OpenProvider([CLSID_MSOLEDBSQL]);
      prNativeClient:
        case GetConnection.Options.NativeClientVersion of
          ncAuto:
            hr := OpenProvider([CLSID_SQLNCLI11, CLSID_SQLNCLI10, CLSID_SQLNCLI]);
          nc2005:
            hr := OpenProvider([CLSID_SQLNCLI]);
          nc2008:
            hr := OpenProvider([CLSID_SQLNCLI10]);
          nc2012:
            hr := OpenProvider([CLSID_SQLNCLI11]);
        else
          Assert(False);
        end;
      prCompact: begin
        TmpCompactVer := GetConnection.Options.CompactVersion;
        CheckCompactDBFile(TmpCompactVer);
        case TmpCompactVer of
          cvAuto:
            hr := OpenProvider([CLSID_SQLSERVERCE_4_0, CLSID_SQLSERVERCE_3_5, CLSID_SQLSERVERCE_3_0]);
          cv30:
            hr := OpenProvider([CLSID_SQLSERVERCE_3_0]);
          cv35:
            hr := OpenProvider([CLSID_SQLSERVERCE_3_5]);
          cv40:
            hr := OpenProvider([CLSID_SQLSERVERCE_4_0]);
        else
          Assert(False);
        end;
      end;
    else
      DatabaseError(SBadProviderName);
    end;

    if hr = REGDB_E_CLASSNOTREG then
      DatabaseError(SMSSQLNotFound)
    else
      Check(hr, FOwner.Component);

    QueryIntf(FIDBInitialize, IID_IDBProperties, FIDBProperties);

    // Set initialization properties
    SetConnectionProperties;
    //SetDatabase(FDatabase); - setted on SetConnectionProperties

    // Now establish the connection to the data source.
    hr := FIDBInitialize.Initialize;
    if (hr = E_FAIL) and (GetConnection.Provider = prCompact) then begin // Database file does not exist
      try
        OLEDBError(hr, FOwner.Component);
      except
        on e: EOLEDBError do begin
          if GetConnection.Options.ForceCreateDatabase and ((Pos('The database file cannot be found', e.Message) > 0) or (cardinal(e.FOLEDBErrorCode) = $80004005)) then begin
            CreateCompactDatabase;
            hr := FIDBInitialize.Initialize;
          end
          else
            raise;
        end;
      end;
    end;
    Check(hr, FOwner.Component);

    SetDataSourceProperties;

    // Create the SessionObject
    QueryIntf(FIDBInitialize, IID_IDBCreateSession, FIDBCreateSession);
    Check(FIDBCreateSession.CreateSession(nil, IID_ISessionProperties, iu), FOwner.Component);
    FISessionProperties := ISessionProperties(iu);
    SetSessionProperties;

    // Get properties
    GetConnectionProperties;
  except
    ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBConnector.Disconnect;
begin
  if FOwner.NativeConnection then
    ReleaseInterfaces;

  FreeAndNil(FColumnsMetaInfo);
end;

procedure TOLEDBConnector.SetDatabase(const Value: string);
var
  OLEDBProperties: TOLEDBPropertiesSet;
begin
  if FIDBProperties <> nil then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_DATASOURCE);
    try
      OLEDBProperties.AddPropStr(DBPROP_CURRENTCATALOG, MSSQLInfo.NormalizeName(Value));
      OLEDBProperties.SetProperties(FIDBProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
end;

function TOLEDBConnector.GetClientVersion: string;
begin
  Result := FProviderVer;
end;

function TOLEDBConnector.GetClientVersionFull: string;
begin
  Result := FProviderFriendlyName;
end;

function TOLEDBConnector.GetClientMajorVersion: integer;
begin
  Result := FProviderMajorVer;
end;

function TOLEDBConnector.IsOutputLOBSupported: boolean;
begin
  Result := FIsOutputLOBSupported;
end;

class function TOLEDBConnector.CloneException(E: Exception): Exception;
begin
  if E is EOLEDBError then begin
    Result := EOLEDBError.Create(EOLEDBError(E).ErrorCode, WideString(EOLEDBError(E).Message));
    EOLEDBError(Result).Assign(EOLEDBError(E));
  end
  else
    Result := Exception.Create(E.Message);
end;

{$IFNDEF LITE}
procedure TOLEDBConnector.Enlist(Transaction: TMTSTransaction);
var
  TRIsolationLevel: Variant;
  IsolationLevel: integer;
  IsolationFlags: integer;
  TransactionOptions: ITransactionOptions;
begin
  Transaction.GetProp(CRProps.prIsolationLevel, TRIsolationLevel);
  case TCRIsolationLevel(Integer(TRIsolationLevel)) of
    ilReadUnCommitted:
      IsolationLevel := ISOLATIONLEVEL_READUNCOMMITTED;
    ilReadCommitted:
      IsolationLevel := ISOLATIONLEVEL_READCOMMITTED;
    ilRepeatableRead:
      IsolationLevel := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      IsolationLevel := ISOLATIONLEVEL_SERIALIZABLE;
  else
    Assert(False);
    IsolationLevel := 0;
  end;
  IsolationFlags := 0;
  TransactionOptions := nil;

  QueryIntf(FISessionProperties, IID_ITransactionJoin, FITransactionJoin);
  Check(FITransactionJoin.JoinTransaction(Transaction.MTSTransaction, IsolationLevel, IsolationFlags, TransactionOptions), FOwner.Component);
end;

procedure TOLEDBConnector.UnEnlist(Transaction: TMTSTransaction);
begin
  if FITransactionJoin <> nil then
    Check(FITransactionJoin.JoinTransaction(nil, 0, 0, nil), FOwner.Component);
end;
{$ENDIF}

{ TOLEDBCommand }

constructor TOLEDBCommand.Create;
begin
  inherited;

  FQueryIntCnt := 0;
  FParamsAccessorDataAvaible := False;
  FRowsAffected := -1;
end;

destructor TOLEDBCommand.Destroy;
begin
  FreeAndNil(FExecutor);

  FISSAsynchStatus := nil;
  ClearIMultipleResults;
  FIUnknownNext := nil; // Clear this interface before RequestParams to avoid AV. See TDbxSdaTestSet.DoTestSPNextRecordSet
  RequestParamsIfPossible;
  UnPrepare;
  Assert(FQueryIntCnt = 0, Format('TOLEDBCommand.Destroy - interfaces not released (%d)', [FQueryIntCnt]));
  if TmpPiece <> nil then
    Marshal.FreeHGlobal(TmpPiece);

  inherited;
end;

class function TOLEDBCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TMSSQLInfo;
end;

class function TOLEDBCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TMSParamDesc;
end;

class function TOLEDBCommand.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

{$IFNDEF LITE}
class function TOLEDBCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMSMapRules;
end;
{$ENDIF}

procedure TOLEDBCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;
    FConnection := TMSSQLConnection(Value);
  end;
end;

function TOLEDBCommand.GetConnector: TOLEDBConnector;
begin
  Result := TOLEDBConnector(FConnection.GetConnector);
end;

function TOLEDBCommand.GetRowsAffected: NativeInt;
begin
  Result := FRowsAffected;
end;

procedure TOLEDBCommand.SetRowsAffected(Value: NativeInt);
begin
  FRowsAffected := Value;
end;

class function TOLEDBCommand.InternalGetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
  ParamsInfo: TDAParamsInfo; Iters: integer): string;
begin
  Result := TMSSQLCommandHelper.GetBatchSQL(SQL, ParsedSQLType, ParamsInfo, Iters);
end;

function TOLEDBCommand.NeedPrepareOnBatch: boolean;
begin
  Result := False;
end;

procedure TOLEDBCommand.Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil);
begin
  Assert(FConnection <> nil);
  if Status <> S_OK then
    GetConnector.Check(Status, Component, AnalyzeMethod, nil);
end;

procedure TOLEDBCommand.QueryInterfaces(const QueryPrepare: boolean); // QueryPrepare must be True to request IID_ICommandPrepare
var
  CreateCommand: IDBCreateCommand;
  iu: IUnknown;
begin
  if FQueryIntCnt = 0 then begin
    Assert(FConnection <> nil);
    QueryIntf(GetConnector.FISessionProperties, IID_IDBCreateCommand, CreateCommand);

    Check(CreateCommand.CreateCommand(nil, IID_ICommandText, iu));
    FICommandText := ICommandText(iu);
    if QueryPrepare then
      QueryIntf(FICommandText, IID_ICommandPrepare, FICommandPrepare)
    else
      FICommandPrepare := nil;
    QueryIntf(FICommandText, IID_ICommandProperties, FICommandProperties);
  end;

  Inc(FQueryIntCnt);
end;

procedure TOLEDBCommand.ReleaseInterfaces;
begin
  if FQueryIntCnt = 0 then // Exception on TOLEDBRecordSet.InternalOpen -> TOLEDBRecordSet.QueryCommandInterfaces -> TOLEDBCommand.QueryInterfaces
    Exit;

  if FQueryIntCnt = 1 then begin
    FBreakExecCS.Acquire;
    try
      FICommandText := nil;
    finally
      FBreakExecCS.Release;
    end;
    FICommandPrepare := nil;
    FICommandProperties := nil;
  end;

  Dec(FQueryIntCnt);
end;

procedure TOLEDBCommand.ClearIMultipleResults;
begin
  FIMultipleResults := nil; // Rowsets not also provided

  if ((FIAbort <> nil) and (GetCursorState = csFetching)) then
    FIAbort.Abort;
  FIAbort := nil;
end;

var
  AnsiCodePageName: string;

function GetCurrentAnsiCodePage: string;
  function LCIDToCodePage(ALcid: cardinal): Integer;
  const
    CP_ACP = 0;
    LOCALE_IDEFAULTANSICODEPAGE = $00001004;
    BufSize = 7;
  var
    ResultCode: Integer;
    Buffer: array [0..BufSize - 1] of Char;
  begin
    GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, BufSize);
    Val(Buffer, Result, ResultCode);
    if ResultCode <> 0 then
      Result := CP_ACP;
  end;
var
  CodePage: integer;
  pIMultiLanguage: IMultiLanguage;
  CodePageInfo: TMIMECPInfo;
begin
  if AnsiCodePageName = '' then begin
    CodePage := LCIDToCodePage(GetThreadLocale);
    AnsiCodePageName := 'ASCII';
    if CoCreateInstance(CLSID_CMultiLanguage, nil, CLSCTX_INPROC_SERVER, IID_IMultiLanguage, pIMultiLanguage) = S_OK then
      if pIMultiLanguage.GetCodePageInfo(CodePage, CodePageInfo) = S_OK then
        AnsiCodePageName := CodePageInfo.wszWebCharset;
  end;
  Result := AnsiCodePageName;
end;

procedure TOLEDBCommand.SetCommandProp;
var
  OLEDBProperties: TOLEDBPropertiesSet;
  Str: WideString;
  g: TGUID;
begin
{$IFDEF AUTOTEST}
  Inc(__SetCommandPropCount);
{$ENDIF}
  Assert(FICommandText <> nil);

  Str := WideString(FSQL);
  g := DBGUID_DEFAULT;

  Check(FICommandText.SetCommandText(g, PWideChar(Str)));

  OLEDBProperties := TOLEDBPropertiesSet.Create(GetConnector, DBPROPSET_ROWSET);
  try
    if FConnection.Provider <> prCompact then
      OLEDBProperties.AddPropInt(DBPROP_COMMANDTIMEOUT, FCommandTimeout);
    if not FRequestResultSet and FNonBlocking then begin
      if not (ProviderIsNativeClient(GetConnector.FProviderId) or ProviderIsMSOLEDBDriver(GetConnector.FProviderId)) then
        raise Exception.Create(SSQLNCLINeeds);
      OLEDBProperties.AddPropInt(DBPROP_ROWSET_ASYNCH, DBPROPVAL_ASYNCH_INITIALIZE);
    end;
    OLEDBProperties.SetProperties(FICommandProperties);
  finally
    OLEDBProperties.Free;
  end;

  if FNotification and not FDelayedSubsciption then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(GetConnector, DBPROPSET_SQLSERVERROWSET);
    try
      OLEDBProperties.AddPropStr(SSPROP_QP_NOTIFICATION_MSGTEXT, FNotificationMessage, True);
      OLEDBProperties.AddPropUInt(SSPROP_QP_NOTIFICATION_TIMEOUT, FNotificationTimeout);
      OLEDBProperties.AddPropStr(SSPROP_QP_NOTIFICATION_OPTIONS, Format('service=%s', [FNotificationService]), True);
      OLEDBProperties.SetProperties(FICommandProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;

  if GetUseOutputStream then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(GetConnector, DBPROPSET_STREAM);
    try
      case FOutputEncoding of
        oeANSI:
          OLEDBProperties.AddPropStr(DBPROP_OUTPUTENCODING, GetCurrentAnsiCodePage);
        oeUTF8:
          OLEDBProperties.AddPropStr(DBPROP_OUTPUTENCODING, 'UTF-8');
        oeUnicode:
          OLEDBProperties.AddPropStr(DBPROP_OUTPUTENCODING, 'Unicode');
        else
          Assert(False);
      end;
      OLEDBProperties.AddPropIntf(DBPROP_OUTPUTSTREAM, FOLEDBStream, True);
      OLEDBProperties.SetProperties(FICommandProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
end;

procedure TOLEDBCommand.SetParameterInfo;
var
  CommandWithParameters: ICommandWithParameters;
  SSCommandWithParameters: ISSCommandWithParameters;
  i, j: integer;
  rgParamOrdinals: array of DB_UPARAMS;
  rgParamBindInfo: array of TDBParamBindInfo;
  ParamDesc: TParamDesc;
  hr: HResult;
  ParamCount: integer;

  ParamVarType: TVarType;
  prgParamOrdinals: IntPtr;
  prgParamBindInfo: IntPtr;
  IsUnicode: boolean;
  ParamName: string;

  TableObject: TMSSQLTableObject;
  FieldsArray: Variant;
  OLEDBParamPropertiesSet: TOLEDBParamPropertiesSet;
  Database, SchemaName, TableTypeName: string;

begin
  QueryIntf(FICommandText, IID_ICommandWithParameters, CommandWithParameters);
  CommandWithParameters.SetParameterInfo(0, nil, nil); // Clear, just in case

  ParamCount := FParams.Count;
  if ParamCount <> 0 then begin
    OLEDBParamPropertiesSet := TOLEDBParamPropertiesSet.Create(GetConnector, DBPROPSET_SQLSERVERPARAMETER);
    try

      SetLength(rgParamOrdinals, ParamCount);
      SetLength(rgParamBindInfo, ParamCount);
      try
        for i := 0 to ParamCount - 1 do begin
          ParamDesc := FParams[i];
          rgParamOrdinals[i] := i + 1;

          case ParamDesc.GetDataType and varTypeMask of
            dtUnknown: begin
              ParamVarType := VarType(ParamDesc.Value);
              case ParamVarType of
                varSmallint: { vt_i2           2 }
                  rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
                varInteger:  { vt_i4           3 }
                  rgParamBindInfo[i].pwszDataSourceType := dstInt;
                varSingle:   { vt_r4           4 }
                  rgParamBindInfo[i].pwszDataSourceType := dstReal;
                varDouble:   { vt_r8           5 }
                  rgParamBindInfo[i].pwszDataSourceType := dstFloat;
                varCurrency: { vt_cy           6 }
                  rgParamBindInfo[i].pwszDataSourceType := dstMoney;
                varDate:     { vt_date         7 }
                  rgParamBindInfo[i].pwszDataSourceType := dstDatetime;
                varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: { vt_bstr         8 }
                  rgParamBindInfo[i].pwszDataSourceType := dstNVarchar;
                varString:
                  rgParamBindInfo[i].pwszDataSourceType := dstVarchar;
                varBoolean:  { vt_bool        11 }
                  rgParamBindInfo[i].pwszDataSourceType := dstBit;
                varShortInt:
                  rgParamBindInfo[i].pwszDataSourceType := dstTinyint;
                varWord:
                  rgParamBindInfo[i].pwszDataSourceType := dstInt;
                varInt64:    { vt_i8          20 }
                  rgParamBindInfo[i].pwszDataSourceType := dstBigint;
                varByte:     { vt_ui1         17 }
                  rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
                varLongWord: { vt_ui4         19 }
                  rgParamBindInfo[i].pwszDataSourceType := dstBigint;
                else
                  rgParamBindInfo[i].pwszDataSourceType := dstSql_variant;
              end;
            end;

            dtString, dtExtString:
              if IsOutputLOB(ParamDesc) and GetConnector.IsOutputLOBSupported then
                rgParamBindInfo[i].pwszDataSourceType := dstVarcharMax
              else
                rgParamBindInfo[i].pwszDataSourceType := dstVarchar;

            dtWideString, dtExtWideString:
              if IsOutputLOB(ParamDesc) and GetConnector.IsOutputLOBSupported then
                rgParamBindInfo[i].pwszDataSourceType := dstNVarcharMax
              else
                rgParamBindInfo[i].pwszDataSourceType := dstNVarchar;

            dtInt8, dtUInt8:
              rgParamBindInfo[i].pwszDataSourceType := dstTinyint;
            dtInt16:
              rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
            dtInt32, dtWord:
              rgParamBindInfo[i].pwszDataSourceType := dstInt;
            dtInt64, dtUInt64, dtUInt32:
              rgParamBindInfo[i].pwszDataSourceType := dstBigint;
            dtSingle:
              rgParamBindInfo[i].pwszDataSourceType := dstReal;
            dtFloat:
              rgParamBindInfo[i].pwszDataSourceType := dstFloat;
            dtDate, dtTime, dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}:
              rgParamBindInfo[i].pwszDataSourceType := dstDatetime;
            dtBoolean:
              rgParamBindInfo[i].pwszDataSourceType := dstBit;
            dtCurrency:
              rgParamBindInfo[i].pwszDataSourceType := dstMoney;
            dtBlob:
              if ParamDesc.GetSubDataType = sdtMSUDT then
                rgParamBindInfo[i].pwszDataSourceType := dstVarBinary
              else
                rgParamBindInfo[i].pwszDataSourceType := dstImage;
            dtMemo, dtWideMemo: begin
              case VarType(ParamDesc.Value) of
                varOleStr{$IFDEF VER12P}, varUString{$ENDIF}:
                  IsUnicode := True;
              else
                IsUnicode := False;
              end;
              if not IsUnicode
                and (VarType(ParamDesc.Value) = varSharedObject)
                and (TVarData(ParamDesc.Value).VPointer <> nil) then begin
                // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
                IsUnicode := TBlob(TVarData(ParamDesc.Value).VPointer).IsUnicode;
              end;

              if IsUnicode then
                rgParamBindInfo[i].pwszDataSourceType := dstNVarchar
              else
                rgParamBindInfo[i].pwszDataSourceType := dstVarchar;
            end;

            dtXML:
              rgParamBindInfo[i].pwszDataSourceType := dstXML;
            dtVariant:
              rgParamBindInfo[i].pwszDataSourceType := dstSql_variant;
            dtBytes:
              rgParamBindInfo[i].pwszDataSourceType := dstBinary;
            dtVarBytes, dtExtVarBytes:
              rgParamBindInfo[i].pwszDataSourceType := dstVarbinary;

            dtBCD, dtFmtBCD:
              rgParamBindInfo[i].pwszDataSourceType := dstMoney;

            dtGuid:
              rgParamBindInfo[i].pwszDataSourceType := dstGuid;

            dtTable: begin
              rgParamBindInfo[i].pwszDataSourceType := dstTable;
              ParamDesc.SetParamType(pdInput);
              TableObject := TMSSQLTableObject(ParamDesc.GetObject);
              if (TableObject <> nil) and not TableObject.GetIsNull then begin
                if TableObject.TableTypeName <> '' then begin
                  MSSQLInfo.SplitObjectName(TableObject.TableTypeName, Database, SchemaName, TableTypeName);
                  SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
                  TableTypeName := SQLInfo.NormalizeName(TableTypeName, False, True);
                  if SchemaName <> '' then
                    OLEDBParamPropertiesSet.AddProp(SSPROP_PARAM_TYPE_SCHEMANAME, SchemaName, i + 1);
                  OLEDBParamPropertiesSet.AddProp(SSPROP_PARAM_TYPE_TYPENAME, TableTypeName, i + 1);
                end;

                FieldsArray := null;
                for j := 0 to TableObject.RecordSet.Fields.Count - 1 do begin
                  if TableObject.RecordSet.Fields[j].ReadOnly then begin
                    if VarIsNull(FieldsArray) then
                      FieldsArray := VarArrayCreate([0, 0], VT_UI2)
                    else
                      VarArrayRedim(FieldsArray, VarArrayHighBound(FieldsArray, 1) + 1);
                    FieldsArray[VarArrayHighBound(FieldsArray, 1)] := TableObject.RecordSet.Fields[j].ActualFieldNo;
                  end;
                end;

                if not VarIsNull(FieldsArray) then
                  OLEDBParamPropertiesSet.AddProp(SSPROP_PARAM_TABLE_DEFAULT_COLUMNS, FieldsArray, i + 1);
              end;
            end;

          else
            Assert(False, Format('Unknown datatype for param %s[%d] = %X', [ParamDesc.GetName, i, ParamDesc.GetDataType]));
          end;

          case ParamDesc.GetDataType of
            dtString, dtExtString, dtBytes, dtVarBytes, dtExtVarBytes:
              if ParamDesc.GetSize > 0 then
                rgParamBindInfo[i].ulParamSize := ParamDesc.GetSize
              else
                rgParamBindInfo[i].ulParamSize := MaxNonBlobFieldLen;
            dtWideString, dtExtWideString:
              if ParamDesc.GetSize > 0 then
                rgParamBindInfo[i].ulParamSize := ParamDesc.GetSize
              else
                rgParamBindInfo[i].ulParamSize := MaxNonBlobFieldLen div SizeOf(WideChar);
            dtDate, dtTime: begin
              rgParamBindInfo[i].ulParamSize := SizeOf(TDateTime);
              rgParamBindInfo[i].bScale := 3;
            end;
            dtDatetime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
              rgParamBindInfo[i].ulParamSize := SizeOf(TDBTimeStamp);
              rgParamBindInfo[i].bScale := 3;
            end;
          else
            rgParamBindInfo[i].ulParamSize := High(NativeUInt);
          end;
          case ParamDesc.GetParamType of
            pdInput:
              rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISNULLABLE;
            pdOutput, pdResult:
              rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISOUTPUT + DBPARAMFLAGS_ISNULLABLE;
            pdUnknown, pdInputOutput:
              rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT + DBPARAMFLAGS_ISNULLABLE;
          end;

          if (FConnection.Provider = prCompact) and (ParamDesc.GetName <> '') then begin
            ParamName := ParamDesc.GetName;
            rgParamBindInfo[i].pwszName := Marshal.AllocHGlobal(Length(ParamName) * SizeOf(WideChar) + SizeOf(WideChar));
            CopyBuffer(Marshal.StringToHGlobalUni(WideString(ParamName)), rgParamBindInfo[i].pwszName, Length(ParamName) * SizeOf(WideChar) + SizeOf(WideChar));
          end
          else
            rgParamBindInfo[i].pwszName := nil;
        end;

        prgParamOrdinals := @rgParamOrdinals[0];
        prgParamBindInfo := @rgParamBindInfo[0];
        hr := CommandWithParameters.SetParameterInfo(ParamCount, prgParamOrdinals, prgParamBindInfo);
      finally
        if FConnection.Provider = prCompact then
          for i := 0 to ParamCount - 1 do begin
            if rgParamBindInfo[i].pwszName <> nil then
              Marshal.FreeHGlobal(rgParamBindInfo[i].pwszName);
            rgParamBindInfo[i].pwszName := nil;
          end;
      end;

      if hr <> DB_S_TYPEINFOOVERRIDDEN then
        Check(hr);

      if OLEDBParamPropertiesSet.PropCount > 0 then begin
        QueryIntf(FICommandText, IID_ISSCommandWithParameters, SSCommandWithParameters);
        Check(OLEDBParamPropertiesSet.SetParameterProperties(SSCommandWithParameters));
      end;
    finally
      OLEDBParamPropertiesSet.Free;
    end;
  end;
end;

procedure TOLEDBCommand.GetParameterInfo;
var
  CommandWithParameters: ICommandWithParameters;
  cParams: NativeUInt;
  prgParamInfo, prgParamInfoEl: PDBPARAMINFO;
  ppNamesBuffer: PPOleStr;
  ParamDesc: TParamDesc;
  rgParamInfo: PDBPARAMINFO;
  DataType: word;
  IsLong: Boolean;
  i: Integer;
begin
  QueryIntf(FICommandText, IID_ICommandWithParameters, CommandWithParameters);

  ppNamesBuffer := nil;
  try
    Check(CommandWithParameters.GetParameterInfo(cParams, prgParamInfo, ppNamesBuffer));
    prgParamInfoEl := prgParamInfo;

    for i := 0 to integer(cParams) - 1 do begin
      if i >= FParams.Count then
        break;

      ParamDesc := FParams[i];
      rgParamInfo := prgParamInfoEl;

      case rgParamInfo.dwFlags and (DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT) of
        DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT:
          ParamDesc.SetParamType(pdInputOutput);
        DBPARAMFLAGS_ISINPUT:
          ParamDesc.SetParamType(pdInput);
        DBPARAMFLAGS_ISOUTPUT:
          if ParamDesc.GetParamType <> pdResult then
            ParamDesc.SetParamType(pdOutput);
      end;

      IsLong := (rgParamInfo.dwFlags and DBPARAMFLAGS_ISLONG) <> 0;
      if ConvertOLEDBTypeToInternalFormat(rgParamInfo.wType, IsLong, True,
         CalcEnableBCD, CalcEnableFMTBCD, False, {$IFDEF LITE}0, 0,{$ENDIF} 0, 0,
         True, FConnection.Options.WideMemos, True, DataType, FConnection.Provider)
      then begin
        ParamDesc.SetDataType(DataType);
        if rgParamInfo.ulParamSize <> High(NativeUInt) then
          ParamDesc.SetSize(rgParamInfo.ulParamSize);

        case DataType of
          dtString, dtWideString, dtMemo, dtWideMemo, dtXML:
            if (rgParamInfo.wType = DBTYPE_WSTR) or (rgParamInfo.wType = DBTYPE_XML) then
              ParamDesc.SetSubDataType(sdtWide);
          dtBlob:
            if rgParamInfo.wType = DBTYPE_UDT then
              ParamDesc.SetSubDataType(sdtMSUDT);
        end;

        prgParamInfoEl := PtrOffset(prgParamInfoEl, sizeof(DBPARAMINFO));
      end;
    end;
  finally
    FreeCoMem(prgParamInfo);
    FreeCoMem(ppNamesBuffer);
  end;
end;

procedure TOLEDBCommand.Prepare;
begin
  if GetPrepared then
    Exit;

  QueryInterfaces(True);
  try
    FRPCCall := FIsSProc and __UseRPCCallStyle;
    SetCommandProp;
    if FRPCCall or (not FUseDescribeParams and not ParamsInfoOldBehavior) then
      SetParameterInfo;

    Check(FICommandPrepare.Prepare(0)); // If statement is wrong in some cases exception may be occured in other place
  {$IFDEF AUTOTEST}
    Inc(__ServerPrepareCount);
  {$ENDIF}
    if FUseDescribeParams then
      GetParameterInfo;

    inherited;
    FPrepared := True;
  except
    ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBCommand.Unprepare;
begin
  if GetPrepared then begin
    FIUnknown := nil;
    RequestParamsIfPossible;

    if (FICommandPrepare <> nil) and not FRPCCall then
      Check(FICommandPrepare.UnPrepare);
    inherited;
    FPrepared := False;
    FRPCCall := False;

    ReleaseInterfaces;
  end;
end;

function TOLEDBCommand.GetPrepared: boolean;
begin
  Result := FPrepared;
end;

procedure TOLEDBCommand.SetOutputStream(Stream: TStream);
begin
  if Stream <> nil then
    FOLEDBStream := TOLEDBStream.Create(Stream)
  else
  if FOLEDBStream <> nil then begin
    FOLEDBStream._Release;
    FOLEDBStream := nil;
  end;
end;

function TOLEDBCommand.GetUseOutputStream: boolean;
begin
  Result := FOLEDBStream <> nil;
end;

function NeedOptimizationByRefParameter(ParamDesc: TParamDesc; Connection: TMSSQLConnection): boolean;
begin
  // Optimization for input-only parameters, store by ref
  Result := (((ParamDesc.GetParamType = pdInput) and
    (ParamDesc.GetDataType in CharsByRef + BytesByRef))
    or (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo]))
    and (Connection.Provider <> prCompact)
    and not (IsOutputLOB(ParamDesc) and TOLEDBConnector(Connection.GetConnector).IsOutputLOBSupported);
end;

procedure FillBindingForParam(Ordinal: integer; ParamDesc: TMSParamDesc; Connection: TMSSQLConnection;
  var pBind: TDBBinding; var BindMemorySize: NativeUInt; const ValueAvaliable: boolean;
  const IsWide: boolean; BatchIters: integer = 0);

  procedure SetBindDefaults;
  begin
    pBind.dwPart := DBPART_STATUS or DBPART_LENGTH or DBPART_VALUE; // WAR Length is not always need and may be removed in some cases
    pointer(pBind.pTypeInfo) := nil;
    pBind.pBindExt := nil;
    pBind.dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    pBind.dwFlags := 0;
    pBind.bPrecision := 11;
    pBind.bScale := 0;
  end;

  procedure SetBindData;
    function GetMaxLen: NativeInt; // Also correct pBind.wType. Must be called after ConvertInternalTypeToOLEDB
    var
      IsUnicode: boolean;
      DataType: word;
      ParamVarType: TVarType;
      ParamValuePtr: PVariant;
      i: integer;
    begin
      DataType := ParamDesc.GetDataType;
      case DataType of
        dtUnknown:
          Result := SizeOf_OleVariant;
        dtString:
          if not ValueAvaliable or (ParamDesc.GetSize = 0) then
            Result := MaxNonBlobFieldLen
          else
            Result := ParamDesc.GetSize + 1{#0};
        dtWideString:
          if not ValueAvaliable or (ParamDesc.GetSize = 0) then
            Result := MaxNonBlobFieldLen * sizeof(WideChar)
          else
            Result := (ParamDesc.GetSize + 1{#0}) * sizeof(WideChar);
        dtBytes, dtVarBytes:
          Result := ParamDesc.GetSize;
        dtBoolean:
          Result := sizeof(WordBool);
        dtInt8, dtUInt8:
          Result := sizeof(Byte);
        dtInt16:
          Result := sizeof(SmallInt);
        dtWord:
          if Connection.Provider = prCompact then
            Result := sizeof(Word)
          else
            Result := sizeof(Integer);
        dtInt32:
          Result := sizeof(Integer);
        dtUInt32:
          if Connection.Provider = prCompact then
            Result := sizeof(Integer)
          else
            Result := sizeof(Int64);
        dtInt64, dtUInt64:
          Result := sizeof(Int64);
        dtSingle:
          Result := sizeof(Single);
        dtFloat:
          if pBind.wType = DBTYPE_NUMERIC then
            Result := SizeOf_TDBNumeric
          else
            Result := sizeof(Double);
        dtCurrency:
          Result := sizeof(Double);
        dtDate:
          Result := sizeof(TDateTime);
        dtTime: begin
          if (Connection.ServerMajorVer >= 10) and (Connection.ClientMajorVer >= 10) then begin
            Result := sizeof(TDBTime);
            pBind.wType := DBTYPE_DBTIME;
          end
          else
            Result := sizeof(TDateTime);
        end;
        dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
          Result := sizeof(TDBTimeStamp);
          pBind.wType := DBTYPE_DBTIMESTAMP;
        end;
        dtXML: begin
          Result := SizeOf_OleVariant;
          pBind.wType := DBTYPE_VARIANT;
        end;
        dtBlob, dtMemo, dtWideMemo:
          if Connection.Provider <> prCompact then
            if IsOutputLOB(ParamDesc) and TOLEDBConnector(Connection.GetConnector).IsOutputLOBSupported then
              Result := sizeof(TOLEDBStream) // varchar(max)
            else
              Result := 0 // Only Input, store ByRef
          else begin
            if ((DataType = dtMemo) or (DataType = dtWideMemo)) {$IFDEF LITE} and ((not sdtWide) and ParamDesc.GetSubDataType = dtXML){$ENDIF} then begin
              Result := TBlob(ParamDesc.GetObject).Size + 2{#0#0};

              if (ParamDesc.GetSubDataType and sdtWide) = 0 then
                pBind.wType := DBTYPE_STR
              else
                pBind.wType := DBTYPE_WSTR;
            end
            else
              Result := sizeof(TOLEDBStream);
          end;
        dtGuid:
          Result := sizeof(TGuid);
        dtVariant:
          Result := SizeOf_OleVariant;
        dtBCD: begin
          if pBind.wType = DBTYPE_NUMERIC then
            Result := SizeOf_TDBNumeric
          else
            Result := sizeof(Currency);
        end;

        dtFmtBCD, dtExtended: begin
          if Connection.Provider <> prCompact then begin
            Result := SizeOfTBcd * 2;
            pBind.wType := DBTYPE_STR;
          end
          else
            Result := SizeOf_TDBNumeric;
        end;
        dtTable:
          Result := sizeof(TObject);
      else
        Result := 0;
      end;

      if (Connection.Provider = prCompact) and IsLargeDataTypeUsed(ParamDesc) and ParamDesc.GetNull then begin
        Result := 0;
        case DataType of
          dtBlob:
            pBind.wType := DBTYPE_BYTES;
          dtMemo, dtWideMemo:
            pBind.wType := DBTYPE_WSTR;
          else
            Assert(False);
        end;
      end;

      if NeedOptimizationByRefParameter(ParamDesc, Connection) then begin
        // Optimization for input-only parameters, store by ref
        Result := SizeOf(IntPtr);
        if DataType in BytesByRef then // This is Bytes by Ref
          pBind.wType := DBTYPE_BYREF or DBTYPE_BYTES
        else begin // This is (Wide)String by Ref
          ParamValuePtr := ParamDesc.GetItemPtr(0);
          ParamVarType := VarType(ParamValuePtr^);

          if ParamVarType = 0 then
            for i := 1 to BatchIters - 1 do begin
              ParamValuePtr := ParamDesc.GetItemPtr(i);
              ParamVarType := VarType(ParamValuePtr^);
              if ParamVarType <> 0 then
                break;
            end;

          if not ValueAvaliable then
            IsUnicode := (ParamDesc.GetDataType in [dtWideString, dtExtWideString, dtWideMemo]) or IsWide
          else
            case ParamVarType of
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}:
                IsUnicode := True;
            else
              IsUnicode := False;
            end;
          if not IsUnicode
             and (ParamVarType = varSharedObject)
             and (PVarData(ParamValuePtr).VPointer <> nil) then
            IsUnicode := TBlob(PVarData(ParamValuePtr).VPointer).IsUnicode;

          if IsUnicode then // WideString
            pBind.wType := DBTYPE_BYREF or DBTYPE_WSTR
          else // (VType = varString) String
            pBind.wType := DBTYPE_BYREF or DBTYPE_STR;
        end;
      end
      else
        if (ParamDesc.GetParamType in [pdOutput, pdInputOutput]) // Truncate params, if too long
          and (DataType in [dtString, dtWideString, dtBytes, dtVarBytes]) then begin
          if not ((Connection.ServerMajorVer >= 9) and (Connection.ClientMajorVer >= 9) and (Result > MaxNonBlobFieldLen)) then
            if DataType = dtString then
              Result := MaxNonBlobFieldLen + 1
            else
              Result := MaxNonBlobFieldLen;
        end;

      Assert(Result >= 0);
    end;

  var
    Provider: TMSProvider;
    Obj: {$IFNDEF UNIDACPRO}OLEDBIntf{$ELSE}OLEDBIntfUni{$ENDIF}.DBOBJECT;
  begin
    pBind.iOrdinal := Ordinal;
    pBind.eParamIO := ConvertCRParamTypeToOLEDB(ParamDesc.GetParamType);
    if IsOutputLOB(ParamDesc) and not TOLEDBConnector(Connection.GetConnector).IsOutputLOBSupported then // not "varchar(max)"
      DatabaseErrorFmt(SBadOutputParam, [ParamDesc.GetName]);

    // int64 parameters conversion
    if (Connection.ClientMajorVer < 8) and not IsWindowsVista and
      (Connection.Provider <> prCompact) and (ParamDesc.GetDataType = dtInt64) then
      ParamDesc.SetDataType(dtFloat);

    // currency parameters conversion
    if (ParamDesc.GetDataType = dtCurrency) and (VarType(ParamDesc.Value) = varCurrency) then
      ParamDesc.SetDataType(dtBCD); // To prevent SQL Server exception on setting float value to smallmoney parameter

    if Connection <> nil then
      Provider := Connection.Provider
    else
      Provider := prAuto;
    pBind.wType := ConvertInternalTypeToOLEDB(ParamDesc.GetDataType, ParamDesc.GetSubDataType, Provider);

    pBind.obStatus := BindMemorySize;
    pBind.obLength := pBind.obStatus + sizeof(DBSTATUS);
    pBind.obValue := pBind.obLength + sizeof(DBLENGTH);

    pBind.cbMaxLen := GetMaxLen; // Also correct pBind.wType. Must be called after ConvertInternalTypeToOLEDB
    if not ((pBind.wType = DBTYPE_BYTES) or (pBind.wType = DBTYPE_WSTR)) then
      Assert(pBind.cbMaxLen > 0, Format('Unknown datatype for param %s[%d] = %X', [ParamDesc.GetName, Ordinal, ParamDesc.GetDataType]));
    BindMemorySize := pBind.obValue + pBind.cbMaxLen;

    if pBind.wType = DBTYPE_IUNKNOWN then begin
      Obj.iid := IID_ISequentialStream;
      if pBind.eParamIO = DBPARAMIO_INPUT then
        Obj.dwFlags := STGM_READ
      else
        Obj.dwFlags := STGM_READWRITE;
      pBind.pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
      DBOBJECT(pBind.pObject^) := Obj;
    end;
    if pBind.wType = DBTYPE_TABLE then begin
      Obj.iid := IID_IRowset;
      Obj.dwFlags := 0;
      pBind.pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
      DBOBJECT(pBind.pObject^) := Obj;
    end;
  end;

begin
  SetBindDefaults;
  SetBindData;
end;

procedure SaveParamValue(const ParamDesc: TParamDesc; const pBind: TDBBinding;
  var ParamsAccessorData: TParamsAccessorData;
  {$IFDEF HAVE_COMPRESS}const CompressBlobMode: TCompressBlobMode;{$ENDIF}
  Connection: TMSSQLConnection; ItemIndex: integer = 0);
var
  pValue, pLength, pStatus: IntPtr;
  // pParamData: PVarData;
  ParamVarType: TVarType;
  ParamVarPtr: IntPtr;
  ParamValuePtr: PVariant;

  sa: AnsiString;
  ws: WideString;
  s: string;
  l: UINT;
  c: Currency;
  i64: Int64;

  Blob: TBlob;
  DotPos: integer;
  Bcd: TBcd;
{$IFDEF HAVE_COMPRESS}
  Compress: boolean;
{$ENDIF}
  dt: TDateTime;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  DBTimeStamp: TDBTimeStamp;
{$IFNDEF FPC}
  SQLTimeStamp: TSQLTimeStamp;
{$ENDIF}
  Stream: IntPtr;
  OLEDBStream: TOLEDBStream;
  DBTime: TDBTime;
  Rowset: IRowset;
begin
  if pBind.eParamIO in [DBPARAMIO_INPUT, DBPARAMIO_INPUT + DBPARAMIO_OUTPUT] then begin
    pStatus := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obStatus);
    pValue := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obValue); // Destination

    if not TMSParamDesc(ParamDesc).GetIsBound then begin
      FillChar(pValue, pBind.cbMaxLen, 0);
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_DEFAULT));
    end
    else
    if ParamDesc.ItemNull[ItemIndex] then begin
      FillChar(pValue, pBind.cbMaxLen, 0);
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL));
    end
    else begin
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_OK));

      pLength := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obLength);
      ParamValuePtr := ParamDesc.GetItemPtr(ItemIndex);
      ParamVarType := VarType(ParamValuePtr^);
      // pParamData := @TVarData(ParamDesc.Value); // Source

      if NeedOptimizationByRefParameter(ParamDesc, Connection) then begin
        // Optimization for input-only parameters, store by ref
        if ParamVarType = varArray + varByte then begin
          ParamVarPtr := PVarData(ParamValuePtr).VArray.Data;
          l := PVarData(ParamValuePtr).VArray.Bounds[0].ElementCount;
          Marshal.WriteIntPtr(pValue, ParamVarPtr);
          Marshal.WriteInt32(pLength, l);
        end
        else if ParamVarType = varSharedObject then begin
          Assert(PVarData(ParamValuePtr).VPointer <> nil);
          // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
          Blob := PVarData(ParamValuePtr).VPointer;
          Blob.Defrag;
        {$IFDEF HAVE_COMPRESS}
          Compress := Blob is TCompressedBlob;
          if Compress then
            TCompressedBlob(Blob).Compressed := (CompressBlobMode = cbServer) or (CompressBlobMode = cbClientServer);
          if Compress and TCompressedBlob(Blob).Compressed then
            Marshal.WriteInt32(pLength, Integer(TCompressedBlob(Blob).CompressedSize))
          else
        {$ENDIF}
            Marshal.WriteInt32(pLength, Integer(Blob.Size));

          if IntPtr(Blob.FirstPiece) = nil then begin
            if ParamDesc.GetDataType in [dtString, dtMemo] then
              Marshal.WriteIntPtr(pValue, PAnsiChar(EmptyString))
            else
            if ParamDesc.GetDataType in [dtWideString, dtWideMemo] then
              Marshal.WriteIntPtr(pValue, PWideChar(EmptyWString))
            else
              Marshal.WriteIntPtr(pValue, nil);
          end
          else
            Marshal.WriteIntPtr(pValue, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)));
        end
        else begin // CharsByRef Input parameter
          ParamVarPtr := PVarData(ParamValuePtr).VPointer;
          if (ParamVarPtr = nil) and (VarIsEmpty(ParamValuePtr^) or (ParamValuePtr^ = '')) then
            if (ParamVarType = varOleStr) {$IFDEF VER12P} or (ParamVarType = varUString) {$ENDIF} then // WideString
              ParamVarPtr := PWideChar(EmptyWString)
            else
              ParamVarPtr := PAnsiChar(EmptyString);
          Marshal.WriteIntPtr(pValue, ParamVarPtr);
          if (ParamDesc.GetDataType in CharsByRef) then
            if ParamVarPtr <> nil then begin
              if (ParamVarType = varOleStr) {$IFDEF VER12P} or (ParamVarType = varUString) {$ENDIF} then // WideString
                l := Length(WideString(ParamValuePtr^)) * SizeOf(WideChar)
              else // Pascal string
              {$IFDEF VER12P}
                l := Length(AnsiString(ParamValuePtr^));
              {$ELSE}
                l := Length(ParamValuePtr^);
              {$ENDIF}
            end
            else
              l := 0
          else
            l := Integer(ParamDesc.GetSize);
          Marshal.WriteInt32(pLength, l);
        end;
      end
      else
        case ParamDesc.GetDataType of
          dtUnknown:
            SetOleVariant(pValue, ParamValuePtr^);
          dtString: begin
            sa := AnsiString(ParamValuePtr^);
            l := Length(sa);
            if l > pBind.cbMaxLen - 1{#0} then // Truncate too long values
              l := pBind.cbMaxLen - 1{#0};
            Marshal.WriteInt32(pLength, l);

            if l > 0 then
              CopyBufferAnsi(sa, pValue, l + 1{#0});
          end;
          dtWideString: begin
            ws := ParamValuePtr^;
            l := Length(ws) * sizeof(WideChar);
            if l > (pBind.cbMaxLen - 2{#0}){ * sizeof(WideChar)} then // Truncate too long values
              l := (pBind.cbMaxLen - 2{#0}){ * sizeof(WideChar)};
            Marshal.WriteInt32(pLength, l);

            if l > 0 then
              CopyBufferUni(ws, pValue, l + 2{#0#0});
          end;
          dtBytes, dtVarBytes: begin
            case ParamVarType of
              varArray + varByte: begin
                l :=  PVarData(ParamValuePtr).VArray.Bounds[0].ElementCount;
                if l > pBind.cbMaxLen then // Truncate too long values
                  l := pBind.cbMaxLen;
                Move(PVarData(ParamValuePtr).VArray.Data^, pValue^, l);
                Marshal.WriteInt32(pLength, l);
              end;
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
                sa := AnsiString(ParamValuePtr^);
                l :=  Length(sa);
                if l > pBind.cbMaxLen {without #0} then // Truncate too long values
                  l := pBind.cbMaxLen {without #0};
                Marshal.WriteInt32(pLength, l);

                if l > 0 then
                  CopyBufferAnsi(sa, pValue, l);
              end;
              else
                raise {$IFDEF NODBACCESS}Exception{$ELSE}EDatabaseError{$ENDIF}.Create('Unknown BLOB field type (must be varArray + varByte, varOleStr)');
            end;
          end;
          dtInt8, dtUInt8:
            Marshal.WriteByte(pValue, Byte(ParamValuePtr^));
          dtInt16:
            Marshal.WriteInt16(pValue, SmallInt(ParamValuePtr^));
          dtWord:
            if Connection.Provider = prCompact then
              Marshal.WriteInt16(pValue, SmallInt(Word(ParamValuePtr^)))
            else
              Marshal.WriteInt32(pValue, Integer(Word(ParamValuePtr^)));
          dtInt32:
            Marshal.WriteInt32(pValue, Integer(ParamValuePtr^));
          dtUInt32:
            if Connection.Provider = prCompact then
              Marshal.WriteInt32(pValue, Integer(cardinal(ParamValuePtr^)))
            else begin
              i64 := ParamValuePtr^; // Explicit Convert!
              Marshal.WriteInt64(pValue, i64);
            end;
          dtSingle:
            Marshal.WriteInt32(pValue, CRBitConverter.SingleToInt32Bits(ParamValuePtr^));
          dtFloat: begin
            if pBind.wType = DBTYPE_NUMERIC then
              PDBNumeric(pValue)^ := DoubleToDBNumeric(Double(ParamValuePtr^), 0, 0)
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamValuePtr^)));
          end;
          dtCurrency: begin
            if pBind.wType = DBTYPE_CY then begin
              c := ParamValuePtr^;
              PCurrency(pValue)^ := c;
            end
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamValuePtr^)));
          end;
          dtDate:
            Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(TDateTime(ParamValuePtr^)));
          dtTime: begin
            if pBind.wType = DBTYPE_DBTIME then begin
              dt := TDateTime(ParamValuePtr^);
              DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
              DBTime.hour := AHour;
              DBTime.minute := AMinute;
              DBTime.second := ASecond;
              DBTime.fraction := AMilliSecond * 1000000; // milliseconds to billionths of a second
              PDBTime(pValue)^ := DBTime;
            end
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(TDateTime(ParamValuePtr^)));
          end;
          dtDateTime: begin
            dt := TDateTime(Double(ParamValuePtr^));
            DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
            DBTimeStamp.year := AYear;
            DBTimeStamp.month := AMonth;
            DBTimeStamp.day := ADay;
            DBTimeStamp.hour := AHour;
            DBTimeStamp.minute := AMinute;
            DBTimeStamp.second := ASecond;
            DBTimeStamp.fraction := AMilliSecond * 1000000; // milliseconds to billionths of a second
            PDBTimeStamp(pValue)^ := DBTimeStamp;
          end;
        {$IFNDEF FPC}
          dtSQLTimeStamp: begin
            SQLTimeStamp := VarToSQLTimeStamp(ParamValuePtr^);
            SQLTimeStamp.Fractions := SQLTimeStamp.Fractions * 1000000; // milliseconds to billionths of a second
            PSQLTimeStamp(pValue)^ := SQLTimeStamp;
          end;
        {$ENDIF}
          dtBoolean:
            Marshal.WriteInt16(pValue, SmallInt(WordBool(Boolean(ParamValuePtr^)))); // Convert to boolean is useful to bypass Delphi bug
          dtInt64, dtUInt64: begin
            i64 := ParamValuePtr^; // Explicit Convert!
            Marshal.WriteInt64(pValue, i64);
          end;
          dtGuid:
            if not VarIsEmpty(ParamValuePtr^) and not VarIsNull(ParamValuePtr^) then
              PGuid(pValue)^ := StrToGUID(ParamValuePtr^);
          dtVariant:
            SetOleVariant(pValue, ParamValuePtr^);
          dtBCD:
            if pBind.wType = DBTYPE_NUMERIC then
              PDBNumeric(pValue)^ := DoubleToDBNumeric(ParamValuePtr^, 0, 0)
            else begin
              c := ParamValuePtr^;
              PCurrency(pValue)^ := c;
            end;
          dtFmtBCD, dtExtended: begin
            if pBind.wType = DBTYPE_NUMERIC then begin
              if VarIsFMTBcd(ParamValuePtr^) then
                PDBNumeric(pValue)^ := BcdToDBNumeric(VarToBcd(ParamValuePtr^))
              else begin
                s := string(ParamValuePtr^);
                Bcd := StrToBcd(s);
                PDBNumeric(pValue)^ := BcdToDBNumeric(Bcd);
              end;
            end
            else begin
              sa := AnsiString(ParamValuePtr^);
              if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
                DotPos := Pos(AnsiString({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator), sa);
                if DotPos <> 0 then
                  sa[DotPos] := '.';
              end;
              l := Length(sa);
              Marshal.WriteInt32(pLength, l);

              if l > 0 then
                CopyBufferAnsi(sa, pValue, l + 1{#0});
            end;
          end;
          dtXML:
            SetOleVariant(pValue, ParamValuePtr^);
          dtMemo, dtWideMemo, dtBlob:
            if pBind.wType = DBTYPE_IUNKNOWN then begin
              // Create stream
              OLEDBStream := TOLEDBStream.Create(TBlob(ParamDesc.GetObject), nil);
              Stream := Marshal.GetIUnknownForObject(OLEDBStream);
              Marshal.WriteIntPtr(pValue, Stream);
              // Set stream size
              if Connection.Provider = prCompact then
                Marshal.WriteInt32(pLength, Integer(OLEDBStream.Size))
              else
                Marshal.WriteInt32(pLength, -1);
            end
            else begin
              ws := TBlob(ParamDesc.GetObject).AsWideString;
              l := Length(ws) * sizeof(WideChar) + 2{#0#0};
              if l > (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar) then // Truncate too long values
                l := (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar);
              Marshal.WriteInt32(pLength, l);

              if l > 0 then
                CopyBufferUni(ws, pValue, l);
            end;
          dtTable: begin
            Assert(IsClass(ParamDesc.GetObject, TMSSQLTableObject));
            Rowset := TOLEDBTableTypeRecordSet(TMSSQLTableObject(ParamDesc.GetObject).RecordSet).GetFilledRowset;
            Rowset._AddRef;
            Marshal.WriteIntPtr(pValue, IntPtr(Rowset));
            Marshal.WriteInt32(pLength, -1);
          end;
        else
          Assert(False, Format('ParamDesc - %s, Unknown DataType = %d', [ParamDesc.GetName, ParamDesc.GetDataType]));
        end;
      end;
  end;
end;

procedure TOLEDBCommand.CreateAndFillParamAccs;
var
  OwnerCommand: TOLEDBCommand;
  ParamCnt, OwnerParamCnt: integer;
  BatchIters: integer;

  procedure PrepareParamAcc(out BindMemorySize: NativeUInt); // Set preliminary binding data
  var
    i: integer;
    ParamDesc: TMSParamDesc;
  begin
    Assert(Length(FParamsAccessorData.rgBindings) <> 0);
    for i := 0 to ParamCnt - 1 do
      FParamsAccessorData.rgBindings[i].pObject := nil;

    BindMemorySize := 0;
    for i := 0 to ParamCnt - 1 do begin
      if OwnerCommand = nil then
        ParamDesc := TMSParamDesc(FParams[i])
      else
        ParamDesc := TMSParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamCnt].ParamRef);

      FillBindingForParam(i + 1, ParamDesc, FConnection, FParamsAccessorData.rgBindings[i], BindMemorySize, True, False, BatchIters);
    end;
  end;

var
  rgBindings: PDBBinding;
  rgStatus: PDBBINDSTATUSArray;
  i: integer;
  BindMemorySize: NativeUInt;
  TableObject: TMSSQLTableObject;
  ParamDesc: TMSParamDesc;
  ItemIndex: integer;
begin
  Assert(FICommandText <> nil, 'FICommandText must be setted to CreateAndFillParamAccs');
  Assert(not FParamsAccessorDataAvaible, 'procedure CreateAndFillParamAccs already called');

  OwnerCommand := TOLEDBCommand(BatchOwner);
  if OwnerCommand = nil then begin
    OwnerParamCnt := 0;
    ParamCnt := FParams.Count;
    BatchIters := 0;
  end
  else begin
    OwnerParamCnt := OwnerCommand.FParamsInfo.Count;
    ParamCnt := OwnerParamCnt * FBatchIters;
    BatchIters := FBatchIters;
  end;

  rgStatus := Marshal.AllocHGlobal(ParamCnt * SizeOf(DBBINDSTATUS));
  IntPtr(FParamsAccessorData.Accessor) := nil;

  FCanReadParams := False;

  try
    try
      FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
      FParamsAccessorData.ExecuteParams.pData := nil;
      FParamsAccessorData.ExecuteParams.cParamSets := 1;

      SetLength(FParamsAccessorData.rgBindings, ParamCnt);
      PrepareParamAcc(BindMemorySize);

      FParamsAccessorData.ExecuteParams.pData := Marshal.AllocHGlobal(BindMemorySize);
      FillChar(FParamsAccessorData.ExecuteParams.pData, BindMemorySize, 0);

      for i := 0 to ParamCnt - 1 do begin
        if OwnerCommand = nil then begin
          ParamDesc := TMSParamDesc(FParams[i]);
          ItemIndex := 0;
        end
        else begin
          ParamDesc := TMSParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamCnt].ParamRef);
          ItemIndex := FBatchOffset + i div OwnerParamCnt;
        end;

        SaveParamValue(ParamDesc, FParamsAccessorData.rgBindings[i],
          FParamsAccessorData, {$IFDEF HAVE_COMPRESS}FCompressBlob,{$ENDIF}
          FConnection, ItemIndex);
      end;

      QueryIntf(FICommandText, IID_IAccessor, FParamsAccessorData.Accessor);
      rgBindings := @FParamsAccessorData.rgBindings[0];
      Check(FParamsAccessorData.Accessor.CreateAccessor(
        DBACCESSOR_PARAMETERDATA, ParamCnt, rgBindings, BindMemorySize,
        FParamsAccessorData.ExecuteParams.HACCESSOR, rgStatus));
      FParamsAccessorDataAvaible := True;
    except
      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        if OwnerCommand = nil then
          for i := 0 to ParamCnt - 1 do begin
            if FParamsAccessorData.rgBindings[i].pObject <> nil then
              Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
            if FParams[i].GetDataType = dtTable then begin
              TableObject := TMSSQLTableObject(FParams[i].GetObject);
              if TableObject.RecordSet <> nil then
                TOLEDBRecordSet(TableObject.RecordSet).ReleaseAllInterfaces;
            end;
          end;
        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      FParamsAccessorDataAvaible := False;
      raise;
    end;
  finally
    Marshal.FreeHGlobal(rgStatus);
  end;
end;

procedure TOLEDBCommand.Execute;

  procedure DoExecute;
  var
    AsynchComplete: boolean;

    function OpenOrExec: IUnknown;
    var
      OwnerCommand: TOLEDBCommand;
      ParamsCount: integer;
      pParams: PDBPARAMS;
      RequestInt: TGUID;
      hr: HRESULT;
      i: Integer;

    begin
      if not GetPrepared then
        SetCommandProp;

      if FRequestResultSet or FNonBlocking then
        RequestInt := IID_IUnknown
      else
        if GetUseOutputStream then
          RequestInt := IID_ISequentialStream
        else
          RequestInt := IID_NULL;

      try
        OwnerCommand := TOLEDBCommand(BatchOwner);

        if OwnerCommand = nil then
          ParamsCount := FParams.Count
        else
          ParamsCount := OwnerCommand.FParamsInfo.Count;

        if FRPCCall then
          SetParameterInfo
        else if OwnerCommand = nil then
          for i := 0 to ParamsCount - 1 do
            if FParams[i].GetDataType = dtTable then begin
              SetParameterInfo;
              Break;
            end;

        if ParamsCount = 0 then
          hr := ICommand(FICommandText).Execute(nil, RequestInt, nil, FRowsAffected, Result)
        else begin
          CreateAndFillParamAccs;
          pParams := @FParamsAccessorData.ExecuteParams;
          hr := ICommand(FICommandText).Execute(nil, RequestInt, pParams, FRowsAffected, Result);
        end;

        if FWaitForBreak then
          FExecuting := False;

        AsynchComplete := True;
        if FNonBlocking and (hr = DB_S_ASYNCHRONOUS) then
          AsynchComplete := False
        else
          Check(hr, Analyze);

      {$IFDEF AUTOTEST}
        Inc(__ServerExecuteCount);
      {$ENDIF}
      except
        on E: Exception do begin
          if (E is EOLEDBError) and (EOLEDBError(E).ErrorCode = DB_E_ABORTLIMITREACHED) then
            Unprepare;
          Result := nil;
          FIUnknown := nil;
          ClearIMultipleResults;
          FISSAsynchStatus := nil;

          raise;
        end;
      end;
    end;

  var
    IUnk: IUnknown;
  begin
    Assert(FIUnknown = nil);
    FLastExecWarning := False;
    if FIMultipleResults = nil then begin
      IUnk := OpenOrExec; // After call IUnk may be [nil, IUnknown (message), IUnknown (cursor)]
      Assert(FRequestResultSet or FNonBlocking or GetUseOutputStream or (IUnk = nil));

      if FRequestResultSet and (IUnk <> nil) then begin
        if FRequestMultipleResults then begin
          QueryIntf(IUnk, IID_IMultipleResults, FIMultipleResults);
          IUnk.QueryInterface(IID_ISSAbort, FIAbort);
        end
        else // This is a server cursor or DbxSda call
          FIUnknown := IUnk;

        IUnk := nil;
      end;

      if not FRequestResultSet and FNonBlocking and not AsynchComplete then begin
        Assert(IUnk <> nil);
        QueryIntf(IUnk, IID_ISSAsynchStatus, FISSAsynchStatus);
      end;
    end;

    if FRequestResultSet and (FIMultipleResults <> nil) then
      if FIUnknownNext = nil then
        GetNextResult(FIUnknown, FRowsAffected)
      else begin
        FIUnknown := FIUnknownNext;
        FIUnknownNext := nil;
        FRowsAffected := FRowsAffectedNext;
      end;
  end;
begin
  if (FCursorState <> csInactive) and (FCursorState <> csPrepared) then
    Exit;

  FExecuting := True;
  FWaitForBreak := False;

  SetCursorState(csExecuting);
  QueryInterfaces(False); // If QueryInterfaces already called then do nothing
  try
    DoExecute;
  except
    on E: Exception do begin
      if (not FNonBlocking) or FRequestResultSet or (FISSAsynchStatus = nil) then
        EndExecute(E);

      ClearIMultipleResults;
      FIUnknownNext := nil;
      FIUnknown := nil;
      RequestParamsIfPossible;
      raise;
    end;
  end;

  if FNonBlocking and (FISSAsynchStatus <> nil) then
    CreateExecutor;
  if (not FNonBlocking) or FRequestResultSet or (FISSAsynchStatus = nil) then
    EndExecute(nil);
end;

procedure TOLEDBCommand.Close;
begin
  ClearIMultipleResults;
  FIUnknownNext := nil;
  FIUnknown := nil;
  RequestParamsIfPossible;
end;

procedure TOLEDBCommand.DoExecuteException(Sender: TObject; E: Exception; var Fail: boolean); // MainThread context
begin
  if E is EOLEDBError then
    FConnection.DoError(EOLEDBError(E), Fail);
end;

procedure TOLEDBCommand.WaitAsynchCompletion; // FExecuter.FThread context
var
  hr: HRESULT;
  Completed: boolean;
begin
  if FISSAsynchStatus <> nil then begin
    try
      Completed := False;
      while not FExecutor.Terminated do begin
        hr := FISSAsynchStatus.WaitForAsynchCompletion(100);
        case hr of
          S_OK:
            Completed := True;
          DB_S_ASYNCHRONOUS:;
          else
            GetConnector.Check(hr, nil);
        end;
        if Completed then
          break;
      end;
      if not Completed then
        FISSAsynchStatus.Abort(DB_NULL_HCHAPTER, DBASYNCHOP_OPEN);
    finally
      FISSAsynchStatus := nil;
    end;
  end;
end;

procedure TOLEDBCommand.EndExecute(E: Exception);
begin
  try
    try
      RequestParamsIfPossible;
    finally
      ReleaseInterfaces;
    end;

    if FIUnknown = nil then
      FCursorState := csInactive
    else
      FCursorState := csExecuted;
  finally
    inherited;
  end;
end;

procedure TOLEDBCommand.ConvertStreamToBlob(pValue: IntPtr; Size: integer; Blob: TBlob; IsUnicodeField: Boolean; OmitXMLPreamble: boolean = False);
var
  Stream: ISequentialStream;
{$IFDEF VER22P}
  BytesReadedFromStream: FixedUInt;
{$ELSE}
  BytesReadedFromStream: LongInt;
{$ENDIF}
  XML: WideChar;

  Piece: PPieceHeader;
  PieceSize: integer;
  SizeAvailable: boolean;

  SourceEncoding: Encoding;
  DestEncoding: Encoding;
  Buf: TBytes;
begin
{$IFNDEF VER9P}
  Buf := nil;
{$ENDIF}
  IntPtr(Stream) := Marshal.ReadIntPtr(pValue);
  if (IntPtr(Stream) = nil) or (Size = 0) then begin
    Blob.Write(0, 0, nil); // Set IsNull to False
    Exit;
  end;

  SizeAvailable := Size <> -1;

  if OmitXMLPreamble then
    // Skip FF FE bytes, CR 16149
    Stream.Read(@XML, SizeOf(WideChar), @BytesReadedFromStream);

  repeat
    if TmpPiece <> nil then
      PieceSize := TmpPiece.Size
    else begin
      if (Size >= DefaultPieceSize) or not SizeAvailable then
        PieceSize := DefaultPieceSize
      else
        PieceSize := Size;
      Blob.AllocPiece(TmpPiece, PieceSize);
    end;

    try
      Stream.Read(PtrOffset(TmpPiece, SizeOf(TPieceHeader)), PieceSize, @BytesReadedFromStream);
      TmpPiece.Used := BytesReadedFromStream;

      if SizeAvailable then
        Dec(Size, integer(BytesReadedFromStream));
    finally
      if integer(BytesReadedFromStream) <> PieceSize then begin
        if BytesReadedFromStream > 0 then begin
          Blob.AllocPiece(Piece, BytesReadedFromStream);
          Move(PtrOffset(TmpPiece, SizeOf(TPieceHeader))^, PtrOffset(Piece, SizeOf(TPieceHeader))^, BytesReadedFromStream);
          Piece.Used := BytesReadedFromStream;
          Blob.AppendPiece(Piece);
        end;
      end
      else begin
        Blob.AppendPiece(TmpPiece);
        TmpPiece := nil;
      end;
    end;
  until ((Size = 0) and SizeAvailable) or ((PieceSize <> integer(BytesReadedFromStream)) and not SizeAvailable);

  if Blob.IsNull then
    Blob.Write(0, 0, nil); // Set IsNull to False

  if Blob.IsUnicode then
    DestEncoding := Encoding.Unicode
  else
    DestEncoding := Encoding.Default;

  if IsUnicodeField then
    SourceEncoding := Encoding.Unicode
  else
    SourceEncoding := Encoding.Default;

  if SourceEncoding <> DestEncoding then begin
    Buf := Encoding.Convert(SourceEncoding, DestEncoding, Blob.AsBytes, 0, Blob.Size);
    Blob.Size := Length(Buf);
    if Length(Buf) > 0 then
      Blob.Write(0, Length(Buf), @Buf[0])
    else
      Blob.Write(0, 0, nil);
  end;

{$IFDEF HAVE_COMPRESS}
  if (Blob is TCompressedBlob) and (FCompressBlob in [cbClient, cbClientServer]) then
    TCompressedBlob(Blob).Compressed := True;
{$ENDIF}
end;

procedure TOLEDBCommand.RequestAndFreeParamAccs;

  procedure ProcessParam(ParamDesc: TParamDesc; var pBind: TDBBinding);
  var
    Status: DWORD;
    pLength, pValue, pStatus: IntPtr;
    l: UINT;
    c: Currency;
    i64: Int64;
  {$IFDEF VER6}
    ui64: UInt64;
  {$ENDIF}
    IsNull: boolean;

    DBTimeStamp: TDBTimeStamp;
  {$IFNDEF FPC}
    SQLTimeStamp: TSQLTimeStamp;
  {$ENDIF}
    dt: TDateTime;
    Blob: TSharedObject;
  begin
    if (ParamDesc.GetParamType in [pdUnknown, pdInputOutput, pdOutput, pdResult])
      and not ((ParamDesc.GetParamType = pdUnknown) and (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo, dtXML, dtTable])) then begin
      pStatus :=  PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obStatus);
      Status := DWORD(Marshal.ReadInt32(pStatus));
      IsNull := (Status = DBSTATUS_S_ISNULL) or ((Status in [DBSTATUS_E_BADACCESSOR, DBSTATUS_E_UNAVAILABLE]) and (ParamDesc.GetNull or (ParamDesc.GetParamType <> pdInputOutput)));
      if not (IsOutputLOB(ParamDesc) and GetConnector.IsOutputLOBSupported) then begin
        if IsNull then
          ParamDesc.Value := Null
        else
          ParamDesc.SetNull(False);
      end
      else begin
        Blob := ParamDesc.GetObject;
        try
          ParamDesc.SetNull(IsNull);
        finally
          ParamDesc.SetObject(Blob);
        end;
      end;

      if not (Status in [DBSTATUS_S_OK, DBSTATUS_S_ISNULL, DBSTATUS_S_TRUNCATED]) then
        Exit;

      if ParamDesc.GetNull and not (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo]) then
        Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL))
      else begin
        pValue := PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obValue);
        case ParamDesc.GetDataType of
          dtUnknown:
            ParamDesc.SetValue(GetOleVariant(pValue));
          dtString:
            ParamDesc.SetValue(Marshal.PtrToStringAnsi(pValue));
          dtWideString:
            ParamDesc.SetValue(Marshal.PtrToStringUni(pValue));
          dtBytes, dtVarBytes: begin
            pLength := PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obLength);
            l := Marshal.ReadInt32(pLength);
            if l > 0 then begin
              ParamDesc.SetValue(VarArrayCreate([0, l - 1], varByte));
              CopyBuffer(pValue, TVarData(ParamDesc.Value).VArray.Data, l);
            end;
          end;
          dtInt8, dtUInt8:
            ParamDesc.SetValue(Byte(Marshal.ReadByte(pValue)));
          dtInt16:
            ParamDesc.SetValue(SmallInt(Marshal.ReadInt16(pValue)));
          dtWord:
            ParamDesc.SetValue(Word(Marshal.ReadInt16(pValue)));
          dtInt32, dtUInt32:
            ParamDesc.SetValue(Integer(Marshal.ReadInt32(pValue)));
          dtSingle:
            ParamDesc.SetValue(CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(pValue)));
          dtFloat:
            ParamDesc.SetValue(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue)));
          dtCurrency:
            ParamDesc.SetValue(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue)));
          dtDate, dtTime:
            ParamDesc.SetValue(TDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue))));
          dtDateTime: begin
            DBTimeStamp := PDBTimeStamp(pValue)^;
            dt := EncodeDateTime(DBTimeStamp.year, DBTimeStamp.month, DBTimeStamp.day, DBTimeStamp.hour, DBTimeStamp.minute, DBTimeStamp.second, DBTimeStamp.fraction div 1000000{Billionths of a second to milliseconds});
            ParamDesc.SetValue(dt);
          end;
        {$IFNDEF FPC}
          dtSQLTimeStamp: begin
            SQLTimeStamp := PSQLTimeStamp(pValue)^;
            SQLTimeStamp.Fractions := SQLTimeStamp.Fractions div 1000000; // billionths to milliseconds of a second
            ParamDesc.SetValue(VarSQLTimeStampCreate(SQLTimeStamp));
          end;
        {$ENDIF}
          dtBoolean:
            ParamDesc.SetValue(Boolean(WordBool(Marshal.ReadInt16(pValue))));
          dtInt64:
            ParamDesc.SetValue(Marshal.ReadInt64(pValue));
          dtUInt64: begin
          {$IFDEF FPC}
            ParamDesc.SetValue(UInt64(Marshal.ReadInt64(pValue)));
          {$ELSE}{$IFDEF VER7P}
            ParamDesc.SetValue(UInt64(Marshal.ReadInt64(pValue)));
          {$ELSE}
            // Delphi 6 conversion bug
            i64 := Marshal.ReadInt64(pValue);
            ui64 := UInt64(i64);
            ParamDesc.SetValue(ui64);
          {$ENDIF}{$ENDIF}
          end;
          dtGuid:
            ParamDesc.SetValue(GUIDToString(PGuid(pValue)^));
          dtVariant:
            ParamDesc.SetValue(GetOleVariant(pValue));
          dtBCD: begin
            i64 := Marshal.ReadInt64(pValue);
            c := PCurrency(@i64)^;
            ParamDesc.SetValue(c);
          end;
          dtFmtBCD, dtExtended: begin
            if FConnection.Provider <> prCompact then
              ParamDesc.SetValue(Marshal.PtrToStringAnsi(pValue))
            else
              ParamDesc.SetValue(VarFMTBcdCreate(DBNumericToBcd(PDBNumeric(pValue)^)));
          end;
          dtXML:
            ParamDesc.SetValue(GetOleVariant(pValue));
          dtBlob, dtMemo, dtWideMemo: begin
            if ParamDesc.GetNull then begin
              Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL));
              pLength := PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obLength);
              if (Marshal.ReadIntPtr(pValue) = nil) or (Marshal.ReadInt32(pLength) <= 0) then
                Exit;
            end;

            Blob := ParamDesc.GetObject;
            Assert(Blob <> nil);
            TBlob(Blob).Clear;
            ConvertStreamToBlob(pValue, -1, TBlob(Blob), (ParamDesc.GetSubDataType and sdtWide) <> 0);
          end;
        else
          Assert(False, Format('ParamDesc - %s, Unknown DataType = %d', [ParamDesc.GetName, ParamDesc.GetDataType]));
        end;
      end;
    end;
  end;

var
  TableObject: TMSSQLTableObject;
  OwnerCommand: TOLEDBCommand;
  ParamCnt: integer;
  i: integer;
begin
  if not FParamsAccessorDataAvaible then
    Exit;

  Assert(FIUnknown = nil, 'Before RequestAndFreeParamAccs interface FIUnknown must be released');
  // Assert(FIMultipleResults = nil, 'Before RequestAndFreeParamAccs interface FIMultipleResults must be released');

  OwnerCommand := TOLEDBCommand(BatchOwner);
  if OwnerCommand = nil then
    ParamCnt := FParams.Count
  else
    ParamCnt := OwnerCommand.ParamsInfo.Count * FBatchIters;

  if ParamCnt > 0 then begin
    try
      if FConnection <> nil then begin
        if FParamsAccessorData.ExecuteParams.HACCESSOR <> 0 then
          Check(FParamsAccessorData.Accessor.ReleaseAccessor(FParamsAccessorData.ExecuteParams.HACCESSOR, nil));
        FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
        FParamsAccessorData.Accessor := nil;

        if OwnerCommand = nil then
          for i := 0 to ParamCnt - 1 do
            ProcessParam(FParams[i], FParamsAccessorData.rgBindings[i]);
      end;
    finally
      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        if OwnerCommand = nil then
          for i := 0 to ParamCnt - 1 do begin
            if FParamsAccessorData.rgBindings[i].pObject <> nil then
              Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
            if FParams[i].GetDataType = dtTable then begin
              TableObject := TMSSQLTableObject(FParams[i].GetObject);
              if TableObject.RecordSet <> nil then
                TOLEDBRecordSet(TableObject.RecordSet).ReleaseAllInterfaces;
            end;
          end;

        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      FParamsAccessorDataAvaible := False;
    end;

    FCanReadParams := True;
    if Assigned(FReadParams) and (FConnection <> nil) then
      FReadParams;
  end;
end;

procedure TOLEDBCommand.RequestParamsIfPossible; // Call RequestAndFreeParamAccs if interfaces is cleared
begin
  if FIUnknown = nil then begin
    if (FIMultipleResults <> nil) and not FNextResultRequested then begin
      try
        GetNextResult(FIUnknownNext, FRowsAffectedNext);
      except
        on E: EOLEDBError do
          if E.FOLEDBErrorCode <> DB_E_CATASTROPHICFAILURE then
            raise;
        else
          raise;
      end;

      FNextResultRequested := FIMultipleResults <> nil;
    end;

    if FIMultipleResults = nil then
      RequestAndFreeParamAccs;
  end;
end;

function TOLEDBCommand.Analyze(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
const
  ParamHeader = 'Parameter[%d] %s - %s (Status = %Xh).';
var
  i: integer;
  ParamStatus: DWORD;
  ParamName: string;
  Msg: WideString;
begin
  Result := '';
  if (Status and $80000000) <> 0 then begin // Severity bit (see OLEDBC.pas line 5489) is not 0
    if Status = DB_E_OBJECTOPEN then
      Result := WideString(SObjectOpen)
    else
      if FParamsAccessorData.ExecuteParams.pData <> nil then begin
        Msg := '';
        with FParamsAccessorData do
          for i := 0 to FParams.Count - 1 do begin
            ParamName := FParams[i].GetName;
            if ParamName = '' then
              ParamName := IntToStr(i)
            else
              ParamName := ':' + ParamName;
            ParamStatus := DWORD(Marshal.ReadInt32(ExecuteParams.pData, rgBindings[i].obStatus));
            case ParamStatus of
              DBSTATUS_S_OK, DBSTATUS_S_ISNULL, DBSTATUS_S_DEFAULT:;
              DBSTATUS_E_BADACCESSOR:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SInvalidParamType, ParamStatus]);
              DBSTATUS_E_CANTCONVERTVALUE:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SInvalidValue, ParamStatus]);
              DBSTATUS_S_TRUNCATED:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SDataTruncated, ParamStatus]);
              DBSTATUS_E_SIGNMISMATCH:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SSignMismatch, ParamStatus]);
              DBSTATUS_E_DATAOVERFLOW:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SDataOverflow, ParamStatus]);
              DBSTATUS_E_CANTCREATE:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SOutOfMemory, ParamStatus]);
              DBSTATUS_E_UNAVAILABLE:
                {AddInfoToErr(Msg, ParamHeader, [i, ParamName, SUnavaible, ParamStatus])};
              DBSTATUS_E_INTEGRITYVIOLATION:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SIntegrityViolation, ParamStatus]);
              DBSTATUS_E_SCHEMAVIOLATION:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SShemaViolation, ParamStatus]);
              DBSTATUS_E_BADSTATUS:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SBadStatus, ParamStatus]);
            else
              AddInfoToErr(Msg, ParamHeader, [i, ParamName, SUnknownStatus, ParamStatus]);
            end;
          end; // for
        Result := Msg;
      end; // if Status = DB_E_OBJECTOPEN
  end
  else // if (Status and $80000000) <> 0 then begin
    if Status = DB_S_ERRORSOCCURRED then
      FLastExecWarning := True;
end;

procedure TOLEDBCommand.GetNextResult(out ResultSet: IUnknown; out RowsAffected: NativeInt);
var
  hr, lasthr: HRESULT;
  OldRowsAffected: NativeInt;
  CurrentErr, PrevErr: EOLEDBError;
begin
  CurrentErr := nil;
  try
    lasthr := 0;
    repeat
      OldRowsAffected := FRowsAffected;
      hr := FIMultipleResults.GetResult(nil, 0, IID_IUnknown, RowsAffected, ResultSet);

      try
        if (hr = DB_E_OBJECTOPEN) and (ResultSet = nil) and (lasthr <> DB_E_OBJECTOPEN) then
          RequestAndFreeParamAccs
        else
          if hr <> DB_S_NORESULT then
            Check(hr, Analyze);
        lasthr := hr;
        GetConnector.OLEDBError(0, Component); // Check Info messages

        if ResultSet <> nil then begin
          OldRowsAffected := RowsAffected;
          Break;
        end;

        if FWaitForBreak then begin
          FWaitForBreak := False;
          GetConnector.OLEDBError(DB_E_CANCELED, Component);
        end;

      except
        on E: EOLEDBError do begin
          PrevErr := CurrentErr;
          CurrentErr := E;
          AddInfoToErr(CurrentErr, PrevErr);
          PrevErr.Free;
          if (FIMultipleResults <> nil) and (ResultSet = nil) and (hr <> DB_S_NORESULT) and (hr <> DB_E_CATASTROPHICFAILURE) then
            hr := S_OK // continue
          else begin
            PrevErr := CurrentErr;
            CurrentErr := nil;
            raise PrevErr;
          end;
        end
        else
          raise;
      end;

    until (hr <> S_OK) and (hr <> DB_E_OBJECTOPEN);

    RowsAffected := OldRowsAffected;
    if CurrentErr <> nil then begin
      PrevErr := CurrentErr;
      CurrentErr := nil;
      raise PrevErr;
    end;

  finally
    CurrentErr.Free;
    if ResultSet = nil then
      ClearIMultipleResults; // Rowsets not also provided
  end;
end;

procedure TOLEDBCommand.CancelCommand;
begin
  if FICommandText <> nil then begin
    Check(ICommand(FICommandText).Cancel);
    while FExecuting do begin
      sleep(50);
      if FExecuting then
        Check(ICommand(FICommandText).Cancel);
    end;
  end;
end;

procedure TOLEDBCommand.DescribeSPParams(MasterDatabase: boolean; out OriginalDatabase: string);

  procedure FillParams(ParamsMetaInfo: TOLEDBRecordSet);
  var
    TypeFld, HasDefaultFld, DataTypeFld, OctetLengthFld, CharMaxLenFld: TFieldDesc; // offsets
    Param: TMSParamDesc;
    ParamType: TParamDirection;
    TypeName, ParamName, HasDefault: string;
    DataType: word;
    IsLong: boolean;
    RecBuf: IntPtr;
    dt: integer;
    OctetLength: integer;
  begin
    FParams.Clear;
    TypeFld := ParamsMetaInfo.FieldByName('PARAMETER_TYPE'); // DataSize = 4
    HasDefaultFld := ParamsMetaInfo.FieldByName('PARAMETER_HASDEFAULT'); // DataSize = 2
    DataTypeFld := ParamsMetaInfo.FieldByName('DATA_TYPE'); // DataSize = 4
    OctetLengthFld := ParamsMetaInfo.FieldByName('CHARACTER_OCTET_LENGTH'); // DataSize = 4
    CharMaxLenFld := ParamsMetaInfo.FieldByName('CHARACTER_MAXIMUM_LENGTH'); // DataSize = 4

    ParamsMetaInfo.AllocRecBuf(RecBuf);
    try
      while True do begin
        ParamsMetaInfo.GetNextRecord(RecBuf);
        if ParamsMetaInfo.Eof then
          Break;

        OctetLength := Marshal.ReadInt32(RecBuf, OctetLengthFld.DataOffset);
        IsLong := (OctetLength >= MaxInt - 1) or (OctetLength = 0);
        dt := Marshal.ReadInt32(RecBuf, DataTypeFld.DataOffset);
        TypeName := ParamsMetaInfo.GetFieldStrValue(RecBuf, ParamsMetaInfo.FieldByName('TYPE_NAME'));

        if ConvertOLEDBTypeToInternalFormat(dt, IsLong, TypeName <> 'varbinary',
          CalcEnableBCD, CalcEnableFMTBCD, False, {$IFDEF LITE}0, 0,{$ENDIF} 0, 0,
          True, FConnection.Options.WideMemos, True, DataType, FConnection.Provider)
        then begin
          ParamType := ConvertOLEDBParamTypeToCR(Marshal.ReadInt32(RecBuf, TypeFld.DataOffset));
          Param := TMSParamDesc.Create;
          FParams.Add(Param);
          Param.SetParamType(ParamType);
          Param.SetDataType(DataType);
          if DataType = dtTable then
            Param.TableTypeName := TypeName;

          ParamName := GetParamNameWODog(ParamsMetaInfo.GetFieldStrValue(RecBuf, ParamsMetaInfo.FieldByName('PARAMETER_NAME')));
          Param.SetName(ParamName);

          if Marshal.ReadInt16(RecBuf, HasDefaultFld.DataOffset) <> 0 then begin
            HasDefault := ParamsMetaInfo.GetFieldStrValue(RecBuf, ParamsMetaInfo.FieldByName('PARAMETER_HASDEFAULT'));
            Param.SetValue(HasDefault);
          end;

          if IsOutputLOB(Param) and not GetConnector.IsOutputLOBSupported then
            Param.SetParamType(pdInput);

          Param.SetSize(Marshal.ReadInt32(RecBuf, CharMaxLenFld.DataOffset));
        end
        else
          DatabaseErrorFmt(SBadFieldType, [TypeName, dt]);
      end;
    finally
      if RecBuf <> nil then
        ParamsMetaInfo.FreeRecBuf(RecBuf);
    end;
  end;

  procedure ParseFullName(out Database: string; out Owner: string; out ProcName: string);
  begin
    MSSQLInfo.SplitObjectName(FStoredProcName, OriginalDatabase, Owner, ProcName);
    Owner := SQLInfo.NormalizeName(Owner, False, True);
    ProcName := SQLInfo.NormalizeName(ProcName, False, True);
    if OriginalDatabase <> '' then
      Database := SQLInfo.NormalizeName(OriginalDatabase, False, True)
    else
      if not MasterDatabase then
        Database := FConnection.Database
      else
        Database := DefaultSDACDatabase;
  end;

var
  Database, Owner, ProcName, OverloadNum: string;
  ParamsMetaInfo: TOLEDBRecordSet;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  ParseFullName(Database, Owner, ProcName);

  ParamsMetaInfo := TOLEDBRecordSet.Create;
  try
    ParamsMetaInfo.SetConnection(FConnection);
    ParamsMetaInfo.SetProp(prFetchAll, True);

    if FConnection.IsSQLAzureEdition then begin
      ParseProcName(ProcName, OverloadNum);
      if OverloadNum = '' then
        OverloadNum := '1';

      ParamsMetaInfo.SetSQL(Format('exec sys.sp_procedure_params_managed N''%s'', %s, N''%s'', NULL', [ProcName, OverloadNum, Owner]));
    end
    else begin
      SetLength(rgRestrictions, 3);
      if (Length(ProcName) > 1) and (ProcName[1] = '#') then // for temporary stored procedures
        rgRestrictions[0] := 'tempdb'
      else
        rgRestrictions[0] := DataBase;
      rgRestrictions[1] := Owner;
      rgRestrictions[2] := ProcName;

      Rowset := GetConnector.GetSchemaRowset(DBSCHEMA_PROCEDURE_PARAMETERS, rgRestrictions);
      ParamsMetaInfo.SetIRowset(Rowset);
    end;

    ParamsMetaInfo.Open;
    FillParams(ParamsMetaInfo);
  finally
    ParamsMetaInfo.Close;
    ParamsMetaInfo.UnPrepare;
    ParamsMetaInfo.Free;
  end;
end;

function TOLEDBCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
var
  i: integer;
  BracketAdded: boolean;
  Database, ProcName, OverloadNum, s: string;
  ParamName: string;
begin
  if FConnection = nil then
    DatabaseError(SConnectionNotDefined);

  FStoredProcName := Name;
  FConnection.Connect('');

  if NeedDescribe then begin
    DescribeSPParams(False, Database);
    if (FParams.Count = 0) and (Database = '') then begin
      s := LowerCase(Copy(Name, 1, 3));
      if (s = 'sp_') or (s = 'xp_') then
        DescribeSPParams(True, Database);
    end;
  end;

  ProcName := Name;
  ParseProcName(ProcName, OverloadNum);
  FSQL := 'CALL ' + SQLInfo.NormalizeName(ProcName, Char('"'), Char('"'){May be bug in MS SQL Server});
  if OverloadNum <> '' then
    FSQL := FSQL + ';' + OverloadNum;

  Result := FSQL;

  BracketAdded := False;
  for i := 0 to FParams.Count - 1 do begin
    ParamName := FParams[i].GetName;
    if ParamName = '' then
      ParamName := '?'
    else
      ParamName := ':' + ParamName;
    if FParams[i].GetParamType = pdResult then begin
      FSQL := '? = ' + FSQL;
      Result := ParamName + ' = ' + Result
    end
    else begin
      if BracketAdded then begin
        FSQL := FSQL + ', ?';
        Result := Result + ', ' + ParamName
      end
      else begin
        BracketAdded := True;
        FSQL := FSQL + ' (?';
        Result := Result + ' (' + ParamName
      end;
    end;
  end;
  if BracketAdded then begin
    FSQL := FSQL + ')';
    Result := Result + ')';
  end;
  FSQL := '{' + FSQL + '}';
  Result := '{' + Result + '}';
  FUserSQL := Result;
end;

function TOLEDBCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prCanReadParams:
      Value := FCanReadParams and (FIUnknown = nil) and (FIMultipleResults = nil);
    prDelayedSubsciption:
      Value := FDelayedSubsciption;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOLEDBCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prNotification:
      FNotification := Value;
    prNotificationMessage:
      FNotificationMessage := Value;
    prNotificationService:
      FNotificationService := Value;
    prNotificationTimeout:
      FNotificationTimeout := Value;
    prOutputStream:
    {$IFDEF CPU64}
      SetOutputStream(TStream(Int64(Value)));
    {$ELSE}
      SetOutputStream(TStream(Integer(Value)));
    {$ENDIF}
    prOutputEncoding:
      FOutputEncoding := TMSOutputEncoding(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOLEDBRecorSet }

constructor TOLEDBRecordSet.Create;
begin
  inherited;

  FWideStrings := True; // Native fields mapping
  FWideMemos := True;
  FUniqueRecords := {$IFNDEF LITE}True{$ELSE}False{$ENDIF};
  FHideSystemUniqueFields := True;
  FCursorType := ctDefaultResultSet;

  FRequestSQLObjects := {$IFNDEF LITE}True{$ELSE}False{$ENDIF};
  FNativeRowset := True;
  FCursorTypeChanged := nil;
end;

destructor TOLEDBRecordSet.Destroy;
begin
  inherited;
end;

function TOLEDBRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TMSFieldDesc;
end;

class function TOLEDBRecordSet.ExceptionIsCanceled(E: Exception): boolean;
begin
  Result := (E is EOLEDBError) and (EOLEDBError(E).ErrorCode = DB_E_CANCELED);
end;

function TOLEDBRecordSet.IsWideField(Field: TFieldDesc): Boolean;
begin
  Result := (Field.SubDataType and sdtWide) <> 0;
end;

function TOLEDBRecordSet.GetWideMemos: boolean;
begin
  if (FCommand <> nil) and (FCommand.GetConnection <> nil) then
    Result := TMSSQLConnection(FCommand.GetConnection).Options.WideMemos
  else
    Result := FWideMemos;
end;

function TOLEDBRecordSet.CursorTypeForFetching: TMSCursorType;
begin
  if (FCursorType = ctBaseTable) and not FReadOnly and FCursorUpdate then
    Result := ctDynamic
  else
    Result := FCursorType;
end;

function TOLEDBRecordSet.NeedGetRecordAfterGotoBookmark: boolean;
begin
  Result := IsServerCursor;
end;

function TOLEDBRecordSet.IsServerCursor: boolean;
begin
  Result := CursorTypeForFetching in ServerCursorTypes;
end;

function TOLEDBRecordSet.IsStaticCursor: boolean;
begin
  Result := CursorTypeForFetching in [ctStatic, ctKeyset];
end;

function TOLEDBRecordSet.IsDynamicCursor: boolean;
begin
  Result := CursorTypeForFetching = ctDynamic;
end;

function TOLEDBRecordSet.GetIndicatorItemSize: Integer;
begin
  Result := OLE_DB_INDICATOR_SIZE;
end;

function TOLEDBRecordSet.GetStatus(Field: TFieldDesc; RecBuf: IntPtr): DWORD;
begin
  if Field.FieldDescKind <> fdkCalculated then
    Result := DWORD(Marshal.ReadInt32(RecBuf, FDataSize + (Field.FieldNo - 1) * OLE_DB_INDICATOR_SIZE))
  else
    Result := DWORD(Marshal.ReadInt32(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * OLE_DB_INDICATOR_SIZE));
end;

procedure TOLEDBRecordSet.SetStatus(Field: TFieldDesc; RecBuf: IntPtr; Value: DWORD);
begin
  if Field.FieldDescKind <> fdkCalculated then
    Marshal.WriteInt32(RecBuf, FDataSize + (Field.FieldNo - 1) * OLE_DB_INDICATOR_SIZE, Integer(Value))
  else
    Marshal.WriteInt32(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * OLE_DB_INDICATOR_SIZE, Integer(Value));
end;

function TOLEDBRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
{$IFNDEF LITE}
  if not FInFetching then
    CheckFetched(RecBuf, Field);
{$ENDIF}
  Result := GetStatus(Field, RecBuf) = DBSTATUS_S_ISNULL;
  if Result then
    Result := GetNullByBlob(Field, RecBuf);
end;

procedure TOLEDBRecordSet.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  Blob: TBlob;
begin
  if Value then begin
    SetStatus(Field, RecBuf, DBSTATUS_S_ISNULL);
    if Field.IsBlob then begin // clear Blob value
      Blob := TBlob(InternalGetObject(PtrOffset(RecBuf, Field.DataOffset)));
      if Blob <> nil then
        Blob.Clear;
    end;
  end
  else
    SetStatus(Field, RecBuf, DBSTATUS_S_OK);

{$IFNDEF LITE}
  SetEncrypted(Field, RecBuf, not Value);
{$ENDIF}
end;

procedure TOLEDBRecordSet.GetField(Field: TFieldDesc; RecBuf: IntPtr; Dest: IntPtr; out DestLen: Word; NeedConvert: boolean; out IsBlank: boolean);
begin
  CheckBCDOverflow(Field, RecBuf);

  inherited;
end;

procedure TOLEDBRecordSet.GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False);
begin
  CheckBCDOverflow(Field, RecBuf);

  inherited;
end;

{ Open/Close }

procedure TOLEDBRecordSet.InternalPrepare;
var
  ColumnsInfo: IColumnsInfo;
  cColumns: NativeUInt;
  prgInfo: PDBCOLUMNINFO;
  pStringsBuffer: IntPtr;
begin
  QueryCommandInterfaces(True);
  try
    SetCommandProp;
    inherited;
  finally
    ReleaseCommandInterfaces; // FCommand.QueryIntCnt counter is increased in inherited
  end;

  try
    // Detect CommandType
    if FNativeRowset and not FCommand.FRPCCall then begin
      // If statement is wrong in some cases exception may be occured now
      QueryIntf(FCommand.FICommandPrepare, IID_IColumnsInfo, ColumnsInfo);
      Assert(ColumnsInfo <> nil);

      pStringsBuffer := nil;
      try
        // If statement is wrong in some cases exception may be occured now
        Check(ColumnsInfo.GetColumnInfo(cColumns, PDBCOLUMNINFO(prgInfo), pStringsBuffer));

        if cColumns > 0 then
          FCommand.CommandType := ctCursor
        else
          FCommand.CommandType := ctStatement;
      finally
        FreeCoMem(prgInfo);
        FreeCoMem(pStringsBuffer);
      end;
    end;
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TOLEDBRecordSet.InternalUnPrepare;
begin
  try
    inherited;
  finally
    ReleaseAllInterfaces;
    FCommand.SetCursorState(csInactive);
    FCommand.CommandType := ctUnknown;
  end;
end;

procedure TOLEDBRecordSet.ClearHRow;
begin
  if FHRowAccessible then begin
    Assert(FIRowset <> nil, 'TOLEDBRecordSet.ClearHRow - FIRowset must be setted');
    Check(FIRowset.ReleaseRows(FRowsObtained, FPRowArray, nil, nil, nil));
    FHRowAccessible := False;
  end;
end;

function TOLEDBRecordSet.CanDisconnect: boolean;
begin
  Result := inherited CanDisconnect
    and (FCommand.FIUnknown = nil)
    and (FIRowset = nil)
    and (FCommand.FIUnknownNext = nil)
    and (FCommand.FIMultipleResults = nil);
end;

function TOLEDBRecordSet.RowsReturn: boolean;
begin
  if FCommand.CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FIUnknown <> nil) or (FIRowset <> nil);
end;

procedure TOLEDBRecordSet.RowsetUpdateCommit;
const
  RowHeader = '%s (Status = %Xh).';
var
  pRowStatus: PDBRowStatus;
  RowStatus: DBRowStatus;
  Msg: WideString;
  RowsetUpdate: IRowsetUpdate;
begin
  if not Supports(FIRowsetUpdate, IID_IRowsetUpdate, RowsetUpdate) then
    Exit;

  try
    Check(IRowsetUpdate(FIRowsetUpdate).Update(DB_NULL_HCHAPTER, 1, FPRowArray, nil, nil, pRowStatus));
  except
    on E: Exception do begin
      if pRowStatus <> nil then begin
        Msg := '';
        RowStatus := DBRowStatus(Marshal.ReadInt32(pRowStatus));
        case RowStatus of
          DBROWSTATUS_S_OK:;
          DBROWSTATUS_S_MULTIPLECHANGES:
            AddInfoToErr(Msg, RowHeader, [SRowMultipleChanges, RowStatus]);
          DBROWSTATUS_S_PENDINGCHANGES:
            AddInfoToErr(Msg, RowHeader, [SRowPendingChanges, RowStatus]);
          DBROWSTATUS_E_CANCELED:
            AddInfoToErr(Msg, RowHeader, [SRowCanceled, RowStatus]);
          // DBROWSTATUS_E_CANTRELEASE = $00000006;
          DBROWSTATUS_E_CONCURRENCYVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowConcurrencyViolation, RowStatus]);
          DBROWSTATUS_E_DELETED:
            AddInfoToErr(Msg, RowHeader, [SRowDeleted, RowStatus]);
          // DBROWSTATUS_E_PENDINGINSERT = $00000009;
          // DBROWSTATUS_E_NEWLYINSERTED = $0000000A;
          DBROWSTATUS_E_INTEGRITYVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowIntegrityViolation, RowStatus]);
          DBROWSTATUS_E_INVALID:
            Assert(False);
          // DBROWSTATUS_E_MAXPENDCHANGESEXCEEDED = $0000000D;
          // DBROWSTATUS_E_OBJECTOPEN = $0000000E;
          // DBROWSTATUS_E_OUTOFMEMORY = $0000000F;
          DBROWSTATUS_E_PERMISSIONDENIED:
            AddInfoToErr(Msg, RowHeader, [SRowPermissionDenied, RowStatus]);
          DBROWSTATUS_E_LIMITREACHED:
            AddInfoToErr(Msg, RowHeader, [SRowLimitReached, RowStatus]);
          DBROWSTATUS_E_SCHEMAVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowSchemaViolation, RowStatus]);
          DBROWSTATUS_E_FAIL:
            AddInfoToErr(Msg, RowHeader, [SRowFail, RowStatus]);
        else
          AddInfoToErr(Msg, RowHeader, [SUnknownStatus, RowStatus]);
        end;

        FreeCoMem(pRowStatus);
        AddInfoToErr(E, string(Msg), []);
      end;

      RowsetUpdateRollback;

      raise E;
    end;
  end;
end;

procedure TOLEDBRecordSet.RowsetUpdateRollback;
var
  RowsetUpdate: IRowsetUpdate;
begin
  if not Supports(FIRowsetUpdate, IID_IRowsetUpdate, RowsetUpdate) then
    Exit;

  Check(IRowsetUpdate(FIRowsetUpdate).Undo(DB_NULL_HCHAPTER, 1, FPRowArray, nil, nil, nil));
end;

procedure TOLEDBRecordSet.InternalAppend(RecBuf: IntPtr);
begin
  if FIRowsetUpdate = nil then
    inherited
  else begin
    ClearHRow;
    InternalAppendOrUpdate(RecBuf, True);
  end;

  if FCursorType = ctKeySet then
    Inc(FRecordCount);
end;

procedure TOLEDBRecordSet.InternalDelete;
var
  hr: HResult;
begin
  if FIRowsetUpdate = nil then
    inherited
  else begin
    Assert(FHRowAccessible, 'FHRow must be accessible');
    hr := FIRowsetUpdate.DeleteRows(DB_NULL_HCHAPTER, 1, FPRowArray, nil);
    Check(hr);
    RowsetUpdateCommit;
  end;
end;

procedure TOLEDBRecordSet.InternalUpdate(RecBuf: IntPtr);
begin
  if FIRowsetUpdate = nil then
    inherited
  else begin
    Assert(FHRowAccessible, 'FHRow must be accessible');
    InternalAppendOrUpdate(RecBuf, False);
  end;
end;

procedure TOLEDBRecordSet.InternalAppendOrUpdate(RecBuf: IntPtr; IsAppend: boolean);
var
  StreamList: TList;

  function SetDataToRow(const Row: HROW; OldRecBuf, NewRecBuf: IntPtr; ForceUseAllFields: boolean): boolean;
    procedure PrepareConvertableFields(const AccessorBlock: TAccessorBlock); // Server Cursors. Before send data to OLEDB
    var
      i: integer;
      pValue: IntPtr;
      DataSize: Integer;
      Field: TFieldDesc;
      dt: TDateTime;
      AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
      DBTimeStamp: TDBTimeStamp;
      g: TGUID;
      Bcd: TBcd;
      s: AnsiString;
      DotPos, l: integer;
    begin
      for i := 0 to Length(AccessorBlock.Fields) - 1 do begin
        Field := AccessorBlock.Fields[i];
        if (Field.FieldDescKind = fdkData) and not GetNull(Field, NewRecBuf) then begin
          pValue := PtrOffset(NewRecBuf, Field.DataOffset);
          case Field.DataType of
            dtFixedChar, dtString: begin
              DataSize := Marshal.ReadUInt16(NewRecBuf, Field.Offset);
              Marshal.WriteNativeInt(pValue, Field.Length + 1, DataSize);
            end;
            dtFixedWideChar, dtWideString: begin
              DataSize := Marshal.ReadUInt16(NewRecBuf, Field.Offset) * SizeOf(WideChar);
              Marshal.WriteNativeInt(pValue, (Field.Length + 1) * SizeOf(WideChar), DataSize);
            end;
            dtBytes, dtVarBytes: begin
              DataSize := Marshal.ReadUInt16(NewRecBuf, Field.Offset);
              Marshal.WriteNativeInt(pValue, Field.Length, DataSize);
            end;
            dtDateTime: begin
              dt := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue));
              DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
              DBTimeStamp.year := AYear;
              DBTimeStamp.month := AMonth;
              DBTimeStamp.day := ADay;
              DBTimeStamp.hour := AHour;
              DBTimeStamp.minute := AMinute;
              DBTimeStamp.second := ASecond;
              DBTimeStamp.fraction := AMilliSecond * 1000000; // milliseconds to billionths of a second
              PDBTimeStamp(pValue)^ := DBTimeStamp;
            end;
            dtCurrency:
              if Provider = prCompact then
                PCurrency(pValue)^ := PDouble(pValue)^;
            dtFmtBCD: begin
              Bcd := PBcd(pValue)^;
              // DBNumeric
              s := AnsiString(BcdToStr(Bcd));
              if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
                DotPos := Pos(AnsiString({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator), s);
                if DotPos <> 0 then
                  s[DotPos] := '.';
              end;
              l := Length(s);
              if l > 0 then
                CopyBufferAnsi(s, pValue, l + 1{#0});
            end;
            dtGuid: begin
              g := StringToGUID(string(Marshal.PtrToStringAnsi(pValue)));
              CopyBuffer(@g, pValue, sizeof(g));
            end;
          end;
        end;
      end;
    end;

    procedure PostPlainAccessorBlock(
      const AccessorBlock: TAccessorBlock;
      out NeedToPost: boolean); // Used to skip unchanged BLOB fields
    var
      pValue: IntPtr;
      BlobField: TFieldDesc;
      Blob: TBlob;
      OLEDBStream: TOLEDBStream;
      pUnk: IntPtr;
    begin
      NeedToPost := True;
      PrepareConvertableFields(AccessorBlock);
      if AccessorBlock.BlobField = nil then
        Exit;

      BlobField := AccessorBlock.BlobField;
      if not GetNull(BlobField, NewRecBuf) then begin
      // ConvertBlobToStream
        pValue := PtrOffset(NewRecBuf, BlobField.DataOffset);
        Blob := TBlob(GetGCHandleTarget(PIntPtr(pValue)^));

        NeedToPost := Blob.CanRollback or (FCursorType = TMSCursorType(ctTableType));
        if NeedToPost then begin
          // Create stream
          OLEDBStream := TOLEDBStream.Create(Blob, StreamList);
          pUnk := Marshal.GetIUnknownForObject(OLEDBStream);
          Marshal.AddRef(pUnk);
          Marshal.WriteIntPtr(pValue, pUnk);
          // Set stream size
          Marshal.WriteInt32(NewRecBuf, BlobField.DataOffset + sizeof(IntPtr{ISeqStream}), OLEDBStream.Size);
        end;
      end;
    end;

  var
    FetchBlockOffset: integer;

    procedure PostExternalAccessorBlock(const AccessorBlock: TAccessorBlock);
    var
      i, p: integer;
      Size: Integer;
      Blob: TSharedObject;
      pValue, pc: IntPtr;
      pov: POleVariant;
      Field: TFieldDesc;
    begin
      Assert(AccessorBlock.BlobField = nil);

      for i := 0 to Length(AccessorBlock.Fields) - 1 do begin
        Field := AccessorBlock.Fields[i];
        if IsNeedFetchBlock(Field) then begin
          pValue := PtrOffset(NewRecBuf, Field.DataOffset);
          case Field.DataType of
            dtBlob, dtMemo, dtWideMemo: begin
              Blob := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(pValue)));
              pc := PtrOffset(FFetchBuffer, FetchBlockOffset + OLE_DB_INDICATOR_SIZE);

              p := 0;
              if not GetNull(Field, NewRecBuf) then begin
                Size := TBlob(Blob).Size;
                if Size > MaxNonBlobFieldLen then // see IncFetchBlockOffset
                  Size := MaxNonBlobFieldLen;

                if Size > 0 then
                  p := TBlob(Blob).Read(0, Size, pc);
              end
              else
                Size := 0;

              if Field.DataType = dtBlob then
                Marshal.WriteNativeInt(pc, Field.Length, Size)
              else if  Field.DataType = dtMemo then begin
                Marshal.WriteNativeInt(pc, MaxNonBlobFieldLen + 1, Size);
                Marshal.WriteByte(pc, p, 0{#0});
              end
              else begin
                Marshal.WriteNativeInt(pc, MaxNonBlobFieldLen + SizeOf(WideChar), Size);
                Marshal.WriteUInt16(pc, p, 0{#0})
              end;
            end;
            dtVariant: begin
              Blob := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(pValue)));
              pov := POleVariant(PtrOffset(FFetchBuffer, FetchBlockOffset + OLE_DB_INDICATOR_SIZE));
              Marshal.WriteInt16(pov, varEmpty); // TVarData(pov^).VType := varEmpty;
              SetOleVariant(pov, TVariantObject(Blob).Value);
            end;
          else
            Assert(False);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Field);
        end;
      end;
    end;

    procedure CopyStatusFBlock(const ToFBlock: boolean); // if ToFBlock is True then status is copied from NewRecBuf to FetchBlock. Otherwise - from FetchBlock to NewRecBuf
    var
      i: integer;
      FetchBlockOffset: integer;
      Status: DWORD;
      Field: TFieldDesc;
    begin
      FetchBlockOffset := 0;
      for i := 0 to FFields.Count - 1 do begin
        Field := FFields[i];
        if (Field.FieldDescKind = fdkData) and IsNeedFetchBlock(Field) then begin
          if ToFBlock then begin
            Status := GetStatus(Field, NewRecBuf);
            Marshal.WriteInt32(FFetchBuffer, FetchBlockOffset, Integer(Status));
          end
          else begin
            Status := DWORD(Marshal.ReadInt32(FFetchBuffer, FetchBlockOffset));
            SetStatus(Field, NewRecBuf, Status);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Field);
        end;
      end;
    end;

  var
    AccNum: integer;
    hr: HResult;
    NeedToPost: boolean;
  begin
    Result := False;
    FetchBlockOffset := 0;

    for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do begin
      Assert((Length(FFetchAccessorData.AccessorBlocks[AccNum].Fields) = 1) // CR 4082
        or (FCursorType = TMSCursorType(ctTableType)));
      if (FCursorType = TMSCursorType(ctTableType)) // for TableTypeRecordSet
        or ForceUseAllFields
        or (not IsAppend and FieldModified(FFetchAccessorData.AccessorBlocks[AccNum].Fields[0], OldRecBuf, NewRecBuf))
        or (IsAppend and (not GetNull(FFetchAccessorData.AccessorBlocks[AccNum].Fields[0], NewRecBuf)
            or (FFetchAccessorData.AccessorBlocks[AccNum].BlockType = abBlob)))
      then begin
        case FFetchAccessorData.AccessorBlocks[AccNum].BlockType of
          abFetchBlock: begin
            // Prepare data
            CopyStatusFBlock(True);
            PostExternalAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum]);
            // Set data to IRowset
            if FHRowAccessible then
              hr := FIRowsetUpdate.SetData(FRowArray[0], FFetchAccessorData.AccessorBlocks[AccNum].hAcc, FFetchBuffer)
            else
              hr := FIRowsetUpdate.InsertRow(DB_NULL_HCHAPTER, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, FFetchBuffer, FRowArray[0]);
            CopyStatusFBlock(False);

            // Analyze OLE DB result
            Check(hr, AnalyzeFieldsStatus, NewRecBuf);
            FRowsObtained := 1;
            FHRowAccessible := True;
            Result := True;
          end;
          abOrdinary, abBLOB: begin
            // Prepare data
            PostPlainAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum], NeedToPost);
            if NeedToPost then begin
              // Set data to IRowset
              if FHRowAccessible then
                hr := FIRowsetUpdate.SetData(FRowArray[0], FFetchAccessorData.AccessorBlocks[AccNum].hAcc, NewRecBuf)
              else
                hr := FIRowsetUpdate.InsertRow(DB_NULL_HCHAPTER, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, NewRecBuf, FRowArray[0]);

              // Analyze OLE DB result
              Check(hr, AnalyzeFieldsStatus, NewRecBuf);
              FRowsObtained := 1;
              FHRowAccessible := True;
              Result := True;
            end;
          end;
          abReadOnly:;
        end;
      end;
    end;
  end;

var
  NewRecBuf, OldRecBuf: IntPtr;
  OLEDBStream: TOLEDBStream;
  i: integer;
begin
  Assert(FIRowsetUpdate <> nil, 'FCommand.FIRowsetUpdate must be setted');

  NewRecBuf := nil;
  OldRecBuf := nil;
  StreamList := TList.Create;
  try
    AllocRecBuf(OldRecBuf);
    GetRecord(OldRecBuf);

    AllocRecBuf(NewRecBuf); // Store old values to prevent conversion. Blob fields is not stored
    CopyBuffer(RecBuf, NewRecBuf, RecordSize);

    if Length(FRowArray) = 0 then begin
      SetLength(FRowArray, 1);
      FPRowArray := @FRowArray[0];
    end;

    try
      if not SetDataToRow(FRowArray[0], OldRecBuf, NewRecBuf, False) then
        SetDataToRow(FRowArray[0], OldRecBuf, NewRecBuf, True);
    except
      if FHRowAccessible then
        RowsetUpdateRollback;
      raise;
    end;

    if FHRowAccessible then
      RowsetUpdateCommit;
  finally
    FreeRecBuf(NewRecBuf);
    FreeRecBuf(OldRecBuf);

    // Remove streams
    for i := StreamList.Count - 1 downto 0 do begin
      OLEDBStream := TOLEDBStream(StreamList[i]);
      OLEDBStream.FStreamList := nil;
      OLEDBStream._Release;
    end;
    StreamList.Free;
  end;
end;

function TOLEDBRecordSet.FieldModified(FieldDesc: TFieldDesc; OldRecBuf, NewRecBuf: IntPtr): boolean;
begin
  if FieldDesc.IsBlob then
    Result := GetBlob(FieldDesc, NewRecBuf).CanRollback
  else
    Result := CompareFields(OldRecBuf, NewRecBuf, FieldDesc, [coOrdinalCompare], False) <> 0;
end;

procedure TOLEDBRecordSet.SetCommandProp;
var
  OLEDBProperties: TOLEDBPropertiesSet;
  IRowsetUpdateRequired: boolean;
  IsSQLEverywhere: boolean;
begin
{$IFDEF AUTOTEST}
  Inc(__SetRecordSetCommandPropCount);
{$ENDIF}

  Assert(FNativeRowset, 'FNativeRowset must be True');
  Assert(FCommand.FIMultipleResults = nil, 'FCommand.FIMultipleResults must be nil');

  IsSQLEverywhere := Provider = prCompact;
  if FCursorType in ServerCursorTypes then begin
    // Server cursor has no sence in disconnected mode
    if DisconnectedMode then
      DatabaseError(SDMandServerCursors);
    FFetchRows := 1;
    FFetchAll := False;
    FCommand.FRequestMultipleResults := False;
  end
  else
    FCommand.FRequestMultipleResults := not IsSQLEverywhere and not FDisableMultipleResults;

  IRowsetUpdateRequired := (ClientMajorVer = 7);

  OLEDBProperties := TOLEDBPropertiesSet.Create(FCommand.GetConnector, DBPROPSET_ROWSET);
  try
    with OLEDBProperties do begin
      // AddPropInt(DBPROP_ACCESSORDER, DBPROPVAL_AO_SEQUENTIALSTORAGEOBJECTS); - no performance improvement
      AddPropInt(DBPROP_ACCESSORDER, DBPROPVAL_AO_RANDOM);

      case FCursorType of
        ctDefaultResultSet: begin
          if not IsSQLEverywhere then begin
            if FCommand.FNotification then begin
              FCommand.FDelayedSubsciption := FUniqueRecords and not FReadOnly;
              AddPropBool(DBPROP_UNIQUEROWS, FUniqueRecords and not FReadOnly);
            end
            else
              AddPropBool(DBPROP_UNIQUEROWS, FUniqueRecords);
          end;
          AddPropBool(DBPROP_IColumnsRowset, FRequestSQLObjects);
          if not IsSQLEverywhere then begin
            AddPropBool(DBPROP_IMultipleResults, FCommand.FRequestMultipleResults);
            AddPropBool(DBPROP_SERVERCURSOR, False);
          end;
          AddPropBool(DBPROP_OWNINSERT, False);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, False);
          AddPropBool(DBPROP_OWNUPDATEDELETE, False);
          AddPropBool(DBPROP_IRowsetChange, False);
          AddPropBool(DBPROP_IRowsetUpdate, False);
        end;
        ctStatic: begin
          {Static RO}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_IRowsetChange, False);
          AddPropBool(DBPROP_IRowsetUpdate, False);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, False);
          AddPropBool(DBPROP_OWNINSERT, False);
          AddPropBool(DBPROP_OWNUPDATEDELETE, False);
          AddPropBool(DBPROP_REMOVEDELETED, False);
          AddPropBool(DBPROP_IRowsetResynch, False);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, False);
          AddPropBool(DBPROP_SERVERDATAONINSERT, False);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, True);
          AddPropBool(DBPROP_BOOKMARKS, True);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
        ctKeyset: begin
          {Keyset}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
          AddPropBool(DBPROP_OWNINSERT, True);
          AddPropBool(DBPROP_OWNUPDATEDELETE, True);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_IMMOBILEROWS, True);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, True);
          AddPropBool(DBPROP_BOOKMARKS, True);
          AddPropBool(DBPROP_REMOVEDELETED, True);

          // RO or RW cursor
          AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, IRowsetUpdateRequired);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, True);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
        ctDynamic: begin
          {Dynamic}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_CANHOLDROWS, False);
          AddPropBool(DBPROP_OTHERINSERT, True);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
          AddPropBool(DBPROP_OWNINSERT, True);
          AddPropBool(DBPROP_OWNUPDATEDELETE, True);
          AddPropBool(DBPROP_REMOVEDELETED, True);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, False);
          AddPropBool(DBPROP_SERVERDATAONINSERT, False);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_IMMOBILEROWS, False);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, False);
          AddPropBool(DBPROP_IRowsetScroll, False);
          AddPropBool(DBPROP_BOOKMARKS, False);
          AddPropBool(DBPROP_LITERALBOOKMARKS, False);

          // RO or RW cursor
          AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, IRowsetUpdateRequired);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
      end;
    end;
    OLEDBProperties.SetProperties(FCommand.FICommandProperties);
  finally
    OLEDBProperties.Free;
  end;
end;

procedure TOLEDBRecordSet.QueryCommandInterfaces(const QueryPrepare: boolean); // Create ConnectionSwap if need. Call FCommand.QueryInterfaces.
begin
  FCommand.QueryInterfaces(QueryPrepare);
end;

procedure TOLEDBRecordSet.ReleaseCommandInterfaces;
begin
  FCommand.ReleaseInterfaces;
end;
    
procedure TOLEDBRecordSet.QueryRecordSetInterfaces;
{  procedure GetProperties;
  const
    PropSetCnt = 1;
    PropCnt = 12;
  var
    RowsetInfo: IRowsetInfo;

    rgPropertyIDSets: PDBPropIDSetArray;
    DBPropIDArray: array[0..PropCnt - 1] of DBPROPID;

    rgPropertySets: PDBPropSet;
    cPropertySets: UINT;
    b: boolean;
    i: integer;
    st: DBPROPSTATUS;
  begin
    FIRowset.QueryInterface(IID_IRowsetInfo, RowsetInfo);

    DBPropIDArray[0] := DBPROP_SERVERCURSOR;
    DBPropIDArray[1] := DBPROP_IRowsetChange;
    DBPropIDArray[2] := DBPROP_IRowsetUpdate;
    DBPropIDArray[3] := DBPROP_OWNINSERT;
    DBPropIDArray[4] := DBPROP_OTHERINSERT;
    DBPropIDArray[5] := DBPROP_OTHERUPDATEDELETE;
    DBPropIDArray[6] := DBPROP_OWNUPDATEDELETE;
    DBPropIDArray[7] := DBPROP_REMOVEDELETED;
    DBPropIDArray[8] := DBPROP_IRowsetResynch;
    DBPropIDArray[9] := DBPROP_CHANGEINSERTEDROWS;
    DBPropIDArray[10] := DBPROP_SERVERDATAONINSERT;
    DBPropIDArray[11] := DBPROP_UNIQUEROWS;

    GetMem1(rgPropertyIDSets, PropSetCnt * sizeof(DBPropIDSet));
    try
      rgPropertyIDSets[0].guidPropertySet := DBPROPSET_ROWSET;
      rgPropertyIDSets[0].cPropertyIDs := PropCnt;
      rgPropertyIDSets[0].rgPropertyIDs := @DBPropIDArray;

      Check(RowsetInfo.GetProperties(PropSetCnt, rgPropertyIDSets,
        cPropertySets, PDBPropSet(rgPropertySets)));

      Assert(rgPropertySets <> nil, 'Cannot get properties');

      for i := 0 to PropCnt - 1 do
      begin
        b := rgPropertySets.rgProperties[i].vValue;
        st := rgPropertySets.rgProperties[i].dwStatus;
        b := not b;
      end;

    finally
      FreeMem1(rgPropertyIDSets);
      FCommand.FConnection.Malloc.Free(rgPropertySets.rgProperties);
      FCommand.FConnection.Malloc.Free(rgPropertySets);
    end;
  end;
}
begin
  try
    if FNativeRowset then begin
      Assert(FIRowset = nil, 'Duplicate call to TOLEDBRecordSet.QueryRecordSetInterfaces');
      Assert(FCommand.FIUnknown <> nil, 'FCommand.FIUnknown must be setted');

      QueryIntf(FCommand.FIUnknown, IID_IRowset, FIRowset);

      if FCursorType in [ctKeyset, ctStatic] then
        try
          QueryIntf(FIRowset, IID_IRowsetLocate, FIRowsetLocate);
        finally
          if FIRowsetLocate = nil then
            DatabaseError(SRecordsetBookmarksRequired);
        end;

      {if (FCursorType in [ctKeyset, ctDynamic]) then
        GetProperties;}
      if (FCursorType in [ctKeyset, ctDynamic]) and not FReadOnly and FCursorUpdate then
        Supports(FIRowset, IID_IRowsetUpdate, FIRowsetUpdate);

      FCommand.FIUnknown := nil;
    end;
  except
    ReleaseRecordSetInterfaces;
    raise;
  end;
end;

procedure TOLEDBRecordSet.ReleaseRecordSetInterfaces;
begin
  FIRowsetUpdate := nil;
  FIRowset := nil;
  FIRowsetLocate := nil;

  if FNativeRowset or (CursorTypeForFetching = ctBaseTable) then
    RequestParamsIfPossible;

  FNativeRowset := True;
end;

procedure TOLEDBRecordSet.ReleaseAllInterfaces;
begin
  ClearHRow;
  FreeFetchBuffer;

  if FNativeRowset then begin
    if not FCommand.FOpenNext then begin
      FCommand.ClearIMultipleResults;
      FCommand.FIUnknownNext := nil;
    end;
    FCommand.FIUnknown := nil;
  end;

  ReleaseRecordSetInterfaces;
end;

procedure TOLEDBRecordSet.InternalOpen(DisableInitFields: boolean = False);

  procedure OpenBaseTableRowset;
  var
    IUnk: IUnknown;
    FOpenRowset: IOpenRowset;
    TableID: PDBID;
    TableIDAccessor: TDBIDAccessor;
    OLEDBPropertiesSet: TOLEDBPropertiesSet;
    Connector: TOLEDBConnector;
  begin
    if DisconnectedMode and not FReadOnly and FCursorUpdate then
      DatabaseError(SDMandServerCursors);

    Connector := FCommand.GetConnector;
    Connector.Check(Connector.FIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
    FOpenRowset := IOpenRowset(IUnk);

    TableID := Marshal.AllocHGlobal(SizeOf(TDBID));
    TableIDAccessor := TDBIDAccessor.Create(TableID);
    OLEDBPropertiesSet := TOLEDBPropertiesSet.Create(Connector, DBPROPSET_ROWSET);
    try
      TableIDAccessor.eKind := DBKIND_NAME;
      TableIDAccessor.pwszName := Marshal.AllocHGlobal(Length(FBaseTableName) * SizeOf(WideChar) + SizeOf(WideChar));
      CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(FBaseTableName), 0, Length(FBaseTableName)))),
        TableIDAccessor.pwszName, Length(FBaseTableName) * SizeOf(WideChar) + SizeOf(WideChar));

      OLEDBPropertiesSet.AddPropBool(DBPROP_BOOKMARKS, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OWNUPDATEDELETE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OWNINSERT, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OTHERINSERT, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_CANFETCHBACKWARDS, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_QUICKRESTART, True);

      OLEDBPropertiesSet.AddPropBool(DBPROP_CANHOLDROWS, False);
      if Provider = prCompact then
        OLEDBPropertiesSet.AddPropInt(DBPROP_MAXOPENROWS, 1);

      // Transactions support
      OLEDBPropertiesSet.AddPropBool(DBPROP_COMMITPRESERVE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_ABORTPRESERVE, True);

      OLEDBPropertiesSet.AddPropBool(DBPROP_IColumnsRowset, FRequestSQLObjects);
      OLEDBPropertiesSet.AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, Boolean(ClientMajorVer = 7));

      Connector.Check(FOpenRowset.OpenRowset(nil, TableID, nil,
        IID_IRowset, 1, PDBPropIDSetArray(OLEDBPropertiesSet.InitPropSet), IUnk), nil);

      SetIRowset(IRowset(IUnk));

      if not FReadOnly and FCursorUpdate then begin
        QueryIntf(FIRowset, IID_IRowsetUpdate, FIRowsetUpdate);
        FFlatBuffers := True;
        FFetchAll := False;
        FFetchRows := 1;
      end;
    finally
      OLEDBPropertiesSet.Free;
      Marshal.FreeHGlobal(TableIDAccessor.pwszName);
      Marshal.FreeHGlobal(TableID);
      TableIDAccessor.Free;
    end;
  end;

begin
{$IFNDEF LITE}
  if IsServerCursor and (FSmartFetchState <> sfNone) then
    DatabaseError(SSFAndServerCursors);
{$ENDIF}

  try
    if FCursorType = ctBaseTable then
      OpenBaseTableRowset;

    if FNativeRowset then
      QueryCommandInterfaces(False); // If QueryInterfaces already called then do nothing. Need to prevent clear FICommandText & FICommandProperties after command.execute before CreateFieldDescByRowset

    inherited;
  except
    if FNativeRowset then
      ReleaseCommandInterfaces;

    raise;
  end;
end;

procedure TOLEDBRecordSet.InternalClose;
begin
  if FCommand.FNonBlocking then
    BreakFetch;

  ReleaseAllInterfaces;
  FreeData; // To destroy Blobs

  if FNativeRowset then
    ReleaseCommandInterfaces;

  FCommand.FCursorState := csInactive;
  if (not Prepared) or FCommand.FIsSProc then
    FCommand.CommandType := ctUnknown;

  FCommand.FNextResultRequested := False;
  FLastFetchEnd := False;
  FBulkExecuting := False;
  FIUnknownIsAssigned := False;

  inherited;
end;

procedure TOLEDBRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0);

  procedure ProcessCursorType;// Analyze CursorType changes
  var
    ActualCursorType: TMSCursorType; // Cursor type after OLEDB execute.
    ActualSCReadOnly: boolean; // ReadOnly after OLEDB execute. Only for server cursors and CursorUpdate = True

    procedure AnalyzeCursorType; // Analyzed by FCommand.FIUnknown
    var
      RowsetInfo: IRowsetInfo;
      PropValues: TPropValues;
    begin
      with TOLEDBPropertiesGet.Create(FCommand.GetConnector, DBPROPSET_ROWSET) do
        try
          AddPropId(DBPROP_SERVERCURSOR);
          AddPropId(DBPROP_OTHERUPDATEDELETE);
          AddPropId(DBPROP_IRowsetLocate);
          AddPropId(DBPROP_IRowsetUpdate);

          // Getting info interface
          if FCommand.FIUnknown <> nil then begin
            QueryIntf(FCommand.FIUnknown, IID_IRowsetInfo, RowsetInfo);
            GetProperties(RowsetInfo, PropValues);
          end
          else begin
            Assert(FCommand.FICommandProperties <> nil);
            GetProperties(FCommand.FICommandProperties, PropValues);
          end;
        finally
          Free;
        end;

      if {DBPROP_SERVERCURSOR} PropValues[0] = False then
        ActualCursorType := ctDefaultResultSet
      else begin // Static, KeySet, Dynamic
        if {DBPROP_OTHERUPDATEDELETE} PropValues[1] = False then begin // Static
          ActualCursorType := ctStatic;
          ActualSCReadOnly := True;
        end
        else begin // KeySet, Dynamic
          if {DBPROP_IRowsetLocate} PropValues[2] = False then // Dynamic
            ActualCursorType := ctDynamic
          else
            ActualCursorType := ctKeySet; // KeySet

          ActualSCReadOnly := {DBPROP_IRowsetUpdate} PropValues[3] = False;
        end;
      end;
    end;

  var
    IsChanged, IsROChanged: boolean;
  begin
    ActualCursorType := FCursorType;
    ActualSCReadOnly := FReadOnly;
    AnalyzeCursorType;

    IsROChanged := (FCursorType in ServerCursorTypes) and FCursorUpdate and (FReadOnly <> ActualSCReadOnly);
    IsChanged := (FCursorType <> ActualCursorType) or IsROChanged;

    if IsChanged then begin
      FCursorType := ActualCursorType;
      if IsROChanged then
        FReadOnly := ActualSCReadOnly;

      Assert(Assigned(FCursorTypeChanged));
      try
        FCursorTypeChanged; // may be exception
      except
        FCommand.FIUnknown := nil;
        FCommand.ClearIMultipleResults;
        raise;
      end;
    end;
  end;

begin
  if not FNativeRowset then
    Exit; // CommandExec is not need for non-Native rowsets

  if FIUnknownIsAssigned then begin
    FCommand.CommandType := ctCursor;
    Exit;
  end;

  QueryCommandInterfaces(False);
  try
    if not Prepared
      and (FCommand.FIMultipleResults = nil)
      and FNativeRowset then // This is a first call to non-prepared DataSet.Command.Execute
      SetCommandProp;
    inherited;

    if (FCommand.FIUnknown <> nil) and FCommand.FLastExecWarning then
      ProcessCursorType;

    // Must be after ProcessCursorType to prevent wrong setting CommandType
    if ((FCommand.FIUnknown <> nil) or (FIRowset <> nil)) and not FBulkExecuting then
      FCommand.CommandType := ctCursor
    else
      FCommand.CommandType := ctStatement;

  finally
    ReleaseCommandInterfaces;
    if FCommand.CommandType <> ctCursor then
      FCommand.SetCursorState(csInactive);
  end;
end;

function TOLEDBRecordSet.IsFullReopen: boolean;
begin
  Result := inherited IsFullReopen or not IsServerCursor;
end;

procedure TOLEDBRecordSet.Reopen;
begin
  if not IsFullReopen then
    ReleaseAllInterfaces;

  if not FNativeRowset and (FCursorType <> ctBaseTable) then
    SetIRowset(FCommand.GetConnector.GetSchemaRowset(FSchema, FRestrictions));

  inherited;
end;

function TOLEDBRecordSet.GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
begin
  FSchema := Schema;
  FRestrictions := rgRestrictions;

  Assert(FCommand.FConnection <> nil);
  Result := FCommand.GetConnector.GetSchemaRowset(FSchema, FRestrictions);
end;

procedure TOLEDBRecordSet.Disconnect;
begin
  Assert(FCommand <> nil);
  //Cache connection depenednt information
  GetClientMajorVer;
  GetProvider;

  ReleaseAllInterfaces;
  inherited;
end;


function TOLEDBRecordSet.GetCommandClass: TSqlCommandClass;
begin
  Result := TOLEDBCommand;
end;

procedure TOLEDBRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TOLEDBCommand(Value);
end;

function TOLEDBRecordSet.IsMetaInfo: boolean;
begin
  Result := not NativeRowset;
end;

function TOLEDBRecordSet.GetIRowset: IRowset;
begin
  Result := FIRowset;
end;

procedure TOLEDBRecordSet.SetIRowset(Rowset: IRowset);
begin
  Close;
  Unprepare;

  FIRowset := Rowset;
  FNativeRowset := False;
  if FCursorType <> ctBaseTable then begin
    FReadOnly := True; // Non-native rowset cannot be modified
    FFlatBuffers := True;
  end;

  FCommand.FCursorState := csExecuted;
  FCommand.CommandType := ctUnknown;
end;

procedure TOLEDBRecordSet.AssignToNextResult(Dest: TSqlRecordSet);
begin
  while (FCommand.FIMultipleResults <> nil) and (FCommand.FIUnknown = nil) do
    FCommand.GetNextResult(FCommand.FIUnknown, FCommand.FRowsAffected);

  if FCommand.FIUnknown = nil then
    DatabaseError(SNoNextResultSet, nil);

  Assert(Dest <> nil);
  TOLEDBRecordSet(Dest).FCommand.FIUnknown := FCommand.FIUnknown;
  TOLEDBRecordSet(Dest).FCommand.FRowsAffected := FCommand.FRowsAffected;
  TOLEDBRecordSet(Dest).FIUnknownIsAssigned := True;
  FCommand.FIUnknown := nil;
end;

function TOLEDBRecordSet.HasNextResult: boolean;
begin
  Result := FCommand.FIUnknownNext <> nil;
end;

function TOLEDBRecordSet.CheckNextResult: boolean;
begin
  Result := FCommand.FIUnknownNext <> nil;
end;

procedure TOLEDBRecordSet.Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
begin
  if FNativeRowset or (FCursorType = TMSCursorType(ctTableType)) then
    FCommand.GetConnector.Check(Status, Component, AnalyzeMethod, Arg)
  else
    if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) then
      raise EOLEDBError.Create(Status, WideString(Format(SOLEDBError, [Status])));
end;

procedure TOLEDBRecordSet.RequestParamsIfPossible;
begin
  if (FIRowsetLocate = nil) and (FIRowsetUpdate = nil) and (FIRowset = nil) then begin
    FCommand.RequestParamsIfPossible;

    if (FCommand.FIUnknown = nil)
      and ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then begin
      FCommand.SetCursorState(csFetched);
      if not Prepared then
        ReleaseCommandInterfaces;
    end;
  end;
end;

function TOLEDBRecordSet.GetClientMajorVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FClientMajorVer := FCommand.FConnection.ClientMajorVer;
  Result := FClientMajorVer;
end;

function TOLEDBRecordSet.GetProvider: TMSProvider;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FProvider := FCommand.FConnection.Provider;
  Result := FProvider;
end;

{$IFDEF LITE}
function TOLEDBRecordSet.GetBCDPrecision: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.Options.BCDPrecision
  else
    Result := 0;
end;

function TOLEDBRecordSet.GetBCDScale: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.Options.BCDScale
  else
    Result := 0;
end;
{$ENDIF}

procedure TOLEDBRecordSet.CheckBCDOverflow(Field: TFieldDesc; RecBuf: IntPtr);
begin
  if (Field.DataType = dtBCD)
    and (GetStatus(Field, RecBuf) = DBSTATUS_E_DATAOVERFLOW) then
    raise Exception.Create(SBCDOverflow);
end;

function TOLEDBRecordSet.AnalyzeFieldsStatus(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
const
  FieldHeader = 'Field[%d] %s - %s (Status = %Xh).';
var
  i: integer;
  FieldStatus: DWORD;
  FieldName: string;
  Msg: WideString;
  pRec: IntPtr;
  Field: TFieldDesc;
begin
  Result := '';
  if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) then begin
    pRec := Arg;
    Msg := '';
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.FieldDescKind = fdkData then begin
        FieldName := Field.Name;
        if FieldName = '' then
          FieldName := IntToStr(i)
        else
          FieldName := ':' + FieldName;
        FieldStatus := GetStatus(Field, pRec);
        case FieldStatus of
          DBSTATUS_S_OK, DBSTATUS_S_ISNULL, DBSTATUS_S_DEFAULT:;
          DBSTATUS_E_BADACCESSOR:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SBadAccessor, FieldStatus]);
          DBSTATUS_E_CANTCONVERTVALUE:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SInvalidValue, FieldStatus]);
          DBSTATUS_S_TRUNCATED:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SDataTruncated, FieldStatus]);
          DBSTATUS_E_SIGNMISMATCH:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SSignMismatch, FieldStatus]);
          DBSTATUS_E_DATAOVERFLOW:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SDataOverflow, FieldStatus]);
          DBSTATUS_E_CANTCREATE:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SCantCreate, FieldStatus]);
          DBSTATUS_E_UNAVAILABLE:
            {AddInfoToErr(Msg, FieldHeader, [i, FieldName, SUnavaible, FieldStatus])};
          DBSTATUS_E_PERMISSIONDENIED:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SPermissionDenied, FieldStatus]);
          DBSTATUS_E_INTEGRITYVIOLATION:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SIntegrityViolation, FieldStatus]);
          DBSTATUS_E_SCHEMAVIOLATION:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SShemaViolation, FieldStatus]);
          DBSTATUS_E_BADSTATUS:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SBadStatus, FieldStatus]);
        else
          AddInfoToErr(Msg, FieldHeader, [i, FieldName, SUnknownStatus, FieldStatus]);
        end;
      end;
    end;
    Result := Msg;
  end;
end;

function TOLEDBRecordSet.UseExtNumeric: boolean;
begin
  Result := not IsServerCursor;
end;

function TOLEDBRecordSet.UseExtTimeStamp: boolean;
begin
  Result := (Provider = prCompact) or IsServerCursor;
end;

function TOLEDBRecordSet.ConvertDBCOLUMNINFOToFieldDesc(
  const FieldName: string; // prgInfoEl.pwszName
  const ActualFieldName: string; // if prgInfoEl.columnid.eKind = DBKIND_NAME then ActualFieldName := prgInfoEl.columnid.uName.pwszName else ActualFieldName := prgInfoEl.pwszName;
  const FieldNo: DBORDINAL; // prgInfoEl.iOrdinal
  const OLEDBType: DBTYPE; // prgInfoEl.wType
  const dwFlags: DBCOLUMNFLAGS; // prgInfoEl.dwFlags
  const IsAutoIncrement: boolean;
  const Precision: integer; //prgInfoEl.bPrecision
  const Scale: integer; //prgInfoEl.bScale
  const ColumnSize: DBLENGTH; // prgInfoEl.ulColumnSize
  Field: TMSFieldDesc // Result
  ): boolean; // return False, if this field type not supported

  function IsFlagSetted(const Flag: UINT): boolean;
  begin
    Result := (dwFlags and Flag) <> 0;
  end;

  procedure ConvertFlags;
  begin
    // DBCOLUMNFLAGS_ISFIXEDLENGTH, DBCOLUMNFLAGS_ISLONG,
    // DBCOLUMNFLAGS_ISNULLABLE, DBCOLUMNFLAGS_MAYBENULL
    // DBCOLUMNFLAGS_WRITE, DBCOLUMNFLAGS_WRITEUNKNOWN
    // DBCOLUMNFLAGS_ISROWID, DBCOLUMNFLAGS_ISROWVER
    // DBCOLUMNFLAGS_SCALEISNEGATIVE

    Field.Fixed := IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH);
    Field.IsKey := IsFlagSetted(DBCOLUMNFLAGS_KEYCOLUMN);
    if Provider = prCompact then
      Field.Required := not IsFlagSetted(DBCOLUMNFLAGS_ISNULLABLE) and not IsFlagSetted(DBCOLUMNFLAGS_MAYBENULL)
    else
      Field.Required := not IsFlagSetted(DBCOLUMNFLAGS_ISNULLABLE);
    Field.IsTimestamp := IsFlagSetted(DBCOLUMNFLAGS_ISROWVER);
    Field.IsLong := IsFlagSetted(DBCOLUMNFLAGS_ISLONG);

    Field.ReadOnly :=
      not IsFlagSetted(DBCOLUMNFLAGS_WRITE) and
      not IsFlagSetted(DBCOLUMNFLAGS_WRITEUNKNOWN);

    if FNativeRowset
      and not (FCursorType in ServerCursorTypes)
      and Field.ReadOnly then
      Field.ReadOnly := Field.IsTimestamp or IsAutoIncrement;

    Field.IsAutoIncrement := IsAutoIncrement;
  end;

  function CreateUniqueFieldName(const FieldName: string): string;
  var
    AliasNum: integer;
  begin
    if FieldName = '' then begin
      AliasNum := 1;
      repeat
        Result := UniqueFieldNamePrefix + IntToStr(AliasNum);
        Inc(AliasNum);
      until FindField(Result) = nil;
    end
    else
      Result := FieldName;
  end;

var
  InternalType: word;
{$IFNDEF LITE}
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
{$IFNDEF LITE}
  if Precision = 0 then
    Field.DBLength := Integer(ColumnSize)
  else
    Field.DBLength := Precision;
  Field.DBScale := Scale;
  Field.DBType := TMSConverterManager.GetDBType(OLEDBType, dwFlags, Field.DBLength);

  FetchConverter := GetMapFetchConverter(FieldName, Field.DBType, Field.DBLength, Field.DBScale);

  if FetchConverter <> nil then begin
    InternalType := FetchConverter.InternalDataType;
    Result := True;
  end
  else
{$ENDIF}
  begin
    Result := ConvertOLEDBTypeToInternalFormat(OLEDBType,
      IsFlagSetted(DBCOLUMNFLAGS_ISLONG), IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH),
      FCommand.CalcEnableBCD, FCommand.CalcEnableFMTBCD, FCommand.FSensibleBCDMapping,
      {$IFDEF LITE}GetBCDPrecision, GetBCDScale,{$ENDIF}
      Precision, Scale, FWideStrings, GetWideMemos, False, InternalType, Provider);
    if not Result then
      Exit;

    // --- Correct access to SQL 2000 server from 7.0 Client
    if FNativeRowset and
      (ClientMajorVer < 8) and not IsWindowsVista and (Provider <> prCompact) and
      (OLEDBType = DBTYPE_NUMERIC) and (Precision = 19) then
      InternalType := dtInt64;

    if not FLongStrings and (ColumnSize > 255) then
      if InternalType = dtString then
        InternalType := dtMemo
      else
      if InternalType = dtWideString then
        InternalType := dtWideMemo;
  end;

  if not FFlatBuffers and (ColumnSize >= FlatBufferLimit) and (FCursorType = ctDefaultResultSet) and (OLEDBType <> DBTYPE_VARIANT) then begin
    case InternalType of
      dtString:
        InternalType := dtExtString;
      dtWideString:
        InternalType := dtExtWideString;
      dtVarBytes:
        InternalType := dtExtVarBytes;
    end;
  end;

  Field.DataType := InternalType;
  Field.SubDataType := dtUnknown;

  if FNativeRowset then
    Field.Name := CreateUniqueFieldName(FieldName)
  else
    Field.Name := FieldName;

  if ActualFieldName <> '' then
    Field.ActualName := ActualFieldName
  else
    Field.ActualName := Field.Name;

  Field.ActualFieldNo := FieldNo;

  ConvertFlags;

  {if FNativeRowset then
    Field.Hidden := integer(prgInfoEl.columnid.uGuid.pguid) <> 1;
  // :( This is impossible because we needs TField for update SQLs and for ColumnsMetaInfo}

  Field.Size := GetBufferSize(Field.DataType, integer(ColumnSize));

  case InternalType of
    dtBoolean, dtUInt8, dtInt16, dtWord, dtInt32, dtUInt32, dtInt64, dtUInt64:
      ;
    dtSingle: begin
      Field.Scale := Scale;
      // Field.Length := Precision; // Precision cannot be greater then 7
      Field.Length := 7;
    end;
    dtFloat, dtBCD, dtFmtBCD: begin
      case InternalType of
        dtFloat: begin
          Field.Scale := Scale;
          if OLEDBType = DBTYPE_R8 then
            Field.Length := 15
          else
            Field.Length := Precision; // Precision cannot be greater then 15
        end;
        // Multibyte fields
        dtBCD: begin
          Field.Scale := Scale;
          Field.Length := Precision;
        end;
        dtFmtBCD: begin
          if Precision < SizeOfTBcd then
            Field.Size := SizeOfTBcd + 1{'+/-'} + 1{'.'} + 1{#0}
          else
            Field.Size := Precision + 1{'.'} + 1 {#0}; // To right notation of large NUMERIC values
          Field.Scale := Scale;
          Field.Length := Precision;

          if Field.Length > MaxFMTBcdDigits then
            Field.Length := MaxFMTBcdDigits;
          if Field.Scale > Field.Length then // if length was reduced
            Field.Scale := Field.Length;
        end;
      end;
      if (OLEDBType = DBTYPE_NUMERIC) and UseExtNumeric then
        Field.SubDataType := sdtNumeric;
    end;
    dtCurrency: begin
      Field.Scale := Scale;
      Field.Length := Precision; // Precision cannot be greater then 15
    end;

    dtDateTime:
      if UseExtTimeStamp then
        Field.Size := sizeof(TDBTimeStamp)
      else
        Field.Size := sizeof(TDateTime);
    dtDate, dtTime:
      ;
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Field.Size := sizeof(TDBTimeStamp);
  {$ENDIF}
    dtGuid:
      Field.Length := 38; {Length(GuidString)}

    dtString, dtWideString, dtExtString, dtExtWideString: begin
      if OLEDBType = DBTYPE_VARIANT then begin
        Field.Length := MaxNonBlobFieldLen;
      end
      else begin
        Field.Length := Word(ColumnSize);
        if OLEDBType = DBTYPE_WSTR then
          Field.SubDataType := sdtWide;
      end;
    end;

    dtBytes, dtVarBytes, dtExtVarBytes:
      Field.Length := Word(ColumnSize);

    dtBlob, dtMemo, dtWideMemo, dtXML: begin
      Field.Size := sizeof(IntPtr) {ISeqStream} + sizeof(DBLENGTH) {OLE DB readed bytes};

      if not IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then // UDT field
        Field.Length := Word(ColumnSize);

    {$IFDEF LITE}
      if OLEDBType = DBTYPE_XML then
        Field.SubDataType := dtXML;
    {$ENDIF}
      if (OLEDBType = DBTYPE_WSTR) or (OLEDBType = DBTYPE_XML) then
        Field.SubDataType := Field.SubDataType or sdtWide;
      if OLEDBType = DBTYPE_UDT then
        Field.SubDataType := sdtMSUDT;
    end;

    dtVariant:
      ;
  else
    Result := False;
  end;
end;

procedure TOLEDBRecordSet.CreateFieldDescsByInfo(IUnk: IUnknown);
  function GetHiddenColumnsCount: UINT;
  var
    PropValues: TPropValues;
  begin
    with TOLEDBPropertiesGet.Create(FCommand.GetConnector, DBPROPSET_ROWSET) do
      try
        AddPropId(DBPROP_HIDDENCOLUMNS);
        GetProperties(FCommand.FICommandProperties, PropValues);
      finally
        Free;
      end;
    Result := PropValues[0];
  end;

var
  ColumnsInfo: IColumnsInfo;
  i: integer;
  Field: TMSFieldDesc;

  cColumns: NativeUInt;
  prgInfo, prgInfoEl: PDBCOLUMNINFO;
  rgInfoEl: PDBCOLUMNINFO;
  pStringsBuffer: IntPtr;

  ActualFieldName: string;
  TableInfo: TCRTableInfo;
begin
  if IUnk = nil then
    Exit; // This query does not return rowset

  QueryIntf(IUnk, IID_IColumnsInfo, ColumnsInfo);
  Assert(ColumnsInfo <> nil);

  pStringsBuffer := nil;
  Check(ColumnsInfo.GetColumnInfo(cColumns, PDBCOLUMNINFO(prgInfo), pStringsBuffer));

  // Add hidden columns count for SQL 2000
  if FNativeRowset and ((ClientMajorVer >= 8) or IsWindowsVista) and FUniqueRecords and not FHideSystemUniqueFields then
    cColumns := cColumns + GetHiddenColumnsCount;

  if cColumns > 0 then
    try
      FBookmarkField := nil;
      prgInfoEl := prgInfo;
      for i := 0 to cColumns - 1 do begin
        rgInfoEl := prgInfoEl;
        Field := TMSFieldDesc(CreateFieldDesc);
        try
          if rgInfoEl.columnid.eKind = DBKIND_NAME then
            ActualFieldName := string(Marshal.PtrToStringUni(rgInfoEl.columnid.uName.pwszName))
          else
            ActualFieldName := rgInfoEl.pwszName;

          if rgInfoEl.bPrecision = $FF then
            rgInfoEl.bPrecision := 0;
          if rgInfoEl.bScale = $FF then
            rgInfoEl.bScale := 0;

          if ConvertDBCOLUMNINFOToFieldDesc(
            rgInfoEl.pwszName,
            ActualFieldName,
            rgInfoEl.iOrdinal,
            rgInfoEl.wType,
            rgInfoEl.dwFlags,
            False,
            rgInfoEl.bPrecision,
            rgInfoEl.bScale,
            rgInfoEl.ulColumnSize,
            Field) then
          begin
            if FCursorType = ctBaseTable then begin
              // Fill TablesInfo structure
              Assert(FBaseTableName <> '');
              TableInfo := FTablesInfo.FindByName(FBaseTableName);
              if TableInfo = nil then begin
                TableInfo := FTablesInfo.Add;
                TableInfo.TableName := FBaseTableName;
                TableInfo.TableAlias := '';
              end;
              Field.TableInfo := TableInfo;
            end;

            if Field.ActualFieldNo > 0 then begin
              FFields.Add(Field);
              Field.FieldNo := FFields.Count;
            end
            else begin // Bookmark column have FieldNo = 0
              Field.Hidden := True;
              FBookmarkField := Field;
            end;
          end
          else
            if FNativeRowset then
              DatabaseErrorFmt(SBadFieldType, [rgInfoEl.pwszName, rgInfoEl.wType])
            else
              Field.Free;
        except
          Field.Free;
          FBookmarkField.Free;
          FBookmarkField := nil;
          raise;
        end;
        prgInfoEl := PtrOffset(prgInfoEl, sizeof(DBCOLUMNINFO));
      end;

      if FBookmarkField <> nil then
        FFields.Add(FBookmarkField);
    finally
      FreeCoMem(prgInfo);
      FreeCoMem(pStringsBuffer);
    end;
end;

{$IFNDEF LITE}
procedure TOLEDBRecordSet.CheckColumnsMetaInfoIdx;
begin
  if FFldFieldNameIdx <> - 1 then
    Exit;

  FFldFieldNameIdx := FFields.IndexOf(FieldByName('DBCOLUMN_NAME'));

  FFldCatalogNameIdx := FFields.IndexOf(FindField('DBCOLUMN_BASECATALOGNAME')); // SQL Everywhere
  FFldSchemaNameIdx := FFields.IndexOf(FindField('DBCOLUMN_BASESCHEMANAME')); // SQL Everywhere

  FFldTableNameIdx := FFields.IndexOf(FieldByName('DBCOLUMN_BASETABLENAME'));
  FFldColumnNameIdx := FFields.IndexOf(FieldByName('DBCOLUMN_BASECOLUMNNAME'));

  FFldPrecisionIdx := FFields.IndexOf(FieldByName('DBCOLUMN_PRECISION'));
  FFldScaleIdx := FFields.IndexOf(FieldByName('DBCOLUMN_SCALE'));
  FFldGuidIdx := FFields.IndexOf(FieldByName('DBCOLUMN_GUID'));

  {
  SELECT BaseColumnName AS ColumnAlias ...

  FIConnection.ProviderVer = [07.01.0623, 07.01.0690, 07.01.0819, 07.01.0961]
    IDNAME = BaseColumnName
    NAME = ColumnAlias
    BaseColumnName = BaseColumnName

  FIConnection.ProviderVer = [08.00.0194, 08.00.0528, 08.10.7430, 08.10.9001]
    IDNAME = ColumnAlias
    NAME = ColumnAlias
    BaseColumnName = BaseColumnName
  }

  // ??? May be swap?
  //FFldFieldNameIdx := Fields.IndexOf(FieldByName('DBCOLUMN_IDNAME'));
  //FFldActualFieldNameIdx := Fields.IndexOf(FieldByName('DBCOLUMN_NAME'));

  FFldColumnNumberIdx := FFields.IndexOf(FieldByName('DBCOLUMN_NUMBER'));

  FFldIsAutoIncIdx := FFields.IndexOf(FindField('DBCOLUMN_ISAUTOINCREMENT')); // SQL Everywhere
  FFldTypeIdx := FFields.IndexOf(FieldByName('DBCOLUMN_TYPE'));
  FFldFlagsIdx := FFields.IndexOf(FieldByName('DBCOLUMN_FLAGS'));
  FFldColumnSizeIdx := FFields.IndexOf(FieldByName('DBCOLUMN_COLUMNSIZE'));
  FFldComputeModeIdx := FFields.IndexOf(FindField('DBCOLUMN_COMPUTEMODE'));

  // XML schema support
  FFldXMLSchemaCollCatalogNameIdx := FFields.IndexOf(FindField('DBCOLUMN_XML_SCHEMACOLLECTION_CATALOGNAME'));
  FFldXMLSchemaCollSchemaNameIdx := FFields.IndexOf(FindField('DBCOLUMN_XML_SCHEMACOLLECTION_SCHEMANAME'));
  FFldXMLSchemaCollNameIdx := FFields.IndexOf(FindField('DBCOLUMN_XML_SCHEMACOLLECTIONNAME'));

  // UDT support
  FFldUDTSchemanameIdx := FFields.IndexOf(FindField('DBCOLUMN_UDT_SCHEMANAME'));
  FFldUDTNameIdx := FFields.IndexOf(FindField('DBCOLUMN_UDT_NAME'));
  FFldUDTCatalognameIdx := FFields.IndexOf(FindField('DBCOLUMN_UDT_CATALOGNAME'));
  FFldAssemblyTypenameIdx := FFields.IndexOf(FindField('DBCOLUMN_ASSEMBLY_TYPENAME'));
end;

procedure TOLEDBRecordSet.CreateFieldDescsByRowset(IUnk: IUnknown);
var
  RecBuf: IntPtr;
  ColumnsMetaInfo: TOLEDBRecordSet;

  function GetWordValue(Idx: integer): Word;
  var
    Field: TFieldDesc;
  begin
    Field := ColumnsMetaInfo.Fields[Idx];
    if ColumnsMetaInfo.GetNull(Field, RecBuf) then
      Result := 0
    else
      Result := Word(Marshal.ReadInt16(RecBuf, Field.DataOffset));
  end;

  function GetDWordValue(Idx: integer): cardinal;
  var
    Field: TFieldDesc;
  begin
    Field := ColumnsMetaInfo.Fields[Idx];
    if ColumnsMetaInfo.GetNull(Field, RecBuf) then
      Result := 0
    else
      Result := cardinal(Marshal.ReadInt32(RecBuf, Field.DataOffset));
  end;

var
  ColumnsRowset: IColumnsRowset;
  iu: IUnknown;
  CMIRowset: IRowset;

  Field: TMSFieldDesc;
  FieldNo: cardinal;
  TableName, FieldName, ActualFieldName: string;
  FldGuidValue, FieldGuid: string;
  TableInfo: TSqlTableInfo;
  IsAutoIncrement: boolean;

  ColumnsRecordSet: TOLEDBRecordSet;
  Value: variant;
begin
  if IUnk = nil then
    Exit; // This query does not return rowset

  QueryIntf(IUnk, IID_IColumnsRowset, ColumnsRowset);
  Check(ColumnsRowset.GetColumnsRowset(nil, 0, nil, IID_IRowset, 0, nil, iu)); {Default properties - default result set}
  CMIRowset := IRowset(iu);
  Assert(CMIRowset <> nil);
  FBookmarkField := nil;
  RecBuf := nil;

  ColumnsMetaInfo := FCommand.GetConnector.CheckColumnsMetaInfo;
  ColumnsMetaInfo.SetIRowset(CMIRowset);
  ColumnsMetaInfo.Open;
  try
    ColumnsMetaInfo.CheckColumnsMetaInfoIdx;
    ColumnsMetaInfo.AllocRecBuf(RecBuf);

    while True do begin
      ColumnsMetaInfo.GetNextRecord(RecBuf);
      if ColumnsMetaInfo.Eof then
        Break;

      Field := TMSFieldDesc(CreateFieldDesc);
      try
        FieldNo := GetDWordValue(ColumnsMetaInfo.FFldColumnNumberIdx);
        FieldName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldFieldNameIdx]);
        if ColumnsMetaInfo.FFldColumnNameIdx <> -1 then
          ActualFieldName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldColumnNameIdx])
        else
          ActualFieldName := '';

        IsAutoIncrement := False;
        if ColumnsMetaInfo.FFldIsAutoIncIdx <> - 1 then
          IsAutoIncrement := WordBool(GetWordValue(ColumnsMetaInfo.FFldIsAutoIncIdx));

        if ConvertDBCOLUMNINFOToFieldDesc(
          FieldName,
          ActualFieldName,
          FieldNo,
          GetWordValue(ColumnsMetaInfo.FFldTypeIdx),
          GetDWordValue(ColumnsMetaInfo.FFldFlagsIdx),
          IsAutoIncrement,
          GetWordValue(ColumnsMetaInfo.FFldPrecisionIdx),
          GetWordValue(ColumnsMetaInfo.FFldScaleIdx),
          GetDWordValue(ColumnsMetaInfo.FFldColumnSizeIdx),
          Field) then begin

          if ColumnsMetaInfo.FFldCatalogNameIdx <> -1 then
            Field.BaseCatalogName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldCatalogNameIdx]);
          if ColumnsMetaInfo.FFldSchemaNameIdx <> -1 then
            Field.BaseSchemaName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldSchemaNameIdx]);
          Field.BaseTableName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldTableNameIdx]);

          if ColumnsMetaInfo.FFldXMLSchemaCollCatalogNameIdx <> -1 then
            Field.XMLSchemaCollectionCatalogName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollCatalogNameIdx]);
          if ColumnsMetaInfo.FFldXMLSchemaCollSchemaNameIdx <> -1 then
            Field.XMLSchemaCollectionSchemaName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollSchemaNameIdx]);
          if ColumnsMetaInfo.FFldXMLSchemaCollNameIdx <> -1 then
            Field.XMLSchemaCollectionName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollNameIdx]);
          Field.XMLTyped := (Field.XMLSchemaCollectionCatalogName <> '') or (Field.XMLSchemaCollectionSchemaName <> '') or
            (Field.XMLSchemaCollectionName <> '');

          if ColumnsMetaInfo.FFldUDTSchemanameIdx <> -1 then
            Field.UDTSchemaname := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTSchemanameIdx]);
          if ColumnsMetaInfo.FFldUDTNameIdx <> -1 then
            Field.UDTName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTNameIdx]);
          if ColumnsMetaInfo.FFldUDTCatalognameIdx <> -1 then
            Field.UDTCatalogname := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTCatalognameIdx]);
          if ColumnsMetaInfo.FFldAssemblyTypenameIdx <> -1 then
            Field.AssemblyTypename := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldAssemblyTypenameIdx]);
        {$IFNDEF LITE}
          if (Field.UDTName <> '') and (Field.AssemblyTypename <> '') then
            Field.UDTDispatcher := TUDTDispatcher.Create;
        {$ENDIF}

          Field.ReadOnly := Field.ReadOnly or (ActualFieldName = '');

          if ColumnsMetaInfo.FFldComputeModeIdx <> -1 then
            Field.ReadOnly := Field.ReadOnly or not (GetDWordValue(ColumnsMetaInfo.FFldComputeModeIdx) in [0, DBCOMPUTEMODE_NOTCOMPUTED]);

          if FHideSystemUniqueFields or (FCursorType in ServerCursorTypes) then // Hide implicitly requested columns
            if (ClientMajorVer >= 8) or IsWindowsVista then begin
              FldGuidValue := LowerCase(ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldGuidIdx]));
              FieldGuid := LowerCase(IntToHex(FieldNo, 8)) + '-0000-0000-0000-000000000000';
              if UidWithBraces then
                FieldGuid := '{' + FieldGuid + '}';

              Field.Hidden := (FldGuidValue <> '') and (FldGuidValue <> FieldGuid);
            end
            else
              Field.Hidden := not ColumnsMetaInfo.GetNull(ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldGuidIdx], RecBuf);

          // Fill TablesInfo structure
          TableName := GenerateTableName(Field.BaseCatalogName, Field.BaseSchemaName, Field.BaseTableName,
            FCommand.SQLInfo.NormalizeName(FCommand.FConnection.Database, False, True));
          if TableName <> '' then begin
            TableInfo := TSqlTableInfo(FTablesInfo.FindByName(TableName));
            if TableInfo = nil then begin
              TableInfo := TSqlTableInfo(FTablesInfo.Add);
              TableInfo.TableName := TableName;
              TableInfo.BaseTableName := Field.BaseTableName;
              TableInfo.TableAlias := '';
            end;
            Field.TableInfo := TableInfo;
          end
          else
            Field.TableInfo := nil;

          if FieldNo > 0 then begin
            FFields.Add(Field);
            Field.FieldNo := FFields.Count;
          end
          else begin // Bookmark column have FieldNo = 0
            Field.Hidden := True;
            FBookmarkField := Field;
          end;

        end
        else
          DatabaseErrorFmt(SBadFieldType, [FieldName, GetWordValue(ColumnsMetaInfo.FFldTypeIdx)])
      except
        Field.Free;
        FBookmarkField.Free;
        FBookmarkField := nil;
        raise;
      end;
    end;

    if FBookmarkField <> nil then
      FFields.Add(FBookmarkField);

  finally
    if RecBuf <> nil then
      ColumnsMetaInfo.FreeRecBuf(RecBuf);

    ColumnsMetaInfo.AutoInitFields := False; // Save fields for reusing
    ColumnsMetaInfo.Close;

    if IsLibrary then
      FreeAndNil(FCommand.GetConnector.FColumnsMetaInfo);
  end;
  Assert(not (FCursorType in [ctStatic, ctKeySet]) or (FBookmarkField <> nil));

  if (Provider = prCompact) and not FPopulatingKeyInfo and FUniqueRecords then begin
    if FTablesInfo.Count > 0 then begin
      RecBuf := nil;
      TableInfo := TSqlTableInfo(FTablesInfo[0]);
      ColumnsRecordSet := TOLEDBRecordSet.Create;
      FPopulatingKeyInfo := True;
      try
        ColumnsRecordSet.SetConnection(FCommand.FConnection);
        ColumnsRecordSet.SetSQL(Format(
          'SELECT' + DALineSeparator +
          '  A.COLUMN_NAME, B.AUTOINC_INCREMENT' + DALineSeparator +
          'FROM' + DALineSeparator +
          '  INFORMATION_SCHEMA.KEY_COLUMN_USAGE A INNER JOIN INFORMATION_SCHEMA.COLUMNS B' + DALineSeparator +
          '  ON A.TABLE_NAME = B.TABLE_NAME and A.COLUMN_NAME = B.COLUMN_NAME' + DALineSeparator +
          '  LEFT JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS C' + DALineSeparator +
          '  ON A.CONSTRAINT_NAME = C.CONSTRAINT_NAME' + DALineSeparator +
          'WHERE' + DALineSeparator +
          '  C.CONSTRAINT_NAME IS NULL AND A.TABLE_NAME = %s', [AnsiQuotedStr(TableInfo.TableName, '''')]));
        ColumnsRecordSet.Open;
        ColumnsRecordSet.AllocRecBuf(RecBuf);
        while True do begin
          ColumnsRecordSet.GetNextRecord(RecBuf);
          if ColumnsRecordSet.Eof then
            Break;
          if not ColumnsRecordSet.GetNull(ColumnsRecordSet.FieldByName('COLUMN_NAME'), RecBuf) then begin
            ColumnsRecordSet.GetFieldAsVariant(ColumnsRecordSet.FieldByName('COLUMN_NAME'), RecBuf, Value);
            Field := TMSFieldDesc(FFields.FindField(string(Value)));
            if Field <> nil then begin
              Field.IsKey := True;
              Field.IsAutoIncrement := not ColumnsRecordSet.GetNull(ColumnsRecordSet.FieldByName('AUTOINC_INCREMENT'), RecBuf);
              if not (FCursorType in ServerCursorTypes) then
                Field.ReadOnly := Field.IsTimestamp or Field.IsAutoIncrement;
            end;
          end;
        end;
      finally
        if RecBuf <> nil then
          ColumnsRecordSet.FreeRecBuf(RecBuf);
        ColumnsRecordSet.Free;
        FPopulatingKeyInfo := False;
      end;
    end;
  end;

  FillTablesAliases;
end;
{$ENDIF}

procedure TOLEDBRecordSet.CreateFieldDescs;

  procedure InternalCreateFieldDescs(IUnk: IUnknown);
  begin
    FTablesInfo.BeginUpdate;
    try
    {$IFNDEF LITE}
      if FNativeRowset and FRequestSQLObjects then
        CreateFieldDescsByRowset(IUnk)
      else
    {$ENDIF}
        CreateFieldDescsByInfo(IUnk);
    finally
      FTablesInfo.EndUpdate;
    end;
  end;

begin
  if not FNativeRowset then
    FCommand.CommandType := ctCursor;

  if FCommand.CommandType = ctUnknown then begin // This is a FieldDefs.Update call
    QueryCommandInterfaces(False);
    try
      SetCommandProp;
      FCommand.Execute;
    finally
      ReleaseCommandInterfaces; // FCommand.QueryIntCnt counter is increased in inherited
      FCommand.SetCursorState(csInactive); // copied from OpenCursor method to prevent
                                           // blocking execute after fielddefs.update
    end;
    InternalCreateFieldDescs(FCommand.FIUnknown);

    // Free interfaces
    FCommand.ClearIMultipleResults;
    FCommand.FIUnknown := nil;

    // Free param accessors (must be after clearing interfaces)
    RequestParamsIfPossible;

    // We does not need to process non-Native rowsets or ServerCursors
    // QueryRecordSetInterfaces not required too
    Exit;
  end;

  if not FNativeRowset then
    InternalCreateFieldDescs(FIRowset)
  else begin
    Assert(FIRowset = nil);
    if not FCommand.FOpenNext and Prepared and not FCommand.FRPCCall then
      InternalCreateFieldDescs(FCommand.FICommandPrepare)
    else
      InternalCreateFieldDescs(FCommand.FIUnknown);
  end;

  if FFields.Count = 0 then
    DatabaseError(SNotRows, nil);
end;

class function TOLEDBRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  Result := inherited GetBufferSize(DataType, LengthInChars);
  if HasValueLen(DataType) then
    if not (DataType in [dtExtString, dtExtWideString, dtExtVarBytes, dtGuid]) then
      Result := Result + SizeOf(DBLENGTH);
end;

function TOLEDBRecordSet.IsNeedFetchBlock(FieldDesc: TFieldDesc): boolean; // Return True if field need to fetch into separate buffer
begin
  Assert(FieldDesc.FieldDescKind = fdkData);
  case FieldDesc.DataType of
    dtExtString, dtExtWideString, dtExtVarBytes, dtVariant:
      Result := True;
    dtBlob, dtMemo, dtWideMemo, dtXML:
      Result := not IsLargeDataTypeUsed(FieldDesc);
    dtFloat, dtBcd:
      Result := FieldDesc.SubDataType = sdtNumeric;
    else
      Result := False;
  end;
end;

procedure TOLEDBRecordSet.IncFetchBlockOffset(var FetchBlockOffset: integer; Field: TFieldDesc);
begin
  case Field.DataType of
    dtExtString, dtExtWideString:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + 2 {#0#0 terminator} + sizeof(DBLENGTH) {Length});
    dtMemo, dtWideMemo:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + 2 {#0#0 terminator} + sizeof(DBLENGTH) {Length});
    dtBlob:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + Field.Length + sizeof(DBLENGTH) {Length});
    dtExtVarBytes:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + sizeof(DBLENGTH) {Length});
    dtVariant:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + SizeOf_OleVariant);
    dtFloat, dtBcd:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + SizeOf_TDBNumeric);
    else
      Assert(False);
  end;
end;

procedure TOLEDBRecordSet.AllocFetchBuffer;
var
  UsedIRowsetUpdate: boolean;

  function AddFieldToABList(FieldDesc: TFieldDesc): integer; // Add new AB, if need. If AB already present then return its index
    function AddAB(const BlockType: TAccessorBlockType): integer;
    begin
      Result := Length(FFetchAccessorData.AccessorBlocks);
      SetLength(FFetchAccessorData.AccessorBlocks, Result + 1);

      FFetchAccessorData.AccessorBlocks[Result].BlockType := BlockType;
      FFetchAccessorData.AccessorBlocks[Result].hAcc := 0;
      FFetchAccessorData.AccessorBlocks[Result].BlobField := nil;
    end;

  var
    BlockType: TAccessorBlockType;
    i: integer;
    IsLarge: boolean;
  begin
    Result := -1;
    try
      IsLarge := IsLargeDataTypeUsed(FieldDesc);

      if IsNeedFetchBlock(FieldDesc) then
        BlockType := abFetchBlock // This is a string by ref - long string conversion used
      else
        if FieldDesc.ReadOnly and UsedIRowsetUpdate then
          BlockType := abReadOnly
        else
          if IsLarge and UsedIRowsetUpdate then
            BlockType := abBLOB
          else
            BlockType := abOrdinary;

      if not UsedIRowsetUpdate {// CR 4082} and (BlockType <> abBLOB) then
        for i := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
          if FFetchAccessorData.AccessorBlocks[i].BlockType = BlockType then begin
            // Test BLOB fields
            if IsLarge then begin
              if FFetchAccessorData.AccessorBlocks[i].BlobField = nil then
                FFetchAccessorData.AccessorBlocks[i].BlobField := FieldDesc
              else begin
                BlockType := abBLOB;
                Result := AddAB(BlockType);
                FFetchAccessorData.AccessorBlocks[Result].BlobField := FieldDesc;
                Exit;
              end;
            end;

            if (Provider = prCompact) and (FieldDesc.SubDataType = sdtNumeric) then
              Result := AddAB(BlockType)
            else
              Result := i;

            Exit;
          end;

      // Accessor block not found! Create new
      Result := AddAB(BlockType);
      if IsLarge then
        FFetchAccessorData.AccessorBlocks[Result].BlobField := FieldDesc;
    finally
      Assert(Result <> - 1);
      i := Length(FFetchAccessorData.AccessorBlocks[Result].Fields);
      SetLength(FFetchAccessorData.AccessorBlocks[Result].Fields, i + 1);
      FFetchAccessorData.AccessorBlocks[Result].Fields[i] := FieldDesc;
    end;
  end;

  // Fill internal structures in accessor block
  procedure FillBindingStructInAccBlock(
    rgBindings: TDBBindingArray;
    var AccessorBlock: TAccessorBlock);
  var
    i: integer;
    Field: TFieldDesc;
    Obj: {$IFNDEF UNIDACPRO}OLEDBIntf{$ELSE}OLEDBIntfUni{$ENDIF}.DBOBJECT;
  begin
    for i := 0 to Length(AccessorBlock.Fields) - 1 do begin
      Field := AccessorBlock.Fields[i];

      rgBindings[i].iOrdinal := Field.ActualFieldNo;
      rgBindings[i].obValue := Field.DataOffset;
      if FBookmarkField = Field then
        rgBindings[i].obStatus := DataSize + (FFields.Count - 1) * OLE_DB_INDICATOR_SIZE
      else
        rgBindings[i].obStatus := DataSize + (Field.FieldNo - 1) * OLE_DB_INDICATOR_SIZE;
      rgBindings[i].obLength := 0;

      rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS;
      rgBindings[i].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
      rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
      rgBindings[i].cbMaxLen := Field.Size;

      if (FCursorType in [ctStatic, ctKeyset, ctBaseTable]) and (rgBindings[i].iOrdinal = 0) then
        rgBindings[i].dwFlags := DBCOLUMNFLAGS_ISBOOKMARK
      else
        rgBindings[i].dwFlags := 0;

      rgBindings[i].wType := ConvertInternalTypeToOLEDB(Field.DataType, Field.SubDataType, Provider);

      if not IsLargeDataTypeUsed(Field) then begin
        case Field.DataType of
          dtString, dtWideString: begin
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            rgBindings[i].obValue := Field.DataOffset;
            if Field.DataType = dtWideString then
              rgBindings[i].cbMaxLen := (Field.Length + 1) * SizeOf(WideChar)
            else
              rgBindings[i].cbMaxLen := Field.Length + 1;
            rgBindings[i].obLength := rgBindings[i].obValue + rgBindings[i].cbMaxLen;
          end;
          dtExtString, dtExtWideString: begin
            Assert(AccessorBlock.BlockType = abFetchBlock);
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            if Field.DataType = dtExtWideString then
              rgBindings[i].cbMaxLen := MaxNonBlobFieldLen + SizeOf(WideChar)
            else
              rgBindings[i].cbMaxLen := MaxNonBlobFieldLen + 1;
            rgBindings[i].obStatus := FFetchBufferSize;
            rgBindings[i].obLength := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
            rgBindings[i].obValue := rgBindings[i].obLength + sizeof(DBLENGTH);
            IncFetchBlockOffset(FFetchBufferSize, Field);
          end;
          dtMemo, dtWideMemo: begin // Long string conversion used
            Assert(AccessorBlock.BlockType = abFetchBlock);
            if Field.DataType = dtWideMemo then begin
              rgBindings[i].wType := DBTYPE_WSTR;
              rgBindings[i].cbMaxLen := MaxNonBlobFieldLen + SizeOf(WideChar);
            end
            else begin
              rgBindings[i].wType := DBTYPE_STR;
              rgBindings[i].cbMaxLen := MaxNonBlobFieldLen + 1;
            end;
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            rgBindings[i].obStatus := FFetchBufferSize;
            rgBindings[i].obValue := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
            rgBindings[i].obLength := rgBindings[i].obValue + rgBindings[i].cbMaxLen;
            IncFetchBlockOffset(FFetchBufferSize, Field);
          end;
          dtVariant: begin
            Assert(AccessorBlock.BlockType = abFetchBlock);
            rgBindings[i].wType := DBTYPE_VARIANT;
            rgBindings[i].cbMaxLen := SizeOf_OleVariant;
            rgBindings[i].obStatus := FFetchBufferSize;
            rgBindings[i].obValue := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
            IncFetchBlockOffset(FFetchBufferSize, Field);
          end;
          dtBytes, dtVarBytes: begin
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            rgBindings[i].obValue := Field.DataOffset;
            rgBindings[i].obLength := rgBindings[i].obValue + Field.Length;
            rgBindings[i].cbMaxLen := Field.Length;
          end;
          dtExtVarBytes: begin
            Assert(AccessorBlock.BlockType = abFetchBlock);
            rgBindings[i].cbMaxLen := MaxNonBlobFieldLen; // WAR on changing must change FetchExternalAccessorBlock
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            rgBindings[i].obStatus := FFetchBufferSize;
            rgBindings[i].obValue := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
            rgBindings[i].obLength := rgBindings[i].obValue + rgBindings[i].cbMaxLen;
            IncFetchBlockOffset(FFetchBufferSize, Field);
          end;
          dtBlob: begin
            Assert(AccessorBlock.BlockType = abFetchBlock);
            rgBindings[i].wType := DBTYPE_BYTES;
            rgBindings[i].cbMaxLen := Field.Length;
            rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            rgBindings[i].obStatus := FFetchBufferSize;
            rgBindings[i].obValue := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
            rgBindings[i].obLength := rgBindings[i].obValue + rgBindings[i].cbMaxLen;
            IncFetchBlockOffset(FFetchBufferSize, Field);
          end;
          dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
            if UseExtTimeStamp and (Provider <> prCompact) then
              rgBindings[i].wType := DBTYPE_DBTIMESTAMP;
          end;
          dtFloat, dtBcd:
            if Field.SubDataType = sdtNumeric then begin
              Assert(AccessorBlock.BlockType = abFetchBlock);
              rgBindings[i].wType := DBTYPE_NUMERIC;
              rgBindings[i].cbMaxLen := SizeOf_TDBNumeric;
              rgBindings[i].obStatus := FFetchBufferSize;
              rgBindings[i].obValue := rgBindings[i].obStatus + OLE_DB_INDICATOR_SIZE;
              IncFetchBlockOffset(FFetchBufferSize, Field);
            end;
        end;
      end
      else begin
        Assert(Field.DataType in [dtMemo, dtWideMemo, dtBlob, dtXML], 'Non-compartible values of Field.DataType and IsStreamUsed');

        Obj.iid := IID_ISequentialStream;
        Obj.dwFlags := STGM_READ;
        rgBindings[i].pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
        DBOBJECT(rgBindings[i].pObject^) := Obj;

        if not FReadOnly then begin
          rgBindings[i].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
          rgBindings[i].obLength := rgBindings[i].obValue + sizeof(IntPtr);
        end;
      end;

      if rgBindings[i].wType = DBTYPE_NUMERIC then begin
        rgBindings[i].bPrecision := Field.Length;
        rgBindings[i].bScale := Field.Scale;
      end;
    end;
  end;

var
  rgStatus: PDBBINDSTATUSArray;
  rgBindings: TDBBindingArray;
  hr: HResult;
  FieldCntAB: integer;
  Field: TFieldDesc;
  i, j: integer;
begin
  if FFetchAccessorData.Accessor <> nil then
    Exit;

  FFetchBuffer := nil;
  FFetchBufferSize := 0;
  UsedIRowsetUpdate := (FIRowsetUpdate <> nil) and
    (FCursorType <> TMSCursorType(ctTableType)); // for TableTypeRecordSet
  QueryIntf(FIRowset, IID_IAccessor, FFetchAccessorData.Accessor);

  // Separate fields to AccessorBlocks
  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    if (Field.FieldDescKind = fdkData) and (Field.ActualFieldNo > -1) {KeyOnly SmartFetchState} and
       ((FCursorType <> TMSCursorType(ctTableType)) or not Field.ReadOnly) then // don't set readonly fields for TVP
      AddFieldToABList(Field);
  end;

  // Create Accessors
  for i := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do begin
    FieldCntAB := Length(FFetchAccessorData.AccessorBlocks[i].Fields);
    rgStatus := Marshal.AllocHGlobal(FieldCntAB * SizeOf(DBBINDSTATUS));
    try
      SetLength(rgBindings, FieldCntAB);

      for j := 0 to FieldCntAB - 1 do begin
        rgBindings[j].pTypeInfo := nil;
        rgBindings[j].pObject := nil;
        rgBindings[j].pBindExt := nil;
      end;

      FillBindingStructInAccBlock(rgBindings, FFetchAccessorData.AccessorBlocks[i]);

      // Create accessor
      hr := FFetchAccessorData.Accessor.CreateAccessor(DBACCESSOR_ROWDATA, FieldCntAB, rgBindings, 0, FFetchAccessorData.AccessorBlocks[i].hAcc, rgStatus);
      Check(hr);
    finally
      if Length(rgBindings) <> 0 then begin
        for j := 0 to FieldCntAB - 1 do
          if rgBindings[j].pObject <> nil then
            Marshal.FreeHGlobal(rgBindings[j].pObject);
        SetLength(rgBindings, 0);
      end;
      if rgStatus <> nil then
        Marshal.FreeHGlobal(rgStatus);
    end;
  end;

  if FFetchBufferSize <> 0 then
    FFetchBuffer := Marshal.AllocHGlobal(FFetchBufferSize);
end;

procedure TOLEDBRecordSet.FreeFetchBuffer;
var
  AccNum: integer;
begin
  if FFetchAccessorData.Accessor = nil then
    Exit;

  for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
    Check(FFetchAccessorData.Accessor.ReleaseAccessor(FFetchAccessorData.AccessorBlocks[AccNum].hAcc, nil));
  FFetchAccessorData.Accessor := nil;
  SetLength(FFetchAccessorData.AccessorBlocks, 0);

  inherited;
end;

procedure TOLEDBRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  case CursorTypeForFetching of
    ctDefaultResultSet, ctBaseTable:
      inherited;
    ctStatic, ctKeySet: begin
      LastItem.Order := Marshal.ReadInt32(IntPtr(Block), sizeof(TBlockHeader) + sizeof(TItemHeader) + FBookmarkField.DataOffset);
      FBookmarkValue := LastItem.Order;
    end;
    ctDynamic:
      LastItem.Order := 0;
  end;
end;

procedure TOLEDBRecordSet.ProcessNoResult(FetchBack: boolean);
begin
  case CursorTypeForFetching of
    ctDefaultResultSet, ctBaseTable: begin
      if FLastFetchEnd then begin
        ClearHRow;
        FreeFetchBuffer;
        FCommand.FIUnknown := nil;
        ReleaseRecordSetInterfaces;
        FLastFetchEnd := False;
      end;
    end;
  end;
end;

function TOLEDBRecordSet.FetchingAccessible(FetchBack: boolean): boolean;
var
  Field: TMSFieldDesc;
  hr: HResult;
  AccNum: integer;
  pHBlock: PBlockHeader;
  pRec: IntPtr;
  i: integer;
begin
  if ((FCommand.FIUnknown <> nil) or not FNativeRowset) and (Length(FFetchAccessorData.AccessorBlocks) = 0) then begin
    // This is a first call to Fetch. FIUnknown tested for prevent recreating accessors after fetching all strings
    // Query interfaces, create accessors etc
    QueryRecordSetInterfaces;

    AllocFetchBuffer;
    BlockMan.DefaultItemCount := FFetchRows;

    FLastFetchBack := False;
    FLastFetchEnd := False;
    FLastFetchOK := True;
    FHRowAccessible := False;

    if FCursorType in ServerCursorTypes then
      FNoCountData := True;

    if CursorTypeForFetching = ctDynamic then begin
      FRecordCount := -1;
      FRowsFetched := -1;
    end
    else
    if FCursorType in [ctStatic, ctKeySet] then begin
      // Setting FRecordCount for ctStatic, ctKeySet
      FFetchFromBookmark := True;
      FBookmarkValue := DBBMK_LAST;
      FBookmarkSize := sizeof(byte);
      if ReadNextRows(1, False) > 0 then begin
        BlockMan.AllocBlock(pHBlock, 1);
        try
          pRec := PtrOffset(BlockMan.FirstBlock, SizeOf(TBlockHeader) + SizeOf(TItemHeader));
          for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
            if FFetchAccessorData.AccessorBlocks[AccNum].Fields[0] = FBookmarkField then begin
              hr := FIRowset.GetData(FRowArray[0], FFetchAccessorData.AccessorBlocks[AccNum].hAcc, pRec);
              Check(hr, AnalyzeFieldsStatus, pRec);
              FRecordCount := Marshal.ReadInt32(pRec, FBookmarkField.DataOffset);
              FRowsFetched := FRecordCount;
              Break;
            end;
        finally
          BlockMan.FreeBlock(pHBlock);
        end;

        FFetchFromBookmark := True;
        FBookmarkValue := DBBMK_FIRST;
        ReadNextRows(1, False);
      end;

      FFetchFromBookmark := True; // First record reading without offsetting
      FBookmarkValue := Integer(DBBMK_FIRST);
      FBookmarkSize := sizeof(FBookmarkValue);
    end;

    // Clear MaxTimestamp for RefreshQuick
    for i := 0 to FFields.Count - 1 do begin
      Field := TMSFieldDesc(FFields[i]);
      if Field.IsTimestamp and (Field.TableInfo <> nil) then
        TSqlTableInfo(Field.TableInfo).MaxTimestamp := 0;
    end;
  end;

  if (FIRowset = nil) or ((FCursorType = ctKeySet) and not FCursorUpdate and (FBookmarkValue = - 1)) then
    Result := False
  else
    Result := ReadNextRows(FFetchRows, FetchBack) > 0;
end;

function TOLEDBRecordSet.ReadNextRows(FetchRows: integer; FetchBack: boolean): NativeUInt;
var
  RowsOffset, RowsRequested: NativeInt;
  hr: HResult;
begin
  try
    if not FLastFetchEnd then begin
      ClearHRow; // Clear previous obtained rows
      SetLength(FRowArray, FetchRows);
      FPRowArray := @FRowArray[0];

      // Backward fetch processing
      if FetchBack then
        RowsRequested := - FetchRows
      else
        RowsRequested := FetchRows;

      RowsOffset := 0;

      // Get data from OLEDB
      if FCursorType in [ctKeyset, ctStatic] then begin
        if not FFetchFromBookmark then
          if FetchBack then
            RowsOffset := - 1
          else
            RowsOffset := + 1;

        Assert(FIRowsetLocate <> nil);
        // FIRowsetLocate.GetRowsAt does not change current IRowset fetch position
        hr := FIRowsetLocate.GetRowsAt(0, DB_NULL_HCHAPTER, FBookmarkSize, @FBookmarkValue, RowsOffset, RowsRequested, FRowsObtained, FPRowArray);
      end
      else begin
        if CursorTypeForFetching = ctDynamic then begin
          if FLastFetchOK then begin
            if FFetchFromBookmark then begin // Reread previous readed row
              if FLastFetchBack = FetchBack then begin
                if FetchBack then
                  RowsOffset := + 1
                else
                  RowsOffset := - 1;
              end;
            end
            else
              if FLastFetchBack <> FetchBack then
                if FetchBack then
                  RowsOffset := - 1
                else
                  RowsOffset := + 1;
          end
          else
            if FLastFetchBack = FetchBack then
              if FetchBack then
                RowsOffset := + 1
              else
                RowsOffset := - 1;
        end;

        hr := FIRowset.GetNextRows(DB_NULL_HCHAPTER, RowsOffset, RowsRequested, FRowsObtained, FPRowArray);
      end;

      FFetchFromBookmark := False; // Clear flag, setted on InternalOpen
      FLastFetchBack := FetchBack;
      if FRowsObtained > 0 then
        FHRowAccessible := True;

      FLastFetchEnd := ((FNativeRowset and (FCursorType = ctDefaultResultSet)) or
        (CursorTypeForFetching = ctBaseTable)) and (hr = DB_S_ENDOFROWSET); // CR10007

      // Process rows
      if (hr <> DB_S_ENDOFROWSET)
        and (hr <> DB_S_ROWLIMITEXCEEDED)
        and not ((hr = DB_E_BADBOOKMARK) and (FBookmarkValue = DBBMK_FIRST))
        and not ((hr = DB_E_BADSTARTPOSITION) and (ClientMajorVer = 7)) then
        Check(hr);
    end
    else
      FRowsObtained := 0;

    FLastFetchOK := FRowsObtained > 0;
    Result := FRowsObtained;

    if FRowsObtained = 0 then
      case CursorTypeForFetching of
        ctDynamic: begin
          if FetchBack then begin
            FBof := True;
            // Server cursor position is under first row, FHRow is not accessible
            // Need to call GetNextRows with params (RowsOffset = 1, RowsRequested = - 1)
            RowsOffset := 1;
            RowsRequested := -1;
          end
          else  begin
            FEof := True;
            // Server cursor position is below last row, FHRow is not accessible
            // Need to call GetNextRows with params (RowsOffset = - 1, RowsRequested = 1)
            RowsOffset := -1;
            RowsRequested := 1;
          end;

          FIRowset.GetNextRows(DB_NULL_HCHAPTER, RowsOffset, RowsRequested, FRowsObtained, FPRowArray);

          if FRowsObtained > 0 then begin
            if FirstItem = nil then
              Result := FRowsObtained;

            FHRowAccessible := True;
          end
          else begin
            CurrentItem := nil; // FHRow is not accessible and we need to refetch data from server
            FBof := True;
            FEof := True;
            FLastFetchBack := False;
          end;

          FLastFetchOK := True;
        end;
        else
          ProcessNoResult(FetchBack);
      end;
  except
    on E: Exception do begin
      ProcessFetchedException(E);
      raise;
    end;
  end;
end;

procedure TOLEDBRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  pRec: IntPtr;
  Row: integer;
  FetchBlockOffset: integer;

  procedure PrepareConvertableFields; // After get data from OLEDB
  var
    i: integer;
    pValue: IntPtr;
    ValueLen: Cardinal;
    Field: TMSFieldDesc;

    FieldLength, FieldScale: word;
    t: Boolean;
    Str: String;
    Bcd: TBcd;
    DBTimeStamp: TDBTimeStamp;
    dt: TDateTime;
    d: double;
    CurrTimestamp, FieldMaxTimestamp: Int64;
    TimeStamp: TTimeStamp;
  begin
    for i := 0 to FFields.Count - 1 do begin
      Field := TMSFieldDesc(FFields[i]);
      if (Field.FieldDescKind = fdkData) and (Field.ActualFieldNo > -1) then
        if not GetNull(Field, pRec) then begin
          pValue := PtrOffset(pRec, Field.DataOffset);

          // Get max Timestamp value for RefreshQuick
          if Field.IsTimestamp and (Field.TableInfo <> nil) then begin
            CurrTimestamp := Reverse8(PInt64(pValue)^);
            FieldMaxTimestamp := TSqlTableInfo(Field.TableInfo).MaxTimestamp;
            if {$IFDEF VER7P}UInt64{$ENDIF}(FieldMaxTimestamp) < {$IFDEF VER7P}UInt64{$ENDIF}(CurrTimestamp) then
              TSqlTableInfo(Field.TableInfo).MaxTimestamp := CurrTimestamp;
          end;

          case Field.DataType of
            dtString: begin
              ValueLen := Marshal.ReadUInt32(pValue, Field.Length + 1);
              if Field.Fixed then
                t := TrimFixedChar
              else
                t := TrimVarChar;
              if t then
                ValueLen := StrTrim(pValue, ValueLen);
              Marshal.WriteUInt16(pRec, Field.Offset, Word(ValueLen));
            end;
            dtWideString: begin
              ValueLen := Marshal.ReadUInt32(pValue, (Field.Length  + 1) * SizeOf(WideChar)) shr 1;
              if Field.Fixed then
                t := TrimFixedChar
              else
                t := TrimVarChar;
              if t then
                ValueLen := StrTrimW(pValue, ValueLen);
              Marshal.WriteUInt16(pRec, Field.Offset, Word(ValueLen));
            end;
            dtBytes, dtVarBytes: begin
              ValueLen := Marshal.ReadUInt32(pValue, Field.Length);
              Marshal.WriteUInt16(pRec, Field.Offset, Word(ValueLen));
            end;
            dtGuid: begin
              Str := ConvertGuidToString(PGUID(pValue)^, UidWithBraces);
              ValueLen := Length(Str);
              CopyBuffer(PAChar(AnsiString(Str)), pValue, ValueLen);
              Marshal.WriteByte(pValue, ValueLen, 0);
              Marshal.WriteUInt16(pRec, Field.Offset, Word(ValueLen));
            end;
          {$IFDEF FPC}
            dtInt8, dtUInt8:
              Marshal.WriteByte(pValue, 1, 0);
          {$ENDIF}
            dtFmtBCD: begin
              Assert(GetStatus(Field, pRec) = DBSTATUS_S_OK);
              Bcd := DBNumericToBCD(TDBNumeric(pValue^));

              FieldLength := Field.Length;
              FieldScale := Field.Scale;
              CRFunctions.NormalizeBcd(Bcd, PBcd(pValue)^, FieldLength, FieldScale);
            end;
            dtTime: begin
              dt := PDateTime(pValue)^;
              TimeStamp := DateTimeToTimeStamp(dt);
              TimeStamp.Date := DateDelta;
              dt := TimeStampToDateTime(TimeStamp);
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(dt)));
            end;
            dtDateTime:
              if UseExtTimeStamp then begin
                DBTimeStamp := PDBTimeStamp(pValue)^;
                dt := EncodeDateTime(DBTimeStamp.year, DBTimeStamp.month, DBTimeStamp.day, DBTimeStamp.hour, DBTimeStamp.minute, DBTimeStamp.second, DBTimeStamp.fraction div 1000000{Billionths of a second to milliseconds});
                Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(dt)));
              end;
            dtSQLTimeStamp:
              PDBTimeStamp(pValue)^.fraction := PDBTimeStamp(pValue)^.fraction div 100;
            dtCurrency:
              if Provider = prCompact then begin
                d := Currency(pValue^);
                Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(d));
              end;
          end;
        end;
    end;
  end;

  procedure FetchPlainAccessorBlock(const AccessorBlock: TAccessorBlock);
  var
    hr: HResult;
    Blob: TBlob;
    Field: TFieldDesc;
    pValue: IntPtr;
    Length: integer;
  begin
    // Get data from IRowset
    hr := FIRowset.GetData(FRowArray[Row], AccessorBlock.hAcc, pRec);
    Check(hr, AnalyzeFieldsStatus, pRec);

    // ConvertMemoToBlob;
    if AccessorBlock.BlobField <> nil then begin
      Field := AccessorBlock.BlobField;
      pValue := PtrOffset(pRec, Field.DataOffset);
      if FReadOnly then
        Length := -1
      else
        Length := Marshal.ReadInt32(pRec, Field.DataOffset + SizeOf(IntPtr));

      Blob := CreateBlob(Field);
      try
        if GetStatus(Field, pRec) <> DBSTATUS_S_ISNULL then // Can't use GetNull->GetNullByBlob
          FCommand.ConvertStreamToBlob(pValue, Length, Blob, (Field.SubDataType and sdtWide) <> 0,
            {$IFDEF LITE}((not sdtWide) and Field.SubDataType = dtXML){$ELSE}(Field.DataType = dtXML){$ENDIF});
      finally
        Marshal.WriteIntPtr(pValue, Blob.GCHandle);
      end;
    end;
  end;

  procedure FetchExternalAccessorBlock(const AccessorBlock: TAccessorBlock);
  var
    hr: HResult;
    i: integer;
    Status: DWORD;
    Obj: TSharedObject;
    Blob: TBlob;
    pValue: IntPtr;
    pFetchBlockValue: IntPtr;

    Field: TFieldDesc;
    Size: Integer;
    HeapBuf: IntPtr;
    t: boolean;
    d: double;
    c: currency;
    i64: Int64;
  begin
    Assert(AccessorBlock.BlobField = nil);
    // Get data from IRowset
    hr := FIRowset.GetData(FRowArray[Row], AccessorBlock.hAcc, FFetchBuffer);

    // Copy status from external buf to pRec. Need to correct work CheckAndAnalyzeFieldsStatus
    for i := 0 to Length(AccessorBlock.Fields) - 1 do begin
      Field := AccessorBlock.Fields[i];
      if (Field.FieldDescKind = fdkData) and IsNeedFetchBlock(Field) then begin
        Status := DWORD(Marshal.ReadInt32(FFetchBuffer, FetchBlockOffset));
        SetStatus(Field, pRec, Status);

        pFetchBlockValue := PtrOffset(FFetchBuffer, FetchBlockOffset + OLE_DB_INDICATOR_SIZE);
        pValue := PtrOffset(pRec, Field.DataOffset);

        case Field.DataType of
          dtExtString, dtExtWideString: begin
            if GetNull(Field, pRec) then
              Marshal.WriteIntPtr(pValue, nil)
            else begin
              if Field.Fixed then
                t := TrimFixedChar
              else
                t := TrimVarChar;
              Size := Marshal.ReadInt32(pFetchBlockValue);
              pFetchBlockValue := PtrOffset(pFetchBlockValue, sizeof(DBLENGTH));
              if Field.DataType = dtExtString then
                if t then
                  Marshal.WriteIntPtr(pValue, FStringHeap.AllocTrimmedStr(pFetchBlockValue, Size))
                else
                  Marshal.WriteIntPtr(pValue, FStringHeap.AllocStr(pFetchBlockValue, Size))
              else begin
                Size := Size shr 1;
                if t then
                  Marshal.WriteIntPtr(pValue, FStringHeap.AllocTrimmedWideStr(pFetchBlockValue, Size))
                else
                  Marshal.WriteIntPtr(pValue, FStringHeap.AllocWideStr(pFetchBlockValue, Size));
              end;
              Marshal.WriteUInt16(pRec, Field.Offset, Word(Size));
            end;
          end;
          dtExtVarBytes:
            if GetNull(Field, pRec) then
              Marshal.WriteIntPtr(pValue, nil)
            else begin
              Size := Marshal.ReadInt32(pFetchBlockValue, MaxNonBlobFieldLen);
              HeapBuf := FStringHeap.NewBuf(Size);
              CopyBuffer(pFetchBlockValue, HeapBuf, Size);
              Marshal.WriteIntPtr(pValue, HeapBuf);
              Marshal.WriteUInt16(pRec, Field.Offset, Word(Size));
            end;
          dtMemo, dtWideMemo: begin
            Blob := TBlob.Create(Field.DataType = dtWideMemo);
            if Status <> DBSTATUS_S_ISNULL then begin
              if Field.DataType = dtWideMemo then
                Size := Marshal.ReadInt32(pFetchBlockValue, MaxNonBlobFieldLen + SizeOf(WideChar))
              else
                Size := Marshal.ReadInt32(pFetchBlockValue, MaxNonBlobFieldLen + 1);
              if Size > 0 then begin
                Blob.RollbackEnabled := False;
                Blob.Write(0, Size, pFetchBlockValue);
                Blob.RollbackEnabled := True;
              end
              else
                Blob.Write(0, 0, nil); // Set IsNull to False
            end;
            Marshal.WriteIntPtr(pValue, Blob.GCHandle);
          end;
          dtBlob: begin
            Assert(Field.SubDataType = sdtMSUDT);
            Blob := TBlob.Create;
            if Status <> DBSTATUS_S_ISNULL then begin
              Size := Marshal.ReadInt32(pFetchBlockValue, Field.Length);
              if Size > 0 then begin
                Blob.RollbackEnabled := False;
                Blob.Write(0, Size, pFetchBlockValue);
                Blob.RollbackEnabled := True;
              end
              else
                Blob.Write(0, 0, nil); // Set IsNull to False
            end;
            Marshal.WriteIntPtr(pValue, Blob.GCHandle);
          end;
          dtVariant: begin
            Obj := TVariantObject.Create;
            TVariantObject(Obj).Value := GetOleVariant(pFetchBlockValue);
            OleVarClear(pFetchBlockValue);
            Marshal.WriteIntPtr(pValue, Obj.GCHandle);
          end;
          dtFloat, dtBcd:
            if not GetNull(Field, pRec) then begin
              d := DBNumericToDouble(TDBNumeric(pFetchBlockValue^));

              if Field.DataType = dtFloat then
                Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(d))
              else begin
                c := d;
                i64 := Int64((@c)^);
                Marshal.WriteInt64(pValue, i64);
              end;
            end;
          else
            Assert(False);
        end;
        IncFetchBlockOffset(FetchBlockOffset, Field);
        Assert(FetchBlockOffset <= FFetchBufferSize);
      end;
    end;

    Check(hr, AnalyzeFieldsStatus, pRec);
  end;

var
  AccNum: integer;
begin
  RowsObtained := 0;
  pRec := PtrOffset(Block, SizeOf(TBlockHeader) + SizeOf(TItemHeader));

  while True do begin
    RowsObtained := RowsObtained + integer(FRowsObtained);

    for Row := 0 to FRowsObtained - 1 do begin
      FetchBlockOffset := 0;
      for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
        if FFetchAccessorData.AccessorBlocks[AccNum].BlockType = abFetchBlock then
          FetchExternalAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum])
        else
          FetchPlainAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum]);

      PrepareConvertableFields;

      pRec := PtrOffset(pRec, SizeOf(TItemHeader) + RecordSize);
    end;

    if (RowsObtained >= FFetchRows) or FLastFetchEnd then
      break;

    if ReadNextRows(FFetchRows - RowsObtained, FetchBack) = 0 then
      break;
  end;
end;

function TOLEDBRecordSet.ProcessFetchedException(E: Exception): boolean;
begin
  ReleaseAllInterfaces;
  FCommand.FNextResultRequested := False;
  Result := False;
end;

function TOLEDBRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCursorType:
      Value := Variant(FCursorType);
    prDisableMultipleResults:
      Value := FDisableMultipleResults;
    prUniqueRecords:
      Value := FUniqueRecords;
    prWideMemos:
      Value := FWideMemos;
    prWideStrings:
      Value := FWideStrings;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOLEDBRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBaseTableName:
      FBaseTableName := Value;
    prCursorType: begin
      FCursorType := TMSCursorType(Value);
      if FCursorType in ServerCursorTypes then begin
        FFetchAll := False;
        if (FIndexFieldNames <> '') and not CachedUpdates then // for TableTypeRecordSet
          DatabaseError(SLocalSortingServerCursor);
      end;
    end;
    prDisableMultipleResults:
      FDisableMultipleResults := Value;
    prHideSystemUniqueFields:
      FHideSystemUniqueFields := Value;
    prNotificationMessage:
      FCommand.SetProp(prNotificationMessage, Value);
    prNotificationService:
      FCommand.SetProp(prNotificationService, Value);
    prNotificationTimeout:
      FCommand.SetProp(prNotificationTimeout, Value);
    prUniqueRecords:
      FUniqueRecords := Value;
    prWideMemos:
      FWideMemos := Value;
    prWideStrings:
      FWideStrings := Value;

    prBulkExecuting:
      FBulkExecuting := Value;
    prRequestSQLObjects:
      FRequestSQLObjects := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOLEDBTableTypeRecordSet }

function TOLEDBTableTypeRecordSet.UseExtNumeric: boolean;
begin
  Result := False;
end;

function TOLEDBTableTypeRecordSet.UseExtTimeStamp: boolean;
begin
  Result := True;
end;

function TOLEDBTableTypeRecordSet.IsFullReopen: boolean;
begin
  Result := False;
end;

procedure TOLEDBTableTypeRecordSet.OpenTVPRowset;
var
  IUnk: IUnknown;
  FOpenRowset: IOpenRowset;
  TableID: TDBID;
  DatabaseName, SchemaName, TableTypeName: string;
  Connector: TOLEDBConnector;
begin
  Assert(FIRowset = nil);
  Connector := FCommand.GetConnector;
  Connector.Check(Connector.FIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
  FOpenRowset := IOpenRowset(IUnk);

  MSSQLInfo.SplitObjectName(FTableTypeName, DatabaseName, SchemaName, TableTypeName);
  if (DatabaseName <> '') or (SchemaName <> '') then
    TableTypeName := FTableTypeName
  else
    TableTypeName := DefaultSDACSchema + '.' + TableTypeName;

  TableID.eKind := DBKIND_GUID_NAME;
  TableID.uGuid.guid := CLSID_ROWSET_TVP;
  TableID.uName.pwszName := Marshal.AllocHGlobal(Length(TableTypeName) * SizeOf(WideChar) + SizeOf(WideChar));
  try
    CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(TableTypeName), 0, Length(TableTypeName)))),
      TableID.uName.pwszName, Length(TableTypeName) * SizeOf(WideChar) + SizeOf(WideChar));
    IUnk := nil;

    Connector.Check(FOpenRowset.OpenRowset(nil, @TableID, nil, IID_IRowset, 0, nil, IUnk), nil);
  finally
    Marshal.FreeHGlobal(TableID.uName.pwszName);
  end;

  if IUnk = nil then
    raise Exception.Create(STableTypeNotSupported);

  FIRowset := IRowset(IUnk);
  FNativeRowset := False;
  FFlatBuffers := True;
  FFetchAll := False;
  FFetchRows := 1;
  FCommand.FCursorState := csExecuted;
end;

procedure TOLEDBTableTypeRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  OpenTVPRowset;

  FCommand.CommandType := ctUnknown;
  FCursorType := ctDefaultResultSet;
  CachedUpdates := True;
  LocalUpdate := True;

  inherited;
end;

{$IFNDEF LITE}
procedure TOLEDBTableTypeRecordSet.GetFieldDescsByRowset(IUnk: IUnknown);
var
  ColumnsRowset: IColumnsRowset;
  iu: IUnknown;
  CMIRowset: IRowset;
  RecBuf: IntPtr;
  ColumnsMetaInfo: TOLEDBRecordSet;
  Field: TMSFieldDesc;
  i: integer;
begin
  if IUnk = nil then
    Exit; // This query does not return rowset

  QueryIntf(IUnk, IID_IColumnsRowset, ColumnsRowset);
  Check(ColumnsRowset.GetColumnsRowset(nil, 0, nil, IID_IRowset, 0, nil, iu)); {Default properties - default result set}
  CMIRowset := IRowset(iu);
  Assert(CMIRowset <> nil);
  RecBuf := nil;

  ColumnsMetaInfo := FCommand.GetConnector.CheckColumnsMetaInfo;
  ColumnsMetaInfo.SetIRowset(CMIRowset);
  ColumnsMetaInfo.Open;
  try
    ColumnsMetaInfo.CheckColumnsMetaInfoIdx;
    ColumnsMetaInfo.AllocRecBuf(RecBuf);

    for i := 0 to Fields.Count - 1 do begin
      ColumnsMetaInfo.GetNextRecord(RecBuf);
      if ColumnsMetaInfo.Eof then
        Break;

      Field := TMSFieldDesc(Fields[i]);
      if Field.DataType = dtXML then begin
        if ColumnsMetaInfo.FFldXMLSchemaCollCatalogNameIdx <> -1 then
          Field.XMLSchemaCollectionCatalogName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollCatalogNameIdx]);
        if ColumnsMetaInfo.FFldXMLSchemaCollSchemaNameIdx <> -1 then
          Field.XMLSchemaCollectionSchemaName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollSchemaNameIdx]);
        if ColumnsMetaInfo.FFldXMLSchemaCollNameIdx <> -1 then
          Field.XMLSchemaCollectionName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldXMLSchemaCollNameIdx]);
        Field.XMLTyped := (Field.XMLSchemaCollectionCatalogName <> '') or (Field.XMLSchemaCollectionSchemaName <> '') or
          (Field.XMLSchemaCollectionName <> '');
      end
      else
      if Field.SubDataType = sdtMSUDT then begin
        if ColumnsMetaInfo.FFldUDTSchemanameIdx <> -1 then
          Field.UDTSchemaname := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTSchemanameIdx]);
        if ColumnsMetaInfo.FFldUDTNameIdx <> -1 then
          Field.UDTName := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTNameIdx]);
        if ColumnsMetaInfo.FFldUDTCatalognameIdx <> -1 then
          Field.UDTCatalogname := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldUDTCatalognameIdx]);
        if ColumnsMetaInfo.FFldAssemblyTypenameIdx <> -1 then
          Field.AssemblyTypename := ColumnsMetaInfo.GetFieldStrValue(RecBuf, ColumnsMetaInfo.Fields[ColumnsMetaInfo.FFldAssemblyTypenameIdx]);
        if (Field.UDTName <> '') and (Field.AssemblyTypename <> '') then
          Field.UDTDispatcher := TUDTDispatcher.Create;
      end;
    end;
  finally
    if RecBuf <> nil then
      ColumnsMetaInfo.FreeRecBuf(RecBuf);

    ColumnsMetaInfo.AutoInitFields := False; // Save fields for reusing
    ColumnsMetaInfo.Close;

    if IsLibrary then
      FreeAndNil(FCommand.GetConnector.FColumnsMetaInfo);
  end;
end;
{$ENDIF}

procedure TOLEDBTableTypeRecordSet.CreateFieldDescs;
begin
  FCommand.CommandType := ctCursor;

  FTablesInfo.BeginUpdate;
  try
    CreateFieldDescsByInfo(FIRowset);
  {$IFNDEF LITE}
    GetFieldDescsByRowset(FIRowset);
  {$ENDIF}
  finally
    FTablesInfo.EndUpdate;
  end;

  if FFields.Count = 0 then
    DatabaseError(SNotRows, nil);
end;

procedure TOLEDBTableTypeRecordSet.ExplicitInitFields;
var
  NeedClose: boolean;
begin
  if FCommand.CommandType = ctUnknown then begin
    OpenTVPRowset;
    FCursorType := ctDefaultResultSet;
    NeedClose := True;
  end
  else
    NeedClose := False;

  try
    inherited;
  finally
    if NeedClose then begin
      ReleaseRecordSetInterfaces;
      FCommand.FCursorState := csInactive;
      FCommand.CommandType := ctUnknown;
    end;
  end;
end;

procedure TOLEDBTableTypeRecordSet.InternalPrepare;
begin
//
end;

function TOLEDBTableTypeRecordSet.FetchingAccessible(FetchBack: boolean): boolean;
begin
  Result := False;
  if FCursorType = ctDefaultResultSet then
    ReleaseRecordSetInterfaces;
end;

function TOLEDBTableTypeRecordSet.GetFilledRowset: IRowset;
var
  OldCurrentItem: PItemHeader;
  RecBuf: IntPtr;
begin
  try
    if FIRowset = nil then begin
      OpenTVPRowset;
      FCursorType := TMSCursorType(ctTableType);
      QueryIntf(FIRowset, IID_IRowsetChange, FIRowsetUpdate);
      AllocFetchBuffer;

      OldCurrentItem := CurrentItem;
      try
        AllocRecBuf(RecBuf);
        try
          SetToBegin;
          while True do begin
            GetNextRecord(RecBuf);
            if EOF then
              break;

            InternalAppendOrUpdate(RecBuf, True);
            ClearHRow;
          end;
        finally
          FreeRecBuf(RecBuf);
        end;
      finally
        CurrentItem := OldCurrentItem;
      end;
    end;

    Result := FIRowset;
  except
    ReleaseAllInterfaces;
    raise;
  end;
end;

function TOLEDBTableTypeRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prTableTypeName:
      FTableTypeName := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOLEDBTransaction }

destructor TOLEDBTransaction.Destroy;
begin
  ReleaseInterfaces;

  inherited;
end;

procedure TOLEDBTransaction.ReleaseInterfaces;
begin
  FITransactionLocal := nil;
end;

{$IFDEF SQL_TRANSACTION}
procedure TOLEDBTransaction.StartTransaction;
var
  Connection: TMSSQLConnection;
begin
  Connection := TMSSQLConnection(FConnections[0]);
  Connection.SetIsolationLevel(FIsolationLevel);
  Connection.ExecSQL('BEGIN TRAN');
end;

procedure TOLEDBTransaction.Commit;
var
  Connection: TMSSQLConnection;
begin
  Connection := TMSSQLConnection(FConnections[0]);
  Connection.ExecSQL('COMMIT');
end;

procedure TOLEDBTransaction.Rollback;
var
  Connection: TMSSQLConnection;
begin
  Connection := TMSSQLConnection(FConnections[0]);
  Connection.ExecSQL('ROLLBACK');
end;
{$ELSE}

procedure TOLEDBTransaction.Check(const Status: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
var
  Connection: TMSSQLConnection;
begin
  if Status <> S_OK then begin
    Assert(FConnections.Count > 0);
    Connection := TMSSQLConnection(FConnections[0]);
    TOLEDBConnector(Connection.GetConnector).Check(Status, Component, AnalyzeMethod, Arg);
  end;
end;

procedure TOLEDBTransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  FITransactionLocal := TOLEDBTransaction(Source).FITransactionLocal;
end;

procedure TOLEDBTransaction.StartTransaction;
var
  i: integer;
  Connection: TMSSQLConnection;
  ulTransactionLevel: UINT;
  pulTransactionLevel: IntPtr;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if FConnections.Count > 1 then
    raise Exception.Create(SMultiConnectionsInTransaction);

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  Connection := TMSSQLConnection(FConnections[0]);
  QueryIntf(TOLEDBConnector(Connection.GetConnector).FISessionProperties, IID_ITransactionLocal, FITransactionLocal);
  try
    if FITransactionLocal <> nil then begin
      pulTransactionLevel := AllocOrdinal(ulTransactionLevel);
      try
        Check(FITransactionLocal.StartTransaction(ConvertIsolationLevelToOLEDBIsoLevel(FIsolationLevel), 0, nil, pulTransactionLevel), Component);
      finally
        FreeOrdinal(pulTransactionLevel);
      end;
      FActive := True;
      FNativeTransaction := True;
    end;
  except
    ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBTransaction.Commit;
begin
  if (FITransactionLocal <> nil) and FNativeTransaction then
    Check(ITransaction(FITransactionLocal).Commit(False{WAR may be troubles with server cursors}, XACTTC_SYNC, 0), Component);
  FActive := False;
end;

procedure TOLEDBTransaction.Rollback;
begin
  if (FITransactionLocal <> nil) and FNativeTransaction then
    Check(ITransaction(FITransactionLocal).Abort(nil, False, False), Component);
  FActive := False;
end;

procedure TOLEDBTransaction.Savepoint(const Name: string);
var
  Connection: TMSSQLConnection;
  cmd: string;
begin
  Assert(FConnections.Count = 1);

  Connection := TMSSQLConnection(FConnections[0]);
  //  SELECT is necessary to execute a statement to implicitly start the transaction
  // because ITransactionLocal::BeginTransaction actually execute SET   ON in some cases
  if ((Connection.ClientMajorVer <= 8) or (Connection.ServerMajorVer <= 8))
    and not IsCompactEdition(Connection.ServerMajorVer) then
    cmd := 'IF (@@TRANCOUNT = 0) SELECT 1 FROM SYSOBJECTS WHERE 0=1; ';
  cmd := cmd + 'SAVE TRANSACTION ' + Name;
  Connection.ExecuteSQL(cmd);
end;

procedure TOLEDBTransaction.RollbackToSavepoint(const Name: string);
var
  Connection: TMSSQLConnection;
begin
  Assert(FConnections.Count = 1);

  Connection := TMSSQLConnection(FConnections[0]);
  Connection.ExecuteSQL('ROLLBACK TRANSACTION ' + Name);
end;

{$ENDIF}

{$IFNDEF LITE}
{ TOLEDBMetaData }

function TOLEDBMetaData.CreateRecordSet: TCRRecordSet;
begin
  FRecordSet := TOLEDBRecordSet.Create;
  Result := FRecordSet;
end;

function TOLEDBMetaData.RequestIRowset(const MetaDataType: TMSMetaDataType; Restrictions: TStrings): TData;

  function GetTableType(MetaDataType: TMSMetaDataType): string;
  begin
    case MetaDataType of
      _otAliases, _otAliasesInfo:
        Result := 'ALIAS';
      _otTables, _otTablesInfo:
        Result := 'TABLE';
      _otSynonyms, _otSynonymsInfo:
        Result := 'SYNONYM';
      _otSystemTables, _otSystemTablesInfo:
        Result := 'SYSTEM TABLE';
      _otViews, _otViewsInfo:
        Result := 'VIEW';
      _otGlobalTempTables, _otGlobalTempTablesInfo:
        Result := 'GLOBAL TEMPORARY';
      _otLocalTempTables, _otLocalTempTablesInfo:
        Result := 'LOCAL TEMPORARY';
      _otSystemViews, _otSystemViewsInfo:
        Result := 'SYSTEM VIEW';
      _otExternalTablesInfo:
        Result := 'EXTERNAL TABLE';
    else
      Assert(False);
    end;
  end;

var
  Schema: TGUID;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  DatabaseName, SchemaName: string;
  AssemblyID, ReferencedAssemblyID: integer;
begin
  DatabaseName := Trim(Restrictions.Values['TABLE_CATALOG']);
  SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);

  case MetaDataType of
    _otDatabases: begin
      SetLength(rgRestrictions, 1);
      Schema := DBSCHEMA_CATALOGS;
      rgRestrictions[0] := DatabaseName;
    end;
    _otColumnPrivileges: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_COLUMN_PRIVILEGES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := Restrictions.Values['COLUMN_NAME'];
    end;
    _otColumns: begin
      SetLength(rgRestrictions, 4);
      Schema := DBSCHEMA_COLUMNS;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := Restrictions.Values['COLUMN_NAME'];
    end;
    _otForeignKeys: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_FOREIGN_KEYS;
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := Restrictions.Values['TABLE_NAME'];
    end;
    _otPrimaryKeys: begin
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        SetLength(rgRestrictions, 3);
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
        rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
        Schema := DBSCHEMA_PRIMARY_KEYS;
      end
      else begin
        SetLength(rgRestrictions, 7);
        rgRestrictions[2] := Restrictions.Values['CONSTRAINT_NAME'];
        rgRestrictions[5] := Restrictions.Values['TABLE_NAME'];
        // Other Restriction columns not supported
        Schema := DBSCHEMA_KEY_COLUMN_USAGE;
      end;
    end;
    _otIndexes, _otIndexColumns: begin
      SetLength(rgRestrictions, 5);
      Schema := DBSCHEMA_INDEXES;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['INDEX_NAME'];
      rgRestrictions[4] := Restrictions.Values['TABLE_NAME'];
    end;
    _otServerTypes: begin
      SetLength(rgRestrictions, 0);
      Schema := DBSCHEMA_PROVIDER_TYPES;
    end;
    _otSchemata: begin
      SetLength(rgRestrictions, 2);
      Schema := DBSCHEMA_SCHEMATA;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
    end;
    _otStatistics: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_STATISTICS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
    end;
    _otStoredProcs: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_PROCEDURES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['PROCEDURE_NAME'];
    end;
    _otStoredProcParams: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_PROCEDURE_PARAMETERS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['PROCEDURE_NAME'];
    end;
    _otAliases, _otTables, _otSynonyms, _otSystemTables, _otViews, _otGlobalTempTables, _otLocalTempTables, _otSystemViews,
    _otAliasesInfo, _otTablesInfo, _otSynonymsInfo, _otSystemTablesInfo, _otViewsInfo, _otGlobalTempTablesInfo,
    _otLocalTempTablesInfo, _otExternalTablesInfo, _otSystemViewsInfo: begin
      SetLength(rgRestrictions, 4);

      if MetaDataType in [_otAliases, _otTables, _otSynonyms, _otSystemTables, _otViews, _otGlobalTempTables, _otLocalTempTables, _otSystemViews] then
        Schema := DBSCHEMA_TABLES
      else
        Schema := DBSCHEMA_TABLES_INFO;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := GetTableType(MetaDataType);
    end;
    _otTableConstraints: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_TABLE_CONSTRAINTS;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['CONSTRAINT_NAME'];
      rgRestrictions[5] := Restrictions.Values['TABLE_NAME'];
    end;
    _otTablePrivileges: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_TABLE_PRIVILEGES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
    end;
    _otLinkedServers: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_LINKEDSERVERS{1});
      Schema := DBSCHEMA_LINKEDSERVERS;
      rgRestrictions[0] := Restrictions.Values['LINKED_SERVER'];
    end;
    _otAssemblies: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_ASSEMBLIES{4});
      Schema := DBSCHEMA_SQL_ASSEMBLIES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['ASSEMBLY_NAME'];
      AssemblyID := StrToIntDef(Restrictions.Values['ASSEMBLY_ID'], 0);
      if AssemblyID <> 0 then
        rgRestrictions[3] := AssemblyID;
    end;
    _otAssemblyDependencies: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_ASSEMBLY_DEPENDENCIES{4});
      Schema := DBSCHEMA_SQL_ASSEMBLY_DEPENDENCIES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      AssemblyID := StrToIntDef(Restrictions.Values['ASSEMBLY_ID'], 0);
      if AssemblyID <> 0 then
        rgRestrictions[2] := AssemblyID;
      ReferencedAssemblyID := StrToIntDef(Restrictions.Values['REFERENCED_ASSEMBLY_ID'], 0);
      if ReferencedAssemblyID <> 0 then
        rgRestrictions[3] := ReferencedAssemblyID;
    end;
    _otUserTypes: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_USER_TYPES{3});
      Schema := DBSCHEMA_SQL_USER_TYPES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['UDT_NAME'];
    end;
    _otXMLCollections: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_XML_COLLECTIONS{4});
      Schema := DBSCHEMA_XML_COLLECTIONS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['SCHEMA_COLLECTION_NAME'];
      rgRestrictions[3] := Restrictions.Values['TARGET_NAMESPACE_URI'];
    end;
    _otCheckConstraints: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_CHECK_CONSTRAINTS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['CONSTRAINT_NAME'];
    end;
    _otCheckConstraintsByTable: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_CHECK_CONSTRAINTS_BY_TABLE;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := Restrictions.Values['CONSTRAINT_NAME'];
    end;
    _otTableStatistics: begin
      SetLength(rgRestrictions, 7);
      Schema := DBSCHEMA_TABLE_STATISTICS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := Restrictions.Values['STATISTICS_NAME'];
    end;
    _otConstraintColumnUsage: begin
      SetLength(rgRestrictions, 7);
      Schema := DBSCHEMA_CONSTRAINT_COLUMN_USAGE;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_NAME'];
      rgRestrictions[3] := Restrictions.Values['COLUMN_NAME'];
      rgRestrictions[6] := Restrictions.Values['CONSTRAINT_NAME'];
    end;
    _otTableTypes: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_SQL_TABLE_TYPES;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['TABLE_TYPE_NAME'];
    end;
    _otTableTypePrimaryKeys: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_SQL_TABLE_TYPE_PRIMARY_KEYS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := Restrictions.Values['TABLE_TYPE_NAME'];
    end;
    _otTableTypeColumns: begin
      SetLength(rgRestrictions, 4);
      Schema := DBSCHEMA_SQL_TABLE_TYPE_COLUMNS;
      if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := Restrictions.Values['TABLE_TYPE_NAME'];
      rgRestrictions[3] := Restrictions.Values['COLUMN_NAME'];
    end
  else
    raise Exception.Create(SUnsupportedMetaDataKind);
  end;

  Rowset := (FRecordSet as TOLEDBRecordSet).GetSchemaRowset(Schema, rgRestrictions);
  TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOLEDBMetaData.GetTables(Restrictions: TStrings): TData;
var
  Catalog, TableSchema, TableName, TableTypes, Scope: string;
  TypesList: TStringList;
  i: integer;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  if (TableTypes = '') and (Scope = 'LOCAL') then
    TableTypes := 'TABLE,VIEW';

  SetLength(rgRestrictions, 4);
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := TableName;

  TypesList := TStringList.Create;
  try
    ParseTypes(TableTypes, TypesList);
    CreateTablesFields;
    FMemData.Open;
    for i := 0 to TypesList.Count - 1 do begin
      rgRestrictions[3] := TypesList[i];
      Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_TABLES, rgRestrictions);
      TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
      FRecordSet.Open;
      CopyTablesData(Restrictions);
      FRecordSet.Close;
    end;
  finally
    TypesList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyTablesData(Restrictions: TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snTABLE_TYPE    = 4;
  snDATE_CREATED  = 8;
  snDATE_MODIFIED = 9;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
  dnDATE_CREATED  = 5;
  dnDATE_MODIFIED = 6;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snTABLE_NAME, snTABLE_TYPE, snDATE_CREATED, snDATE_MODIFIED],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnTABLE_TYPE, dnDATE_CREATED, dnDATE_MODIFIED]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetColumns(Restrictions: TStrings): TData;
const
  fmtGetColumnsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(c.name AS VARCHAR(128)) AS COLUMN_NAME,'#$D#$A +
    '  c.colorder /*WAR undoc*/ AS POSITION,'#$D#$A +
    '  c.xtype AS DATA_TYPE,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN c.xtype = 36 THEN 38 -- uniqueidentifier (GUID) '#$D#$A +
    '    ELSE c.length '#$D#$A +
    '  END AS DATA_LENGTH,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN c.scale IS NULL THEN NULL '#$D#$A +
    '    ELSE c.prec '#$D#$A +
    '  END AS DATA_PRECISION,'#$D#$A +
    '  c.scale AS DATA_SCALE,'#$D#$A +
    '  c.isnullable AS NULLABLE,'#$D#$A +
    '  dc.definition AS DEFAULT_VALUE '#$D#$A +
    'FROM '#$D#$A +
    '  sysobjects o, syscolumns c '#$D#$A +
    '  LEFT JOIN sys.default_constraints dc ON (c.cdefault = dc.object_id) '#$D#$A +
    'WHERE '#$D#$A +
    '  o.type in (''U'', ''V'', ''S'') '#$D#$A +
    '  AND o.id = c.id '#$D#$A +
    '  %s'#$D#$A +
    'ORDER BY o.uid, o.name, c.colorder';

var
  Catalog, TableSchema, TableName, ColumnName: string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause: string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'c.name', ColumnName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtGetColumnsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 4);
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := TableName;
  rgRestrictions[3] := ColumnName;

  Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_COLUMNS, rgRestrictions);
  TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
  FRecordSet.Open;

  CreateColumnsFields;
  FMemData.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyColumnsData(Restrictions: TStrings);
const
  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;
var
  snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snDATA_TYPE,
  snLENGTH, snNUM_PRECISION, snNUM_SCALE, snDATETIME_PRECISION, snNULLABLE, snDEFAULT: integer;
  Value: variant;
  Nullable: integer;
begin
  snCATALOG            := FRecordSet.FieldByName('TABLE_CATALOG').FieldNo;
  snSCHEMA             := FRecordSet.FieldByName('TABLE_SCHEMA').FieldNo;
  snTABLE_NAME         := FRecordSet.FieldByName('TABLE_NAME').FieldNo;
  snCOLUMN_NAME        := FRecordSet.FieldByName('COLUMN_NAME').FieldNo;
  snPOSITION           := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snDATA_TYPE          := FRecordSet.FieldByName('DATA_TYPE').FieldNo;
  snLENGTH             := FRecordSet.FieldByName('CHARACTER_MAXIMUM_LENGTH').FieldNo;
  snNUM_PRECISION      := FRecordSet.FieldByName('NUMERIC_PRECISION').FieldNo;
  snNUM_SCALE          := FRecordSet.FieldByName('NUMERIC_SCALE').FieldNo;
  snDATETIME_PRECISION := FRecordSet.FieldByName('DATETIME_PRECISION').FieldNo;
  snNULLABLE           := FRecordSet.FieldByName('IS_NULLABLE').FieldNo;
  snDEFAULT            := FRecordSet.FieldByName('COLUMN_DEFAULT').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE, snDEFAULT],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCOLUMN_NAME, dnPOSITION, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE, dnDEFAULT]);

    Value := FRecordSetHelper.FieldValues[snDATETIME_PRECISION];
    if not VarIsNull(Value) then
      FMemDataHelper.FieldValues[dnPRECISION] := Value;

    Value := FRecordSetHelper.FieldValues[snNULLABLE];
    if Boolean(Value) then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
var
  Catalog, ProcSchema, ProcName, ParamName: string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider = prCompact then begin
    CreateProcedureParametersFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

  Catalog := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
  ProcSchema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);

  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.IsSQLAzureEdition then begin
    if ProcName = '' then
      raise Exception.CreateFmt(SRestrictionMustBeSet, ['PROCEDURE_NAME']);

    if ProcSchema = '' then
      ProcSchema := 'NULL'
    else
      ProcSchema := 'N''' + ProcSchema + '''';
    if ParamName = '' then
      ParamName := 'NULL'
    else
      ParamName := 'N''' + ParamName + '''';

    FRecordSet.SetSQL(Format('exec sys.sp_procedure_params_managed N''%s'', NULL, %s, %s', [ProcName, ProcSchema, ParamName]));
    FRecordSet.Open;
  end
  else begin
    SetLength(rgRestrictions, 4);
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := ProcSchema;
    rgRestrictions[2] := ProcName;
    rgRestrictions[3] := ParamName;

    Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_PROCEDURE_PARAMETERS, rgRestrictions);
    TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
    FRecordSet.Open;
  end;

  CreateProcedureParametersFields;
  FMemData.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyProcedureParametersData(Restrictions: TStrings);
const
  dnCATALOG    = 1;
  dnSCHEMA     = 2;
  dnPROC_NAME  = 3;
  dnPARAM_NAME = 4;
  dnPOSITION   = 5;
  dnDIRECTION  = 6;
  dnDATA_TYPE  = 7;
  dnLENGTH     = 8;
  dnPRECISION  = 9;
  dnSCALE      = 10;
var
  snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snPOSITION, snPARAM_TYPE,
  snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE: integer;
  ParamType: integer;
  Direction: string;
begin
  snCATALOG       := FRecordSet.FieldByName('PROCEDURE_CATALOG').FieldNo;
  snSCHEMA        := FRecordSet.FieldByName('PROCEDURE_SCHEMA').FieldNo;
  snPROC_NAME     := FRecordSet.FieldByName('PROCEDURE_NAME').FieldNo;
  snPARAM_NAME    := FRecordSet.FieldByName('PARAMETER_NAME').FieldNo;
  snPOSITION      := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snPARAM_TYPE    := FRecordSet.FieldByName('PARAMETER_TYPE').FieldNo;
  snDATA_TYPE     := FRecordSet.FieldByName('DATA_TYPE').FieldNo;
  snLENGTH        := FRecordSet.FieldByName('CHARACTER_MAXIMUM_LENGTH').FieldNo;
  snNUM_PRECISION := FRecordSet.FieldByName('NUMERIC_PRECISION').FieldNo;
  snNUM_SCALE     := FRecordSet.FieldByName('NUMERIC_SCALE').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snPOSITION, snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME, dnPARAM_NAME, dnPOSITION, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE]);

    ParamType := FRecordSetHelper.FieldValues[snPARAM_TYPE];
    case ParamType of
      DBPARAMTYPE_INPUT:
        Direction := 'IN';
      DBPARAMTYPE_INPUTOUTPUT:
        Direction := 'IN/OUT';
      DBPARAMTYPE_OUTPUT, DBPARAMTYPE_RETURNVALUE:
        Direction := 'OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  fmtGetIndexColumnsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS INDEX_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  CAST(c.name AS VARCHAR(128)) AS COLUMN_NAME,'#$D#$A +
    '  xc.key_ordinal AS COLUMN_POSITION,'#$D#$A +
    '  (CASE WHEN xc.is_descending_key <> 0 THEN ''DESC'' ELSE ''ASC'' END) AS SORT_ORDER '#$D#$A +
    'FROM sysobjects o, sys.indexes x, syscolumns c, sys.index_columns xc '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.object_id '#$D#$A +
    '  AND o.id = c.id '#$D#$A +
    '  AND o.id = xc.object_id '#$D#$A +
    '  AND x.index_id = xc.index_id '#$D#$A +
    '  AND c.colid = xc.column_id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name, xc.key_ordinal';

var
  Catalog, TableSchema, TableName, IndexName: string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause: string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtGetIndexColumnsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 5);
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := IndexName;
  rgRestrictions[4] := TableName;

  Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_INDEXES, rgRestrictions);
  TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
  FRecordSet.Open;

  CreateIndexColumnsFields;
  FMemData.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyIndexColumnsData(Restrictions: TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snINDEX_CATALOG   = 4;
  snINDEX_SCHEMA    = 5;
  snINDEX_NAME      = 6;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
var
  SortOrder: string;
  IntValue: integer;
  snCOLUMN_NAME, snPOSITION, snCOLLATION: integer;
begin
  snCOLUMN_NAME := FRecordSet.FieldByName('COLUMN_NAME').FieldNo;
  snPOSITION    := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snCOLLATION   := FRecordSet.FieldByName('COLLATION').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snINDEX_CATALOG, snINDEX_SCHEMA, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    IntValue := FRecordSetHelper.FieldValues[snCOLLATION];
    if IntValue = DB_COLLATION_DESC then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  fmtGetConstraintsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(t_obj.schema_id) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(t_obj.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS CONSTRAINT_NAME,'#$D#$A +
    '  CASE o.type '#$D#$A +
    '    WHEN ''PK'' THEN ''PRIMARY KEY'' '#$D#$A +
    '    WHEN ''UQ'' THEN ''UNIQUE'' '#$D#$A +
    '    WHEN ''F'' THEN ''FOREIGN KEY'' '#$D#$A +
    '    WHEN ''C'' THEN ''CHECK'' '#$D#$A +
    '    ELSE null '#$D#$A +
    '  END AS CONSTRAINT_TYPE,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_CATALOG,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_SCHEMA,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_NAME '#$D#$A +
    'FROM '#$D#$A +
    '  sys.all_objects o, sys.all_objects t_obj '#$D#$A +
    'WHERE '#$D#$A +
    '  o.type in (''PK'', ''UQ'', ''F'', ''C'') '#$D#$A +
    '  AND t_obj.type in (''U'', ''V'', ''S'') '#$D#$A +
    '  AND o.parent_object_id = t_obj.object_id '#$D#$A +
    '  %s'#$D#$A +
    'ORDER BY t_obj.name, o.name';

var
  Catalog, TableSchema, TableName, ConstraintName, Types: string;
  TypesList: TStringList;
  i: integer;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause, CType: string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    if Types <> '' then begin
      TypesList := TStringList.Create;
      try
        ParseTypes(Types, TypesList);
        for i := 0 to TypesList.Count - 1 do begin
          if TypesList[i] = 'PRIMARY KEY' then
            CType := '''PK'''
          else
          if TypesList[i] = 'UNIQUE' then
            CType := '''UQ'''
          else
          if TypesList[i] = 'FOREIGN KEY' then
            CType := '''F'''
          else
          if TypesList[i] = 'CHECK' then
            CType := '''C'''
          else
            CType := '';
          if (WhereClause <> '') and (CType <> '') then
            WhereClause := WhereClause + ',';
          WhereClause := WhereClause + CType;
        end;
      finally
        TypesList.Free;
      end;
      if WhereClause <> '' then
        WhereClause := 'o.type in (' + WhereClause + ')';
    end;

    AddWhere(WhereClause, 'schema_name(t_obj.schema_id)', TableSchema);
    AddWhere(WhereClause, 't_obj.name', TableName);
    AddWhere(WhereClause, 'o.name', ConstraintName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtGetConstraintsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 7);
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider <> prCompact then begin
    rgRestrictions[3] := Catalog;
    rgRestrictions[4] := TableSchema;
  end;
  rgRestrictions[2] := ConstraintName;
  rgRestrictions[5] := TableName;

  TypesList := TStringList.Create;
  try
    ParseTypes(Types, TypesList);
    CreateConstraintsFields;
    FMemData.Open;

    case TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider of
      prCompact: begin
        Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_TABLE_CONSTRAINTS, rgRestrictions);
        TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
        WhereClause := '';
        for i := 0 to TypesList.Count - 1 do begin
          if Trim(TypesList[i]) = '' then
            Continue;

          if WhereClause <> '' then
            WhereClause := WhereClause + ',';
          WhereClause := WhereClause + QuotedStr(TypesList[i]);
        end;
        if (WhereClause <> '') then
          FRecordSet.FilterText := 'CONSTRAINT_TYPE in (' + WhereClause + ')';
        FRecordSet.Open;
        CopyConstraintsData(Restrictions);
        FRecordSet.Close;
      end;
      else
        for i := 0 to TypesList.Count - 1 do begin
          rgRestrictions[6] := TypesList[i];
          Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_TABLE_CONSTRAINTS, rgRestrictions);
          TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
          FRecordSet.Open;
          CopyConstraintsData(Restrictions);
          FRecordSet.Close;
        end;
    end;
  finally
    TypesList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyConstraintsData(Restrictions: TStrings);
const
  dnCATALOG         = 1;
  dnSCHEMA          = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
var
  snCATALOG, snSCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCONSTRAINT_TYPE: integer;
begin
  snCATALOG         := FRecordSet.FieldByName('TABLE_CATALOG').FieldNo;
  snSCHEMA          := FRecordSet.FieldByName('TABLE_SCHEMA').FieldNo;
  snTABLE_NAME      := FRecordSet.FieldByName('TABLE_NAME').FieldNo;
  snCONSTRAINT_NAME := FRecordSet.FieldByName('CONSTRAINT_NAME').FieldNo;
  snCONSTRAINT_TYPE := FRecordSet.FieldByName('CONSTRAINT_TYPE').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCONSTRAINT_TYPE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME, dnCONSTRAINT_TYPE]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetDatabases(Restrictions: TStrings): TData;
var
  Catalog: string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  if TOLEDBRecordSet(FRecordSet).FCommand.FConnection.Provider = prCompact then begin
    CreateDatabasesFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

  Catalog := Trim(Restrictions.Values['CATALOG_NAME']);
  SetLength(rgRestrictions, 1);
  rgRestrictions[0] := Catalog;

  Rowset := TOLEDBRecordSet(FRecordSet).GetSchemaRowset(DBSCHEMA_CATALOGS, rgRestrictions);
  TOLEDBRecordSet(FRecordSet).SetIRowset(Rowset);
  FRecordSet.Open;

  CreateDatabasesFields;
  FMemData.Open;
  CopyDatabasesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CopyDatabasesData(Restrictions: TStrings);
const
  snCATALOG_NAME = 1;
  dnCATALOG_NAME = 1;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG_NAME], [dnCATALOG_NAME]);
    FMemDataHelper.AppendRecord;
  end;
end;

{$ENDIF}

{ TDBIDAccessor }

constructor TDBIDAccessor.Create(APDBID: PDBID);
begin
  inherited Create;

  FPDBID := APDBID;
end;

procedure TDBIDAccessor.SetPDBID(Value: PDBID);
begin
  FPDBID := Value;
end;

function TDBIDAccessor.GeteKind: DBKIND;
begin
  Result := Marshal.ReadInt32(FPDBID, OffsetOf_DBID_eKind);
end;

procedure TDBIDAccessor.SeteKind(Value: DBKIND);
begin
  Marshal.WriteInt32(FPDBID, OffsetOf_DBID_eKind, Value);
end;

function TDBIDAccessor.GetpwszName: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FPDBID, OffsetOf_DBID_uName);
end;

procedure TDBIDAccessor.SetpwszName(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FPDBID, OffsetOf_DBID_uName, Value);
end;

{ TOLEDBLoader }

constructor TOLEDBLoader.Create;
begin
  inherited;

  FSkipReadOnlyFieldDescs := False;
end;

destructor TOLEDBLoader.Destroy;
begin
  inherited;
end;

function TOLEDBLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeepIdentity:
      FKeepIdentity := Value;
    prKeepNulls:
      FKeepNulls := Value;
    prRowsPerBatch:
      FRowsPerBatch := Value;
    prKilobytesPerBatch:
      FKilobytesPerBatch := Value;
    prLockTable:
      FLockTable := Value;
    prCheckConstraints:
      FCheckConstraints := Value;
    prFireTrigger:
      FFireTrigger := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOLEDBLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeepIdentity:
      Value := FKeepIdentity;
    prKeepNulls:
      Value := FKeepNulls;
    prRowsPerBatch:
      Value := FRowsPerBatch;
    prKilobytesPerBatch:
      Value := FKilobytesPerBatch;
    prLockTable:
      Value := FLockTable;
    prCheckConstraints:
      Value := FCheckConstraints;
    prFireTrigger:
      Value := FFireTrigger;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

class function TOLEDBLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TMSLoaderColumn;
end;

procedure TOLEDBLoader.Prepare;
const
  MaxPropCount = 3;
var
  v: variant;
  TableID: PDBID;
  TableIDAccessor: TDBIDAccessor;
  BindMemorySize: NativeUInt;
  rgBindings: PDBBinding;
  rgStatus: PDBBINDSTATUSArray;
  i: integer;
  OLEDBPropertiesSet: TOLEDBPropertiesSet;
  OptionsStr: string;
  IUnk: IUnknown;
  ParamDesc: TMSParamDesc;
  Ordinal: integer;
  Connector: TOLEDBConnector;

  procedure AddOption(var OptionStr: string; const Value: string);
  begin
    if OptionStr <> '' then
      OptionStr := OptionStr + ', ';
    OptionStr := OptionStr + Value;
  end;

begin
  inherited;

  FConnection.GetProp(prProvider, v);
  if TMSProvider(v) = prCompact then
    raise Exception.Create(SLoaderNotSupport);
  if TMSProvider(v) = prDirect then
    raise Exception.Create('Not implemented yet');

  Connector := TOLEDBConnector(FConnection.GetConnector);
  Connector.Check(Connector.FIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
  FIOpenRowset := IOpenRowset(IUnk);

  TableID := Marshal.AllocHGlobal(SizeOf(TDBID));
  TableIDAccessor := TDBIDAccessor.Create(TableID);
  OLEDBPropertiesSet := TOLEDBPropertiesSet.Create(Connector, DBPROPSET_SQLSERVERROWSET);
  try
    TableIDAccessor.eKind := DBKIND_NAME;
    TableIDAccessor.pwszName := Marshal.AllocHGlobal(Length(TableName) * SizeOf(WideChar) + SizeOf(WideChar));
    CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(TableName), 0, Length(TableName)))),
      TableIDAccessor.pwszName, Length(TableName) * SizeOf(WideChar) + SizeOf(WideChar));

    OLEDBPropertiesSet.AddPropBool(SSPROP_FASTLOADKEEPIDENTITY, FKeepIdentity);
    OLEDBPropertiesSet.AddPropBool(SSPROP_FASTLOADKEEPNULLS, FKeepNulls);
    OptionsStr := '';
    if FRowsPerBatch > 0 then
      AddOption(OptionsStr, 'ROWS_PER_BATCH = ' + IntToStr(FRowsPerBatch));
    if FKilobytesPerBatch > 0 then
      AddOption(OptionsStr, 'KILOBYTES_PER_BATCH = ' + IntToStr(FKilobytesPerBatch));
    if FLockTable then
      AddOption(OptionsStr, 'TABLOCK');
    if FCheckConstraints then
      AddOption(OptionsStr, 'CHECK_CONSTRAINTS');
    if FFireTrigger then
      AddOption(OptionsStr, 'FIRE_TRIGGERS');
    if OptionsStr <> '' then
      OLEDBPropertiesSet.AddPropStr(SSPROP_FASTLOADOPTIONS, OptionsStr);

    Connector.Check(FIOpenRowset.OpenRowset(nil, TableID, nil, IID_IRowsetFastLoad, 1,
      PDBPropIDSetArray(OLEDBPropertiesSet.InitPropSet), IUnk), nil);
    FIRowsetFastLoad := IRowsetFastLoad(IUnk);
  finally
    OLEDBPropertiesSet.Free;
    Marshal.FreeHGlobal(TableIDAccessor.pwszName);
    Marshal.FreeHGlobal(TableID);
    TableIDAccessor.Free;
  end;
  
  rgStatus := Marshal.AllocHGlobal(Columns.Count * SizeOf(DBBINDSTATUS));
  IntPtr(FParamsAccessorData.Accessor) := nil;
  try
    try
      CheckColumnsInfo;

      FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
      FParamsAccessorData.ExecuteParams.pData := nil;
      FParamsAccessorData.ExecuteParams.cParamSets := 1;

      SetLength(FParamsAccessorData.rgBindings, Columns.Count);

      for i := 0 to Columns.Count - 1 do
        FParamsAccessorData.rgBindings[i].pObject := nil;

      BindMemorySize := 0;

      FBlobList := TObjectList.Create;
      FParamDescs := TParamDescs.Create;
      for i := 0 to Columns.Count - 1 do begin
        ParamDesc := TMSParamDesc.Create;
        try
          ParamDesc.SetName(Columns[i].Name); // +++
          ParamDesc.SetDataType(Columns[i].DataType);
          ParamDesc.SetParamType(pdInput);
          if Columns[i].ActualFieldNo > 0 then
            Ordinal := Columns[i].ActualFieldNo
          else
            Ordinal := i + 1;
          FillBindingForParam(Ordinal, ParamDesc, FConnection,
            FParamsAccessorData.rgBindings[i], BindMemorySize, False,
            TMSLoaderColumn(Columns[i]).IsWide);
          FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
          ParamDesc.SetSize(FParamsAccessorData.rgBindings[i].cbMaxLen);
          ParamDesc.SetNull(UseBlankValues);
        finally
          FParamDescs.Add(ParamDesc);
        end;
      end;

      FParamsAccessorData.ExecuteParams.pData := Marshal.AllocHGlobal(BindMemorySize);
      FillChar(FParamsAccessorData.ExecuteParams.pData, BindMemorySize, 0);

      QueryIntf(FIRowsetFastLoad, IID_IAccessor, FParamsAccessorData.Accessor);
      rgBindings := @FParamsAccessorData.rgBindings[0];

      for i := 0 to Columns.Count - 1 do begin
        FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_INPUT;
        SaveParamValue(FParamDescs[i], FParamsAccessorData.rgBindings[i], FParamsAccessorData
          {$IFDEF HAVE_COMPRESS}, cbNone{$ENDIF}, FConnection);
        FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
      end;

      Connector.Check(FParamsAccessorData.Accessor.CreateAccessor(DBACCESSOR_ROWDATA, Columns.Count,
        rgBindings, BindMemorySize, FParamsAccessorData.ExecuteParams.HACCESSOR, rgStatus), nil);
    except
      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        for i := 0 to Columns.Count - 1 do begin
          if FParamsAccessorData.rgBindings[i].pObject <> nil then
            Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
        end;
        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      raise;
    end;
  finally
    Marshal.FreeHGlobal(rgStatus);
  end;
end;

procedure TOLEDBLoader.LoadRow;
begin
  try
    TOLEDBConnector(FConnection.GetConnector).Check(FIRowsetFastLoad.InsertRow(FParamsAccessorData.ExecuteParams.HACCESSOR,
      FParamsAccessorData.ExecuteParams.pData), nil);
  except
    FIRowsetFastLoad := nil;
    raise;
  end;
  Inc(FLoadedRows);

  DoAfterLoadRow;
end;

procedure TOLEDBLoader.DoAfterLoadRow;
var
  i: Integer;
begin
  if FObjectReleaseNeeded then
    for i := 0 to FParamDescs.Count - 1 do
      DoReleaseObjectRef(FParamDescs[i].Value);

  if FUseBlankValues then
    for i := 0 to FParamDescs.Count - 1 do begin
      FParamDescs[i].SetNull(True);
      FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_INPUT;
      SaveParamValue(FParamDescs[i], FParamsAccessorData.rgBindings[i], FParamsAccessorData
        {$IFDEF HAVE_COMPRESS}, cbNone{$ENDIF}, FConnection);
      FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
    end;
end;

procedure TOLEDBLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    LoadRow;
end;

function TOLEDBLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := FParamDescs <> nil;
end;

procedure TOLEDBLoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
var
  ParamDesc: CRAccess.TParamDesc;
  pLength: IntPtr;
  Blob: MemData.TBlob;
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    LoadRow;

  inherited;

  ParamDesc := FParamDescs.Items[Col];
  if (Columns[Col].DataType = dtBlob) and VarIsStr(Value) then begin
  {$IFDEF HAVE_COMPRESS}
    Blob := TCompressedBlob.Create;
  {$ELSE}
    Blob := MemData.TBlob.Create;
  {$ENDIF}
    FBlobList.Add(Blob);
    Blob.AsString := Value;
    ParamDesc.SetObject(Blob);
    ParamDesc.SetNull(IntPtr(Blob.FirstPiece) = nil);
  end
  else begin
    Blob := nil;
    ParamDesc.SetValue(Unassigned);
    DoAddObjectRef(Value);
    ParamDesc.SetValue(Value);
  end;

  try
    FParamsAccessorData.rgBindings[Col].eParamIO := DBPARAMIO_INPUT;
    SaveParamValue(ParamDesc, FParamsAccessorData.rgBindings[Col], FParamsAccessorData
      {$IFDEF HAVE_COMPRESS}, cbNone{$ENDIF}, FConnection);
    FParamsAccessorData.rgBindings[Col].eParamIO := DBPARAMIO_NOTPARAM;

    if not (ParamDesc.GetDataType in CharsByRef + BytesByRef) and not (ParamDesc.GetDataType in [dtUnknown]) then begin
      pLength := PtrOffset(FParamsAccessorData.ExecuteParams.pData, FParamsAccessorData.rgBindings[Col].obLength);
      Marshal.WriteInt32(pLength, FParamsAccessorData.rgBindings[Col].cbMaxLen);
    end;
  except
    if Blob = nil then begin
      DoReleaseObjectRef(Value);
      ParamDesc.SetValue(Unassigned);
    end;
    raise;
  end;
end;

procedure TOLEDBLoader.Finish;
begin
  inherited;

  try
    if FIRowsetFastLoad <> nil then
      TOLEDBConnector(FConnection.GetConnector).Check(FIRowsetFastLoad.Commit(True), nil);

    FBlobList.Free;
    FParamDescs.Free;

    if FParamsAccessorData.ExecuteParams.pData <> nil then
      TOLEDBConnector(FConnection.GetConnector).Check(FParamsAccessorData.Accessor.ReleaseAccessor(FParamsAccessorData.ExecuteParams.HACCESSOR, nil), nil);
  finally
    FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
    FParamsAccessorData.Accessor := nil;
    if FParamsAccessorData.ExecuteParams.pData <> nil then
      Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
    FParamsAccessorData.ExecuteParams.pData := nil;

    FIOpenRowset := nil;
    FIRowsetFastLoad := nil;
    Reset;
  end;
end;

procedure TOLEDBLoader.SetConnection(Value: TCRConnection);
begin
  inherited;
  FConnection := TMSSQLConnection(Value);
end;

procedure TOLEDBLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  TMSLoaderColumn(Column).IsWide := (FieldDesc.SubDataType and sdtWide) <> 0;
end;

{ TOLEDBProperties }

{ TOLEDBPropertiesSet }

const
  MaxPropCount = 20;

constructor TOLEDBPropertiesSet.Create(Connector: TOLEDBConnector; const GuidPropertySet: TGUID);
begin
  inherited Create;

  FConnector := Connector;
  Init(GuidPropertySet);
end;

procedure TOLEDBPropertiesSet.Init(const GuidPropertySet: TGUID);
begin
  FPropSet := Marshal.AllocHGlobal(SizeOf(DBPROPSET));
  FPropSet.cProperties := 0;
  FPropSet.guidPropertySet := GuidPropertySet;
  FPropSet.rgProperties := Marshal.AllocHGlobal(MaxPropCount * SizeOf_DBProp);
  FillChar(FPropSet.rgProperties, MaxPropCount * SizeOf_DBProp, 0);
end;

destructor TOLEDBPropertiesSet.Destroy;
begin
  Clear;

  inherited;
end;

procedure TOLEDBPropertiesSet.Clear;
var
  i: integer;
  rgProperty: PDBProp;
begin
  if IntPtr(FPropSet) <> nil then begin
    if FPropSet.rgProperties <> nil then begin
      for i := 0 to Integer(FPropSet.cProperties) - 1 do begin
        rgProperty := GetDBPropPtr(i);
        rgProperty.vValue := Unassigned;
      end;

      Marshal.FreeHGlobal(FPropSet.rgProperties);
    end;

    Marshal.FreeHGlobal(FPropSet);
  end;
end;

function TOLEDBPropertiesSet.GetPropCount: UINT;
begin
  Result := FPropSet.cProperties;
end;

procedure TOLEDBPropertiesSet.SetPropCount(Value: UINT);
begin
  FPropSet.cProperties := Value;
end;

procedure TOLEDBPropertiesSet.Check(const Status: HRESULT);
begin
  try
    FConnector.Check(Status, nil);
  except
    on E: Exception do begin
      AddInfoToErr(E, GetInitPropSetStatus, []);
      raise E;
    end;
  end;
end;

function TOLEDBPropertiesSet.GetInitPropSetStatus: string;
var
  i: integer;
  p: PDBProp;
begin
  Result := GUIDToString(FPropSet.guidPropertySet);

  for i := 0 to PropCount - 1 do begin
    p := GetDBPropPtr(i);
    if p.dwStatus <> 0 then
      Result := Format('%s'#$D#$A'[%d] := $%X. PropId := %d', [Result, i, p.dwStatus, p.dwPropertyID]);
  end;
end;

function TOLEDBPropertiesSet.GetDBPropPtr(Index: UINT): PDBProp;
begin
  Assert(PropCount <= MaxPropCount);
  Assert(Index <= PropCount);
  Result := PtrOffset(FPropSet.rgProperties, Integer(Index * SizeOf_DBProp));
end;

function TOLEDBPropertiesSet.InitProp(const dwPropertyID: DBPROPID; const Required: boolean = False): PDBProp;
begin
  Result := GetDBPropPtr(PropCount);
  Result.dwPropertyID := dwPropertyID;
  if Required then
    Result.dwOptions := DBPROPOPTIONS_REQUIRED
  else
    Result.dwOptions := DBPROPOPTIONS_OPTIONAL;
  Result.dwStatus := 0;
  Result.colid := DB_NULLID;

  PropCount := PropCount + 1;
end;

procedure TOLEDBPropertiesSet.AddPropSmallInt(const dwPropertyID: DBPROPID; const Value: Smallint);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID);
  p.vValue := VarAsType(Value, VT_I2);
end;

procedure TOLEDBPropertiesSet.AddPropInt(const dwPropertyID: DBPROPID; const Value: Integer);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, True);
  p.vValue := VarAsType(Value, VT_I4);
end;

procedure TOLEDBPropertiesSet.AddPropUInt(const dwPropertyID: DBPROPID; const Value: Cardinal);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, True);
  p.vValue := VarAsType(Integer(Value), VT_UI4);
  TVarData(p.vValue).VType := VT_UI4;
end;

procedure TOLEDBPropertiesSet.AddPropBool(const dwPropertyID: DBPROPID; const Value: boolean; const Required: boolean = False);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, Required);
  p.vValue := VarAsBooleanType(Value);
end;

procedure TOLEDBPropertiesSet.AddPropStr(const dwPropertyID: DBPROPID; const Value: string; const Required: boolean = False);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, Required);
  p.vValue := Value;
end;

procedure TOLEDBPropertiesSet.AddPropIntf(const dwPropertyID: DBPROPID; const Value: TInterfacedObject; const Required: boolean = False);
var
  p: PDBProp;
  pData: IntPtr;
begin
  p := InitProp(dwPropertyID, Required);
  pData := Marshal.GetIUnknownForObject(Value);
  p.vValue := IUnknown(pData);
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: IDBProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FPropSet)));
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: ISessionProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FPropSet)));
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: ICommandProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FPropSet)));
end;

{ TOLEDBParamPropertiesSet }

procedure TOLEDBParamPropertiesSet.Init(const GuidPropertySet: TGUID);
var
  i: integer;
  PropSet: PDBPropSet;
  ParamProps: PSSParamProps;
begin
  FSSParamsCount := 0;
  FSSParamProp := Marshal.AllocHGlobal(MaxPropCount * SizeOf(SSPARAMPROPS));
  FPropSet := Marshal.AllocHGlobal(MaxPropCount * SizeOf(DBPROPSET));
  FDBProp := Marshal.AllocHGlobal(MaxPropCount * SizeOf_DBProp);
  FillChar(FDBProp, MaxPropCount * SizeOf_DBProp, 0);

  for i := 0 to MaxPropCount - 1 do begin
    PropSet := PtrOffset(FPropSet, Integer(i * SizeOf(DBPROPSET)));
    PropSet.rgProperties := PtrOffset(FDBProp, i * integer(SizeOf_DBProp));
    PropSet.cProperties := 1;
    PropSet.guidPropertySet := GuidPropertySet;

    ParamProps := PtrOffset(FSSParamProp, Integer(i * SizeOf(SSPARAMPROPS)));
    ParamProps.iOrdinal := 0;
    ParamProps.cPropertySets := 1;
    ParamProps.rgPropertySets := PropSet;
  end;
end;

procedure TOLEDBParamPropertiesSet.Clear;
var
  i: integer;
  Prop: PDBProp;
begin
  if IntPtr(FDBProp) <> nil then begin
    for i := 0 to Integer(FSSParamsCount) - 1 do begin
      Prop := GetDBPropPtr(i);
      Prop.vValue := Unassigned;
    end;

    Marshal.FreeHGlobal(FSSParamProp);
    Marshal.FreeHGlobal(FPropSet);
    Marshal.FreeHGlobal(FDBProp);
  end;
end;

function TOLEDBParamPropertiesSet.GetPropCount: UINT;
begin
  Result := FSSParamsCount;
end;

procedure TOLEDBParamPropertiesSet.SetPropCount(Value: UINT);
begin
  FSSParamsCount := Value;
end;

function TOLEDBParamPropertiesSet.GetDBPropPtr(Index: UINT): PDBProp;
begin
  Assert(PropCount <= MaxPropCount);
  Assert(Index <= PropCount);
  Result := PtrOffset(FDBProp, Integer(Index * SizeOf_DBProp));
end;

procedure TOLEDBParamPropertiesSet.AddProp(const dwPropertyID: DBPROPID; const Value: OleVariant; const Ordinal: DBORDINAL);
var
  p: PDBProp;
  ParamProps: PSSParamProps;
begin
  p := InitProp(dwPropertyID, True);
  p.vValue := Value;

  ParamProps := PtrOffset(FSSParamProp, Integer((FSSParamsCount - 1) * SizeOf(SSPARAMPROPS)));
  ParamProps.iOrdinal := Ordinal;
end;

function TOLEDBParamPropertiesSet.SetParameterProperties(Obj: ISSCommandWithParameters): HResult;
begin
  Assert(Obj <> nil);
  Result := Obj.SetParameterProperties(FSSParamsCount, FSSParamProp);
end;

{ TOLEDBPropertiesGet }

constructor TOLEDBPropertiesGet.Create(Connector: TOLEDBConnector; const GuidPropertySet: TGUID);
begin
  inherited Create;

  FConnector := Connector;

  FPropSet := Marshal.AllocHGlobal(SizeOf(DBPROPSET));
  FPropSet.cProperties := 0;
  FPropSet.rgProperties := nil;
  FPropSet.guidPropertySet := GuidPropertySet;
end;

destructor TOLEDBPropertiesGet.Destroy;
begin
  Marshal.FreeHGlobal(FPropSet);
  inherited;
end;

procedure TOLEDBPropertiesGet.AddPropId(Id: DBPROPID);
begin
  Assert(FPropIdsGC = nil);
  SetLength(FPropIds, Length(FPropIds) + 1);
  FPropIds[Length(FPropIds) - 1] := Id;
end;

procedure TOLEDBPropertiesGet.Check(const Status: HRESULT);
begin
  FConnector.Check(Status, nil);
end;

function TOLEDBPropertiesGet.GetDBPropPtr(rgProperties: PDBPropArray; Index: UINT): PDBProp;
begin
  Assert(Index <= FPropSet.cProperties);
  Result := PtrOffset(rgProperties, Integer(Index * SizeOf_DBProp));
end;

procedure TOLEDBPropertiesGet.PrepareToGet;
begin
  FPropIdsGC := AllocGCHandle(FPropIds, True);
  FPropSet.rgProperties := GetAddrOfPinnedObject(FPropIdsGC);
  FPropSet.cProperties := Length(FPropIds);
end;

procedure TOLEDBPropertiesGet.ProcessResult(rgPropertySets: PDBPropSet; out PropValues: TPropValues);
var
  i: integer;
begin
  SetLength(PropValues, FPropSet.cProperties);
  for i := 0 to FPropSet.cProperties - 1 do
    PropValues[i] := GetDBPropPtr(rgPropertySets.rgProperties, i).vValue;
end;

procedure TOLEDBPropertiesGet.ClearResult(rgPropertySets: PDBPropSet);
var
  i: integer;
begin
  FreeGCHandle(FPropIdsGC);
  FPropIdsGC := nil;
  FPropSet.rgProperties := nil;

  for i := 0 to FPropSet.cProperties - 1 do
    GetDBPropPtr(rgPropertySets.rgProperties, i).vValue := Unassigned;

  FreeCoMem(rgPropertySets.rgProperties);
  FreeCoMem(rgPropertySets);
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: IDBProperties; out PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: IRowsetInfo; out PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: ICommandProperties; out PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

{ TOLEDBErrors }

constructor TOLEDBErrors.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TOLEDBErrors.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TOLEDBErrors.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TOLEDBErrors.GetCount: integer;
begin
  Result := FList.Count;
end;

function TOLEDBErrors.GetError(Index: Integer): EOLEDBError;
begin
  Result := EOLEDBError(FList[Index]);
end;

procedure TOLEDBErrors.Assign(Source: TOLEDBErrors);
begin
  Clear;
  Add(Source);
end;

procedure TOLEDBErrors.Add(Source: TOLEDBErrors);
var
  i: integer;
  SrcErr, DstErr: EOLEDBError;
begin
  for i := 0 to Source.Count - 1 do begin
    SrcErr := Source.Errors[i];
    DstErr := nil;
    if SrcErr is EMSError then
      DstErr := EMSError.Create(SrcErr.ErrorCode, WideString(SrcErr.Message))
    else
      if SrcErr is EOLEDBError then
        DstErr := EOLEDBError.Create(SrcErr.ErrorCode, WideString(SrcErr.Message))
      else
        Assert(False);

    DstErr.Assign(SrcErr);
    FList.Add(DstErr);
  end;
end;

{ EOLEDBError }

constructor EOLEDBError.Create(ErrorCode: integer; const Msg: WideString);
begin
  inherited Create(ErrorCode, string(Msg));

  FMessageWide := Msg;
  FErrors := TOLEDBErrors.Create;
end;

destructor EOLEDBError.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function EOLEDBError.GetErrorCount: integer;
begin
  Result := FErrors.Count;
end;

function EOLEDBError.GetError(Index: Integer): EOLEDBError;
begin
  Result := FErrors[Index];
end;

procedure EOLEDBError.Assign(Source: EOLEDBError);
begin
  FOLEDBErrorCode := Source.FOLEDBErrorCode;
  Fiid := Source.Fiid;
{$IFNDEF NODBACCESS}
  Component := Source.Component;
{$ENDIF}
  FErrors.Assign(Source.FErrors);
end;

{ EMSError }

constructor EMSError.Create(const pServerErrorInfo: SSERRORINFO; OLEDBErrorCode: integer; Msg: WideString);
begin
  inherited Create(pServerErrorInfo.lNative, Msg);
{$IFDEF LITE}
  FErrorCode := pServerErrorInfo.lNative;
{$ENDIF}

  FMSSQLErrorCode := pServerErrorInfo.lNative;
  FServerName := string(pServerErrorInfo.pwszServer);
  FProcName := string(pServerErrorInfo.pwszProcedure);
  FState := pServerErrorInfo.bState;
  FSeverityClass := pServerErrorInfo.bClass;
  FLineNumber := pServerErrorInfo.wLineNumber;
  FLastMessage := string(pServerErrorInfo.pwszMessage);
end;

procedure EMSError.Assign(Source: EOLEDBError);
var
  Src: EMSError;
begin
  inherited;

  if Source is EMSError then begin
    Src := EMSError(Source);
    FMSSQLErrorCode := Src.FMSSQLErrorCode;

    FServerName := Src.ServerName;
    FProcName := Src.ProcName;
    FState := Src.State;
    FSeverityClass := Src.SeverityClass;
    FLineNumber := Src.LineNumber;
    FLastMessage := Src.LastMessage;
  end;
end;

{$IFNDEF NODBACCESS}
function EMSError.IsFatalError: boolean;
begin
  Result := SeverityClass >= 20; {fatal error}
  if not Result then
    Result :=
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode <= NE_MAX_NETERROR))  // Network Library
    or
      ((SeverityClass = 10) and (State = 1) and (MSSQLErrorCode = WSAECONNRESET)) // Win sockets
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = WSAECONNRESET)) // Win sockets
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_PIPE_NOT_CONNECTED)) // Named pipes
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_NETNAME_DELETED)) // Named pipes
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_NO_DATA)); // Named pipes being closed
end;

function EMSError.IsKeyViolation: boolean;
begin
  Result := (ErrorCode = 2627{Violation of %ls constraint '%.*ls'. Cannot insert duplicate key in object '%.*ls'.})
end;
{$ENDIF}

var
  DataSourceTypes: TBytes;

procedure InitDataSourceTypes;
var
  pDataSourceTypes: IntPtr;
  byteIndex: integer;

  function AddType(const TypeName: WideString): IntPtr;
  var
    Cnt: integer;
  begin
    Result := PtrOffset(pDataSourceTypes, byteIndex);
    Cnt := Encoding.Unicode.GetBytes(TypeName, {$IFDEF MOBILE}0{$ELSE}1{$ENDIF}, Length(TypeName), DataSourceTypes, byteIndex);
    DataSourceTypes[byteIndex + Cnt] := 0;
    DataSourceTypes[byteIndex + Cnt + 1] := 0;
    byteIndex := byteIndex + Cnt + 2;
    Assert(byteIndex + Cnt < Length(DataSourceTypes));
  end;

begin
  SetLength(DataSourceTypes, 350);
  pDataSourceTypes := @DataSourceTypes[0];
  byteIndex := 0;

  dstSmallint := AddType('smallint');
  dstInt := AddType('int');
  dstReal := AddType('real');
  dstFloat := AddType('float');
  dstMoney := AddType('money');
  dstDateTime := AddType('datetime');
  dstNVarChar := AddType('nvarchar');
  dstNVarCharMax := AddType('nvarchar(max)');
  dstVarChar := AddType('varchar');
  dstVarCharMax := AddType('varchar(max)');
  dstBit := AddType('bit');
  dstTinyInt := AddType('tinyint');
  dstBigint := AddType('bigint');
  dstSql_variant := AddType('sql_variant');
  dstImage := AddType('image');
  dstBinary := AddType('binary');
  dstVarBinary := AddType('varbinary');
  dstGuid := AddType('uniqueidentifier');
  dstXML := AddType('xml');
  dstTable := AddType('table');
end;

procedure FinalizeDataSourceTypes;
begin
end;

var
  OSVersionInfo: TOSVersionInfo;

initialization
  __UseRPCCallStyle := True;
  InitDataSourceTypes;
  ParamsInfoOldBehavior := False; // delete 03.06.2006
  // Windows Vista detecting
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Windows.GetVersionEx(OSVersionInfo);
  IsWindowsVista := OSVersionInfo.dwMajorVersion = 6;
  AnsiCodePageName := '';
  OpenSqlFilestream := @NCNotLink;
{$IFDEF FPC}
  LoadOleLib;
{$ENDIF}

finalization
  LocaleIdentifierList.Free;
{$IFDEF DEBUG} if StreamCnt <> 0 then MessageBox(0, PChar(IntToStr(StreamCnt) + ' Stream(s) hasn''t been released'), 'DA warning', MB_OK); {$ENDIF}
  FinalizeDataSourceTypes;
  GlobaIMalloc := nil;
  FreeNCLib;
{$IFDEF FPC}
  FreeOleLib;
{$ENDIF}

end.
