
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  ReportBuilder support
//  Based on Digital Metaphors Corporation's demos
//////////////////////////////////////////////////

unit daUniDAC;

interface
uses
  SysUtils,  // System.SysUtils
  Classes,   // System.Classes

  DB,        // Data.DB
  Uni,

  ppTypes,
  ppUtils,
  ppDB,
  ppDBPipe,
  ppClasUt,

  daDB,
  daDataView,
  daQueryDataView;

type
  {Universal Data Access Components (UniDAC) DataView Classes:
     1.  UniDAC TDataSet descendants
           - TDataSets that can be children of a DataView.
           - Override the HasParent method of TComponent to return True
           - Must be registerd with the Delphi IDE using the RegisterNoIcon procedure

       a. TdaChildUniDACQuery        - TUniQuery descendant that can be a child of a DataView
       b. TdaChildUniDACTable        - TUniTable descendant that can be a child of a DataView
       b. TdaChildUniDACStoredProc   - TUniStoredProc descendant that can be a child of a DataView

     3.  TdaUniDACSession
           - descendant of TppSession
           - implements GetDatabaseNames, GetTableNames, etc.

     4.  TdaUniDACDataSet
          - descendant of TppDataSet
          - implements GetFieldNames for SQL

     5.  TdaUniDACQueryDataView
          - descendant of TppQueryDataView
          - uses the above classes to create the required
            Query -> DataSource -> Pipeline -> Report connection
          - uses the TdaSQL object built by the QueryWizard to assign
            SQL to the TUniDACQuery etc.
      }

  { TdaChildUniDACQuery }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaChildUniDACQuery = class(TUniQuery)
  public
    function HasParent: Boolean; override;
  end;  {class, TdaChildUniDACQuery}

  { TdaChildUniDACTable }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaChildUniDACTable = class(TUniTable)
  public
    function HasParent: Boolean; override;
  end;  {class, TdaChildUniDACTable}

  { TdaChildUniDACStoredProc }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaChildUniDACStoredProc = class(TUniStoredProc)
  public
    function HasParent: Boolean; override;
  end;  {class, TdaChildUniDACStoredProc}

  { TdaUniDACSession }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaUniDACSession = class(TdaSession)
  private

  protected
    function GetDefaultDatabase(const aDatabaseName: String): TComponent; override;
    procedure GetTableNamesForConnection(const aConnection: TUniConnection; aList: TStrings); virtual;
    function IsNamedDatabase(const aDatabaseName: String; aDatabase: TComponent): Boolean; override;

  public
    class function ClassDescription: String; override;
    class function DataSetClass: TdaDataSetClass; override;
    class function DatabaseClass: TComponentClass; override;
    class function GetDefaultUniDACConnection: TUniConnection;
    class function GetUniDatabaseType(aUniDacConnection: TUniConnection): TppDatabaseType;

    procedure GetDatabaseNames(aList: TStrings); override;
    function  GetDatabaseType(const aDatabaseName: String): TppDatabaseType; override;
    procedure GetTableNames(const aDatabaseName: String; aList: TStrings); override;
    function  ValidDatabaseTypes: TppDatabaseTypes; override;

  end; {class, TdaUniDACSession}

  { TdaUniDACDataSet }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaUniDACDataSet = class(TdaDataSet)
  private
    FQuery: TUniQuery;
    FConnection: TUniConnection;

    function GetQuery: TUniQuery;
	
  protected
    procedure BuildFieldList; override;
    function  GetActive: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetDatabase(aDatabase: TComponent); override;
    procedure SetDataName(const aDataName: String); override;

    property Query: TUniQuery read GetQuery;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    class function ClassDescription: String; override;

    procedure GetFieldNamesForSQL(aList: TStrings; aSQL: TStrings); override;
    procedure GetFieldsForSQL(aList: TList; aSQL: TStrings); override;
  end; {class, TdaUniDACDataSet}

  { TdaUniDACQueryDataView }
{$IFDEF VER230}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TdaUniDACQueryDataView = class(TdaQueryDataView)
    private
      FDataSource: TppChildDataSource;
      FQuery: TdaChildUniDACQuery;

    protected
      procedure SQLChanged; override;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      class function SessionClass: TClass; override;

      procedure Init; override;
      procedure ConnectPipelinesToData; override;

    published
      property DataSource: TppChildDataSource read FDataSource;

  end; {class, TdaUniDACQueryDataView}


  {Delphi design time registration}
  procedure Register;

implementation

const
  cDefaultConnection = 'DefaultUniDACConnection';

var
  uUniDACConnection: TUniConnection;


{******************************************************************************
 *
 ** R E G I S T E R
 *
{******************************************************************************}

procedure Register;
begin
  {UniDAC DataAccess Components}
  RegisterNoIcon([TdaChildUniDACQuery, TdaChildUniDACTable, TdaChildUniDACStoredProc]);

  {UniDAC DataViews}
  RegisterNoIcon([TdaUniDACQueryDataView]);
end;


{******************************************************************************
 *
 ** C H I L D  U N I D A C  D A T A   A C C E S S   C O M P O N E N T S
 *
{******************************************************************************}

{------------------------------------------------------------------------------}
{ TdaChildUniDACQuery.HasParent }

function TdaChildUniDACQuery.HasParent: Boolean;
begin
  Result := True;
end; {function, HasParent}

{------------------------------------------------------------------------------}
{ TdaChildUniDACTable.HasParent }

function TdaChildUniDACTable.HasParent: Boolean;
begin
  Result := True;
end; {function, HasParent}

{------------------------------------------------------------------------------}
{ TdaChildUniDACStoredProc.HasParent }

function TdaChildUniDACStoredProc.HasParent: Boolean;
begin
  Result := True;
end; {function, HasParent}

{******************************************************************************
 *
 ** U N I D A C  S E S S I O N
 *
{******************************************************************************}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.ClassDescription }

class function TdaUniDACSession.ClassDescription: String;
begin
  Result := 'UniDACSession';
end; {class function, ClassDescription}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.DataSetClass }

class function TdaUniDACSession.DataSetClass: TdaDataSetClass;
begin
  Result := TdaUniDACDataSet;
end; {class function, DataSetClass}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.DatabaseClass }

class function TdaUniDACSession.DatabaseClass: TComponentClass;
begin
  Result := TUniConnection;
end;

class function TdaUniDACSession.GetDefaultUniDACConnection: TUniConnection;
begin

  {create the default  Connection, if needed}
  if (uUniDACConnection = nil) then
    begin
      uUniDACConnection := TUniConnection.Create(nil);
      uUniDACConnection.Name := cDefaultConnection;

    end;

  Result := uUniDACConnection;

end;

class function TdaUniDacSession.GetUniDatabaseType(aUniDacConnection: TUniConnection): TppDatabaseType;
begin
   if aUniDacConnection.ProviderName='SQL Server' then
    Result := dtMSSQLServer
   else if aUniDacConnection.ProviderName='Oracle' then
    Result := dtOracle
   else if aUniDacConnection.ProviderName='MySQL' then
    Result := dtMySQL
   else if aUniDacConnection.ProviderName='InterBase' then
    Result := dtInterBase
   else if aUniDacConnection.ProviderName='PostgreSQL' then
    Result := dtPostgreSQL
   else
    Result := dtOther;

end; {procedure, GetDatabaseType}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.GetDatabaseType }

function TdaUniDACSession.GetDatabaseType(const aDatabaseName: String): TppDatabaseType;
var
  aUniDacConnection: TUniConnection;
begin
   aUniDacConnection := GetDefaultUniDacConnection;
   Result := GetUniDatabaseType(aUniDacConnection);
end; {procedure, GetDatabaseType}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.GetDatabaseNames }

procedure TdaUniDACSession.GetDatabaseNames(aList: TStrings);
begin
  {call inherited to build list of available TADOConnection components}
  inherited GetDatabaseNames(aList);

  {could add hard-coded connection strings here, or could
   read from an .ini file }

end; {procedure, GetDatabaseNames}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.GetDefaultDatabase }

function TdaUniDACSession.GetDefaultDatabase(const aDatabaseName: String): TComponent;
var
  lConnection: TUniConnection;
begin

  lConnection := GetDefaultUniDACConnection;

  if (lConnection.Server <> aDatabaseName) then
    begin
      if lConnection.Connected then
        lConnection.Connected := False;
      lConnection.Server := aDatabaseName;

    end;

  Result := lConnection;

end; {function, GetDefaultDatabase}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.GetTableNames }

procedure TdaUniDACSession.GetTableNames(const aDatabaseName: String; aList: TStrings);
var
  lConnection: TUniConnection;
begin

  {get the connection}
  lConnection := TUniConnection(GetDatabaseForName(aDatabaseName));

  {connection must be active to get table names}
  if not lConnection.Connected then
    lConnection.Connected := True;

  GetTableNamesForConnection(lConnection, aList);

end;

{------------------------------------------------------------------------------}
{ TdaUniDACSession.GetTableNamesForConnection }

procedure TdaUniDACSession.GetTableNamesForConnection(const aConnection: TUniConnection; aList: TStrings);
var
  lNameField: TField;
  lMetaData: TUniMetadata;

begin
  aList.BeginUpdate;

  lMetaData := TUniMetadata.Create(nil);

  try

    lMetaData.Connection := aConnection;
    lMetaData.MetaDatakind := 'Tables';


    // get table names
    lMetaData.Restrictions.Values['TABLE_TYPE'] := 'TABLE';

    lMetaData.Open;

    lNameField := lMetaData.FieldByName('TABLE_NAME') as TStringField;
    while not lMetaData.Eof do
      begin
        aList.Add(lNameField.Value);
        lMetaData.Next
      end;

    lMetaData.Close;

    // get view names
    lMetaData.Restrictions.Values['TABLE_TYPE'] := 'VIEW';

    lMetaData.Open;

    lNameField := lMetaData.FieldByName('TABLE_NAME') as TStringField;
    while not lMetaData.Eof do
      begin
        aList.Add(lNameField.Value);
        lMetaData.Next
      end;


  finally
    lMetaData.Free;
    aList.EndUpdate;

  end;


end;

{------------------------------------------------------------------------------}
{ TdaUniDACSession.IsNamedDatabase }

function TdaUniDACSession.IsNamedDatabase(const aDatabaseName: String; aDatabase: TComponent): Boolean;
begin

  Result := (AnsiCompareText(aDatabase.Name, aDatabaseName) = 0) or
            (AnsiCompareText(TUniConnection(aDatabase).Server, aDatabaseName) = 0);

end; {function, IsNamedDatabase}

{------------------------------------------------------------------------------}
{ TdaUniDACSession.ValidDatabaseTypes }

function TdaUniDACSession.ValidDatabaseTypes: TppDatabaseTypes;
begin
  Result := [dtMSSQLServer, dtOracle, dtMySQL, dtInterBase, dtPostgreSQL, dtOther];
end; {function, ValidDatabaseTypes}

{******************************************************************************
 *
 ** U N I D A C  D A T A S E T
 *
{******************************************************************************}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.Create }

constructor TdaUniDACDataSet.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FQuery := nil;
end; {constructor, Create}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.Destroy }

destructor TdaUniDACDataSet.Destroy;
begin
  FQuery.Free;

  inherited Destroy;
end; {destructor, Destroy}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.ClassDescription }

class function TdaUniDACDataSet.ClassDescription: String;
begin
  Result := 'UniDACDataSet';
end; {class function, ClassDescription}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.GetActive }

function TdaUniDACDataSet.GetActive: Boolean;
begin
  Result := GetQuery.Active
end; {function, GetActive}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.SetActive }

procedure TdaUniDACDataSet.SetActive(Value: Boolean);
begin
  GetQuery.Active := Value;
end; {procedure, SetActive}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.GetQuery }

function TdaUniDACDataSet.GetQuery: TUniQuery;
begin
  {create UniDACDataSet, if needed}
  if (FQuery = nil) then
    FQuery := TUniQuery.Create(Self);

  Result := FQuery;
end; {procedure, GetQuery}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.SetDatabase }

procedure TdaUniDACDataSet.SetDatabase(aDatabase: TComponent);
begin
  inherited SetDatabase(aDatabase);

  {table cannot be active to set database property}
  if GetQuery.Active then
    FQuery.Active := False;

  FConnection := TUniConnection(aDatabase);

  {get UniDAC Connection for name}
  FQuery.Connection := FConnection;

end; {procedure, SetDatabaseName}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.SetDataName }

procedure TdaUniDACDataSet.SetDataName(const aDataName: String);
begin
  inherited SetDataName(aDataName);

  {dataset cannot be active to set data name}
  if GetQuery.Active then
    FQuery.Active := False;

  {construct an SQL statment that returns an empty result set,
   this is used to get the field information }
  FQuery.SQL.Text := 'SELECT * FROM ' + aDataName +
                     ' WHERE ''c'' <> ''c'' ';
end; {procedure, SetDataName}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.BuildFieldList }

procedure TdaUniDACDataSet.BuildFieldList;
var
  liIndex: Integer;
  lQueryField: TField;
  lField: TppField;
begin
  inherited BuildFieldList;


  {set table to active}
  if not(GetQuery.Active) then
    FQuery.Active := True;

  {create TppField objects for each field in the table}
  for liIndex := 0 to FQuery.FieldCount - 1 do begin
    lQueryField := FQuery.Fields[liIndex];

    lField := TppField.Create(nil);

    lField.TableName    := DataName;
    lField.FieldName    := lQueryField.FieldName;
    lField.DataType     := ppConvertFieldType(lQueryField.DataType);

    AddField(lField);
  end;
end; {function, BuildFieldList}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.GetFieldNamesForSQL }

procedure TdaUniDACDataSet.GetFieldNamesForSQL(aList: TStrings; aSQL: TStrings);
var
  lQuery: TUniQuery;
begin
  aList.Clear;

  {create a temporary query}
  lQuery := TUniQuery.Create(Self);

  {set the database and SQL properties}
  lQuery.Connection := FConnection;
  lQuery.SQL := aSQL;

  {get the field names}
  lQuery.GetFieldNames(aList);

  lQuery.Free;
end; {procedure, GetFieldNamesForSQL}

{------------------------------------------------------------------------------}
{ TdaUniDACDataSet.GetFieldsForSQL }

procedure TdaUniDACDataSet.GetFieldsForSQL(aList: TList; aSQL: TStrings);
var
  lQuery: TUniQuery;
  lQueryField: TField;
  lField: TppField;
  liIndex: Integer;
begin
  aList.Clear;

  {create a temporary query}
  lQuery := TUniQuery.Create(Self);

  {assign databae and SQL properties}
  lQuery.Connection := FConnection;
  lQuery.SQL := aSQL;

  {set query to active}
  lQuery.Active := True;

  {create a TppField object for each field in the query}
  for liIndex := 0 to lQuery.FieldCount - 1 do begin
    lQueryField := lQuery.Fields[liIndex];

    lField := TppField.Create(nil);

    lField.FieldName    := lQueryField.FieldName;
    lField.DataType     := ppConvertFieldType(lQueryField.DataType);

    aList.Add(lField);
  end;

  lQuery.Free;
end; {procedure, GetFieldsForSQL}



{******************************************************************************
 *
 ** U N I D A C  Q U E R Y   D A T A V I E W
 *
{******************************************************************************}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.Create }

constructor TdaUniDACQueryDataView.Create(aOwner: TComponent);
begin

  inherited Create(aOwner);

  {notes: 1. must use ChildQuery, ChildDataSource, ChildPipeline etc.
          2. use Self as owner for Query, DataSource etc.
          3. do NOT assign a Name }

  FQuery := TdaChildUniDACQuery.Create(Self);

  FDataSource := TppChildDataSource.Create(Self);
  FDataSource.DataSet := FQuery;

end; {constructor, Create}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.Destroy }

destructor TdaUniDACQueryDataView.Destroy;
begin
  FDataSource.Free;
  FQuery.Free;

  inherited Destroy;

end; {destructor, Destroy}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.SessionClass }

class function TdaUniDACQueryDataView.SessionClass: TClass;
begin
  Result := TdaUniDACSession;
end; {class function, SessionClass}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.ConnectPipelinesToData }

procedure TdaUniDACQueryDataView.ConnectPipelinesToData;
begin

  if DataPipelineCount = 0 then Exit;

  {need to reconnect here}
  TppDBPipeline(DataPipelines[0]).DataSource := FDataSource;

end; {procedure, ConnectPipelinesToData}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.Init }

procedure TdaUniDACQueryDataView.Init;
var
  lDataPipeline: TppChildDBPipeline;

begin

  inherited Init;

  if DataPipelineCount > 0 then Exit;

  {note: DataView's owner must own the DataPipeline }
  lDataPipeline := TppChildDBPipeline(ppComponentCreate(Self, TppChildDBPipeline));
  lDataPipeline.DataSource := FDataSource;
 
  lDataPipeline.AutoCreateFields := False;

  {add DataPipeline to the dataview }
  lDataPipeline.DataView := Self;

end; {procedure, Init}

{------------------------------------------------------------------------------}
{ TdaUniDACQueryDataView.SQLChanged }

procedure TdaUniDACQueryDataView.SQLChanged;
var
  lDatabase: TComponent;
begin

  if FQuery.Active then
    FQuery.Close;

  lDatabase := Session.GetDatabaseForName(SQL.DatabaseName);

  if (lDatabase = nil) then
    raise EDataError.Create('TdaUniDACQueryDataView.SQLChanged: No MSConnection object found, ' + SQL.DatabaseName);

  FQuery.Connection := TUniConnection(lDatabase);
  FQuery.SQL := SQL.MagicSQLText;

end; {procedure, SQLChanged}




initialization
  {register the UniDAC descendant classes}
  RegisterClasses([TdaChildUniDACQuery, TdaChildUniDACTable, TdaChildUniDACStoredProc]);

  {register DADE descendant session, dataset, dataview}
  daRegisterSession(TdaUniDACSession);
  daRegisterDataSet(TdaUniDACDataSet);
  daRegisterDataView(TdaUniDACQueryDataView);

  {initialize internal reference variables}
  uUniDACConnection     := nil;

finalization
  {free the default connection object}
  uUniDACConnection.Free;

  {unregister the UniDAC descendant classes}
  UnRegisterClasses([TdaChildUniDACQuery, TdaChildUniDACTable, TdaChildUniDACStoredProc]);

  {unregister DADE descendant the session, dataset, dataview}
  daUnRegisterSession(TdaUniDACSession);
  daUnRegisterDataSet(TdaUniDACDataSet);
  daUnRegisterDataView(TdaUniDACQueryDataView);
end.
