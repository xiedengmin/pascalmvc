{$IFNDEF CLR}
{$I Dac.inc}
unit DBToolsIntf;
{$ENDIF}
interface

uses
{$IFDEF CLR}
  System.Runtime.InteropServices,
  Variants,
  CoreLab.DbTools;
{$ELSE}
  Windows, ActiveX;
{$ENDIF}

type
{$IFDEF WIN32_64}
  ParameterType = TOleEnum;
{$ELSE}
  TConnectionInfoArray = array of ConnectionInfo;
{$ENDIF}
// Constants for enum ParameterType
const
  ParameterType_Input = ParameterType(0);
  ParameterType_InputOutput = ParameterType(1);
  ParameterType_Output = ParameterType(2);
  ParameterType_ReturnValue = ParameterType(3);

  Devart_DbTools_InterfacesMajorVersion = 2;
  Devart_DbTools_InterfacesMinorVersion = 0;

// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
{$IFDEF WIN32_64}
type
  ConnectionInfo = packed record
    Name: WideString;
    ConnectionString: WideString;
  end;

  CommandParameterInfo = packed record
    Name: PWideChar;
    ParameterType: ParameterType;
    DataType: Integer;
    _slack_space: dword; // Align by 16 bytes
    Value: OleVariant;
  end;

  TConnectionInfoArray = PSafeArray;

// *********************************************************************//
// Interface: ISqlEditor
// Flags:     (256) OleAutomation
// GUID:      {471AE99C-AE21-491B-89E3-A2C717C81CA9}
// Simple interface of the SQL editor.
// *********************************************************************//
  ISqlEditor = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
    ['{471AE99C-AE21-491B-89E3-A2C717C81CA9}']
    function get_Handle: integer; stdcall;
    function get_Text: PWideChar; stdcall;
    procedure set_Text(const Param1: PWideChar); stdcall;
    function get_Modified: LongBool; stdcall;
    procedure set_Modified(Param1: LongBool); stdcall;
    function get_ReadOnly: LongBool; stdcall;
    procedure set_ReadOnly(Param1: LongBool); stdcall;

    procedure SetConnection(const ConnectionString: WideString); stdcall;
    property Modified: LongBool read get_Modified write set_Modified;
    property Handle: integer read get_Handle;
    property ReadOnly: LongBool read get_ReadOnly write set_ReadOnly;
    property Text: PWideChar read get_Text write set_Text;
  end;

// *********************************************************************//
// Interface: ISqlSource
// Flags:     (256) OleAutomation
// GUID:      {471AE99C-AE21-491B-89E3-A2C717C81CA8}
// Describes any component with editable SQL query on the designer. A 'Command' for example.
// *********************************************************************//
  ISqlSource = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
    ['{471AE99C-AE21-491B-89E3-A2C717C81CA8}']
    function Get_Name: PWideChar; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ConnectionString: PWideChar; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_DesignerName: PWideChar; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ParameterCount: Integer; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_ParameterCount(const Param1: Integer); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_Sql: PWideChar; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_Sql(const Param1: PWideChar); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Close; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure GetParameter(const index: Integer; out Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure SetParameter(const index: Integer; Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ID: PWideChar; {$IFDEF WIN32_64}stdcall;{$ENDIF}
  end;

// *********************************************************************//
// Interface: ISqlSourceNotifier
// Flags:     (256) OleAutomation
// GUID:      {D4858B82-70FB-4E30-8BE5-30CF625A37E2}
// Feedback interface for the SQL service. Allows to notify about any componet changings.
// *********************************************************************//
  ISqlSourceNotifier = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
    ['{D4858B82-70FB-4E30-8BE5-30CF625A37E2}']
    procedure OnSqlSourceChanged; stdcall;
    procedure OnSqlSourceDeleted; stdcall;
    procedure OnSqlSourceRenamed(const prevName: WideString); stdcall;
  end;

// *********************************************************************//
// Interface: IDbToolsService
// Flags:     (256) OleAutomation
// GUID:      {7FB8EF3F-AB68-48D1-9DD5-F85C05DF4A90}
// An interface of the service, that provides basics functionalities of editing or executing an SQL query.
// *********************************************************************//
  IDbToolsService = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
    ['{7FB8EF3F-AB68-48D1-9DD5-F85C05DF4A90}']
    procedure CreateSqlEditor(out editor: ISqlEditor); stdcall;
    procedure DesignerClosing(const DesignerName: WideString); stdcall;
    procedure EditDatabaseObject(const ConnectionString: WideString; const objectType: WideString;
                                 const fullName: WideString); stdcall;
    procedure EditSql(const sqlSource: ISqlSource; asQuery: LongBool;
                      out notifier: ISqlSourceNotifier); stdcall;
    procedure ExecuteSql(const sqlSource: ISqlSource; debug: LongBool); stdcall;
    procedure FindInDatabaseExplorer(const ConnectionString: WideString;
                                     const objectType: WideString; const fullName: WideString); stdcall;
    function GetConnections: PSafeArray; stdcall;
    procedure RetrieveData(const sqlSource: ISqlSource; asDocument: LongBool); stdcall;
  end;

// *********************************************************************//
// Interface: IDbForgeServiceEvents
// Flags:     (256) OleAutomation
// GUID:      {55CBF937-D3F9-486f-B34A-A435F4B9FCCC}
// Simple interface of the SQL editor.
// *********************************************************************//
 IDbForgeServiceEvents = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
  ['{55CBF937-D3F9-486f-B34A-A435F4B9FCCC}']
  procedure OnDeactivate; {$IFDEF WIN32_64}stdcall;{$ENDIF}
 end;

// *********************************************************************//
// Interface: IDbForgeService
// Flags:     (256) OleAutomation
// GUID:      {2A4633FC-8980-4f4f-BBE6-B300670748FB}
// An interface of the service, that provides basics functionalities of editing or executing an SQL query.
// *********************************************************************//
  IDbForgeService = interface{$IFNDEF CLR}(IUnknown){$ENDIF}
    ['{2A4633FC-8980-4f4f-BBE6-B300670748FB}']
    procedure CreateSqlEditor(const events: IDbForgeServiceEvents; out editor: ISqlEditor); stdcall;
    procedure DesignerClosing(const DesignerName: WideString); stdcall;
    procedure EditDatabaseObject(const ConnectionString: WideString; const objectType: WideString;
                                 const fullName: WideString); stdcall;
    procedure EditSql(const sqlSource: ISqlSource; asQuery: LongBool;
                      out notifier: ISqlSourceNotifier); stdcall;
    procedure ExecuteSql(const sqlSource: ISqlSource; debug: LongBool; out notifier: ISqlSourceNotifier); stdcall;
    procedure FindInDatabaseExplorer(const ConnectionString: WideString;
                                     const objectType: WideString; const fullName: WideString); stdcall;
    function GetConnections: {$IFDEF WIN32_64}PSafeArray{$ELSE}TConnectionInfoArray{$ENDIF}; stdcall;
    procedure RetrieveData(const sqlSource: ISqlSource; asDocument: LongBool); stdcall;
  end;

{$ENDIF}

implementation

end.
