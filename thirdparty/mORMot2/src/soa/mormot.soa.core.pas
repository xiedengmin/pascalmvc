/// Interface-based SOA Process Core Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.soa.core;

{
  *****************************************************************************

   Shared Interface-based Service Oriented Architecture (SOA) Process
    - TOrmServiceLog TOrmServiceNotifications Classes
    - TServiceFactory Abstract Service Provider
    - TServiceFactoryServerAbstract Abstract Service Provider
    - TServiceContainer Abstract Services Holder
    - SOA Related Interfaces
    - TServicesPublishedInterfacesList Services Catalog

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.lib.z, // use crc32() for contract hashing
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core;


{ ************ TOrmServiceLog TOrmServiceNotifications Classes }

type
  /// common ancestor for storing interface-based service execution statistics
  // - each call could be logged and monitored in the database
  // - TServiceMethodExecute could store all its calls in such a table
  // - enabled on server side via either TServiceFactoryServer.SetServiceLog or
  // TServiceContainerServer.SetServiceLog method
  TOrmServiceLog = class(TOrmNoCaseExtended)
  protected
    fMethod: RawUtf8;
    fInput: variant;
    fOutput: variant;
    fUser: integer;
    fSession: integer;
    fTime: TModTime;
    fMicroSec: integer;
    fIP: RawUtf8;
  public
    /// overriden method creating an index on the Method/MicroSec columns
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  published
    /// the 'interface.method' identifier of this call
    // - this column will be indexed, for fast SQL queries, with the MicroSec
    // column (for performance tuning)
    property Method: RawUtf8
      read fMethod write fMethod;
    /// the input parameters, as a JSON document
    // - will be stored in JSON_FAST_EXTENDED format, i.e. with
    // shortened field names, for smaller TEXT storage
    // - content may be searched using JsonGet/JsonHas SQL functions on a
    // SQlite3 storage, or with direct document query under MongoDB/PostgreSQL
    property Input: variant
      read fInput write fInput;
    /// the output parameters, as a JSON document, including result: for a function
    // - will be stored in JSON_FAST_EXTENDED format, i.e. with
    // shortened field names, for smaller TEXT storage
    // - content may be searched using JsonGet/JsonHas SQL functions on a
    // SQlite3 storage, or with direct document query under MongoDB/PostgreSQL
    property Output: variant
      read fOutput write fOutput;
    /// the Session ID, if there is any
    property Session: integer
      read fSession write fSession;
    /// the User ID, if there is an identified Session
    property User: integer
      read fUser write fUser;
    /// will be filled by the ORM when this record is written in the database
    property Time: TModTime
      read fTime write fTime;
    /// execution time of this method, in micro seconds
    property MicroSec: integer
      read fMicroSec write fMicroSec;
    /// if not localhost/127.0.0.1, the remote IP address
    property IP: RawUtf8
      read fIP write fIP;
  end;

  /// execution statistics used for DB-based asynchronous notifications
  // - as used by TServiceFactoryClient.SendNotifications
  // - here, the Output column may contain the information about an error
  // occurred during process
  TOrmServiceNotifications = class(TOrmServiceLog)
  protected
    fSent: TTimeLog;
  public
    /// this overriden method will create an index on the 'Sent' column
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
    /// search for pending events since a supplied ID
    // - returns FALSE if no notification was found
    // - returns TRUE ad fill a TDocVariant array of JSON Objects, including
    // "ID": field, and Method as "MethodName": field
    class function LastEventsAsObjects(const Rest: IRestOrm; LastKnownID: TID;
      Limit: integer; Service: TInterfaceFactory; out Dest: TDocVariantData;
      const MethodName: RawUtf8 = 'Method'; IDAsHexa: boolean = false): boolean;
    /// allows to convert the Input array into a proper single JSON Object
    // - "ID": field will be included, and Method as "MethodName": field
    function SaveInputAsObject(Service: TInterfaceFactory;
      const MethodName: RawUtf8 = 'Method'; IDAsHexa: boolean = false): variant; virtual;
    /// run FillOne and SaveInputAsObject into a TDocVariant array of JSON Objects
    // - "ID": field will be included, and Method as "MethodName": field
    procedure SaveFillInputsAsObjects(Service: TInterfaceFactory;
      out Dest: TDocVariantData; const MethodName: RawUtf8 = 'Method';
      IDAsHexa: boolean = false);
  published
    /// when this notification has been sent
    // - equals 0 until it was actually notified
    property Sent: TTimeLog
      read fSent write fSent;
  end;

  /// class-reference type (metaclass) for storing interface-based service
  // execution statistics
  // - you could inherit from TOrmServiceLog, and specify additional
  // fields corresponding to the execution context
  TOrmServiceLogClass = class of TOrmServiceLog;

  /// class-reference type (metaclass) for storing interface-based service
  // execution statistics used for DB-based asynchronous notifications
  // - as used by TServiceFactoryClient.SendNotifications
  TOrmServiceNotificationsClass = class of TOrmServiceNotifications;


{ ************ TServiceFactory Abstract Service Provider }

type
  /// exception dedicated to interface based service implementation
  EServiceException = class(ESynException);

  /// the possible Server-side instance implementation patterns for
  // interface-based services
  // - each interface-based service will be implemented by a corresponding
  // class instance on the server: this parameter is used to define how
  // class instances are created and managed
  // - on the Client-side, each instance will be handled depending on the
  // server side implementation (i.e. with sicClientDriven behavior if necessary)
  // - sicSingle: one object instance is created per call - this is the
  // most expensive way of implementing the service, but is safe for simple
  // workflows (like a one-type call); this is the default setting for
  // TRestServer.ServiceRegister method
  // - sicShared: one object instance is used for all incoming calls and is
  // not recycled subsequent to the calls - the implementation should be
  // thread-safe on the server side
  // - sicClientDriven: one object instance will be created in synchronization
  // with the client-side lifetime of the corresponding interface: when the
  // interface will be released on client, it will be released on the server
  // side - a numerical identifier will be transmitted for all JSON requests
  // - sicPerSession, sicPerUser and sicPerGroup modes will maintain one
  // object instance per running session / user / group (only working if
  // RESTful authentication is enabled) - since it may be shared among users or
  // groups, the sicPerUser and sicPerGroup implementation should be thread-safe
  // - sicPerThread will maintain one object instance per calling thread - it
  // may be useful instead of sicShared mode if the service process expects
  // some per-heavy thread initialization, for instance
  TServiceInstanceImplementation = (
    sicSingle,
    sicShared,
    sicClientDriven,
    sicPerSession,
    sicPerUser,
    sicPerGroup,
    sicPerThread);

  /// set of Server-side instance implementation patterns for
  // interface-based services
  TServiceInstanceImplementations = set of TServiceInstanceImplementation;

  /// how TServiceFactoryServer.SetOptions() will set the options value
  TServiceMethodOptionsAction = (
    moaReplace,
    moaInclude,
    moaExclude);

  /// internal per-method list of execution context as hold in TServiceFactory
  TServiceFactoryExecution = record
    /// the list of denied TAuthGroup ID(s)
    // - used on server side within TRestServerUriContext.ExecuteSoaByInterface
    // - bit 0 for client TAuthGroup.ID=1 and so on...
    // - is therefore able to store IDs up to 256 (maximum bit of 255 is a
    // limitation of the pascal compiler itself)
    // - void by default, i.e. no denial = all groups allowed for this method
    Denied: set of 0..255;
    /// execution options for this method (about thread safety or logging)
    Options: TInterfaceMethodOptions;
    /// where execution information should be written as TOrmServiceLog
    // - is a weak pointer to a IRestOrm instance to avoid reference counting
    LogRest: pointer;
    /// the TOrmServiceLog class to use, as defined in LogRest.Model
    LogClass: TOrmServiceLogClass;
  end;

  /// points to the execution context of one method within TServiceFactory
  PServiceFactoryExecution = ^TServiceFactoryExecution;

  /// several method execution execution contexts
  TServiceFactoryExecutionDynArray = array of TServiceFactoryExecution;

  /// an abstract service provider, as registered in TServiceContainer
  // - each registered interface has its own TServiceFactory instance, available
  // as one TSqlServiceContainer item from TRest.Services property
  // - this will be either implemented by a registered TInterfacedObject on the
  // server, or by a on-the-fly generated fake TInterfacedObject class
  // communicating via JSON on a client
  // - TRestServer will have to register an interface implementation as:
  // ! Server.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
  // - TRestClientUri will have to register an interface remote access as:
  // ! Client.ServiceRegister([TypeInfo(ICalculator)],sicShared));
  // note that the implementation (TServiceCalculator) remain on the server side
  // only: the client only needs the ICalculator interface
  // - then TRestServer and TRestClientUri will both have access to the
  // service, via their Services property, e.g. as:
  // !var
  // !  I: ICalculator;
  // !...
  // ! if Services.Info(ICalculator).Get(I) then
  // !   result := I.Add(10,20);
  // which is in practice to be used with the faster wrapper method:
  // ! if Services.Resolve(ICalculator,I) then
  // !   result := I.Add(10,20);
  TServiceFactory = class(TInjectableObject)
  protected
    fInterface: TInterfaceFactory;
    fInterfaceUri: RawUtf8;
    fInterfaceMangledUri: RawUtf8;
    fInstanceCreation: TServiceInstanceImplementation;
    fOrm: IRestOrm;
    fSharedInstance: TInterfacedObject;
    fContract: RawUtf8;
    fContractHash: RawUtf8;
    fContractExpected: RawUtf8;
    fExecution: TServiceFactoryExecutionDynArray;
    /// union of all fExecution[].Options
    fAnyOptions: TInterfaceMethodOptions;
    procedure ExecutionAction(const aMethod: array of RawUtf8;
      aOptions: TInterfaceMethodOptions; aAction: TServiceMethodOptionsAction);
    function GetInterfaceTypeInfo: PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInterfaceIID: TGUID;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the service provider parameters
    // - it will check and retrieve all methods of the supplied interface,
    // and prepare all internal structures for its serialized execution
    // - supplied TInterfaceResolver should be able to resolve IRestOrm,
    // and is typically a TRest instance
    constructor Create(aOwner: TInterfaceResolver; aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      const aContractExpected: RawUtf8); reintroduce;
    /// retrieve an instance of this interface
    // - this virtual method will be overridden to reflect the expected
    // behavior of client or server side
    // - can be used as such to resolve an I: ICalculator interface:
    // ! var I: ICalculator;
    // ! begin
    // !   if fClient.Services.Info(TypeInfo(ICalculator)).Get(I) then
    // !   ... use I
    function Get(out Obj): boolean; virtual; abstract;
    /// retrieve the published signature of this interface
    // - is always available on TServiceFactoryServer, but TServiceFactoryClient
    // will be able to retrieve it only if TServiceContainerServer.PublishSignature
    // is set to TRUE (which is not the default setting, for security reasons)
    function RetrieveSignature: RawUtf8; virtual; abstract;
    /// access to the registered Interface RTTI information
    property InterfaceFactory: TInterfaceFactory
      read fInterface;
    /// the registered Interface low-level Delphi RTTI type
    // - just maps InterfaceFactory.InterfaceTypeInfo
    property InterfaceTypeInfo: PRttiInfo
      read GetInterfaceTypeInfo;
    /// the registered Interface GUID
    // - just maps InterfaceFactory.InterfaceIID
    property InterfaceIID: TGUID
      read GetInterfaceIID;
    /// the service contract, serialized as a JSON object
    // - a "contract" is in fact the used interface signature, i.e. its
    //   implementation mode (InstanceCreation) and all its methods definitions
    // - a possible value for a one-method interface defined as such:
    // ! function ICalculator.Add(n1,n2: integer): integer;
    // may be returned as the following JSON object:
    // $ {"contract":"Calculator","implementation":"shared",
    // $  "methods":[{"method":"Add",
    // $  "arguments":[{"argument":"self","direction":"in","type":"self"},
    // $               {"argument":"n1","direction":"in","type":"integer"},
    // $               {"argument":"n2","direction":"in","type":"integer"},
    // $               {"argument":"Result","direction":"out","type":"integer"}
    // $ ]}]}
    property Contract: RawUtf8
      read fContract;
    /// the published service contract, as expected by both client and server
    // - by default, will contain ContractHash property value (for security)
    // - but you can override this value using plain Contract or any custom
    // value (e.g. a custom version number) - in this case, both TServiceFactoryClient
    // and TServiceFactoryServer instances must have a matching ContractExpected
    // - this value is returned by a '_contract_' pseudo-method name, with the URI:
    // $ POST /root/Interface._contract_
    // or (if TRestRoutingJsonRpc is used):
    // $ POST /root/Interface
    // $ (...)
    // $ {"method":"_contract_","params":[]}
    // (e.g. to be checked in TServiceFactoryClient.Create constructor)
    // - if set to SERVICE_CONTRACT_NONE_EXPECTED (i.e. '*'), the client won't
    // check and ask the server contract for consistency: it may be used e.g.
    // for accessing a plain REST HTTP server which is not based on mORMot,
    // so may not implement POST /root/Interface._contract_
    property ContractExpected: RawUtf8
      read fContractExpected write fContractExpected;
    /// direct access to the low-level per-method execution rights
    property Execution: TServiceFactoryExecutionDynArray
      read fExecution;
  published
    /// access to the associated TRest ORM instance
    property ORM: IRestOrm
      read fOrm;
  published
    /// the registered Interface URI
    // - in fact this is the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator
    property InterfaceUri: RawUtf8
      read fInterfaceUri;
    /// the registered Interface mangled URI
    // - in fact this is encoding the GUID using BinToBase64Uri(), e.g.
    // ! ['{c9a646d3-9c61-4cb7-bfcd-ee2522c8f633}'] into '00amyWGct0y_ze4lIsj2Mw'
    // - can be substituted to the clear InterfaceUri name
    property InterfaceMangledUri: RawUtf8
      read fInterfaceMangledUri;
    /// how each class instance is to be created
    // - only relevant on the server side; on the client side, this class will
    // be accessed only to retrieve a remote access instance, i.e. sicSingle
    property InstanceCreation: TServiceInstanceImplementation
      read fInstanceCreation;
    /// a hash of the service contract, serialized as a JSON string
    // - this may be used instead of the JSON signature, to enhance security
    // (i.e. if you do not want to publish the available methods, but want
    // to check for the proper synchronization of both client and server)
    // - a possible value may be: "C351335A7406374C"
    property ContractHash: RawUtf8
      read fContractHash;
  end;

const
  /// the Server-side instance implementation patterns without any ID
  SERVICE_IMPLEMENTATION_NOID = [sicSingle, sicShared];


function ToText(si: TServiceInstanceImplementation): PShortString; overload;



{ ************ TServiceFactoryServerAbstract Abstract Service Provider }

type
  /// abstract TServiceFactoryServer parent with a fluent interface for options
  // - defining methods to customize the service implementation on Server side
  // - as returned by TRestServer.ServiceDefine and ServiceRegister overloaded methods
  // - by default, all methods are allowed to execution: you can call AllowAll,
  // DenyAll, Allow or Deny in order to specify your exact security policy
  // - those methods returns the self instance to provide a fluent interface
  // - defined here to avoid circular references between mormot.rest.server.pas
  // and mormot.soa.server.pas
  TServiceFactoryServerAbstract = class(TServiceFactory)
  protected
    fByPassAuthentication: boolean;
    fResultAsJsonObject: boolean;
    fResultAsJsonObjectWithoutResult: boolean;
    fResultAsXMLObject: boolean;
    fResultAsXMLObjectIfAcceptOnlyXML: boolean;
    fResultAsXMLObjectNameSpace: RawUtf8;
    fExcludeServiceLogCustomAnswer: boolean;
    function GetAuthGroupIDs(const aGroup: array of RawUtf8;
      out IDs: TIDDynArray): boolean;
  public
    /// allow all methods execution for all TAuthGroup
    // - all Groups will be affected by this method (on both client and server sides)
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function AllowAll: TServiceFactoryServerAbstract;
    /// allow all methods execution for the specified TAuthGroup ID(s)
    // - the specified group ID(s) will be used to authorize remote service
    // calls from the client side
    // - you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fServer.MainFieldID(TAuthGroup,'User');
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function AllowAllByID(const aGroupID: array of TID): TServiceFactoryServerAbstract;
    /// allow all methods execution for the specified TAuthGroup names
    // - is just a wrapper around the other AllowAllByID() method, retrieving the
    // Group ID from its main field
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function AllowAllByName(const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
    /// deny all methods execution for all TAuthGroup
    // - all Groups will be affected by this method (on both client and server sides)
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function DenyAll: TServiceFactoryServerAbstract;
    /// deny all methods execution for the specified TAuthGroup ID(s)
    // - the specified group ID(s) will be used to authorize remote service
    // calls from the client side
    // - you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fServer.MainFieldID(TAuthGroup,'User');
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function DenyAllByID(const aGroupID: array of TID): TServiceFactoryServerAbstract;
    /// dent all methods execution for the specified TAuthGroup names
    // - is just a wrapper around the other DenyAllByID() method, retrieving the
    // Group ID from its main field
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function DenyAllByName(const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
    /// allow specific methods execution for the all TAuthGroup
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - all Groups will be affected by this method (on both client and server sides)
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function Allow(const aMethod: array of RawUtf8): TServiceFactoryServerAbstract;
    /// allow specific methods execution for the specified TAuthGroup ID(s)
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - the specified group ID(s) will be used to authorize remote service
    // calls from the client side
    // - you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fServer.MainFieldID(TAuthGroup,'User');
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function AllowByID(const aMethod: array of RawUtf8;
      const aGroupID: array of TID): TServiceFactoryServerAbstract;
    /// allow specific methods execution for the specified TAuthGroup name(s)
    // - is just a wrapper around the other AllowByID() method, retrieving the
    // Group ID from its main field
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function AllowByName(const aMethod: array of RawUtf8;
      const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
    /// deny specific methods execution for the all TAuthGroup
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - all Groups will be affected by this method (on both client and server sides)
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function Deny(const aMethod: array of RawUtf8): TServiceFactoryServerAbstract;
    /// deny specific methods execution for the specified TAuthGroup ID(s)
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - the specified group ID(s) will be used to unauthorize remote service
    // calls from the client side
    // - you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fServer.MainFieldID(TAuthGroup,'User');
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function DenyByID(const aMethod: array of RawUtf8;
      const aGroupID: array of TID): TServiceFactoryServerAbstract; overload;
    /// deny specific methods execution for the specified TAuthGroup name(s)
    // - is just a wrapper around the other DenyByID() method, retrieving the
    // Group ID from its main field
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function DenyByName(const aMethod: array of RawUtf8;
      const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
    /// define execution options for a given set of methods
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - if no method name is given (i.e. []), option will be set for all methods
    // - include optExecInMainThread will force the method(s) to be called within
    // a RunningThread.Synchronize() call - slower, but thread-safe
    // - this method returns self in order to allow direct chaining of settings
    // calls, in a fluent interface
    function SetOptions(const aMethod: array of RawUtf8;
      aOptions: TInterfaceMethodOptions;
      aAction: TServiceMethodOptionsAction = moaReplace): TServiceFactoryServerAbstract;
    /// define execution options for the whole interface
    // - fluent alternative of setting homonymous boolean properties of this class
    // - this method returns self in order to allow direct chaining of settings
    function SetWholeOptions(aOptions: TInterfaceOptions): TServiceFactoryServerAbstract; 
    /// define the instance life time-out, in seconds
    // - for sicClientDriven, sicPerSession, sicPerUser or sicPerGroup modes
    // - raise an exception for other kind of execution
    // - this method returns self in order to allow direct chaining of setting
    // calls for the service, in a fluent interface
    function SetTimeoutSec(value: cardinal): TServiceFactoryServerAbstract;
      virtual; abstract;
    /// log method execution information to a TOrmServiceLog table
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - if no method name is given (i.e. []), option will be set for all methods
    // - will write to the specified aLogRest instance, and will disable
    // writing if aLogRest is nil
    // - will write to a (inherited) TOrmServiceLog table, as available in
    // TRest's model, unless a dedicated table is specified as aLogClass
    // - this method returns self in order to allow direct chaining of security
    // calls, in a fluent interface
    function SetServiceLog(const aMethod: array of RawUtf8;
      const aLogRest: IRestOrm;
      aLogClass: TOrmServiceLogClass = nil): TServiceFactoryServerAbstract;
      virtual; abstract;
    /// set to TRUE disable Authentication method check for the whole interface
    // - by default (FALSE), all interface-based services will require valid
    // RESTful authentication (if enabled on the server side); setting TRUE will
    // disable authentication for all methods of this interface
    // (e.g. for returning some HTML content from a public URI, or to implement
    // a public service catalog)
    property ByPassAuthentication: boolean
      read fByPassAuthentication write fByPassAuthentication;
    /// set to TRUE to return the interface's methods result as JSON object
    // - by default (FALSE), any method execution will return a JSON array with
    // all VAR/OUT parameters, in order
    // - TRUE will generate a JSON object instead, with the VAR/OUT parameter
    // names as field names (and "Result" for any function result) - may be
    // useful e.g. when working with JavaScript clients
    // - Delphi clients (i.e. TServiceFactoryClient/TInterfacedObjectFake) will
    // transparently handle both formats
    // - this value can be overridden by setting ForceServiceResultAsJsonObject
    // for a given TRestServerUriContext (e.g. for server-side JavaScript work)
    property ResultAsJsonObject: boolean
      read fResultAsJsonObject write fResultAsJsonObject;
    /// set to TRUE to return the interface's methods result as JSON object
    // with no '{"result":{...}}' nesting
    // - could be used e.g. for plain non mORMot REST Client with in sicSingle
    // or sicShared mode kind of services
    // - on client side, consider using TRestClientUri.ServiceDefineSharedApi
    property ResultAsJsonObjectWithoutResult: boolean
      read fResultAsJsonObjectWithoutResult write fResultAsJsonObjectWithoutResult;
    /// set to TRUE to return the interface's methods result as XML object
    // - by default (FALSE), method execution will return a JSON array with
    // all VAR/OUT parameters, or a JSON object if ResultAsJsonObject is TRUE
    // - TRUE will generate a XML object instead, with the VAR/OUT parameter
    // names as field names (and "Result" for any function result) - may be
    // useful e.g. when working with some XML-only clients
    // - Delphi clients (i.e. TServiceFactoryClient/TInterfacedObjectFake) does
    // NOT handle this XML format yet
    // - this value can be overridden by setting ForceServiceResultAsXMLObject
    // for a given TRestServerUriContext instance
    property ResultAsXMLObject: boolean
      read fResultAsXMLObject write fResultAsXMLObject;
    /// set to TRUE to return XML objects for the interface's methods result
    // if the Accept: HTTP header is exactly 'application/xml' or 'text/xml'
    // - the header should be exactly 'Accept: application/xml' or
    // 'Accept: text/xml' (and no other value)
    // - in this case, ForceServiceResultAsXMLObject will be set for this
    // particular TRestServerUriContext instance, and result returned as XML
    // - using this method allows to mix standard JSON requests (from JSON
    // or AJAX clients) and XML requests (from XML-only clients)
    property ResultAsXMLObjectIfAcceptOnlyXML: boolean
      read fResultAsXMLObjectIfAcceptOnlyXML write fResultAsXMLObjectIfAcceptOnlyXML;
    /// specify a custom name space content when returning a XML object
    // - by default, no name space will be appended - but such rough XML will
    // have potential validation problems
    // - you may use e.g. XMLUTF8_NAMESPACE, which will append <content ...> ...
    // </content> around the generated XML data
    property ResultAsXMLObjectNameSpace: RawUtf8
      read fResultAsXMLObjectNameSpace write fResultAsXMLObjectNameSpace;
    /// disable base64-encoded TOrmServiceLog.Output for methods
    // returning TServiceCustomAnswer record (to reduce storage size)
    property ExcludeServiceLogCustomAnswer: boolean
      read fExcludeServiceLogCustomAnswer write fExcludeServiceLogCustomAnswer;
  end;



{ ************ TServiceContainer Abstract Services Holder }

type
  /// used to lookup one service in a global list of interface-based services
  TServiceContainerInterface = record
    /// one 'service' item, as set at URI, e.g. 'Calculator'
    InterfaceName: RawUtf8;
    /// the associated service provider
    Service: TServiceFactory;
  end;

  /// pointer to one  lookup in a global list of interface-based services
  PServiceContainerInterface = ^TServiceContainerInterface;

  /// used to store all s in a global list of interface-based services
  TServiceContainerInterfaces = array of TServiceContainerInterface;

  /// used to lookup one method in a global list of interface-based services
  TServiceContainerInterfaceMethod = record
    /// one 'service.method' item, as set at URI
    // - e.g.'Calculator.Add','Calculator.Multiply'...
    InterfaceDotMethodName: RawUtf8;
    /// the associated service provider
    InterfaceService: TServiceFactory;
    /// the index of the method for the given service
    // - 0..2 indicates _free_/_contract_/_signature_ pseudo-methods
    // - then points to InterfaceService.Interface.Methods[InterfaceMethodIndex-3]
    InterfaceMethodIndex: integer;
  end;

  /// pointer to one method lookup in a global list of interface-based services
  PServiceContainerInterfaceMethod = ^TServiceContainerInterfaceMethod;

  /// used to store all methods in a global list of interface-based services
  TServiceContainerInterfaceMethods = array of TServiceContainerInterfaceMethod;

  /// used in TServiceContainer to identify fListInterfaceMethod[] entries
  // - maximum bit count of 255 is a limitation of the pascal compiler itself
  TServiceContainerInterfaceMethodBits = set of 0..255;

  /// a global services provider class
  // - used to maintain a list of interfaces implementation
  // - inherits from TInterfaceResolverInjected and its Resolve() methods,
  // compatible with TInjectableObject
  TServiceContainer = class(TInterfaceResolverInjected)
  protected
    fOwner: TInterfaceResolver; // is a TRest instance
    // list of services ['Calculator',...]
    fInterface: TServiceContainerInterfaces;
    fInterfaces: TDynArrayHashed;
    // list of service.method ['Calculator.Add','Calculator.Multiply',...]
    fInterfaceMethod: TServiceContainerInterfaceMethods;
    fInterfaceMethods: TDynArrayHashed;
    fExpectMangledUri: boolean;
    procedure SetExpectMangledUri(Mangled: boolean);
    procedure SetInterfaceMethodBits(MethodNamesCsv: PUtf8Char;
      IncludePseudoMethods: boolean; out bits: TServiceContainerInterfaceMethodBits);
    function GetMethodName(ListInterfaceMethodIndex: integer): RawUtf8;
    procedure CheckInterface(const aInterfaces: array of PRttiInfo);
    procedure ClearServiceList; virtual;
    function AddServiceInternal(aService: TServiceFactory): PtrInt; virtual;
    function AddServiceMethodInternal(const aInterfaceDotMethodName: RawUtf8;
      aService: TServiceFactory; var aMethodIndex: integer): PServiceContainerInterfaceMethod; virtual;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    /// retrieve a service provider from its URI
    function GetService(const aUri: RawUtf8): TServiceFactory;
  public
    /// initialize the Services list
    // - supplied TInterfaceResolver should be able to resolve IRestOrm,
    // and is typically a TRest instance
    constructor Create(aOwner: TInterfaceResolver); virtual;
    /// release all registered services
    destructor Destroy; override;
    /// return the number of registered service interfaces
    // - you can use InterfaceList[] to access the instances
    function Count: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a service provider from its index in the list
    // - returns nil if out of range index
    function Index(aIndex: integer): TServiceFactory; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a service provider from its GUID / Interface type
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...])
    // - on match, it will return the service the corresponding interface factory
    // - returns nil if the GUID does not match any registered interface
    // - can be used as such to resolve an I: ICalculator interface
    // ! if fClient.Services.Info(ICalculator).Get(I) then
    // !   ... use I
    {$ifdef FPC_HAS_CONSTREF}
    function Info(constref aGuid: TGUID): TServiceFactory; overload;
    {$else}
    function Info(const aGuid: TGUID): TServiceFactory; overload;
    {$endif FPC_HAS_CONSTREF}
    /// retrieve a service provider from its type information
    // - on match, it  will return the service the corresponding interface factory
    // - returns nil if the type information does not match any registered interface
    // - can be used as such to resolve an I: ICalculator interface
    // ! if fClient.Services.Info(TypeInfo(ICalculator)).Get(I) then
    // !   ... use I
    // - is defined as virtual so that e.g. TServiceContainerClient will
    // automatically register the interface, if it was not already done
    function Info(aTypeInfo: PRttiInfo): TServiceFactory; overload; virtual;
    /// notify the other side that the given Callback event interface is released
    // - this default implementation will do nothing
    function CallBackUnRegister(const Callback: IInvokable): boolean; virtual;
    /// retrieve all registered Services TGUID
    procedure SetGUIDs(out Services: TGuidDynArray);
    /// retrieve all registered Services names
    // - i.e. all interface names without the initial 'I', e.g. 'Calculator' for
    // ICalculator
    procedure SetInterfaceNames(out Names: TRawUtf8DynArray);
    /// retrieve all registered Services contracts as a JSON array
    // - i.e. a JSON array of TServiceFactory.Contract JSON objects
    function AsJson: RawJson;
    /// retrieve a service provider from its URI
    // - it expects the supplied URI variable  to be e.g. '00amyWGct0y_ze4lIsj2Mw'
    // or 'Calculator', depending on the ExpectMangledUri property
    // - on match, it  will return the service the corresponding interface factory
    // - returns nil if the URI does not match any registered interface
    property Services[const aUri: RawUtf8]: TServiceFactory
      read GetService; default;
    /// direct access to the internal list of interfdce services ['Calculator',...]
    property InterfaceList: TServiceContainerInterfaces
      read fInterface;
    /// direct access to the internal list of service.method
    // - e.g. ['Calculator.Add','Calculator.Multiply',...]
    property InterfaceMethod: TServiceContainerInterfaceMethods
      read fInterfaceMethod;
    /// direct access to the internal list of service.method wrapper
    property InterfaceMethods: TDynArrayHashed
      read fInterfaceMethods;
    /// the associated TRest instance, owning these services
    property Owner: TInterfaceResolver
      read fOwner;
    /// set if the URI is expected to be mangled from the GUID
    // - by default (FALSE), the clear service name is expected to be supplied at
    // the URI level (e.g. 'Calculator')
    // - if this property is set to TRUE, the mangled URI value will be expected
    // instead (may enhance security) - e.g. '00amyWGct0y_ze4lIsj2Mw'
    property ExpectMangledUri: boolean
      read fExpectMangledUri write SetExpectMangledUri;
  end;


{ ************ SOA Related Interfaces }

type
  /// prototype of a class implementing redirection of a given interface
  // - as returned e.g. by TRest.MultiRedirect method
  // - can be used as a main callback, then call Redirect() to manage
  // an internal list of redirections
  // - when you release this instance, will call Rest.Service.CallbackUnregister
  // with the associated fake callback generated
  IMultiCallbackRedirect = interface
    ['{E803A30A-8C06-4BB9-94E6-EB87EACFE980}']
    /// add or remove an interface callback to the internal redirection list
    // - will register a callback if aSubscribe is true
    // - will unregister a callback if aSubscribe is false
    // - supplied aCallback shoud implement the expected interface GUID
    // - this method will be implemented as thread-safe
    // - you can specify some method names, or all methods redirection if []
    procedure Redirect(const aCallback: IInvokable;
      const aMethodsNames: array of RawUtf8; aSubscribe: boolean = true); overload;
    /// add or remove a class instance callback to the internal redirection list
    // - will register a callback if aSubscribe is true
    // - will unregister a callback if aSubscribe is false
    // - supplied aCallback instance should implement the expected interface GUID
    // - this method will be implemented as thread-safe
    // - you can specify some method names, or all methods redirection if []
    procedure Redirect(const aCallback: TInterfacedObject;
      const aMethodsNames: array of RawUtf8; aSubscribe: boolean = true); overload;
  end;

  /// service definition with a method which will be called when a callback
  // interface instance is released on the client side
  // - may be used to implement safe publish/subscribe mechanism using
  // interface callbacks, e.g. over WebSockets
  IServiceWithCallbackReleased = interface(IInvokable)
    ['{8D518FCB-62C3-42EB-9AE7-96ED322140F7}']
    /// will be called when a callback is released on the client side
    // - this method matches the TInterfaceFactory.MethodIndexCallbackReleased
    // signature, so that it will be called with the interface instance by
    // TServiceContainerServer.FakeCallbackRelease
    // - you may use it as such - see sample Project31ChatServer.dpr:
    // ! procedure TChatService.CallbackReleased(const callback: IInvokable;
    // !   const interfaceName: RawUtf8);
    // ! begin  // unsubscribe from fConnected: array of IChatCallback
    // !   if interfaceName = 'IChatCallback' then
    // !     InterfaceArrayDelete(fConnected, callback);
    // ! end;
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
  end;

  /// a callback interface used to notify a TOrm modification in real time
  // - will be used e.g. by TRestServer.RecordVersionSynchronizeSubscribeMaster()
  // - all methods of this interface will be called asynchronously when
  // transmitted via our WebSockets implementation, since they are defined as
  // plain procedures
  // - each callback instance should be private to a specific TOrm
  IServiceRecordVersionCallback = interface(IInvokable)
    ['{8598E6BE-3590-4F76-9449-7AF7AF4241B0}']
    /// this event will be raised on any Add on a versioned record
    // - the supplied JSON object will contain the TRecordVersion field
    procedure Added(const NewContent: RawJson);
    /// this event will be raised on any Update on a versioned record
    // - the supplied JSON object will contain the TRecordVersion field
    procedure Updated(const ModifiedContent: RawJson);
    /// this event will be raised on any Delete on a versioned record
    procedure Deleted(const ID: TID; const Revision: TRecordVersion);
    /// allow to optimize process for WebSockets "jumbo frame" items
    // - this method may be called with isLast=false before the first method
    // call of this interface, then with isLast=true after the call of the
    // last method of the "jumbo frame"
    // - match TInterfaceFactory.MethodIndexCurrentFrameCallback signature
    // - allow e.g. to create a temporary TRestBatch for jumbo frames
    // - if individual frames are received, this method won't be called
    procedure CurrentFrame(isLast: boolean);
  end;

  /// a list of callback interfaces to notify TOrm modifications
  // - you can use InterfaceArray*() wrapper functions to manage the list
  IServiceRecordVersionCallbackDynArray = array of IServiceRecordVersionCallback;



{ ***************** TServicesPublishedInterfacesList Services Catalog }

type
    /// a specialized UTF-8 string type, used for TRestServerUri storage
  // - URI format is 'address:port/root', but port or root are optional
  // - you could use TRestServerUri record to store and process it
  TRestServerUriString = type RawUtf8;

  /// a list of UTF-8 strings, used for TRestServerUri storage
  // - URI format is 'address:port/root', but port or root are optional
  // - you could use TRestServerUri record to store and process each item
  TRestServerUriStringDynArray = array of TRestServerUriString;

  /// used to access a TRestServer from its TRestServerUriString URI
  // - URI format is 'address:port/root', and may be transmitted as
  // TRestServerUriString text instances
  {$ifdef USERECORDWITHMETHODS}
  TRestServerUri = record
  {$else}
  TRestServerUri = object
  {$endif USERECORDWITHMETHODS}
  private
    function GetUri: TRestServerUriString;
    procedure SetUri(const Value: TRestServerUriString);
  public
    /// the TRestServer IP Address or DNS name
    Address: RawUtf8;
    /// the TRestServer IP port
    Port: RawUtf8;
    /// the TRestServer model Root
    Root: RawUtf8;
    /// returns TRUE if all field values do match, case insensitively
    function Equals(const other: TRestServerUri): boolean;
    /// property which allows to read or set the Address/Port/Root fields as
    // one UTF-8 text field (i.e. a TRestServerUriString instance)
    // - URI format is 'address:port/root', but port or root are optional
    property URI: TRestServerUriString
      read GetUri write SetURI;
  end;

  /// store a list of TRestServer URIs
  TRestServerUriDynArray = array of TRestServerUri;

  /// used to publish all Services supported by a TRestServer instance
  // - as expected by TRestServer.ServicesPublishedInterfaces
  // - can be serialized as a JSON object via RecordLoadJson/RecordSaveJson
  {$ifdef USERECORDWITHMETHODS}
  TServicesPublishedInterfaces = record
  {$else}
  TServicesPublishedInterfaces = object
  {$endif USERECORDWITHMETHODS}
  public
    /// how this TRestServer could be accessed
    PublicUri: TRestServerUri;
    /// the list of supported services names
    // - in fact this is the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator
    Names: TRawUtf8DynArray;
  end;

  /// store a list of published Services supported by a TRestServer instance
  TServicesPublishedInterfacesDynArray = array of TServicesPublishedInterfaces;

  /// used e.g. by TRestServer to store a list of TServicesPublishedInterfaces
  TServicesPublishedInterfacesList = class(TSynPersistentRWLightLock)
  private
    fDynArray: TDynArray;
    fDynArrayTimeoutTix: TDynArray;
    fTimeoutTix: TInt64DynArray;
    fTimeoutTixCount: integer;
    fLastPublishedJson: cardinal;
    fTimeOut: integer;
  public
    /// the internal list of published services
    // - the list is stored in-order, i.e. it will follow the RegisterFromJson()
    // execution order: the latest registrations will appear last
    List: TServicesPublishedInterfacesDynArray;
    /// how many items are actually stored in List[]
    Count: integer;
    /// initialize the storage
    // - an optional time out period, in milliseconds, may be defined - but the
    // clients should ensure that RegisterFromClientJson() is called in order
    // to refresh the list (e.g. from _contract_ HTTP body)
    constructor Create(aTimeoutMS: integer); reintroduce; virtual;
    /// add the JSON serialized TServicesPublishedInterfaces to the list
    // - called by TRestServerUriContext.InternalExecuteSoaByInterface when
    // the client provides its own services as _contract_ HTTP body
    // - warning: supplied PublishedJson will be parsed in place, so modified
    procedure RegisterFromClientJson(var PublishedJson: RawUtf8);
    /// set the list from JSON serialized TServicesPublishedInterfacesDynArray
    // - may be used to duplicate the whole TRestServer.AssociatedServices
    // content, as returned from /root/Stat?findservice=*
    // - warning: supplied PublishedJson will be parsed in place, so modified
    procedure RegisterFromServerJson(var PublishedJson: RawUtf8);
    /// search for a public URI in the registration list
    function FindUri(const aPublicUri: TRestServerUri): PtrInt;
    /// search for the latest registrations of a service, by name
    // - will lookup for the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator - warning: research is case-sensitive
    // - if the service name has been registered several times, all
    // registration will be returned, the latest in first position
    function FindService(const aServiceName: RawUtf8): TRestServerUriDynArray;
    /// return all services URI by name, from the registration list, as URIs
    // - will lookup for the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator - warning: research is case-sensitive
    // - the returned string will contain all matching server URI, the latest
    // registration being the first to appear, e.g.
    // $ ["addresslast:port/root","addressprevious:port/root","addressfirst:port/root"]
    function FindServiceAll(
      const aServiceName: RawUtf8): TRestServerUriStringDynArray; overload;
    /// return all services URI by name, from the registration list, as JSON
    // - will lookup for the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator - warning: research is case-sensitive
    // - the returned JSON array will contain all matching server URI, encoded as
    // a TRestServerUri JSON array, the latest registration being
    // the first to appear, e.g.
    // $ [{"Address":"addresslast","Port":"port","Root":"root"},...]
    // - if aServiceName='*', it will return ALL registration items, encoded as
    // a TServicesPublishedInterfaces JSON array, e.g.
    // $ [{"PublicUri":{"Address":"1.2.3.4","Port":"123","Root":"root"},"Names":['Calculator']},...]
    procedure FindServiceAll(const aServiceName: RawUtf8;
      aWriter: TJsonWriter); overload;
    /// the number of milliseconds after which an entry expires
    // - is 0 by default, meaning no expiration
    // - you can set it to a value so that any service URI registered with
    // RegisterFromJson() AFTER this property modification may expire
    property TimeOut: integer
      read fTimeOut write fTimeOut;
  end;


implementation


{ ************ TOrmServiceLog TOrmServiceNotifications Classes }

{ TOrmServiceLog }

class procedure TOrmServiceLog.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
begin
  inherited;
  if FieldName = '' then
    Server.CreateSqlMultiIndex(self, ['Method', 'MicroSec'], false);
end;


{ TOrmServiceNotifications }

class procedure TOrmServiceNotifications.InitializeTable(
  const Server: IRestOrmServer; const FieldName: RawUtf8;
  Options: TOrmInitializeTableOptions);
begin
  inherited;
  if (FieldName = '') or
     (FieldName = 'Sent') then
    Server.CreateSqlMultiIndex(self, ['Sent'], false);
end;

class function TOrmServiceNotifications.LastEventsAsObjects(
  const Rest: IRestOrm; LastKnownID: TID; Limit: integer; Service: TInterfaceFactory;
  out Dest: TDocVariantData; const MethodName: RawUtf8; IDAsHexa: boolean): boolean;
var
  res: TOrmServiceNotifications;
begin
  res := CreateAndFillPrepare(Rest, 'ID > ? order by ID limit %',
    [Limit], [LastKnownID], 'ID,Method,Input');
  try
    if res.FillTable.RowCount > 0 then
    begin
      res.SaveFillInputsAsObjects(Service, Dest, MethodName, IDAsHexa);
      result := true;
    end
    else
      result := false;
  finally
    res.Free;
  end;
end;

function TOrmServiceNotifications.SaveInputAsObject(
  Service: TInterfaceFactory; const MethodName: RawUtf8; IDAsHexa: boolean): variant;
var
  m: integer;
begin
  VarClear(result{%H-});
  with TDocVariantData(result) do
    if IDAsHexa then
      InitObject([
        'ID',       Int64ToHex(fID),
        MethodName, Method], JSON_FAST)
    else
      InitObject([
        'ID',       fID,
        MethodName, Method], JSON_FAST);
  m := Service.FindMethodIndex(Method);
  if m >= 0 then
    Service.Methods[m].ArgsAsDocVariantObject(
      _Safe(fInput)^, TDocVariantData(result), true);
end;

procedure TOrmServiceNotifications.SaveFillInputsAsObjects(
  Service: TInterfaceFactory; out Dest: TDocVariantData; const MethodName: RawUtf8;
  IDAsHexa: boolean);
begin
  Dest.InitFast(FillTable.RowCount, dvArray);
  while FillOne do
    Dest.AddItem(SaveInputAsObject(Service, MethodName, IDAsHexa));
end;


{ ************ TServiceFactory Abstract Service Provider }

{ TServiceFactory }

constructor TServiceFactory.Create(aOwner: TInterfaceResolver;
  aInterface: PRttiInfo; aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8);
begin
  inherited CreateWithResolver(aOwner, {raiseIfNotFound=}true);
  fInterface := TInterfaceFactory.Get(aInterface);
  fInstanceCreation := aInstanceCreation;
  fInterfaceMangledUri := BinToBase64Uri(@fInterface.InterfaceIID, SizeOf(TGUID));
  fInterfaceUri := fInterface.InterfaceUri;
  if fOrm.Model.GetTableIndex(fInterfaceUri) >= 0 then
    raise EServiceException.CreateUtf8('%.Create: I% routing name is ' +
      'already used by a % SQL table name', [self, fInterfaceUri, fInterfaceUri]);
  SetLength(fExecution, fInterface.MethodsCount);
  // compute interface signature (aka "contract"), serialized as a JSON object
  FormatUtf8('{"contract":"%","implementation":"%","methods":%}',
    [fInterfaceUri, LowerCase(TrimLeftLowerCaseShort(ToText(InstanceCreation))),
     fInterface.Contract], fContract);
  fContractHash := '"' + CardinalToHex(Hash32(fContract)) +
    CardinalToHex(CRC32string(fContract)) + '"'; // 2 hashes to avoid collision
  if aContractExpected <> '' then // override default contract
    if aContractExpected[1] <> '"' then
      // stored as JSON string
      fContractExpected := '"' + aContractExpected + '"'
    else
      fContractExpected := aContractExpected
  else
    fContractExpected := fContractHash; // for security
end;

function TServiceFactory.GetInterfaceTypeInfo: PRttiInfo;
begin
  if (self <> nil) and
     (fInterface <> nil) then
    result := fInterface.InterfaceTypeInfo
  else
    result := nil;
end;

function TServiceFactory.GetInterfaceIID: TGUID;
begin
  result := fInterface.InterfaceIID;
end;

procedure TServiceFactory.ExecutionAction(const aMethod: array of RawUtf8;
  aOptions: TInterfaceMethodOptions; aAction: TServiceMethodOptionsAction);

  procedure SetAction(var exec: TServiceFactoryExecution);
  begin
    case aAction of
      moaReplace:
        exec.Options := aOptions;
      moaInclude:
        exec.Options := exec.Options + aOptions;
      moaExclude:
        exec.Options := exec.Options - aOptions;
    end;
  end;

var
  i, m: PtrInt;
begin
  if high(aMethod) < 0 then
    for i := 0 to fInterface.MethodsCount - 1 do
      SetAction(fExecution[i])
  else
    for m := 0 to high(aMethod) do
      SetAction(fExecution[fInterface.CheckMethodIndex(aMethod[m])]);
  fAnyOptions := [];
  for i := 0 to fInterface.MethodsCount - 1 do
    fAnyOptions := fAnyOptions + fExecution[i].Options;
end;

function ToText(si: TServiceInstanceImplementation): PShortString;
begin
  result := GetEnumName(TypeInfo(TServiceInstanceImplementation), ord(si));
end;


{ ************ TServiceFactoryServerAbstract Abstract Service Provider }

function TServiceFactoryServerAbstract.GetAuthGroupIDs(
  const aGroup: array of RawUtf8; out IDs: TIDDynArray): boolean;
var
  i: PtrInt;
begin
  result := (self <> nil) and
    fOrm.MainFieldIDs(
      fOrm.Model.GetTableInherited(DefaultTAuthGroupClass), aGroup, IDs);
  if result then
    for i := 0 to high(IDs) do
      // fExecution[].Denied set is able to store IDs up to 256 only
      if IDs[i] > 255 then
        raise EServiceException.CreateUtf8(
          'Unsupported %.Allow/Deny with GroupID=% >255', [self, IDs[i]]);
end;

function TServiceFactoryServerAbstract.AllowAll: TServiceFactoryServerAbstract;
var
  m: PtrInt;
begin
  if self <> nil then
    for m := 0 to fInterface.MethodsCount - 1 do
      FillcharFast(fExecution[m].Denied, SizeOf(fExecution[m].Denied), 0);
  result := self;
end;

function TServiceFactoryServerAbstract.AllowAllByID(
  const aGroupID: array of TID): TServiceFactoryServerAbstract;
var
  m, g: PtrInt;
begin
  if self <> nil then
    for m := 0 to fInterface.MethodsCount - 1 do
      with fExecution[m] do
        for g := 0 to high(aGroupID) do
          exclude(Denied, aGroupID[g] - 1);
  result := self;
end;

function TServiceFactoryServerAbstract.AllowAllByName(
  const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
var
  IDs: TIDDynArray;
begin
  if GetAuthGroupIDs(aGroup, IDs) then
    AllowAllByID(IDs);
  result := self;
end;

function TServiceFactoryServerAbstract.DenyAll: TServiceFactoryServerAbstract;
var
  m: PtrInt;
begin
  if self <> nil then
    for m := 0 to fInterface.MethodsCount - 1 do
      FillcharFast(fExecution[m].Denied, SizeOf(fExecution[m].Denied), 255);
  result := self;
end;

function TServiceFactoryServerAbstract.DenyAllByID(
  const aGroupID: array of TID): TServiceFactoryServerAbstract;
var
  m, g: PtrInt;
begin
  if self <> nil then
    for m := 0 to fInterface.MethodsCount - 1 do
      with fExecution[m] do
        for g := 0 to high(aGroupID) do
          include(Denied, aGroupID[g] - 1);
  result := self;
end;

function TServiceFactoryServerAbstract.DenyAllByName(
  const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
var
  IDs: TIDDynArray;
begin
  if GetAuthGroupIDs(aGroup, IDs) then
    DenyAllByID(IDs);
  result := self;
end;

function TServiceFactoryServerAbstract.Allow(
  const aMethod: array of RawUtf8): TServiceFactoryServerAbstract;
var
  m: PtrInt;
begin
  if self <> nil then
    for m := 0 to high(aMethod) do
      FillcharFast(
        fExecution[fInterface.CheckMethodIndex(aMethod[m])].Denied,
        SizeOf(fExecution[0].Denied), 0);
  result := self;
end;

function TServiceFactoryServerAbstract.AllowByID(
  const aMethod: array of RawUtf8;
  const aGroupID: array of TID): TServiceFactoryServerAbstract;
var
  m, g: PtrInt;
begin
  if self <> nil then
    if high(aGroupID) >= 0 then
      for m := 0 to high(aMethod) do
        with fExecution[fInterface.CheckMethodIndex(aMethod[m])] do
          for g := 0 to high(aGroupID) do
            exclude(Denied, aGroupID[g] - 1);
  result := self;
end;

function TServiceFactoryServerAbstract.AllowByName(
  const aMethod: array of RawUtf8;
  const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
var
  IDs: TIDDynArray;
begin
  if GetAuthGroupIDs(aGroup, IDs) then
    AllowByID(aMethod, IDs);
  result := self;
end;

function TServiceFactoryServerAbstract.Deny(
  const aMethod: array of RawUtf8): TServiceFactoryServerAbstract;
var
  m: PtrInt;
begin
  if self <> nil then
    for m := 0 to high(aMethod) do
      FillcharFast(
        fExecution[fInterface.CheckMethodIndex(aMethod[m])].Denied,
        SizeOf(fExecution[0].Denied), 255);
  result := self;
end;

function TServiceFactoryServerAbstract.DenyByID(
  const aMethod: array of RawUtf8;
  const aGroupID: array of TID): TServiceFactoryServerAbstract;
var
  m, g: PtrInt;
begin
  if self <> nil then
    for m := 0 to high(aMethod) do
      with fExecution[fInterface.CheckMethodIndex(aMethod[m])] do
        for g := 0 to high(aGroupID) do
          include(Denied, aGroupID[g] - 1);
  result := self;
end;

function TServiceFactoryServerAbstract.DenyByName(const aMethod: array of RawUtf8;
  const aGroup: array of RawUtf8): TServiceFactoryServerAbstract;
var
  IDs: TIDDynArray;
begin
  if GetAuthGroupIDs(aGroup, IDs) then
    DenyByID(aMethod, IDs);
  result := self;
end;

function TServiceFactoryServerAbstract.SetOptions(
  const aMethod: array of RawUtf8; aOptions: TInterfaceMethodOptions;
  aAction: TServiceMethodOptionsAction): TServiceFactoryServerAbstract;
var
  opt, mode: TInterfaceMethodOptions;
  m: PtrInt;
begin
  if self <> nil then
  begin
    opt := aOptions * INTERFACEMETHOD_THREADOPTIONS;
    {$ifdef OSWINDOWS}
    if Assigned(ServiceSingle) and
       (opt * [optExecInMainThread, optFreeInMainThread] <> []) then
       raise EServiceException.CreateUtf8('%.SetOptions(I%): [%] are not ' +
         'compatible with a Windows Service which has no main thread',
         [self, fInterfaceUri, ToText(opt)]);
    {$endif OSWINDOWS}
    if (opt <> []) and
       (aAction in [moaReplace, moaInclude]) and
       (fInstanceCreation = sicPerThread) then
      raise EServiceException.CreateUtf8(
        '%.SetOptions(I%,[%]) is not compatible with sicPerThread',
        [self, fInterfaceUri, ToText(opt)]);
    ExecutionAction(aMethod, aOptions, aAction);
    opt := fAnyOptions * INTERFACEMETHOD_THREADOPTIONS;
    if opt <> [] then
      if (optFreeInPerInterfaceThread in opt) and
         not (optExecInPerInterfaceThread in opt) then
        raise EServiceException.CreateUtf8(
          '%.SetOptions(I%,optFreeInPerInterfaceThread)' +
          ' without optExecInPerInterfaceThread', [self, fInterfaceUri])
      else for m := 0 to high(INTERFACEMETHOD_PERTHREADOPTIONS) do
      begin
        mode := INTERFACEMETHOD_PERTHREADOPTIONS[m];
        if (opt * mode <> []) and
           (opt - mode <> []) then
        raise EServiceException.CreateUtf8('%.SetOptions(I%): incompatible [%]',
          [self, fInterfaceUri, ToText(opt)]);
      end;
  end;
  result := self;
end;

function TServiceFactoryServerAbstract.SetWholeOptions(
  aOptions: TInterfaceOptions): TServiceFactoryServerAbstract;
begin
  if self <> nil then
  begin
    fByPassAuthentication := (optByPassAuthentication in aOptions);
    fResultAsJsonObject := (optResultAsJsonObject in aOptions);
    fResultAsJsonObjectWithoutResult := (optResultAsJsonObjectWithoutResult in aOptions);
    fResultAsXMLObject := (optResultAsXMLObject in aOptions);
    fResultAsXMLObjectIfAcceptOnlyXML := (optResultAsXMLObjectIfAcceptOnlyXML in aOptions);
    fExcludeServiceLogCustomAnswer := (optExcludeServiceLogCustomAnswer in aOptions);
  end;
  result := self;
end;


{ ************ TServiceContainer Abstract Services Holder }

{ TServiceContainer }

constructor TServiceContainer.Create(aOwner: TInterfaceResolver);
begin
  fOwner := aOwner;
  ClearServiceList;
end;

procedure TServiceContainer.ClearServiceList;
begin
  fInterface := nil;
  fInterfaceMethod := nil;
  fInterfaces.InitSpecific(TypeInfo(TServiceContainerInterfaces),
    fInterface, ptRawUtf8, nil, {caseinsensitive=}not fExpectMangledUri);
  fInterfaceMethods.InitSpecific(TypeInfo(TServiceContainerInterfaceMethods),
    fInterfaceMethod, ptRawUtf8, nil, {caseinsensitive=}not fExpectMangledUri);
end;

destructor TServiceContainer.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(fInterface) do
    fInterface[i].Service.Free;
  inherited;
end;

function TServiceContainer.Count: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fInterface);
end;

function TServiceContainer.AddServiceMethodInternal(
  const aInterfaceDotMethodName: RawUtf8; aService: TServiceFactory;
  var aMethodIndex: integer): PServiceContainerInterfaceMethod;
begin
  result := fInterfaceMethods.AddUniqueName(aInterfaceDotMethodName);
  result^.InterfaceService := aService;
  result^.InterfaceMethodIndex := aMethodIndex;
  inc(aMethodIndex);
end;

function TServiceContainer.AddServiceInternal(aService: TServiceFactory): PtrInt;
var
  ndx: integer;
  im: TServiceInternalMethod;
  m: PtrInt;
  uri: RawUtf8;
begin
  if (self = nil) or
     (aService = nil) then
    raise EServiceException.CreateUtf8('%.AddServiceInternal(%)', [self, aService]);
  // add TServiceFactory to the im list
  if ExpectMangledUri then
    uri := aService.fInterfaceMangledUri
  else
    uri := aService.fInterfaceUri;
  PServiceContainerInterface(fInterfaces.AddUniqueName(uri, @result))^.
    Service := aService;
  // add associated methods - first SERVICE_PSEUDO_METHOD[], then from interface
  uri := uri + '.';
  ndx := 0;
  for im := Low(im) to High(im) do
    AddServiceMethodInternal(uri + SERVICE_PSEUDO_METHOD[im], aService, ndx);
  for m := 0 to aService.fInterface.MethodsCount - 1 do
    AddServiceMethodInternal(uri + aService.fInterface.Methods[m].Uri, aService, ndx);
end;

procedure TServiceContainer.CheckInterface(const aInterfaces: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(aInterfaces) do
    if aInterfaces[i] = nil then
      raise EServiceException.CreateUtf8('%: aInterfaces[%]=nil', [self, i])
    else
      with aInterfaces[i]^ do
        if InterfaceGuid = nil then
          raise EServiceException.CreateUtf8('%: % is not an interface',
            [self, RawName])
        else if not (ifHasGuid in InterfaceType^.IntfFlags) then
          raise EServiceException.CreateUtf8('%: % interface has no GUID',
            [self, RawName])
        else if Info(InterfaceGuid^) <> nil then
          raise EServiceException.CreateUtf8('%: % GUID already registered',
            [self, RawName])

end;

procedure TServiceContainer.SetExpectMangledUri(Mangled: boolean);
var
  i: PtrInt;
  toregisteragain: TServiceContainerInterfaces;
begin
  if Mangled = fExpectMangledUri then
    exit;
  fExpectMangledUri := Mangled;
  toregisteragain := fInterface; // same services, but other URIs
  ClearServiceList;
  for i := 0 to high(toregisteragain) do
    AddServiceInternal(toregisteragain[i].Service);
end;

procedure TServiceContainer.SetInterfaceMethodBits(MethodNamesCsv: PUtf8Char;
  IncludePseudoMethods: boolean; out bits: TServiceContainerInterfaceMethodBits);
var
  i, n, m: PtrInt;
  method: RawUtf8;
begin
  FillCharFast(bits, SizeOf(bits), 0);
  n := length(fInterfaceMethod);
  if n > SizeOf(bits) shl 3 then
    raise EServiceException.CreateUtf8('%.SetInterfaceMethodBits: n=%', [self, n]);
  if IncludePseudoMethods then
    for i := 0 to n - 1 do
      if fInterfaceMethod[i].InterfaceMethodIndex < Length(SERVICE_PSEUDO_METHOD) then
        include(bits, i);
  while MethodNamesCsv <> nil do
  begin
    GetNextItem(MethodNamesCsv, ',', method);
    if PosExChar('.', method) = 0 then
    begin
      for i := 0 to n - 1 do
        with fInterfaceMethod[i] do // O(n) search is fast enough here
        begin
          m := InterfaceMethodIndex - Length(SERVICE_PSEUDO_METHOD);
          if (m >= 0) and
             IdemPropNameU(method, InterfaceService.fInterface.Methods[m].Uri) then
            include(bits, i);
        end;
    end
    else
    begin
      i := fInterfaceMethods.FindHashed(method); // O(1) search
      if i >= 0 then
        include(bits, i);
    end;
  end;
end;

function TServiceContainer.GetMethodName(ListInterfaceMethodIndex: integer): RawUtf8;
begin
  if cardinal(ListInterfaceMethodIndex) >= cardinal(length(fInterfaceMethod)) then
    result := ''
  else
    with fInterfaceMethod[ListInterfaceMethodIndex] do
      result := InterfaceService.fInterface.GetMethodName(InterfaceMethodIndex);
end;

function TServiceContainer.GetService(const aUri: RawUtf8): TServiceFactory;
var
  i: PtrInt;
begin
  if (self <> nil) and
     (aUri <> '') then
  begin
    i := fInterfaces.FindHashed(aUri);
    if i >= 0 then
      result := fInterface[i].Service
    else
      result := nil;
  end
  else
    result := nil;
end;

function TServiceContainer.Info(aTypeInfo: PRttiInfo): TServiceFactory;
var
  n: TDALen;
  p: PServiceContainerInterface;
begin
  if self <> nil then
  begin
    p := pointer(fInterface);
    if p <> nil then
    begin
      n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
      repeat
        result := p^.Service;
        if result.fInterface.InterfaceTypeInfo = aTypeInfo then
          exit;
        inc(p);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

{$ifdef FPC_HAS_CONSTREF}
function TServiceContainer.Info(constref aGuid: TGUID): TServiceFactory;
{$else}
function TServiceContainer.Info(const aGuid: TGUID): TServiceFactory;
{$endif FPC_HAS_CONSTREF}
var
  n: TDALen;
  p: PServiceContainerInterface;
  g: THash128Rec absolute aGuid;
begin
  // very efficient generated asm on FPC
  if self <> nil then
  begin
    p := pointer(fInterface);
    if p <> nil then
    begin
      n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
      repeat
        result := p^.Service;
        with PHash128Rec(@result.fInterface.InterfaceIID)^ do
          if (g.L = L) and
             (g.H = H) then
            exit;
        inc(p);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

procedure TServiceContainer.SetGUIDs(out Services: TGuidDynArray);
var
  i, n: PtrInt;
begin
  if self = nil then
    exit;
  n := length(fInterface);
  SetLength(Services, n);
  for i := 0 to n - 1 do
    Services[i] := fInterface[i].Service.fInterface.InterfaceIID;
end;

procedure TServiceContainer.SetInterfaceNames(out Names: TRawUtf8DynArray);
var
  i, n: PtrInt;
begin
  if self = nil then
    exit;
  n := length(fInterface);
  SetLength(Names, n);
  for i := 0 to n - 1 do
    Names[i] := fInterface[i].Service.fInterface.InterfaceUri;
end;

function TServiceContainer.AsJson: RawJson;
var
  WR: TTextWriter;
  i: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if (self = nil) or
     (fInterface = nil) then
    exit;
  WR := TTextWriter.CreateOwnedStream(temp);
  try
    WR.Add('[');
    for i := 0 to high(fInterface) do
    begin
      WR.AddString(fInterface[i].Service.Contract);
      WR.AddComma;
    end;
    WR.CancelLastComma;
    WR.Add(']');
    WR.SetText(RawUtf8(result));
  finally
    WR.Free;
  end;
end;

function TServiceContainer.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
var
  factory: TServiceFactory;
begin
  factory := Info(aInterface);
  if factory = nil then
    result := inherited TryResolve(aInterface, Obj)
  else
    result := factory.Get(Obj);
end;

function TServiceContainer.Index(aIndex: integer): TServiceFactory;
begin
  if (self = nil) or
     (cardinal(aIndex) > cardinal(high(fInterface))) then
    result := nil
  else
    result := fInterface[aIndex].Service;
end;

function TServiceContainer.CallBackUnRegister(const Callback: IInvokable): boolean;
begin
  result := false; // nothing to be done here
end;


{ ***************** TServicesPublishedInterfacesList Services Catalog }

{ TRestServerUri }

function TRestServerUri.GetUri: TRestServerUriString;
begin
  result := Address;
  if Port <> '' then
    result := result + ':' + Port;
  if root <> '' then
    result := result + '/' + root;
end;

procedure TRestServerUri.SetUri(const Value: TRestServerUriString);
begin
  Split(Value, ':', Address, Port);
  if Port <> '' then
    Split(Port, '/', Port, root)
  else
    Split(Address, '/', Address, root);
end;

function TRestServerUri.Equals(const other: TRestServerUri): boolean;
begin
  result := IdemPropNameU(Address, other.Address) and
            IdemPropNameU(Port, other.Port) and
            IdemPropNameU(root, other.Root);
end;


{ TServicesPublishedInterfacesList }

constructor TServicesPublishedInterfacesList.Create(aTimeoutMS: integer);
begin
  inherited Create; // may have been overriden
  fTimeOut := aTimeoutMS;
  fDynArray.Init(TypeInfo(TServicesPublishedInterfacesDynArray), List, @Count);
  fDynArrayTimeoutTix.Init(TypeInfo(TInt64DynArray), fTimeoutTix, @fTimeoutTixCount);
end;

function TServicesPublishedInterfacesList.FindUri(
  const aPublicUri: TRestServerUri): PtrInt;
var
  tix: Int64;
begin
  tix := GetTickCount64;
  Safe.ReadLock;
  try
    for result := 0 to Count - 1 do
      if List[result].PublicUri.Equals(aPublicUri) then
        if (fTimeOut = 0) or
           (fTimeoutTix[result] < tix) then
          exit;
    result := -1;
  finally
    Safe.ReadUnLock;
  end;
end;

function TServicesPublishedInterfacesList.FindService(
  const aServiceName: RawUtf8): TRestServerUriDynArray;
var
  i, n: PtrInt;
  tix: Int64;
begin
  tix := GetTickCount64;
  result := nil;
  Safe.ReadLock;
  try
    n := 0;
    for i := Count - 1 downto 0 do
      // downwards to return the latest first
      if FindPropName(List[i].Names, aServiceName) >= 0 then
        if (fTimeOut = 0) or
           (fTimeoutTix[i] < tix) then
        begin
          SetLength(result, n + 1);
          result[n] := List[i].PublicUri;
          inc(n);
        end;
  finally
    Safe.ReadUnLock;
  end;
end;

function TServicesPublishedInterfacesList.FindServiceAll(
  const aServiceName: RawUtf8): TRestServerUriStringDynArray;
var
  i: PtrInt;
  n: integer;
  tix: Int64;
begin
  tix := GetTickCount64;
  result := nil;
  n := 0;
  Safe.ReadLock;
  try
    for i := Count - 1 downto 0 do
      // downwards to return the latest first
      if FindPropName(List[i].Names, aServiceName) >= 0 then
        if (fTimeOut = 0) or
           (fTimeoutTix[i] < tix) then
          AddRawUtf8(TRawUtf8DynArray(result), n, List[i].PublicUri.Uri);
  finally
    Safe.ReadUnLock;
  end;
  SetLength(result, n);
end;

procedure TServicesPublishedInterfacesList.FindServiceAll(
  const aServiceName: RawUtf8; aWriter: TJsonWriter);
var
  i: PtrInt;
  tix: Int64;
begin
  tix := GetTickCount64;
  Safe.ReadLock;
  try
    aWriter.Add('[');
    if aServiceName = '*' then
    begin
      // for RegisterFromServer: return all TServicesPublishedInterfaces
      for i := 0 to Count - 1 do
        if (fTimeOut = 0) or
           (fTimeoutTix[i] < tix) then
        begin
          aWriter.AddRecordJson(@List[i], TypeInfo(TServicesPublishedInterfaces));
          aWriter.AddComma;
        end;
    end
    else
      // from SQLRestClientUri.ServiceRetrieveAssociated:
      // search matching (and non deprecated) services as TRestServerUri
      for i := Count - 1 downto 0 do        // downwards to return the latest first
        if FindPropName(List[i].Names, aServiceName) >= 0 then
          if (fTimeOut = 0) or
             (fTimeoutTix[i] < tix) then
          begin
            aWriter.AddRecordJson(@List[i].PublicUri, TypeInfo(TRestServerUri));
            aWriter.AddComma;
          end;
    aWriter.CancelLastComma;
    aWriter.Add(']');
  finally
    Safe.ReadUnLock;
  end;
end;

//TODO : to be implemented in mormot.soa.client
{
function TServicesPublishedInterfacesList.RegisterFromServer(Client: TRestClientUri): boolean;
var json: RawUtf8;
begin
  result := Client.CallBackGet('stat',['findservice','*'],json)=HTTP_SUCCESS;
  if result and
     (json<>'') then
    RegisterFromServerJson(json);
end;
}

procedure TServicesPublishedInterfacesList.RegisterFromServerJson(
  var PublishedJson: RawUtf8);
var
  tix: Int64;
  i: PtrInt;
begin
  Safe.WriteLock;
  try
    fDynArray.LoadFromJson(pointer(PublishedJson));
    fDynArrayTimeoutTix.Count := Count;
    tix := GetTickCount64;
    if fTimeout = 0 then
      inc(tix, maxInt)
    else
      inc(tix, fTimeout);
    for i := 0 to Count - 1 do
      fTimeoutTix[i] := tix;
  finally
    Safe.WriteUnLock;
  end;
end;

procedure TServicesPublishedInterfacesList.RegisterFromClientJson(
  var PublishedJson: RawUtf8);
var
  i: PtrInt;
  nfo: TServicesPublishedInterfaces;
  crc: cardinal;
  tix: Int64;
  P: PUtf8Char;
begin
  if PublishedJson = '' then
    exit;
  crc := crc32c(0, pointer(PublishedJson), length(PublishedJson));
  if (self = nil) or
     ((fLastPublishedJson <> 0) and
      (crc = fLastPublishedJson)) then
    // rough but efficient in practice, when similar _contract_
    exit;
  P := pointer(PublishedJson);
  if P^ = '[' then
    // when transmitted as [params] in a _contract_ HTTP body content
    inc(P);
  if (RecordLoadJson(nfo, P, TypeInfo(TServicesPublishedInterfaces)) = nil) or
     (nfo.PublicUri.Address = '') then
    // invalid supplied JSON content
    exit;
  Safe.WriteLock;
  try
    // store so that the latest updated version is always at the end
    for i := 0 to Count - 1 do
      if List[i].PublicUri.Equals(nfo.PublicUri) then
      begin
        // we ignore the Timeout here
        fDynArray.Delete(i);
        fDynArrayTimeoutTix.Delete(i);
        break;
      end;
    if nfo.Names <> nil then
    begin
      fDynArray.Add(nfo);
      tix := GetTickCount64;
      if fTimeout = 0 then
        inc(tix, maxInt)
      else
        inc(tix, fTimeout);
      fDynArrayTimeoutTix.Add(tix);
    end;
    fLastPublishedJson := crc;
  finally
    Safe.WriteUnLock;
  end;
end;


const
  // text definitions, registered in unit's initialization block below
  _TRestServerUri =
    'Address,Port,Root: RawUtf8';
  _TServicesPublishedInterfaces =
    'PublicUri:TRestServerUri Names: array of RawUtf8';

procedure InitializeUnit;
begin
  Rtti.RegisterFromText(
    [TypeInfo(TRestServerUri), _TRestServerUri,
     TypeInfo(TServicesPublishedInterfaces), _TServicesPublishedInterfaces]);
end;


initialization
  InitializeUnit;

end.

