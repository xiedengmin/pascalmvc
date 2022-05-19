///  Middle-Level POSIX Daemon / Windows Service Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.app.daemon;

{
  *****************************************************************************

   Daemon (e.g. Windows Service) Stand-Alone Background Executable Support
    - Parent Daemon Settings Class
    - Parent Daemon Application Class

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.data,
  mormot.core.log,
  mormot.crypt.core;



{ ************ Parent Daemon Settings Class }

type
  /// abstract parent containing information able to initialize a TSynDaemon class
  // - will handle persistence as JSON local files
  // - could fallback to read an .INI file if no valid JSON is found
  // - by default in this abstract parent class, no property is published to let
  // inherited classes define the values customizable from JSON serialization
  TSynDaemonAbstractSettings  = class(TSynJsonFileSettings)
  protected
    fServiceName: string;
    fServiceDisplayName: string;
    fServiceExecutable: string;
    fServiceDependencies: string;
    fLog: TSynLogInfos;
    fLogRotateFileCount: integer;
    fLogPath: TFileName;
    fLogClass: TSynLogClass;
  public
    /// initialize and set the default settings
    constructor Create; override;
    /// define the log information into the supplied TSynLog class
    // - if you don't call this method, the logging won't be initiated
    // - is to be called typically in the overriden Create constructor of the
    // associated TSynDaemon class, just after "inherited Create"
    procedure SetLog(aLogClass: TSynLogClass);
    /// returns user-friendly description of the service, including version
    // information and company copyright (if available)
    function ServiceDescription: string; virtual;
    /// read-only access to the TSynLog class, if SetLog() has been called
    property LogClass: TSynLogClass
      read fLogClass;
    /// optional service dependencies
    // - not published by default: could be defined if needed, or e.g. set in
    // overriden constructor
    // - several depending services may be set by appending #0 between names
    property ServiceDependencies: string
      read fServiceDependencies write fServiceDependencies;
    /// the service name, as used internally by Windows or the TSynDaemon class
    // - default is the executable name
    property ServiceName: string
      read fServiceName write fServiceName;
    /// the service name, as displayed by Windows or at the console level
    // - default is the executable name
    property ServiceDisplayName: string
      read fServiceDisplayName write fServiceDisplayName;
    /// the service executable path and parameters
    // - default is void '', so the executable name (with full path) will be used
    // - by definition, is available only on Windows
    property ServiceExecutable: string
      read fServiceExecutable write fServiceExecutable;
    /// if not void, will enable the logs (default is LOG_STACKTRACE)
    property Log: TSynLogInfos
      read fLog write fLog;
    /// allow to customize where the logs should be written
    property LogPath: TFileName
      read fLogPath write fLogPath;
    /// how many files will be rotated
    // - default is 2
    property LogRotateFileCount: integer
      read fLogRotateFileCount write fLogRotateFileCount;
  end;

  /// abstract parent containing information able to initialize a TSynDaemon class
  // - by default, will publish the main properties to that RTTI will handle
  // properly persistence of those fields in the JSON local settings file
  TSynDaemonSettings  = class(TSynDaemonAbstractSettings)
  published
    /// the service name, as used internally by Windows or the TSynDaemon class
    // - default is the executable name
    property ServiceName;
    /// the service name, as displayed by Windows or at the console level
    // - default is the executable name
    property ServiceDisplayName;
    /// the service executable path and parameters
    // - default is none '', so the executable name (with full path) will be used
    property ServiceExecutable;
    /// if not void, will enable the logs (default is LOG_STACKTRACE)
    property Log: TSynLogInfos
      read fLog write fLog;
    /// allow to customize where the logs should be written
    property LogPath: TFileName
      read fLogPath write fLogPath;
    /// how many files will be rotated (default is 2)
    property LogRotateFileCount;
  end;
  
  /// meta-class of TSynDaemon settings information
  TSynDaemonSettingsClass = class of TSynDaemonAbstractSettings;



{ ************ Parent Daemon Application Class }

type
  /// exception raised during POSIX Daemon / Windows Service process
  EDaemon = class(ESynException);

  /// abstract parent to implements a POSIX Daemon / Windows Service
  // - inherit from this abstract class and override Start and Stop methods
  // - you may consider using TDDDAdministratedDaemon from dddInfraApps
  TSynDaemon = class(TSynPersistent)
  protected
    fConsoleMode: boolean;
    fWorkFolderName: TFileName;
    fSettings: TSynDaemonAbstractSettings;
    function CustomCommandLineSyntax: string; virtual;
    {$ifdef OSWINDOWS}
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif OSWINDOWS}
  public
    /// initialize the daemon, creating the associated settings
    // - TSynDaemonSettings instance will be owned and freed by the daemon
    // - any non supplied folder name will be replaced by a default value
    // (executable folder under Windows, or /etc /var/log on Linux)
    constructor Create(aSettingsClass: TSynDaemonSettingsClass;
      const aWorkFolder, aSettingsFolder, aLogFolder: TFileName;
      const aSettingsExt: TFileName = '.settings';
      const aSettingsName: TFileName = ''); reintroduce;
    /// main entry point of the daemon, to process the command line switches
    // - aAutoStart is used only under Windows
    procedure CommandLine(aAutoStart: boolean = true);
    /// inherited class should override this abstract method with proper process
    procedure Start; virtual; abstract;
    /// inherited class should override this abstract method with proper process
    // - should do nothing if the daemon was already stopped
    procedure Stop; virtual; abstract;
    /// call Stop, finalize the instance, and its settings
    destructor Destroy; override;
  published
    /// if this instance was run as /console or /verb
    property ConsoleMode: boolean
      read fConsoleMode;
    /// the settings associated with this daemon
    // - will be allocated in Create constructor, and released in Destroy
    property Settings: TSynDaemonAbstractSettings
      read fSettings;
  end;



implementation


{ ************ Parent Daemon Settings Class }

{ TSynDaemonAbstractSettings }

constructor TSynDaemonAbstractSettings.Create;
begin
  inherited Create;
  fLog := LOG_STACKTRACE + [sllNewRun];
  fLogRotateFileCount := 2;
  Utf8ToStringVar(Executable.ProgramName, fServiceName);
  fServiceDisplayName := fServiceName;
end;

function TSynDaemonAbstractSettings.ServiceDescription: string;
var
  versionnumber: string;
begin
  result := ServiceDisplayName;
  with Executable.Version do
  begin
    versionnumber := DetailedOrVoid;
    if versionnumber <> '' then
      result := result + ' ' + versionnumber;
    if CompanyName <> '' then
      result := FormatString('% - (c)% %', [result, BuildYear, CompanyName]);
  end;
end;

procedure TSynDaemonAbstractSettings.SetLog(aLogClass: TSynLogClass);
begin
  if (self <> nil) and
     (Log <> []) and
     (aLogClass <> nil) then
    with aLogClass.Family do
    begin
      DestinationPath := LogPath;
      PerThreadLog := ptIdentifiedInOneFile; // ease multi-threaded server debug
      RotateFileCount := LogRotateFileCount;
      if RotateFileCount > 0 then
      begin
        RotateFileSizeKB := 20 * 1024; // rotate by 20 MB logs
        FileExistsAction := acAppend;  // as expected in rotation mode
      end
      else
        HighResolutionTimestamp := true;
      Level := Log;
      fLogClass := aLogClass;
    end;
end;



{ ************ Parent Daemon Application Class }

{ TSynDaemon }

constructor TSynDaemon.Create(aSettingsClass: TSynDaemonSettingsClass;
  const aWorkFolder, aSettingsFolder, aLogFolder,
        aSettingsExt, aSettingsName: TFileName);
var
  fn: TFileName;
begin
  inherited Create; // may have been overriden
  if aWorkFolder = '' then
    fWorkFolderName := Executable.ProgramFilePath
  else
    fWorkFolderName := EnsureDirectoryExists(aWorkFolder, true);
  if aSettingsClass = nil then
    aSettingsClass := TSynDaemonSettings;
  fSettings := aSettingsClass.Create;
  fn := aSettingsFolder;
  if fn = '' then
    fn := {$ifdef OSWINDOWS}fWorkFolderName{$else}'/etc/'{$endif};
  fn :=  EnsureDirectoryExists(fn);
  if aSettingsName = '' then
    fn := fn + Utf8ToString(Executable.ProgramName)
  else
    fn := fn + aSettingsName;
  fSettings.LoadFromFile(fn + aSettingsExt);
  if fSettings.LogPath = '' then
    if aLogFolder = '' then
      fSettings.LogPath :=
        {$ifdef OSWINDOWS}fWorkFolderName{$else}GetSystemPath(spLog){$endif}
    else
      fSettings.LogPath := EnsureDirectoryExists(aLogFolder);
end;

destructor TSynDaemon.Destroy;
begin
  if fSettings <> nil then
    fSettings.SaveIfNeeded;
  Stop;
  inherited Destroy;
  FreeAndNil(fSettings);
end;

{$ifdef OSWINDOWS}
procedure TSynDaemon.DoStart(Sender: TService);
begin
  Start;
end;

procedure TSynDaemon.DoStop(Sender: TService);
begin
  Stop;
end;
{$endif OSWINDOWS}

function TSynDaemon.CustomCommandLineSyntax: string;
begin
  result := '';
end;

{$I-}

type
  TExecuteCommandLineCmd = (
     cNone,
     cVersion,
     cVerbose,
     cStart,
     cStop,
     cState,
     cSilentKill,
     cHelp,
     cInstall,
     cRun,
     cFork,
     cUninstall,
     cConsole,
     cKill);

const
  CMD_CHR: array[cHelp .. cKill] of AnsiChar = (
    'H',  // cHelp
    'I',  // cInstall
    'R',  // cRun
    'F',  // cFork
    'U',  // cUninstall
    'C',  // cConsole
    'K'); // cKill

procedure TSynDaemon.CommandLine(aAutoStart: boolean);
var
  cmd, c: TExecuteCommandLineCmd;
  p: PUtf8Char;
  ch: AnsiChar;
  param: RawUtf8;
  exe: RawByteString;
  log: TSynLog;
  {$ifdef OSWINDOWS}
  service: TServiceSingle;
  ctrl: TServiceController;
  {$endif OSWINDOWS}

  procedure WriteCopyright;
  var
    msg, name, copyright: string;
    i: integer;
  begin
    msg := fSettings.ServiceDescription;
    i := Pos(' - ', msg);
    if i = 0 then
      name := msg
    else
    begin
      name := copy(msg, 1, i - 1);
      copyright := copy(msg, i + 3, 1000);
    end;
    TextColor(ccLightGreen);
    writeln(' ', name);
    writeln(StringOfChar('-', length(name) + 2));
    TextColor(ccGreen);
    if {%H-}copyright <> '' then
      writeln(' ', copyright);
    writeln;
    TextColor(ccLightGray);
  end;

  procedure Syntax;
  var
    spaces, custom: string;
  begin
    WriteCopyright;
    writeln('Try with one of the switches:');
    spaces := StringOfChar(' ', length(Executable.ProgramName) + 4);
    {$ifdef OSWINDOWS}
    writeln('   ', Executable.ProgramName,
      ' /console -c /verbose /help -h /version');
    writeln(spaces, '/install /uninstall /start /stop /state');
    {$else}
    writeln(' ./', Executable.ProgramName,
      ' --console -c --verbose --help -h --version');
    writeln(spaces, '--run -r --fork -f --kill -k');
    {$endif OSWINDOWS}
    custom := CustomCommandLineSyntax;
    if custom <> '' then
      writeln(spaces, custom);
  end;

  function cmdText: RawUtf8;
  begin
    result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd), ord(cmd));
  end;

  procedure Show(Success: boolean);
  var
    msg: RawUtf8;
    error: integer;
  begin
    WriteCopyright;
    if Success then
    begin
      msg := 'Successfully executed';
      TextColor(ccWhite);
    end
    else
    begin
      error := GetLastError;
      msg := FormatUtf8('Error 0x% [%] occured with',
        [CardinalToHexShort(error), StringToUtf8(SysErrorMessage(error))]);
      TextColor(ccLightRed);
      ExitCode := 1; // notify error to caller batch
    end;
    msg := FormatUtf8('% [%] (%) on Service ''%''',
      [msg, param, cmdText, fSettings.ServiceName]);
    writeln(Utf8ToConsole(msg));
    TextColor(ccLightGray);
    log.Log(sllDebug, 'CommandLine: %', [msg], self);
  end;

begin
  if (self = nil) or
     (fSettings = nil) then
    exit;
  log := nil;
  param := TrimU(StringToUtf8(paramstr(1)));
  cmd := cNone;
  if (param <> '') and
     (param[1] in ['/', '-']) then
  begin
    p := @param[2];
    if p^ = '-' then
      // allow e.g. --fork switch (idem to /f -f /fork -fork)
      inc(p);
    ch := NormToUpper[p^];
    for c := low(CMD_CHR) to high(CMD_CHR) do
      if CMD_CHR[c] = ch then
      begin
        cmd := c;
        break;
      end;
    if cmd = cNone then
      byte(cmd) := ord(cVersion) +
        IdemPCharArray(p, ['VERS',      // cVersion
                           'VERB',      // cVerbose
                           'START',     // cStart
                           'STOP',      // cStop
                           'STAT',      // cState
                           'SILENTK']); // cSilentKill
    end;
  try
    case cmd of
    cHelp:
      Syntax;
    cVersion:
      begin
        WriteCopyright;
        exe := StringFromFile(Executable.ProgramFileName);
        writeln(' ', fSettings.ServiceName,
          #13#10' Size: ', length(exe), ' bytes (', KB(exe), ')' +
          #13#10' Build date: ', Executable.Version.BuildDateTimeString,
          #13#10' MD5: ', Md5(exe),
          #13#10' SHA256: ', Sha256(exe));
        if Executable.Version.Version32 <> 0 then
          writeln(' Version: ', Executable.Version.Detailed);
      end;
    cConsole,
    cVerbose:
      begin
        WriteCopyright;
        writeln('Launched in ', cmdText, ' mode'#10);
        TextColor(ccLightGray);
        log := fSettings.fLogClass.Add;
        if (cmd = cVerbose) and
           (log <> nil) then
        begin
          log.Family.Level := LOG_VERBOSE;
          log.Family.EchoToConsole := LOG_VERBOSE;
        end;
        try
          log.Log(sllNewRun, 'Start % /% %', [fSettings.ServiceName, cmdText,
            Executable.Version.DetailedOrVoid], self);
          fConsoleMode := true;
          Start;
          writeln('Press [Enter] to quit');
          ioresult;
          ConsoleWaitForEnterKey;
          writeln('Shutting down server');
        finally
          ioresult;
          log.Log(sllNewRun, 'Stop /%', [cmdText], self);
          Stop;
        end;
    end;
    {$ifdef OSWINDOWS}
    // implement the daemon as a Windows Service
    else if fSettings.ServiceName = '' then
      if cmd = cNone then
        Syntax
      else
      begin
        TextColor(ccLightRed);
        writeln('No ServiceName specified - please fix the settings');
      end
    else
    case cmd of
      cNone:
        if param = '' then
        begin
          // executed as a background service
          service := TServiceSingle.Create(
            fSettings.ServiceName, fSettings.ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServiceSingleRun then
              // blocking until service shutdown
              Show(true)
            else if GetLastError = 1063 then
              Syntax
            else
              Show(false);
          finally
            service.Free;
          end;
        end
        else
          Syntax;
      cInstall:
        with fSettings do
          Show(TServiceController.Install(ServiceName, ServiceDisplayName,
            ServiceDescription, aAutoStart, ServiceExecutable,
            ServiceDependencies) <> ssNotInstalled);
      cStart,
      cStop,
      cUninstall,
      cState:
        begin
          ctrl := TServiceController.CreateOpenService(
                    '', '', fSettings.ServiceName);
          try
            case cmd of
              cStart:
                Show(ctrl.Start([]));
              cStop:
                Show(ctrl.Stop);
              cUninstall:
                begin
                  ctrl.Stop;
                  Show(ctrl.Delete);
                end;
              cState:
                writeln(fSettings.ServiceName,
                  ' State=', ServiceStateText(ctrl.State));
            end;
          finally
            ctrl.Free;
          end;
        end;
    else
      Syntax;
    end;
    {$else}
    // POSIX Run/Fork background execution of the executable
    cRun,
    cFork:
      RunUntilSigTerminated(self, {dofork=}(cmd = cFork), Start, Stop,
        fSettings.fLogClass.DoLog, fSettings.ServiceName);
    cKill,
    cSilentKill:
      if RunUntilSigTerminatedForKill then
      begin
        if cmd <> cSilentKill then
          writeln('Forked process ',
            Executable.ProgramName, ' killed successfully');
      end
      else
        raise EDaemon.Create('No forked process found to be killed');
    else
      Syntax;
    {$endif OSWINDOWS}
    end;
  except
    on E: Exception do
    begin
      if cmd <> cSilentKill then
        ConsoleShowFatalException(E, true);
      ExitCode := 1; // indicates error
    end;
  end;
  if cmd <> cSilentKill then
    TextColor(ccLightGray);
  ioresult;
end;


end.

