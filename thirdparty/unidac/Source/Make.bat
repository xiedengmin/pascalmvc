@echo off
rem **********************************************************************
rem *
rem * UniDAC
rem *
rem * Tasks:
rem *   1) Compile Dac packages;
rem *   2) Compile CRControls package;
rem *   2) Compile UniDAC packages;
rem *
rem * Command line:
rem *   call ..\Make.bat IDEName IDEVer WIN32
rem *   
rem * Parameters:
rem *   IDEName = (Delphi, CBuilder)
rem *   IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
rem *   Platform = (WIN32, WIN64, OSX32, iOSSimulator, iOSDevice, iOSDevice32, iOSDevice64, Android32, Android64, Linux64) WIN32 - default
rem **********************************************************************

rem Prepare ==============================================================
rem ======================================================================
set IDEName=%1
set IDEVer=%2
set Platform=%3
set NeedPause=%4
set PrjName=UniDAC
set PrjNameL=unidac

set OracleProvider=TRUE
set InterBaseProvider=TRUE
set MySQLProvider=TRUE
set TDSProvider=TRUE
set SQLServerProvider=TRUE
set PostgreSQLProvider=TRUE
set RedshiftProvider=TRUE
set SQLiteProvider=TRUE
set ODBCProvider=TRUE
set ASEProvider=TRUE
set AccessProvider=TRUE
set DB2Provider=TRUE
set AdvantageProvider=TRUE
set DBFProvider=TRUE
set NexusDBProvider=FALSE
set MongoDBProvider=TRUE

set BigCommerceProvider=TRUE
set DynamicsCRMProvider=TRUE
set ExactTargetProvider=TRUE
set FreshBooksProvider=TRUE
set MagentoProvider=TRUE
set MailChimpProvider=TRUE
set NetSuiteProvider=TRUE
set QuickBooksProvider=TRUE
set SalesforceProvider=TRUE
set SugarCRMProvider=TRUE
set ZohoCRMProvider=TRUE

if %Platform%A==WIN32A goto CompileProviders
if %Platform%A==WIN64A goto CompileProviders

set AccessProvider=FALSE
set DB2Provider=FALSE
set AdvantageProvider=FALSE
set DBFProvider=FALSE
set NexusDBProvider=FALSE

set BigCommerceProvider=FALSE
set DynamicsCRMProvider=FALSE
set ExactTargetProvider=FALSE
set FreshBooksProvider=FALSE
set MagentoProvider=FALSE
set MailChimpProvider=FALSE
set NetSuiteProvider=FALSE
set QuickBooksProvider=FALSE
set SalesforceProvider=FALSE
set SugarCRMProvider=FALSE
set ZohoCRMProvider=FALSE

if %Platform%A==OSX32A goto CompileProviders
if %Platform%A==OSX64A goto CompileProviders
if %Platform%A==Linux64A goto CompileProviders

set ODBCProvider=FALSE

:CompileProviders

pushd

if not defined OracleProvider     set OracleProvider=FALSE
if not defined InterBaseProvider  set InterBaseProvider=FALSE
if not defined MySQLProvider      set MySQLProvider=FALSE
if not defined TDSProvider        set TDSProvider=FALSE
if not defined SQLServerProvider  set SQLServerProvider=FALSE
if not defined PostgreSQLProvider set PostgreSQLProvider=FALSE
if not defined RedshiftProvider   set RedshiftProvider=FALSE
if not defined SQLiteProvider     set SQLiteProvider=FALSE
if not defined ODBCProvider       set ODBCProvider=FALSE
if not defined ASEProvider        set ASEProvider=FALSE
if not defined AccessProvider     set AccessProvider=FALSE
if not defined DB2Provider        set DB2Provider=FALSE
if not defined AdvantageProvider  set AdvantageProvider=FALSE
if not defined DBFProvider        set DBFProvider=FALSE
if not defined NexusDBProvider    set NexusDBProvider=FALSE
if not defined MongoDBProvider    set MongoDBProvider=FALSE

if not defined BigCommerceProvider set BigCommerceProvider=FALSE
if not defined DynamicsCRMProvider set DynamicsCRMProvider=FALSE
if not defined ExactTargetProvider set ExactTargetProvider=FALSE
if not defined FreshBooksProvider  set FreshBooksProvider=FALSE
if not defined MagentoProvider     set MagentoProvider=FALSE
if not defined MailChimpProvider   set MailChimpProvider=FALSE
if not defined NetSuiteProvider    set NetSuiteProvider=FALSE
if not defined QuickBooksProvider  set QuickBooksProvider=FALSE
if not defined SalesforceProvider  set SalesforceProvider=FALSE
if not defined SugarCRMProvider    set SugarCRMProvider=FALSE
if not defined ZohoCRMProvider     set ZohoCRMProvider=FALSE

rem Test IDEName
if %IDEName%A==DelphiA goto IDENameOK
if %IDEName%A==CBuilderA goto IDENameOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer
echo    IDEName = (Delphi, CBuilder)
goto Err
:IDENameOK

rem Test IDEVer
if %IDEVer%A==6A goto IDEVerOK
if %IDEVer%A==7A goto IDEVerOK
if %IDEVer%A==9A goto IDEVerOK
if %IDEVer%A==10A goto IDEVerOK
if %IDEVer%A==11A goto IDEVer11
if %IDEVer%A==12A goto IDEVerOK
if %IDEVer%A==14A goto IDEVerOK
if %IDEVer%A==15A goto IDEVerOK
if %IDEVer%A==16A goto IDEVerOK
if %IDEVer%A==17A goto IDEVerOK
if %IDEVer%A==18A goto IDEVerOK
if %IDEVer%A==19A goto IDEVerOK
if %IDEVer%A==20A goto IDEVerOK
if %IDEVer%A==21A goto IDEVerOK
if %IDEVer%A==22A goto IDEVerOK
if %IDEVer%A==23A goto IDEVerOK
if %IDEVer%A==24A goto IDEVerOK
if %IDEVer%A==25A goto IDEVerOK
if %IDEVer%A==26A goto IDEVerOK
if %IDEVer%A==27A goto IDEVerOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer Platform
echo    IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
goto Err

:IDEVer11:
set PkgVer=105
goto PkgVerOK

:IDEVerOK
set PkgVer=%IDEVer%0

:PkgVerOK

if not %Platform%A==WIN64A goto PlatformOSX32
set PlatformDir=Win64
goto PlatformOK
:PlatformOSX32
if not %Platform%A==OSX32A goto PlatformOSX64
set PlatformDir=OSX32
goto PlatformOK
:PlatformOSX64
if not %Platform%A==OSX64A goto iOSSimulator
set PlatformDir=OSX64
goto PlatformOK
:iOSSimulator
if not %Platform%A==iOSSimulatorA goto iOSDevice
set PlatformDir=iOSSimulator
goto PlatformOK
:iOSDevice
if not %Platform%A==iOSDeviceA goto iOSDevice32
set PlatformDir=iOSDevice
goto PlatformOK
:iOSDevice32
if not %Platform%A==iOSDevice32A goto iOSDevice64
set PlatformDir=iOSDevice32
goto PlatformOK
:iOSDevice64
if not %Platform%A==iOSDevice64A goto Android
set PlatformDir=iOSDevice64
goto PlatformOK
:Android
if not %Platform%A==AndroidA goto Android32
set PlatformDir=Android
goto PlatformOK
:Android32
if not %Platform%A==Android32A goto Android64
set PlatformDir=Android32
goto PlatformOK
:Android64
if not %Platform%A==Android64A goto Linux64
set PlatformDir=Android64
goto PlatformOK
:Linux64
if not %Platform%A==Linux64A goto PlatformWin32
set PlatformDir=Linux64
goto PlatformOK
:PlatformWin32
set Platform=WIN32
set PlatformDir=Win32
:PlatformOK

set CompilerOptions=-B -LE. -LN. -I.. -U..;..\..\Lib\Delphi%IDEVer%\%PlatformDir% -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win
set CompilerOptionsVCL=-B -LE. -LN. -I.. -U..;..\..\Lib\Delphi%IDEVer%\%PlatformDir% -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell

set Linux64UnitPath=..;..\sqlite3;..\..\Lib\Delphi%IDEVer%\Linux64;%IdeDir%\lib\Linux64\release";%IdeDir%\Imports";%IdeDir%\include";%IdeDir%\redist\Linux64";
set CompilerOptionsLinux64=-B -JL -LE. -E. -N. -LN. -$O+ -$D- -$I- -$L- -$Q- -$R- -$W- -DPRO -TX.so -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win; -I%Linux64UnitPath% -O%Linux64UnitPath% -R%Linux64UnitPath% -U%Linux64UnitPath% --syslibroot:%LinuxSDKDir% --libpath:%LinuxSDKDir%\usr\lib\gcc\x86_64-linux-gnu\5;%LinuxSDKDir%\usr\lib\x86_64-linux-gnu;%LinuxSDKDir%\lib\x86_64-linux-gnu 

if %IDEVer%A==16A goto NotResetCompileOptions
if %IDEVer%A==17A goto NotResetCompileOptions
if %IDEVer%A==18A goto NotResetCompileOptions
if %IDEVer%A==19A goto NotResetCompileOptions
if %IDEVer%A==20A goto NotResetCompileOptions
if %IDEVer%A==21A goto NotResetCompileOptions
if %IDEVer%A==22A goto NotResetCompileOptions
if %IDEVer%A==23A goto NotResetCompileOptions
if %IDEVer%A==24A goto NotResetCompileOptions
if %IDEVer%A==25A goto NotResetCompileOptions
if %IDEVer%A==26A goto NotResetCompileOptions
if %IDEVer%A==27A goto NotResetCompileOptions
set PlatformDir=.
set CompilerOptions=-B -LE. -LN. -I.. -U..
set CompilerOptionsVCL=-B -LE. -LN. -I.. -U..
:NotResetCompileOptions

if %IDEName%A==CBuilderA goto CBuilder

rem Compile ==============================================================
if not %Platform%A==WIN32A goto Win64Compiler
set Compiler=%IdeDir%\Bin\dcc32.exe"
goto CompilerOK
:Win64Compiler
if not %Platform%A==WIN64A goto OSX32Compiler
set Compiler=%IdeDir%\Bin\dcc64.exe"
goto CompilerOK
:OSX32Compiler
if not %Platform%A==OSX32A goto OSX64Compiler
set Compiler=%IdeDir%\Bin\dccosx.exe"
goto CompilerOK
:OSX64Compiler
if not %Platform%A==OSX64A goto iOSSimulator
set Compiler=%IdeDir%\Bin\dccosx64.exe"
goto CompilerOK
:iOSSimulator
if not %Platform%A==iOSSimulatorA goto iOSDevice
set Compiler=%IdeDir%\Bin\dccios32.exe"
goto CompilerOK
:iOSDevice
if not %Platform%A==iOSDeviceA goto iOSDevice32
set Compiler=%IdeDir%\Bin\dcciosarm.exe"
goto CompilerOK
:iOSDevice32
if not %Platform%A==iOSDevice32A goto iOSDevice64
set Compiler=%IdeDir%\Bin\dcciosarm.exe"
goto CompilerOK
:iOSDevice64
if not %Platform%A==iOSDevice64A goto Android
set Compiler=%IdeDir%\Bin\dcciosarm64.exe"
goto CompilerOK
:Android
if not %Platform%A==AndroidA goto Android32
set Compiler=%IdeDir%\Bin\dccaarm.exe"
goto CompilerOK
:Android32
if not %Platform%A==Android32A goto Android64
set Compiler=%IdeDir%\Bin\dccaarm.exe"
goto CompilerOK
:Android64
if not %Platform%A==Android64A goto Linux64
set Compiler=%IdeDir%\Bin\dccaarm64.exe"
goto CompilerOK
:Linux64
if not %Platform%A==Linux64A goto InvalidPlatform
set Compiler=%IdeDir%\Bin\dcclinux64.exe"
set CompilerOptions=%CompilerOptionsLinux64%
:CompilerOK

rem Compile DAC packages =================================================
%Compiler% %CompilerOptions% dac%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==OSX32A goto SkipDVcl
if %Platform%A==iOSSimulatorA goto SkipDVcl
if %Platform%A==iOSDeviceA goto SkipDVcl
if %Platform%A==iOSDevice32A goto SkipDVcl
if %Platform%A==iOSDevice64A goto SkipDVcl
if %Platform%A==AndroidA goto SkipDVcl
if %Platform%A==Android32A goto SkipDVcl
if %Platform%A==Android64A goto SkipDVcl
if %Platform%A==Linux64A goto SkipDVcl
%Compiler% %CompilerOptionsVCL% dacvcl%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDVcl

if %IDEVer%A==16A goto CompileDFmx
if %IDEVer%A==17A goto CompileDFmx
if %IDEVer%A==18A goto CompileDFmx
if %IDEVer%A==19A goto CompileDFmx
if %IDEVer%A==20A goto CompileDFmx
if %IDEVer%A==21A goto CompileDFmx
if %IDEVer%A==22A goto CompileDFmx
if %IDEVer%A==23A goto CompileDFmx
if %IDEVer%A==24A goto CompileDFmx
if %IDEVer%A==25A goto CompileDFmx
if %IDEVer%A==26A goto CompileDFmx
if %IDEVer%A==27A goto CompileDFmx
goto SkipDFmx

:CompileDFmx
set FmxFramework=FALSE
if %IDEVer%A==16A set FmxRegKey=Embarcadero\BDS\9.0
if %IDEVer%A==17A set FmxRegKey=Embarcadero\BDS\10.0
if %IDEVer%A==18A set FmxRegKey=Embarcadero\BDS\11.0
if %IDEVer%A==19A set FmxRegKey=Embarcadero\BDS\12.0
if %IDEVer%A==20A set FmxRegKey=Embarcadero\BDS\14.0
if %IDEVer%A==21A set FmxRegKey=Embarcadero\BDS\15.0
if %IDEVer%A==22A set FmxRegKey=Embarcadero\BDS\16.0
if %IDEVer%A==23A set FmxRegKey=Embarcadero\BDS\17.0
if %IDEVer%A==24A set FmxRegKey=Embarcadero\BDS\18.0
if %IDEVer%A==25A set FmxRegKey=Embarcadero\BDS\19.0
if %IDEVer%A==26A set FmxRegKey=Embarcadero\BDS\20.0
if %IDEVer%A==27A set FmxRegKey=Embarcadero\BDS\21.0

if not "%ProgramFiles(x86)%A" == "A" set FmxRegKey=Wow6432Node\%FmxRegKey%

REG QUERY "HKEY_LOCAL_MACHINE\SOFTWARE\%FmxRegKey%\Known Packages" /v $(BDS)\bin\dclfmxstd%PkgVer%.bpl
@if errorlevel 1 goto DetectFmx
set FmxFramework=TRUE
goto FmxDetected
:DetectFmx
REG QUERY "HKEY_LOCAL_MACHINE\SOFTWARE\%FmxRegKey%\Known Packages" /v $(BDSBIN)\dclfmxstd%PkgVer%.bpl
@if errorlevel 1 goto FmxDetected
set FmxFramework=TRUE
:FmxDetected

if not %FmxFramework%==TRUE goto SkipDFmx 
if %Platform%A==Linux64A goto SkipDFmx
%Compiler% %CompilerOptions% dacfmx%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDFmx

if %Platform%A==WIN64A goto SkipDcl
if %Platform%A==OSX32A goto SkipDcl
if %Platform%A==iOSSimulatorA goto SkipDcl
if %Platform%A==iOSDeviceA goto SkipDcl
if %Platform%A==iOSDevice32A goto SkipDcl
if %Platform%A==iOSDevice64A goto SkipDcl
if %Platform%A==AndroidA goto SkipDcl
if %Platform%A==Android32A goto SkipDcl
if %Platform%A==Android64A goto SkipDcl
if %Platform%A==Linux64A goto SkipDcl
%Compiler% %CompilerOptionsVCL% dcldac%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDcl

rem Compile CRControls package ===========================================
if %Platform%A==OSX32A goto Skip_Controls
if %Platform%A==iOSSimulatorA goto Skip_Controls
if %Platform%A==iOSDeviceA goto Skip_Controls
if %Platform%A==iOSDevice32A goto Skip_Controls
if %Platform%A==iOSDevice64A goto Skip_Controls
if %Platform%A==AndroidA goto Skip_Controls
if %Platform%A==Android32A goto Skip_Controls
if %Platform%A==Android64A goto Skip_Controls
if %Platform%A==Linux64A goto Skip_Controls
%Compiler% %CompilerOptionsVCL% crcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Controls

if %Platform%A==WIN64A goto Skip_Dcl
if %Platform%A==OSX32A goto Skip_Dcl
if %Platform%A==iOSSimulatorA goto Skip_Dcl
if %Platform%A==iOSDeviceA goto Skip_Dcl
if %Platform%A==iOSDevice32A goto Skip_Dcl
if %Platform%A==iOSDevice64A goto Skip_Dcl
if %Platform%A==AndroidA goto Skip_Dcl
if %Platform%A==Android32A goto Skip_Dcl
if %Platform%A==Android64A goto Skip_Dcl
if %Platform%A==Linux64A goto Skip_Dcl
%Compiler% %CompilerOptionsVCL% dclcrcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Dcl

rem Compile VirtualDAC packages ===========================================
set NoStatic=
if not %Platform%A==iOSSimulatorA goto skipNoStatic
set NoStatic=-DNOSTATIC
:skipNoStatic
%Compiler% %NoStatic% %CompilerOptions% vquery%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==WIN64A goto Skip___Dcl
if %Platform%A==OSX32A goto Skip___Dcl
if %Platform%A==iOSSimulatorA goto Skip___Dcl
if %Platform%A==iOSDeviceA goto Skip___Dcl
if %Platform%A==iOSDevice32A goto Skip___Dcl
if %Platform%A==iOSDevice64A goto Skip___Dcl
if %Platform%A==AndroidA goto Skip___Dcl
if %Platform%A==Android32A goto Skip___Dcl
if %Platform%A==Android64A goto Skip___Dcl
if %Platform%A==Linux64A goto Skip___Dcl
%Compiler% %CompilerOptionsVCL% dclvquery%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip___Dcl

rem Compile UniDAC packages ===========================================
%Compiler% %CompilerOptions% %PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==OSX32A goto Skip__Vcl
if %Platform%A==iOSSimulatorA goto Skip__Vcl
if %Platform%A==iOSDeviceA goto Skip__Vcl
if %Platform%A==iOSDevice32A goto Skip__Vcl
if %Platform%A==iOSDevice64A goto Skip__Vcl
if %Platform%A==AndroidA goto Skip__Vcl
if %Platform%A==Android32A goto Skip__Vcl
if %Platform%A==Android64A goto Skip__Vcl
if %Platform%A==Linux64A goto Skip__Vcl
%Compiler% %CompilerOptionsVCL% %PrjNameL%vcl%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Vcl

if %IDEVer%A==16A goto Compile__Fmx
if %IDEVer%A==17A goto Compile__Fmx
if %IDEVer%A==18A goto Compile__Fmx
if %IDEVer%A==19A goto Compile__Fmx
if %IDEVer%A==20A goto Compile__Fmx
if %IDEVer%A==21A goto Compile__Fmx
if %IDEVer%A==22A goto Compile__Fmx
if %IDEVer%A==23A goto Compile__Fmx
if %IDEVer%A==24A goto Compile__Fmx
if %IDEVer%A==25A goto Compile__Fmx
if %IDEVer%A==26A goto Compile__Fmx
if %IDEVer%A==27A goto Compile__Fmx
goto Skip__Fmx

:Compile__Fmx
if not %FmxFramework%==TRUE goto Skip__Fmx 
if %Platform%A==Linux64A goto Skip__Fmx
%Compiler% %CompilerOptions% %PrjNameL%fmx%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Fmx

if %Platform%A==WIN64A goto Skip__Dcl
if %Platform%A==OSX32A goto Skip__Dcl
if %Platform%A==iOSSimulatorA goto Skip__Dcl
if %Platform%A==iOSDeviceA goto Skip__Dcl
if %Platform%A==iOSDevice32A goto Skip__Dcl
if %Platform%A==iOSDevice64A goto Skip__Dcl
if %Platform%A==AndroidA goto Skip__Dcl
if %Platform%A==Android32A goto Skip__Dcl
if %Platform%A==Android64A goto Skip__Dcl
if %Platform%A==Linux64A goto Skip__Dcl
%Compiler% %CompilerOptionsVCL% dcl%PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Dcl

if %Platform%A==WIN64A goto Skip_Fmx_Dcl
if %Platform%A==OSX32A goto Skip_Fmx_Dcl
if %Platform%A==iOSSimulatorA goto Skip_Fmx_Dcl
if %Platform%A==iOSDeviceA goto Skip_Fmx_Dcl
if %Platform%A==iOSDevice32A goto Skip_Fmx_Dcl
if %Platform%A==iOSDevice64A goto Skip_Fmx_Dcl
if %Platform%A==AndroidA goto Skip_Fmx_Dcl
if %Platform%A==Android32A goto Skip_Fmx_Dcl
if %Platform%A==Android64A goto Skip_Fmx_Dcl
if %Platform%A==Linux64A goto Skip_Fmx_Dcl
if not %FmxFramework%A==TRUEA goto Skip_Fmx_Dcl 
%Compiler% %CompilerOptions% dcl%PrjNameL%fmx%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Fmx_Dcl

rem Compile Providers packages ===========================================
if not %OracleProvider%==TRUE goto ibproviderW32
%Compiler% %CompilerOptions% oraprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:ibproviderW32
if not %InterBaseProvider%==TRUE goto myproviderW32
%Compiler% %CompilerOptions% ibprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:myproviderW32
if not %MySQLProvider%==TRUE goto tdsprovider32
%Compiler% %CompilerOptions% myprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:tdsprovider32
if not %TDSProvider%==TRUE goto msproviderW32
%Compiler% %CompilerOptions% tdsprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:msproviderW32
if not %SQLServerProvider%==TRUE goto pgproviderW32
%Compiler% %CompilerOptions% msprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:pgproviderW32
if not %PostgreSQLProvider%==TRUE goto rsproviderW32
%Compiler% %CompilerOptions% pgprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:rsproviderW32
if not %RedshiftProvider%==TRUE goto liteproviderW32
%Compiler% %CompilerOptions% rsprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:liteproviderW32
if not %SQLiteProvider%==TRUE goto odbcproviderW32
%Compiler% %CompilerOptions% -U..\UniProviders\SQLite\ liteprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:odbcproviderW32
if not %ODBCProvider%==TRUE goto aseproviderW32
%Compiler% %CompilerOptions% odbcprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:aseproviderW32
if not %ASEProvider%==TRUE goto accessproviderW32
%Compiler% %CompilerOptions% aseprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:accessproviderW32
if not %AccessProvider%==TRUE goto db2providerW32
%Compiler% %CompilerOptions% accessprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:db2providerW32
if not %DB2Provider%==TRUE goto adsproviderW32
%Compiler% %CompilerOptions% db2provider%PkgVer%.dpk
@if errorlevel 1 goto Err

:adsproviderW32
if not %AdvantageProvider%==TRUE goto dbfproviderW32
%Compiler% %CompilerOptions% adsprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:dbfproviderW32
if not %DBFProvider%==TRUE goto nexusproviderW32
%Compiler% %CompilerOptions% dbfprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:nexusproviderW32
if not %NexusDBProvider%==TRUE goto mongoproviderW32
%Compiler% %CompilerOptions% nexusprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:mongoproviderW32
if not %MongoDBProvider%==TRUE goto bigcommerceproviderW32
%Compiler% %CompilerOptions% mongoprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:bigcommerceproviderW32
if not %BigCommerceProvider%==TRUE goto dynamicscrmproviderW32
%Compiler% %CompilerOptions% bigcommerceprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:dynamicscrmproviderW32
if not %DynamicsCRMProvider%==TRUE goto exacttargetproviderW32 
%Compiler% %CompilerOptions% dynamicsprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:exacttargetproviderW32
if not %ExactTargetProvider%==TRUE goto freshbooksroviderW32
%Compiler% %CompilerOptions% exacttargetprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:freshbooksroviderW32
if not %FreshBooksProvider%==TRUE goto magentoproviderW32
%Compiler% %CompilerOptions% freshbooksprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:magentoproviderW32
if not %MagentoProvider%==TRUE goto mailchimpproviderW32
%Compiler% %CompilerOptions% magentoprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:mailchimpproviderW32
if not %MailChimpProvider%==TRUE goto netsuiteproviderW32
%Compiler% %CompilerOptions% mailchimpprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:netsuiteproviderW32
if not %NetSuiteProvider%==TRUE goto quickbooksproviderW32
%Compiler% %CompilerOptions% netsuiteprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:quickbooksproviderW32
if not %QuickBooksProvider%==TRUE goto salesforceproviderW32
%Compiler% %CompilerOptions% quickbooksprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:salesforceproviderW32
if not %SalesforceProvider%==TRUE goto sugarcrmproviderW32
%Compiler% %CompilerOptions% salesforceprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:sugarcrmproviderW32
if not %SugarCRMProvider%==TRUE goto zohocrmproviderW32
%Compiler% %CompilerOptions% sugarprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:zohocrmproviderW32
if not %ZohoCRMProvider%==TRUE goto SkipProviderW32
%Compiler% %CompilerOptions% zohoprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:SkipProviderW32

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcp        move *.dcp               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.dcu" move "%%i\*.dcu" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

rem MAC OS X files ===
if not %Platform%A==OSX32A goto SkipOSX32Lib
if exist *.dylib      move *.dylib             ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipOSX32Lib

if not %Platform%A==iOSSimulatorA goto SkipiOSSimulatorLib
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSSimulatorLib

if not %Platform%A==iOSDeviceA goto SkipiOSDeviceLib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDeviceLib 

if not %Platform%A==iOSDevice32A goto SkipiOSDevice32Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDevice32Lib 

if not %Platform%A==iOSDevice64A goto SkipiOSDevice64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDevice64Lib 

if not %Platform%A==AndroidA goto SkipAndroidLib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroidLib 

if not %Platform%A==Android32A goto SkipAndroid32Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroid32Lib 

if not %Platform%A==Android64A goto SkipAndroid64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.o" move "%%i\*.o" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroid64Lib 

if not %Platform%A==Linux64A goto SkipLinux64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipLinux64Lib

rem CBuilder files ===
if exist  *.bpi       move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.lib       move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.a         move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.hpp       move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist  ..\*.hpp    move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
rem Copy providers ===
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.hpp" move "%%i\*.hpp" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

goto end

:CBuilder
rem Compile ==============================================================
rem Compile DAC packages =================================================
cd %DacDir%

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dac%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dacvcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dacvcl%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcldac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcldac%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile CRControls package ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk CRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f CRControls%PkgVer%.mak
@if errorlevel 1 goto Err
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dclCRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dclCRControls%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile VirtualDAC packages ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk vquery%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f vquery%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile UniDAC packages ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%vcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%vcl%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcl%PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcl%PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile Providers packages ===========================================
if not %OracleProvider%==TRUE goto ibproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk oraprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f oraprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:ibproviderCB
if not %InterBaseProvider%==TRUE goto myproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk ibprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f ibprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:myproviderCB
if not %MySQLProvider%==TRUE goto tdsproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk myprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f myprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:tdsproviderCB
if not %TDSProvider%==TRUE goto msproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk tdsprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f tdsprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:msproviderCB
if not %SQLServerProvider%==TRUE goto pgproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk msprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f msprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:pgproviderCB
if not %PostgreSQLProvider%==TRUE goto rsproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk pgprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f pgprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:rsproviderCB
if not %RedshiftProvider%==TRUE goto liteproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk rsprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f rsprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:liteproviderCB
if not %SQLiteProvider%==TRUE goto odbcproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk liteprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f liteprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:odbcproviderCB
if not %ODBCProvider%==TRUE goto aseproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk odbcprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f odbcprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:aseproviderCB
if not %ASEProvider%==TRUE goto accessproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk aseprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f aseprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:accessproviderCB
if not %AccessProvider%==TRUE goto db2providerCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk accessprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f accessprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:db2providerCB
if not %DB2Provider%==TRUE goto adsproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk db2provider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f db2provider%PkgVer%.mak
@if errorlevel 1 goto Err

:adsproviderCB
if not %AdvantageProvider%==TRUE goto dbfproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk adsprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f adsprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:dbfproviderCB
if not %DBFProvider%==TRUE goto nexusproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dbfprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dbfprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:nexusproviderCB
if not %NexusDBProvider%==TRUE goto mongoproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk nexusprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f nexusprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:mongoproviderCB
if not %MongoDBProvider%==TRUE goto SkipProviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk mongoprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f mongoprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:SkipProviderCB

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.tds        move *.tds               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.mak        move *.mak               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%

if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.lib        move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.obj        move *.obj               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.hpp        move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.hpp     move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%

for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.obj" move "%%i\*.obj" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.hpp" move "%%i\*.hpp" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

goto end

:InvalidPlatform
echo Invalid Platform

:Err
if %NeedPause%A == NA goto end
if %NeedPause%A == NOA goto end
pause

:end
popd
