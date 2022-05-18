{$B-}

{$I IdeVer.inc}

{$IFDEF CPUX64} 
  {$DEFINE CPU64}  { CPU64 - 64 bit Intel and ARM processor }
{$ENDIF}
{$IFDEF CPUARM64} 
  {$DEFINE CPU64} { CPU64 - 64 bit Intel and ARM processor }
{$ENDIF}
{$IFDEF CPU86}
  {$DEFINE CPUX86} { Lazarus compatibility }
{$ENDIF}
{$IFDEF CPUX86} 
  {$DEFINE CPU32} { CPU32 - 32 bit Intel and ARM processor }
{$ENDIF}
{$IFDEF CPUARM32} 
  {$DEFINE CPU32} { CPU32 - 32 bit Intel and ARM processor }
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}

{$IFDEF IOS}
  {$DEFINE MOBILE}
  {$IFNDEF IOS64}
    {$IFNDEF IOS32}
      {$DEFINE IOS32}
    {$ENDIF} 
  {$ENDIF} 
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE MOBILE}
  {$IFNDEF ANDROID64}
    {$IFNDEF ANDROID32}
      {$DEFINE ANDROID32}
    {$ENDIF} 
  {$ENDIF} 
{$ENDIF}

{$IFDEF IOS}
  {$IFDEF CPUARM}
    {$IFNDEF CPUARM64}
       {$ALIGN 8} // workaround on ALIGN for IOS32 (define CPUARM32 is unknown in XE7 and low)
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE MACOS}
  {$IFDEF CPU32}
    {$DEFINE MACOS32}
  {$ENDIF}
  {$IFDEF CPU64}
    {$DEFINE MACOS64}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF UNIX}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$IFDEF IPHONESIM}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$IFNDEF MSWINDOWS}
  {$IFDEF CPU64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF PUREPASCAL}
    {$ASMMODE INTEL}
  {$ENDIF}
  {$MODE DELPHI}
  {$INTERFACES COM}
{$ELSE}
  {$IFDEF NEXTGEN}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$IFDEF MOBILE}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$IFDEF LINUX}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
  {$IFDEF MACOS64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE USE_SYNEDIT}
{$ENDIF}

{$IFDEF INTERNAL}
{$IFDEF BCB}
  {$DEFINE BDE_SHARED} // Use TUpdateAction from DBTables
{$ENDIF}
{$ENDIF}

{$IFNDEF NOINLINE}
  {$IFDEF FPC}
    {$DEFINE USE_INLINE}
  {$ENDIF}
  {$IFDEF VER9P}
    {$DEFINE USE_INLINE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER7P}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF VER6P}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFNDEF VER6P}
  // Delphi5, CBuilder5 - UTF8 functions is not implemented
  {$DEFINE UTF8}
{$ENDIF}

{----------------------------
 d5-d6 - ZLib objs
 d7p   - Delphi internal
 ----------------------------}
{$IFNDEF FPC} // should be removed after fix ZLib in FPC
  {$DEFINE HAVE_COMPRESS_INTERFACE}
  {$IFDEF VER7P}
    {$IFNDEF LINUX}
      {$DEFINE HAVE_COMPRESS_INTERNAL} // ZLib build in Delphi
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF LITE}
    {$DEFINE HAVE_COMPRESS} // Define TCompressBlob class
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
//  {$IFNDEF NODBTOOLS} // for all products without db tools integration
//    {$IFDEF VER6P}
//      {$DEFINE DBTOOLS}      // include DBToolsClient unit and related code
//      {$IFDEF VER9}
//        {$UNDEF DBTOOLS}
//      {$ENDIF}
//      {$IFDEF VER10}
//        {$UNDEF DBTOOLS}
//      {$ENDIF}
//    {$ENDIF}
//  {$ENDIF}

  {$DEFINE WITH_IPROVIDER}
{$ENDIF}

{$IFDEF WIN32_64}
{$IFDEF VER6P}
{$IFNDEF BCB}
{$IFNDEF FPC}
  {$DEFINE USE_CODE_EDITOR}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF USE_CODE_EDITOR}
  {$DEFINE USE_EXT_MENU}
{$ENDIF}
{$IFDEF USE_SYNEDIT}
  {$DEFINE USE_EXT_MENU}
{$ENDIF}


{$IFDEF VER12P}
  {$DEFINE IS_UNICODE}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE IS_UTF8}
  {$DEFINE IS_UTF8_INCLUDE_MEMO}
    {$DEFINE ONLY_UTF8_MEMO}
{$ELSE}
  {$IFDEF LINUX}
    {$DEFINE IS_UTF8}
    {$IFDEF NEXTGEN}
      {$DEFINE IS_UTF8_EXCLUDE_MEMO}
    {$ELSE}
      {$DEFINE IS_UTF8_INCLUDE_MEMO}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// dbExpress for PostgreSQL supports SSL connection
{$IFNDEF IOS}
  {$DEFINE HAVE_OPENSSL}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF PUREPASCAL}
    {$IFNDEF CPU64}
      {$DEFINE UseASM}   // use Assembler optimated core
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF VER7P}
  {$DEFINE USE_TFORMATSETTINGS}
{$ENDIF}
{$IFDEF VER16P}
  {$DEFINE USE_FORMATSETTINGS}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE USE_FORMATSETTINGS}
  {$DEFINE USE_TFORMATSETTINGS}
{$ENDIF}

  {$DEFINE ONLY_NULL_TERMINATED_STRINGS}

{$IFDEF LITE}
  {$DEFINE NODBACCESS}
{$ENDIF}

{$TYPEDADDRESS OFF}
