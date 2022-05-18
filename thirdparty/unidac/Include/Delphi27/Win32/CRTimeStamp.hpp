// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRTimeStamp.pas' rev: 34.00 (Windows)

#ifndef CrtimestampHPP
#define CrtimestampHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysConst.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.DateUtils.hpp>
#include <System.Variants.hpp>
#include <Data.SqlTimSt.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crtimestamp
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall IsSQLTimeStampBlank(const Data::Sqltimst::TSQLTimeStamp &TimeStamp);
extern DELPHI_PACKAGE int __fastcall SQLTimeStampCompare(const Data::Sqltimst::TSQLTimeStamp &Value1, const Data::Sqltimst::TSQLTimeStamp &Value2);
extern DELPHI_PACKAGE bool __fastcall IsSQLTimeStampOffsetBlank(const Data::Sqltimst::TSQLTimeStampOffset &TimeStampOffset);
extern DELPHI_PACKAGE int __fastcall SQLTimeStampOffsetCompare(const Data::Sqltimst::TSQLTimeStampOffset &Value1, const Data::Sqltimst::TSQLTimeStampOffset &Value2);
extern DELPHI_PACKAGE System::TDateTime __fastcall SQLTimeStampOffsetToDateTime(const Data::Sqltimst::TSQLTimeStampOffset &DateTimeOffset);
extern DELPHI_PACKAGE Data::Sqltimst::TSQLTimeStampOffset __fastcall StrToSQLTimeStampOffset(const System::UnicodeString S, int Scale = 0x3);
extern DELPHI_PACKAGE Data::Sqltimst::TSQLTimeStamp __fastcall ConvertFromUTC(const Data::Sqltimst::TSQLTimeStampOffset &Value);
extern DELPHI_PACKAGE Data::Sqltimst::TSQLTimeStamp __fastcall ConvertToUTC(const Data::Sqltimst::TSQLTimeStampOffset &Value);
extern DELPHI_PACKAGE void __fastcall DateTimeToString(System::UnicodeString &Result, const System::UnicodeString Format, const Data::Sqltimst::TSQLTimeStamp &TimeStamp, const System::Sysutils::TFormatSettings &AFormatSettings);
extern DELPHI_PACKAGE unsigned __fastcall ExtractMSecFromString(const System::UnicodeString S, const System::Sysutils::TFormatSettings &AFormatSettings, int Scale);
}	/* namespace Crtimestamp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRTIMESTAMP)
using namespace Crtimestamp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrtimestampHPP
