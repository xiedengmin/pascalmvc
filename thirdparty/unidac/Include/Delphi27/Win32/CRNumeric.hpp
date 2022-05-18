// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRNumeric.pas' rev: 34.00 (Windows)

#ifndef CrnumericHPP
#define CrnumericHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Data.DBConsts.hpp>
#include <Data.FmtBcd.hpp>
#include <DAConsts.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRBigInteger.hpp>
#include <MemUtils.hpp>

//-- user supplied -----------------------------------------------------------
#include <OleDB.h>

namespace Crnumeric
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const int SizeOf_TDBNumeric = int(0x13);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DBNumericToStr(const DB_NUMERIC &Value);
extern DELPHI_PACKAGE Data::Fmtbcd::TBcd __fastcall DBNumericToBCD(const DB_NUMERIC &Value);
extern DELPHI_PACKAGE double __fastcall DBNumericToDouble(const DB_NUMERIC &Value);
extern DELPHI_PACKAGE DB_NUMERIC __fastcall BcdToDBNumeric(const Data::Fmtbcd::TBcd &Bcd);
extern DELPHI_PACKAGE DB_NUMERIC __fastcall DoubleToDBNumeric(double Value, int Precision, int Scale);
}	/* namespace Crnumeric */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRNUMERIC)
using namespace Crnumeric;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrnumericHPP
