// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DacVcl.pas' rev: 34.00 (Windows)

#ifndef DacvclHPP
#define DacvclHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Data.DB.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <MemData.hpp>
#include <DBAccess.hpp>
#include <DASQLMonitor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dacvcl
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Uitypes::TCursor __fastcall GetScreenCursor(void);
extern DELPHI_PACKAGE void __fastcall SetScreenCursor(System::Uitypes::TCursor Value);
extern DELPHI_PACKAGE void __fastcall SetCursor(int Value);
extern DELPHI_PACKAGE void __fastcall ShowDebugForm(Dasqlmonitor::TDASQLMonitorClass DASQLMonitorClass, System::Classes::TComponent* Component, System::UnicodeString SQL, Dbaccess::TDAParams* Params, System::UnicodeString Caption);
extern DELPHI_PACKAGE bool __fastcall ShowConnectForm(Dbaccess::TCustomConnectDialog* ConnectDialog);
extern DELPHI_PACKAGE void __fastcall StartWait(void);
extern DELPHI_PACKAGE void __fastcall StopWait(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ApplicationTitle(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetHelpFileName(const System::UnicodeString ProjectName, bool UseCHM = false);
}	/* namespace Dacvcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DACVCL)
using namespace Dacvcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DacvclHPP
