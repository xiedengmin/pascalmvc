// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADumpProgress.pas' rev: 34.00 (Windows)

#ifndef DadumpprogressHPP
#define DadumpprogressHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.DBGrids.hpp>
#include <Vcl.DBCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <DADump.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadumpprogress
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADumpProgressForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDADumpProgressForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Buttons::TBitBtn* BitBtn1;
	Vcl::Comctrls::TProgressBar* ProgressBar1;
	Vcl::Comctrls::TProgressBar* ProgressBar2;
	void __fastcall DADumpBackupProgress(System::TObject* Sender, System::UnicodeString TableName, int TableNum, int TableCount, int Percent);
	void __fastcall DADumpRestoreProgress(System::TObject* Sender, int Percent);
	void __fastcall FormActivate(System::TObject* Sender);
	void __fastcall BitBtn1Click(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	
protected:
	bool IsBackup;
	bool WaitForTerminate;
	bool InProgress;
	void __fastcall Process();
	
public:
	Dadump::TDADump* DADump;
	void __fastcall Backup();
	void __fastcall Restore();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TDADumpProgressForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDADumpProgressForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDADumpProgressForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDADumpProgressForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dadumpprogress */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADUMPPROGRESS)
using namespace Dadumpprogress;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadumpprogressHPP
