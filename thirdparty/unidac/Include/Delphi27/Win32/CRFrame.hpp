// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRFrame.pas' rev: 34.00 (Windows)

#ifndef CrframeHPP
#define CrframeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Classes.hpp>
#include <DAEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRFrame;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TCRFrameClass;

class PASCALIMPLEMENTATION TCRFrame : public Vcl::Forms::TFrame
{
	typedef Vcl::Forms::TFrame inherited;
	
__published:
	void __fastcall FrameEnter(System::TObject* Sender);
	
protected:
	bool FModified;
	Daeditor::TDAEditorForm* FEditor;
	bool FActivated;
	Vcl::Comctrls::TTabSheet* __fastcall GetPage();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	
public:
	virtual Vcl::Controls::TWinControl* __fastcall ActiveControl();
	void __fastcall Activate();
	void __fastcall Finish();
	void __fastcall ReActivate();
	__property bool Activated = {read=FActivated, nodefault};
	__property Vcl::Comctrls::TTabSheet* Page = {read=GetPage};
	__property Daeditor::TDAEditorForm* Editor = {read=FEditor, write=FEditor};
	__property bool Modified = {read=FModified, write=FModified, nodefault};
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TCRFrame(System::Classes::TComponent* AOwner) : Vcl::Forms::TFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TCRFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRFrame(HWND ParentWindow) : Vcl::Forms::TFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRFRAME)
using namespace Crframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrframeHPP
