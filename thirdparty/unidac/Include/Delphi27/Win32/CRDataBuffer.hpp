// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDataBuffer.pas' rev: 34.00 (Windows)

#ifndef CrdatabufferHPP
#define CrdatabufferHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdatabuffer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDataBuffer;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataBuffer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FReceiveBuffer;
	void *FReadBuffer;
	void *FWriteBuffer;
	int FReadPos;
	int FWritePos;
	int FChunkSize;
	int __fastcall GetDataLength();
	
public:
	__fastcall TDataBuffer(int ChunkSize);
	__fastcall virtual ~TDataBuffer();
	void __fastcall Clear();
	void __fastcall Read(const char * Data, int Offset, int Count);
	void __fastcall Write(const char * Data, int Offset, int Count);
	int __fastcall SearchFromIndex(const System::Byte SearchByte, const int Index);
	__property int DataLength = {read=GetDataLength, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crdatabuffer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDATABUFFER)
using namespace Crdatabuffer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdatabufferHPP
