// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CREncryption.pas' rev: 34.00 (Windows)

#ifndef CrencryptionHPP
#define CrencryptionHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRSymmetricAlgorithm.hpp>
#include <CRHashAlgorithm.hpp>
#include <CRRNG.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crencryption
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCREncryptor;
class DELPHICLASS EInvalidEncData;
class DELPHICLASS EInvalidHash;
class DELPHICLASS TCREncryptorUtils;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCRDecryptedDataType : unsigned char { ddtDecrypted, ddtNonEncrypted, ddtError };

typedef System::DynamicArray<System::Byte> _dt_Crencryption_1;
typedef _dt_Crencryption_1 __fastcall (__closure *TBytesEncryptionMethod)(const char * Data, unsigned &Length);

typedef System::Variant __fastcall (__closure *TEncryptionMethod)(const char * Data, System::Word DataType, unsigned &Length);

typedef void __fastcall (__closure *TDecryptionMethod)(void * Data, System::Word DataType, unsigned &Length, /* out */ TCRDecryptedDataType &DecryptedDataType);

enum DECLSPEC_DENUM TCREncryptionAlgorithm : unsigned char { eaTripleDES, eaBlowfish, eaAES128, eaAES192, eaAES256, eaCast128, eaRC4 };

enum DECLSPEC_DENUM TCRHashAlgorithm : unsigned char { haSHA1, haMD5 };

enum DECLSPEC_DENUM TCREncDataHeader : unsigned char { ehTagAndHash, ehTag, ehNone };

enum DECLSPEC_DENUM TCRInvalidHashAction : unsigned char { ihFail, ihSkipData, ihIgnoreError };

class PASCALIMPLEMENTATION TCREncryptor : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TCREncDataHeader FDataHeader;
	TCREncryptionAlgorithm FEncryptionAlgorithm;
	TCRHashAlgorithm FHashAlgorithm;
	TCRInvalidHashAction FInvalidHashAction;
	Crsymmetricalgorithm::TSymmetricAlgorithm* FEncryptor;
	Crhashalgorithm::THashAlgorithm* FHashSHA1;
	Crhashalgorithm::THashAlgorithm* FHashMD5;
	Crhashalgorithm::THashAlgorithm* FHash;
	System::UnicodeString FPassword;
	System::DynamicArray<System::Byte> FKeyFromPassword;
	System::DynamicArray<System::Byte> FKey;
	void *FTmpBuffer;
	void __fastcall SetEncryptionAlgorithm(const TCREncryptionAlgorithm Value);
	void __fastcall SetPassword(const System::UnicodeString Value);
	void __fastcall Password2Key();
	void __fastcall CheckEncryptor();
	
protected:
	System::DynamicArray<System::Byte> __fastcall EncryptBytes(const char * Data, unsigned &Length);
	System::Variant __fastcall Encrypt(const char * Data, System::Word DataType, unsigned &Length);
	void __fastcall Decrypt(void * Data, System::Word DataType, unsigned &Length, /* out */ TCRDecryptedDataType &DecryptedDataType);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadEncryptedPassword(System::Classes::TReader* Reader);
	void __fastcall ReadPassword(System::Classes::TReader* Reader);
	void __fastcall WriteEncryptedPassword(System::Classes::TWriter* Writer);
	System::UnicodeString __fastcall EncryptToHex(const System::UnicodeString Value);
	System::UnicodeString __fastcall DecryptFromHex(const System::UnicodeString Value);
	virtual System::DynamicArray<System::Byte> __fastcall EncryptPassword(const System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall DecryptPassword(const System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TCREncryptor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCREncryptor();
	void __fastcall SetKey(const void *Key, int Count)/* overload */;
	void __fastcall SetKey(const System::DynamicArray<System::Byte> Key, int Offset, int Count)/* overload */;
	
__published:
	__property TCREncDataHeader DataHeader = {read=FDataHeader, write=FDataHeader, default=0};
	__property TCREncryptionAlgorithm EncryptionAlgorithm = {read=FEncryptionAlgorithm, write=SetEncryptionAlgorithm, default=1};
	__property TCRHashAlgorithm HashAlgorithm = {read=FHashAlgorithm, write=FHashAlgorithm, default=0};
	__property TCRInvalidHashAction InvalidHashAction = {read=FInvalidHashAction, write=FInvalidHashAction, default=0};
	__property System::UnicodeString Password = {read=FPassword, write=SetPassword, stored=false};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidEncData : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidEncData(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidEncData(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidEncData(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidEncData(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidEncData(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidEncData(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidEncData(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidEncData(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidEncData(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidEncData(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidEncData(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidEncData(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidEncData() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidHash : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EInvalidHash(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EInvalidHash(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidHash(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidHash(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidHash(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidHash(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidHash(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidHash(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidHash(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidHash(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidHash(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidHash(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidHash() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCREncryptorUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod TBytesEncryptionMethod __fastcall BytesEncryptor(TCREncryptor* Obj);
	__classmethod TEncryptionMethod __fastcall Encryptor(TCREncryptor* Obj);
	__classmethod TDecryptionMethod __fastcall Decryptor(TCREncryptor* Obj);
public:
	/* TObject.Create */ inline __fastcall TCREncryptorUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCREncryptorUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool IgnoreInvalidHashLength;
extern DELPHI_PACKAGE Crrng::TScRandom* __fastcall _GetRandomGenerator(void);
}	/* namespace Crencryption */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRENCRYPTION)
using namespace Crencryption;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrencryptionHPP
