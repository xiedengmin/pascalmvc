// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteConstsVirtual.pas' rev: 35.00 (Windows)

#ifndef LiteconstsvirtualHPP
#define LiteconstsvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Liteconstsvirtual
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TLiteEncryptionAlgorithm : unsigned char { leTripleDES, leBlowfish, leAES128, leAES192, leAES256, leCast128, leRC4, leDefault };

enum DECLSPEC_DENUM TConnectMode : unsigned char { cmDefault, cmReadWrite, cmReadOnly };

enum DECLSPEC_DENUM TLockingMode : unsigned char { lmNormal, lmExclusive };

enum DECLSPEC_DENUM TSynchronous : unsigned char { smOff, smNormal, smFull, smExtra };

enum DECLSPEC_DENUM TJournalMode : unsigned char { jmDelete, jmTruncate, jmPersist, jmMemory, jmWAL, jmOff };

typedef System::StaticArray<System::UnicodeString, 8> Liteconstsvirtual__1;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Liteconstsvirtual__1 LiteEncryptionAlgorithm;
static const TLiteEncryptionAlgorithm DefaultEncryptionAlgorithm = (TLiteEncryptionAlgorithm)(7);
static const bool DefValDirect = false;
static const bool DefValForceCreateDatabase = false;
static const bool DefValUnknownAsString = false;
static const bool DefValEnableSharedCache = false;
static const System::Int8 DefValBusyTimeout = System::Int8(0x0);
static const TConnectMode DefValConnectMode = (TConnectMode)(0);
static const TLockingMode DefValLockingMode = (TLockingMode)(1);
static const TSynchronous DefValSynchronous = (TSynchronous)(0);
static const TJournalMode DefValJournalMode = (TJournalMode)(0);
extern DELPHI_PACKAGE System::ResourceString _SStoredProcNotSupported;
#define Liteconstsvirtual_SStoredProcNotSupported System::LoadResourceString(&Liteconstsvirtual::_SStoredProcNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SClientLibraryDiffers;
#define Liteconstsvirtual_SClientLibraryDiffers System::LoadResourceString(&Liteconstsvirtual::_SClientLibraryDiffers)
extern DELPHI_PACKAGE System::ResourceString _SIncorrectParamCount;
#define Liteconstsvirtual_SIncorrectParamCount System::LoadResourceString(&Liteconstsvirtual::_SIncorrectParamCount)
extern DELPHI_PACKAGE System::ResourceString _SIncorrectParameter;
#define Liteconstsvirtual_SIncorrectParameter System::LoadResourceString(&Liteconstsvirtual::_SIncorrectParameter)
extern DELPHI_PACKAGE System::ResourceString _SInMemoryDB;
#define Liteconstsvirtual_SInMemoryDB System::LoadResourceString(&Liteconstsvirtual::_SInMemoryDB)
extern DELPHI_PACKAGE System::ResourceString _STempDB;
#define Liteconstsvirtual_STempDB System::LoadResourceString(&Liteconstsvirtual::_STempDB)
extern DELPHI_PACKAGE System::ResourceString _SDatabaseDoesntExist;
#define Liteconstsvirtual_SDatabaseDoesntExist System::LoadResourceString(&Liteconstsvirtual::_SDatabaseDoesntExist)
extern DELPHI_PACKAGE System::ResourceString _SBHCaption;
#define Liteconstsvirtual_SBHCaption System::LoadResourceString(&Liteconstsvirtual::_SBHCaption)
extern DELPHI_PACKAGE System::ResourceString _SDBBrowse;
#define Liteconstsvirtual_SDBBrowse System::LoadResourceString(&Liteconstsvirtual::_SDBBrowse)
extern DELPHI_PACKAGE System::ResourceString _SDirectNotSupported;
#define Liteconstsvirtual_SDirectNotSupported System::LoadResourceString(&Liteconstsvirtual::_SDirectNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SFunctionNotLinked;
#define Liteconstsvirtual_SFunctionNotLinked System::LoadResourceString(&Liteconstsvirtual::_SFunctionNotLinked)
extern DELPHI_PACKAGE System::ResourceString _SDirectIsDisabled;
#define Liteconstsvirtual_SDirectIsDisabled System::LoadResourceString(&Liteconstsvirtual::_SDirectIsDisabled)
extern DELPHI_PACKAGE System::ResourceString _SCannotUseAlgorithm;
#define Liteconstsvirtual_SCannotUseAlgorithm System::LoadResourceString(&Liteconstsvirtual::_SCannotUseAlgorithm)
extern DELPHI_PACKAGE System::ResourceString _SDefaultAlgorithm;
#define Liteconstsvirtual_SDefaultAlgorithm System::LoadResourceString(&Liteconstsvirtual::_SDefaultAlgorithm)
extern DELPHI_PACKAGE System::ResourceString _SEncryptionKeyIsEmpty;
#define Liteconstsvirtual_SEncryptionKeyIsEmpty System::LoadResourceString(&Liteconstsvirtual::_SEncryptionKeyIsEmpty)
extern DELPHI_PACKAGE System::ResourceString _SEncryptionKeyMustBeEmpty;
#define Liteconstsvirtual_SEncryptionKeyMustBeEmpty System::LoadResourceString(&Liteconstsvirtual::_SEncryptionKeyMustBeEmpty)
extern DELPHI_PACKAGE System::ResourceString _SCannotEncrypt;
#define Liteconstsvirtual_SCannotEncrypt System::LoadResourceString(&Liteconstsvirtual::_SCannotEncrypt)
extern DELPHI_PACKAGE System::ResourceString _SBackupSourceNotSpecified;
#define Liteconstsvirtual_SBackupSourceNotSpecified System::LoadResourceString(&Liteconstsvirtual::_SBackupSourceNotSpecified)
extern DELPHI_PACKAGE System::ResourceString _SBackupDestinationNotSpecified;
#define Liteconstsvirtual_SBackupDestinationNotSpecified System::LoadResourceString(&Liteconstsvirtual::_SBackupDestinationNotSpecified)
extern DELPHI_PACKAGE System::ResourceString _SBackupConnectionsEqual;
#define Liteconstsvirtual_SBackupConnectionsEqual System::LoadResourceString(&Liteconstsvirtual::_SBackupConnectionsEqual)
extern DELPHI_PACKAGE System::ResourceString _SBackupSourceNotConnected;
#define Liteconstsvirtual_SBackupSourceNotConnected System::LoadResourceString(&Liteconstsvirtual::_SBackupSourceNotConnected)
extern DELPHI_PACKAGE System::ResourceString _SBackupDestinationNotConnected;
#define Liteconstsvirtual_SBackupDestinationNotConnected System::LoadResourceString(&Liteconstsvirtual::_SBackupDestinationNotConnected)
}	/* namespace Liteconstsvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITECONSTSVIRTUAL)
using namespace Liteconstsvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LiteconstsvirtualHPP
