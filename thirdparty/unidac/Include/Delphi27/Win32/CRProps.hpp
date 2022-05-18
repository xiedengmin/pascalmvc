// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRProps.pas' rev: 34.00 (Windows)

#ifndef CrpropsHPP
#define CrpropsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crprops
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::Int8 prUsername = System::Int8(0x1);
static const System::Int8 prPassword = System::Int8(0x2);
static const System::Int8 prServer = System::Int8(0x3);
static const System::Int8 prConnectionTimeout = System::Int8(0x4);
static const System::Int8 prAutoCommit = System::Int8(0x5);
static const System::Int8 prSQL = System::Int8(0x6);
static const System::Int8 prScanParams = System::Int8(0x7);
static const System::Int8 prSQLType = System::Int8(0x8);
static const System::Int8 prRowsProcessed = System::Int8(0x9);
static const System::Int8 prUniDirectional = System::Int8(0xa);
static const System::Int8 prFetchRows = System::Int8(0xb);
static const System::Int8 prFetchAll = System::Int8(0xc);
static const System::Int8 prLongStrings = System::Int8(0xe);
static const System::Int8 prFlatBuffers = System::Int8(0xf);
static const System::Int8 prConvertEOL = System::Int8(0x10);
static const System::Int8 prIndexFieldNames = System::Int8(0x11);
static const System::Int8 prCompressBlobMode = System::Int8(0x12);
static const System::Int8 prDisconnectedMode = System::Int8(0x13);
static const System::Int8 prDisableParamScan = System::Int8(0x14);
static const System::Int8 prQuoteNames = System::Int8(0x15);
static const System::Int8 prIsolationLevel = System::Int8(0x16);
static const System::Int8 prTransactionReadOnly = System::Int8(0x17);
static const System::Int8 prDatabase = System::Int8(0x18);
static const System::Int8 prPort = System::Int8(0x19);
static const System::Int8 prMaxStringSize = System::Int8(0x1a);
static const System::Int8 prEnableBCD = System::Int8(0x1b);
static const System::Int8 prEnableFmtBCD = System::Int8(0x1c);
static const System::Int8 prCanReadParams = System::Int8(0x1d);
static const System::Int8 prReadOnly = System::Int8(0x1e);
static const System::Int8 prIsStoredProc = System::Int8(0x1f);
static const System::Int8 prIsSelectParams = System::Int8(0x20);
static const System::Int8 prLockFetchAll = System::Int8(0x21);
static const System::Int8 prDefaultSortType = System::Int8(0x22);
static const System::Int8 prLastInsertId = System::Int8(0x23);
static const System::Int8 prFieldsAsString = System::Int8(0x24);
static const System::Int8 prExtendedFieldsInfo = System::Int8(0x25);
static const System::Int8 prDefaultValues = System::Int8(0x26);
static const System::Int8 prFieldOrigins = System::Int8(0x27);
static const System::Int8 prUseDescribeParams = System::Int8(0x28);
static const System::Int8 prRoAfterUpdate = System::Int8(0x29);
static const System::Int8 prEncryptedFields = System::Int8(0x2a);
static const System::Int8 prOpenNext = System::Int8(0x2b);
static const System::Int8 prSensibleBCDMapping = System::Int8(0x2c);
static const System::Int8 prCursorUpdate = System::Int8(0x2d);
static const System::Int8 prMultipleConnections = System::Int8(0x2e);
static const System::Int8 prUuidWithBraces = System::Int8(0x2f);
static const System::Int8 prPrefetchRows = System::Int8(0x30);
static const System::Int8 prUseHttp = System::Int8(0x32);
static const System::Int8 prHttpUrl = System::Int8(0x33);
static const System::Int8 prHttpUsername = System::Int8(0x34);
static const System::Int8 prHttpPassword = System::Int8(0x35);
static const System::Int8 prHttpTrustServerCertificate = System::Int8(0x36);
static const System::Int8 prProxyHostname = System::Int8(0x37);
static const System::Int8 prProxyPort = System::Int8(0x38);
static const System::Int8 prProxyUsername = System::Int8(0x39);
static const System::Int8 prProxyPassword = System::Int8(0x3a);
static const System::Int8 prProxySocksVersion = System::Int8(0x3b);
static const System::Int8 prProxyResolveDNS = System::Int8(0x3c);
static const System::Int8 prUseSSL = System::Int8(0x3d);
static const System::Int8 prSSLCA = System::Int8(0x3e);
static const System::Int8 prSSLCert = System::Int8(0x3f);
static const System::Int8 prSSLKey = System::Int8(0x40);
static const System::Int8 prSSLCipher = System::Int8(0x41);
static const System::Int8 prSSLIgnoreServerCertificateValidity = System::Int8(0x42);
static const System::Int8 prSSLIgnoreServerCertificateConstraints = System::Int8(0x43);
static const System::Int8 prSSLTrustServerCertificate = System::Int8(0x44);
static const System::Int8 prSSLIgnoreServerCertificateInsecurity = System::Int8(0x45);
static const System::Int8 prUseSSH = System::Int8(0x47);
static const System::Int8 prSSHHostname = System::Int8(0x48);
static const System::Int8 prSSHPort = System::Int8(0x49);
static const System::Int8 prSSHUsername = System::Int8(0x4a);
static const System::Int8 prSSHPassword = System::Int8(0x4b);
static const System::Int8 prSSHClientKey = System::Int8(0x4c);
static const System::Int8 prSSHServerKey = System::Int8(0x4d);
static const System::Int8 prSSHStoragePath = System::Int8(0x4e);
static const System::Int8 prSSHClientKeyPassword = System::Int8(0x4f);
static const System::Int8 prIPVersion = System::Int8(0x50);
static const System::Int8 prFullRefresh = System::Int8(0x51);
static const System::Int8 prSetFieldsReadOnly = System::Int8(0x52);
static const System::Int8 prKeyFields = System::Int8(0x53);
static const System::Int8 prUpdatingTable = System::Int8(0x54);
static const System::Int8 prKeySequence = System::Int8(0x55);
static const System::Int8 prPrefetchedFields = System::Int8(0x56);
static const System::Int8 prLiveBlockOnSmartFetch = System::Int8(0x57);
static const System::Int8 prParamsProcessed = System::Int8(0x58);
static const System::Int8 prCommandTimeout = System::Int8(0x59);
static const System::Int8 prUseUnicode = System::Int8(0x5a);
static const System::Int8 prInsertAllSetFields = System::Int8(0x5b);
static const System::Int8 prNonBlocking = System::Int8(0x5c);
static const System::Int8 prBatchIters = System::Int8(0x5d);
static const System::Int8 prBatchOffset = System::Int8(0x5e);
static const System::Int8 prUseBlankValues = System::Int8(0x5f);
static const System::Int8 prUTCDates = System::Int8(0x60);
}	/* namespace Crprops */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRPROPS)
using namespace Crprops;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrpropsHPP
