// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAConsts.pas' rev: 34.00 (Windows)

#ifndef DaconstsHPP
#define DaconstsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daconsts
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::WideChar DALineSeparator = (System::WideChar)(0xd);
#define SLLineSeparator L"\r\n"
static const System::WideChar Tabulation = (System::WideChar)(0x9);
static const System::Int8 MaxUTF8CharLen = System::Int8(0x3);
static const bool DefValLoginPrompt = true;
static const bool DefValPooling = false;
static const System::Int8 DefValConnectionLifetime = System::Int8(0x0);
static const System::Int8 DefValMaxPoolSize = System::Int8(0x64);
static const System::Int8 DefValMinPoolSize = System::Int8(0x0);
static const bool DefValValidate = false;
static const System::Int8 DefValConnectionTimeout = System::Int8(0xf);
static const bool DefValUseUnicode = false;
static const System::Int8 DefValProxyPort = System::Int8(0x0);
extern DELPHI_PACKAGE System::ResourceString _SUnknownDataType;
#define Daconsts_SUnknownDataType System::LoadResourceString(&Daconsts::_SUnknownDataType)
extern DELPHI_PACKAGE System::ResourceString _SDataTypeNotSupported;
#define Daconsts_SDataTypeNotSupported System::LoadResourceString(&Daconsts::_SDataTypeNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SFieldNotFound;
#define Daconsts_SFieldNotFound System::LoadResourceString(&Daconsts::_SFieldNotFound)
extern DELPHI_PACKAGE System::ResourceString _SAttributeNotFount;
#define Daconsts_SAttributeNotFount System::LoadResourceString(&Daconsts::_SAttributeNotFount)
extern DELPHI_PACKAGE System::ResourceString _SCannotConvertType;
#define Daconsts_SCannotConvertType System::LoadResourceString(&Daconsts::_SCannotConvertType)
extern DELPHI_PACKAGE System::ResourceString _SIllegalFilter;
#define Daconsts_SIllegalFilter System::LoadResourceString(&Daconsts::_SIllegalFilter)
extern DELPHI_PACKAGE System::ResourceString _SIllegalConstraint;
#define Daconsts_SIllegalConstraint System::LoadResourceString(&Daconsts::_SIllegalConstraint)
extern DELPHI_PACKAGE System::ResourceString _SNeedBlobType;
#define Daconsts_SNeedBlobType System::LoadResourceString(&Daconsts::_SNeedBlobType)
extern DELPHI_PACKAGE System::ResourceString _SInvalidSharedObject;
#define Daconsts_SInvalidSharedObject System::LoadResourceString(&Daconsts::_SInvalidSharedObject)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBlob;
#define Daconsts_SInvalidBlob System::LoadResourceString(&Daconsts::_SInvalidBlob)
extern DELPHI_PACKAGE System::ResourceString _SBlobMustBeCached;
#define Daconsts_SBlobMustBeCached System::LoadResourceString(&Daconsts::_SBlobMustBeCached)
extern DELPHI_PACKAGE System::ResourceString _SCachedAlreadyEnabled;
#define Daconsts_SCachedAlreadyEnabled System::LoadResourceString(&Daconsts::_SCachedAlreadyEnabled)
extern DELPHI_PACKAGE System::ResourceString _SKeyFieldsRequired;
#define Daconsts_SKeyFieldsRequired System::LoadResourceString(&Daconsts::_SKeyFieldsRequired)
extern DELPHI_PACKAGE System::ResourceString _SKeyFieldsReq;
#define Daconsts_SKeyFieldsReq System::LoadResourceString(&Daconsts::_SKeyFieldsReq)
extern DELPHI_PACKAGE System::ResourceString _SNoKeyFields;
#define Daconsts_SNoKeyFields System::LoadResourceString(&Daconsts::_SNoKeyFields)
extern DELPHI_PACKAGE System::ResourceString _SBadTableInfoName;
#define Daconsts_SBadTableInfoName System::LoadResourceString(&Daconsts::_SBadTableInfoName)
extern DELPHI_PACKAGE System::ResourceString _SBadStatementType;
#define Daconsts_SBadStatementType System::LoadResourceString(&Daconsts::_SBadStatementType)
extern DELPHI_PACKAGE System::ResourceString _SBadUpdatingTable;
#define Daconsts_SBadUpdatingTable System::LoadResourceString(&Daconsts::_SBadUpdatingTable)
extern DELPHI_PACKAGE System::ResourceString _SKeyFieldNotFound;
#define Daconsts_SKeyFieldNotFound System::LoadResourceString(&Daconsts::_SKeyFieldNotFound)
extern DELPHI_PACKAGE System::ResourceString _SNotRows;
#define Daconsts_SNotRows System::LoadResourceString(&Daconsts::_SNotRows)
extern DELPHI_PACKAGE System::ResourceString _SIndexFieldNamesWithUniDirectional;
#define Daconsts_SIndexFieldNamesWithUniDirectional System::LoadResourceString(&Daconsts::_SIndexFieldNamesWithUniDirectional)
extern DELPHI_PACKAGE System::ResourceString _SInvalidUnComprBlobSize;
#define Daconsts_SInvalidUnComprBlobSize System::LoadResourceString(&Daconsts::_SInvalidUnComprBlobSize)
extern DELPHI_PACKAGE System::ResourceString _SInvalidComprBlobSize;
#define Daconsts_SInvalidComprBlobSize System::LoadResourceString(&Daconsts::_SInvalidComprBlobSize)
extern DELPHI_PACKAGE System::ResourceString _SInvalidComprBlobHeader;
#define Daconsts_SInvalidComprBlobHeader System::LoadResourceString(&Daconsts::_SInvalidComprBlobHeader)
extern DELPHI_PACKAGE System::ResourceString _SInvalidComprBlobData;
#define Daconsts_SInvalidComprBlobData System::LoadResourceString(&Daconsts::_SInvalidComprBlobData)
extern DELPHI_PACKAGE System::ResourceString _SDataSetIsNotPrepared;
#define Daconsts_SDataSetIsNotPrepared System::LoadResourceString(&Daconsts::_SDataSetIsNotPrepared)
extern DELPHI_PACKAGE System::ResourceString _SInvalidKeyField;
#define Daconsts_SInvalidKeyField System::LoadResourceString(&Daconsts::_SInvalidKeyField)
extern DELPHI_PACKAGE System::ResourceString _SNotCachedUpdate;
#define Daconsts_SNotCachedUpdate System::LoadResourceString(&Daconsts::_SNotCachedUpdate)
extern DELPHI_PACKAGE System::ResourceString _SUpdateWrongDB;
#define Daconsts_SUpdateWrongDB System::LoadResourceString(&Daconsts::_SUpdateWrongDB)
extern DELPHI_PACKAGE System::ResourceString _SDataSetMustBeInState;
#define Daconsts_SDataSetMustBeInState System::LoadResourceString(&Daconsts::_SDataSetMustBeInState)
extern DELPHI_PACKAGE System::ResourceString _SDataSetMustNotHavePendingUpdates;
#define Daconsts_SDataSetMustNotHavePendingUpdates System::LoadResourceString(&Daconsts::_SDataSetMustNotHavePendingUpdates)
extern DELPHI_PACKAGE System::ResourceString _SConnectionNotDefined;
#define Daconsts_SConnectionNotDefined System::LoadResourceString(&Daconsts::_SConnectionNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SConnectionNotConnected;
#define Daconsts_SConnectionNotConnected System::LoadResourceString(&Daconsts::_SConnectionNotConnected)
extern DELPHI_PACKAGE System::ResourceString _SConnectionOpen;
#define Daconsts_SConnectionOpen System::LoadResourceString(&Daconsts::_SConnectionOpen)
extern DELPHI_PACKAGE System::ResourceString _SConnectionClosed;
#define Daconsts_SConnectionClosed System::LoadResourceString(&Daconsts::_SConnectionClosed)
extern DELPHI_PACKAGE System::ResourceString _SCannotConnect;
#define Daconsts_SCannotConnect System::LoadResourceString(&Daconsts::_SCannotConnect)
extern DELPHI_PACKAGE System::ResourceString _SMacroNotFound;
#define Daconsts_SMacroNotFound System::LoadResourceString(&Daconsts::_SMacroNotFound)
extern DELPHI_PACKAGE System::ResourceString _SNotInTransaction;
#define Daconsts_SNotInTransaction System::LoadResourceString(&Daconsts::_SNotInTransaction)
extern DELPHI_PACKAGE System::ResourceString _SInTransaction;
#define Daconsts_SInTransaction System::LoadResourceString(&Daconsts::_SInTransaction)
extern DELPHI_PACKAGE System::ResourceString _STransactionNotAssigned;
#define Daconsts_STransactionNotAssigned System::LoadResourceString(&Daconsts::_STransactionNotAssigned)
extern DELPHI_PACKAGE System::ResourceString _SUpdateFailed;
#define Daconsts_SUpdateFailed System::LoadResourceString(&Daconsts::_SUpdateFailed)
extern DELPHI_PACKAGE System::ResourceString _SCustomUpdateFailed;
#define Daconsts_SCustomUpdateFailed System::LoadResourceString(&Daconsts::_SCustomUpdateFailed)
extern DELPHI_PACKAGE System::ResourceString _SRefreshFailed;
#define Daconsts_SRefreshFailed System::LoadResourceString(&Daconsts::_SRefreshFailed)
extern DELPHI_PACKAGE System::ResourceString _SInvalidFetchRows;
#define Daconsts_SInvalidFetchRows System::LoadResourceString(&Daconsts::_SInvalidFetchRows)
extern DELPHI_PACKAGE System::ResourceString _SNoCorrespondParam;
#define Daconsts_SNoCorrespondParam System::LoadResourceString(&Daconsts::_SNoCorrespondParam)
extern DELPHI_PACKAGE System::ResourceString _SUnknownParamDataType;
#define Daconsts_SUnknownParamDataType System::LoadResourceString(&Daconsts::_SUnknownParamDataType)
extern DELPHI_PACKAGE System::ResourceString _SRecordChanged;
#define Daconsts_SRecordChanged System::LoadResourceString(&Daconsts::_SRecordChanged)
extern DELPHI_PACKAGE System::ResourceString _SRecordDeleted;
#define Daconsts_SRecordDeleted System::LoadResourceString(&Daconsts::_SRecordDeleted)
extern DELPHI_PACKAGE System::ResourceString _SRecordNotFound;
#define Daconsts_SRecordNotFound System::LoadResourceString(&Daconsts::_SRecordNotFound)
extern DELPHI_PACKAGE System::ResourceString _STableNameNotDefined;
#define Daconsts_STableNameNotDefined System::LoadResourceString(&Daconsts::_STableNameNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SStoredProcNotDefined;
#define Daconsts_SStoredProcNotDefined System::LoadResourceString(&Daconsts::_SStoredProcNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SConnectionIsClosed;
#define Daconsts_SConnectionIsClosed System::LoadResourceString(&Daconsts::_SConnectionIsClosed)
extern DELPHI_PACKAGE System::ResourceString _SCannotPerformIfPooling;
#define Daconsts_SCannotPerformIfPooling System::LoadResourceString(&Daconsts::_SCannotPerformIfPooling)
extern DELPHI_PACKAGE System::ResourceString _SMaxConnectionsReached;
#define Daconsts_SMaxConnectionsReached System::LoadResourceString(&Daconsts::_SMaxConnectionsReached)
extern DELPHI_PACKAGE System::ResourceString _SDataSetNotDefined;
#define Daconsts_SDataSetNotDefined System::LoadResourceString(&Daconsts::_SDataSetNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SCannotChangeIsUnicode;
#define Daconsts_SCannotChangeIsUnicode System::LoadResourceString(&Daconsts::_SCannotChangeIsUnicode)
extern DELPHI_PACKAGE System::ResourceString _SColumnNotFound;
#define Daconsts_SColumnNotFound System::LoadResourceString(&Daconsts::_SColumnNotFound)
extern DELPHI_PACKAGE System::ResourceString _SNoTimers;
#define Daconsts_SNoTimers System::LoadResourceString(&Daconsts::_SNoTimers)
extern DELPHI_PACKAGE System::ResourceString _sBlobNotCompressed;
#define Daconsts_sBlobNotCompressed System::LoadResourceString(&Daconsts::_sBlobNotCompressed)
extern DELPHI_PACKAGE System::ResourceString _SCompressorNotLinked;
#define Daconsts_SCompressorNotLinked System::LoadResourceString(&Daconsts::_SCompressorNotLinked)
extern DELPHI_PACKAGE System::ResourceString _SUncompressorNotLinked;
#define Daconsts_SUncompressorNotLinked System::LoadResourceString(&Daconsts::_SUncompressorNotLinked)
extern DELPHI_PACKAGE System::ResourceString _SUpdateComponentCircularReferences;
#define Daconsts_SUpdateComponentCircularReferences System::LoadResourceString(&Daconsts::_SUpdateComponentCircularReferences)
extern DELPHI_PACKAGE System::ResourceString _SUpdateComponentInvalidType;
#define Daconsts_SUpdateComponentInvalidType System::LoadResourceString(&Daconsts::_SUpdateComponentInvalidType)
extern DELPHI_PACKAGE System::ResourceString _SUpdateObjectEmptySQL;
#define Daconsts_SUpdateObjectEmptySQL System::LoadResourceString(&Daconsts::_SUpdateObjectEmptySQL)
extern DELPHI_PACKAGE System::ResourceString _SDateEncodeError;
#define Daconsts_SDateEncodeError System::LoadResourceString(&Daconsts::_SDateEncodeError)
extern DELPHI_PACKAGE System::ResourceString _SInvalidXML;
#define Daconsts_SInvalidXML System::LoadResourceString(&Daconsts::_SInvalidXML)
extern DELPHI_PACKAGE System::ResourceString _SWrongTblCount;
#define Daconsts_SWrongTblCount System::LoadResourceString(&Daconsts::_SWrongTblCount)
extern DELPHI_PACKAGE System::ResourceString _SBackupQueryWrongTableCount;
#define Daconsts_SBackupQueryWrongTableCount System::LoadResourceString(&Daconsts::_SBackupQueryWrongTableCount)
extern DELPHI_PACKAGE System::ResourceString _SBHCaption;
#define Daconsts_SBHCaption System::LoadResourceString(&Daconsts::_SBHCaption)
extern DELPHI_PACKAGE System::ResourceString _SBHTableData;
#define Daconsts_SBHTableData System::LoadResourceString(&Daconsts::_SBHTableData)
extern DELPHI_PACKAGE System::ResourceString _SAreYouSureRestore;
#define Daconsts_SAreYouSureRestore System::LoadResourceString(&Daconsts::_SAreYouSureRestore)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBatchMove;
#define Daconsts_SInvalidBatchMove System::LoadResourceString(&Daconsts::_SInvalidBatchMove)
extern DELPHI_PACKAGE System::ResourceString _SInvalidLexem;
#define Daconsts_SInvalidLexem System::LoadResourceString(&Daconsts::_SInvalidLexem)
extern DELPHI_PACKAGE System::ResourceString _SEmptySQLStatement;
#define Daconsts_SEmptySQLStatement System::LoadResourceString(&Daconsts::_SEmptySQLStatement)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBlobPosition;
#define Daconsts_SInvalidBlobPosition System::LoadResourceString(&Daconsts::_SInvalidBlobPosition)
extern DELPHI_PACKAGE System::ResourceString _SNoConnectionsInTransaction;
#define Daconsts_SNoConnectionsInTransaction System::LoadResourceString(&Daconsts::_SNoConnectionsInTransaction)
extern DELPHI_PACKAGE System::ResourceString _SMultiConnectionsInTransaction;
#define Daconsts_SMultiConnectionsInTransaction System::LoadResourceString(&Daconsts::_SMultiConnectionsInTransaction)
extern DELPHI_PACKAGE System::ResourceString _SConnectionInTransactionNotActive;
#define Daconsts_SConnectionInTransactionNotActive System::LoadResourceString(&Daconsts::_SConnectionInTransactionNotActive)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedIsolationLevel;
#define Daconsts_SUnsupportedIsolationLevel System::LoadResourceString(&Daconsts::_SUnsupportedIsolationLevel)
extern DELPHI_PACKAGE System::ResourceString _SIsolationLevelNotSupportedWithMTS;
#define Daconsts_SIsolationLevelNotSupportedWithMTS System::LoadResourceString(&Daconsts::_SIsolationLevelNotSupportedWithMTS)
extern DELPHI_PACKAGE System::ResourceString _SReadOnlyTransactionNotSupported;
#define Daconsts_SReadOnlyTransactionNotSupported System::LoadResourceString(&Daconsts::_SReadOnlyTransactionNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SOperationNotSupported;
#define Daconsts_SOperationNotSupported System::LoadResourceString(&Daconsts::_SOperationNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SMultipleTransactionsNotSupported;
#define Daconsts_SMultipleTransactionsNotSupported System::LoadResourceString(&Daconsts::_SMultipleTransactionsNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SMTSNotSupported;
#define Daconsts_SMTSNotSupported System::LoadResourceString(&Daconsts::_SMTSNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SVarSubtypeNotSupported;
#define Daconsts_SVarSubtypeNotSupported System::LoadResourceString(&Daconsts::_SVarSubtypeNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SInvalidParamsArray;
#define Daconsts_SInvalidParamsArray System::LoadResourceString(&Daconsts::_SInvalidParamsArray)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedMetaDataKind;
#define Daconsts_SUnsupportedMetaDataKind System::LoadResourceString(&Daconsts::_SUnsupportedMetaDataKind)
extern DELPHI_PACKAGE System::ResourceString _SRestrictionMustBeSet;
#define Daconsts_SRestrictionMustBeSet System::LoadResourceString(&Daconsts::_SRestrictionMustBeSet)
extern DELPHI_PACKAGE System::ResourceString _SEventsNotDefined;
#define Daconsts_SEventsNotDefined System::LoadResourceString(&Daconsts::_SEventsNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SMDIsNotMemDataSet;
#define Daconsts_SMDIsNotMemDataSet System::LoadResourceString(&Daconsts::_SMDIsNotMemDataSet)
extern DELPHI_PACKAGE System::ResourceString _SInternalError;
#define Daconsts_SInternalError System::LoadResourceString(&Daconsts::_SInternalError)
extern DELPHI_PACKAGE System::ResourceString _SNumericOverflow;
#define Daconsts_SNumericOverflow System::LoadResourceString(&Daconsts::_SNumericOverflow)
extern DELPHI_PACKAGE System::ResourceString _SEInvalidInputArgs;
#define Daconsts_SEInvalidInputArgs System::LoadResourceString(&Daconsts::_SEInvalidInputArgs)
extern DELPHI_PACKAGE System::ResourceString _SEPositiveOverflow;
#define Daconsts_SEPositiveOverflow System::LoadResourceString(&Daconsts::_SEPositiveOverflow)
extern DELPHI_PACKAGE System::ResourceString _SENegativeUnderflow;
#define Daconsts_SENegativeUnderflow System::LoadResourceString(&Daconsts::_SENegativeUnderflow)
extern DELPHI_PACKAGE System::ResourceString _SEValueHasNoInverse;
#define Daconsts_SEValueHasNoInverse System::LoadResourceString(&Daconsts::_SEValueHasNoInverse)
extern DELPHI_PACKAGE System::ResourceString _SEInvalidBigIntegerRadix;
#define Daconsts_SEInvalidBigIntegerRadix System::LoadResourceString(&Daconsts::_SEInvalidBigIntegerRadix)
extern DELPHI_PACKAGE System::ResourceString _SENotPositiveExponents;
#define Daconsts_SENotPositiveExponents System::LoadResourceString(&Daconsts::_SENotPositiveExponents)
extern DELPHI_PACKAGE System::ResourceString _SProtectionCircular;
#define Daconsts_SProtectionCircular System::LoadResourceString(&Daconsts::_SProtectionCircular)
extern DELPHI_PACKAGE System::ResourceString _SNotInitialized;
#define Daconsts_SNotInitialized System::LoadResourceString(&Daconsts::_SNotInitialized)
extern DELPHI_PACKAGE System::ResourceString _SInvalidInputArgs;
#define Daconsts_SInvalidInputArgs System::LoadResourceString(&Daconsts::_SInvalidInputArgs)
extern DELPHI_PACKAGE System::ResourceString _SInvalidIV;
#define Daconsts_SInvalidIV System::LoadResourceString(&Daconsts::_SInvalidIV)
extern DELPHI_PACKAGE System::ResourceString _SInvalidKey;
#define Daconsts_SInvalidKey System::LoadResourceString(&Daconsts::_SInvalidKey)
extern DELPHI_PACKAGE System::ResourceString _SInvalidKeySize;
#define Daconsts_SInvalidKeySize System::LoadResourceString(&Daconsts::_SInvalidKeySize)
extern DELPHI_PACKAGE System::ResourceString _SHashChangedAfterWrite;
#define Daconsts_SHashChangedAfterWrite System::LoadResourceString(&Daconsts::_SHashChangedAfterWrite)
extern DELPHI_PACKAGE System::ResourceString _SGCMModeMustBeUsed;
#define Daconsts_SGCMModeMustBeUsed System::LoadResourceString(&Daconsts::_SGCMModeMustBeUsed)
extern DELPHI_PACKAGE System::ResourceString _SGCMCannotBeUsed;
#define Daconsts_SGCMCannotBeUsed System::LoadResourceString(&Daconsts::_SGCMCannotBeUsed)
extern DELPHI_PACKAGE System::ResourceString _SEncryptionNotSupported;
#define Daconsts_SEncryptionNotSupported System::LoadResourceString(&Daconsts::_SEncryptionNotSupported)
extern DELPHI_PACKAGE System::ResourceString _SInvalidEncDataSize;
#define Daconsts_SInvalidEncDataSize System::LoadResourceString(&Daconsts::_SInvalidEncDataSize)
extern DELPHI_PACKAGE System::ResourceString _SInvalidEncData;
#define Daconsts_SInvalidEncData System::LoadResourceString(&Daconsts::_SInvalidEncData)
extern DELPHI_PACKAGE System::ResourceString _SInvalidHash;
#define Daconsts_SInvalidHash System::LoadResourceString(&Daconsts::_SInvalidHash)
extern DELPHI_PACKAGE System::ResourceString _SEncryptorNotDefined;
#define Daconsts_SEncryptorNotDefined System::LoadResourceString(&Daconsts::_SEncryptorNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SEncrFieldsNotDefined;
#define Daconsts_SEncrFieldsNotDefined System::LoadResourceString(&Daconsts::_SEncrFieldsNotDefined)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamNameUnknown;
#define Daconsts_SConnectParamNameUnknown System::LoadResourceString(&Daconsts::_SConnectParamNameUnknown)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamNameMissing;
#define Daconsts_SConnectParamNameMissing System::LoadResourceString(&Daconsts::_SConnectParamNameMissing)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamValueMissing;
#define Daconsts_SConnectParamValueMissing System::LoadResourceString(&Daconsts::_SConnectParamValueMissing)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamInvalidChar;
#define Daconsts_SConnectParamInvalidChar System::LoadResourceString(&Daconsts::_SConnectParamInvalidChar)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamAddedTwice;
#define Daconsts_SConnectParamAddedTwice System::LoadResourceString(&Daconsts::_SConnectParamAddedTwice)
extern DELPHI_PACKAGE System::ResourceString _SInvalidConnectParamValue;
#define Daconsts_SInvalidConnectParamValue System::LoadResourceString(&Daconsts::_SInvalidConnectParamValue)
extern DELPHI_PACKAGE System::ResourceString _SConnectParamInternalError;
#define Daconsts_SConnectParamInternalError System::LoadResourceString(&Daconsts::_SConnectParamInternalError)
extern DELPHI_PACKAGE System::ResourceString _SLoginParameterServer;
#define Daconsts_SLoginParameterServer System::LoadResourceString(&Daconsts::_SLoginParameterServer)
extern DELPHI_PACKAGE System::ResourceString _SLoginParameterUsername;
#define Daconsts_SLoginParameterUsername System::LoadResourceString(&Daconsts::_SLoginParameterUsername)
extern DELPHI_PACKAGE System::ResourceString _SLoginParameterPassword;
#define Daconsts_SLoginParameterPassword System::LoadResourceString(&Daconsts::_SLoginParameterPassword)
extern DELPHI_PACKAGE System::ResourceString _SUnexpectedMapping;
#define Daconsts_SUnexpectedMapping System::LoadResourceString(&Daconsts::_SUnexpectedMapping)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedMapping;
#define Daconsts_SUnsupportedMapping System::LoadResourceString(&Daconsts::_SUnsupportedMapping)
extern DELPHI_PACKAGE System::ResourceString _SInvalidMapRuleExpr;
#define Daconsts_SInvalidMapRuleExpr System::LoadResourceString(&Daconsts::_SInvalidMapRuleExpr)
extern DELPHI_PACKAGE System::ResourceString _SUnexpectedEndOfMapRuleExpr;
#define Daconsts_SUnexpectedEndOfMapRuleExpr System::LoadResourceString(&Daconsts::_SUnexpectedEndOfMapRuleExpr)
extern DELPHI_PACKAGE System::ResourceString _SUnexpectedSymbolInMapRuleExpr;
#define Daconsts_SUnexpectedSymbolInMapRuleExpr System::LoadResourceString(&Daconsts::_SUnexpectedSymbolInMapRuleExpr)
extern DELPHI_PACKAGE System::ResourceString _SNotDefinedDBType;
#define Daconsts_SNotDefinedDBType System::LoadResourceString(&Daconsts::_SNotDefinedDBType)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedDBType;
#define Daconsts_SUnsupportedDBType System::LoadResourceString(&Daconsts::_SUnsupportedDBType)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDBLength;
#define Daconsts_SInvalidDBLength System::LoadResourceString(&Daconsts::_SInvalidDBLength)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDBScale;
#define Daconsts_SInvalidDBScale System::LoadResourceString(&Daconsts::_SInvalidDBScale)
extern DELPHI_PACKAGE System::ResourceString _SNotDefinedFieldType;
#define Daconsts_SNotDefinedFieldType System::LoadResourceString(&Daconsts::_SNotDefinedFieldType)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedFieldType;
#define Daconsts_SUnsupportedFieldType System::LoadResourceString(&Daconsts::_SUnsupportedFieldType)
extern DELPHI_PACKAGE System::ResourceString _SInvalidFieldLength;
#define Daconsts_SInvalidFieldLength System::LoadResourceString(&Daconsts::_SInvalidFieldLength)
extern DELPHI_PACKAGE System::ResourceString _SInvalidFieldScale;
#define Daconsts_SInvalidFieldScale System::LoadResourceString(&Daconsts::_SInvalidFieldScale)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDataMapping;
#define Daconsts_SInvalidDataMapping System::LoadResourceString(&Daconsts::_SInvalidDataMapping)
extern DELPHI_PACKAGE System::ResourceString _SInvalidIntegerValue;
#define Daconsts_SInvalidIntegerValue System::LoadResourceString(&Daconsts::_SInvalidIntegerValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidNumericValue;
#define Daconsts_SInvalidNumericValue System::LoadResourceString(&Daconsts::_SInvalidNumericValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBooleanValue;
#define Daconsts_SInvalidBooleanValue System::LoadResourceString(&Daconsts::_SInvalidBooleanValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDateTimeValue;
#define Daconsts_SInvalidDateTimeValue System::LoadResourceString(&Daconsts::_SInvalidDateTimeValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidSQLTimeStampValue;
#define Daconsts_SInvalidSQLTimeStampValue System::LoadResourceString(&Daconsts::_SInvalidSQLTimeStampValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidGUIDValue;
#define Daconsts_SInvalidGUIDValue System::LoadResourceString(&Daconsts::_SInvalidGUIDValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidIntervalValue;
#define Daconsts_SInvalidIntervalValue System::LoadResourceString(&Daconsts::_SInvalidIntervalValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBinaryValue;
#define Daconsts_SInvalidBinaryValue System::LoadResourceString(&Daconsts::_SInvalidBinaryValue)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBlobValue;
#define Daconsts_SInvalidBlobValue System::LoadResourceString(&Daconsts::_SInvalidBlobValue)
extern DELPHI_PACKAGE System::ResourceString _SValueOutOfRange;
#define Daconsts_SValueOutOfRange System::LoadResourceString(&Daconsts::_SValueOutOfRange)
extern DELPHI_PACKAGE System::ResourceString _SInvalidValueScale;
#define Daconsts_SInvalidValueScale System::LoadResourceString(&Daconsts::_SInvalidValueScale)
extern DELPHI_PACKAGE System::ResourceString _SFractionTruncated;
#define Daconsts_SFractionTruncated System::LoadResourceString(&Daconsts::_SFractionTruncated)
extern DELPHI_PACKAGE System::ResourceString _SDataTruncated;
#define Daconsts_SDataTruncated System::LoadResourceString(&Daconsts::_SDataTruncated)
extern DELPHI_PACKAGE System::ResourceString _SValueTooLong;
#define Daconsts_SValueTooLong System::LoadResourceString(&Daconsts::_SValueTooLong)
extern DELPHI_PACKAGE System::ResourceString _SStringTooLong;
#define Daconsts_SStringTooLong System::LoadResourceString(&Daconsts::_SStringTooLong)
extern DELPHI_PACKAGE System::ResourceString _SBinaryTooLong;
#define Daconsts_SBinaryTooLong System::LoadResourceString(&Daconsts::_SBinaryTooLong)
extern DELPHI_PACKAGE System::ResourceString _SParameterIsDmlArray;
#define Daconsts_SParameterIsDmlArray System::LoadResourceString(&Daconsts::_SParameterIsDmlArray)
extern DELPHI_PACKAGE System::ResourceString _SInvalidIndex;
#define Daconsts_SInvalidIndex System::LoadResourceString(&Daconsts::_SInvalidIndex)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDmlArrayType;
#define Daconsts_SInvalidDmlArrayType System::LoadResourceString(&Daconsts::_SInvalidDmlArrayType)
extern DELPHI_PACKAGE System::ResourceString _SCannotSetSingleValue;
#define Daconsts_SCannotSetSingleValue System::LoadResourceString(&Daconsts::_SCannotSetSingleValue)
extern DELPHI_PACKAGE System::ResourceString _SCannotGetSingleValue;
#define Daconsts_SCannotGetSingleValue System::LoadResourceString(&Daconsts::_SCannotGetSingleValue)
extern DELPHI_PACKAGE System::ResourceString _SParameterTypeDiffers;
#define Daconsts_SParameterTypeDiffers System::LoadResourceString(&Daconsts::_SParameterTypeDiffers)
extern DELPHI_PACKAGE System::ResourceString _SUnknownParameterDataType;
#define Daconsts_SUnknownParameterDataType System::LoadResourceString(&Daconsts::_SUnknownParameterDataType)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBatchOperation;
#define Daconsts_SInvalidBatchOperation System::LoadResourceString(&Daconsts::_SInvalidBatchOperation)
extern DELPHI_PACKAGE System::ResourceString _SInvalidBatchParameters;
#define Daconsts_SInvalidBatchParameters System::LoadResourceString(&Daconsts::_SInvalidBatchParameters)
extern DELPHI_PACKAGE System::ResourceString _SConditionNotFound;
#define Daconsts_SConditionNotFound System::LoadResourceString(&Daconsts::_SConditionNotFound)
extern DELPHI_PACKAGE System::ResourceString _SNoDataToFetch;
#define Daconsts_SNoDataToFetch System::LoadResourceString(&Daconsts::_SNoDataToFetch)
extern DELPHI_PACKAGE System::ResourceString _SUnknownSmartFetchMode;
#define Daconsts_SUnknownSmartFetchMode System::LoadResourceString(&Daconsts::_SUnknownSmartFetchMode)
extern DELPHI_PACKAGE System::ResourceString _SOnlySelectAllowed;
#define Daconsts_SOnlySelectAllowed System::LoadResourceString(&Daconsts::_SOnlySelectAllowed)
extern DELPHI_PACKAGE System::ResourceString _SSmartFetchIsUnallowed;
#define Daconsts_SSmartFetchIsUnallowed System::LoadResourceString(&Daconsts::_SSmartFetchIsUnallowed)
extern DELPHI_PACKAGE System::ResourceString _SFieldsNotCorresponding;
#define Daconsts_SFieldsNotCorresponding System::LoadResourceString(&Daconsts::_SFieldsNotCorresponding)
extern DELPHI_PACKAGE System::ResourceString _SRecordsetIsClosed;
#define Daconsts_SRecordsetIsClosed System::LoadResourceString(&Daconsts::_SRecordsetIsClosed)
extern DELPHI_PACKAGE System::ResourceString _SIndexFieldNamesWithLiveBlock;
#define Daconsts_SIndexFieldNamesWithLiveBlock System::LoadResourceString(&Daconsts::_SIndexFieldNamesWithLiveBlock)
extern DELPHI_PACKAGE System::ResourceString _SInvalidRecordNumber;
#define Daconsts_SInvalidRecordNumber System::LoadResourceString(&Daconsts::_SInvalidRecordNumber)
extern DELPHI_PACKAGE System::ResourceString _SOpeningWasCanceled;
#define Daconsts_SOpeningWasCanceled System::LoadResourceString(&Daconsts::_SOpeningWasCanceled)
extern DELPHI_PACKAGE System::ResourceString _SLocalSortingServerCursor;
#define Daconsts_SLocalSortingServerCursor System::LoadResourceString(&Daconsts::_SLocalSortingServerCursor)
extern DELPHI_PACKAGE System::ResourceString _SIncorrectConnectionType;
#define Daconsts_SIncorrectConnectionType System::LoadResourceString(&Daconsts::_SIncorrectConnectionType)
}	/* namespace Daconsts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DACONSTS)
using namespace Daconsts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaconstsHPP
