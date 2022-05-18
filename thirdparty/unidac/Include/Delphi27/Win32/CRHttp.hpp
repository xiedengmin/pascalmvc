// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRHttp.pas' rev: 34.00 (Windows)

#ifndef CrhttpHPP
#define CrhttpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Types.hpp>
#include <System.Math.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRBase64.hpp>
#include <CRVio.hpp>
#include <CRSecureConnection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crhttp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TScRequestCachePolicy;
class DELPHICLASS TScWebHeaderCollection;
class DELPHICLASS TScWebRequestHeaderCollection;
class DELPHICLASS TScWebResponseHeaderCollection;
class DELPHICLASS TCRHttpWebRequest;
class DELPHICLASS TCRHttpWebResponse;
class DELPHICLASS HttpException;
class DELPHICLASS TScHttpWebResponseHelper;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TScRequestMethod : unsigned char { rmGET, rmHEAD, rmOPTIONS, rmPOST, rmPUT, rmDELETE, rmTRACE, rmCONNECT, rmPATCH };

enum DECLSPEC_DENUM TScHttpStatusCode : unsigned char { scAccepted, scAmbiguous, scBadGateway, scBadRequest, scConflict, scContinue, scCreated, scExpectationFailed, scForbidden, scFound, scGatewayTimeout, scGone, scHttpVersionNotSupported, scInternalServerError, scLengthRequired, scMethodNotAllowed, scMoved, scMovedPermanently, scMultipleChoices, scNoContent, scNonAuthoritativeInformation, scNotAcceptable, scNotFound, scNotImplemented, scNotModified, scOK, scPartialContent, scPaymentRequired, scPreconditionFailed, scProxyAuthenticationRequired, scRedirect, scRedirectKeepVerb, scRedirectMethod, scRequestedRangeNotSatisfiable, scRequestEntityTooLarge, scRequestTimeout, scRequestUriTooLong, scResetContent, scSeeOther, scServiceUnavailable, scSwitchingProtocols, 
	scTemporaryRedirect, scUnauthorized, scUnsupportedMediaType, scUnused, scUpgradeRequired, scUseProxy, scUnknown };

typedef System::Set<TScHttpStatusCode, TScHttpStatusCode::scAccepted, TScHttpStatusCode::scUnknown> TScHttpStatusCodes;

typedef void __fastcall (__closure *TScBeforeSendDataEvent)(System::TObject* Sender, __int64 Offset, __int64 Count, bool &Cancel);

typedef void __fastcall (__closure *TScOnGetNextChunkDataEvent)(System::TObject* Sender, /* out */ char * &Buffer, /* out */ int &Count);

enum DECLSPEC_DENUM TScRequestCacheLevel : unsigned char { clDefault, clNoCacheNoStore, clReload };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScRequestCachePolicy : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TScRequestCacheLevel FLevel;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TScRequestCachePolicy(TScRequestCacheLevel RequestCacheLevel);
	
__published:
	__property TScRequestCacheLevel Level = {read=FLevel, write=FLevel, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TScRequestCachePolicy() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TScOnCheckNewStrValueEvent)(const System::UnicodeString Key, const System::UnicodeString Value);

class PASCALIMPLEMENTATION TScWebHeaderCollection : public Crtypes::TStrValueStringList
{
	typedef Crtypes::TStrValueStringList inherited;
	
private:
	Clrclasses::WideStringBuilder* SB;
	TScOnCheckNewStrValueEvent FOnCheckNewHeader;
	System::UnicodeString __fastcall SaveToString(const System::UnicodeString Separator);
	System::UnicodeString __fastcall GetText();
	void __fastcall SetText(const System::UnicodeString Value);
	
public:
	__fastcall TScWebHeaderCollection();
	__fastcall virtual ~TScWebHeaderCollection();
	virtual System::UnicodeString __fastcall ToString();
	__property System::UnicodeString Text = {read=GetText, write=SetText};
	__property TScOnCheckNewStrValueEvent OnCheckNewHeader = {read=FOnCheckNewHeader, write=FOnCheckNewHeader};
};


class PASCALIMPLEMENTATION TScWebRequestHeaderCollection : public TScWebHeaderCollection
{
	typedef TScWebHeaderCollection inherited;
	
private:
	TCRHttpWebRequest* FOwner;
	
protected:
	virtual void __fastcall CheckNewValue(const System::UnicodeString Key, const System::UnicodeString Value);
	
public:
	__fastcall TScWebRequestHeaderCollection(TCRHttpWebRequest* Owner);
	virtual System::UnicodeString __fastcall ToString();
public:
	/* TScWebHeaderCollection.Destroy */ inline __fastcall virtual ~TScWebRequestHeaderCollection() { }
	
};


class PASCALIMPLEMENTATION TScWebResponseHeaderCollection : public TScWebHeaderCollection
{
	typedef TScWebHeaderCollection inherited;
	
private:
	TCRHttpWebResponse* FOwner;
	
public:
	__fastcall TScWebResponseHeaderCollection(TCRHttpWebResponse* Owner);
	virtual System::UnicodeString __fastcall ToString();
public:
	/* TScWebHeaderCollection.Destroy */ inline __fastcall virtual ~TScWebResponseHeaderCollection() { }
	
};


class PASCALIMPLEMENTATION TCRHttpWebRequest : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
private:
	System::UnicodeString FAccept;
	TScRequestCachePolicy* FCachePolicy;
	System::UnicodeString FConnection;
	System::UnicodeString FConnectionGroupName;
	__int64 FContentLength;
	System::UnicodeString FContentType;
	System::Classes::TStringList* FCookies;
	Crsecureconnection::TScNetworkCredential* FCredentials;
	System::TDateTime FDate;
	System::UnicodeString FExpect;
	System::UnicodeString FFrom;
	TScWebHeaderCollection* FHeaders;
	System::UnicodeString FHost;
	System::TDateTime FIfModifiedSince;
	bool FKeepAlive;
	TScRequestMethod FMethod;
	TScRequestMethod FOldMethod;
	Crsecureconnection::TScVersion* FProtocolVersion;
	Crsecureconnection::TScWebProxy* FProxy;
	System::UnicodeString FRange;
	int FReadWriteTimeout;
	System::UnicodeString FReferer;
	System::UnicodeString FRequestUri;
	System::UnicodeString FTransferEncoding;
	System::UnicodeString FUpgrade;
	System::UnicodeString FUserAgent;
	Crvio::TIPVersion FIPVersion;
	int FMaximumAutomaticRedirections;
	int FMaximumAutomaticReconnections;
	__int64 FContentWrote;
	bool FSendChunked;
	int FSendBlockSize;
	int FStatusCode;
	System::UnicodeString FStatusDescription;
	TScHttpStatusCodes FAllowedStatuses;
	System::Classes::TStream* FRequestStream;
	System::DynamicArray<System::Byte> FSendBuffer;
	TScOnGetNextChunkDataEvent FOnGetNextChunkData;
	System::Classes::TNotifyEvent FOnConnected;
	System::Classes::TNotifyEvent FOnAuthenticationNeeded;
	System::Classes::TNotifyEvent FBeforeSendRequest;
	System::Classes::TNotifyEvent FAfterSendRequest;
	System::Classes::TNotifyEvent FAfterRetrieveHeaders;
	TScBeforeSendDataEvent FBeforeSendData;
	Crvio::TSSLOptions* FSSLOptions;
	Crvio::TCRIOHandler* FIOHandler;
	void __fastcall ReadHeadersText(System::Classes::TReader* Reader);
	void __fastcall WriteHeadersText(System::Classes::TWriter* Writer);
	bool __fastcall GetIsSecure();
	TScHttpStatusCode __fastcall GetStatusCode();
	void __fastcall SetAccept(const System::UnicodeString Value);
	void __fastcall SetCachePolicy(TScRequestCachePolicy* Value);
	void __fastcall SetConnection(const System::UnicodeString Value);
	void __fastcall SetConnectionGroupName(const System::UnicodeString Value);
	void __fastcall SetContentLength(const __int64 Value);
	void __fastcall SetContentType(const System::UnicodeString Value);
	void __fastcall SetCookies(System::Classes::TStringList* Value);
	void __fastcall SetCredentials(Crsecureconnection::TScNetworkCredential* Value);
	void __fastcall SetDate(const System::TDateTime Value);
	void __fastcall SetExpect(const System::UnicodeString Value);
	void __fastcall SetFrom(const System::UnicodeString Value);
	void __fastcall SetHeaders(TScWebHeaderCollection* Value);
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall SetIfModifiedSince(const System::TDateTime Value);
	void __fastcall CheckKeepAliveHeader();
	void __fastcall SetKeepAlive(const bool Value);
	void __fastcall SetMethod(const TScRequestMethod Value);
	void __fastcall SetProtocolVersion(Crsecureconnection::TScVersion* Value);
	void __fastcall SetProxy(Crsecureconnection::TScWebProxy* Value);
	void __fastcall SetRange(const System::UnicodeString Value);
	void __fastcall SetReadWriteTimeout(const int Value);
	void __fastcall SetReferer(const System::UnicodeString Value);
	void __fastcall SetRequestUri(const System::UnicodeString Value);
	void __fastcall SetTransferEncoding(const System::UnicodeString Value);
	void __fastcall SetUpgrade(const System::UnicodeString Value);
	void __fastcall SetUserAgent(const System::UnicodeString Value);
	void __fastcall SetIPVersion(const Crvio::TIPVersion Value);
	void __fastcall SetMaximumAutomaticRedirections(const int Value);
	void __fastcall SetMaximumAutomaticReconnections(const int Value);
	void __fastcall SetSendChunked(const bool Value);
	void __fastcall SetSendBlockSize(const int Value);
	void __fastcall SetSSLOptions(Crvio::TSSLOptions* Value);
	void __fastcall SetRequestStream(System::Classes::TStream* Value);
	void __fastcall DoBeforeSendRequest();
	void __fastcall DoAfterSendRequest();
	void __fastcall DoAfterRetrieveHeaders();
	
protected:
	Crsecureconnection::TScSecureConnection* FSecureConnection;
	System::UnicodeString FAddress;
	System::UnicodeString FScheme;
	System::UnicodeString FPort;
	System::UnicodeString FQuery;
	System::UnicodeString FResource;
	System::UnicodeString FParameters;
	System::UnicodeString FPath;
	System::UnicodeString FFragment;
	System::UnicodeString FNetworkLocation;
	int FPortNo;
	void __fastcall Init();
	void __fastcall GetNextChunkData(/* out */ char * &Buffer, /* out */ int &Count);
	void __fastcall WriteChunk(char * Buffer, int Count, Clrclasses::TScCancellationToken* CancellationToken);
	void __fastcall WriteChunkSize(int Size);
	void __fastcall InternalWriteData(Clrclasses::TScCancellationToken* CancellationToken);
	void __fastcall CheckRequest();
	void __fastcall SendRequest(Clrclasses::TScCancellationToken* CancellationToken = (Clrclasses::TScCancellationToken*)(0x0));
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	__classmethod System::UnicodeString __fastcall DefaultPort(const System::UnicodeString Scheme);
	void __fastcall CheckInactive();
	
public:
	HRESULT __stdcall Read(void * pv, unsigned cb, System::PFixedUInt pcbRead);
	HRESULT __stdcall Write(void * pv, unsigned cb, System::PFixedUInt pcbWritten)/* overload */;
	void __fastcall WriteBuffer(const char * Buffer, int Offset, int Count)/* overload */;
	void __fastcall WriteBuffer(const System::DynamicArray<System::Byte> Data)/* overload */;
	void __fastcall WriteData(System::Classes::TStream* Stream);
	System::DynamicArray<System::Byte> __fastcall ReadBuffer();
	__fastcall TCRHttpWebRequest(const System::UnicodeString URL);
	__fastcall virtual ~TCRHttpWebRequest();
	void __fastcall Abort();
	void __fastcall Disconnect();
	TCRHttpWebResponse* __fastcall GetResponse(Clrclasses::TScCancellationToken* CancellationToken = (Clrclasses::TScCancellationToken*)(0x0));
	void __fastcall SetAllowedStatuses(const TScHttpStatusCodes &Value);
	__property System::UnicodeString Address = {read=FAddress};
	__property bool IsSecure = {read=GetIsSecure, nodefault};
	__property TScHttpStatusCode StatusCode = {read=GetStatusCode, nodefault};
	__property System::UnicodeString StatusDescription = {read=FStatusDescription};
	__property System::Classes::TStream* RequestStream = {read=FRequestStream, write=SetRequestStream};
	__property TScOnGetNextChunkDataEvent OnGetNextChunkData = {read=FOnGetNextChunkData, write=FOnGetNextChunkData};
	__property Crvio::TCRIOHandler* IOHandler = {read=FIOHandler, write=FIOHandler};
	
__published:
	__property System::UnicodeString Accept = {read=FAccept, write=SetAccept};
	__property TScRequestCachePolicy* CachePolicy = {read=FCachePolicy, write=SetCachePolicy};
	__property System::UnicodeString Connection = {read=FConnection, write=SetConnection};
	__property System::UnicodeString ConnectionGroupName = {read=FConnectionGroupName, write=SetConnectionGroupName};
	__property __int64 ContentLength = {read=FContentLength, write=SetContentLength, default=-1};
	__property System::UnicodeString ContentType = {read=FContentType, write=SetContentType};
	__property System::Classes::TStringList* Cookies = {read=FCookies, write=SetCookies};
	__property Crsecureconnection::TScNetworkCredential* Credentials = {read=FCredentials, write=SetCredentials};
	__property System::TDateTime Date = {read=FDate, write=SetDate};
	__property System::UnicodeString Expect = {read=FExpect, write=SetExpect};
	__property System::UnicodeString From = {read=FFrom, write=SetFrom};
	__property TScWebHeaderCollection* Headers = {read=FHeaders, write=SetHeaders};
	__property System::UnicodeString Host = {read=FHost, write=SetHost};
	__property System::TDateTime IfModifiedSince = {read=FIfModifiedSince, write=SetIfModifiedSince};
	__property bool KeepAlive = {read=FKeepAlive, write=SetKeepAlive, default=1};
	__property TScRequestMethod Method = {read=FMethod, write=SetMethod, default=0};
	__property Crsecureconnection::TScVersion* ProtocolVersion = {read=FProtocolVersion, write=SetProtocolVersion};
	__property Crsecureconnection::TScWebProxy* Proxy = {read=FProxy, write=SetProxy};
	__property System::UnicodeString Range = {read=FRange, write=SetRange};
	__property int ReadWriteTimeout = {read=FReadWriteTimeout, write=SetReadWriteTimeout, default=15};
	__property System::UnicodeString Referer = {read=FReferer, write=SetReferer};
	__property System::UnicodeString RequestUri = {read=FRequestUri, write=SetRequestUri};
	__property System::UnicodeString TransferEncoding = {read=FTransferEncoding, write=SetTransferEncoding};
	__property System::UnicodeString Upgrade = {read=FUpgrade, write=SetUpgrade};
	__property System::UnicodeString UserAgent = {read=FUserAgent, write=SetUserAgent};
	__property Crvio::TIPVersion IPVersion = {read=FIPVersion, write=SetIPVersion, default=0};
	__property int MaximumAutomaticRedirections = {read=FMaximumAutomaticRedirections, write=SetMaximumAutomaticRedirections, default=50};
	__property int MaximumAutomaticReconnections = {read=FMaximumAutomaticReconnections, write=SetMaximumAutomaticReconnections, default=10};
	__property bool SendChunked = {read=FSendChunked, write=SetSendChunked, default=0};
	__property int SendBlockSize = {read=FSendBlockSize, write=SetSendBlockSize, default=32768};
	__property System::Classes::TNotifyEvent OnConnected = {read=FOnConnected, write=FOnConnected};
	__property System::Classes::TNotifyEvent OnAuthenticationNeeded = {read=FOnAuthenticationNeeded, write=FOnAuthenticationNeeded};
	__property System::Classes::TNotifyEvent BeforeSendRequest = {read=FBeforeSendRequest, write=FBeforeSendRequest};
	__property System::Classes::TNotifyEvent AfterSendRequest = {read=FAfterSendRequest, write=FAfterSendRequest};
	__property System::Classes::TNotifyEvent AfterRetrieveHeaders = {read=FAfterRetrieveHeaders, write=FAfterRetrieveHeaders};
	__property TScBeforeSendDataEvent BeforeSendData = {read=FBeforeSendData, write=FBeforeSendData};
	__property Crvio::TSSLOptions* SSLOptions = {read=FSSLOptions, write=SetSSLOptions};
private:
	void *__ISequentialStream;	// ISequentialStream 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
	operator _di_ISequentialStream()
	{
		_di_ISequentialStream intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ISequentialStream*(void) { return (ISequentialStream*)&__ISequentialStream; }
	#endif
	
};


class PASCALIMPLEMENTATION TCRHttpWebResponse : public System::Classes::TInterfacedPersistent
{
	typedef System::Classes::TInterfacedPersistent inherited;
	
private:
	System::Classes::TStringList* FCookies;
	TScWebHeaderCollection* FHeaders;
	TScRequestMethod FMethod;
	Crsecureconnection::TScVersion* FProtocolVersion;
	System::UnicodeString FLocation;
	int FStatusCode;
	System::UnicodeString FStatusDescription;
	System::UnicodeString FResponseUri;
	int FRetryAfter;
	__int64 FContentLength;
	__int64 FContentRead;
	bool FChunked;
	System::DynamicArray<System::Byte> FTmpBuf;
	Crtypes::TScOnProgressEvent FOnProgress;
	System::UnicodeString __fastcall GetContentEncoding();
	System::UnicodeString __fastcall GetContentType();
	System::TDateTime __fastcall GetLastModified();
	System::UnicodeString __fastcall GetServer();
	TScHttpStatusCode __fastcall GetStatusCode();
	bool __fastcall GetIsSecure();
	
protected:
	Crsecureconnection::TScSecureConnection* FSecureConnection;
	void __fastcall DoProgress(const __int64 Total, const __int64 Current);
	int __fastcall ReadChunk(const char * Buffer, int Offset, int Count);
	int __fastcall InternalRead(const char * Buffer, int Offset, int Count);
	void __fastcall SkipAll();
	void __fastcall RetrieveHeaders(Clrclasses::TScCancellationToken* CancellationToken = (Clrclasses::TScCancellationToken*)(0x0));
	
public:
	HRESULT __stdcall Read(void * pv, unsigned cb, System::PFixedUInt pcbRead)/* overload */;
	HRESULT __stdcall Write(void * pv, unsigned cb, System::PFixedUInt pcbWritten);
	int __fastcall ReadBuffer(const char * Buffer, int Offset, int Count);
	System::UnicodeString __fastcall ReadAsString();
	System::DynamicArray<System::Byte> __fastcall ReadAsBytes();
	int __fastcall ReadToStream(System::Classes::TStream* Stream);
	__fastcall TCRHttpWebResponse();
	__fastcall virtual ~TCRHttpWebResponse();
	void __fastcall Abort();
	System::UnicodeString __fastcall GetResponseHeader(const System::UnicodeString HeaderName);
	bool __fastcall WaitForData(int MillisecondsTimeout);
	__property bool IsSecure = {read=GetIsSecure, nodefault};
	__property System::UnicodeString ContentEncoding = {read=GetContentEncoding};
	__property __int64 ContentLength = {read=FContentLength};
	__property System::UnicodeString ContentType = {read=GetContentType};
	__property System::Classes::TStringList* Cookies = {read=FCookies};
	__property TScWebHeaderCollection* Headers = {read=FHeaders};
	__property System::TDateTime LastModified = {read=GetLastModified};
	__property TScRequestMethod Method = {read=FMethod, nodefault};
	__property Crsecureconnection::TScVersion* ProtocolVersion = {read=FProtocolVersion};
	__property System::UnicodeString ResponseUri = {read=FResponseUri};
	__property System::UnicodeString Server = {read=GetServer};
	__property TScHttpStatusCode StatusCode = {read=GetStatusCode, nodefault};
	__property System::UnicodeString StatusDescription = {read=FStatusDescription};
	__property Crtypes::TScOnProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
private:
	void *__ISequentialStream;	// ISequentialStream 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
	operator _di_ISequentialStream()
	{
		_di_ISequentialStream intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator ISequentialStream*(void) { return (ISequentialStream*)&__ISequentialStream; }
	#endif
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION HttpException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	int FStatusCode;
	System::UnicodeString FServerMessage;
	TScHttpStatusCode __fastcall GetStatusCode();
	
public:
	__fastcall HttpException(const System::UnicodeString Msg)/* overload */;
	__fastcall HttpException(int StatusCode, const System::UnicodeString Msg)/* overload */;
	__fastcall HttpException(TScHttpStatusCode StatusCode, const System::UnicodeString Msg)/* overload */;
	__fastcall HttpException(int StatusCode, const System::UnicodeString Msg, const System::UnicodeString ServerMessage)/* overload */;
	__property int Code = {read=FStatusCode, nodefault};
	__property TScHttpStatusCode StatusCode = {read=GetStatusCode, nodefault};
	__property System::UnicodeString ServerMessage = {read=FServerMessage};
public:
	/* Exception.CreateFmt */ inline __fastcall HttpException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall HttpException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall HttpException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall HttpException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall HttpException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall HttpException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall HttpException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall HttpException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall HttpException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall HttpException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall HttpException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~HttpException() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScHttpWebResponseHelper : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod Crsecureconnection::TScSecureConnection* __fastcall GetSecureConnection(TCRHttpWebResponse* Obj);
	__classmethod void __fastcall SetSecureConnection(TCRHttpWebResponse* Obj, Crsecureconnection::TScSecureConnection* Value);
public:
	/* TObject.Create */ inline __fastcall TScHttpWebResponseHelper() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TScHttpWebResponseHelper() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define Content_Type_WWWForm L"application/x-www-form-urlencoded"
#define Content_Type_Octet L"application/octet-stream"
#define Accept_MediaRange1 L"www/source, text/html, video/mpeg, image/jpeg, image/x-tif"\
	L"f"
#define Accept_MediaRange2 L"image/x-rgb, image/x-xbm, image/gif, */*, application/post"\
	L"script"
#define HttpScheme L"http:"
}	/* namespace Crhttp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRHTTP)
using namespace Crhttp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrhttpHPP
