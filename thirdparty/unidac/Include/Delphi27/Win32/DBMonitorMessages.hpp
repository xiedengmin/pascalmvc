// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBMonitorMessages.pas' rev: 34.00 (Windows)

#ifndef DbmonitormessagesHPP
#define DbmonitormessagesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <Winapi.Windows.hpp>
#include <System.Types.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRVioTcp.hpp>

//-- user supplied -----------------------------------------------------------
class TCallStackItem;

namespace Dbmonitormessages
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMessagePacker;
class DELPHICLASS TMessageUnPacker;
class DELPHICLASS TMonitorMessage;
class DELPHICLASS TEventMessage;
class DELPHICLASS TStartupMessage;
struct TMsgSQLParam;
class DELPHICLASS TEventSender;
class DELPHICLASS TCallStackItem;
class DELPHICLASS TEventStartMessage;
class DELPHICLASS TEventEndMessage;
class DELPHICLASS TSocketMessagePacker;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMessagePacker : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual void __fastcall WriteByte(System::Byte Value) = 0 ;
	virtual void __fastcall WriteInteger(int Value) = 0 ;
	virtual void __fastcall WriteString(const System::WideString Value) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TMessagePacker() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMessagePacker() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMessageUnPacker : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual void __fastcall ReadByte(/* out */ System::Byte &Value) = 0 ;
	virtual void __fastcall ReadInteger(/* out */ int &Value) = 0 ;
	virtual void __fastcall ReadCardinal(/* out */ unsigned &Value) = 0 ;
	virtual void __fastcall ReadString(/* out */ System::WideString &Value) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TMessageUnPacker() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMessageUnPacker() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMonitorMessage : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int MessageType;
	int ProcessId;
	virtual void __fastcall Write(TMessagePacker* Packer);
	virtual void __fastcall Read(TMessageUnPacker* Packer);
public:
	/* TObject.Create */ inline __fastcall TMonitorMessage() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMonitorMessage() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TEventMessage : public TMonitorMessage
{
	typedef TMonitorMessage inherited;
	
public:
	int EventType;
	System::WideString Description;
	System::TDateTime TimeStamp;
	int Index;
	__fastcall TEventMessage();
	virtual void __fastcall Write(TMessagePacker* Packer);
	virtual void __fastcall Read(TMessageUnPacker* Packer);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEventMessage() { }
	
};


class PASCALIMPLEMENTATION TStartupMessage : public TEventMessage
{
	typedef TEventMessage inherited;
	
public:
	System::WideString ExeName;
	System::WideString Host;
	__fastcall TStartupMessage();
	virtual void __fastcall Write(TMessagePacker* Packer);
	virtual void __fastcall Read(TMessageUnPacker* Packer);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStartupMessage() { }
	
};


struct DECLSPEC_DRECORD TMsgSQLParam
{
public:
	System::WideString Name;
	System::WideString DataType;
	System::WideString ParamType;
	System::WideString Value;
};


typedef System::DynamicArray<TMsgSQLParam> TMsgSQLParams;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEventSender : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int ID;
	bool Deleted;
	System::WideString Name;
	int ObjectType;
	System::WideString TypeName;
	TEventSender* Parent;
	bool Highlighted;
public:
	/* TObject.Create */ inline __fastcall TEventSender() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TEventSender() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCallStackItem : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TCallStackItem*> _TCallStackItem__1;
	
	
private:
	TCallStackItem* FParent;
	void __fastcall SetParent(TCallStackItem* Value);
	
public:
	System::WideString Name;
	_TCallStackItem__1 Childs;
	bool Excluded;
	bool Shortened;
	bool Highlighted;
	__property TCallStackItem* Parent = {read=FParent, write=SetParent};
	__fastcall virtual ~TCallStackItem();
	bool __fastcall ExcludedFromTree();
	TCallStackItem* __fastcall ParentNotExcluded();
	TCallStackItem* __fastcall ParentNotExcludedFromTree();
public:
	/* TObject.Create */ inline __fastcall TCallStackItem() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TEventStartMessage : public TEventMessage
{
	typedef TEventMessage inherited;
	
public:
	int EventID;
	unsigned StartTime;
	int ObjectID;
	System::WideString ObjectName;
	int ObjectType;
	System::WideString ObjectTypeName;
	int ParentID;
	System::WideString ParentName;
	int ParentType;
	System::WideString ParentTypeName;
	System::WideString SQL;
	TMsgSQLParams Params;
	System::DynamicArray<System::WideString> CallStack;
	TEventSender* SenderObject;
	TEventEndMessage* EndEvent;
	TCallStackItem* CallStackItem;
	__fastcall TEventStartMessage();
	virtual void __fastcall Write(TMessagePacker* Packer);
	virtual void __fastcall Read(TMessageUnPacker* Packer);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEventStartMessage() { }
	
};


class PASCALIMPLEMENTATION TEventEndMessage : public TEventMessage
{
	typedef TEventMessage inherited;
	
public:
	int EventID;
	unsigned EndTime;
	int Failed;
	System::WideString ErrorText;
	TMsgSQLParams Params;
	int RowsAffected;
	__fastcall TEventEndMessage();
	virtual void __fastcall Write(TMessagePacker* Packer);
	virtual void __fastcall Read(TMessageUnPacker* Packer);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEventEndMessage() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSocketMessagePacker : public TMessagePacker
{
	typedef TMessagePacker inherited;
	
private:
	System::UnicodeString FHost;
	int FPort;
	unsigned FReconnectTimeout;
	unsigned FSendTimeout;
	Crviotcp::TCRVioTcp* FVioTcp;
	unsigned FLastConnectTime;
	bool FLastConnectFailed;
	System::DynamicArray<System::Byte> FBuffer;
	int FBufferOffset;
	
public:
	__fastcall TSocketMessagePacker();
	__fastcall virtual ~TSocketMessagePacker();
	bool __fastcall Open();
	void __fastcall Close();
	bool __fastcall IsActive();
	bool __fastcall CheckActive();
	void __fastcall ClearBuffer();
	void __fastcall CheckAndRealloc(int ACount);
	void __fastcall Flush();
	void __fastcall WriteMessage(TMonitorMessage* Msg);
	virtual void __fastcall WriteByte(System::Byte Value);
	virtual void __fastcall WriteInteger(int Value);
	virtual void __fastcall WriteString(const System::WideString Value);
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property unsigned ReconnectTimeout = {read=FReconnectTimeout, write=FReconnectTimeout, nodefault};
	__property unsigned SendTimeout = {read=FSendTimeout, write=FSendTimeout, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MT_EVENT = System::Int8(0x1);
static const System::Int8 MT_STARTUP = System::Int8(0x2);
static const System::Int8 MT_EVENTSTART = System::Int8(0x4);
static const System::Int8 MT_EVENTEND = System::Int8(0x5);
static const System::Int8 MT_PING = System::Int8(0x6);
static const System::Word DBMonitorPort = System::Word(0x3e8);
static const System::Word DefaultReconnectTimeout = System::Word(0x1388);
static const System::Word DefaultSendTimeout = System::Word(0x3e8);
extern DELPHI_PACKAGE unsigned __fastcall GetTickInterval(unsigned StartTickCount, unsigned FinishTickCount);
}	/* namespace Dbmonitormessages */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBMONITORMESSAGES)
using namespace Dbmonitormessages;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbmonitormessagesHPP
