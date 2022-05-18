{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "netsuiteprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "netsuiteprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "netsuiteprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "netsuiteprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "netsuiteprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "netsuiteprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "netsuiteprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "netsuiteprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "netsuiteprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "netsuiteprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
