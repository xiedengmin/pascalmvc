{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "zohoprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "zohoprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "zohoprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "zohoprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "zohoprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "zohoprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "zohoprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "zohoprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "zohoprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "zohoprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "zohoprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "zohoprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "zohoprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "zohoprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "zohoprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "zohoprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "zohoprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
