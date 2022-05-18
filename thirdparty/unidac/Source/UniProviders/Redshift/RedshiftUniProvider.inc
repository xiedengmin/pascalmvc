{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "rsprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "rsprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "rsprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "rsprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "rsprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "rsprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "rsprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "rsprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "rsprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "rsprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "rsprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "rsprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "rsprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "rsprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "rsprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "rsprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "rsprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
