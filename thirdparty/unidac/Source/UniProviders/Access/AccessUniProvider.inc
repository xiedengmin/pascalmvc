{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "accessprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "accessprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "accessprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "accessprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "accessprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "accessprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "accessprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "accessprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "accessprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "accessprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "accessprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "accessprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "accessprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "accessprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "accessprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "accessprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "accessprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
