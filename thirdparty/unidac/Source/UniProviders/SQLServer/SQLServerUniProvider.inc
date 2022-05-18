{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "tdsprovider100.bpi"'}
{$HPPEMIT '#pragma link "msprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "tdsprovider105.bpi"'}
{$HPPEMIT '#pragma link "msprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "tdsprovider120.bpi"'}
{$HPPEMIT '#pragma link "msprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "tdsprovider140.bpi"'}
{$HPPEMIT '#pragma link "msprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "tdsprovider150.bpi"'}
{$HPPEMIT '#pragma link "msprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "tdsprovider160.bpi"'}
{$HPPEMIT '#pragma link "msprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "tdsprovider170.bpi"'}
{$HPPEMIT '#pragma link "msprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "tdsprovider180.bpi"'}
{$HPPEMIT '#pragma link "msprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider190.bpi"'}
{$HPPEMIT '#pragma link "msprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider200.bpi"'}
{$HPPEMIT '#pragma link "msprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider210.bpi"'}
{$HPPEMIT '#pragma link "msprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider220.bpi"'}
{$HPPEMIT '#pragma link "msprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider230.bpi"'}
{$HPPEMIT '#pragma link "msprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider240.bpi"'}
{$HPPEMIT '#pragma link "msprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider250.bpi"'}
{$HPPEMIT '#pragma link "msprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider260.bpi"'}
{$HPPEMIT '#pragma link "msprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "tdsprovider270.bpi"'}
{$HPPEMIT '#pragma link "msprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "tdsprovider100.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "tdsprovider105.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "tdsprovider120.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "tdsprovider140.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "tdsprovider150.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "tdsprovider160.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider170.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider170.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider180.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider180.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider190.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider190.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider200.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider200.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider210.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider210.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider220.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider220.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider230.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider230.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider240.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider240.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider250.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider250.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider260.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider260.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "tdsprovider270.lib")'}
{$HPPEMIT '#pragma comment(lib, "msprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "tdsprovider270.a")'}
{$HPPEMIT '#pragma comment(lib, "msprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
