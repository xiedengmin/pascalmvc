{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "vquery100.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "vquery105.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "vquery120.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "vquery140.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "vquery150.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "vquery160.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "vquery170.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "vquery180.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery190.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery200.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery210.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery220.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery230.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery240.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery250.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery260.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery270.bpi"'}
{$HPPEMIT '#pragma link "dbfprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "vquery100.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "vquery105.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "vquery120.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "vquery140.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "vquery150.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "vquery160.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery170.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery170.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery180.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery180.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery190.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery190.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery200.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery200.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery210.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery210.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery220.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery220.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery230.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery230.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery240.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery240.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery250.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery250.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery260.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery260.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery270.lib")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery270.a")'}
{$HPPEMIT '#pragma comment(lib, "dbfprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
