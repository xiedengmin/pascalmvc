{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma link "vquery100.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider100.bpi"'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma link "vquery105.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider105.bpi"'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma link "vquery120.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider120.bpi"'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma link "vquery140.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider140.bpi"'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma link "vquery150.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider150.bpi"'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma link "vquery160.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider160.bpi"'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$HPPEMIT '#pragma link "vquery170.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider170.bpi"'}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$HPPEMIT '#pragma link "vquery180.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider180.bpi"'}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery190.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider190.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery200.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider200.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery210.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider210.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery220.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider220.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery230.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider230.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery240.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider240.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery250.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider250.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery260.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider260.bpi"'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFNDEF MSWINDOWS}
{$HPPEMIT LINKUNIT}
{$ELSE}
{$HPPEMIT '#pragma link "vquery270.bpi"'}
{$HPPEMIT '#pragma link "mongoprovider270.bpi"'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} { Delphi 2006 }
{$HPPEMIT '#pragma comment(lib, "vquery100.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider100.lib")'}
{$ENDIF}
{$IFDEF VER11} { RAD Studio 2007 }
{$HPPEMIT '#pragma comment(lib, "vquery105.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider105.lib")'}
{$ENDIF}
{$IFDEF VER12} { RAD Studio 2009 }
{$HPPEMIT '#pragma comment(lib, "vquery120.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider120.lib")'}
{$ENDIF}
{$IFDEF VER14} { RAD Studio 2010 }
{$HPPEMIT '#pragma comment(lib, "vquery140.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider140.lib")'}
{$ENDIF}
{$IFDEF VER15} { RAD Studio XE }
{$HPPEMIT '#pragma comment(lib, "vquery150.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider150.lib")'}
{$ENDIF}
{$IFDEF VER16} { RAD Studio XE2 }
{$HPPEMIT '#pragma comment(lib, "vquery160.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider160.lib")'}
{$ENDIF}
{$IFDEF VER17} { RAD Studio XE3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery170.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider170.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery170.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider170.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER18} { RAD Studio XE4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery180.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider180.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery180.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider180.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER19} { RAD Studio XE5 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery190.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider190.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery190.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider190.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER20} { RAD Studio XE6 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery200.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider200.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery200.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider200.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER21} { RAD Studio XE7 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery210.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider210.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery210.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider210.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER22} { RAD Studio XE8 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery220.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider220.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery220.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider220.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER23} { RAD Studio 10 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery230.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider230.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery230.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider230.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER24} { RAD Studio 10.1 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery240.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider240.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery240.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider240.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER25} { RAD Studio 10.2 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery250.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider250.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery250.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider250.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER26} { RAD Studio 10.3 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery260.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider260.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery260.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider260.a")'}
{$ENDIF}
{$ENDIF}
{$IFDEF VER27} { RAD Studio 10.4 }
{$IFDEF WIN32}
{$HPPEMIT '#pragma comment(lib, "vquery270.lib")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider270.lib")'}
{$ELSE}
{$HPPEMIT '#pragma comment(lib, "vquery270.a")'}
{$HPPEMIT '#pragma comment(lib, "mongoprovider270.a")'}
{$ENDIF}
{$ENDIF}
{$HPPEMIT '#endif'}
