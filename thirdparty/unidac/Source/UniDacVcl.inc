{$HPPEMIT '#ifdef USEPACKAGES'}
{$IFDEF VER10} {Delphi 2006}
{$HPPEMIT '#pragma link "UniDacVcl100.bpi"'}
{$ENDIF}
{$IFDEF VER11} {Delphi 2007}
{$HPPEMIT '#pragma link "UniDacVcl105.bpi"'}
{$ENDIF}
{$IFDEF VER12} {Delphi 2009}
{$HPPEMIT '#pragma link "UniDacVcl120.bpi"'}
{$ENDIF}
{$IFDEF VER14} {Delphi 2010}
{$HPPEMIT '#pragma link "UniDacVcl140.bpi"'}
{$ENDIF}
{$IFDEF VER15} {Delphi XE}
{$HPPEMIT '#pragma link "UniDacVcl150.bpi"'}
{$ENDIF}
{$IFDEF VER16} {Delphi XE2}
{$HPPEMIT '#pragma link "UniDacVcl160.bpi"'}
{$ENDIF}
{$IFDEF VER17} {Delphi XE3}
{$HPPEMIT '#pragma link "UniDacVcl170.bpi"'}
{$ENDIF}
{$IFDEF VER18} {Delphi XE4}
{$HPPEMIT '#pragma link "UniDacVcl180.bpi"'}
{$ENDIF}
{$IFDEF VER19} {Delphi XE5}
{$HPPEMIT '#pragma link "UniDacVcl190.bpi"'}
{$ENDIF}
{$IFDEF VER20} {Delphi XE6}
{$HPPEMIT '#pragma link "UniDacVcl200.bpi"'}
{$ENDIF}
{$IFDEF VER21} {Delphi XE7}
{$HPPEMIT '#pragma link "UniDacVcl210.bpi"'}
{$ENDIF}
{$HPPEMIT '#else'}
{$IFDEF VER10} {Delphi 2006}
{$HPPEMIT '#pragma comment(lib, "unidacvcl100")'}
{$ENDIF}
{$IFDEF VER11} {Delphi 2007}
{$HPPEMIT '#pragma comment(lib, "unidacvcl105")'}
{$ENDIF}
{$IFDEF VER12} {Delphi 2009}
{$HPPEMIT '#pragma comment(lib, "unidacvcl120")'}
{$ENDIF}
{$IFDEF VER14} {Delphi 2010}
{$HPPEMIT '#pragma comment(lib, "unidacvcl140")'}
{$ENDIF}
{$IFDEF VER15} {Delphi XE}
{$HPPEMIT '#pragma comment(lib, "unidacvcl150")'}
{$ENDIF}
{$IFDEF VER16} {Delphi XE2}
{$HPPEMIT '#pragma comment(lib, "unidacvcl160")'}
{$ENDIF}
{$IFDEF VER17} {Delphi XE3}
{$HPPEMIT '#pragma comment(lib, "unidacvcl170")'}
{$ENDIF}
{$IFDEF VER18} {Delphi XE4}
{$HPPEMIT '#pragma comment(lib, "unidacvcl180")'}
{$ENDIF}
{$IFDEF VER19} {Delphi XE5}
{$HPPEMIT '#pragma comment(lib, "unidacvcl190")'}
{$ENDIF}
{$IFDEF VER20} {Delphi XE6}
{$HPPEMIT '#pragma comment(lib, "unidacvcl200")'}
{$ENDIF}
{$IFDEF VER21} {Delphi XE7}
{$HPPEMIT '#pragma comment(lib, "unidacvcl210")'}
{$ENDIF}
{$HPPEMIT '#endif'}