
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}

{$DEFINE UNIDACPRO}

{$IFDEF IS_UNICODE}
  {$IFDEF MSWINDOWS}
    {$DEFINE USE_UNICODE_DRIVER}
  {$ENDIF}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$DEFINE USE_UTF8_DRIVER}
{$ENDIF}
