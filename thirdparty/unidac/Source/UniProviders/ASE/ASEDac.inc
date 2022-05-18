
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}

{$DEFINE UNIDACPRO}

{$IFNDEF NOTDS}
  {$DEFINE TDS}           // enable Direct Mode
{$ENDIF}

{$IFNDEF MOBILE}
{$IFNDEF NO_ODBC_PROVIDER}
  {$DEFINE ODBC_PROVIDER}  
{$ENDIF}
{$ENDIF}

{$IFNDEF LITE}
  {$DEFINE USE_DB_ERROR}
{$ENDIF}

{$DEFINE USE_SSL}

{$DEFINE DAC}
