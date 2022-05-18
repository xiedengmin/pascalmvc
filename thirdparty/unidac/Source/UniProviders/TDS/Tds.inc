
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

// Type of build

{$I Dac.inc}

{$DEFINE UNIDACPRO}

{$IFNDEF NOTDS}
  {$DEFINE TDS}           // enable Direct Mode
{$ENDIF}


{$IFDEF UNIX}
{$IFNDEF DARWIN}
  {$DEFINE LINUX_BSD}
{$ENDIF}
{$ENDIF}

{$DEFINE USE_SSL}

{$DEFINE DAC}
