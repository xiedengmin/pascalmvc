
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

// Type of build

{$I Dac.inc}

{$DEFINE UNIDACPRO}

//{$IFDEF FPC}
//  {$DEFINE NONET}         // Remove this line to enable Direct Mode in FPC
//{$ENDIF}

{$IFNDEF NONET}
  {$DEFINE NET}           // NET
{$ENDIF}

{$DEFINE _DEBUG}          // DEBUG

{$IFDEF MSWINDOWS}
  {$DEFINE USE_ORAMEMO}   // Use TOraMemo if it is possible

  {$IFNDEF STD}
    {$IFNDEF FPC}
      {$DEFINE SMART_REFRESH}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
  {$J+}                   // allows to write typed constants
{$ENDIF}

{$UNDEF HAVE_COMPRESS_INTERNAL}

