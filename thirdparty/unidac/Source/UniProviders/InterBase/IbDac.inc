
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

// Type of build

{$I Dac.inc}

{$DEFINE UNIDACPRO}

{$DEFINE _DEBUG}          // DEBUG

{$IFDEF UNIX}
  {$J+}                   // allows to write typed constants
{$ENDIF}


{$IFDEF CPUARM}
  {$DEFINE IBTOGO_STATIC}
  {$UNDEF IBTOGO_USE_ABI}
  {$IFDEF VER27P}
    {$IFDEF ANDROID}
      {$DEFINE IBTOGO_USE_ABI}
    {$ENDIF}
  {$ELSE}
    {$IFDEF ANDROID64}
      {$DEFINE IBTOGO_USE_ABI}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
