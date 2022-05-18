
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}

{$DEFINE UNIDACPRO}

{$IFNDEF NOSTATIC}
  {$IFDEF STD}
    {$IFNDEF LITE}
      {$DEFINE NOSTATIC}
    {$ENDIF}
    {$IFNDEF NOSQL}
      {$DEFINE NOSQL}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
    {$DEFINE NOSTATIC}
  {$ENDIF}

  {$IFNDEF NOSTATIC}
    {$IFDEF IOS}
      {$IFNDEF CPUARM}
        {$DEFINE NOSTATIC}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFNDEF NOSQL}
    {$DEFINE NOSQL}
  {$ENDIF}
{$ENDIF}

