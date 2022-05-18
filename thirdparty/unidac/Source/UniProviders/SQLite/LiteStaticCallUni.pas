
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF VIRTUAL_QUERY}
unit LiteStaticCallUni;

{$DEFINE UNIDACPRO}
{$ENDIF}

interface

function InitStaticFunction(const API: pointer): boolean;

implementation

{$IFNDEF NOSTATIC}
uses
{$IFDEF VIRTUAL_QUERY}
  LiteCallVirtual, LiteStaticVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteCall, LiteStatic;
{$ELSE}
  LiteCallUni, LiteStaticUni;
{$ENDIF}
{$ENDIF}
{$ENDIF}

function InitStaticFunction(const API: pointer): boolean;
begin
{$IFNDEF NOSTATIC}
  DoInitStaticFunction(TSQLite3API(API));
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
