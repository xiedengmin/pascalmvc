
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgScriptProcessorUni;

{$ENDIF}

interface

uses
  Classes, SysUtils,
  CRParser, DAScript;

type
  TPgScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  PgParser;
{$ELSE}
  PgParserUni;
{$ENDIF}

{ TPgScriptProcessor }

function TPgScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

end.
