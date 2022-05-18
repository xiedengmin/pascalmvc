//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I NexusDac.inc}
unit NexusParserUni;
{$ENDIF}

interface

{$IFNDEF DUMMY}
uses
  Classes, SysUtils, CRTypes, CRParser;

type
  TNexusParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: char): boolean; override;
  end;
{$ENDIF}

implementation

{$IFNDEF DUMMY}
function TNexusParser.IsIdentQuote(Ch: char): boolean;
begin
  Result := False;
  case Ch of
    '''', '"':
      Result := True;
  end;
end;
{$ENDIF}

end.
