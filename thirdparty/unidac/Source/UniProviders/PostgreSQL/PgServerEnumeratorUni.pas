
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//  PostgreSQL Server Enumerator
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgServerEnumeratorUni;

interface

uses
  Classes, CRServerEnumerator;

type
  TPgServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: TStrings); override;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  CRFunctions;
{$ENDIF}
{ TPgServerEnumerator }

procedure TPgServerEnumerator.GetServerList(List: TStrings);
{$IFDEF MSWINDOWS}
var
  List1: TStringList;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  List1 := TStringList.Create;
  try
    CRNetManager.GetServerList(List1, 'postgresql');
    AssignStrings(List1, List);
  finally
    List1.Free;
  end;
{$ENDIF}
end;



end.
