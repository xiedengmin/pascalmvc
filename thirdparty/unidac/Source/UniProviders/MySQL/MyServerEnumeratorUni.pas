
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  MySQL Server Enumerator
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyServerEnumeratorUni;

interface

uses
  Classes, SysUtils, CRServerEnumerator;

type

  TMyServerEnumerator = class (TCRServerEnumerator)
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure GetServerList(List: TStrings); override;
  end;

implementation

{ TMyServerEnumerator }

function TMyServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TMyServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TMyServerEnumerator.GetServerList(List: TStrings);
begin
{$IFDEF MSWINDOWS}
  CRNetManager.GetServerList(List, 'mysql');
{$ENDIF}
end;

end.
