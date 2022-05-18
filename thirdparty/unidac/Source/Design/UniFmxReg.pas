
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}
unit UniFmxReg;
{$ENDIF}

interface

uses
  Classes,
  UniDacFmx;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('UniDAC', [TUniConnectDialogFmx]);
end;

end.
