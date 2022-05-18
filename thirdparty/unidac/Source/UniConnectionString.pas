
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  UniConnectionString
//////////////////////////////////////////////////

{$I UniDac.inc}

unit UniConnectionString;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRTypes, CRParser, CRAccess, CRVio, CRConnectionString,
  UniConsts;

const
  cpProviderName = -1001;

type
  TUniConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;

    procedure InitExtStringBuilder; override;
  public
    constructor Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod); override;

    property ExtStringBuilderClass;
  end;

  TUniConnectionDefStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  UniProvider;

{ TUniConnectionStringBuilder }

constructor TUniConnectionStringBuilder.Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod);
begin
  inherited;

  InitExtStringBuilder;
end;

procedure TUniConnectionStringBuilder.InitParams;
begin
  AddParam(ppHighest, 'Provider Name', ['ProviderName'], cpProviderName, varString, '');
end;

procedure TUniConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
var
  ProviderName: string;
begin
  case Param.Code of
    cpProviderName: begin
      ProviderName := Trim(VarToStr(Value));
      if (ProviderName <> '') and (UniProviders.GetProvider(ProviderName) = nil) then
        CheckProviderName(ProviderName)
      else
        inherited SetParamValue(Param, Value);
    end
    else
      inherited SetParamValue(Param, Value);
  end;
end;

procedure TUniConnectionStringBuilder.InitExtStringBuilder;
begin
  if FExtStringBuilderClass <> nil then
    FExtStringBuilder := FExtStringBuilderClass.Create(GetProp, SetProp)
  else
    FExtStringBuilder := TUniConnectionDefStringBuilder.Create(GetProp, SetProp);
end;

{ TUniConnectionDefStringBuilder }

procedure TUniConnectionDefStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Server'], prServer, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, DefValPort);
end;

end.




