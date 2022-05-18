
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualQueryDesignUtils;

interface

uses
  Classes, SysUtils, Dialogs,
  CRDataTypeMap, 
  DBAccess, DADesignUtils;

type
  TVirtualDesignUtils = class(TDADesignUtils)
    class function GetProjectName: string; override;

  { Data Type Mapping }
    class function GetConverterManagerClass: TConverterManagerClass; override;
  end;

implementation

uses
  VirtualDataTypeMap;

{ TLiteDesignUtils }

class function TVirtualDesignUtils.GetProjectName: string;
begin
  Result := 'VirtualQuery';
end;

class function TVirtualDesignUtils.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TConverterManagerClass(TVirtualMapRules.GetConverterManager.ClassType);
end;

end.
