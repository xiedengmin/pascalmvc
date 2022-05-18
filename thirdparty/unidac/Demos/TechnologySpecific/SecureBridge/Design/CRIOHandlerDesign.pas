
//////////////////////////////////////////////////
//  Dac Components
//  Copyright © 1997-2021 Devart. All right reserved.
//  Design
//////////////////////////////////////////////////

unit CRIOHandlerDesign;

{$I SB.inc}

interface

uses
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
{$ENDIF}
  SysUtils, Classes;

type
  TCertNamesEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  CRSSLIOHandler;

{ TCertNamesEditor }

function TCertNamesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TCertNamesEditor.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  i: integer;
begin
  List := TStringList.Create;
  try
    if GetComponent(0) is TCRSSLIOHandler then begin
      if TCRSSLIOHandler(GetComponent(0)).Storage <> nil then
        TCRSSLIOHandler(GetComponent(0)).Storage.Certificates.GetCertificateNames(List);
    end;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TCRSSLIOHandler, 'CertName', TCertNamesEditor);
  RegisterPropertyEditor(TypeInfo(string), TCRSSLIOHandler, 'CACertName', TCertNamesEditor);
end;

end.

