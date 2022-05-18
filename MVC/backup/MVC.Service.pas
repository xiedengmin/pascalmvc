{*******************************************************}
{                                                       }
{                                       }
{                              }
{                     }
{                                                       }
{*******************************************************}
unit MVC.Service;

interface

uses
  SysUtils, Classes, MVC.DB, MVC.Config,mvc.dbMySql57, mvc.dbSQLite;

type
  TService = class(TInterfacedObject)
  private
   // FDb: TDB;

  public
  //  property Db: TDB read FDb;

    function Q(str: string): string;
    function GetGUID: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TService }
function TService.GetGUID: string;
var
  LTep: TGUID;
  sGUID: string;
begin
  CreateGUID(LTep);
  sGUID := GUIDToString(LTep);
  sGUID := StringReplace(sGUID, '-', '', [rfReplaceAll]);
  sGUID := Copy(sGUID, 2, Length(sGUID) - 2);
  result := sGUID;
end;

constructor TService.Create;
begin

  inherited Destroy;
 // FDb := TDB.Create;
end;

destructor TService.Destroy;
begin
 // Db.Free;

  inherited Destroy;
  inherited;
end;

function TService.Q(str: string): string;
begin
  Result := QuotedStr(str);
end;

end.

