
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Error
//////////////////////////////////////////////////


{$I Odac.inc}
unit OraErrorUni;

interface

uses
  SysUtils, Classes,
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
  CRTypes, CRAccess,
{$IFNDEF UNIDACPRO}
  OraCall;
{$ELSE}
  OraCallUni;
{$ENDIF}

type
{$IFDEF NODBACCESS}
  EOraError = class(ECRError)
{$ELSE}
  EOraError = class(EDAError)
{$ENDIF}
  private
    FSender: TComponent;
  public
    constructor Create(ErrorCode: integer; const Msg: string; Component: TObject = nil);
    destructor Destroy; override;

  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

    property Sender: TComponent read FSender write FSender;
  end;

  TOraError = class
    class procedure Check(OCIErrorGet: _OCIErrorGet; Status: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
    class procedure DoOraError(OCIErrorGet: _OCIErrorGet; ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
    class function GetOraError(OCIErrorGet: _OCIErrorGet; ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError; var ErrorMsg: string): sword;
  end;

  procedure RaiseError(Msg: string);
  procedure RaiseOraError(ErrorCode: integer; Msg: string);

const
  erKeyViol           = 0;
  erRequiredFieldMissing = 1;
  erCheck             = 2;
  erLockRecord        = -54;
  erParentKeyNotFound = -2291;
  erChildRecordCount  = -2292;

implementation

uses
{$IFDEF CLR}
  System.Runtime.InteropServices;
{$ELSE}
  CLRClasses;
{$ENDIF}

var
  OracleErrorMaxLength : integer;

procedure RaiseError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseOraError(ErrorCode: integer; Msg: string);
begin
  raise EOraError.Create(ErrorCode, Msg);
end;

{ TOraError }

class procedure TOraError.Check(OCIErrorGet: _OCIErrorGet; Status: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
begin
  if Status <> OCI_SUCCESS then
    DoOraError(OCIErrorGet, Status, UnicodeEnv, hOCIError);
end;

class procedure TOraError.DoOraError(OCIErrorGet: _OCIErrorGet; ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
var
  Msg: string;
  OrgErrorCode: sword;
begin
  OrgErrorCode := ErrorCode;
  ErrorCode := GetOraError(OCIErrorGet, ErrorCode, UnicodeEnv, hOCIError, Msg);

  if OrgErrorCode <> OCI_SUCCESS_WITH_INFO then
    raise EOraError.Create(Abs(ErrorCode), TrimRight(Msg));
end;

class function TOraError.GetOraError(OCIErrorGet: _OCIErrorGet; ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError; var ErrorMsg: string): sword;
var
  hMsg: IntPtr;
  St: string;
  Res: sword;
  BufSize: integer;
begin
//  Home.CheckOCI80;

  case ErrorCode of
    OCI_SUCCESS: begin
      Result := ErrorCode;
      Exit;
    end;
    OCI_ERROR,
    OCI_SUCCESS_WITH_INFO,
    OCI_NO_DATA:
    begin
      BufSize := OracleErrorMaxLength * SizeOfCharOCI(UnicodeEnv);
      hMsg := Marshal.AllocHGlobal(BufSize);
      try
        Marshal.WriteInt16(hMsg, 0);
        Res := OCIErrorGet(hOCIError, 1, nil, ErrorCode, hMsg, BufSize, OCI_HTYPE_ERROR);
        ErrorMsg := PtrToStringOCI(hMsg, UnicodeEnv);
      finally
        Marshal.FreeHGlobal(hMsg);
      end;

      case Res of
        OCI_SUCCESS:
          if ErrorMsg = '' then
            ErrorMsg := 'ORA-' + IntToStr(ErrorCode);
        OCI_ERROR: begin // For some errors ORACLE BUG
          St := Copy(ErrorMsg, 1, OracleErrorMaxLength);
          if Pos('ORA-', St) = 1 then begin
            St := Copy(St, 5, 5);
            try
              ErrorCode := StrToInt(St);
            except
            end;
          end;
        end;
        OCI_SUCCESS_WITH_INFO:
          ErrorMsg := 'OCI_SUCCESS_WITH_INFO';
        OCI_NO_DATA:
          ErrorMsg := 'OCI_NO_DATA';
      end;
    end;
    OCI_INVALID_HANDLE:
      ErrorMsg := 'OCI_INVALID_HANDLE';
    OCI_NEED_DATA:
      ErrorMsg := 'OCI_NEED_DATA';
  else
    ErrorMsg := 'Unknown error: ORA-' + IntToStr(ErrorCode);
  end;
  ErrorCode := Abs(ErrorCode);
  Result := ErrorCode;
end;

{ EOracleError }

constructor EOraError.Create(ErrorCode: integer; const Msg: string; Component: TObject = nil);
begin
  inherited Create(ErrorCode, Msg);

  FSender := nil;
{$IFNDEF NODBACCESS}
  FComponent := Component;
{$ENDIF}
end;

destructor EOraError.Destroy;
begin
  inherited;
end;

{$IFNDEF NODBACCESS}
function EOraError.IsFatalError: boolean;
begin
  Result := (ErrorCode = OCI_SESSION_KILLED) or
    (ErrorCode = OCI_NOT_LOGGEDON) or
    (ErrorCode = OCI_EOF_COMMUNICATION) or
    (ErrorCode = OCI_NOT_CONNECTED) or
    (ErrorCode = 12203) or // after break;
    (ErrorCode = 12571) or
    (ErrorCode = 12514) or
    (ErrorCode = 12152) or //Oracle 9.2.0.6 fix (CR 8513)
    (ErrorCode = 3113) or
    (ErrorCode = 3135);
end;

function EOraError.IsKeyViolation: boolean;
begin
  Result := ErrorCode = 1;
end;
{$ENDIF}

initialization
  OracleErrorMaxLength := 1024;

end.
