{$IFNDEF CLR}
{$I Odac.inc}
unit OraScriptProcessorUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  CRTypes, DBAccess, DAScript, CRParser, CRAccess,
  {$IFNDEF UNIDACPRO}
    OraClasses;
  {$ELSE}
    OraClassesUni;
  {$ENDIF}

const
  ST_NORMAL     = 100;  // normal SQL query
  ST_SQLPLUS    = $100; // mask for SQL*Plus commands
  ST_IGNORED    = ST_SQLPLUS + 1;  // ignored SQL*Plus command
  ST_CONNECT    = ST_SQLPLUS + 2;  // SQL*Plus command CONNECT
  ST_DISCONNECT = ST_SQLPLUS + 3;  // SQL*Plus command DISCONNECT
  ST_DEFINE     = ST_SQLPLUS + 4;  // SQL*Plus command DEFINE
  ST_EXECUTE    = ST_SQLPLUS + 5;  // SQL*Plus command EXECUTE
  ST_PRIVILEGES = ST_SQLPLUS + 6;  // GRANT or REVOKE command

type
  TCustomOraScriptProcessor = class (TDAScriptProcessor)
  protected
    FCodes: array of Integer;
    FStatementType: integer;
    FInWrapped: boolean;

    function ExecuteNext: boolean; override;
    function SlashIsDelimiter: boolean; override;

    procedure SetConnectMode(Connection: TCustomDAConnection; ConnectMode: TConnectMode); virtual; abstract;
    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
    function GetReady(Code: integer): boolean; override;
    procedure DoBeforeStatementExecute(var SQL: string; StatementType: integer; var Omit: boolean); override;
    function IsSpecificSQL(StatementType: integer): boolean; override;
  public
    constructor Create(Owner: TDAScript); override;
  end;

implementation

uses
  DAConsts,
  {$IFNDEF UNIDACPRO}
    OraConsts, OraParser;
  {$ELSE}
    OraConstsUni, OraParserUni;
  {$ENDIF}

{ TCustomOraScriptProcessor }

const
   HighCodeIndex = 2;

constructor TCustomOraScriptProcessor.Create(Owner: TDAScript);
begin
  inherited;

  SetLength(FCodes, HighCodeIndex + 1);
end;

function TCustomOraScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

function TCustomOraScriptProcessor.ExecuteNext: boolean;
begin
  FInWrapped := False;
  FStatementType := ST_UNKNOWN;

  Result := inherited ExecuteNext;
end;

procedure TCustomOraScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
var
  i: integer;
begin
  inherited;

  //Analize and set StatementType
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    if StatementType in [ST_UNKNOWN, ST_STATEMENT] then
      for i := 0 to HighCodeIndex - 1 do
        FCodes[i] := 0
    else
      for i := 0 to HighCodeIndex - 1 do
        FCodes[i] := FCodes[i + 1];

    FCodes[HighCodeIndex] := Code;

    case StatementType of
      ST_UNKNOWN, ST_STATEMENT:
        case Code of
          lxBEGIN, lxDECLARE:
            StatementType := ST_SPECIFIC_SQL;
          lxGRANT, lxREVOKE:
            StatementType := ST_PRIVILEGES;

          // check SQL*Plus commands
          lxEXIT, lxPAUSE, lxPROMPT, lxQUIT, lxREMARK, lxREM, lxUNDEFINE, lxACCEPT:
            StatementType := ST_IGNORED;
          lxCONNECT:
            StatementType := ST_CONNECT;
          lxDISCONNECT:
            StatementType := ST_DISCONNECT;
          lxDEFINE:
            StatementType := ST_DEFINE;
          lxEXECUTE, lxEXEC:
            StatementType := ST_EXECUTE;
        else
          StatementType := ST_NORMAL;
        end;
      ST_NORMAL:
        case Code of
          lxPROCEDURE, lxFUNCTION, lxPACKAGE, lxTRIGGER:
            case FCodes[HighCodeIndex - 1] of
              lxWITH:
                StatementType := ST_SPECIFIC_SQL;
              lxCREATE:
                StatementType := ST_SPECIFIC_SQL;
              lxREPLACE:
                if FCodes[HighCodeIndex - 2] = lxOR then
                  StatementType := ST_SPECIFIC_SQL;
              lxEDITIONABLE:
                if (FCodes[HighCodeIndex - 2] = lxCREATE) or
                   (FCodes[HighCodeIndex - 2] = lxREPLACE)
                then
                  StatementType := ST_SPECIFIC_SQL;
              lxNONEDITIONABLE:
                if (FCodes[HighCodeIndex - 2] = lxCREATE) or
                   (FCodes[HighCodeIndex - 2] = lxREPLACE)
                then
                  StatementType := ST_SPECIFIC_SQL;
            end;
          lxBODY:
            if FCodes[HighCodeIndex - 1] = lxTYPE then
              StatementType := ST_SPECIFIC_SQL;
          lxJAVA:
            StatementType := ST_SPECIFIC_SQL;

          // SQL*Plus commands
          lxDEFINE:
            if (FCodes[HighCodeIndex - 1] = lxSET) and (FCodes[HighCodeIndex - 2] = 0) then
              StatementType := ST_IGNORED;
        end;
      ST_SPECIFIC_SQL:
        case Code of
          lxSOURCE, lxRESOURCE, lxCLASS:
            if FCodes[HighCodeIndex - 1] = lxJAVA then
              StatementType := ST_NORMAL;
        end;
    end;
  end;

  FStatementType := StatementType;
end;

function TCustomOraScriptProcessor.GetReady(Code: integer): boolean;
begin
  if FInWrapped then begin
    FInWrapped := False;
    FSQL.Append(FSt);
    Result := true;
  end
  else if Code = lxWRAPPED then begin
    FInWrapped := True;
    Result := False;
  end
  else if (FStatementType and ST_SQLPLUS <> 0) and (FStatementType<>ST_PRIVILEGES) and ((Pos(#13, FSt) > 0) or (Pos(#10, FSt) > 0)) then
    Result := True
  else
    Result := False;
end;

procedure TCustomOraScriptProcessor.DoBeforeStatementExecute(var SQL: string; StatementType: integer; var Omit: boolean);
var
  SQLParser: TParser;
  MacroName, MacroVal: string;
  Code: integer;
  St: string;
  Macro: TMacro;
  ConnectStr: string;
  AUsername, APassword, AServer: string;
  AConnectMode: TConnectMode;

  procedure BypassBlanks;
  begin
    repeat
      Code := SQLParser.GetNext(St);
    until (Code <> lcBlank) or (Code = lcEnd);
  end;

  procedure RaiseException;
  begin
    raise Exception.CreateFmt(SInvalidLexem, [St, SQLParser.CurrPos + 1 - Length(St), SQL]);
  end;

begin
  inherited;

  if Omit then
    Exit;

  case StatementType of
    ST_IGNORED:
      Omit := True;

    ST_CONNECT: begin
      SQLParser := GetSQLParser(SQL);

      Code := SQLParser.GetNext(St); // lxCONNECT
      BypassBlanks;

      ConnectStr := '';
      while (Code <> lcEnd) do begin
        ConnectStr := ConnectStr + St;
        Code := SQLParser.GetNext(St);
      end;

      ParseConnectString(ConnectStr, AUsername, APassword, AServer, AConnectMode);
      UsedConnection.Username := AUsername;
      UsedConnection.Password := APassword;
      UsedConnection.Server := AServer;
      SetConnectMode(UsedConnection, AConnectMode);
      UsedConnection.Connect;

      Omit := True;
    end;

    ST_DISCONNECT: begin
      UsedConnection.Disconnect;
      Omit := True;
    end;

    ST_DEFINE: begin
      SQLParser := GetSQLParser(SQL);

      Code := SQLParser.GetNext(St); // lxDEFINE
      BypassBlanks;

      if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then
        MacroName := St
      else
        RaiseException;

      Code := SQLParser.GetNext(St);

      // for names that begin with number
      if (Code = lcIdent) or (Code >= lxSQLFirst) then begin
        MacroName := MacroName + St;
        Code := SQLParser.GetNext(St);
      end;

      if Code = lcBlank then
        BypassBlanks;

      if St <> '=' then
        RaiseException;

      BypassBlanks;

      if OCISQLInfo.IsQuoted(St) then
        MacroVal := OCISQLInfo.UnQuote(St)
      else begin
        MacroVal := St;
        repeat
          Code := SQLParser.GetNext(St);
          if (Code = lcEnd) or (Code = lcBlank) then
            Break;
          MacroVal := MacroVal + St;
        until False;
      end;

      Macro := FOwner.Macros.FindMacro(MacroName);
      if Macro = nil then begin
        Macro := TMacro(FOwner.Macros.Add);
        Macro.Name := MacroName;
      end;
      Macro.Value := MacroVal;

      Omit := True;
    end;

    ST_EXECUTE: begin
      SQLParser := GetSQLParser(SQL);
      Code := SQLParser.GetNext(St);
      if Code = lcBlank then
        BypassBlanks;
      if (Code = lxEXEC) or (Code = lxEXECUTE) then
        SQL := Copy(SQL, SQLParser.CurrPos + 1, Length(SQL) - SQLParser.CurrPos);
      if SQL[Length(SQL)] <> ';' then
        SQL := SQL + ';';
      SQL := 'BEGIN'#13#10 + SQL + #13#10'END;';
    end;
  end;
end;

function TCustomOraScriptProcessor.SlashIsDelimiter: boolean;
begin
  Result := True;
end;

function TCustomOraScriptProcessor.IsSpecificSQL(StatementType: integer): boolean;
begin
  Result := inherited IsSpecificSQL(StatementType) or (StatementType = ST_EXECUTE);
end;

end.
