
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFParserUni;

interface

uses
  SysUtils, Classes,
  CRTypes, CRFunctions, CRParser,
{$IFDEF DBFENGINE}
  CRVirtualData,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  DBFConsts;
{$ELSE}
  DBFConstsUni;
{$ENDIF}

const
  lxDBFFirst = 1000;

  lxCREATE    = lxDBFFirst;
  lxDROP      = lxDBFFirst + 1;
  lxTABLE     = lxDBFFirst + 2;
  lxPACK      = lxDBFFirst + 3;
  lxMEMO      = lxDBFFirst + 4;
  lxDBF       = lxDBFFirst + 5;
  lxZAP       = lxDBFFirst + 6;
  lxREINDEX   = lxDBFFirst + 7;
  lxALTER     = lxDBFFirst + 8;
  lxADD       = lxDBFFirst + 9;
  lxCOLUMN    = lxDBFFirst + 10;
  lxNOT       = lxDBFFirst + 11;
  lxNULL      = lxDBFFirst + 12;
  lxPRIMARY   = lxDBFFirst + 13;
  lxUNIQUE    = lxDBFFirst + 14;
  //lxKEY       = lxDBFFirst + 15;
  lxDEFAULT   = lxDBFFirst + 16;
  //lxAUTOINC   = lxDBFFirst + 17;
  lxNEXTVALUE = lxDBFFirst + 18;
  lxSTEP      = lxDBFFirst + 19;
  lxINDEX     = lxDBFFirst + 20;
  lxON        = lxDBFFirst + 21;
  lxFOREIGN   = lxDBFFirst + 22;
  lxDELETE    = lxDBFFirst + 23;
  lxCHECK     = lxDBFFirst + 24;
  lxIN        = lxDBFFirst + 25;
  lxCOLLATE   = lxDBFFirst + 26;
  lxCASE      = lxDBFFirst + 27;
  lxEXCEPT    = lxDBFFirst + 28;

  lxABS       = lxDBFFirst;
  lxALLTRIM   = lxDBFFirst + 1;
  lxCHR       = lxDBFFirst + 2;
  lxINT       = lxDBFFirst + 3;
  lxFLOOR     = lxDBFFirst + 4;
  lxLEFT      = lxDBFFirst + 5;
  lxLOWER     = lxDBFFirst + 6;
  lxLTRIM     = lxDBFFirst + 7;
  lxRIGHT     = lxDBFFirst + 8;
  lxROUND     = lxDBFFirst + 9;
  lxSTR       = lxDBFFirst + 10;
  lxSUBSTR    = lxDBFFirst + 11;
  lxTRIM      = lxDBFFirst + 12;
  lxUPPER     = lxDBFFirst + 13;
  lxVAL       = lxDBFFirst + 14;
  lxAT        = lxDBFFirst + 15;
  lxLEN       = lxDBFFirst + 16;
  lxIIF       = lxDBFFirst + 17;
  lxMIN       = lxDBFFirst + 18;
  lxMAX       = lxDBFFirst + 19;
  lxMOD       = lxDBFFirst + 20;
  lxRECNO     = lxDBFFirst + 21;
  lxREPLICATE = lxDBFFirst + 22;
  lxSPACE     = lxDBFFirst + 23;
  lxCEIL      = lxDBFFirst + 24;
  lxSQRT      = lxDBFFirst + 25;
  lxLOG       = lxDBFFirst + 26;
  lxLOG10     = lxDBFFirst + 27;
  lxASC       = lxDBFFirst + 28;
  lxPROPER    = lxDBFFirst + 29;
  lxISUPPER   = lxDBFFirst + 30;
  lxISLOWER   = lxDBFFirst + 31;
  lxISDIGIT   = lxDBFFirst + 32;
  lxISALPHA   = lxDBFFirst + 33;
  lxPADL      = lxDBFFirst + 34;
  lxPADR      = lxDBFFirst + 35;
  lxPADC      = lxDBFFirst + 36;
  lxDTOC      = lxDBFFirst + 37;
  lxCTOD      = lxDBFFirst + 38;
  lxDTOS      = lxDBFFirst + 39;
  lxTTOC      = lxDBFFirst + 40;
  lxTTOD      = lxDBFFirst + 41;
  lxEMPTY     = lxDBFFirst + 42;
  lxSTRTRAN   = lxDBFFirst + 43;
  lxCHRTRAN   = lxDBFFirst + 44;
  lxDELETED   = lxDBFFirst + 45;
  lxSTRIPF    = lxDBFFirst + 46;
  lxTRANSFORM = lxDBFFirst + 47;

  lxNotEqual  = lxDBFFirst + 1000;
  lxMoreEqual = lxDBFFirst + 1001;
  lxLessEqual = lxDBFFirst + 1002;

type
  TDBFExpressionType = (etNone, etRoot, etGroup, etGroupEnd, etField, etConstant, etOperation, etFunction, etComma);
  TParseStage = (psNone, psLeft, psRight, psFunction, psComplete);

  TDBFExpression = class
  public
    Parent: TDBFExpression;
    Text: string;
    ExpressionType: TDBFExpressionType;
    ExpressionSubType: integer;
    ResultType: AnsiChar;
    Value,
    Result: variant;
    Left, Right: TDBFExpression;
    Fields: TStringList;
    Arguments: TList;
    Stage: TParseStage;
    FieldNo: integer;
    ForceBreak,
    Closed: boolean;

    constructor Create; overload;
    constructor Create(AType: TDBFExpressionType); overload;
    constructor Create(AParent: TDBFExpression; AType: TDBFExpressionType); overload;
    destructor Destroy; override;
  end;

  TDBFParser = class(TSQLParser)
  protected
    procedure InitParser; override;
  end;

  TDBFExpressionParser = class(TParser)
  protected
    procedure InitParser; override;
  public
    function ParseExpression(const Expression: string): TDBFExpression;
  end;

  TDBFExprParser = class(TParser)
  protected
    procedure InitParser; override;
  end;

  TDBFExpressionParserExt = class
  private
    FParser: TDBFExprParser;
    FExpression: string;
  protected
    function VisitExpression(const Parent, Group: TDBFExpression): TDBFExpression;

    function VisitFunction(const Parent, Group: TDBFExpression; FunctionType: integer; const FunctionName: string): TDBFExpression;
    function VisitField(const Parent, Group: TDBFExpression; const FieldName: string): TDBFExpression;
    function VisitOperation(const Parent, Group, LeftOperand: TDBFExpression; OperationType: integer; const OperationName: string): TDBFExpression;
    function VisitConstant(const Parent, Group: TDBFExpression; ConstantType: integer; Value: string): TDBFExpression;
    function VisitGroup(const Parent, Group: TDBFExpression): TDBFExpression;
  public
    constructor Create(const Expression: string);
    destructor Destroy; override;

    class function Parse(const Expression: string): TDBFExpression;
  end;

  TDBFExpressionVisitor = class
  protected
    function VisitGroup(const Expression: TDBFExpression): TDBFExpression; virtual;
    function VisitField(const Expression: TDBFExpression): TDBFExpression; virtual;
    function VisitConstant(const Expression: TDBFExpression): TDBFExpression; virtual;
    function VisitOperation(const Expression: TDBFExpression): TDBFExpression; virtual;
    function VisitFunction(const Expression: TDBFExpression): TDBFExpression; virtual;
  public
    function VisitExpression(const Expression: TDBFExpression): TDBFExpression; virtual;
  end;

  TDBFExpressionNormalizer = class(TDBFExpressionVisitor)
  protected
    function VisitOperation(const Expression: TDBFExpression): TDBFExpression; override;
  end;

{$IFDEF DBFENGINE}
  TOnGetDBFValue = function(const Value: TVirtualValue; const FieldNo: integer; const FieldName: string): variant of object;

  TDBFExpressionCalculator = class(TDBFExpressionVisitor)
  private
    FSender: TObject;
    FOnGetValue: TOnGetDBFValue;
    FValue: TVirtualValue;
    FFieldNo: integer;

    function VarTypeToDbfType(const Value: variant): AnsiChar;
    function DetectResultType(const LeftValue, RightValue: variant): TVarType;
  protected
    function VisitGroup(const Expression: TDBFExpression): TDBFExpression; override;
    function VisitField(const Expression: TDBFExpression): TDBFExpression; override;
    function VisitConstant(const Expression: TDBFExpression): TDBFExpression; override;
    function VisitOperation(const Expression: TDBFExpression): TDBFExpression; override;
    function VisitFunction(const Expression: TDBFExpression): TDBFExpression; override;
  public
    function VisitExpression(const Expression: TDBFExpression): TDBFExpression; override;
    function CalculateExpression(const Sender: TObject; const Expression: TDBFExpression; const Value: TVirtualValue; const FieldNo: integer; const OnGetValueFunction: TOnGetDBFValue): TDBFExpression;
  end;
{$ENDIF}

var
  DBFKeywordLexems: TLexemList;

implementation

uses
  Variants,
  Math,
  MemData,
{$IFNDEF UNIDACPRO}
  DBFFunction;
{$ELSE}
  DBFFunctionUni;
{$ENDIF}

var
  DBFExprKeywordLexems: TLexemList;

{ TDBFExpression }

constructor TDBFExpression.Create;
begin
  inherited Create;

  Arguments := TList.Create;
  Fields := TStringList.Create;

  Value := null;
  Result := null;
  FieldNo := -1;
end;

constructor TDBFExpression.Create(AType: TDBFExpressionType);
begin
  Create;

  ExpressionType := AType;
end;

constructor TDBFExpression.Create(AParent: TDBFExpression; AType: TDBFExpressionType);
begin
  Create(AType);

  Parent := AParent;
end;

destructor TDBFExpression.Destroy;
var
  i: integer;
begin
  for i := 0 to Arguments.Count - 1 do
    TDBFExpression(Arguments[i]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
  Arguments.Free;
  Fields.Free;
  Left.Free;
  Right.Free;

  inherited;
end;

{ TDBFParser }

procedure TDBFParser.InitParser;
begin
  inherited;

  FKeywordLexems := DBFKeywordLexems;
end;

{ TDBFExpressionParser }

procedure TDBFExpressionParser.InitParser;
begin
  inherited;

  FKeywordLexems := DBFExprKeywordLexems;
end;

function TDBFExpressionParser.ParseExpression(const Expression: string): TDBFExpression;
var
  CurrentGroup,
  CurrentExpression: TDBFExpression;
  n: integer;
  s: string;

  function GetExpression: TDBFExpression;
  var
    Code, n: integer;
    St, s, Field: string;
  begin
    Result := nil;

    Code := GetNext(St);
    if Code = lcEnd then
      Exit;

    Result := TDBFExpression.Create;

    case Code of
      lxLeftBracket: Result.ExpressionType := etGroup;
      lxRightBracket: Result.ExpressionType := etGroupEnd;
      lcIdent: begin
        Result.ExpressionType := etField;
        Field := St;
        while True do begin
          Code := GetNext(St);
          if (Code = lcIdent) or (Code = lxPoint) then
            Field := Field + St
          else begin
            Back;
            Break;
          end;
        end;
        Result.Value := Field;
      end;
      lcString,
      lcNumber: begin
        Result.ExpressionType := etConstant;
        if Code = lcString then
          Result.Value := St
        else begin
          if TryStrToInt(St, n) then
            Result.Value := n
          else begin
            St := StringReplace(St, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
            St := StringReplace(St, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
            Result.Value := StrToFloat(St);
          end;
        end;
      end;
      lxPlus,
      lxDash,
      lxAsterisk,
      lxSlash,
      lxEqual,
      lxMore,
      lxLess: begin
        Result.ExpressionType := etOperation;
        Result.ExpressionSubType := Code;
      end;
      lxComma: Result.ExpressionType := etComma;
      lxRECNO: begin
        Code := GetNext(s);
        if Code = lxLeftBracket then begin
          Result.ExpressionType := etFunction;
          Result.ExpressionSubType := Code;
        end
        else begin
          Result.ExpressionType := etField;
          Result.Value := St;
        end;
        Back;
      end;
      lxABS,
      lxFLOOR,
      lxROUND,
      lxINT,
      lxSTR,
      lxCHR,
      lxVAL,
      lxLTRIM,
      lxALLTRIM,
      lxTRIM,
      lxLEN,
      lxLOWER,
      lxUPPER,
      lxLEFT,
      lxRIGHT,
      lxSUBSTR,
      lxAT,
      lxIIF,
      lxMIN,
      lxMAX,
      lxMOD,
      lxREPLICATE,
      lxSPACE,
      lxCEIL,
      lxSQRT,
      lxLOG,
      lxLOG10,
      lxASC,
      lxPROPER,
      lxISUPPER,
      lxISLOWER,
      lxISDIGIT,
      lxISALPHA,
      lxPADL,
      lxPADR,
      lxPADC,
      lxDTOC,
      lxCTOD,
      lxDTOS,
      lxTTOC,
      lxTTOD,
      lxEMPTY,
      lxSTRTRAN,
      lxCHRTRAN,
      lxDELETED,
      lxSTRIPF,
      lxTRANSFORM: begin
        Result.ExpressionType := etFunction;
        Result.ExpressionSubType := Code;
      end;
    end;
  end;

begin
  FCurrentBlock := Expression;
  TextLength := Length(Expression);
  FBlockSize := TextLength;

  InitParser;

  DecSeparator := '.';
  OmitBlank := True;
  OmitComment := True;

  Result := TDBFExpression.Create;
  Result.Text := Expression;
  Result.ExpressionType := etRoot;

  CurrentGroup := Result;
  Result.Stage := psNone;

  repeat
    CurrentExpression := GetExpression;
    if CurrentExpression = nil then
      Break;

    CurrentExpression.Parent := CurrentGroup;
    case CurrentGroup.Stage of
      psNone,
      psLeft: begin
        CurrentGroup.Left := CurrentExpression;
        CurrentGroup.Stage := psRight;

        if (CurrentExpression.ExpressionType = etOperation) and (CurrentExpression.ExpressionSubType = lxDash) then begin
          CurrentExpression.ExpressionSubType := lxAsterisk;

          CurrentExpression.Left := TDBFExpression.Create;
          CurrentExpression.Left.Parent := CurrentExpression;
          CurrentExpression.Left.ExpressionType := etConstant;
          CurrentExpression.Left.Value := -1;

          CurrentGroup := CurrentExpression;
          CurrentGroup.Stage := psRight;
          Continue;
        end;
      end;
      psRight: begin
        case CurrentGroup.ExpressionType of
          etRoot,
          etGroup: if not (CurrentExpression.ExpressionType in [etGroup, etGroupEnd, etOperation]) then
            raise Exception.Create('Invalid expression');
          etOperation: begin
            if (CurrentExpression.ExpressionType = etOperation) and (CurrentGroup.ExpressionSubType in [lxMore, lxLess]) and (CurrentExpression.ExpressionSubType in [lxEqual, lxMore]) then begin
              if CurrentExpression.ExpressionSubType = lxEqual then begin
                if CurrentGroup.ExpressionSubType = lxMore then
                  CurrentGroup.ExpressionSubType := lxMoreEqual
                else
                  CurrentGroup.ExpressionSubType := lxLessEqual;
              end
              else if CurrentGroup.ExpressionSubType = lxLess then
                CurrentGroup.ExpressionSubType := lxNotEqual
              else begin
                FreeAndNil(CurrentExpression);
                raise Exception.Create('Invalid expression');
              end;

              FreeAndNil(CurrentExpression);
              Continue;
            end;

            CurrentGroup.Right := CurrentExpression;
            CurrentGroup.Stage := psComplete;

            if (CurrentExpression.ExpressionType = etOperation) and (CurrentExpression.ExpressionSubType = lxDash) then begin
              CurrentExpression.ExpressionSubType := lxAsterisk;

              CurrentExpression.Left := TDBFExpression.Create;
              CurrentExpression.Left.Parent := CurrentExpression;
              CurrentExpression.Left.ExpressionType := etConstant;
              CurrentExpression.Left.Value := -1;

              CurrentGroup := CurrentExpression;
              CurrentGroup.Stage := psRight;
              Continue;
            end;

            if CurrentExpression.ExpressionType <> etGroup then begin
              CurrentGroup := CurrentGroup.Parent;
              if not (CurrentGroup.Stage in [psFunction, psComplete]) then
                CurrentGroup.Stage := psRight;
            end;
          end;
        end;
      end;
      psComplete: begin
        repeat
          CurrentGroup := CurrentGroup.Parent;
        until CurrentGroup.Stage <> psComplete;
      end;
      psFunction: if not (CurrentExpression.ExpressionType in [etComma, etGroupEnd, etOperation]) then
        CurrentGroup.Arguments.Add(CurrentExpression);
    else
      Assert(False);
    end;

    case CurrentExpression.ExpressionType of
      etGroup: begin
        CurrentGroup := CurrentExpression;
        CurrentGroup.Stage := psLeft;
      end;
      etGroupEnd: begin
        FreeAndNil(CurrentExpression);
        CurrentGroup := CurrentGroup.Parent;
      end;
      etComma: FreeAndNil(CurrentExpression);
      etField: begin
        s := UpperCase(string(CurrentExpression.Value));
        if Result.Fields.IndexOf(s) < 0 then
          Result.Fields.Add(s);
      end;
      etConstant: ;
      etOperation: begin
        if CurrentGroup.ExpressionType <> etFunction then begin
          CurrentExpression.Parent := CurrentGroup;
          CurrentExpression.Left := CurrentGroup.Left;
          CurrentExpression.Left.Parent := CurrentExpression;
          CurrentGroup.Left := CurrentExpression;
        end
        else begin
          n := CurrentGroup.Arguments.Count;
          if n = 0 then
            raise Exception.Create('No arguments for the operation');
          CurrentExpression.Left := CurrentGroup.Arguments[n - 1];
          CurrentExpression.Left.Parent := CurrentExpression;
          CurrentGroup.Arguments[n - 1] := CurrentExpression;
        end;
        CurrentGroup := CurrentExpression;
        CurrentGroup.Stage := psRight;
      end;
      etFunction: begin
        CurrentGroup := CurrentExpression;
        CurrentGroup.Stage := psFunction;

        CurrentExpression := GetExpression;
        if (CurrentExpression = nil) or (CurrentExpression.ExpressionType <> etGroup) then begin
          CurrentExpression.Free;
          raise Exception.Create('Invalid expression');
        end;
        CurrentExpression.Free;
      end;
    end;
  until False;

  Result.Fields.Sorted := True;
end;

{ TDBFExprParser }

procedure TDBFExprParser.InitParser;
begin
  inherited;

  FKeywordLexems := DBFExprKeywordLexems;

  DecSeparator := '.';
  OmitBlank := True;
  OmitComment := True;
end;

{ TDBFExpressionParserExt }

constructor TDBFExpressionParserExt.Create;
begin
  inherited Create;

  FExpression := Expression;
  FParser := TDBFExprParser.Create(FExpression);
end;

destructor TDBFExpressionParserExt.Destroy;
begin
  FParser.Free;

  inherited;
end;

function TDBFExpressionParserExt.VisitExpression(const Parent, Group: TDBFExpression): TDBFExpression;
var
  Code: integer;
  St: string;
  Tmp: TDBFExpression;
begin
  Result := nil;

  while True do begin
    if (Result <> nil) and Result.ForceBreak then begin
      Result.ForceBreak := False;
      Break;
    end;
    Code := FParser.GetNext(St);
    if Code = lcEnd then
      Exit;

      case Code of
        lxLeftBracket: begin
          Result := VisitGroup(Parent, Group);
        end;
        lxRightBracket: begin
          Tmp := Result;
          while (Tmp <> nil) and (Tmp <> Group) do begin
            Tmp.ForceBreak := True;
            Tmp := Tmp.Parent;
          end;
          if Group <> nil then
            Group.Closed := True;
          Break;
        end;
        lcIdent:
          Result := VisitField(Parent, Group, St);
        lcString,
        lcNumber:
          Result := VisitConstant(Parent, Group, Code, St);
        lxPlus,
        lxDash,
        lxAsterisk,
        lxSlash,
        lxEqual,
        lxMore,
        lxLess:
          Result := VisitOperation(Parent, Group, Result, Code, St);
        lxComma:
          Break;
        lxRECNO,
        lxABS,
        lxFLOOR,
        lxROUND,
        lxINT,
        lxSTR,
        lxCHR,
        lxVAL,
        lxLTRIM,
        lxALLTRIM,
        lxTRIM,
        lxLEN,
        lxLOWER,
        lxUPPER,
        lxLEFT,
        lxRIGHT,
        lxSUBSTR,
        lxAT,
        lxIIF,
        lxMIN,
        lxMAX,
        lxMOD,
        lxREPLICATE,
        lxSPACE,
        lxCEIL,
        lxSQRT,
        lxLOG,
        lxLOG10,
        lxASC,
        lxPROPER,
        lxISUPPER,
        lxISLOWER,
        lxISDIGIT,
        lxISALPHA,
        lxPADL,
        lxPADR,
        lxPADC,
        lxDTOC,
        lxCTOD,
        lxDTOS,
        lxTTOC,
        lxTTOD,
        lxEMPTY,
        lxSTRTRAN,
        lxCHRTRAN,
        lxDELETED,
        lxSTRIPF,
        lxTRANSFORM:
          Result := VisitFunction(Parent, Group, Code, St);
      end;
  end;
end;

function TDBFExpressionParserExt.VisitFunction(const Parent, Group: TDBFExpression; FunctionType: integer; const FunctionName: string): TDBFExpression;
var
  Arg: TDBFExpression;
  Code: integer;
  St: string;
begin
  Result := TDBFExpression.Create(Parent, etFunction);
  Result.ExpressionSubType := FunctionType;
  Result.Value := FunctionName;

  Code := FParser.GetNext(St);
  if (Code = lcEnd) or (Code <> lxLeftBracket) then begin
    FParser.Back;
    Result.Free;
    Result := VisitField(Parent, Group, FunctionName);
    Exit;
  end;

  repeat
    Arg := VisitExpression(Result, Result);

    if Arg = nil then
      Exit;

    Result.Arguments.Add(Arg);
    Arg.Parent := Result;
  until Result.Closed;
end;

function TDBFExpressionParserExt.VisitField(const Parent, Group: TDBFExpression; const FieldName: string): TDBFExpression;
var
  St, Name: string;
  Code: integer;
  Tmp: TDBFExpression;
begin
  Result := TDBFExpression.Create(Parent, etField);

  Name := FieldName;
  while True do begin
    Code := FParser.GetNext(St);

    if Code = lxLeftBracket then begin
      Result.Free;
      raise Exception.Create('Unknown function ' + FieldName);
    end;

    if ((Code = lcIdent) or (Code = lxPoint)) and not (Code in [lxPlus, lxDash, lxAsterisk, lxSlash, lxEqual, lxMore, lxLess]) then
      Name := Name + St
    else begin
      FParser.Back;
      Break;
    end;
  end;

  Result.Value := Name;

  Tmp := Result;
  while Tmp <> nil do begin
    Tmp.Fields.Add(UpperCase(Name));
    Tmp := Tmp.Parent;
  end;
end;

function TDBFExpressionParserExt.VisitOperation(const Parent, Group, LeftOperand: TDBFExpression; OperationType: integer; const OperationName: string): TDBFExpression;
begin
  Result := TDBFExpression.Create(Parent, etOperation);
  Result.ExpressionSubType := OperationType;
  Result.Value := OperationName;

  Result.Left := LeftOperand;
  LeftOperand.Parent := Result;
  Result.Right := VisitExpression(Result, Group);
end;

function TDBFExpressionParserExt.VisitConstant(const Parent, Group: TDBFExpression; ConstantType: integer; Value: string): TDBFExpression;
var
  n: integer;
begin
  Result := TDBFExpression.Create(Parent, etConstant);
  if ConstantType = lcString then
    Result.Value := Value
  else begin
    if TryStrToInt(Value, n) then
      Result.Value := n
    else begin
      Value := StringReplace(Value, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Value := StringReplace(Value, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
      Result.Value := StrToFloat(Value);
    end;
  end;
end;

function TDBFExpressionParserExt.VisitGroup(const Parent, Group: TDBFExpression): TDBFExpression;
var
  Arg: TDBFExpression;
begin
  Result := TDBFExpression.Create(Parent, etGroup);

  Arg := VisitExpression(Result, Result);

  if Arg = nil then
    Exit;

  Result.Arguments.Add(Arg);
  Arg.Parent := Result;
end;

class function TDBFExpressionParserExt.Parse(const Expression: string): TDBFExpression;
var
  Parser: TDBFExpressionParserExt;
begin
  Parser := TDBFExpressionParserExt.Create(Expression);
  try
    Result := Parser.VisitExpression(nil, nil);
    Result.Parent := nil;
  finally
    Parser.Free;
  end;
end;

{ TDBFExpressionVisitor }

function TDBFExpressionVisitor.VisitExpression(const Expression: TDBFExpression): TDBFExpression;
begin
  case Expression.ExpressionType of
    etRoot,
    etGroup: Result := VisitGroup(Expression);
    etField: Result := VisitField(Expression);
    etConstant: Result := VisitConstant(Expression);
    etOperation: Result := VisitOperation(Expression);
    etFunction: Result := VisitFunction(Expression);
  else
    raise Exception.Create('Invalid expression');
  end;
end;

function TDBFExpressionVisitor.VisitGroup(const Expression: TDBFExpression): TDBFExpression;
var
  i: integer;
begin
  for i := 0 to Expression.Arguments.Count - 1 do
    Expression.Arguments[i] := VisitExpression(Expression.Arguments[i]);
  Result := Expression;
end;

function TDBFExpressionVisitor.VisitField(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := Expression;
end;

function TDBFExpressionVisitor.VisitConstant(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := Expression;
end;

function TDBFExpressionVisitor.VisitOperation(const Expression: TDBFExpression): TDBFExpression;
begin
  Expression.Left := VisitExpression(Expression.Left);
  Expression.Right := VisitExpression(Expression.Right);
  Result := Expression;
end;

function TDBFExpressionVisitor.VisitFunction(const Expression: TDBFExpression): TDBFExpression;
var
  i: integer;
begin
  for i := 0 to Expression.Arguments.Count - 1 do
    Expression.Arguments[i] := VisitExpression(Expression.Arguments[i]);
  Result := Expression;
end;

{ TDBFExpressionNormalizer }

function TDBFExpressionNormalizer.VisitOperation(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := inherited VisitOperation(Expression);

  if (Expression.ExpressionSubType in [lxAsterisk, lxSlash]) and
     ((Expression.Left.ExpressionType = etOperation) or (Expression.Right.ExpressionType = etOperation))
  then if (Expression.Left.ExpressionType = etOperation) and
     (Expression.Left.ExpressionSubType in [lxPlus, lxDash])
  then begin
    Result := Expression.Left;
    Result.Parent := Expression.Parent;

    Expression.Left := Result.Right;
    Expression.Left.Parent := Expression;

    Result.Right := Expression;
    Expression.Parent := Result;
  end
  else if (Expression.Right.ExpressionType = etOperation) and
     (Expression.Right.ExpressionSubType in [lxPlus, lxDash])
  then begin
    Result := Expression.Right;
    Result.Parent := Expression.Parent;

    Expression.Right := Result.Left;
    Expression.Right.Parent := Expression;

    Result.Left := Expression;
    Expression.Parent := Result;
  end;
end;

{$IFDEF DBFENGINE}

{ TDBFExpressionCalculator }

function TDBFExpressionCalculator.DetectResultType(const LeftValue, RightValue: variant): TVarType;
begin
  if VarIsStr(LeftValue) or VarIsStr(RightValue) then
    Result := varString
  else if VarIsFloat(LeftValue) or VarIsFloat(RightValue) then
    Result := varDouble
  else if VarIsOrdinal(LeftValue) and VarIsOrdinal(RightValue) then
    Result := varInteger
  else
    Result := varString;
end;

function TDBFExpressionCalculator.VarTypeToDbfType(const Value: variant): AnsiChar;
begin
  Result := {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF};

  if VarIsStr(Value) then
    Result := DBF_TYPE_CHAR
  else if VarIsNumeric(Value) or VarIsDate(Value) then
    Result := DBF_TYPE_NUMERIC;
end;

function TDBFExpressionCalculator.VisitGroup(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := inherited VisitGroup(Expression);
  if Result.Arguments.Count > 0 then
    Result.Result := TDBFExpression(Result.Arguments[0]).Result;
end;

function TDBFExpressionCalculator.VisitField(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := inherited VisitField(Expression);

  if Assigned(FOnGetValue) then
    Result.Result := FOnGetValue(FValue, FFieldNo, Result.Value);
end;

function TDBFExpressionCalculator.VisitConstant(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := inherited VisitConstant(Expression);
  Result.Result := Result.Value;
  Result.ResultType := VarTypeToDbfType(Result.Result);
end;

function TDBFExpressionCalculator.VisitOperation(const Expression: TDBFExpression): TDBFExpression;
var
  ResultType: TVarType;
  LeftValue, RightValue: variant;
begin
  Result := inherited VisitOperation(Expression);
  Result.Result := null;

  if VarIsNull(Result.Left.Result) or VarIsNull(Result.Right.Result) then
    Exit;

  ResultType := DetectResultType(Result.Left.Result, Result.Right.Result);
  case Result.ExpressionSubType of
    lxPlus: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue + RightValue
      else
        raise Exception.Create('Invalid arguments for addition');
    end;
    lxDash: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue - RightValue
      else
        raise Exception.Create('Invalid arguments for substraction');
    end;
    lxAsterisk: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue * RightValue
      else
        raise Exception.Create('Invalid arguments for multiplication');
    end;
    lxSlash: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue / RightValue
      else
        raise Exception.Create('Invalid arguments for division');
    end;
    lxEqual: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue = RightValue
      else
        raise Exception.Create('Invalid arguments for comparison');
    end;
    lxNotEqual: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue <> RightValue
      else
        raise Exception.Create('Invalid arguments for comparison');
    end;
    lxMoreEqual: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue >= RightValue
      else
        raise Exception.Create('Invalid arguments for comparison');
    end;
    lxLessEqual: begin
      if CastValue(Result.Left.Result, ResultType, LeftValue)
      and CastValue(Result.Right.Result, ResultType, RightValue)
      then
        Result.Result := LeftValue <= RightValue
      else
        raise Exception.Create('Invalid arguments for comparison');
    end;
  end;
end;

function TDBFExpressionCalculator.VisitFunction(const Expression: TDBFExpression): TDBFExpression;
var
  FunctionInfo: TDBFFunctionInfo;
  Args: TVariantArray;
  i: integer;
begin
  Result := inherited VisitFunction(Expression);
  Result.Result := null;

  SetLength(Args, Result.Arguments.Count);
  try
    for i := 0 to Result.Arguments.Count - 1 do
      Args[i] := TDBFExpression(Result.Arguments[i]).Result;
    FunctionInfo := DBFFunctions.Find(VarToStr(Result.Value), Args);
    Assert(FunctionInfo <> nil);

    DBFFunctions.Execute(FunctionInfo, Args, Result.Result);
  finally
    SetLength(Args, 0);
  end;
end;

function TDBFExpressionCalculator.VisitExpression(const Expression: TDBFExpression): TDBFExpression;
begin
  Result := inherited VisitExpression(Expression);
  Result.ResultType := VarTypeToDbfType(Result.Result);
end;

function TDBFExpressionCalculator.CalculateExpression(const Sender: TObject; const Expression: TDBFExpression; const Value: TVirtualValue; const FieldNo: integer; const OnGetValueFunction: TOnGetDBFValue): TDBFExpression;
begin
  FSender := Sender;
  FValue := Value;
  FFieldNo := FieldNo;
  FOnGetValue := OnGetValueFunction;

  Result := VisitExpression(Expression);
end;

{$ENDIF}

initialization
  DBFKeywordLexems := TLexemList.Create;
  DBFKeywordLexems.Assign(SQLKeywordLexems);

  DBFKeywordLexems.Add('CREATE',    lxCREATE);
  DBFKeywordLexems.Add('DROP',      lxDROP);
  DBFKeywordLexems.Add('TABLE',     lxTABLE);
  DBFKeywordLexems.Add('PACK',      lxPACK);
  DBFKeywordLexems.Add('MEMO',      lxMEMO);
  DBFKeywordLexems.Add('DBF',       lxDBF);
  DBFKeywordLexems.Add('ZAP',       lxZAP);
  DBFKeywordLexems.Add('REINDEX',   lxREINDEX);
  DBFKeywordLexems.Add('ALTER',     lxALTER);
  DBFKeywordLexems.Add('ADD',       lxADD);
  DBFKeywordLexems.Add('COLUMN',    lxCOLUMN);
  DBFKeywordLexems.Add('NOT',       lxNOT);
  DBFKeywordLexems.Add('NULL',      lxNULL);
  DBFKeywordLexems.Add('PRIMARY',   lxPRIMARY);
  DBFKeywordLexems.Add('UNIQUE',    lxUNIQUE);
  //DBFKeywordLexems.Add('KEY',     lxKEY);
  DBFKeywordLexems.Add('DEFAULT',   lxDEFAULT);
  //DBFKeywordLexems.Add('AUTOINC', lxAUTOINC);
  DBFKeywordLexems.Add('NEXTVALUE', lxNEXTVALUE);
  DBFKeywordLexems.Add('STEP',      lxSTEP);
  DBFKeywordLexems.Add('INDEX',     lxINDEX);
  DBFKeywordLexems.Add('ON',        lxON);
  DBFKeywordLexems.Add('FOREIGN',   lxFOREIGN);
  DBFKeywordLexems.Add('DELETE',    lxDELETE);
  DBFKeywordLexems.Add('CHECK',     lxCHECK);
  DBFKeywordLexems.Add('IN',        lxIN);
  DBFKeywordLexems.Add('COLLATE',   lxCOLLATE);
  DBFKeywordLexems.Add('CASE',      lxCASE);
  DBFKeywordLexems.Add('EXCEPT',    lxEXCEPT);
  DBFKeywordLexems.Sort;

  DBFExprKeywordLexems := TLexemList.Create;

  DBFExprKeywordLexems.Add('ABS',       lxABS);
  DBFExprKeywordLexems.Add('ALLTRIM',   lxALLTRIM);
  DBFExprKeywordLexems.Add('CHR',       lxCHR);
  DBFExprKeywordLexems.Add('ASC',       lxASC);
  DBFExprKeywordLexems.Add('INT',       lxINT);
  DBFExprKeywordLexems.Add('FLOOR',     lxFLOOR);
  DBFExprKeywordLexems.Add('LEFT',      lxLEFT);
  DBFExprKeywordLexems.Add('LOWER',     lxLOWER);
  DBFExprKeywordLexems.Add('LTRIM',     lxLTRIM);
  DBFExprKeywordLexems.Add('RIGHT',     lxRIGHT);
  DBFExprKeywordLexems.Add('ROUND',     lxROUND);
  DBFExprKeywordLexems.Add('STR',       lxSTR);
  DBFExprKeywordLexems.Add('SUBSTR',    lxSUBSTR);
  DBFExprKeywordLexems.Add('TRIM',      lxTRIM);
  DBFExprKeywordLexems.Add('UPPER',     lxUPPER);
  DBFExprKeywordLexems.Add('VAL',       lxVAL);
  DBFExprKeywordLexems.Add('AT',        lxAT);
  DBFExprKeywordLexems.Add('LEN',       lxLEN);
  DBFExprKeywordLexems.Add('IIF',       lxIIF);
  DBFExprKeywordLexems.Add('MIN',       lxMIN);
  DBFExprKeywordLexems.Add('MAX',       lxMAX);
  DBFExprKeywordLexems.Add('MOD',       lxMOD);
  DBFExprKeywordLexems.Add('RECNO',     lxRECNO);
  DBFExprKeywordLexems.Add('REPLICATE', lxREPLICATE);
  DBFExprKeywordLexems.Add('SPACE',     lxSPACE);
  DBFExprKeywordLexems.Add('SQRT',      lxSQRT);
  DBFExprKeywordLexems.Add('LOG',       lxLOG);
  DBFExprKeywordLexems.Add('LOG10',     lxLOG10);
  DBFExprKeywordLexems.Add('CEIL',      lxCEIL);
  DBFExprKeywordLexems.Add('PROPER',    lxPROPER);
  DBFExprKeywordLexems.Add('ISUPPER',   lxISUPPER);
  DBFExprKeywordLexems.Add('ISLOWER',   lxISLOWER);
  DBFExprKeywordLexems.Add('ISDIGIT',   lxISDIGIT);
  DBFExprKeywordLexems.Add('ISALPHA',   lxISALPHA);
  DBFExprKeywordLexems.Add('PADL',      lxPADL);
  DBFExprKeywordLexems.Add('PADR',      lxPADR);
  DBFExprKeywordLexems.Add('PADC',      lxPADC);
  DBFExprKeywordLexems.Add('DTOC',      lxDTOC);
  DBFExprKeywordLexems.Add('CTOD',      lxCTOD);
  DBFExprKeywordLexems.Add('DTOS',      lxDTOS);
  DBFExprKeywordLexems.Add('TTOC',      lxTTOC);
  DBFExprKeywordLexems.Add('TTOD',      lxTTOD);
  DBFExprKeywordLexems.Add('EMPTY',     lxEMPTY);
  DBFExprKeywordLexems.Add('STRTRAN',   lxSTRTRAN);
  DBFExprKeywordLexems.Add('CHRTRAN',   lxCHRTRAN);
  DBFExprKeywordLexems.Add('DELETED',   lxDELETED);
  DBFExprKeywordLexems.Add('STRIPF',    lxSTRIPF);
  DBFExprKeywordLexems.Add('TRANSFORM', lxTRANSFORM);
  DBFExprKeywordLexems.Sort;

finalization
  DBFKeywordLexems.Free;
  DBFExprKeywordLexems.Free;

end.
