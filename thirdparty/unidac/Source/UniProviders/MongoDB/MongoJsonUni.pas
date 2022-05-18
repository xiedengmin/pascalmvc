
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoJsonUni;

interface

uses
  SysUtils,
  CRParser,
  CRJson;

type
  TCommandReader = class(TJSONTextReader)
  protected
    function InternalReadString(const Unescape: boolean): string; override;
    procedure InternalReadLexem; override;
  public
    procedure Initialize; override;

    function ReadTag: TJSONTag; override;
  end;

  TCommandWriter = class(TJSONTextWriter)
  protected
    procedure InternalWriteAsString(const Value: string; const Escape: boolean); override;
  end;

  TCommandSerializer = class(TJSONSerializer)
  protected
    class function GetTextWriterClass: TJSONWriterClass; override;
  end;

  TCommandDeserializer = class(TJSONDeserializer)
  protected
    class function GetTextReaderClass: TJSONReaderClass; override;

    function ProcessPair(const Parent: TJSONObject; const Tag: TJSONTag): TJSONPair; override;
    function ProcessRegex(const Parent: TJSONValue): TJSONRegex; override;
  end;

implementation

uses
  CLRClasses;

type
  TInternalParser = class(TJSONParser);

function IsAltQuoted(const Value: string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result := (Value[1] = '''') and (Value[l] = '''');
end;

function AltUnQuote(const Value: string): string;
begin
  if IsAltQuoted(Value) then
    Result := Copy(Value, 2, length(Value) - 2)
  else
    Result := Value;
end;

function IsRegexPattern(const Value: string): boolean;
begin
  Result := (Length(Value) > 0) and (Value[1] = '/');
end;

procedure SplitRegex(const Value: string; out Pattern, Options: string);
var
  Builder: StringBuilder;
  i, n: integer;
  c: char;
begin
  n := Length(Value);
  Builder := StringBuilder.Create(n);

  try
    i := 2;

    while i <= n do begin
      c := Value[i];
      if c = '/' then begin
        if Value[i - 1] = '\' then
          Builder.Append(c)
        else begin
          Inc(i);
          Break;
        end;
      end
      else
        Builder.Append(c);

      Inc(i);
    end;

    Pattern := Builder.ToString;
    Options := Copy(Value, i, n - i + 1);
  finally
    Builder.Free;
  end;
end;

{ TCommandReader }

function TCommandReader.InternalReadString(const Unescape: boolean): string;
var
  PrevCode: integer;
begin
  Result := '';
  Code := FParser.GetNext(Lexem);
  if Code = lcEnd then
    Exit;

  if IsQuoted(Lexem) then
    Result := UnQuote(Lexem)
  else if IsAltQuoted(Lexem) then
    Result := AltUnQuote(Lexem)
  else if Code = lxSlash then begin
    Result := Lexem;
    PrevCode := lcEnd;

    repeat
      Code := FParser.GetNext(Lexem);
      if Code = lcEnd then
        Break;

      Result := Result + Lexem;

      if (Code = lxSlash) and (PrevCode <> lxBackSlash) then begin
        Code := FParser.GetNext(Lexem);
        if Code = lcEnd then
          Exit;

        if (Code = lxComma) or (Code = lxColon) or (Lexem = '}') or (Lexem = ']') then begin
          FParser.Back;
          Break;
        end;

        Result := Result + Lexem;

        Break;
      end;

      PrevCode := Code;
    until Code = lcEnd;

    Code := lcString;
  end
  else begin
    Result := Lexem;

    repeat
      Code := FParser.GetNext(Lexem);
      if Code = lcEnd then
        Break;

      if (Code = lxComma) or (Code = lxColon) or (Lexem = '}') or (Lexem = ']') then begin
        FParser.Back;
        Break;
      end;

      Result := Result + Lexem;
    until Code = lcEnd;
  end;

  if Unescape and not IsRegexPattern(Result) then
    Result := InternalUnescape(Result);
end;

procedure TCommandReader.InternalReadLexem;
var
  Lex: string;
  SavedPos: integer;
begin
  inherited;

  if Code = lcEnd then
    Exit;

  if Code = lxDollar then begin
    SavedPos := TInternalParser(FParser).OldPos;
    Code := FParser.GetNext(Lex);

    if Code = lcEnd then
      Exit;

    Lexem := Lexem + Lex;
    Code := lcString;
    TInternalParser(FParser).OldPos := SavedPos;
  end;
end;

procedure TCommandReader.Initialize;
begin
  inherited;
end;

function TCommandReader.ReadTag: TJSONTag;
begin
  Result := inherited ReadTag;

  if Result = jtNone then begin
    FParser.Back;

    Code := FParser.GetNext(Lexem);

    if (Code = lxSlash) or ((Code = lcString) and IsAltQuoted(Lexem)) then
      Result := jtString;

    FParser.Back;
  end;
end;

{ TCommandWriter }

procedure TCommandWriter.InternalWriteAsString(const Value: string; const Escape: boolean);
begin
  if IsRegexPattern(Value) then
    inherited InternalWriteAsString(Value, False)
  else
    inherited;
end;

{ TCommandSerializer }

class function TCommandSerializer.GetTextWriterClass: TJSONWriterClass;
begin
  Result := TCommandWriter;
end;

{ TCommandDeserializer }

class function TCommandDeserializer.GetTextReaderClass: TJSONReaderClass;
begin
  Result := TCommandReader;
end;

function TCommandDeserializer.ProcessPair(const Parent: TJSONObject; const Tag: TJSONTag): TJSONPair;
var
  Pattern, Options: string;
begin
  Result := inherited ProcessPair(Parent, Tag);

  if (Result.Value.Tag = jtString) and (IsRegexPattern(Result.Value.AsString)) then begin
    SplitRegex(Result.Value.AsString, Pattern, Options);

    Result.Value{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
    Result.Value := TJSONRegex.Create(Result);

    if FUseUnicode then begin
      TJSONRegex(Result.Value).Pattern := TJSONAnsiString.Create(Result.Value);
      TJSONAnsiString(TJSONRegex(Result.Value).Pattern).AsAnsiString := {$IFNDEF NEXTGEN}AnsiString{$ENDIF}(Pattern);

      TJSONRegex(Result.Value).Options := TJSONAnsiString.Create(Result.Value);
      TJSONAnsiString(TJSONRegex(Result.Value).Options).AsAnsiString := {$IFNDEF NEXTGEN}AnsiString{$ENDIF}(Options);
    end
    else begin
      TJSONRegex(Result.Value).Pattern := TJSONWideString.Create(Result.Value);
      TJSONWideString(TJSONRegex(Result.Value).Pattern).AsWideString := {$IFNDEF NEXTGEN}WideString{$ENDIF}(Pattern);

      TJSONRegex(Result.Value).Options := TJSONWideString.Create(Result.Value);
      TJSONWideString(TJSONRegex(Result.Value).Options).AsWideString := {$IFNDEF NEXTGEN}WideString{$ENDIF}(Options);
    end;
  end;
end;

function TCommandDeserializer.ProcessRegex(const Parent: TJSONValue): TJSONRegex;
var
  Pattern, Options: string;
begin
  Result := TJSONRegex.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise Exception.Create(cInvalidRegex);

    Result.Pattern := ProcessCString(Result, True);
    if IsRegexPattern(Result.Pattern.AsString) then begin
      SplitRegex(Result.Pattern.AsString, Pattern, Options);
      if Result.Pattern is TJSONAnsiString then
        TJSONAnsiString(Result.Pattern).AsAnsiString := {$IFNDEF NEXTGEN}AnsiString{$ENDIF}(Pattern)
      else
        TJSONWideString(Result.Pattern).AsWideString := {$IFNDEF NEXTGEN}WideString{$ENDIF}(Pattern);
    end;

    if FReader.ReadElementSeparator then begin
      FReader.ReadRegexOptionsBegin;
      Result.Options := ProcessCString(Result, True);
      FReader.ReadRegexOptionsEnd;
    end
    else begin
      if FUseUnicode then begin
        TJSONRegex(Result).Options := TJSONAnsiString.Create(Result);
        TJSONAnsiString(TJSONRegex(Result).Options).AsAnsiString := {$IFNDEF NEXTGEN}AnsiString{$ENDIF}(Options);
      end
      else begin
        TJSONRegex(Result).Options := TJSONWideString.Create(Result);
        TJSONWideString(TJSONRegex(Result).Options).AsWideString := {$IFNDEF NEXTGEN}WideString{$ENDIF}(Options);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
