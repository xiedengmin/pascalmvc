unit MVC.Tool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, base64, Interfaces;

type
  ITool = interface
    function PathFmt(path: string): string;
    function UrlFmt(url: string): string;
    function GetGUID: string;
    function StringFormat(Asrc: string): string;
    function Unicode(Asrc: string): string;
    function UnicodeEncode(Asrc: string): string;
    function UnicodeDecode(Asrc: string): string;
    function BitmapToString(img: TBitmap): string;
    function getVCode(out num: string): string; //返回图片的base64编码
    function IsHZ(ch: widechar): boolean;

    function StringToBitmap(imgStr: string): TBitmap;
    function StringFormatF(Asrc: string): string;
    function NumToImage(num: string): string;
  end;

  { TTool }

  TTool = class(TInterfacedObject, ITool)
  private

  public
    function PathFmt(path: string): string;
    function UrlFmt(url: string): string;
    function GetGUID: string;
    function StringFormat(Asrc: string): string;

    function NumToImage(num: string): string;
    function BitmapToString(img: TBitmap): string;
    function getVCode(out num: string): string;
    function IsHZ(ch: widechar): boolean;
    function UnicodeEncode(Asrc: string): string;
    function UnicodeDecode(Asrc: string): string;
    function Unicode(Asrc: string): string;
    function StringToBitmap(imgStr: string): TBitmap;
    function StringFormatF(Asrc: string): string;
  end;

function IITool: ITool;
function IsUtf8Format(AnsiStr: ansistring): boolean;
function forceutf8(unknown: string): string;
function loadtxt(path: string): string;
  function GetContentFromFile(fileName: string; var errMsg: string;
  var strLst: TStringList): boolean;
implementation

function IITool: ITool;
var
  tool: ITool;
begin
  tool := TTool.Create;
  Result := tool;
end;

function TTool.getVCode(out num: string): string;
var
  code: string;
  i: integer;
const
  str = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
begin
  for i := 0 to 3 do
  begin
    code := code + Copy(str, Random(Length(str)), 1);
  end;
  num := code;
  Result := NumToImage(code);
  //result:=stringtobitmap(code);
end;

function TTool.NumToImage(num: string): string;
var
  bmp_t: Graphics.TBitmap;
  i: integer;
  s: string;
begin
  bmp_t := TBitmap.Create;
  try
    bmp_t.SetSize(90, 35);
    bmp_t.Transparent := True;
    for i := 1 to length(num) do
    begin
      s := num[i];
      bmp_t.Canvas.Rectangle(0, 0, 90, 35);
      bmp_t.Canvas.Pen.Style := psClear;
      bmp_t.Canvas.Brush.Style := bsClear;
      bmp_t.Canvas.Font.Color := Random(256) and $C0; // 新建个水印字体颜色
      //      bmp_t.Canvas.Font.Size := Random(6) + 11;
      bmp_t.Canvas.Font.Height := Random(5) + 24; //高分屏显示不全
      bmp_t.Canvas.Font.Style := [fsBold];
      bmp_t.Canvas.Font.Name := 'Verdana';
      bmp_t.Canvas.TextOut(i * 15, 5, s); // 加入文字
    end;
    s := IITool.BitmapToString(bmp_t);
    Result := s;
  finally
    FreeAndNil(bmp_t);
  end;
end;

function TTool.StringFormat(Asrc: string): string;
var
  s: string;
begin
  s := Asrc.Replace(#7, '\a').Replace(#8, '\b').Replace(#12, '\f');
  s := s.Replace(#9, '\t').Replace(#11, '\v').Replace(#92, '\\');
  s := s.Replace(#39, '''').Replace(#34, '\"').Replace(#63, '\?');
  s := s.Replace(#13, '\\r').Replace(#10, '\\n');
  Result := s;

end;


function TTool.StringToBitmap(imgStr: string): TBitmap;
    {var
  ss: TStringStream;
  ms: TMemoryStream;
  bitmap: TBitmap;
begin
  ss := TStringStream.Create(imgStr);
  ms := TMemoryStream.Create;
  DecodeStream(ss, ms); //将base64字符流还原为内存流
  ms.Position := 0;
  bitmap := TBitmap.Create;
  bitmap.LoadFromStream(ms);
  ss.Free;
  ms.Free;
  result := bitmap;
end; }
var
  ss: TStringStream;
  ms: TMemoryStream;
  bitmap: TBitmap;
  b64: Tbase64encodingstream;
  TempStr: string;
begin
  try

    ss := TStringStream.Create(imgStr);
    ms := TMemoryStream.Create;

    // DecodeStream(ss, ms); //将base64字符流还原为内存流
    // ms.Position := 0;


    B64 := TBase64EncodingStream.Create(ss);
    //    B64.WriteBuffer(s, s.Size);
    //   SetLength(TempStr, s.Size);
    //    s.Read(tempStr[1], s.Size);
    //   ccount := s.Size;
    //    B64.Write(tempStr[1], ccount);
    bitmap := TBitmap.Create;
    bitmap.LoadFromStream(b64);
  finally
    B64.Free;
    ss.Free;
    ms.Free;
  end;

  Result := bitmap;

end;
///将Bitmap位图转化为base64字符串

function TTool.BitmapToString(img: TBitmap): string;
var
  B64: TBase64EncodingStream;
  ms: TMemoryStream;
  ss: TStringStream;
  s: string;
  TempStr: string;
  ccount: integer;
begin
  ms := TMemoryStream.Create;
  img.SaveToStream(ms);
  ss := TStringStream.Create('');
  ms.Position := 0;
  // EncodeStream(ms, ss); //将内存流编码为base64字符流
  try
    B64 := TBase64EncodingStream.Create(ss);
    SetLength(TempStr, ms.Size);
    ms.Read(tempStr[1], ms.Size);
    ccount := ms.Size;
    B64.WriteBuffer(tempStr[1], ms.Size);
    s := ss.DataString;
  finally
    B64.Free;
  end;
  ms.Free;
  ss.Free;
  Result := s;
end;



function TTool.Unicode(Asrc: string): string;
var
  w: word;
  hz: WideString;
  i: integer;
  s: string;
begin

  hz := Asrc;

  for i := 1 to Length(hz) do
  begin
    w := Ord(hz[i]);
    s := s + '\u' + IntToHex(w, 4);
  end;
  Result := LowerCase(s);
end;

function TTool.UnicodeEncode(Asrc: string): string;
var
  w: word;
  hz: WideString;
  i: integer;
  s: string;
begin

  hz := StringFormat(Asrc);

  for i := 1 to Length(hz) do
  begin
    if IsHZ(hz[i]) then
    begin
      w := Ord(hz[i]);
      s := s + '\u' + IntToHex(w, 4);
    end
    else
      s := s + hz[i];
  end;
  Result := s;
end;


function TTool.UnicodeDecode(Asrc: string): string;
var
  index: integer;
  temp, top, last: string;
begin
  index := 1;
  while index >= 0 do
  begin
    index := Pos('\u', Asrc) - 1;
    if index < 0 then         //非 unicode编码不转换 ,自动过滤
    begin
      last := Asrc;
      Result := Result + last;
      Exit;
    end;
    top := Copy(Asrc, 1, index);
    // 取出 编码字符前的 非 unic 编码的字符，如数字
    temp := temp + Copy(Asrc, index + 1, 6); // 取出编码，包括 \u,如\u4e3f
    Delete(temp, 1, 2);
    Delete(Asrc, 1, index + 6);

  end;
  Result := Result + top + widechar(StrToInt('$' + temp));
end;
//判断是否是UTF编码格式
//-----------------------------------
//Delphi 判断AnsiStr是否是UTF编码格式 IsUtf8Format
//https://blog.51cto.com/u_15127634/4024794
function IsUtf8Format(AnsiStr: ansistring): boolean;
var
  I, iCount, chr: integer;
  c: ansichar;
  nBytes: integer; // UFT-8可用1-6个字节编码,ASCII用一个字节
  bAllAscii: boolean; // 如果全部都是ASCII, 说明不是UTF-8
begin
  Result := False;
  nBytes := 0;
  bAllAscii := True;
  iCount := Length(AnsiStr);
  for I := 1 to iCount do
  begin
    c := AnsiStr[I];
    chr := Ord(c);
    // 判断是否ASCII编码,如果不是,说明有可能是UTF-8,ASCII用7位编码,但用一个字节存,最高位标记为0,o0xxxxxxx;中文ASCII编码可能最高位为1
    if (chr and $80) <> 0 then
      bAllAscii := False;
    // 如果不是ASCII码,应该是多字节符,计算字节数
    if nBytes = 0 then
    begin
      if chr > $80 then
      begin
        if (chr >= $fc) and (chr <= $fd) then // 1111 1100 and 1111 1101
          nBytes := 6
        else if chr >= $f8 then // 1111 1000
          nBytes := 5
        else if chr >= $f0 then // 1111 0000
          nBytes := 4
        else if chr >= $e0 then // 1110 0000
          nBytes := 3
        else if chr >= $c0 then // 1100 0000
          nBytes := 2
        else
          Exit;
        Dec(nBytes);
      end;
    end
    else // 多字节符的非首字节,应为 10xxxxxx
    begin
      if (chr and $c0) <> $80 then Exit;
      Dec(nBytes);
    end;
  end;
  // 违返规则
  if nBytes > 0 then
    Exit;
  // 如果全部都是ASCII, 说明不是 UTF-8
  if bAllAscii then
    Exit;

  Result := True;
end;

function forceutf8(unknown: string): string;
begin
     {$IFDEF UNIX}
         result := unknown;
     {$else}
  Result := Ansitoutf8(unknown);
     {$ENDIF}
end;

//判断字符是否是汉字
function TTool.IsHZ(ch: widechar): boolean;
var
  i: integer;
begin
  i := Ord(ch);
  if (i < 19968) or (i > 40869) then
    Result := False
  else
    Result := True;
end;

function IsNumberic(Vaule: string): boolean;   //判断Vaule是不是数字
var
  i: integer;
begin
  Result := True;   //设置返回值为是（真）
  Vaule := trim(Vaule); //去空格
  for i := 1 to length(Vaule) do //准备循环
  begin
    if not (Vaule[i] in ['0'..'9']) then
      //如果Vaule的第i个字不是0-9中的任一个
    begin
      Result := False; //返回值 不是（假）
      exit; //退出函数
    end;
  end;
end;



function TTool.UrlFmt(url: string): string;
var
  ret: string;
begin
  ret := url.Replace('\\', '/').Replace('//', '/').Replace('\', '/');
  Result := ret;
end;



function TTool.StringFormatF(Asrc: string): string;
var
  s: string;
begin
  s := Asrc;
  s := s.Replace(#92, '\\');
  Result := s;

end;


function TTool.GetGUID: string;
var
  LTep: TGUID;
  sGUID: string;
begin
  CreateGUID(LTep);
  sGUID := GUIDToString(LTep);
  sGUID := StringReplace(sGUID, '-', '', [rfReplaceAll]);
  sGUID := Copy(sGUID, 2, Length(sGUID) - 2);
  Result := sGUID;
end;

function TTool.PathFmt(path: string): string;
var
  ret: string;
begin
  {$IFDEF MSWINDOWS}
  ret := path.Replace('\\', '\').Replace('//', '\').Replace('/', '\');
  {$ELSE}
  ret := path.Replace('\\', '/').Replace('//', '/').Replace('\', '/');
  {$ENDIF}
  Result := ret;
end;

function loadtxt(path: string): string;
var
  s, sln: string;
var
  fs: TFileStream;
  str: string;
begin
  fs := TFileStream.Create(path, fmOpenRead);
  try
    SetLength(str, fs.size);
    fs.ReadBuffer((PChar(str))^, fs.size);
    Result := str;
  finally
    fs.Free;
  end;
end;

function GetContentFromFile(fileName: string; var errMsg: string;
  var strLst: TStringList): boolean;
var
  F: TextFile;
  i: integer;
  fileStr: string;
begin
  Result := False;
  strLst:=Tstringlist.Create;
  if FileExists(fileName) then
  begin
    AssignFile(F, fileName);  //把一个外部文件名和一个文件变量相关联
    try
      try
        Reset(F);   //为读打开文件并把文件指针移动到文件首
        while not EOF(F) do
        begin
          ReadLn(F, fileStr);  //从文本文件中读取信息
          Str := StringReplace(Str,   #13#10, '',   [rfReplaceAll]);
          strLst.Add(fileStr);
        end;
        Result := True;
      except
        on E: Exception do
        begin
          ErrMsg := E.Message;
        end;
      end;
    finally
      CloseFile(F);  //关闭文件
    end;
  end
  else
  begin
    ErrMsg := '文件不存在';
  end;
end;

end.
