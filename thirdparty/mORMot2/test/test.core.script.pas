/// regression tests for the framework Scripting abilities
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.script;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.lib.quickjs;

type
  /// this test case will validate several low-level protocols
  TTestCoreScript = class(TSynTestCase)
  published
    /// QuickJS low-level direct API tests
    procedure QuickJSLowLevel;
  end;

  
implementation


{ TTestCoreScript }

var
  output: RawUtf8;

function dolog(ctx: JSContext; this_val: JSValueRaw; argc: integer;
  argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
begin
  for i := 0 to argc - 1 do
    AddToCsv(ctx.ToUtf8(argv[i]), output);
  result := JS_UNDEFINED;
end;

procedure TTestCoreScript.QuickJSLowLevel;
var
  rt: JSRuntime;
  cx: JSContext;

  function Run(const js: RawUtf8): RawUtf8;
  var
    v: JSValue;
    res: variant;
  begin
    Check(cx <> nil, 'cx');
    v := cx.Eval(js, '', JS_EVAL_TYPE_GLOBAL, result);
    if result = '' then
    begin
      Check(cx.ToVariant(v, res), 'tovar');
      cx.ToUtf8(v, result);
      if v.IsString or
         v.IsInt32 then
        CheckEqual(VariantToUtf8(res), result, 'istring/isint32')
      else if v.IsFloat then
        CheckSame(VariantToDoubleDef(res), GetExtended(pointer(result)),
          DOUBLE_SAME, 'isfloat');
    end;
    cx.Free(v);
    //writeln(js,' = ',result);
  end;

var
  v, v2: RawUtf8;
  j: JSValue;
  i: integer;
  d: double;
  i64: Int64;
  va: variant;
begin
  rt := JS_NewRuntime;
  try
    // validate numbers/text to/from JSValue conversion
    cx := rt.New;
    try
      cx.SetFunction([], 'log', @dolog, 1);
      for i := -100 to 100 do
      begin
        // 32-bit integer
        j.From32(i * 777);
        Check(j.IsNumber);
        Check(not j.IsNull);
        Check(not j.IsRefCounted);
        Check(not j.IsFloat);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsInt32);
        CheckEqual(j.Int32, i * 777);
        Check(cx.ToVariantFree(j, va));
        Check(va = i * 777);
        cx.Free(j); // do-nothing
        // float
        d := i * 777777777.77;
        j.FromNum(d);
        Check(j.IsNumber);
        Check(not j.IsRefCounted);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsFloat = (i <> 0));
        if i = 0 then
          CheckEqual(j.Int32, 0)
        else
          CheckSame(j.F64, d);
        i64 := i * Int64(77777777777);
        cx.Free(j);
        // 32-bit and 64-bit integer
        j.From64(i64);
        Check(j.IsNumber);
        Check(not j.IsRefCounted);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsInt32 = (abs(i64) < maxInt));
        if j.IsInt32 then
          CheckEqual(j.Int32, i64)
        else
          Check(j.IsFloat);
        CheckEqual(j.Int64, i64);
        cx.Free(j);
        // UTF-8 text
        if i and 7 = 0 then
          v := Int32ToUtf8(i) // Ascii
        else
          v := RandomUtf8(i + 100); // UTF-8
        j := cx.From(v);
        Check(not j.IsNumber);
        Check(not j.IsNull);
        Check(not j.IsFloat);
        Check(j.IsString);
        Check(j.IsRefCounted);
        cx.ToUtf8(j, v2);
        CheckEqual(v, v2, v);
        cx.Free(j);
        Check(j.Equals(j));
      end;
      j.From(true);
      Check(not j.IsRefCounted);
      Check(j.Bool);
      j.From(false);
      Check(not j.IsRefCounted);
      Check(not J.Bool);
      for i := 1 to 1000 do
      begin
        // basic runtime execution
        CheckEqual(Run(
          '2+2'),
          '4');
        CheckEqual(Run(
          'a=1234567; a.toString()'),
          '1234567');
        CheckEqual(Run(
          'function add(x, y) { return x + y; } add(434,343)'),
          '777');
        CheckEqual(Run(
          'add(434.732,343.045)'),
          '777.777');
        CheckEqual(Run(
          'function fn(x,y) { return (Math.log(y) / Math.log(x)).toString(); }'#10 +
          'fn(5,625)'),
          '4'); // 5 x 5 x 5 x 5 = 625
        v := Run(
          'Date.now()');
        CheckUtf8(abs(UnixMSTimeUtcFast - round(GetExtended(pointer(v)))) < 100,
          'timestamp - may fail during slow debugging [%]', [v]);
{        v := Run('new Date().toString();');
        Check(PosEx(' ' +UInt32ToUtf8(CurrentYear), v) > 0); }
        v := Run(
          'console.log("Hello World");');
        Check(PosEx('''console'' is not defined', v) > 0);
        j := cx.Call('', 'add', [777, i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = i + 777);
        j := cx.Call('', 'add', ['777', i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = '777' + UInt32ToUtf8(i));
        j := cx.Call('', 'add', ['3', i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = '3' + UInt32ToUtf8(i));
        va := cx.CallVariant('', 'add', [777.777, i]);
        CheckSame(double(va), 777.777 + i);
        va := cx.CallVariant('', 'add', ['777', i]);
        Check(va = '777' + UInt32ToUtf8(i));
      end;
      CheckEqual(Run('JSON.stringify({ x: 5, y: 6 })'), '{"x":5,"y":6}');
      CheckEqual(Run('[3, 4, 5].map(x => x ** 10).forEach(x => log(x))'), 'undefined');
      CheckEqual(output, '59049,1048576,9765625');
      Check(not cx.GetValue('notexisting', j));
      CheckEqual(cx.EvalGlobal('var car = {type:"Fiat", model:"500", color:"white"};'), '');
      v := '{"type":"Fiat","model":"500","color":"white"}';
      CheckEqual(Run('JSON.stringify(car)'), v);
      Check(cx.GetValue(['car'], j));
      CheckEqual(cx.ToUtf8(j), v);
      Check(cx.ToVariantFree(j, va));
      Check(_Safe(va, dvObject)^.Count = 3);
      Check(cx.GetValue('add', j));
      Check(cx.ToVariant(j, va));
      v := cx.ToUtf8Free(j);
      CheckEqual(v, 'function add(x, y) { return x + y; }');
      Check(v = va);
    finally
      cx.Done;
    end;
  finally
    Check(rt.DoneSafe = '');
  end;
end;


end.

