/// regression tests for Several Network Protocols
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.net.proto;

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
  mormot.core.test,
  mormot.core.perf,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.async,
  mormot.net.rtsphttp;

type
  /// this test case will validate several low-level protocols
  TNetworkProtocols = class(TSynTestCase)
  protected
    procedure DoRtspOverHttp(options: TAsyncConnectionsOptions);
  published
    /// RTSP over HTTP, as implemented in SynProtoRTSPHTTP unit
    procedure RtspOverHttp;
    /// RTSP over HTTP, with always temporary buffering
    procedure RtspOverHttpBufferedWrite;
  end;

  
implementation

procedure RtspRegressionTests(proxy: TRtspOverHttpServer; test: TSynTestCase;
  clientcount, steps: integer);
type
  TReq = record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUtf8;
  end;
var
  streamer: TCrtSocket;
  req: array of TReq;

  procedure Shutdown;
  var
    r, rmax: PtrInt;
    log: ISynLog;
    timer, one: TPrecisionTimer;
  begin
    log := proxy.Log.Enter(proxy, 'Shutdown');
    // first half deletes POST first, second half deletes GET first
    timer.Start;
    rmax := clientcount - 1;
    for r := 0 to rmax shr 1 do
      req[r].post.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 1 %', [timer.Stop], proxy);
    timer.Start;
    req[0].stream.Free; // validates remove POST when RTSP already down
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 2 %', [timer.Stop], proxy);
    timer.Start;
    for r := (rmax shr 1) + 1 to rmax do
      req[r].get.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 3 %', [timer.Stop], proxy);
    timer.Start;
    for r := 0 to rmax shr 1 do
      req[r].get.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 4 %', [timer.Stop], proxy);
    timer.Start;
    for r := (rmax shr 1) + 1 to rmax do
      req[r].post.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 5 %', [timer.Stop], proxy);
    timer.Start;
    sleep(10);
    //proxy.Shutdown; // don't make any difference
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests SHUTDOWN 6 %', [timer.Stop], proxy);
    for r := 1 to rmax do
    begin
      one.Start;
      //req[r].stream.OnLog := TSynLog.DoLog;
      req[r].stream.Free;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests SHUTDOWN 6-% %', [r, one.Stop], proxy);
    end;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests % SHUTDOWN 7 %', [timer.Stop], proxy);
    timer.Start;
    streamer.Free;
    if log <> nil then
      log.Log(sllCustom1, 'RegressionTests ENDED %', [timer.Stop], proxy);
  end;

var
  rmax, r, i: PtrInt;
  text: RawUtf8;
  log: ISynLog;
begin
  // here we follow the steps and content stated by https://goo.gl/CX6VA3
  log := proxy.Log.Enter(proxy, 'Tests');
  if (proxy = nil) or
     (proxy.RtspServer <> '127.0.0.1') then
    test.Check(false, 'expect a running proxy on 127.0.0.1')
  else
  try
    rmax := clientcount - 1;
    streamer := TCrtSocket.Bind(proxy.RtspPort);
    try
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % GET', [clientcount], proxy);
      SetLength(req, clientcount);
      for r := 0 to rmax do
        with req[r] do
        begin
          session := TSynTestCase.RandomIdentifier(20 + r and 15);
          get := THttpSocket.Open('localhost', proxy.Server.Port, nlTcp, 1000);
          get.SndLow('GET /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Accept: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10#13#10);
          get.SockRecvLn(text);
          test.Check(text = 'HTTP/1.0 200 OK');
          get.GetHeader(false);
          test.Check(hfConnectionClose in get.Http.HeaderFlags);
          test.Check(get.SockConnected);
          test.Check(get.Http.ContentType = RTSP_MIME);
        end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], proxy);
      for r := 0 to rmax do
        with req[r] do
        begin
          post := TCrtSocket.Open('localhost', proxy.Server.Port);
          post.SndLow('POST /sw.mov HTTP/1.0'#13#10 +
            'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
            'x-sessioncookie: ' + session + #13#10 +
            'Content-Type: ' + RTSP_MIME + #13#10 +
            'Pragma: no-cache'#13#10 +
            'Cache-Control: no-cache'#13#10 +
            'Content-Length: 32767'#13#10 +
            'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
          stream := streamer.AcceptIncoming(nil, {async=}false);
          if stream = nil then
          begin
            test.Check(false);
            exit;
          end;
          stream.Sock.SetLinger(0); // otherwise shutdown takes 40ms with epoll
          test.Check(get.SockConnected);
          test.Check(post.SockConnected);
        end;
      for i := 0 to steps do
      begin
        if log <> nil then
          log.Log(sllCustom1, 'RegressionTests % RUN #%', [clientcount, i], proxy);
        // send a RTSP command once in a while to the POST request
        if i and 7 = 0 then
        begin
          for r := 0 to rmax do
            req[r].post.SndLow(
              'REVTQ1JJQkUgcnRzcDovL3R1Y2tydS5hcHBsZS5jb20vc3cubW92IFJUU1AvMS4w'#13#10 +
              'DQpDU2VxOiAxDQpBY2NlcHQ6IGFwcGxpY2F0aW9uL3NkcA0KQmFuZHdpZHRoOiAx'#13#10 +
              'NTAwMDAwDQpBY2NlcHQtTGFuZ3VhZ2U6IGVuLVVTDQpVc2VyLUFnZW50OiBRVFMg'#13#10 +
              'KHF0dmVyPTQuMTtjcHU9UFBDO29zPU1hYyA4LjYpDQoNCg=='); 
          for r := 0 to rmax do
            test.check(req[r].stream.SockReceiveString =
              'DESCRIBE rtsp://tuckru.apple.com/sw.mov RTSP/1.0'#13#10 +
              'CSeq: 1'#13#10 +
              'Accept: application/sdp'#13#10 +
              'Bandwidth: 1500000'#13#10 +
              'Accept-Language: en-US'#13#10 +
              'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10#13#10);
        end;
        // stream output should be redirected to the GET request
        for r := 0 to rmax do
          req[r].stream.SndLow(req[r].session); // session text as video stream
        if log <> nil then
          log.Log(sllCustom1, 'RegressionTests % RUN #% SndLow',
            [clientcount, i], proxy);
        for r := 0 to rmax do
          with req[r] do
          begin
            text := get.SockReceiveString;
            //if log <> nil then
            //  log.Log(sllCustom1, 'RegressionTests % #%/% received %',
            //    [clientcount, r, rmax, text], proxy);
            test.check(text = session);
          end;
      end;
      if log <> nil then
        log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], proxy);
    finally
      Shutdown;
    end;
  except
    on E: Exception do
      test.Check(false, E.ClassName);
  end;
end;

procedure TNetworkProtocols.DoRtspOverHttp(options: TAsyncConnectionsOptions);
var
  N: integer;
  proxy: TRtspOverHttpServer;
begin
  {$ifdef OSDARWIN}
  N := 10;
  {$else}
  N := 100;
  {$endif OSDARWIN}
  proxy := TRtspOverHttpServer.Create(
    '127.0.0.1', '3999', '3998', TSynLog, nil, nil, options, {threads=}1);
    // threads=1 is the safest & fastest - but you may set 16 for testing
  try
    proxy.WaitStarted(1);
    RtspRegressionTests(proxy, self, N, 10);
  finally
    proxy.Free;
  end;
end;

const
  //ASYNC_OPTION = ASYNC_OPTION_DEBUG;
  ASYNC_OPTION = ASYNC_OPTION_VERBOSE;

procedure TNetworkProtocols.RtspOverHttp;
begin
  DoRtspOverHttp(ASYNC_OPTION);
end;

procedure TNetworkProtocols.RtspOverHttpBufferedWrite;
begin
  DoRtspOverHttp(ASYNC_OPTION + [acoWritePollOnly]);
end;

end.

