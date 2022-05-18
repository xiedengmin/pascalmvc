
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SDAC Server Enumerator
//////////////////////////////////////////////////

{$I Sdac.inc}
unit MSServerEnumeratorUni;

interface

uses
  Classes, SysUtils, Types, StrUtils,
{$IFDEF MSWINDOWS}
  Windows, ActiveX, Registry,
{$ENDIF}
  CRTypes, CRVio, CRVioUDP, CLRClasses, CRFunctions, CRServerEnumerator, MemData,
{$IFNDEF UNIDACPRO}  
  {$IFDEF MSWINDOWS}
  OLEDBIntf, OLEDBC, OLEDBAccess,
  {$ENDIF}
  MSProps;
{$ELSE}
  {$IFDEF MSWINDOWS}
  OLEDBIntfUni, OLEDBCUni, OLEDBAccessUni,
  {$ENDIF}
  MSPropsUni;
{$ENDIF}

type
  _TMSProvider = (_prAuto, _prSQL, _prNativeClient, _prCompact, _prDirect, _prMSOLEDB);

  TMSServerEnumerator = class (TCRServerEnumerator)
  private
    FProvider: _TMSProvider;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure GetServerList(List: TStrings); override;
  end;

implementation

{ TMSServerEnumerator }

function TMSServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prProvider:
      FPRovider := _TMSProvider(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMSServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TMSServerEnumerator.GetServerList(List: TStrings);
var
  ServerList: TStringList;

{$IFDEF MSWINDOWS}
  procedure GetServerListByReg;

    procedure AddComputerName(sl: TStringList);
    var
      CompName: string;
    {$IFDEF CLR}
      sb: StringBuilder;
    {$ENDIF}
      Size: Cardinal;
      i: Integer;
    begin
      if sl.Count < 1 then
        Exit;
      Size := MAX_COMPUTERNAME_LENGTH + 1;
      SetLength(CompName, Size);

    {$IFDEF CLR}
      sb := StringBuilder.Create(Size);
      try
        GetComputerName(sb, Size);
        CompName := sb.ToString;
      finally
        sb.Free;
      end;
    {$ELSE}
      if not GetComputerName(PChar(CompName), Size) then
        Exit;
      SetLength(CompName, Size);
    {$ENDIF}

      for i := 0 to sl.Count - 1 do
        if AnsiUpperCase(sl[i]) = 'MSSQLSERVER' then
          sl[i] := CompName                  // not named instance
        else
          sl[i] := CompName + '\' + sl[i];   // named instance
    end;

  var
    Reg: TRegistry;
    i: integer;
    Value: string;
    sl: TStringList;
  begin
    Reg := TRegistry.Create;
    sl := TStringList.Create;
    try
      // get server list to which Enterprise Manager was connected
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\MSSQLServer\Client\ConnectTo') then begin
        Reg.GetValueNames(sl);

        // Delete non-server occurences
        for i := sl.Count - 1 downto 0 do begin
          Value := Reg.ReadString(sl[i]);
          if Pos(',', Value) = 0 then
            sl.Delete(i);
        end;

        sl.Sort;
        ServerList.AddStrings(sl);
        Reg.CloseKey;
      end;

      // get local SQL Server instances
      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL') then begin
        Reg.GetValueNames(sl);

        AddComputerName(sl);
        ServerList.AddStrings(sl);
      end;

    finally
      Reg.Free;
      sl.Free;
    end;
  end;
  procedure GetServerListByOleDBEnum;
  var
    SourcesRowset: ISourcesRowset;
    Rowset: IRowset;
    ds: TOLEDBRecordSet;
    iu: IUnknown;
    RecBuf: IntPtr;
    DataBuf: IntPtr;
    DataLen: Word;
    IsBlank: boolean;
    Field: TFieldDesc;
  begin
    if CoCreateInstance(CLSID_SQLOLEDB_ENUMERATOR,
                        nil,
                        CLSCTX_INPROC_SERVER,
                        IID_ISourcesRowset,
                        SourcesRowset) <> S_OK then
      Exit;

    if SourcesRowset.GetSourcesRowset(nil, IID_IRowset, 0, nil, iu) <> S_OK then
      Exit;

    Rowset := IRowset(iu);
    ds := TOLEDBRecordSet.Create;
    try
      ds.DataTypeMap.Enabled := False;
      ds.SetIRowset(Rowset);
      ds.Open;

      Field := ds.Fields[0];
      ds.AllocRecBuf(RecBuf);
      DataBuf := Marshal.AllocHGlobal(Field.Size + 1);
      try
        while True do begin
          ds.GetNextRecord(RecBuf);
          if ds.Eof then
            Break;

          ds.GetField(Field, RecBuf, DataBuf, DataLen, False, IsBlank);
          if IsBlank then
            Continue;

          if Field.DataType = dtWideString then
            ServerList.Add(string(Marshal.PtrToStringUni(DataBuf, DataLen)))
          else
            ServerList.Add(string(Marshal.PtrToStringAnsi(DataBuf, DataLen)));
        end;
      finally
        ds.Close;
        Marshal.FreeHGlobal(RecBuf);
        Marshal.FreeHGlobal(DataBuf);
      end;
    finally
      ds.Free;
    end;
  end;
{$ENDIF}

(* yet another Udp
  procedure GetServerListBySocket;
  var
    r: integer;
    FSd: integer;
    SndBuf, Nodelay, Broadcast, Timeout: integer;
    sock_addr: TSockAddr;
    buf: Byte;
    Buffer: array[0..8191] of Byte;
    Count: Integer;
    AnsiStr: AnsiString;
    str: string;
    pLexem, p1, p2: Integer;
    ServerName, InstanceName: string;
  begin
    if WsaData.wVersion = 0 then begin // not initialized
      r := WSAStartup($0101, WsaData);
      if r <> 0 then
        raise Exception.CreateFmt(SSocketError, [r]);
    end;

    FSd := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if FSd = SOCKET_ERROR then
      raise Exception.CreateFmt(SSocketError, [FSd]);

    try
      SndBuf := 0;
      setsockopt(FSd, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@SndBuf), sizeof(SndBuf));
      Nodelay := 1;
      setsockopt(FSd, SOL_SOCKET, TCP_NODELAY, PAnsiChar(@Nodelay), sizeof(Nodelay));
      Broadcast := 1;
      setsockopt(FSd, SOL_SOCKET, SO_BROADCAST, PAnsiChar(@Broadcast), sizeof(Broadcast));
      Timeout := 500;
      setsockopt(FSd, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@Timeout), sizeof(Timeout));
      sock_addr.sin_family := AF_INET;
      sock_addr.sin_addr.s_addr := -1; // inet_addr(PChar('255.255.255.255'))
      sock_addr.sin_port := htons(1434);

      buf := 3; //ping
      sendto(FSd, buf, 1, 0, sock_addr, sizeof(sock_addr));

      while True do begin
        Count := recv(FSd, Buffer, sizeof(Buffer), 0);
        if Count <= 0 then
          Break;

        SetLength(AnsiStr, Count - 3);
        Move(Buffer[3], AnsiStr[1], Length(AnsiStr));
        str := string(AnsiStr);

        pLexem := Pos('ServerName', str);
        while pLexem > 0 do begin
          p1 := PosEx(';', str, pLexem + 10{Length('ServerName')});
          if p1 = 0 then
            raise Exception.Create(SInvalidValue);
          p2 := PosEx(';', str, p1 + 1);
          if p2 = 0 then
            raise Exception.Create(SInvalidValue);
          ServerName := Copy(str, p1 + 1, p2 - p1 - 1);

          pLexem := PosEx('InstanceName', str, p2 + 1);
          p1 := PosEx(';', str, pLexem + 12{Length('InstanceName')});
          if p1 = 0 then
            raise Exception.Create(SInvalidValue);
          p2 := PosEx(';', str, p1 + 1);
          if p2 = 0 then
            raise Exception.Create(SInvalidValue);
          InstanceName := Copy(str, p1 + 1, p2 - p1 - 1);
          if AnsiUpperCase(InstanceName) <> 'MSSQLSERVER' then
            ServerName := ServerName + '\' + InstanceName;

          ServerList.Add(ServerName);
          pLexem := PosEx('ServerName', str, p2 + 1);
        end;
      end;

    finally
      closesocket(FSd);
    end;
  end;
*)

  procedure GetServerListUdp;
  var
    num_try, len, Count: integer;
    udp: TCRVioUdp;
    Buffer: array[0..8191] of Byte;
    AnsiStr: AnsiString;
    str, instName, temp: string;
    p1, p2: Integer;
    a: TStringDynArray;
  begin
  {$IFNDEF VER9P}
    a := nil;
  {$ENDIF}
    udp := TCRVioUdp.Create('', {DefaultSDACUdpPort}1434, ivIPv4);
    try
      udp.IsBroadcast := True;
      udp.CreateSocket;
      udp.ReceiveTimeout := 1;

     	for num_try := 1 to 3 do begin
     		{ send the request }
     		Buffer[0] := 3;
     		if udp.Write(@Buffer, 0, 1) = 0 then
          Break; // error?

        while True do begin
          Count := udp.Read(@Buffer, 0, sizeof(Buffer));
          if Count <= 3 {0} then // no more data available
            Break;

          len := Count - 3;
          SetLengthA(AnsiStr, len);
          Move(Buffer[3], PAnsiChar(AnsiStr)^, len);
          str := string(AnsiStr);

          p1 := Pos('ServerName', str);
          p2 := 0;
          if p1 = 0 then
            Continue;
          while True do
          try
            p2 := PosEx('ServerName', str, p1 + 1);
            if p2 = 0 then
              if p1 < Length(str) then
                p2 := Length(str)
              else
                Break;

            temp := Copy(str, p1, p2 - p1);
            a := SplitString(temp, ';');
            if Length(a) >= 10 then begin
              // ServerName, value, InstanceName, value, IsClustered, value, Version, value, tcp, value
              if UpperCase(a[3]) = 'MSSQLSERVER' then
                instName := UpperCase(a[1])
              else
                instName := UpperCase(a[1]) + '\' + UpperCase(a[3]);

              if ServerList.IndexOf(instName) < 0 then
                ServerList.Add(instName);
            end;
          finally
            p1 := p2;
          end;
        end;
      end;
    finally
      udp.Free;
    end;
  end;

  procedure OrderList;
  var
    i: integer;
  begin
    ServerList.Sort;
    // delete duplicates
    i := 0;
    while i < ServerList.Count - 1 do
      if AnsiUpperCase(ServerList[i]) = AnsiUpperCase(ServerList[i + 1]) then
        ServerList.Delete(i + 1)
      else
        inc(i);
  end;

begin
  List.Clear;

  if FProvider = _prCompact then
    Exit;

  ServerList := TStringList.Create;
  try
  {$IFDEF MSWINDOWS}
    GetServerListByOleDBEnum;
    GetServerListByReg;
  {$ENDIF}
    GetServerListUdp;
    OrderList;
    List.AddStrings(ServerList);
  finally
    ServerList.Free;
  end;

end;

end.
