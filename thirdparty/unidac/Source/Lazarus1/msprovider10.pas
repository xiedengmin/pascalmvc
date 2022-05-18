unit msprovider10; 

interface

uses
//    {$IFDEF MSWINDOWS} OLEDBIntfUni, OLEDBCUni, OLEDBAccessUni, MSUDTUni,{$ENDIF}  
    SQLServerUniProvider, MSParserUni, MSConstsUni,
    MSConnectionPoolUni, MSServicesUni, 
    MSScriptProcessorUni, MSDataTypeMapUni,
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SQLServerUniProvider', @SQLServerUniProvider.Register); 
end; 

initialization
  RegisterPackage('msprovider10', @Register); 
end.
