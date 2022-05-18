unit tdsprovider10; 

{$I Tds.inc}

interface

uses
{$IFDEF USE_SSL}
  TdsAlgorithmSupportUni, 
  TdsBridgeUni, 
  TdsEC25519Uni, 
  TdsASN1Uni, 
  TdsCipherSuitesUni,
  TdsClientHandshakeLayerUni, 
  TdsCertificateConstsUni, 
  TdsOidsUni, 
  TdsLayersUni, 
  TdsMD5SHA1CSPUni,
  TdsReaderWriterUni,
  TdsReceiveBufferUni,
  TdsSSL3HandshakeLayerUni,
  TdsSSLConstsUni,
  TdsSSLMessagesUni,
  TdsSSLExtensionsUni,
  TdsSSLTypesUni,
  TdsTLS1HandshakeLayerUni,
  TdsTLS13HandshakeLayerUni,
  TdsCertificateExtsUni,
  TdsCryptoAPIIntfUni,
  TdsCryptoAPIStorageUni,
  TdsUtilsUni,
{$ENDIF}
  TdsConstsUni, 
  TdsTypesUni, 
  TdsNetUni, 
  TdsPacketsUni, 
  TdsProtocolUni, 
  TdsClassesUni;

implementation

end.
