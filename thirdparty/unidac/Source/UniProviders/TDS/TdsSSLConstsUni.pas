
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Tds.inc}
unit TdsSSLConstsUni;

interface

const
{$IFDEF MSWINDOWS}
  SLLineSeparator = #$D#$A;
{$ELSE}
  SLLineSeparator = #$A;
{$ENDIF}
  DALineSeparator = #$D; // !!! for _TrialImpl

const
  CR = 13;
  LF = 10;
  CRLF = #13#10;
  CHAR0 = #0;
  BACKSPACE = #8;
  TAB = #9;
  CHAR32 = #32;
  LWS = TAB + CHAR32;

  LOCAL_HOST = 'localhost';
  DEFAULT_TIMEOUT = 15;
  FTP_DEFAULT_PORT = 21;
  SSH_DEFAULT_PORT = 22;
  SMTP_DEFAULT_PORT = 25;
  SMTPS_DEFAULT_PORT = 465;
  DEFAULT_SOCKET_BUFFER_SIZE = 32 * 1024; // 32Kb

const
/// Common Algorithms
  SHASH_None     = 'NONE';
  SHASH_SHA1     = 'SHA1';
  SHASH_SHA2_256 = 'SHA256';
  SHASH_SHA2_512 = 'SHA512';
  SHASH_SHA2_224 = 'SHA224';
  SHASH_SHA2_384 = 'SHA384';
  SHASH_MD5      = 'MD5';
  SHASH_MD4      = 'MD4';
  SHASH_MD2      = 'MD2';

  Shmac_md2      = 'hmac-md2';
  Shmac_md4      = 'hmac-md4';
  Shmac_md5      = 'hmac-md5';
  Shmac_sha1     = 'hmac-sha1';
  Shmac_sha2_256 = 'hmac-sha2-256';
  Shmac_sha2_224 = 'hmac-sha2-224';
  Shmac_sha2_512 = 'hmac-sha2-512';
  Shmac_sha2_384 = 'hmac-sha2-384';
  SNone          = 'none';
  SZLib          = 'zlib';
  SZLibOpenSSH   = 'zlib@openssh.com';
  STripleDES_cbc = '3des-cbc';
  SBlowfish_cbc  = 'blowfish-cbc';
  SAES128_cbc    = 'aes128-cbc';
  SAES192_cbc    = 'aes192-cbc';
  SAES256_cbc    = 'aes256-cbc';
  SCast128_cbc   = 'cast128-cbc';
  STripleDES_ctr = '3des-ctr';
  SBlowfish_ctr  = 'blowfish-ctr';
  SAES128_ctr    = 'aes128-ctr';
  SAES192_ctr    = 'aes192-ctr';
  SAES256_ctr    = 'aes256-ctr';
  SCast128_ctr   = 'cast128-ctr';
  SAES128_gcm    = 'aes128-gcm';
  SAES192_gcm    = 'aes192-gcm';
  SAES256_gcm    = 'aes256-gcm';

/// SSH Protocol
  S_Devart15                  = 'SSH-1.5-Devart-1.0';
  S_Devart20                  = 'SSH-2.0-Devart-8.0';

  Stcpip_forward              = 'tcpip-forward';
  Sdirect_tcpip               = 'direct-tcpip';
  Sforwarded_tcpip            = 'forwarded-tcpip';
  Scancel_tcpip_forward       = 'cancel-tcpip-forward';
  Ssession                    = 'session';

  SDiffieHellmanGroup1        = 'diffie-hellman-group1-sha1';
  SDiffieHellmanGroup14       = 'diffie-hellman-group14-sha1';
  SDiffieHellmanExchSHA1      = 'diffie-hellman-group-exchange-sha1';
  SDiffieHellmanExchSHA256    = 'diffie-hellman-group-exchange-sha256';

  RSA_TYPE_HEADER             = 'ssh-rsa';
  RSA_SHA256_TYPE_HEADER      = 'rsa-sha2-256';
  RSA_SHA512_TYPE_HEADER      = 'rsa-sha2-512';
  DSA_TYPE_HEADER             = 'ssh-dss';
  ECDSA_TYPE_HEADER           = 'ecdsa';
  ECDSA_SHA2_TYPE_HEADER      = 'ecdsa-sha2-';
  ED25519_TYPE_HEADER         = 'ssh-ed25519';

  SSH_NIST_p192               = 'nistp192';
  SSH_NIST_p224               = 'nistp224';
  SSH_NIST_p256               = 'nistp256';
  SSH_NIST_p384               = 'nistp384';
  SSH_NIST_p521               = 'nistp521';
  SSH_NIST_k163               = 'nistk163';
  SSH_NIST_k233               = 'nistk233';
  SSH_NIST_b233               = 'nistb233';
  SSH_NIST_k283               = 'nistk283';
  SSH_NIST_k409               = 'nistk409';
  SSH_NIST_b409               = 'nistb409';
  SSH_NIST_t571               = 'nistt571';

  SECDSA_Sha2_Nistp256        = 'ecdsa-sha2-nistp256';
  SECDSA_Sha2_Nistp384        = 'ecdsa-sha2-nistp384';
  SECDSA_Sha2_Nistp521        = 'ecdsa-sha2-nistp521';

  SECDH_Sha2_Nistp256         = 'ecdh-sha2-nistp256';
  SECDH_Sha2_Nistp384         = 'ecdh-sha2-nistp384';
  SECDH_Sha2_Nistp521         = 'ecdh-sha2-nistp521';
  SECDH_Sha2_Nistk163         = 'ecdh-sha2-1.3.132.0.1';
  SECDH_Sha2_Nistp192         = 'ecdh-sha2-1.2.840.10045.3.1.1';
  SECDH_Sha2_Nistp224         = 'ecdh-sha2-1.3.132.0.33';
  SECDH_Sha2_Nistk233         = 'ecdh-sha2-1.3.132.0.26';
  SECDH_Sha2_Nistb233         = 'ecdh-sha2-1.3.132.0.27';
  SECDH_Sha2_Nistk283         = 'ecdh-sha2-1.3.132.0.16';
  SECDH_Sha2_Nistk409         = 'ecdh-sha2-1.3.132.0.36';
  SECDH_Sha2_Nistb409         = 'ecdh-sha2-1.3.132.0.37';
  SECDH_Sha2_Nistt571         = 'ecdh-sha2-1.3.132.0.38';

  SCurve25519_Sha256          = 'curve25519-sha256';

  Spublickey                  = 'publickey';
  Spassword                   = 'password';
  Sssh_connection             = 'ssh-connection';
  Sssh_userauth               = 'ssh-userauth';
  Skeyboard_interactive       = 'keyboard-interactive';
  Sssh_authentication         = 'SSH authentication';
  SPassword_authentication    = 'Password authentication';
  SPasswordPrompt             = 'Password: ';
  SKeepalive                  = 'keepalive@';
  SKeepaliveCom               = 'keepalive@com';
  SSimplePuttyTartarus        = 'simple@putty.projects.tartarus.org';

{$IFNDEF LITE}
resourcestring
{$ELSE}
const
{$ENDIF}
/// Common Errors
  SUnknown                       = 'Unknown';
  SInternalError                 = 'Internal Error';
  SInvalidInputArgs              = 'Invalid input arguments';
  SInvalidInputArg               = 'Invalid input argument: %s';
  SNotOverriddenMethod           = 'Method must be overridden in the inherited class';
  SInvalidObjectClass            = 'Invalid object class: %s expected instead of %s';
  SListIndexError                = 'List index out of bounds (%d)';
  SAssignError                   = 'Cannot assign a %s to a %s';
  SNotInitialized                = '%s is not initialized. Set Key first';
  SInvalidIV                     = 'Invalid Initialization Vector';
  SInvalidKeySize                = 'Length from Encryption key is invalid.'#13#10'Keysize for %s must be %d-%d bytes';
  SGCMModeMustBeUsed             = 'GCM mode must be used';
  SGCMCannotBeUsed               = 'GCM supports only symmetric block ciphers whose block size is 128 bits';
  SInvalidEncData                = 'Encrypted data is corrupt';
  SCannotCreateEvent             = 'Cannot create event:'#13#10'%s.'#13#10'Error Code: %d';
  SNoTimers                      = 'Not enough timers available';
  SWaitError                     = 'Wait error';
  SGetLastError                  = {$IFDEF MSWINDOWS}'WSAGetLastError return %d($%X)'{$ELSE}'Error code = %d($%X)'{$ENDIF};
  SSocketErr                     = 'Socket error';
  SSocketErrWithErr              = 'Socket error. '#13#10+{$IFDEF MSWINDOWS}'WSAGetLastError return %d($%X)'{$ELSE}'Error code = %d($%X)'{$ENDIF};
  SInvalidCipherAlgorithm        = 'Invalid cipher algorithm';
  SInvalidHashAlgorithm          = 'Invalid hash algorithm';
  SInvalidCompressionAlgorithm   = 'Invalid compression algorithm';
  SInvalidPublicKeyAlgorithm     = 'Invalid public key algorithm';
  SInvalidKeyExchangeAlgorithm   = 'Invalid signature algorithm';
  SInvalidSignatureAlgorithm     = 'Invalid key exchange algorithm';
  SUnknownHashAlgorithm          = 'Unknown hash algorithm';
  SUnknownSignatureAlgorithm     = 'Unknown signature algorithm';
  SClientOpened                  = 'Changing an option is not allowed when the client is open';
  SServerOpened                  = 'Changing an option is not allowed when the server is open';
  SClientClosed                  = 'Changing an option is not allowed when the client is closed';
  SCompressionNotSupported       = 'Compression is not supported by server';
  SCompressorNotAssigned         = 'Compressor is not assigned';
  SAcceptTimedOut                = 'Error on accepting of incoming connection attempt: Accept timed out';
  SConnectionNotSecure           = 'Connection is not secure';
  SSourceConnectionNotSecure     = 'Source connection is not secure';
  SAsyncEventProcessorNotStarted = 'Async event processor must be started';
  SInvalidReplyCode              = 'Reply code is invalid: %s';
  SEventHandlerNotDefined        = 'The %s event handler is not defined';

/// SSH
  SMsgAcceptServerKey         = 'Hostkey not found in storage. Do you want to accept a key received from server?';
  SConnectionNotDefined       = 'Connection is not defined';
  SChannelNotDefined          = 'SSH channel is not defined';
  SConnectionTimeout          = 'Connection timeout expired';
  SCannotSendData             = 'Cannot send data to server';
  SCannotSendClientData       = 'Cannot send data to client';
  SCannotListenPF             = 'Cannot listen forwarded port';
  SHostKeysNotFound           = 'Host keys not found';
  SHostKeyNotVerifed          = 'Host key not verified';
  SUnexpectedEOP              = 'Unexpected end of data packet';

  SChannelFailedAdministrativelyProhibited = 'Opening channel is administratively prohibited';
  SChannelFailedConnectionFailed           = 'Opening channel failed - connection failed';
  SChannelFailedUnknownChannelType         = 'Opening channel failed - unknown channel type';
  SChannelFailed              = 'Opening channel failed; packet type = %d';
  SChannelError               = 'Opening channel failed with error code %d';
  SAuthenticationFailed       = 'Authentication failed ';
  SNoPacketReceived           = 'Cannot authenticate - no packet received';
  SInvalidServerVersion       = 'Format of server version is invalid (%s)';
  SNotCompatibleServerVersion = 'The protocol version of server is not compatible for SSH%d';
  SInvalidPacketSize          = 'Packet size %d is invalid';
  SInvalidPaddingSize         = 'Padding length %d is invalid';
  SEmptyUsername              = 'User name is not set';
  SClientNotDefined           = 'Client not defined';
  SOpenChannelError           = 'Open Channel Error';
  SBadKeyAlgorithm            = 'Algorithm of key %s is not %s';
  SNotDirectChannel           = 'Direct mode must be set';
  SPrivateKeyNotFound         = 'Private key not found';
  SNotEnoughData              = 'There is no enough data to read';
  SUnsupportedSessionType     = 'Unsupported session type: %s';
  SUnsupportedRequestType     = 'Unsupported request type: %s';
  SServerNotSupportShell      = 'SSH server does not support shell';
  SChannelNotConnected        = 'SSH channel not connected';
  SClientE_OutOfRange         = 'Client kex "e" is out of range';
  SServerF_OutOfRange         = 'Server kex "f" is out of range';
  SRemoteAndDirectSet         = 'Remote must be False when Direct is set';
  SDynamicAndDirectSet        = 'Dynamic must be False when Direct is set';
  SRemoteAndDynamicSet        = 'Remote must be False when Dynamic is set';

/// SFTP
  SUnknownReplyExtensionType  = 'Unknown reply extension packet type';
  SUnknownFileType            = 'Unknown file type';
  SUnknownTextHintValue       = 'Unknown text-hint value';
  SUnsupportedAttribute       = 'Unsupported attribute';
  SUnsupportedBlockMode       = 'Unsupported block mode';
  SUnsupportedExtensionName   = 'Unsupported extension name';
  SUnsupportedFileAttrData    = 'Unsupported file attributes data';
  SServerNotSupportSFTP       = 'SSH server does not support SFTP protocol';
  SUnsupportedSFTPOperation   = 'Current SFTP version does not support this operation';
  SUnknownSFTPClientVersion   = 'Unknown SFTP client version';
  SUnknownSFTPServerVersion   = 'Unknown SFTP server version';
  SSFTPClientActive           = 'Cannot perform initialization on an active SFTP client';
  SInvalidFileHandle          = 'Invalid file handle';
  SStreamReadError            = 'Stream read error';
  SSFTPServerError            = 'SFTP server error occurred: %s';
  SPathNotAllowed             = 'Path is not allowed';
  SImpersonationLogonUserFail = 'Impersonation logon user fail: %s';
  SImpersonateUserFail        = 'Impersonate user fail: %s';
  SSFTPInitializationFail     = 'SFTP Initialization fail: %s';
  SOperationNotAllowedWhenNonBlocking = 'The operation is not allowed when NonBlocking is True';
  SFileDownloadInterrupted    = 'File download interrupted by user';
  SFileUploadInterrupted      = 'File upload interrupted by user';

/// SSH & SSL
  SVerifyKeyExAlgFailed       = 'The negotiation of key exchange algorithm is failed';
  SVerifyHostKeyFailed        = 'The negotiation of host key algorithm is failed';
  SVerifyEncAlgFailed         = 'The negotiation of encryption algorithm is failed';
  SVerifyMacAlgFailed         = 'The negotiation of mac algorithm is failed';
  SVerifyCompressAlgFailed    = 'The negotiation of compression algorithm is failed';
  SUnexpectedPacketType       = 'Protocol error: unexpected packet type';
  SUnknownMessageType         = 'Protocol error: unknown message type';
  SUnexpectedPacket           = 'Protocol error: unexpected packet';
  SInvalidMessage             = 'Protocol error: message is invalid';
  SInvalidMessageLength       = 'Protocol error: message length is invalid';
  SCorruptMessage             = 'Protocol error: message is corrupt';
  STimeoutSession             = 'Timeout expired, session has not responded';
  SSocketClosed               = 'Socket closed. Cannot receive data';
  SConnectionClosed           = 'Connection is closed';
  SConnectionClosedWithMessage= 'Connection was closed by the other side with message: '#13#10'%s';
  SWrongHostname              = 'Wrong host name';
  SWrongPort                  = 'Wrong port number';
  SHashVerificationNotCorrespond = 'The computed hash verification does not correspond to the recieved';

/// SSL
  SCannotWriteSocketData      = 'Cannot write data to socket';
  SSocketNotConnected         = 'Socket not connected';
  SInvalidEllipticCurveName   = 'Invalid Elliptic Curve name';
  SUnknownProtocolVersion     = 'Unknown protocol version';
  SIllegalRandomParameter     = 'Illegal random parameter was received in the ServerHello message';
  SIllegalSessionIDParameter  = 'Illegal session ID parameter was received in the ServerHello message';
  SIllegalKeyShareParameters  = 'Illegal Key Share parameters were received in the ServerHello message';
  SErrorServerKeyExchangeNonExportable = 'The ServerKeyExchange message should not be sent for non-exportable ciphers';
  SErrorServerKeyExchangePublicKey = 'The server public key from the ServerKeyExchange message is of non-exportable length';
  SPeerCertificateNotReceived = 'The peer certificate for authentication was not received from the other side';
  SServerCertificateNotReceived = 'The server certificate for authentication was not received from server';
  SECurveDomainTypeNotSupported = 'Elliptic Curve domain type is not supported';
  SErrorHandshakeProcedure    = 'The handshake procedure was not completed successfully before application data was received';
  SErrorPublicKeyLength       = 'The pulic key length should be at least 1024 bits';
  SErrorDiffieHelmanKeyLength = 'The Diffie-Helman key length should be at least 1024 bits';
  SFailureAlert               = 'The other side has sent a failure alert: [%d]';
  SNotAgreeOnProtocol         = 'The client and server could not agree on the protocol version to use';
  SMissingExtension           = 'Required extension is missing';
  SNormalConnectionIsNotSupported = 'Only changing from a normal connection to a secure connection is supported';
  SServerCertificateNotSpecified = 'The server certificate is not specified';
  SCertificateMustBePrivate   = 'If a certificate is specified, it must have a private key';
  SSsl3PsepdoRandomError      = 'The SSL3 pseudo random function can only output 416 bytes';
  SInvalidOperation           = 'Invalid operation';
  SWrongExtensionData         = 'Wrong extension data';
  SInvalidRenegotiationInfo   = 'Invalid renegotiation info';
  SRenegotiationDenied        = 'Renegotiation is denied';
  SUnknownExtensionClass      = 'Unknown extension class type: %s';
  SUnsupportedExtension       = 'Unsupported extension: %s';
  SReceivedExtensionDuplicated = 'Received extension is duplicated: %s';
  SDataSentAfterShutdown      = 'Invalid SSL server behaviour - data was sent after the shutdown alert';
  SSessionTicketDenied        = 'Session Ticket extension is denied';
  SSourceSessionTicketAbsent  = 'Source client does not have Session ticket';
  SExtensionClassDuplicated   = 'Extension class is duplicated: %s';
  SCertificateNotCorrespondToCertificateRequest = 'The certificate does not correspond to the received Certificate Request';
  SCertificateNotCorrespondToRequiredSignatureAlgorithms = 'The certificate does not correspond to the required signature algorithms';
  SInvalidSignatureSchemeAlgorithm = 'Invalid Signature Scheme algorithm';
  SCookiesInInitialClientHello = 'Client must not use cookies in its initial ClientHello';
  SSSLNegotiationCommandFailed = 'SSL negotiation command is failed';
  SExtendedMasterSecretModeRequired = 'The Extended Master Secret mode is required';
  SDuplicateKExNamedGroup      = 'KEx named group is duplicated';
  SSesionTicketTimeExpired     = 'Sesion ticket time expired';
  SNewSessionTicketLifetimeMoreMax = 'NewSessionTicketLifetime can not be more than 604800 seconds {7 days}';

/// ScBridge
  SCryptoAPIStorageNotSupportUsers = 'TScCryptoAPIStorage does not support the Users list';
  SCryptoAPIStorageNotSupportCRLs = 'TScCryptoAPIStorage does not support the CRLs list';
  SObjNotReady                = '%s is not ready';
  SKeyNotReady                = 'Key is not ready';
  SBrokenKey                  = 'Key is broken';
  SStorageNoSet               = 'Storage is no set';
  SIPCorruptData              = 'Invalid PKCS1 padding, data is corrupt';
  SMessageTooLong             = 'Message too long';
  SInvalidLength              = 'Invalid message length';
  SHashChangedAfterWrite      = 'Hash key cannot be changed after the first write to the stream';
  SPositiveOverflow           = 'Value positive overflow';
  SNegativeUnderflow          = 'Value negative underflow';
  SValueHasNoInverse          = 'The BigInteger value has no inverse';
  SInvalidBigIntegerRadix     = 'Radix must be in following range: [2..36]';
  SBitCountMultiple           = 'BitCount must be divisible 128';
  SUseGenerateECMethod        = 'Use the GenerateEC method to generate ECC key';
  SNotPositiveExponents       = 'Only positive exponent is allowed';
  SCSPError                   = 'CryptoServiceProvider Error';
  SNotCompatibleFormatWithPassphrase = 'KeyFormat does not support Passphrase';
  SNotCompatibleFormatWithComment = 'KeyFormat does not support Comments';
  SNotCompatibleFormatWithPublicKeyOnly = 'KeyFormat is not compatible with the PublicKeyOnly mode';
  SNotCompatibleFormatWithECKey = 'Key format is not compatible with the Elliptic Curve key';
  SNullName                   = 'Name cannot be empty';
  SCannotSignData             = 'The data can not be signed by a non private key';
  SCannotDecryptData          = 'The data can not be decrypted by a non private key';
  SDSACannotEncrypt           = 'The data can not be encrypted by a DSA key';
  SECCannotEncrypt            = 'The data can not be encrypted by a EC key';
  SCipherNotSupported         = 'This cipher algorithm is not supported';
  SKeyAuthNotSupported        = 'Public key authentication is not permitted for this user';
  SWrongDataFormat            = 'Wrong data format';
  SObjectNotFound             = 'Object not found';
  SObjNameNotFound            = 'Object "%s" not found';
  SKeyPathNotFound            = 'KeyPath not found';
  SObjectAlreadyExists        = 'An object named %s already exists';
  SKeyNotPrivate              = 'Key not private';
  SObjNameMissing             = 'Object name missing';
  SDuplicateObjName           = 'Object name is duplicated';
  SKeyNotFound                = 'Key %s not found';
  SUserNameMissing            = 'User name missing';
  SDuplicateUserName          = 'User name is duplicated';
  SUserNotFound               = 'User %s not found';
  SCertificateNameMissing     = 'Certificate name missing';
  SDuplicateCertificateName   = 'Certificate name is duplicated';
  SCertificateNotFound        = 'Certificate %s not found';
  SCertificateNotExists       = 'Certificate not exists';
  SCertificateNotReady        = 'Certificate is not ready';
  SCRLNotReady                = 'CRL is not ready';
  SLargeExpLength             = 'Exponent length is too large';
  SCertificateNotValid        = 'The certificate is not valid';
  SNonCertificate             = 'Data does not contain a certificate';
  SCouldNotAcquireCSP         = 'Could not acquire crypto service provider context';
  SEmptyStorageName           = 'The storage name cannot be empty';
  SErrorRetrievingHash        = 'An error occurred when retrieving the hash of the certificate: %s';
  SErrorRequestingIssuer      = 'An error occurred when requesting the issuer name';
  SErrorRequestingSubject     = 'An error occurred when requesting the subject name';
  SCannotFindCertificate      = 'The certificate could not be found in the storage';
  SBadKeyData                 = 'Could not import key from the data: %s';
  SCanNotAssociatePrivateKey  = 'Can not associate the private key with the certificate: %s';
  SErrorSigningData           = 'An error occurred when signing data: %s';
  SErrorVerifyingData         = 'An error occurred when verifying data: %s';
  SErrorEncryptingData        = 'An error occurred when encrypting data: %s';
  SUnableFindCertChain        = 'Unable to find the certificate chain';
  SUnableVerifyCert           = 'Unable to verify the certificate: %s';
  SCertSignatureNotValid      = 'The certificate signature is not valid';
  SInsecureSignature          = 'The certificate has insecure signature';
  SForbiddenSignature         = 'The certificate has forbidden signature';
  SInvalidPolicies            = 'The certificate policies extension is not valid';
  SUnknownCriticalExtension   = 'The certificate contains unknown critical extension';
  SCRLNotFound                = 'The CRL is not found';
  SCRLIsNotValid              = 'The CRL is not valid';
  SCertificateIsRevoked       = 'The certificate is revoked';
  SCertNotValidityPeriod      = 'The certificate is not within its validity period';
  SCertCANotValid             = 'The basic constraints of the certificate are not valid, or they are missing';
  SCertKeyUsageNotValid       = 'The certificate key usage extension is not valid';
  SCertNotTrusted             = 'The certificate is not trusted by the trust provider';
  SInvalidSubjectName         = 'The certificate subject name is not complies the server DNS name';
  SIssuerNotEqualSubject      = 'The certificate issuer is not equal to the parent certificate subject';
  SDistinguishedNameNotFound  = 'Distinguished name %s not found';
  SCertStorageCannotBeChanged = 'Ceritificate''s storage cannot be changed';
  SWrongCertContext           = 'Wrong certificate context';
  SWrongCRLContext            = 'Wrong CRL context';
  SCertKeyCannotBeChanged     = 'Certificate key cannot be changed';
  SCannotOpenRegistry         = 'Cannot access a registry key';
  SCannotFindProviderName     = 'Cannot find specified provider name';
  SChangingReadOnlyStorage    = 'Cannot change read-only storage';
  SUnknownAlgorithmI          = 'Unknown algorithm %d';
  SUnknownAlgorithmS          = 'Unknown algorithm %s';
  SUnknownMGFId               = 'Unknown mask generation function identifier';
  SImproperPaddingMode        = 'Padding mode is improper';
  SInvalidSaltLength          = 'Invalid salt length';
  SChangingReadOnlyObject     = 'Object is read-only and cannot be changed';
  SwNAFError                  = 'Error on Non-Adjacent Form comptuting';
  SPointInfinitive            = 'Point is Infinitive';
  SUnknownECParametersFormat  = 'Unknown EC parameters format';
  SUnsupportedECParameters    = 'Unsupported EC parameters';
  SUnknownEllipticCurveId     = 'Unknown Elliptic Curve identifier';
  SECCryptographyNotInitialized = 'ECCryptography is not initialized';
  SWrongECPointFormat         = 'Wrong Elliptic Curve point format';
  SWrongEllipticCurvesParameters = 'Wrong Elliptic Curves parameters';
  SPreHashTypeNotSupported    = 'This PreHash type is not supported';
  SWrongPKCS12Context         = 'Wrong PKCS #12 format';
  SPKCS12DataBroken           = 'PKCS #12 data is broken';
  SCertificateSignNotSet      = 'CertificateSign is not set';
  SCertificateEncNotSet       = 'CertificateEnc is not set';
  SPasswordNotSpecified       = 'Password not specified';
  SUnsupportedKeyEncryption   = 'Unsupported key encryption';
  SUnsupportedBCryptParameters = 'Unsupported BCrypt parameters';

/// ASN1
  SCannotConvert              = 'Value cannot be converted to specified data type';
  SChoiceLexemNotDefined      = 'Choice lexem is not defined';
  SDataTypeIsSet              = 'Data type is already set';
  SLexemNotFound              = 'Lexem ''%s'' not found';

/// CMS
  SCertificateNotCorrespondToRecipient = 'The certificate does not correspond to the recipient identifier';
  SNullSubjectKeyIdentifier   = 'The subject key identifier cannot be empty';
  SStorageAndCertNoSet        = 'Neither storage nor certificate is specified';
  SCertificateNotDefined      = 'The certificate is not defined';
  SUnknownCertFormat          = 'Unknown certificate format';
  SContentDataNotFound        = 'Content data not found';
  SSignatureNotFound          = 'Signature not found';
  SInvalidIdentifierType      = 'Invalid identifier type';
  SInvalidRecipientInfo       = 'Invalid recipient info type';
  SRecipientInfoNotSupported  = 'The recipient info type is not supported';
  SEncryptionAlgorithmReadOnly= 'Encryption algorithm can be changed only by the Init method';
  SDataNotEncrypted           = 'Data must be encrypted before encoding';
  SDataNotSigned              = 'Data must be signed before encoding';
  SSmallCertificateKeyLength  = 'The message should not be encrypted because the certificate key length should be at least 1024 bits';
  SRecipientInfoNotFound      = 'Recipient Info matching the specified certificate not found in the Recipient Infos list';
  SInvalidSignature           = 'The signature is corrupt or invalid';
  SMessageDigestNotExists     = 'The signer information does not contain the stored message digest';
  SInvalidMessageDigest       = 'Data integrity of the message signer information is not verified';
  SAttributeCannotBeIncluded  = 'The %s attribute cannot be included manually';
  SNotSignedData              = 'Data type is not signed data type';
  SNotEnvelopedData           = 'Data type is not enveloped data type';

/// FTP
  SFTPClientActive            = 'FTP client is already connected';
  SFTPClientNotConnected      = 'FTP client not connected';
  SFTPServerError             = 'FTP server error occurred: %s';
  SFTPInvalidPortFormat       = 'Invalid Port format: %s';
  SFTPOperationCancelled      = 'Operation is cancelled';
  SFTPSetNoPassiveModeWithUseNATFastConnection = 'Can not set non passive mode when UseNATFastConnection is set';
  SFTPSetUseExtendedDataAddressWithIPv6 = 'UseExtendedDataAddress must be True when IPVersion is IPv6';
  SFTPSetUseExtendedDataAddressWithUseNATFastConnection = 'UseExtendedDataAddress must be True when UseNATFastConnection is set';
  SFTPSetUseClearingControlChannelWithNoTLS = 'Can not set UseClearingControlChannel when TLS is no used';
  SFTPSetEncryptDataChannelWithNoTLS = 'Can not set EncryptDataChannel when TLS is no used';
  SFTPSetEncryptDataChannelAfterCCC = 'Can not set EncryptDataChannel after CCC command was sent';
  SFTPEncryptionNoCompatibility = 'EncryptDataChannel and UseClearingControlChannel must be False when TLS protocol is no used';
  SFTPSiteToSiteWhenNATFastConnection = 'Site to Site transfer is not permitted when NAT fast connection is used';
  SFTPIPVersionsDifferent       = 'IP versions must be the same';
  SFTPTransferModesDifferent    = 'Transfer modes must be the same';
  SFTPDataProtectionDifferent   = 'Data protection is different on site to site transfer';
  SFTPSSCNNotSupported          = 'SSCN is not supported on both servers';

/// SignalR
  SStartPositionMoreSize        = 'Start position can not be more than Size';
  SStartPositionMoreEnd         = 'Start position can not be more than End position';
  SStartPositionMorePosition    = 'Start position can not be less than Position';
  SPositionMoreSize             = 'Position can not be more than Size';
  SConsumedPositionLessPosition = 'Consumed position can not be less than Position';
  SExaminedPositionLessConsumed = 'Examined position can not be less than Consumed';
  SPositionChanged              = 'Position was be changed in another thread';
  SSizeCanNotBeChanged          = 'Size can not be changed';
  SReadingTimedOut              = 'The reading timed out after %d milliseconds';
  SWritingTimedOut              = 'The writing timed out after %d milliseconds';
  SFlushCanceled                = 'Flush is canceled';

/// SMTP
  SSMTPServerError              = 'SMTP server error occurred: %s';
  SSMTPClientActive             = 'SMTP client is already connected';
  SSMTPClientNotConnected       = 'SMTP client not connected';
  SThisIsMultiPartMessageInMIMEFormat = 'This is a multi-part message in MIME format';
  SAttachCannotBeEncodedWhenTransferEncodingSet = 'Message attachments cannot be encoded when ContentTransferEncoding is set';
  SSASLNotReady                 = 'The specified SASL handlers are not ready';
  SSASLNotSupported             = 'The specified SASL handlers are not supported';
  SSASLMechanismRequired        = 'SASL mechanism is required to authenticate server';
  SUnknownOTPMethod             = 'Unknown OTP method';

type
  TScErrorCode = (
    seSuccess,
  /// Common Errors
    seInternalError, seInvalidInputArgs, seInvalidInputArg, seNotOverriddenMethod,
    seInvalidObjectClass, seAssignError, seInvalidEncData,
    seClientOpened, seServerOpened, seClientClosed,
    seInvalidCipherAlgorithm, seInvalidHashAlgorithm, seInvalidCompressionAlgorithm,
    seInvalidPublicKeyAlgorithm, seInvalidKeyExchangeAlgorithm, seInvalidSignatureAlgorithm,
    seUnknownHashAlgorithm, seUnknownSignatureAlgorithm,
    seCompressionNotSupported, seCompressorNotAssigned,
    seAcceptTimedOut, seConnectionNotSecure, seSourceConnectionNotSecure,
    seInvalidReplyCode, seEventHandlerNotDefined,
  /// SSH
    seConnectionNotDefined, seChannelNotDefined, seConnectionTimeout,
    seCannotSendData, seCannotSendClientData, seCannotListenPF,
    seHostKeysNotFound, seHostKeyNotVerifed, seUnexpectedEOP,
    seNoPacketReceived, seAuthenticationFailed, seTimeoutSession,
    seVerifyKeyExAlgFailed, seVerifyHostKeyFailed, seVerifyEncAlgFailed,
    seVerifyCompressAlgFailed, seVerifyMacAlgFailed,
    seInvalidServerVersion, seNotCompatibleServerVersion,
    seInvalidPacketSize, seInvalidPaddingSize, seEmptyUsername,
    seClientNotDefined, seOpenChannelError, seBadKeyAlgorithm,
    seNotDirectChannel, sePrivateKeyNotFound, seNotEnoughData,
    seUnsupportedSessionType, seUnsupportedRequestType, seServerNotSupportShell,
    seClientE_OutOfRange, seServerF_OutOfRange,
    seRemoteAndDirectSet, seDynamicAndDirectSet, seRemoteAndDynamicSet,
    seChannelFailedAdministrativelyProhibited,
  /// SFTP
    seUnknownReplyExtensionType, seUnknownFileType, seUnknownTextHintValue,
    seUnsupportedAttribute, seUnsupportedBlockMode, seUnsupportedExtensionName,
    seUnsupportedFileAttrData, seServerNotSupportSFTP, seUnsupportedSFTPOperation,
    seUnknownSFTPClientVersion, seUnknownSFTPServerVersion, seSFTPClientActive,
    seInvalidFileHandle, seStreamReadError, seSFTPServerError, sePathNotAllowed,
    seImpersonationLogonUserFail, seImpersonateUserFail, seSFTPInitializationFail,
    seOperationNotAllowedWhenNonBlocking,
    seFileDownloadInterrupted, seFileUploadInterrupted,

  /// SSH & SSL
    seUnexpectedPacketType, seUnknownMessageType, seUnexpectedPacket,
    seInvalidMessage, seInvalidMessageLength, seCorruptMessage,
    seSocketClosed, seConnectionClosed, seConnectionClosedWithMessage,
    seWrongHostname, seWrongPort, seHashVerificationNotCorrespond,
  /// SSL
    seCannotWriteSocketData, seSocketNotConnected, seInvalidEllipticCurveName,
    seUnknownProtocolVersion, seIllegalRandomParameter,
    seIllegalSessionIDParameter, seIllegalKeyShareParameters,
    seErrorServerKeyExchangeNonExportable, seErrorServerKeyExchangePublicKey,
    sePeerCertificateNotReceived, seServerCertificateNotReceived,
    seECurveDomainTypeNotSupported,
    seErrorHandshakeProcedure, seErrorPublicKeyLength, seErrorDiffieHelmanKeyLength,
    seFailureAlert, seNotAgreeOnProtocol, seMissingExtension, seNormalConnectionIsNotSupported,
    seServerCertificateNotSpecified, seCertificateMustBePrivate, seSsl3PsepdoRandomError,
    seInvalidOperation, seWrongExtensionData,
    seInvalidRenegotiationInfo, seRenegotiationDenied,
    seUnknownExtensionClass, seUnsupportedExtension,
    seReceivedExtensionDuplicated, seDataSentAfterShutdown,
    seSessionTicketDenied, seSourceSessionTicketAbsent, seExtensionClassDuplicated,
    seCertificateNotCorrespondToCertificateRequest, seCertificateNotCorrespondToRequiredSignatureAlgorithms,
    seInvalidSignatureSchemeAlgorithm, seCookiesInInitialClientHello,
    seSSLNegotiationCommandFailed, seExtendedMasterSecretModeRequired,
    seDuplicateKExNamedGroup, seSesionTicketTimeExpired,
  /// ScBridge
    seCryptoAPIStorageNotSupportUsers, seCryptoAPIStorageNotSupportCRLs,
    seObjNotReady, seKeyNotReady, seBrokenKey, seStorageNoSet, seIPCorruptData,
    seHashChangedAfterWrite, sePositiveOverflow, seNegativeUnderflow,
    seValueHasNoInverse, seInvalidBigIntegerRadix, seBitCountMultiple,
    seUseGenerateECMethod, seNotPositiveExponents, seCSPError,
    seNotCompatibleFormatWithPublicKeyOnly, seNotCompatibleFormatWithECKey,
    seNotCompatibleFormatWithPassphrase, seNotCompatibleFormatWithComment,
    seNullName, seCannotSignData, seCannotDecryptData, seDSACannotEncrypt, seECCannotEncrypt,
    seCipherNotSupported, seKeyAuthNotSupported, seWrongDataFormat,
    seObjectNotFound, seKeyPathNotFound, seObjectAlreadyExists,
    seKeyNotPrivate, seObjNameMissing, seDuplicateObjName,
    seUserNameMissing, seDuplicateUserName,
    seCertificateNameMissing, seDuplicateCertificateName, seCertificateNotExists,
    seCertificateNotReady, seCRLNotReady, seLargeExpLength,
    seCertificateNotValid, seCertNotValidityPeriod,
    seCertCANotValid, seCertKeyUsageNotValid, seIssuerNotEqualSubject,
    seInvalidSubjectName, seCertNotTrusted, seCertSignatureNotValid,
    seInsecureSignature, seForbiddenSignature, seInvalidPolicies,
    seUnknownCriticalExtension, seCRLNotFound, seCRLIsNotValid,
    seCertificateIsRevoked, seNonCertificate, seEmptyStorageName,
    seErrorRequestingIssuer, seErrorRequestingSubject,
    seCannotFindCertificate, seBadKeyData, seErrorSigningData,
    seErrorVerifyingData, seErrorEncryptingData, seUnableFindCertChain,
    seUnableVerifyCert, seDistinguishedNameNotFound,
    seCertStorageCannotBeChanged, seWrongCertContext, seWrongCRLContext,
    seCertKeyCannotBeChanged, seCannotOpenRegistry, seCannotFindProviderName,
    seChangingReadOnlyStorage, seUnknownAlgorithm, seUnknownMGFId,
    seImproperPaddingMode, seChangingReadOnlyObject, sewNAFError,
    sePointInfinitive, seUnknownECParametersFormat, seUnsupportedECParameters,
    seUnknownEllipticCurveId, seECCryptographyNotInitialized,
    seWrongECPointFormat, seWrongEllipticCurvesParameters, sePreHashTypeNotSupported,
    seWrongPKCS12Context, sePKCS12DataBroken, seCertificateSignNotSet, seCertificateEncNotSet,
    sePasswordNotSpecified, seUnsupportedKeyEncryption, seUnsupportedBCryptParameters,
  /// ASN1
    seCannotConvert, seChoiceLexemNotDefined, seDataTypeIsSet, seLexemNotFound,
  /// CMS
    seCertificateNotCorrespondToRecipient, seNullSubjectKeyIdentifier,
    seStorageAndCertNoSet, seCertificateNotDefined, seUnknownCertFormat,
    seContentDataNotFound, seSignatureNotFound, seInvalidIdentifierType,
    seInvalidRecipientInfo, seRecipientInfoNotSupported, seEncryptionAlgorithmReadOnly,
    seDataNotEncrypted, seDataNotSigned,
    seSmallCertificateKeyLength, seRecipientInfoNotFound,
    seInvalidSignature, seMessageDigestNotExists, seInvalidMessageDigest,
    seAttributeCannotBeIncluded, seNotSignedData, seNotEnvelopedData,
  /// FTP
    seFTPClientActive, seFTPClientNotConnected, seFTPServerError,
    seFTPInvalidPortFormat, seFTPOperationCancelled,
    seFTPSetNoPassiveModeWithUseNATFastConnection,
    seFTPSetUseExtendedDataAddressWithIPv6, seFTPSetUseExtendedDataAddressWithUseNATFastConnection,
    seFTPSetUseClearingControlChannelWithNoTLS, seFTPSetEncryptDataChannelWithNoTLS,
    seFTPSetEncryptDataChannelAfterCCC, seFTPEncryptionNoCompatibility,
    seFTPSiteToSiteWhenNATFastConnection, seFTPIPVersionsDifferent,
    seFTPTransferModesDifferent, seFTPDataProtectionDifferent, seFTPSSCNNotSupported,
  /// SignalR
    seStartPositionMoreSize, seStartPositionMoreEnd, seStartPositionMorePosition,
    sePositionMoreSize, seConsumedPositionLessPosition, seExaminedPositionLessConsumed,
    sePositionChanged, seSizeCanNotBeChanged,
  /// SMTP
    seSMTPServerError, seSMTPClientActive, seSMTPClientNotConnected,
    seAttachCannotBeEncodedWhenTransferEncodingSet,
    seSASLNotReady, seSASLNotSupported, seSASLMechanismRequired, seUnknownOTPMethod
  );

const
  ScErrorMessages: array [TScErrorCode] of string = (
    '',
  /// Common Errors
    SInternalError, SInvalidInputArgs, SInvalidInputArg, SNotOverriddenMethod,
    SInvalidObjectClass, SAssignError, SInvalidEncData,
    SClientOpened, SServerOpened, SClientClosed,
    SInvalidCipherAlgorithm, SInvalidHashAlgorithm, SInvalidCompressionAlgorithm,
    SInvalidPublicKeyAlgorithm, SInvalidKeyExchangeAlgorithm, SInvalidSignatureAlgorithm,
    SUnknownHashAlgorithm, SUnknownSignatureAlgorithm,
    SCompressionNotSupported, SCompressorNotAssigned,
    SAcceptTimedOut, SConnectionNotSecure, SSourceConnectionNotSecure,
    SInvalidReplyCode, SEventHandlerNotDefined,
  /// SSH
    SConnectionNotDefined, SChannelNotDefined, SConnectionTimeout,
    SCannotSendData, SCannotSendClientData, SCannotListenPF,
    SHostKeysNotFound, SHostKeyNotVerifed, SUnexpectedEOP,
    SNoPacketReceived, SAuthenticationFailed, STimeoutSession,
    SVerifyKeyExAlgFailed, SVerifyHostKeyFailed, SVerifyEncAlgFailed,
    SVerifyCompressAlgFailed, SVerifyMacAlgFailed,
    SInvalidServerVersion, SNotCompatibleServerVersion,
    SInvalidPacketSize, SInvalidPaddingSize, SEmptyUsername,
    SClientNotDefined, SOpenChannelError, SBadKeyAlgorithm,
    SNotDirectChannel, SPrivateKeyNotFound, SNotEnoughData,
    SUnsupportedSessionType, SUnsupportedRequestType, SServerNotSupportShell,
    SClientE_OutOfRange, SServerF_OutOfRange,
    SRemoteAndDirectSet, SDynamicAndDirectSet, SRemoteAndDynamicSet,
    SChannelFailedAdministrativelyProhibited,
  /// SFTP
    SUnknownReplyExtensionType, SUnknownFileType, SUnknownTextHintValue,
    SUnsupportedAttribute, SUnsupportedBlockMode, SUnsupportedExtensionName,
    SUnsupportedFileAttrData, SServerNotSupportSFTP, SUnsupportedSFTPOperation,
    SUnknownSFTPClientVersion, SUnknownSFTPServerVersion, SSFTPClientActive,
    SInvalidFileHandle, SStreamReadError, SSFTPServerError, SPathNotAllowed,
    SImpersonationLogonUserFail, SImpersonateUserFail, SSFTPInitializationFail,
    SOperationNotAllowedWhenNonBlocking,
    SFileDownloadInterrupted, SFileUploadInterrupted,

  /// SSH & SSL
    SUnexpectedPacketType, SUnknownMessageType, SUnexpectedPacket,
    SInvalidMessage, SInvalidMessageLength, SCorruptMessage,
    SSocketClosed, SConnectionClosed, SConnectionClosedWithMessage,
    SWrongHostname, SWrongPort, SHashVerificationNotCorrespond,
  /// SSL
    SCannotWriteSocketData, SSocketNotConnected, SInvalidEllipticCurveName,
    SUnknownProtocolVersion, SIllegalRandomParameter,
    SIllegalSessionIDParameter, SIllegalKeyShareParameters,
    SErrorServerKeyExchangeNonExportable, SErrorServerKeyExchangePublicKey,
    SPeerCertificateNotReceived, SServerCertificateNotReceived,
    SECurveDomainTypeNotSupported,
    SErrorHandshakeProcedure, SErrorPublicKeyLength, SErrorDiffieHelmanKeyLength,
    SFailureAlert, SNotAgreeOnProtocol, SMissingExtension, SNormalConnectionIsNotSupported,
    SServerCertificateNotSpecified, SCertificateMustBePrivate, SSsl3PsepdoRandomError,
    SInvalidOperation, SWrongExtensionData,
    SInvalidRenegotiationInfo, SRenegotiationDenied,
    SUnknownExtensionClass, SUnsupportedExtension,
    SReceivedExtensionDuplicated, SDataSentAfterShutdown,
    SSessionTicketDenied, SSourceSessionTicketAbsent, SExtensionClassDuplicated,
    SCertificateNotCorrespondToCertificateRequest, SCertificateNotCorrespondToRequiredSignatureAlgorithms,
    SInvalidSignatureSchemeAlgorithm, SCookiesInInitialClientHello,
    SSSLNegotiationCommandFailed, SExtendedMasterSecretModeRequired,
    SDuplicateKExNamedGroup, SSesionTicketTimeExpired,
  /// ScBridge
    SCryptoAPIStorageNotSupportUsers, SCryptoAPIStorageNotSupportCRLs,
    SObjNotReady, SKeyNotReady, SBrokenKey, SStorageNoSet, SIPCorruptData,
    SHashChangedAfterWrite, SPositiveOverflow, SNegativeUnderflow,
    SValueHasNoInverse, SInvalidBigIntegerRadix, SBitCountMultiple,
    SUseGenerateECMethod, SNotPositiveExponents, SCSPError,
    SNotCompatibleFormatWithPublicKeyOnly, SNotCompatibleFormatWithECKey,
    SNotCompatibleFormatWithPassphrase, SNotCompatibleFormatWithComment,
    SNullName, SCannotSignData, SCannotDecryptData, SDSACannotEncrypt, SECCannotEncrypt,
    SCipherNotSupported, SKeyAuthNotSupported, SWrongDataFormat,
    SObjectNotFound, SKeyPathNotFound, SObjectAlreadyExists,
    SKeyNotPrivate, SObjNameMissing, SDuplicateObjName,
    SUserNameMissing, SDuplicateUserName,
    SCertificateNameMissing, SDuplicateCertificateName, SCertificateNotExists,
    SCertificateNotReady, SCRLNotReady, SLargeExpLength,
    SCertificateNotValid, SCertNotValidityPeriod,
    SCertCANotValid, SCertKeyUsageNotValid, SIssuerNotEqualSubject,
    SInvalidSubjectName, SCertNotTrusted, SCertSignatureNotValid,
    SInsecureSignature, SForbiddenSignature, SInvalidPolicies,
    SUnknownCriticalExtension, SCRLNotFound, SCRLIsNotValid,
    SCertificateIsRevoked, SNonCertificate, SEmptyStorageName,
    SErrorRequestingIssuer, SErrorRequestingSubject,
    SCannotFindCertificate, SBadKeyData, SErrorSigningData,
    SErrorVerifyingData, SErrorEncryptingData, SUnableFindCertChain,
    SUnableVerifyCert, SDistinguishedNameNotFound,
    SCertStorageCannotBeChanged, SWrongCertContext, SWrongCRLContext,
    SCertKeyCannotBeChanged, SCannotOpenRegistry, SCannotFindProviderName,
    SChangingReadOnlyStorage, SUnknownAlgorithmS, SUnknownMGFId,
    SImproperPaddingMode, SChangingReadOnlyObject, SwNAFError,
    SPointInfinitive, SUnknownECParametersFormat, SUnsupportedECParameters,
    SUnknownEllipticCurveId, SECCryptographyNotInitialized,
    SWrongECPointFormat, SWrongEllipticCurvesParameters, SPreHashTypeNotSupported,
    SWrongPKCS12Context, SPKCS12DataBroken, SCertificateSignNotSet, SCertificateEncNotSet,
    SPasswordNotSpecified, SUnsupportedKeyEncryption, SUnsupportedBCryptParameters,
  /// ASN1
    SCannotConvert, SChoiceLexemNotDefined, SDataTypeIsSet, SLexemNotFound,
  /// CMS
    SCertificateNotCorrespondToRecipient, SNullSubjectKeyIdentifier,
    SStorageAndCertNoSet, SCertificateNotDefined, SUnknownCertFormat,
    SContentDataNotFound, SSignatureNotFound, SInvalidIdentifierType,
    SInvalidRecipientInfo, SRecipientInfoNotSupported, SEncryptionAlgorithmReadOnly,
    SDataNotEncrypted, SDataNotSigned,
    SSmallCertificateKeyLength, SRecipientInfoNotFound,
    SInvalidSignature, SMessageDigestNotExists, SInvalidMessageDigest,
    SAttributeCannotBeIncluded, SNotSignedData, SNotEnvelopedData,
  /// FTP
    SFTPClientActive, SFTPClientNotConnected, SFTPServerError,
    SFTPInvalidPortFormat, SFTPOperationCancelled,
    SFTPSetNoPassiveModeWithUseNATFastConnection,
    SFTPSetUseExtendedDataAddressWithIPv6, SFTPSetUseExtendedDataAddressWithUseNATFastConnection,
    SFTPSetUseClearingControlChannelWithNoTLS, SFTPSetEncryptDataChannelWithNoTLS,
    SFTPSetEncryptDataChannelAfterCCC, SFTPEncryptionNoCompatibility,
    SFTPSiteToSiteWhenNATFastConnection, SFTPIPVersionsDifferent,
    SFTPTransferModesDifferent, SFTPDataProtectionDifferent, SFTPSSCNNotSupported,
  /// SignalR
    SStartPositionMoreSize, SStartPositionMoreEnd, SStartPositionMorePosition,
    SPositionMoreSize, SConsumedPositionLessPosition, SExaminedPositionLessConsumed,
    SPositionChanged, SSizeCanNotBeChanged,
  /// SMTP
    SSMTPServerError, SSMTPClientActive, SSMTPClientNotConnected,
    SAttachCannotBeEncodedWhenTransferEncodingSet,
    SSASLNotReady, SSASLNotSupported, SSASLMechanismRequired, SUnknownOTPMethod
  );

  MIME_TYPES: array[0..383, 0..1] of string = (
    ('.aac', 'audio/mp4'),
    ('.aif', 'audio/x-aiff'),
    ('.aifc', 'audio/x-aiff'),
    ('.aiff', 'audio/x-aiff'),
    ('.au', 'audio/basic'),
    ('.gsm', 'audio/x-gsm'),
    ('.kar', 'audio/midi'),
    ('.m3u', 'audio/mpegurl'),
    ('.m4a', 'audio/x-mpg'),
    ('.mid', 'audio/midi'),
    ('.midi', 'audio/midi'),
    ('.mpega', 'audio/x-mpg'),
    ('.mp2', 'audio/x-mpg'),
    ('.mp3', 'audio/x-mpg'),
    ('.mpga', 'audio/x-mpg'),
    ('.m3u', 'audio/x-mpegurl'),
    ('.pls', 'audio/x-scpls'),
    ('.qcp', 'audio/vnd.qcelp'),
    ('.ra', 'audio/x-realaudio'),
    ('.ram', 'audio/x-pn-realaudio'),
    ('.rm', 'audio/x-pn-realaudio'),
    ('.sd2', 'audio/x-sd2'),
    ('.sid', 'audio/prs.sid'),
    ('.snd', 'audio/basic'),
    ('.wav', 'audio/x-wav'),
    ('.wax', 'audio/x-ms-wax'),
    ('.wma', 'audio/x-ms-wma'),
    ('.mjf', 'audio/x-vnd.AudioExplosion.MjuiceMediaFile'),
    ('.nml', 'animation/narrative'),
    ('.art', 'image/x-jg'),
    ('.bmp', 'image/bmp'),
    ('.cdr', 'image/x-coreldraw'),
    ('.cdt', 'image/x-coreldrawtemplate'),
    ('.cpt', 'image/x-corelphotopaint'),
    ('.djv', 'image/vnd.djvu'),
    ('.djvu', 'image/vnd.djvu'),
    ('.gif', 'image/gif'),
    ('.ief', 'image/ief'),
    ('.ico', 'image/x-icon'),
    ('.jng', 'image/x-jng'),
    ('.jpg', 'image/jpeg'),
    ('.jpeg', 'image/jpeg'),
    ('.jpe', 'image/jpeg'),
    ('.pat', 'image/x-coreldrawpattern'),
    ('.pcx', 'image/pcx'),
    ('.pbm', 'image/x-portable-bitmap'),
    ('.pgm', 'image/x-portable-graymap'),
    ('.pict', 'image/x-pict'),
    ('.png', 'image/x-png'),
    ('.pnm', 'image/x-portable-anymap'),
    ('.pntg', 'image/x-macpaint'),
    ('.ppm', 'image/x-portable-pixmap'),
    ('.psd', 'image/x-psd'),
    ('.qtif', 'image/x-quicktime'),
    ('.ras', 'image/x-cmu-raster'),
    ('.rf', 'image/vnd.rn-realflash'),
    ('.rgb', 'image/x-rgb'),
    ('.rp', 'image/vnd.rn-realpix'),
    ('.sgi', 'image/x-sgi'),
    ('.svg', 'image/svg+xml'),
    ('.svgz', 'image/svg+xml'),
    ('.targa', 'image/x-targa'),
    ('.tif', 'image/x-tiff'),
    ('.wbmp', 'image/vnd.wap.wbmp'),
    ('.webp', 'image/webp'),
    ('.xbm', 'image/xbm'),
    ('.xbm', 'image/x-xbitmap'),
    ('.xpm', 'image/x-xpixmap'),
    ('.xwd', 'image/x-xwindowdump'),
    ('.323', 'text/h323'),
    ('.xml', 'text/xml'),
    ('.uls', 'text/iuls'),
    ('.txt', 'text/plain'),
    ('.rtx', 'text/richtext'),
    ('.wsc', 'text/scriptlet'),
    ('.rt', 'text/vnd.rn-realtext'),
    ('.htt', 'text/webviewhtml'),
    ('.htc', 'text/x-component'),
    ('.vcf', 'text/x-vcard'),
    ('.asf', 'video/x-ms-asf'),
    ('.asx', 'video/x-ms-asf'),
    ('.avi', 'video/x-msvideo'),
    ('.dl', 'video/dl'),
    ('.dv', 'video/dv'),
    ('.flc', 'video/flc'),
    ('.fli', 'video/fli'),
    ('.gl', 'video/gl'),
    ('.lsf', 'video/x-la-asf'),
    ('.lsx', 'video/x-la-asf'),
    ('.mng', 'video/x-mng'),
    ('.mp2', 'video/mpeg'),
    ('.mp3', 'video/mpeg'),
    ('.mp4', 'video/mpeg'),
    ('.mpeg', 'video/x-mpeg2a'),
    ('.mpa', 'video/mpeg'),
    ('.mpe', 'video/mpeg'),
    ('.mpg', 'video/mpeg'),
    ('.ogv', 'video/ogg'),
    ('.moov', 'video/quicktime'),
    ('.mov', 'video/quicktime'),
    ('.mxu', 'video/vnd.mpegurl'),
    ('.qt', 'video/quicktime'),
    ('.qtc', 'video/x-qtc'),
    ('.rv', 'video/vnd.rn-realvideo'),
    ('.ivf', 'video/x-ivf'),
    ('.webm', 'video/webm'),
    ('.wm', 'video/x-ms-wm'),
    ('.wmp', 'video/x-ms-wmp'),
    ('.wmv', 'video/x-ms-wmv'),
    ('.wmx', 'video/x-ms-wmx'),
    ('.wvx', 'video/x-ms-wvx'),
    ('.rms', 'video/vnd.rn-realvideo-secure'),
    ('.asx', 'video/x-ms-asf-plugin'),
    ('.movie', 'video/x-sgi-movie'),
    ('.7z', 'application/x-7z-compressed'),
    ('.a', 'application/x-archive'),
    ('.aab', 'application/x-authorware-bin'),
    ('.aam', 'application/x-authorware-map'),
    ('.aas', 'application/x-authorware-seg'),
    ('.abw', 'application/x-abiword'),
    ('.ace', 'application/x-ace-compressed'),
    ('.ai', 'application/postscript'),
    ('.alz', 'application/x-alz-compressed'),
    ('.ani', 'application/x-navi-animation'),
    ('.arj', 'application/x-arj'),
    ('.asf', 'application/vnd.ms-asf'),
    ('.bat', 'application/x-msdos-program'),
    ('.bcpio', 'application/x-bcpio'),
    ('.boz', 'application/x-bzip2'),
    ('.bz', 'application/x-bzip'),
    ('.bz2', 'application/x-bzip2'),
    ('.cab', 'application/vnd.ms-cab-compressed'),
    ('.cat', 'application/vnd.ms-pki.seccat'),
    ('.ccn', 'application/x-cnc'),
    ('.cco', 'application/x-cocoa'),
    ('.cdf', 'application/x-cdf'),
    ('.cer', 'application/x-x509-ca-cert'),
    ('.chm', 'application/vnd.ms-htmlhelp'),
    ('.chrt', 'application/vnd.kde.kchart'),
    ('.cil', 'application/vnd.ms-artgalry'),
    ('.class', 'application/java-vm'),
    ('.com', 'application/x-msdos-program'),
    ('.clp', 'application/x-msclip'),
    ('.cpio', 'application/x-cpio'),
    ('.cpt', 'application/mac-compactpro'),
    ('.cqk', 'application/x-calquick'),
    ('.crd', 'application/x-mscardfile'),
    ('.crl', 'application/pkix-crl'),
    ('.csh', 'application/x-csh'),
    ('.dar', 'application/x-dar'),
    ('.dbf', 'application/x-dbase'),
    ('.dcr', 'application/x-director'),
    ('.deb', 'application/x-debian-package'),
    ('.dir', 'application/x-director'),
    ('.dist', 'vnd.apple.installer+xml'),
    ('.distz', 'vnd.apple.installer+xml'),
    ('.dll', 'application/x-msdos-program'),
    ('.dmg', 'application/x-apple-diskimage'),
    ('.doc', 'application/msword'),
    ('.dot', 'application/msword'),
    ('.dvi', 'application/x-dvi'),
    ('.dxr', 'application/x-director'),
    ('.ebk', 'application/x-expandedbook'),
    ('.eps', 'application/postscript'),
    ('.evy', 'application/envoy'),
    ('.exe', 'application/x-msdos-program'),
    ('.fdf', 'application/vnd.fdf'),
    ('.fif', 'application/fractals'),
    ('.flm', 'application/vnd.kde.kivio'),
    ('.fml', 'application/x-file-mirror-list'),
    ('.gzip', 'application/x-gzip'),
    ('.gnumeric', 'application/x-gnumeric'),
    ('.gtar', 'application/x-gtar'),
    ('.gz', 'application/x-gzip'),
    ('.hdf', 'application/x-hdf'),
    ('.hlp', 'application/winhlp'),
    ('.hpf', 'application/x-icq-hpf'),
    ('.hqx', 'application/mac-binhex40'),
    ('.hta', 'application/hta'),
    ('.ims', 'application/vnd.ms-ims'),
    ('.ins', 'application/x-internet-signup'),
    ('.iii', 'application/x-iphone'),
    ('.iso', 'application/x-iso9660-image'),
    ('.jar', 'application/java-archive'),
    ('.karbon', 'application/vnd.kde.karbon'),
    ('.kfo', 'application/vnd.kde.kformula'),
    ('.kon', 'application/vnd.kde.kontour'),
    ('.kpr', 'application/vnd.kde.kpresenter'),
    ('.kpt', 'application/vnd.kde.kpresenter'),
    ('.kwd', 'application/vnd.kde.kword'),
    ('.kwt', 'application/vnd.kde.kword'),
    ('.latex', 'application/x-latex'),
    ('.lha', 'application/x-lzh'),
    ('.lcc', 'application/fastman'),
    ('.lrm', 'application/vnd.ms-lrm'),
    ('.lz', 'application/x-lzip'),
    ('.lzh', 'application/x-lzh'),
    ('.lzma', 'application/x-lzma'),
    ('.lzo', 'application/x-lzop'),
    ('.lzx', 'application/x-lzx'),
    ('.m13', 'application/x-msmediaview'),
    ('.m14', 'application/x-msmediaview'),
    ('.mpp', 'application/vnd.ms-project'),
    ('.mvb', 'application/x-msmediaview'),
    ('.man', 'application/x-troff-man'),
    ('.mdb', 'application/x-msaccess'),
    ('.me', 'application/x-troff-me'),
    ('.ms', 'application/x-troff-ms'),
    ('.msi', 'application/x-msi'),
    ('.mpkg', 'vnd.apple.installer+xml'),
    ('.mny', 'application/x-msmoney'),
    ('.nix', 'application/x-mix-transfer'),
    ('.o', 'application/x-object'),
    ('.oda', 'application/oda'),
    ('.odb', 'application/vnd.oasis.opendocument.database'),
    ('.odc', 'application/vnd.oasis.opendocument.chart'),
    ('.odf', 'application/vnd.oasis.opendocument.formula'),
    ('.odg', 'application/vnd.oasis.opendocument.graphics'),
    ('.odi', 'application/vnd.oasis.opendocument.image'),
    ('.odm', 'application/vnd.oasis.opendocument.text-master'),
    ('.odp', 'application/vnd.oasis.opendocument.presentation'),
    ('.ods', 'application/vnd.oasis.opendocument.spreadsheet'),
    ('.ogg', 'application/ogg'),
    ('.odt', 'application/vnd.oasis.opendocument.text'),
    ('.otg', 'application/vnd.oasis.opendocument.graphics-template'),
    ('.oth', 'application/vnd.oasis.opendocument.text-web'),
    ('.otp', 'application/vnd.oasis.opendocument.presentation-template'),
    ('.ots', 'application/vnd.oasis.opendocument.spreadsheet-template'),
    ('.ott', 'application/vnd.oasis.opendocument.text-template'),
    ('.p10', 'application/pkcs10'),
    ('.p12', 'application/x-pkcs12'),
    ('.p7b', 'application/x-pkcs7-certificates'),
    ('.p7m', 'application/pkcs7-mime'),
    ('.p7r', 'application/x-pkcs7-certreqresp'),
    ('.p7s', 'application/pkcs7-signature'),
    ('.package', 'application/vnd.autopackage'),
    ('.pfr', 'application/font-tdpfr'),
    ('.pkg', 'vnd.apple.installer+xml'),
    ('.pdf', 'application/pdf'),
    ('.pko', 'application/vnd.ms-pki.pko'),
    ('.pl', 'application/x-perl'),
    ('.pnq', 'application/x-icq-pnq'),
    ('.pot', 'application/mspowerpoint'),
    ('.pps', 'application/mspowerpoint'),
    ('.ppt', 'application/mspowerpoint'),
    ('.ppz', 'application/mspowerpoint'),
    ('.ps', 'application/postscript'),
    ('.pub', 'application/x-mspublisher'),
    ('.qpw', 'application/x-quattropro'),
    ('.qtl', 'application/x-quicktimeplayer'),
    ('.rar', 'application/rar'),
    ('.rdf', 'application/rdf+xml'),
    ('.rjs', 'application/vnd.rn-realsystem-rjs'),
    ('.rm', 'application/vnd.rn-realmedia'),
    ('.rmf', 'application/vnd.rmf'),
    ('.rmp', 'application/vnd.rn-rn_music_package'),
    ('.rmx', 'application/vnd.rn-realsystem-rmx'),
    ('.rnx', 'application/vnd.rn-realplayer'),
    ('.rpm', 'application/x-redhat-package-manager'),
    ('.rsml', 'application/vnd.rn-rsml'),
    ('.rtsp', 'application/x-rtsp'),
    ('.rss', 'application/rss+xml'),
    ('.scm', 'application/x-icq-scm'),
    ('.ser', 'application/java-serialized-object'),
    ('.scd', 'application/x-msschedule'),
    ('.sda', 'application/vnd.stardivision.draw'),
    ('.sdc', 'application/vnd.stardivision.calc'),
    ('.sdd', 'application/vnd.stardivision.impress'),
    ('.sdp', 'application/x-sdp'),
    ('.setpay', 'application/set-payment-initiation'),
    ('.setreg', 'application/set-registration-initiation'),
    ('.sh', 'application/x-sh'),
    ('.shar', 'application/x-shar'),
    ('.shw', 'application/presentations'),
    ('.sit', 'application/x-stuffit'),
    ('.sitx', 'application/x-stuffitx'),
    ('.skd', 'application/x-koan'),
    ('.skm', 'application/x-koan'),
    ('.skp', 'application/x-koan'),
    ('.skt', 'application/x-koan'),
    ('.smf', 'application/vnd.stardivision.math'),
    ('.smi', 'application/smil'),
    ('.smil', 'application/smil'),
    ('.spl', 'application/futuresplash'),
    ('.ssm', 'application/streamingmedia'),
    ('.sst', 'application/vnd.ms-pki.certstore'),
    ('.stc', 'application/vnd.sun.xml.calc.template'),
    ('.std', 'application/vnd.sun.xml.draw.template'),
    ('.sti', 'application/vnd.sun.xml.impress.template'),
    ('.stl', 'application/vnd.ms-pki.stl'),
    ('.stw', 'application/vnd.sun.xml.writer.template'),
    ('.svi', 'application/softvision'),
    ('.sv4cpio', 'application/x-sv4cpio'),
    ('.sv4crc', 'application/x-sv4crc'),
    ('.swf', 'application/x-shockwave-flash'),
    ('.swf1', 'application/x-shockwave-flash'),
    ('.sxc', 'application/vnd.sun.xml.calc'),
    ('.sxi', 'application/vnd.sun.xml.impress'),
    ('.sxm', 'application/vnd.sun.xml.math'),
    ('.sxw', 'application/vnd.sun.xml.writer'),
    ('.sxg', 'application/vnd.sun.xml.writer.global'),
    ('.t', 'application/x-troff'),
    ('.tar', 'application/x-tar'),
    ('.tcl', 'application/x-tcl'),
    ('.tex', 'application/x-tex'),
    ('.texi', 'application/x-texinfo'),
    ('.texinfo', 'application/x-texinfo'),
    ('.tbz', 'application/x-bzip-compressed-tar'),
    ('.tbz2', 'application/x-bzip-compressed-tar'),
    ('.tgz', 'application/x-compressed-tar'),
    ('.tlz', 'application/x-lzma-compressed-tar'),
    ('.tr', 'application/x-troff'),
    ('.trm', 'application/x-msterminal'),
    ('.troff', 'application/x-troff'),
    ('.tsp', 'application/dsptype'),
    ('.torrent', 'application/x-bittorrent'),
    ('.ttz', 'application/t-time'),
    ('.txz', 'application/x-xz-compressed-tar'),
    ('.udeb', 'application/x-debian-package'),
    ('.uin', 'application/x-icq'),
    ('.urls', 'application/x-url-list'),
    ('.ustar', 'application/x-ustar'),
    ('.vcd', 'application/x-cdlink'),
    ('.vor', 'application/vnd.stardivision.writer'),
    ('.vsl', 'application/x-cnet-vsl'),
    ('.wcm', 'application/vnd.ms-works'),
    ('.wb1', 'application/x-quattropro'),
    ('.wb2', 'application/x-quattropro'),
    ('.wb3', 'application/x-quattropro'),
    ('.wdb', 'application/vnd.ms-works'),
    ('.wks', 'application/vnd.ms-works'),
    ('.wmd', 'application/x-ms-wmd'),
    ('.wms', 'application/x-ms-wms'),
    ('.wmz', 'application/x-ms-wmz'),
    ('.wp5', 'application/wordperfect5.1'),
    ('.wpd', 'application/wordperfect'),
    ('.wpl', 'application/vnd.ms-wpl'),
    ('.wps', 'application/vnd.ms-works'),
    ('.wri', 'application/x-mswrite'),
    ('.xfdf', 'application/vnd.adobe.xfdf'),
    ('.xls', 'application/x-msexcel'),
    ('.xlb', 'application/x-msexcel'),
    ('.xpi', 'application/x-xpinstall'),
    ('.xps', 'application/vnd.ms-xpsdocument'),
    ('.xsd', 'application/vnd.sun.xml.draw'),
    ('.xul', 'application/vnd.mozilla.xul+xml'),
    ('.z', 'application/x-compress'),
    ('.zoo', 'application/x-zoo'),
    ('.zip', 'application/x-zip-compressed'),
    ('.wbmp', 'image/vnd.wap.wbmp'),
    ('.wml', 'text/vnd.wap.wml'),
    ('.wmlc', 'application/vnd.wap.wmlc'),
    ('.wmls', 'text/vnd.wap.wmlscript'),
    ('.wmlsc', 'application/vnd.wap.wmlscriptc'),
    ('.asm', 'text/x-asm'),
    ('.p', 'text/x-pascal'),
    ('.pas', 'text/x-pascal'),
    ('.cs', 'text/x-csharp'),
    ('.c', 'text/x-csrc'),
    ('.c++', 'text/x-c++src'),
    ('.cpp', 'text/x-c++src'),
    ('.cxx', 'text/x-c++src'),
    ('.cc', 'text/x-c++src'),
    ('.h', 'text/x-chdr'),
    ('.h++', 'text/x-c++hdr'),
    ('.hpp', 'text/x-c++hdr'),
    ('.hxx', 'text/x-c++hdr'),
    ('.hh', 'text/x-c++hdr'),
    ('.java', 'text/x-java'),
    ('.css', 'text/css'),
    ('.js', 'text/javascript'),
    ('.htm', 'text/html'),
    ('.html', 'text/html'),
    ('.xhtml', 'application/xhtml+xml'),
    ('.xht', 'application/xhtml+xml'),
    ('.rdf', 'application/rdf+xml'),
    ('.rss', 'application/rss+xml'),
    ('.ls', 'text/javascript'),
    ('.mocha', 'text/javascript'),
    ('.shtml', 'server-parsed-html'),
    ('.xml', 'text/xml'),
    ('.sgm', 'text/sgml'),
    ('.sgml', 'text/sgml'),
    ('.mht', 'message/rfc822')
  );

  OTP_DICTIONARY: array[0..2047] of string = (
    'A', 'ABE', 'ACE', 'ACT', 'AD', 'ADA', 'ADD',
    'AGO', 'AID', 'AIM', 'AIR', 'ALL', 'ALP', 'AM', 'AMY',
    'AN', 'ANA', 'AND', 'ANN', 'ANT', 'ANY', 'APE', 'APS',
    'APT', 'ARC', 'ARE', 'ARK', 'ARM', 'ART', 'AS', 'ASH',
    'ASK', 'AT', 'ATE', 'AUG', 'AUK', 'AVE', 'AWE', 'AWK',
    'AWL', 'AWN', 'AX', 'AYE', 'BAD', 'BAG', 'BAH', 'BAM',
    'BAN', 'BAR', 'BAT', 'BAY', 'BE', 'BED', 'BEE', 'BEG',
    'BEN', 'BET', 'BEY', 'BIB', 'BID', 'BIG', 'BIN', 'BIT',
    'BOB', 'BOG', 'BON', 'BOO', 'BOP', 'BOW', 'BOY', 'BUB',
    'BUD', 'BUG', 'BUM', 'BUN', 'BUS', 'BUT', 'BUY', 'BY',
    'BYE', 'CAB', 'CAL', 'CAM', 'CAN', 'CAP', 'CAR', 'CAT',
    'CAW', 'COD', 'COG', 'COL', 'CON', 'COO', 'COP', 'COT',
    'COW', 'COY', 'CRY', 'CUB', 'CUE', 'CUP', 'CUR', 'CUT',
    'DAB', 'DAD', 'DAM', 'DAN', 'DAR', 'DAY', 'DEE', 'DEL',
    'DEN', 'DES', 'DEW', 'DID', 'DIE', 'DIG', 'DIN', 'DIP',
    'DO', 'DOE', 'DOG', 'DON', 'DOT', 'DOW', 'DRY', 'DUB',
    'DUD', 'DUE', 'DUG', 'DUN', 'EAR', 'EAT', 'ED', 'EEL',
    'EGG', 'EGO', 'ELI', 'ELK', 'ELM', 'ELY', 'EM', 'END',
    'EST', 'ETC', 'EVA', 'EVE', 'EWE', 'EYE', 'FAD', 'FAN',
    'FAR', 'FAT', 'FAY', 'FED', 'FEE', 'FEW', 'FIB', 'FIG',
    'FIN', 'FIR', 'FIT', 'FLO', 'FLY', 'FOE', 'FOG', 'FOR',
    'FRY', 'FUM', 'FUN', 'FUR', 'GAB', 'GAD', 'GAG', 'GAL',
    'GAM', 'GAP', 'GAS', 'GAY', 'GEE', 'GEL', 'GEM', 'GET',
    'GIG', 'GIL', 'GIN', 'GO', 'GOT', 'GUM', 'GUN', 'GUS',
    'GUT', 'GUY', 'GYM', 'GYP', 'HA', 'HAD', 'HAL', 'HAM',
    'HAN', 'HAP', 'HAS', 'HAT', 'HAW', 'HAY', 'HE', 'HEM',
    'HEN', 'HER', 'HEW', 'HEY', 'HI', 'HID', 'HIM', 'HIP',
    'HIS', 'HIT', 'HO', 'HOB', 'HOC', 'HOE', 'HOG', 'HOP',
    'HOT', 'HOW', 'HUB', 'HUE', 'HUG', 'HUH', 'HUM', 'HUT',
    'I', 'ICY', 'IDA', 'IF', 'IKE', 'ILL', 'INK', 'INN',
    'IO', 'ION', 'IQ', 'IRA', 'IRE', 'IRK', 'IS', 'IT',
    'ITS', 'IVY', 'JAB', 'JAG', 'JAM', 'JAN', 'JAR', 'JAW',
    'JAY', 'JET', 'JIG', 'JIM', 'JO', 'JOB', 'JOE', 'JOG',
    'JOT', 'JOY', 'JUG', 'JUT', 'KAY', 'KEG', 'KEN', 'KEY',
    'KID', 'KIM', 'KIN', 'KIT', 'LA', 'LAB', 'LAC', 'LAD',
    'LAG', 'LAM', 'LAP', 'LAW', 'LAY', 'LEA', 'LED', 'LEE',
    'LEG', 'LEN', 'LEO', 'LET', 'LEW', 'LID', 'LIE', 'LIN',
    'LIP', 'LIT', 'LO', 'LOB', 'LOG', 'LOP', 'LOS', 'LOT',
    'LOU', 'LOW', 'LOY', 'LUG', 'LYE', 'MA', 'MAC', 'MAD',
    'MAE', 'MAN', 'MAO', 'MAP', 'MAT', 'MAW', 'MAY', 'ME',
    'MEG', 'MEL', 'MEN', 'MET', 'MEW', 'MID', 'MIN', 'MIT',
    'MOB', 'MOD', 'MOE', 'MOO', 'MOP', 'MOS', 'MOT', 'MOW',
    'MUD', 'MUG', 'MUM', 'MY', 'NAB', 'NAG', 'NAN', 'NAP',
    'NAT', 'NAY', 'NE', 'NED', 'NEE', 'NET', 'NEW', 'NIB',
    'NIL', 'NIP', 'NIT', 'NO', 'NOB', 'NOD', 'NON', 'NOR',
    'NOT', 'NOV', 'NOW', 'NU', 'NUN', 'NUT', 'O', 'OAF',
    'OAK', 'OAR', 'OAT', 'ODD', 'ODE', 'OF', 'OFF', 'OFT',
    'OH', 'OIL', 'OK', 'OLD', 'ON', 'ONE', 'OR', 'ORB',
    'ORE', 'ORR', 'OS', 'OTT', 'OUR', 'OUT', 'OVA', 'OW',
    'OWE', 'OWL', 'OWN', 'OX', 'PA', 'PAD', 'PAL', 'PAM',
    'PAN', 'PAP', 'PAR', 'PAT', 'PAW', 'PAY', 'PEA', 'PEG',
    'PEN', 'PEP', 'PER', 'PET', 'PEW', 'PHI', 'PI', 'PIE',
    'PIN', 'PIT', 'PLY', 'PO', 'POD', 'POE', 'POP', 'POT',
    'POW', 'PRO', 'PRY', 'PUB', 'PUG', 'PUN', 'PUP', 'PUT',
    'QUO', 'RAG', 'RAM', 'RAN', 'RAP', 'RAT', 'RAW', 'RAY',
    'REB', 'RED', 'REP', 'RET', 'RIB', 'RID', 'RIG', 'RIM',
    'RIO', 'RIP', 'ROB', 'ROD', 'ROE', 'RON', 'ROT', 'ROW',
    'ROY', 'RUB', 'RUE', 'RUG', 'RUM', 'RUN', 'RYE', 'SAC',
    'SAD', 'SAG', 'SAL', 'SAM', 'SAN', 'SAP', 'SAT', 'SAW',
    'SAY', 'SEA', 'SEC', 'SEE', 'SEN', 'SET', 'SEW', 'SHE',
    'SHY', 'SIN', 'SIP', 'SIR', 'SIS', 'SIT', 'SKI', 'SKY',
    'SLY', 'SO', 'SOB', 'SOD', 'SON', 'SOP', 'SOW', 'SOY',
    'SPA', 'SPY', 'SUB', 'SUD', 'SUE', 'SUM', 'SUN', 'SUP',
    'TAB', 'TAD', 'TAG', 'TAN', 'TAP', 'TAR', 'TEA', 'TED',
    'TEE', 'TEN', 'THE', 'THY', 'TIC', 'TIE', 'TIM', 'TIN',
    'TIP', 'TO', 'TOE', 'TOG', 'TOM', 'TON', 'TOO', 'TOP',
    'TOW', 'TOY', 'TRY', 'TUB', 'TUG', 'TUM', 'TUN', 'TWO',
    'UN', 'UP', 'US', 'USE', 'VAN', 'VAT', 'VET', 'VIE',
    'WAD', 'WAG', 'WAR', 'WAS', 'WAY', 'WE', 'WEB', 'WED',
    'WEE', 'WET', 'WHO', 'WHY', 'WIN', 'WIT', 'WOK', 'WON',
    'WOO', 'WOW', 'WRY', 'WU', 'YAM', 'YAP', 'YAW', 'YE',
    'YEA', 'YES', 'YET', 'YOU', 'ABED', 'ABEL', 'ABET', 'ABLE',
    'ABUT', 'ACHE', 'ACID', 'ACME', 'ACRE', 'ACTA', 'ACTS', 'ADAM',
    'ADDS', 'ADEN', 'AFAR', 'AFRO', 'AGEE', 'AHEM', 'AHOY', 'AIDA',
    'AIDE', 'AIDS', 'AIRY', 'AJAR', 'AKIN', 'ALAN', 'ALEC', 'ALGA',
    'ALIA', 'ALLY', 'ALMA', 'ALOE', 'ALSO', 'ALTO', 'ALUM', 'ALVA',
    'AMEN', 'AMES', 'AMID', 'AMMO', 'AMOK', 'AMOS', 'AMRA', 'ANDY',
    'ANEW', 'ANNA', 'ANNE', 'ANTE', 'ANTI', 'AQUA', 'ARAB', 'ARCH',
    'AREA', 'ARGO', 'ARID', 'ARMY', 'ARTS', 'ARTY', 'ASIA', 'ASKS',
    'ATOM', 'AUNT', 'AURA', 'AUTO', 'AVER', 'AVID', 'AVIS', 'AVON',
    'AVOW', 'AWAY', 'AWRY', 'BABE', 'BABY', 'BACH', 'BACK', 'BADE',
    'BAIL', 'BAIT', 'BAKE', 'BALD', 'BALE', 'BALI', 'BALK', 'BALL',
    'BALM', 'BAND', 'BANE', 'BANG', 'BANK', 'BARB', 'BARD', 'BARE',
    'BARK', 'BARN', 'BARR', 'BASE', 'BASH', 'BASK', 'BASS', 'BATE',
    'BATH', 'BAWD', 'BAWL', 'BEAD', 'BEAK', 'BEAM', 'BEAN', 'BEAR',
    'BEAT', 'BEAU', 'BECK', 'BEEF', 'BEEN', 'BEER', 'BEET', 'BELA',
    'BELL', 'BELT', 'BEND', 'BENT', 'BERG', 'BERN', 'BERT', 'BESS',
    'BEST', 'BETA', 'BETH', 'BHOY', 'BIAS', 'BIDE', 'BIEN', 'BILE',
    'BILK', 'BILL', 'BIND', 'BING', 'BIRD', 'BITE', 'BITS', 'BLAB',
    'BLAT', 'BLED', 'BLEW', 'BLOB', 'BLOC', 'BLOT', 'BLOW', 'BLUE',
    'BLUM', 'BLUR', 'BOAR', 'BOAT', 'BOCA', 'BOCK', 'BODE', 'BODY',
    'BOGY', 'BOHR', 'BOIL', 'BOLD', 'BOLO', 'BOLT', 'BOMB', 'BONA',
    'BOND', 'BONE', 'BONG', 'BONN', 'BONY', 'BOOK', 'BOOM', 'BOON',
    'BOOT', 'BORE', 'BORG', 'BORN', 'BOSE', 'BOSS', 'BOTH', 'BOUT',
    'BOWL', 'BOYD', 'BRAD', 'BRAE', 'BRAG', 'BRAN', 'BRAY', 'BRED',
    'BREW', 'BRIG', 'BRIM', 'BROW', 'BUCK', 'BUDD', 'BUFF', 'BULB',
    'BULK', 'BULL', 'BUNK', 'BUNT', 'BUOY', 'BURG', 'BURL', 'BURN',
    'BURR', 'BURT', 'BURY', 'BUSH', 'BUSS', 'BUST', 'BUSY', 'BYTE',
    'CADY', 'CAFE', 'CAGE', 'CAIN', 'CAKE', 'CALF', 'CALL', 'CALM',
    'CAME', 'CANE', 'CANT', 'CARD', 'CARE', 'CARL', 'CARR', 'CART',
    'CASE', 'CASH', 'CASK', 'CAST', 'CAVE', 'CEIL', 'CELL', 'CENT',
    'CERN', 'CHAD', 'CHAR', 'CHAT', 'CHAW', 'CHEF', 'CHEN', 'CHEW',
    'CHIC', 'CHIN', 'CHOU', 'CHOW', 'CHUB', 'CHUG', 'CHUM', 'CITE',
    'CITY', 'CLAD', 'CLAM', 'CLAN', 'CLAW', 'CLAY', 'CLOD', 'CLOG',
    'CLOT', 'CLUB', 'CLUE', 'COAL', 'COAT', 'COCA', 'COCK', 'COCO',
    'CODA', 'CODE', 'CODY', 'COED', 'COIL', 'COIN', 'COKE', 'COLA',
    'COLD', 'COLT', 'COMA', 'COMB', 'COME', 'COOK', 'COOL', 'COON',
    'COOT', 'CORD', 'CORE', 'CORK', 'CORN', 'COST', 'COVE', 'COWL',
    'CRAB', 'CRAG', 'CRAM', 'CRAY', 'CREW', 'CRIB', 'CROW', 'CRUD',
    'CUBA', 'CUBE', 'CUFF', 'CULL', 'CULT', 'CUNY', 'CURB', 'CURD',
    'CURE', 'CURL', 'CURT', 'CUTS', 'DADE', 'DALE', 'DAME', 'DANA',
    'DANE', 'DANG', 'DANK', 'DARE', 'DARK', 'DARN', 'DART', 'DASH',
    'DATA', 'DATE', 'DAVE', 'DAVY', 'DAWN', 'DAYS', 'DEAD', 'DEAF',
    'DEAL', 'DEAN', 'DEAR', 'DEBT', 'DECK', 'DEED', 'DEEM', 'DEER',
    'DEFT', 'DEFY', 'DELL', 'DENT', 'DENY', 'DESK', 'DIAL', 'DICE',
    'DIED', 'DIET', 'DIME', 'DINE', 'DING', 'DINT', 'DIRE', 'DIRT',
    'DISC', 'DISH', 'DISK', 'DIVE', 'DOCK', 'DOES', 'DOLE', 'DOLL',
    'DOLT', 'DOME', 'DONE', 'DOOM', 'DOOR', 'DORA', 'DOSE', 'DOTE',
    'DOUG', 'DOUR', 'DOVE', 'DOWN', 'DRAB', 'DRAG', 'DRAM', 'DRAW',
    'DREW', 'DRUB', 'DRUG', 'DRUM', 'DUAL', 'DUCK', 'DUCT', 'DUEL',
    'DUET', 'DUKE', 'DULL', 'DUMB', 'DUNE', 'DUNK', 'DUSK', 'DUST',
    'DUTY', 'EACH', 'EARL', 'EARN', 'EASE', 'EAST', 'EASY', 'EBEN',
    'ECHO', 'EDDY', 'EDEN', 'EDGE', 'EDGY', 'EDIT', 'EDNA', 'EGAN',
    'ELAN', 'ELBA', 'ELLA', 'ELSE', 'EMIL', 'EMIT', 'EMMA', 'ENDS',
    'ERIC', 'EROS', 'EVEN', 'EVER', 'EVIL', 'EYED', 'FACE', 'FACT',
    'FADE', 'FAIL', 'FAIN', 'FAIR', 'FAKE', 'FALL', 'FAME', 'FANG',
    'FARM', 'FAST', 'FATE', 'FAWN', 'FEAR', 'FEAT', 'FEED', 'FEEL',
    'FEET', 'FELL', 'FELT', 'FEND', 'FERN', 'FEST', 'FEUD', 'FIEF',
    'FIGS', 'FILE', 'FILL', 'FILM', 'FIND', 'FINE', 'FINK', 'FIRE',
    'FIRM', 'FISH', 'FISK', 'FIST', 'FITS', 'FIVE', 'FLAG', 'FLAK',
    'FLAM', 'FLAT', 'FLAW', 'FLEA', 'FLED', 'FLEW', 'FLIT', 'FLOC',
    'FLOG', 'FLOW', 'FLUB', 'FLUE', 'FOAL', 'FOAM', 'FOGY', 'FOIL',
    'FOLD', 'FOLK', 'FOND', 'FONT', 'FOOD', 'FOOL', 'FOOT', 'FORD',
    'FORE', 'FORK', 'FORM', 'FORT', 'FOSS', 'FOUL', 'FOUR', 'FOWL',
    'FRAU', 'FRAY', 'FRED', 'FREE', 'FRET', 'FREY', 'FROG', 'FROM',
    'FUEL', 'FULL', 'FUME', 'FUND', 'FUNK', 'FURY', 'FUSE', 'FUSS',
    'GAFF', 'GAGE', 'GAIL', 'GAIN', 'GAIT', 'GALA', 'GALE', 'GALL',
    'GALT', 'GAME', 'GANG', 'GARB', 'GARY', 'GASH', 'GATE', 'GAUL',
    'GAUR', 'GAVE', 'GAWK', 'GEAR', 'GELD', 'GENE', 'GENT', 'GERM',
    'GETS', 'GIBE', 'GIFT', 'GILD', 'GILL', 'GILT', 'GINA', 'GIRD',
    'GIRL', 'GIST', 'GIVE', 'GLAD', 'GLEE', 'GLEN', 'GLIB', 'GLOB',
    'GLOM', 'GLOW', 'GLUE', 'GLUM', 'GLUT', 'GOAD', 'GOAL', 'GOAT',
    'GOER', 'GOES', 'GOLD', 'GOLF', 'GONE', 'GONG', 'GOOD', 'GOOF',
    'GORE', 'GORY', 'GOSH', 'GOUT', 'GOWN', 'GRAB', 'GRAD', 'GRAY',
    'GREG', 'GREW', 'GREY', 'GRID', 'GRIM', 'GRIN', 'GRIT', 'GROW',
    'GRUB', 'GULF', 'GULL', 'GUNK', 'GURU', 'GUSH', 'GUST', 'GWEN',
    'GWYN', 'HAAG', 'HAAS', 'HACK', 'HAIL', 'HAIR', 'HALE', 'HALF',
    'HALL', 'HALO', 'HALT', 'HAND', 'HANG', 'HANK', 'HANS', 'HARD',
    'HARK', 'HARM', 'HART', 'HASH', 'HAST', 'HATE', 'HATH', 'HAUL',
    'HAVE', 'HAWK', 'HAYS', 'HEAD', 'HEAL', 'HEAR', 'HEAT', 'HEBE',
    'HECK', 'HEED', 'HEEL', 'HEFT', 'HELD', 'HELL', 'HELM', 'HERB',
    'HERD', 'HERE', 'HERO', 'HERS', 'HESS', 'HEWN', 'HICK', 'HIDE',
    'HIGH', 'HIKE', 'HILL', 'HILT', 'HIND', 'HINT', 'HIRE', 'HISS',
    'HIVE', 'HOBO', 'HOCK', 'HOFF', 'HOLD', 'HOLE', 'HOLM', 'HOLT',
    'HOME', 'HONE', 'HONK', 'HOOD', 'HOOF', 'HOOK', 'HOOT', 'HORN',
    'HOSE', 'HOST', 'HOUR', 'HOVE', 'HOWE', 'HOWL', 'HOYT', 'HUCK',
    'HUED', 'HUFF', 'HUGE', 'HUGH', 'HUGO', 'HULK', 'HULL', 'HUNK',
    'HUNT', 'HURD', 'HURL', 'HURT', 'HUSH', 'HYDE', 'HYMN', 'IBIS',
    'ICON', 'IDEA', 'IDLE', 'IFFY', 'INCA', 'INCH', 'INTO', 'IONS',
    'IOTA', 'IOWA', 'IRIS', 'IRMA', 'IRON', 'ISLE', 'ITCH', 'ITEM',
    'IVAN', 'JACK', 'JADE', 'JAIL', 'JAKE', 'JANE', 'JAVA', 'JEAN',
    'JEFF', 'JERK', 'JESS', 'JEST', 'JIBE', 'JILL', 'JILT', 'JIVE',
    'JOAN', 'JOBS', 'JOCK', 'JOEL', 'JOEY', 'JOHN', 'JOIN', 'JOKE',
    'JOLT', 'JOVE', 'JUDD', 'JUDE', 'JUDO', 'JUDY', 'JUJU', 'JUKE',
    'JULY', 'JUNE', 'JUNK', 'JUNO', 'JURY', 'JUST', 'JUTE', 'KAHN',
    'KALE', 'KANE', 'KANT', 'KARL', 'KATE', 'KEEL', 'KEEN', 'KENO',
    'KENT', 'KERN', 'KERR', 'KEYS', 'KICK', 'KILL', 'KIND', 'KING',
    'KIRK', 'KISS', 'KITE', 'KLAN', 'KNEE', 'KNEW', 'KNIT', 'KNOB',
    'KNOT', 'KNOW', 'KOCH', 'KONG', 'KUDO', 'KURD', 'KURT', 'KYLE',
    'LACE', 'LACK', 'LACY', 'LADY', 'LAID', 'LAIN', 'LAIR', 'LAKE',
    'LAMB', 'LAME', 'LAND', 'LANE', 'LANG', 'LARD', 'LARK', 'LASS',
    'LAST', 'LATE', 'LAUD', 'LAVA', 'LAWN', 'LAWS', 'LAYS', 'LEAD',
    'LEAF', 'LEAK', 'LEAN', 'LEAR', 'LEEK', 'LEER', 'LEFT', 'LEND',
    'LENS', 'LENT', 'LEON', 'LESK', 'LESS', 'LEST', 'LETS', 'LIAR',
    'LICE', 'LICK', 'LIED', 'LIEN', 'LIES', 'LIEU', 'LIFE', 'LIFT',
    'LIKE', 'LILA', 'LILT', 'LILY', 'LIMA', 'LIMB', 'LIME', 'LIND',
    'LINE', 'LINK', 'LINT', 'LION', 'LISA', 'LIST', 'LIVE', 'LOAD',
    'LOAF', 'LOAM', 'LOAN', 'LOCK', 'LOFT', 'LOGE', 'LOIS', 'LOLA',
    'LONE', 'LONG', 'LOOK', 'LOON', 'LOOT', 'LORD', 'LORE', 'LOSE',
    'LOSS', 'LOST', 'LOUD', 'LOVE', 'LOWE', 'LUCK', 'LUCY', 'LUGE',
    'LUKE', 'LULU', 'LUND', 'LUNG', 'LURA', 'LURE', 'LURK', 'LUSH',
    'LUST', 'LYLE', 'LYNN', 'LYON', 'LYRA', 'MACE', 'MADE', 'MAGI',
    'MAID', 'MAIL', 'MAIN', 'MAKE', 'MALE', 'MALI', 'MALL', 'MALT',
    'MANA', 'MANN', 'MANY', 'MARC', 'MARE', 'MARK', 'MARS', 'MART',
    'MARY', 'MASH', 'MASK', 'MASS', 'MAST', 'MATE', 'MATH', 'MAUL',
    'MAYO', 'MEAD', 'MEAL', 'MEAN', 'MEAT', 'MEEK', 'MEET', 'MELD',
    'MELT', 'MEMO', 'MEND', 'MENU', 'MERT', 'MESH', 'MESS', 'MICE',
    'MIKE', 'MILD', 'MILE', 'MILK', 'MILL', 'MILT', 'MIMI', 'MIND',
    'MINE', 'MINI', 'MINK', 'MINT', 'MIRE', 'MISS', 'MIST', 'MITE',
    'MITT', 'MOAN', 'MOAT', 'MOCK', 'MODE', 'MOLD', 'MOLE', 'MOLL',
    'MOLT', 'MONA', 'MONK', 'MONT', 'MOOD', 'MOON', 'MOOR', 'MOOT',
    'MORE', 'MORN', 'MORT', 'MOSS', 'MOST', 'MOTH', 'MOVE', 'MUCH',
    'MUCK', 'MUDD', 'MUFF', 'MULE', 'MULL', 'MURK', 'MUSH', 'MUST',
    'MUTE', 'MUTT', 'MYRA', 'MYTH', 'NAGY', 'NAIL', 'NAIR', 'NAME',
    'NARY', 'NASH', 'NAVE', 'NAVY', 'NEAL', 'NEAR', 'NEAT', 'NECK',
    'NEED', 'NEIL', 'NELL', 'NEON', 'NERO', 'NESS', 'NEST', 'NEWS',
    'NEWT', 'NIBS', 'NICE', 'NICK', 'NILE', 'NINA', 'NINE', 'NOAH',
    'NODE', 'NOEL', 'NOLL', 'NONE', 'NOOK', 'NOON', 'NORM', 'NOSE',
    'NOTE', 'NOUN', 'NOVA', 'NUDE', 'NULL', 'NUMB', 'OATH', 'OBEY',
    'OBOE', 'ODIN', 'OHIO', 'OILY', 'OINT', 'OKAY', 'OLAF', 'OLDY',
    'OLGA', 'OLIN', 'OMAN', 'OMEN', 'OMIT', 'ONCE', 'ONES', 'ONLY',
    'ONTO', 'ONUS', 'ORAL', 'ORGY', 'OSLO', 'OTIS', 'OTTO', 'OUCH',
    'OUST', 'OUTS', 'OVAL', 'OVEN', 'OVER', 'OWLY', 'OWNS', 'QUAD',
    'QUIT', 'QUOD', 'RACE', 'RACK', 'RACY', 'RAFT', 'RAGE', 'RAID',
    'RAIL', 'RAIN', 'RAKE', 'RANK', 'RANT', 'RARE', 'RASH', 'RATE',
    'RAVE', 'RAYS', 'READ', 'REAL', 'REAM', 'REAR', 'RECK', 'REED',
    'REEF', 'REEK', 'REEL', 'REID', 'REIN', 'RENA', 'REND', 'RENT',
    'REST', 'RICE', 'RICH', 'RICK', 'RIDE', 'RIFT', 'RILL', 'RIME',
    'RING', 'RINK', 'RISE', 'RISK', 'RITE', 'ROAD', 'ROAM', 'ROAR',
    'ROBE', 'ROCK', 'RODE', 'ROIL', 'ROLL', 'ROME', 'ROOD', 'ROOF',
    'ROOK', 'ROOM', 'ROOT', 'ROSA', 'ROSE', 'ROSS', 'ROSY', 'ROTH',
    'ROUT', 'ROVE', 'ROWE', 'ROWS', 'RUBE', 'RUBY', 'RUDE', 'RUDY',
    'RUIN', 'RULE', 'RUNG', 'RUNS', 'RUNT', 'RUSE', 'RUSH', 'RUSK',
    'RUSS', 'RUST', 'RUTH', 'SACK', 'SAFE', 'SAGE', 'SAID', 'SAIL',
    'SALE', 'SALK', 'SALT', 'SAME', 'SAND', 'SANE', 'SANG', 'SANK',
    'SARA', 'SAUL', 'SAVE', 'SAYS', 'SCAN', 'SCAR', 'SCAT', 'SCOT',
    'SEAL', 'SEAM', 'SEAR', 'SEAT', 'SEED', 'SEEK', 'SEEM', 'SEEN',
    'SEES', 'SELF', 'SELL', 'SEND', 'SENT', 'SETS', 'SEWN', 'SHAG',
    'SHAM', 'SHAW', 'SHAY', 'SHED', 'SHIM', 'SHIN', 'SHOD', 'SHOE',
    'SHOT', 'SHOW', 'SHUN', 'SHUT', 'SICK', 'SIDE', 'SIFT', 'SIGH',
    'SIGN', 'SILK', 'SILL', 'SILO', 'SILT', 'SINE', 'SING', 'SINK',
    'SIRE', 'SITE', 'SITS', 'SITU', 'SKAT', 'SKEW', 'SKID', 'SKIM',
    'SKIN', 'SKIT', 'SLAB', 'SLAM', 'SLAT', 'SLAY', 'SLED', 'SLEW',
    'SLID', 'SLIM', 'SLIT', 'SLOB', 'SLOG', 'SLOT', 'SLOW', 'SLUG',
    'SLUM', 'SLUR', 'SMOG', 'SMUG', 'SNAG', 'SNOB', 'SNOW', 'SNUB',
    'SNUG', 'SOAK', 'SOAR', 'SOCK', 'SODA', 'SOFA', 'SOFT', 'SOIL',
    'SOLD', 'SOME', 'SONG', 'SOON', 'SOOT', 'SORE', 'SORT', 'SOUL',
    'SOUR', 'SOWN', 'STAB', 'STAG', 'STAN', 'STAR', 'STAY', 'STEM',
    'STEW', 'STIR', 'STOW', 'STUB', 'STUN', 'SUCH', 'SUDS', 'SUIT',
    'SULK', 'SUMS', 'SUNG', 'SUNK', 'SURE', 'SURF', 'SWAB', 'SWAG',
    'SWAM', 'SWAN', 'SWAT', 'SWAY', 'SWIM', 'SWUM', 'TACK', 'TACT',
    'TAIL', 'TAKE', 'TALE', 'TALK', 'TALL', 'TANK', 'TASK', 'TATE',
    'TAUT', 'TEAL', 'TEAM', 'TEAR', 'TECH', 'TEEM', 'TEEN', 'TEET',
    'TELL', 'TEND', 'TENT', 'TERM', 'TERN', 'TESS', 'TEST', 'THAN',
    'THAT', 'THEE', 'THEM', 'THEN', 'THEY', 'THIN', 'THIS', 'THUD',
    'THUG', 'TICK', 'TIDE', 'TIDY', 'TIED', 'TIER', 'TILE', 'TILL',
    'TILT', 'TIME', 'TINA', 'TINE', 'TINT', 'TINY', 'TIRE', 'TOAD',
    'TOGO', 'TOIL', 'TOLD', 'TOLL', 'TONE', 'TONG', 'TONY', 'TOOK',
    'TOOL', 'TOOT', 'TORE', 'TORN', 'TOTE', 'TOUR', 'TOUT', 'TOWN',
    'TRAG', 'TRAM', 'TRAY', 'TREE', 'TREK', 'TRIG', 'TRIM', 'TRIO',
    'TROD', 'TROT', 'TROY', 'TRUE', 'TUBA', 'TUBE', 'TUCK', 'TUFT',
    'TUNA', 'TUNE', 'TUNG', 'TURF', 'TURN', 'TUSK', 'TWIG', 'TWIN',
    'TWIT', 'ULAN', 'UNIT', 'URGE', 'USED', 'USER', 'USES', 'UTAH',
    'VAIL', 'VAIN', 'VALE', 'VARY', 'VASE', 'VAST', 'VEAL', 'VEDA',
    'VEIL', 'VEIN', 'VEND', 'VENT', 'VERB', 'VERY', 'VETO', 'VICE',
    'VIEW', 'VINE', 'VISE', 'VOID', 'VOLT', 'VOTE', 'WACK', 'WADE',
    'WAGE', 'WAIL', 'WAIT', 'WAKE', 'WALE', 'WALK', 'WALL', 'WALT',
    'WAND', 'WANE', 'WANG', 'WANT', 'WARD', 'WARM', 'WARN', 'WART',
    'WASH', 'WAST', 'WATS', 'WATT', 'WAVE', 'WAVY', 'WAYS', 'WEAK',
    'WEAL', 'WEAN', 'WEAR', 'WEED', 'WEEK', 'WEIR', 'WELD', 'WELL',
    'WELT', 'WENT', 'WERE', 'WERT', 'WEST', 'WHAM', 'WHAT', 'WHEE',
    'WHEN', 'WHET', 'WHOA', 'WHOM', 'WICK', 'WIFE', 'WILD', 'WILL',
    'WIND', 'WINE', 'WING', 'WINK', 'WINO', 'WIRE', 'WISE', 'WISH',
    'WITH', 'WOLF', 'WONT', 'WOOD', 'WOOL', 'WORD', 'WORE', 'WORK',
    'WORM', 'WORN', 'WOVE', 'WRIT', 'WYNN', 'YALE', 'YANG', 'YANK',
    'YARD', 'YARN', 'YAWL', 'YAWN', 'YEAH', 'YEAR', 'YELL', 'YOGA',
    'YOKE');

implementation

end.

