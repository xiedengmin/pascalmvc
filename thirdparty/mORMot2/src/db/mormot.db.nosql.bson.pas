/// Database Framework BSON Encoding for MongoDB
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.nosql.bson;

{
  *****************************************************************************

   Efficient BSON Support for MongoDB Clients
    - BSON Decimal128 Value
    - BSON ObjectID Value
    - TBsonVariantData / TBsonVariant Custom Variant Storage
    - TBsonElement / TBsonIterator for BSON Decoding
    - TBsonWriter for BSON Encoding
    - High-Level BSON/JSON Function Helpers

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core;


{ ************ BSON Decimal128 Value }

type
  /// binary representation of a 128-bit decimal, stored as 16 bytes
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, and processed by the TDecimal128 object
  TDecimal128Bits = record
    case integer of
      0:
        (lo, hi: QWord);
      1:
        (l, h: Int64);
      2:
        (b: array[0..15] of byte);
      3:
        (c: array[0..3] of cardinal);
  end;

  /// points to a 128-bit decimal binary
  PDecimal128Bits = ^TDecimal128Bits;

  /// enough characters to contain any TDecimal128 text representation
  TDecimal128Str = array[0..42] of AnsiChar;

  /// some special 128-bit decimal values
  // - see TDecimal128.SetSpecial to set the corresponding value
  // - dsvError is returned by TDecimal128.FromText() on parsing error
  // - dsvValue indicates that this is not a known "special" value, but some
  // valid decimal number
  TDecimal128SpecialValue = (
    dsvError,
    dsvValue,
    dsvNan,
    dsvZero,
    dsvPosInf,
    dsvNegInf,
    dsvMin,
    dsvMax);

  /// handles a 128-bit decimal value
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, i.e. betDecimal128 TBsonElementType
  // - the betFloat BSON format stores a 64-bit floating point value, which
  // doesn't have exact decimals, so may suffer from rounding or approximation
  // - for instance, if you work with some currency values, you may store
  // betDecimal128 values in MongoDB - the easiest way is to include it as a
  // TBsonVariant instance, via the NumberDecimal() function
  // - there is no mathematical operator/methods for Decimal128 Value Objects,
  // as required by MongoDB specifications: any computation must be done
  // explicitly on native language value representation (e.g. currency, TBCD or
  // any BigNumber library) - use ToCurr/FromCurr or ToText/FromText to make
  // the appropriate safe conversions
  {$ifdef USERECORDWITHMETHODS}
  TDecimal128 = record
  {$else}
  TDecimal128 = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the raw binary storage
    Bits: TDecimal128Bits;
    /// fills with the Zero value
    // - note: under IEEE 754, Zero can have sign and exponents, so is not Hi=Lo=0
    // - is the same as Fill(dsvZero)
    procedure SetZero;
    /// fills with a special value
    // - dsvError or dsvValue will set dsvNan binary content
    procedure SetSpecial(special: TDecimal128SpecialValue);
    /// checks if the value matches one of the known special values
    // - will search for dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    function IsSpecial: TDecimal128SpecialValue;
    /// fills with a 32-bit signed value
    procedure FromInt32(value: integer);
    /// fills with a 32-bit unsigned value
    procedure FromUInt32(value: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a 64-bit signed value
    procedure FromInt64(value: Int64);
    /// fills with a 64-bit unsigned value
    procedure FromQWord(value: QWord);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a fixed decimal value, as stored in currency
    // - will store the content with explictly four decimals, as in currency
    // - by design, this method is very fast and accurate
    procedure FromCurr(const value: currency);
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(text: PUtf8Char; textlen: integer): TDecimal128SpecialValue; overload;
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(const text: RawUtf8): TDecimal128SpecialValue; overload;
    /// convert a variant into one Decimal128 value
    // - will first check for a TBsonVariant containing a betDecimal128 (e.g.
    // as retrieved via the ToVariant method)
    // - will recognize currency and VariantToInt64() stored values
    // - then will try to convert the variant from its string value, expecting
    // a floating-point text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// fills with a native floating-point value
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    // - this method is just a wrapper around ExtendedToShort and ToText,
    // so you should provide the expected precision, from the actual storage
    // variable (you may specify e.g. SINGLE_PRECISION or EXTENDED_PRECISION if
    // you don't use a double kind of value)
    function FromFloat(const value: TSynExtended; precision: integer = 0): boolean;
    /// fast bit-per-bit value comparison
    function Equals(const other: TDecimal128): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts the value to its string representation
    // - returns the number of AnsiChar written to Buffer
    function ToText(out Buffer: TDecimal128Str): integer; overload;
    /// converts this Decimal128 value to its string representation
    function ToText: RawUtf8; overload;
    /// converts this Decimal128 value to its string representation
    procedure ToText(var result: RawUtf8); overload;
    /// convert this Decimal128 value to its TBsonVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this Decimal128 value to its TBsonVariant custom variant value
    procedure ToVariant(out Result: variant); overload;
    /// converts this Decimal128 value to a floating-point value
    // - by design, some information may be lost during conversion
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    function ToFloat: TSynExtended;
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    function ToCurr: currency; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    procedure ToCurr(out result: currency); overload;
    /// converts this Decimal128 value to its string representation
    procedure AddText(W: TJsonWriter);
  end;

  /// points to a 128-bit decimal value
  PDecimal128 = ^TDecimal128;

const
  /// the textual representation of the TDecimal128 special values
  DECIMAL128_SPECIAL_TEXT: array[TDecimal128SpecialValue] of RawUtf8 = (
    '',                                           // dsvError
    '',                                           // dsvValue
    'NaN',                                        // dsvNan
    '0',                                          // dsvZero
    'Infinity',                                   // dsvPosInf
    '-Infinity',                                  // dsvNegInf
    '-9.999999999999999999999999999999999E+6144', // dsvMin
    '9.999999999999999999999999999999999E+6144'); // dsvMax

  BSON_DECIMAL128_HI_NAN = $7c00000000000000;
  BSON_DECIMAL128_HI_INT64POS = $3040000000000000; // 0 fixed decimals
  BSON_DECIMAL128_HI_INT64NEG = $b040000000000000;
  BSON_DECIMAL128_HI_CURRPOS = $3038000000000000;  // 4 fixed decimals
  BSON_DECIMAL128_HI_CURRNEG = $b038000000000000;
  BSON_DECIMAL128_EXPONENT_MAX = 6111;
  BSON_DECIMAL128_EXPONENT_MIN = -6176;
  BSON_DECIMAL128_EXPONENT_BIAS = 6176;
  BSON_DECIMAL128_MAX_DIGITS = 34;

/// ready-to-be displayed text of a TDecimal128SpecialValue
function ToText(spec: TDecimal128SpecialValue): PShortString; overload;



{ ************ BSON ObjectID Value }

type
    /// 24-bit storage, mapped as a 3 bytes buffer
  // - as used fo TBsonObjectID.MachineID and TBsonObjectID.Counter
  TBson24 = record
    b1, b2, b3: byte;
  end;

  /// points to 24-bit storage, mapped as a 3 bytes buffer
  PBson24 = ^TBson24;

  {$A-}

  /// BSON ObjectID 12-byte internal binary representation
  // - in MongoDB, documents stored in a collection require a unique _id field
  // that acts as a primary key: by default, it uses such a 12-byte ObjectID
  // - by design, sorting by _id: ObjectID is roughly equivalent to sorting by
  // creation time, so ease sharding and BTREE storage
  // - in our ODM, we rather use 64-bit genuine integer identifiers (TID),
  // as computed by an internal sequence or TSynUniqueIdentifierGenerator
  // - match betObjectID TBsonElementType
  {$ifdef USERECORDWITHMETHODS}
  TBsonObjectID = record
  {$else}
  TBsonObjectID = object
  {$endif USERECORDWITHMETHODS}
    /// big-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    UnixCreateTime: cardinal;
    /// 3-byte machine identifier
    // - ComputeNew will use a hash of Executable.Host and Executable.User
    MachineID: TBson24;
    /// 2-byte process id
    // - ComputeNew will derivate it from MainThreadID
    ProcessID: word;
    /// 3-byte counter, starting with a random value
    // - used to avoid collision
    Counter: TBson24;
    /// set all internal fields to zero
    procedure Init;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// ObjectID content be filled with some unique values
    // - this implementation is thread-safe
    procedure ComputeNew;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(const Text: RawUtf8): boolean; overload;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(Text: PUtf8Char): boolean; overload;
    /// convert a variant into one ObjectID
    // - will first check for a TBsonVariant containing a betObjectID
    // - then will try to convert the variant from its string value, expecting
    // an hexadecimal text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// convert this ObjectID to its hexadecimal string value
    function ToText: RawUtf8; overload;
    /// convert this ObjectID to its hexadecimal string value
    procedure ToText(var result: RawUtf8); overload;
    /// convert this ObjectID to its TBsonVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this ObjectID to its TBsonVariant custom variant value
    procedure ToVariant(var result: variant); overload;
    /// returns the timestamp portion of the ObjectId() object as a TDateTime
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // so you can compare it to NowUtc returned time
    function CreateDateTime: TDateTime;
    /// compare two Object IDs
    function Equal(const Another: TBsonObjectID): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare two Object IDs, the second being stored in a TBsonVariant
    function Equal(const Another: variant): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// points to a BSON ObjectID internal binary representation
  PBsonObjectID = ^TBsonObjectID;

  {$A+}


  
{ ************ TBsonVariantData / TBsonVariant Custom Variant Storage }

type
  /// exception type used for BSON process
  EBsonException = class(ESynException);

  /// storage of a BSON binary document
  // - a specific type is defined for consistency with this unit classes
  // - binary content should follow the "int32 e_list #0" standard layout
  TBsonDocument = RawByteString;

  /// dynamic array of BSON binary document storage
  TBsonDocumentDynArray = array of TBsonDocument;

  /// element types for BSON internal representation
  TBsonElementType = (
    betEOF,
    betFloat,
    betString,
    betDoc,
    betArray,
    betBinary,
    betDeprecatedUndefined,
    betObjectID,
    betBoolean,
    betDateTime,
    betNull,
    betRegEx,
    betDeprecatedDbptr,
    betJS,
    betDeprecatedSymbol,
    betJSScope,
    betInt32,
    betTimestamp,
    betInt64,
    betDecimal128);

  /// points to an element type for BSON internal representation
  PBsonElementType = ^TBsonElementType;

  /// sub-types for betBinary element BSON internal representation
  TBsonElementBinaryType = (
    bbtGeneric,
    bbtFunction,
    bbtOldBinary,
    bbtOldUUID,
    bbtUUID,
    bbtMD5,
    bbtUser = $80);

  {$A-}

  /// memory structure used for some special BSON storage as variant
  // - betObjectID kind will store a TBsonObjectID
  // - betBinary kind will store a BLOB content as RawByteString
  // - betDoc and betArray kind will store a BSON document, in its original
  // binary format as RawByteString (TBsonDocument)
  // - betDeprecatedDbptr, betJSScope, betTimestamp and betRegEx will store the
  // raw original BSON content as RawByteString
  // - betJS and betDeprecatedSymbol will store the UTF-8 encoded string
  // as a RawUtf8
  // - betDeprecatedUndefined or betMinKey/betMaxKey do not contain any data
  // - betDecimal128 will store the TDecimal128 16 bytes binary buffer
  // - warning: VBlob/VText use should match BSON_ELEMENTVARIANTMANAGED constant
  TBsonVariantData = packed record
    /// the variant type
    VType: TVarType;
    /// the kind of element stored
    case VKind: TBsonElementType of
      betObjectID:
        (
      {$ifdef fpc} {$push} {$endif} {$hints off}
        // does not complain if Filler is declared but void
        VFiller: array[1..SizeOf(TVarData) - SizeOf(TVarType)
          - SizeOf(TBsonElementType) - SizeOf(TBsonObjectID)] of byte;
      {$ifdef fpc} {$pop} {$else} {$hints on} {$endif}
        VObjectID: TBsonObjectID);
      betBinary,
      betDoc,
      betArray,
      betRegEx,
      betDeprecatedDbptr,
      betTimestamp,
      betJSScope,
      betDecimal128:
        (
        /// store the raw binary content as a RawByteString (or TBsonDocument for
        // betDoc/betArray, i.e. the "int32 e_list #0" standard layout)
        // - you have to use RawByteString(VBlob) when accessing this field
        // - e.g. for betRegEx, it will contain raw [cstring cstring] content
        VBlob: pointer;);
      betJS,
      betDeprecatedSymbol:
        (
        /// store here a RawUtf8 with the associated text
        // - you have to use RawUF8(VText) when accessing this field
        VText: pointer;);
  end;
  
  {$A+}

  /// points to memory structure used for some special BSON storage as variant
  PBsonVariantData = ^TBsonVariantData;

  /// custom variant type used to store some special BSON elements
  // - internal layout will follow TBsonVariantData
  // - handled kind of item are complex BSON types, like betObjectID, betBinary
  // or betDoc/betArray
  // - it will allow conversion to/from string (and to date for ObjectID)
  TBsonVariant = class(TSynInvokeableVariantType)
  protected
    function GetNewDoc(const BsonDoc: TBsonDocument): variant;
  public
    /// notify the TryJsonToVariant/ToJson abilities
    constructor Create; override;
    /// customization of JSON conversion into TBsonVariant kind of variants
    function TryJsonToVariant(var Json: PUtf8Char; var Value: variant;
      EndOfObject: PUtf8Char): boolean; override;
    /// variant serialization will use modMongoStrict JSON-compatible mode
    procedure ToJson(W: TJsonWriter; Value: PVarData); override;
    /// variant serialization will use modMongoStrict JSON-compatible mode
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// clear the instance
    procedure Clear(var V: TVarData); override;
    /// copy one instance
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// compare two variant values
    // - handle comparison of any variant, including TBsonVariant, via a
    // temporary JSON conversion, and case-sensitive comparison
    // - it uses case-sensitive text (hexadecimal) comparison for betObjectID
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    /// convert a TBsonDocument binary content into a TBsonVariant of kind
    // betDoc or betArray
    // - see also all BsonVariant() overloaded functions, which also create
    // a TBsonVariant betDoc instance
    procedure FromBsonDocument(const BsonDoc: TBsonDocument; var result: variant;
      Kind: TBsonElementType = betDoc);
    /// convert a BLOB binary content into a TBsonVariant of kind betBinary
    // - if Bin is '', will store a NULL variant
    procedure FromBinary(const Bin: RawByteString;
      BinType: TBsonElementBinaryType; var result: variant);
    /// convert a JSON content into a TBsonVariant of kind betDoc or betArray
    // - warning: the supplied JSON buffer will be modified in-place
    // - will create a plain variant value if the JSON doesn't start with [ or {
    procedure FromJson(json: PUtf8Char; var result: variant);
    /// returns TRUE if the supplied variant stores the supplied BSON kind of value
    function IsOfKind(const V: variant; Kind: TBsonElementType): boolean;
    /// retrieve a betBinary content stored in a TBsonVariant instance
    // - returns TRUE if the supplied variant is a betBinary, and set the
    // binary value into the supplied Blob variable
    // - returns FALSE otherwise
    function ToBlob(const V: Variant; var Blob: RawByteString): boolean;
    /// convert a TBsonDocument binary content into a TBsonVariant of kind betDoc
    // - is the default property, so that you can write:
    // ! BsonVariantType[Bson(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - see also all BsonVariant() overloaded functions, which also create
    // a TBsonVariant betDoc instance
    property NewDoc[const BsonDoc: TBsonDocument]: variant
      read GetNewDoc; default;
  end;



{ ************ TBsonElement / TBsonIterator for BSON Decoding }

type
  /// define how betDoc/betArray BSON elements will be converted as variants
  // - by default a TBsonVariant custom type will be returned, containing the
  // raw BSON binary content of the embedded document or array
  // - asDocVariantPerValue or asDocVariantPerReference could be used to
  // create a tree of TDocVariant custom kind of variant, able to access
  // to its nested properties via late-binding (asDocVariantPerReference being
  // also much faster in some cases - but less safe - than asDocVariantPerValue)
  // - asDocVariantPerValue will set JSON_[mDefault] settings:
  // ! [dvoReturnNullForUnknownProperty]
  // - asDocVariantPerReference will set JSON_[mFast]/JSON_FAST
  // settings:
  // ! [dvoValueCopiedByReference,dvoReturnNullForUnknownProperty]
  // - asDocVariantInternNamesPerValue and asDocVariantInternNamesPerReference
  // will include dvoInternalNames to the TDocVariant.Options
  TBsonDocArrayConversion = (
    asBsonVariant,
    asDocVariantPerValue,
    asDocVariantPerReference,
    asDocVariantInternNamesPerValue,
    asDocVariantInternNamesPerReference);

  /// how TBsonElement.AddMongoJson() method and AddMongoJson() and
  // VariantSaveMongoJson() functions will render their JSON content
  // - modMongoStrict and modNoMongo will follow the JSON RFC specifications
  // - modMongoShell will use a syntax incompatible with JSON RFC, but more
  // common to MongoDB daily use - as 'ObjectId()' or '{ field: /acme.*corp/i }'
  // - modMongoStrict will use the MongoDB Extended JSON syntax
  // - modNoMongo will serialize dates as ISO-8601 strings, ObjectID as hexadecimal
  // string and other MongoDB special objects in WrBase64() format
  // - see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  TMongoJsonMode = (
    modNoMongo,
    modMongoStrict,
    modMongoShell);

  {$A-}

  /// data structure used during BSON binary decoding of one BSON element
  // - will be retrieved by FromVariant() or FromNext()
  // - see http://bsonspec.org/#/specification
  // - this structure has been optimized to map the BSON binary content,
  // without any temporary memory allocation (the SAX way)
  {$ifdef USERECORDWITHMETHODS}
  TBsonElement = record
  {$else}
  TBsonElement = object
  {$endif USERECORDWITHMETHODS}
  private
    /// used internally to set the TBsonElement content, once Kind has been set
    procedure FromBson(bson: PByte);
  public
    /// the UTF-8 encoded name of this element
    Name: PUtf8Char;
    /// the name length (in chars) of this element - 16-bit encoded
    NameLen: word;
    /// index of this element in the original sequence list
    // - is correct only when the element has been reset before the parsing
    // loop, e.g.:
    // ! item.Index := -1; // so item.Index will count starting at 0
    // ! while item.FromNext(elem.Document) do
    // !   writeln(item.Index,' ',Item.Name,' ',Item.ValueBytes);
    Index: integer;
    /// the element type
    Kind: TBsonElementType;
    /// number of bytes in the BSON element
    // - will include the trailing #0 for string element
    ElementBytes: integer;
    /// pointer to the BSON element value
    // - is the raw value, without any parsing, e.g. points to a double value or
    // a document: "int32 e_list #0" standard layout (same as TBsonDocument)
    // - you may cast it for simple types:
    // ! PDouble(Element)^   PBoolean(Element)^        PInteger(Element)^
    // ! PInt64(Element)^    PBsonObjectID(Element)^   PDecimal128(Element)^
    // - or use the nested Data variant record to access more complex content
    // - warning: equals nil for betString/betJS after FromVariant()
    Element: pointer;
    /// depending on the Kind, will point to parsed complex sub-data
    // - since variable records can't have properties, we nest this information
    // within this main Data variable record
    // - not all Kind are handled here, only any complex data
    Data: record
      case TBsonElementType of
        betFloat,
        betBoolean,
        betInt32,
        betDateTime,
        betInt64:
          (
          /// this variable is not to be used directly, but for some internal
          // temporary storage, e.g. with FromVariant()
          // - use P*(Element)^ typecast instead
          InternalStorage: Int64;);
        betString,
        betJS:
          (
          /// points to the #0 ending string
          Text: PUtf8Char;
          /// number of bytes in Text (excluding trailing #0)
          TextLen: integer;);
        betDoc,
        betArray:
          (
          /// points to a "e_list #0" standard layout
          DocList: PByte;);
        betBinary:
          (
          /// points to the binary content
          Blob: pointer;
          /// number of bytes in Blob
          BlobLen: integer;
          /// corresponding sub-type of this Blob
          BlobSubType: TBsonElementBinaryType;);
        betRegEx:
          (RegEx: PUtf8Char;
           RegExLen: integer;
           RegExOptions: PUtf8Char;
           RegExOptionsLen: integer;);
        betJSScope:
          (JavaScript: PUtf8Char;
           JavaScriptLen: integer;
           ScopeDocument: PByte;);
        betTimestamp:
          ({ map InternalStorage: Int64 }
           time_t: cardinal;
           ordinal: cardinal;);
    end;
    /// fill a BSON Element structure from a variant content and associated name
    // - perform the reverse conversion as made with ToVariant()
    // - since the result won't store any data but points to the original binary
    // content, the supplied Name/Value instances should remain available as long as
    // you will access to the result content
    // - aName here is just for conveniency, and could be left void
    // - supplied aTemp variable will be used for temporary storage, private to
    // this initialized TBsonElement
    procedure FromVariant(const aName: RawUtf8; const aValue: Variant;
      var aTemp: RawByteString);
    /// fill a BSON Element structure from a TBsonVariantData value
    procedure FromBsonVariant(aValue: PVarData);
      {$ifdef HASINLINE} inline; {$endif}
    /// fill a BSON Element structure from a BSON document
    // - will check the document length then set Kind := betDoc and Data.DocList
    // - will return TRUE if the supplied doc has a valid length, FALSE otherwise
    // - you can later on use DocItemToVariant, DocItemToRawUtf8 or
    // DocItemToInteger methods
    // - the supplied "doc" variable should remain available until you are done
    // with this TBsonElement wrapper
    function FromDocument(const doc: TBsonDocument): boolean;
    /// fill a BSON Element structure from a BSON encoded binary buffer list
    // - parse the next BSON element: BSON parameter should point to the
    // "e_list" of the "int32 e_list #0" BSON document
    // - will decode the supplied binary buffer into the BSON element structure,
    // then it will let BSON point to the next element, and return TRUE
    // - returns FALSE when you reached betEOF, so that you can use it in a loop,
    // and retrieve all the content as consecutive events, without any memory
    // allocation (the SAX way):
    // ! var bson: PByte;
    // !     item: TBsonElement;
    // ! ...
    // ! BsonParseLength(bson);
    // ! while item.FromNext(bson) do
    // !   writeln(item.Name);
    // - will raise an EBsonException if BSON content is not correct
    // - as an alternative, consider using TBsonIterator, which wrap both a
    // PByte and a TBsonElement into one convenient item
    function FromNext(var BSON: PByte): boolean;
    /// search for a given name in a BSON encoded binary buffer list
    // - BSON parameter should point to the first "e_list" item of the
    // "int32 e_list #0" BSON document
    // - returns false if the item was not found (with case-insensitive search)
    // - otherwise returns TRUE and the matching element content has been
    // decoded within this TBsonElement structure
    function FromSearch(BSON: PByte; const aName: RawUtf8): boolean;
    /// convert a BSON element, as retrieved by TBsonElement.FromNext(),
    // into a variant
    // - it will return either standard variant values, or TBsonVariant custom type
    // for most complex kind of elements (see TBsonVariantData type definition)
    // - note that betString types will be stored as RawUtf8 varString
    // - by default, it will return TBsonVariant custom variants for documents or
    // arrays - but if storeDocArrayAsDocVariant is set, it will return a
    // TDocVariant custom kind of variant, able to access to its nested
    // properties via late-binding
    function ToVariant(
      DocArrayConversion: TBsonDocArrayConversion = asBsonVariant): variant; overload;
    /// convert a BSON element, as retrieved by TBsonElement.FromNext(),
    // into a variant
    // - same as the other ToVariant() overloaded function, but avoiding a copy
    // of the resulting variant
    procedure ToVariant(var result: variant;
      DocArrayConversion: TBsonDocArrayConversion = asBsonVariant); overload;
    /// convert a BSON element into an UTF-8 string
    // - any complex types (e.g. nested documents) will be converted via a
    // variant
    function ToRawUtf8: RawUtf8;
    /// convert a BSON element into an integer value
    // - will work only for betBoolean/betInt32/betInt64 types
    // - any other kind of values will return the supplied default value
    function ToInteger(const default: Int64 = 0): Int64;
    /// search a variant property value within the BSON element as document
    // - returns true if aName has been found as property in the BSON element,
    // and fills aValue with the corresponding value
    // - returns false if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToVariant(const aName: RawUtf8; var aValue: variant;
      DocArrayConversion: TBsonDocArrayConversion = asBsonVariant): boolean;
    /// search an UTF-8 property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns '' if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToRawUtf8(const aName: RawUtf8): RawUtf8;
    /// search an integer property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns default if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToInteger(const aName: RawUtf8; const default: Int64 = 0): Int64;
    /// convert a BSON element, as retrieved by TBsonElement.FromNext(), into
    // its JSON representation
    // - this method will use by default the MongoDB Extended JSON syntax for
    // specific MongoDB objects but you may use modMongoShell if needed
    // - will raise an EBsonException if element is not correct
    procedure AddMongoJson(W: TJsonWriter;
      Mode: TMongoJsonMode = modMongoStrict); overload;
  end;

  PBsonElement = ^TBsonElement;

  /// data structure used for iterating over a BSON binary buffer
  // - is just a wrapper around a PByte value, to be used with a TBsonDocument
  {$ifdef USERECORDWITHMETHODS}
  TBsonIterator = record
  {$else}
  TBsonIterator = object
  {$endif USERECORDWITHMETHODS}
  private
    fBson: PByte;
  public
    /// map the current item, after the Next method did return TRUE
    // - map the global document, after Init() but before the first Next call
    Item: TBsonElement;
    /// initialize the iteration on the supplied BSON document
    // - will check the document length and returns TRUE if it is correct
    // - only accepted kind are betDoc and betArray (but not used later)
    // - you can then use the Next method to iterate over the Item elements
    // - after this call, the Item property map to the global BSON document
    // (note that after any call to the Next method, Item will map the current
    // iterated value, and not the global BSON document any more)
    function Init(const doc: TBsonDocument; kind: TBsonElementType = betArray): boolean;
    /// will iterate on the BSON document
    // - returns TRUE if the item has been retrieved into the Item property
    // - returns FALSE if we reached the end of the supplied BSON buffer
    function Next: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  {$A+}
  


{ ************ TBsonWriter for BSON Encoding }

type
    /// used to write the BSON context
  TBsonWriter = class(TBufferWriter)
  { note: inlining methods generates 70% SLOWER code due to inefficient compiler :( }
  protected
    fDocumentCount: integer;
    fDocument: array of record
      Offset: cardinal;
      Length: cardinal;
    end;
    fDocumentStack: integer;
    fDocumentStackOffset: TCardinalDynArray;
    fDocumentArray: integer;
    procedure WriteCollectionName(Flags: integer; const CollectionName: RawUtf8);
  public
    /// rewind the Stream to the position when Create() was called
    // - this will also reset the internal document offset table
    procedure CancelAll; override;

    /// write an element with no value
    // - elemType can be either betNull, betMinKey or betMaxKey
    procedure BsonWrite(const name: RawUtf8; elemtype: TBsonElementType); overload;
    /// write a boolean value
    procedure BsonWrite(const name: RawUtf8; const value: boolean); overload;
    /// write a floating point value
    procedure BsonWrite(const name: RawUtf8; const value: Double); overload;
    /// write a 32 bit integer value
    procedure BsonWrite(const name: RawUtf8; const value: integer); overload;
    /// write a 64 bit integer value
    procedure BsonWrite(const name: RawUtf8; const value: Int64); overload;
    /// write a string (UTF-8) value
    procedure BsonWrite(const name: RawUtf8; const value: RawUtf8;
      isJavaScript: boolean = false); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BsonWrite(const name: RawUtf8; value: PUtf8Char); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BsonWriteString(const name: RawUtf8; value: PUtf8Char; valueLen: integer);
    /// write a binary (BLOB) value
    procedure BsonWrite(const name: RawUtf8; Data: pointer; DataLen: integer); overload;
    /// write an ObjectID value
    procedure BsonWrite(const name: RawUtf8; const value: TBsonObjectID); overload;
    /// write a RegEx value
    procedure BsonWriteRegEx(const name: RawUtf8; const RegEx, Options: RawByteString);
    /// write a data/time value
    procedure BsonWriteDateTime(const name: RawUtf8; const value: TDateTime);
    /// write an element with no value
    procedure BsonWrite(const name: RawUtf8; const elem: TBsonElement); overload;
    /// write a BsonVariant instance value
    procedure BsonWrite(const name: RawUtf8; const bson: TBsonVariantData); overload;
    /// write a DocVariant instance value
    procedure BsonWrite(const name: RawUtf8; const doc: TDocVariantData); overload;
    /// write a TDecimal128 value
    procedure BsonWrite(const name: RawUtf8; const value: TDecimal128); overload;
    /// write a variant value
    // - handle simple types (numbers, strings...) and custom types (TDocVariant
    // and TBsonVariant, trying a translation to JSON for other custom types)
    procedure BsonWriteVariant(const name: RawUtf8; const value: variant); overload;
    /// write an open array (const Args: array of const) argument
    // - handle simple types (numbers, strings...) and custom types (TDocVariant)
    procedure BsonWrite(const name: RawUtf8; const value: TVarRec); overload;
    /// write a value from the supplied JSON content
    // - is able to handle any kind of values, including nested documents or
    // BSON extended syntax (if DoNotTryExtendedMongoSyntax=false)
    // - this method is used recursively by BsonWriteDocFromJson(), and should
    // not be called directly
    // - will return JSON=nil in case of unexpected error in the supplied JSON
    procedure BsonWriteFromJson(const name: RawUtf8; var Json: PUtf8Char;
      EndOfObject: PUtf8Char; DoNotTryExtendedMongoSyntax: boolean = false);

    /// recursive writing of a BSON document or value from a TDocVariant
    // object or array, used e.g. by Bson(const doc: TDocVariantData) function
    // - caller should execute BsonAdjustDocumentsSize() on the resulting buffer
    // - this method will call BsonDocumentBegin/BsonDocumentEnd internally
    // - will raise an EBsonException if doc is not a valid TDocVariant or null
    // or if the resulting binary content is bigger than BSON_MAXDOCUMENTSIZE
    procedure BsonWriteDoc(const doc: TDocVariantData);
    /// write an object specified as name/value pairs as a BSON document
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aBsonWriter.BsonWriteObject(['name','John','year',1972]);
    // - this method wil be faster than using a BsonWriteDoc(_ObjFast(...))
    procedure BsonWriteObject(const NameValuePairs: array of const);
    /// write a projection specified as fieldname:1 pairs as a BSON document
    procedure BsonWriteProjection(const FieldNamesCsv: RawUtf8);
    /// write an object as query parameter
    // - will handle all SQL operators, including IN (), IS NULL or LIKE
    // - see @http://docs.mongodb.org/manual/reference/operator/query
    // - inverted should be TRUE e.g. for a NOT ... expression
    // - returns TRUE on success, FALSE if the operator is not implemented yet
    function BsonWriteQueryOperator(name: RawUtf8; inverted: boolean;
      op: TSelectStatementOperator; const Value: variant): boolean;
    /// write one array item, i.e. the ASCII index name as text
    // - only one level of array should be used per TBsonWriter class
    procedure BsonWriteArray(const kind: TBsonElementType); overload;
    /// write an array specified as a list of items as a BSON document
    // - data must be supplied as a list of values e.g.
    // ! aBsonWriter.BsonWriteArray(['John',1972]);
    // - this method wil be faster than using a BsonWriteDoc(_ArrFast(...))
    procedure BsonWriteArray(const Items: array of const); overload;
    /// write an array of integers as a BSON Document
    procedure BsonWriteArrayOfInteger(const Integers: array of integer);
    /// write an array of integers as a BSON Document
    procedure BsonWriteArrayOfInt64(const Integers: array of Int64);
    /// write some BSON document from a supplied (extended) JSON array or object
    // - warning: the incoming JSON buffer will be modified in-place: so you
    // should make a private copy before running this method (see e.g. TSynTempBuffer)
    // - will handle only '{ ... }', '[ ... ]' or 'null' input, with the standard
    // strict JSON format, or BSON-like extensions, e.g. unquoted field names:
    // $ {id:10,doc:{name:"John",birthyear:1972}}
    // - if DoNotTryExtendedMongoSyntax is default FALSE, then the MongoDB Shell
    // syntax will also be recognized to create BSON custom types, like
    // $ new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
    // see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
    // $ {id:new ObjectId(),doc:{name:"John",date:ISODate()}}
    // $ {name:"John",field:/acme.*corp/i}
    // - if DoNotTryExtendedMongoSyntax is TRUE, process may be slightly faster
    // - will create the BSON binary without any temporary TDocVariant storage
    function BsonWriteDocFromJson(Json: PUtf8Char; aEndOfObject: PUtf8Char;
      out Kind: TBsonElementType; DoNotTryExtendedMongoSyntax: boolean = false): PUtf8Char;

    /// to be called before a BSON document will be written
    // - each BsonDocumentBegin should be followed by its nested BsonDocumentEnd
    procedure BsonDocumentBegin; overload;
    /// to be called before a BSON document will be written
    // - each BsonDocumentBegin should be followed by its nested BsonDocumentEnd
    // - you could create a new BSON object by specifying a name and its
    // type, i.e. either betDoc or betArray
    procedure BsonDocumentBegin(const name: RawUtf8;
      kind: TBsonElementType = betDoc); overload;
    /// to be called before a BSON document will be written in an array
    // - only one level of array should be used per TBsonWriter class
    procedure BsonDocumentBeginInArray(const name: RawUtf8;
      kind: TBsonElementType = betDoc);
    /// to be called when a BSON document has been written
    // - it will store the current stream position into an internal array,
    // which will be written when you call AdjustDocumentsSize()
    // - you can optional specify how many nested documents should be closed,
    // and/or if it should not write an ending betEof item
    procedure BsonDocumentEnd(CloseNumber: integer = 1; WriteEndingZero: boolean = true);
    /// after all content has been written, call this method on the resulting
    // memory buffer to store all document size as expected by the standard
    procedure BsonAdjustDocumentsSize(BSON: PByteArray); virtual;
    /// flush the content and return the whole binary encoded stream
    // - call BsonAdjustDocumentsSize() to adjust all internal document sizes
    // - expect the TBsonWriter instance to have been created as such:
    // ! TBsonWriter.Create(TRawByteStringStream) or Create(tmp)
    procedure ToBsonDocument(var result: TBsonDocument); virtual;
    /// flush the content and return the whole document as a TBsonVariant
    // - call ToBsonDocument() to adjust all internal document sizes
    // - expect the TBsonWriter instance to have been created as such:
    // ! TBsonWriter.Create(TRawByteStringStream) or Create(tmp)
    procedure ToBsonVariant(var result: variant; Kind: TBsonElementType = betDoc);
  end;



{ ************ High-Level BSON/JSON Function Helpers }

const
  /// fake BSON element type which compares lower than all other possible values
  // - element type sounds to be stored as shortint, so here $ff=-1<0=betEOF
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMinKey = $ff;
  /// fake BSON element type which compares higher than all other possible values
  // - element type sounds to be stored as shortint, so here betInt64=$12<$7f
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMaxKey = $7f;

  /// kind of elements which will store a RawByteString/RawUtf8 content
  // within its TBsonVariant kind
  // - i.e. TBsonVariantData.VBlob/VText field is to be managed
  BSON_ELEMENTVARIANTMANAGED =
    [betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr, betTimestamp,
     betJSScope, betJS, betDeprecatedSymbol, betDecimal128];

  /// by definition, maximum MongoDB document size is 16 MB
  BSON_MAXDOCUMENTSIZE = 16 * 1024 * 1024;

  /// special JSON string content which will be used to store a betDeprecatedUndefined item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_UNDEFINED: array[boolean] of string[23] = (
    '{"$undefined":true}',
    'undefined');

  /// special JSON string content which will be used to store a betMinKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MINKEY: array[boolean] of string[15] = (
    '{"$minKey":1}',
    'MinKey');

  /// special JSON string content which will be used to store a betMaxKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MAXKEY: array[boolean] of string[15] = (
    '{"$maxKey":1}',
    'MaxKey');

  /// special JSON patterns which will be used to format a betObjectID item
  // - *[false,*] is to be written before the hexadecimal ID, *[true,*] after
  BSON_JSON_OBJECTID: array[boolean, TMongoJsonMode] of string[15] = (
    ('"', '{"$oid":"', 'ObjectId("'),
    ('"', '"}', '")'));

  /// special JSON patterns which will be used to format a betBinary item
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  BSON_JSON_BINARY: array[boolean, boolean] of string[15] = (
    ('{"$binary":"', '","$type":"'),
    ('BinData(', ',"'));

  /// special JSON string content which will be used to store a betDeprecatedDbptr
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  // - (not used by now for this deprecated content)
  BSON_JSON_DBREF: array[boolean, 0..2] of string[15] = (
    ('{"$ref":"', '","$id":"', '"}'),
    ('DBRef("', '","', '")'));

  /// special JSON string content which will be used to store a betRegEx
  BSON_JSON_REGEX: array[0..2] of string[15] = (
    '{"$regex":"', '","$options":"', '"}');

  /// special JSON patterns which will be used to format a betDateTime item
  // - *[*,false] is to be written before the date value, *[*,true] after
  BSON_JSON_DATE: array[TMongoJsonMode, boolean] of string[15] = (
    ('"', '"'),
    ('{"$date":"', '"}'),
    ('ISODate("', '")'));

  /// special JSON patterns which will be used to format a betDecimal128 item
  // - *[false,*] is to be written before the decimal value, *[true,*] after
  BSON_JSON_DECIMAL: array[boolean, TMongoJsonMode] of string[23] = (
    ('"', '{"$numberDecimal":"', 'NumberDecimal("'), ('"', '"}', '")'));

var
  /// global TCustomVariantType used to register BSON variant types
  // - if you use this unit, both TDocVariant and TBsonVariant custom types
  // will be registered, since they are needed for any MongoDB / BSON process
  BsonVariantType: TBsonVariant;

/// ready-to-be displayed text of a TBsonElementType value
function ToText(kind: TBsonElementType): PShortString; overload;

/// create a TBsonVariant custom variant type containing a BSON Object ID
// - will be filled with some unique values, ready to create a new document key
// - will store a BSON element of betObjectID kind
function ObjectID: variant; overload;

/// create a TBsonVariant Object ID custom variant type from a supplied text
// - will raise an EBsonException if the supplied text is not valid hexadecimal
// - will set a BSON element of betObjectID kind
function ObjectID(const Hexa: RawUtf8): variant; overload;

/// convert a TBsonVariant Object ID custom variant into a TBsonObjectID
// - raise an exception if the supplied variant is not a TBsonVariant Object ID
function BsonObjectID(const aObjectID: variant): TBsonObjectID;

/// create a TBsonVariant JavaScript custom variant type from a supplied code
// - will set a BSON element of betJS kind
function JavaScript(const JS: RawUtf8): variant; overload;

/// create a TBsonVariant JavaScript and associated scope custom variant type
// from a supplied code and document
// - will set a BSON element of betJSScope kind
function JavaScript(const JS: RawUtf8; const Scope: TBsonDocument): variant; overload;

/// create a TBsonVariant Decimal128 from some text corresponding to
// a floating-point number
// - will store internally a TDecimal128 storage
function NumberDecimal(const Value: RawUtf8): variant; overload;

/// create a TBsonVariant Decimal128 from a currency fixed decimal
// - will store internally a TDecimal128 storage, with explictly 4 decimals
// - if you want to store some floating-point value, use plain BSON double format
function NumberDecimal(const Value: currency): variant; overload;

/// store some object content into BSON encoded binary
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.:
// ! aBson := Bson(['name','John','year',1972]);
// - you can define nested arrays or objects as TDocVariant, e.g:
// ! aBSON := Bson(['bsonDat',_Arr(['awesome',5.05, 1986])]);
// - or you can specify nested arrays or objects with '['..']' or '{'..'}':
// ! aBSON := Bson(['BSON','[','awesome',5.05,1986,']'])
// ! u := BsonToJson(Bson(['doc','{','name','John','year',1982,'}','id',123]));
// ! assert(u='{"doc":{"name":"John","year":1982},"id":123}');
// ! u := BsonToJson(Bson(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]));
// ! assert(u='{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
// - will create the BSON binary without any temporary TDocVariant storage
function Bson(const NameValuePairs: array of const): TBsonDocument; overload;

/// create a fields selector BSON document from a field names list
// - can be used via a TBsonVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BsonToJson(BsonFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BsonFieldSelector(const FieldNames: array of RawUtf8): TBsonDocument; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used via a TBsonVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BsonToJson(BsonFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BsonFieldSelector(const FieldNamesCsv: RawUtf8): TBsonDocument; overload;

/// store some object content, supplied as (extended) JSON, into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! Bson('{id:10,doc:{name:"John",birthyear:1972}}');
// - MongoDB Shell syntax will also be recognized to create TBsonVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! Bson('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! Bson('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage, by
// calling JsonBufferToBsonDocument() on a temporary copy of the supplied JSON
function Bson(const Json: RawUtf8; kind: PBsonElementType = nil): TBsonDocument; overload;
  {$ifndef ISDELPHI20092010}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// store some object content, supplied as (extended) JSON and parameters,
// into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - MongoDB Shell syntax will also be recognized to create TBsonVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - typical use could be:
// ! Bson('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! Bson('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! Bson('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986])
// ! Bson('{%:?}',['BSON'],[_Arr(['awesome',5.05,1986])])
// ! Bson('{name:?,field:/%/i}',['acme.*corp'],['John']);
// ! Bson('{id:new ObjectId(),doc:{name:?,date:ISODate(?)}}',[],['John',NowUtc]);
// - will create the BSON binary without any temporary TDocVariant storage,
// by calling JsonBufferToBsonDocument() on the generated JSON content
// - since all content will be transformed into JSON internally, use this
// method only if the supplied parameters are simple types, and identified
// explicitly via BSON-like extensions: any complex value (e.g. a TDateTime
// or a BsonVariant binary) won't be handled as expected - use the overloaded
// Bson() with explicit BsonVariant() name/value pairs instead
function Bson(const Format: RawUtf8; const Args, Params: array of const;
  kind: PBsonElementType = nil): TBsonDocument; overload;

/// store some TDocVariant custom variant content into BSON encoded binary
// - will write either a BSON object or array, depending of the internal
// layout of this TDocVariantData instance (i.e. Kind property value)
// - if supplied variant is not a TDocVariant, raise an EBsonException
function Bson(const doc: TDocVariantData): TBsonDocument; overload;

/// store an array of integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BsonFromIntegers(const Integers: array of integer): TBsonDocument;

/// store an array of 64 bit integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BsonFromInt64s(const Integers: array of Int64): TBsonDocument;

/// store some object content, supplied as (extended) JSON into BSON binary
// - warning: the supplied JSON buffer will be modified in-place, if necessary:
// so you should create a temporary copy before calling this function, or call
// Bson(const Json: RawUtf8) function instead
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// also be recognized to create BSON custom values, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! Bson('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! Bson('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage
// - will return the kind of BSON document created, i.e. either betDoc or betArray
function JsonBufferToBsonDocument(Json: PUtf8Char; var doc: TBsonDocument;
  DoNotTryExtendedMongoSyntax: boolean = false): TBsonElementType;

/// store one JSON array into an array of BSON binary
// - since BSON documents are limited to 16 MB by design, this function
// will allow to process huge data content, as soon as it is provided as array
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// be recognized to create BSON custom values - but it will be slightly slower
function JsonBufferToBsonArray(Json: PUtf8Char; out docs: TBsonDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean = false): boolean;

/// store some object content into a TBsonVariant betDoc type instance
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, as expected by the corresponding overloaded Bson() function
function BsonVariant(const NameValuePairs: array of const): variant; overload;

/// create a fields selector BSON document from a field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJson(BsonVariantFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BsonVariantFieldSelector(const FieldNames: array of RawUtf8): variant; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJson(BsonVariantFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BsonVariantFieldSelector(const FieldNamesCsv: RawUtf8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBsonVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded Bson() function

function BsonVariant(const Json: RawUtf8): variant; overload;

/// store some object content, supplied as (extended) JSON, into a TBsonVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded Bson() function
// - warning: this overloaded method will mofify the supplied JSON buffer
// in-place: you can use the overloaded BsonVariant(const Json: RawUtf8) function
// instead if you do not want to modify the input buffer content

procedure BsonVariant(Json: PUtf8Char; var result: variant); overload;

/// store some object content, supplied as (extended) JSON and parameters,
// into a TBsonVariant betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded Bson() function
function BsonVariant(const Format: RawUtf8;
  const Args, Params: array of const): variant; overload;

/// convert a TDocVariant variant into a TBsonVariant betDoc type instance
function BsonVariant(const doc: TDocVariantData): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store an array of integer into a TBsonVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray

function BsonVariantFromIntegers(const Integers: array of integer): variant;

/// store an array of 64 bit integer into a TBsonVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BsonVariantFromInt64s(const Integers: array of Int64): variant;

/// parse the header of a BSON encoded binary buffer, and return its length
// - BSON should point to a "int32 e_list #0" BSON document (like TBsonDocument)
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value, and raise an
// EBsonException if this comparison fails
// - as an alternative, consider using TBsonIterator, which wrap both a PByte
// and a TBsonElement into one convenient item
function BsonParseLength(var BSON: PByte; ExpectedBSONLen: integer = 0): integer;

/// parse the next element in supplied BSON encoded binary buffer list
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document
// - will decode the supplied binary buffer as a variant, then it will let BSON
// point to the next element, and return TRUE
// - returns FALSE when you reached betEOF, so that you can use it in a loop:
// ! var bson: PByte;
// !     name: RawUtf8;
// !     value: variant;
// ! ...
// ! BsonParseLength(bson);
// ! while BsonParseNextElement(bson,name,value) do
// !   writeln(name,':',value);
// - by default, it will return TBsonVariant custom variants for documents or
// arrays - but if storeDocArrayAsDocVariant is set, it will return a
// TDocVariant custom kind of variant, able to access to its nested
// properties via late-binding
// - if you want to parse a BSON list as fast as possible, you should better use
// TBsonElement.FromNext() which avoid any memory allocation (the SAX way) - in
// fact, this function is just a wrapper around TBsonElement.FromNext + ToVariant
// - as an alternative, consider using TBsonIterator, which wrap both a PByte
// and a TBsonElement into one convenient item
function BsonParseNextElement(var BSON: PByte; var name: RawUtf8;
  var element: variant; DocArrayConversion: TBsonDocArrayConversion = asBsonVariant): boolean;

/// search for a property by number in a a supplied BSON encoded binary buffer
// - BSON should point to a "int32 e_list #0" BSON document (like TBsonDocument)
// - returns FALSE if the list has too few elements (starting at index 0)
// - otherwise, returns TRUE then let item point to the corresponding element
function BsonPerIndexElement(BSON: PByte; index: integer; var item: TBsonElement): boolean;

/// convert a BSON document into a TDocVariant variant instance
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - by definition, asBsonVariant is not allowed as Option value
procedure BsonToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer = 0;
  Option: TBsonDocArrayConversion = asDocVariantPerReference);

/// convert a TBsonDocument into a TDocVariant variant instance
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - by definition, asBsonVariant is not allowed as Option value
function BsonDocumentToDoc(const BSON: TBsonDocument;
  Option: TBsonDocArrayConversion = asDocVariantPerReference): variant;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON document into its JSON representation
// - BSON should point to a "int32 e_list #0" BSON document
// - Kind should be either betDoc or betArray
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BsonToJson(BSON: PByte; Kind: TBsonElementType;
  ExpectedBSONLen: integer = 0; Mode: TMongoJsonMode = modMongoStrict): RawUtf8;

/// convert a TBsonDocument into its JSON representation
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BsonDocumentToJson(const BSON: TBsonDocument;
  Mode: TMongoJsonMode = modMongoStrict): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON list of elements into its JSON representation
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document,
// i.e. the item data as expected by TBsonElement.FromNext()
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure BsonListToJson(BsonList: PByte; Kind: TBsonElementType;
  W: TJsonWriter; Mode: TMongoJsonMode = modMongoStrict);

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure AddMongoJson(const Value: variant; W: TJsonWriter;
  Mode: TMongoJsonMode = modMongoStrict); overload;

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - in addition to default modMongoStrict as rendered by VariantSaveJson(),
// this function can render the supplied variant with the Mongo Shell syntax
// or even raw JSON content
function VariantSaveMongoJson(const Value: variant; Mode: TMongoJsonMode): RawUtf8;



implementation


{ ************ BSON Decimal128 Value }

{ TDecimal128 }

// see https://github.com/mongodb/libbson/blob/master/src/bson/bson-decimal128.c

procedure TDecimal128.SetZero;
begin
  Bits.lo := 0;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

const
  D128: array[TDecimal128SpecialValue] of TDecimal128Bits = (
    // dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_INT64POS
  ), (
    lo: 0;
    hi: $7800000000000000
  ), (
    lo: 0;
    hi: QWord($f800000000000000)
  ), (
    lo: $378d8e63ffffffff;
    hi: QWord($dfffed09bead87c0)
  ), (
    lo: $378d8e63ffffffff;
    hi: $5fffed09bead87c0
  ));

procedure TDecimal128.SetSpecial(special: TDecimal128SpecialValue);
begin
  Bits := D128[special];
end;

function TDecimal128.IsSpecial: TDecimal128SpecialValue;
begin
  for result := dsvNan to high(D128) do
    if (D128[result].hi = Bits.hi) and
       (D128[result].lo = Bits.lo) then
      exit;
  result := dsvValue;
end;

procedure TDecimal128.FromInt32(value: integer);
begin
  if value >= 0 then
  begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end
  else
  begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromUInt32(value: cardinal);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

procedure TDecimal128.FromInt64(value: Int64);
begin
  if value >= 0 then
  begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end
  else
  begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromQWord(value: QWord);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

function TDecimal128.FromFloat(const value: TSynExtended; precision: integer): boolean;
var
  tmp: ShortString;
begin
  if (precision <= 0) or
     (precision = DOUBLE_PRECISION) then
    tmp[0] := AnsiChar(DoubleToShort(@tmp, value))
  else
    tmp[0] := AnsiChar(ExtendedToShort(@tmp, value, precision));
  result := true;
  case FloatToShortNan(tmp) of
    fnNan:
      SetSpecial(dsvNan);
    fnInf:
      SetSpecial(dsvPosInf);
    fnNegInf:
      SetSpecial(dsvNegInf);
  else
    result := FromText(@tmp[1], ord(tmp[0])) <> dsvError;
  end;
end;

procedure TDecimal128.FromCurr(const value: currency);
begin
  // force exactly 4 decimals
  if value < 0 then
  begin
    Bits.lo := -PInt64(@value)^;
    Bits.hi := QWord(BSON_DECIMAL128_HI_CURRNEG);
  end
  else
  begin
    Bits.lo := PInt64(@value)^;
    Bits.hi := BSON_DECIMAL128_HI_CURRPOS;
  end;
end;

function TDecimal128.Equals(const other: TDecimal128): boolean;
begin
  result := (Bits.lo = other.Bits.lo) and
            (Bits.hi = other.Bits.hi);
end;

function div128bits9digits(var value: THash128Rec): PtrUInt;
var
  r, t: QWord;
  i: PtrUInt;
begin
  r := 0;
  for i := 0 to high(value.c) do
  begin
    {$ifdef CPU32} // circumvent bug at least with FPC 3.2
    Int64Rec(r).Hi := Int64Rec(r).Lo;
    Int64Rec(r).Lo := 0;
    {$else}
    r := r shl 32;    // adjust remainder to match value of next dividend
    {$endif CPU32}
    inc(r, value.c[i]); // add the divided to _rem
    if r = 0 then
      continue;
    t := r div 1000000000;
    value.c[i] := t;
    dec(r, t * 1000000000);
  end;
  result := r;
end;

procedure append(var dest: PUtf8Char; var dig: PByte; digits: PtrInt);
  {$ifdef HASINLINE}inline;{$endif}
begin
  if digits > 0 then
    repeat
      dest^ := AnsiChar(dig^ + ord('0'));
      inc(dig);
      inc(dest);
      dec(digits);
      if digits = 0 then
        break;
    until false;
end;

function TDecimal128.ToText(out Buffer: TDecimal128Str): integer;
var
  dest: PUtf8Char;
  dig: PByte;
  exp, sciexp, signdig, radixpos, j, k: PtrInt;
  combi, biasedexp, signmsb: PtrUInt;
  leastdig, fastdiv: cardinal;
  digbuffer: array[0..35] of byte;
  _128: THash128Rec;
begin
  dest := @Buffer;
  if Bits.h < 0 then
  begin
    dest^ := '-';
    inc(dest);
  end;
  if (Bits.lo = 0) and
     (Bits.hi = 0) then
  begin
    dest^ := '0';
    result := 1;
    exit;
  end;
  combi := (Bits.c[3] shr 26) and $1f;
  if combi shr 3 = 3 then
    case combi of
      30:
        begin
          result := AppendRawUtf8ToBuffer(dest,
            DECIMAL128_SPECIAL_TEXT[dsvPosInf]) - PUtf8Char(@Buffer);
          exit;
        end;
      31:
        begin
          result := AppendRawUtf8ToBuffer(@Buffer,
            DECIMAL128_SPECIAL_TEXT[dsvNan]) - PUtf8Char(@Buffer);
          exit;
        end;
    else
      begin
        biasedexp := (Bits.c[3] shr 15) and $3fff;
        signmsb := ((Bits.c[3] shr 14) and 1) + 8;
      end;
    end
  else
  begin
    biasedexp := (Bits.c[3] shr 17) and $3fff;
    signmsb := (Bits.c[3] shr 14) and 7;
  end;
  exp := biasedexp - BSON_DECIMAL128_EXPONENT_BIAS;
  _128.c[0] := (Bits.c[3] and $3fff) + ((signmsb and $0f) shl 14);
  _128.c[1] := Bits.c[2];
  _128.c[2] := Bits.c[1];
  _128.c[3] := Bits.c[0];
  FillCharFast(digbuffer, SizeOf(digbuffer), 0);
  dig := @digbuffer;
  if ((_128.lo = 0) and
      (_128.hi = 0)) or
     (_128.c[0] >= 1 shl 17) then
    signdig := 1 // non-canonical or zero -> 0
  else
  begin
    for k := 3 downto 0 do
    begin
      if (_128.lo = 0) and
         (_128.hi = 0) then
        break;
      leastdig := div128bits9digits(_128);
      if leastdig = 0 then
        continue;
      for j := 8 downto 0 do
      begin
        {$ifdef CPU32DELPHI}
        asm // Delphi compiler is not efficient about division
          mov     eax, leastdig
          mov     fastdiv, eax
          mov     edx, 3435973837
          mul     edx
          shr     edx, 3
          mov     leastdig, edx
        end;
        {$else}
        fastdiv := leastdig;
        leastdig := leastdig div 10; // FPC will use reciprocal division
        {$endif CPU32DELPHI}
        digbuffer[k * 9 + j] := fastdiv - leastdig * 10;
        if leastdig = 0 then
          break;
      end;
    end;
    signdig := 36; // 4*9 = k*j loops above
    while dig^ = 0 do
    begin
      dec(signdig);
      inc(dig);
    end;
  end;
  sciexp := signdig - 1 + exp;
  if (sciexp < -6) or
     (exp > 0) then
  begin
    // scientific format
    dest^ := AnsiChar(dig^ + ord('0'));
    inc(dig);
    inc(dest);
    dec(signdig);
    if signdig <> 0 then
    begin
      dest^ := '.';
      inc(dest);
      append(dest, dig, signdig);
    end;
    if sciexp > 0 then
      PWord(dest)^ := ord('E') + ord('+') shl 8
    else
    begin
      PWord(dest)^ := ord('E') + ord('-') shl 8;
      sciexp := -sciexp;
    end;
    dest := AppendUInt32ToBuffer(dest + 2, sciexp)
  end
  else
  begin
    if exp >= 0 then // regular format with no decimal place
      append(dest, dig, signdig)
    else
    begin
      radixpos := signdig + exp;
      if radixpos > 0 then // non-zero digits before radix
        append(dest, dig, radixpos)
      else
      begin
        dest^ := '0'; // leading zero before radix point
        inc(dest);
      end;
      dest^ := '.';   // radix char
      inc(dest);
      while radixpos < 0 do
      begin
        // leading zeros after radix
        dest^ := '0';
        inc(dest);
        inc(radixpos);
      end;
      append(dest, dig, signdig - radixpos);
    end;
  end;
  result := dest - PUtf8Char(@Buffer);
end;

function TDecimal128.ToText: RawUtf8;
var
  tmp: TDecimal128Str;
begin
  FastSetString(result, @tmp, ToText(tmp));
end;

procedure TDecimal128.ToText(var result: RawUtf8);
var
  tmp: TDecimal128Str;
begin
  FastSetString(result, @tmp, ToText(tmp));
end;

procedure TDecimal128.AddText(W: TJsonWriter);
var
  tmp: TDecimal128Str;
begin
  W.AddNoJsonEscape(@tmp, ToText(tmp));
end;

function TDecimal128.ToVariant: variant;
begin
  ToVariant(result);
end;

procedure TDecimal128.ToVariant(out Result: variant);
begin
  with TBsonVariantData(Result) do
  begin
    VType := BsonVariantType.VarType;
    VKind := betDecimal128;
    VBlob := nil;
    FastSetRawByteString(RawByteString(VBlob), @Bits, SizeOf(TDecimal128));
  end;
end;

function TDecimal128.ToFloat: TSynExtended;
var
  tmp: TDecimal128Str;
begin
  tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
  result := GetExtended(@tmp);
end;

function TDecimal128.ToCurr: currency;
begin
  ToCurr(result);
end;

procedure TDecimal128.ToCurr(out result: currency);
var
  tmp: TDecimal128Str;
  res64: Int64 absolute result;
begin
  if Bits.hi = QWord(BSON_DECIMAL128_HI_CURRNEG) then // was e.g. FromCurr
    res64 := -Bits.lo
  else if Bits.hi = BSON_DECIMAL128_HI_CURRPOS then
    res64 := Bits.lo
  else
  begin
    tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
    res64 := StrToCurr64(@tmp);
  end;
end;

function TDecimal128.FromText(text: PUtf8Char; textlen: integer): TDecimal128SpecialValue;
var
  P, PEnd: PUtf8Char;
  c: AnsiChar;
  flags: set of (negative, signed, radix, nonzero);
  digits: array[0..BSON_DECIMAL128_MAX_DIGITS - 1] of byte;
  firstnon0, digread, digstored, digcount, radixpos, digfirst, diglast, exp,
    signdig, i: PtrInt;
  signhi, signlo, biasedexp: QWord;
  sign: THash128Rec;
begin
  for result := dsvNan to dsvNegInf do
    if IdemPropNameU(DECIMAL128_SPECIAL_TEXT[result], text, textlen) then
    begin
      Bits := D128[result];
      exit; // fast recognition of special text values (including '0')
    end;
  Bits := D128[dsvError];
  result := dsvError;
  if (textlen = 0) or
     (text = nil) then
    exit;
  P := text;
  PEnd := text + textlen;
  flags := [];
  if P^ in ['+', '-'] then
  begin
    include(flags, signed);
    if P^ = '-' then
      include(flags, negative);
    inc(P);
  end;
  digcount := 0;
  digread := 0;
  digstored := 0;
  radixpos := 0;
  firstnon0 := 0;
  exp := 0;
  while P < PEnd do
  begin
    c := P^;
    case c of
      '.':
        if radix in flags then // duplicated '.'
          exit
        else
        begin
          include(flags, radix);
          inc(P);
          continue;
        end;
      '0'..'9':
        if digstored < BSON_DECIMAL128_MAX_DIGITS then
          if (c > '0') or
             (nonzero in flags) then
          begin
            if not (nonzero in flags) then
            begin
              firstnon0 := digread;
              include(flags, nonzero);
            end;
            digits[digstored] := ord(c) - ord('0');
            inc(digstored);
          end;
      'E', 'e':
        begin
          inc(P);
          if P >= PEnd then
            exit;
          exp := GetInteger(P, PEnd);
          break;
        end;
    else
      exit;
    end;
    if nonzero in flags then
      inc(digcount);
    if radix in flags then
      inc(radixpos);
    inc(digread);
    inc(P);
  end;
  if digread = 0 then
    exit;
  digfirst := 0;
  if digstored = 0 then
  begin
    // value is zero
    diglast := 0;
    digits[0] := 0;
    digcount := 1;
    digstored := 1;
    signdig := 0;
  end
  else
  begin
    diglast := digstored - 1;
    signdig := digcount;
    // handle trailing zeros as non-significant
    while text[firstnon0 + signdig - 1 +
               ord(radix in flags) + ord(signed in flags)] = '0' do
      dec(signdig);
  end;
  if (exp <= radixpos) and
     (radixpos - exp > 1 shl 14) then
    exp := BSON_DECIMAL128_EXPONENT_MIN
  else
    dec(exp, radixpos);
  while exp > BSON_DECIMAL128_EXPONENT_MAX do
  begin
    inc(diglast);
    digits[diglast] := 0;
    if diglast - digfirst > BSON_DECIMAL128_MAX_DIGITS then
      if signdig = 0 then
      begin
        // zero clamping is allowed
        exp := BSON_DECIMAL128_EXPONENT_MAX;
        break;
      end
      else
        exit; // overflow is not permitted
    dec(exp);
  end;
  while (exp < BSON_DECIMAL128_EXPONENT_MIN) or
        (digstored < digcount) do
  begin
    if diglast = 0 then
      if signdig = 0 then
      begin
        // zero clamping
        exp := BSON_DECIMAL128_EXPONENT_MIN;
        break;
      end
      else
        exit; // overflow
    if digstored < digcount then
      if (text[digcount - 1 + ord(signed in flags) +
          ord(radix in flags)] <> '0') and
         (signdig <> 0) then
        exit
      else // overflow
        dec(digcount)
    else // adjust to non stored digits
    if digits[diglast] <> 0 then
      exit
    else // inexact rounding
      dec(diglast); // adjust to round
    if exp < BSON_DECIMAL128_EXPONENT_MAX then
      inc(exp)
    else
      exit;
  end;
  if diglast - digfirst + 1 < signdig then
    if text[firstnon0 + diglast +
            ord(signed in flags) + ord(radix in flags)] <> '0' then
      exit; // inexact rouding
  signhi := 0;
  signlo := 0;
  if signdig <> 0 then // if not zero
    {$ifdef CPU32DELPHI} // use "shl" under x86 to avoid slower "call _llmul"
    if diglast - digfirst < 17 then
      for i := digfirst to diglast do
        inc(signlo, signlo + signlo shl 3 + digits[i])
    else
    begin
      for i := digfirst to diglast - 17 do
        inc(signhi, signhi + signhi shl 3 + digits[i]);
      for i := diglast - 16 to diglast do
        inc(signlo, signlo + signlo shl 3 + digits[i]);
    end;
    {$else}
    if diglast - digfirst < 17 then
      for i := digfirst to diglast do
        signlo := signlo * 10 + digits[i]
    else
    begin
      for i := digfirst to diglast - 17 do
        signhi := signhi * 10 + digits[i];
      for i := diglast - 16 to diglast do
        signlo := signlo * 10 + digits[i];
    end;
    {$endif CPU32DELPHI}
  if signhi = 0 then
  begin
    sign.L := signlo;
    sign.H := 0;
  end
  else
  begin
    mul64x64(signhi, 100000000000000000, sign);
    inc(sign.L, signlo);
    {$ifdef FPC}
    if sign.L < signlo then
    {$else} // manual QWord processs for less efficient Delphi compilers
    if (sign.c1 < TQWordRec(signlo).H) or
       ((sign.c1 = TQWordRec(signlo).H) and
        (sign.c0 < TQWordRec(signlo).L)) then
    {$endif FPC}
      inc(sign.H);
  end;
  biasedexp := exp + BSON_DECIMAL128_EXPONENT_BIAS;
  if (sign.H shr 49) and 1 <> 0 then
    Bits.hi := (QWord(3) shl 61) or
               ((biasedexp and $3fff) shl 47) or
               (sign.H and $7fffffffffff)
  else
    Bits.hi := ((biasedexp and $3fff) shl 49) or
               (sign.H and $1ffffffffffff);
  Bits.lo := sign.L;
  if negative in flags then
    Bits.c[3] := Bits.c[3] or $80000000;
  result := dsvValue;
end;

function TDecimal128.FromText(const text: RawUtf8): TDecimal128SpecialValue;
begin
  result := FromText(pointer(text), length(text));
end;

function TDecimal128.FromVariant(const value: variant): boolean;
var
  txt: RawUtf8;
  wasString: boolean;
  bson: TBsonVariantData absolute value;
  v64: Int64;
begin
  if bson.VType = varVariantByRef then
  begin
    result := FromVariant(PVariant(TVarData(value).VPointer)^);
    exit;
  end;
  if (bson.VType = BsonVariantType.VarType) and
     (bson.VKind = betDecimal128) then
    Bits := PDecimal128(bson.VBlob)^.Bits
  else if bson.VType = varWord64 then
    FromQWord(TVarData(value).VInt64)
  else if VariantToInt64(value, v64) then
    FromInt64(v64)
  else if bson.VType = varCurrency then
    FromCurr(TVarData(value).VCurrency)
  else
  begin
    VariantToUtf8(value, txt, wasString);
    result := FromText(txt) <> dsvError;
    exit;
  end;
  result := true;
end;


{ ************ BSON ObjectID Value }

{ TBsonObjectID }

const
  COUNTER_MASK = $ffffff;

var
  GlobalBsonObjectID: record
    Safe: TLightLock;
    DefaultValues: packed record
      Counter: cardinal;
      MachineID: TBson24;
      ProcessID: word;
    end;
    LastCreateTime: cardinal;
    LastCounter: cardinal;
  end;

procedure InitBsonObjectIDComputeNew;
begin
  with GlobalBsonObjectID.DefaultValues do
  begin
    Counter := Random32 and COUNTER_MASK;
    with Executable do
      PCardinal(@MachineID)^ := crc32c(crc32c(
        0, pointer(Host), length(Host)), pointer(User), length(User));
    ProcessID := crc32c(0, @MainThreadID, SizeOf(MainThreadID)); // lower 16-bit
  end;
end;

procedure TBsonObjectID.Init;
begin
  // 12 bytes fill zero
  with PHash128Rec(@self)^ do
  begin
    i0 := 0;
    i1 := 0;
    i2 := 0;
  end;
end;

procedure TBsonObjectID.ComputeNew;
var
  now, count: cardinal;
begin
  now := UnixTimeUtc; // fast API call (no need of cache) outside of the lock
  with GlobalBsonObjectID do
  begin
    Safe.Lock;
    {$ifdef HASFASTTRYFINALLY}
    try
    {$else}
    begin
    {$endif HASFASTTRYFINALLY}
      if now > LastCreateTime then
      begin
        LastCreateTime := now;
        count := DefaultValues.Counter; // reset
      end
      else
      begin
        count := LastCounter + 1;
        if count and COUNTER_MASK = DefaultValues.Counter then
        begin
          count := DefaultValues.Counter;
          inc(LastCreateTime); // collision -> cheat on timestamp
        end;
      end;
      Counter.b1 := count shr 16; // stored as bigendian
      Counter.b2 := count shr 8;
      Counter.b3 := count;
      LastCounter := count;
      UnixCreateTime := bswap32(LastCreateTime);
      MachineID := DefaultValues.MachineID;
      ProcessID := DefaultValues.ProcessID;
    {$ifdef HASFASTTRYFINALLY}
    finally
    {$endif HASFASTTRYFINALLY}
      Safe.UnLock;
    end;
  end;
end;

function TBsonObjectID.Equal(const Another: TBsonObjectID): boolean;
begin
  // first check Counter last field, which is more likely to diverse
  {$ifdef CPU64}
  result := (PIntegerArray(@Self)[2] = PIntegerArray(@Another)[2]) and
            (PInt64(@Self)^ = PInt64(@Another)^);
  {$else}
  result := (PIntegerArray(@Self)[2] = PIntegerArray(@Another)[2]) and
            (PIntegerArray(@Self)[1] = PIntegerArray(@Another)[1]) and
            (PIntegerArray(@Self)[0] = PIntegerArray(@Another)[0]);
  {$endif CPU64}
end;

function TBsonObjectID.Equal(const Another: variant): boolean;
var
  oid2: TBsonObjectID;
begin
  result := oid2.FromVariant(Another) and
            Equal(oid2);
end;

function TBsonObjectID.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(bswap32(UnixCreateTime));
end;

function TBsonObjectID.ToText: RawUtf8;
begin
  ToText(result);
end;

function TBsonObjectID.ToVariant: variant;
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    VType := BsonVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

procedure TBsonObjectID.ToVariant(var result: variant);
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    VType := BsonVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

function TBsonObjectID.FromText(const Text: RawUtf8): boolean;
begin
  if length(Text) = SizeOf(self) * 2 then
    result := mormot.core.text.HexToBin(Pointer(Text), @self, SizeOf(self))
  else
    result := false;
end;

function TBsonObjectID.FromText(Text: PUtf8Char): boolean;
begin
  result := mormot.core.text.HexToBin(Pointer(Text), @self, SizeOf(self));
end;

function TBsonObjectID.FromVariant(const value: variant): boolean;
var
  txt: RawUtf8;
  wasString: boolean;
  bson: PBsonVariantData;
begin
  bson := @value;
  if bson^.VType = varVariantByRef then
    bson := TVarData(value).VPointer;
  if (bson^.VType = BsonVariantType.VarType) and
     (bson^.VKind = betObjectID) then
  begin
    self := bson^.VObjectID;
    result := true;
  end
  else
  begin
    VariantToUtf8(value, txt, wasString);
    result := wasString and FromText(txt);
  end;
end;

procedure TBsonObjectID.ToText(var result: RawUtf8);
begin
  FastSetString(result, nil, SizeOf(self) * 2);
  mormot.core.text.BinToHex(@self, pointer(result), SizeOf(self));
end;



{ ************ TBsonVariantData / TBsonVariant Custom Variant Storage }

// defined here for proper inlining
procedure TBsonElement.FromBsonVariant(aValue: PVarData);
begin
  // here we know that aValue is a TBsonVariantData
  Name := nil;
  NameLen := 0;
  Kind := PBsonVariantData(aValue)^.VKind;
  if Kind = betObjectID then
  begin
    ElementBytes := SizeOf(TBsonObjectID);
    Element := @PBsonVariantData(aValue)^.VObjectID;
  end
  else
    FromBson(PBsonVariantData(aValue)^.VBlob);
end;


{ TBsonVariant }

constructor TBsonVariant.Create;
begin
  fOptions := [sioHasTryJsonToVariant, sioHasToJson];
  inherited Create;
end;

procedure TBsonVariant.ToJson(W: TJsonWriter; Value: PVarData);
var
  item: TBsonElement;
begin
  {%H-}item.FromBsonVariant(Value); // Value is a TBsonVariantData
  item.AddMongoJson(W, modMongoStrict);
end;

function TBsonVariant.GetNewDoc(const BsonDoc: TBsonDocument): variant;
begin
  FromBsonDocument(BsonDoc, result);
end;

function TBsonVariant.IsOfKind(const V: variant; Kind: TBsonElementType): boolean;
begin
  with TBsonVariantData(V) do
    if VType = varVariantByRef then
      result := IsOfKind(PVariant(TVarData(V).VPointer)^, Kind)
    else
      result := (self <> nil) and
                (VType = VarType) and
                (VKind = Kind);
end;

function TBsonVariant.ToBlob(const V: Variant; var Blob: RawByteString): boolean;
begin
  with TVarData(V) do
    if VType = varVariantByRef then
    begin
      result := ToBlob(PVariant(VPointer)^, Blob);
      exit;
    end;
  with TBsonVariantData(V) do
  begin
    result := (VType = VarType) and
              (VKind = betBinary);
    if result then
      if (VBlob = nil) or
         (PInteger(VBlob)^ <> Length(RawByteString(VBlob)) - (SizeOf(integer) + 1)) then
        Blob := ''
      else
        FastSetRawByteString(Blob, PAnsiChar(VBlob) + (SizeOf(integer) + 1), PInteger(VBlob)^);
  end;
end;

procedure TBsonVariant.FromBinary(const Bin: RawByteString;
  BinType: TBsonElementBinaryType; var result: variant);
var
  Len: integer;
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    if Bin = '' then
    begin
      VType := varNull; // stores a NULL
      exit;
    end;
    // stored as "\x05" e_name int32 BinType (byte*)
    VType := VarType;
    VKind := betBinary;
    VBlob := nil; // avoid GPF here below
    Len := length(Bin);
    SetLength(RawByteString(VBlob), Len + (SizeOf(integer) + 1));
    PInteger(VBlob)^ := Len;
    PByteArray(VBlob)^[SizeOf(integer)] := ord(BinType);
    MoveFast(pointer(Bin)^, PByteArray(VBlob)^[SizeOf(integer) + 1], Len);
  end;
end;

procedure TBsonVariant.FromBsonDocument(const BsonDoc: TBsonDocument;
  var result: variant; Kind: TBsonElementType);
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    VType := VarType;
    VKind := Kind;
    VBlob := nil; // avoid GPF here below
    RawByteString(VBlob) := BsonDoc;
  end;
end;

procedure TBsonVariant.FromJson(json: PUtf8Char; var result: variant);
begin
  VarClear(result);
  if json = nil then
    exit;
  if json^ in [#1..' '] then
    repeat
      inc(json)
    until not (json^ in [#1..' ']);
  if json^ in ['{', '['] then
    with TBsonVariantData(result) do
    begin
      VType := VarType;
      VBlob := nil; // avoid GPF here below
      VKind := JsonBufferToBsonDocument(json, TBsonDocument(VBlob));
    end
  else
    VariantLoadJson(result, json);
end;

const
  BSON_JSON_NEWDATE: string[8] = 'ew Date('; // circumvent Delphi XE4 Win64 bug

function TBsonVariant.TryJsonToVariant(var Json: PUtf8Char; var Value: variant;
  EndOfObject: PUtf8Char): boolean;
// warning: code should NOT modify JSON buffer in-place, unless it returns true
var
  bsonvalue: TBsonVariantData absolute Value;
  varvalue: TVarData absolute Value;

  procedure Return(kind: TBsonElementType; P: PUtf8Char;
    GotoEndOfObject: AnsiChar);
  begin
    if GotoEndOfObject <> #0 then
      while P^ <> GotoEndOfObject do
        if P^ = #0 then
        begin
          if kind in [betRegEx, betDecimal128] then
            RawByteString(bsonvalue.VBlob) := ''; // avoid memory leak
          exit;
        end
        else
          inc(P);
    P := GotoNextNotSpace(P + 1);
    if EndOfObject <> nil then
      EndOfObject^ := P^;
    inc(P, ord(P^ <> #0));
    Json := P;
    case kind of
      betObjectID,
      betRegEx,
      betDecimal128:
        begin
          // see TBsonWriter.BsonWrite()
          bsonvalue.VType := VarType;
          bsonvalue.VKind := kind;
        end;
      betDateTime:
        varvalue.VType := varDate;
    end;
    result := true;
  end;

  procedure ReturnInt(kindint: integer; P: PUtf8Char;
    GotoEndOfObject: AnsiChar);
    {$ifdef HASINLINE}inline;{$endif}
  // redirection function to circumvent FPC trunk limitation
  var
    kind: TBsonElementType absolute kindint;
  begin
    Return(kind, P, GotoEndOfObject);
  end;

  procedure TryDate(P: PUtf8Char; GotoEndOfObject: AnsiChar);
  var
    L: integer;
  begin
    P := GotoNextNotSpace(P);
    if GotoEndOfObject = ')' then
      if P^ = ')' then
      begin
        // new date() constructor
        varvalue.VDate := NowUtc;
        Return(betDateTime, P, #0);
        exit;
      end
      else if P^ in ['0'..'9'] then
      begin
        varvalue.VDate := GetNextItemDouble(P, ')');
        if (varvalue.VDate <> 0) and
           (P <> nil) then
        begin
          Return(betDateTime, P - 1, #0);
          exit;
        end;
      end;
    if P^ <> '"' then
      exit;
    if PCardinal(P)^ = JSON_SQLDATE_MAGIC_QUOTE_C then
      inc(P, 3); // ignore\uFFF1 code for DateTimeToSql/TimeLogToSql functions
    L := 1;
    while P[L] <> '"' do
      if P[L] <= ' ' then
        exit
      else
        inc(L);
    Iso8601ToDateTimePUtf8CharVar(P + 1, L, varvalue.VDate);
    if varvalue.VDate <> 0 then
      Return(betDateTime, P + L + 1, GotoEndOfObject);
  end;

  procedure TryObjectID(P: PUtf8Char; GotoEndOfObject: AnsiChar);
  begin
    P := GotoNextNotSpace(P);
    if (GotoEndOfObject = ')') and
       (P^ = ')') then
    begin
      // ObjectId() constructor
      bsonvalue.VObjectID.ComputeNew;
      Return(betObjectID, P, #0);
      exit;
    end;
    if P^ <> '"' then
      exit;
    if bsonvalue.VObjectID.FromText(P + 1) then
      Return(betObjectID, P + 25, GotoEndOfObject);
  end;

  procedure TryDecimal(P: PUtf8Char; GotoEndOfObject: AnsiChar);
  var
    dec: TDecimal128;
    L: integer;
  begin
    if P^ <> '"' then
      exit;
    inc(P);
    L := 0;
    while P[L] <> '"' do
      if not (P[L] in ['0'..'9', 'e', 'E', '+', '-', '.']) then
        exit
      else
        inc(L);
    if dec.FromText(P, L) = dsvError then
      exit;
    bsonvalue.VBlob := nil; // avoid GPF
    FastSetRawByteString(RawByteString(bsonvalue.VBlob), @dec, SizeOf(TDecimal128));
    Return(betDecimal128, P + L + 1, GotoEndOfObject);
  end;

var
  Reg, Opt: PUtf8Char;
  RegLen, OptLen: integer;

  procedure ReturnRegEx(P: PUtf8Char; GotoEndOfObject: AnsiChar);
  var
    buf: PAnsiChar;
  begin
    bsonvalue.VBlob := nil; // avoid GPF
    FastSetRawByteString(RawByteString(bsonvalue.VBlob), nil, RegLen + OptLen + 2);
    buf := bsonvalue.VBlob;
    MoveFast(Reg^, buf^, RegLen);
    inc(buf, RegLen);
    buf^ := #0;
    inc(buf);
    MoveFast(Opt^, buf^, OptLen);
    inc(buf, OptLen);
    buf^ := #0;
    Return(betRegEx, P, GotoEndOfObject);
  end;

  procedure TryRegExShell(P: PUtf8Char);
  begin
    RegLen := 0;
    while P[RegLen] <> '/' do
      if P[RegLen] <= ' ' then
        exit
      else
        inc(RegLen);
    Reg := P;
    inc(P, RegLen);
    if P^ <> '/' then
      exit
    else
      inc(P);
    OptLen := 0;
    while tcWord in TEXT_CHARS[P[OptLen]] do
      inc(OptLen);
    if P[OptLen] = #0 then
      exit;
    Opt := P;
    ReturnRegEx(Opt + OptLen - 1, #0);
  end;

  procedure TryRegExStrict(P: PUtf8Char);
  begin
    // warning: this won't escape double quotes...
    P := GotoNextNotSpace(P);
    if P^ <> '"' then
      exit
    else
      inc(P);
    RegLen := 0;
    while P[RegLen] <> '"' do
      if P[RegLen] <= ' ' then
        exit
      else
        inc(RegLen);
    Reg := P;
    P := GotoNextNotSpace(Reg + RegLen + 1);
    if P^ <> ',' then
      Exit; // $regex:"acme*.corp",$options:"i"}
    P := GotoNextNotSpace(P + 1);
    if P^ = '"' then
      inc(P);
    if PInt64(P)^ <> PInt64(@BSON_JSON_REGEX[1][4])^ then
      exit
    else
      inc(P, 8);
    if P^ = '"' then
      inc(P);
    P := GotoNextNotSpace(P);
    if P^ <> ':' then
      exit;
    P := GotoNextNotSpace(P + 1);
    if P^ <> '"' then
      exit
    else
      inc(P);
    OptLen := 0;
    while P[OptLen] <> '"' do
      if P[OptLen] <= ' ' then
        exit
      else
        inc(OptLen);
    Opt := P;
    ReturnRegEx(Opt + OptLen + 1, '}');
  end;

var
  P: PUtf8Char;
  up: PNormTableByte;
begin
  // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  result := false;
  // here JSON does not start with " or 1..9 (obvious simple types)
  P := Json;
  up := @NormToUpperAnsi7;
  case PNormTable(up)^[P^] of
    '{':
      begin
        // strict MongoDB objects e.g. {"$undefined":true} or {"$oid":".."}
        repeat
          inc(P)
        until not (P^ in [#1..' ']);
        if P^ <> '"' then
          exit;
        repeat
          inc(P)
        until not (P^ in [#1..' ']);
        if P[0] = '$' then
          case P[1] of
            'u':
              {$ifdef CPUINTEL}
              if (PInt64(P + 2)^ = $64656E696665646E) and
                 (PCardinal(P + 8)^ = $3A226465) then
              {$else} // ARM is not eficient at loading constants
              if (PInt64(P + 2)^ = PInt64(@BSON_JSON_UNDEFINED[false][5])^) and
                 (PCardinal(P + 8)^ = PCardinal(@BSON_JSON_UNDEFINED[false][11])^) then
              {$endif CPUINTEL}
                Return(betDeprecatedUndefined, P + 12, '}');
            'm':
              {$ifdef CPUINTEL}
              if PInt64(P + 1)^ = $3A2279654B6E696D then
              {$else}
              if PInt64(P + 1)^ = PInt64(@BSON_JSON_MINKEY[false][4])^ then
              {$endif CPUINTEL}
                ReturnInt(betMinKey, P + 9, '}')
              {$ifdef CPUINTEL}
              else if PInt64(P + 1)^ = $3A2279654B78616D then
              {$else}
              else if PInt64(P + 1)^ = PInt64(@BSON_JSON_MAXKEY[false][4])^ then
              {$endif CPUINTEL}
                ReturnInt(betMaxKey, P + 9, '}');
            'o':
              {$ifdef CPUINTEL}
              if PInteger(P + 2)^ = $3A226469 then
              {$else}
              if PInteger(P + 2)^ = PInteger(@BSON_JSON_OBJECTID[false, modMongoStrict][5])^ then
              {$endif CPUINTEL}
                TryObjectID(P + 6, '}');
            'd':
              {$ifdef CPUINTEL}
              if PInteger(P + 2)^ = $22657461 then
              {$else}
              if PInteger(P + 2)^ = PInteger(@BSON_JSON_DATE[modMongoStrict, false][5])^ then
              {$endif CPUINTEL}
                TryDate(P + 7, '}');
            'r':
              {$ifdef CPUINTEL}
              if PInt64(P)^ = $3A22786567657224 then
              {$else}
              if PInt64(P)^ = PInt64(@BSON_JSON_REGEX[0][3])^ then
              {$endif CPUINTEL}
                TryRegExStrict(P + 8);
            'n':
              {$ifdef CPUINTEL}
              if (PInt64(P)^ = $447265626D756E24) and
                 (PInt64(P + 8)^ = $3A226C616D696365) then
              {$else}
              if (PInt64(P)^ = PInt64(@BSON_JSON_DECIMAL[false, modMongoStrict][3])^) and
                 (PInt64(P + 8)^ = PInt64(@BSON_JSON_DECIMAL[false, modMongoStrict][11])^) then
              {$endif CPUINTEL}
                TryDecimal(P + 16, '}');
          end;
      end;
    // MongoDB Shell Mode extended syntax
    'U':
      if StrILNotNil(P + 1,  @BSON_JSON_UNDEFINED[true][2], up, 8) = 8 then
        Return(betDeprecatedUndefined, P + 8, #0);
    'M':
      if StrILNotNil(P + 1, @BSON_JSON_MINKEY[true][2], up, 5) = 5 then
        ReturnInt(betMinKey, P + 5, #0)
      else if StrILNotNil(P + 1, @BSON_JSON_MAXKEY[true][2], up, 7) = 7 then
        ReturnInt(betMaxKey, P + 5, #0);
    'O':
      if StrILNotNil(P + 1, @BSON_JSON_OBJECTID[false, modMongoShell][2], up, 8) = 8 then
        TryObjectID(P + 9, ')');
    'N':
      if StrILNotNil(P + 1, @BSON_JSON_NEWDATE[1], up, 8) = 8 then
        TryDate(P + 9, ')')
      else if StrILNotNil(P + 1, @BSON_JSON_DECIMAL[false, modMongoShell][2], up, 13) = 13 then
        TryDecimal(P + 14, ')');
    'I':
      if StrILNotNil(P + 1, @BSON_JSON_DATE[modMongoShell, false][2], up, 7) = 7 then
        TryDate(P + 8, ')');
    '/':
      TryRegExShell(P + 1);
  end;
end;

procedure TBsonVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest, Source, VarType);
end;

procedure TBsonVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  tmp: RawUtf8;
  wasString: boolean;
begin
  if AVarType = VarType then
  begin
    // content any variant content into BsonVariant()
    VariantToUtf8(Variant(Source), tmp, wasString);
    if wasString then
    begin
      VarClear(variant(Dest));
      if TBsonVariantData(Dest).VObjectID.FromText(tmp) then
      begin
        Dest.VType := VarType;
        TBsonVariantData(Dest).VKind := betObjectID;
        exit;
      end;
      variant(Dest) := BsonVariant(tmp); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end
  else
  begin
    if Source.VType <> VarType then
      RaiseCastError;
    with TBsonVariantData(Source) do
      if (VKind = betObjectID) and
         (AVarType in [varDate, varDouble]) then
      begin
        // convert an ObjectID to its TDateTime part
        Dest.VType := AVarType;
        Dest.VDate := VObjectID.CreateDateTime;
        exit;
      end
      else
      begin
        if VKind = betObjectID then
          // convert an ObjectID to its text representation
          VObjectID.ToText(tmp)
        else
          // convert other values to JSON
          tmp := VariantSaveMongoJson(variant(Source), modMongoShell);
        RawUtf8ToVariant(tmp, Dest, AVarType);
      end;
  end;
end;

procedure TBsonVariant.Clear(var V: TVarData);
begin
  if TBsonVariantData(V).VKind in BSON_ELEMENTVARIANTMANAGED then
    RawByteString(TBsonVariantData(V).VBlob) := '';
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TBsonVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: boolean);
begin
  if Indirect then
    SimplisticCopy(Dest, Source, true)
  else
  begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
    with TBsonVariantData(Dest) do
      if VKind in BSON_ELEMENTVARIANTMANAGED then
      begin
        VBlob := nil; // avoid GPF
        RawByteString(VBlob) := RawByteString(TBsonVariantData(Source).VBlob);
      end;
  end;
end;

procedure TBsonVariant.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
var
  res: integer;
  LeftU, RightU: RawUtf8;
begin
  LeftU := VariantSaveMongoJson(variant(Left), modMongoStrict);
  RightU := VariantSaveMongoJson(variant(Right), modMongoStrict);
  if LeftU = RightU then
    Relationship := crEqual
  else
  begin
    res := StrComp(pointer(LeftU), pointer(RightU));
    if res < 0 then
      Relationship := crLessThan
    else if res > 0 then
      Relationship := crGreaterThan
    else
      Relationship := crEqual;
  end;
end;



{ ************ TBsonElement/TBsonIterator for BSON Decoding }

// used by TBsonElement.ToVariant() method and BsonToDoc() procedure
procedure BsonItemsToDocVariant(Kind: TBsonElementType; BSON: PByte;
  var Doc: TDocVariantData; Option: TBsonDocArrayConversion);
const
  OPTIONS: array[TBsonDocArrayConversion] of TDocVariantOptions = (
    [],
    [dvoReturnNullForUnknownProperty],
    [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference],
    [dvoReturnNullForUnknownProperty, dvoInternNames],
    [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference, dvoInternNames]);
var
  k: TDocVariantKind;
  i, n, cap: integer;
  intnames: TRawUtf8Interning;
  items: array[0..63] of TBsonElement;
begin
  // very fast optimized code
  if BSON = nil then
    TVarData(Doc).VType := varNull
  else
  begin
    intnames := nil;
    case Kind of
      betDoc:
        begin
          k := dvObject;
          if dvoInternNames in Doc.Options then
            intnames := DocVariantType.InternNames;
        end;
      betArray:
        k := dvArray;
    else
      exit; // leave Doc=varEmpty
    end;
    Doc.Init(OPTIONS[Option], k);
    cap := 0;
    repeat // will handle up to 64 TBsonElement per loop (via items[])
      n := 0;
      while {%H-}items[n].FromNext(BSON) do
      begin
        inc(n);
        if n = length(items) then
          break; // avoid buffer overflow
      end;
      if n = 0 then
        break;
      inc(cap, n); // pre-allocate Doc.Names[]/Values[]
      if Doc.Capacity < cap then
        Doc.Capacity := NextGrow(cap); // faster for huge arrays
      for i := 0 to n - 1 do
      begin
        if Kind = betDoc then
          if intnames <> nil then
            intnames.Unique(Doc.Names[i + Doc.Count], items[i].Name, items[i].NameLen)
          else
            FastSetString(Doc.Names[i + Doc.Count], items[i].Name, items[i].NameLen);
        items[i].ToVariant(Doc.Values[i + Doc.Count], Option);
      end;
      Doc.SetCount(Doc.Count + n);
    until (BSON = nil) or
          (BSON^ = byte(betEOF));
  end;
end;


{ TBsonElement }

var
  /// size (in bytes) of a BSON element
  // - equals -1 for varying elements
  BSON_ELEMENTSIZE: array[TBsonElementType] of integer = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    0, SizeOf(Double), -1, -1, -1, -1,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    0, SizeOf(TBsonObjectID), 1, SizeOf(Int64),
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    0, -1, -1, -1, -1,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
    -1, SizeOf(integer), SizeOf(Int64), SizeOf(Int64), SizeOf(TDecimal128));

  /// types which do not have an exact equivalency to a standard variant
  // type will be mapped as varUnknown - and will be changed into
  // BsonVariantType.VarType
  BSON_ELEMENTTYPES: array[TBsonElementType] of word = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    varEmpty, varDouble, varString, varUnknown, varUnknown, varUnknown,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    varEmpty, varUnknown, varBoolean, varDate,
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    varNull, varUnknown, varUnknown, varUnknown, varUnknown,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
    varUnknown, varInteger, varUnknown, varInt64, varUnknown);

function TBsonElement.ToVariant(DocArrayConversion: TBsonDocArrayConversion): variant;
begin
  ToVariant(result, DocArrayConversion);
end;

procedure TBsonElement.ToVariant(var result: variant;
  DocArrayConversion: TBsonDocArrayConversion);
var
  res: TVarData absolute result;
  resBSON: TBsonVariantData absolute result;
begin
  VarClear(result);
  res.VAny := nil; // avoid GPF below
  case Kind of
    betFloat:
      res.VDouble := unaligned(PDouble(Element)^);
    betString:
      FastSetString(RawUtf8(res.VAny), Data.Text, Data.TextLen);
    betJS,
    betDeprecatedSymbol:
      FastSetString(RawUtf8(resBSON.VText), Data.Text, Data.TextLen);
    betDoc,
    betArray:
      if DocArrayConversion = asBsonVariant then
        FastSetRawByteString(TBsonDocument(resBSON.VBlob), Element, ElementBytes)
      else
      begin
        BsonItemsToDocVariant(Kind, Data.DocList, TDocVariantData(result), DocArrayConversion);
        exit;
      end;
    betBinary,
    betRegEx,
    betDeprecatedDbptr,
    betJSScope,
    betTimestamp,
    betDecimal128:
      FastSetRawByteString(RawByteString(resBSON.VBlob), Element, ElementBytes);
    betObjectID:
      resBSON.VObjectID := PBsonObjectID(Element)^;
    betBoolean:
      res.VInteger := PByte(Element)^; // canonical VBoolean
    betDateTime:
      res.VDate := UnixMSTimeToDateTime(PUnixMSTime(Element)^);
    betInt32:
      res.VInteger := PInteger(Element)^;
    betInt64:
      res.VInt64 := PInt64(Element)^;
  // betNull, betDeprecatedUndefined, betMinKey or betMaxKey has no data
  end;
  res.VType := BSON_ELEMENTTYPES[Kind];
  if res.VType = varUnknown then
  begin
    resBSON.VType := BsonVariantType.VarType;
    resBSON.VKind := Kind;
  end;
end;

function TBsonElement.ToInteger(const default: Int64): Int64;
begin
  case Kind of
    betBoolean:
      result := PByte(Element)^;
    betFloat:
      result := Trunc(unaligned(PDouble(Element)^));
    betInt32:
      result := PInteger(Element)^;
    betInt64:
      result := PInt64(Element)^;
  else
    result := default;
  end;
end;

function TBsonElement.ToRawUtf8: RawUtf8;

  procedure ComplexType;
  var
    V: variant;
    wasString: boolean;
  begin
    ToVariant(V);
    VariantToUtf8(V, result, wasString);
  end;

begin
  case Kind of // direct conversion of most simple types
    betFloat:
      DoubleToStr(unaligned(PDouble(Element)^), result);
    betString:
      FastSetString(result, Data.Text, Data.TextLen);
    betInt32:
      Int32ToUtf8(PInteger(Element)^, result);
    betInt64:
      Int64ToUtf8(PInt64(Element)^, result);
    betBoolean:
      result := BOOL_UTF8[PBoolean(Element)^];
    betDecimal128:
      PDecimal128(Element)^.ToText(result);
    betObjectID:
      PBsonObjectID(Element)^.ToText(result);
  else
    ComplexType;
  end;
end;

function TBsonElement.DocItemToVariant(const aName: RawUtf8; var aValue: variant;
  DocArrayConversion: TBsonDocArrayConversion): boolean;
var
  item: TBsonElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
  begin
    item.ToVariant(aValue, DocArrayConversion);
    result := true;
  end
  else
    result := false;
end;

function TBsonElement.DocItemToRawUtf8(const aName: RawUtf8): RawUtf8;
var
  item: TBsonElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
    result := item.ToRawUtf8
  else
    result := '';
end;

function TBsonElement.DocItemToInteger(
  const aName: RawUtf8; const default: Int64): Int64;
var
  item: TBsonElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
    result := item.ToInteger(default)
  else
    result := default;
end;

procedure TBsonElement.AddMongoJson(W: TJsonWriter; Mode: TMongoJsonMode);
label
  Bin, regex;
begin
  case integer(Kind) of
    ord(betFloat):
      W.AddDouble(unaligned(PDouble(Element)^));
    ord(betString),
    ord(betJS),
    ord(betDeprecatedSymbol):
      begin
        W.Add('"');
        W.AddJsonEscape(Data.Text, Data.TextLen);
        W.Add('"');
      end;
    ord(betDoc),
    ord(betArray):
      BsonListToJson(Data.DocList, Kind, W, Mode);
    ord(betObjectID):
      begin
        W.AddShort(BSON_JSON_OBJECTID[false, Mode]);
        W.AddBinToHex(Element, SizeOf(TBsonObjectID));
        W.AddShort(BSON_JSON_OBJECTID[true, Mode]);
      end;
    ord(betDeprecatedUndefined):
      W.AddShort(BSON_JSON_UNDEFINED[Mode = modMongoShell]);
    ord(betBinary):
      case Mode of
        modNoMongo:
          W.WrBase64(Data.Blob, Data.BlobLen, true);
        modMongoStrict:
          begin
            W.AddShort(BSON_JSON_BINARY[false, false]);
            W.WrBase64(Data.Blob, Data.BlobLen, false);
            W.AddShort(BSON_JSON_BINARY[false, true]);
            W.AddBinToHex(@Data.BlobSubType, 1);
            W.AddShorter('"}');
          end;
        modMongoShell:
          begin
            W.AddShort(BSON_JSON_BINARY[true, false]);
            W.AddBinToHex(@Data.BlobSubType, 1);
            W.AddShort(BSON_JSON_BINARY[true, true]);
            W.WrBase64(Data.Blob, Data.BlobLen, false);
            W.AddShorter('")');
          end;
      end;
    ord(betRegEx):
      case Mode of
        modNoMongo:
Bin:      W.WrBase64(Element, ElementBytes, true);
        modMongoStrict:
          goto regex;
        modMongoShell:
          if (PosChar(Data.RegEx, '/') = nil) and
             (PosChar(Data.RegExOptions, '/') = nil) then
          begin
            W.Add('/');
            W.AddNoJsonEscape(Data.RegEx, Data.RegExLen);
            W.Add('/');
            W.AddNoJsonEscape(Data.RegExOptions, Data.RegExOptionsLen);
          end
          else
          begin
regex:      W.AddShort(BSON_JSON_REGEX[0]);
            W.AddJsonEscape(Data.RegEx, Data.RegExLen);
            W.AddShort(BSON_JSON_REGEX[1]);
            W.AddJsonEscape(Data.RegExOptions, Data.RegExOptionsLen);
            W.AddShort(BSON_JSON_REGEX[2]);
          end;
      end;
    ord(betDeprecatedDbptr):
      goto Bin; // no specific JSON construct for this deprecated item
    ord(betJSScope):
      goto Bin; // no specific JSON construct for this item yet
    ord(betTimestamp):
      goto Bin; // internal content will always be written as raw binary
    ord(betBoolean):
      W.Add(PBoolean(Element)^);
    ord(betDateTime):
      begin
        W.AddShort(BSON_JSON_DATE[Mode, false]);
        W.AddUnixMSTime(Element, false);
        W.AddShort(BSON_JSON_DATE[Mode, true]);
      end;
    ord(betNull):
      W.AddNull;
    ord(betInt32):
      W.Add(PInteger(Element)^);
    ord(betInt64):
      W.Add(PInt64(Element)^);
    ord(betDecimal128):
      begin
        W.AddShort(BSON_JSON_DECIMAL[false, Mode]);
        PDecimal128(Element)^.AddText(W);
        W.AddShort(BSON_JSON_DECIMAL[true, Mode]);
      end;
    betMinKey:
      W.AddShort(BSON_JSON_MINKEY[Mode = modMongoShell]);
    betMaxKey:
      W.AddShort(BSON_JSON_MAXKEY[Mode = modMongoShell]);
  else
    raise EBsonException.CreateUtf8(
      'TBsonElement.AddMongoJson: unexpected type %', [integer(Kind)]);
  end;
end;

procedure TBsonElement.FromVariant(const aName: RawUtf8; const aValue: Variant;
  var aTemp: RawByteString);
const
  ELEMKIND: array[varEmpty..varWord64] of TBsonElementType = (
    betEOF, betNull,
    betInt32, betInt32, betFloat, betFloat, betFloat, betDateTime, betString,
    betEOF, betEOF, betBoolean, betEof, betEOF, betEOF, betEOF, betInt32,
    betInt32, betInt32, betInt64, betInt64, betInt64);
var
  v: PVarData;
  vbson: PBsonVariantData absolute v;
  vdoc: PDocVariantData absolute v;
  vt: cardinal;
label
  str, st2;
begin
  v := @aValue;
  while v.VType = varVariantByRef do
    v := v.VPointer;
  FillCharFast(self, SizeOf(self), 0);
  Name := pointer(aName);
  NameLen := length(aName);
  vt := v.VType;
  case vt of
    0..varDate,
    varBoolean..high(ELEMKIND):
      begin
        // simple types will be inlined in 64-bit InternalStorage
        Element := @Data.InternalStorage;
        Kind := ELEMKIND[vt];
        case Kind of
          betFloat:
            unaligned(PDouble(Element)^) := double(aValue);
          betDateTime:
            PUnixMSTime(Element)^ := DateTimeToUnixMSTime(v.VDate);
          betBoolean:
            if v.VBoolean then // normalize
              PBoolean(Element)^ := true
            else
              PBoolean(Element)^ := false;
          betInt32:
            if not VariantToInteger(aValue, PInteger(Element)^) then
              raise EBsonException.Create('TBsonElement.FromVariant(betInt32)');
          betInt64:
            if not VariantToInt64(aValue, PInt64(Element)^) then
              raise EBsonException.Create('TBsonElement.FromVariant(betInt64)');
        end;
        ElementBytes := BSON_ELEMENTSIZE[Kind];
      end;
    varString:
      if (v.VAny <> nil) and
         (PInteger(v.VAny)^ and $ffffff = JSON_SQLDATE_MAGIC_C) and
         Iso8601CheckAndDecode(PUtf8Char(v.VAny) + 3, Length(RawUtf8(v.VAny)) - 3,
           PDateTime(@Data.InternalStorage)^) then
      begin
        // recognized TJsonWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
        Element := @Data.InternalStorage;
        Kind := betDateTime;
        ElementBytes := BSON_ELEMENTSIZE[betDateTime];
      end
      else
      begin
        Kind := betString;
        Data.Text := v.VAny;
        Data.TextLen := Length(RawUtf8(v.VAny));
st2:    ElementBytes := Data.TextLen + 1;
        if v.VAny = nil then
          Data.InternalStorage := 1
        else
          Element := nil; // special case handled by TBsonWriter.BsonWrite()
      end;
  {$ifdef HASVARUSTRING}
    varUString:
      begin
        RawUnicodeToUtf8(v.VAny, length(UnicodeString(v.VAny)), RawUtf8(aTemp));
        goto str;
      end;
  {$endif HASVARUSTRING}
    varOleStr:
      begin
        RawUnicodeToUtf8(v.VAny, length(WideString(v.VAny)), RawUtf8(aTemp));
str:    Kind := betString;
        Data.Text := pointer(aTemp);
        Data.TextLen := Length(aTemp);
        goto st2;
      end;
  else
    if vt = cardinal(BsonVariantType.VarType) then // inlined FromBsonVariant()
    begin
      Kind := vbson.VKind;
      if Kind = betObjectID then
      begin
        ElementBytes := SizeOf(TBsonObjectID);
        Element := @vbson.VObjectID;
      end
      else
      begin
        FromBson(vbson.VBlob); // complex type stored as a RawByteString
        if ElementBytes < 0 then
          raise EBsonException.CreateUtf8('TBsonElement.FromVariant(bson,%)',
            [ToText(Kind)^]);
      end;
    end
    else if vt = cardinal(DocVariantVType) then
    begin
      if vdoc.IsObject then
        Kind := betDoc
      else if vdoc.IsArray then
        Kind := betArray
      else
        raise EBsonException.CreateUtf8('TBsonElement.FromVariant(doc,%)',
          [ToText(vdoc.Kind)^]);
      aTemp := Bson(vdoc^);
      FromBson(pointer(aTemp));
      if ElementBytes < 0 then
        raise EBsonException.CreateUtf8('TBsonElement.FromVariant(docbson,%)',
          [ToText(Kind)^]);
    end
    else
      raise EBsonException.CreateUtf8(
        'TBsonElement.FromVariant(VType=%)', [v.VType]);
  end;
end;

function TBsonElement.FromDocument(const doc: TBsonDocument): boolean;
var
  n: integer;
begin
  FillCharFast(self, SizeOf(self), 0);
  n := length(doc);
  if (n >= 4) and
     (PInteger(doc)^ = n) then
  begin
    Kind := betDoc;
    FromBson(pointer(doc));
    result := true;
  end
  else
    result := false;
end;

procedure TBsonElement.FromBson(bson: PByte);
begin
  // see http://bsonspec.org/#/specification
  Element := bson;
  case Kind of // handle variable-size storage
    betString,
    betJS,
    betDeprecatedSymbol:
      begin
        // "\x02" e_name string
        ElementBytes := PInteger(bson)^ + SizeOf(integer); // int32 (byte*) "\x00"
        Data.TextLen := PInteger(bson)^ - 1;
        inc(bson, SizeOf(integer));
        Data.Text := pointer(bson);
      end;
    betDoc,
    betArray:
      begin
        // "\x03" e_name document
        ElementBytes := PInteger(bson)^;
        inc(bson, SizeOf(integer)); // points to a "e_list #0"
        Data.DocList := bson;
      end;
    betBinary:
      begin
        // "\x05" e_name int32 subtype (byte*)
        ElementBytes := PInteger(bson)^ + (SizeOf(integer) + 1);
        Data.BlobLen := PInteger(bson)^;
        inc(bson, SizeOf(integer));
        Data.BlobSubType := TBsonElementBinaryType(bson^);
        inc(bson);
        Data.Blob := bson;
      end;
    betRegEx:
      begin
        // "\x0B" e_name cstring cstring
        Data.RegEx := Element;
        Data.RegExLen := StrLen(Data.RegEx);
        Data.RegExOptions := Data.RegEx + Data.RegExLen + 1;
        Data.RegExOptionsLen := StrLen(Data.RegExOptions);
        ElementBytes := Data.RegExLen + Data.RegExOptionsLen + 2;
      end;
    betJSScope:
      begin
        // "\x0F" e_name  int32 string document
        ElementBytes := PInteger(bson)^;
        inc(bson, SizeOf(integer));
        Data.JavaScriptLen := PInteger(bson)^ - 1;
        inc(bson, SizeOf(integer));
        Data.JavaScript := pointer(bson);
        inc(bson, Data.JavaScriptLen + 1);
        Data.ScopeDocument := bson;
      end;
  else
    if Kind > high(BSON_ELEMENTSIZE) then // e.g. betMinKey betMaxKey
      ElementBytes := 0
    else
      ElementBytes := BSON_ELEMENTSIZE[Kind]; // fixed size storage
  end;
end;

function TBsonElement.FromNext(var BSON: PByte): boolean;
var
  P: PUtf8Char;
begin
  if BSON = nil then
  begin
    result := false;
    exit;
  end;
  Kind := TBsonElementType(BSON^);
  case integer(Kind) of
    ord(betEOF):
      result := false;
    ord(betFloat)..ord(betDecimal128),
    betMinKey,
    betMaxKey:
      begin
        P := PUtf8Char(BSON) + 1;
        Name := P;
        NameLen := StrLen(P);
        BSON := pointer(P + NameLen + 1);
        FromBson(BSON);
        if ElementBytes < 0 then
          raise EBsonException.CreateUtf8(
            'TBsonElement.FromNext: unexpected size % for type %',
            [ElementBytes, ord(Kind)]);
        inc(BSON, ElementBytes);
        inc(Index);
        result := true;
      end;
  else
    raise EBsonException.CreateUtf8(
      'TBsonElement.FromNext: unexpected type %', [ord(Kind)]);
  end;
end;

function TBsonElement.FromSearch(BSON: PByte; const aName: RawUtf8): boolean;
begin
  result := true;
  while FromNext(BSON) do
    if IdemPropNameU(aName, Name, NameLen) then
      exit;
  result := false;
end;


{ TBsonIterator }

function TBsonIterator.Init(const doc: TBsonDocument;
  kind: TBsonElementType): boolean;
var
  n: integer;
begin
  FillCharFast(self, SizeOf(self), 0);
  n := length(doc);
  if (kind in [betDoc, betArray]) and
     (n >= 4) and
     (PInteger(doc)^ = n) then
  begin
    item.Kind := kind;
    item.FromBson(pointer(doc));
    fBson := item.Data.DocList;
    result := true;
  end
  else
    result := false;
end;

function TBsonIterator.Next: boolean;
begin
  result := item.FromNext(fBson);
end;


{ ************ TBsonWriter for BSON Encoding }

{ TBsonWriter }

procedure TBsonWriter.CancelAll;
begin
  inherited;
  fDocumentCount := 0;
end;

procedure TBsonWriter.WriteCollectionName(Flags: integer; const CollectionName: RawUtf8);
begin
  Write4(Flags);
  if CollectionName = '' then
    raise EBsonException.Create('Missing collection name');
  Write(pointer(CollectionName), length(CollectionName) + 1); // +1 for #0
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; elemtype: TBsonElementType);
begin
  if name = '' then
    Write2(ord(elemtype)) // write with trailing #0 for void name
  else
  begin
    Write1(ord(elemtype));
    {$ifdef HASINLINE}
    Write(pointer(name), length(name) + 1); // +1 for #0
    {$else}
    Write(pointer(name), PInteger(PtrInt(name) - SizeOf(integer))^ + 1); // +1 for #0
    {$endif HASINLINE}
  end;
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: integer);
begin
  BsonWrite(name, betInt32);
  Write4(value);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: Double);
begin
  BsonWrite(name, betFloat);
  Write8(@value);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: boolean);
begin
  BsonWrite(name, betBoolean);
  Write1(ord(value));
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: Int64);
begin
  if (value >= low(integer)) and
     (value <= high(integer)) then
  begin
    BsonWrite(name, betInt32);
    Write4(value);
  end
  else
  begin
    BsonWrite(name, betInt64);
    Write8(@value);
  end;
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: TBsonObjectID);
begin
  BsonWrite(name, betObjectID);
  Write(@value, SizeOf(value));
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: TDecimal128);
begin
  BsonWrite(name, betDecimal128);
  Write(@value, SizeOf(value));
end;

procedure TBsonWriter.BsonWriteRegEx(const name: RawUtf8;
  const RegEx, Options: RawByteString);
begin
  BsonWrite(name, betRegEx); // cstring cstring
  Write(pointer(RegEx), length(RegEx));
  Write1(0);
  Write(pointer(Options), length(Options));
  Write1(0);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: RawUtf8;
  isJavaScript: boolean);
const
  TYP: array[boolean] of TBsonElementType = (betString, betJS);
var
  L: integer;
begin
  BsonWrite(name, TYP[isJavaScript]);
  L := length(value) + 1; // +1 for ending #0
  Write4(L);
  if L = 1 then
    Write1(0)
  else
    Write(pointer(value), L);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; value: PUtf8Char);
var
  L: integer;
begin
  BsonWrite(name, betString);
  L := StrLen(value) + 1;
  Write4(L);
  if L = 1 then
    Write1(0)
  else
    Write(value, L);
end;

procedure TBsonWriter.BsonWriteString(const name: RawUtf8; value: PUtf8Char;
  valueLen: integer);
begin
  BsonWrite(name, betString);
  inc(valueLen);
  Write4(valueLen);
  if valueLen = 1 then
    Write1(0)
  else
    Write(value, valueLen);
end;

procedure TBsonWriter.BsonWriteDateTime(const name: RawUtf8; const value: TDateTime);
var
  ms: TUnixMSTime;
begin
  ms := DateTimeToUnixMSTime(value);
  BsonWrite(name, betDateTime);
  Write8(@ms);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; Data: pointer; DataLen: integer);
begin
  BsonWrite(name, betBinary);
  Write4(DataLen);
  Write1(ord(bbtGeneric));
  Write(Data, DataLen);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const elem: TBsonElement);
begin
  BsonWrite(name, elem.Kind);
  if (elem.Element = nil) and // handle special case of TBsonElement.FromVariant()
     (elem.Kind in [betString, betJS, betDeprecatedSymbol]) then
  begin
    Write4(elem.Data.TextLen + 1); // int32 (byte*) "\x00"
    Write(elem.Data.Text, elem.Data.TextLen + 1);
  end
  else
    Write(elem.Element, elem.ElementBytes);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const bson: TBsonVariantData);
begin
  case bson.VKind of
    betObjectID:
      BsonWrite(name, bson.VObjectID);
  else
    begin
      BsonWrite(name, bson.VKind);
      WriteBinary(RawByteString(bson.VBlob));
    end;
  end;
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const doc: TDocVariantData);
begin
  if doc.IsObject then
    BsonWrite(name, betDoc)
  else if doc.IsArray then
    BsonWrite(name, betArray)
  else
    raise EBsonException.Create('Undefined nested document');
  BsonWriteDoc(doc);
end;

procedure TBsonWriter.BsonWriteArray(const kind: TBsonElementType);
begin
  BsonWrite(UInt32ToUtf8(fDocumentArray), kind);
  inc(fDocumentArray);
  if kind in [betDoc, betArray] then
    BsonDocumentBegin;
end;

procedure TBsonWriter.BsonDocumentBegin;
begin
  if fDocumentStack >= Length(fDocumentStackOffset) then
    SetLength(fDocumentStackOffset, NextGrow(fDocumentStack));
  fDocumentStackOffset[fDocumentStack] := TotalWritten;
  inc(fDocumentStack);
  Write4(0);
end;

procedure TBsonWriter.BsonDocumentBegin(const name: RawUtf8; kind: TBsonElementType);
begin
  if not (kind in [betDoc, betArray]) then
    raise EBsonException.Create('BsonDocumentBegin(?)');
  BsonWrite(name, kind);
  BsonDocumentBegin;
end;

procedure TBsonWriter.BsonDocumentBeginInArray(const name: RawUtf8;
  kind: TBsonElementType);
begin
  if fDocumentArray > 0 then
    BsonDocumentEnd;
  BsonWriteArray(kind);
  BsonDocumentBegin(name);
end;

procedure TBsonWriter.BsonDocumentEnd(CloseNumber: integer; WriteEndingZero: boolean);
begin
  while CloseNumber > 0 do
  begin
    if (CloseNumber > 1) or
       WriteEndingZero then
      Write1(0);
    if fDocumentStack = 0 then
      raise EBsonException.CreateUtf8('Unexpected %.BsonDocumentEnd', [self]);
    dec(fDocumentStack);
    if fDocumentCount >= Length(fDocument) then
      SetLength(fDocument, NextGrow(fDocumentCount));
    with fDocument[fDocumentCount] do
    begin
      Offset := fDocumentStackOffset[fDocumentStack];
      Length := TotalWritten - Offset;
    end;
    inc(fDocumentCount);
    dec(CloseNumber);
  end;
end;

procedure TBsonWriter.BsonAdjustDocumentsSize(BSON: PByteArray);
var
  i: PtrInt;
begin
  for i := 0 to fDocumentCount - 1 do
    with fDocument[i] do
      PCardinal(@BSON[Offset])^ := Length;
end;

procedure TBsonWriter.ToBsonDocument(var result: TBsonDocument);
begin
  result := FlushTo;
  BsonAdjustDocumentsSize(pointer(result));
end;

procedure TBsonWriter.ToBsonVariant(var result: variant; Kind: TBsonElementType);
var
  doc: TBsonDocument;
begin
  ToBsonDocument(doc);
  BsonVariantType.FromBsonDocument(doc, result, Kind);
end;

procedure TBsonWriter.BsonWrite(const name: RawUtf8; const value: TVarRec);
var
  tmp: TTempUtf8;
begin
  case value.VType of
    vtBoolean:
      BsonWrite(name, value.VBoolean);
    vtInteger:
      BsonWrite(name, value.VInteger);
    vtCurrency:
      BsonWrite(name, value.VCurrency^);
    vtExtended:
      BsonWrite(name, value.VExtended^);
    vtVariant:
      BsonWriteVariant(name, value.VVariant^);
    {$ifdef FPC} vtQWord, {$endif}
    vtInt64:
      BsonWrite(name, value.VInt64^);
    vtString,
    vtAnsiString,
    {$ifdef HASVARUSTRING} vtUnicodeString, {$endif}
    vtPChar,
    vtChar,
    vtWideChar,
    vtWideString:
      begin
        VarRecToTempUtf8(value, tmp);
        BsonWriteString(name, tmp.Text, tmp.Len);
        if tmp.TempRawUtf8 <> nil then
          RawUtf8(tmp.TempRawUtf8) := '';
      end;
  else
    raise EBsonException.CreateUtf8(
      '%.BsonWrite(TVarRec.VType=%)', [self, value.VType]);
  end;
end;

procedure TBsonWriter.BsonWriteVariant(const name: RawUtf8; const value: variant);

  procedure WriteComplex;
  var
    temp: RawUtf8;
    json: PUtf8Char;
  begin
    with TVarData(value) do
      case VType of
      {$ifdef HASVARUSTRING}
        varUString:
          begin
            RawUnicodeToUtf8(VAny, length(UnicodeString(VAny)), temp);
            BsonWrite(name, temp);
          end;
      {$endif HASVARUSTRING}
        varOleStr:
          begin
            RawUnicodeToUtf8(VAny, length(WideString(VAny)), temp);
            BsonWrite(name, temp);
          end;
      else
        begin
          _VariantSaveJson(value, twJsonEscape, temp);
          json := pointer(temp);
          BsonWriteFromJson(name, json, nil);
          if json = nil then
            raise EBsonException.CreateUtf8(
              '%.BsonWriteVariant(VType=%)', [self, VType]);
        end;
      end;
  end;

var
  dt: TDateTime;
begin
  with TVarData(value) do
    case VType of
      varEmpty,
      varNull:
        BsonWrite(name, betNull);
      varSmallint:
        BsonWrite(name, VSmallInt);
      varShortInt:
        BsonWrite(name, VShortInt);
      varWord:
        BsonWrite(name, VWord);
      varLongWord:
        BsonWrite(name, VLongWord);
      varByte:
        BsonWrite(name, VByte);
      varBoolean:
        BsonWrite(name, VBoolean);
      varInteger:
        BsonWrite(name, VInteger);
      varWord64,
      varInt64:
        BsonWrite(name, VInt64);
      varSingle:
        BsonWrite(name, VSingle);
      varDouble:
        BsonWrite(name, VDouble);
      varDate:
        BsonWriteDateTime(name, VDate);
      varCurrency:
        BsonWrite(name, VCurrency);
      varString:
        if (VAny <> nil) and
           (PInteger(VAny)^ and $ffffff = JSON_SQLDATE_MAGIC_C) and
           Iso8601CheckAndDecode(PUtf8Char(VAny) + 3, Length(RawUtf8(VAny)) - 3, dt) then
          // recognized TJsonWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
          BsonWriteDateTime(name, dt)
        else
          BsonWrite(name, RawUtf8(VAny)); // expect UTF-8 content
    else
      if VType = varVariantByRef then
        BsonWriteVariant(name, PVariant(VPointer)^)
      else if VType = BsonVariantType.VarType then
        BsonWrite(name, TBsonVariantData(value))
      else if VType = DocVariantType.VarType then
        BsonWrite(name, TDocVariantData(value))
      else
        WriteComplex;
    end;
end;

procedure TBsonWriter.BsonWriteDoc(const doc: TDocVariantData);
var
  Name: RawUtf8;
  i: PtrInt;
begin
  BsonDocumentBegin;
  if doc.VarType > varNull then // null,empty will write {}
    if doc.VarType <> DocVariantType.VarType then
      raise EBsonException.CreateUtf8(
        '%.BsonWriteDoc(VType=%)', [self, doc.VarType])
    else
      for i := 0 to doc.Count - 1 do
      begin
        if doc.Names <> nil then
          Name := doc.Names[i]
        else
          UInt32ToUtf8(i, Name);
        BsonWriteVariant(Name, doc.Values[i]);
        if TotalWritten > BSON_MAXDOCUMENTSIZE then
          raise EBsonException.CreateUtf8('%.BsonWriteDoc(size=%>max %)',
            [self, TotalWritten, BSON_MAXDOCUMENTSIZE]);
      end;
  BsonDocumentEnd;
end;

procedure TBsonWriter.BsonWriteProjection(const FieldNamesCsv: RawUtf8);
var
  FieldNames: TRawUtf8DynArray;
  i: PtrInt;
begin
  CsvToRawUtf8DynArray(pointer(FieldNamesCsv), FieldNames);
  BsonDocumentBegin;
  for i := 0 to high(FieldNames) do
    BsonWrite(FieldNames[i], 1);
  BsonDocumentEnd;
end;

const
  QUERY_OPS: array[opNotEqualTo..opIn] of RawUtf8 = (
    '$ne',   // opNotEqualTo
    '$lt',   // opLessThan
    '$lte',  // opLessThanOrEqualTo
    '$gt',   // opGreaterThan
    '$gte',  // opGreaterThanOrEqualTo
    '$in');  // opIn

  INVERT_OPS: array[opEqualTo..opGreaterThanOrEqualTo] of TSelectStatementOperator = (
    opNotEqualTo,            // opEqualTo
    opEqualTo,               // opNotEqualTo
    opGreaterThanOrEqualTo,  // opLessThan
    opGreaterThan,           // opLessThanOrEqualTo
    opLessThanOrEqualTo,     // opGreaterThan
    opLessThan);             // opGreaterThanOrEqualTo

function TBsonWriter.BsonWriteQueryOperator(name: RawUtf8; inverted: boolean;
  op: TSelectStatementOperator; const Value: variant): boolean;
var
  wasString: boolean;
  like: RawUtf8;
  len: integer;
  doInvert: boolean;
begin
  result := false; // error on premature exit
  case op of
    // http://docs.mongodb.org/manual/faq/developers/#faq-developers-query-for-nulls
    // {$type:10} would return only existing fields, but our ODM do not insert
    // blobs by default -> do not use {$type:10} trick but plain {field:null}
    opIsNull:
      op := opEqualTo;     // here Value=null
    opIsNotNull:
      op := opNotEqualTo;  // here Value=null
  end;
  doInvert := false;
  if inverted then
    if op <= high(INVERT_OPS) then
      op := INVERT_OPS[op]
    else
    begin
      doInvert := true;
      BsonDocumentBegin(name);
      name := '$not';
    end;
  case op of
    opEqualTo:
      BsonWriteVariant(name, Value);
    opNotEqualTo .. opIn:
      begin
        BsonDocumentBegin(name);
        BsonWriteVariant(QUERY_OPS[op], Value);
        BsonDocumentEnd;
      end;
    opLike:
      begin
        VariantToUtf8(Value, like, wasString);
        len := length(like);
        if (len = 0) or
           not wasString then
          exit;
        if like[1] = '%' then
          if len = 1 then
            // LIKE '%' is invalid
            exit
          else if like[len] = '%' then
            if len = 2 then
              // LIKE '%%' is invalid
              exit
            else
              // LIKE '%a%' -> /a/
              TrimChars(like, 1, 1)
          else
            // LIKE '%a'  -> /a$/
            like := copy(like, 2, len - 1) + '$'
        else
        if like[len] = '%' then
          // LIKE 'a%'  -> /^a/
          like := '^' + copy(like, 1, len - 1)
        else
          // LIKE 'a'   -> /^a$/
          like := '^' + like + '$';
        BsonWriteRegEx(name, like, 'i'); // /like/i for case-insensitivity
      end;
    opContains:
      begin
        // http://docs.mongodb.org/manual/reference/operator/query/in
        BsonDocumentBegin(name);
        if _Safe(Value)^.IsArray then
          BsonWriteVariant(QUERY_OPS[opIn], Value)
        else
        begin
          BsonWrite(QUERY_OPS[opIn], betArray);
          BsonWriteArray([Value]);
        end;
        BsonDocumentEnd;
      end;
  else
    exit; // unhandled operator
  end;
  if doInvert then
    BsonDocumentEnd;
  result := true;
end;

procedure TBsonWriter.BsonWriteObject(const NameValuePairs: array of const);
var
  Name: RawUtf8;
  i: PtrInt;
begin
  BsonDocumentBegin;
  for i := 0 to (length(NameValuePairs) shr 1) - 1 do
  begin
    VarRecToUtf8(NameValuePairs[i * 2], Name);
    BsonWrite(Name, NameValuePairs[i * 2 + 1]);
  end;
  BsonDocumentEnd;
end;

procedure TBsonWriter.BsonWriteArray(const Items: array of const);
var
  i: PtrInt;
begin
  BsonDocumentBegin;
  for i := 0 to high(Items) do
    BsonWrite(UInt32ToUtf8(i), Items[i]);
  BsonDocumentEnd;
end;

procedure TBsonWriter.BsonWriteArrayOfInteger(const Integers: array of integer);
var
  i: PtrInt;
begin
  BsonDocumentBegin;
  for i := 0 to high(Integers) do
    BsonWrite(UInt32ToUtf8(i), Integers[i]);
  BsonDocumentEnd;
end;

procedure TBsonWriter.BsonWriteArrayOfInt64(const Integers: array of Int64);
var
  i: PtrInt;
begin
  BsonDocumentBegin;
  for i := 0 to high(Integers) do
    BsonWrite(UInt32ToUtf8(i), Integers[i]);
  BsonDocumentEnd;
end;

procedure TBsonWriter.BsonWriteFromJson(const name: RawUtf8; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; DoNotTryExtendedMongoSyntax: boolean);
var
  tmp: variant;
  blob: RawByteString;
  VDouble: double;
  ValueDateTime: TDateTime absolute VDouble;
  Kind: TBsonElementType;
  info: TGetJsonField;
begin
  if Json^ in [#1..' '] then
    repeat
      inc(Json)
    until not (Json^ in [#1..' ']);
  if not DoNotTryExtendedMongoSyntax and
     BsonVariantType.TryJsonToVariant(Json, tmp, EndOfObject) then
    // was betDateTime, betObjectID or betRegEx, from strict or extended Json
    BsonWriteVariant(name, tmp)
  else
    // try from simple types
    case Json^ of
      #0:
        begin
          Json := nil;
          exit;
        end;
      '[':
        begin
          // nested array
          BsonWrite(name, betArray);
          Json := BsonWriteDocFromJson(Json, EndOfObject, Kind,
            DoNotTryExtendedMongoSyntax);
        end;
      '{':
        begin
          // nested document
          BsonWrite(name, betDoc);
          Json := BsonWriteDocFromJson(Json, EndOfObject, Kind,
            DoNotTryExtendedMongoSyntax);
        end;
    else
      begin
        // simple types
        info.Json := Json;
        info.GetJsonField;
        Json := info.Json;
        if Json = nil then
          Json := @NULCHAR;
        if EndOfObject <> nil then
          EndOfObject^ := info.EndOfObject;
        if (info.Value = nil) or
           not info.WasString then
          if GetVariantFromNotStringJson(info.Value, TVarData(tmp), true) then
          begin
            BsonWriteVariant(name, tmp); // null,boolean,Int64,double
            exit;
          end;
        // found no simple value -> check text value
        if Base64MagicCheckAndDecode(info.Value, info.ValueLen, blob) then
          // recognized '\uFFF0base64encodedbinary' pattern
          BsonWrite(name, pointer(blob), length(blob))
        else if Iso8601CheckAndDecode(info.Value, info.ValueLen, ValueDateTime) or
                ((PInteger(info.Value)^ and $ffffff = JSON_SQLDATE_MAGIC_C) and
                 Iso8601CheckAndDecode(info.Value + 3, info.ValueLen - 3, ValueDateTime)) then
          // recognized TJsonWriter.AddDateTime() pattern
          BsonWriteDateTime(name, ValueDateTime)
        else
          // append the in-place escaped Json text
          BsonWriteString(name, info.Value, info.ValueLen);
      end;
    end;
  if TotalWritten > BSON_MAXDOCUMENTSIZE then
    raise EBsonException.CreateUtf8('Unexpected %.BsonWriteDoc(size=% > max=%)',
      [self, TotalWritten, BSON_MAXDOCUMENTSIZE]);
end;

function TBsonWriter.BsonWriteDocFromJson(Json: PUtf8Char; aEndOfObject: PUtf8Char;
  out Kind: TBsonElementType; DoNotTryExtendedMongoSyntax: boolean): PUtf8Char;
var
  ndx: cardinal;
  EndOfObject: AnsiChar;
  Name: RawUtf8;
begin
  result := nil; // parsing error
  if Json = nil then
    exit;
  if Json^ in [#1..' '] then
    repeat
      inc(Json)
    until not (Json^ in [#1..' ']);
  case Json^ of
    '[':
      begin
        Kind := betArray;
        BsonDocumentBegin;
        repeat
          inc(Json)
        until not (Json^ in [#1..' ']);
        ndx := 0;
        if Json^ = ']' then
          inc(Json)
        else
          repeat
            UInt32ToUtf8(ndx, Name);
            BsonWriteFromJson(Name, Json, @EndOfObject, DoNotTryExtendedMongoSyntax);
            if Json = nil then
              exit; // invalid content
            inc(ndx);
          until EndOfObject = ']';
      end;
    '{':
      begin
        Kind := betDoc;
        BsonDocumentBegin;
        repeat
          inc(Json)
        until not (Json^ in [#1..' ']);
        if Json^ = '}' then
          inc(Json)
        else
          repeat
            // see http://docs.mongodb.org/manual/reference/mongodb-extended-Json
            Name := GetJsonPropName(Json); // BSON/Json accepts "" as key name
            BsonWriteFromJson(Name, Json, @EndOfObject, DoNotTryExtendedMongoSyntax);
            if (Json = nil) or
               (EndOfObject = #0) then
              exit; // invalid content
          until EndOfObject = '}';
      end;
    'n',
    'N':
      if IdemPChar(Json + 1, 'ULL') then
      begin
        // append null as {}
        Kind := betDoc;
        BsonDocumentBegin;
        inc(Json, 4);
      end
      else
        exit;
  else
    exit;
  end;
  BsonDocumentEnd;
  if Json^ in [#1..' '] then
    repeat
      inc(Json)
    until not (Json^ in [#1..' ']);
  if aEndOfObject <> nil then
    aEndOfObject^ := Json^;
  if Json^ <> #0 then
    repeat
      inc(Json)
    until not (Json^ in [#1..' ']);
  result := Json; // indicates successfully parsed
end;


{ ************ High-Level BSON/JSON Function Helpers }

function BsonParseLength(var BSON: PByte; ExpectedBSONLen: integer): integer;
begin
  if (BSON = nil) or
     ((ExpectedBSONLen <> 0) and
      (PInteger(BSON)^ <> ExpectedBSONLen)) then
    raise EBsonException.Create('Incorrect supplied BSON document content');
  result := PInteger(BSON)^;
  inc(PInteger(BSON));
end;

function BsonParseNextElement(var BSON: PByte; var name: RawUtf8;
  var element: variant; DocArrayConversion: TBsonDocArrayConversion): boolean;
var
  item: TBsonElement;
begin
  result := item.FromNext(BSON);
  if result then
  begin
    FastSetString(name, item.Name, item.NameLen);
    item.ToVariant(element, DocArrayConversion);
  end;
end;

function BsonPerIndexElement(BSON: PByte; index: integer;
  var item: TBsonElement): boolean;
begin
  result := true;
  if (index >= 0) and
     (BSON <> nil) and
     (BsonParseLength(BSON) <> 0) then
    while item.FromNext(BSON) do
      if index = 0 then
        exit
      else
        dec(index);
  result := false;
end;

procedure BsonToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer;
  Option: TBsonDocArrayConversion);
begin
  if Option = asBsonVariant then
    raise EBsonException.Create('BsonToDoc(option=asBsonVariant) is not allowed');
  VarClear(Result);
  BsonParseLength(BSON, ExpectedBSONLen);
  BsonItemsToDocVariant(betDoc, BSON, TDocVariantData(Result), Option);
end;

function BsonDocumentToDoc(const BSON: TBsonDocument;
  Option: TBsonDocArrayConversion): variant;
begin
  BsonToDoc(pointer(BSON), result, length(BSON));
end;

procedure BsonListToJson(BsonList: PByte; Kind: TBsonElementType; W: TJsonWriter;
  Mode: TMongoJsonMode);
var
  item: TBsonElement;
begin
  case Kind of
    betDoc:
      if BsonList^ = byte(betEOF) then
        W.Add('{', '}')
      else
      begin
        W.Add('{');
        while item.FromNext(BsonList) do
        begin
          if Mode = modMongoShell then
          begin
            W.AddNoJsonEscape(item.Name, item.NameLen);
            W.Add(':');
          end
          else
            W.AddProp(item.Name, item.NameLen);
          item.AddMongoJson(W, Mode);
          W.AddComma;
        end;
        W.CancelLastComma;
        W.Add('}');
      end;
    betArray:
      begin
        W.Add('[');
        while item.FromNext(BsonList) do
        begin
          item.AddMongoJson(W, Mode);
          W.AddComma;
        end;
        W.CancelLastComma;
        W.Add(']');
      end;
  else
    raise EBsonException.CreateUtf8('BsonListToJson(Kind=%)', [ord(Kind)]);
  end;
end;

function BsonDocumentToJson(const BSON: TBsonDocument; Mode: TMongoJsonMode): RawUtf8;
begin
  result := BsonToJson(pointer(BSON), betDoc, length(BSON), Mode);
end;

function BsonToJson(BSON: PByte; Kind: TBsonElementType;
  ExpectedBSONLen: integer; Mode: TMongoJsonMode): RawUtf8;
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  BsonParseLength(BSON, ExpectedBSONLen);
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    BsonListToJson(BSON, Kind, W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddMongoJson(const Value: variant; W: TJsonWriter; Mode: TMongoJsonMode);

  procedure AddCustom;
  var
    item: TBsonElement;
    temp: RawByteString;
  begin
    item.FromVariant('', Value, temp);
    item.AddMongoJson(W, Mode);
  end;

begin
  if TVarData(Value).VType < $10F then
    W.AddVariant(Value, twJsonEscape)
  else
    AddCustom; // sub-procedure to avoid implicit try..finally
end;

function VariantSaveMongoJson(const Value: variant; Mode: TMongoJsonMode): RawUtf8;
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    AddMongoJson(Value, W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ main BSON* functions }

function ToText(kind: TBsonElementType): PShortString;
begin
  result := GetEnumName(TypeInfo(TBsonElementType), ord(kind));
end;

function ToText(spec: TDecimal128SpecialValue): PShortString;
begin
  result := GetEnumName(TypeInfo(TDecimal128SpecialValue), ord(spec));
end;

function ObjectID: variant;
var
  ID: TBsonObjectID;
begin
  ID.ComputeNew;
  ID.ToVariant(result);
end;

function ObjectID(const Hexa: RawUtf8): variant;
var
  ID: TBsonObjectID;
begin
  if ID.FromText(Hexa) then
    ID.ToVariant(result)
  else
    raise EBsonException.CreateUtf8('Invalid ObjectID("%")', [Hexa]);
end;

function BsonObjectID(const aObjectID: variant): TBsonObjectID;
begin
  if not result.FromVariant(aObjectID) then
    raise EBsonException.Create('BsonObjectID() over not ObjectID variant');
end;

function JavaScript(const JS: RawUtf8): variant;
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    VType := BsonVariantType.VarType;
    VKind := betJS;
    VText := nil; // avoid GPF
    RawUtf8(VText) := JS;
  end;
end;

function JavaScript(const JS: RawUtf8; const Scope: TBsonDocument): variant;
var
  Len, JSLen: integer;
begin
  VarClear(result);
  with TBsonVariantData(result) do
  begin
    VType := BsonVariantType.VarType;
    VKind := betJSScope;
    JSLen := Length(JS) + 1;                            // string = int32 text#0
    Len := SizeOf(integer) * 2 + JSLen + length(Scope); // int32 string document
    VBlob := nil; // avoid GPF
    SetLength(RawByteString(VBlob), Len);
    PIntegerArray(VBlob)^[0] := Len;                    // length:int32
    PIntegerArray(VBlob)^[1] := JSLen;                  // string:int32
    MoveFast(pointer(JS)^, PAnsiChar(VBlob)[8], JSLen); // string:text#0
    MoveFast(pointer(Scope)^, PAnsiChar(VBlob)[8 + JSLen], Length(Scope)); // document
  end;
end;

function NumberDecimal(const Value: RawUtf8): variant;
var
  dec: TDecimal128;
begin
  if dec.FromText(Value) = dsvError then
    raise EBsonException.CreateUtf8('Invalid NumberDecimal("%")', [Value]);
  dec.ToVariant(result);
end;

function NumberDecimal(const Value: currency): variant;
var
  dec: TDecimal128;
begin
  dec.FromCurr(Value);
  dec.ToVariant(result);
end;

function Bson(const doc: TDocVariantData): TBsonDocument;
var
  tmp: TTextWriterStackBuffer;
begin
  if doc.VarType = varVariantByRef then
  begin
    result := Bson(PDocVariantData(TVarData(doc).VPointer)^);
    exit;
  end;
  if doc.VarType <> DocVariantType.VarType then
    raise EBsonException.CreateUtf8(
      'Bson(doc) is % not a TDocVariant', [doc.VarType]);
  with TBsonWriter.Create(tmp) do
  try
    BsonWriteDoc(doc);
    ToBsonDocument(result);
  finally
    Free;
  end;
end;

function BsonFromIntegers(const Integers: array of integer): TBsonDocument;
var
  tmp: TTextWriterStackBuffer;
begin
  with TBsonWriter.Create(tmp) do
  try
    BsonWriteArrayOfInteger(Integers);
    ToBsonDocument(result);
  finally
    Free;
  end;
end;

function BsonFromInt64s(const Integers: array of Int64): TBsonDocument;
var
  tmp: TTextWriterStackBuffer;
begin
  with TBsonWriter.Create(tmp) do
  try
    BsonWriteArrayOfInt64(Integers);
    ToBsonDocument(result);
  finally
    Free;
  end;
end;

function Bson(const NameValuePairs: array of const): TBsonDocument;
var
  W: TBsonWriter;
  name: RawUtf8;
  a: PtrInt;
  tmp: TTextWriterStackBuffer;

  procedure WriteValue;
  var
    ndx: cardinal;
  begin
    case VarRecAsChar(NameValuePairs[a]) of
      ord('['):
        begin
          W.BsonDocumentBegin(name, betArray);
          ndx := 0;
          repeat
            inc(a);
            if VarRecAsChar(NameValuePairs[a]) = ord(']') then
              break;
            UInt32ToUtf8(ndx, name);
            WriteValue;
            inc(ndx);
          until a = high(NameValuePairs);
          W.BsonDocumentEnd;
        end;
      ord('{'):
        begin
          W.BsonDocumentBegin(name, betDoc);
          repeat
            inc(a);
            VarRecToUtf8(NameValuePairs[a], name);
            if (a = high(NameValuePairs)) or
               (name = '}') then
              break;
            inc(a);
            WriteValue;
          until a = high(NameValuePairs);
          W.BsonDocumentEnd;
        end
    else
      W.BsonWrite(name, NameValuePairs[a]);
    end;
  end;

begin
  W := TBsonWriter.Create(tmp);
  try
    W.BsonDocumentBegin;
    a := 0;
    while a < high(NameValuePairs) do
    begin
      VarRecToUtf8(NameValuePairs[a], name);
      inc(a);
      WriteValue;
      inc(a);
    end;
    W.BsonDocumentEnd;
    W.ToBsonDocument(result);
  finally
    W.Free;
  end;
end;

function BsonFieldSelector(const FieldNames: array of RawUtf8): TBsonDocument;
var
  i: PtrInt;
  W: TBsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBsonWriter.Create(tmp);
  try
    W.BsonDocumentBegin;
    for i := 0 to high(FieldNames) do
      W.BsonWrite(FieldNames[i], 1);
    W.BsonDocumentEnd;
    W.ToBsonDocument(result);
  finally
    W.Free;
  end;
end;

function BsonFieldSelector(const FieldNamesCsv: RawUtf8): TBsonDocument;
var
  FieldNames: TRawUtf8DynArray;
begin
  CsvToRawUtf8DynArray(pointer(FieldNamesCsv), FieldNames);
  result := BsonFieldSelector(FieldNames);
end;

function JsonBufferToBsonDocument(Json: PUtf8Char; var doc: TBsonDocument;
  DoNotTryExtendedMongoSyntax: boolean): TBsonElementType;
var
  W: TBsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBsonWriter.Create(tmp);
  try
    W.BsonWriteDocFromJson(Json, nil, result, DoNotTryExtendedMongoSyntax);
    W.ToBsonDocument(doc);
  finally
    W.Free;
  end;
end;

function JsonBufferToBsonArray(Json: PUtf8Char; out docs: TBsonDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean): boolean;
var
  W: TBsonWriter;
  doc: TBsonDocument;
  EndOfObject: AnsiChar;
  Kind: TBsonElementType;
  n: integer;
  tmp: TTextWriterStackBuffer;
begin
  result := false;
  if Json = nil then
    exit;
  Json := GotoNextNotSpace(Json);
  if Json^ <> '[' then
    exit;
  Json := GotoNextNotSpace(Json + 1);
  n := 0;
  W := TBsonWriter.Create(tmp);
  try
    repeat
      Json := W.BsonWriteDocFromJson(Json, @EndOfObject, Kind,
        DoNotTryExtendedMongoSyntax);
      if Json = nil then
        exit;
      W.ToBsonDocument(doc);
      if n >= length({%H-}docs) then
        SetLength(docs, NextGrow(n));
      docs[n] := doc;
      inc(n);
      W.CancelAll;
    until EndOfObject = ']';
  finally
    W.Free;
  end;
  SetLength(docs, n);
  result := true;
end;

function Bson(const Format: RawUtf8; const Args, Params: array of const;
  kind: PBsonElementType): TBsonDocument;
var
  json: RawUtf8; // since we use FormatUtf8(), TSynTempBuffer is useless here
  v: variant;
  k: TBsonElementType;
begin
  if (Format = '?') and
     (high(Params) >= 0) then
  begin
    VarRecToVariant(Params[0], v);
    if DocVariantType.IsOfType(v) then
    begin
      result := Bson(TDocVariantData(v));
      if kind <> nil then
        if TDocVariantData(v).IsArray then
          kind^ := betArray
        else
          kind^ := betDoc;
      exit;
    end;
  end;
  json := FormatUtf8(Format, Args, Params, {json=}true);
  UniqueRawUtf8(json); // ensure Format is untouched if Args=[]
  k := JsonBufferToBsonDocument(pointer(json), result);
  if kind <> nil then
    kind^ := k;
end;

function Bson(const Json: RawUtf8; kind: PBsonElementType): TBsonDocument;
var
  tmp: TSynTempBuffer;
  k: TBsonElementType;
begin
  tmp.Init(Json);
  try
    k := JsonBufferToBsonDocument(tmp.buf, result);
    if kind <> nil then
      kind^ := k;
  finally
    tmp.Done;
  end;
end;

function BsonVariant(const NameValuePairs: array of const): variant;
begin
  BsonVariantType.FromBsonDocument(Bson(NameValuePairs), result, betDoc);
end;

function BsonVariant(const Json: RawUtf8): variant;
var
  k: TBsonElementType;
  b: RawByteString;
begin
  b := Bson(Json, @k);
  BsonVariantType.FromBsonDocument(b, result, k);
end;

procedure BsonVariant(Json: PUtf8Char; var result: variant);
var
  tmp: TBsonDocument;
begin
  JsonBufferToBsonDocument(Json, tmp);
  BsonVariantType.FromBsonDocument(tmp, result);
end;

function BsonVariant(const Format: RawUtf8; const Args, Params: array of const):
  variant; overload;
var
  k: TBsonElementType;
  b: RawByteString;
begin
  b := Bson(Format, Args, Params, @k);
  BsonVariantType.FromBsonDocument(b, result, k);
end;

function BsonVariant(const doc: TDocVariantData): variant;
var
  k: TBsonElementType;
begin
  if doc.IsArray then
    k := betArray
  else
    k := betDoc;
  BsonVariantType.FromBsonDocument(Bson(doc), result, k);
end;

function BsonVariantFieldSelector(const FieldNames: array of RawUtf8): variant;
begin
  BsonVariantType.FromBsonDocument(BsonFieldSelector(FieldNames), result);
end;

function BsonVariantFieldSelector(const FieldNamesCsv: RawUtf8): variant;
begin
  BsonVariantType.FromBsonDocument(BsonFieldSelector(FieldNamesCsv), result);
end;

function BsonVariantFromIntegers(const Integers: array of integer): variant;
begin
  BsonVariantType.FromBsonDocument(BsonFromIntegers(Integers), result, betArray);
end;

function BsonVariantFromInt64s(const Integers: array of Int64): variant;
begin
  BsonVariantType.FromBsonDocument(BsonFromInt64s(Integers), result, betArray);
end;



initialization
  Assert(SizeOf(TDecimal128) = 16);
  Assert(ord(betEof) = $00);
  Assert(ord(betInt64) = $12);
  Assert(ord(betDecimal128) = $13);
  Assert(ord(bbtGeneric) = $00);
  Assert(ord(bbtMD5) = $05);
  Assert(ord(bbtUser) = $80);
  Assert(SizeOf(TBsonObjectID) = 12);
  Assert(SizeOf(TBsonVariantData) = SizeOf(variant));
  BsonVariantType := SynRegisterCustomVariantType(TBsonVariant) as TBsonVariant;
  InitBsonObjectIDComputeNew;


end.

