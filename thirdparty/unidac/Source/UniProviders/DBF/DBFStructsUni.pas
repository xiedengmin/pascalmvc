
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFStructsUni;

interface

{$IFDEF NEXTGEN}
uses
  CRTypes;
{$ENDIF}

const
  DBaseFieldSize = $20;
  DBase7FieldSize = $30;

type
  TVFPTimeStamp = packed record
    Date: Integer; // the day under the Julian Day System (JDS)
    Time: Integer; // the number of milliseconds since midnight
  end;

  { Numeric }

  PDBaseNumeric = ^TDBaseNumeric;
  TDBaseNumeric = packed record
    IntLen: Byte; // +$34
    SignScaleUnk: Byte; // bit 7 = sign ($80), bits 6-2 = count of tetrades used,	bits 1-0 = always 01 ?
    Fraction: array[0 .. 9] of Byte;
  end;

  { DBF }

  PDBFHeader = ^TDBFHeader;
  TDBFHeader = packed record
    // bits 0-2 indicate version number: 3 for dBASE Level 5, 4 for dBASE Level 7
    // Bit 3 and bit 7 indicate presence of a dBASE IV or dBASE for Windows memo file;
    // bits 4-6 indicate the presence of a dBASE IV SQL table; bit 7 indicates the presence of any .DBT memo file
    // (either a dBASE III PLUS type or a dBASE IV or dBASE for Windows memo file)
    version: Byte;
    // Date of last update; in YYMMDD format.  Each byte contains the number as a binary.
    // YY is added to a base of 1900 decimal to determine the actual year.
    // Therefore, YY has possible values from 0x00-0xFF, which allows for a range from 1900-2155
    year, month, day: Byte;

    numRecs: Integer;  // Number of records in the table
    dataOffset: Word;   // Position of first data record
    recLen: Word;       // Number of bytes in the record including mark byte
    reserved: Word;     // filled with zeros
    transaction: Byte;  // Flag indicating incomplete dBASE IV transaction, 01 - transaction protected
    encrypted: Byte;    // dBASE IV encryption flag, 01 - encrypted
    reservedMU: array[0 .. 11] of Byte; // Reserved for multi-user processing
    // Production MDX flag; 0x01 if a production .MDX file exists for this table; 0x00 if no .MDX file exists
    // 0x01   file has a structural mdx/cdx
    // 0x02   file has a Memo field
    // 0x04   file is a database (.dbc)
    flags: Byte;
    // Language driver ID
    langDrv: Byte;
    reserved2: Word;
  end;

  PDBFDBase7Header = ^TDBFDBase7Header;
  TDBFDBase7Header = packed record
    DB3: TDBFHeader;

    // from here DBase 7 specific
    langDrvName: array[0 .. 31] of AnsiChar; // Language driver name
    reserved3: integer;
    // Field Descriptor Array, 48 bytes each
    // n+1 	1 byte 	0x0D stored as the Field Descriptor terminator
    // n+2  Field Properties Structure
  end;

  PDBFField = ^TDBFField;
  TDBFField = packed record
    case Byte of
      0:( // dBase3P
        DB3Name: array[0 .. 10] of AnsiChar;               // 0 - 10
        DB3FType: AnsiChar;                                // 11
        DB3Offset: integer;                                // 12 - 15 FoxPro, else 0
        DB3Len: Byte;                                      // 16
        DB3NumDecimal: Byte;                               // 17
        case Byte of
          0:( // dBase
            DB3eserved: array[0 .. 12] of Byte;
            DB3FlagMDX: Byte; // dBASE IV only
          );
          1:( // VisualFoxPro
            FPFlags: Byte;                                // 18
              //0x01   System Column (not visible to user)
              //0x02   Column can store null values
              //0x04   Binary column (CHAR, MEMO, Integer, Currency ...)
              //0x06   (0x02+0x04) When a field nullable and binary (Integer, Currency, and Character/Memo fields)
              //0x0C   Column is autoincrementing
            FPAutoincNextValue: integer;                  // 19 - 22
            FPAutoincStepValue: Byte;                     // 23
            FPReserved: array[0 .. 7] of Byte;            // 24 – 31
          );
      );
      1:( // dBase7
        DB7Name: array[0 .. 31] of AnsiChar;
        DB7FType: AnsiChar;
        DB7Len: Byte;
        DB7NumDecimal: Byte;
        DB7Reserved: Word;
        DB7FlagMDX: Byte; // Production .MDX field flag; 0x01 if field has an index tag in the production .MDX file; 0x00 if the field is not indexed.
        DB7Reserved2: integer; // official structure layout is wrong (dBase 10 program uses another)
        DB7AutoincNextValue: integer;
        DB7Reserved3: Word;
      );
  end;

  // This contains a header describing the Field Properties array, followed by the actual array, followed by property data. It is contained in the .DBF header and comes immediately after the Field Descriptor terminator
  TDBFDBase7FieldProperties = packed record
    NumOfProps: Word;           // 0-1 	16-bit number 	Number of Standard Properties.
    StartOfStandardDescr: Word; // 2-3 	16-bit number 	Start of Standard Property Descriptor Array. (see 1.3.1 Standard Property and Constraint Descriptor Array)
    NumOfCustomProps: Word;     // 4-5 	16-bit number 	Number of Custom Properties.
    StartOfCustomDescr: Word;   // 6-7 	16-bit number 	Start of Custom Property Descriptor Array. (see 1.3.2 Custom Property Descriptor Array)
    NumOfRI: Word;              // 8-9 	16-bit number 	Number of Referential Integrity (RI) properties.
    StartOfRIDescr: Word;       // 10-11 	16-bit number 	Start of RI Property Descriptor Array. (see 1.3.3 Referential Integrity Property Descriptor Array)
    StartOfData: Word;          // 12-13 	16-bit number 	Start of data - this points past the Descriptor arrays to data used by the arrays - for example Custom property names are stored here.
    SizeOfStruct: Word;         // 14-15 	16-bit number 	Actual size of structure, including data (Note: in the .DBF this will be padded with zeroes to the nearest 0x200, and may have 0x1A at the end). If the structure contains RI data, it will not be padded.
    // 16-n 	15 bytes each 	Standard Property Descriptor Array (n = (15*number of standard properties) + 16). (see 1.3.1 Standard Property and Constraint Descriptor Array)
    // (n+1)-m 	14 bytes each 	Custom Property Descriptor Array (m = n+ 14*number of custom properties). (see 1.3.2 Custom Property Descriptor Array)
    // (m+1)-o 	22 bytes each 	RI Property Descriptor Array (o = m+ 22*number of RI properties). (see 1.3.3 Referential Integrity Property Descriptor Array)
  end;

  TDBFDBase7StandardDescriptor = packed record
    GenNum: Word;     // 0-1 	16-bit number 	Generational number. More than one value may exist for a property. The current value is the value with the highest generational number.
    Offset: Word;     // 2-3 	16-bit number 	Table field offset - base one. 01 for the first field in the table, 02 for the second field, etc. Note: this will be 0 in the case of a constraint.
    Prop: Byte;       // 4 	8-bit number 	Which property is described in this record:
      // 01 Required
      // 02 Min
      // 03 Max
      // 04 Default
      // 06 Database constraint
    FieldType: Byte;  // 5 	1 byte 	Field Type:
      // 00 No type - constraint
      // 01 Char
      // 02 Numeric
      // 03 Memo
      // 04 Logical
      // 05 Date
      // 06 Float
      // 08 OLE
      // 09 Binary
      // 11 Long
      // 12 Timestamp
      // 13 Double
      // 14 AutoIncrement (not settable from the Inspector)
    IsConstraint: Byte; // 6 	1 byte 	0x00 if the array element is a constraint, 0x02 otherwise.
    Reserved: integer; // 7-10 	4 bytes 	Reserved
    DataOffset: Word;   // 11-12 	16-bit number 	Offset from the start of this structure to the data for the property. The Required property has no data associated with it, so it is always 0.
    DataLength: Word;   // 13-14 	16-bit number 	Width of database field associated with the property, and hence size of the data (includes 0 terminator in the case of a constraint).
  end;

  TDBFDBase7CustomDescriptor = packed record
    GenNum: Word;     // 0-1 	16-bit number 	Generational number. More than one value may exist for a property. The current value is the value with the highest generational number.
    Offset: Word;     // 2-3 	16-bit number 	Table field offset - base one.  01 for the first field in the table, 02 for the second field, etc.
    FieldType: Byte;  // 4 	1 byte 	Field Type
      // 01 Char
      // 02 Numeric
      // 03 Memo
      // 04 Logical
      // 05 Date
      // 06 Float
      // 08 OLE
      // 09 Binary
      // 11 Long
      // 12 Timestamp
      // 13 Double
      // 14 AutoIncrement (not settable from the Inspector)
    Reserved: Byte;       // 5 	1 byte 	Reserved
    PropNameOffset: Word; // 6-7 	16-bit number 	Offset from the start of this structure to the Custom property name.
    PropNameLength: Word; // 8-9 	16-bit number 	Length of the Custom property name.
    DataOffset: Word;     // 10-11 	16-bit number 	Offset from the start of this structure to the Custom property data.
    DataLength: Word;     // 12-13 	16-bit number 	Length of the Custom property data (does not include null terminator).
  end;

  TDBFDBase7RIDescriptor = packed record
    IsMaster: Byte;   // 0 	8-bit number 	0x07 if Master (parent), 0x08 if Dependent (child).
    SeqNum: Word;     // 1-2 	16-bit number 	Sequential number, 1 based counting. If this number is 0, this RI rule has been dropped.
    NameOffset: Word; // 3-4 	16-bit number 	Offset of the RI rule name - 0 terminated.
    NameSize: Word;   // 5-6 	16-bit number 	Size of previous value.
    ForeignTableNameOffset: Word; // 7-8 	16-bit number 	Offset of the name of the Foreign Table - 0 terminated.
    ForeignTableNameSize: Word;   // 9-10 	16-bit number 	Size of previous value.
    UpdateDelBehaviour: Byte;     // 11 	1 byte 	Update & delete behaviour:
      // Update Cascade 0x10
      // Delete Cascade 0x01
    FieldNumOfLinkingKey: Word;   // 12-13 	16-bit number 	Number of fields in the linking key.
    LocalTableTagNameOffset: Word; // 14-15 	16-bit number 	Offset of the Local Table tag name - 0 terminated.
    LocalTableTagNameLength: Word; // 16-17 	16-bit number 	Size of previous value.
    ForeignTableTagNameOffset: Word; // 18-19 	16-bit number 	Offset of the Foreign Table tag name - 0 terminated.
    ForeignTableTagNameLength: Word; // 20-21 	16-bit number 	Size of previous value.
  end;

  { Memo }

  { DBT }

  // mapped to block size: 512 dBase3 or from BlockSize

  PDBT3Header = ^TDBT3Header;
  TDBT3Header = packed record
    NextBlock: integer;
  end;

  PDBT4Header = ^TDBT4Header;
  TDBT4Header = packed record
    NextBlock: integer;
    Dummy: integer;
    TableName: array[0 .. 7] of AnsiChar;
    Ver: integer; // low byte suppose to be ver, typical 00 00 02 01
    BlockSize: Word;
  end;

  PDBT4BlockHeader = ^TDBT4BlockHeader;
  TDBT4BlockHeader = packed record
    Sign: integer;
    Size: integer; // including header
  end;

  { HiPer-Six }

  PSMTHeader = ^TSMTHeader;
  TSMTHeader = packed record
    NextBlock: integer; // in block sizes
    BlockSize: integer; // $40
    TableName: array[0 .. 7] of AnsiChar; // 'SIxMemo'
  end;

  PSMTMemoLink = ^TSMTMemoLink;
  TSMTMemoLink = packed record
    Signature: Word;
    Len: integer;
    Offset: integer; // in blocks
  end;

  { FPT }

  PFPTHeader = ^TFPTHeader;
  TFPTHeader = packed record
    NextBlock: integer;      // 00 – 03 	Location of next free block, BigEndian
    Unused: Word;             // 04 – 05 	Unused
    BlockSize: Word;          // 06 – 07 	Block size (bytes per block), BigEndian
    Unused2: array[0 .. 503] of Byte;  // 08 – 511 	Unused
  end;

  PFPTBlockHeader = ^TFPTBlockHeader;
  TFPTBlockHeader = packed record
    Sign: integer; // 00 – 03 	Block signature 1 (indicates the type of data in the block)
      // 0 – picture (picture field type)
      // 1 – text (memo field type)
    Size: integer; // 04 – 07 	Length of memo (in bytes), without header
    // 08 – n 	Memo text (n = length)
  end;

  { Indexes }

  // Maximum length of key expression text and maximum length of conditional expression text
  // NTX 256 bytes
  // IDX (non-compact) 220 bytes
  // IDX (compact) 512 bytes**
  // CDX 512 bytes**

  // Maximum length of evaluated key expression
  // NTX 256 bytes
  // IDX (non-compact) 100 bytes
  // IDX (compact) 240 bytes
  // CDX 240 bytes

  (* MDX files structure

  00000: Header
  +--------------------------------+
  |  TMDXHeader                    |
  +--------------------------------+

  00200: Tags (NumTags of TagSize)
  +--------------------------------------+
  | 0: unused                            |
  +--------------------------------------+
  | 1: HeaderPageNo (offset to RootBlock)|
  +--------------------------------------+
  | 2 to NumTags                         |
  +--------------------------------------+

  XXXXX: blocks of PageSize * BlockSize (0x200 * 2)
  +--------------------------------------+
  | NumEntries                           |
  +--------------------------------------+
  | PrevBlock (block created before)     |                          Entry
  +--------------------------------------+         +--------------------------------------+
  | [0]: Entry                           | ---->   | RecBlockNo:                          |
  +--------------------------------------+         | if leaf then RecNo                   |
  | ...                                  |         | else points to left block            |
  +--------------------------------------+         +--------------------------------------+
  | [NumEntries - 1]: Entry              |         | Key                                  |
  +--------------------------------------+         +--------------------------------------+
  | [NumEntries]:                        |            left          Asc            right
  | if RecBlockNo = 0 then leaf          |         +--------+--------------------+--------+
  | else RecBlocNo points to right block |         | V => K |         Key        | V <= K |
  | (or as extra entry)                  |         +--------+--------------------+--------+
  +--------------------------------------+                          Desc
                                                   +--------+--------------------+--------+
                                                   | V <= K |         Key        | V >= K |
                                                   +--------+--------------------+--------+
  *)

  { MDX }
  PMDXHeader = ^TMDXHeader;
  TMDXHeader = packed record
    MdxVersion : Byte;     // 0
    Year       : Byte;     // 1
    Month      : Byte;     // 2
    Day        : Byte;     // 3
    FileName   : array[0 .. 15] of AnsiChar;   // 4..19, 8 bytes used
    BlockSize  : Word;     // 20..21
    BlockAdder : Word;     // 22..23 block size in bytes BlockSize * FPageSize
    ProdFlag   : Byte;     // 24
    NumTags    : Byte;     // 25 Number of entries in tag table. Max no. is 48 (30h)
    TagSize    : Byte;     // 26 Length of each tag table entry. Max is 32 (20h)
    Dummy1     : Byte;     // 27
    TagsUsed   : Word;     // 28..29
    Dummy2     : Byte;     // 30
    Language   : Byte;     // 31
    NumPages   : Integer;  // 32..35
    FreePage   : Integer;  // 36..39
    BlockFree  : Integer;  // 40..43
    UpdYear    : Byte;     // 44
    UpdMonth   : Byte;     // 45
    UpdDay     : Byte;     // 46
    // suppose to be of PageSize 512 plus TagSize header with TagFlag as yet another record
    //Reserved   : array[0 .. 481] of Byte;  // 47..511
  end;

  TMDXTagInfo = packed record
    KeyFormat: Byte;      // 00h: Calculated, 10h: Data Field
    ForwardTagLess: Byte; // first points to next
    ForwardTagMore: Byte; // 0-th tag points to first, first to primary, others to 0
    BackwardTag: Byte;    // 2-nd and 3-rd points to first
    Reserved: Byte;       // $02 except 0-th tag
    KeyType: AnsiChar;    // C : Character, N : Numeric, D : Date
  end;

  PMDXTag = ^TMDXTag;
  TMDXTag = packed record
    HeaderPageNo: Integer;      // 0..3
    case Integer of             // MDX header version
      2:                        // dBase 4
        (
          Ver2TagName: array[0 .. 10] of AnsiChar;
          Ver2TagInfo: TMDXTagInfo;
        );
      3: // dBase 7
        (
          Ver3TagName: array[0 .. 32] of AnsiChar;
          Ver3TagInfo: TMDXTagInfo;
        );
  end;

  PIndexHeader = ^TIndexHeader;
  TIndexHeader = packed record
    RootPage       : Integer;  // 0..3
    NumPages       : Integer;  // 4..7
    KeyFormat      : Byte;     // 8      00h: Right, Left, DTOC
                               //        08h: Descending order
                               //        10h: String
                               //        20h: Distinct
                               //        40h: Unique
    KeyType        : AnsiChar;     // 9  C : Character
                               //        N : Numerical
                               //        D : Date
    Dummy          : Word;     // 10..11, second byte it seems charset
    KeyLen         : Word;     // 12..13
    MaxNumKeys     : Word;     // 14..15 Max.no.of keys/page (including extra 4 bytes) = (BlockSize - SizeOf(TMDXBlock) - Sizeof(EntryNo)) div KeyRecLen
    SecondaryKeyType: Word;    // 16..17 00h: DB4: C/N; DB3: C
                               //        01h: DB4: D  ; DB3: N/D
    KeyRecLen      : Word;     // 18..19 Length of key entry in page
    Version        : Byte;     // 20 not clear, 1 - empty, after block been split 2 (filled to maxentries) changed to 3 (filled half+1) - leaf depth?
    Dummy2         : Word;     // 21..22
    Unique         : Byte;     // 23 like $20
    Expression     : array[0 .. 219] of AnsiChar; // 24..243, field name or expression
    Dummy3         : Byte;     // 244
    ForExist       : Byte;     // 245
    KeyExist       : Byte;     // 246 0 if empty, 1 if any added
    Dummy4         : Byte;     // 247 ???
    FirstBlock     : Longint;  // 248..251   first node that contains data
    LastBlock      : Longint;  // 252..255   last node that contains data
    // dBase 4, 7 extra:
    unk: Byte;
    UpdYear        : Byte;
    UpdMonth       : Byte;
    UpdDay         : Byte;
  end;

  PMDXEntry = ^TMdxEntry;
  TMDXEntry = packed record
    RecBlockNo: Integer;       // 0..3   either recno or blockno, recno from 1 including deleted records
    KeyData   : AnsiChar;      // 4..    first byte of data
  end;

  PMDXBlock = ^TMDXBlock; // TMDXPage
  TMDXBlock = packed record
    NumEntries : Integer;
    PrevBlock  : Integer;
    // KeysData   : PMDXEntry; // array of NumEntries of TIndexHeader.KeyRecLen size
  end;

  { NDX}
  PNDXEntry  = ^TNDXEntry;
  TNDXEntry  = packed record
    LowerPageNo: Integer;      //  0..3 lower page
    RecNo      : Integer;      //  4..7 recno
    KeyData    : AnsiChar;
  end;

  PNDXPage  = ^TNDXPage;
  TNDXPage  = packed record
    NumEntries: Integer;       //  0..3
    FirstEntry: TNDXEntry;
  end;

  { CDX}
  PCDXHeader = ^TCDXHeader;
  TCDXHeader = packed record
    RootNode:   Integer;
    FreeList:   Integer;    // 0 if empty
    Version:    Integer;    // some counter
    KeyLength:  Word;
    Options:    Byte;       // 1: unique, 2: nullable, 8: FOR clause, $10h: Bit vector (SoftC), $20: Compact index format, $40h: Compounding index header, $80: Structure index
    Signature:  Byte;                                      // 15
    Reserved:   array [0 .. 485] of Byte;                  // 16    501
    Desc:       Word;     {0 = ascending; 1=descending}    // 502   2
    ExpressionLen:   Word;                                  // 504   2
    FilterLen:  Word;     {length of FOR clause}           // 506   2
    Reserved2:  Word;                                      // 508   2
    KeyExprLen: Word;     {length of index expression}     // 510   2
    KeyPool:    array [0 .. 511] of AnsiChar;              // 512 – 1023 	Key expression pool (uncompiled)
  end;

  // "Key + RecNo + Offset" , as in Index nodes
  PCDXKeyInfo = ^TCDXKeyInfo;
  TCDXKeyInfo = packed record
    RecNo: integer; // big endian
    Offset: integer; // big endian
  end;

  PCDXCompactIndexNode = ^TCDXCompactIndexNode;
  TCDXCompactIndexNode = packed record
    Attributes: Word; // 0 – index node, 1 – root node, 2 – leaf node
    NumOfKeys:  Word; // 0, 1 or many
    LeftNode:   Integer; // Pointer to the node directly to the left of current node (on same level; -1 if not present)
    RightNode:  Integer; // Pointer to the node directly to right of the current node (on same level; -1 if not present)
    case Byte of                                                        //12
      0:( // Exterior
        FreeSpace:  Word; // Available free space in node
        RecNumMask: Integer; // Record number mask
        DupCountMask: Byte; // Duplicate byte count mask
        TrailCountMask: Byte; // Trailing byte count mask
        NumBitsRecNo: Byte; // Number of bits used for record number
        NumBitsDupCount: Byte; // Number of bits used for duplicate count
        NumBitsTrailCount: Byte; // Number of bits used for trail count
        NumBytesRecNo: Byte; // Number of bytes holding record number, duplicate count and trailing count: (NumBitsRecNo + NumBitsDupCount + NumBitsTrailCount) / 8
        // At the start of this area, the recno/dupCount/trailCount is stored (bit compressed). Each entry requires the number of bytes as indicated by byte 023
        // The key values are placed at the end of this area (working backwards) and are stored by eliminating any duplicates with the previous key and any trailing blanks.
        IndexKeys: array [0 .. 487] of Byte; // 24-511
        );
      1:( // Interior
        // Up to 500 characters containing the key value for the length of the key with a four-byte hexadecimal number (stored in normal left-to-right format):
        // This node always contains the index key, record number and intra-index pointer.
        // The key/four-byte hexadecimal number combinations will occur the number of times indicated in bytes 02 – 03 (NumOfKeys)
        data_pool: array [0 .. 499] of Byte; // array of key of keysize + TCDXNodeKeyInfo
        );
  end;

  { NSX }

const
  NSX_TAGNAME       = 11;
  NSX_MAXTAGS       = 50;
  NSX_PAGELEN_BITS  = 10;
  NSX_PAGELEN       = 1 shl NSX_PAGELEN_BITS;
  NSX_MAXEXPLEN     = 256;
  NSX_LEAFKEYOFFSET = 6;
  NSX_STACKSIZE     = 32;

type
  PNSXTagItem = ^TNSXTagItem;
  TNSXTagItem = packed record
   TagName: array[0..NSX_TAGNAME] of AnsiChar;  //* name of tag in ASCIIZ */
   TagOffset: Cardinal;                         //* Tag header offset */
  end;

  PNSXHeader = ^TNSXHeader;
  TNSXHeader = packed record
    Signature: Byte;           //* "i" = 0x69 */
    IndexFlags: Byte;          //* 0x00 */
    TagCount: Word;            //* number of tags in index file */
    Version: Word;             //* cyclic counter for concurrent access */
    FreePage: Cardinal;        //* offset of first free page in index file */
    FileSize: Cardinal;        //* the index file length */
    TagList: array[0..NSX_MAXTAGS - 1] of TNSXTagItem;
    Unused: array[0..NSX_PAGELEN - 14 - NSX_MAXTAGS * sizeof(TNSXTagItem) - 1] of Byte;
  end;

  PNSXTagHeader = ^TNSXTagHeader;
  TNSXTagHeader = packed record
    Signature: Byte;           //* "i" = 0x69 */
    TagFlags: Byte;            //* update flags: NSX_TAG_* */
    RootPage: Cardinal;        //* offset of tag root page */
    KeyType: Word;             //* index key type: NSX_TYPE_* */
    KeySize: Word;             //* index key size */
    Unique: Word;              //* 0x0001 for UNIQUE indexes */
    Descend: Word;             //* 0x0001 for descend indexes */
    KeyExpr: array[0..NSX_MAXEXPLEN - 1] of AnsiChar; //* index KEY expression ASCIIZ */
    ForExpr: array[0..NSX_MAXEXPLEN - 1] of AnsiChar; //* index FOR expression ASCIIZ */
    Unused: array[0..NSX_PAGELEN - 14 - NSX_MAXEXPLEN - NSX_MAXEXPLEN - 1] of Byte;
  end;

  PNSXTagHeaderUpdt = ^TNSXTagHeaderUpdt;
  TNSXTagHeaderUpdt = packed record
    Signature: Byte;           //* "i" = 0x69 */
    TagFlags: Byte;            //* update flags: NSX_TAG_* */
    RootPage: Cardinal;        //* offset of tag root page */
  end;

  PNSXBranchPage = ^TNSXBranchPage;
  TNSXBranchPage = packed record
    NodeID: Byte;              //* NSX_BRANCHPAGE | ( lRoot ? NSX_ROOTPAGE : 0 ) */
    RecNoLen: Byte;            //* number of bytes for recno in branch keys - seems to be unused */
    KeyCount: Word;            //* number of key in page */
    LowerPage: Cardinal;       //* offset to the page with lower keys */
    KeyData: array[0..NSX_PAGELEN - 9] of Byte;  //* with branch keys */
  end;

  PNSXLeafPage = ^TNSXLeafPage;
  TNSXLeafPage = packed record
    NodeID: Byte;              //* NSX_LEAFPAGE | ( lRoot ? NSX_ROOTPAGE : 0 ) */
    RecNoLen: Byte;            //* number of bytes for recno in leaf keys */
    KeyCount: Word;            //* number of key in page */
    UsedArea: Word;            //* area used in page -> offset to free area */
    KeyData: array[0..NSX_PAGELEN - NSX_LEAFKEYOFFSET - 1] of Byte;  //* with branch keys */
  end;

  PNSXKeyInfo = ^TNSXKeyInfo;
  TNSXKeyInfo = packed record
    page: Cardinal;     //* page number */
    rec: Cardinal;      //* record number */
    mode: Cardinal;     //* comparison mode NSX_CMP_* */
    val: Byte;          //* key value */
  end;

  PNSXTreeStack = ^TNSXTreeStack;
  TNSXTreeStack = packed record
    page: Cardinal;
    ikey: integer;
    value: Pointer;
  end;

  PNSXPageInfo = ^TNSXPageInfo;
  PPNSXPageInfo =^PNSXPageInfo;
  TNSXPageInfo = packed record
    Page: Cardinal;
    Changed: boolean;
    iUsed: Cardinal;
    uiKeys: Word;
    uiOffset: Word;
    pNext: PNSXPageInfo;
    pPrev: PNSXPageInfo;
    buffer: array[0..NSX_PAGELEN - 1] of Byte;
    rootHeader: TNSXHeader;
    tagHeader: TNSXTagHeader;
  end;

  TNSXPageHead = packed record
    case Byte of
      0:(
        buffer: Pointer;
        );
      1:(
        header: PNSXHeader;
        );
  end;

  PNSXScope = ^TNSXScope;
  TNSXScope = packed record
    scopeItem: Pointer;
    scopeKey: PNSXKeyInfo;
    scopeKeyLen: Word;
  end;

  PNSXIndex = ^TNSXIndex;

  PNSXTagInfo = ^TNSXTagInfo;
  PPNSXTagInfo = ^PNSXTagInfo;
  TNSXTagInfo = packed record
    TagName: PAnsiChar;
    KeyExpr: PAnsiChar;
    ForExpr: PAnsiChar;
    pKeyItem: Pointer;
    pForItem: Pointer;
    top: TNSXScope;
    bottom: TNSXScope;
    fUsrDescend: boolean;
    AscendKey: boolean;
    UniqueKey: boolean;
    Custom: boolean;
    ChgOnly: boolean;
    Partial: boolean;
    Template: boolean;
    MultiKey: boolean;
    HdrChanged: boolean;
    TagBOF: boolean;
    TagEOF: boolean;
    HotFor: boolean;
    HeadBlock: Cardinal;
    RootBlock: Cardinal;
    TagFlags: Byte;
    KeyType: Byte;
    TrailChar: Byte;
    KeyLength: Word;
    nField: Word;
    uiNumber: Word;
    MaxKeys: Word;
    CurKeyOffset: Word;
    CurKeyNo: Word;
    stackSize: Word;
    stackLevel: Word;
    stack: PNSXTreeStack;
    keyCount: Cardinal;
    CurKeyInfo: PNSXKeyInfo;
    HotKeyInfo: PNSXKeyInfo;
    pIndex: PNSXIndex;
  end;

  TNSXLockData = packed record
    offset: Cardinal;
    size: Cardinal;
    next: Cardinal;
    tolock: Cardinal;
    ltype: integer;
    count: integer;
  end;

{TODO: }
//  PNSXArea = ^TNSXArea;
  PNSXArea = Pointer;

  TNSXIndex = packed record
    IndexName: PAnsiChar;
    RealName: PAnsiChar;
    Version: Cardinal;       //* index VERSION filed to signal index updates for other stations */
    NextAvail: Cardinal;     //* next free page in index file */
    FileSize: Cardinal;      //* index file size */
    pArea: PNSXArea;
    pFile: Pointer;          // PHB_FILE
    fDelete: boolean;       //* delete on close flag */
    fReadonly: boolean;
    fShared: boolean;
    fFlush: boolean;
    LargeFile: boolean;
    Changed: boolean;
    Update: boolean;
    Production: boolean;    //* Production index */
    lockData: TNSXLockData;   //* index lock data */
    lockWrite: integer;     //* number of write lock set */
    lockRead: integer;      //* number of read lock set */
    HeaderBuff: TNSXHeader;
    fValidHeader: boolean;
    iTags: integer;
    lpTags: PPNSXTagInfo;
    ulPages: Cardinal;
    ulPageLast: Cardinal;
    ulPagesDepth: Cardinal;
    pages: PPNSXPageInfo;
    pChanged: PNSXPageInfo;
    pFirst: PNSXPageInfo;
    pLast: PNSXPageInfo;
    pNext: PNSXIndex;   //* The next index in the list */
  end;

  PNSXSwapPage = ^TNSXSwapPage;
  TNSXSwapPage = packed record
    nOffset: Cardinal;    //* offset in temporary file */
    ulKeys: Cardinal;     //* number of keys in page */
    ulKeyBuf: Cardinal;   //* number of keys in memory buffer */
    ulCurKey: Cardinal;   //* current key in memory buffer */
    pKeyPool: PByte;   //* memory buffer */
  end;

  PNSXSortInfo = ^TNSXSortInfo;
  TNSXSortInfo = packed record
    pTag: PNSXTagInfo;           //* current Tag */
    pTempFile: Pointer;      // (PHB_FILE) * handle to temporary file */
    szTempFileName: PAnsiChar; //* temporary file name */
    keyLen: integer;         //* key length */
    trailChar: byte;      //* index key trail character */
    recSize: byte;        //* record size in leaf keys */
    fUnique: boolean;        //* HB_TRUE if index is unique */
    fReindex: boolean;       //* HB_TRUE if reindexing is in process */
    ulMaxRec: Cardinal;       //* the highest record number */
    ulTotKeys: Cardinal;      //* total number of keys indexed */
    ulKeys: Cardinal;         //* keys in currently created page */
    ulPages: Cardinal;        //* number of pages */
    ulCurPage: Cardinal;      //* current page */
    ulPgKeys: Cardinal;       //* maximum number of key in page memory buffer */
    ulMaxKey: Cardinal;       //* maximum number of keys in single page */
    pKeyPool: Pointer;       // (HB_UCHAR *) * memory buffer for current page then for pages */
    pStartKey: Pointer;      //* beginning of key pool after sorting */
    pSwapPage: PNSXSwapPage;   //* list of pages */
    NodeList: array[0..NSX_STACKSIZE - 1] of PNSXPageInfo; //* Stack of pages */
    ulFirst: Cardinal;
    pSortedPages: Pointer; // HB_ULONG *
    pLastKey: array[0..NSX_STACKSIZE - 1] of byte; //* last key val */
    ulLastRec: Cardinal;
    ulLastLeaf: Cardinal;     //* last non empty leaf page written to tag */
    pBuffIO: Pointer;        // (HB_UCHAR *) * index IO buffer */
    ulSizeIO: Cardinal;       //* size of IO buffer in index pages */
    ulPagesIO: Cardinal;      //* number of index pages in buffer */
    ulFirstIO: Cardinal;      //* first page in buffer */
    ulLastIO: Cardinal;       //* last page in buffer */
  end;

//  TNSXArea = packed record
//   DBFAREA dbfarea;
//   HB_BOOL        fIdxAppend;       /* HB_TRUE if new record is added */
//   HB_BOOL        fSetTagNumbers;   /* Tag number should be recreated */
//   LPNSXINDEX     lpIndexes;        /* Pointer to list of indexes */
//   LPTAGINFO      lpCurTag;         /* Pointer to current order */
//   LPNSXSORTINFO  pSort;            /* Index build structure */
//  end;

implementation

end.
