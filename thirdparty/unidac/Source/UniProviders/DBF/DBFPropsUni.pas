
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//  DBF Properties
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFPropsUni;

interface

const
  prDBFBase               = 2000;

  prCollatingSequence     = prDBFBase + 1;
  prDirect                = prDBFBase + 2;
  prDBFFormat             = prDBFBase + 3;
  prCodePage              = prDBFBase + 4;
  prConnectMode           = prDBFBase + 5;
  prIndexOnReading        = prDBFBase + 6;
  prIgnoreDataErrors      = prDBFBase + 8;
  prIgnoreMetadataErrors  = prDBFBase + 9;
  prIdentifierCase        = prDBFBase + 10;
  prAllFieldsAsNullable   = prDBFBase + 11;
  prIgnoreIndexErrors     = prDBFBase + 12;
  prIgnoreBrokenTables    = prDBFBase + 13;

implementation

end.
