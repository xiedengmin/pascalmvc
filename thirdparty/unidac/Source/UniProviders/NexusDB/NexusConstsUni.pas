//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I NexusDac.inc}
unit NexusConstsUni;
{$ENDIF}

interface

const
  NexusDefValPort = 16000;

resourcestring
  SDMandServerCursors       = 'DisconnectedMode not allowed for server cursors';
  SCUandServerCursors       = 'CachedUpdates not allowed for server cursors';
  SBlobNotAllocatted        = 'BLOB is not allocated';
  SLocalSortingServerCursor = 'Local sorting is not compatible with server cursor types';
  SSavepointNotExist        = 'Savepoint does not exist';
  
implementation

end.
