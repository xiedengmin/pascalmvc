
//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  NexusDB Properties
//  Created:            04.06.15
//////////////////////////////////////////////////

{$I NexusDac.inc}
unit NexusPropsUni;


interface

const
  prNexusBase             = 2000;
  prDatabaseReadOnly      = prNexusBase + 1;
  prCommandReadOnly       = prNexusBase + 2;
  prDirectLoad            = prNexusBase + 3;
  prServerCursor          = prNexusBase + 4;
  prCursorUpdate          = prNexusBase + 5;
  prHeartbeatInterval     = prNexusBase + 6;
  prLostConnectionTimeout = prNexusBase + 7;
  prWatchdogInterval      = prNexusBase + 8;
  prDetectFieldsOnPrepare = prNexusBase + 9;
  prProtocol          	  = prNexusBase + 10;
  prSecretKey         	  = prNexusBase + 11;

implementation

end.
