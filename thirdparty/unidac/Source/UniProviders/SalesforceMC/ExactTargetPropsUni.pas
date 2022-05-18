
//////////////////////////////////////////////////
//  ExactTarget Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ExactTargetDac.inc}
unit ExactTargetPropsUni;

interface

uses
   CRProps;

const
  prExactTargetBase = 2000;

  prAuthentication = prExactTargetBase + 1;
  prPartnerIDs = prExactTargetBase + 2;
  prAppSandbox = prExactTargetBase + 3;
  prAppClientID = prExactTargetBase + 4;
  prAppClientSecret = prExactTargetBase + 5;

implementation

end.
