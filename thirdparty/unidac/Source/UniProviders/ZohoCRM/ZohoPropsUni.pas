
//////////////////////////////////////////////////
//  Zoho CRM Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ZohoDac.inc}
unit ZohoPropsUni;

interface

uses
   CRProps;

const
  prZohoBase      = 2000;

  prApiVersion                = prZohoBase + 1;
  prAccessToken               = prZohoBase + 2;
  prRefreshToken              = prZohoBase + 3;
  prAuthenticationToken       = prZohoBase + 4;
  prEnableNonApprovedRecords  = prZohoBase + 5;

implementation

end.
