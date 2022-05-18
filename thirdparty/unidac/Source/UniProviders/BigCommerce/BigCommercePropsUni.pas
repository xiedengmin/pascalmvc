
//////////////////////////////////////////////////
//  BigCommerce Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I BigCommerceDac.inc}
unit BigCommercePropsUni;

interface

uses
   CRProps;

const
  prBigCommerceBase     = 2000;

  prAuthentication = prBigCommerceBase + 1;
  prStoreId = prBigCommerceBase + 2;
  prClientId = prBigCommerceBase + 3;
  prAccessToken = prBigCommerceBase + 4;
  prAuthenticationToken = prBigCommerceBase + 5;

implementation

end.
