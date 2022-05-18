
//////////////////////////////////////////////////
//  QuickBooks Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I QuickBooksDac.inc}
unit QuickBooksPropsUni;

interface

uses
   CRProps;

const
  prQuickBooksBase    = 2000;

  prCompanyId         = prQuickBooksBase + 1;
  prAccessToken       = prQuickBooksBase + 2;
  prAccessTokenSecret = prQuickBooksBase + 3;
  prConsumerKey       = prQuickBooksBase + 4;
  prConsumerKeySecret = prQuickBooksBase + 5;
  prRefreshToken      = prQuickBooksBase + 6;
  prSandbox           = prQuickBooksBase + 7;

implementation

end.
