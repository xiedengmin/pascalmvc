
//////////////////////////////////////////////////
//  FreshBooks Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I FreshBooksDac.inc}
unit FreshBooksPropsUni;

interface

uses
   CRProps;

const
  prFreshBooksBase      = 2000;

  prApiVersion          = prFreshBooksBase + 1;
  prCompanyName         = prFreshBooksBase + 2;
  prAccessToken         = prFreshBooksBase + 3;
  prRefreshToken        = prFreshBooksBase + 4;
  prAuthenticationToken = prFreshBooksBase + 5;

implementation

end.
