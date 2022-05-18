
//////////////////////////////////////////////////
//  MailChimp Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MailChimpDac.inc}
unit MailChimpPropsUni;

interface

uses
   CRProps;

const
  prMailChimpBase = 2000;

  prApiKey              = prMailChimpBase + 1;
  prMergeCustomFields   = prMailChimpBase + 2;
  prApiVersion          = prMailChimpBase + 3;
  prMergeTagAsFieldName = prMailChimpBase + 4;

implementation

end.
