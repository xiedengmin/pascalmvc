
//////////////////////////////////////////////////
//  MailChimp Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MailChimpDac.inc}
unit MailChimpConstsUni;

interface

type
  TMergeCustomFields = (mcfNone, mcfJoinCommon, mcfJoinAll);
  TApiVersion = (apiVer2, apiVer3);

const
  DefMergeCustomFields = mcfJoinCommon;
  DefApiVersion: TApiVersion = apiVer3;

implementation

end.
