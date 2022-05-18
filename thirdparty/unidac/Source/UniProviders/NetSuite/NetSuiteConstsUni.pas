
//////////////////////////////////////////////////
//  NetSuite Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NetSuiteDac.inc}
unit NetSuiteConstsUni;

interface

type
  TAuthenticationType  = (atBasic, atTokenBased);

const
  DefAuthenticationType: TAuthenticationType = atBasic;

implementation

end.
