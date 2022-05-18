
//////////////////////////////////////////////////
//  NetSuite Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NetSuiteDac.inc}
unit NetSuitePropsUni;

interface

uses
   CRProps;

const
  prNetSuiteBase  = 2000;

  prAccountId      = prNetSuiteBase +  1;
  prApplicationId  = prNetSuiteBase +  2;
  prCustomTables   = prNetSuiteBase +  3;
  prCustomFields   = prNetSuiteBase +  4;
  prSandbox        = prNetSuiteBase +  5;
  prAuthentication = prNetSuiteBase +  6;
  prConsumerKey    = prNetSuiteBase +  7;
  prConsumerSecret = prNetSuiteBase +  8;
  prToken          = prNetSuiteBase +  9;
  prTokenSecret    = prNetSuiteBase + 10;
  prRoleId         = prNetSuiteBase + 11;

implementation

end.
