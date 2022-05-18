
//////////////////////////////////////////////////
//  Salesforce Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SalesforceDac.inc}
unit SalesforcePropsUni;

interface

uses
   CRProps;

const
  prSalesforceBase = 2000;

  prSecurityToken   = prSalesforceBase + 1;
  prIncludeDeleted  = prSalesforceBase + 2;

implementation

end.
