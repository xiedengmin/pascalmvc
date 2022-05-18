
//////////////////////////////////////////////////
//  ExactTarget Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ExactTargetDac.inc}
unit ExactTargetConstsUni;

interface

type
  TAuthenticationType  = (atUserAndPassword, atAppCenterClient);

const
  DefAuthenticationType: TAuthenticationType = atUserAndPassword;
  DefUrl = 'https://webservice.s7.exacttarget.com/Service.asmx';

implementation

end.
