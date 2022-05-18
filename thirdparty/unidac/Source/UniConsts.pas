
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

unit UniConsts;

interface

// default connection property values
const
  DefValPort = 0;

resourcestring
  SProviderNotDefined    = 'Provider is not defined';
  SInvalidProviderName   = '%s is not a valid provider name';
  SProviderNotRegistered = '%s provider is not registered. You should add the %s unit to the uses ' +
    'clause of any unit in your project or place the %s component on the form.';
  SAlertsNotSupported    = 'Event notification is not supported by DBMS or provider';   
  SInvalidOptionName   = '"%s" is not a valid option name for %s UniProvider';

  // UniMacros
  SEmptyFunctionName = 'Empty function name';
  SEmptyMacroOrFunction = 'Empty macro or function name';
  SInvalidBracketCount = 'Invalid bracket count in function call';
  SInvalidDate = 'Invalid year-month-day string. The format is {date ''yyyy-mm-dd''}';
  SInvalidTime = 'Invalid hour-minute-second string. The format is {time ''hh:mm:ss''}';
  SInvalidTimestamp = 'Invalid timestamp string. The format is {timestamp ''yyyy-mm-dd hh:mm:ss''}';
  SNotCompletedIF = '{if} statement not completed';
  SUnexpectedChar = 'Unexpected character';
  SUnexpectedElse = 'Unexpected {else}';
  SUnexpectedEndif = 'Unexpected {endif}';
  SUnknownDatepart = 'Unknown datepart: %s';
  SUnknownFunction = 'Unknown function: %s';
  SWrongArgCnt = 'Wrong argument count for function: %s';
  SCyclicConditions = 'Cyclic macros conditions';

implementation

end.
