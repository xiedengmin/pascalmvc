{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Cases for Interbase DBC Bug Reports         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestBugDbcOracle;

interface

{$I ZBugReport.inc}

uses
  Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZCompatibility,
  ZDbcOracle, ZSqlTestCase;

type

  {** Implements a DBC bug report test case for Oracle }
  TZTestDbcOracleBugReport = class(TZAbstractDbcSQLTestCase)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestNum1;
    procedure TestBlobValues;
    procedure TestTicket437;
  end;

implementation

uses ZTestCase;

{ TZTestDbcOracleBugReport }

function TZTestDbcOracleBugReport.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

{**
  NUMBER must be froat
}
procedure TZTestDbcOracleBugReport.TestNum1;
const
  col_id_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  col_num_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('SELECT * FROM Table_Num1');
  with ResultSet do
  begin
    with GetMetadata do
    begin
      CheckEquals(ord(stInteger), Ord(GetColumnType(col_id_Index)), 'id column type');
      CheckEquals(ord(stDouble), Ord(GetColumnType(col_num_Index)), 'Num column type');
    end;
    CheckEquals(True, Next, 'ResultSet.Next');
    CheckEquals(1, GetInt(col_id_Index), 'id value');
    CheckEquals(54321.0123456789, GetDouble(col_num_Index), 1E-11, 'Num value');
    Close;
  end;
end;

procedure TZTestDbcOracleBugReport.TestTicket437;
const
  col_id_Index      = FirstDbcIndex;
  col_text_Index    = col_id_Index +1;
  col_clobek_index  = col_text_Index +1;
  col_longek_index  = col_clobek_index +1;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  LongLobRow1, LongLobRow2, LongLobRow3: IZBlob;
begin
  if SkipForReason(srClosedBug) then Exit;

  Statement := Connection.CreateStatement;
  Statement.SetResultSetType(rtScrollInsensitive);
  Statement.SetResultSetConcurrency(rcUpdatable);

  ResultSet := Statement.ExecuteQuery('select * from table_ticket437 order by id');
  with ResultSet do try
    with GetMetadata do
    begin
      CheckEquals(Ord(stInteger), Ord(GetColumnType(col_id_Index)), 'id column type');
      CheckEquals(Ord(stString), Ord(GetColumnType(col_text_Index)), 'text column type');
      CheckEquals(Ord(stAsciiStream), Ord(GetColumnType(col_clobek_index)), 'clobek column type');
      CheckEquals(Ord(stAsciiStream), Ord(GetColumnType(col_longek_index)), 'longek column type');
    end;
    Check(Next, 'ResultSet.Next');
    CheckEquals('ASD', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('ASD', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('ASD', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow1 := GetBlob(col_longek_index);
    Check(Next, 'ResultSet.Next');
    CheckEquals('ASDF', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('ASDF', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('ASDF', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow2 := GetBlob(col_longek_index);
    Check(Next, 'ResultSet.Next');
    CheckEquals('QWERT', GetString(col_text_Index), 'the value of text varchar2(4000) field');
    CheckEquals('QWERT', GetString(col_clobek_index), 'the value of clobek clob field');
    CheckEquals('QWERT', GetString(col_longek_index), 'the value of longek long field');
    LongLobRow3 := GetBlob(col_longek_index);

    CheckEquals('ASD', LongLobRow1.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString{$ENDIF}, 'the value of longek row 1');
    CheckEquals('ASDF', LongLobRow2.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString{$ENDIF}, 'the value of longek row 2');
    CheckEquals('QWERT', LongLobRow3.{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetRawByteString{$ENDIF}, 'the value of longek row 3');
  finally
    LongLobRow1 := nil;
    LongLobRow2 := nil;
    LongLobRow3 := nil;
    Close;
  end;
end;

procedure TZTestDbcOracleBugReport.TestBlobValues;
begin
  if SkipForReason(srClosedBug) then Exit;

  with Connection.CreateStatement.ExecuteQuery('select * from blob_values') do
  begin
    CheckEquals(6, GetMetadata.GetColumnCount);
    Check(next);
    Close;
  end;
end;

initialization
  RegisterTest('bugreport',TZTestDbcOracleBugReport.Suite);
end.
