{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Test Case for Interbase Tokenizer Classes       }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestInterbaseToken;

interface

{$I ZParseSql.inc}

{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}
uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZTokenizer,
  ZInterbaseToken, ZTestTokenizer;

type

  {** Implements a test case for InterbaseTokenizer classes. }
  TZTestInterbaseTokenizer = class(TZAbstractTokenizerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestWhitespaceState;
    procedure TestQuoteState;
    procedure TestCommentState;
    procedure TestSymbolState;
    procedure TestWordState;
    procedure TestNumberState;
  end;
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
implementation
{$IFNDEF DISABLE_INTERBASE_AND_FIREBIRD}

{ TZTestInterbaseTokenizer }

{**
  Sets up the test environment before tests.
}
procedure TZTestInterbaseTokenizer.SetUp;
begin
  Tokenizer := TZInterbaseTokenizer.Create;
end;

{**
  Runs a test for comments.
}
procedure TZTestInterbaseTokenizer.TestCommentState;
const
  TokenString1: string = '-aaa/*bbb*/ccc'#10;
  TokenTypes1: array[0..4] of TZTokenType = (
    ttSymbol, ttWord, ttComment, ttWord, ttWhitespace);
  TokenValues1: array[0..4] of string = (
    '-', 'aaa', '/*bbb*/', 'ccc', #10);
  TokenString2: string = '-aaa/*bbb*/ccc--simple line comment'#10#10;
  TokenTypes2: array[0..5] of TZTokenType = (
    ttSymbol, ttWord, ttComment, ttWord, ttComment, ttWhitespace);
  TokenValues2: array[0..5] of string = (
    '-', 'aaa', '/*bbb*/', 'ccc', '--simple line comment'#10, #10);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString2, [toSkipEOF]),
    TokenTypes2, TokenValues2);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestInterbaseTokenizer.TestQuoteState;
const
  TokenString1: string = '"a""aa" ''cc''''c''';
  TokenTypes1: array[0..1] of TZTokenType = (
    ttWord, ttQuoted);
  TokenValues1: array[0..1] of string = (
    '"a""aa"', '''cc''''c''');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for symbols.
}
procedure TZTestInterbaseTokenizer.TestSymbolState;
const
  TokenString1: string = '=<>>=!< < >';
  TokenTypes1: array[0..5] of TZTokenType = (
    ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol, ttSymbol);
  TokenValues1: array[0..5] of string = (
    '=', '<>', '>=', '!<', '<', '>');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for whitespaces.
}
procedure TZTestInterbaseTokenizer.TestWhitespaceState;
const
  TokenString1: string = 'aaa '#9'ccc'#10#13;
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttWhitespace, ttWord, ttWhitespace);
  TokenValues1: array[0..3] of string = (
    'aaa', ' '#9, 'ccc', #10#13);
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1, [toSkipEOF]),
    TokenTypes1, TokenValues1);
end;

{**
  Runs a test for words.
}
procedure TZTestInterbaseTokenizer.TestWordState;
const
  TokenString1: string = ' _a_a. $b$b p2p';
  TokenTypes1: array[0..3] of TZTokenType = (
    ttWord, ttSymbol, ttWord, ttWord);
  TokenValues1: array[0..3] of string = (
    '_a_a', '.', '$b$b', 'p2p');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

{**
  Runs a test for quoted strings.
}
procedure TZTestInterbaseTokenizer.TestNumberState;
const
  TokenString1: string = '.A .123 123.456a 123.456e10 2E-12c';
  TokenTypes1: array[0..7] of TZTokenType = (
    ttSymbol, ttWord, ttFloat, ttFloat, ttWord, ttFloat, ttFloat, ttWord);
  TokenValues1: array[0..7] of string = (
    '.', 'A', '.123', '123.456', 'a', '123.456e10', '2E-12', 'c');
begin
  CheckTokens(Tokenizer.TokenizeBuffer(TokenString1,
    [toSkipEOF, toSkipWhitespaces]), TokenTypes1, TokenValues1);
end;

initialization
  RegisterTest('parsesql',TZTestInterbaseTokenizer.Suite);
{$ENDIF DISABLE_INTERBASE_AND_FIREBIRD}
end.

