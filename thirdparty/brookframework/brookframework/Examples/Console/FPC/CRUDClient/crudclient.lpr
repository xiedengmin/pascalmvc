(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

program crudclient;

{$MODE DELPHI}
{$WARN 4046 OFF}
{$WARN 5062 OFF}

uses
  DB,
  Client;

const
  URL_SERVER = 'http://localhost:8080';

procedure ListAllPersons;
begin
  with ListPersons(URL_SERVER) do
  try
    while not EOF do
    begin
      WriteLn(FieldByName('id').AsInteger, ' ', FieldByName('name').AsString);
      Next;
    end;
  finally
    Free;
  end;
end;

procedure AddRandomPersons;
var
  VDataSet: TDataSet;
  VField: TField;
begin
  VDataSet := CreatePersonsDataSet;
  VField := VDataSet.FieldByName('name');
  VDataSet.Append;
  VField.AsString := 'Person ' + NewGuid;
  VDataSet.Append;
  VField.AsString := 'Person ' + NewGuid;
  SavePersons(URL_SERVER, VDataSet);
end;

begin
  Randomize;
  ListAllPersons;
  AddRandomPersons;
  ListAllPersons;
end.
