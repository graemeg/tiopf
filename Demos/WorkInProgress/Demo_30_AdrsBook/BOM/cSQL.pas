{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Change history:
    Created: Jan 2000

  Notes: SQL uses in the address book application

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit cSQL;

interface
const

  // Read the primary key (oid & name) of all people
  cSQLPersonRead_PK =
    'select        ' +
    '  oid         ' +
    ' ,last_name   ' +
    ' ,first_name  ' +
    'from          ' +
    '  person      ' +
    'order by      ' +
    '  last_name   ' +
    '  ,first_name ';

  // Given a person's OID, read their details
  cSQLPersonRead_Detail =
    'select        ' +
    '  title       ' +
    ' ,initials    ' +
    ' ,notes       ' +
    'from          ' +
    '  person      ' +
    'where         ' +
    '  oid =:oid  ';

  // Given a person's OID, update their details
  cSQLPersonUpdate =
    'update person ' +
    'set           ' +
    ' last_name   =:last_name  ' +
    ' ,first_name =:first_name ' +
    ' ,title      =:title      ' +
    ' ,initials   =:initials   ' +
    ' ,notes      =:notes      ' +
    'where                      ' +
    '  oid        =:oid        ';

  // Insert a new person
  cSQLPersonCreate =
    'insert into person ' +
    '(                 ' +
    ' OID               ' +
    ' ,last_name        ' +
    ' ,first_name       ' +
    ' ,title            ' +
    ' ,initials         ' +
    ' ,notes            ' +
    ')                  ' +
    'values             ' +
    '(                 ' +
    ':OID              ' +
    ' ,:last_name       ' +
    ' ,:first_name      ' +
    ' ,:title           ' +
    ' ,:initials        ' +
    ' ,:notes           ' +
    ')                  ';

  // Delete a person
  cSQLPersonDelete =
    'delete from person ' +
    'where OID =:OID   ';

{
  cSQLPersonRead_notes =
    'select        ' +
    '  oid         ' +
    '  ,owner_oid  ' +
    ' ,notes       ' +
    'from          ' +
    '  notes       ' +
    'where         ' +
    '  owner_oid =:owner_oid  ';
}

  cSQLPersonAddressRead =
    'select       ' +
    '  oid        ' +
    ' ,owner_oid  ' +
    ' ,adrs_type  ' +
    ' ,lines      ' +
    ' ,state      ' +
    ' ,pcode      ' +
    ' ,country    ' +
    'from         ' +
    '  adrs       ' +
    'where        ' +
    ' owner_oid =:owner_oid ';

  cSQLPersonEAddressRead =
    'select                   ' +
    '   oid                   ' +
    '  ,owner_oid             ' +
    '  ,eadrs_type            ' +
    '  ,text                  ' +
    'from                     ' +
    ' eadrs                   ' +
    'where                    ' +
    '  owner_oid =:owner_oid ';

  cSQLPersonEAddressCreate =
    'insert into EAdrs ' +
    '(                ' +
    '   OID            ' +
    '  ,Owner_OID      ' +
    '  ,EAdrs_Type     ' +
    '  ,Text           ' +
    ')                 ' +
    'Values            ' +
    '(                ' +
    '  :OID           ' +
    '  ,:Owner_OID     ' +
    '  ,:EAdrs_Type    ' +
    '  ,:Text          ' +
    ')                 ';

  cSQLPersonEAddressUpdate =
    'update EAdrs                ' +
    'set                         ' +
    '   Owner_OID  =:Owner_OID  ' +
    '  ,EAdrs_Type =:EAdrs_Type ' +
    '  ,Text       =:Text       ' +
    'where                       ' +
    '   OID        =:OID        ';

  cSQLPersonEAddressDelete =
    'delete from EAdrs ' +
    'where OID =:OID  ';

  cSQLPersonAddressCreate =
    'insert into adrs ' +
    '(               ' +
    '  oid            ' +
    ' ,owner_oid      ' +
    ' ,adrs_type      ' +
    ' ,lines          ' +
    ' ,state          ' +
    ' ,pcode          ' +
    ' ,country        ' +
    ')                ' +
    'Values           ' +
    '(               ' +
    ' :oid           ' +
    ' ,:owner_oid     ' +
    ' ,:adrs_type     ' +
    ' ,:lines         ' +
    ' ,:state         ' +
    ' ,:pcode         ' +
    ' ,:country       ' +
    ')                ';

  cSQLPersonAddressUpdate =
    'update adrs               ' +
    'set                       ' +
    '  owner_oid  =:owner_oid ' +
    ' ,adrs_type  =:adrs_type ' +
    ' ,lines      =:lines     ' +
    ' ,state      =:state     ' +
    ' ,pcode      =:pcode     ' +
    ' ,country    =:country   ' +
    'where                     ' +
    '  oid        =:oid       ';

  cSQLPersonAddressDelete =
    'delete from adrs          ' +
    'where                     ' +
    '  oid        =:oid       ';


implementation

end.
