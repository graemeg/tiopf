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

  Revision history:
    November 2001, Peter Hinrichsen, Created.

  Purpose:
    There are now three strategies available for mapping objects to a relational
    database:

    Hard code the SQL into the application, and use the Visitor framework to map
    the SQL to the objects. This is implemented by linking in the unit
    Adrs_SrvHardCodeSQL.pas

    Use the tiSQLManager application to maintain the SQL outside the
    application. This has the advantage of decoupling the SQL from the
    application, but the disadvantage of forcing you to add the tiSQLManager
    tables to the database. (This will be corrected when we have an XML
    persistence layer available)

    Setup mappings between the objects and tables, properties and columns and
    let the persistence framework generate the SQL for you. This can be
    implemented by linking in Adrs_SrvAutoGenSQL.pas (This strategy is under
    construction, and this example will work in most clases. There are some
    problems with BLOBS, and the way the mappings are defined is a little messy)

    This demo uses the Auto OO-DB map framework.

  Classes:

  ToDo:
    Better documentation of how this all works.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ Notes:

  1. The AutoGenSQL mapping demo has been thoroughly tested with
     DUnit and is quite stable. This is the best place to get started,
     however is will produce very verbose SQL. The AutoGenSQL demo will
     work with any SQL persistence layer, as well as the XML persistence
     layer.

     IF YOU WANT XML, THEN YOU MUST USE AutoGenSQL.

  2. The HardCodedSQL is one of the original demos, but has not been
     DUnit tested as thoroughly as the AutoGenSQL approach. The
     HardCodedSQL will produce the most optimised application, but will
     NOT work with the SQL database.

  3. The QueryManager framework is also one of the original parts of the
     tiOPF, but is harder to get running. The advantage of the QueryManager
     is that all SQL is stored outside the EXE in the database. This is great
     when there are lots of complex queries that you will want a DBA to work
     on independently of the Delphi developers.

  Note also that the DUnit tests for this demo will only work for the
  AutoGenSQL framework.

}


{$I tiDefines.inc}

unit Adrs_Dependencies;

interface
uses
   tiPerObjOIDGUID
  ,Adrs_BOM
  ,Adrs_SrvAutoGenSQL  // For auto generated SQL
  ;

implementation

end.
