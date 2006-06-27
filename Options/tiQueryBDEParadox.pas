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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryBDEParadox;

interface
uses
  Classes
  ,dbTables
  ,tiQuery
  ,tiQueryBDEAbs
  ;

type

  TtiDatabaseBDEParadox = class( TtiDatabaseBDEAbs )
  protected
    procedure   SetupDBParams ; override ;
    function    FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       ReadMetaDataTables( pData : TtiDBMetaData ) ; override ;
    procedure       ReadMetaDataFields( pData : TtiDBMetaDataTable ) ; override ;
    procedure       RollBack ; override ; // Not supported in BDE
    function        Test : boolean ; override ;
  end ;

  TtiQueryBDEParadox = class( TtiQueryBDE ) ;


implementation
uses
   tiDBConnectionPool
  ,tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseBDEParadox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseBDEParadox.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
var
  lDir : string;
begin
  lDir := ExpandFileName( psDatabaseName ) ;
  if DatabaseExists(lDir,psUserName,psPassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistBDEParadox, psDatabaseName, psUserName, psPassword);
  if not ForceDirectories(lDir) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistBDEParadox, psDatabaseName, psUserName, psPassword);
end;

class function TtiDatabaseBDEParadox.DatabaseExists(const psDatabaseName,psUserName, psPassword: string): boolean;
var
  lDir : string ;
begin
  lDir := ExpandFileName( psDatabaseName ) ;
  result := DirectoryExists(lDir) ;
end;

function TtiDatabaseBDEParadox.FieldMetaDataToSQLCreate( const pFieldMetaData: TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString     : result := 'VarChar( ' + IntToStr( pFieldMetaData.Width ) + ' )' ;
    qfkInteger    : result := 'Integer'  ;
    qfkFloat      : result := 'Numeric( 11, 5 )' ;
    qfkDateTime   : result := 'TimeStamp'  ;
    qfkLogical    : result := 'Boolean'  ;
    qfkBinary     : result := 'Blob( 1, 2 )'  ;
    qfkLongString : result := 'Blob( 1, 1 )'  ;
  else
    raise EtiOPFInternalException.Create( 'Invalid FieldKind') ;
  end ;
end;

procedure TtiDatabaseBDEParadox.ReadMetaDataFields(pData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
  lDelphiTable : TTable ;
  i : integer ;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lDelphiTable := TTable.Create( nil ) ;
  try
    lDelphiTable.DatabaseName := DatabaseName ;
    lDelphiTable.TableName := lTable.Name ;
    lDelphiTable.FieldDefs.Update ;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create ;
      lField.Name := lDelphiTable.FieldDefs[i].Name ;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
    end ;
    lTable.ObjectState := posClean ;
  finally
    lDelphiTable.Free;
  end ;
end;

procedure TtiDatabaseBDEParadox.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  lsl : TStringList ;
  i : integer ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lsl := TStringList.Create ;
  try
    tiFilesToStringList( DatabaseName,
                         '*.db',
                         lsl,
                         false ) ;
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create ;
      lTable.Name := tiExtractFileNameOnly( lsl.Strings[i] ) ;
      lTable.ObjectState := posPK ;
      lMetaData.Add( lTable ) ;
      lMetaData.ObjectState := posClean ;
    end ;
  finally
    lsl.Free;
  end ;
end;

procedure TtiDatabaseBDEParadox.RollBack;
begin
  inherited;
{
  In "borland.public.cppbuilder.database" found this:
  <quote>
  The following limitations apply to local transactions:
      Automatic crash recovery is not provided.
      Data definition statements are not supported.
      For Paradox, local transactions can only be performed on tables with
  valid indexes. Data cannot be rolled back on Paradox tables that do not
  have indexes.
      Transactions cannot be run against temporary tables.
      Transactions cannot be run against the BDE ASCII driver.
      TransIsolation level must only be set to tiDirtyRead.

  Closing a cursor on a table during a transaction rolls back the
  transaction unless:

      Several tables are open.
      The cursor is closed on a table to which no changes were made.
  </quote>
}
end;

procedure TtiDatabaseBDEParadox.SetupDBParams;
begin
  Database.TransIsolation := tiDirtyRead ;

  // Use this line if you want to use an existing BDE alias
  //Database.AliasName := DatabaseName ;

  // Use these lines if you want to create the BDE alias on the fly
  Database.DriverName := 'STANDARD' ;
  Database.Params.Values[ 'PATH' ] := DatabaseName ;
  Database.Params.Values[ 'DEFAULT DRIVER' ] := 'PARADOX' ;

end;

function TtiDatabaseBDEParadox.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
              cTIPersistBDEParadox,
              TtiDBConnectionPoolDataAbs,
              TtiQueryBDEParadox,
              TtiDatabaseBDEParadox ) ;
finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer( cTIPersistBDEParadox ) ;

end.


