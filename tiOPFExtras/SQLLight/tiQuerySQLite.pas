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

unit tiQuerySQLite;

interface
uses
  Classes
  ,dbTables
  ,cTIPersist
  ,tiQuery
  ,tiQuerySQLiteAbs
  ,SQLite3
  ,DB
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseSQLite = class( TtiDatabaseSQLiteAbs )
  protected
    procedure   SetupDBParams ; override ;
    function    FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       ReadMetaDataTables( pData : TPersistent ) ; override ;
    procedure       ReadMetaDataFields( pData : TPersistent ) ; override ;
    procedure       RollBack ; override ; // Not supported in BDE
    function        Test : boolean ; override ;
  end ;

  // ---------------------------------------------------------------------------
  TtiQuerySQLite = class( TtiQuerySQLiteAbs ) ;


implementation
uses
  tiDBConnectionPool
  ,tiPtnVisPerObj
  ,tiUtils
  ,tiPersist
  ,SysUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseSQLite
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseSQLite.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
var
  lDatabase : TtiSQLite3DB;
  lFile : string;
begin
  lFile := ExpandFileName( psDatabaseName ) ;
  if DatabaseExists(lFile,psUserName,psPassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistSQLite, psDatabaseName, psUserName, psPassword);
  if not ForceDirectories(ExtractFileDir(lFile)) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistSQLite, psDatabaseName, psUserName, psPassword);

  lDatabase := TtiSQLite3DB.Create ;
  lDatabase.MustExist := False;
  lDatabase.Database := lFile;
  lDatabase.Open;
  lDatabase.Close;
  lDatabase.Free;
end;

class function TtiDatabaseSQLite.DatabaseExists(const psDatabaseName,psUserName, psPassword: string): boolean;
var
  lFile : string ;
begin
  lFile := ExpandFileName( psDatabaseName ) ;
  result := FileExists(lFile) ;
end;

function TtiDatabaseSQLite.FieldMetaDataToSQLCreate( const pFieldMetaData: TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString     : result := 'VarChar( ' + IntToStr( pFieldMetaData.Width ) + ' )' ;
    qfkInteger    : result := 'Integer'  ;
    qfkFloat      : result := 'Numeric( 11, 5 )' ;
    qfkDateTime   : result := 'TimeStamp'  ;
    qfkLogical    : result := 'Boolean'  ;
    qfkBinary     : result := 'Blob( 1, 2 )'  ;
    qfkLongString : result := 'Longtext'  ;
  else
    tiFmtException( 'Invalid FieldKind', ClassName, 'FieldMetaDataToSQLCreate' ) ;
  end ;
end;

procedure TtiDatabaseSQLite.ReadMetaDataFields(pData: TPersistent);
var
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
  i, FieldLen, FieldDec : integer ;
  lsl : TStrings;
  FieldType : TFieldType;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lsl := TStringList.Create;
  try
    DataBase.GetFieldNames(lTable.Name,lsl);
    for i := 0 to Pred(lsl.Count) do begin
      DataBase.GetFieldDefInfo(lTable.Name,lsl.Strings[i],FieldType,FieldLen,FieldDec);
      lField := TtiDBMetaDataField.Create ;
      lField.Name := lsl.Strings[i];
      lField.Width := 0;
      case FieldType of
        ftString   : begin lField.Kind := qfkString;   lField.Width := FieldLen; end;
        ftFloat    : lField.Kind := qfkFloat;
        ftDate     : lField.Kind := qfkDateTime;
        ftDateTime : lField.Kind := qfkDateTime;
        ftTime     : lField.Kind := qfkDateTime;
        ftBoolean  : lField.Kind := qfkLogical;
        ftCurrency : lField.Kind := qfkFloat;
        ftBlob     : lField.Kind := qfkBinary;
        ftGraphic  : lField.Kind := qfkBinary;
        ftMemo     : lField.Kind := qfkLongString;
      end;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
    end ;
    lTable.ObjectState := posClean ;
  finally
    lsl.Free;
  end ;
end;

procedure TtiDatabaseSQLite.ReadMetaDataTables(pData: TPersistent);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  lsl : TStringList ;
  i : integer ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lsl := TStringList.Create ;
  try
    Database.GetTableNames(lsl);
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

procedure TtiDatabaseSQLite.RollBack;
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

procedure TtiDatabaseSQLite.SetupDBParams;
begin
  inherited;
  DataBase.DatabaseName := DatabaseName;
end;

function TtiDatabaseSQLite.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

Initialization
  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistSQLite,
              TtiDBConnectionPoolDataAbs,
              TtiQuerySQLite,
              TtiDatabaseSQLite ) ;
finalization
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistSQLite ) ;

end.


