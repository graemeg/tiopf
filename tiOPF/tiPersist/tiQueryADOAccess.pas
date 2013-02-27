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

unit tiQueryADOAccess;

interface
uses
  Classes
  ,ADODb
  ,tiQueryADOAbs
  ,tiQuery
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseADOAccess = class( TtiDatabaseADOAbs )
  protected
    procedure   SetupDBParams ; override ;
    function    FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ): boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       ReadMetaDataTables( pData : TPersistent ) ; override ;
    procedure       ReadMetaDataFields( pData : TPersistent ) ; override ;
    function        Test : boolean ; override ;
  end ;

  // ---------------------------------------------------------------------------
  TtiQueryADOAccess = class( TtiQueryADO ) ;


implementation
uses
  tiDBConnectionPool
  ,tiPtnVisPerObj
  ,tiUtils
  ,SysUtils
  ,tiPersist
  ,cTIPersist
  ,tiWin32
  ,DB
  ,ADOX_TLB
  ,tiDialogs
  ;

const
  GLOBAL_PASSWORD='GLOBAL_PASSWORD';
  cADOProvider   = 'Provider=Microsoft.Jet.OLEDB.4.0' ;

  cAccessFileVersion2    = '3' ;
  cAccessFileVersion95   = '4' ;
  cAccessFileVersion2000 = '5' ;
  cAccessFileVersion2002 = '6' ;
  // Set this constant for the MSAccess file version created by CreateDatabase
  cAccessFileVersion     = cAccessFileVersion2000 ;

function GetConnectionString( const pDatabaseName, pUserName, pPassword : string ) : string ;
var
  pwd_str: string;
begin
  if SameText( pUserName, GLOBAL_PASSWORD ) then
  begin
    pwd_str:=Format('Jet OLEDB:Database Password=%s;', [pPassword]);
  end
  else if ( not SameText( pUserName, 'null' )) and
          ( pUserName <> '' ) then
  begin
    pwd_str:=Format('Password=%s;User ID=%s;', [pPassword, pUserName]);
  end
  else pwd_str:='';
  result :=
    Format( cADOProvider + ';' +
            pwd_str +
            'Data Source=%s;' +
            'Persist Security Info=False',
            [pDatabaseName]);
end ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseBDEParadox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseADOAccess.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
var
  lADOXCatalog : TADOXCatalog ;
  lConnectionString : string ;
begin
  // http://delphi.about.com/library/weekly/aa072401b.htm
  lConnectionString :=
    cADOProvider +
    ';Data Source=' + psDatabaseName +
    ';Jet OLEDB:Engine Type=' + cAccessFileVersion;

  lADOXCatalog := TADOXCatalog.Create(nil) ;
  try
    lADOXCatalog.Create1(lConnectionString);
  finally
    lADOXCatalog.Free;
  end ;
end;

class function TtiDatabaseADOAccess.DatabaseExists(const psDatabaseName,psUserName, psPassword: string):boolean;
var
  lDatabase : TtiDatabaseADOAccess ;
begin
  lDatabase := TtiDatabaseADOAccess.Create ;
  try
    try
      lDatabase.Connect( psDatabaseName, psUserName, psPassword, '' ) ;
      result := true ;
    except
      on e:exception do
        result := false ;
    end ;
    lDatabase.Connected := false ;
  finally
    lDatabase.Free;
  end ;
end;

function TtiDatabaseADOAccess.FieldMetaDataToSQLCreate(
  const pFieldMetaData: TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString     : result := 'VarChar( ' + IntToStr( pFieldMetaData.Width ) + ')' ;
    qfkInteger    : result := 'Integer' ;
    qfkFloat      : result := 'Numeric( 11, 5 )' ;
    qfkDateTime   : result := 'Date' ;
    qfkLogical    : result := 'Logical' ;
    qfkBinary     : result := 'Image' ;
    qfkLongString : result := 'Memo' ;
  else
    tiFmtException( 'Invalid FieldKind', ClassName, 'FieldMetaDataToSQLCreate' ) ;
  end ;
end;

procedure TtiDatabaseADOAccess.ReadMetaDataFields(pData: TPersistent);
var
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
  lDelphiTable : TADOTable ;
  i : integer ;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lDelphiTable := TADOTable.Create( nil ) ;
  try
    lDelphiTable.Connection := Connection ;
    lDelphiTable.TableName := lTable.Name ;
    {$IFDEF DELPHI5}
    // Table must be active, but only with D5
    lDelphiTable.Active := true ;
    {$ENDIF}
    lDelphiTable.FieldDefs.Update ;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create ;
      lField.Name  := lDelphiTable.FieldDefs[i].Name ;
      lField.Kind  := FieldDataTypeToTIQueryFieldKind(lDelphiTable.FieldDefs[i].DataType) ;
      if lField.Kind = qfkString then
        lField.Width := lDelphiTable.FieldDefs[i].Size
      else
        lField.Width := 0 ;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
    end ;
    lTable.ObjectState := posClean ;
    {$IFDEF DELPHI5}
    try
      // This call will raise an exception under D5 if the record set is empty.
      lDelphiTable.Active := false ;
    except end ;
    {$ENDIF}
  finally
    lDelphiTable.Free;
  end ;
end;

procedure TtiDatabaseADOAccess.ReadMetaDataTables(pData: TPersistent);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  lsl : TStringList ;
  i : integer ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lsl := TStringList.Create ;
  try
    Connection.GetTableNames(lsl, false);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create ;
      lTable.Name := lsl.Strings[i] ;
      lTable.ObjectState := posPK ;
      lMetaData.Add( lTable ) ;
      lMetaData.ObjectState := posClean ;
    end ;
  finally
    lsl.Free;
  end ;
end;


{ Piero Bonanno: 18/11/2001
  I've downloaded your latest version of tiOPF. I've noted that there
  isn't support for username and password, so I've added it (but I think
  is not much used in Access). Since, in my opinion, global password for
  Access (that is, a password for the entire db), I've added a little
  trick: the global password can be specified usig 'GLOBAL_PASSWORD' as
  username. It works, but I think should be support for custom params
  (like roles in IB). The TtiDatabase could have a TStringList for the
  custom params (accessed by CustomParams.Values[<param_name>]).}

procedure TtiDatabaseADOAccess.SetupDBParams;
begin
  Connection.LoginPrompt:=false;
  Connection.IsolationLevel := ilReadCommitted ;
  Connection.ConnectionString:= GetConnectionString( DatabaseName, UserName, Password ) ;
end;

function TtiDatabaseADOAccess.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

Initialization
  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistADOAccess,
              TtiDBConnectionPoolDataAbs,
              TtiQueryADOAccess,
              TtiDatabaseADOAccess ) ;

finalization
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistADOAccess ) ;

end.


