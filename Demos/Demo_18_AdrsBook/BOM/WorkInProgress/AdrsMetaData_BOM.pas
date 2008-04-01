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

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit AdrsMetaData_BOM;

interface
uses
   tiObject
  ,Classes
  ,tiQuery
 ;

type

  // ToDo: Require code to automate this - perhaps registering the
  //       meta data along with the table-class mappings. This would
  //       make it possible to auto-generate tables.
  //
  //       Also require code to determine if a table has changed it's
  //       structure - much harder.

  TAdrsMetaData = class(TtiObject)
  private
    FDBMetaData: TtiDBMetaData;
    function    CheckField(const pTable: TtiDBMetaDataTable;
                            const pFieldName: string;
                            const pFieldKind: TtiQueryFieldKind;
                            pFieldWidth: integer = 0): boolean;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Refresh;

    function    TableExists_LookUpListName: boolean;
    procedure   CreateTable_LookupListName;
    function    CheckStructure_LookUpListName: boolean;

    function    TableExists_LookUpListValue: boolean;
    procedure   CreateTable_LookupListValue;
    function    CheckStructure_LookUpListValue: boolean;

    function    TableExists_Company: boolean;
    procedure   CreateTable_Company;
    function    CheckStructure_Company: boolean;

    function    TableExists_Person: boolean;
    procedure   CreateTable_Person;
    function    CheckStructure_Person: boolean;

    function    TableExists_EAdrs: boolean;
    procedure   CreateTable_EAdrs;
    function    CheckStructure_EAdrs: boolean;

    function    TableExists_Adrs: boolean;
    procedure   CreateTable_Adrs;
    function    CheckStructure_Adrs: boolean;

    procedure   CreateTables;
    function    CheckTables: boolean;
    procedure   DropTables;

  end;

procedure CheckDatabaseStructure;
procedure DropCreateDatabaseStructure;

const
  cTableName_Adrs = 'Adrs';
  cTableName_EAdrs = 'EAdrs';
  cTableName_Person = 'Person';
  cTableName_Company = 'Company';
  cTableName_LookupListValue = 'Lookup_List_Value';
  cTableName_LookupListName = 'Lookup_List_Name';


implementation
uses
  tiOPFManager
  ,tiDBConnectionPool
  ,SysUtils
  ,tiDialogs
  ,tiUtils
  ,Adrs_BOM
 ;

procedure CheckDatabaseStructure;
var
  lMetaData: TAdrsMetaData;
  lTablesOK: boolean;
begin
  lMetaData:= TAdrsMetaData.Create;
  try
    lMetaData.Refresh;
    lTablesOK:= lMetaData.CheckTables;
  finally
    lMetaData.Free;
  end;
  if lTablesOK then
    Exit; //==>
    
  tiAppWarning('There is a problem with some of the tables ' +
         'in the AdrsBook database.' + Cr(2) +
         'The tables will be recreated.');
  FreeAndNilAdrsBook;
  DropCreateDatabaseStructure;

end;

procedure DropCreateDatabaseStructure;
var
  lMetaData: TAdrsMetaData;
begin
  lMetaData:= TAdrsMetaData.Create;
  try
    lMetaData.Refresh;
    lMetaData.DropTables;
    lMetaData.CreateTables;
    lMetaData.Refresh;
    if not lMetaData.CheckTables then
      raise exception.create('Error creating tables');
    PopulateAdrsBook(gAdrsBook);
  finally
    lMetaData.Free;
  end;
end;

{ TAdrsMetaData }

function TAdrsMetaData.CheckField(const pTable: TtiDBMetaDataTable;
  const pFieldName: string; const pFieldKind: TtiQueryFieldKind;
  pFieldWidth: integer): boolean;
var
  lField: TtiDBMetaDataField;
begin
  result:= false;
  if pTable = nil then Exit; //==>
  lField:= pTable.FindByFieldName(pFieldName);
  if lField = nil then Exit; //==>
  if lField.Kind <> pFieldKind then Exit;
// ToDo: CSV, Paradox and IBX persistence layers will not
//       read column witdths yet...
//  if lField.Width <> pFieldWidth then Exit;
  result:= true;
end;

function TAdrsMetaData.CheckStructure_Adrs: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_Adrs);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'Owner_OID', qfkString, 36) and
    CheckField(lTable, 'Adrs_Type', qfkString, 36) and
    CheckField(lTable, 'Lines', qfkString, 180) and
    CheckField(lTable, 'Suburb', qfkString, 30) and
    CheckField(lTable, 'State', qfkString, 30) and
    CheckField(lTable, 'PCode', qfkString, 20) and
    CheckField(lTable, 'Country', qfkString, 30);
end;

function TAdrsMetaData.CheckStructure_Company: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_Company);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'Company_Name', qfkString, 60) and
    CheckField(lTable, 'Notes', qfkString, 255);
end;

function TAdrsMetaData.CheckStructure_EAdrs: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_EAdrs);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'Owner_OID', qfkString, 36) and
    CheckField(lTable, 'EAdrs_Type', qfkString, 36) and
    CheckField(lTable, 'EAdrs_Text', qfkString, 60);
end;

function TAdrsMetaData.CheckStructure_LookUpListName: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_LookupListName);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'List_Name', qfkString, 10);
end;

function TAdrsMetaData.CheckStructure_LookUpListValue: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_LookupListValue);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'Owner_OID', qfkString, 36) and
    CheckField(lTable, 'Item_Text', qfkString, 30);
end;

function TAdrsMetaData.CheckStructure_Person: boolean;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= FDBMetaData.FindByTableName(cTableName_Person);
  result:=
    CheckField(lTable, 'OID', qfkString, 36) and
    CheckField(lTable, 'Owner_OID', qfkString, 36) and
    CheckField(lTable, 'First_Name', qfkString, 60) and
    CheckField(lTable, 'Family_Name', qfkString, 60) and
    CheckField(lTable, 'Title', qfkString, 10) and
    CheckField(lTable, 'Initials', qfkString, 10) and
    CheckField(lTable, 'Notes', qfkString, 255);
end;

function TAdrsMetaData.CheckTables: boolean;
begin
  result:=
    CheckStructure_LookUpListName and
    CheckStructure_LookUpListValue and
    CheckStructure_Company and
    CheckStructure_Person and
    CheckStructure_EAdrs and
    CheckStructure_Adrs;
end;

constructor TAdrsMetaData.Create;
begin
  inherited;
  FDBMetaData:= TtiDBMetaData.Create;
end;

procedure TAdrsMetaData.CreateTables;
begin
  if not TableExists_LookUpListName then
    CreateTable_LookupListName;

  if not TableExists_LookUpListValue then
    CreateTable_LookupListValue;

  if not TableExists_Company then
    CreateTable_Company;

  if not TableExists_Person then
    CreateTable_Person;

  if not TableExists_EAdrs then
    CreateTable_EAdrs;

  if not TableExists_Adrs then
    CreateTable_Adrs;

end;

procedure TAdrsMetaData.CreateTable_Adrs;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_Adrs;
    lTable.AddInstance('OID', qfkString, 36);
    lTable.AddInstance('Owner_OID', qfkString, 36);
    lTable.AddInstance('Adrs_Type', qfkString, 36);
    lTable.AddInstance('Lines', qfkString, 180);
    lTable.AddInstance('Suburb', qfkString, 30);
    lTable.AddInstance('State', qfkString, 30);
    lTable.AddInstance('PCode', qfkString, 20);
    lTable.AddInstance('Country', qfkString, 30);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsMetaData.CreateTable_Company;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_Company;
    lTable.AddInstance('OID', qfkString, 36);
    lTable.AddInstance('Company_Name', qfkString, 60);
    lTable.AddInstance('Notes', qfkString, 255);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsMetaData.CreateTable_EAdrs;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_EAdrs;
    lTable.AddInstance('OID', qfkString,        36);
    lTable.AddInstance('Owner_OID', qfkString,  36);
    lTable.AddInstance('EAdrs_Type', qfkString, 36);
    lTable.AddInstance('EAdrs_Text', qfkString, 60);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsMetaData.CreateTable_LookupListName;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_LookupListName;
    lTable.AddInstance('OID', qfkString, 36);
    lTable.AddInstance('List_Name', qfkString, 10);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsMetaData.CreateTable_LookupListValue;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_LookupListValue;
    lTable.AddInstance('OID', qfkString, 36);
    lTable.AddInstance('Owner_OID', qfkString, 36);
    lTable.AddInstance('Item_Text', qfkString, 30);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsMetaData.CreateTable_Person;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= cTableName_Person;
    lTable.AddInstance('OID', qfkString, 36);
    lTable.AddInstance('Owner_OID', qfkString, 36);
    lTable.AddInstance('First_Name', qfkString, 60);
    lTable.AddInstance('Family_Name', qfkString, 60);
    lTable.AddInstance('Title', qfkString, 10);
    lTable.AddInstance('Initials', qfkString, 10);
    lTable.AddInstance('Notes', qfkString, 255);
    GTIOPFManager.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

destructor TAdrsMetaData.Destroy;
begin
  FDBMetaData.Free;
  inherited;
end;

procedure TAdrsMetaData.DropTables;
begin
  if TableExists_LookUpListName then
    GTIOPFManager.DropTable(cTableName_LookupListName);

  if TableExists_LookUpListValue then
    GTIOPFManager.DropTable(cTableName_LookupListValue);

  if TableExists_Company then
    GTIOPFManager.DropTable(cTableName_Company);

  if TableExists_Person then
    GTIOPFManager.DropTable(cTableName_Person);

  if TableExists_EAdrs then
    GTIOPFManager.DropTable(cTableName_EAdrs);

  if TableExists_Adrs then
    GTIOPFManager.DropTable(cTableName_Adrs);

end;

procedure TAdrsMetaData.Refresh;
var
  lPooledDB: TPooledDB;
  i: integer;
begin
  FDBMetaData.Clear;
  lPooledDB:= GTIOPFManager.DefaultDBConnectionPool.Lock;
  try
    lPooledDB.Database.ReadMetaDataTables(FDBMetaData);
    for i:= 0 to FDBMetaData.Count - 1 do
      lPooledDB.Database.ReadMetaDataFields(FDBMetaData.Items[i]);
  finally
    GTIOPFManager.DefaultDBConnectionPool.UnLock(lPooledDB);
  end;
end;

function TAdrsMetaData.TableExists_Adrs: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_Adrs) <> nil;
end;

function TAdrsMetaData.TableExists_Company: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_Company) <> nil;
end;

function TAdrsMetaData.TableExists_EAdrs: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_EAdrs) <> nil;
end;

function TAdrsMetaData.TableExists_LookUpListName: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_LookupListName) <> nil;
end;

function TAdrsMetaData.TableExists_LookUpListValue: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_LookupListValue) <> nil;
end;

function TAdrsMetaData.TableExists_Person: boolean;
begin
  result:= FDBMetaData.FindByTableName(cTableName_Person) <> nil;
end;

end.

