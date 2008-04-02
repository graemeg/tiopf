unit Adrs_CreateXML;

{$I tiDefines.inc}

interface
uses
   tiObject
  ,Classes
  ,tiQuery
 ;

type

  TAdrsCreateXML = class(TtiObject)
  private
    FDatabase: TtiDatabase;
    procedure   CreateTable_AdrsType;
    procedure   CreateTable_EAdrsType;
    procedure   CreateTable_Person;
    procedure   CreateTable_EAdrs;
    procedure   CreateTable_Adrs;
    procedure   InsertPerson;
    procedure   InsertAdrsType(const ATable, AOID, AAdrsType: string);
  public
    constructor Create; override;
    destructor  Destroy; override;
    class procedure Execute(const ADatabaseName: string);
  end;

implementation
uses
  SysUtils
  ,tiUtils
  ,Adrs_BOM,
  tiQueryXMLLight
 ;

{ TAdrsMetaData }

class procedure TAdrsCreateXML.Execute(const ADatabaseName: string);
var
  LO: TAdrsCreateXML;
begin
  if FileExists(ADatabaseName) then
    tiDeleteFile(ADatabaseName);
  LO:= TAdrsCreateXML.Create;
  try
    LO.FDatabase.CreateDatabase(ADatabaseName, '', '');
    LO.FDatabase.Connect(ADatabaseName, '', '', '');
    LO.CreateTable_AdrsType;
    LO.CreateTable_EAdrsType;
    LO.CreateTable_Person;
    LO.CreateTable_EAdrs;
    LO.CreateTable_Adrs;
    LO.InsertAdrsType('adrs_type', '1001', 'Work');
    LO.InsertAdrsType('adrs_type', '1002', 'Home');
    LO.InsertAdrsType('adrs_type', '1003', 'Postal');
    LO.InsertAdrsType('eadrs_type', '2001', 'Work');
    LO.InsertAdrsType('eadrs_type', '2002', 'Home');
    LO.InsertAdrsType('eadrs_type', '2003', 'Mobile');
    LO.InsertAdrsType('eadrs_type', '2004', 'Fax');
    LO.InsertAdrsType('eadrs_type', '2005', 'EMail');
    LO.InsertPerson;
  finally
    LO.Free;
  end;
end;

procedure TAdrsCreateXML.InsertAdrsType(const ATable, AOID, AAdrsType: string);
var
  LParams: TtiQueryParams;
begin
  LParams:= TtiQueryParams.Create;
  try
    LParams.SetValueAsString('oid', AOID);
    LParams.SetValueAsString('text', AAdrsType);
    FDatabase.InsertRow(ATable, LParams);
  finally
    LParams.Free;
  end;
end;

procedure TAdrsCreateXML.InsertPerson;
var
  LParams: TtiQueryParams;
begin
  LParams:= TtiQueryParams.Create;
  try
    LParams.SetValueAsString('oid', '3000');
    LParams.SetValueAsString('first_name', 'Edna');
    LParams.SetValueAsString('last_name', 'Everage');
    LParams.SetValueAsString('title', 'Dame');
    FDatabase.InsertRow('person', LParams);

    LParams.Clear;
    LParams.SetValueAsString('oid', '4000');
    LParams.SetValueAsString('oid_person', '3000');
    LParams.SetValueAsString('oid_adrs_type', '1001');
    LParams.SetValueAsString('lines', 'Arts Centre');
    LParams.SetValueAsString('suburb', 'Melbourne');
    LParams.SetValueAsString('state', 'VIC');
    LParams.SetValueAsString('pcode', '3000');
    LParams.SetValueAsString('country', 'Australia');
    FDatabase.InsertRow('adrs', LParams);

    LParams.Clear;
    LParams.SetValueAsString('oid', '5000');
    LParams.SetValueAsString('oid_person', '3000');
    LParams.SetValueAsString('oid_eadrs_type', '2005');
    LParams.SetValueAsString('eadrs_text', 'dameedna@housewifesuperstar.com.au');
    FDatabase.InsertRow('eadrs', LParams);

  finally
    LParams.Free;
  end;
end;

constructor TAdrsCreateXML.Create;
begin
  inherited;
  FDatabase:= TtiDatabaseXMLLight.Create;
end;

procedure TAdrsCreateXML.CreateTable_Adrs;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= 'adrs';
    lTable.AddInstance('oid', qfkString, 36);
    lTable.AddInstance('oid_person', qfkString, 36);
    lTable.AddInstance('oid_adrs_type', qfkString, 36);
    lTable.AddInstance('lines', qfkString, 180);
    lTable.AddInstance('suburb', qfkString, 30);
    lTable.AddInstance('state', qfkString, 30);
    lTable.AddInstance('pcode', qfkString, 20);
    lTable.AddInstance('country', qfkString, 30);
    FDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsCreateXML.CreateTable_EAdrs;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= 'eadrs';
    lTable.AddInstance('oid', qfkString,        36);
    lTable.AddInstance('oid_person', qfkString,  36);
    lTable.AddInstance('oid_eadrs_type', qfkString, 36);
    lTable.AddInstance('eadrs_text', qfkString, 60);
    FDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsCreateXML.CreateTable_AdrsType;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= 'adrs_type';
    lTable.AddInstance('oid', qfkString, 36);
    lTable.AddInstance('text', qfkString, 60);
    FDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsCreateXML.CreateTable_EAdrsType;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= 'eadrs_type';
    lTable.AddInstance('oid', qfkString, 36);
    lTable.AddInstance('text', qfkString, 60);
    FDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TAdrsCreateXML.CreateTable_Person;
var
  lTable: TtiDBMetaDataTable;
begin
  lTable:= TtiDBMetaDataTable.Create;
  try
    lTable.Name:= 'person';
    lTable.AddInstance('oid', qfkString, 36);
    lTable.AddInstance('first_name', qfkString, 60);
    lTable.AddInstance('last_name', qfkString, 60);
    lTable.AddInstance('title', qfkString, 10);
    lTable.AddInstance('initials', qfkString, 10);
    lTable.AddInstance('notes', qfkString, 255);
    FDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

destructor TAdrsCreateXML.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

end.

