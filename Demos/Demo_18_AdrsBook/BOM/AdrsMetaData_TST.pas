unit AdrsMetaData_TST;

interface
uses
   tiOPFManagerAbs_TST
  ,tiVisitorDB
  ,AdrsMetaData_BOM
 ;

type

  TTestAdrsMetaData = class(TtiTestCase)
  private
    FAdrsMetaData: TAdrsMetaData;
  protected
    procedure Setup; override;
    procedure TearDown;override;
  public
    constructor Create(MethodName: string); override;
  published
    procedure TableAdrs;
    procedure TableEAdrs;
    procedure TablePerson;
    procedure TableCompany;
    procedure TableLookup_List_Value;
    procedure TableLookup_List_Name;

    procedure CreateTables;

  end;

procedure RegisterTests;

implementation
uses
  tiDBConnectionSetupAbs_TST
  ,ctiOPFManager
  ,TestFramework
  ,tiOPFManager
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  RegisterDBTests('AdrsMetaData', TTestAdrsMetaData);
end;

{ TTestAdrsMetaData }

constructor TTestAdrsMetaData.Create(MethodName: string);
begin
  inherited;
  SetupTasks:= [sutPerLayer, sutDBConnection];
end;

procedure TTestAdrsMetaData.CreateTables;
begin
  FAdrsMetaData.CreateTables;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.CheckTables, 'CheckTables failed');
end;

procedure TTestAdrsMetaData.Setup;
begin
  inherited;
  try GTIOPFManager.DropTable('adrs') except end;
  try GTIOPFManager.DropTable('EAdrs') except end;
  try GTIOPFManager.DropTable('Person') except end;
  try GTIOPFManager.DropTable('Company') except end;
  try GTIOPFManager.DropTable('Lookup_List_Value') except end;
  try GTIOPFManager.DropTable('Lookup_List_Name') except end;
  FAdrsMetaData:=TAdrsMetaData.Create;
  FAdrsMetaData.Refresh;
end;

procedure TTestAdrsMetaData.TableAdrs;
begin
  Check(not FAdrsMetaData.TableExists_Adrs, 'Adrs exists');
  FAdrsMetaData.CreateTable_Adrs;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_Adrs, 'Adrs does not exist');
  Check(FAdrsMetaData.CheckStructure_Adrs, 'Adrs Structure');
end;

procedure TTestAdrsMetaData.TableCompany;
begin
  Check(not FAdrsMetaData.TableExists_Company, 'Company exists');
  FAdrsMetaData.CreateTable_Company;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_Company, 'Company does not exist');
  Check(FAdrsMetaData.CheckStructure_Company, 'Company Structure');
end;

procedure TTestAdrsMetaData.TableEAdrs;
begin
  Check(not FAdrsMetaData.TableExists_EAdrs, 'EAdrs exists');
  FAdrsMetaData.CreateTable_EAdrs;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_EAdrs, 'EAdrs does not exist');
  Check(FAdrsMetaData.CheckStructure_EAdrs, 'EAdrs Structure');
end;

procedure TTestAdrsMetaData.TableLookup_List_Name;
begin
  Check(not FAdrsMetaData.TableExists_LookUpListName, 'Lookup_List_Name exists');
  FAdrsMetaData.CreateTable_LookupListName;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_LookUpListName, 'Lookup_List_Name does not exist');
  Check(FAdrsMetaData.CheckStructure_LookUpListName, 'Lookup_List_Name Structure');
end;

procedure TTestAdrsMetaData.TableLookup_List_Value;
begin
  Check(not FAdrsMetaData.TableExists_LookUpListValue, 'Lookup_List_Value exists');
  FAdrsMetaData.CreateTable_LookupListValue;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_LookUpListValue, 'Lookup_List_Value does not exist');
  Check(FAdrsMetaData.CheckStructure_LookUpListValue, 'Lookup_List_Value Structure');
end;

procedure TTestAdrsMetaData.TablePerson;
begin
  Check(not FAdrsMetaData.TableExists_Person, 'Person exists');
  FAdrsMetaData.CreateTable_Person;
  FAdrsMetaData.Refresh;
  Check(FAdrsMetaData.TableExists_Person, 'Person does not exist');
  Check(FAdrsMetaData.CheckStructure_Person, 'Person Structure');
end;

procedure TTestAdrsMetaData.TearDown;
begin
  FAdrsMetaData.Free;
  inherited;
end;

end.
