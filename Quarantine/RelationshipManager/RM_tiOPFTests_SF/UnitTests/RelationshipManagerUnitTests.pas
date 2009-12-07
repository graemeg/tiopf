unit RelationshipManagerUnitTests;

interface

uses
  TestFrameWork,
  RelationshipManagerUnit;

type
  TTestBO = class(TBusinessObject)
    private
      FRef: integer;
      procedure SetRef(const Value: integer);
    public
      constructor Create; override;
      //just so that we can easily identify the object in the tests
      property Ref: integer read FRef write SetRef;
    end;

  TBusinessObjectTests = class(TTestCase)
    private
      BO: TBusinessObject;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestCreate;
    end;

  TRelationshipManagerTests = class(TTestCase)
    private
      RM: TRelationshipManager;
      BO1, BO2, BO3: TTestBO;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestAddRelationship;
      procedure TestAddRelationshipTwice;
      procedure TestAddRelationshipWithNullObject;
      procedure TestRelationshipExists;
      procedure TestFindSourceObject;
      procedure TestFindSourceObjects;
      procedure TestFindTargetObject;
      procedure TestFindTargetObjects;
      procedure TestRemoveRelationship;
      procedure TestDeleteAllObjectRelationships;
      procedure TestRelationshipExistsAfterRemoval;
    end;

const
  REL_TYPE1 = 1;

implementation

uses
  SysUtils,
  tiObject;
  
{ TTestBO }

constructor TTestBO.Create;
begin
  //do not call inherited => no ref to RM defined
end;

procedure TTestBO.SetRef(const Value: integer);
begin
  FRef := Value;
end;

{ TBusinessObjectTests }

procedure TBusinessObjectTests.SetUp;
begin
  inherited;
  BO := TBusinessObject.Create;
end;

procedure TBusinessObjectTests.TearDown;
begin
  FreeAndNil(BO);
  inherited;
end;

procedure TBusinessObjectTests.TestCreate;
begin
  CheckTrue(BO.RelationshipManager = GetRelationshipManager,
   'RelationshipManager error');
end;

{ TRelationshipManagerTests }

procedure TRelationshipManagerTests.SetUp;
begin
  inherited;
  RM := TRelationshipManager.Create;
  BO1 := TTestBO.Create;
  BO1.Ref := 1;
  BO1.RelationshipManager := RM;
  BO2 := TTestBO.Create;
  BO2.Ref := 2;
  BO2.RelationshipManager := RM;
  BO3 := TTestBO.Create;
  BO3.Ref := 3;
  BO3.RelationshipManager := RM;
end;

procedure TRelationshipManagerTests.TearDown;
begin
  FreeAndNil(RM);
  inherited;
end;

procedure TRelationshipManagerTests.TestAddRelationship;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);

  CheckEquals(1,RM.Relationships.CountNotDeleted,'Relationships.Count error');
  CheckTrue(RM.Relationships[0].SourceObject = BO1, 'AddRelationship FromObject error');
  CheckTrue(RM.Relationships[0].TargetObject = BO2, 'AddRelationship ToObject error');
  CheckEquals(1,RM.Relationships[0].RelType, 'AddRelationship RelType error');
end;

procedure TRelationshipManagerTests.TestAddRelationshipTwice;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO2, REL_TYPE1);

  CheckEquals(1,RM.Relationships.CountNotDeleted,'Relationships.Count error');
end;

procedure TRelationshipManagerTests.TestAddRelationshipWithNullObject;
begin
  RM.AddRelationship(BO1, nil, REL_TYPE1);
  CheckEquals(0,RM.Relationships.CountNotDeleted,
   'AddRelationship with nil target error');

  RM.AddRelationship(nil, BO2, REL_TYPE1);
  CheckEquals(0,RM.Relationships.CountNotDeleted,
   'AddRelationship with nil source error');
end;

procedure TRelationshipManagerTests.TestRelationshipExists;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);

  CheckTrue(RM.RelationshipExists(BO1,BO2,REL_TYPE1), 'RelationshipExists error');
end;

procedure TRelationshipManagerTests.TestFindSourceObject;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO3, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  CheckTrue(BO1 = RM.FindSourceObject(BO3,REL_TYPE1), 'FindSourceObject error');
end;

procedure TRelationshipManagerTests.TestFindSourceObjects;
var
  List: TtiObjectList;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO3, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  List := TtiObjectList.Create;
  List.OwnsObjects := false;
  try
    RM.FindSourceObjects(BO3,REL_TYPE1,List);
    CheckEquals(2, List.Count, 'Count error');
    CheckTrue(List.IndexOf(BO1) > -1, 'List.IndexOf(BO1) error');
    CheckTrue(List.IndexOf(BO2) > -1, 'List.IndexOf(BO2) error');
  finally
    FreeAndNil(List);
  end;
end;

procedure TRelationshipManagerTests.TestFindTargetObject;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO3, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  CheckTrue(BO2 = RM.FindTargetObject(BO1,REL_TYPE1), 'FindTargetObject error');
end;

procedure TRelationshipManagerTests.TestFindTargetObjects;
var
  List: TtiObjectList;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO3, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  List := TtiObjectList.Create;
  List.OwnsObjects := false;
  try
    RM.FindTargetObjects(BO1,REL_TYPE1,List);

    CheckEquals(2, List.Count, 'Count error');
    CheckTrue(List.IndexOf(BO2) > -1, 'List.IndexOf(BO2) error');
    CheckTrue(List.IndexOf(BO3) > -1, 'List.IndexOf(BO3) error');
  finally
    FreeAndNil(List);
  end;
end;

procedure TRelationshipManagerTests.TestRemoveRelationship;
var
  List: TtiObjectList;
begin

  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  RM.RemoveRelationship(BO1,BO2,REL_TYPE1);

  CheckEquals(1, RM.Relationships.CountNotDeleted, 'Count error');

  List := TtiObjectList.Create;
  List.OwnsObjects := false;
  try
    RM.FindTargetObjects(BO1,REL_TYPE1,List);
    CheckEquals(0, List.Count,'FindTo BO1 Count error');

    RM.FindSourceObjects(BO2,REL_TYPE1,List);
    CheckEquals(0, List.Count,'FindFrom BO2 Count error');

    RM.FindTargetObjects(BO2,REL_TYPE1,List);
    CheckEquals(1, List.Count,'FindTo BO2 Count error');

    RM.FindSourceObjects(BO3,REL_TYPE1,List);
    CheckEquals(1, List.Count,'FindFrom BO3 Count error');
  finally
    FreeAndNil(List);
  end;
end;

procedure TRelationshipManagerTests.TestDeleteAllObjectRelationships;
var
  List: TtiObjectList;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.AddRelationship(BO1, BO3, REL_TYPE1);
  RM.AddRelationship(BO2, BO1, REL_TYPE1);
  RM.AddRelationship(BO2, BO3, REL_TYPE1);

  RM.DeleteObjectRelationships(BO1);

  CheckEquals(1, RM.Relationships.CountNotDeleted, 'Count error');
  List := TtiObjectList.Create;
  List.OwnsObjects := false;
  try
    RM.FindTargetObjects(BO1,REL_TYPE1,List);
    CheckEquals(0, List.Count,'FindTo BO1 Count error');

    RM.FindSourceObjects(BO1,REL_TYPE1,List);
    CheckEquals(0, List.Count,'FindFrom BO1 Count error');

    RM.FindTargetObjects(BO2,REL_TYPE1,List);
    CheckEquals(1, List.Count,'FindTo BO2 Count error');
  finally
    FreeAndNil(List);
  end;
end;

procedure TRelationshipManagerTests.TestRelationshipExistsAfterRemoval;
begin
  RM.AddRelationship(BO1, BO2, REL_TYPE1);
  RM.RemoveRelationship(BO1, BO2, REL_TYPE1);

  CheckFalse(RM.RelationshipExists(BO1,BO2,REL_TYPE1), 'RelationshipExists error');
end;

initialization

  TestFramework.RegisterTest('BusinessObjectUnitTests Suite',
    TBusinessObjectTests.Suite);
  TestFramework.RegisterTest('BusinessObjectUnitTests Suite',
    TRelationshipManagerTests.Suite);

end.
