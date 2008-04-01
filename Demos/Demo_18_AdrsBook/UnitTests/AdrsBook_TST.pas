unit AdrsBook_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework,
  Adrs_BOM,
  AdrsBook_TSTSetup;

const
  cOIDPerson = '0001';
  cOIDEAdrs  = '0002';

type

  TTestAdrsBook = class(TtiTestCase)
  private
    FAdrsBookSetup: TAdrsBookTestSetup;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure EmptyDatabase;
    property  AdrsBookSetup: TAdrsBookTestSetup read FAdrsBookSetup;
  public
    constructor Create(MethodName: string); override;
    destructor  Destroy; override;

  published
    procedure PersonList_Read;
    procedure Person_Create;
    procedure Person_Update;
    procedure Person_Delete;

    procedure EAdrs_Read;
    procedure EAdrs_Create;
    procedure EAdrs_Update;
    procedure EAdrs_Delete;

  end;

procedure RegisterTests;

implementation
uses
  tiConstants,
  tiOPFManager,
  tiObject,
  SysUtils,
  TestFramework;

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestAdrsBook.Suite);
end;

{ TTestAdrsBook }

constructor TTestAdrsBook.Create(MethodName: string);
begin
  inherited;
  FAdrsBookSetup:= TAdrsBookTestSetup.Create(Self);
end;

destructor TTestAdrsBook.Destroy;
begin
  FAdrsBookSetup.Free;
  inherited;
end;

procedure TTestAdrsBook.EAdrs_Create;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LEAdrs: TEAdrs;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    LPerson:= LAdrsBook.People.Items[0];
    LEAdrs:= AdrsBookSetup.EAdrs_Create(cOIDEAdrs);
    LEAdrs.ObjectState:= posCreate;
    LPerson.EAddressList.Add(LEAdrs);
    LPerson.Save;
    Check(LEAdrs.ObjectState = posClean);
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    LPerson:= LAdrsBook.People.Items[0];
    LPerson.Read;
    CheckEquals(1, LPerson.EAddressList.Count);
    LEAdrs:= LPerson.EAddressList.Items[0];
    AdrsBookSetup.EAdrs_Check(cOIDEAdrs, LEAdrs);
  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestAdrsBook.EAdrs_Delete;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LEAdrs: TEAdrs;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  AdrsBookSetup.EAdrs_Insert(cOIDPerson, cOIDEAdrs);
  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    LPerson:= LAdrsBook.People.Items[0];
    LPerson.Read;
    LEAdrs:= LPerson.EAddressList.Items[0];

    LEAdrs.Deleted:= True;
    LPerson.Save;
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    LPerson:= LAdrsBook.People.Items[0];
    LPerson.Read;
  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestAdrsBook.EAdrs_Read;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LEAdrs: TEAdrs;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  AdrsBookSetup.EAdrs_Insert(cOIDPerson, cOIDEAdrs);
  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    LPerson:= LAdrsBook.People.Items[0];
    Check(LPerson.ObjectState = posPK);

//    LPerson.Read;

    Check(LPerson.ObjectState = posClean);
//    CheckEquals(1, LPerson.EAdrsList.Count);
//    LEAdrs:= LPerson.EAdrsList.Items[0];

//    AdrsBookSetup.EAdrs_Check(cOIDEAdrs, LEAdrs);


  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestAdrsBook.EAdrs_Update;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
  LEAdrs: TEAdrs;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  AdrsBookSetup.EAdrs_Insert(cOIDPerson, cOIDEAdrs);
  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    LPerson:= LAdrsBook.People.Items[0];
    LPerson.Read;
    LEAdrs:= LPerson.EAddressList.Items[0];
    AdrsBookSetup.EAdrs_Set(cOIDEAdrs+'1', LEAdrs);
    LEAdrs.Dirty:= True;
    LPerson.Save;
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
    LAdrsBook.Read;
    LPerson:= LAdrsBook.People.Items[0];
    LPerson.Read;
    LEAdrs:= LPerson.EAddressList.Items[0];
    AdrsBookSetup.EAdrs_Check(cOIDEAdrs+'1', LEAdrs);
  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestAdrsBook.EmptyDatabase;
begin
  GTIOPFManager.ExecSQL('delete from eadrs');
  GTIOPFManager.ExecSQL('delete from person');
end;

procedure TTestAdrsBook.Person_Create;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
begin
  LAdrsBook:= TAdrsBook.Create;
  try
    LPerson:= AdrsBookSetup.Person_Create(cOIDPerson);
    LPerson.ObjectState:= posCreate;
    LAdrsBook.People.Add(LPerson);
//    LAdrsBook.Save;
    Check(LPerson.ObjectState = posClean);
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    AdrsBookSetup.Person_Check(cOIDPerson, LAdrsBook.People.Items[0]);
  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestAdrsBook.Person_Delete;
var
  LAdrsBook: TAdrsBook;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    LAdrsBook.People.Items[0].Deleted:= True;
//    LAdrsBook.Save;
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(0, LAdrsBook.People.Count);
  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestAdrsBook.PersonList_Read;
var
  LAdrsBook: TAdrsBook;
  LPerson: TPerson;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    LPerson:= LAdrsBook.People.Items[0];
    AdrsBookSetup.Person_Check(cOIDPerson, LPerson);
    Check(LPerson.ObjectState = posPK);
  finally
    LAdrsBook.Free;
  end;
end;

procedure TTestAdrsBook.Person_Update;
var
  LAdrsBook: TAdrsBook;
begin
  AdrsBookSetup.Person_Insert(cOIDPerson);
  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    AdrsBookSetup.Person_Set(cOIDPerson + '1', LAdrsBook.People.Items[0]);
    LAdrsBook.People.Items[0].Dirty:= True;
//    LAdrsBook.Save;
  finally
    LAdrsBook.Free;
  end;

  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    CheckEquals(1, LAdrsBook.People.Count);
    AdrsBookSetup.Person_Check(cOIDPerson + '1', LAdrsBook.People.Items[0]);
  finally
    LAdrsBook.Free;
  end;

end;

procedure TTestAdrsBook.Setup;
begin
  inherited;
  EmptyDatabase;
end;

procedure TTestAdrsBook.TearDown;
begin
  inherited;
end;

end.



