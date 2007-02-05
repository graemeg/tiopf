unit tiRTTI_TST;

{$I tiDefines.inc}

interface

uses
  Classes
  {$IFDEF FPC}
  ,testregistry
  {$ELSE}
  ,TestFrameWork
  {$ENDIF}
  ,tiTestFramework
  ,tiObject
  ,tstPerFramework_BOM
  ;
  
type

  TTesttiRTTI = class(TtiTestCase)
  published
    procedure TestSetProperty_Simple;
    procedure TestSetProperty_PropertyPath;
    procedure TestGetProperty_Simple;
    procedure TestGetProperty_PropertyPath;
  end;
  

  TtiRTTITestClass = class(TtiObject)
  private
    FTestItem: TtiOPFTestItem;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property TestItem: TtiOPFTestItem read FTestItem write FTestItem;
  end;
  
  
  TtiRTTITestClassB = class(TtiObject)
  private
    FTestItemB: TtiRTTITestClass;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property TestItemB: TtiRTTITestClass read FTestItemB write FTestItemB;
  end;
  

procedure RegisterTests;


implementation

uses
  tiDUnitDependencies
  ,tiRTTI
  ,SysUtils
  ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTesttiRTTI);
end;

procedure TTesttiRTTI.TestSetProperty_Simple;
var
  c: TtiOPFTestItem;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  c := TtiOPFTestItem.Create;
  try
    tiSetProperty(c, 'StrField', 'Graeme');
    CheckEquals('Graeme', c.StrField, 'Failed on 1');

    tiSetProperty(c, 'IntField', 32);
    CheckEquals(32, c.IntField, 'Failed on 2');

    tiSetProperty(c, 'FloatField', 12.345);
    CheckEquals(12.345, c.FloatField, 0.0001, 'Failed on 3');

    tiSetProperty(c, 'DateField', lDate);
    CheckEquals(lDate, c.DateField, 'Failed on 4');

    tiSetProperty(c, 'BoolField', True);
    CheckEquals(True, c.BoolField, 'Failed on 5');
  finally
    c.Free;
  end;
end;

procedure TTesttiRTTI.TestSetProperty_PropertyPath;
var
  c: TtiRTTITestClass;
  c1: TtiRTTITestClassB;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  c := TtiRTTITestClass.Create;
  try
    tiSetProperty(c, 'TestItem.StrField', 'Graeme');
    CheckEquals('Graeme', c.TestItem.StrField, 'Failed on 1');

    tiSetProperty(c, 'TestItem.IntField', 32);
    CheckEquals(32, c.TestItem.IntField, 'Failed on 2');

    tiSetProperty(c, 'TestItem.FloatField', 12.345);
    CheckEquals(12.345, c.TestItem.FloatField, 0.0001, 'Failed on 3');

    tiSetProperty(c, 'TestItem.DateField', lDate);
    CheckEquals(lDate, c.TestItem.DateField, 'Failed on 4');

    tiSetProperty(c, 'TestItem.BoolField', True);
    CheckEquals(True, c.TestItem.BoolField, 'Failed on 5');
  finally
    c.Free;
  end;
  
  c1 := TtiRTTITestClassB.Create;
  try
    tiSetProperty(c1, 'TestItemB.TestItem.StrField', 'Graeme');
    CheckEquals('Graeme', c1.TestItemB.TestItem.StrField, 'Failed on 1');

    tiSetProperty(c1, 'TestItemB.TestItem.IntField', 32);
    CheckEquals(32, c1.TestItemB.TestItem.IntField, 'Failed on 2');

    tiSetProperty(c1, 'TestItemB.TestItem.FloatField', 12.345);
    CheckEquals(12.345, c1.TestItemB.TestItem.FloatField, 0.0001, 'Failed on 3');

    tiSetProperty(c1, 'TestItemB.TestItem.DateField', lDate);
    CheckEquals(lDate, c1.TestItemB.TestItem.DateField, 'Failed on 4');

    tiSetProperty(c1, 'TestItemB.TestItem.BoolField', True);
    CheckEquals(True, c1.TestItemB.TestItem.BoolField, 'Failed on 5');
  finally
    c1.Free;
  end;
end;

procedure TTesttiRTTI.TestGetProperty_Simple;
var
  c: TtiOPFTestItem;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  c := TtiOPFTestItem.Create;
  try
    c.StrField    := 'Graeme';
    c.IntField    := 32;
    c.FloatField  := 12.345;
    c.DateField   := lDate;
    c.BoolField   := True;

    CheckEquals('Graeme', tiGetProperty(c, 'StrField'), 'Failed on 1');
    CheckEquals(32, tiGetProperty(c, 'IntField'), 'Failed on 2');
    CheckEquals(12.345, tiGetProperty(c, 'FloatField'), 0.0001, 'Failed on 3');
    CheckEquals(lDate, tiGetProperty(c, 'DateField'), 'Failed on 4');
    CheckEquals(True, tiGetProperty(c, 'BoolField'), 'Failed on 5');
  finally
    c.Free;
  end;
end;

procedure TTesttiRTTI.TestGetProperty_PropertyPath;
var
  c: TtiRTTITestClass;
  c1: TtiRTTITestClassB;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  c := TtiRTTITestClass.Create;
  try
    c.TestItem.StrField    := 'Graeme';
    c.TestItem.IntField    := 32;
    c.TestItem.FloatField  := 12.345;
    c.TestItem.DateField   := lDate;
    c.TestItem.BoolField   := True;

    CheckEquals('Graeme', tiGetProperty(c, 'TestItem.StrField'), 'Failed on 1');
    CheckEquals(32, tiGetProperty(c, 'TestItem.IntField'), 'Failed on 2');
    CheckEquals(12.345, tiGetProperty(c, 'TestItem.FloatField'), 0.0001, 'Failed on 3');
    CheckEquals(lDate, tiGetProperty(c, 'TestItem.DateField'), 'Failed on 4');
    CheckEquals(True, tiGetProperty(c, 'TestItem.BoolField'), 'Failed on 5');
  finally
    c.Free;
  end;
  
  c1 := TtiRTTITestClassB.Create;
  try
    c1.TestItemB.TestItem.StrField    := 'Graeme';
    c1.TestItemB.TestItem.IntField    := 32;
    c1.TestItemB.TestItem.FloatField  := 12.345;
    c1.TestItemB.TestItem.DateField   := lDate;
    c1.TestItemB.TestItem.BoolField   := True;

    CheckEquals('Graeme', tiGetProperty(c1, 'TestItemB.TestItem.StrField'), 'Failed on 1');
    CheckEquals(32, tiGetProperty(c1, 'TestItemB.TestItem.IntField'), 'Failed on 2');
    CheckEquals(12.345, tiGetProperty(c1, 'TestItemB.TestItem.FloatField'), 0.0001, 'Failed on 3');
    CheckEquals(lDate, tiGetProperty(c1, 'TestItemB.TestItem.DateField'), 'Failed on 4');
    CheckEquals(True, tiGetProperty(c1, 'TestItemB.TestItem.BoolField'), 'Failed on 5');
  finally
    c1.Free;
  end;
end;

{ TtiRTTITestClass }

constructor TtiRTTITestClass.Create;
begin
  inherited Create;
  FTestItem := TtiOPFTestItem.Create;
end;

destructor TtiRTTITestClass.Destroy;
begin
  FTestItem.Free;
  inherited Destroy;
end;

{ TtiRTTITestClassB }

constructor TtiRTTITestClassB.Create;
begin
  inherited Create;
  FTestItemB := TtiRTTITestClass.Create;
end;

destructor TtiRTTITestClassB.Destroy;
begin
  FTestItemB.Free;
  inherited Destroy;
end;

end.

