unit tiBaseMediator_TST;

{$I tiDefines.inc}

interface

uses
  TypInfo
  ,Classes
  ,SysUtils
  ,tiObject
  {$IFDEF FPC}
  ,testregistry
  {$ENDIF}
  ,tiTestFramework
  ,tiBaseMediator
  ;

type

  TTestTIBaseMediator = class(TtiTestCase)
    FTestA: TtiObject;
    FTestB: TtiObject;
    FTestC: TtiObject;
    FTestD: TtiObject;
    FTestE: TtiObject;
    FComponentA: TComponent;
    FComponentB: TComponent;
    FComponentC: TComponent;
    FComponentD: TComponent;
  private
    procedure DoTestFindError;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function RegisterMediatorA(ASubjectClass: TSubjectClass; APropName: string): TMediatorDef;
    function RegisterMediatorB(ASubjectClass: TSubjectClass; APropName: string): TMediatorDef;
  published
    procedure TestCreate;
    procedure TestAdd1;
    procedure TestAdd2;
    procedure TestAdd3;
    procedure TestAdd4;
    procedure TestAdd5;
    procedure TTestHandles1;
    procedure TTestHandles2;
    procedure TTestHandles3;
    procedure TBetterHandle1;
    procedure TBetterHandle2;
    procedure TBetterHandle3;
    procedure TBetterHandle4;
    procedure TestFind1;
    procedure TestFind2;
    procedure TestFind3;
    procedure TestFind4;
    procedure TestFind5;
    procedure TestFind6;
    procedure TestFind7;
    procedure TestFind8;
  end;


procedure RegisterTests;


implementation

uses
  tiTestDependencies;

type
  // Auxiliary components

  {
  Subject tree

  TTiObject
    +--- TTestSubject
    |      +--- TTestSubjectA
    |      |      +--- TTestSubjectD
    |      |
    |      +--- TTestSubjectB
    |             +--- TTestSubjectE
    +--- TTestSubjectC
}

  TTestSubject = class(TtiObject)
  private
    FAsBoolean: Boolean;
    FAsFloat: double;
    FAsInteger: integer;
    FAsString: string;
  published
    property AsInteger: integer read FAsInteger write FAsInteger;
    property AsString: string read FAsString write FAsString;
    property AsFloat: double read FAsFloat write FAsFloat;
    property AsBoolean: Boolean read FAsBoolean write FAsBoolean;
  end;


  TTestSubjectA = class(TTestSubject);
  TTestSubjectB = class(TTestSubject);
  TTestSubjectD = class(TTestSubjectA);
  TTestSubjectE = class(TTestSubjectB);


  TTestSubjectC = class(TtiObject)
  private
    FAsBoolean: Boolean;
    FAsFloat: double;
    FAsInteger: integer;
    FAsString: string;
  published
    property AsInteger: integer read FAsInteger write FAsInteger;
    property AsString: string read FAsString write FAsString;
    property AsFloat: double read FAsFloat write FAsFloat;
    property AsBoolean: Boolean read FAsBoolean write FAsBoolean;
  end;

  {
  "GUI" Tree

  TTestComponent
    +--- TComponentA
    |      +--- TComponentD
    +--- TComponentB
    +--- TComponentC
}

  TTestComponent = class(TComponent)
  private
    FAsBoolean: Boolean;
    FAsFloat: double;
    FAsInteger: integer;
    FAsString: string;
  published
    property AsInteger: integer read FAsInteger write FAsInteger;
    property AsString: string read FAsString write FAsString;
    property AsFloat: double read FAsFloat write FAsFloat;
    property AsBoolean: Boolean read FAsBoolean write FAsBoolean;
  end;


  TComponentA = class(TTestComponent);
  TComponentB = class(TTestComponent);
  TComponentC = class(TTestComponent);
  TComponentD = class(TComponentA);

  {
  Mediator Tree ("GUI" ComponentClass)

  TTestMediator (TTestComponent)
    +--- TTestMediatorA (TComponentA)
    |      +--- TTestMediatorC (TComponentC)
    +--- TTestMediatorB (TComponentB)
    |      +--- TTestMediatorD (TComponentD)
    +--- TTestMediatorE (TComponentE)
    +--- TTestMediatorComposite (TComponentA)
}

  TTestMediator = class(TMediatorView)
    FGuiControl: TTestComponent;
  protected
    function GetGuiControl: TComponent; override;
    procedure SetGuiControl(const AValue: TComponent); override;
  public
    class function ComponentClass: TClass; override;
  end;


  TTestMediatorA = class(TTestMediator)
    class function ComponentClass: TClass; override;
  end;


  TTestMediatorB = class(TTestMediator)
    class function ComponentClass: TClass; override;
  end;


  TTestMediatorC = class(TTestMediatorA)
  end;


  TTestMediatorD = class(TTestMediatorB)
    class function ComponentClass: TClass; override;
  end;


  TTestMediatorE = class(TTestMediator)
    class function ComponentClass: TClass; override;
  end;


  TTestMediatorComposite = class(TTestMediator)
    class function CompositeMediator: Boolean; override;
    class function ComponentClass: TClass; override;
  end;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIBaseMediator);
end;

{ TTestMediatorComposite }

class function TTestMediatorComposite.CompositeMediator: Boolean;
begin
  Result := True;
end;

class function TTestMediatorComposite.ComponentClass: TClass;
begin
  Result := TComponentA;
end;

{ TTestMediatorD }

class function TTestMediatorD.ComponentClass: TClass;
begin
  Result := TComponentD;
end;

{ TTestMediatorE }

class function TTestMediatorE.ComponentClass: TClass;
begin
  Result := TComponentC;
end;

{ TTestMediatorB }

class function TTestMediatorB.ComponentClass: TClass;
begin
  Result := TComponentB;
end;

{ TTestMediator }

function TTestMediator.GetGuiControl: TComponent;
begin
  Result := FGuiControl;
end;

procedure TTestMediator.SetGuiControl(const AValue: TComponent);
begin
  FGuiControl := AValue as TTestComponent;
end;

class function TTestMediator.ComponentClass: TClass;
begin
  Result := TTestComponent;
end;


{ TTestMediatorA }

class function TTestMediatorA.ComponentClass: TClass;
begin
  Result := TComponentA;
end;


{ TTestTIBaseMediator }

procedure TTestTIBaseMediator.SetUp;
begin
  inherited;
  CheckNotNull(gMediatorManager, 'Global manager assigned');
  gMediatorManager.Defs.Clear;
  CheckEquals(0, gMediatorManager.Defs.Count, 'Global manager empty');
  FTestA      := TTestSubjectA.Create;
  FTestB      := TTestSubjectB.Create;
  FTestC      := TTestSubjectC.Create;
  FTestD      := TTestSubjectD.Create;
  FTestE      := TTestSubjectE.Create;
  FComponentA := TComponentA.Create(nil);
  FComponentB := TComponentB.Create(nil);
  FComponentC := TComponentC.Create(nil);
  FComponentD := TComponentD.Create(nil);
end;

procedure TTestTIBaseMediator.TearDown;
begin
  FreeAndNil(FTestA);
  FreeAndNil(FTestB);
  FreeAndNil(FComponentA);
  FreeAndNil(FComponentB);
  FreeAndNil(FComponentC);
  FreeAndNil(FComponentD);
  inherited;
end;

function TTestTIBaseMediator.RegisterMediatorA(ASubjectClass: TSubjectClass; APropName: string): TMediatorDef;
begin
  Result := gMediatorManager.RegisterMediator(TTestMediatorA, ASubjectClass, APropName);
end;

function TTestTIBaseMediator.RegisterMediatorB(ASubjectClass: TSubjectClass; APropName: string): TMediatorDef;
begin
  Result := gMediatorManager.RegisterMediator(TTestMediatorB, ASubjectClass, APropName);
end;

procedure TTestTIBaseMediator.TestCreate;
begin
  CheckNotNull(gMediatorManager, 'Global manager assigned');
  CheckEquals(0, gMediatorManager.Defs.Count, 'Global manager empty');
end;

procedure TTestTIBaseMediator.TestAdd1;
var
  MR, M: TMediatorDef;
begin
  MR := RegisterMediatorA(TTestSubjectA, 'AsInteger');
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M  := gMediatorManager.Defs[0];
  CheckSame(MR, M, 'Mediator at correct position');
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TComponentA, M.MediatorClass.ComponentClass, 'Mediator component class correct');
  CheckEquals(TTestSubjectA, M.MinSubjectClass, 'Min subject class correct');
  if (M.PropertyTypes <> []) then
    Fail('PropertyTypes must be empty when property name registered');
  CheckEquals('AsInteger', M.PropertyName, 'Property name correct');
end;

procedure TTestTIBaseMediator.TestAdd2;
var
  M: TMediatorDef;
  p: PPropInfo;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, [tkInteger]);
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M := gMediatorManager.Defs[0];
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectA, M.MinSubjectClass, 'Min subject class correct');
  if (M.PropertyTypes <> [tkInteger]) then
  begin
    p := GetPropInfo(M, 'PropertyTypes');
    Fail('PropertyTypes does not match tkInteger: ' + SetToString(p, Integer(M.PropertyTypes), True));
  end;    
  CheckEquals('', M.PropertyName, 'Property name empty');
end;

procedure TTestTIBaseMediator.TestAdd3;
const
  Props = tkProperties - [tkClass, tkInterface, tkDynArray {$IFDEF FPC}, tkObject, tkInterfaceRaw{$ENDIF}];
var
  M: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA);
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M := gMediatorManager.Defs[0];
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectA, M.MinSubjectClass, 'Min subject class correct');
  if (M.PropertyTypes <> Props) then
    Fail('PropertyTypes does not match tkProperties-[tkClass,tkObject,tkInterface,tkDynArray,tkInterfaceRaw]');
  CheckEquals('', M.PropertyName, 'Property name empty');
end;

procedure TTestTIBaseMediator.TestAdd4;
var
  M, MR1, MR2: TMediatorDef;
begin
  MR1 := RegisterMediatorA(TTestSubjectA, 'AsInteger');
  MR2 := RegisterMediatorA(TTestSubjectB, 'AsString');
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M   := gMediatorManager.Defs[0];
  CheckSame(MR1, M, 'Mediator 1 at correct position');
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectA, M.MinSubjectClass, 'Min subject class correct');
  CheckEquals('AsInteger', M.PropertyName, 'Property name empty');
  M := gMediatorManager.Defs[1];
  CheckSame(MR2, M, 'Mediator 2 at correct position');
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectB, M.MinSubjectClass, 'Min subject class correct');
  CheckEquals('AsString', M.PropertyName, 'Property name empty');
end;

procedure TTestTIBaseMediator.TestAdd5;
var
  M: TMediatorDef;
begin
  RegisterMediatorA(TTestSubjectA, 'AsInteger');
  RegisterMediatorB(TTestSubjectB, 'AsString');
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M := gMediatorManager.Defs[0];
  CheckEquals(TTestMediatorA, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectA, M.MinSubjectClass, 'Min subject class correct');
  CheckEquals('AsInteger', M.PropertyName, 'Property name empty');
  M := gMediatorManager.Defs[1];
  CheckEquals(TTestMediatorB, M.MediatorClass, 'Mediator class correct');
  CheckEquals(TTestSubjectB, M.MinSubjectClass, 'Min subject class correct');
  CheckEquals('AsString', M.PropertyName, 'Property name empty');
end;

procedure TTestTIBaseMediator.TTestHandles1;
var
  M: TMediatorDef;
begin
  RegisterMediatorA(TTestSubjectA, 'AsInteger');
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M := gMediatorManager.Defs[0];
  // Test handling of subject and property
  CheckEquals(True, M.Handles(FTestA, FComponentA, GetPropInfo(FTestA, 'AsInteger')), 'Mediator handles A');
  CheckEquals(True, M.Handles(FTestD, FComponentA, GetPropInfo(FTestD, 'AsInteger')), 'Mediator handles D');
  CheckEquals(False, M.Handles(FTestE, FComponentA, GetPropInfo(FTestE, 'AsInteger')), 'Mediator does not handle E');
  CheckEquals(False, M.Handles(FTestA, FComponentA, GetPropInfo(FTestA, 'AsString')), 'Mediator does not handle asstring');
end;

procedure TTestTIBaseMediator.TTestHandles2;
var
  M: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, [tkInteger]);
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M := gMediatorManager.Defs[0];
  // Test handling of component and property
  CheckEquals(True, M.Handles(FTestA, FComponentA, GetPropInfo(FTestA, 'AsInteger')), 'Mediator handles A.AsInteger');
  CheckEquals(False, M.Handles(FTestA, FComponentA, GetPropInfo(FTestA, 'AsString')), 'Mediator does not handle A.AsString');
end;

procedure TTestTIBaseMediator.TTestHandles3;
var
  M: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, [tkInteger]);
  CheckEquals(1, gMediatorManager.Defs.Count, 'Global manager has 1 definition');
  M := gMediatorManager.Defs[0];
  // Test handling of descendent component and property
  CheckEquals(True, M.Handles(FTestA, FComponentD, GetPropInfo(FTestA, 'AsInteger')), 'Mediator handles TComponentD.AsInteger');
end;

procedure TTestTIBaseMediator.TBetterHandle1;
var
  M1, M2: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubject, 'AsInteger');
  gMediatorManager.RegisterMediator(TTestMediatorB, TTestSubject, [tkInteger, tkString]);
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M1 := gMediatorManager.Defs[0];
  M2 := gMediatorManager.Defs[1];
  CheckEquals(True, M1.BetterMatch(nil), 'Instance has always precedence over nil');
  CheckEquals(True, M1.BetterMatch(M2), 'Property name has precedence over types');
  CheckEquals(False, M2.BetterMatch(M1), 'Property name has precedence over types');
end;

procedure TTestTIBaseMediator.TBetterHandle2;
var
  M1, M2: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsInteger');
  gMediatorManager.RegisterMediator(TTestMediatorD, TTestSubjectA, 'AsInteger');
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M1 := gMediatorManager.Defs[0];
  M2 := gMediatorManager.Defs[1];
  CheckEquals(True, M2.BetterMatch(M1), 'Closer GUI class match takes precedence');
  CheckEquals(False, M1.BetterMatch(M2), 'Closer GUI class match takes precedence');
end;

procedure TTestTIBaseMediator.TBetterHandle3;
var
  M1, M2: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsInteger');
  gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectD, 'AsInteger');
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M1 := gMediatorManager.Defs[0];
  M2 := gMediatorManager.Defs[1];
  CheckEquals(True, M2.BetterMatch(M1), 'Closer subject class match takes precedence');
end;

procedure TTestTIBaseMediator.TBetterHandle4;
var
  M1, M2: TMediatorDef;
begin
  gMediatorManager.RegisterMediator(TTestMediatorD, TTestSubjectA, 'AsInteger');
  gMediatorManager.RegisterMediator(TTestMediatorB, TTestSubjectD, 'AsInteger');
  CheckEquals(2, gMediatorManager.Defs.Count, 'Global manager has 2 definitions');
  M1 := gMediatorManager.Defs[0];
  M2 := gMediatorManager.Defs[1];
  CheckEquals(True, M2.BetterMatch(M1), 'Closer subject class match takes precedence');
end;

procedure TTestTIBaseMediator.TestFind1;
var
  M, M1, M2: TMediatorDef;
begin
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubjectA, 'AsInteger');
  M2 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsInteger');
  M  := gMediatorManager.FindDefFor(FTestA, FComponentA, 'AsInteger');
  CheckSame(M2, M, 'Correct closest mediator found');
end;

procedure TTestTIBaseMediator.TestFind2;
var
  M, M1, M2, M3: TMediatorDef;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M2 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubjectA, 'AsInteger');
  M3 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsInteger');
  M  := gMediatorManager.FindDefFor(FTestA, FComponentA, 'AsInteger');
  CheckSame(M3, M, 'Correct closest mediator found');
end;

procedure TTestTIBaseMediator.TestFind3;
var
  M, M1, M2, M3: TMediatorDef;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M2 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubjectB, 'AsInteger');
  M3 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsInteger');
  M  := gMediatorManager.FindDefFor(FTestA, FComponentB, 'AsInteger');
  CheckSame(M1, M, 'Correct closest mediator found');
end;

procedure TTestTIBaseMediator.TestFind4;
var
  M, M1, M2, M3: TMediatorDef;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M3 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsString');
  M  := gMediatorManager.FindDefFor(FTestA, FComponentB, 'AsInteger');
  CheckSame(M1, M, 'Correct closest mediator found');
end;

procedure TTestTIBaseMediator.DoTestFindError;
begin
  gMediatorManager.FindDefFor(FTestA, FComponentB, 'BlaBla');
end;

procedure TTestTIBaseMediator.TestFind5;
var
  M1, M2: TMediatorDef;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M2 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsString');
  try
    DoTestFindError;
    //  AssertException('Wrong property name',EPropertyError,@DoTestFindError);
    Fail('Failed on 1');
  except
    on e: Exception do
      CheckIs(e, EPropertyError, 'Wrong property name');
  end;

end;

procedure TTestTIBaseMediator.TestFind6;
var
  M1, M2: TMediatorDef;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M2 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubjectA, 'AsString');
  CheckNull(gMediatorManager.FindDefFor(FTestA, FComponentB, PPropInfo(nil)));
end;

procedure TTestTIBaseMediator.TestFind7;
var
  M1, M: TMediatorDef;
begin
  // Specifics
  M1 := gMediatorManager.RegisterMediator(TTestMediatorA, TTestSubject, 'AsString');
  M  := gMediatorManager.FindDefFor(FTestC, FComponentA, 'AsString');
  CheckNull(M, 'No mediator found, no matching test class');
end;

procedure TTestTIBaseMediator.TestFind8;
var
  M1, M2, Mr: TMediatorDef;
  L: TtiObjectList;
begin
  // Catchall
  M1 := gMediatorManager.RegisterMediator(TTestMediator, TTestSubject);
  // Specifics
  M2 := gMediatorManager.RegisterMediator(TTestMediatorComposite, TtiObjectList);
  L  := TtiObjectList.Create;
  try
    Mr := gMediatorManager.FindDefFor(L, FComponentA, PPropInfo(nil));
  finally
    L.Free;
  end;
  CheckNotNull(Mr, 'No mediator');
  CheckEquals(TTestMediatorComposite, Mr.MediatorClass);
end;


end.

