unit tiRTTI_TST;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiTestFramework
  ,tiObject
  ,tiBOMsForTesting
  ;

type

  TTesttiRTTI = class(TtiTestCase)
  private
    procedure TestGetPropertyException;
  published
    procedure tiGetSimplePropType;
    procedure tiVarSimplePropType;
    procedure tiIsNumericProp;
    procedure tiGetPropertyNamesObject;
    procedure tiGetPropertyNamesClass;
    procedure tiGetPropertyNamesEnum;
    procedure tiIsReadWritePropObject;
    procedure tiIsReadWritePropClass;
    procedure tiIsPublishedProp_Simple;
    procedure tiIsPublishedProp_PropertyPath;
    procedure SetProperty_Simple;
    procedure SetProperty_PropertyPath;
    procedure GetProperty_Simple;
    procedure GetProperty_PropertyPath;
    procedure GetPropertyClass;
    procedure PropertyInheritsFrom;
    procedure GetSetPropertyEnum;
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
  tiTestDependencies,
  tiRTTI,
  tiBaseObject,
  tiExcept,
  SysUtils,
  {$IFDEF DELPHI6ORABOVE}
  Variants,
  {$ENDIF}
  TypInfo;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTesttiRTTI);
end;

procedure TTesttiRTTI.SetProperty_Simple;
var
  LItem: TtiOPFTestItem;
  LDate: TDateTime;
begin
  LDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  LItem := TtiOPFTestItem.Create;
  try
    tiSetProperty(LItem, 'StrField', 'Graeme');
    CheckEquals('Graeme', LItem.StrField, 'StrField');

    tiSetProperty(LItem, 'IntField', 32);
    CheckEquals(32, LItem.IntField, 'IntField');

    tiSetProperty(LItem, 'FloatField', 12.345);
    CheckEquals(12.345, LItem.FloatField, 0.0001, 'FloatField');

    tiSetProperty(LItem, 'DateField', LDate);
    CheckEquals(LDate, LItem.DateField, 'DateField');

    tiSetProperty(LItem, 'BoolField', True);
    CheckEquals(True, LItem.BoolField, 'BoolField');

  finally
    LItem.Free;
  end;
end;

procedure TTesttiRTTI.TestGetPropertyException;
begin
  tiGetProperty(nil, 'BoolField');
end;

procedure TTesttiRTTI.tiGetPropertyNamesClass;
var
  lsl : TStringList;
begin
  lsl := TStringList.Create;
  try
    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [ tkLString {$IFDEF FPC},tkAString{$ENDIF}  {$IFDEF UNICODE} , tkUString {$ENDIF} ]);
    CheckEquals(3, lsl.Count, 'Failed on StringProp');
    CheckEquals('Caption',            lsl.Strings[0], 'Caption');
    CheckEquals('StringProp',         lsl.Strings[1], 'StringProp');
    CheckEquals('ReadOnlyStringProp', lsl.Strings[2], 'ReadOnlyStringProp');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [ tkString]);
    CheckEquals(2, lsl.Count, 'Failed on ShortStringProp');
    CheckEquals('ShortStringProp', lsl.Strings[0], 'Failed on ShortStringProp');
    CheckEquals('ReadOnlyShortStringProp', lsl.Strings[1], 'Failed on ReadOnlyShortStringProp');

{$IF defined(FPC) and defined(FPC_WIDESTRING_EQUAL_UNICODESTRING)}
    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [ tkUString ]);
{$ELSE}
    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [ tkWString ]);
{$IFEND}
    CheckEquals(2, lsl.Count, 'Failed on WideStringProp');
    CheckEquals('WideStringProp', lsl.Strings[0], 'Failed on WideStringProp');
    CheckEquals('ReadOnlyWideStringProp', lsl.Strings[1], 'Failed on ReadOnlyWideStringProp');

{$IFDEF UNICODE}
// char and wide char are the same...
      tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkWChar]);
      CheckEquals(4, lsl.Count, 'Failed on WideCharProp');
      CheckEquals('CharProp', lsl.Strings[0], 'Failed on WideCharProp');
      CheckEquals('WideCharProp', lsl.Strings[1], 'Failed on WideCharProp');
      CheckEquals('ReadOnlyCharProp', lsl.Strings[2], 'Failed on ReadOnlyCharProp');
      CheckEquals('ReadOnlyWideCharProp', lsl.Strings[3], 'Failed on ReadOnlyWideCharProp');

      tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkChar]);
      CheckEquals(0, lsl.Count, 'Failed on CharProp');
{$ELSE}
      tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkWChar]);
      CheckEquals(2, lsl.Count, 'Failed on WideCharProp');
      CheckEquals('WideCharProp', lsl.Strings[0], 'Failed on WideCharProp');
      CheckEquals('ReadOnlyWideCharProp', lsl.Strings[1], 'Failed on ReadOnlyWideCharProp');

      tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkChar]);
      CheckEquals(2, lsl.Count, 'Failed on CharProp');
      CheckEquals('CharProp', lsl.Strings[0], 'Failed on CharProp');
      CheckEquals('ReadOnlyCharProp', lsl.Strings[1], 'Failed on ReadOnlyCharProp');
{$ENDIF}

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, ctkString);
    CheckEquals(11, lsl.Count, 'Failed testing ctkString');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkInteger]);
    CheckEquals(2, lsl.Count, 'Failed on IntProp');
    CheckEquals('IntProp', lsl.Strings[0], 'Failed on IntProp');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkInt64]);
    CheckEquals(2, lsl.Count, 'Failed on Int64Prop');
    CheckEquals('Int64Prop', lsl.Strings[0], 'Failed on Int64Prop');

    { Delphi doesn't have this type defined }
    {$IFDEF FPC}
    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkBool]);
    CheckEquals(2, lsl.Count, 'Failed on tkBool');
    CheckEquals('BoolProp',          lsl.Strings[0], 'Failed on BoolProp');
    CheckEquals('ReadOnlyBoolProp',  lsl.Strings[1], 'Failed on ReadOnlyBoolProp');
    {$ENDIF}

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, ctkInt);
    {$IFDEF FPC}
    CheckEquals(6, lsl.Count, 'Failed testing ctkInt');
    {$ELSE}
    CheckEquals(4, lsl.Count, 'Failed testing ctkInt');
    {$ENDIF}

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkFloat]);
    CheckEquals(4, lsl.Count, 'Failed on tkFloatProp');
    CheckEquals('DateTimeProp', lsl.Strings[0], 'Failed on tkFloatProp');
    CheckEquals('FloatProp',    lsl.Strings[1], 'Failed on tkFloatProp');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, ctkFloat);
    CheckEquals(4, lsl.Count, 'Failed testing ctkFloat');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, ctkNumeric);
    CheckEquals(8, lsl.Count, 'Failed testing ctkNumeric');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, ctkSimple);
    {$IFDEF FPC}
    CheckEquals(21, lsl.Count, 'Failed testing ctkSimple');
    {$ELSE}
    CheckEquals(19, lsl.Count, 'Failed testing ctkSimple');
    {$ENDIF}

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkClass]);
    CheckEquals(2, lsl.Count, 'Failed on ObjectProp');
    CheckEquals('ObjectProp', lsl.Strings[0], 'Failed on ObjectProp');

    tiRTTI.tiGetPropertyNames(TTestGetPropNames, lsl, [tkMethod]);
    CheckEquals(2, lsl.Count, 'Failed on MethodProp');
    CheckEquals('MethodProp', lsl.Strings[0], 'Failed on MethodProp');
  finally
    lsl.Free;
  end;
end;

procedure TTesttiRTTI.tiGetPropertyNamesEnum;
var
  lsl : TStringList;
  lObj : TtiBaseObject;
begin
  lsl := TStringList.Create;
  try
    lObj := TTestGetPropNames.Create;
    try
      {$IFDEF FPC}
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkEnumeration ]);
      CheckEquals(2, lsl.Count, 'Failed on 1');
      CheckEquals('EnumProp', lsl.Strings[0], 'EnumProp');
      CheckEquals('ReadOnlyEnumProp', lsl.Strings[1], 'ReadOnlyEnumProp');
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkBool ]);
      CheckEquals(2, lsl.Count, 'Failed on 1');
      CheckEquals('BoolProp', lsl.Strings[0], 'BoolProp');
      CheckEquals('ReadOnlyBoolProp', lsl.Strings[1], 'ReadOnlyBoolProp');
      {$ELSE}
      // Delphi doesn't differentiate between Enum and Boolean types
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkEnumeration ]);
      CheckEquals(4, lsl.Count, 'Failed on 1');
      CheckEquals('BoolProp', lsl.Strings[0], 'BoolProp');
      CheckEquals('EnumProp', lsl.Strings[1], 'EnumProp');
      CheckEquals('ReadOnlyBoolProp', lsl.Strings[2], 'ReadOnlyBoolProp');
      CheckEquals('ReadOnlyEnumProp', lsl.Strings[3], 'ReadOnlyEnumProp');
      {$ENDIF}

      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkSet ]);
      CheckEquals(2, lsl.Count, 'Failed on 2');
      CheckEquals('EnumSetProp', lsl.Strings[0], 'EnumSetProp');
      CheckEquals('ReadOnlyEnumSetProp', lsl.Strings[1], 'ReadOnlyEnumSetProp');
    finally
      lObj.Free;
    end;
  finally
    lsl.Free;
  end;
end;

procedure TTesttiRTTI.tiGetPropertyNamesObject;
var
  lsl : TStringList;
  lObj : TtiBaseObject;
begin
  lsl := TStringList.Create;
  try
    lObj := TTestGetPropNames.Create;
    try
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkLString
          {$IFDEF FPC},tkAString{$ENDIF}
          {$IFDEF UNICODE},tkUString{$ENDIF} ]);
      CheckEquals(3, lsl.Count, 'Failed on StringProp');
      CheckEquals('Caption',            lsl.Strings[0], 'Caption');
      CheckEquals('StringProp',         lsl.Strings[1], 'StringProp');
      CheckEquals('ReadOnlyStringProp', lsl.Strings[2], 'ReadOnlyStringProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkString]);
      CheckEquals(2, lsl.Count, 'Failed on ShortStringProp');
      CheckEquals('ShortStringProp', lsl.Strings[0], 'Failed on ShortStringProp');
      CheckEquals('ReadOnlyShortStringProp', lsl.Strings[1], 'Failed on ReadOnlyShortStringProp');

{$IF defined(FPC) and defined(FPC_WIDESTRING_EQUAL_UNICODESTRING)}
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkUString ]);
{$ELSE}
      tiRTTI.tiGetPropertyNames(lObj, lsl, [ tkWString ]);
{$IFEND}
      CheckEquals(2, lsl.Count, 'Failed on WideStringProp');
      CheckEquals('WideStringProp', lsl.Strings[0], 'Failed on WideStringProp');
      CheckEquals('ReadOnlyWideStringProp', lsl.Strings[1], 'Failed on ReadOnlyWideStringProp');

{$IFDEF UNICODE}
// char and wide char are the same...
      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkWChar]);
      CheckEquals(4, lsl.Count, 'Failed on WideCharProp');
      CheckEquals('CharProp', lsl.Strings[0], 'Failed on WideCharProp');
      CheckEquals('WideCharProp', lsl.Strings[1], 'Failed on WideCharProp');
      CheckEquals('ReadOnlyCharProp', lsl.Strings[2], 'Failed on ReadOnlyCharProp');
      CheckEquals('ReadOnlyWideCharProp', lsl.Strings[3], 'Failed on ReadOnlyWideCharProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkChar]);
      CheckEquals(0, lsl.Count, 'Failed on CharProp');
{$ELSE}
      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkWChar]);
      CheckEquals(2, lsl.Count, 'Failed on WideCharProp');
      CheckEquals('WideCharProp', lsl.Strings[0], 'Failed on WideCharProp');
      CheckEquals('ReadOnlyWideCharProp', lsl.Strings[1], 'Failed on ReadOnlyWideCharProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkChar]);
      CheckEquals(2, lsl.Count, 'Failed on CharProp');
      CheckEquals('CharProp', lsl.Strings[0], 'Failed on CharProp');
      CheckEquals('ReadOnlyCharProp', lsl.Strings[1], 'Failed on ReadOnlyCharProp');
{$ENDIF}
      tiRTTI.tiGetPropertyNames(lObj, lsl, ctkString);
      CheckEquals(11, lsl.Count, 'Failed testing ctkString');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkInteger]);
      CheckEquals(2, lsl.Count, 'Failed on IntProp');
      CheckEquals('IntProp', lsl.Strings[0], 'Failed on IntProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkInt64]);
      CheckEquals(2, lsl.Count, 'Failed on Int64Prop');
      CheckEquals('Int64Prop', lsl.Strings[0], 'Failed on Int64Prop');

      { Delphi doesn't have this type defined! }
      {$IFDEF FPC}
      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkBool]);
      CheckEquals(2, lsl.Count, 'Failed on tkBool');
      CheckEquals('BoolProp',          lsl.Strings[0], 'Failed on BoolProp');
      CheckEquals('ReadOnlyBoolProp',  lsl.Strings[1], 'Failed on ReadOnlyBoolProp');
      {$ENDIF}

      tiRTTI.tiGetPropertyNames(lObj, lsl, ctkInt);
      {$IFDEF FPC}
      CheckEquals(6, lsl.Count, 'Failed testing ctkInt');
      {$ELSE}
      CheckEquals(4, lsl.Count, 'Failed testing ctkInt');
      {$ENDIF}

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkFloat]);
      CheckEquals(4, lsl.Count, 'Failed on tkFloatProp');
      CheckEquals('DateTimeProp', lsl.Strings[0], 'Failed on tkFloatProp');
      CheckEquals('FloatProp',    lsl.Strings[1], 'Failed on tkFloatProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, ctkFloat);
      CheckEquals(4, lsl.Count, 'Failed testing ctkFloat');

      tiRTTI.tiGetPropertyNames(lObj, lsl, ctkNumeric);
      CheckEquals(8, lsl.Count, 'Failed testing ctkNumeric');

      tiRTTI.tiGetPropertyNames(lObj, lsl, ctkSimple);
      {$IFDEF FPC}
      CheckEquals(21, lsl.Count, 'Failed testing ctkSimple');
      {$ELSE}
      CheckEquals(19, lsl.Count, 'Failed testing ctkSimple');
      {$ENDIF}

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkClass]);
      CheckEquals(2, lsl.Count, 'Failed on ObjectProp');
      CheckEquals('ObjectProp', lsl.Strings[0], 'Failed on ObjectProp');

      tiRTTI.tiGetPropertyNames(lObj, lsl, [tkMethod]);
      CheckEquals(2, lsl.Count, 'Failed on MethodProp');
      CheckEquals('MethodProp', lsl.Strings[0], 'Failed on MethodProp');
    finally
      lObj.Free;
    end;
  finally
    lsl.Free;
  end;
end;

procedure TTesttiRTTI.tiGetSimplePropType;
var
  lObj : TTestGetPropNames;
begin
  lObj := TTestGetPropNames.Create;
  try
    Check(tiRTTI.tiGetSimplePropType(lObj, 'StringProp')      = tiTKString, 'Failed on StringProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'ShortStringProp') = tiTKString, 'Failed on ShortStringProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'WideStringProp')  = tiTKString, 'Failed on WideStringProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'CharProp')        = tiTKString, 'Failed on CharProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'WideCharProp')    = tiTKString, 'Failed on WideCharProp');

    Check(tiRTTI.tiGetSimplePropType(lObj, 'IntProp')         = tiTKInteger, 'Failed on IntProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'Int64Prop')       = tiTKInteger, 'Failed on Int64Prop');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'BoolProp')        = tiTKBoolean, 'Failed on BoolProp');

    Check(tiRTTI.tiGetSimplePropType(lObj, 'FloatProp')       = tiTKFloat, 'Failed on FloatProp');
    Check(tiRTTI.tiGetSimplePropType(lObj, 'DateTimeProp')    = tiTKDateTime, 'Failed on DateTimeProp');

    try
      tiRTTI.tiGetSimplePropType(lObj, 'ObjectProp');
      Check(false, 'Failed on ObjectProp');
    except
      on e:exception do
        CheckIs(e, Exception, 'Failed on ObjectProp');
    end;

    try
      tiRTTI.tiGetSimplePropType(lObj, 'MethodProp');
      Check(false, 'Failed on MethodProp');
    except
      on e:exception do
        CheckIs(e, Exception, 'Failed on MethodProp');
    end;

    // Sub-properties
    Check(tiRTTI.tiGetSimplePropType(lObj, 'ObjectProp.StringProp') = tiTKString, 'Failed on ObjectProp.StringProp');
  finally
    lObj.Free;
  end;
end;

procedure TTesttiRTTI.tiIsNumericProp;
var
  lObj : TTestGetPropNames;
begin
  lObj := TTestGetPropNames.Create;
  try
    Check(not tiRTTI.tiIsNumericProp(lObj, 'StringProp'     ), 'Failed on StringProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'ShortStringProp'), 'Failed on ShortStringProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'WideStringProp' ), 'Failed on WideStringProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'CharProp'       ), 'Failed on CharProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'WideCharProp'   ), 'Failed on WideCharProp');
    Check(    tiRTTI.tiIsNumericProp(lObj, 'IntProp'        ), 'Failed on IntProp');
    Check(    tiRTTI.tiIsNumericProp(lObj, 'Int64Prop'      ), 'Failed on Int64Prop');
    Check(    tiRTTI.tiIsNumericProp(lObj, 'DateTimeProp'   ), 'Failed on DateTimeProp');
    Check(    tiRTTI.tiIsNumericProp(lObj, 'FloatProp'      ), 'Failed on FloatProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'ObjectProp'     ), 'Failed on ObjectProp');
    Check(not tiRTTI.tiIsNumericProp(lObj, 'MethodProp'     ), 'Failed on MethodProp');
  finally
    lObj.Free;
  end;
end;

procedure TTesttiRTTI.tiIsReadWritePropClass;
begin
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyStringProp'),      'Failed on ReadOnlyStringProp'      );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyShortStringProp'), 'Failed on ReadOnlyShortStringProp' );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyWideStringProp'),  'Failed on ReadOnlyWideStringProp ' );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyCharProp'),        'Failed on ReadOnlyCharProp'        );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyWideCharProp'),    'Failed on ReadOnlyWideCharProp'    );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyIntProp'),         'Failed on ReadOnlyIntProp'         );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyInt64Prop'),       'Failed on ReadOnlyInt64Prop'       );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyDateTimeProp'),    'Failed on ReadOnlyDateTimeProp'    );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyFloatProp'),       'Failed on ReadOnlyFloatProp'       );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyObjectProp'),      'Failed on ReadOnlyObjectProp'      );
  Check(not tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ReadOnlyMethodProp'),      'Failed on ReadOnlyMethodProp'      );

  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'StringProp'),  'Failed on OnlyStringProp'   );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ShortStringProp'), 'Failed on ShortStringProp'  );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'WideStringProp'),  'Failed on WideStringProp '  );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'CharProp'),        'Failed on CharProp'         );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'WideCharProp'),    'Failed on WideCharProp'     );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'IntProp'),         'Failed on IntProp'          );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'Int64Prop'),       'Failed on Int64Prop'        );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'DateTimeProp'),    'Failed on DateTimeProp'     );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'FloatProp'),       'Failed on FloatProp'        );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'ObjectProp'),      'Failed on ObjectProp'       );
  Check(tiRTTI.tiIsReadWriteProp(TTestGetPropNames, 'MethodProp'),      'Failed on MethodProp'       );

end;

procedure TTesttiRTTI.tiIsPublishedProp_Simple;
var
  c: TTestGetPropNames;
begin
  c := TTestGetPropNames.Create;
  try
    CheckTrue(tiIsPublishedProp(c, 'StringProp'), 'Failed on 1');
    CheckTrue(tiIsPublishedProp(c, 'ShortStringProp'), 'Failed on 2');
    CheckTrue(tiIsPublishedProp(c, 'WideStringProp'), 'Failed on 3');
    CheckTrue(tiIsPublishedProp(c, 'CharProp'), 'Failed on 4');
    CheckTrue(tiIsPublishedProp(c, 'IntProp'), 'Failed on 5');

    CheckTrue(tiIsPublishedProp(c, 'Int64Prop'), 'Failed on 6');
    CheckTrue(tiIsPublishedProp(c, 'BoolProp'), 'Failed on 7');
    CheckTrue(tiIsPublishedProp(c, 'DateTimeProp'), 'Failed on 8');
    CheckTrue(tiIsPublishedProp(c, 'FloatProp'), 'Failed on 9');
    CheckTrue(tiIsPublishedProp(c, 'ObjectProp'), 'Failed on 10');
    CheckTrue(tiIsPublishedProp(c, 'MethodProp'), 'Failed on 11');
    CheckTrue(tiIsPublishedProp(c, 'ReadOnlyIntProp'), 'Failed on 12');
  finally
    c.Free;
  end;
end;

procedure TTesttiRTTI.tiIsPublishedProp_PropertyPath;
var
  c: TtiRTTITestClass;
  s1: TtiOPFTestItemWithClassProp;
  s1BackupReference: TtiOPFTestItemWithClassProp;
begin
  c := TtiRTTITestClass.Create;
  try
    CheckTrue(tiIsPublishedProp(c, 'TestItem.StrField'), 'Failed on 1');
    CheckTrue(tiIsPublishedProp(c, 'TestItem.IntField'), 'Failed on 2');
    CheckTrue(tiIsPublishedProp(c, 'TestItem.FloatField'), 'Failed on 3');
    CheckTrue(tiIsPublishedProp(c, 'TestItem.DateField'), 'Failed on 4');
    CheckTrue(tiIsPublishedProp(c, 'TestItem.BoolField'), 'Failed on 5');
  finally
    c.Free;
  end;

  // Test deeper levels
  s1 := TtiOPFTestItemWithClassProp.Create;
  try
    CheckTrue(tiIsPublishedProp(s1, 'ClassField'), 'Failed on 6');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.StrField'), 'Failed on 7');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.IntField'), 'Failed on 8');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.FloatField'), 'Failed on 9');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.DateField'), 'Failed on 10');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.BoolField'), 'Failed on 11');

    // with instances
    s1BackupReference := TtiOPFTestItemWithClassProp.Create;
    s1.ClassField := s1BackupReference;
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.IntField'), 'Failed on 12');
    s1.ClassField.ClassField := TtiOPFTestItemWithClassProp.Create;
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.ClassField.FloatField'), 'Failed on 13');
    s1.ClassField.ClassField.ClassField := TtiOPFTestItemWithClassProp.Create;
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.ClassField.ClassField.DateField'), 'Failed on 14');

    // nil instances
    s1.ClassField := TtiOPFTestItemWithClassProp.Create; // overwriten previous reference - hence the backup
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.IntField'), 'Failed on 15');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.ClassField.FloatField'), 'Failed on 16');
    CheckTrue(tiIsPublishedProp(s1, 'ClassField.ClassField.ClassField.ClassField.DateField'), 'Failed on 17');
  finally
    s1BackupReference.Free;
    s1.Free;
  end;

end;

procedure TTesttiRTTI.tiIsReadWritePropObject;
var
  lObj : TTestGetPropNames;
begin
  lObj := TTestGetPropNames.Create;
  try
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyStringProp'),      'Failed on ReadOnlyStringProp'      );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyShortStringProp'), 'Failed on ReadOnlyShortStringProp' );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyWideStringProp'),  'Failed on ReadOnlyWideStringProp ' );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyCharProp'),        'Failed on ReadOnlyCharProp'        );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyWideCharProp'),    'Failed on ReadOnlyWideCharProp'    );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyIntProp'),         'Failed on ReadOnlyIntProp'         );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyInt64Prop'),       'Failed on ReadOnlyInt64Prop'       );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyBoolProp'),        'Failed on ReadOnlyBoolProp'        );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyDateTimeProp'),    'Failed on ReadOnlyDateTimeProp'    );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyFloatProp'),       'Failed on ReadOnlyFloatProp'       );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyObjectProp'),      'Failed on ReadOnlyObjectProp'      );
    Check(not tiRTTI.tiIsReadWriteProp(lObj, 'ReadOnlyMethodProp'),      'Failed on ReadOnlyMethodProp'      );

    Check(tiRTTI.tiIsReadWriteProp(lObj, 'StringProp'),  'Failed on OnlyStringProp'   );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'ShortStringProp'), 'Failed on ShortStringProp'  );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'WideStringProp'),  'Failed on WideStringProp '  );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'CharProp'),        'Failed on CharProp'         );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'WideCharProp'),    'Failed on WideCharProp'     );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'IntProp'),         'Failed on IntProp'          );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'Int64Prop'),       'Failed on Int64Prop'        );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'BoolProp'),        'Failed on BoolProp'         );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'DateTimeProp'),    'Failed on DateTimeProp'     );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'FloatProp'),       'Failed on FloatProp'        );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'ObjectProp'),      'Failed on ObjectProp'       );
    Check(tiRTTI.tiIsReadWriteProp(lObj, 'MethodProp'),      'Failed on MethodProp'       );
  finally
    lObj.Free;
  end;
end;

procedure TTesttiRTTI.tiVarSimplePropType;
begin
  Check(tiRTTI.tiVarSimplePropType('string') = tiTKString,   'Failed on tiTKString'  );
  Check(tiRTTI.tiVarSimplePropType(123     ) = tiTKInteger,  'Failed on tiTKInteger' );
  Check(tiRTTI.tiVarSimplePropType(true    ) = tiTKBoolean,  'Failed on tiTKBoolean' );
  Check(tiRTTI.tiVarSimplePropType(123.456 ) = tiTKFloat,    'Failed on tiTKFloat'   );
  Check(tiRTTI.tiVarSimplePropType(Now     ) = tiTKDateTime, 'Failed on tiTKDateTime');
end;

procedure TTesttiRTTI.SetProperty_PropertyPath;
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

procedure TTesttiRTTI.GetProperty_Simple;
var
  LItem: TtiOPFTestItem;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  LItem := TtiOPFTestItem.Create;
  try
    LItem.StrField    := 'Graeme';
    LItem.IntField    := 32;
    LItem.FloatField  := 12.345;
    LItem.DateField   := lDate;
    LItem.BoolField   := True;

    CheckEquals('Graeme', tiGetProperty(LItem, 'StrField'), 'Failed on 1');
    CheckEquals(32, tiGetProperty(LItem, 'IntField'), 'Failed on 2');
    CheckEquals(12.345, tiGetProperty(LItem, 'FloatField'), 0.0001, 'Failed on 3');
    CheckEquals(lDate, tiGetProperty(LItem, 'DateField'), 'Failed on 4');
    CheckEquals(True, tiGetProperty(LItem, 'BoolField'), 'Failed on 5');
    Check(VarIsNull(tiGetProperty(LItem, 'NonExistantProperty')));
    CheckException(TestGetPropertyException, EtiOPFDataException);

  finally
    LItem.Free;
  end;
end;

procedure TTesttiRTTI.GetProperty_PropertyPath;
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

procedure TTesttiRTTI.GetPropertyClass;
begin
  Check(TtiRTTITestClass = tiGetPropertyClass(TtiRTTITestClassB, 'TestItemB'), 'Failed on 1');
  Check(nil = tiGetPropertyClass(TtiRTTITestClassB, 'NotAProperty'), 'Failed on 2');
  Check(TtiOPFTestItem = tiGetPropertyClass(TtiRTTITestClassB, 'TestItemB.TestItem'), 'Failed on 3');
  Check(nil = tiGetPropertyClass(TtiRTTITestClassB, 'TestItemB.NotAProperty'), 'Failed on 4');
end;

procedure TTesttiRTTI.PropertyInheritsFrom;
begin
  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB', TtiRTTITestClass), 'Failed on 1');
  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB', TtiObject), 'Failed on 2');
  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB', TObject), 'Failed on 3');

  CheckFalse(tiPropertyInheritsFrom(TtiRTTITestClassB, 'NotAProperty', TObject), 'Failed on 4');
  CheckFalse(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB', TPersistent), 'Failed on 5');

  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB.TestItem', TtiOPFTestItem), 'Failed on 6');
  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB.TestItem', TtiObject), 'Failed on 7');
  CheckTrue(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB.TestItem', TObject), 'Failed on 8');

  CheckFalse(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB.TestItem', TtiRTTITestClass), 'Failed on 9');
  CheckFalse(tiPropertyInheritsFrom(TtiRTTITestClassB, 'TestItemB.NotAProperty', TObject), 'Failed on 10');
end;

procedure TTesttiRTTI.GetSetPropertyEnum;
var
  obj: TTestGetPropNames;
  s: String;
begin
  obj := TTestGetPropNames.Create;
  try
    s := tiGetProperty(obj, 'EnumProp');
    CheckTrue(s = 'enOne', 'Failed on 1');

    tiSetProperty(obj, 'EnumProp', enThree);
    s := tiGetProperty(obj, 'EnumProp');
    CheckTrue(s = 'enThree', 'Failed on 2');
  finally
    obj.Free;
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

