unit tiBOMsForTesting;

{$I tiDefines.inc}

interface

uses
  tiObject,
  Classes,
  tiOid,
  tiFilteredObjectList;

type

  TtiObjectByPropertyForTestingAbs = class(TtiObject)
  public
    procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
  end;

  TtiOPFTestStringProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FStrField: string;
  published
    property StrField: string read FStrField write FStrField;
  end;

  TtiOPFTestOIdProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FOIDField: TtiOID;
    procedure SetOIDField(const Value: TtiOID);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property OIDField: TtiOID read FOIDField write SetOIDField;
  end;

  TtiOPFTestNotesProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FNotesField: string;
  published
    property NotesField: string read FNotesField write FNotesField;
  end;


  TtiOPFTestIntegerProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FIntField: integer;
  published
    property IntField: integer read FIntField write FIntField;
  end;

{$IFDEF TESTINT64}
  TtiOPFTestInt64Prop = class(TtiObjectByPropertyForTestingAbs)
  private
    FInt64Field: int64;
    function GetInt64Field: int64;
    procedure SetInt64Field(const AValue: int64);
  published
    property Int64Field: int64 read GetInt64Field write SetInt64Field;
  end;
{$ENDIF}

  TtiOPFTestFloatProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FFloatField: extended;
  published
    property FloatField: extended read FFloatField write FFloatField;
  end;


  TtiOPFTestDateTimeProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FDateField: TDateTime;
  published
    property DateField: TDateTime read FDateField write FDateField;
  end;


  TtiOPFTestBooleanProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FBoolField: boolean;
  published
    property BoolField: boolean read FBoolField write FBoolField;
  end;


  TtiOPFTestStreamProp = class(TtiObjectByPropertyForTestingAbs)
  private
    FStream: TStream;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property StreamField: TStream read FStream write FStream;
  end;

  TtiObjectListForTesting  = class;
  TtiObjectListNestedForTesting = class;
  TtiOPFTestItem  = class;

  TtiObjectListForTesting = class(TtiFilteredObjectList)  // used for Criteria tests
  private
    function GetOIDAsInteger: integer;
  protected
    function GetItems(i: integer): TtiObjectListNestedForTesting; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiObjectListNestedForTesting); reintroduce;
  public
    property Items[i: integer]: TtiObjectListNestedForTesting read GetItems write SetItems;
    procedure Add(AObject: TtiObjectListNestedForTesting); reintroduce;
    procedure Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
  published
    property OIDAsInteger: integer read GetOIDAsInteger;
  end;

  TtiObjectListNestedForTesting = class(TtiFilteredObjThreadList)  // used for Criteria tests
  private
    FIntField:   integer;
    FFloatField: extended;
    FStrField:   string;
    FDateField:  TDateTime;
    FBoolField:  boolean;
    FNotesField: string;
    function GetOIDAsInteger: integer;
  protected
    function GetOwner: TtiObjectListForTesting; reintroduce;
    procedure SetOwner(const AValue: TtiObjectListForTesting); reintroduce;
    function GetItems(i: integer): TtiOPFTestItem; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiOPFTestItem); reintroduce;
  public
    property Items[i: integer]: TtiOPFTestItem read GetItems write SetItems;
    procedure Add(AObject: TtiOPFTestItem); reintroduce;
    procedure Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
  published
    property OIDAsInteger: integer read GetOIDAsInteger;
    property StrField: string read FStrField write FStrField;
    property IntField: integer read FIntField write FIntField;
    property FloatField: extended read FFloatField write FFloatField;
    property DateField: TDateTime read FDateField write FDateField;
    property BoolField: boolean read FBoolField write FBoolField;
    property NotesField: string read FNotesField write FNotesField;
  end;


  TtiOPFTestItem = class(TtiObject)
  private
    FIntField:   integer;
    FFloatField: extended;
    FStrField:   string;
    FDateField:  TDateTime;
    FBoolField:  boolean;
    FNotesField: string;
    function GetOIDAsInteger: integer;
  protected
    function GetOwner: TtiObjectListNestedForTesting; reintroduce;
    procedure SetOwner(const AValue: TtiObjectListNestedForTesting); reintroduce;
  public
    property Owner: TtiObjectListNestedForTesting read GetOwner write SetOwner;
  published
    property OIDAsInteger: integer read GetOIDAsInteger;
    property StrField: string read FStrField write FStrField;
    property IntField: integer read FIntField write FIntField;
    property FloatField: extended read FFloatField write FFloatField;
    property DateField: TDateTime read FDateField write FDateField;
    property BoolField: boolean read FBoolField write FBoolField;
    property NotesField: string read FNotesField write FNotesField;
  end;


  TtiObjectParentForTesting = class;

  TtiObjectParentForTestingAbs = class(TtiObject)
  private
    FStrField:   string;
    FIntField:   integer;
    FFloatField: extended;
    FDateField:  TDateTime;
  public
    procedure   ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    //procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
  published
    property StrField: string read FStrField write FStrField;
    property IntField: integer read FIntField write FIntField;
    property FloatField: extended read FFloatField write FFloatField;
    property DateField: TDateTime read FDateField write FDateField;
  end;


  TtiObjectParentForTesting = class(TtiObjectParentForTestingAbs);
  TtiObjectChildForTestingA = class(TtiObjectParentForTesting);
  TtiObjectChildForTestingB = class(TtiObjectParentForTesting);

  TtiObjectParentForTestingGrouped = class(TtiObjectParentForTestingAbs);
  TtiOPFTestChildGrouped_A = class(TtiObjectParentForTestingGrouped);
  TtiOPFTestChildGrouped_B = class(TtiObjectParentForTestingGrouped);

  TtiObjectParentForTestingGroup = class(TtiObjectList)
  private
  protected
    function GetItems(i: integer): TtiObjectParentForTestingGrouped; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiObjectParentForTestingGrouped); reintroduce;
  public
    //procedure   ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    property Items[i: integer]: TtiObjectParentForTestingGrouped read GetItems write SetItems;
    procedure Add(AObject: TtiObjectParentForTestingGrouped; ADefDispOrdr: boolean = True); reintroduce;
  published
  end;


  TTestGetPropNamesAbs = class(TtiObject)
  private
    FStringProp:     string;
    FCharProp:       char;
    FShortStringStringProp: ShortString;
    FWideStringProp: WideString;
    FWideCharProp:   widechar;
    FInt64Prop:      int64;
    FIntProp:        integer;
    FFloatProp:      extended;
    FMethodProp:     TNotifyEvent;
    FObjectProp:     TObject;
    FDateTimeProp:   TDateTime;
    FBoolProp:       boolean;
  protected
    property StringProp: string read FStringProp write FStringProp;
    property ShortStringProp: ShortString read FShortStringStringProp write FShortStringStringProp;
    property WideStringProp: WideString read FWideStringProp write FWideStringProp;
    property CharProp: char read FCharProp write FCharProp;
    property WideCharProp: widechar read FWideCharProp write FWideCharProp;
    property IntProp: integer read FIntProp write FIntProp;
    property Int64Prop: int64 read FInt64Prop write FInt64Prop;
    property BoolProp: boolean read FBoolProp write FBoolProp;
    property DateTimeProp: TDateTime read FDateTimeProp write FDateTimeProp;
    property FloatProp: extended read FFloatProp write FFloatProp;

    property ObjectProp: TObject read FObjectProp write FObjectProp;
    property MethodProp: TNotifyEvent read FMethodProp write FMethodProp;
    // These are the leftovers
    // tkUnknown, tkEnumeration, tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

    property ReadOnlyStringProp: string read FStringProp;
    property ReadOnlyShortStringProp: ShortString read FShortStringStringProp;
    property ReadOnlyWideStringProp: WideString read FWideStringProp;
    property ReadOnlyCharProp: char read FCharProp;
    property ReadOnlyWideCharProp: widechar read FWideCharProp;
    property ReadOnlyIntProp: integer read FIntProp;
    property ReadOnlyInt64Prop: int64 read FInt64Prop;
    property ReadOnlyBoolProp: boolean read FBoolProp;
    property ReadOnlyDateTimeProp: TDateTime read FDateTimeProp;
    property ReadOnlyFloatProp: extended read FFloatProp;
    property ReadOnlyObjectProp: TObject read FObjectProp;
    property ReadOnlyMethodProp: TNotifyEvent read FMethodProp;
  end;


  TTestListToOutput = class(TTestGetPropNamesAbs)
  protected
    function GetCaption: string; override;
  published
    property StringProp;
    property IntProp;
    property DateTimeProp;
    property FloatProp;
  end;


  TTestListOfPersistents = class(TtiObjectList)
  public
    constructor Create; override;
    function AsString: string; overload;
    function AsString(const AFieldDelim: string; const ARowDelim: string; AFields: TStringList): string; overload;
  end;


  TTestGetPropNames = class(TTestGetPropNamesAbs)
  published
    property StringProp;
    property ShortStringProp;
    property WideStringProp;
    property CharProp;
    property WideCharProp;
    property IntProp;
    property Int64Prop;
    property BoolProp;
    property DateTimeProp;
    property FloatProp;
    property ObjectProp;
    property MethodProp;
    property ReadOnlyStringProp;
    property ReadOnlyShortStringProp;
    property ReadOnlyWideStringProp;
    property ReadOnlyCharProp;
    property ReadOnlyWideCharProp;
    property ReadOnlyIntProp;
    property ReadOnlyInt64Prop;
    property ReadOnlyBoolProp;
    property ReadOnlyDateTimeProp;
    property ReadOnlyFloatProp;
    property ReadOnlyObjectProp;
    property ReadOnlyMethodProp;
  end;


procedure RegisterMappings;


implementation

uses
  tiAutoMap
  ,
  tiOPFManager
  ,
  SysUtils
  ,
  tiUtils
  ,
  tiTestFramework
  ,
  tiConstants;

procedure RegisterMappings;
begin
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestStringProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestStringProp, cTIQueryTableName, 'StrField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestOIdProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestOIdProp, cTIQueryTableName, 'OIDField', cTIQueryColName);


  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestNotesProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestNotesProp, cTIQueryTableName, 'NotesField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestIntegerProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestIntegerProp, cTIQueryTableName, 'IntField', cTIQueryColName);

{$IFDEF TESTINT64}
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestInt64Prop, cTIQueryTableNameInt64, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestInt64Prop, cTIQueryTableNameInt64, 'Int64Field', cTIQueryColName);
{$ENDIF}
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestFloatProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestFloatProp, cTIQueryTableName, 'FloatField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestDateTimeProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestDateTimeProp, cTIQueryTableName, 'DateField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestBooleanProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestBooleanProp, cTIQueryTableName, 'BoolField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestStreamProp, cTIQueryTableName, 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestStreamProp, cTIQueryTableName, 'StreamField', cTIQueryColName);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'StrField', 'Group_Str_Field', [pktReadable]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'IntField', 'Group_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'FloatField', 'Group_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'DateField', 'Group_Date_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'BoolField', 'Group_Bool_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectListNestedForTesting, 'Test_Group', 'NotesField', 'Group_Notes_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiObjectListForTesting, TtiObjectListNestedForTesting);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'Owner.OID', 'OID_Group', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'StrField', 'Item_Str_Field', [pktReadable]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'IntField', 'Item_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'FloatField', 'Item_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'DateField', 'Item_Date_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'BoolField', 'Item_Bool_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestItem, 'Test_Item', 'NotesField', 'Item_Notes_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiObjectListNestedForTesting, TtiOPFTestItem);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTesting, 'Test_Parent', 'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTesting, 'Test_Parent', 'StrField', 'Parent_Str_Field');

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingA, cTableNameTIOPFTestChild_A, 'OID', 'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingA, cTableNameTIOPFTestChild_A, 'IntField', 'Child_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingA, cTableNameTIOPFTestChild_A,
    'FloatField', 'Child_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TtiObjectParentForTesting, TtiObjectChildForTestingA);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingB, cTableNameTIOPFTestChild_B, 'OID', 'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingB, cTableNameTIOPFTestChild_B, 'IntField', 'Child_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectChildForTestingB, cTableNameTIOPFTestChild_B,
    'FloatField', 'Child_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TtiObjectParentForTesting, TtiObjectChildForTestingB);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTestingGroup, cTableNameTIOPFTestParentGroup, 'OID', 'OID', [pktDB]);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTestingGrouped, cTableNameTIOPFTestParentGrouped,
    'OID', 'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTestingGrouped, cTableNameTIOPFTestParentGrouped,
    'Owner.OID', 'Owner_OID', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiObjectParentForTestingGrouped, cTableNameTIOPFTestParentGrouped,
    'StrField', 'Parent_Str_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiObjectParentForTestingGroup, TtiObjectParentForTestingGrouped);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_A, cTableNameTIOPFTestChildGrouped_A,
    'OID', 'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_A, cTableNameTIOPFTestChildGrouped_A,
    'IntField', 'Child_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_A, cTableNameTIOPFTestChildGrouped_A,
    'FloatField', 'Child_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TtiObjectParentForTestingGrouped, TtiOPFTestChildGrouped_A);
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiObjectParentForTestingGroup, TtiOPFTestChildGrouped_A);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_B, cTableNameTIOPFTestChildGrouped_B,
    'OID', 'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_B, cTableNameTIOPFTestChildGrouped_B,
    'IntField', 'Child_Int_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiOPFTestChildGrouped_B, cTableNameTIOPFTestChildGrouped_B,
    'FloatField', 'Child_Float_Field');
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TtiObjectParentForTestingGrouped, TtiOPFTestChildGrouped_B);
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiObjectParentForTestingGroup, TtiOPFTestChildGrouped_B);
end;


{ TTestClasses }

procedure TtiObjectListForTesting.Add(AObject: TtiObjectListNestedForTesting);
begin
  inherited Add(AObject);
end;


function TtiObjectListForTesting.GetItems(i: integer): TtiObjectListNestedForTesting;
begin
  Result := TtiObjectListNestedForTesting(inherited GetItems(i));
end;


function TtiObjectListForTesting.GetOIDAsInteger: integer;
begin
  if OID.AsString <> '' then
    Result := StrToInt(OID.AsString)
  else
    Result := 0;
end;


procedure TtiObjectListForTesting.Read(const ADBConnectionName: string; APersistenceLayerName: string = '');
var
  i: integer;
begin
  inherited;
  if not Criteria.HasOrderBy then
  begin
    SortByProps(['OIDAsInteger']);
    ;
    for i := 0 to Count - 1 do
      Items[i].SortByProps(['OIDAsInteger']);
  end;
end;


procedure TtiObjectListForTesting.ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = '');
var
  i: integer;
begin
  inherited;
  SortByProps(['OIDAsInteger']);
  for i := 0 to Count - 1 do
    Items[i].SortByProps(['OIDAsInteger']);
end;


procedure TtiObjectListForTesting.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectListForTesting.SetItems(i: integer; const AValue: TtiObjectListNestedForTesting);
begin
  inherited SetItems(i, AValue);
end;


{ TTestClass }

function TtiOPFTestItem.GetOIDAsInteger: integer;
begin
  Result := StrToInt(OID.AsString);
end;


function TtiOPFTestItem.GetOwner: TtiObjectListNestedForTesting;
begin
  Result := TtiObjectListNestedForTesting(inherited GetOwner);
end;


procedure TtiOPFTestItem.SetOwner(const AValue: TtiObjectListNestedForTesting);
begin
  inherited SetOwner(AValue);
end;


{ TtiObjectListNestedForTesting }

procedure TtiObjectListNestedForTesting.Add(AObject: TtiOPFTestItem);
begin
  inherited Add(AObject);
end;


function TtiObjectListNestedForTesting.GetItems(i: integer): TtiOPFTestItem;
begin
  Result := TtiOPFTestItem(inherited GetItems(i));
end;


function TtiObjectListNestedForTesting.GetOwner: TtiObjectListForTesting;
begin
  Result := TtiObjectListForTesting(inherited GetOwner);
end;


procedure TtiObjectListNestedForTesting.Read(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectListNestedForTesting.ReadThis(
  const ADBConnectionName: string; APersistenceLayerName: string);
begin
  inherited;
end;

function TtiObjectListNestedForTesting.GetOIDAsInteger: integer;
begin
  Result := StrToInt(OID.AsString);
end;


procedure TtiObjectListNestedForTesting.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectListNestedForTesting.SetItems(i: integer; const AValue: TtiOPFTestItem);
begin
  inherited SetItems(i, AValue);
end;


procedure TtiObjectListNestedForTesting.SetOwner(const AValue: TtiObjectListForTesting);
begin
  inherited SetOwner(AValue);
end;


{ TtiObjectParentForTestingGroup }

procedure TtiObjectParentForTestingGroup.Add(AObject: TtiObjectParentForTestingGrouped; ADefDispOrdr: boolean);
begin
  inherited Add(AObject, ADefDispOrdr);
end;


function TtiObjectParentForTestingGroup.GetItems(i: integer): TtiObjectParentForTestingGrouped;
begin
  Result := TtiObjectParentForTestingGrouped(inherited GetItems(i));
end;


procedure TtiObjectParentForTestingGroup.Read(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectParentForTestingGroup.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectParentForTestingGroup.SetItems(i: integer; const AValue: TtiObjectParentForTestingGrouped);
begin
  inherited SetItems(i, AValue);
end;


{ TtiObjectParentForTestingList }

{
procedure TtiObjectParentForTestingList.Add(AObject: TtiObjectParentForTesting; ADefDispOrdr: boolean);
begin
  inherited Add(AObject, ADefDispOrdr);
end;

function TtiObjectParentForTestingList.GetItems(i: integer): TtiObjectParentForTesting;
begin
  result := TtiObjectParentForTesting(inherited GetItems(i));
end;

procedure TtiObjectParentForTestingList.SetItems(i: integer; const AValue: TtiObjectParentForTesting);
begin
  inherited SetItems(i, AValue);
end;
}


{ TtiOPFTestStreamProp }

constructor TtiOPFTestStreamProp.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;


destructor TtiOPFTestStreamProp.Destroy;
begin
  FStream.Free;
  inherited;
end;

 { TtiOPFTestInt64Prop }

{$IFDEF TESTINT64}
 // Getters and setters used to enable breakpoints for debugging
function TtiOPFTestInt64Prop.GetInt64Field: int64;
begin
  Result := FInt64Field;
end;


procedure TtiOPFTestInt64Prop.SetInt64Field(const AValue: int64);
begin
  FInt64Field := AValue;
end;

{$ENDIF}


function TTestListOfPersistents.AsString(const AFieldDelim: string; const ARowDelim: string; AFields: TStringList): string;
var
  i, j:   integer;
  lLine:  string;
  lField: string;
  e:      extended;
begin
  Assert(AFieldDelim <> '', 'AFieldDelim not assigned');
  Assert(ARowDelim <> '', 'ARowDelim not assigned');
  Assert(AFields <> NIL, 'AFields not assigned');
  Result := '';
  for i := 0 to AFields.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + AFieldDelim;
    Result := Result + AFields.Strings[i];
  end;
  for i := 0 to Count - 1 do
  begin
    lLine := '';
    for j := 0 to AFields.Count - 1 do
    begin
      lField := AFields.Strings[j];
      if SameText(lField, 'Caption') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim) + IntToStr(i);
      if SameText(lField, 'StringProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim) + IntToStr(i);
      if SameText(lField, 'IntProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim) + IntToStr(i);
      if SameText(lField, 'DateTimeProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim) + FormatDateTime(csWinDateTimeFormat, EncodeDate(2002, 01, 01) + i / 24);
      if SameText(lField, 'FloatProp') then
      begin
        e     := i + (i / 10);
        lLine := tiAddTrailingValue(lLine, AFieldDelim) + FloatToStr(e);
      end;
    end;
    if Result <> '' then
      Result := Result + ARowDelim;
    Result := Result + lLine;
  end;
end;


function TTestListOfPersistents.AsString: string;
var
  lsl: TStringList;
begin
  lsl := TStringList.Create;
  try
    lsl.Add('Caption');
    lsl.Add('StringProp');
    lsl.Add('IntProp');
    lsl.Add('DateTimeProp');
    lsl.Add('FloatProp');
    Result := AsString(',', CrLf, lsl);
  finally
    lsl.Free;
  end;
end;


constructor TTestListOfPersistents.Create;
var
  lData: TTestListToOutput;
  i:     integer;
begin
  inherited;
  for i := 0 to 9 do
  begin
    lData           := TTestListToOutput.Create;
    lData.DateTimeProp := EncodeDate(2002, 01, 01) + i / 24;
    lData.FloatProp := i + i / 10;
    lData.StringProp := IntToStr(i);
    lData.IntProp   := i;
    Add(lData);
  end;
end;


{ TTestListToOutput }

function TTestListToOutput.GetCaption: string;
begin
  Result := IntToStr(IntProp);
end;


{ TtiOPFTestOIdProp }

constructor TtiOPFTestOIdProp.Create;
begin
  inherited;
  {$IFDEF OID_AS_INT64}
  FOIDField := cNullOIDInteger;
  {$ELSE}
  FOIDField := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
  {$ENDIF}
end;

destructor TtiOPFTestOIdProp.Destroy;
begin
  {$IFNDEF OID_AS_INT64}
  FOIDField.Free;
  {$ENDIF}
  inherited;
end;

procedure TtiOPFTestOIdProp.SetOIDField(const Value: TtiOID);
begin
  FOIDField.Assign(Value);
end;

{ TtiObjectParentForTestingAbs }

procedure TtiObjectParentForTestingAbs.ReadThis(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

procedure TtiObjectParentForTestingAbs.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  inherited;
end;

  { TtiObjectByPropertyForTestingAbs }

  procedure TtiObjectByPropertyForTestingAbs.Read(const ADBConnectionName: string;
    APersistenceLayerName: string);
  begin
    inherited;
  end;

  procedure TtiObjectByPropertyForTestingAbs.Save(const ADBConnectionName: string;
    APersistenceLayerName: string);
  begin
    inherited;
  end;

end.







