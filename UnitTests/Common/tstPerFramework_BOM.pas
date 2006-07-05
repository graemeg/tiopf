unit tstPerFramework_BOM;

{$I tiDefines.inc}

interface
uses
   tiObject
//  ,tiVisitor
  ,Classes
//  ,Contnrs
  ;


type
  TtiOPFTestStringProp = class( TtiObject )
  private
    FStrField: string;
  published
    property StrField   : string  read FStrField   write FStrField ;
  end ;


  TtiOPFTestNotesProp = class( TtiObject )
  private
    FNotesField: string;
  published
    property NotesField : string  read FNotesField write FNotesField ;
  end ;


  TtiOPFTestIntegerProp = class( TtiObject )
  private
    FIntField: Integer;
  published
    property IntField   : Integer read FIntField   write FIntField ;
  end ;


{$IFDEF TESTINT64}
  TtiOPFTestInt64Prop = class( TtiObject )
  private
    FInt64Field: Int64;
    function GetInt64Field: Int64;
    procedure SetInt64Field(const Value: Int64);
  published
    property Int64Field   : Int64 read GetInt64Field   write SetInt64Field ;
  end ;
{$ENDIF}


  TtiOPFTestFloatProp = class( TtiObject )
  private
    FFloatField: real;
  published
    property FloatField : real    read FFloatField write FFloatField ;
  end ;


  TtiOPFTestDateTimeProp = class( TtiObject )
  private
    FDateField: TDateTime;
  published
    property DateField  : TDateTime read FDateField write FDateField ;
  end ;


  TtiOPFTestBooleanProp = class( TtiObject )
  private
    FBoolField: Boolean;
  published
    property BoolField  : Boolean read FBoolField write FBoolField ;
  end ;


  TtiOPFTestStreamProp = class( TtiObject )
  private
    FStream: TStream;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  published
    property StreamField : TStream read FStream write FStream ;
  end ;


  TtiOPFTestData  = class ;
  TtiOPFTestGroup = class ;
  TtiOPFTestItem  = class ;


  TtiOPFTestData = class( TtiObjectList )
  private
    function GetOIDAsInteger: integer;
  protected
    function    GetItems(i: integer): TtiOPFTestGroup ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiOPFTestGroup); reintroduce ;
  public
    property    Items[i:integer] : TtiOPFTestGroup read GetItems write SetItems ;
    procedure   Add( pObject : TtiOPFTestGroup ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    procedure   Read( const pDBConnectionName : string ; pPerLayerName : string = '' ) ; override ;
    procedure   ReadPK( const pDBConnectionName : string ; pPerLayerName : string = '' ) ; override ;
  published
    property    OIDAsInteger : integer read GetOIDAsInteger ;
  end ;


  TtiOPFTestGroup = class( TtiObjectList )
  private
    FIntField: integer;
    FFloatField: real;
    FStrField: string;
    FDateField: TDateTime;
    FBoolField: Boolean;
    FNotesField: string;
    function GetOIDAsInteger: integer;
  protected
    function    GetOwner: TtiOPFTestData; reintroduce ;
    procedure   SetOwner(const Value: TtiOPFTestData); reintroduce ;
    function    GetItems(i: integer): TtiOPFTestItem ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiOPFTestItem); reintroduce ;
  public
    property    Items[i:integer] : TtiOPFTestItem read GetItems write SetItems ;
    procedure   Add( pObject : TtiOPFTestItem ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  published
    property    OIDAsInteger : integer read GetOIDAsInteger ;
    property    StrField   : string  read FStrField   write FStrField ;
    property    IntField   : integer read FIntField   write FIntField ;
    property    FloatField : real    read FFloatField write FFloatField ;
    property    DateField  : TDateTime read FDateField write FDateField ;
    property    BoolField  : Boolean read FBoolField write FBoolField ;
    property    NotesField : string  read FNotesField write FNotesField ;
  end ;


  TtiOPFTestItem = class( TtiObject )
  private
    FIntField: integer;
    FFloatField: real;
    FStrField: string;
    FDateField: TDateTime;
    FBoolField: Boolean;
    FNotesField: string;
    function    GetOIDAsInteger: integer;
  protected
    function    GetOwner: TtiOPFTestGroup; reintroduce ;
    procedure   SetOwner(const Value: TtiOPFTestGroup); reintroduce ;
  public
    property    Owner       : TtiOPFTestGroup read GetOwner      write SetOwner ;
  published
    property OIDAsInteger : integer read GetOIDAsInteger ;
    property StrField   : string  read FStrField   write FStrField ;
    property IntField   : integer read FIntField   write FIntField ;
    property FloatField : real    read FFloatField write FFloatField ;
    property DateField  : TDateTime read FDateField write FDateField ;
    property BoolField  : Boolean read FBoolField write FBoolField ;
    property NotesField : string  read FNotesField write FNotesField ;
  end ;


  TtiOPFTestParent = class ;


  TtiOPFTestParentAbs = class( TtiObject )
  private
    FStrField: string;
    FIntField: integer;
    FFloatField: real;
    FDateField: TDateTime;
  public
  published
    property StrField   : string  read FStrField   write FStrField ;
    property IntField   : integer read FIntField   write FIntField ;
    property FloatField : real    read FFloatField write FFloatField ;
    property DateField  : TDateTime read FDateField write FDateField ;
  end;


  TtiOPFTestParent  = class( TtiOPFTestParentAbs ) ;
  TtiOPFTestChild_A = class( TtiOPFTestParent ) ;
  TtiOPFTestChild_B = class( TtiOPFTestParent ) ;

{
  TtiOPFTestParentList = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TtiOPFTestParent ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiOPFTestParent); reintroduce ;
  public
    property    Items[i:integer] : TtiOPFTestParent read GetItems write SetItems ;
    procedure   Add( pObject : TtiOPFTestParent   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;
}

  TtiOPFTestParentGrouped   = class( TtiOPFTestParentAbs ) ;
  TtiOPFTestChildGrouped_A  = class( TtiOPFTestParentGrouped ) ;
  TtiOPFTestChildGrouped_B  = class( TtiOPFTestParentGrouped ) ;


  TtiOPFTestParentGroup = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TtiOPFTestParentGrouped ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiOPFTestParentGrouped); reintroduce ;
  public
    property    Items[i:integer] : TtiOPFTestParentGrouped read GetItems write SetItems ;
    procedure   Add( pObject : TtiOPFTestParentGrouped   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;


  TTestGetPropNamesAbs = class( TtiObject )
  private
    FStringProp: string;
    FCharProp: Char;
    FShortStringStringProp: ShortString;
    FWideStringProp: WideString;
    FWideCharProp: WideChar;
    FInt64Prop: Int64;
    FIntProp: integer;
    FFloatProp: Extended;
    FMethodProp: TNotifyEvent;
    FObjectProp: TObject;
    FDateTimeProp: TDateTime;
    FBoolProp: Boolean;
  protected
    property StringProp : string read FStringProp write FStringProp ;
    property ShortStringProp : ShortString read FShortStringStringProp write FShortStringStringProp ;
    property WideStringProp  : WideString Read FWideStringProp write FWideStringProp ;
    property CharProp : Char Read FCharProp write FCharProp ;
    property WideCharProp : WideChar Read FWideCharProp write FWideCharProp ;
    property IntProp : integer read FIntProp write FIntProp ;
    property Int64Prop : Int64 read FInt64Prop write FInt64Prop ;
    property BoolProp  : Boolean read FBoolProp write FBoolProp ;
    property DateTimeProp : TDateTime read FDateTimeProp write FDateTimeProp ;
    property FloatProp : Extended read FFloatProp write FFloatProp ;

    property ObjectProp : TObject read FObjectProp write FObjectProp ;
    property MethodProp : TNotifyEvent read FMethodProp write FMethodProp ;
    // These are the leftovers
    // tkUnknown, tkEnumeration, tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

    property ReadOnlyStringProp : string read FStringProp ;
    property ReadOnlyShortStringProp : ShortString read FShortStringStringProp ;
    property ReadOnlyWideStringProp  : WideString Read FWideStringProp ;
    property ReadOnlyCharProp : Char Read FCharProp ;
    property ReadOnlyWideCharProp : WideChar Read FWideCharProp ;
    property ReadOnlyIntProp : integer read FIntProp ;
    property ReadOnlyInt64Prop : Int64 read FInt64Prop ;
    property ReadOnlyBoolProp  : boolean read FBoolProp ;
    property ReadOnlyDateTimeProp : TDateTime read FDateTimeProp ;
    property ReadOnlyFloatProp : Extended read FFloatProp ;
    property ReadOnlyObjectProp : TObject read FObjectProp ;
    property ReadOnlyMethodProp : TNotifyEvent read FMethodProp ;
  end ;


  TTestListToOutput = class( TTestGetPropNamesAbs )
  protected
    function GetCaption: string; override ;
  published
    property StringProp ;
    property IntProp ;
    property DateTimeProp ;
    property FloatProp ;
  end ;


  TTestListOfPersistents = class( TtiObjectList )
  public
    constructor Create ; override ;
    function AsString : string ; overload;
    function AsString(const AFieldDelim: string; const ARowDelim: string; AFields: TStringList ) : string ; overload;
  end ;


  TTestGetPropNames = class( TTestGetPropNamesAbs )
  published
    property StringProp ;
    property ShortStringProp ;
    property WideStringProp ;
    property CharProp ;
    property WideCharProp ;
    property IntProp ;
    property Int64Prop ;
    property BoolProp ;
    property DateTimeProp ;
    property FloatProp ;
    property ObjectProp ;
    property MethodProp ;
    property ReadOnlyStringProp ;
    property ReadOnlyShortStringProp ;
    property ReadOnlyWideStringProp ;
    property ReadOnlyCharProp ;
    property ReadOnlyWideCharProp ;
    property ReadOnlyIntProp ;
    property ReadOnlyInt64Prop ;
    property ReadOnlyBoolProp ;
    property ReadOnlyDateTimeProp ;
    property ReadOnlyFloatProp ;
    property ReadOnlyObjectProp ;
    property ReadOnlyMethodProp ;
  end;


procedure RegisterMappings ;


implementation
uses
  tiClassToDBMap_BOM
  ,tiOPFManager
  ,SysUtils
  ,tiUtils
  ,tiTestFramework
  ,tiConstants
  ;


procedure RegisterMappings ;
begin
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStringProp,   cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStringProp,   cTIQueryTableName, 'StrField',   cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestNotesProp,    cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestNotesProp,    cTIQueryTableName, 'NotesField', cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestIntegerProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestIntegerProp,  cTIQueryTableName, 'IntField',   cTIQueryColName ) ;

{$IFDEF TESTINT64}
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestInt64Prop,    cTIQueryTableNameInt64, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestInt64Prop,    cTIQueryTableNameInt64, 'Int64Field',   cTIQueryColName ) ;
{$ENDIF}
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestFloatProp,    cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestFloatProp,    cTIQueryTableName, 'FloatField', cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestDateTimeProp, cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestDateTimeProp, cTIQueryTableName, 'DateField',  cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestBooleanProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestBooleanProp,  cTIQueryTableName, 'BoolField',  cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStreamProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStreamProp,  cTIQueryTableName, 'StreamField',  cTIQueryColName ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'OID',        'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'StrField',   'Group_Str_Field', [pktReadable] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'IntField',   'Group_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'FloatField', 'Group_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'DateField',  'Group_Date_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'BoolField',  'Group_Bool_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'NotesField', 'Group_Notes_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiOPFTestData, TtiOPFTestGroup ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'OID',        'OID',       [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'Owner.OID',  'OID_Group', [pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'StrField',   'Item_Str_Field', [pktReadable] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'IntField',   'Item_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'FloatField', 'Item_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'DateField',  'Item_Date_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'BoolField',  'Item_Bool_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'NotesField', 'Item_Notes_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiOPFTestGroup, TtiOPFTestItem ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParent, 'Test_Parent', 'OID',        'OID',        [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParent, 'Test_Parent', 'StrField',   'Parent_Str_Field' ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'IntField',   'Child_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'FloatField', 'Child_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParent, TtiOPFTestChild_A ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'IntField',   'Child_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'FloatField', 'Child_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParent, TtiOPFTestChild_B ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGroup, cTableNameTIOPFTestParentGroup, 'OID',        'OID',      [pktDB] ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'OID',        'OID',        [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'Owner.OID',  'Owner_OID',  [pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'StrField',   'Parent_Str_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestParentGrouped ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'IntField',   'Child_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'FloatField', 'Child_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParentGrouped, TtiOPFTestChildGrouped_A ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestChildGrouped_A ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'IntField',   'Child_Int_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'FloatField', 'Child_Float_Field' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParentGrouped, TtiOPFTestChildGrouped_B ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestChildGrouped_B );
end;


{ TTestClasses }

procedure TtiOPFTestData.Add(pObject: TtiOPFTestGroup;pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;


function TtiOPFTestData.GetItems(i: integer): TtiOPFTestGroup;
begin
  result := TtiOPFTestGroup( inherited GetItems( i )) ;
end;


function TtiOPFTestData.GetOIDAsInteger: integer;
begin
  if OID.AsString <> '' then
    result := StrToInt( OID.AsString )
  else
    result := 0 ;
end;


procedure TtiOPFTestData.Read( const pDBConnectionName : string ; pPerLayerName : string = '' );
var
  i : integer ;
begin
  inherited;
  SortByProps(['OIDAsInteger']); ;
  for i := 0 to Count - 1 do
    Items[i].SortByProps(['OIDAsInteger']) ;
end;


procedure TtiOPFTestData.ReadPK( const pDBConnectionName : string ; pPerLayerName : string = '' );
var
  i : integer ;
begin
  inherited;
  SortByProps(['OIDAsInteger']); ;
  for i := 0 to Count - 1 do
    Items[i].SortByProps(['OIDAsInteger']) ;
end;


procedure TtiOPFTestData.SetItems(i: integer; const Value: TtiOPFTestGroup);
begin
  inherited SetItems( i, Value ) ;
end;


{ TTestClass }

function TtiOPFTestItem.GetOIDAsInteger: integer;
begin
  result := StrToInt(OID.AsString);
end;


function TtiOPFTestItem.GetOwner: TtiOPFTestGroup;
begin
  result := TtiOPFTestGroup( inherited GetOwner ) ;
end;


procedure TtiOPFTestItem.SetOwner(const Value: TtiOPFTestGroup);
begin
  inherited SetOwner( Value ) ;
end;


{ TtiOPFTestGroup }

procedure TtiOPFTestGroup.Add(pObject: TtiOPFTestItem; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;


function TtiOPFTestGroup.GetItems(i: integer): TtiOPFTestItem;
begin
  result := TtiOPFTestItem( inherited GetItems( i )) ;
end;


function TtiOPFTestGroup.GetOwner: TtiOPFTestData;
begin
  result := TtiOPFTestData( inherited GetOwner ) ;
end;


function TtiOPFTestGroup.GetOIDAsInteger: integer;
begin
  result := StrToInt( OID.AsString ) ;
end;


procedure TtiOPFTestGroup.SetItems(i: integer; const Value: TtiOPFTestItem);
begin
  inherited SetItems( i, Value ) ;
end;


procedure TtiOPFTestGroup.SetOwner(const Value: TtiOPFTestData);
begin
  inherited SetOwner( Value ) ;
end;


{ TtiOPFTestParentGroup }

procedure TtiOPFTestParentGroup.Add(pObject: TtiOPFTestParentGrouped;pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;


function TtiOPFTestParentGroup.GetItems(i: integer): TtiOPFTestParentGrouped;
begin
  result := TtiOPFTestParentGrouped( inherited GetItems( i )) ;
end;


procedure TtiOPFTestParentGroup.SetItems(i: integer; const Value: TtiOPFTestParentGrouped);
begin
  inherited SetItems( i, Value ) ;
end;


{ TtiOPFTestParentList }

{
procedure TtiOPFTestParentList.Add(pObject: TtiOPFTestParent; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TtiOPFTestParentList.GetItems(i: integer): TtiOPFTestParent;
begin
  result := TtiOPFTestParent( inherited GetItems( i )) ;
end;

procedure TtiOPFTestParentList.SetItems(i: integer; const Value: TtiOPFTestParent);
begin
  inherited SetItems( i, Value ) ;
end;
}


{ TtiOPFTestStreamProp }

constructor TtiOPFTestStreamProp.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;


destructor TtiOPFTestStreamProp.destroy;
begin
  FStream.Free;
  inherited;
end;

{ TtiOPFTestInt64Prop }

{$IFDEF TESTINT64}
// Getters and setters used to enable breakpoints for debugging
function TtiOPFTestInt64Prop.GetInt64Field: Int64;
begin
  result := FInt64Field;
end;


procedure TtiOPFTestInt64Prop.SetInt64Field(const Value: Int64);
begin
  FInt64Field := Value;
end;
{$ENDIF}


function TTestListOfPersistents.AsString( const AFieldDelim: string; const ARowDelim: string; AFields: TStringList ) : string;
var
  i, j : integer ;
  lLine : string ;
  lField: string;
  e: extended;
begin
  Assert(AFieldDelim <> '', 'AFieldDelim not assigned');
  Assert(ARowDelim <> '', 'ARowDelim not assigned');
  Assert(AFields <> nil, 'AFields not assigned');
  result := '';
  for i := 0 to AFields.Count - 1 do
  begin
    if result <> '' then
      result := result + AFieldDelim;
    result := result + AFields.Strings[i];
  end;
  for i := 0 to Count - 1 do
  begin
    lLine := '';
    for j := 0 to AFields.Count - 1 do
    begin
      lField := AFields.Strings[j];
      if SameText(lField, 'Caption') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim ) + IntToStr(i);
      if SameText(lField, 'StringProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim ) + IntToStr(i);
      if SameText(lField, 'IntProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim ) + IntToStr(i);
      if SameText(lField, 'DateTimeProp') then
        lLine := tiAddTrailingValue(lLine, AFieldDelim ) + FormatDateTime( csWinDateTimeFormat, EncodeDate(2002, 01, 01) + i / 24 );
      if SameText(lField, 'FloatProp') then
      begin
        e := i + (i / 10);
        lLine := tiAddTrailingValue(lLine, AFieldDelim ) + FloatToStr(e);
      end
    end ;
    if result <> '' then
      result := result + ARowDelim ;
    result := result + lLine ;
  end ;
end;


function TTestListOfPersistents.AsString : string;
var
  lsl: TStringList;
begin
  lsl := TStringList.Create;
  try
    {$IFNDEF FPC202}
    lsl.Add('Caption');
    {$ENDIF}
    lsl.Add('StringProp');
    lsl.Add('IntProp');
    lsl.Add('DateTimeProp');
    lsl.Add('FloatProp');
    {$IFDEF FPC202}
      {$Note Remove this after FPC 2.0.3 has been released}
    lsl.Add('Caption');   { Due to a bug in FPC 2.0.2 that has been reported. }
    {$ENDIF}
    result := AsString( ',', CrLf, lsl);
  finally
    lsl.Free;
  end;
end;


constructor TTestListOfPersistents.Create;
var
  lData : TTestListToOutput ;
  i : integer ;
begin
  inherited ;
  for i := 0 to 9 do
  begin
    lData := TTestListToOutput.Create ;
    lData.DateTimeProp := EncodeDate(2002, 01, 01) + i / 24 ;
    lData.FloatProp    := i + i /10 ;
    lData.StringProp   := IntToStr( i ) ;
    lData.IntProp      := i ;
    Add( lData ) ;
  end ;
end;


{ TTestListToOutput }

function TTestListToOutput.GetCaption: string;
begin
  result := IntToStr(IntProp);
end;


end.

