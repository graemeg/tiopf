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

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tstPerFramework_BOM;

interface
uses
  tiPtnVisPerObj
  ,Classes
  ;

type

  TtiOPFTestStringProp = class( TPerObjAbs )
  private
    FStrField: string;
  published
    property StrField   : string  read FStrField   write FStrField ;
  end ;

  TtiOPFTestNotesProp = class( TPerObjAbs )
  private
    FNotesField: string;
  published
    property NotesField : string  read FNotesField write FNotesField ;
  end ;

  TtiOPFTestIntegerProp = class( TPerObjAbs )
  private
    FIntField: Integer;
  published
    property IntField   : Integer read FIntField   write FIntField ;
  end ;

{$IFDEF TESTINT64}
{$ENDIF}

{$IFDEF TESTINT64}
  TtiOPFTestInt64Prop = class( TPerObjAbs )
  private
    FInt64Field: Int64;
    function GetInt64Field: Int64;
    procedure SetInt64Field(const Value: Int64);
  published
    property Int64Field   : Int64 read GetInt64Field   write SetInt64Field ;
  end ;
{$ENDIF}


  TtiOPFTestFloatProp = class( TPerObjAbs )
  private
    FFloatField: real;
  published
    property FloatField : real    read FFloatField write FFloatField ;
  end ;

  TtiOPFTestDateTimeProp = class( TPerObjAbs )
  private
    FDateField: TDateTime;
  published
    property DateField  : TDateTime read FDateField write FDateField ;
  end ;

  TtiOPFTestBooleanProp = class( TPerObjAbs )
  private
    FBoolField: Boolean;
  published
    property BoolField  : Boolean read FBoolField write FBoolField ;
  end ;

  TtiOPFTestStreamProp = class( TPerObjAbs )
  private
    FStream: TStream;
  public
    constructor create ; override ;
    destructor  destroy ; override ;
  published
    property StreamField : TStream read FStream write FStream ;
  end ;


  TtiOPFTestData  = class ;
  TtiOPFTestGroup = class ;
  TtiOPFTestItem  = class ;

  //----------------------------------------------------------------------------
  TtiOPFTestData = class( TPerObjList )
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

  //----------------------------------------------------------------------------
  TtiOPFTestGroup = class( TPerObjList )
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

  //----------------------------------------------------------------------------
  TtiOPFTestItem = class( TPerObjAbs )
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

  TtiOPFTestParentAbs = class( TPerObjAbs )
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


  TtiOPFTestParent = class( TtiOPFTestParentAbs ) ;
  TtiOPFTestChild_A = class( TtiOPFTestParent ) ;
  TtiOPFTestChild_B = class( TtiOPFTestParent ) ;

{
  TtiOPFTestParentList = class( TPerObjList )
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

  TtiOPFTestParentGrouped = class( TtiOPFTestParentAbs ) ;
  TtiOPFTestChildGrouped_A = class( TtiOPFTestParentGrouped ) ;
  TtiOPFTestChildGrouped_B = class( TtiOPFTestParentGrouped ) ;

  TtiOPFTestParentGroup = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TtiOPFTestParentGrouped ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiOPFTestParentGrouped); reintroduce ;
  public
    property    Items[i:integer] : TtiOPFTestParentGrouped read GetItems write SetItems ;
    procedure   Add( pObject : TtiOPFTestParentGrouped   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

procedure RegisterMappings ;

implementation
uses
  tiClassToDBMap_BOM
  ,tiPersist
  ,tiPersistAbs_TST
  ,SysUtils
  ;

procedure RegisterMappings ;
begin

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStringProp,   cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStringProp,   cTIQueryTableName, 'StrField',   cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestNotesProp,    cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestNotesProp,    cTIQueryTableName, 'NotesField', cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestIntegerProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestIntegerProp,  cTIQueryTableName, 'IntField',   cTIQueryColName ) ;

{$IFDEF TESTINT64}
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestInt64Prop,    cTIQueryTableNameInt64, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestInt64Prop,    cTIQueryTableNameInt64, 'Int64Field',   cTIQueryColName ) ;
{$ENDIF}
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestFloatProp,    cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestFloatProp,    cTIQueryTableName, 'FloatField', cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestDateTimeProp, cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestDateTimeProp, cTIQueryTableName, 'DateField',  cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestBooleanProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestBooleanProp,  cTIQueryTableName, 'BoolField',  cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStreamProp,  cTIQueryTableName, 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestStreamProp,  cTIQueryTableName, 'StreamField',  cTIQueryColName ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'OID',        'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'StrField',   'Group_Str_Field', [pktReadable] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'IntField',   'Group_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'FloatField', 'Group_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'DateField',  'Group_Date_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'BoolField',  'Group_Bool_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestGroup, 'Test_Group', 'NotesField', 'Group_Notes_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiOPFTestData, TtiOPFTestGroup ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'OID',        'OID',       [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'Owner.OID',  'OID_Group', [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'StrField',   'Item_Str_Field', [pktReadable] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'IntField',   'Item_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'FloatField', 'Item_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'DateField',  'Item_Date_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'BoolField',  'Item_Bool_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestItem, 'Test_Item', 'NotesField', 'Item_Notes_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiOPFTestGroup, TtiOPFTestItem ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParent, 'Test_Parent', 'OID',        'OID',        [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParent, 'Test_Parent', 'StrField',   'Parent_Str_Field' ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'IntField',   'Child_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_A,  cTableNameTIOPFTestChild_A,  'FloatField', 'Child_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParent, TtiOPFTestChild_A ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'IntField',   'Child_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChild_B,  cTableNameTIOPFTestChild_B,  'FloatField', 'Child_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParent, TtiOPFTestChild_B ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGroup, cTableNameTIOPFTestParentGroup, 'OID',        'OID',      [pktDB] ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'OID',        'OID',        [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'Owner.OID',  'Owner_OID',  [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestParentGrouped, cTableNameTIOPFTestParentGrouped, 'StrField',   'Parent_Str_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestParentGrouped ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'IntField',   'Child_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_A,  cTableNameTIOPFTestChildGrouped_A,  'FloatField', 'Child_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParentGrouped, TtiOPFTestChildGrouped_A ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestChildGrouped_A ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'OID',        'OID',       [pktDB, pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'IntField',   'Child_Int_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiOPFTestChildGrouped_B,  cTableNameTIOPFTestChildGrouped_B,  'FloatField', 'Child_Float_Field' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterInheritance( TtiOPFTestParentGrouped, TtiOPFTestChildGrouped_B ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiOPFTestParentGroup, TtiOPFTestChildGrouped_B ) ;


end ;

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

constructor TtiOPFTestStreamProp.create;
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

end.
