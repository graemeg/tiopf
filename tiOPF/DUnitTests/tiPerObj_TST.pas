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
    June, 2002, Peter Hinrichsen, Created

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerObj_TST;

interface

uses
  Classes  // needed for TStringList
  ,TestFrameWork
  ,tiPtnVisPerObj
  ,tstPerFramework_BOM
  ,tiPersistAbs_TST
  ;

type

  TtstPerObjList = class ;
  TtstPerObjAbs = class ;

  TTestPerObjAbsCreatNew = class( TtiPerTestCase )
  public
    constructor Create(MethodName: string); override ;
  published
    procedure   CreateNew ;            // Next OID suppressed
    procedure   CreateNew_OIDGUID    ; // GUID OID
    procedure   CreateNew_OIDInteger ; // GUID Integer
  end;

  TTestPerObjAbs = class( TTestCase )
  private
    procedure CheckTSTPerObjAbs(pData: TtstPerObjAbs; pValue: integer);
    procedure SetTSTPerObjAbs(pData: TtstPerObjAbs; pValue: integer);
    procedure CheckTSTPerObjList(pData: TtstPerObjList; pValue: integer);
    procedure SetTSTPerObjList(pData: TtstPerObjList; pValue: integer);
    procedure TstFindMethod( pPerObjAbs : TPerObjAbs ; var pbFound : boolean ) ;
    procedure TstFindMethodWithParam( pPerObjAbs : TPerObjAbs ; var pbFound : boolean; pUserContext: Pointer ) ;
    procedure TstFindAll( pPerObjAbs : TPerObjAbs ; var pbFound : boolean ) ;
    function  CreateTestDataList: TtstPerObjList;
  published
    procedure Owner ;
    procedure Deleted ;
    procedure Dirty ;
    procedure Index ;
    procedure SetPropValue ;
    procedure GetPropValue ;
    procedure IsReadWriteProp ;

    procedure Equals ;
    procedure ObjectStateAsString ;
    procedure FindByOID ;
    procedure FindWithMethod ;
    procedure FindWithMethodAndParam ;
    procedure FindAllWithMethod ;
    procedure IsUnique ;
    procedure AssignFlat ;
    procedure AssignList ;
    procedure AssignCompound ;
    procedure CloneFlat ;
    procedure CloneList ;
    procedure CloneCompound ;
    procedure TopOfHierarchy ;
    procedure IsValid ;
  end ;

  TTestPerObjList = class( TTestCase )
  private
    function CreateList : TtstPerObjList;
    procedure DoForEachMethod(pData: TPerObjAbs);
  published
    procedure   Add;
    procedure   AddOwner;
    procedure   Items;
    procedure   Count;
    procedure   CountNotDeleted;
    procedure   OwnsObjects;
    procedure   ItemOwner;
    procedure   AutoSetItemOwner;
    procedure   ForEachMethod;
    procedure   ForEachMethodRegular;
    procedure   Clear;
    procedure   Empty;
    procedure   IndexOf;
    procedure   Last;
    procedure   First;
    procedure   LastExcludeDeleted;
    procedure   Delete;
    procedure   Remove;
    procedure   RemoveOwner;
    procedure   Extract;
    procedure   InsertByIndex ;
    procedure   InsertByObject ;
    procedure   MarkListItemsForDeletion;
    procedure   MarkListItemsDirty;
    procedure   PropToStrings;
    procedure   FindByProps_DirectValue;
    procedure   FindByProps_TypedValue;
    procedure   SortByOID;
    procedure   SortByDispOrder;
    procedure   SortByProps;
  end ;

  //----------------------------------------------------------------------------
  TtstPerObjList = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TtstPerObjAbs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtstPerObjAbs); reintroduce ;
  public
    property    Items[i:integer] : TtstPerObjAbs read GetItems write SetItems ;
    procedure   Add( pObject : TtstPerObjAbs   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    function    Clone : TtstPerObjList ; reintroduce ;
    constructor CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; override ;
  published
  end ;

  TtstOrdProp = ( tstOrdProp_1, tstOrdProp_2, tstOrdProp_3 ) ;

  //----------------------------------------------------------------------------
  TtstPerObjAbs = class( TPerObjAbs )
  private
    FBoolProp: boolean;
    FIntProp: integer;
    FFloatProp: real;
    FStrProp: string;
    FDateProp: TDateTime;
    FOrdProp: TtstOrdProp;
  protected
    function    GetOwner: TtstPerObjList; reintroduce ;
    procedure   SetOwner(const Value: TtstPerObjList ); reintroduce ;
  public
    constructor Create ; override ;
    constructor CreateNew(const pOwner : TPerObjAbs ; const pDatabaseName : string = '' ; const pPerLayerName : string = ''); override ;
    property    Owner       : TtstPerObjList             read GetOwner      write SetOwner ;
    procedure   Populate ;
    function    Clone : TtstPerObjAbs ; reintroduce ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published          
    property    StrProp   : string      read FStrProp   write FStrProp ;
    property    IntProp   : integer     read FIntProp   write FIntProp ;
    property    FloatProp : real        read FFloatProp write FFloatProp ;
    property    DateProp  : TDateTime   read FDateProp  write FDateProp ;
    property    BoolProp  : boolean     read FBoolProp  write FBoolProp ;
    property    OrdProp   : TtstOrdProp read FOrdProp   write FOrdProp ;
    property    StrPropReadOnly : string read FStrProp ;
  end ;

  TtstPerObjOwnedObj = class( TtstPerObjAbs )
  private
    FObjProp: TtstPerObjAbs;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  published
    property ObjProp : TtstPerObjAbs read FObjProp ;
  end ;

  TtstPerObjOwnedObjCanAssign = class( TtstPerObjOwnedObj )
  protected
    procedure   AssignClassProps(pSource: TPerObjAbs); override ;
  public
    function    Clone : TtstPerObjOwnedObjCanAssign ; reintroduce ;
  end ;

  TtstPerObjMappedObj = class( TtstPerObjAbs )
  private
    FObjProp: TtstPerObjAbs;
  published
    property ObjProp : TtstPerObjAbs read FObjProp write FObjProp ;
  end ;

procedure RegisterTests ;

const
  cStrPropErrorMsg    = 'Please enter a StrProp value' ;
  cStrPropErrorCode   = 1 ;
  cIntPropErrorMsg    = 'Please enter an IntProp value' ;
  cIntpropErrorCode   = 2 ;
  cFloatPropErrorMsg  = 'Please enter a FloatProp value';
  cFloatPropErrorCode = 3 ;

implementation
uses
  SysUtils
  ,tiDBConnectionSetupAbs_TST
  ,TypInfo
  ,tiDUnitUtils
  ,tiDUnitDependencies
  ,tiPersist

  ,tiPerObjOIDGUID
  ,tiPerObjOIDInteger

  ;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
  begin
    RegisterTest( TTestPerObjAbs.Suite );
    RegisterTest( TTestPerObjList.Suite );
  end ;
  tiDUnitDependencies.RegisterDBTests( 'TPerObjAbs.CreateNew', TTestPerObjAbsCreatNew );
end ;

function CreateTestData : TtstPerObjList ;
var
  lOID : Integer ;
  i : integer ;
  lData : TtstPerObjOwnedObj ;
begin
  lOID := 1 ;
  result := TtstPerObjList.Create ;
  result.ObjectState := posClean ;
  result.OID.AsString := IntToStr(lOID) ;
  Inc( lOID ) ;

  for i := 0 to 9 do
  begin
    lData := TtstPerObjOwnedObj.Create ;
    lData.ObjectState := posClean ;
    lData.OID.AsString := IntToStr(lOID) ;
    Inc( lOID ) ;
    lData.ObjProp.ObjectState := posClean ;
    lData.ObjProp.OID.AsString := IntToStr(lOID) ;
    Inc( lOID ) ;
    result.Add( lData ) ;
  end ;

end ;
{ TTestPerObjAbs }

procedure TTestPerObjAbs.AssignCompound;
var
  lTestExceptionFrom : TtstPerObjOwnedObj ;
  lTestExceptionTo   : TtstPerObjOwnedObj ;
  lFrom : TtstPerObjOwnedObjCanAssign ;
  lTo   : TtstPerObjOwnedObjCanAssign ;
begin
  lTestExceptionFrom := TtstPerObjOwnedObj.Create ;
  try
    lTestExceptionTo := TtstPerObjOwnedObj.Create ;
    try
      try
        lTestExceptionTo.Assign( lTestExceptionFrom ) ;
        Fail( 'An exception was not raised when it should have been.' ) ;
      except
        on e:exception do
          Check( true, 'Should have special type of exception?' ) ;
      end ;
    finally
      lTestExceptionTo.Free ;
    end ;
  finally
    lTestExceptionFrom.Free ;
  end ;

  lFrom := TtstPerObjOwnedObjCanAssign.Create ;
  try
    SetTSTPerObjAbs( lFrom, 1 ) ;
    SetTSTPerObjAbs( lFrom.ObjProp, 2 ) ;
    lTo   := TtstPerObjOwnedObjCanAssign.Create ;
    try
      lTo.Assign( lFrom ) ;
      CheckTSTPerObjAbs( lFrom, 1 ) ;
      CheckTSTPerObjAbs( lFrom.ObjProp, 2 ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;

end;

procedure TTestPerObjAbs.SetTSTPerObjAbs( pData : TtstPerObjAbs ; pValue : integer ) ;
begin
  pData.StrProp   := IntToStr( pValue ) ;
  pData.IntProp   := pValue ;
  pData.FloatProp := pValue + pValue / 10 + pValue / 100 + pValue + pValue / 1000 ;
  pData.DateProp  := pValue ;
  pData.BoolProp  := pValue mod 2 = 0 ;
  pData.OID.AsString        := IntToStr(pValue) ;
  pData.DispOrder  := pValue ;
end ;

procedure TTestPerObjAbs.CheckTSTPerObjAbs( pData : TtstPerObjAbs ; pValue : integer ) ;
begin
  CheckEquals( IntToStr( pValue ), pData.StrProp, 'Failed on StrField' ) ;
  CheckEquals( pValue, pData.IntProp,'Failed on IntField' ) ;
  CheckEquals( pValue + pValue / 10 + pValue / 100 + pValue + pValue / 1000, pData.FloatProp, cDUnitTestFloatPrecision, 'Failed on FloatField' ) ;
  CheckEquals( pValue, pData.DateProp, 0.00001, 'Failed on DateField' ) ;
  CheckEquals( pValue mod 2 = 0, pData.BoolProp, 'Failed on Bool' ) ;
  CheckEquals( IntToStr(pValue), pData.OID.AsString,'Failed on OID' ) ;
  CheckEquals( pValue, pData.DispOrder,'Failed on DispOrder' ) ;
end ;

procedure TTestPerObjAbs.SetTSTPerObjList( pData : TtstPerObjList ; pValue : integer ) ;
begin
  pData.OID.AsString        := IntToStr(pValue) ;
  pData.DispOrder  := pValue ;
end ;

procedure TTestPerObjAbs.CheckTSTPerObjList( pData : TtstPerObjList ; pValue : integer ) ;
begin
  CheckEquals( IntToStr(pValue), pData.OID.AsString,'Failed on OID' ) ;
  CheckEquals( pValue, pData.DispOrder,'Failed on DispOrder' ) ;
end ;

procedure TTestPerObjAbs.AssignFlat;
var
  lFrom : TtstPerObjAbs ;
  lTo   : TtstPerObjAbs ;
begin
  lFrom := TtstPerObjAbs.Create ;
  try
    SetTSTPerObjAbs( lFrom, 1 ) ;
    lTo   := TtstPerObjAbs.Create ;
    try
      SetTSTPerObjAbs( lTo, 2 ) ;
      lTo.Assign( lFrom ) ;
      CheckTSTPerObjAbs( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;
end;

procedure TTestPerObjAbs.AssignList;
var
  lFrom : TtstPerObjList ;
  lItem : TtstPerObjAbs ;
  lTo   : TtstPerObjList ;
begin
  lFrom := TtstPerObjList.Create ;
  try
    lFrom.OwnsObjects := true ;
    SetTSTPerObjList( lFrom, 1 ) ;
    lItem := TtstPerObjAbs.Create ;
    SetTSTPerObjAbs( lItem, 2 ) ;
    lFrom.Add( lItem ) ;
    lTo   := TtstPerObjList.Create ;
    try
      SetTSTPerObjList( lTo, 3 ) ;
      lTo.Assign( lFrom ) ;
      CheckTSTPerObjList( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
      CheckEquals( 1, lTo.Count, 'Failed on Count' ) ;
      CheckTSTPerObjAbs( lTo.Items[0], 2 ) ;
      Check( lTo = lTo.Items[0].Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;

  lFrom := TtstPerObjList.Create ;
  try
    lFrom.OwnsObjects := false ;
    lFrom.AutoSetItemOwner := false ;
    SetTSTPerObjList( lFrom, 1 ) ;
    lItem := TtstPerObjAbs.Create ;
    SetTSTPerObjAbs( lItem, 2 ) ;
    lFrom.Add( lItem ) ;
    lTo   := TtstPerObjList.Create ;
    lTo.OwnsObjects := false ;
    lTo.AutoSetItemOwner := false ;
    try
      SetTSTPerObjList( lTo, 3 ) ;
      lTo.Assign( lFrom ) ;
      CheckTSTPerObjList( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
      CheckEquals( 1, lTo.Count, 'Failed on Count' ) ;
      CheckTSTPerObjAbs( lTo.Items[0], 2 ) ;
      Check( nil = lTo.Items[0].Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;


end;

procedure TTestPerObjAbs.CloneFlat;
var
  lFrom : TtstPerObjAbs ;
  lTo   : TtstPerObjAbs ;
begin
  lFrom := TtstPerObjAbs.Create ;
  try
    SetTSTPerObjAbs( lFrom, 1 ) ;
    lTo   := lFrom.Clone ;
    try
      SetTSTPerObjAbs( lTo, 2 ) ;
      lTo.Assign( lFrom ) ;
      CheckTSTPerObjAbs( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;
end;

// Must think more about this as it might require DB access
procedure TTestPerObjAbsCreatNew.CreateNew;
var
  lData : TtstPerObjList ;
  lItem : TtstPerObjAbs ;
begin
  lData := TtstPerObjList.CreateNew ;
  try
    Check( lData.ObjectState = posCreate, 'Failed on ObjectState = posCreate' ) ;
    Check( lData.OID.AsString = IntToStr( Integer( lData )), 'Failed on OID' ) ;
    lItem := TtstPerObjAbs.CreateNew( lData ) ;
    Check( lItem.ObjectState = posCreate, 'Failed on ObjectState = posCreate' ) ;
    Check( lItem.OID.AsString = IntToStr(Integer( lItem )), 'Failed on OID' ) ;
    Check( lItem.Owner = lData, 'Failed on lItem.Owner = lData' ) ;
  finally
    lData.Free;
  end;
end;

procedure TTestPerObjAbs.Deleted;
var
  lData  : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem  : TtstPerObjOwnedObj;
  lMap   : TtstPerObjMappedObj;
begin
  lData := TtstPerObjList.Create;
  try
    Check( Not lData.Deleted, 'Failed on 1' ) ;
    lData.ObjectState := posPK;
    Check( Not lData.Deleted, 'Failed on 2' ) ;
    lData.ObjectState := posClean;
    Check( Not lData.Deleted, 'Failed on 3' ) ;
    lData.ObjectState := posCreate;
    Check( not lData.Deleted, 'Failed on 4' ) ;
    lData.ObjectState := posUpdate;
    Check( not lData.Deleted, 'Failed on 5' ) ;
    lData.ObjectState := posDelete;
    Check( lData.Deleted, 'Failed on 6' ) ;
    lData.ObjectState := posDeleted;
    Check( lData.Deleted, 'Failed on 7' ) ;
    lData.ObjectState := posClean;
    Check( Not lData.Deleted, 'Failed on 8' ) ;

    lGroup := TtstPerObjAbs.Create ;
    lData.Add( lGroup ) ;
    lItem := TtstPerObjOwnedObj.Create ;
    lData.Add( lItem ) ;

    lData.Deleted := true ;
    Check( lData.Deleted, 'Failed on 9' ) ;
    Check( lGroup.Deleted, 'Failed on 10' ) ;
    Check( lItem.Deleted, 'Failed on 11' ) ;

  finally
    lData.Free;
  end;

  lMap   := TtstPerObjMappedObj.Create ;
  try
    lMap.ObjectState := posClean ;
    lGroup := TtstPerObjAbs.Create ;
    lGroup.ObjectState := posClean ;
    lMap.ObjProp := lGroup ;
    lMap.Deleted := true ;
    Check( lMap.ObjectState  = posDelete, 'Failed on 12' ) ;
    // Mapped objects should not have their object state touched
    // during a delete, only owned objects should.
    Check( lGroup.ObjectState = posClean, 'Failed on 13' ) ;

  finally
    lMap.Free ;
  end ;



end;

procedure TTestPerObjAbs.Dirty;
var
  lData  : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem  : TtstPerObjOwnedObj;
begin
  lData := TtstPerObjList.Create;
  try
    Check( Not lData.Dirty, 'Failed on 1' ) ;
    lData.ObjectState := posPK;
    Check( Not lData.Dirty, 'Failed on 2' ) ;
    lData.ObjectState := posClean;
    Check( Not lData.Dirty, 'Failed on 3' ) ;
    lData.ObjectState := posCreate;
    Check( lData.Dirty, 'Failed on 4' ) ;
    lData.ObjectState := posUpdate;
    Check( lData.Dirty, 'Failed on 5' ) ;
    lData.ObjectState := posDelete;
    Check( lData.Dirty, 'Failed on 6' ) ;
    lData.ObjectState := posDeleted;
    Check( not lData.Dirty, 'Failed on 7' ) ;
    lData.ObjectState := posClean;
    Check( Not lData.Dirty, 'Failed on 8' ) ;

    lGroup := TtstPerObjAbs.Create ;
    lData.Add( lGroup ) ;
    Check( Not lData.Dirty, 'Failed on 9');
    lGroup.ObjectState := posUpdate ;
    Check( lData.Dirty, 'Failed on 10' ) ;
    lGroup.ObjectState := posClean ;

    lItem := TtstPerObjOwnedObj.Create ;
    lData.Add( lItem ) ;
    Check( Not lData.Dirty, 'Failed on 11' ) ;
    lItem.ObjectState := posUpdate ;
    Check( lData.Dirty, 'Failed on 12' ) ;
    lItem.ObjectState := posClean ;

    lItem.ObjProp.ObjectState := posUpdate ;
    Check( lData.Dirty, 'Failed on 13');

  finally
    lData.Free;
  end;

end;

procedure TTestPerObjAbs.Equals;
var
  lObj1      : TtstPerObjAbs ;
  lObj2      : TtstPerObjAbs ;
  lObj3      : TtstPerObjAbs ;
  lObj4      : TtstPerObjAbs ;
  lList1     : TtstPerObjList ;
  lList2     : TtstPerObjList ;
  lOwnedObj1 : TtstPerObjOwnedObj ;
  lOwnedObj2 : TtstPerObjOwnedObj ;
begin
  lObj1 := TtstPerObjAbs.Create ;
  try
    lObj2 := TtstPerObjAbs.Create ;
    try
      Check( lObj1.Equals( lObj2 ), 'Failed on 1' ) ;
      Check( lObj2.Equals( lObj1 ), 'Failed on 2' ) ;

      lObj2.BoolProp  := true ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 3' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 4' ) ;
      lObj2.BoolProp  := false ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 5' ) ;

      lObj2.IntProp   := 10 ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 6' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 7' ) ;
      lObj2.IntProp   := 1 ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 8' ) ;

      lObj2.FloatProp := 1.1111112 ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 9' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 10' ) ;
      lObj2.FloatProp := 1.1111111 ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 11' ) ;

      lObj2.StrProp   := 'testing, testing' ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 12' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 13' ) ;
      lObj2.StrProp   := 'testing' ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 14' ) ;

      lObj2.DateProp  := StrToDate( '01/01/2002' ) + 1 ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 15' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 16' ) ;
      lObj2.DateProp  := StrToDate( '01/01/2002' ) ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 17' ) ;

      lObj2.OrdProp   := tstOrdProp_2 ;
      Check( not lObj1.Equals( lObj2 ), 'Failed on 18' ) ;
      Check( not lObj2.Equals( lObj1 ), 'Failed on 18' ) ;
      lObj2.OrdProp   := tstOrdProp_1 ;
      Check( lObj1.Equals( lObj2 ), 'Failed on 19' ) ;

    finally
      lObj2.Free ;
    end ;
  finally
    lObj1.Free ;
  end ;

  lList1 := TtstPerObjList.Create ;
  try
    lList2 := TtstPerObjList.Create ;
    try
      Check( lList1.Equals( lList2 ), 'Failed on 20' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 21' ) ;

      lObj1 := TtstPerObjAbs.Create ;
      lList1.Add( lObj1 ) ;
      Check( not lList1.Equals( lList2 ), 'Failed on 22' ) ;
      Check( not lList2.Equals( lList1 ), 'Failed on 23' ) ;

      lObj2 := TtstPerObjAbs.Create ;
      lList2.Add( lObj2 ) ;
      Check( lList1.Equals( lList2 ), 'Failed on 24' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 25' ) ;

      lObj3 := TtstPerObjAbs.Create ;
      lList1.Add( lObj3 ) ;
      Check( not lList1.Equals( lList2 ), 'Failed on 26' ) ;
      Check( not lList2.Equals( lList1 ), 'Failed on 27' ) ;

      lObj4 := TtstPerObjAbs.Create ;
      lList2.Add( lObj4 ) ;
      Check( lList1.Equals( lList2 ), 'Failed on 28' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 29' ) ;

      lObj1.StrProp := 'hos' ;
      Check( not lList1.Equals( lList2 ), 'Failed on 30' ) ;
      Check( not lList2.Equals( lList1 ), 'Failed on 31' ) ;

      lObj1.Populate ;
      Check( lList1.Equals( lList2 ), 'Failed on 32' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 33' ) ;

      lObj1.OID.AsString := '1' ;
      lObj2.OID.AsString := '2' ;
      Check( lList1.Equals( lList2 ), 'Failed on 34' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 35' ) ;
      lObj1.Populate ;
      lObj2.Populate ;

      lObj1.ObjectState := posEmpty ;
      lObj2.ObjectState := posClean ;
      Check( lList1.Equals( lList2 ), 'Failed on 36' ) ;
      Check( lList2.Equals( lList1 ), 'Failed on 37' ) ;

    finally
      lList2.Free ;
    end ;
  finally
    lList1.Free ;
  end ;

  lOwnedObj1 := TtstPerObjOwnedObj.Create ;
  try
    lOwnedObj2 := TtstPerObjOwnedObj.Create ;
    try
      Check( lOwnedObj1.Equals( lOwnedObj2 ), 'Failed on 38' ) ;
      Check( lOwnedObj2.Equals( lOwnedObj1 ), 'Failed on 39' ) ;

      lOwnedObj1.ObjProp.StrProp := 'HOS' ;
      Check( not lOwnedObj1.Equals( lOwnedObj2 ), 'Failed on 40' ) ;
      Check( not lOwnedObj2.Equals( lOwnedObj1 ), 'Failed on 41' ) ;
      lOwnedObj1.ObjProp.Populate ;
      Check( lOwnedObj1.Equals( lOwnedObj2 ), 'Failed on 42' ) ;
      Check( lOwnedObj2.Equals( lOwnedObj1 ), 'Failed on 43' ) ;

    finally
      lOwnedObj2.Free ;
    end ;
  finally
    lOwnedObj1.Free ;
  end ;

end;

procedure TTestPerObjAbs.FindAllWithMethod;
var
  lData  : TtstPerObjList ;
  lList : TList ;
  i : integer ;
begin
  lData := CreateTestDataList ;
  try
    lList := TList.Create ;
    try
      lData.FindAll( TstFindAll, lList ) ;
      CheckEquals( 6, lList.Count, 'Failed on count' ) ;
      for i := 0 to 5 do
        CheckEquals( IntToStr(i), TtstPerObjAbs( lList.Items[i] ).OID.AsString, 'Failed on OID' ) ;
    finally
      lList.Free ;
    end ;
  finally
    lData.Free ;
  end ;
end;

procedure TTestPerObjAbs.FindByOID;
var
  lData  : TtstPerObjList ;
  lToFind : TPerObjAbs ;
  lFound : TPerObjAbs ;
  i : integer ;
begin
  lData := CreateTestData ;
  try
    lToFind := lData ;
    lFound := lData.Find( lToFind.OID ) ;
    CheckSame( lToFind, lFound, 'Failed on 1' ) ;
    for i := 0 to lData.Count - 1 do
    begin
      lToFind := lData.Items[i];
      lFound  := lData.Find( lToFind.OID ) ;
      CheckSame( lToFind, lFound, 'Failed on 2' ) ;
      lToFind := (lData.Items[i] as TtstPerObjOwnedObj).ObjProp;
      lFound  := lData.Find( lToFind.OID ) ;
      CheckSame( lToFind, lFound, 'Failed on 3' ) ;
    end ;
  finally
    lData.Free ;
  end ;
end;

function TTestPerObjAbs.CreateTestDataList : TtstPerObjList;
var
  lItem  : TtstPerObjOwnedObj ;
  i      : integer ;
begin
  result := TtstPerObjList.Create ;
  SetTSTPerObjList( Result, -1 ) ;
  for i := 0 to 4 do
  begin
    lItem := TtstPerObjOwnedObj.Create ;
    Result.Add( lItem ) ;
    SetTSTPerObjAbs( lItem, i*2 ) ;
    SetTSTPerObjAbs( lItem.ObjProp, i*2+1 ) ;
  end ;
end ;

procedure TTestPerObjAbs.FindWithMethod;
var
  lData  : TtstPerObjList ;
  lFound : TtstPerObjAbs ;
begin
  lData := CreateTestDataList ;
  try
    lFound := lData.Find( TstFindMethod ) as TtstPerObjAbs ;
    Check( Assigned( lFound ), 'Find failed' ) ;
    CheckEquals( '1', lFound.OID.AsString, 'Find failed' ) ;
  finally
    lData.Free ;
  end ;
end;

procedure TTestPerObjAbs.TstFindMethod(pPerObjAbs: TPerObjAbs; var pbFound: boolean);
begin
  pbFound := false ;
  if not ( pPerObjAbs is TtstPerObjAbs ) then
    Exit ; //==>
  pbFound := TtstPerObjAbs( pPerObjAbs ).IntProp = 1 ;
end;

procedure TTestPerObjAbs.FindWithMethodAndParam;
var
  lData  : TtstPerObjList ;
  lFound : TtstPerObjAbs ;
begin
  lData := CreateTestDataList ;
  try
    lFound := lData.Find( TstFindMethodWithParam, Pointer( 1 )) as TtstPerObjAbs ;
    Check( Assigned( lFound ), 'Find failed' ) ;
    CheckEquals( '1', lFound.OID.AsString, 'Find failed' ) ;

    lFound := lData.Find( TstFindMethodWithParam, Pointer( 100 )) as TtstPerObjAbs ;
    Check( not Assigned( lFound ), 'Found when it shoud have failed' ) ;

  finally
    lData.Free ;
  end ;
end;

procedure TTestPerObjAbs.Index;
var
  lData  : TtstPerObjList;
  lItem0 : TtstPerObjAbs;
  lItem1 : TtstPerObjAbs;
  lItem2 : TtstPerObjAbs;
  lItem3: TtstPerObjAbs;
begin
  lData := TtstPerObjList.Create;
  try
    lData.ObjectState := posClean ;
    lItem0 := TtstPerObjAbs.Create ;
    try
      lItem0.ObjectState := posClean ;
      lItem0.Index ;
      Check(false, 'Exception not raised');
    except
      on e:exception do
        CheckIs( e, Exception, 'Exception of the correct class not raised' ) ;
    end ;
    lData.Add( lItem0 ) ;
    CheckEquals( 0, lItem0.Index ) ;
    lItem1 := TtstPerObjAbs.Create ;
    lItem1.ObjectState := posClean ;
    lData.Add( lItem1 ) ;
    CheckEquals( 1, lItem1.Index ) ;
    lItem2 := TtstPerObjAbs.Create ;
    lItem2.ObjectState := posClean ;
    lData.Insert( lItem1, lItem2 ) ;

    CheckEquals( 0, lItem0.Index ) ;
    CheckEquals( 1, lItem2.Index ) ;
    CheckEquals( 2, lItem1.Index ) ;

    lItem3 := TtstPerObjAbs.Create ;
    lItem3.ObjectState := posClean ;
    lData.Insert( 2, lItem3 ) ;
    CheckEquals( 0, lItem0.Index ) ;
    CheckEquals( 1, lItem2.Index ) ;
    CheckEquals( 2, lItem3.Index ) ;
    CheckEquals( 3, lItem1.Index ) ;

  finally
    lData.Free;
  end;
end;

procedure TTestPerObjAbs.ObjectStateAsString;
var
  lItem  : TtiOPFTestItem ;
begin
  lItem  := TtiOPFTestItem.Create ;
  try
    CheckEquals( 'posEmpty', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posPK;
    CheckEquals( 'posPK', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posCreate;
    CheckEquals( 'posCreate', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posUpdate;
    CheckEquals( 'posUpdate', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posDelete;
    CheckEquals( 'posDelete', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posDeleted;
    CheckEquals( 'posDeleted', lItem.ObjectStateAsString ) ;
    lItem.ObjectState := posClean;
    CheckEquals( 'posClean', lItem.ObjectStateAsString ) ;
  finally
    lItem.Free ;
  end ;
end;

procedure TTestPerObjAbs.Owner;
var
  lGroup : TtiOPFTestGroup ;
  lItem  : TtiOPFTestItem ;
begin
  // This is really a bit trivial, but here goes anyway...
  lGroup := TtiOPFTestGroup.Create;
  try
    lItem  := TtiOPFTestItem.Create ;
    try
      CheckNull( lItem.Owner ) ;
      lItem.Owner := lGroup ;
      CheckNotNull( lItem.Owner ) ;
      Check( lItem.Owner = lGroup ) ;
      lItem.Owner := nil ;
      CheckNull( lItem.Owner ) ;
    finally
      lItem.Free ;
    end ;
  finally
    lGroup.Free ;
  end;
end;

procedure TTestPerObjAbs.TopOfHierarchy;
var
  lData  : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem  : TtstPerObjOwnedObj;
begin
  lData := TtstPerObjList.Create;
  try
    lGroup := TtstPerObjAbs.Create ;
    lData.Add( lGroup ) ;
    lItem := TtstPerObjOwnedObj.Create ;
    lData.Add( lItem ) ;
    Check( lData.TopOfHierarchy = lData, 'Failed on 1' ) ;
    Check( lGroup.TopOfHierarchy = lData, 'Failed on 2' ) ;
    Check( lItem.TopOfHierarchy = lData, 'Failed on 3' ) ;

  finally
    lData.Free;
  end;
end;

procedure TTestPerObjAbs.IsUnique;
var
  lTop : TtstPerObjList ;
  lItem : TtstPerObjAbs ;
begin
  lTop := TtstPerObjList.Create ;
  try
    lTop.OID.AsString := '1' ;
    Check( lTop.IsUnique( lTop ), 'Failed on test 1' ) ;
    lItem := TtstPerObjAbs.Create ;
    lItem.OID.AsString := '1' ;
    Check( not lTop.IsUnique( lItem ), 'Failed on test 2' ) ;
    lTop.Add( lItem ) ;
    Check( not lTop.IsUnique( lItem ), 'Failed on test 3' ) ;
    lItem := TtstPerObjAbs.Create ;
    lItem.OID.AsString := '2' ;
    Check( lTop.IsUnique( lItem ), 'Failed on test 4' ) ;
    lTop.Add( lItem ) ;
    Check( lTop.IsUnique( lItem ), 'Failed on test 5' ) ;
  finally
    lTop.Free ;
  end ;
end;

procedure TTestPerObjAbs.CloneCompound;
var
  lFrom : TtstPerObjOwnedObjCanAssign ;
  lTo   : TtstPerObjOwnedObjCanAssign ;
begin

  lFrom := TtstPerObjOwnedObjCanAssign.Create ;
  try
    SetTSTPerObjAbs( lFrom, 1 ) ;
    SetTSTPerObjAbs( lFrom.ObjProp, 2 ) ;
    lTo   := lFrom.Clone ;
    try
      CheckTSTPerObjAbs( lFrom, 1 ) ;
      CheckTSTPerObjAbs( lFrom.ObjProp, 2 ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;

end;

procedure TTestPerObjAbs.CloneList;
var
  lFrom : TtstPerObjList ;
  lItem : TtstPerObjAbs ;
  lTo   : TtstPerObjList ;
begin
  lFrom := TtstPerObjList.Create ;
  try
    lFrom.OwnsObjects := true ;
    SetTSTPerObjList( lFrom, 1 ) ;
    lItem := TtstPerObjAbs.Create ;
    SetTSTPerObjAbs( lItem, 2 ) ;
    lFrom.Add( lItem ) ;
    lTo   := lFrom.Clone ;
    try
      CheckTSTPerObjList( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
      CheckEquals( 1, lTo.Count, 'Failed on Count' ) ;
      CheckTSTPerObjAbs( lTo.Items[0], 2 ) ;
      CheckSame( lTo, lTo.Items[0].Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;

  lFrom := TtstPerObjList.Create ;
  try
    lFrom.OwnsObjects := false ;
    lFrom.AutoSetItemOwner := false ;
    SetTSTPerObjList( lFrom, 1 ) ;
    lItem := TtstPerObjAbs.Create ;
    SetTSTPerObjAbs( lItem, 2 ) ;
    lFrom.Add( lItem ) ;
    lTo   := lFrom.Clone ;
    try
      CheckTSTPerObjList( lTo, 1 ) ;
      Check( posEmpty = lTo.ObjectState, 'Failed on ObjectState' ) ;
      CheckEquals( true, lTo.SelfIterate,'Failed on SelfIterate' ) ;
      Check( nil = lTo.Owner, 'Failed on Owner' ) ;
      CheckEquals( 1, lTo.Count, 'Failed on Count' ) ;
      CheckTSTPerObjAbs( lTo.Items[0], 2 ) ;
      // This test will fail because lTo.AutoSetItemOwner must be set
      // and there is no opportunity to do this because Clone is being
      // called.
      // CheckSame( lFrom, lTo.Items[0].Owner, 'Failed on Owner' ) ;
    finally
      lTo.Free ;
    end ;
  finally
    lFrom.Free ;
  end ;
end;

procedure TTestPerObjAbs.TstFindMethodWithParam(pPerObjAbs: TPerObjAbs;
  var pbFound: boolean; pUserContext: Pointer);
begin
  pbFound := false ;
  if not ( pPerObjAbs is TtstPerObjAbs ) then
    Exit ; //==>
  pbFound := TtstPerObjAbs( pPerObjAbs ).IntProp = Integer( pUserContext ) ;
end;

procedure TTestPerObjAbs.TstFindAll( pPerObjAbs : TPerObjAbs ; var pbFound : boolean ) ;
begin
  pbFound := false ;
  if not ( pPerObjAbs is TtstPerObjAbs ) then
    Exit ; //==>
  pbFound := TtstPerObjAbs( pPerObjAbs ).IntProp <= 5 ;
end;

procedure TTestPerObjAbs.IsValid;
var
  lData : TtstPerObjAbs ;
  lErrors : TPerObjErrors ;
begin
  lData := TtstPerObjAbs.Create ;
  try
    lData.StrProp := '' ;
    lData.IntProp := 0 ;
    lData.FloatProp := 0 ;
    lErrors := TPerObjErrors.Create ;
    try
      Check( not lData.IsValid, 'Valid passed when it should have failed' ) ;
      lData.IsValid( lErrors ) ;
      CheckEquals( 3, lErrors.Count, 'Wrong number of errors returned' ) ;
      CheckEquals( cStrPropErrorMsg,    lErrors.Items[0].ErrorMessage, 'Failed on test 1' ) ;
      CheckEquals( cStrPropErrorCode,   lErrors.Items[0].ErrorCode, 'Failed on test 2' ) ;
      CheckEquals( cIntPropErrorMsg,    lErrors.Items[1].ErrorMessage, 'Failed on test 3' ) ;
      CheckEquals( cIntpropErrorCode,   lErrors.Items[1].ErrorCode, 'Failed on test 4' ) ;
      CheckEquals( cFloatPropErrorMsg,  lErrors.Items[2].ErrorMessage, 'Failed on test 5' ) ;
      CheckEquals( cFloatPropErrorCode, lErrors.Items[2].ErrorCode, 'Failed on test 6' ) ;

      CheckNotNull(   lErrors.FindByMessage( cStrPropErrorMsg )) ;
      CheckEquals( cStrPropErrorMsg, lErrors.FindByMessage( cStrPropErrorMsg ).ErrorMessage ) ;
      CheckNotNull(   lErrors.FindByMessage( cIntPropErrorMsg )) ;
      CheckEquals( cIntPropErrorMsg, lErrors.FindByMessage( cIntPropErrorMsg ).ErrorMessage ) ;
      CheckNotNull(   lErrors.FindByMessage( cFloatPropErrorMsg )) ;
      CheckEquals( cFloatPropErrorMsg, lErrors.FindByMessage( cFloatPropErrorMsg ).ErrorMessage ) ;

      lData.StrProp := 'test' ;
      lData.IntProp := 1 ;
      lData.FloatProp := 1.1 ;
      Check( lData.IsValid, 'Valid failed when it should have passed' ) ;
      lData.IsValid( lErrors ) ;
      CheckEquals( 0, lErrors.Count, 'Wrong number of errors returned' ) ;
    finally
      lErrors.Free ;
    end ;

  finally
    lData.Free ;
  end ;

end;

procedure TTestPerObjAbs.SetPropValue;
var
  lData : TtstPerObjAbs ;
begin

  lData := TtstPerObjAbs.Create ;
  try

    lData.PropValue['StrProp'] := 'test' ;
    CheckEquals( 'test', lData.StrProp, 'StrProp' ) ;

    lData.PropValue['IntProp'] := 1234 ;
    CheckEquals( 1234, lData.IntProp, 'IntProp' ) ;

    lData.PropValue['FloatProp'] := 1234.5678 ;
    CheckEquals( 1234.5678, lData.FloatProp, cDUnitTestFloatPrecision, 'FloatProp' ) ;

    lData.PropValue['DateProp'] := EncodeDate( 2003, 01, 01 ) ;
    CheckEquals( EncodeDate( 2003, 01, 01 ), lData.DateProp, 0.00001, 'DateProp' ) ;

    lData.PropValue['BoolProp'] := true ;
    CheckEquals( true, lData.BoolProp, 'BoolProp true' ) ;
    lData.PropValue['BoolProp'] := false ;
    CheckEquals( false, lData.BoolProp, 'BoolProp false' ) ;

    lData.PropValue['BoolProp'] := 'true' ;
    CheckEquals( true, lData.BoolProp, 'BoolProp ''true''' ) ;
    lData.PropValue['BoolProp'] := 'false' ;
    CheckEquals( false, lData.BoolProp, 'BoolProp ''false''' ) ;

    lData.PropValue['BoolProp'] := 'TRUE' ;
    CheckEquals( true, lData.BoolProp, 'BoolProp ''TRUE''' ) ;
    lData.PropValue['BoolProp'] := 'FALSE' ;
    CheckEquals( false, lData.BoolProp, 'BoolProp ''FALSE''' ) ;

    lData.PropValue['BoolProp'] := 'True' ;
    CheckEquals( true, lData.BoolProp, 'BoolProp ''True''' ) ;
    lData.PropValue['BoolProp'] := 'False' ;
    CheckEquals( false, lData.BoolProp, 'BoolProp ''False''' ) ;

    lData.PropValue['BoolProp'] := 'T' ;
    CheckEquals( true, lData.BoolProp, 'BoolProp ''T''' ) ;
    lData.PropValue['BoolProp'] := 'F' ;
    CheckEquals( false, lData.BoolProp, 'BoolProp ''F''' ) ;

    lData.PropValue['BoolProp'] := 1 ;
    CheckEquals( true, lData.BoolProp, 'BoolProp 1' ) ;
    lData.PropValue['BoolProp'] := 0 ;
    CheckEquals( false, lData.BoolProp, 'BoolProp 0' ) ;

    lData.PropValue['BoolProp'] := '1' ;
    CheckEquals( true, lData.BoolProp, 'BoolProp ''1''' ) ;
    lData.PropValue['BoolProp'] := '0' ;
    CheckEquals( false, lData.BoolProp, 'BoolProp ''0''' ) ;

    lData.PropValue['OrdProp'] := tstOrdProp_2 ;
    Check( tstOrdProp_2 = lData.OrdProp, 'OrdProp' ) ;

  finally
    lData.Free ;
  end ;

end;

procedure TTestPerObjAbs.GetPropValue;
var
  lData : TtstPerObjAbs ;
  lOrd  : String ;
  lStr : string ;      
  lInt : integer ;
  lFloat : real ;
begin

  lData := TtstPerObjAbs.Create ;
  try

    lData.StrProp := 'test' ;
    lStr := lData.PropValue['StrProp'] ;
    CheckEquals( 'test', lStr, 'StrProp' ) ;

    lData.IntProp := 1234 ;
    lInt := lData.PropValue['IntProp'] ;
    CheckEquals( 1234, lInt, 'IntProp' ) ;

    lData.FloatProp := 1234.5678 ;
    lFloat := lData.PropValue['FloatProp'] ;
    CheckEquals( 1234.5678, lFloat, cDUnitTestFloatPrecision, 'FloatProp' ) ;

    lData.DateProp := EncodeDate( 2003, 01, 01 ) ;
    CheckEquals( EncodeDate( 2003, 01, 01 ), lData.PropValue['DateProp'], 0.00001, 'DateProp' ) ;

    lData.BoolProp := true ;
    CheckEquals( true, lData.PropValue['BoolProp'], 'BoolProp' ) ;

    // This is a bit of a mess, but it was the
    // best I could come up with to get PropValue working
    // with ordinals.
    lData.OrdProp := tstOrdProp_2 ;
    lOrd := lData.PropValue['OrdProp'] ;
    CheckEquals( 'tstOrdProp_2',
      GetEnumName( TypeInfo( TtstOrdProp ),
                   ord( tstOrdProp_2 )),
      'OrdProp' ) ;

  finally
    lData.Free ;
  end ;
end;

procedure TTestPerObjAbs.IsReadWriteProp;
var
  lData : TtstPerObjAbs ;
begin

  lData := TtstPerObjAbs.Create ;
  try

    Check( lData.IsReadWriteProp('StrProp'),   'StrProp') ;
    Check( lData.IsReadWriteProp('IntProp'),   'IntProp') ;
    Check( lData.IsReadWriteProp('FloatProp'), 'FloatProp') ;
    Check( lData.IsReadWriteProp('DateProp'),  'DateProp') ;
    Check( lData.IsReadWriteProp('BoolProp'),  'BoolProp') ;
    Check( lData.IsReadWriteProp('OrdProp'),   'OrdProp') ;
    Check( lData.IsReadWriteProp('OID'),       'OID') ;
    Check( lData.IsReadWriteProp('DispOrder'), 'DispOrder') ;
    Check( not lData.IsReadWriteProp('StrPropReadOnly'), 'StrPropReadOnly') ;

  finally
    lData.Free ;
  end ;

end;

procedure TTestPerObjAbsCreatNew.CreateNew_OIDGUID;
var
  lItem : TPerObjAbs ;
begin
  gTIPerMgr.DefaultOIDClassName := cOIDClassNameGUID ;
  lItem := TPerObjAbs.CreateNew ;
  try
    CheckIs( lItem.OID, TOIDGUID, 'OID not a TOIDGUID' ) ;
    CheckEquals( 36, Length( lItem.OID.AsString ), 'OID does not look like a GUID' ) ;
  finally
    lItem.Free;
  end ;
end;

constructor TTestPerObjAbsCreatNew.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [sutPerLayer, sutDBConnection];
end;

{ TTestPerObjList }

procedure TTestPerObjList.Add;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.List.IndexOf( lData ), 'Failed on ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.AutoSetItemOwner;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    CheckSame( lList, lData1.Owner, 'Failed on test 1' ) ;
    lList.AutoSetItemOwner := false ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData2 ) ;
    CheckNull( lData2.Owner, 'Failed on test 2' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.PropToStrings;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lsl         : TStringList ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData2 := TtstPerObjAbs.Create ;
    lData3 := TtstPerObjAbs.Create ;
    lData1.StrProp := '1' ;
    lData2.StrProp := '2' ;
    lData3.StrProp := '3' ;
    lList.Add( lData1 ) ;
    lList.Add( lData2 ) ;
    lList.Add( lData3 ) ;
    lsl := TStringList.Create ;
    try
      lList.PropToStrings( lsl, 'StrProp' );
      CheckEquals( 3,    lsl.Count, 'Failed on lsl.Count' ) ;
      CheckEquals( '1',  lsl.Strings[0], 'Failed on 1' ) ;
      CheckSame( lData1, lsl.Objects[0], 'Failed on 1.Objects' ) ;
      CheckEquals( '2',  lsl.Strings[1], 'Failed on 2' ) ;
      CheckSame( lData2, lsl.Objects[1], 'Failed on 1.Objects' ) ;
      CheckEquals( '3',  lsl.Strings[2], 'Failed on 3' ) ;
      CheckSame( lData3, lsl.Objects[2], 'Failed on 1.Objects' ) ;
    finally
      lsl.Free ;
    end ;

  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.Clear;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData );
    CheckEquals( 1, lList.Count, 'Count' ) ;
    lList.Clear ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
    try
      lData.Caption ;
      Fail( 'lData exists but is should have been freed' ) ;
    except
      on e:exception do
        //OK
    end ;

    lList.OwnsObjects := false ;
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData );
    CheckEquals( 1, lList.Count, 'Count' ) ;
    lList.Clear ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
    try
      lData.Caption ;
    except
      on e:exception do
        Fail( 'lData was freed when it should not have been' ) ;
    end ;

  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.Count;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    CheckEquals( 0, lList.Count, 'Failed on 0' ) ;
    for i := 1 to 10 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.Count, 'Failed on ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.CountNotDeleted;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    CheckEquals( 0, lList.Count, 'Failed on 0' ) ;
    for i := 1 to 10 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.CountNotDeleted, 'Failed on ' + IntToStr( i )) ;
    end ;
    for i := 0 to 9 do
    begin
      lList.Items[i].Deleted := true ;
      CheckEquals( 9-i, lList.CountNotDeleted, 'Failed on ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

function TTestPerObjList.CreateList: TtstPerObjList;
var
  xItem : TtstPerObjAbs;
begin
  result:=TtstPerObjList.Create;
  // 1
  xItem:=TtstPerObjAbs.Create;
  xItem.OID.AsString:='1';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=1;
    FFloatProp:=1.23;
    FStrProp:='123';
    FDateProp:=EncodeDate(2000,01,02)+EncodeTime(3,4,5,6);
  end;
  result.Add(xItem);
  // 2
  xItem:=TtstPerObjAbs.Create;
  xItem.OID.AsString:='2';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=-4942345;
    FFloatProp:=4.56;
    FStrProp:='456';
    FDateProp:=EncodeDate(2000,02,03)+EncodeTime(4,5,6,7);
  end;
  result.Add(xItem);
  // 3
  xItem:=TtstPerObjAbs.Create;
  xItem.OID.AsString:='3';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=394554;
    FFloatProp:=123.456;
    FStrProp:='TEST VALUE';
    FDateProp:=EncodeDate(1999,02,06)+EncodeTime(20,0,0,0);
  end;
  result.Add(xItem);
  // 4
  xItem:=TtstPerObjAbs.Create;
  xItem.OID.AsString:='4';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=19867;
    FFloatProp:=235465.34321;
    FStrProp:='WHY USE WINDOWS WHEN THERE IS A DOOR?'; //:))
    FDateProp:=0;
  end;
  result.Add(xItem);
  // 5
  xItem:=TtstPerObjAbs.Create;
  xItem.OID.AsString:='5';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=936;
    FFloatProp:=345/678;
    FStrProp:='HOW IT WORKS?';
    FDateProp:=EncodeDate(2004,05,12);
  end;
  result.Add(xItem);
end;

procedure TTestPerObjList.Delete;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.IndexOf( lData ), 'Failed on add ' + IntToStr( i )) ;
    end ;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i] ;
      lList.Delete( i ) ;
      CheckEquals( -1, lList.IndexOf( lData ), 'Failed on delete ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.Empty;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData );
    lList.Empty ;
    try
      lData.Caption ;
    except
      on e:exception do
        Fail( 'lData was freed when it should not have been' ) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.Extract;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.IndexOf( lData ), 'Failed on add ' + IntToStr( i )) ;
    end ;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i] ;
      lList.Extract( lData ) ;
      if i > 0 then
        CheckEquals( i, lList.Count, 'Failed on Extract (Count) ' + IntToStr( i ))
      else
        CheckEquals( 0, lList.Count, 'Failed on Extract (Count) ' + IntToStr( i )) ;
      CheckEquals( -1, lList.IndexOf( lData ), 'Failed on Extract (IndexOf) ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.FindByProps_DirectValue;
var
  xList : TtstPerObjList;
  xItem : TtstPerObjAbs;
begin
  xList:=CreateList;
  try
    // by boolean
    CheckEquals(5,xList.Count,'Wrong list! - Wrong count!');
    xItem:=TtstPerObjAbs(xList.FindByProps(['BoolProp'],[true]));
    CheckNotNull(xItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',xItem.OID.AsString,'Find By Boolean value - wrong object!');
    xItem:=TtstPerObjAbs(xList.FindByProps(['BoolProp'],[false]));
    CheckNull(xItem,'Find By Boolean value - result when not expected');
    // by int
    xItem:=TtstPerObjAbs(xList.FindByProps(['IntProp'],[-4942345]));
    CheckNotNull(xItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',xItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    xItem:=TtstPerObjAbs(xList.FindByProps(['IntProp'],[7878787]));
    CheckNull(xItem,'Find By Integer value - result when not expected 7878787');
    // by string
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],['TEST VALUE']));
    CheckNotNull(xItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',xItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],['Test Value'],false));
    CheckNotNull(xItem,'Find By String value (not case sensitive) - NO result when expected <Test Value>');
    CheckEquals('3',xItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test Value>');
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],['Test Value'],true));
    CheckNull(xItem,'Find By String value (case sensitive) - result when not expected <Test Value>');
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],['_Test Value_'],false));
    CheckNull(xItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');
    // float
    xItem:=TtstPerObjAbs(xList.FindByProps(['FloatProp'],[235465.34321]));
    CheckNotNull(xItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',xItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    xItem:=TtstPerObjAbs(xList.FindByProps(['FloatProp'],[235465.3432]));
    CheckNull(xItem,'Find By Float value - result when not expected 235465.3432');
    // by Date
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[EncodeDate(2004,05,12)]));
    CheckNotNull(xItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',xItem.OID.AsString,'Find By Date value - wrong object!');
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[0]));
    CheckNotNull(xItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',xItem.OID.AsString,'Find By Date value - wrong object!');
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[EncodeDate(2004,05,13)]));
    CheckNull(xItem,'Find By Date value 2004-05-13 - result when not expected');
    // by DateTime
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[EncodeDate(2000,02,03)+EncodeTime(4,5,6,7)]));
    CheckNotNull(xItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',xItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[EncodeDate(1999,02,06)+EncodeTime(20,0,0,1)]));
    CheckNull(xItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    xList.Free;
  end;
end;

procedure TTestPerObjList.FindByProps_TypedValue;
var
  xList : TtstPerObjList;
  xItem : TtstPerObjAbs;
  xInt : integer;
  xDate : TDateTime;
  xStr : string;
  xFloat : real;
  xBool : boolean;
begin
  xList:=CreateList;
  try
    // by boolean
    CheckEquals(5,xList.Count,'Wrong list! - Wrong count!');
    xBool:=true;
    xItem:=TtstPerObjAbs(xList.FindByProps(['BoolProp'],[xBool]));
    CheckNotNull(xItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',xItem.OID.AsString,'Find By Boolean value - wrong object!');
    xBool:=false;
    xItem:=TtstPerObjAbs(xList.FindByProps(['BoolProp'],[xBool]));
    CheckNull(xItem,'Find By Boolean value - result when not expected');
    // by int
    xInt:=-4942345;
    xItem:=TtstPerObjAbs(xList.FindByProps(['IntProp'],[xInt]));
    CheckNotNull(xItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',xItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    xInt:=7878787;
    xItem:=TtstPerObjAbs(xList.FindByProps(['IntProp'],[xInt]));
    CheckNull(xItem,'Find By Integer value - result when not expected 7878787');
    // by string
    xStr:='TEST VALUE';
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],[xStr]));
    CheckNotNull(xItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',xItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');

    xStr:='Test Value';
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],[xStr],false));
    CheckNotNull(xItem,'Find By String value (not case sensitive) - NO result when expected <Test Value>');
    CheckEquals('3',xItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test Value>');
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],[xStr],true));
    CheckNull(xItem,'Find By String value (case sensitive) - result when not expected <Test Value>');

    xStr:='_Test Value_';
    xItem:=TtstPerObjAbs(xList.FindByProps(['StrProp'],[xStr],false));
    CheckNull(xItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');

    // float
    xFloat:=235465.34321;
    xItem:=TtstPerObjAbs(xList.FindByProps(['FloatProp'],[xFloat]));
    CheckNotNull(xItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',xItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    xFloat:=235465.3432;
    xItem:=TtstPerObjAbs(xList.FindByProps(['FloatProp'],[xFloat]));
    CheckNull(xItem,'Find By Float value - result when not expected 235465.3432');

    // by Date
    xDate:=EncodeDate(2004,05,12);
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[xDate]));
    CheckNotNull(xItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',xItem.OID.AsString,'Find By Date value - wrong object!');

    xDate:=0;
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[xDate]));
    CheckNotNull(xItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',xItem.OID.AsString,'Find By Date value - wrong object!');

    xDate:=EncodeDate(2004,05,13);
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[xDate]));
    CheckNull(xItem,'Find By Date value 2004-05-13 - result when not expected');

    // by DateTime
    xDate:=EncodeDate(2000,02,03)+EncodeTime(4,5,6,7);
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[xDate]));
    CheckNotNull(xItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',xItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');

    xDate:=EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
    xItem:=TtstPerObjAbs(xList.FindByProps(['DateProp'],[xDate]));
    CheckNull(xItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    xList.Free;
  end;
end;

procedure TTestPerObjList.First;
var
  lList : TtstPerObjList ;
  lData0 : TtstPerObjAbs ;
  lData1 : TtstPerObjAbs ;
  lData2 : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData0 := TtstPerObjAbs.Create ;
    lList.Add( lData0 ) ;
    lData1 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData2 ) ;
    CheckSame( lData0, lList.First, 'Failed on First' ) ;
    lData0.Deleted := true ;
    // This test will fail. Should it?
    // CheckSame( lData1, lList.First, 'Failed on First when the first was deleted' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.DoForEachMethod( pData : TPerObjAbs ) ;
begin
  Assert( pData is TtstPerObjAbs, 'pData not a TtstPerObjAbs' ) ;
  TtstPerObjAbs( pData ).StrProp := 'tested' ;
end ;

procedure TTestPerObjList.ForEachMethod;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData2 := TtstPerObjAbs.Create ;
    lData3 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lList.Add( lData2 ) ;
    lList.Add( lData3 ) ;
    lList.ForEach( DoForEachMethod );
    CheckEquals( 'tested', lList.Items[0].StrProp, 'Failed on 1' ) ;
    CheckEquals( 'tested', lList.Items[1].StrProp, 'Failed on 2' ) ;
    CheckEquals( 'tested', lList.Items[2].StrProp, 'Failed on 3' ) ;
  finally
    lList.Free;
  end;
end;

procedure DoForEachMethodRegular( pData : TPerObjAbs ) ;
begin
  Assert( pData is TtstPerObjAbs, 'pData not a TtstPerObjAbs' ) ;
  TtstPerObjAbs( pData ).StrProp := 'tested' ;
end ;

procedure TTestPerObjList.ForEachMethodRegular;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData2 := TtstPerObjAbs.Create ;
    lData3 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lList.Add( lData2 ) ;
    lList.Add( lData3 ) ;
    lList.ForEach( DoForEachMethodRegular );
    CheckEquals( 'tested', lList.Items[0].StrProp, 'Failed on 1' ) ;
    CheckEquals( 'tested', lList.Items[1].StrProp, 'Failed on 2' ) ;
    CheckEquals( 'tested', lList.Items[2].StrProp, 'Failed on 3' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.IndexOf;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData2 ) ;
    CheckEquals( 0, lList.IndexOf( lData1 ), 'Failed on test 1' ) ;
    CheckEquals( 1, lList.IndexOf( lData2 ), 'Failed on test 2' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.InsertByObject;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lData4      : TtstPerObjAbs ;
  lData5      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData1.ObjectState := posClean ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lData2.ObjectState := posClean ;
    lList.Add( lData2 ) ;
    lData3 := TtstPerObjAbs.Create ;
    lData3.ObjectState := posClean ;
    lData4 := TtstPerObjAbs.Create ;
    lData4.ObjectState := posClean ;
    lData5 := TtstPerObjAbs.Create ;
    lData5.ObjectState := posClean ;
    // Test inserting before the last element
    lList.Insert( lData2, lData3 );
    CheckSame( lData1, lList.Items[0], 'Failed on test 1' ) ;
    CheckSame( lData2, lList.Items[2], 'Failed on test 2' ) ;
    CheckSame( lData3, lList.Items[1], 'Failed on test 3' ) ;

    // Test inserting to position 0
    lList.Insert( lData1, lData4 );
    CheckSame( lData4, lList.Items[0], 'Failed on test 4' ) ;
    CheckSame( lData1, lList.Items[1], 'Failed on test 5' ) ;
    CheckSame( lData2, lList.Items[3], 'Failed on test 6' ) ;
    CheckSame( lData3, lList.Items[2], 'Failed on test 7' ) ;

    // Test inserting in the middle
    lList.Insert( lData1, lData5 );
    CheckSame( lData4, lList.Items[0], 'Failed on test 8' ) ;
    CheckSame( lData5, lList.Items[1], 'Failed on test 9' ) ;
    CheckSame( lData1, lList.Items[2], 'Failed on test 10' ) ;
    CheckSame( lData2, lList.Items[4], 'Failed on test 11' ) ;
    CheckSame( lData3, lList.Items[3], 'Failed on test 12' ) ;

  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.InsertByIndex;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lData4      : TtstPerObjAbs ;
  lData5      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData1.ObjectState := posClean ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lData2.ObjectState := posClean ;
    lList.Add( lData2 ) ;
    lData3 := TtstPerObjAbs.Create ;
    lData3.ObjectState := posClean ;
    lData4 := TtstPerObjAbs.Create ;
    lData4.ObjectState := posClean ;
    lData5 := TtstPerObjAbs.Create ;
    lData5.ObjectState := posClean ;
    // Test inserting before the last element
    lList.Insert( 1, lData3 );
    CheckSame( lData1, lList.Items[0], 'Failed on test 1' ) ;
    CheckSame( lData2, lList.Items[2], 'Failed on test 2' ) ;
    CheckSame( lData3, lList.Items[1], 'Failed on test 3' ) ;

    // Test inserting to position 0
    lList.Insert( 0, lData4 );
    CheckSame( lData4, lList.Items[0], 'Failed on test 4' ) ;
    CheckSame( lData1, lList.Items[1], 'Failed on test 5' ) ;
    CheckSame( lData2, lList.Items[3], 'Failed on test 6' ) ;
    CheckSame( lData3, lList.Items[2], 'Failed on test 7' ) ;

    // Test inserting in the middle
    lList.Insert( 1, lData5 );
    CheckSame( lData4, lList.Items[0], 'Failed on test 8' ) ;
    CheckSame( lData5, lList.Items[1], 'Failed on test 9' ) ;
    CheckSame( lData1, lList.Items[2], 'Failed on test 10' ) ;
    CheckSame( lData2, lList.Items[4], 'Failed on test 11' ) ;
    CheckSame( lData3, lList.Items[3], 'Failed on test 12' ) ;

  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.ItemOwner;
var
  lList       : TtstPerObjList ;
  lItemOwner1 : TtstPerObjAbs ;
  lData       : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    CheckSame( lList, lList.ItemOwner, 'Failed on test 1' ) ;
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData ) ;
    CheckSame( lList, lData.Owner, 'Failed on test 2' ) ;
    lItemOwner1 := TtstPerObjAbs.Create ;
    try
      lList.ItemOwner := lItemOwner1 ;
      CheckSame( lItemOwner1, lList.ItemOwner, 'Failed on test 3' ) ;
      CheckSame( lItemOwner1, lData.Owner, 'Failed on test 4' ) ;
    finally
      lItemOwner1.Free ;
    end ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.Items;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckSame( lData, lList.Items[i], 'Failed on ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.Last;
var
  lList : TtstPerObjList ;
  lData0 : TtstPerObjAbs ;
  lData1 : TtstPerObjAbs ;
  lData2 : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData0 := TtstPerObjAbs.Create ;
    lList.Add( lData0 ) ;
    lData1 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData2 ) ;
    CheckSame( lData2, lList.Last, 'Failed on Last' ) ;
    lData0.Deleted := true ;
    Check( lData1 <> lList.First, 'Failed on First when the first was deleted' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.LastExcludeDeleted;
var
  lList : TtstPerObjList ;
  lData0 : TtstPerObjAbs ;
  lData1 : TtstPerObjAbs ;
  lData2 : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData0 := TtstPerObjAbs.Create ;
    lList.Add( lData0 ) ;
    lData1 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData2 ) ;
    CheckSame( lData2, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted' ) ;
    lData2.Deleted := true ;
    CheckSame( lData1, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.MarkListItemsDirty;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData2 := TtstPerObjAbs.Create ;
    lData1.ObjectState := posClean ;
    lData2.ObjectState := posClean ;
    lList.Add( lData1 ) ;
    lList.Add( lData2 ) ;
    lList.MarkListItemsDirty ;
    Check( lList.Items[0].Dirty, 'Failed on test 1' ) ;
    Check( lList.Items[1].Dirty, 'Failed on test 2' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.MarkListItemsForDeletion;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData2 := TtstPerObjAbs.Create ;
    lList.Add( lData1 ) ;
    lList.Add( lData2 ) ;
    Check( not lList.Items[0].Deleted, 'Failed on test 1' ) ;
    Check( not lList.Items[1].Deleted, 'Failed on test 2' ) ;
    lList.MarkListItemsForDeletion ;
    Check( lList.Items[0].ObjectState = posDelete, 'Failed on test 3' ) ;
    Check( lList.Items[1].ObjectState = posDelete, 'Failed on test 4' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.OwnsObjects;
var
  lList : TtstPerObjList ;
begin
  // A trivial test because OwnsObjects is delegated to the
  // owned instance of TObjectList
  lList := TtstPerObjList.Create ;
  try
    Check( lList.OwnsObjects, 'failed on OwnsObjects = true' ) ;
    lList.OwnsObjects := false ;
    Check( not lList.OwnsObjects, 'failed on OwnsObjects = false' ) ;
    lList.OwnsObjects := true ;        
    Check( lList.OwnsObjects, 'failed on OwnsObjects = true' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.Remove;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  i : integer ;
begin
  lList := TtstPerObjList.Create ;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create ;
      lList.Add( lData ) ;
      CheckEquals( i, lList.IndexOf( lData ), 'Failed on add ' + IntToStr( i )) ;
    end ;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i] ;
      lList.Remove( lData ) ;
      if i > 0 then
        CheckEquals( i, lList.Count, 'Failed on Remove (Count) ' + IntToStr( i ))
      else
        CheckEquals( 0, lList.Count, 'Failed on Remove (Count) ' + IntToStr( i )) ;
      CheckEquals( -1, lList.IndexOf( lData ), 'Failed on Remove (IndexOf) ' + IntToStr( i )) ;
    end ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.SortByOID;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lData4      : TtstPerObjAbs ;
  lData5      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData1.OID.AsString := '1' ;
    lData2 := TtstPerObjAbs.Create ;
    lData2.OID.AsString := '2' ;
    lData3 := TtstPerObjAbs.Create ;
    lData3.OID.AsString := '3' ;
    lData4 := TtstPerObjAbs.Create ;
    lData4.OID.AsString := '4' ;
    lData5 := TtstPerObjAbs.Create ;
    lData5.OID.AsString := '5' ;

    lList.Add( lData2 ) ;
    lList.Add( lData1 ) ;
    lList.Add( lData3 ) ;
    lList.Add( lData5 ) ;
    lList.Add( lData4 ) ;

    lList.SortByOID ;
    CheckSame( lData1, lList.Items[0], 'Failed on test 1' ) ;
    CheckSame( lData2, lList.Items[1], 'Failed on test 2' ) ;
    CheckSame( lData3, lList.Items[2], 'Failed on test 3' ) ;
    CheckSame( lData4, lList.Items[3], 'Failed on test 4' ) ;
    CheckSame( lData5, lList.Items[4], 'Failed on test 5' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.SortByProps;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lData4      : TtstPerObjAbs ;
  lData5      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData1.StrProp := '1' ;
    lData2 := TtstPerObjAbs.Create ;
    lData2.StrProp := '2' ;
    lData3 := TtstPerObjAbs.Create ;
    lData3.StrProp := '3' ;
    lData4 := TtstPerObjAbs.Create ;
    lData4.StrProp := '4' ;
    lData5 := TtstPerObjAbs.Create ;
    lData5.StrProp := '5' ;

    lList.Add( lData2 ) ;
    lList.Add( lData1 ) ;
    lList.Add( lData3 ) ;
    lList.Add( lData5 ) ;
    lList.Add( lData4 ) ;

    lList.SortByProps(['StrProp']);
    CheckSame( lData1, lList.Items[0], 'Failed on test 1' ) ;
    CheckSame( lData2, lList.Items[1], 'Failed on test 2' ) ;
    CheckSame( lData3, lList.Items[2], 'Failed on test 3' ) ;
    CheckSame( lData4, lList.Items[3], 'Failed on test 4' ) ;
    CheckSame( lData5, lList.Items[4], 'Failed on test 5' ) ;
  finally
    lList.Free;
  end;
end;


procedure TTestPerObjList.SortByDispOrder;
var
  lList       : TtstPerObjList ;
  lData1      : TtstPerObjAbs ;
  lData2      : TtstPerObjAbs ;
  lData3      : TtstPerObjAbs ;
  lData4      : TtstPerObjAbs ;
  lData5      : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lData1 := TtstPerObjAbs.Create ;
    lData1.DispOrder := 1 ;
    lData2 := TtstPerObjAbs.Create ;
    lData2.DispOrder := 2 ;
    lData3 := TtstPerObjAbs.Create ;
    lData3.DispOrder := 3 ;
    lData4 := TtstPerObjAbs.Create ;
    lData4.DispOrder := 4 ;
    lData5 := TtstPerObjAbs.Create ;
    lData5.DispOrder := 5 ;

    lList.Add( lData2, false ) ;
    lList.Add( lData1, false ) ;
    lList.Add( lData3, false ) ;
    lList.Add( lData5, false ) ;
    lList.Add( lData4, false ) ;

    lList.SortByDispOrder ;
    CheckSame( lData1, lList.Items[0], 'Failed on test 1' ) ;
    CheckSame( lData2, lList.Items[1], 'Failed on test 2' ) ;
    CheckSame( lData3, lList.Items[2], 'Failed on test 3' ) ;
    CheckSame( lData4, lList.Items[3], 'Failed on test 4' ) ;
    CheckSame( lData5, lList.Items[4], 'Failed on test 5' ) ;
  finally
    lList.Free;
  end;
end;

procedure TTestPerObjList.AddOwner;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
begin
  lList := TtstPerObjList.Create ;
  try
    lList.AutoSetItemOwner := true ;
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData ) ;
    CheckSame(lList, lData.Owner, '#1');
  finally
    lList.Free ;
  end ;
  lList := TtstPerObjList.Create ;
  try
    lList.AutoSetItemOwner := false ;
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData ) ;
    CheckNull(lData.Owner, '#2');
  finally
    lList.Free ;
  end ;
end;

procedure TTestPerObjList.RemoveOwner;
var
  lList : TtstPerObjList ;
  lData : TtstPerObjAbs ;
  lList1 : TtstPerObjList ;
begin
  lList := TtstPerObjList.Create ;
  try
    lList.AutoSetItemOwner := true ;
    lData := TtstPerObjAbs.Create ;
    lList.Add( lData ) ;
    CheckSame(lList, lData.Owner, '#1');
    lList.Remove(lData);
    CheckNull(lData.Owner, '#2');
  finally
    lList.Free ;
  end ;
  lList := TtstPerObjList.Create ;
  try
    lList.AutoSetItemOwner := false ;
    lData := TtstPerObjAbs.Create ;
    lList1 := TtstPerObjList.Create ;
    try
      lData.Owner := lList1;
      lList.Add( lData ) ;
      CheckSame(lList1, lData.Owner, '#3');
      lList.Remove(lData);
      CheckSame(lList1, lData.Owner, '#3');
    finally
      lList1.Free;
    end;
  finally
    lList.Free ;
  end ;

end;

{ TtstPerObjList }

procedure TtstPerObjList.Add(pObject: TtstPerObjAbs; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TtstPerObjList.Clone: TtstPerObjList;
begin
  result := TtstPerObjList( inherited Clone ) ;
end;

constructor TtstPerObjList.CreateNew(const pDatabaseName : string = '' ; const pPerLayerName : string = '');
begin
  Create ;
  OID.AsString := IntToStr( Integer( Self )) ;
  ObjectState := posCreate ;
end;

function TtstPerObjList.GetItems(i: integer): TtstPerObjAbs;
begin
  result := TtstPerObjAbs( inherited GetItems(i));
end;

procedure TtstPerObjList.SetItems(i: integer; const Value: TtstPerObjAbs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TtstPerObjAbs }

function TtstPerObjAbs.Clone: TtstPerObjAbs;
begin
  result := TtstPerObjAbs( inherited Clone ) ;
end;

constructor TtstPerObjAbs.Create;
begin
  inherited;
  Populate;
end;

constructor TtstPerObjAbs.CreateNew(const pOwner : TPerObjAbs ; const pDatabaseName : string = '' ; const pPerLayerName : string = '');
begin
  Create ;
  Owner := pOwner as TtstPerObjList;
  OID.AsString := IntToStr( Integer( Self )) ;
  ObjectState := posCreate ;
end;

function TtstPerObjAbs.GetOwner: TtstPerObjList;
begin
  result := TtstPerObjList( inherited GetOwner ) ;
end;

function TtstPerObjAbs.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid( pErrors ) ;
  if StrProp = '' then
    pErrors.AddError( 'StrProp', cStrPropErrorMsg, cStrPropErrorCode ) ;
  if IntProp = 0 then
    pErrors.AddError( 'IntProp', cIntPropErrorMsg, cIntPropErrorCode ) ;
  if FloatProp = 0 then
    pErrors.AddError( 'FloatProp', cFloatPropErrorMsg, cFloatPropErrorCode ) ;
  result := pErrors.Count = 0 ;
end;

procedure TtstPerObjAbs.Populate;
begin
  BoolProp  := false ;
  IntProp   := 1 ;
  FloatProp := 1.1111111 ;
  StrProp   := 'testing' ;
  DateProp  := EncodeDate(2002,01,01);
  OrdProp   := tstOrdProp_1 ;
end;

procedure TtstPerObjAbs.SetOwner(const Value: TtstPerObjList);
begin
  inherited SetOwner( Value ) ;
end;

{ TtstPerObjOwnedObj }

constructor TtstPerObjOwnedObj.Create;
begin
  inherited;
  FObjProp := TtstPerObjAbs.Create ;
end;

destructor TtstPerObjOwnedObj.Destroy;
begin
  FObjProp.Free ;
  inherited;
end;

{ TtstPerObjOwnedObjCanAssign }

procedure TtstPerObjOwnedObjCanAssign.AssignClassProps( pSource: TPerObjAbs);
begin
  ObjProp.Assign( TtstPerObjOwnedObjCanAssign( pSource ).ObjProp ) ;
end;

function TtstPerObjOwnedObjCanAssign.Clone: TtstPerObjOwnedObjCanAssign;
begin
  result := TtstPerObjOwnedObjCanAssign( inherited Clone ) ;
end;

procedure TTestPerObjAbsCreatNew.CreateNew_OIDInteger;
var
  lItem : TPerObjAbs ;
  lSavedPerLayerName : string ;
  lSavedDatabaseName : string ;
begin
  CreateNextOIDIntTable ;
  try
    gTIPerMgr.DefaultOIDClassName := cOIDClassNameInteger ;
    lItem := TPerObjAbs.CreateNew( DatabaseName, PerLayerName ) ;
    try
      CheckIs( lItem.OID, TOIDInteger, 'OID not a TOIDInteger' ) ;
      CheckNotEquals( '', lItem.OID.AsString, 'OID not assigned' );
    finally
      lItem.Free;
    end ;

    lSavedPerLayerName := gTIPerMgr.DefaultPerLayerName ;
    lSavedDatabaseName := gTIPerMgr.DefaultDBConnectionName ;
    gTIPerMgr.DefaultPerLayerName := PerLayerName ;
    try
      gTIPerMgr.DefaultDBConnectionName := DatabaseName ;
      try
        lItem := TPerObjAbs.CreateNew ;
        try
          CheckIs( lItem.OID, TOIDInteger, 'OID not a TOIDInteger' ) ;
          CheckNotEquals( '', lItem.OID.AsString, 'OID not assigned' );
        finally
          lItem.Free;
        end ;
      finally
        gTIPerMgr.DefaultPerLayerName := lSavedPerLayerName ;
      end;
    finally
      gTIPerMgr.DefaultDBConnectionName := lSavedDatabaseName ;
    end;
  finally
    DropNextOIDTable ;
  end;
end;

end.
