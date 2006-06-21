unit Adrs_TST;

interface
uses
   tiPersistAbs_TST
  ,tiPtnVisPerObj
  ,Adrs_BOM
  ;

type

  TTestAdrs = class( TTestPerFrameworkConnectAbs )
  private
    procedure EmptyTables ;
    procedure InsertTestLookupListName( pOID : Integer ; const pListName : string ) ;
    procedure InsertTestLookupListItem( pOwnerOID : Integer ; pOID : Integer ; const pItemText : string ) ;
    procedure InsertTestAdrs( pOwnerOID : Integer ; pOID : Integer ; pAdrsTypeOID : integer ; const pValue : string ) ;
    procedure InsertTestEAdrs( pOwnerOID : Integer ; pOID : Integer ; pAdrsTypeOID : integer ; const pValue : string ) ;
    procedure InsertTestPerson( pOwnerOID : Integer ; pOID : Integer ; const pValue : string ) ;
    procedure InsertTestCompany( pOID : Integer ; const pValue : string ) ;

    procedure CheckLookupListName( pData : TLookupList ; pOID : Integer ; const pListName : string ) ;
    procedure CheckLookupListItem( pData : TLookupListItem ; pOID : Integer ; const pItemText : string ) ;
    procedure CheckAdrs( pData : TAdrs ; pOID : Integer ; pAdrsTypeOID : integer ; const pValue : string ) ;
    procedure CheckEAdrs( pData : TEAdrs ; pOID : Integer ; pAdrsTypeOID : integer ; const pValue : string ) ;
    procedure CheckPerson( pData : TPerson ; pOID : Integer ; const pValue : string ) ;
    procedure CheckCompany( pData : TCompany ; pOID : Integer ; const pValue : string ) ;
    function  CreateCompoundCompany : TCompany ;
    procedure CheckCompoundCompany( pCompany : TCompany ) ;

  protected
    procedure Setup ; override ;
    procedure TearDown;override;
  published
    procedure TestLookupList_Read        ;
    procedure TestLookupList_Save        ;
    procedure TestLookupList_Update      ;
    procedure TestLookupList_Delete      ;

    procedure TestLookupListItem_Read    ;
    procedure TestLookupListItem_Save    ;
    procedure TestLookupListItem_Update  ;
    procedure TestLookupListItem_Delete  ;

    procedure TestLookupLists_Read       ;

    procedure TestAdrs_Read              ;
    procedure TestAdrs_Save              ;
    procedure TestAdrs_Update            ;
    procedure TestAdrs_Delete            ;
    procedure TestAdrs_Assign            ;
    procedure TestAdrs_Clone             ;

    procedure TestEAdrs_Read             ;
    procedure TestEAdrs_Save             ;
    procedure TestEAdrs_Update           ;
    procedure TestEAdrs_Delete           ;
    procedure TestEAdrs_Assign           ;
    procedure TestEAdrs_Clone            ;

    procedure TestPersonFlat_Read        ;
    procedure TestPersonFlat_Save        ;
    procedure TestPersonFlat_Update      ;
    procedure TestPersonFlat_Delete      ;
    procedure TestPersonFlat_Assign      ;
    procedure TestPersonFlat_Clone       ;

    procedure TestPersonCompound_Read    ;
    procedure TestPersonCompound_Save    ;
    procedure TestPersonCompound_Update  ;
    procedure TestPersonCompound_Delete  ;

    procedure TestCompanyFlat_Read       ;
    procedure TestCompanyFlat_Save       ;
    procedure TestCompanyFlat_Update     ;
    procedure TestCompanyFlat_Delete     ;
    procedure TestCompanyFlat_Assign     ;
    procedure TestCompanyFlat_Clone      ;

    procedure TestCompanyCompound_Read   ;
    procedure TestCompanyCompound_Save   ;
    procedure TestCompanyCompound_Update ;
    procedure TestCompanyCompound_Delete ;
    procedure TestCompanyCompound_Assign ;
    procedure TestCompanyCompound_Clone  ;

  end ;

  TTestAdrsIBX = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;

  TTestAdrsBDEParadox = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;

  TTestAdrsADOAccess = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;

  TTestAdrsADOSQLServer = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;

  TTestAdrsDOA = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;

  TTestAdrsXML = class( TTestAdrs )
  protected
    procedure Setup ; override ;
  end ;



procedure RegisterTests ;
implementation
uses
  tiDBConnectionSetup_TST
  ,cTIPersist
  ,TestFramework
  ,tiPersist
  ,SysUtils
  ,tiQuery
  ,tiPtnVisPerObj_Cli
  ;

const
  cOIDLookupListName  = 1 ;
  cOIDLookupListItem  = 2 ;
  cOIDLookupListItem1 = 3 ;
  cOIDAdrs            = 4 ;
  cOIDEAdrs           = 5 ;
  cOIDPerson          = 6 ;
  cOIDCompany         = 7 ;

  cUpdateValue        = 'test' ;

{ TTestAdrs }

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.IsRegistered( cTIPersistIBX ) then
    RegisterTest( 'Adrs', TTestAdrsIBX.Suite );

  if gPerFrameworkSetupFactory.IsRegistered( cTIPersistBDEParadox ) then
    RegisterTest( 'Adrs', TTestAdrsBDEParadox.Suite );

  if gPerFrameworkSetupFactory.IsRegistered( cTIPersistADOAccess ) then
    RegisterTest( 'Adrs', TTestAdrsADOAccess.Suite );

  if gPerFrameworkSetupFactory.IsRegistered( cTIPersistADOSQLServer ) then
    RegisterTest( 'Adrs', TTestAdrsADOSQLServer.Suite );

  if gPerFrameworkSetupFactory.IsRegistered( cTIPersistXML ) then
    RegisterTest( 'Adrs', TTestAdrsXML.Suite );

    //RegisterTest( 'TIQuery', TTestTIQueryDOA.Suite );
end ;

{ TTestAdrs }

procedure TTestAdrs.TestAdrs_Delete;
var
  lAdrses : TAdrsList ;
  lAdrs : TAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestAdrs( -1, cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 1, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    lAdrs := lAdrses.Items[0];
    lAdrs.ObjectState := posDelete ;
    lAdrses.Save ;
    Check( posDeleted = lAdrs.ObjectState, 'Failed on lAdr.ObjectState = posDelete' ) ;
  finally
    lAdrses.Free;
  end ;

  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 0, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
  finally
    lAdrses.Free;
  end ;

end;

procedure TTestAdrs.TestAdrs_Read;
var
  lAdrses : TAdrsList ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestAdrs( -1, cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 1, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckAdrs( lAdrses.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
  finally
    lAdrses.Free;
  end ;
end;

procedure TTestAdrs.TestAdrs_Save;
var
  lAdrses : TAdrsList ;
  lAdrs : TAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.ObjectState := posClean ;
    lAdrs := TAdrs.Create ;
    lAdrs.OID.AsString := IntToStr(cOIDAdrs) ;
    lAdrs.ObjectState := posCreate ;
    lAdrs.Lines   := IntToStr( cOIDAdrs ) ;
    lAdrs.Suburb  := IntToStr( cOIDAdrs ) ;
    lAdrs.State   := IntToStr( cOIDAdrs ) ;
    lAdrs.PCode   := IntToStr( cOIDAdrs ) ;
    lAdrs.Country := IntToStr( cOIDAdrs ) ;
    lAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lAdrses.Add( lAdrs ) ;
    lAdrses.Save ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
  finally
    lAdrses.Free;
  end ;

  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 1, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckAdrs( lAdrses.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
  finally
    lAdrses.Free;
  end ;

end;

procedure TTestAdrs.TestAdrs_Update;
var
  lAdrses : TAdrsList ;
  lAdrs   : TAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestAdrs( -1, cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 1, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    lAdrs := lAdrses.Items[0];
    lAdrs.Lines   := cUpdateValue ;
    lAdrs.Suburb  := cUpdateValue ;
    lAdrs.State   := cUpdateValue ;
    lAdrs.PCode   := cUpdateValue ;
    lAdrs.Country := cUpdateValue ;
    lAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem1) ;
    lAdrs.ObjectState := posUpdate ;
    lAdrses.Save ;
  finally
    lAdrses.Free;
  end ;

  lAdrses := TAdrsList.Create ;
  try
    lAdrses.OID.AsString := '-1' ;
    lAdrses.Read ;
    CheckEquals( 1, lAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckAdrs( lAdrses.Items[0], cOIDAdrs, cOIDLookupListItem1, cUpdateValue );
  finally
    lAdrses.Free;
  end ;

end;

procedure TTestAdrs.TestCompanyFlat_Delete;
var
  lCompanies : TCompanies ;
begin
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 1, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
    lCompanies.Items[0].ObjectState := posDelete ;
    lCompanies.Save ;
    Check( posDeleted = lCompanies.Items[0].ObjectState, 'Failed on lCompanies.Items[0].ObjectState = posDeleted' ) ;
  finally
    lCompanies.Free;
  end ;
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 0, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
  finally
    lCompanies.Free;
  end ;
end;

procedure TTestAdrs.TestCompanyFlat_Read;
var
  lCompanies : TCompanies ;
begin
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 1, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
    Check( posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean' ) ;
    Check( posPK = lCompanies.Items[0].ObjectState, 'Failed on lCompanies.Items[0].ObjectState = posClean' ) ;
    CheckCompany( lCompanies.Items[0], cOIDCompany, IntToStr( cOIDCompany ));
  finally
    lCompanies.Free;
  end ;
end;

procedure TTestAdrs.TestCompanyFlat_Save;
var
  lCompanies : TCompanies ;
  lCompany : TCompany ;
begin
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompany := TCompany.Create ;
    lCompany.OID.AsString := IntToStr(cOIDCompany);
    lCompany.Notes := IntToStr( cOIDCompany ) ;
    lCompany.CompanyName := IntToStr( cOIDCompany ) ;
    lCompany.ObjectState := posCreate ;
    lCompanies.Add( lCompany ) ;
    lCompanies.Save ;
    Check( posClean = lCompany.ObjectState, 'Failed on lCompany.ObjectState = posClean' ) ;
  finally
    lCompanies.Free;
  end ;
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 1, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
    Check( posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean' ) ;
    Check( posPK = lCompanies.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
    CheckCompany( lCompanies.Items[0], cOIDCompany, IntToStr( cOIDCompany ));
  finally
    lCompanies.Free;
  end ;
end;

procedure TTestAdrs.TestCompanyFlat_Update;
var
  lCompanies : TCompanies ;
  lCompany : TCompany ;
begin
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 1, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
    lCompany := lCompanies.Items[0];
    lCompany.Notes := cUpdateValue ;
    lCompany.CompanyName := cUpdateValue ;
    lCompany.ObjectState := posUpdate ;
    lCompanies.Save ;
    Check( posClean = lCompany.ObjectState, 'Failed on lCompany.ObjectState = posClean' ) ;
  finally
    lCompanies.Free;
  end ;
  lCompanies := TCompanies.Create ;
  try
    lCompanies.OID.AsString := '-1' ;
    lCompanies.Read ;
    CheckEquals( 1, lCompanies.Count, 'Failed on lCompanies.Count' ) ;
    Check( posClean = lCompanies.ObjectState, 'Failed on lCompanies.ObjectState = posClean' ) ;
    Check( posPK = lCompanies.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
    CheckCompany( lCompanies.Items[0], cOIDCompany, cUPdateValue);
  finally
    lCompanies.Free;
  end ;
end;

procedure TTestAdrs.TestEAdrs_Delete;
var
  lEAdrses : TEAdrsList ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestEAdrs( -1, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 1, lEAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    lEAdrses.Items[0].ObjectState := posDelete ;
    lEAdrses.Save ;
    Check( posDeleted = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posDeleted' ) ;
  finally
    lEAdrses.Free;
  end ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 0, lEAdrses.Count, 'Failed on lEAdrses.Count' ) ;
  finally
    lEAdrses.Free;
  end ;
end;

procedure TTestAdrs.TestEAdrs_Read;
var
  lEAdrses : TEAdrsList ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestEAdrs( -1, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 1, lEAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckEAdrs( lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
  finally
    lEAdrses.Free;
  end ;
end;

procedure TTestAdrs.TestEAdrs_Save;
var
  lEAdrses : TEAdrsList ;
  lEAdrs   : TEAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrs   := TEAdrs.Create ;
    lEAdrs.ObjectState := posCreate ;
    lEAdrses.Add( lEAdrs ) ;
    lEAdrs.OID.AsString := IntToStr(cOIDEAdrs) ;
    lEAdrs.Text := IntToStr(cOIDEAdrs) ;
    lEAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lEAdrses.Save ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
  finally
    lEAdrses.Free;
  end ;

  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 1, lEAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckEAdrs( lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
  finally
    lEAdrses.Free;
  end ;

end;

procedure TTestAdrs.TestEAdrs_Update;
var
  lEAdrses : TEAdrsList ;
  lEAdrs : TEAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestEAdrs( -1, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
  gAdrsBook.AdrsTypes.Read ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 1, lEAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    lEAdrs   := lEAdrses.Items[0] ;
    CheckEAdrs( lEAdrs, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
    lEAdrs.Text := cUpdateValue ;
    lEAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem1) ;
    lEAdrs.ObjectState := posUpdate ;
    lEAdrses.Save ;
  finally
    lEAdrses.Free;
  end ;
  lEAdrses := TEAdrsList.Create ;
  try
    lEAdrses.OID.AsString := '-1' ;
    lEAdrses.Read ;
    CheckEquals( 1, lEAdrses.Count, 'Failed on lAdrses.Count' ) ;
    Check( posClean = lEAdrses.ObjectState, 'Failed on lAdrses.ObjectState = posClean' ) ;
    Check( posClean = lEAdrses.Items[0].ObjectState, 'Failed on lAdrses.Items[0].ObjectState = posClean' ) ;
    CheckEAdrs( lEAdrses.Items[0], cOIDEAdrs, cOIDLookupListItem1, cUpdateValue );
  finally
    lEAdrses.Free;
  end ;
end;

procedure TTestAdrs.TestLookupList_Delete;
var
  lLookupLists : TLookupLists ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    lLookupLists.Items[0].ObjectState := posDelete ;
    lLookupLists.Save ;
    Check( posDeleted = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
  finally
    lLookupLists.Free ;
  end ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 0, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupList_Read;
var
  lLookupLists : TLookupLists ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupList_Save;
var
  lLookupLists : TLookupLists ;
  lLookupList  : TLookupList ;
begin
  lLookupLists := TLookupLists.Create ;
  try
    lLookupList  := TLookupList.Create ;
    lLookupList.ObjectState := posCreate ;
    lLookupList.OID.AsString := IntToStr(cOIDLookupListName) ;
    lLookupList.ListName := IntToStr( cOIDLookupListName ) ;
    lLookupLists.Add( lLookupList ) ;
    lLookupLists.Save ;
    Check( posClean = lLookupList.ObjectState, 'Failed on lLookupList.ObjectState = posClean' ) ;
  finally
    lLookupLists.Free ;
  end ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupList_Update;
var
  lLookupLists : TLookupLists ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
    lLookupLists.Items[0].ListName := cUpdateValue ;
    lLookupLists.Items[0].ObjectState := posUpdate ;
    lLookupLists.Save ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
  finally
    lLookupLists.Free ;
  end ;
  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, cUpdateValue) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupListItem_Delete;
var
  lLookupLists : TLookupLists ;
  lLookupListItem  : TLookupListItem ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;

    CheckEquals( 1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count' ) ;
    lLookupListItem  := lLookupLists.Items[0].Items[0] ;
    lLookupListItem.ObjectState := posDelete ;
    lLookupLists.Save ;
    Check( posDeleted = lLookupListItem.ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
  finally
    lLookupLists.Free ;
  end ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    CheckEquals( 0, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count' ) ;
  finally
    lLookupLists.Free ;
  end ;

end;

procedure TTestAdrs.TestLookupListItem_Read;
var
  lLookupLists : TLookupLists ;
  lLookupList  : TLookupList ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;

    CheckEquals( 1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count' ) ;
    lLookupList  := lLookupLists.Items[0] ;
    Check( posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListItem( lLookupList.Items[0], cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupListItem_Save;
var
  lLookupList  : TLookupList ;
  lLookupListItem : TLookupListItem ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;

  lLookupList := TLookupList.Create ;
  try
    lLookupList.ObjectState := posClean ;
    lLookupList.OID.AsString := IntToStr(cOIDLookupListName) ;
    lLookupListItem := TLookupListItem.Create ;
    lLookupListItem.ObjectState := posCreate ;
    lLookupListItem.OID.AsString := IntToStr(cOIDLookupListItem) ;
    lLookupListItem.Text := IntToStr( cOIDLookupListItem ) ;
    lLookupList.Add( lLookupListItem ) ;
    lLookupList.Save ;
    Check( posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
  finally
    lLookupList.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupListItem_Update;
var
  lLookupLists : TLookupLists ;
  lLookupList  : TLookupList ;
  lLookupListItem : TLookupListItem ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    CheckEquals( 1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count' ) ;
    lLookupListItem := lLookupLists.Items[0].Items[0] ;
    CheckLookupListItem( lLookupListItem, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;

    lLookupListItem.Text := cUpdateValue ;
    lLookupListItem.ObjectState := posUpdate ;
    lLookupLists.Save ;
  finally
    lLookupLists.Free ;
  end ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 1, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListName( lLookupLists.Items[0], cOIDLookupListName, IntToStr( cOIDLookupListName )) ;

    CheckEquals( 1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count' ) ;
    lLookupList  := lLookupLists.Items[0] ;
    Check( posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    Check( posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
    CheckLookupListItem( lLookupList.Items[0], cOIDLookupListItem, cUpdateValue) ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestLookupLists_Read;
var
  lLookupLists     : TLookupLists ;
  lLookupList      : TLookupList ;
  i, j : integer ;
begin
  for i := 1 to 9 do
  begin
    InsertTestLookupListName( i*10, IntToStr( i*10 )) ;
    for j := 1 to 9 do
      InsertTestLookupListItem( i*10, i*10+j, IntToStr( i*10+j )) ;
  end ;

  lLookupLists := TLookupLists.Create ;
  try
    lLookupLists.Read ;
    CheckEquals( 9, lLookupLists.Count, 'Failed on count' ) ;
    Check( posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
    for i := 0 to 8 do
    begin
      Check( posClean = lLookupLists.Items[i].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
      CheckLookupListName( lLookupLists.Items[i], (i+1)*10, IntToStr( (i+1)*10 )) ;
      lLookupList  := lLookupLists.Items[i] ;
      CheckEquals( 9, lLookupList.Count, 'Failed on lLookupList.count' ) ;
      for j := 0 to 8 do
      begin
        Check( posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean' ) ;
        Check( posClean = lLookupList.Items[j].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean' ) ;
        CheckLookupListItem( lLookupList.Items[j], (i+1)*10+j+1, IntToStr( (i+1)*10+j+1 )) ;
      end ;
    end ;
  finally
    lLookupLists.Free ;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Delete;
var
  lPeople : TPeople ;
begin
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 1, lPeople.Count, 'Failed on lPeople.Count' ) ;
    lPeople.Items[0].ObjectState := posDelete;
    lPeople.Save ;
    Check( posDeleted = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posDeleted' ) ;
  finally
    lPeople.Free;
  end ;

  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 0, lPeople.Count, 'Failed on lPeople.Count' ) ;
  finally
    lPeople.Free;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Read;
var
  lPeople : TPeople ;
begin
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 1, lPeople.Count, 'Failed on lPeople.Count' ) ;
    Check( posClean = lPeople.ObjectState, 'Failed on lPeople.ObjectState = posClean' ) ;
    Check( posPK = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
    CheckPerson( lPeople.Items[0], cOIDPerson, IntToStr( cOIDPerson ));
  finally
    lPeople.Free;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Save;
var
  lPeople : TPeople ;
  lPerson : TPerson ;
begin
  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPerson := TPerson.Create ;
    lPerson.ObjectState := posCreate ;
    lPerson.OID.AsString := IntToStr(cOIDPerson) ;
    lPerson.Notes := IntToStr( cOIDPerson ) ;
    lPerson.Title := IntToStr( cOIDPerson ) ;
    lPerson.Initials := IntToStr( cOIDPerson ) ;
    lPerson.FirstName := IntToStr( cOIDPerson ) ;
    lPerson.LastName  := IntToStr( cOIDPerson ) ;
    lPeople.Add( lPerson ) ;
    lPeople.Save ;
    Check( posClean = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
  finally
    lPeople.Free;
  end ;
  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 1, lPeople.Count, 'Failed on lPeople.Count' ) ;
    Check( posClean = lPeople.ObjectState, 'Failed on lPeople.ObjectState = posClean' ) ;
    Check( posPK = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
    CheckPerson( lPeople.Items[0], cOIDPerson, IntToStr( cOIDPerson ));
  finally
    lPeople.Free;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Update;
var
  lPeople : TPeople ;
  lPerson : TPerson ;
begin
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 1, lPeople.Count, 'Failed on lPeople.Count' ) ;
    lPerson := lPeople.Items[0];
    lPerson.Notes := cUpdateValue ;
    lPerson.Title := cUpdateValue ;
    lPerson.Initials := cUpdateValue ;
    lPerson.FirstName := cUpdateValue ;
    lPerson.LastName  := cUpdateValue ;
    lPerson.ObjectState := posUpdate ;
    lPeople.Save ;
    Check( posClean = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
  finally
    lPeople.Free;
  end ;

  lPeople := TPeople.Create ;
  try
    lPeople.OID.AsString := '-1' ;
    lPeople.Read ;
    CheckEquals( 1, lPeople.Count, 'Failed on lPeople.Count' ) ;
    Check( posClean = lPeople.ObjectState, 'Failed on lPeople.ObjectState = posClean' ) ;
    Check( posPK = lPeople.Items[0].ObjectState, 'Failed on lPeople.Items[0].ObjectState = posClean' ) ;
    CheckPerson( lPeople.Items[0], cOIDPerson, cUpdateValue);
  finally
    lPeople.Free;
  end ;
end;

procedure TTestAdrs.EmptyTables;
begin
  gTIPerMgr.DeleteRow( 'Adrs', nil ) ;
  gTIPerMgr.DeleteRow( 'EAdrs', nil ) ;
  gTIPerMgr.DeleteRow( 'Person', nil ) ;
  gTIPerMgr.DeleteRow( 'Company', nil ) ;
  gTIPerMgr.DeleteRow( 'Lookup_List_Value', nil ) ;
  gTIPerMgr.DeleteRow( 'Lookup_List_Name', nil ) ;
end;

procedure TTestAdrs.Setup;
begin
  inherited;
  EmptyTables ;
  FreeAndNilAdrsBook;
end;

procedure TTestAdrs.InsertTestLookupListName(pOID: Integer; const pListName: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID'] := pOID ;
    lParams.ParamAsVariant['List_Name'] := pListName ;
    gTIPerMgr.InsertRow( 'Lookup_List_Name', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckLookupListName(
  pData: TLookupList;
  pOID: Integer;
  const pListName: string);
begin
  CheckEquals( IntToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( pListName, pData.ListName, 'Failed on ListName' ) ;
end;

procedure TTestAdrs.InsertTestLookupListItem(
  pOwnerOID, pOID: Integer;
  const pItemText: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID'] := pOID ;
    lParams.ParamAsVariant['Owner_OID'] := pOwnerOID ;
    lParams.ParamAsVariant['Item_Text'] := pItemText ;
    gTIPerMgr.InsertRow( 'Lookup_List_Value', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckLookupListItem(
  pData: TLookupListItem;
  pOID: Integer;
  const pItemText: string);
begin
  CheckEquals( INtToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( pItemText, pData.Text, 'Failed on ItemText' ) ;
end;

procedure TTestAdrs.InsertTestAdrs(
  pOwnerOID, pOID, pAdrsTypeOID: Integer;
  const pValue: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID']         := pOID ;
    lParams.ParamAsVariant['Owner_OID']   := pOwnerOID ;
    lParams.ParamAsVariant['Adrs_Type']   := pAdrsTypeOID ;
    lParams.ParamAsVariant['Country']     := pValue ;
    lParams.ParamAsVariant['Lines']       := pValue ;
    lParams.ParamAsVariant['Suburb']      := pValue ;
    lParams.ParamAsVariant['State']       := pValue ;
    lParams.ParamAsVariant['PCode']       := pValue ;
    gTIPerMgr.InsertRow( 'Adrs', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckAdrs(pData : TAdrs ; pOID, pAdrsTypeOID: integer; const pValue: string);
begin
  CheckEquals( IntToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( IntToStr(pAdrsTypeOID), pData.AdrsTypeOID, 'Failed on AdrsTypeOID' ) ;
  CheckEquals( pValue, pData.Lines, 'Failed on Lines' ) ;
  CheckEquals( pValue, pData.Suburb, 'Failed on Suburb' ) ;
  CheckEquals( pValue, pData.State, 'Failed on State' ) ;
  CheckEquals( pValue, pData.PCode, 'Failed on PostCode' ) ;
  CheckEquals( pValue, pData.Country, 'Failed on Country' ) ;
end;

procedure TTestAdrs.TearDown;
begin
  EmptyTables ;
  inherited;
end;

procedure TTestAdrs.InsertTestEAdrs(
  pOwnerOID, pOID, pAdrsTypeOID: integer;
  const pValue: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID']         := pOID ;
    lParams.ParamAsVariant['Owner_OID']   := pOwnerOID ;
    lParams.ParamAsVariant['EAdrs_Type']  := pAdrsTypeOID ;
    lParams.ParamAsVariant['EAdrs_Text']  := pValue ;
    gTIPerMgr.InsertRow( 'EAdrs', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckEAdrs(
  pData: TEAdrs; pOID, pAdrsTypeOID: integer;
  const pValue: string);
begin
  CheckEquals( IntToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( IntToStr(pAdrsTypeOID), pData.AdrsTypeOID, 'Failed on AdrsTypeOID' ) ;
  CheckEquals( pValue, pData.Text, 'Failed on Text' ) ;
end;

procedure TTestAdrs.InsertTestPerson(pOwnerOID, pOID: Integer; const pValue: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID'] := pOID ;
    lParams.ParamAsVariant['Owner_OID'] := pOwnerOID ;
    lParams.ParamAsVariant['Notes'] := pValue ;
    lParams.ParamAsVariant['Title'] := pValue ;
    lParams.ParamAsVariant['Initials'] := pValue ;
    lParams.ParamAsVariant['First_Name'] := pValue ;
    lParams.ParamAsVariant['Family_Name'] := pValue ;
    gTIPerMgr.InsertRow( 'Person', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckPerson(
  pData: TPerson; pOID: Integer;
  const pValue: string);
begin
  CheckEquals( IntToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( pValue, pData.Notes, 'Failed on Notes' ) ;
  CheckEquals( pValue, pData.Title, 'Failed on Title' ) ;
  CheckEquals( pValue, pData.Initials, 'Failed on Initials' ) ;
  CheckEquals( pValue, pData.FirstName, 'Failed on FirstName' ) ;
  CheckEquals( pValue, pData.LastName, 'Failed on LastName' ) ;
end;

procedure TTestAdrs.TestCompanyCompound_Delete;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  InsertTestAdrs(   cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));
  InsertTestPerson( cOIDCompany, cOIDPerson, IntToStr( cOIDPerson )) ;

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  gAdrsBook.Companies.Items[0].ObjectState := posDelete ;
  gAdrsBook.Save ;
  Check( posDeleted = gAdrsBook.Companies.Items[0].ObjectState, 'gAdrsBook.Companies.Items[0].ObjectState' );

  FreeAndNilAdrsBook ;
  gAdrsBook.Read ;
  CheckEquals( 0, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;

end;

procedure TTestAdrs.TestCompanyCompound_Read;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  InsertTestAdrs(   cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));
  InsertTestPerson( cOIDCompany, cOIDPerson, IntToStr( cOIDPerson )) ;

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckCompany( gAdrsBook.Companies.Items[0], cOIDCompany, IntToStr( cOIDCompany )) ;
  CheckAdrs( gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs )) ;
  CheckEAdrs( gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr( cOIDEAdrs )) ;
  CheckPerson( gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, IntToStr( cOIDPerson )) ;
end;

procedure TTestAdrs.TestCompanyCompound_Save;
var
  lCompany : TCompany ;
  lAdrs : TAdrs ;
  lEAdrs : TEAdrs ;
  lPerson : TPerson ;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;

  gAdrsBook.Read ;

  lCompany := TCompany.Create;
  lCompany.OID.AsString := IntToStr(cOIDCompany) ;
  lCompany.ObjectState := posCreate ;
  lCompany.CompanyName := IntToStr( cOIDCompany ) ;
  lCompany.Notes       := IntToStr( cOIDCompany ) ;
  gAdrsBook.Companies.Add( lCompany ) ;

  lAdrs    := TAdrs.Create ;
  lAdrs.OID.AsString := IntToStr(cOIDAdrs) ;
  lAdrs.ObjectState := posCreate ;
  lAdrs.Lines   := IntToStr( cOIDAdrs ) ;
  lAdrs.Suburb  := IntToStr( cOIDAdrs ) ;
  lAdrs.State   := IntToStr( cOIDAdrs ) ;
  lAdrs.PCode   := IntToStr( cOIDAdrs ) ;
  lAdrs.Country := IntToStr( cOIDAdrs ) ;
  lAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
  lCompany.AddressList.Add( lAdrs ) ;

  lEAdrs   := TEAdrs.Create ;
  lEAdrs.OID.AsString := IntToStr(cOIDEAdrs) ;
  lEAdrs.ObjectState := posCreate ;
  lEAdrs.Text := IntToStr( cOIDEAdrs ) ;
  lEAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem1) ;
  lCompany.EAddressList.Add( lEAdrs ) ;

  lPerson  := TPerson.Create;
  lPerson.OID.AsString := IntToStr(cOIDPerson) ;
  lPerson.ObjectState := posCreate ;
  lPerson.Title       := IntToStr( cOIDPerson ) ;
  lPerson.Initials    := IntToStr( cOIDPerson ) ;
  lPerson.LastName    := IntToStr( cOIDPerson ) ;
  lPerson.FirstName   := IntToStr( cOIDPerson ) ;
  lPerson.Notes       := IntToStr( cOIDPerson ) ;
  lCompany.People.Add( lPerson ) ;
  gAdrsBook.Save ;
  FreeAndNilAdrsBook;

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckCompany( gAdrsBook.Companies.Items[0], cOIDCompany, IntToStr( cOIDCompany )) ;
  CheckAdrs( gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs )) ;
  CheckEAdrs( gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr( cOIDEAdrs )) ;
  CheckPerson( gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, IntToStr( cOIDPerson )) ;

end;

procedure TTestAdrs.TestCompanyCompound_Update;
var
  lCompany : TCompany ;
  lAdrs : TAdrs ;
  lEAdrs : TEAdrs ;
  lPerson : TPerson ;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestCompany( cOIDCompany, IntToStr( cOIDCompany ));
  InsertTestAdrs(   cOIDCompany, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDCompany, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));
  InsertTestPerson( cOIDCompany, cOIDPerson, IntToStr( cOIDPerson )) ;

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Items[0].EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.Companies.Count, 'gAdrsBook.Companies.Count' ) ;

  lCompany := gAdrsBook.Companies.Items[0] ;
  lCompany.ObjectState := posUpdate ;
  lCompany.CompanyName := cUpdateValue ;
  lCompany.Notes       := cUpdateValue ;

  lAdrs    := lCompany.AddressList.Items[0] ;
  lAdrs.ObjectState := posUpdate ;
  lAdrs.Lines   := cUpdateValue ;
  lAdrs.Suburb  := cUpdateValue ;
  lAdrs.State   := cUpdateValue ;
  lAdrs.PCode   := cUpdateValue ;
  lAdrs.Country := cUpdateValue ;

  lEAdrs   := lCompany.EAddressList.Items[0] ;
  lEAdrs.ObjectState := posUpdate ;
  lEAdrs.Text := cUpdateValue ;

  lPerson  := lCompany.People.Items[0];
  lPerson.ObjectState := posUpdate ;
  lPerson.Title := cUpdateValue ;
  lPerson.Initials := cUpdateValue ;
  lPerson.LastName := cUpdateValue ;
  lPerson.FirstName := cUpdateValue ;
  lPerson.Notes := cUpdateValue ;
  gAdrsBook.Save ;

  CheckCompany( gAdrsBook.Companies.Items[0], cOIDCompany, cUpdateValue) ;
  CheckAdrs( gAdrsBook.Companies.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, cUpdateValue ) ;
  CheckEAdrs( gAdrsBook.Companies.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, cUpdateValue ) ;
  CheckPerson( gAdrsBook.Companies.Items[0].People.Items[0], cOIDPerson, cUpdateValue ) ;

end;

procedure TTestAdrs.TestPersonCompound_Delete;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  InsertTestAdrs(   cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.People.Count, 'gAdrsBook.People.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count' ) ;
  gAdrsBook.People.Items[0].ObjectState := posDelete ;
  gAdrsBook.Save ;
  Check( posDeleted = gAdrsBook.People.Items[0].ObjectState, 'Failed on posDeleted = gAdrsBook.People.Items[0]' ) ;

  FreeAndNilAdrsBook ;
  gAdrsBook.Read ;
  CheckEquals( 0, gAdrsBook.People.Count, 'gAdrsBook.People.Count' ) ;

end;

procedure TTestAdrs.TestPersonCompound_Read;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  InsertTestAdrs(   cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));

  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.People.Count, 'gAdrsBook.People.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count' ) ;
  CheckPerson( gAdrsBook.People.Items[0], cOIDPerson, IntToStr( cOIDPerson )) ;
  CheckAdrs( gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs )) ;
  CheckEAdrs( gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr( cOIDEAdrs )) ;
end;

procedure TTestAdrs.TestPersonCompound_Save;
var
  lPerson : TPerson ;
  lAdrs   : TAdrs ;
  lEAdrs  : TEAdrs ;
begin

  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.Read ;

  lPerson := TPerson.Create ;
  lPerson.OID.AsString := IntToStr(cOIDPerson) ;
  lPerson.ObjectState := posCreate ;
  lPerson.Title := IntToStr( cOIDPerson ) ;
  lPerson.Initials  := IntToStr( cOIDPerson ) ;
  lPerson.FirstName := IntToStr( cOIDPerson ) ;
  lPerson.LastName  := IntToStr( cOIDPerson ) ;
  lPerson.Notes     := IntToStr( cOIDPerson ) ;
  gAdrsBook.People.Add( lPerson ) ;

  lAdrs   := TAdrs.Create ;
  lAdrs.OID.AsString := IntToStr(cOIDAdrs) ;
  lAdrs.ObjectState := posCreate ;
  lAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
  lAdrs.Lines := IntToStr( cOIDAdrs ) ;
  lAdrs.Suburb := IntToStr( cOIDAdrs ) ;
  lAdrs.State := IntToStr( cOIDAdrs ) ;
  lAdrs.PCode := IntToStr( cOIDAdrs ) ;
  lAdrs.Country := IntToStr( cOIDAdrs ) ;
  lPerson.AddressList.Add( lAdrs ) ;

  lEAdrs  := TEAdrs.Create ; ;
  lEADrs.OID.AsString := IntToStr(cOIDEAdrs) ;
  lEAdrs.ObjectState := posCreate ;
  lEAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem1);
  lEAdrs.Text := IntToStr( cOIDEAdrs ) ;
  lPerson.EAddressList.Add( lEAdrs ) ;
  gAdrsBook.Save ;

  FreeAndNilAdrsBook ;
  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.People.Count, 'gAdrsBook.People.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count' ) ;
  CheckPerson( gAdrsBook.People.Items[0], cOIDPerson, IntToStr( cOIDPerson )) ;
  CheckAdrs( gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs )) ;
  CheckEAdrs( gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr( cOIDEAdrs )) ;

end;

procedure TTestAdrs.TestPersonCompound_Update;
var
  lPerson : TPerson ;
  lAdrs   : TAdrs ;
  lEAdrs  : TEAdrs ;
begin
  gAdrsBook ;
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  InsertTestPerson( -1, cOIDPerson, IntToStr( cOIDPerson ));
  InsertTestAdrs(   cOIDPerson, cOIDAdrs,   cOIDLookupListItem, IntToStr( cOIDAdrs ));
  InsertTestEAdrs(  cOIDPerson, cOIDEAdrs,  cOIDLookupListItem1, IntToStr( cOIDEAdrs ));
  gAdrsBook.Read ;

  lPerson := gAdrsBook.People.Items[0] ;
  lPerson.ObjectState := posUpdate ;
  lPerson.Title       := cUpdateValue ;
  lPerson.Initials    := cUpdateValue ;
  lPerson.FirstName   := cUpdateValue ;
  lPerson.LastName    := cUpdateValue ;
  lPerson.Notes       := cUpdateValue ;

  lAdrs             := lPerson.AddressList.Items[0];
  lAdrs.ObjectState := posUpdate;
  lAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem1) ;
  lAdrs.Lines       := cUpdateValue ;
  lAdrs.Suburb      := cUpdateValue;
  lAdrs.State       := cUpdateValue ;
  lAdrs.PCode       := cUpdateValue;
  lAdrs.Country     := cUpdateValue ;

  lEAdrs            := lPerson.EAddressList.Items[0];
  lEAdrs.ObjectState := posUpdate ;
  lEAdrs.AdrsTypeOID := IntToStr(cOIDLookupListItem);
  lEAdrs.Text := cUpdateValue ;
  gAdrsBook.Save ;

  FreeAndNilAdrsBook ;
  gAdrsBook.Read ;
  CheckEquals( 1, gAdrsBook.People.Count, 'gAdrsBook.People.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].AddressList.Count, 'gAdrsBook.People.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, gAdrsBook.People.Items[0].EAddressList.Count, 'gAdrsBook.People.Items[0].EAdrses.Count' ) ;
  CheckPerson( gAdrsBook.People.Items[0], cOIDPerson, cUpdateValue ) ;
  CheckAdrs( gAdrsBook.People.Items[0].AddressList.Items[0], cOIDAdrs, cOIDLookupListItem1, cUpdateValue) ;
  CheckEAdrs( gAdrsBook.People.Items[0].EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem, cUpdateValue) ;

end;

procedure TTestAdrs.InsertTestCompany(pOID: Integer;
  const pValue: string);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.ParamAsVariant['OID'] := pOID ;
    lParams.ParamAsVariant['Notes'] := pValue ;
    lParams.ParamAsVariant['Company_Name'] := pValue ;
    gTIPerMgr.InsertRow( 'Company', lParams ) ;
  finally
    lParams.Free;
  end;
end;

procedure TTestAdrs.CheckCompany(pData: TCompany; pOID: Integer;
  const pValue: string);
begin
  CheckEquals( IntToStr(pOID), pData.OID.AsString, 'Failed on OID' ) ;
  CheckEquals( pValue, pData.Notes, 'Failed on Notes' ) ;
  CheckEquals( pValue, pData.CompanyName, 'Failed on CompanyName' ) ;
end;

procedure TTestAdrs.TestAdrs_Assign;
var
  lAdrsFrom : TAdrs ;
  lAdrsTo   : TAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lAdrsFrom := TAdrs.Create ;
  try
    lAdrsFrom.OID.AsString := IntToStr(cOIDAdrs) ;
    lAdrsFrom.ObjectState := posClean ;
    lAdrsFrom.Lines   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.Suburb  := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.State   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.PCode   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.Country := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lAdrsTo := TAdrs.Create ;
    try
      lAdrsTo.Assign( lAdrsFrom ) ;
      CheckAdrs( lAdrsTo, cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
    finally
      lAdrsTo.Free ;
    end ;
  finally
    lAdrsFrom.Free ;
  end ;

end;

procedure TTestAdrs.TestAdrs_Clone;
var
  lAdrsFrom : TAdrs ;
  lAdrsTo   : TAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lAdrsFrom := TAdrs.Create ;
  try
    lAdrsFrom.OID.AsString := IntToStr(cOIDAdrs) ;
    lAdrsFrom.ObjectState := posClean ;
    lAdrsFrom.Lines   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.Suburb  := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.State   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.PCode   := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.Country := IntToStr( cOIDAdrs ) ;
    lAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lAdrsTo := lAdrsFrom.Clone ;
    try
      CheckAdrs( lAdrsTo, cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs ));
    finally
      lAdrsTo.Free ;
    end ;
  finally
    lAdrsFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestCompanyCompound_Assign;
var
  lCompanyFrom : TCompany ;
  lCompanyTo   : TCompany ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.Read ;

  lCompanyFrom := CreateCompoundCompany ;
  try
    CheckCompoundCompany( lCompanyFrom ) ;
    lCompanyTo := TCompany.Create ;
    try
      lCompanyTo.Assign( lCompanyFrom ) ;
      CheckCompoundCompany( lCompanyTo ) ;
    finally
      lCompanyTo.Free ;
    end ;
  finally
    lCompanyFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestCompanyCompound_Clone;
var
  lCompanyFrom : TCompany ;
  lCompanyTo   : TCompany ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem1, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.Read ;

  lCompanyFrom := CreateCompoundCompany ;
  try
    CheckCompoundCompany( lCompanyFrom ) ;
    lCompanyTo := lCompanyFrom.Clone ;
    try
      CheckCompoundCompany( lCompanyTo ) ;
    finally
      lCompanyTo.Free ;
    end ;
  finally
    lCompanyFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestCompanyFlat_Assign;
var
  lCompanyFrom : TCompany ;
  lCompanyTo   : TCompany ;
begin
  lCompanyFrom := TCompany.Create ;
  try
    lCompanyFrom.ObjectState := posCreate ;
    lCompanyFrom.OID.AsString := IntToStr(cOIDCompany) ;
    lCompanyFrom.Notes := IntToStr( cOIDCompany ) ;
    lCompanyFrom.CompanyName := IntToStr( cOIDCompany ) ;
    CheckCompany( lCompanyFrom, cOIDCompany, IntToStr( cOIDCompany ));
    lCompanyTo := TCompany.Create ;
    try
      lCompanyTo.Assign( lCompanyFrom ) ;
      CheckCompany( lCompanyTo, cOIDCompany, IntToStr( cOIDCompany ));
    finally
      lCompanyTo.Free ;
    end ;
  finally
    lCompanyFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestCompanyFlat_Clone;
var
  lCompanyFrom : TCompany ;
  lCompanyTo   : TCompany ;
begin
  lCompanyFrom := TCompany.Create ;
  try
    lCompanyFrom.ObjectState := posCreate ;
    lCompanyFrom.OID.AsString := IntToStr(cOIDCompany) ;
    lCompanyFrom.Notes := IntToStr( cOIDCompany ) ;
    lCompanyFrom.CompanyName := IntToStr( cOIDCompany ) ;
    CheckCompany( lCompanyFrom, cOIDCompany, IntToStr( cOIDCompany ));
    lCompanyTo := lCompanyFrom.Clone;
    try
      CheckCompany( lCompanyTo, cOIDCompany, IntToStr( cOIDCompany ));
    finally
      lCompanyTo.Free ;
    end ;
  finally
    lCompanyFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestEAdrs_Assign;
var
  lEAdrsFrom : TEAdrs ;
  lEAdrsTo   : TEAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lEAdrsFrom := TEAdrs.Create ;
  try
    lEAdrsFrom.OID.AsString := IntToStr(cOIDEAdrs) ;
    lEAdrsFrom.ObjectState := posClean ;
    lEAdrsFrom.Text   := IntToStr( cOIDEAdrs ) ;
    lEAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lEAdrsTo := TEAdrs.Create ;
    try
      lEAdrsTo.Assign( lEAdrsFrom ) ;
      CheckEAdrs( lEAdrsTo, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
    finally
      lEAdrsTo.Free ;
    end ;
  finally
    lEAdrsFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestEAdrs_Clone;
var
  lEAdrsFrom : TEAdrs ;
  lEAdrsTo   : TEAdrs ;
begin
  InsertTestLookupListName( cOIDLookupListName, IntToStr( cOIDLookupListName )) ;
  InsertTestLookupListItem( cOIDLookupListName, cOIDLookupListItem, IntToStr( cOIDLookupListItem )) ;
  gAdrsBook.AdrsTypes.Read ;
  lEAdrsFrom := TEAdrs.Create ;
  try
    lEAdrsFrom.OID.AsString := IntToStr(cOIDEAdrs) ;
    lEAdrsFrom.ObjectState := posClean ;
    lEAdrsFrom.Text   := IntToStr( cOIDEAdrs ) ;
    lEAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
    lEAdrsTo := lEAdrsFrom.Clone ;
    try
      CheckEAdrs( lEAdrsTo, cOIDEAdrs, cOIDLookupListItem, IntToStr( cOIDEAdrs ));
    finally
      lEAdrsTo.Free ;
    end ;
  finally
    lEAdrsFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Assign;
var
  lPersonFrom : TPerson ;
  lPersonTo   : TPerson ;
begin
  lPersonFrom := TPerson.Create ;
  try
    lPersonFrom.ObjectState := posCreate ;
    lPersonFrom.OID.AsString := IntToStr(cOIDPerson) ;
    lPersonFrom.Notes := IntToStr( cOIDPerson ) ;
    lPersonFrom.Title := IntToStr( cOIDPerson ) ;
    lPersonFrom.Initials := IntToStr( cOIDPerson ) ;
    lPersonFrom.FirstName := IntToStr( cOIDPerson ) ;
    lPersonFrom.LastName  := IntToStr( cOIDPerson ) ;
    CheckPerson( lPersonFrom, cOIDPerson, IntToStr( cOIDPerson ));
    lPersonTo := TPerson.Create ;
    try
      lPersonTo.Assign( lPersonFrom ) ;
      CheckPerson( lPersonTo, cOIDPerson, IntToStr( cOIDPerson ));
    finally
      lPersonTo.Free ;
    end ;
  finally
    lPersonFrom.Free ;
  end ;
end;

procedure TTestAdrs.TestPersonFlat_Clone;
var
  lPersonFrom : TPerson ;
  lPersonTo   : TPerson ;
begin
  lPersonFrom := TPerson.Create ;
  try
    lPersonFrom.ObjectState := posCreate ;
    lPersonFrom.OID.AsString := IntToStr(cOIDPerson) ;
    lPersonFrom.Notes := IntToStr( cOIDPerson ) ;
    lPersonFrom.Title := IntToStr( cOIDPerson ) ;
    lPersonFrom.Initials := IntToStr( cOIDPerson ) ;
    lPersonFrom.FirstName := IntToStr( cOIDPerson ) ;
    lPersonFrom.LastName  := IntToStr( cOIDPerson ) ;
    CheckPerson( lPersonFrom, cOIDPerson, IntToStr( cOIDPerson ));
    lPersonTo := lPersonFrom.Clone ;
    try
      CheckPerson( lPersonTo, cOIDPerson, IntToStr( cOIDPerson ));
    finally
      lPersonTo.Free ;
    end ;
  finally
    lPersonFrom.Free ;
  end ;
end;

procedure TTestAdrs.CheckCompoundCompany(pCompany: TCompany);
begin
  CheckEquals( 1, pCompany.AddressList.Count, 'gAdrsBook.Companies.Items[0].Adrses.Count' ) ;
  CheckEquals( 1, pCompany.EAddressList.Count, 'gAdrsBook.Companies.Items[0].EAdrses.Count' ) ;
  CheckCompany( pCompany, cOIDCompany, IntToStr( cOIDCompany )) ;
  CheckAdrs( pCompany.AddressList.Items[0], cOIDAdrs, cOIDLookupListItem, IntToStr( cOIDAdrs )) ;
  CheckEAdrs( pCompany.EAddressList.Items[0], cOIDEAdrs, cOIDLookupListItem1, IntToStr( cOIDEAdrs )) ;
  CheckPerson( pCompany.People.Items[0], cOIDPerson, IntToStr( cOIDPerson )) ;
end;

function TTestAdrs.CreateCompoundCompany: TCompany;
var
  lAdrsFrom : TAdrs ;
  lEAdrsFrom : TEAdrs ;
  lPersonFrom : TPerson ;
begin
  result := TCompany.Create ;
  result.OID.AsString := IntToStr(cOIDCompany) ;
  result.ObjectState := posCreate ;
  result.CompanyName := IntToStr( cOIDCompany ) ;
  result.Notes       := IntToStr( cOIDCompany ) ;

  lAdrsFrom    := TAdrs.Create ;
  lAdrsFrom.OID.AsString := IntToStr(cOIDAdrs) ;
  lAdrsFrom.ObjectState := posCreate ;
  lAdrsFrom.Lines   := IntToStr( cOIDAdrs ) ;
  lAdrsFrom.Suburb  := IntToStr( cOIDAdrs ) ;
  lAdrsFrom.State   := IntToStr( cOIDAdrs ) ;
  lAdrsFrom.PCode   := IntToStr( cOIDAdrs ) ;
  lAdrsFrom.Country := IntToStr( cOIDAdrs ) ;
  lAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem) ;
  result.AddressList.Add( lAdrsFrom ) ;

  lEAdrsFrom   := TEAdrs.Create ;
  lEAdrsFrom.OID.AsString := IntToStr(cOIDEAdrs) ;
  lEAdrsFrom.ObjectState := posCreate ;
  lEAdrsFrom.Text := IntToStr( cOIDEAdrs ) ;
  lEAdrsFrom.AdrsTypeOID := IntToStr(cOIDLookupListItem1) ;
  result.EAddressList.Add( lEAdrsFrom ) ;

  lPersonFrom  := TPerson.Create;
  lPersonFrom.OID.AsString := IntToStr(cOIDPerson) ;
  lPersonFrom.ObjectState := posCreate ;
  lPersonFrom.Title       := IntToStr( cOIDPerson ) ;
  lPersonFrom.Initials    := IntToStr( cOIDPerson ) ;
  lPersonFrom.LastName    := IntToStr( cOIDPerson ) ;
  lPersonFrom.FirstName   := IntToStr( cOIDPerson ) ;
  lPersonFrom.Notes       := IntToStr( cOIDPerson ) ;
  result.People.Add( lPersonFrom ) ;
end;

{ TTestAdrsIBX }

procedure TTestAdrsIBX.Setup;
begin
  FPerLayerID := cTIPersistIBX ;
  inherited;
end;

{ TTestAdrsBDEParadox }

procedure TTestAdrsBDEParadox.Setup;
begin
  FPerLayerID := cTIPersistBDEParadox ;
  inherited;
end;

{ TTestAdrsADOAccess }

procedure TTestAdrsADOAccess.Setup;
begin
  FPerLayerID := cTIPersistADOAccess ;
  inherited;
end;

{ TTestAdrsADOSQLServer }

procedure TTestAdrsADOSQLServer.Setup;
begin
  FPerLayerID := cTIPersistADOSQLServer ;
  inherited;
end;

{ TTestAdrsDOA }

procedure TTestAdrsDOA.Setup;
begin
  FPerLayerID := cTIPersistDOA ;
  inherited;
end;

{ TTestAdrsXML }

procedure TTestAdrsXML.Setup;
begin
  FPerLayerID := cTIPersistXML ;
  inherited;
end;

end.
