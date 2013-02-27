unit DemoTIPerBlobs_Srv;

interface
uses
  tiPtnVisSQL
  ;

type

  TVisDemoImage_ReadPK = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end ;

  TVisDemoImage_ReadOne = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end ;

  TVisDemoImage_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisDemoImage_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisDemoImage_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
    procedure SetupParams    ; override ;
  end ;

procedure CreateImageDemoTable ;
procedure DropImageDemoTable ;

implementation
uses
  tiPersist
  ,tiQuery
  ,tiPtnVisMgr
  ,DemoTIPerBlobs_BOM
  ,tiPtnVisPerObj
  ,Classes
  ,tiDialogs
  ;

procedure CreateImageDemoTable ;
var
  lMetaData : TtiDBMetaDataTable ;
begin
  lMetaData := TtiDBMetaDataTable.Create ;
  try
    lMetaData.Name := 'IMAGE_DEMO' ;
    lMetaData.AddField( 'OID', qfkString, 36 ) ;
    lMetaData.AddField( 'DESCRIPTION', qfkString, 60 ) ;
    lMetaData.AddField( 'IMAGE', qfkBinary ) ;
    gTIPerMgr.CreateTable( lMetaData ) ;
  finally
    lMetaData.Free ;
  end;
end;

procedure DropImageDemoTable ;
begin
  gTIPerMgr.DropTable( 'IMAGE_DEMO' ) ;
end;


{ TVisDemoImage_ReadPK }

function TVisDemoImage_ReadPK.AcceptVisitor: boolean;
begin
  result := ( Visited is TDemoImages ) and
            ( Visited.ObjectState = posEmpty );
end;

procedure TVisDemoImage_ReadPK.Init;
begin
  Query.SQL.Text := 'select oid, description from image_demo' ;
end;

procedure TVisDemoImage_ReadPK.MapRowToObject;
var
  lList : TDemoImages ;
  lData : TDemoImage ;
begin
  lList := ( Visited as TDemoImages ) ;
  lData := TDemoImage.Create ;
  lData.OID.AssignFromTIQuery( Query ) ;
  lData.Description := Query.FieldAsString['Description'];
  lData.ObjectState := posPK ;
  lList.Add(lData);
end;

procedure TVisDemoImage_ReadPK.SetupParams;
begin
  // Do nothing ;
end;

{ TVisDemoImage_ReadOne }

function TVisDemoImage_ReadOne.AcceptVisitor: boolean;
begin
  result := ( Visited is TDemoImage ) and
            ( Visited.ObjectState = posPK );
end;

procedure TVisDemoImage_ReadOne.Init;
begin
  Query.SQL.Text := 'select image from image_demo where oid = :oid' ;
end;

procedure TVisDemoImage_ReadOne.MapRowToObject;
var
  lData : TDemoImage ;
  lStream : TMemoryStream ;
begin
  lData := ( Visited as TDemoImage ) ;
  lStream := TMemoryStream.Create ;
  try
    Query.AssignFieldAsStream('Image', lStream );
    lStream.Position := 0 ;
    lData.Image.LoadFromStream(lStream);
  finally
    lStream.Free ;
  end;
end;

procedure TVisDemoImage_ReadOne.SetupParams;
var
  lData : TDemoImage ;
begin
  lData := ( Visited as TDemoImage ) ;
  lData.OID.AssignToTIQuery( Query ) ;
end;

{ TVisDemoImage_Create }

function TVisDemoImage_Create.AcceptVisitor: boolean;
begin
  result := ( Visited is TDemoImage ) and
            ( Visited.ObjectState = posCreate );
end;

procedure TVisDemoImage_Create.Init;
begin
  Query.SQL.Text :=
    'insert into image_demo ' +
    '( oid, description, image ) ' +
    'values ' +
    '( :oid, :description, :image )' ;
end;

procedure TVisDemoImage_Create.SetupParams;
var
  lData : TDemoImage ;
  lStream : TMemoryStream ;
begin
  lData := ( Visited as TDemoImage ) ;
  lStream := TMemoryStream.Create ;
  try
    lData.OID.AssignToTIQuery( Query ) ;
    Query.ParamAsString['description'] := lData.Description ;
    lData.Image.SaveToStream( lStream ) ;
    Query.AssignParamFromStream( 'image', lStream ) ;
  finally
    lStream.Free ;
  end;
end;

{ TVisDemoImage_Update }

function TVisDemoImage_Update.AcceptVisitor: boolean;
begin
  result := ( Visited is TDemoImage ) and
            ( Visited.ObjectState = posUpdate );
end;

procedure TVisDemoImage_Update.Init;
begin
  Query.SQL.Text :=
    'update image_demo set ' +
    ' description = :description ' +
    ',image = :image ' +
    'where ' +
    'oid = :oid' ;
end;

procedure TVisDemoImage_Update.SetupParams;
var
  lData : TDemoImage ;
  lStream : TMemoryStream ;
begin
  lData := ( Visited as TDemoImage ) ;
  lStream := TMemoryStream.Create ;
  try
    lData.OID.AssignToTIQuery( Query ) ;
    Query.ParamAsString['description'] := lData.Description ;
    lData.Image.SaveToStream( lStream ) ;
    Query.AssignParamFromStream( 'image', lStream ) ;
  finally
    lStream.Free ;
  end;
end;

{ TVisDemoImage_Delete }

function TVisDemoImage_Delete.AcceptVisitor: boolean;
begin
  result := ( Visited is TDemoImage ) and
            ( Visited.ObjectState = posDelete );
end;

procedure TVisDemoImage_Delete.Init;
begin
  Query.SQL.Text := 'delete from image_demo where oid = :oid' ;
end;

procedure TVisDemoImage_Delete.SetupParams;
var
  lData : TDemoImage ;
begin
  lData := ( Visited as TDemoImage ) ;
  lData.OID.AssignToTIQuery( Query ) ;
end;

initialization

  gTIPerMgr.RegReadPKVisitor( TVisDemoImage_ReadPK  ) ;
  gTIPerMgr.RegReadVisitor(   TVisDemoImage_ReadOne ) ;
  gTIPerMgr.RegSaveVisitor(   TVisDemoImage_Create  ) ;
  gTIPerMgr.RegSaveVisitor(   TVisDemoImage_Update  ) ;
  gTIPerMgr.RegSaveVisitor(   TVisDemoImage_Delete  ) ;

end.
