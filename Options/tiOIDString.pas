unit tiOIDString;

{$I tiDefines.inc}

interface
uses
  tiOID
  ,tiBaseObject
  ,tiObject
  ,tiVisitorDB
  ,tiVisitor
  ,tiConstants
  ;

const
  cErrorInvalidStartID = 'Invalid start ID <%s>';

type

  TNextOIDGeneratorString = class ;

  TOIDString = class( TOID )
  private
    FAsString : String ;
  protected
    function  GetAsString: ShortString; override ;
    procedure SetAsString(const Value: ShortString); override ;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const Value: Variant);override;
  public
    function  IsNull : boolean ; override ;
    procedure AssignToTIQueryParam( const pFieldName : string ; const pParams : TtiBaseObject ) ; override ;
    procedure AssignToTIQuery( const pFieldName : string ; const pQuery : TtiBaseObject ) ; override ;
    procedure AssignFromTIQuery( const pFieldName : string ; const pQuery : TtiBaseObject ) ; override ;
    function  EqualsQueryField( const pFieldName : string ; const pQuery : TtiBaseObject ) : boolean ; override;
    procedure Assign( const pSource : TOID ) ; override ;
    function  Compare( const pCompareWith : TOID ) : Integer ; override ;
    procedure SetToNull; override;
    function  NullOIDAsString: String; override ;
  end ;

  TNextOIDData = class( TtiObject )
  private
    FNextOID: String;
    FOIDGenerator: TNextOIDGeneratorString;
  public
    property NextOID : String read FNextOID write FNextOID ;
    property OIDGenerator : TNextOIDGeneratorString read FOIDGenerator write FOIDGenerator ;
  end ;

  TNextOIDGeneratorString = class( TNextOIDGenerator )
  private
    FOIDChars : array of char ;
    FLow  : String ;
    FDirty: boolean;
    FNextOIDData : TNextOIDData ;
    FOIDLength: Byte;
    FOIDPrefix: string;
    function  NextOID( const pDatabaseName : string ; pPerLayerName : string ) : String ;
    function  GetOIDChars: string;
    procedure SetOIDChars(const Value: string);
    function  PadToLength(const Value : string ) : string ;
    function  GetHighOIDLength: Byte;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ; pPerLayerName : string ) ; override ;
    function    IncOID(const pCurrentOID: String): String;
    property    HighOIDLength : Byte read GetHighOIDLength ;

    // You might want to set these at startup
    property    OIDChars : string read GetOIDChars write SetOIDChars ;
    property    OIDLength : Byte read FOIDLength write FOIDLength ;
    property    OIDPrefix : string read FOIDPrefix write FOIDPrefix ;
  end ;

  TVisDBNextOIDAmblerRead = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;

  TVisDBNextOIDAmblerUpdate = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;

const
  cOIDClassNameString = 'OIDClassNameString' ;
  cNextOIDTableName   = 'Next_OID' ;
  cNextOIDFieldName   = 'OID' ;
  // '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  // 88 Characters, so if the OID string is 4 characters long, there are
  // Power( 88, 4 ) available OIDs = 59969536
  // Power( 88, 5 ) available OIDs = 5277319168
  // Integer gives                   2147483647
  // Int64 gives                     4610000000000000000

  cOIDChars           = '!#$%&()*+,-./0123456789:;<=>?@'+
                        'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                        '[\]^_' +
                        'abcdefghijklmnopqrstuvwxyz' ;


implementation
uses
  tiQuery
  ,tiUtils
  ,tiOPFManager
  ,tiLog
  ,tiExcept
  ,SysUtils
  ;

{ TOIDString }

function TOIDString.getAsString: ShortString;
begin
  result := FAsString ;
end;

procedure TOIDString.SetAsString(const Value: ShortString);
begin
  FAsString := Value ;
end;

function TOIDString.IsNull: boolean;
begin
  result := FAsString = NullOIDAsString ;
end;

procedure TOIDString.AssignFromTIQuery(const pFieldName : string ; const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsString := lQuery.FieldAsString[ pFieldName ] ;
end;

procedure TOIDString.AssignToTIQuery(const pFieldName : string ; const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsString[ pFieldName ] := FAsString ;
end;

function TOIDString.EqualsQueryField(const pFieldName: string; const pQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsString = lQuery.FieldAsString[ pFieldName ] ) ;
end;

procedure TOIDString.Assign(const pSource: TOID);
begin
  AsString := pSource.AsString ;
end;

function TOIDString.Compare(const pCompareWith: TOID): Integer;
begin
  Assert( pCompareWith is TOIDString, 'pCompareWith not a pCompareWith' ) ;
  if AsString < TOIDString(pCompareWith).AsString then
    result := -1
  else if AsString > TOIDString(pCompareWith).AsString then
    result := 1
  else
    result := 0 ;
end;

const
  cuLowRange = 100 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorString
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorString.AssignNextOID(const pAssignTo: TOID; const pDatabaseName : string ; pPerLayerName : string );
begin
  Assert( pAssignTo.TestValid(TOID), cTIInvalidObjectError ) ;
  pAssignTo.AsString := NextOID( pDatabaseName, pPerLayerName ) ;
end;

constructor TNextOIDGeneratorString.Create;
begin
  inherited;
  SetOIDChars(cOIDChars);
  OIDLength := 10 ;
  FDirty := true ;
  FNextOIDData := TNextOIDData.Create ;
end;

destructor TNextOIDGeneratorString.Destroy;
begin
  FNextOIDData.Free ;
  inherited;
end;

function TNextOIDGeneratorString.NextOID(const pDatabaseName : string ; pPerLayerName : string ): String;
begin
  if FDirty then
  begin
    FLow := FOIDChars[0] ;
    FNextOIDData.OIDGenerator := Self ;
    gTIOPFManager.VisitorManager.Execute( cNextOIDReadHigh, FNextOIDData, pDatabaseName, pPerLayerName ) ;
    FDirty := false ;
  end else
  begin
    FLow := IncOID( FLow ) ;
    if FLow = FOIDChars[High(FOIDChars)] then
      FDirty := true ;
  end ;

  result := FOIDPrefix + FNextOIDData.NextOID + FLow ;
end;

procedure TOIDString.SetToNull;
begin
  FAsString := NullOIDAsString ;
end;

function TOIDString.GetAsVariant: Variant;
begin
  result := FAsString ;
end;

procedure TOIDString.SetAsVariant(const Value: Variant);
begin
  FAsString := Value ;
end;

function TOIDString.NullOIDAsString: String;
begin
  result := '' ;
end;

procedure TOIDString.AssignToTIQueryParam(const pFieldName: string;const pParams: TtiBaseObject);
var
  lParams : TtiQueryParams ;
begin
  Assert( pParams is TtiQueryParams, 'pQuery not a TtiQuery' ) ;
  lParams := TtiQueryParams( pParams ) ;
  lParams.SetValueAsString(pFieldName, FAsString);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDData ) ;
end;

procedure TVisDBNextOIDAmblerRead.Execute(const pData: TtiVisited);
var
  lNextOIDData : TNextOIDData ;
  lNextOID : String ;
begin

  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  Query.SelectRow( cNextOIDTableName, nil ) ;
  try
    lNextOID := Query.FieldAsString[ cNextOIDFieldName ] ;
  finally
    Query.Close ;
  end ;
  lNextOIDData := ( Visited as TNextOIDData ) ;
  TNextOIDData( Visited ).NextOID :=
    lNextOIDData.OIDGenerator.PadToLength( lNextOID ) ;

end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDData ) ;
end;

function TNextOIDGeneratorString.IncOID(const pCurrentOID: String): String;
  function _GeTNextIDGenerator( const pCurrentOID: String ) : String ;
  var
    i : integer ;
    lLastPos  : integer ;
    lLastChar : Char ;
  begin
    result := pCurrentOID;
    lLastPos := Length(pCurrentOID) ;
    lLastChar := pCurrentOID[lLastPos];
    if lLastChar = FOIDChars[High(FOIDChars)] then
    begin
      result :=
       _GeTNextIDGenerator(
          Copy(pCurrentOID,1, lLastPos-1));
      result := result + FOIDChars[Low(FOIDChars)];
      if lLastPos = 1 then
        result := FOIDChars[Low(FOIDChars)+1] + result ;
      Exit ; //==>
    end ;

    for i := Low(FOIDChars) to High(FOIDChars)-1 do
      if FOIDChars[i] = lLastChar then
      begin
        result := Copy( pCurrentOID, 1, Length(pCurrentOID)-1 ) + FOIDChars[i+1] ;
        Exit ; //==>
      end ;
    end ;
var
  i, j : integer ;
  lChar : Char ;
  lCurrentOID : string ;
begin
  lCurrentOID := pCurrentOID ;     
  for i := 1 to Length( lCurrentOID ) do
    for j := Low( FOIDChars ) to High( FOIDChars ) do
    begin
      lChar := lCurrentOID[i] ;
      if lChar = FOIDChars[j] then
        Break //==>
      else if j < High( FOIDChars ) then
        Continue ; //==>
      // Should only get here if there is an invalid character
      raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidStartID, [lCurrentOID]);
    end ;
  result := _GetNextIDGenerator(lCurrentOID);
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const pData: TtiVisited);
var
  lParams : TtiQueryParams ;
  lNextOID : string ;
  lNextOIDData : TNextOIDData ;
begin
  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  lNextOIDData := ( Visited as TNextOIDData ) ;
  lParams := TtiQueryParams.Create ;
  try
    lNextOID := lNextOIDData.OIDGenerator.IncOID( lNextOIDData.NextOID );
    lParams.SetValueAsString(cNextOIDFieldName, lNextOID)  ;
    Query.UpdateRow( cNextOIDTableName, lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;

function TNextOIDGeneratorString.GetOIDChars: string;
var
  i : integer ;
begin
  result := '' ;
  for i := Low(FOIDChars) to High(FOIDChars) do
    result := result + FOIDChars[i];
end;

procedure TNextOIDGeneratorString.SetOIDChars(const Value: string);
var
  i : integer ;
begin
  SetLength( FOIDChars, Length(Value) ) ;
  for i := 1 to Length(Value) do
    FOIDChars[i-1] := Value[i];
end;

function TNextOIDGeneratorString.PadToLength(const Value: string): string;
begin
  Result := Value ;
  if length( Value ) < HighOIDLength then begin
    while length( Result ) < HighOIDLength do begin
      result := OIDChars[1] + result ;
    end ;
  end
  else if length( Value ) > HighOIDLength then
    result := copy( Value, length( Value )-HighOIDLength+1, HighOIDLength ) ;
end;

function TNextOIDGeneratorString.GetHighOIDLength: Byte;
begin
  result := OIDLength - 1 - Length( FOIDPrefix ) ;
end;

initialization

  gTIOPFManager.OIDFactory.RegisterMapping( cOIDClassNameString, TOIDString, TNextOIDGeneratorString )  ;
  if gTIOPFManager.DefaultOIDClassName = '' then
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameString ;

  gTIOPFManager.VisitorManager.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerRead ) ;
  gTIOPFManager.VisitorManager.RegisterVisitor( cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate ) ;

end.
