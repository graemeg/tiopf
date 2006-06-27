{-----------------------------------------------------------------------------
 Unit Name: tiOIDHex
 Author:    Lukasz Zeligowski
 Purpose:   OID generator. Each OID is 32 character Hexadecimal number. This
            is equal to 128 bit unsigned INTEGER.
            By default 'cache' is 256 OIDs
            There are three usefull 'static' functions:
            TOID.CheckValue - checks if given parameter is proper HEX value
            TOID.IncHex - inc. HEX given by parameter by one
            TOID.ZeroHex - gives ZERO Value Hex with the proper no. of chars

 History:   0.9Beta first version    
-----------------------------------------------------------------------------}

unit tiOIDHex;

{$I tiDefines.inc}

{$IFNDEF OID_AS_INT64}

interface

uses
  tiOID
  ,tiBaseObject
  ,tiObject
  ,tiVisitorDB
  ,tiVisitor
  ;

type
  TOIDHex = class( TOID )
  private
    FAsString : string ;
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
    function  Compare( const pCompareWith : TOID ) : integer ; override ;
    procedure SetToNull; override;
    function  NullOIDAsString : string ; override ;
    class function CheckValue(pValue : ShortString) : boolean;
    class function IncHex(pHex : ShortString; pInc : integer = 1) : ShortString;
    class function ZeroHex : ShortString;
  end ;

  TNextOIDHexData = class( TtiObject )
  private
    FNextHexOID: ShortString;
  public
    property NextHexOID : ShortString read FNextHexOID write FNextHexOID ;
  end ;

  TNextOIDGeneratorHex = class( TNextOIDGenerator )
  private
//    FHigh : Integer ;
    FLow, FLowRange  : integer;
    FLowRangeMask: string;
    FLastOIDValue : string;
    FDirty: boolean;
    FNextOIDHexData : TNextOIDHexData ;
    function NextOID : String ;
  public
    constructor Create ; override ;
    destructor  destroy ; override ;
    procedure   AssignNextOID( const pAssignTo : TOID ; const pDatabaseName : string ; pPerLayerName : string ) ; override ;
  end ;

  TVisDBNextOIDHexAmblerRead = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;

  TVisDBNextOIDHexAmblerUpdate = class( TtiPerObjVisitor )
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;


const
  cOIDClassNameHex = 'OIDClassNameHex' ;
  cgsNextOIDHexReadHigh = 'NextOIDHexReadHigh' ;
  cOIDHexSize = 32;
  cOIDHexChacheSize = 2;

implementation

uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ;

const
  cOIDHexNumber : array [0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
{ TOIDHex }

procedure TOIDHex.Assign(const pSource: TOID);
begin
  AsString := pSource.AsString ;
end;

procedure TOIDHex.AssignFromTIQuery(const pFieldName: string; const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  FAsString := lQuery.FieldAsString[ pFieldName ] ;
end;

procedure TOIDHex.AssignToTIQuery(const pFieldName: string;
  const pQuery: TtiBaseObject);
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  lQuery.ParamAsString[ pFieldName ] := FAsString ;
end;

procedure TOIDHex.AssignToTIQueryParam(const pFieldName: string;const pParams: TtiBaseObject);
var
  lParams : TtiQueryParams ;
begin
  Assert( pParams is TtiQueryParams, 'pQuery not a TtiQuery' ) ;
  lParams := TtiQueryParams( pParams ) ;
  lParams.SetValueAsString(pFieldName, FAsString);
end;

class function TOIDHex.CheckValue(pValue: ShortString): boolean;
var
//  lI64 : int64;
  lHex1, lHex2 : string;
begin
  // Length 32 chars
  result:=false;
  if length(pValue)<>32 then
    exit;
  // Divide: 2 parts 16 chars is 8 bytes is 64 bits...
  lHex1:='$'+Copy(pValue,1,16);
  lHex2:='$'+Copy(pValue,17,16);
  try
    StrToInt64(lHex1);
    StrToInt64(lHex2);   
    //lI64:=StrToInt64(lHex1);
    //lI64:=StrToInt64(lHex2);
    result:=true;
  except
  end;
end;

function TOIDHex.Compare(const pCompareWith: TOID): integer;
begin
  if AsString < pCompareWith.AsString then
    result := -1
  else if AsString > pCompareWith.AsString then
    result := 1
  else
    result := 0 ;
end;

function TOIDHex.EqualsQueryField(const pFieldName: string;
  const pQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery ;
begin
  Assert( pQuery is TtiQuery, 'pQuery not a TtiQuery' ) ;
  lQuery := TtiQuery( pQuery ) ;
  result := ( FAsString = lQuery.FieldAsString[ pFieldName ] ) ;
end;

function TOIDHex.GetAsString: ShortString;
begin
  result:=FAsString;
end;

function TOIDHex.GetAsVariant: Variant;
begin
  result := FAsString ;
end;

class function TOIDHex.IncHex(pHex: ShortString;
  pInc: integer): ShortString;
  procedure _IncHex(pPos : integer);
  var
    lChar : char;
    lValue : integer;
  begin
    if pPos>length(result) then
      raise Exception.Create('Inc Hex (1) exception');
    if pPos<1 then
      raise Exception.Create('Inc Hex (2) exception');
    lChar:=result[pPos];
    lValue:=StrToInt('$'+lChar);
    inc(lValue);
    if lValue>15 then
      _IncHex(pPos-1);
    lValue:=lValue mod 16;
    lChar:=cOIDHexNumber[lValue];
    result[pPos]:=lChar;
  end;
begin
  if pInc<>1 then
    raise Exception.Create('IncHex only with 1');
  result:=pHex;
  _IncHex(length(result));
end;

function TOIDHex.IsNull: boolean;
begin
  result:=(FAsString=NullOIDAsString);
end;

function TOIDHex.NullOIDAsString: string;
begin
  result := '' ;
end;

procedure TOIDHex.SetAsString(const Value: ShortString);
begin
  if CheckValue(Value) then
    FAsString:=Value;
end;

procedure TOIDHex.SetAsVariant(const Value: Variant);
begin
  FAsString := Value ;
end;

procedure TOIDHex.SetToNull;
begin
  FAsString:= NullOIDAsString;
end;

class function TOIDHex.ZeroHex: ShortString;
begin
  result:=StringOfChar('0',cOIDHexSize);
end;

{ TNextOIDGeneratorHex }

procedure TNextOIDGeneratorHex.AssignNextOID(const pAssignTo: TOID; const pDatabaseName : string ; pPerLayerName : string );
begin
  Assert( pAssignTo.TestValid(TOID), cTIInvalidObjectError ) ;
  pAssignTo.AsString := NextOID;
end;

constructor TNextOIDGeneratorHex.Create;
begin
  inherited;
  FLow  := 0;
  FLowRangeMask := StringOfChar('0',cOIDHexChacheSize);;
  FLowRange:=StrToInt('$1'+FLowRangeMask);
  FDirty := true ;
  FNextOIDHexData := TNextOIDHexData.Create ;
end;

destructor TNextOIDGeneratorHex.destroy;
begin
  FNextOIDHexData.Free ;
  inherited;
end;

function TNextOIDGeneratorHex.NextOID: String;
begin
  if FDirty then
  begin
    gTIOPFManager.VisMgr.Execute( cgsNextOIDHexReadHigh, FNextOIDHexData ) ;
    FDirty := false ;
    FLastOIDValue:=FNextOIDHexData.NextHexOID + FLowRangeMask;
  end ;

  result := TOIDHex.IncHex(FLastOIDValue);


  inc(FLow);
  if FLow = FLowRange then
  begin
    FDirty := true ;
    FLow  := 0 ;
  end ;

  FLastOIDValue:=result;
end;

{ TVisDBNextOIDHexAmblerRead }

function TVisDBNextOIDHexAmblerRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDHexData ) ;
end;

procedure TVisDBNextOIDHexAmblerRead.Execute(const pData: TtiVisited);
begin
  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  Query.SelectRow( 'Next_OIDHEX', nil ) ;
  try
    TNextOIDHexData( Visited ).NextHexOID := Query.FieldAsString[ 'OID' ] ;
    if TNextOIDHexData( Visited ).NextHexOID='' then
      TNextOIDHexData( Visited ).NextHexOID:=StringOfChar('0',cOIDHexSize-cOIDHexChacheSize);;
  finally
    Query.Close ;
  end ;
end;

{ TVisDBNextOIDHexAmblerUpdate }

function TVisDBNextOIDHexAmblerUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TNextOIDHexData ) ;
end;

procedure TVisDBNextOIDHexAmblerUpdate.Execute(const pData: TtiVisited);
var
  lParams : TtiQueryParams ;
  lHex : ShortString;
begin
  if gTIOPFManager.Terminated then
    Exit ; //==>

  Inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  lParams := TtiQueryParams.Create ;
  try
    lHex:=TNextOIDHexData( Visited ).NextHexOID;
    lHex:=TOIDHex.IncHex(lHex);
    lParams.SetValueAsString('OID', String(lHex));
    Query.UpdateRow( 'Next_OIDHEX', lParams, nil ) ;
  finally
    lParams.Free ;
  end ;
end;

initialization

  gTIOPFManager.OIDFactory.RegisterMapping( cOIDClassNameHex, TOIDHex, TNextOIDGeneratorHex )  ;

  if gTIOPFManager.DefaultOIDClassName = '' then
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameHex ;

  gTIOPFManager.VisMgr.RegisterVisitor( cgsNextOIDHexReadHigh, TVisDBNextOIDHexAmblerRead ) ;
  gTIOPFManager.VisMgr.RegisterVisitor( cgsNextOIDHexReadHigh, TVisDBNextOIDHexAmblerUpdate ) ;

{$ELSE}
interface
implementation
{$ENDIF}

end.
