unit tiOIDIntFBGen;

{ This is a clone of tiOIDInt64, but for use with firbird/interbase generators.
  Created by Carlo Marona 06 jan 2008
  }

{$I tiDefines.inc}

interface
uses
  tiOID
  ,tiBaseObject
  ,tiObject
  ,tiVisitorDB
  ,tiVisitor
  ,SyncObjs
 ;

type

  //If you want to use many OID, one for every class, you must declare one
  //OID class for every persistance object class inheriting from that.
  //Also you must register those new OID classes in the OIDFactory.
  TOIDIntFBGen = class(TtiOID)
  private
    FAsInt64 : Int64;
  protected
    function  GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const AValue: Variant);override;
    //If you want to use one OID for everyone class you must override this
    //method in your new derived OID class
    function GetFBGeneratorName: string; virtual;
  public
    function  IsNull : boolean; override;
    procedure AssignToTIQueryParam(const AFieldName : string; const AParams : TtiBaseObject); override;
    procedure AssignToTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    procedure AssignFromTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    function  EqualsQueryField(const AFieldName : string; const AQuery : TtiBaseObject): boolean; override;
    procedure Assign(const ASource : TtiOID); override;
    function  Compare(const ACompareWith : TtiOID): Integer; override;
    procedure  SetToNull; override;
    function    NullOIDAsString : string; override;

    property AsInt64 : Int64 read FAsInt64 write FAsInt64;
    //This property will returns the Firebird/Interbase generator name
    //used to retrieve the next OID
    property FBGeneratorName: string read GetFBGeneratorName;
  end;

  TNextOIDFBGenData = class(TtiObject)
  private
    FNextOID: Int64;
    FFBGeneratorName: string;
  public
    property NextOID: Int64 read FNextOID write FNextOID;
    //This property will contains the Firebird/Interbase generator name
    //used by the visitor TVisDBNextOIDAmblerRead
    property FBGeneratorName: string read FFBGeneratorName write FFBGeneratorName;
  end;

  TOIDGeneratorIntFBGenAbs = class(TtiOIDGenerator)
  private
    FNextOIDData: TNextOIDFBGenData;
    FCritSection: TCriticalSection;
    function NextOID(const ADatabaseName: string;
      APersistenceLayerName: string): Int64;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

  TOIDGeneratorIntFBGen = class(TOIDGeneratorIntFBGenAbs)
  public
    class function OIDClass: TtiOIDClass; override;
  end;

  TVisDBNextOIDAmblerRead = class(TtiObjectVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;


//  procedure RegisterNewOIDIntFBGenMapping(const AOIDClassName: string;
//    const AOIDClass: TtiOIDClass);



implementation
uses
  tiQuery
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
 ;




const
  cDefaultOIDClassName = 'OIDClassNameIntFBGen';
  cDefaultFBGeneratorName = 'GEN_NEXT_OID';




//procedure RegisterNewOIDIntFBGenMapping(const AOIDClassName: string;
//  const AOIDClass: TtiOIDClass);
//begin
//  gTIOPFManager.OIDFactory.RegisterMapping(AOIDClassName, AOIDClass,
//    TNextOIDGeneratorIntFBGen) ;
//end;




{ TOIDIntFBGen }

function TOIDIntFBGen.getAsString: String;
begin
  result := IntToStr(FAsInt64);
end;

procedure TOIDIntFBGen.SetAsString(const AValue: String);
begin
  FAsInt64 := StrToInt(AValue);
end;

function TOIDIntFBGen.IsNull: boolean;
begin
  result := FAsInt64 = cNullOIDInteger;
end;

procedure TOIDIntFBGen.AssignFromTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  FAsInt64 := lQuery.FieldAsInteger[ AFieldName ];
end;

procedure TOIDIntFBGen.AssignToTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  lQuery.ParamAsInteger[ AFieldName ]:= FAsInt64;
end;

function TOIDIntFBGen.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  result := (FAsInt64 = lQuery.FieldAsInteger[ AFieldName ]);
end;

procedure TOIDIntFBGen.Assign(const ASource: TtiOID);
begin
  AsString := ASource.AsString;
end;

function TOIDIntFBGen.Compare(const ACompareWith: TtiOID): Integer;
begin
  Assert(ACompareWith is TOIDIntFBGen, 'ACompareWith not a TOIDIntFBGen');
  
  if AsInt64 < TOIDIntFBGen(ACompareWith).AsInt64 then
    result := -1
  else if AsInt64 > TOIDIntFBGen(ACompareWith).AsInt64 then
    result := 1
  else
    result := 0;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorIntFBGenAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TOIDGeneratorIntFBGenAbs.AssignNextOID(const AAssignTo: TtiOID;
  const ADatabaseAliasName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);

  FNextOIDData.FBGeneratorName := TOIDIntFBGen(AAssignTo).FBGeneratorName;

  AAssignTo.AsString :=
    IntToStr(NextOID(ADatabaseAliasName, APersistenceLayerName));
end;

constructor TOIDGeneratorIntFBGenAbs.Create;
begin
  inherited;

  FNextOIDData := TNextOIDFBGenData.Create;
  FCritSection:= TCriticalSection.Create;
end;

destructor TOIDGeneratorIntFBGenAbs.destroy;
begin
  FCritSection.Free;
  FNextOIDData.Free;
  inherited;
end;

function TOIDGeneratorIntFBGenAbs.NextOID(const ADatabaseName : string;
  APersistenceLayerName : string): Int64;
begin
  FCritSection.Enter;

  try
    gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData,
      ADatabaseName, APersistenceLayerName);

    result := FNextOIDData.NextOID;
  finally
    FCritSection.Leave;
  end;
end;

class function TOIDGeneratorIntFBGen.OIDClass: TtiOIDClass;
begin
  Result := TOIDIntFBGen;
end;

procedure TOIDIntFBGen.SetToNull;
begin
  FAsInt64 := cNullOIDInteger;
end;

function TOIDIntFBGen.GetAsVariant: Variant;
begin
  result := FAsInt64;
end;

function TOIDIntFBGen.GetFBGeneratorName: string;
begin
  result := cDefaultFBGeneratorName;
end;

procedure TOIDIntFBGen.SetAsVariant(const AValue: Variant);
begin
  FAsInt64 := AValue;
end;

function TOIDIntFBGen.NullOIDAsString: string;
begin
  result := IntToStr(cNullOIDInteger);
end;

procedure TOIDIntFBGen.AssignToTIQueryParam(const AFieldName: string;const AParams: TtiBaseObject);
var
  lParams : TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsInteger(AFieldName, FAsInt64);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDFBGenData);
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SQLText := Format('Select gen_id(%s, 1) as OID from RDB$DATABASE',
    [TNextOIDFBGenData(Visited).FBGeneratorName]);

  Query.Open;

  try
    TNextOIDFBGenData(Visited).NextOID := Query.FieldAsInteger['OID'];
  finally
    Query.Close;
  end;
end;


initialization

//  gTIOPFManager.OIDFactory.RegisterMapping(cDefaultOIDClassName, TOIDIntFBGen,
//    TNextOIDGeneratorIntFBGen) ;

//  if gTIOPFManager.DefaultOIDClassName = '' then
//    gTIOPFManager.DefaultOIDClassName := cDefaultOIDClassName;

//  if gTIOPFManager.DefaultOIDGenerator = nil then
//    gTIOPFManager.DefaultOIDGenerator := TOIDGeneratorIntFBGen.Create;

  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh,
    TVisDBNextOIDAmblerRead);

end.
