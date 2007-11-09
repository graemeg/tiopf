unit tiOID;

{$I tiDefines.inc}

{$IFDEF OID_AS_INT64}
  {$I tiOIDAsInt64.pas}
{$ELSE}

interface

uses
  Classes
  ,tiBaseObject
  ,Contnrs
  ,tiVisitor
 ;

const
  cDefaultOIDFieldName       = 'OID';

type

  // The abstract OID class
  TOID = class(TtiBaseObject)
  private
  protected
    function  GetAsString: ShortString; virtual; abstract;
    procedure SetAsString(const AValue: ShortString); virtual; abstract;
    function  GetAsVariant: Variant;virtual; abstract;
    procedure SetAsVariant(const AValue: Variant);virtual; abstract;
  public
    constructor Create; virtual;
    property    AsString : ShortString read GetAsString write SetAsString;
    property    AsVariant : Variant read GetAsVariant write SetAsVariant;
    function    IsNull : boolean; virtual; abstract;
    procedure   AssignToTIQueryParam(const AFieldName : string; const AParams : TtiBaseObject); virtual; abstract;
    procedure   AssignToTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); overload; virtual; abstract;
    procedure   AssignFromTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); overload; virtual; abstract;
    procedure   AssignToTIQuery(const AQuery : TtiBaseObject); overload;
    procedure   AssignFromTIQuery(const AQuery : TtiBaseObject); overload;
    function    EqualsQueryField(const AFieldName : string; const AQuery : TtiBaseObject): boolean; virtual; abstract;
    procedure   Assign(const ASource : TOID); reintroduce; virtual;
    function    Compare(const ACompareWith : TOID): integer; virtual; abstract;
    function    Equals(const ACompareWith : TOID): boolean;
    procedure   SetToNull; virtual; abstract;
    function    Clone : TOID;
    function    NullOIDAsString : string; virtual; abstract;

    procedure   GetNextValue(const ADatabaseName : string; const APersistenceLayerName : string); virtual;
  end;

  TOIDClass = class of TOID;

  // Each TOID must have an associated TNextOIDGenerator that is responsible
  // for returning the next OID for that generation stratergy
  TNextOIDGenerator = class(TtiBaseObject)
  private
    FDatabaseName: string;
  public
    constructor Create; virtual;
    procedure   AssignNextOID(const AAssignTo : TOID; const ADatabaseName : string; APersistenceLayerName : string); virtual; abstract;
    property    DatabaseName : string read FDatabaseName write FDatabaseName;
  end;

  TNextOIDGeneratorClass = class of TNextOIDGenerator;

  // Keeps a list of databases and their associated OIDGenerators
  // For example, you might be working with two databases, using the TOIDInteger
  // class of TOID. You might have a different Next_OID table in each database.
  // The TNextOIDMgr keeps track of this.
  // ToDo: Must be able to override this so all OIDs come from one source
  //       This should be given some more thought as this Mgr was introduced
  //       when we had only one class of OID. The relationship between
  //       Persistence Layer->OIDClass->Database->Instance of NextOIDGenerator
  //       must be tidied up.
  TNextOIDMgr = class(TtiBaseObject)
  private
    FList : TObjectList;
    FOwner: TtiBaseObject;
  protected
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   AssignNextOID(const AAssignTo : TOID; const ADatabaseName : string); virtual;
    procedure   UnloadNextOIDGenerator(const ADatabaseName : string);
    function    FindCreateByDatabaseName(const ADatabaseName : string): TNextOIDGenerator;
    function    FindByDatabaseName(const ADatabaseName : string): TNextOIDGenerator;
    procedure   Clear;
    property    Owner : TtiBaseObject read FOwner write FOwner; // A TtiPersistenceLayer
  end;

  // A mapping betweeen a TOID and TNextOIDGenerator. Used under the hood
  // to populate TOID.AValue with the correct value
  TOIDClassMapping = class(TtiBaseObject)
  private
    FOIDClassName: string;
    FOIDClass: TOIDClass;
    FNextOIDGeneratorClass: TNextOIDGeneratorClass;
  public
    property OIDClass : TOIDClass read FOIDClass write FOIDClass;
    property OIDClassName : string read FOIDClassName write FOIDClassName;
    property NextOIDGeneratorClass : TNextOIDGeneratorClass read FNextOIDGeneratorClass write FNextOIDGeneratorClass;
  end;

  // The factory produces the correct class of TOID and it's associated
  // TNextOIDGenerator
  TOIDFactory = class(TtiBaseObject)
  protected
    // These are protected so they can be accessed in a descendant class
    // for unit testing.
    FList : TObjectList;
    function FindByOIDClassName(const AClassName : string) : TOIDClassMapping;
  public
    Constructor Create;
    Destructor  Destroy; override;
    procedure   RegisterMapping(const AOIDClassName : string; const pOIDClass : TOIDClass; const pNextOIDGeneratorClass : TNextOIDGeneratorClass);
    function    CreateOID(const AOIDClassName : string = ''): TOID;
    function    CreateNextOIDGenerator(const AOIDClassName : string): TNextOIDGenerator;
  end;

  function OIDToString(const AOID : TOID): string;
  function OIDEquals(const AOID1, AOID2 : TOID): boolean;
  function OIDCompare(const AOID1, AOID2 : TOID): Integer;

implementation
uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiOPFManager
  ,tiObject
  ,tiVisitorDB
  ,tiConstants
  ,tiPersistenceLayers
  ,tiLog
 ;

function OIDToString(const AOID : TOID): string;
begin
  result := AOID.AsString;
end;

function OIDEquals(const AOID1, AOID2 : TOID): boolean;
begin
  result := AOID1.Equals(AOID2);
end;

function OIDCompare(const AOID1, AOID2 : TOID): Integer;
begin
  result := AOID1.Compare(AOID2);
end;

constructor TOIDFactory.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

function TOIDFactory.CreateNextOIDGenerator(const AOIDClassName: string): TNextOIDGenerator;
var
  lOIDClassMapping : TOIDClassMapping;
begin
  lOIDClassMapping := FindByOIDClassName(AOIDClassName);
  Assert(lOIDClassMapping <> nil, 'Attempt to create unregistered OID class <' + AOIDClassName + '>');
  result := lOIDClassMapping.NextOIDGeneratorClass.Create;
end;

function TOIDFactory.CreateOID(const AOIDClassName: string = ''): TOID;
var
  lOIDClassMapping : TOIDClassMapping;
  lOIDClassName : string;
begin
  if AOIDClassName = '' then
    lOIDClassName := gTIOPFManager.DefaultOIDClassName
  else
    lOIDClassName := AOIDClassName;

  lOIDClassMapping := FindByOIDClassName(lOIDClassName);
  Assert(lOIDClassMapping <> nil,
          'Attempt to create unregistered OID class <' + lOIDClassName + '>' + Cr(2) +
          'You must include one of the delphi pas files tiOIDXXX.pas in your application.');
  result := lOIDClassMapping.OIDClass.Create;

end;

destructor TOIDFactory.Destroy;
begin
  FList.Free;
  inherited;
end;

function TOIDFactory.FindByOIDClassName(const AClassName : string): TOIDClassMapping;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(TOIDClassMapping(FList.Items[i]).OIDClassName, AClassName) then
    begin
      result := TOIDClassMapping(FList.Items[i]);
      Exit; //==>
    end;
end;

procedure TOIDFactory.RegisterMapping(
  const AOIDClassName: string;
  const pOIDClass: TOIDClass;
  const pNextOIDGeneratorClass : TNextOIDGeneratorClass);
var
  lOIDClassMapping : TOIDClassMapping;
begin
  if FindByOIDClassName(AOIDClassName) <> nil then
    raise Exception.Create('Attempt to register duplicated OID type: ' + AOIDClassName + Cr(2) +
                'You can only include one of the delphi PAS files tiOIDXXX.pas in your application.');
  lOIDClassMapping := TOIDClassMapping.Create;
  lOIDClassMapping.OIDClassName := AOIDClassName;
  lOIDClassMapping.OIDClass := pOIDClass;
  lOIDClassMapping.NextOIDGeneratorClass := pNextOIDGeneratorClass;
  FList.Add(lOIDClassMapping);

end;

procedure TOID.AssignFromTIQuery(const AQuery: TtiBaseObject);
begin
  AssignFromTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TOID.AssignToTIQuery(const AQuery: TtiBaseObject);
begin
  AssignToTIQuery(cDefaultOIDFieldName, AQuery);
end;

procedure TOID.Assign(const ASource : TOID);
begin
  Assert(false, ClassName + '.Assign not implemented');
end;

function TOID.Clone: TOID;
begin
  result:=TOID(TOIDClass(ClassType).Create);
  result.Assign(self);
end;

constructor TOID.Create;
begin
  inherited;
  SetToNull; // Just to be sure that ALWAYS it will be setted to NULL value...
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDMgr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNextOIDMgr.Create;
begin
  inherited create;
  FList := TObjectList.Create;
end;

destructor TNextOIDMgr.destroy;
begin
  FList.Free;
  inherited;
end;

function TNextOIDMgr.FindCreateByDatabaseName(const ADatabaseName: string): TNextOIDGenerator;
begin
  result := FindByDatabaseName(ADatabaseName);
  if result = nil then
  begin
    result :=
      gTIOPFManager.OIDFactory.CreateNextOIDGenerator(
        gTIOPFManager.DefaultOIDClassName);
    result.DatabaseName := ADatabaseName;
    FList.Add(result);
  end;
end;

function TNextOIDMgr.FindByDatabaseName(const ADatabaseName: string): TNextOIDGenerator;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(ADatabaseName,
                 TNextOIDGenerator(FList.Items[i]).DatabaseName) then
    begin
      result := TNextOIDGenerator(FList.Items[i]);
      Exit; //==>
    end;
end;

procedure TNextOIDMgr.AssignNextOID(const AAssignTo : TOID; const ADatabaseName : string);
var
  lNextOIDGenerator : TNextOIDGenerator;
  lRegPerLayer : TtiPersistenceLayer;
begin
  Assert(AAssignTo.TestValid(TOID), cTIInvalidObjectError);
  Assert(ADatabaseName <> '', 'Database name not assigned');
  lNextOIDGenerator := FindCreateByDatabaseName(ADatabaseName);
  Assert(lNextOIDGenerator.TestValid(TNextOIDGenerator), cTIInvalidObjectError + ' No NextOIDGenerator found for ' + ADatabaseName);
  lRegPerLayer := Owner as TtiPersistenceLayer;
  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);
  lNextOIDGenerator.AssignNextOID(AAssignTo, ADatabaseName, lRegPerLayer.PersistenceLayerName);
end;

procedure TNextOIDMgr.UnloadNextOIDGenerator(const ADatabaseName: string);
var
  lNextOIDGenerator : TNextOIDGenerator;
begin
  lNextOIDGenerator := FindByDatabaseName(ADatabaseName);
  Assert(lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + ADatabaseName);
  FList.Remove(lNextOIDGenerator);
end;

function TOID.Equals(const ACompareWith: TOID): boolean;
begin
  result := Compare(ACompareWith) = 0;
end;

procedure TOID.GetNextValue(const ADatabaseName: string; const APersistenceLayerName : string);
var
  lRegPerLayer : TtiPersistenceLayer;
  lDatabaseName : string;
begin
  if APersistenceLayerName = '' then
    lRegPerLayer := gTIOPFManager.DefaultPerLayer
  else
    lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(APersistenceLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + APersistenceLayerName + '>');
  if ADatabaseName = '' then
    lDatabaseName := gTIOPFManager.DefaultDBConnectionName
  else
    lDatabaseName := ADatabaseName;
  Assert(lDatabaseName <> '', 'Unable to determine DatabaseName');
  lRegPerLayer.NextOIDMgr.AssignNextOID(Self, lDatabaseName);
end;

{ TNextOIDGenerator }

constructor TNextOIDGenerator.Create;
begin
  inherited;
end;

procedure TNextOIDMgr.Clear;
begin
  FList.Clear;
end;

{$ENDIF}

end.







