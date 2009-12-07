unit tiOIDManager;

interface

uses
  Contnrs,
  tiBaseObject,
  tiObject,
  tiOID;



type
  TOIDClassMapping = class(TtiBaseObject)
  private
    FOIDClass: TtiOIDClass;
    FOIDGeneratorClass: TtiOIDGeneratorClass;
    FObjectClass: TtiObjectClass;
    FOIDGenerator: TtiOIDGenerator;
    function GetOIDGenerator: TtiOIDGenerator;
  public
    destructor Destroy; override;

    property ObjectClass: TtiObjectClass read FObjectClass write FObjectClass;
    property OIDClass : TtiOIDClass read FOIDClass write FOIDClass;
    property OIDGenerator: TtiOIDGenerator read GetOIDGenerator;
    property OIDGeneratorClass : TtiOIDGeneratorClass read FOIDGeneratorClass
      write FOIDGeneratorClass;
  end;

  TOIDManager = Class(TtiBaseObject)
  private
    FUseDefaultMapping: boolean;
    function GetDefaultOIDClass: TtiOIDClass;
    function GetDefaultOIDGeneratorClass: TtiOIDGeneratorClass;
    procedure SetDefaultOIDClass(const Value: TtiOIDClass);
    procedure SetDefaultOIDGeneratorClass(const Value: TtiOIDGeneratorClass);
  protected
    FList : TObjectList;
    function FindByObjectClass(
      const pObjectClass: TtiObjectClass): TOIDClassMapping;
    function FindByObjectClassName(
      const AObjectClassName: string): TOIDClassMapping;
    procedure _RegisterMapping(
      const pObjectClass: TtiObjectClass;
      const pOIDGeneratorClass: TtiOIDGeneratorClass;
      const pOIDClass: TtiOIDClass);
  public
    constructor Create;
    destructor Destroy; override;

    function GetOIDClass(const pObjectClass: TtiObjectClass): TtiOIDClass; overload;
    function GetOIDClass(const AObjectClassName: string): TtiOIDClass; overload;
    function GetOIDGenerator(
      const pObjectClass: TtiObjectClass): TtiOIDGenerator; overload;
    function GetOIDGenerator(
      const AObjectClassName: string): TtiOIDGenerator; overload;
    procedure RegisterMapping(
      const pObjectClass: TtiObjectClass;
      const pOIDGeneratorClass: TtiOIDGeneratorClass;
      const pOIDClass: TtiOIDClass);

    property DefaultOIDClass: TtiOIDClass read GetDefaultOIDClass
      write SetDefaultOIDClass;
    property DefaultOIDGeneratorClass: TtiOIDGeneratorClass
      read GetDefaultOIDGeneratorClass write SetDefaultOIDGeneratorClass;
    property UseDefaultMapping: boolean read FUseDefaultMapping
      write FUseDefaultMapping;
  End;


  function OIDManager: TOIDManager;


implementation


uses
  SysUtils,
  tiUtils,
  tiOPFManager;



var
  OIDManagerInst: TOIDManager;



function OIDManager: TOIDManager;
begin
  if not Assigned(OIDManagerInst) then
    OIDManagerInst := TOIDManager.Create;

  result := OIDManagerInst;
end;




{ TOIDManager }

constructor TOIDManager.Create;
begin
  inherited;

  FList := TObjectList.Create(True);
  FList.OwnsObjects := true;
  FUseDefaultMapping := false;
end;

destructor TOIDManager.Destroy;
begin
  FList.Free;

  inherited;
end;

function TOIDManager.FindByObjectClass(
  const pObjectClass: TtiObjectClass): TOIDClassMapping;
var
  I : integer;
begin
  result := nil;

  for I := 0 to FList.Count - 1 do
    if TOIDClassMapping(FList.Items[i]).ObjectClass = pObjectClass then
    begin
      result := TOIDClassMapping(FList.Items[i]);

      Exit; //==>
    end;
end;

function TOIDManager.FindByObjectClassName(
  const AObjectClassName: string): TOIDClassMapping;
var
  I : integer;
begin
  result := nil;

  for I := 0 to FList.Count - 1 do
    if TOIDClassMapping(FList.Items[i]).ObjectClass.ClassName = AObjectClassName then
    begin
      result := TOIDClassMapping(FList.Items[i]);

      Exit; //==>
    end;
end;

function TOIDManager.GetDefaultOIDClass: TtiOIDClass;
var
  lOIDObjectClassMapping: TOIDClassMapping;
begin
  result := nil;

  lOIDObjectClassMapping := FindByObjectClass(nil);

  if lOIDObjectClassMapping = nil then
    Exit; //-->

  result := lOIDObjectClassMapping.OIDClass;
end;

function TOIDManager.GetDefaultOIDGeneratorClass: TtiOIDGeneratorClass;
var
  lOIDObjectClassMapping: TOIDClassMapping;
begin
  result := nil;

  lOIDObjectClassMapping := FindByObjectClass(nil);

  if lOIDObjectClassMapping = nil then
    Exit; //-->

  result := lOIDObjectClassMapping.OIDGeneratorClass;
end;

function TOIDManager.GetOIDClass(
  const pObjectClass: TtiObjectClass): TtiOIDClass;
var
  lObjectClassMapping,
  lDefaultClassMapping: TOIDClassMapping;
begin
  result := nil;

  lObjectClassMapping := FindByObjectClass(pObjectClass);

  if (lObjectClassMapping = nil) and not FUseDefaultMapping then
    Exit; //-->

  if lObjectClassMapping <> nil then
    result := lObjectClassMapping.OIDClass
  else
  begin
    lDefaultClassMapping := FindByObjectClass(nil);

    if not Assigned(lDefaultClassMapping) then
      raise Exception.Create('No default OIDClassMapping defined!')
    else
      result := lDefaultClassMapping.OIDClass;
  end;
end;

function TOIDManager.GetOIDGenerator(
  const pObjectClass: TtiObjectClass): TtiOIDGenerator;
var
  lObjectClassMapping,
  lDefaultClassMapping: TOIDClassMapping;
begin
  result := nil;

  lObjectClassMapping := FindByObjectClass(pObjectClass);

  if (lObjectClassMapping = nil) and not FUseDefaultMapping then
    Exit; //-->

  if lObjectClassMapping <> nil then
    result := lObjectClassMapping.OIDGenerator
  else
  begin
    lDefaultClassMapping := FindByObjectClass(nil);

    if not Assigned(lDefaultClassMapping) then
      raise Exception.Create('No default OIDClassMapping defined!')
    else
      result := lDefaultClassMapping.OIDGenerator;
  end;
end;

procedure TOIDManager.RegisterMapping(
  const pObjectClass: TtiObjectClass; const pOIDGeneratorClass: TtiOIDGeneratorClass; const pOIDClass: TtiOIDClass);
begin
  if not Assigned(pObjectClass) then
    raise Exception.Create('You need to specify a object class!');

  _RegisterMapping(pObjectClass, pOIDGeneratorClass, pOIDClass);
end;

procedure TOIDManager.SetDefaultOIDClass(const Value: TtiOIDClass);
var
  lDefaultOIDObjectClassMapping: TOIDClassMapping;
begin
  lDefaultOIDObjectClassMapping := FindByObjectClass(nil);

  if Assigned(lDefaultOIDObjectClassMapping) then
  begin
    lDefaultOIDObjectClassMapping := TOIDClassMapping(
      FList.Extract(lDefaultOIDObjectClassMapping));

    _RegisterMapping(nil,
      lDefaultOIDObjectClassMapping.OIDGeneratorClass,
      Value);

    lDefaultOIDObjectClassMapping.Free;
  end;
end;

procedure TOIDManager.SetDefaultOIDGeneratorClass(
  const Value: TtiOIDGeneratorClass);
var
  lDefaultOIDObjectClassMapping: TOIDClassMapping;
begin
  lDefaultOIDObjectClassMapping := FindByObjectClass(nil);

  if Assigned(lDefaultOIDObjectClassMapping) then
  begin
    lDefaultOIDObjectClassMapping := TOIDClassMapping(
      FList.Extract(lDefaultOIDObjectClassMapping));

    _RegisterMapping(nil,
      Value,
      lDefaultOIDObjectClassMapping.OIDClass);

    lDefaultOIDObjectClassMapping.Free;
  end;
end;

procedure TOIDManager._RegisterMapping(
  const pObjectClass: TtiObjectClass; const pOIDGeneratorClass: TtiOIDGeneratorClass; const pOIDClass: TtiOIDClass);
var
  lObjectClassMapping : TOIDClassMapping;
begin
  if FindByObjectClass(pObjectClass) <> nil then
    raise Exception.Create(
      'Attempt to register duplicated OID mapping for class: ' +
      pObjectClass.ClassName + Cr(2) +
      'An object class can only be mapped to one pair of OIDClass:OIDGenerator');

  lObjectClassMapping := TOIDClassMapping.Create;
  lObjectClassMapping.ObjectClass := pObjectClass;
  lObjectClassMapping.OIDClass := pOIDClass;
  lObjectClassMapping.OIDGeneratorClass := pOIDGeneratorClass;

  FList.Add(lObjectClassMapping);
end;

function TOIDManager.GetOIDClass(
  const AObjectClassName: string): TtiOIDClass;
var
  lObjectClassMapping,
  lDefaultClassMapping: TOIDClassMapping;
begin
  result := nil;

  lObjectClassMapping := FindByObjectClassName(AObjectClassName);

  if (lObjectClassMapping = nil) and not FUseDefaultMapping then
    Exit; //-->

  if lObjectClassMapping <> nil then
    result := lObjectClassMapping.OIDClass
  else
  begin
    lDefaultClassMapping := FindByObjectClass(nil);

    if not Assigned(lDefaultClassMapping) then
      raise Exception.Create('No default OIDClassMapping defined!')
    else
      result := lDefaultClassMapping.OIDClass;
  end;
end;

function TOIDManager.GetOIDGenerator(
  const AObjectClassName: string): TtiOIDGenerator;
var
  lObjectClassMapping,
  lDefaultClassMapping: TOIDClassMapping;
begin
  result := nil;

  lObjectClassMapping := FindByObjectClassName(AObjectClassName);

  if (lObjectClassMapping = nil) and not FUseDefaultMapping then
    Exit; //-->

  if lObjectClassMapping <> nil then
    result := lObjectClassMapping.OIDGenerator
  else
  begin
    lDefaultClassMapping := FindByObjectClass(nil);

    if not Assigned(lDefaultClassMapping) then
      raise Exception.Create('No default OIDClassMapping defined!')
    else
      result := lDefaultClassMapping.OIDGenerator;
  end;
end;

{ TOIDClassMapping }

destructor TOIDClassMapping.Destroy;
begin
  if Assigned(FOIDGenerator) then
    FOIDGenerator.Free;

  inherited;
end;

function TOIDClassMapping.GetOIDGenerator: TtiOIDGenerator;
begin
  result := nil;

  if FOIDGeneratorClass = nil then
    Exit; //-->

  if not Assigned(FOIDGenerator) then
    FOIDGenerator := FOIDGeneratorClass.Create;

  result := FOIDGenerator;
end;


initialization

finalization
  if Assigned(OIDManagerInst) then
    FreeAndNil(OIDManagerInst);

end.
