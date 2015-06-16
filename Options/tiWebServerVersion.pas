unit tiWebServerVersion;

// To add an application-specific version create a descendant of
// TtiAppServerVersionAbs, return the app version in GetCurrentFileSyncVersion
// and register your app server version class by calling
// gAppServerVersionFactory.RegisterClass(TMyAppServerVersionClass), from the
// initialization section of your classes unit for example.

// NOTE: If you have existing users at the time that you implement an
// application specific version then you should use an initial version
// of 2.0 (or greater if you have made changes that break compatibility), as
// version 2.0 is the point at which application specific versions were first
// supported and using an earlier version number may cause existing (old)
// applications to think that they are up-to-date.

{$I tiDefines.inc}

interface
uses
  tiBaseObject
  ,tiXML
  ;

const
  cWebServerStatus_unknown = 'unknown';
  cWebServerStatus_passed  = 'passed';
  cWebServerStatus_failed  = 'failed';

  // NOTE: The following old file sync version is only used if an application
  // specific version is not defined, that is if an app server version class is
  // not registered, and is maintained here for backwards compatibility.
  // Do not increment this in future.
  ctiOPFOldFileSyncVersion = '2.0';

type

{
<?xml version="1.0"?>
<appserver>
<connection status="passed"/>
<xml version="2.0"/>
<filesync version="2.0"/>
</appserver>
}

  TtiAppServerVersionAbs = class(TtiBaseObject)
  private
    FConnectionStatus: string;
    FFileSyncVersion: string;
    FXMLVersion: string;
    function    GetAsString: string;
    procedure   SetAsString(const AValue: string);
  protected
    function    GetCurrentFileSyncVersion: string; virtual; abstract;
  public
    // NOTE: DO CREATE A DESCENDANT DIRECTLY.
    // Use gAppServerVersionFactory.CreateInstance instead.
    constructor Create;
    property    ConnectionStatus: string read FConnectionStatus write FConnectionStatus;
    property    XMLVersion: string read FXMLVersion write FXMLVersion;
    // Application specific version checking.
    property    FileSyncVersion: string read FFileSyncVersion write FFileSyncVersion;
    property    AsString: string read GetAsString write SetAsString;
    procedure   LoadDefaultValues;
    procedure   SetConnectionStatus(AValue: boolean);
    function    IsValid: boolean;
    function    IsVersionValid: boolean;
    function    IsDBConnectionValid: boolean;

    class function IsXMLValid(const AXML: string): boolean;
    class function ExpectedAsString: String;
  end;

  // A class reference for the TtiAppServerVersionAbs descendants
  TtiAppServerVersionClass = class of TtiAppServerVersionAbs;

  // Factory pattern - Create a descendant of TtiAppServerVersionAbs at runtime.
  // Do not create an instance directly. Instead call gAppServerVersionFactory.
  TtiAppServerVersionFactory = class(TObject)
  private
    FAppServerVersionClass: TtiAppServerVersionClass;
  public
    procedure RegisterClass(AAppServerVersionClass: TtiAppServerVersionClass);
    procedure UnRegisterClass;
    function CreateInstance: TtiAppServerVersionAbs;
  end;

// The AppServerVersionFactory is a singleton
function gAppServerVersionFactory: TtiAppServerVersionFactory;

implementation

uses
  tiExcept
  ;

// Single instance of TtiAppServerVersionFactory
var
  uAppServerVersionFactory: TtiAppServerVersionFactory;

const
  cXMLNodeAppServer  = 'appserver';
  cXMLNodeConnection = 'connection';
  cXMLNodeFileSync   = 'filesync';
  cXMLAttrStatus     = 'status';
  cXMLNodeXML        = 'xml';
  cXMLAttrVersion    = 'version';

type

  // This is used if an app server version class is not registered. This
  // should not be used directly by user code. To create an instance of an
  // app server version class call gAppServerVersionFactory.CreateInstance.
  TtiAppServerVersionDefault = class(TtiAppServerVersionAbs)
  protected
    function GetCurrentFileSyncVersion: string; override;
  end;

function gAppServerVersionFactory: TtiAppServerVersionFactory;
begin
  if not Assigned(uAppServerVersionFactory) then
    uAppServerVersionFactory := TtiAppServerVersionFactory.Create;
  result := uAppServerVersionFactory;
end;

{ TtiAppServerVersionAbs }

constructor TtiAppServerVersionAbs.Create;
begin
  inherited;
  FConnectionStatus:=cWebServerStatus_unknown;
end;

class function TtiAppServerVersionAbs.ExpectedAsString: String;
var
  L: TtiAppServerVersionAbs;
begin
  L:= gAppServerVersionFactory.CreateInstance;
  try
    L.LoadDefaultValues;
    result:= L.AsString;
  finally
    L.Free;
  end;
end;

function TtiAppServerVersionAbs.GetAsString: string;
begin
  result:=
      cDefaultXMLDocHeader
    + tiXMLTag(cXMLNodeAppServer)
    + tiXMLNodeAttrData(cXMLNodeConnection, cXMLAttrStatus,  FConnectionStatus)
    + tiXMLNodeAttrData(cXMLNodeXML,        cXMLAttrVersion, FXMLVersion)
    + tiXMLNodeAttrData(cXMLNodeFileSync,   cXMLAttrVersion, FFileSyncVersion)
    + tiXMLTagEnd(cXMLNodeAppServer)
end;

class function TtiAppServerVersionAbs.IsXMLValid(const AXML: string): boolean;
var
  L: TtiAppServerVersionAbs;
begin
  L:= gAppServerVersionFactory.CreateInstance;
  try
    L.AsString:= AXML;
    Result:= L.IsValid;
  finally
    L.Free;
  end;
end;

function TtiAppServerVersionAbs.IsDBConnectionValid: boolean;
begin
  result:=
    (ConnectionStatus = cWebServerStatus_passed);
end;

function TtiAppServerVersionAbs.IsValid: boolean;
begin
  result:=
    IsVersionValid and IsDBConnectionValid;
end;

function TtiAppServerVersionAbs.IsVersionValid: boolean;
begin
  result:=
    (FileSyncVersion = GetCurrentFileSyncVersion) and
    (XMLVersion = cTIOPFXMLVersion);
end;

procedure TtiAppServerVersionAbs.LoadDefaultValues;
begin
  FConnectionStatus:= cWebServerStatus_unknown;
  FFileSyncVersion:= GetCurrentFileSyncVersion;
  FXMLVersion:= cTIOPFXMLVersion;
end;

procedure TtiAppServerVersionAbs.SetAsString(const AValue: string);
begin
  FConnectionStatus:= tiParseForSingleNodeAttribute(AValue, cXMLNodeConnection, cXMLAttrStatus);
  FFileSyncVersion:= tiParseForSingleNodeAttribute(AValue,  cXMLNodeFileSync,   cXMLAttrVersion);
  FXMLVersion:= tiParseForSingleNodeAttribute(AValue,       cXMLNodeXML,        cXMLAttrVersion);
end;

procedure TtiAppServerVersionAbs.SetConnectionStatus(AValue: boolean);
begin
  if AValue then
    FConnectionStatus:= cWebServerStatus_passed
  else
    FConnectionStatus:= cWebServerStatus_failed;
end;

{ TtiAppServerVersionDefault }

function TtiAppServerVersionDefault.GetCurrentFileSyncVersion: string;
begin
  result := ctiOPFOldFileSyncVersion;
end;

{ TtiAppServerVersionFactory }

procedure TtiAppServerVersionFactory.RegisterClass(
  AAppServerVersionClass: TtiAppServerVersionClass);
begin
  if Assigned(FAppServerVersionClass) then
    raise EtiOPFProgrammerException.Create(
        'AppServerVersionClass is already registered');
  FAppServerVersionClass := AAppServerVersionClass;
end;

procedure TtiAppServerVersionFactory.UnRegisterClass;
begin
  FAppServerVersionClass := nil;
end;

function TtiAppServerVersionFactory.CreateInstance: TtiAppServerVersionAbs;
begin
  if not Assigned(FAppServerVersionClass) then
    result := TtiAppServerVersionDefault.Create
  else
    result := FAppServerVersionClass.Create;
end;

initialization

finalization
  // Free the AppServerVersionFactory
  uAppServerVersionFactory.Free;

end.
