unit tiWebServerVersion;

interface
uses
  tiBaseObject
 ;

const
  cWebServerStatus_unknown = 'unknown';
  cWebServerStatus_passed  = 'passed';
  cWebServerStatus_failed  = 'failed';
  cXMLVersion = '2.0';
  cFileSyncVersion = '1.3';

type

{
<?xml version="1.0"?>
<appserver>
<connection status="passed"/>
<xml version="2.0"/>
<filesync version="2.0"/>
</appserver>
}

  TtiAppServerVersion = class(TtiBaseObject)
  private
    FConnectionStatus: string;
    FFileSyncVersion: string;
    FXMLVersion: string;
    function    GetAsString: string;
    procedure   SetAsString(const AValue: string);
  public
    constructor Create;
    property    ConnectionStatus: string read FConnectionStatus write FConnectionStatus;
    property    XMLVersion: string read FXMLVersion write FXMLVersion;
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

implementation
uses
  tiXML
 ;

const
  cXMLNodeAppServer  = 'appserver';
  cXMLNodeConnection = 'connection';
  cXMLNodeFileSync   = 'filesync';
  cXMLAttrStatus     = 'status';
  cXMLNodeXML        = 'xml';
  cXMLAttrVersion    = 'version';


{ TtiVersion }

{ TtiWebServerVersion }

constructor TtiAppServerVersion.Create;
begin
  inherited;
  FConnectionStatus:=cWebServerStatus_unknown;
end;

class function TtiAppServerVersion.ExpectedAsString: String;
var
  L: TtiAppServerVersion;
begin
  L:= TtiAppServerVersion.Create;
  try
    L.LoadDefaultValues;
    result:= L.AsString;
  finally
    L.Free;
  end;
end;

function TtiAppServerVersion.GetAsString: string;
begin
  result:=
      cDefaultXMLDocHeader
    + tiXMLTag(cXMLNodeAppServer)
    + tiXMLNodeAttrData(cXMLNodeConnection, cXMLAttrStatus,  FConnectionStatus)
    + tiXMLNodeAttrData(cXMLNodeXML,        cXMLAttrVersion, FXMLVersion)
    + tiXMLNodeAttrData(cXMLNodeFileSync,   cXMLAttrVersion, FFileSyncVersion)
    + tiXMLTagEnd(cXMLNodeAppServer)
end;

class function TtiAppServerVersion.IsXMLValid(const AXML: string): boolean;
var
  L: TtiAppServerVersion;
begin
  L:= TtiAppServerVersion.Create;
  try
    L.AsString:= AXML;
    Result:= L.IsValid;
  finally
    L.Free;
  end;
end;

function TtiAppServerVersion.IsDBConnectionValid: boolean;
begin
  result:=
    (ConnectionStatus = cWebServerStatus_passed);
end;

function TtiAppServerVersion.IsValid: boolean;
begin
  result:=
    IsVersionValid and IsDBConnectionValid;
end;

function TtiAppServerVersion.IsVersionValid: boolean;
begin
  result:=
    (FileSyncVersion = cFileSyncVersion) and
    (XMLVersion = cXMLVersion);
end;

procedure TtiAppServerVersion.LoadDefaultValues;
begin
  FConnectionStatus:= cWebServerStatus_unknown;
  FFileSyncVersion:= cFileSyncVersion;
  FXMLVersion:= cXMLVersion;
end;

procedure TtiAppServerVersion.SetAsString(const AValue: string);
begin
  FConnectionStatus:= tiParseForSingleNodeAttribute(AValue, cXMLNodeConnection, cXMLAttrStatus);
  FFileSyncVersion:= tiParseForSingleNodeAttribute(AValue,  cXMLNodeFileSync,   cXMLAttrVersion);
  FXMLVersion:= tiParseForSingleNodeAttribute(AValue,       cXMLNodeXML,        cXMLAttrVersion);
end;

procedure TtiAppServerVersion.SetConnectionStatus(AValue: boolean);
begin
  if AValue then
    FConnectionStatus:= cWebServerStatus_passed
  else
    FConnectionStatus:= cWebServerStatus_failed;
end;

end.
