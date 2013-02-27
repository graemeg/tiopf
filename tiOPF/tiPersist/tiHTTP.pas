{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    Uses the Adapter pattern to wrapper a HTTP component.

  Classes:
    TtiHTTPAbs   - The vitrual abstract class
    TtiHTTPClass - A class reference so the correct concreate can be created
    TtiHTTPIndy  - An Indy HTTP TtiHTTPAbs wrapper


  ToDo:
    Move the TtiHTTPIndy class to its own unit.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiHTTP;

interface
uses
  Classes
  ,Contnrs
  ,tiObjAbs
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPConnection
  ,IdTCPClient
  ,IdHTTP
  ,cTIPersist
  ;

const
  cErrorDuplicateHTTPClassMapping    = 'Attempt to register duplicate TtiHTTP class mapping <%s>';
  cErrorUnRegisteredHTTPClassMapping = 'Unregistered TtiHTTP class mapping <%s>';

{
  cLogMessageCheckingURL            = 'Checking URL %s' ;
  cLogMessageCheckingWithIndy       = '  Checking with Indy HTTP communication library' ;
  cLogMessageCheckingWithIndyDirect = '    Checking direct connection' ;
  cLogMessageDetectingProxySettings = '    Detecting proxy settings' ;
  cLogMessageCheckingWithIndyProxy  = '    Checking via proxy server' ;
  cLogMessageCheckingWithWinINet    = '  Checking using WinINet.dll';
}

type

  // ---------------------------------------------------------------------------
  TtiHTTPAbs = class( TtiObjAbs )
  private
    FInput  : TStringStream ;
    FOutput : TStringStream ;
    FFormatExceptions: Boolean;
  protected
    function  GetProxyPort: integer; virtual ; abstract ;
    function  GetProxyServer: string; virtual ; abstract ;
    procedure SetProxyPort(const Value: integer); virtual ; abstract ;
    procedure SetProxyServer(const Value: string); virtual ; abstract ;
    function  GetResponseCode: Integer; virtual ; abstract ;
    function  GetResponseText: string; virtual ; abstract ;
  public
    Constructor Create ; virtual ;
    Destructor  Destroy ; override ;
    procedure   Post( const psURL : string ) ; virtual ; abstract ;
    procedure   Get( const psURL : string ) ; virtual ; abstract ;
    property    Input : TStringStream read FInput ;
    property    Output : TStringStream read FOutput ;
    property    ProxyServer : string read GetProxyServer write SetProxyServer ;
    property    ProxyPort   : integer read GetProxyPort  write SetProxyPort ;
    property    ResponseCode: Integer read GetResponseCode;
    property    ResponseText: string  read GetResponseText;
    property    FormatExceptions: Boolean read FFormatExceptions Write FFormatExceptions;
    function    CorrectURL(const pURL: string): string ;
    procedure   Clear ;
    class function MappingName: string ; virtual ; abstract ;
    function    GetMappingName: string ;
  end ;

  TtiHTTPClass = class of TtiHTTPAbs ;

  TtiHTTPFactory = class( TtiObjAbs )
  private
    FList: TClassList ;
    function FindMapping(const pMappingName: string): Integer;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   RegisterMapping(const pMappingName: string; const pMappingClass: TtiHTTPClass);
    function    CreateInstance(const pMappingName: string): TtiHTTPAbs ;
    function    IsInstanceOfType(const pInstance: TtiHTTPAbs; const pMappingName: string) : Boolean ;
  end ;

function  gTIHTTPFactory: TtiHTTPFactory ;
procedure SetDefaultHTTPClass(const pMappingName: string);

const
  cLocalHost = 'http://localhost' ;

var
  gTIHTTPClass : TtiHTTPClass ;

implementation
uses
  Windows
  ,tiLog
  ,SysUtils
  ,tiUtils
  ,tiXML
  ;

var
  uTIHTTPFactory : TtiHTTPFactory;

function gTIHTTPFactory: TtiHTTPFactory;
begin
  if uTIHTTPFactory = nil then
    uTIHTTPFactory := TtiHTTPFactory.Create;
  Result := uTIHTTPFactory;
end;

procedure SetDefaultHTTPClass(const pMappingName: string);
var
  lHTTP : TtiHTTPAbs ;
begin
  lHTTP := gTIHTTPFactory.CreateInstance(pMappingName);
  try
    gTIHTTPClass := TtiHTTPClass(lHTTP.ClassType) ;
  finally
    lHTTP.Free;
  end;
end ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiHTTPAbs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiHTTPAbs.Create;
begin
  Inherited ;
  FInput            := TStringStream.Create( '' ) ;
  FOutput           := TStringStream.Create( '' ) ;
  FFormatExceptions := True ;
end;

// -----------------------------------------------------------------------------
destructor TtiHTTPAbs.Destroy;
begin
  FInput.Free ;
  FOutput.Free ;
  inherited;
end;

procedure TtiHTTPAbs.Clear;
begin
  FInput.Size := 0 ;
  FOutput.Size := 0 ;
end;

{ TtiHTTPFactory }

constructor TtiHTTPFactory.Create;
begin
  inherited;
  FList:= TClassList.Create ;
end;

function TtiHTTPFactory.CreateInstance(const pMappingName: string): TtiHTTPAbs;
var
  lIndex: Integer ;
begin
  lIndex := FindMapping(pMappingName) ;
  if lIndex = -1 then
    raise Exception.CreateFmt(cErrorUnRegisteredHTTPClassMapping, [pMappingName]);
  Result := TtiHTTPClass(FList.Items[lIndex]).Create as TtiHTTPAbs ;
end;

destructor TtiHTTPFactory.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiHTTPFactory.FindMapping(const pMappingName: string): integer;
var
  i : Integer ;
begin
  Result := -1 ;
  for i := 0 to FList.Count - 1 do
    if SameText(TtiHTTPClass(FList.Items[i]).MappingName, pMappingName) then
    begin
      Result := i;
      Exit ; //==>
    end;
end;

function TtiHTTPFactory.IsInstanceOfType(const pInstance: TtiHTTPAbs; const pMappingName: string): Boolean;
begin
  Assert(pInstance<> nil, 'pInstance not assigned');
  Result := SameText(pInstance.MappingName, pMappingName);
end;

procedure TtiHTTPFactory.RegisterMapping(const pMappingName: string;const pMappingClass: TtiHTTPClass);
begin
  if FindMapping(pMappingName) <> -1 then
    raise Exception.CreateFmt(cErrorDuplicateHTTPClassMapping, [pMappingName]);
  Assert(SameText(pMappingClass.MappingName, pMappingName), 'MappingName <> pMappingClass.MappingName');
  FList.Add(pMappingClass);
end;

function TtiHTTPAbs.GetMappingName: string;
begin
  Result := TtiHTTPAbs( ClassType ).MappingName ;
end;


function TtiHTTPAbs.CorrectURL(const pURL: string): string;
var
  lPosSlash: Integer ;
  lPosParams: Integer ;
  lLHS: string;
  lRHS: string;
begin
  lPosSlash := Pos('\', pURL);
  if lPosSlash = 0 then
  begin
    Result := pURL;
    Exit ; //==>
  end;

  lPosParams := Pos('?', pURL);
  if lPosParams = 0 then
    Result := tiStrTran(pURL, '\', '/')
  else begin
    lLHS := Copy(pURL, 1, lPosParams-1);
    lRHS := Copy(pURL, lPosParams, Length(pURL));
    Result := tiStrTran(lLHS, '\', '/') + lRHS ;
  end ;
end;

initialization
finalization
  FreeAndNil(uTIHTTPFactory);

end.

