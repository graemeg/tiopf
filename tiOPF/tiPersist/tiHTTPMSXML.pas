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

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiHTTPMSXml;

interface
uses
  Classes
  ,tiObjAbs
  ,tiHTTP
  ,MSXML_TLB
  ,tiDialogs
  ;

const
  cErrorHTTPServer = 'HTTP/1.1 %d Internal Server Error';

type

  // ---------------------------------------------------------------------------
  TtiHTTPMSXML = class( TtiHTTPAbs )
  private
    FHTTP : IXMLHttpRequest ;
  protected
    function    GetProxyPort: integer; override;
    function    GetProxyServer: string; override;
    procedure   SetProxyPort(const Value: integer); override;
    procedure   SetProxyServer(const Value: string); override;
    function    GetResponseCode: Integer ; override ;
    function    GetResponseText: string; override ;
  public
    Constructor Create ; override ;
    Destructor  Destroy ; override ;
    procedure   Post( const pURL : string ) ; override ;
    procedure   Get( const pURL : string ) ; override ;
    class function MappingName: string ; override ;
  end ;

implementation
uses
  SysUtils
  ,tiUtils
  ,cTIPersist
  ,tiWin32
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiHTTPMSXML
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiHTTPMSXML.Create;
begin
  inherited ;
  tiWin32CoInitialize;
  FHTTP := CoXMLHTTPRequest.Create;
end;

// -----------------------------------------------------------------------------
destructor TtiHTTPMSXML.Destroy;
begin
  tiWin32CoUnInitialize;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiHTTPMSXML.Get(const pURL: string);
var
  lURL: string ;
begin
  lURL := CorrectURL(pURL);
  try
    Input.Position := 0 ;
    FHTTP.open('GET', lURL, False, '', '' );
    FHTTP.Send(Input.DataString);
    if FHTTP.Get_Status <> 200 then
      raise Exception.CreateFmt(cErrorHTTPServer, [FHTTP.Get_Status]);
    Output.Size := 0 ;
    Output.WriteString(FHTTP.responseText);
  except
    on e:exception do
    begin
      if FormatExceptions then
        tiFmtException( e,
                        ClassName,
                        'Get',
                        'URL: ' + pURL + Cr +
                        'Input: ' + Input.DataString )
      else
        raise;
    end ;
  end ;
end;

function TtiHTTPMSXML.GetProxyPort: integer;
begin
  Assert(False, 'GetProxyPort not available in ' + ClassName);
  Result := 0 ;
end;

function TtiHTTPMSXML.GetProxyServer: string;
begin
  Assert(False, 'GetProxyServer not available in ' + ClassName);
  result := '' ;
end;

function TtiHTTPMSXML.GetResponseCode: Integer;
begin
  result := FHTTP.Get_Status;
end;

function TtiHTTPMSXML.GetResponseText: string;
var
  lResponseCode: Integer;
begin
  lResponseCode := ResponseCode;
  if lResponseCode = 200 then
    Result := 'HTTP/1.1 200 OK'
  else
    Result := Format(cErrorHTTPServer, [lResponseCode])
end;

class function TtiHTTPMSXML.MappingName: string;
begin
  Result := cHTTPMsXml ;
end;

procedure TtiHTTPMSXML.Post(const pURL: string);
var
  lURL: string ;
begin
  lURL := CorrectURL(pURL);
  try
    Input.Position := 0 ;
    FHTTP.open('POST', lURL, False, '', '' );
    FHTTP.Send(Input.DataString);
    if FHTTP.Get_Status <> 200 then
      raise Exception.CreateFmt(cErrorHTTPServer, [FHTTP.Get_Status]);
    Output.Size := 0 ;
    Output.WriteString(FHTTP.responseText);
  except
    on e:exception do
    begin
      if FormatExceptions then
        tiFmtException( e,
                        ClassName,
                        'Get',
                        'URL: ' + pURL + Cr +
                        'Input: ' + Input.DataString )
      else
        raise;
    end ;
  end ;
end;

procedure TtiHTTPMSXML.SetProxyPort(const Value: integer);
begin
  Assert(False, 'SetProxyPort not available in ' + ClassName);
end;

procedure TtiHTTPMSXML.SetProxyServer(const Value: string);
begin
  Assert(False, 'SetProxyServer not available in ' + ClassName);
end;

initialization
  gTIHTTPClass := TtiHTTPMSXML ;
  gTIHTTPFactory.RegisterMapping(cHTTPMsXml, TtiHTTPMSXML);
end.
