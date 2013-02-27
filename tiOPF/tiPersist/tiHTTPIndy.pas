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

unit tiHTTPIndy;

interface
uses
  Classes
  ,tiObjAbs
  ,tiHTTP
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPConnection
  ,IdTCPClient
  ,IdHTTP
  ;

type

  // ---------------------------------------------------------------------------
  TtiHTTPIndy = class( TtiHTTPAbs )
  private
    FHTTP : TidHTTP ;
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
    procedure   Post( const psURL : string ) ; override ;
    procedure   Get( const psURL : string ) ; override ;
    class function MappingName: string ; override ;
  end ;

implementation
uses
  SysUtils
  ,tiUtils
  ,cTIPersist
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiHTTPIndy
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiHTTPIndy.Create;
begin
  inherited ;
  FHTTP := TidHTTP.Create( nil ) ;
  FHTTP.ProtocolVersion := pv1_0 ;
end;

// -----------------------------------------------------------------------------
destructor TtiHTTPIndy.Destroy;
begin
  FHTTP.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiHTTPIndy.Get(const psURL: string);
begin
  try
    Input.Position := 0 ;
    Output.Size := 0 ;
    FHTTP.Get( psURL + '?' + Input.DataString, Output ) ;
  except
    on e:exception do
    begin
      if FormatExceptions then
        tiFmtException( e,
                        ClassName,
                        'Get',
                        'URL: ' + psURL + Cr +
                        'Input: ' + Input.DataString )
      else
        raise;
    end ;
  end ;
end;

function TtiHTTPIndy.GetProxyPort: integer;
begin
  result := FHTTP.ProxyParams.ProxyPort ;
end;

function TtiHTTPIndy.GetProxyServer: string;
begin
  result := FHTTP.ProxyParams.ProxyServer ;
end;

function TtiHTTPIndy.GetResponseCode: Integer;
begin
  Result := FHTTP.ResponseCode;
end;

function TtiHTTPIndy.GetResponseText: string;
begin
  Result := FHTTP.ResponseText;
end;

class function TtiHTTPIndy.MappingName: string;
begin
  Result := cHTTPIndy;
end;

procedure TtiHTTPIndy.Post(const psURL: string);
begin
  try
    Output.Size := 0 ;
    Input.Position := 0 ;
    FHTTP.Post( psURL, Input, Output ) ;
  except
    on e:exception do
    begin
      if FormatExceptions then
        tiFmtException( e,
                        ClassName,
                        'Post',
                        'URL: ' + psURL + Cr +
                        'Input:  ' + Input.DataString + Cr +
                        'Message: ' + ResponseText + Cr +
                        'Response code: ' + IntToStr( ResponseCode ))
      else
        raise;
    end ;
  end ;
end;

procedure TtiHTTPIndy.SetProxyPort(const Value: integer);
begin
  FHTTP.ProxyParams.ProxyPort := Value ;
end;

procedure TtiHTTPIndy.SetProxyServer(const Value: string);
begin
  FHTTP.ProxyParams.ProxyServer := Value ;
end;

initialization
  gTIHTTPClass := TtiHTTPIndy ;
  gTIHTTPFactory.RegisterMapping(cHTTPIndy, TtiHTTPIndy);

end.
