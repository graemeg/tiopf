// Delphi 8/2005/2006/2007 for .NET Implementation
// January 2010
//
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.1 (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
// "http://www.mozilla.org/MPL/"
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The Original Code is "DK.Adom_4_3.AddOns.Network.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2010 Dieter Köhler. All Rights Reserved.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU General Public License Version 2 or later (the "GPL"), in which case the
// provisions of the GPL are applicable instead of those above. If you wish to
// allow use of your version of this file only under the terms of the GPL, and
// not to allow others to use your version of this file under the terms of the
// MPL, indicate your decision by deleting the provisions above and replace them
// with the notice and other provisions required by the GPL. If you do not delete
// the provisions above, a recipient may use your version of this file under the
// terms of any one of the MPL or the GPL.

unit DK.Adom_4_3.AddOns.Network;

interface

uses
  IdHTTP, IdHTTPHeaderInfo, IdCookieManager,
    // IdHTTP, IdHTTPHeaderInfo, and IdCookieManager belong to the Internet
    // Direct (Indy) 10.0.52 package available at "http://www.nevrona.com/indy/".
  DK.Adom_4_3.AdomCore_4_3, DK.Utilities.dkUriUtils, DK.Utilities.dkWideStringUtils,
  Classes, SysUtils;

type

  // TIndyResourceResolver
  //
  // TIndyResourceResolver allows to resolve a network resource via an
  // HTTP connection into a memory stream.
  //
  // This class internally uses an Indy HTTP client (TIdHttp)

  TIndyResourceResolver = class(TStandardResourceResolver)
  private
    FIdHttp: TIdHttp;
    FPublicIdCatalog: TUtilsNameValueList;
    function GetAllowCookies: Boolean;
    function GetCookieManager: TIdCookieManager;
    function GetHandleRedirects: Boolean;
    function GetHTTPOptions: TIdHTTPOptions;
    function GetMaxAuthRetries: Integer;
    function GetProtocolVersion: TIdHTTPProtocolVersion;
    function GetProxyParams: TIdProxyConnectionInfo;
    function GetRedirectMaximum: Integer;
    function GetRequest: TIdHTTPRequest;
    procedure SetAllowCookies(const Value: Boolean);
    procedure SetCookieManager(const Value: TIdCookieManager);
    procedure SetHandleRedirects(const Value: Boolean);
    procedure SetHTTPOptions(const Value: TIdHTTPOptions);
    procedure SetMaxAuthRetries(const Value: Integer);
    procedure SetProtocolVersion(const Value: TIdHTTPProtocolVersion);
    procedure SetRedirectMaximum(const Value: Integer);

    function GetOnAuthorization: TIdOnAuthorization;
    function GetOnProxyAuthorization: TIdOnAuthorization;
    function GetOnRedirect: TIdHTTPOnRedirectEvent;
    function GetOnSelectAuthorization: TIdOnSelectAuthorization;
    function GetOnSelectProxyAuthorization: TIdOnSelectAuthorization;
    procedure SetOnAuthorization(const Value: TIdOnAuthorization);
    procedure SetOnProxyAuthorization(const Value: TIdOnAuthorization);
    procedure SetOnRedirect(const Value: TIdHTTPOnRedirectEvent);
    procedure SetOnSelectAuthorization(const Value: TIdOnSelectAuthorization);
    procedure SetOnSelectProxyAuthorization(const Value: TIdOnSelectAuthorization);
  protected
    function AcquireStreamFromUri(const URI: WideString): TStream; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PublicIdCatalog: TUtilsNameValueList read FPublicIdCatalog;
    destructor Destroy; override;
    function ResolveResource(const ABaseURI: WideString;
                               var PublicId,
                                   SystemId: WideString): TStream; override;
  published
    { These are wrappers for the properties and events of the internal HTTP
      client.  For details see the Indy 10 documentation. }
    property AllowCookies: Boolean read GetAllowCookies write SetAllowCookies;
    property CookieManager: TIdCookieManager read GetCookieManager write SetCookieManager;
    property HandleRedirects: Boolean read GetHandleRedirects write SetHandleRedirects default Id_TIdHTTP_HandleRedirects;
    property HTTPOptions: TIdHTTPOptions read GetHTTPOptions write SetHTTPOptions;
    property MaxAuthRetries: Integer read GetMaxAuthRetries write SetMaxAuthRetries default Id_TIdHTTP_MaxAuthRetries;
    property ProtocolVersion: TIdHTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion default Id_TIdHTTP_ProtocolVersion;
    property ProxyParams: TIdProxyConnectionInfo read GetProxyParams;
    property RedirectMaximum: Integer read GetRedirectMaximum write SetRedirectMaximum default Id_TIdHTTP_RedirectMax;
    property Request: TIdHTTPRequest read GetRequest;

    property OnAuthorization: TIdOnAuthorization read GetOnAuthorization write SetOnAuthorization;
    property OnProxyAuthorization: TIdOnAuthorization read GetOnProxyAuthorization write SetOnProxyAuthorization;
    property OnRedirect: TIdHTTPOnRedirectEvent read GetOnRedirect write SetOnRedirect;
    property OnSelectAuthorization: TIdOnSelectAuthorization read GetOnSelectAuthorization write SetOnSelectAuthorization;
    property OnSelectProxyAuthorization: TIdOnSelectAuthorization read GetOnSelectProxyAuthorization write SetOnSelectProxyAuthorization;
  end;

  THttpThreadStatus = ( hsDownloading, hsCompleted, hsError );

  THttpThread = class(TThread)
  private
    FData: TBytes;
    FResourceResolver: TIndyResourceResolver;
    FStatus: THttpThreadStatus;
    FURL: WideString;
    function GetAllowCookies: Boolean;
    function GetCookieManager: TIdCookieManager;
    function GetHandleRedirects: Boolean;
    function GetHTTPOptions: TIdHTTPOptions;
    function GetMaxAuthRetries: Integer;
    function GetProtocolVersion: TIdHTTPProtocolVersion;
    function GetProxyParams: TIdProxyConnectionInfo;
    function GetRedirectMaximum: Integer;
    function GetRequest: TIdHTTPRequest;
    procedure SetAllowCookies(const Value: Boolean);
    procedure SetCookieManager(const Value: TIdCookieManager);
    procedure SetHandleRedirects(const Value: Boolean);
    procedure SetHTTPOptions(const Value: TIdHTTPOptions);
    procedure SetMaxAuthRetries(const Value: Integer);
    procedure SetProtocolVersion(const Value: TIdHTTPProtocolVersion);
    procedure SetRedirectMaximum(const Value: Integer);

    function GetOnAuthorization: TIdOnAuthorization;
    function GetOnProxyAuthorization: TIdOnAuthorization;
    function GetOnRedirect: TIdHTTPOnRedirectEvent;
    function GetOnSelectAuthorization: TIdOnSelectAuthorization;
    function GetOnSelectProxyAuthorization: TIdOnSelectAuthorization;
    procedure SetOnAuthorization(const Value: TIdOnAuthorization);
    procedure SetOnProxyAuthorization(const Value: TIdOnAuthorization);
    procedure SetOnRedirect(const Value: TIdHTTPOnRedirectEvent);
    procedure SetOnSelectAuthorization(const Value: TIdOnSelectAuthorization);
    procedure SetOnSelectProxyAuthorization(const Value: TIdOnSelectAuthorization);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    // Contains the result of the request, if successful:
    property Data: TBytes read FData;
    // The URL for the request:
    property URL: WideString read FURL write FURL;
    // The status of the request:
    property Status: THttpThreadStatus read FStatus;

    { These are wrappers for the properties and events of the internal HTTP
      client.  For details see the Indy 9 documentation. }
    property AllowCookies: Boolean read GetAllowCookies write SetAllowCookies;
    property CookieManager: TIdCookieManager read GetCookieManager write SetCookieManager;
    property HandleRedirects: Boolean read GetHandleRedirects write SetHandleRedirects default Id_TIdHTTP_HandleRedirects;
    property HTTPOptions: TIdHTTPOptions read GetHTTPOptions write SetHTTPOptions;
    property MaxAuthRetries: Integer read GetMaxAuthRetries write SetMaxAuthRetries;
    property ProtocolVersion: TIdHTTPProtocolVersion read GetProtocolVersion write SetProtocolVersion default Id_TIdHTTP_ProtocolVersion;
    property ProxyParams: TIdProxyConnectionInfo read GetProxyParams;
    property RedirectMaximum: Integer read GetRedirectMaximum write SetRedirectMaximum default Id_TIdHTTP_RedirectMax;
    property Request: TIdHTTPRequest read GetRequest;

    property OnAuthorization: TIdOnAuthorization read GetOnAuthorization write SetOnAuthorization;
    property OnProxyAuthorization: TIdOnAuthorization read GetOnProxyAuthorization write SetOnProxyAuthorization;
    property OnRedirect: TIdHTTPOnRedirectEvent read GetOnRedirect write SetOnRedirect;
    property OnSelectAuthorization: TIdOnSelectAuthorization read GetOnSelectAuthorization write SetOnSelectAuthorization;
    property OnSelectProxyAuthorization: TIdOnSelectAuthorization read GetOnSelectProxyAuthorization write SetOnSelectProxyAuthorization;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ADOM 4.3 Add-Ons',[ TIndyResourceResolver ]);
end;

{ TIndyResourceResolver }

function TIndyResourceResolver.AcquireStreamFromUri(const URI: WideString): TStream;
var
  Path: TFilename;
  Authority, Query, Fragment: string; // Only dummies.
  UriAnalyzer: TUriStrAnalyzer;
begin
  Result := nil;
  UriAnalyzer := TUriStrAnalyzer.create;
  try
    with UriAnalyzer do begin
      SetUriReference(URI);
      if HasUriScheme then begin

        if UriScheme = 'file' then begin

          UriStrToFilename(URI, Path, Authority, Query, Fragment);
          if not FileExists(Path) then
            raise EFOpenError.CreateFmt('File "%s" not found.', [ExpandFileName(Path)]);
          Result := TFileStream.Create(Path, fmOpenRead);

        end else if (UriScheme = 'http') then begin  // xxx Add support for 'https'!
          Result := TMemoryStream.Create;
          try
            SetUriFragment('', False); // Ignore fragment identifier (= the part behind an optional # in the URL).
            FIdHttp.Get(UriReference, Result);
            Result.Position := 0;  // Reset loaded stream to its start position.
          except
            TMemoryStream(Result).Free;
            raise EFOpenError.CreateFmt('Cannot retrieve "%s".', [UriReference]);
          end;
        end else raise EFOpenError.CreateFmt('URI scheme "%s" not supported.', [UriScheme]);

      end else raise EFOpenError.CreateFmt('URI "%s" contains no scheme.', [URI]);
    end;
  finally
    UriAnalyzer.Free;
  end;
end;

constructor TIndyResourceResolver.Create(AOwner: TComponent);
begin
  inherited;
  FIdHttp := TIdHttp.Create(Self);
  FPublicIdCatalog := TUtilsNameValueList.Create;
end;

destructor TIndyResourceResolver.Destroy;
begin
  FPublicIdCatalog.Free;
  inherited;
end;

function TIndyResourceResolver.GetAllowCookies: Boolean;
begin
  Result := FIdHttp.AllowCookies;
end;

function TIndyResourceResolver.GetCookieManager: TIdCookieManager;
begin
  Result := FIdHttp.CookieManager;
end;

function TIndyResourceResolver.GetHandleRedirects: Boolean;
begin
  Result := FIdHttp.HandleRedirects;
end;

function TIndyResourceResolver.GetHTTPOptions: TIdHTTPOptions;
begin
  Result := FIdHttp.HTTPOptions;
end;

function TIndyResourceResolver.GetMaxAuthRetries: Integer;
begin
  Result := FIdHttp.MaxAuthRetries;
end;

function TIndyResourceResolver.GetOnAuthorization: TIdOnAuthorization;
begin
  Result := FIdHttp.OnAuthorization;
end;

function TIndyResourceResolver.GetOnProxyAuthorization: TIdOnAuthorization;
begin
  Result := FIdHttp.OnProxyAuthorization;
end;

function TIndyResourceResolver.GetOnRedirect: TIdHTTPOnRedirectEvent;
begin
  Result := FIdHttp.OnRedirect;
end;

function TIndyResourceResolver.GetOnSelectAuthorization: TIdOnSelectAuthorization;
begin
  Result := FIdHttp.OnSelectAuthorization;
end;

function TIndyResourceResolver.GetOnSelectProxyAuthorization: TIdOnSelectAuthorization;
begin
  Result := FIdHttp.OnSelectProxyAuthorization;
end;

function TIndyResourceResolver.GetProtocolVersion: TIdHTTPProtocolVersion;
begin
  Result := FIdHttp.ProtocolVersion;
end;

function TIndyResourceResolver.GetProxyParams: TIdProxyConnectionInfo;
begin
  Result := FIdHttp.ProxyParams;
end;

function TIndyResourceResolver.GetRedirectMaximum: Integer;
begin
  Result := FIdHttp.RedirectMaximum;
end;

function TIndyResourceResolver.GetRequest: TIdHTTPRequest;
begin
  Result := FIdHttp.Request;
end;

function TIndyResourceResolver.ResolveResource(const ABaseURI: WideString;
  var PublicId, SystemId: WideString): TStream;
var
  Index: Integer;
begin
  // Redirect, if public ID is in the catalog:
  with PublicIdCatalog do
    if FindOfName(PublicId, Index) then
      SystemId := Values[Index];
  Result := inherited ResolveResource(ABaseURI, PublicId, SystemId);
end;

procedure TIndyResourceResolver.SetALlowCookies(const Value: Boolean);
begin
  FIdHttp.AllowCookies := Value;
end;

procedure TIndyResourceResolver.SetCookieManager(
  const Value: TIdCookieManager);
begin
  FIdHttp.CookieManager := Value;
end;

procedure TIndyResourceResolver.SetHandleRedirects(const Value: Boolean);
begin
  FIdHttp.HandleRedirects := Value;
end;

procedure TIndyResourceResolver.SetHTTPOptions(
  const Value: TIdHTTPOptions);
begin
  FIdHttp.HTTPOptions := Value;
end;

procedure TIndyResourceResolver.SetMaxAuthRetries(const Value: Integer);
begin
  FIdHttp.MaxAuthRetries := Value;
end;

procedure TIndyResourceResolver.SetOnAuthorization(
  const Value: TIdOnAuthorization);
begin
  FIdHttp.OnAuthorization := Value;
end;

procedure TIndyResourceResolver.SetOnProxyAuthorization(
  const Value: TIdOnAuthorization);
begin
  FIdHttp.OnProxyAuthorization := Value;
end;

procedure TIndyResourceResolver.SetOnRedirect(
  const Value: TIdHTTPOnRedirectEvent);
begin
  FIdHttp.OnRedirect := Value;
end;

procedure TIndyResourceResolver.SetOnSelectAuthorization(
  const Value: TIdOnSelectAuthorization);
begin
  FIdHttp.OnSelectAuthorization := Value;
end;

procedure TIndyResourceResolver.SetOnSelectProxyAuthorization(
  const Value: TIdOnSelectAuthorization);
begin
  FIdHttp.OnSelectProxyAuthorization := Value;
end;

procedure TIndyResourceResolver.SetProtocolVersion(
  const Value: TIdHTTPProtocolVersion);
begin
  FIdHttp.ProtocolVersion := Value;
end;

procedure TIndyResourceResolver.SetRedirectMaximum(const Value: Integer);
begin
  FIdHttp.RedirectMaximum := Value;
end;

{ THttpThread }

constructor THttpThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FResourceResolver := TIndyResourceResolver.Create(nil);
  SetLength(FData, 0);
  FStatus := hsCompleted;
end;

destructor THttpThread.Destroy;
begin
  FResourceResolver.Free;
  inherited;
end;

procedure THttpThread.Execute;

  function StreamToBytes(Stream: TStream): TBytes;
  var
    Size: Longint;
  begin
    Size := Stream.Size - Stream.Position;
    Stream.Read(Result, Size);
  end;

var
  PubId, SysId: WideString;
  InputStream: TStream;
begin
  FStatus := hsDownloading;
  PubId := '';
  SysId := URL;
  try
    InputStream := FResourceResolver.ResolveResource('', PubId, SysId);
    try
      FData := StreamToBytes(InputStream);
    finally
      InputStream.Free;
    end;
    FStatus := hsCompleted;
  except
    FStatus := hsError;
  end;
end;

function THttpThread.GetAllowCookies: Boolean;
begin
  Result := FResourceResolver.AllowCookies;
end;

function THttpThread.GetCookieManager: TIdCookieManager;
begin
  Result := FResourceResolver.CookieManager;
end;

function THttpThread.GetHandleRedirects: Boolean;
begin
  Result := FResourceResolver.HandleRedirects;
end;

function THttpThread.GetHTTPOptions: TIdHTTPOptions;
begin
  Result := FResourceResolver.HTTPOptions;
end;

function THttpThread.GetMaxAuthRetries: Integer;
begin
  Result := FResourceResolver.MaxAuthRetries;
end;

function THttpThread.GetOnAuthorization: TIdOnAuthorization;
begin
  Result := FResourceResolver.OnAuthorization;
end;

function THttpThread.GetOnProxyAuthorization: TIdOnAuthorization;
begin
  Result := FResourceResolver.OnProxyAuthorization;
end;

function THttpThread.GetOnRedirect: TIdHTTPOnRedirectEvent;
begin
  Result := FResourceResolver.OnRedirect;
end;

function THttpThread.GetOnSelectAuthorization: TIdOnSelectAuthorization;
begin
  Result := FResourceResolver.OnSelectAuthorization;
end;

function THttpThread.GetOnSelectProxyAuthorization: TIdOnSelectAuthorization;
begin
  Result := FResourceResolver.OnSelectProxyAuthorization;
end;

function THttpThread.GetProtocolVersion: TIdHTTPProtocolVersion;
begin
  Result := FResourceResolver.ProtocolVersion;
end;

function THttpThread.GetProxyParams: TIdProxyConnectionInfo;
begin
  Result := FResourceResolver.ProxyParams;
end;

function THttpThread.GetRedirectMaximum: Integer;
begin
  Result := FResourceResolver.RedirectMaximum;
end;

function THttpThread.GetRequest: TIdHTTPRequest;
begin
  Result := FResourceResolver.Request;
end;

procedure THttpThread.SetAllowCookies(const Value: Boolean);
begin
  FResourceResolver.AllowCookies := Value;
end;

procedure THttpThread.SetMaxAuthRetries(const Value: Integer);
begin
  FResourceResolver.MaxAuthRetries := Value;
end;

procedure THttpThread.SetCookieManager(const Value: TIdCookieManager);
begin
  FResourceResolver.CookieManager := Value;
end;

procedure THttpThread.SetHandleRedirects(const Value: Boolean);
begin
  FResourceResolver.HandleRedirects := Value;
end;

procedure THttpThread.SetHTTPOptions(const Value: TIdHTTPOptions);
begin
  FResourceResolver.HTTPOptions := Value;
end;

procedure THttpThread.SetOnAuthorization(const Value: TIdOnAuthorization);
begin
  FResourceResolver.OnAuthorization := Value;
end;

procedure THttpThread.SetOnProxyAuthorization(
  const Value: TIdOnAuthorization);
begin
  FResourceResolver.OnProxyAuthorization := Value;
end;

procedure THttpThread.SetOnRedirect(const Value: TIdHTTPOnRedirectEvent);
begin
  FResourceResolver.OnRedirect := Value;
end;

procedure THttpThread.SetOnSelectAuthorization(
  const Value: TIdOnSelectAuthorization);
begin
  FResourceResolver.OnSelectAuthorization := Value;
end;

procedure THttpThread.SetOnSelectProxyAuthorization(
  const Value: TIdOnSelectAuthorization);
begin
  FResourceResolver.OnSelectProxyAuthorization := Value;
end;

procedure THttpThread.SetProtocolVersion(
  const Value: TIdHTTPProtocolVersion);
begin
  FResourceResolver.ProtocolVersion := Value;
end;

procedure THttpThread.SetRedirectMaximum(const Value: Integer);
begin
  FResourceResolver.RedirectMaximum := Value;
end;

end.
