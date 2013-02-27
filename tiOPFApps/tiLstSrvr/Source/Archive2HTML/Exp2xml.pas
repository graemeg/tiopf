{$I tiDefines.inc}

unit Exp2xml;

interface

uses
  Classes, tiPtnVisPerObj, tiPtnVisSQL, IteradorAbs, ListaVariables;

type
//------------------------------------------------------------------------------
  TFiltroXMLAbs = class(TPerObjAbs, IIteradoInterface)
  private
    FStreamSalida: TStream;
    FParams: TListaVariables;
    FxsltParams: array of string;
    FPuedenPI: Boolean;
    FUltNodo: TStringList;
    FRowSetName: string;
    FRowName: string;

  protected
    procedure SetStreamSalida(const Value: TStream);

    function EsAmpLtGt( s: string; i: integer ): boolean;

    function CadenaAtributos( Atributos: array of string ): string;
    procedure EscribirDeclXML( sVersion: string = ''; sEncoding: string = ''; sStandalone: Integer = -1 );
    procedure EscribirRaiz( sNombre: string; Atributos: array of string ); overload;
    procedure EscribirRaiz( sNombre: string ); overload;
    procedure CerrarRaiz;
    procedure EscribirTag( sNombre: string; Atributos: array of string ); overload;
    procedure EscribirTag( sNombre: string ); overload;
    procedure EscribirPCDATA( sValor: string; bEsTag: Boolean = False );
    procedure CerrarTag( sNombre: string = '' );

    procedure PreparaParamsXSL;

    function ParamCount: Integer;
    function ParamName(  idx: Integer ): string;
    function ParamValue( idx: Integer ): string; overload;
    function ParamValue( sName: string ): string; overload;

    procedure InicioSecuencia( sNombreSecuencia: string );
    procedure InicioElemento( sNombreSecuencia: string ); overload;
    procedure InicioElemento( sNombreSecuencia: string; Atributos: array of string ); overload;
    procedure ValorElemento( sNombreValor, sValor: string );
    procedure Valor( sValor: string );
    procedure FinElemento( sNombreSecuencia: string );
    procedure FinSecuencia( sNombreSecuencia: string );

    procedure InicializarStream; virtual;
    property RowSetName: string read FRowSetName;
    property RowName: string read FRowName;
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  protected

    procedure IniciarXML; virtual;
    procedure GenerarXML( sRowSetName: string = '' ); virtual;
    procedure FinalizarXML; virtual;


  public
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;

  public
    constructor Create; override;
    destructor Destroy; override;

    property StreamSalida: TStream read FStreamSalida write SetStreamSalida;
    property Params: TListaVariables read FParams;

    procedure GenerarIteradores;
    procedure AplicarXSL( sArchivoXML, sArchivoXSL, sSalida: string );
  end;
//------------------------------------------------------------------------------
  TFiltroXMLList = class( TFiltroXMLAbs )
  private
    FLista: TList;
    FColumnas: TStringList;

  protected
    procedure GenerarXML( sRowSetName: string = '' ); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Lista:    TList       read FLista write FLista;
    property Columnas: TStringList read FColumnas;


    class procedure ExportarListaXML( pLst: TList;
                                      sArchivoXML: string;
                                      pslColsSelected : TStringList = nil;
                                      sRowSetName: string = '' );
    class procedure AplicarXSLT( pLst: TList;
                                 sArchivoXML,
                                 sArchivoXSL,
                                 sArchivoHTML: string;
                                 pslColsSelected : TStringList = nil;
                                 sRowSetName: string = '' );
  end;
//------------------------------------------------------------------------------
  TFiltroXMLSQL = class( TFiltroXMLAbs )
  private
    FSQL:    TStringList;

  protected
    procedure GenerarXML(sRowSetName: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property SQL: TStringList read FSQL;

    class procedure Exportar( sSQL: string; ssParams: TListaVariables; sArchivoSalida: string; sRowSetName: string = '' );
    class procedure AplicarXSLT( sSQL, sArchivoXML, sArchivoXSL, sSalida: string; ssParams: TListaVariables; sRowSetName: string = '' );
  end;
//------------------------------------------------------------------------------
  TVisPerXMLAbs = class( TVisOwnedQrySelect )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
    procedure   MapRowToObject          ; override ;
    procedure   Final                   ; override ;
  end ;
//------------------------------------------------------------------------------

const
  INFORME  = 'informe';
  ITERADOR = 'iterador';
  ITEM_IT  = 'item_iterador';
  ITEM     = 'item_';
  ROWSET   = 'rowset';
  ROW      = 'row';

implementation

uses
{$IFDEF WINDOWS}
  Windows, FileCtrl, Dialogs,
{$ENDIF}
  SysUtils, TypInfo, libxml2, libxslt, tiUtils;

function CharIsPrintable( const c: char ): boolean;
begin
  Result := ((Ord(c) >=  32) and (Ord(c) <= 126));
end;

{ TFiltroXMLAbs }

function TFiltroXMLAbs.CadenaAtributos(Atributos: array of string): string;
var
  i: Integer;
  sAtr, sVal: string;
  iPosEq: Integer;
begin
  Result := EmptyStr;
  for i := Low( Atributos ) to High( Atributos ) do begin
    iPosEq := Pos( '=', Atributos[i] );
    sAtr := Copy( Atributos[i],          1, iPosEq - 1);
    sVal := copy( Atributos[i], iPosEq + 1, Length( Atributos[i] ) );
    Result := Format( '%s %s="%s"', [Result, sAtr, sVal] );
  end;

  Result := ' ' + Trim( Result );
end;

procedure TFiltroXMLAbs.CerrarTag(sNombre: string);
var
  sBuf: string;
begin
  if( EmptyStr = sNombre ) then begin
    sNombre := FUltNodo.Strings[ FUltNodo.Count - 1 ];
  end;
  while( sNombre <> FUltNodo.Strings[ FUltNodo.Count - 1 ] ) do CerrarTag;
  FUltNodo.Delete( FUltNodo.Count - 1 );
  sBuf := Format( '</%s>', [sNombre] );
  EscribirPCDATA( sBuf, True );

//  sbuf := #13 + #10;
//  streamsalida.Write( PChar( sBuf )[0], 2 );
end;

procedure TFiltroXMLAbs.CerrarRaiz;
begin
  CerrarTag( FUltNodo.Strings[ 0 ] );
  FPuedenPI := True;
end;

constructor TFiltroXMLAbs.Create;
begin
  inherited;

  FUltNodo := TStringList.Create;
  FParams  := TListaVariables.Create;
end;

destructor TFiltroXMLAbs.Destroy;
begin
  while( FUltNodo.Count > 0 ) do CerrarTag;
  FUltNodo.Free;
  FParams.Free;

  StreamSalida := nil;

  inherited;
end;

procedure TFiltroXMLAbs.EscribirDeclXML(sVersion, sEncoding: string;
  sStandalone: Integer);
const
  Boolean2String: array [Boolean] of string = ('no', 'yes');
var
  sFmt, sBuf, sStand: string;
begin
  if( EmptyStr = sVersion    ) then sVersion    := '1.0';
  if( EmptyStr = sEncoding   ) then sEncoding   := 'iso-8859-1';

  sFmt := '<?xml version="%0:s"';

  if( EmptyStr <> sEncoding ) then begin
    sFmt := Format( '%s %s', [sFmt, 'encoding="%1:s"'] );
  end;
  if( -1 <> sStandalone ) then begin
    sFmt := Format( '%s %s', [sFmt, 'standalone="%2:s"'] );
    sStand := Boolean2String[ 0 <> sStandalone ];
  end;

  sFmt := sFmt + '?>';

  sBuf := Format( sFmt, [sVersion, sEncoding, sStand] );
  EscribirPCDATA( sBuf, True );
end;

procedure TFiltroXMLAbs.EscribirTag(sNombre: string; Atributos: array of string);
var
  sBuf: string;
begin
  sBuf := Format( '<%s%s>', [sNombre, CadenaAtributos( Atributos )] );
  EscribirPCDATA( sBuf, True );
  FUltNodo.Add( sNombre );
end;

procedure TFiltroXMLAbs.EscribirTag(sNombre: string);
var
  sBuf: string;
begin
  sBuf := Format( '<%s>', [sNombre] );
  EscribirPCDATA( sBuf, True );
  FUltNodo.Add( sNombre );
end;

procedure TFiltroXMLAbs.EscribirPCDATA(sValor: string; bEsTag: Boolean);
var
  pc: PChar;
  s: string;
  i, p, u: Integer;
begin
  p := 1 + Ord( bEsTag );
  u := Length( sValor ) - Ord( bEsTag );
  for i := p to u do begin
    if(('&' = sValor[ i ]) and (EsAmpLtGt( sValor, i ))) then begin
      s := s + sValor[ i ];
      Continue;
    end;
    if( '&' = sValor[ i ] ) then begin
      s := s + '&amp;';
      Continue;
    end;
    if( '<' = sValor[ i ] ) then begin
      s := s + '&lt;';
      Continue;
    end;
    if( '>' = sValor[ i ] ) then begin
      s := s + '&gt;';
      Continue;
    end;
    if( not CharIsPrintable( sValor[ i ] ) ) then begin
      if( 0 <> Ord( svalor[ i ] ) ) then
        s := s + '&#' + IntToStr( ord( sValor[ i ] ) ) + ';';
      Continue;
    end;
    s := s + sValor[ i ];
  end;
  if( True = bEsTag ) then begin
    s := Trim( sValor[1] + s + sValor[Length(sValor)] );
  end else begin
    s := Trim( s );
  end;

  pc := PChar( s );
  StreamSalida.Write( pc[0], Length( s ) );
end;

procedure TFiltroXMLAbs.EscribirRaiz(sNombre: string;
  Atributos: array of string);
begin
  EscribirTag( sNombre, Atributos );
  FPuedenPI := False;
end;

procedure TFiltroXMLAbs.EscribirRaiz(sNombre: string);
begin
  EscribirTag( sNombre );
  FPuedenPI := False;
end;

procedure TFiltroXMLAbs.SetStreamSalida(const Value: TStream);
begin
  if( Value <> FStreamSalida ) then begin
    if( True = Assigned( FStreamSalida ) ) then FStreamSalida.Free;
    FStreamSalida := Value;

//    if( True = Assigned( FStreamSalida ) ) then EscribirDeclXML;
  end;
end;

procedure TFiltroXMLAbs.InicializarStream;
begin
  EscribirDeclXML;
end;

procedure TFiltroXMLAbs.AplicarXSL(sArchivoXML, sArchivoXSL,
  sSalida: string);
var
  cur: xsltStylesheetPtr;
  doc: xmlDocPtr;
  res: xmlDocPtr;
  tc: xsltTransformContextPtr;
  sDir, sMode: string;
begin
  cur := nil; doc := nil; tc := nil; res := nil;
  try
    sMode := EmptyStr;

    PreparaParamsXSL;

    xmlSubstituteEntitiesDefault( 1 );
    cur := xsltParseStylesheetFile( PChar( sArchivoXSL ) );
    doc := xmlParseFile( PChar( sArchivoXML ) );

    tc := xsltNewTransformContext( cur, doc );
    if( sMode <> EmptyStr ) then begin
      tc.mode := PChar( sMode );
    end;

    res := xsltApplyStylesheetUser( cur, doc, PPChar( FxsltParams ), nil, nil, tc );

    sDir := ExtractFileDir( sSalida );
    if( EmptyStr <> sDir ) then
      ForceDirectories( sDir );

    if( FileExists( sSalida ) ) then
      SysUtils.DeleteFile( sSalida );

    xsltSaveResultToFilename( PChar( sSalida ), res, cur, 0 );
  finally
    xsltFreeTransformContext( tc );
    xsltFreeStylesheet( cur );
    xmlFreeDoc( res );
    xmlFreeDoc( doc );
    xsltCleanupGlobals();
    xmlCleanupParser();
  end;
end;

procedure TFiltroXMLAbs.PreparaParamsXSL;
var
  i, xsltPCount: Integer;
  sParam, sVal: string;
begin
  xsltPCount := 0;
  SetLength( FxsltParams, xsltPCount );
  for i := 0 to FParams.Count - 1 do begin
    sParam := FParams.Names[i];
    if( EmptyStr = sParam ) then Continue;

    sVal   := FParams.Values[ sParam ];

    SetLength(FXSLTParams, Length(FXSLTParams) + 2);

//    SetLength(FXSLTParams[xsltPCount], Length(sParam));
    FXSLTParams[xsltPCount] := sParam;
    Inc(xsltPCount);

//    SetLength(FXSLTParams[xsltPCount], Length(sVal) + 2);
    FXSLTParams[xsltPCount] := '"' + sVal + '"';
    Inc(xsltPCount);
  end;
  Inc( xsltPCount );
  SetLength( FxsltParams, xsltPCount );
  FxsltParams[ xsltPCount - 1 ] := EmptyStr;
end;

function TFiltroXMLAbs.ParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TFiltroXMLAbs.ParamName(idx: Integer): string;
begin
  Result := FParams.Names[ idx ];
end;

function TFiltroXMLAbs.ParamValue(idx: Integer): string;
begin
  Result := FParams.Values[ ParamName( idx ) ];
end;

function TFiltroXMLAbs.ParamValue(sName: string): string;
begin
  Result := FParams.Values[ sName ];
end;

procedure TFiltroXMLAbs.GenerarIteradores;
var
  I: Integer;
begin
  for I := 0 to ParamCount - 1 do begin
    TIterMgr.I.Ejecutar( ParamName( I ), ParamValue( I ), Self );
  end;
end;

procedure TFiltroXMLAbs.FinElemento(sNombreSecuencia: string);
begin
  CerrarTag( sNombreSecuencia );
end;

procedure TFiltroXMLAbs.FinSecuencia(sNombreSecuencia: string);
begin
  CerrarTag( sNombreSecuencia );
end;

procedure TFiltroXMLAbs.InicioElemento(sNombreSecuencia: string;
  Atributos: array of string);
begin
  EscribirTag( sNombreSecuencia, Atributos );
end;

procedure TFiltroXMLAbs.InicioElemento(sNombreSecuencia: string);
begin
  EscribirTag( sNombreSecuencia );
end;

procedure TFiltroXMLAbs.InicioSecuencia(sNombreSecuencia: string);
begin
  EscribirTag( sNombreSecuencia );
end;

procedure TFiltroXMLAbs.Valor(sValor: string);
begin
  EscribirPCDATA( sValor );
end;

procedure TFiltroXMLAbs.ValorElemento(sNombreValor, sValor: string);
begin
  InicioElemento( sNombreValor );
  Valor( sValor );
  FinElemento( sNombreValor );
end;

function TFiltroXMLAbs._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TFiltroXMLAbs._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

class function TFiltroXMLAbs.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TFiltroXMLAbs(Result).FRefCount := 1;
end;

function TFiltroXMLAbs.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TFiltroXMLAbs.FinalizarXML;
begin
  CerrarRaiz;
end;

procedure TFiltroXMLAbs.GenerarXML(sRowSetName: string);
begin
  if( EmptyStr = sRowSetName ) then begin
    FRowSetName := ROWSET;
    FRowName    := ROW;
  end else begin
    FRowSetName := sRowSetName;
    FRowName    := Format( '%s_%s', [ROW, sRowSetName] );
  end;
end;

procedure TFiltroXMLAbs.IniciarXML;
begin
  InicializarStream;
  EscribirRaiz( INFORME );
  GenerarIteradores;
end;

function TFiltroXMLAbs.EsAmpLtGt( s: string; i: integer ): boolean;
begin
  result := ('&amp;'  = Copy(s, i, 5 )) or
            ('&lt;'   = Copy(s, i, 4 )) or
            ('&gt;'   = Copy(s, i, 4 )) or
            ('&quot;' = Copy(s, i, 6 )) or
            ('&apos;' = Copy(s, i, 6 ));
  // Hay que tener en cuenta &#[0-9]{1,3};
end;

{ TFiltroXMLList }

class procedure TFiltroXMLList.AplicarXSLT(pLst: TList;
  sArchivoXML, sArchivoXSL, sArchivoHTML: string;
  pslColsSelected : TStringList = nil; sRowSetName: string = '');
var
  o: TFiltroXMLList;
begin
  o := TFiltroXMLList.CreateNew;
  try
    if( FileExists( sArchivoXML ) ) then begin
      SysUtils.DeleteFile( sArchivoXML );
    end;
    o.StreamSalida := TFileStream.Create( sArchivoXML, fmCreate or fmOpenReadWrite );
    o.Lista := pLst;
    if( nil <> pslColsSelected ) then begin
      o.Columnas.AddStrings( pslColsSelected );
    end else begin
      tiGetPropertyNames( TObject(pLst.Items[0]) as TPersistent,
                          pslColsSelected );
    end;

    o.IniciarXML;
    o.GenerarXML( sRowSetName );
    o.FinalizarXML;
    o.StreamSalida := nil;

    o.AplicarXSL( sArchivoXML, sArchivoXSL, sArchivoHTML );
  finally
    o.Free;
  end;
end;

constructor TFiltroXMLList.Create;
begin
  inherited;

  FColumnas := TStringList.Create;
  FColumnas.Clear;
end;

destructor TFiltroXMLList.Destroy;
begin
  FColumnas.Free;

  inherited;
end;

class procedure TFiltroXMLList.ExportarListaXML(pLst: TList;
  sArchivoXML: string; pslColsSelected : TStringList = nil;
  sRowSetName: string = '');
var
  o: TFiltroXMLList;
begin
  o := TFiltroXMLList.CreateNew;
  try
    if( FileExists( sArchivoXML ) ) then begin
      SysUtils.DeleteFile( sArchivoXML );
    end;
    o.StreamSalida := TFileStream.Create( sArchivoXML, fmCreate or fmOpenReadWrite );

    o.Lista := pLst;
    if( nil <> pslColsSelected ) then begin
      o.Columnas.AddStrings( pslColsSelected );
    end else begin
      tiGetPropertyNames( TObject(pLst.Items[0]) as TPersistent,
                          o.Columnas );
    end;

    o.IniciarXML;
    o.GenerarXML( sRowSetName );
    o.FinalizarXML;
  finally
    o.Free;
  end;
end;

procedure TFiltroXMLList.GenerarXML(sRowSetName: string);
var
  i, j: integer ;
  sVal, sAtributo: string ;
  p: TPersistent ;
begin
  inherited;
  
  Escribirtag( RowSetName );
  for i := 0 to Lista.Count - 1 do begin
    EscribirTag( RowName );
    p := ( TObject( Lista.Items[i]) as TPersistent ) ;
    for j := 0 to Columnas.Count - 1 do begin
      sAtributo := Columnas.Strings[j] ;
      if( 'TDateTime' = GetPropInfo(p, sAtributo).PropType^.Name )then begin
        sVal := tiDateTimeToStr(TypInfo.GetPropValue(p, sAtributo));
      end else begin
        sVal := TypInfo.GetPropValue( p, sAtributo, True ) ;
      end;

      EscribirTag( sAtributo );
      EscribirPCDATA( sVal );
      CerrarTag( sAtributo );
    end ;
    CerrarTag( RowName );
  end ;
  CerrarTag( RowSetName );
end;

{ TFiltroXMLSQL }

class procedure TFiltroXMLSQL.AplicarXSLT(sSQL, sArchivoXML, sArchivoXSL,
  sSalida: string; ssParams: TListaVariables; sRowSetName: string);
var
  o: TFiltroXMLSQL;
begin
  o := TFiltroXMLSQL.Create;
  try
    if( FileExists( sArchivoXML ) ) then begin
      DeleteFile( PChar( sArchivoXML ) );
    end;

    o.SQL.Clear;
    o.SQL.Add( sSQL );

    o.Params.Text := ssParams.Text;

    o.StreamSalida := TFileStream.Create( sArchivoXML, fmCreate or fmOpenReadWrite );
    o.InicializarStream;
    o.GenerarXML( sRowSetName );
    o.StreamSalida := nil;

    o.AplicarXSL( sArchivoXML, sArchivoXSL, sSalida );
  finally
    o.Free;
  end;
end;

constructor TFiltroXMLSQL.Create;
begin
  inherited;

  FSQL := TStringList.Create;
end;

destructor TFiltroXMLSQL.Destroy;
begin
  FSQL.Free;

  inherited;
end;

class procedure TFiltroXMLSQL.Exportar(sSQL: string; ssParams: TListaVariables;
  sArchivoSalida: string; sRowSetName: string);
var
  fltr: TFiltroXMLSQL;
begin
  fltr := TFiltroXMLSQL.CreateNew;
  try
    fltr.SQL.Clear;
    fltr.SQL.Add( sSQL );

    fltr.Params.Text := ssParams.Text;

    fltr.StreamSalida := TFileStream.Create( sArchivoSalida, fmCreate or fmOpenReadWrite or fmShareDenyNone );
    fltr.InicializarStream;
    fltr.GenerarXML( sRowSetName );
  finally
    fltr.Free;
  end;
end;

procedure TFiltroXMLSQL.GenerarXML(sRowSetName: string );
begin
  inherited;

  EscribirTag( RowSetName );
  try
    Read;
  finally
    CerrarTag( RowSetName );
  end;
end;

{ TVisPerXMLAbs }

function TVisPerXMLAbs.AcceptVisitor: boolean;
begin
  Result := Visited is TFiltroXMLSQL;
end;

procedure TVisPerXMLAbs.Final;
begin
  // nada.
end;

procedure TVisPerXMLAbs.Init;
begin
  Query.SQL.Clear;
  Query.SQL.AddStrings( (Visited as TFiltroXMLSQL).SQL );
end;

procedure TVisPerXMLAbs.MapRowToObject;
var
  fltr: TFiltroXMLSQL;
  i, rc: Integer;
  s: string;
begin
  fltr := Visited as TFiltroXMLSQL;

  fltr.EscribirTag( fltr.RowName );
  try
    rc := Query.FieldCount - 1;
    for i := 0 to rc do begin
      s := LowerCase( Query.FieldName( i ) );
      fltr.EscribirTag( s );
      fltr.EscribirPCDATA( Query.FieldAsString[ s ] );
      fltr.CerrarTag( s );
    end;
  finally
    fltr.CerrarTag( fltr.RowName );
  end;
end;

procedure TVisPerXMLAbs.SetupParams;
var
  i, pc: Integer;
  fltr: TFiltroXMLSQL;
  s: string;
begin
  fltr := visited as TFiltroXMLSQL;
  pc := Query.ParamCount - 1;
  for i := 0 to pc do begin
    s := Query.ParamName( i );
    Query.ParamAsString[ s ] := fltr.ParamValue( s );
  end;
end;


end.

