{$I tiDefines.inc}
(*******************************************************************************
  Formato de los parametros de TIterMgr.Ejecutar: sNombre, sParams.
  sNombre, sParams forman una línea sNombre=sParams en el array de parámetros que
  recibe el gestor de informes.

    sNombre: Nombre del iterador (Nombre que contendran los tags de apertura y
               cierre del operador. Los tags para los items serán del formato
               item_<sNombre>.
    sParams: Si contiene información de un iterador será de la forma:
                 Iterador<chSep><TipoIterador><chSep><Param_1><chSep>...
             El iterador ha de recibir sParams de la siguiente forma:
                 <chSep><Param_1>.....<chSep><Param_n>
 ******************************************************************************)
{$I tiDefines.inc}

unit IteradorAbs;

interface

uses
  SysUtils, Classes, tiPtnVisPerObj, tiPtnVisSQL, tiPtnVisMgr, tiPersist;

type
//------------------------------------------------------------------------------
  IIteradoInterface = interface
     ['{D2EE8ED1-90D9-11D7-AE5F-00500448FF80}']
    procedure InicioSecuencia( sNombreSecuencia: string );
    procedure InicioElemento( sNombreSecuencia: string ); overload;
    procedure InicioElemento( sNombreSecuencia: string; Atributos: array of string ); overload;
    procedure ValorElemento( sNombreValor, sValor: string );
    procedure Valor( sValor: string );
    procedure FinElemento( sNombreSecuencia: string );
    procedure FinSecuencia( sNombreSecuencia: string );
  end;
//------------------------------------------------------------------------------
  TIterador = class(TPerObjAbs)
  private
    FchSep: Char;
    FLocalParams: string;
    FObjSalida: TObject;
    FInterfaz: IIteradoInterface;
    FObjPoseeInterfaz: Boolean;
  protected
    property chSep: Char read FchSep;
    property LocalParams: string read FLocalParams;
    property ObjSalida: TObject read FObjSalida;
    property Interfaz: IIteradoInterface Read FInterfaz;
    property ObjPoseeInterfaz: Boolean Read FObjPoseeInterfaz;

    function ExtraeParam( var I: Integer ): string;
    procedure ProcesaParams( sParams: string ); virtual;
    procedure SetObjSalida( o: TObject );
    procedure InsertarIterador( sNombre: string ); virtual; abstract;

    // Impl. IIteradoInterfaz para que la usen las clases que utilicen internamente los iteradores
    procedure InicioSecuencia( sNombreSecuencia: string );
    procedure InicioElemento( sNombreSecuencia: string ); overload;
    procedure InicioElemento( sNombreSecuencia: string; Atributos: array of string ); overload;
    procedure ValorElemento( sNombreValor, sValor: string );
    procedure Valor( sValor: string );
    procedure FinElemento( sNombreSecuencia: string );
    procedure FinSecuencia( sNombreSecuencia: string );
  public
    constructor Create; override;
    destructor Destroy; override;

    class procedure Ejecutar( sNombre, sParams: string; o: TObject );
  end;
//------------------------------------------------------------------------------
  TIteradorClass = class of TIterador;
//------------------------------------------------------------------------------
  TIterador_DiasMes = class(TIterador)
  protected
    procedure InsertarIterador( sNombre: string ); override;
  end;
//------------------------------------------------------------------------------
  TIterMgr = class(TObject)
  private
    FLista: TStringList;

  protected
    function EsIterador( sParams: string ): Boolean;
    function TipoIterador( sParams: string ): string;
    function ObtenParams( sParams: string ): string;

  public
    constructor Create;
    destructor Destroy; override;

    procedure RegistrarIterador( sNombre: string; clIterador: TIteradorClass );
    procedure Ejecutar( sNombre, sParams: string; o: TObject );

    class function I( destruir: Boolean = false ): TIterMgr;
  end;
//------------------------------------------------------------------------------


implementation

uses
  tiUtils, Exp2xml, atConsts;

var
  FInstancia: TIterMgr = nil;

{ TIterMgr }

constructor TIterMgr.Create;
begin
  FLista := TStringList.Create;
end;

destructor TIterMgr.Destroy;
begin
  FLista.Free;

  inherited;
end;

procedure TIterMgr.Ejecutar(sNombre, sParams: string; o: TObject);
var
  lTipo, lParams: string;
  idx: Integer;
begin
  if( True = EsIterador( sParams ) ) then begin
    lTipo   := TipoIterador( sParams );
    lParams := ObtenParams( sParams );
    idx := FLista.IndexOf( UpperCase( lTipo ) );

    if( -1 <> idx ) then begin
      TIteradorClass( FLista.Objects[ idx ] ).Ejecutar( sNombre, lParams, o );
    end;
  end;
end;

function TIterMgr.EsIterador(sParams: string): Boolean;
begin
  Result := CompareText( ITERADOR, Copy( sParams, 1, Length( ITERADOR ) ) ) = 0;
end;

class function TIterMgr.I(destruir: Boolean): TIterMgr;
begin
  if( False = destruir ) then begin
    if( False = Assigned( FInstancia ) ) then begin
      FInstancia := TIterMgr.Create;
    end;
  end else begin
    if( True = Assigned( FInstancia ) ) then begin
      FreeAndNil( FInstancia );
    end;
  end;
  Result := FInstancia;
end;

function TIterMgr.ObtenParams(sParams: string): string;
var
  cSep: Char;
  I: Integer;
begin
  cSep := sParams[ Length( ITERADOR ) + 1 ];

  I := 1;
  ExtraeCadena( sParams, I, cSep );  // Extraemos 'Iterador'
  ExtraeCadena( sParams, I, cSep );  // Extraemos 'Tipo'
  Result := Copy( sParams, I - 1, Length( sParams ) - I + 2 );
end;

procedure TIterMgr.RegistrarIterador(sNombre: string;
  clIterador: TIteradorClass);
begin
  Assert( -1 = FLista.IndexOf( sNombre ), 'Iterador YA registrado' );
  FLista.AddObject( UpperCase( sNombre ), TObject( clIterador ) );
end;

function TIterMgr.TipoIterador(sParams: string): string;
var
  cSep: Char;
  I: Integer;
begin
  cSep := sParams[ Length( ITERADOR ) + 1 ];

  I := 1;
  ExtraeCadena( sParams, I, cSep );  // Extraemos Iterador;
  Result := ExtraeCadena( sParams, I, cSep );
end;

{ TIterador }

constructor TIterador.Create;
begin
  inherited;

end;

destructor TIterador.Destroy;
begin
  inherited;

end;

class procedure TIterador.Ejecutar(sNombre, sParams: string;
  o: TObject);
var
  I: TIterador;
begin
  I := Self.Create;
  try
    I.ProcesaParams(sParams);
    I.SetObjSalida( o );
    if( True = I.ObjPoseeInterfaz ) then begin
      I.InsertarIterador( sNombre );
      I.FInterfaz := nil;
    end;
  finally
    I.Free;
  end;
end;

function TIterador.ExtraeParam(var I: Integer): string;
begin
  Result := ExtraeCadena( LocalParams, I, chSep );
end;

procedure TIterador.FinElemento(sNombreSecuencia: string);
begin
  Interfaz.FinElemento( sNombreSecuencia );
end;

procedure TIterador.FinSecuencia(sNombreSecuencia: string);
begin
  Interfaz.FinSecuencia( sNombreSecuencia );
end;

procedure TIterador.InicioElemento(sNombreSecuencia: string;
  Atributos: array of string);
begin
  Interfaz.InicioElemento( sNombreSecuencia, Atributos );
end;

procedure TIterador.InicioElemento(sNombreSecuencia: string);
begin
  Interfaz.InicioElemento( sNombreSecuencia );
end;

procedure TIterador.InicioSecuencia(sNombreSecuencia: string);
begin
  Interfaz.InicioSecuencia( sNombreSecuencia );
end;

procedure TIterador.ProcesaParams(sParams: string);
begin
  FchSep := sParams[1];
  FLocalParams := Copy( sParams, 2, Length( sParams ) );
end;

procedure TIterador.SetObjSalida(o: TObject);
begin
  FObjSalida := o;
  FObjPoseeInterfaz := o.GetInterface( IIteradoInterface, FInterfaz );

end;

procedure TIterador.Valor(sValor: string);
begin
  Interfaz.Valor( sValor );
end;

procedure TIterador.ValorElemento(sNombreValor, sValor: string);
begin
  Interfaz.ValorElemento( sNombreValor, sValor );
end;

{ TIterador_DiasMes }

procedure TIterador_DiasMes.InsertarIterador(sNombre: string);
var
  sIterador, sItem: string;
  i: Integer;
begin
  sIterador := sNombre;
  sItem := ITEM + sNombre;

  InicioSecuencia( sIterador );
  for i := 1 to 31 do begin
    ValorElemento( sItem, IntToStr( i ) );
  end;
  FinSecuencia( sIterador );
end;

end.
