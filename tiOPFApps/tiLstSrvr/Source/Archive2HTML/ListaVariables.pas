{$I tiDefines.inc}

unit ListaVariables;

interface

uses SysUtils, Classes;

type
  TListaVariables = class(TObject)
  private
    FLista: TStringList;

    function GetCommaText: string;
    function GetNames(idx: Integer): string;
    function GetText: string;
    function GetValues(sNombre: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetText(const Value: string);
    procedure SetValues(sNombre: string; const Value: string);
    function GetList: TStringList;

  protected
    procedure VaciaLista;
    function GetTexto( sSeparadorLineas: string ): string;
    procedure SetTexto( sl: TStringList );
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IndexOfName( sNombre: string ): Integer;
    function Count: Integer;

    property Names  [ idx: Integer    ]: string read GetNames;
    property Values [ sNombre: string ]: string read GetValues write SetValues;

    property List      : TStringList read GetList;
    property Text      : string      read GetText      write SetText;
    property CommaText : string      read GetCommaText write SetCommaText;
  end;
//------------------------------------------------------------------------------

implementation

const
  INICIOVARIABLE = '=·';
  FINVARIABLE    = '·';

{ TListaVariables }

constructor TListaVariables.Create;
begin
  FLista := TStringList.Create;
end;

destructor TListaVariables.Destroy;
begin
  VaciaLista;
  FLista.Free;

  inherited;
end;

function TListaVariables.GetNames(idx: Integer): string;
begin
  Result := FLista.Strings[ idx ];
end;

function TListaVariables.GetText: string;
begin
  Result := GetTexto( #$D + #$A );
end;

procedure TListaVariables.SetText(const Value: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Value;
    SetTexto( sl );
  finally
    sl.Free;
  end;
end;

function TListaVariables.GetCommaText: string;
begin
  Result := GetTexto( ',' );
end;

procedure TListaVariables.SetCommaText(const Value: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := Value;
    SetTexto( sl );
  finally
    sl.Free;
  end;
end;

function TListaVariables.GetValues(sNombre: string): string;
var
  idx: Integer;
  slDatos: TStringList;
begin
  Result := EmptyStr;
  idx := FLista.IndexOf( sNombre );
  if( -1 < idx ) then begin
    slDatos := TStringList( FLista.Objects[ idx ] );
    if( True = Assigned( slDatos ) ) then begin
      Result := copy( slDatos.Text, 1, length( slDatos.Text ) - 2 );
      Result := StringReplace( Result, #$D + #$A, #$20, [rfReplaceAll] );
    end;
  end;
end;

procedure TListaVariables.SetValues(sNombre: string; const Value: string);
var
  idx: Integer;
begin
  idx := IndexOfName( sNombre );
  if( -1 < idx ) then begin
    if( EmptyStr <> Value ) then begin
      TStringList( FLista.Objects[ idx ]  ).Text := Value;
    end else begin
      FLista.Objects[ idx ].Free;
      FLista.Delete( idx );
    end;
  end else begin
    idx := FLista.AddObject( sNombre, TStringList.Create );
    TStringList( FLista.Objects[ idx ] ).Text := Value;
  end;
end;

function TListaVariables.IndexOfName(sNombre: string): Integer;
begin
  Result := FLista.IndexOf( sNombre );
end;

function TListaVariables.GetList: TStringList;
begin
  Result := FLista;
end;

function TListaVariables.Count: Integer;
begin
  Result := FLista.Count;
end;

procedure TListaVariables.VaciaLista;
begin
  while( FLista.Count > 0 ) do begin
    if( True = Assigned( FLista.Objects[ 0 ] ) ) then
      TStringList( FLista.Objects[ 0 ] ).Free;
    FLista.Delete( 0 );
  end;
end;

function TListaVariables.GetTexto(sSeparadorLineas: string): string;
var
  sl: TStringList;
  i: Integer;
  s: string;
begin
  sl := TStringList.Create;
  try
    sl.Clear;
    for i := 0 to FLista.Count - 1 do begin
      s := TStringList( FLista.Objects[ i ] ).Text;
      sl.Add ( FLista.Strings[ i ] + INICIOVARIABLE +
               copy( s, 1, length( s ) - 2 ) + FINVARIABLE );
    end;
    Result := sl.Text;
    Result := StringReplace( Result, '#$D#$A', sSeparadorLineas, [rfReplaceAll] );
  finally
    sl.Free;
  end;
end;

procedure TListaVariables.SetTexto(sl: TStringList);
var
  i, p: Integer;
  sNombre, s: string;
  slVal: TStringList;
begin
  i := 0;
  while( i < sl.Count ) do begin
    s := sl.Strings[ i ];
    p := AnsiPos( INICIOVARIABLE, s );
    if( 0 < p ) then begin
      slVal := TStringList.Create;
      try
        slVal.Clear;
        sNombre := Copy( s, 1, p - 1 );
        slVal.Add( Copy( s, p + Length( INICIOVARIABLE ), Length( s ) ) );
        while( 0 = AnsiPos( FINVARIABLE, slVal.Text ) ) do begin
          Inc( i );
          s := sl.Strings[ i ];
          slVal.Add( s );
        end;
        s := copy( slVal.Text, 1, length( slVal.Text ) - Length( FINVARIABLE ) - 2 );

        SetValues( sNombre, s );
      finally
        slVal.Free;
      end;
    end;
    Inc( i );
  end;
end;

end.
