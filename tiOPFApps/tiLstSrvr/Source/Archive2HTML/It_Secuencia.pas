{$I tiDefines.inc}
(*******************************************************************************
 
 ******************************************************************************)

unit It_Secuencia;

interface

uses
  SysUtils, Classes, IteradorAbs, Exp2xml;

type
  TIterador_Secuencia = class( TIterador )
  private
    FInicio: integer;
    FFinal: integer;
  protected
    procedure ProcesaParams( sParams: string ); override;

    property Inicio: integer read FInicio;
    property Final:  integer read FFinal;

    procedure InsertarIterador( sNombre: string ); override;
  end;

implementation

{ TIterador_Secuencia }

procedure TIterador_Secuencia.InsertarIterador(sNombre: string);
var
  sIterador, sItem: string;
  i: Integer;
begin
  sIterador := sNombre;
  sItem := ITEM + sNombre;

  InicioSecuencia( sIterador );
  for i := Inicio to Final do begin
    ValorElemento( sItem, IntToStr( i ) );
  end;
  FinSecuencia( sIterador );
end;

procedure TIterador_Secuencia.ProcesaParams(sParams: string);
var
  I: integer;
begin
  inherited;

  I := 1;
  FInicio := StrToInt( ExtraeParam( I ) );
  FFinal  := StrToInt( ExtraeParam( I ) );
end;

end.
