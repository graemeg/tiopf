{$I tiDefines.inc}
(*******************************************************************************
 No Tienen Parametros
 ******************************************************************************)

unit It_Authors;

interface

uses
  SysUtils, Classes, IteradorAbs, Exp2xml;

type
  TIterador_Authors = class( TIterador )
  protected
    function ListaOrdenada: TList;

    procedure InsertarIterador( sNombre: string ); override;
  end;

implementation

uses
  Indice_BOM;

{ TIterador_Authors }

procedure TIterador_Authors.InsertarIterador(sNombre: string);
var
  r, s, sIterador, sItem: string;
  i: Integer;
  lst: TList;
begin
  sIterador := sNombre;
  sItem := ITEM + sNombre;

  s := EmptyStr;
  lst := ListaOrdenada;
  InicioSecuencia( sIterador );
  for i := 0 to lst.Count - 1 do begin
    r := TCorreo( lst.Items[i] ).eMail;
    if( r <> s ) then begin
      ValorElemento( sItem, r );
      s := r;
    end;
  end;
  FinSecuencia( sIterador );
end;

function TIterador_Authors.ListaOrdenada: TList;
begin
  Result := TControl.I.ByAuthor;
end;

end.
