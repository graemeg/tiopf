{$I tiDefines.inc}
(*******************************************************************************
 No Tienen Parametros
 ******************************************************************************)

unit It_Subjects;

interface

uses
  SysUtils, Classes, IteradorAbs, Exp2xml;

type
  TIterador_Subjects = class( TIterador )
  protected
    function ListaOrdenada: TList;

    procedure InsertarIterador( sNombre: string ); override;
  end;

implementation

uses
  Indice_BOM;

{ TIterador_Subjects }

procedure TIterador_Subjects.InsertarIterador(sNombre: string);
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
    r := TCorreo( lst.Items[i] ).CleanSubject;
    if( r <> s ) then begin
      ValorElemento( sItem, r );
      s := r;
    end;
  end;
  FinSecuencia( sIterador );
end;

function TIterador_Subjects.ListaOrdenada: TList;
begin
  Result := TControl.I.ByThread;
end;

end.

