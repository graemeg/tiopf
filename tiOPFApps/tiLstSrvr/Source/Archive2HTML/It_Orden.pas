{$I tiDefines.inc}
(*******************************************************************************
 No Tienen Parametros
 ******************************************************************************)

unit It_Orden;

interface

uses
  SysUtils, Classes, IteradorAbs, Exp2xml;

type
  TIterador_Orden = class( TIterador )
  protected
    function ListaOrdenada: TList; virtual; abstract;

    procedure InsertarIterador( sNombre: string ); override;
  end;

  TIterador_OrdenDate = class( TIterador_Orden )
  protected
    function ListaOrdenada: TList; override;
  end;

  TIterador_OrdenThread = class( TIterador_Orden )
  protected
    function ListaOrdenada: TList; override;
  end;

  TIterador_OrdenAuthor = class( TIterador_Orden )
  protected
    function ListaOrdenada: TList; override;
  end;

implementation

uses
  Indice_BOM, Correo_BOM;

{ TIterador_Orden }

procedure TIterador_Orden.InsertarIterador(sNombre: string);
var
  sIterador, sItem: string;
  i: Integer;
  lst: TList;
begin
  sIterador := sNombre;
  sItem := ITEM + sNombre;

  lst := ListaOrdenada;
  InicioSecuencia( sIterador );
  for i := 0 to lst.Count - 1 do begin
    InicioElemento( sItem );
    ValorElemento( 'ID', TCorreo( lst.Items[i] ).Id );
    FinElemento( sItem );
  end;
  FinSecuencia( sIterador );
end;

{ TIterador_OrdenDate }

function TIterador_OrdenDate.ListaOrdenada: TList;
begin
  Result := TtiGroupMailArchive2HTML.I.ByDate;
end;

{ TIterador_OrdenThread }

function TIterador_OrdenThread.ListaOrdenada: TList;
begin
  Result := TtiGroupMailArchive2HTML.I.ByThread;
end;

{ TIterador_OrdenAuthor }

function TIterador_OrdenAuthor.ListaOrdenada: TList;
begin
  Result := TtiGroupMailArchive2HTML.i.ByAuthor;
end;

end.

