{$I tiDefines.inc}

unit Exp2xml_Index;

interface

uses
 Classes, tiPtnVisPerObj, tiPtnVis, atConsts, Exp2xml, Correo_BOM;

type
  TFiltroXMLListCorreo = class( TFiltroXMLAbs )
  private
    FLista: TList;
    FColumnas: TStringList;

    FYearExp: Integer;
    FMonthExp: Integer;
  protected
    procedure GenerarXML( sRowSetName: string = '' ); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Lista:    TList       read FLista write FLista;
    property Columnas: TStringList read FColumnas;

    property MonthExp: Integer read FMonthExp write FMonthExp;
    property YearExp:  Integer read FYearExp  write FYearExp;
  end;

implementation

uses
{$IFDEF WINDOWS}
  FileCtrl,
{$ENDIF}
  SysUtils, tiUtils, xml_util, IteradorAbs, It_Orden, It_Secuencia, TypInfo;

{ TFiltroXMLListCorreo }

constructor TFiltroXMLListCorreo.Create;
begin
  inherited;

  FColumnas := TStringList.Create;
  FColumnas.Clear;
end;

destructor TFiltroXMLListCorreo.Destroy;
begin
  FColumnas.Free;

  inherited;
end;

procedure TFiltroXMLListCorreo.GenerarXML(sRowSetName: string);
var
  i, j: integer ;
  sVal, sAtributo: string ;
  p: TPersistent ;
  c: TCorreo;
begin
  inherited;

  Escribirtag( RowSetName );
  for i := 0 to Lista.Count - 1 do begin
    c := TCorreo(Lista.Items[i]);
    if( ((MonthExp = c.Month) and (YearExp = c.Year)) or
        ((0        = MonthExp) and (0      = YearExp)) ) then begin
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
    end;
  end ;
  CerrarTag( RowSetName );
end;

end.
