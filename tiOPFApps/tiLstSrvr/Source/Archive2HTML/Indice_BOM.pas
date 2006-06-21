{$I tiDefines.inc}

unit Indice_BOM;

interface

uses
 Classes, tiPtnVisPerObj, tiPtnVis, atConsts, Exp2xml, Exp2xml_Index, Correo_BOM;

type
  TtiGroupMailArchive2HTML = class( TObject )
  private
    FPathOrg: string;
    FPathDst: string;
    FXmlTempFile: string;

    FIndexTxt: TIndice;
    FDateOrder:   TList;
    FThreadOrder: TList;
    FAuthorOrder: TList;

    flt: TFiltroXMLListCorreo;
    cur_month, cur_year: integer;
  protected
    constructor Create;

    procedure InicOrder( l: TList );

    procedure SetPathOrg(const Value: string);
    procedure SetPathDst(const Value: string);

    procedure OrderByDate;
    procedure OrderByThread;
    procedure OrderByAuthor;

    function FileOrgCorreo( c: TCorreo ): string;
    function FileDestCorreo( c: TCorreo ): string;

    procedure ExportarListaXML( iMonth: Integer = 0; iYear: Integer = 0 );
    procedure GenerateHTML;
    procedure TransfCorreo( pPerObjAbs: TPerObjAbs );
  public
    destructor Destroy; override;

    class function I( bDestroy: Boolean = False ): TtiGroupMailArchive2HTML;
    procedure ProcesarArchivos( sPathOrg, sPathDst: string );

    property PathOrg: string read FPathOrg write SetPathOrg;
    property PathDst: string read FPathDst write SetPathDst;
    property XmlTempFile: string read FXmlTempFile;

    property IndexTxt: TIndice read FIndexTxt;

    property ByDate:   TList read FDateOrder;
    property ByThread: TList read FThreadOrder;
    property ByAuthor: TList read FAuthorOrder;
  end;

  function CompareByDate(Item1, Item2: Pointer): Integer;
  function CompareByThread(Item1, Item2: Pointer): Integer;
  function CompareByAuthor(Item1, Item2: Pointer): Integer;

implementation

uses
{$IFDEF WINDOWS}
  FileCtrl,
{$ENDIF}
  SysUtils, tiUtils, xml_util, IteradorAbs, It_Orden, It_Secuencia;

type
  TMiFiltroXMLListCorreo = class( TFiltroXMLListCorreo )
  end;

var
  oControl: TtiGroupMailArchive2HTML;

function CompareByAuthor(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr( TCorreo( Item1 ).eMail,
                            TCorreo( Item2 ).eMail );
end;

function CompareByDate(Item1, Item2: Pointer): Integer;
var
  e: Extended;
begin
  e := TCorreo( Item1 ).Date - TCorreo( Item2 ).Date;

  Result := 0 + Ord( e > 0.0 ) - Ord( e < 0.0 );
end;

function CompareByThread(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr( TCorreo( Item1 ).CleanSubject,
                            TCorreo( Item2 ).CleanSubject );
end;

{ TtiGroupMailArchive2HTML }

constructor TtiGroupMailArchive2HTML.Create;
begin
  FIndexTxt := TIndice.Create;

  FDateOrder   := TList.Create;
  FThreadOrder := TList.Create;
  FAuthorOrder := TList.Create;

  FXmlTempFile := tiAddTrailingSlash( tiGetTempDir ) + cTempXml;
//  FXmlTempFile := cTempXml;
end;

destructor TtiGroupMailArchive2HTML.Destroy;
begin
  FDateOrder.Free;
  FThreadOrder.Free;
  FAuthorOrder.Free;

  FIndexTxt.Free;

  inherited;
end;

procedure TtiGroupMailArchive2HTML.ExportarListaXML( iMonth, iYear: integer );
var
  o: TMiFiltroXMLListCorreo;
begin

  o := TMiFiltroXMLListCorreo.CreateNew;
  try
    o.MonthExp := iMonth;
    o.YearExp  := iYear;

    o.Params.Values[ 'DiasMes'  ] := 'Iterador Secuencia 1 31';
    o.Params.Values[ 'Meses'    ] := 'Iterador Secuencia 1 12';
//    o.Params.Values[ 'ByDate'   ] := 'Iterador ByDate';
//    o.Params.Values[ 'ByThread' ] := 'Iterador ByThread';
//    o.Params.Values[ 'ByAuthor' ] := 'Iterador ByAuthor';

    o.Lista := IndexTxt.List;
    o.Columnas.Clear;
    o.Columnas.Add( 'Id'           );
    o.Columnas.Add( 'eMail'        );
    o.Columnas.Add( 'Subject'      );
    o.Columnas.Add( 'CleanSubject' );
    o.Columnas.Add( 'Date'         );
    o.Columnas.Add( 'Day'          );
    o.Columnas.Add( 'Month'        );
    o.Columnas.Add( 'Year'         );


    if( FileExists( XmlTempFile  ) ) then begin
      DeleteFile( XmlTempFile );
    end;
    o.StreamSalida := TFileStream.Create( XmlTempFile, fmCreate );

    o.IniciarXML;
    o.GenerarXML( 'index' );
    o.FinalizarXML;
  finally
    o.Free;
  end;
end;

procedure TtiGroupMailArchive2HTML.GenerateHTML;
begin
  flt := TMiFiltroXMLListCorreo.Create;
  try
//    Writeln( 'Generating master index...' );
    flt.AplicarXSL( XmlTempFile, cMstIdxXsl, PathDst + cIdxHtml );

    IndexTxt.ForEach( TransfCorreo );
  finally
    flt.Free;
  end;
end;

class function TtiGroupMailArchive2HTML.I( bDestroy: Boolean = False ): TtiGroupMailArchive2HTML;
begin
  if( False = bDestroy ) then begin
    if( False = Assigned( oControl ) ) then
      oControl := TtiGroupMailArchive2HTML.Create;
  end else begin
    if( True = Assigned( oControl ) ) then
      FreeAndNil( oControl );
  end;

  Result := oControl;
end;

procedure TtiGroupMailArchive2HTML.InicOrder(l: TList);
var
  i: Integer;
begin
  l.Clear;
  for i := 0 to IndexTxt.Count - 1 do
    l.Add( IndexTxt.Items[ i ] );
end;

procedure TtiGroupMailArchive2HTML.OrderByAuthor;
begin
  InicOrder( ByAuthor );
  ByAuthor.Sort( CompareByAuthor );
end;

procedure TtiGroupMailArchive2HTML.OrderByDate;
begin
  InicOrder( ByDate );
  ByDate.Sort( CompareByDate );
end;

procedure TtiGroupMailArchive2HTML.OrderByThread;
begin
  InicOrder( ByThread );
  ByThread.Sort( CompareByThread );
end;

procedure TtiGroupMailArchive2HTML.ProcesarArchivos(sPathOrg, sPathDst: string);
begin
  PathOrg := sPathOrg;
  PathDst := sPathDst;

//  Write( 'Loading Index.txt....' );
  IndexTxt.LoadIndexTxt( sPathOrg, sPathDst );

//  Writeln( 'Ordering Index.txt by date....' );
  IndexTxt.List.Sort( CompareByDate );

//  Writeln( 'Sorting by thread....' );
//  OrderByThread;
//
//  Writeln( 'Sorting by date....' );
//  OrderByDate;
//
//  Writeln( 'Sorting by author....' );
//  OrderByAuthor;

//  Writeln( 'Generating index.xml....' );
    TIterMgr.I.RegistrarIterador( 'Secuencia',  TIterador_Secuencia     );
//    TIterMgr.I.RegistrarIterador( 'ByDate',   TIterador_OrdenDate   );
//    TIterMgr.I.RegistrarIterador( 'ByThread', TIterador_OrdenThread );
//    TIterMgr.I.RegistrarIterador( 'ByAuthor', TIterador_OrdenAuthor );
  ExportarListaXML;
  GenerateHTML;

end;

procedure TtiGroupMailArchive2HTML.SetPathDst(const Value: string);
begin
  FPathDst := tiAddTrailingSlash( Value );
end;

procedure TtiGroupMailArchive2HTML.SetPathOrg(const Value: string);
begin
  FPathOrg := tiAddTrailingSlash( Value );
end;

procedure TtiGroupMailArchive2HTML.TransfCorreo(pPerObjAbs: TPerObjAbs);
var
  c: TCorreo;
begin

  c := pPerObjAbs as TCorreo;
  WriteLn(Copy('Writing: ' + c.CleanSubject, 1, 79));

  if( (c.Month <> cur_month) or (c.Year <> cur_year) ) then begin
    ExportarListaXML( c.Month, c.Year );

    flt.Params.Values[ 'sel_month'  ] := IntToStr( c.Month );
    flt.params.Values[ 'month_name' ] := ShortMonthNames[ c.Month ];
    flt.Params.Values[ 'sel_year'   ] := IntToStr( c.Year );

//    Writeln( 'Generating default index for ', m, '/', y, '....' );
    flt.AplicarXSL( XmlTempFile, cMonthIdxXsl,  Format( '%s%d\%d\%s', [PathDst, c.Year, c.Month, cIdxHtml] ) );

//    Writeln( 'Generating date index for ', m, '/', y, '....' );
    flt.AplicarXSL( XmlTempFile, cDateIdxXsl,   Format( '%s%d\%d\%s', [PathDst, c.Year, c.Month, cIdxDate] ) );

//    Writeln( 'Generating author index for ', m, '/', y, '....' );
    flt.AplicarXSL( XmlTempFile, cAuthorIdxXsl, Format( '%s%d\%d\%s', [PathDst, c.Year, c.Month, cIdxAuthor] ) );

//    Writeln( 'Generating thread index for ', m, '/', y, '....' );
    flt.AplicarXSL( XmlTempFile, cThreadIdxXsl, Format( '%s%d\%d\%s', [PathDst, c.Year, c.Month, cIdxThread] ) );
  end;

  cur_month := c.Month;
  cur_year  := c.Year;
end;

function TtiGroupMailArchive2HTML.FileDestCorreo(c: TCorreo): string;
begin
  Result := c.FileDest( PathDst );
end;

function TtiGroupMailArchive2HTML.FileOrgCorreo(c: TCorreo): string;
begin
  Result := c.FileOrg( PathOrg );
end;

end.
