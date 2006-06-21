{$I tiDefines.inc}

unit xml_util;

interface

uses
  Indice_BOM, atConsts;

function GetDateFromMail( sPath: string; Id: TId ): TDateTime;
function xmlStrToDateTime(const s: string): TDateTime;

implementation

uses
  SysUtils, Classes, libxml2, libxslt, tiUtils, tiXml;

procedure AssertEncoding( sArch: string );
var
  f: TFileStream;
  s, t: string;
begin
  f := TFileStream.Create( sArch, fmOpenReadWrite );
  try
    s := tiStreamToString( f );
    if( Copy( s, 1, Length( cgXMLTagDocHeader ) ) = cgXMLTagDocHeader ) then begin
      t := StringReplace( s, cgXMLTagDocHeader, cgXMLTagDocHeaderWithEncoding, [] );
      t := StringReplace( t, #0, EmptyStr, [rfReplaceAll] );
      f.Seek( 0, soFromBeginning	 );
      f.Write( PChar( t )[ 0 ], Length( t ) );
    end;
  finally
    f.Free;
  end;

end;

//function GetStringFromMail( sArch: string; sXPathExpr: string ): string;
//var
//  f: TFileStream;
//begin
//  f := TFileStream.Create( sArch, fmOpenRead );
//  try
//    ti tiStreamToString( f );
//  finally
//    variable.Free;
//  end;
//
//end;

function GetStringFromMail( sArch: string; sXPathExpr: string ): string;
const
 BoolVal: array [Boolean] of string = ('False', 'True');
var
  doc:  xmlDocPtr;
  node: xmlNodePtr;
  ctxt: xmlXPathContextPtr;
  res:  xmlXPathObjectPtr;
begin
  doc := nil; ctxt := nil; res := nil;
  try
    AssertEncoding( sArch );
    doc := xmlParseFile( PChar(sArch) );
    if( nil = doc ) then
      exit; // ==>

    node := xmlDocGetRootElement( doc );
    if( nil = node ) then
      exit; // ==>

    ctxt := xmlXPathNewContext( doc );
    ctxt.node := node;
    res := xmlXPathEvalExpression( PChar(sXPathExpr), ctxt );
    if( nil = res ) then
      exit; // ==>

    case res.type_ of
      XPATH_UNDEFINED: Result := 'Undefined!!';
      XPATH_NODESET:   Result := 'Nodeset!!';
      XPATH_BOOLEAN:   Result := BoolVal[ res.boolval <> 0 ];
      XPATH_NUMBER:    Result := FloatToStr( res.floatval );
      XPATH_STRING:    Result := res.stringval;
    else
      Result := '???';
    end;

  finally
    xmlXPathFreeObject( res );
    xmlXPathFreeContext( ctxt );
    xmlFreeDoc( doc );
  end;
end;

function GetDateFromMail( sPath: string; Id: TId ): TDateTime;
var
  sArch, sDate: string;
begin

  sArch := tiAddTrailingSlash( sPath ) + Id + cXmlExt;
//  Writeln( sarch );
  sDate := GetStringFromMail(sArch, 'string(/tiOPFEmailArchive/date)' );
  Result := xmlStrToDateTime( sDate );
end;

function xmlStrToDateTime(const s: string): TDateTime;
var
  d, t: string;
begin
  d := ShortDateFormat;
  t := LongTimeFormat;
  ShortDateFormat := 'dd/MM/yyyy';
  LongTimeFormat  := 'hh:mm:ss';
  try
    if( EmptyStr <> s ) then 
      Result := StrToDateTime( s )
    else
      Result := 0;
  finally
    ShortDateFormat := d;
    LongTimeFormat  := t;
  end;
end;

end.
