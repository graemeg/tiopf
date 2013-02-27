{$I tiDefines.inc}

unit atConsts;

interface

const
  cArchIndice   = 'Index.txt';
  cIdxHtml      = 'default.htm';
  cIdxDate      = 'date.htm';
  cIdxAuthor    = 'authors.htm';
  cIdxThread    = 'threads.htm';

  cTempXml      = 'salida.xml';
  cMailXsl      = 'correo.xsl';
  cMstIdxXsl    = 'mst_idx.xsl';
  cMonthIdxXsl  = 'month_idx.xsl';
  cDateIdxXsl   = 'month_date.xsl';
  cAuthorIdxXsl = 'month_author.xsl';
  cThreadIdxXsl = 'month_thread.xsl';

  cXmlExt = '.xml';
  cHtmlExt = '.htm';

  cgXMLTagDocHeader             = '<?xml version="1.0"?>' ;
  cgXMLTagDocHeaderWithEncoding = '<?xml version="1.0" encoding="iso-8859-1"?>' ;
  
type
  TId      = string[  8];
  TeMail   = string[200];
  TSubject = string[200];


function ExtraeCadena( const s: string; var Pos: Integer; chSeparador: Char ): string;

implementation

uses
  SysUtils;
  
function ExtraeCadena( const s: string; var Pos: Integer; chSeparador: Char ): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(s)) and (s[I] <> chSeparador) do Inc(I);
  Result := Trim(Copy(s, Pos, I - Pos));
  if (I <= Length(s)) and (s[I] = chSeparador) then Inc(I);
  Pos := I;
end;

end.
