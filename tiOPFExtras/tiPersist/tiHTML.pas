{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    Created: Mid 1998

  Purpose:
    A class to write a TList of TPersistents to HTML

  ToDo:
    This should be converted to the visitor framework


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiHTML;

interface
uses
  Classes
  ;

type

  // ---------------------------------------------------------------------------
  TtiHTMLBuilder = class( TObject )
  private
    FStream : TStream ;
    procedure   Write( const psText : string ) ;
    procedure   WriteLn( const psText : string = '' ) ;
    procedure   WriteCaption( pCaption : string ) ;
    procedure   WriteColHeadings( aHeadings : array of string ) ;
    procedure   WriteData( pData : TPersistent ; pslCols : TStringList ) ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   ListToHTML( pList    : TList ;
                            pCaption : string ;
                            aCols : array of string ;
                            aHeadings : array of string ) ;

    property    Stream : TStream read FStream write FStream ;
  end ;

// Singleton
function gHTMLBuilder : TtiHTMLBuilder ;

const
   cHTMLParagraphEnd = '</P>' ;
   cHTMLNewLine = '<br>' ;

implementation
uses
  tiUtils
  ,TypInfo
  ,tiLog
  ,SysUtils
  ;

var
  uHTMLBuilder : TtiHTMLBuilder ;

// Singleton
// -----------------------------------------------------------------------------
function gHTMLBuilder : TtiHTMLBuilder ;
begin
  if uHTMLBuilder = nil then
    uHTMLBuilder := TtiHTMLBuilder.Create ;
  result := uHTMLBuilder ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiHTMLBuilder
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiHTMLBuilder.Create;
begin
  inherited ;
end;

// -----------------------------------------------------------------------------
destructor TtiHTMLBuilder.Destroy;
begin
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.ListToHTML( pList: TList;
                                     pCaption: string ;
                                     aCols : array of string ;
                                     aHeadings : array of string ) ;
var
  lslCols : TStringList ;
  i : integer ;
begin

  // To clear the stream, must learn a better way of doing this
  Assert( FStream <> nil, 'Stream not assinged' ) ;

  Assert( pList <> nil, 'List not assigned' ) ;
  Assert( pList.Count > 0, 'List contains no elements' ) ;
  Assert( (TObject( pList.Items[0] ) is TPersistent ), 'List elements not TPersistent' ) ;
  Assert( High( aCols ) = High( aHeadings ), 'Number of does not match the number of headings' ) ;

  lslCols := TStringList.Create ;

  try

    if High( aCols ) = 0 then
      tiGetPropertyNames( TPersistent( pList.Items[0] ), lslCols )
    else
      for i := Low( aCols ) to High( aCols ) do begin
        if not IsPublishedProp( TPersistent( pList.Items[0] ), aCols[i] ) then
          raise exception.create( aCols[i] + ' is not a published property of ' +
                                  TObject( pList.Items[i] ).ClassName +
                                  '. Called in TtiHTMLBuilder.ListToHTML' ) ;
        lslCols.add( aCols[i] ) ;
      end ;

    WriteCaption( pCaption ) ;
    WriteColHeadings( aHeadings ) ;
    for i := 0 to pList.Count - 1 do
      WriteData( pList.Items[i], lslCols ) ;
    WriteLn( '</TABLE>' ) ;
  finally
    lslCols.Free ;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.Write(const psText: string);
var
  lpcValue : PChar ;
begin
  lpcValue := PChar( psText ) ;
  FStream.WriteBuffer( lpcValue^, length( lpcValue )) ;
end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.WriteCaption(pCaption: string);
begin
  WriteLn( '' ) ;
  WriteLn( '' ) ;
  WriteLn( '<TABLE>' ) ;
  WriteLn( '<table BORDER=2 ALIGN="CENTER">' ) ;
  WriteLn( '<CAPTION><STRONG><FONT SIZE=4>' + pCaption +
           '</FONT></STRONG></CAPTION>' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.WriteColHeadings(aHeadings : array of string);
var
  i : integer ;
begin
  WriteLn( '' ) ;
  WriteLn( '<COL ALIGN="LEFT" SPAN=' +
           IntToStr(High( aHeadings )) + '>' ) ;
  Write( '<tr>' ) ;
  for i := Low(aHeadings) to High(aHeadings) do
    Write( '<th>' + aHeadings[i] ) ;
  WriteLn
end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.WriteData(pData: TPersistent;pslCols: TStringList);
var
  i  : integer ;
  ls : string ;
begin
  Write( '<tr>' ) ;
  for i := 0 to pslCols.Count - 1 do begin
    ls := GetPropValue( pData, pslCols.Strings[i] ) ;
    Write( '<td>' + ls ) ;
  end ;
  WriteLn( '' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiHTMLBuilder.WriteLn(const psText: string = '' );
begin
  Write( psText + CrLf ) ;
end;

initialization

finalization
  if uHTMLBuilder <> nil then
    uHTMLBuilder.Free ;
end.

