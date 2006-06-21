{$I tiDefines.inc}

unit Correo_BOM;

interface

uses
 Classes, tiPtnVisPerObj, tiPtnVis, atConsts, Exp2xml;

type
  TIndice = class;
  TCorreo = class;

  TIndice = class(TPerObjList)
  protected
    function Archivo( sPath: string ): string;
  public
    procedure LoadIndexTxt( sPathOrg, sPathDst: string );
  end;

  TCorreo = class(TPerObjAbs)
  private
    FId: TId;
    FeMail: TeMail;
    FSubject: TSubject;
    FCleanSubject: TSubject;
    FDate: TDateTime;
    FDay: Word;
    FMonth: Word;
    FYear: Word;
    FParentId: TId;
  protected
    procedure SetId(const Value: TId);
    procedure SetDate(const Value: TDateTime);

    function RemoveLeadingValue( pStrLine, pStrValue: string ): string;
    function RemoveLeadingReNumber( pStrLine: string ): string;

    procedure SeteMail(const Value: TeMail);
    procedure SetSubject(const Value: TSubject);
  public
    constructor CreateFromString(const s: string);

    procedure ReadFromString(const s: string);
    procedure DateReadFromFile(const sPath: string);
    function  SubjectCleaner: string;

    function FileOrg(sPath: string): string;
    function FileDest(sPath: string): string;
    procedure GenerateHtml(sPathOrg, sPathDst: string);
  published
    property Id:           TId       read FId        write SetId;
    property eMail:        TeMail    read FeMail     write SeteMail;
    property Subject:      TSubject  read FSubject   write SetSubject;
    property CleanSubject: TSubject  read FCleanSubject;
    property Date:         TDateTime read FDate      write SetDate;

    property Day:   Word read FDay;
    property Month: Word read FMonth;
    property Year:  Word read FYear;
    property ParentId: TId read FParentId write FParentId;
  end;


implementation

uses
{$IFDEF WINDOWS}
  FileCtrl,
{$ENDIF}
  SysUtils, tiUtils, xml_util, IteradorAbs, It_Orden, It_Secuencia;

{ TIndice }

function TIndice.Archivo(sPath: string): string;
begin
  if( EmptyStr = sPath ) then
    sPath := ExtractFilePath( ParamStr(0) );

  sPath := tiAddTrailingSlash(sPath);

  Result := sPath + cArchIndice;
end;

procedure TIndice.LoadIndexTxt(sPathOrg, sPathDst: string);
var
  o: TCorreo;
  lsl: TStringList;
  lS: string;
  i: integer;
begin
  lsl := TStringList.Create;
  try
    lsl.LoadFromFile(Archivo( sPathOrg ));
    for i := 0 to lsl.Count - 1 do
    begin
      try
        ls := lsl.Strings[i];
        WriteLn(Copy('Read: ' + lS, 1, 79));
        o := TCorreo.CreateFromString( lS );
        Add( o );
        o.DateReadFromFile( sPathOrg );
        o.GenerateHtml( sPathOrg, sPathDst );
        o.ObjectState := posClean;
      except
        on e: exception do
          WriteLn('Error: ' + e.message);
        end;
    end;
  finally
    lsl.Free;
  end
end;

{ TCorreo }

function TCorreo.SubjectCleaner: string;
var
  s, t: string;
begin
  t := Subject;
  repeat
    s := t;
    t := RemoveLeadingValue( t, 'R:' );
    t := RemoveLeadingValue( t, 'RE:' );
    t := RemoveLeadingValue( t, 'FW:' );
    t := RemoveLeadingValue( t, 'AW:' );
    t := RemoveLeadingValue( t, 'tiOPF-' );
    t := RemoveLeadingValue( t, 'tiOPF - ' );
    t := RemoveLeadingValue( t, ' ' );
    t := RemoveLeadingReNumber( t );
  until( t = s );

  Result := Trim( t );
end;

constructor TCorreo.CreateFromString(const s: string);
begin
  inherited Create;

  ReadFromString( s );
end;

procedure TCorreo.DateReadFromFile(const sPath: string);
begin
  Date := GetDateFromMail( sPath, Id );
end;

procedure TCorreo.ReadFromString(const s: string);
begin
  Id      := tiToken( s, ',', 1 );
  eMail   := tiToken( s, ',', 2 );
  Subject := tiToken( s, ',', 3 );
end;

function TCorreo.RemoveLeadingReNumber(pStrLine: string): string;
  function _RemoveLeadingReNumber( pStrLine: string ): string;
  var
    s, t: string;
    i: integer;
  begin
    t := pStrLine;
    repeat
      s := t;
      t := RemoveLeadingValue( t, ']' );
      for i := 0 to 9 do
        t := RemoveLeadingValue( t, IntToStr(i) );
    until t = s;

    Result := t;
  end;

begin
  Result := RemoveLeadingValue( pStrLine, '['  );
  if( Result <> pStrLine ) then
    Result := _RemoveLeadingReNumber( Result );

  Result := Trim( Result );
end;

function TCorreo.RemoveLeadingValue(pStrLine, pStrValue: string): string;
var
  lL, lR, lV: integer;
  l, v: string;
begin
  lL := Length( pStrLine );
  lV := Length( pStrValue );
  lR := lL - lV + 1;
  l := UpperCase( pStrLine );
  v := UpperCase( pStrValue );

  if( Copy(l, 1, lV) = v ) then
    Result := Copy( pStrLine, lV + 1, lR )
  else
    Result := pStrLine;
end;

procedure TCorreo.SeteMail(const Value: TeMail);
var
  s: string;
begin
  FeMail := Value;
  s      := StringReplace( Value, '@', ' at ',   [rfReplaceAll] );
  s      := StringReplace( s,     '.', ' dot ',  [rfReplaceAll] );
  FeMail := StringReplace( s,      #0, EmptyStr, [rfReplaceAll] );
end;

procedure TCorreo.SetSubject(const Value: TSubject);
begin
  FSubject := Value;
  FCleanSubject := SubjectCleaner;
end;

procedure TCorreo.SetId(const Value: TId);
begin
  FId := StringReplace( Value, #0, EmptyStr, [rfReplaceAll] );
end;

function TCorreo.FileDest(sPath: string): string;
begin
  Result := tiAddTrailingSlash( sPath ) + Format( '%d\%d\', [Year, Month] );
  ForceDirectories( Result );
  Result := Result + Id + cHtmlExt;
end;

function TCorreo.FileOrg( sPath: string ): string;
begin
  Result := tiAddTrailingSlash( sPath ) + Id + cXmlExt;
end;

procedure TCorreo.GenerateHtml( sPathOrg, sPathDst: string );
var
  flt: TFiltroXMLAbs;
begin
  flt := TFiltroXMLAbs.Create;
  try
    ForceDirectories( ExtractFileDir( FileDest( sPathDst ) ) );

//    Writeln( 'Generating html for message ', c.Id, '....' );
    flt.AplicarXSL( FileOrg( sPathOrg ), cMailXsl, FileDest( sPathDst ) );
  finally
    flt.Free;
  end;
end;

procedure TCorreo.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  DecodeDate( FDate, FYear, FMonth, FDay );

  if( 1899 = FYear ) then begin
    FYear := 2001;
    FDay  :=    1;
    FDate := EncodeDate( FYear, FMonth, FDay );
  end;
end;

end.
