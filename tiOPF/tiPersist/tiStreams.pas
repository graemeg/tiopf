unit tiStreams;

interface
uses
  Classes
  ;

type
                                     
  // ToDo: Using regular stream methods like Position will break
  //       this as the internal position counter, and buffer will
  //       not be updated.

  TtiFileStream = class( TFileStream )
  private
    FLineDelim: string;
    FPos : LongInt ;
    FBufStr : string ;
    FLineDelimLen : Byte ;
    FEOF : boolean ;
    procedure SetLineDelim(const Value: string);
    procedure AppendToBufStr;
  public
    constructor Create(const FileName: string; Mode: Word);
    constructor CreateReadWrite( const pFileName : string ; pOverwrite : boolean = false ) ;
    constructor CreateReadOnly(  const pFileName : string ) ;
    property  LineDelim : string read FLineDelim write SetLineDelim ;
    procedure Write( const pString : string ) ; reintroduce ;
    procedure WriteLn( const pString : string = '' ) ;
    function  ReadLn : string ;
    function  EOF : boolean ;
  end ;

procedure tiWriteToStream( const pStream : TStream ; const pString : string ) ;

implementation
uses
  tiUtils
  ,tiDialogs // Debugging
  ,SysUtils
  ;

procedure tiWriteToStream( const pStream : TStream ; const pString : string ) ;
var
  lpcValue : PChar ;
begin
  Assert( pStream <> nil, 'Stream unassigned.' ) ;
  lpcValue := PChar( pString ) ;
  pStream.WriteBuffer( lpcValue^, length( lpcValue )) ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiFileStream
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiFileStream.Create(const FileName: string; Mode: Word);
begin
  inherited Create(FileName, Mode);
  FLineDelim := CrLf ;
  FLineDelimLen := 2 ;
  FPos := 0 ;
  FBufStr := '' ;
  FEOF := Size = 0 ;
end;

function TtiFileStream.ReadLn: string;
var
  lPos : LongInt ;
begin
  lPos := Pos( LineDelim, FBufStr ) ;
  if ( lPos = 0 ) and
     ( FPos <> Size ) then
  begin
    AppendToBufStr ;
    lPos := Pos( LineDelim, FBufStr ) ;
  end ;
  if lPos > 0 then
  begin
    result := Copy( FBufStr, 1, lPos - 1 ) ;
    FBufStr := Copy( FBufStr, lPos + FLineDelimLen {+ 1}, Length( FBufStr ) - lPos - FLineDelimLen + 1 ) ;
    FEOF := ( FBufStr = '' ) and ( FPos = Size )  ;
  end else
  begin
    FEOF := true ;
    result := FBufStr;
    FBufStr := '' ;
  end
end ;

procedure TtiFileStream.AppendToBufStr ;
var
  lBufLen : Word ;
  ls : string ;
const
  cBufLen = 1024 ;
begin
  if FPos + cBufLen > Size then
    lBufLen := Size - FPos
  else
    lBufLen := cBufLen ;
  SetLength(ls,  lBufLen);
  Read( ls[1], lBufLen ) ;
  FBufStr := FBufStr + ls ;
  Inc( FPos, lBufLen ) ;
end;

procedure TtiFileStream.Write(const pString: string);
begin
  tiWriteToStream( Self, pString ) ;
end;

procedure TtiFileStream.WriteLn(const pString: string = '' );
begin
  Write( pString + FLineDelim ) ;
end;

procedure TtiFileStream.SetLineDelim(const Value: string);
begin
  FLineDelim := Value;
  FLineDelimLen := Length( FLineDelim ) ;
end;

function TtiFileStream.EOF: boolean;
begin
  result := FEOF ;
end;

constructor TtiFileStream.CreateReadWrite(const pFileName: string; pOverwrite : boolean = false );
begin
  if FileExists( pFileName ) and ( not pOverwrite ) then
    Create( pFileName, fmOpenReadWrite or fmShareDenyWrite )
  else
    Create( pFileName, fmCreate or fmShareDenyWrite )
end;                        

constructor TtiFileStream.CreateReadOnly(const pFileName: string);
begin
  Create( pFileName, fmOpenRead or fmShareDenyNone ) ;
end;

end.
