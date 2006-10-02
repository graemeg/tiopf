unit tiCompressNone;

{$I tiDefines.inc}

interface
uses
  tiCompress
  ,Classes
  ;


type
  // Null object pattern - implement the TtiCompress, but with no compression
  TtiCompressNone = class( TtiCompressAbs )
  private
    procedure CopyFile( const psFrom, psTo : string ) ;
  public
    function  CompressStream(   pFrom : TStream ; pTo : TStream ) : Extended ; override ;
    procedure DecompressStream( pFrom : TStream ; pTo : TStream ) ; override ;
    function  CompressBuffer(   const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) : Extended ; override ;
    procedure DecompressBuffer( const pFrom: Pointer  ; const piFromSize : Integer;
                                out   pTo:   Pointer  ; out   piToSize   : Integer) ; override ;
    function  CompressString(   const psFrom : string ; var psTo : string )   : Extended ; override ;
    procedure DecompressString( const psFrom : string ; var psTo : string )   ; override ;
    function  CompressFile(     const psFrom : string ; const psTo : string ) : Extended ; override ;
    procedure DecompressFile(   const psFrom : string ; const psTo : string ) ; override ;
  end;
  

implementation
uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiConstants
  ,tiUtils
  ;


{ TtiCompressNone }
  
// Compress a buffer
function TtiCompressNone.CompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer) : Extended ;
begin
  Assert( false, 'Not implemented yet.' ) ;
  result := 0 ;
end;


// Compress a file
function TtiCompressNone.CompressFile(const psFrom, psTo: string) : Extended ;
begin
  CopyFile(psFrom, psTo);
  result := 1;
end;


// Compress a TStream
function TtiCompressNone.CompressStream(pFrom, pTo: TStream) : Extended ;
begin
  pFrom.Seek(0, soFromBeginning);
  pTo.CopyFrom(pFrom, pFrom.Size);
  pFrom.Seek(0, soFromBeginning);
  pTo.Seek(0, soFromBeginning);
  result := 100;
end;


// Compress a string
function TtiCompressNone.CompressString(const psFrom: string;
  var psTo: string) : Extended ;
begin
  psTo := psFrom ;
  result := 100 ;
end;


// Copy a file from one place to another
procedure TtiCompressNone.CopyFile(const psFrom, psTo: string);
var
  liErrorCode : word ;
begin
  {$IFDEF MSWINDOWS}
  if FileExists( psTo ) then
    Windows.DeleteFile( PChar( psTo )) ;
  Windows.CopyFile( pChar( psFrom ), pChar( psTo ), true ) ;
  liErrorCode := getLastError( ) ;
  {$ENDIF}
  {$IFDEF UNIX}
  if FileExists(psTo) then
    SysUtils.DeleteFile(psTo);
  tiCopyFile(psFrom, psTo);
  liErrorCode := 0; // No error checking yet!
  {$ENDIF}
  if liErrorCode <> 0 then begin
    raise exception.Create( 'Unable to copy <' +
                            psFrom +
                            '> to <' +
                            psTo + '>' + Cr +
                            'Error code: ' +
                            intToStr( liErrorCode ) + Cr +
                            'Error message: ' +
                            SysErrorMessage( liErrorCode )) ;
  end ;
end;


// Decompress a buffer
procedure TtiCompressNone.DecompressBuffer(const pFrom: Pointer;
  const piFromSize: Integer; out pTo: Pointer; out piToSize: Integer);
begin
  Assert(false, 'Not implemented yet.');
end;


// Decompress a file
procedure TtiCompressNone.DecompressFile(const psFrom, psTo: string);
begin
  CopyFile(psFrom, psTo);
end;


// Decompress a TStream
procedure TtiCompressNone.DecompressStream(pFrom, pTo: TStream);
begin
  pFrom.Seek(0, soFromBeginning);
  pTo.CopyFrom(pFrom, pFrom.Size);
  pFrom.Seek(0, soFromBeginning);
  pTo.Seek(0, soFromBeginning);
end;


// Decompress a string
procedure TtiCompressNone.DecompressString(const psFrom: string; var psTo: string);
begin
  psTo := psFrom ;
end;


initialization
  // Register the TtiCompress with the CompressFactory
  gCompressFactory.RegisterClass( cgsCompressNone, TtiCompressNone ) ;
  gtiCompressClass := TtiCompressNone ;

end.
