unit tiDUnitUtils;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiExcept
  ;

type

  EtiOPFDUnitException = class( EtiOPFException )
  ;

function  tiCreateStringOfSize( pSize : LongInt ) : string ;
procedure tiCreateTextFileOfSize( pFileName : string ; pSize : LongInt ) ;
procedure tiDUnitForceRemoveDir( const pDirectory : string ) ;
// Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
procedure tiFillTestingStream(AStream : TStream; ASize : integer);
procedure tiFillTestingStreamASCII(AStream : TStream; ASize : integer);

const
  cDUnitTestFloatPrecision = 0.000001 ;       
  
implementation
uses
  SysUtils
  ,tiUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ;

function  tiCreateStringOfSize(pSize: LongInt): string;
var
  ls: Char;
begin
  result := '';
  While Length(Result) < pSize do
  begin
    ls := Chr( Random( 126 - 32 ) + 32 );
    result := result + ls;
    if (Length(Result)< pSize) and
       (Length(Result) mod 60 = 0) then
      Result := Result + LineEnding;
  end;
end;


procedure tiCreateTextFileOfSize(pFileName: string; pSize: LongInt);
var
  lFileStream : TFileStream ;
  lBuffer   : PChar ;
  lLen      : integer ;
  ls : string ;
begin
  ls := tiCreateStringOfSize( pSize ) ;

  if FileExists( pFileName ) then
    SysUtils.DeleteFile( pFileName ) ;
  lFileStream := TFileStream.Create( pFileName,
                                     fmCreate or fmShareDenyNone );
  try
    lBuffer := PChar( ls );
    lLen := length( ls );
    lFileStream.write( lBuffer^, lLen );
  finally
    lFileStream.Free;
  end;
end;


procedure tiDUnitForceRemoveDir( const pDirectory : string ) ;
begin
  // The easiest way I could think of to delete a directory and all
  // it's child directories and files.
  try
    {$IFDEF MSWINDOWS}
    tiUtils.tiShellExecute( 'cmd.exe',
                            '/X /C "rd ' + pDirectory + ' /S /Q"' ) ;
    Sleep(500);
    // With no sleep, system will return before the shell call has finished.
    // Tried using tiRunEXEAndWait, but kept getting error "System can not find
    // the file specified."
    // Sleep(100) makes the problem go away, so made it Sleep(500) to be sure.
    {$ENDIF MSWINDOWS}
    
    {$IFDEF LINUX}
    tiUtils.tiRunEXEAndWait('rm -f -R ' + pDirectory);
    {$ENDIF LINUX}
  except
    on e:exception do
      raise exception.Create( 'Error in ForceRemoveDirectory(''' +
            pDirectory + '> Message: ' + e.message ) ;
  end;
end ;


procedure tiFillTestingStream(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LWord : word;
  LChar : Char;
begin
  for LCount := 1 to (ASize div 2) do
    begin
    LWord := Random($7FFF);
    AStream.WriteBuffer(LWord, sizeof(Word));
    end;
  if ASize mod 2 = 1 then
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;


procedure tiFillTestingStreamASCII(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LChar : Char;
begin
  for LCount := 1 to ASize do
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

end.
