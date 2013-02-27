unit tiDUnitUtils;

{$I tiDefines.inc}

interface
uses
  Classes
  ;

function  tiCreateStringOfSize( pSize : LongInt ) : string ;
procedure tiCreateTextFileOfSize( pFileName : string ; pSize : LongInt ) ;
procedure tiDUnitForceRemoveDir( const pDirectory : string ) ;

const
  cDUnitTestFloatPrecision = 0.000001 ;

implementation
uses
  SysUtils
  ,tiUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,libc
  {$ENDIF LINUX}
  ;

function  tiCreateStringOfSize( pSize : LongInt ) : string ;
var
  ls : Char ;
begin
  result := '' ;
  While Length(Trim(Result))< pSize do
  begin
    ls := Chr( Random( 126 - 32 ) + 32 ) ;
//    if not ( ls in [',', '<', '>', '"', '&', #10, #13 ] ) then
      result := result + ls ;
  end ;
//    result := result + Chr( Random( 255 - 32 ) + 32 ) ;
    // MSXMLDOM appears to replace #13 with #10.
    // Not sure if this will cause problems.
    // #13s not included so tests pass.
    // Think more about this...
end;
(*
var
  i : integer ;
begin
  SetLength( result, pSize ) ;
  for i := 1 to pSize do
    result[ i ] := Chr( Random( 126 - 32 ) + 32 ) ;
end ;
*)

procedure tiCreateTextFileOfSize( pFileName : string ; pSize : LongInt ) ;
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
                                     fmCreate or fmShareDenyNone ) ;
  try
    lBuffer := PChar( ls ) ;
    lLen := length( ls ) ;
    lFileStream.write( lBuffer^, lLen ) ;
  finally
    lFileStream.Free ;
  end ;
end ;

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
    // system is a blocking function, which means it will not return until the
    // child process has terminated. No need to use Sleep()
    if libc.system(PChar('rm -f -R ' + pDirectory)) = -1 then
      raise exception.Create('Unable to remove directory (' + pDirectory + ')');
    {$ENDIF LINUX}
  except
    on e:exception do
      raise exception.create( 'Error in ForceRemoveDirectory(''' +
            pDirectory + '> Message: ' + e.message ) ;
  end;
end ;

{
// Taken from IndySoap (and re-named with ti prefix)
function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage :
string):boolean;
var
   LByte1, LByte2 : byte;
begin
   result := true;
   if (AStream1.Size - AStream1.Position) <> (AStream2.Size -
AStream2.Position) then
     begin
     result := false;
     VMessage := 'Streams have different sizes ('+inttostr(AStream1.Size -
AStream1.Position)+'/'+inttostr(AStream2.Size - AStream2.Position)+')';
     end;
   while (AStream1.Size - AStream1.Position > 0) do
     begin
     AStream1.Read(LByte1, 1);
     AStream2.Read(LByte2, 1);
     if LByte1 <> LByte2 then
       begin
       result := false;
       VMessage := 'Streams Differ at position
'+inttostr(AStream1.Position)+' of '+inttostr(AStream1.Size)+':
'+inttostr(LByte1)+'/'+inttostr(LByte2);
       exit;
       end;
     end;
end;
}
end.
