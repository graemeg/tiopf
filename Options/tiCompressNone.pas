unit tiCompressNone;

{$I tiDefines.inc}

interface
uses
  tiCompress
  ,Classes
 ;


type
  // Null object pattern - implement the TtiCompress, but with no compression
  TtiCompressNone = class(TtiCompressAbs)
  private
    procedure CopyFile(const AFrom, ATo : string);
  public
    function  CompressStream(  AFrom : TStream; ATo : TStream): Extended; override;
    procedure DecompressStream(AFrom : TStream; ATo : TStream); override;
    function  CompressBuffer(  const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer): Extended; override;
    procedure DecompressBuffer(const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer); override;
    function  CompressString(  const AFrom : string; var ATo : string)  : Extended; override;
    function  CompressString(  const AFrom : AnsiString; var ATo : AnsiString)  : Extended; override;
    procedure DecompressString(const AFrom : string; var ATo : string)  ; override;
    procedure DecompressString(const AFrom : AnsiString; var ATo : AnsiString)  ; override;
    function  CompressFile(    const AFrom : string; const ATo : string): Extended; override;
    procedure DecompressFile(  const AFrom : string; const ATo : string); override;
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
function TtiCompressNone.CompressBuffer(const AFrom: Pointer;
  const AFromSize: Integer; out ATo: Pointer; out AToSize: Integer): Extended;
begin
  Assert(AFrom = AFrom);            // Getting rid of compiler hints, unused params
  Assert(AFromSize = AFromSize);    // "
  Assert(ATo = ATo);                // "
  Assert(AToSize = AToSize);        // "
  Assert(false, 'Not implemented yet.');
  result := 0;
end;


// Compress a file
function TtiCompressNone.CompressFile(const AFrom, ATo: string): Extended;
begin
  CopyFile(AFrom, ATo);
  result := 1;
end;


// Compress a TStream
function TtiCompressNone.CompressStream(AFrom, ATo: TStream): Extended;
begin
  AFrom.Seek(0, soFromBeginning);
  ATo.CopyFrom(AFrom, AFrom.Size);
  AFrom.Seek(0, soFromBeginning);
  ATo.Seek(0, soFromBeginning);
  result := 100;
end;


// Compress a string
function TtiCompressNone.CompressString(const AFrom: string;
  var ATo: string): Extended;
begin
  ATo := AFrom;
  result := 100;
end;


// Compress a string
function TtiCompressNone.CompressString(const AFrom: AnsiString;
  var ATo: AnsiString): Extended;
begin
  ATo := AFrom;
  result := 100;
end;


// Copy a file from one place to another
procedure TtiCompressNone.CopyFile(const AFrom, ATo: string);
var
  liErrorCode : word;
begin
  {$IFDEF MSWINDOWS}
  if FileExists(ATo) then
    Windows.DeleteFile(PChar(ATo));
  Windows.CopyFile(pChar(AFrom), pChar(ATo), true);
  liErrorCode := getLastError();
  {$ENDIF}
  {$IFDEF UNIX}
  if FileExists(ATo) then
    SysUtils.DeleteFile(ATo);
  tiCopyFile(AFrom, ATo);
  liErrorCode := 0; // No error checking yet!
  {$ENDIF}
  if liErrorCode <> 0 then begin
    raise exception.Create('Unable to copy <' +
                            AFrom +
                            '> to <' +
                            ATo + '>' + Cr +
                            'Error code: ' +
                            intToStr(liErrorCode) + Cr +
                            'Error message: ' +
                            SysErrorMessage(liErrorCode));
  end;
end;


// Decompress a buffer
procedure TtiCompressNone.DecompressBuffer(const AFrom: Pointer;
  const AFromSize: Integer; out ATo: Pointer; out AToSize: Integer);
begin
  Assert(AFrom = AFrom);            // Getting rid of compiler hints, unused params
  Assert(AFromSize = AFromSize);    //  "
  Assert(ATo = ATo);                //  "
  Assert(AToSize = AToSize);        //  "
  Assert(false, 'Not implemented yet.');
end;


// Decompress a file
procedure TtiCompressNone.DecompressFile(const AFrom, ATo: string);
begin
  CopyFile(AFrom, ATo);
end;


// Decompress a TStream
procedure TtiCompressNone.DecompressStream(AFrom, ATo: TStream);
begin
  AFrom.Seek(0, soFromBeginning);
  ATo.CopyFrom(AFrom, AFrom.Size);
  AFrom.Seek(0, soFromBeginning);
  ATo.Seek(0, soFromBeginning);
end;


// Decompress a string
procedure TtiCompressNone.DecompressString(const AFrom: string; var ATo: string);
begin
  ATo := AFrom;
end;


// Decompress a string
procedure TtiCompressNone.DecompressString(const AFrom: AnsiString;
  var ATo: AnsiString);
begin
  ATo := AFrom;
end;


initialization
  // Register the TtiCompress with the CompressFactory
  gCompressFactory.RegisterClass(cgsCompressNone, TtiCompressNone);
  gtiCompressClass := TtiCompressNone;

end.

