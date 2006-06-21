unit tiEncryptSimple;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiEncrypt
  ;

const
  cgsEncryptionSimple = 'EncryptionSimple' ;

type

  TEncryptSimple = class( TtiEncryptAbs )
  private
    function  SimpleEncrypt(const Source: String): String;
  protected
    procedure SetSeed   (const Value: string); override ;
  public
    constructor Create ; override ;
    function    EncryptString( const psData : string ) : string ; override ;
    function    DecryptString( const psData : string ) : string ; override ;
    procedure   EncryptStream( const pSrc, pDest : TStream ) ; override ;
    procedure   DecryptStream( const pSrc, pDest : TStream ) ; override ;
  end ;


const
  cDefaultEncryptSeedString : String = '12%6348i(oikruK**9oi57&^1`!@bd)';


implementation
uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TEncryptSimple
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TEncryptSimple.Create;
begin
  inherited;
  Randomize ;
  Seed := cDefaultEncryptSeedString ;
end;


function TEncryptSimple.SimpleEncrypt(const Source: String): String;
var
  Index: Integer;
begin
  SetLength(Result, Length(Source));
  for Index := 1 to Length(Source) do
    Result[Index] := Chr((Ord(Seed[Index mod Length(Seed)]) xor Ord(Source[Index])));
end;


function TEncryptSimple.EncryptString( const psData : string ) : string ;
var
  OrdValue: Byte;
  Index: Integer;
  BitCount: Integer;
  BitValue: Byte;
  ByteValue: Byte;
  Source: String;
begin
  ByteValue := Random(255) + 1;
  Source := SimpleEncrypt(psData);
  SetLength(Result, Length(Source) * 8);
  for Index := 1 to Length(Source) do
  begin
    OrdValue := Ord(Source[Index]);
    for BitCount := 0 to 7 do
    begin
      BitValue := Byte(OrdValue and (1 shl BitCount) = 1 shl BitCount);
      RandSeed := ByteValue;
      ByteValue := (((Random(255) + 1) div 2) * 2) + BitValue;
      Result[(Index - 1) * 8 + BitCount + 1] := Chr(ByteValue);
    end;
  end;

  {null values so string length is not always divisible by 8.. a dead giveaway}
  Randomize;
  for Index := 1 to Random(7) + 1 do
  begin
    Sleep(1);
    Randomize;
    Result := Result + Chr(Random(256));
  end;
end ;


function TEncryptSimple.DecryptString( const psData : string ) : string ;
var
  ListText: String;
  EncryptedOrd: String;
  OrdValue: Integer;
  Index: Integer;
  BitCount: Integer;
begin
  Index := 1;
  ListText := '';
  while Index < Length(psData) do
  begin
    EncryptedOrd := Copy(psData, Index, 8);
    if Length(EncryptedOrd) < 8 then
      Break;
    OrdValue := 0;
    for BitCount := 0 to 7 do
    begin
      if not(Odd(Ord(EncryptedOrd[BitCount + 1]))) then
        Continue
      else
        OrdValue := OrdValue or (1 shl BitCount);
    end;
    ListText := ListText + Chr(OrdValue);
    Inc(Index, 8);
  end;

  result := SimpleEncrypt(ListText);
end ;


procedure TEncryptSimple.EncryptStream( const pSrc, pDest : TStream ) ;
var
  ls : String ;
begin
  pSrc.Seek(0, soFromBeginning);
  SetLength(ls, pSrc.Size);
  pSrc.ReadBuffer(ls[1], pSrc.Size);
  ls := EncryptString(ls);
  pDest.WriteBuffer(Pointer(ls)^, Length(ls));
end ;


procedure TEncryptSimple.DecryptStream( const pSrc, pDest : TStream ) ;
var
  ls : String ;
begin
  pSrc.Seek(0, soFromBeginning);
  SetLength(ls, pSrc.Size);
  pSrc.ReadBuffer(ls[1], pSrc.Size);
  ls := DecryptString(ls);
  pDest.WriteBuffer(Pointer(ls)^, Length(ls));
end ;


procedure TEncryptSimple.SetSeed(const Value: string);
begin
  inherited;
  if SameText( Seed, '' ) then // zero length not valid
    Seed := cDefaultEncryptSeedString ;
end;


initialization
  // Register the TtiEncrypt with the EncryptFactory
  gEncryptFactory.RegisterClass( cgsEncryptionSimple, TEncryptSimple ) ;
  gtiEncryptClass := TEncryptSimple ;

end.
