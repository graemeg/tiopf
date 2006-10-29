{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  Purpose:
    Provide String, Stream and File (via TStrings) encryption & decryption.

  Classes:
    TtiEncryptBlockCipher - Base class for DES and Blow Fish encryption

  Acknowledgements:
    This code was found on the web and refactored to fit the TtiEncryptXxxxxx interface.
    Original header provided below to acknowledge the author, Dave Shapiro.

    Main changes required:
     * Constructor does not take any parameters (Key - default used and can be overridden)
     * Key assumed to be Int64.  (Original was untyped const)  KeyLength functions redundant
     * Block size fixed at SizeOf(Int64) = 8
     * TBlockCipher inherited from TObject, TtiEncryptBlockCipher inherits from TtiEncrypt
     * T64BitBlockCipher merged with TBlockCipher to create TtiEncryptBlockCipher
     * Set src position to 0 and dest size to 0


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{
  Copyright (c) 1998-1999 Dave Shapiro, Professional Software, Inc.
  (daves@cfxc.com) Use and modify freely. Keep this header, please.

                    BlockCiphers class hierarchy:


                             TBlockCipher
                              (abstract)
                                  |
                                  |
                          T64BitBlockCipher
                              (abstract)
                                  |
                --------------------------------------
               |                                      |
             TDESCipher                         TBlowfishCipher


  How it works: TBlockCipher introduces functionality for transforming blocks
  of plaintext into blocks of ciphertext. Specifically, the abstract methods
  EncryptBlock and DecryptBlock are declared for descendants to fill out.
  EncryptStream and DecryptStream are methods completed by TBlockCipher for
  encrypting and decrypting TStreams.

  It's not typesafe: Making this stuff into a single hierarchy requires some
  unsafe typing. First, a block cipher can use any size block. DES and Blowfish
  use 64-bit blocks, but there are many ciphers that use 128-bit blocks. As
  such, EncryptBlock and DecryptBlock take an untyped parameter, and it is left
  to the user of the class to ensure that they're passing the right size block.

  If the user is not sure what the block size is, they should call the virtual
  class method BlockSize.

  There is a similar type-safety problem with the constructor. Some ciphers
  take a 64-bit value as their key. Some take a range in length. At any rate,
  I thought it would be important (or at least cool) to have a virtual
  constructor in this hierarchy. Again, this means sacrificing type safety. In
  the constructor, you pass the key as an untyped const, and the length of
  the key in bytes. The class trusts that the user is sending consistent
  information. TBlockCipher introduces two virtual class methods, MinKeyLength
  and MaxKeyLength for querying key-length information.

  You may be interested in ensuring that this code is correct. There are many
  test vector suites out there. I've tested all ciphers against a lot of stuff;
  it's all correct. If you do choose to verify this stuff on your own, you
  should be aware of endian problems. Intels are little-endian machines, and
  most other stuff is big-endian. Most test suites assume a big-endian
  architecture. At any rate, you can spend all afternoon swapping bytes, trying
  to get things so they agree exactly with others' results. I've done it. It's
  not fun. (Take a peek at the stuff in the '$DEFINE UseTestVectors' part, in
  the Main unit.) The end results are that these ciphers agree with 'official'
  results up to byte-ordering. (Remember that byte-ordering issues occur at
  all points of the encryption, so don't expect to just swap the resulting
  ciphertext bytes and get exactly what the 'official' test vectors say.)
  Chances are, these ciphers aren't compatible across machines, or even with
  other implementations, without some tweaking, which causes a slight
  performance degredation.
}

unit tiEncryptBlockCipher;

{$I tiDefines.inc}

// Turn off writable const
(*$J-*)

interface

uses
   SysUtils
  ,Classes
  ,tiEncrypt
 ;

type
  PDWORD = ^DWORD;
  DWORD = Longword;
  TDoubleDWORD = packed record
    L, R: DWORD;
  end;
  TFourByte = packed record
    B1, B2, B3, B4: Byte;
  end;

  TtiEncryptBlockCipher = class(TtiEncryptAbs)
  private
  protected
  public
    // from original TBlockCipher ...
    class function CiphertextLength(const pPlaintextLength: Longint): Longint; virtual;
    class function BlockSize: Integer;
    procedure EncryptBlock   (var pPlaintext); virtual; abstract;
    procedure DecryptBlock   (var pCiphertext); virtual; abstract;
    function  EncryptedBlock (const pPlaintext: Int64): Int64; virtual; abstract;
    function  DecryptedBlock (const pCiphertext: Int64): Int64; virtual; abstract;
//    function  EncryptedString(const Plaintext: string): string; virtual;
//    function  DecryptedString(const Ciphertext: string): string; virtual;
//    procedure EncryptStream  (const Plaintext, Ciphertext: TStream); virtual;
//    procedure DecryptStream  (const Ciphertext, Plaintext: TStream); virtual;

    // from TtiEncrypt...
    constructor Create; override;
    function    EncryptString(const psData : string): string; override;
    function    DecryptString(const psData : string): string; override;
    procedure   EncryptStream(const pSrc, pDest : TStream); override;
    procedure   DecryptStream(const pSrc, pDest : TStream); override;
  end;

implementation

const
  BlocksPerBuf = 512;

{ TtiEncryptBlockCipher }

class function TtiEncryptBlockCipher.CiphertextLength(const pPlaintextLength: Integer): Longint;
begin
  Result := Succ(pPlaintextLength div BlockSize) * BlockSize;
end;

class function TtiEncryptBlockCipher.BlockSize: Integer;
begin
  Result := SizeOf(Int64);
end;

constructor TtiEncryptBlockCipher.Create;
var
  lKey : Int64;
begin
  inherited;
  with TDoubleDWORD(lKey) do begin
    R := $01234567;
    L := $89abcdef;
  end;
  IntSeed := lKey;
end;

procedure TtiEncryptBlockCipher.DecryptStream(const pSrc, pDest: TStream);
var
  Count: Longint;
  ThisBlockSize, BufSize, I, J: Integer;
  Buf: Pointer;
  P: ^Byte;
begin
  pSrc.Position := 0;
  pDest.Size := 0;
  ThisBlockSize := BlockSize;
  Count := pSrc.Size - pSrc.Position;
  if (Count = 0) or (Count mod ThisBlockSize <> 0) then
  begin
    raise Exception.CreateFmt('Ciphertext length is not a multiple of %d.',
                              [ThisBlockSize]);
  end;
  BufSize := ThisBlockSize * BlocksPerBuf;
  GetMem(Buf, BufSize);
  for I := 1 to Count div BufSize do begin
    pSrc.Read(Buf^, BufSize);
    P := Buf;
    for J := 1 to BlocksPerBuf do begin
      DecryptBlock(P^);
      Inc(P, ThisBlockSize);
    end;
    pDest.Write(Buf^, BufSize);
  end;
  Count := Count mod BufSize;
  pSrc.Read(Buf^, Count);
  P := Buf;
  for J := 1 to Count div ThisBlockSize do begin
    DecryptBlock(P^);
    Inc(P, ThisBlockSize);
  end;
  Dec(P);
  pDest.Write(Buf^, Count - P^);
  FreeMem(Buf);
end;

procedure TtiEncryptBlockCipher.EncryptStream(const pSrc, pDest: TStream);
var
  Count: Longint;
  ThisBlockSize, BufSize, I, N: Integer;
  Buf: Pointer;
  P: ^Byte;
  LastBuf: Boolean;
begin
  pSrc.Position := 0;
  pDest.Size := 0;
  P := nil; // Suppresses superfluous compiler warning.
  Count := 0; // Ditto.
  ThisBlockSize := BlockSize;
  BufSize := ThisBlockSize * BlocksPerBuf;
  GetMem(Buf, BufSize);
  while True do begin
    Count := pSrc.Read(Buf^, BufSize);
    P := Buf;
    LastBuf := Count < BufSize;
    if LastBuf then N := Count div ThisBlockSize else N := BlocksPerBuf;
    for I := 1 to N do begin
      EncryptBlock(P^);
      Inc(P, ThisBlockSize);
    end;
    if LastBuf then Break;
    pDest.Write(Buf^, Count);
  end;
  // We're at the end of the data in a not-completely-full-buffer (or, in the
  // case of Plaintext.Size mod BufSize = 0, at the beginning of an empty
  // buffer, which is just a special case of the former). Now we use the last
  // byte of the current block to give the number of extra padding bytes.
  // This will be a number from 1..ThisBlockSize. Specifically, if the
  // Plaintext length is an exact multiple of ThisBlockSize, the number of
  // extra padding bytes will be ThisBlockSize, i.e. the entire final block
  // is junk. In any other case, the last block has at least some ciphertext.
  Inc(P, ThisBlockSize - 1);
  P^:= Byte(ThisBlockSize - Count mod ThisBlockSize);
  Inc(Count, P^);
  Dec(P, ThisBlockSize - 1);
  EncryptBlock(P^);
  pDest.Write(Buf^, Count);
  FreeMem(Buf);
end;


function TtiEncryptBlockCipher.EncryptString(const psData: string): string;
var
  lPS, lPD: PInt64;
  lSource: Int64;
  I: Integer;
  lNumBlocks: Longint;
  lNumPadBytes: Byte;
begin
  lNumBlocks := Length(psData) div SizeOf(Int64);
  lNumPadBytes := SizeOf(Int64) - Length(psData) mod SizeOf(Int64);
  SetLength(Result, Succ(lNumBlocks) * SizeOf(Int64));
  lPS := Pointer(psData);
  lPD := Pointer(Result);
  for I := 1 to lNumBlocks do begin
    lPD^:= EncryptedBlock(lPS^);
    Inc(lPS);
    Inc(lPD);
  end;
  {
   Fill in the number of padding bytes. Just write the whole block, and then
   overwrite the beginning bytes with Source.
  }
  FillChar(lSource, SizeOf(lSource), lNumPadBytes);
  {
   What if PS points to the end of the string? Won't dereferencing it cause
   a memory problem? Not really. For one, the string will always have a
   trailing null, so there's always one byte, which avoids an AV. Also,
   since PS^ is passed as an untyped var, the compiler will just pass the
   address without dereferencing.
  }
  Move(lPS^, lSource, SizeOf(Int64) - lNumPadBytes);
  lPD^:= EncryptedBlock(lSource);
end;

function TtiEncryptBlockCipher.DecryptString(const psData: string): string;
var
  lDest: Int64;
  lPS, lPD: PInt64;
  I: Integer;
  lNumCiphertextBytes: Longint;
  lNumPadBytes: Byte;
begin
  lNumCiphertextBytes := Length(psData);
  if (lNumCiphertextBytes = 0) or
     (lNumCiphertextBytes mod SizeOf(Int64) <> 0) then
  begin
    raise Exception.CreateFmt('Ciphertext is not a multiple of %d bytes.',
                              [SizeOf(Int64)]);
  end;
  { Decrypt last block first. This tells us how many padding bytes there are. }
  lPS := Pointer(psData);
  Inc(lPS, Pred(lNumCiphertextBytes div SizeOf(Int64)));
  lDest := DecryptedBlock(lPS^);
  lNumPadBytes := TFourByte(TDoubleDWORD(lDest).R).B4;
  SetLength(Result, lNumCiphertextBytes - lNumPadBytes);
  { From the last block, move only the non-padding bytes to the end of Result. }
  Move(lDest, Result[lNumCiphertextBytes - SizeOf(Int64) + 1],
       SizeOf(Int64) - lNumPadBytes);
  lPS := Pointer(psData);
  lPD := Pointer(Result);
  for I := 1 to Length(Result) div SizeOf(Int64) do begin
    lPD^:= DecryptedBlock(lPS^);
    Inc(lPS);
    Inc(lPD);
  end;
end;


end.
