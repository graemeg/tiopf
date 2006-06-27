{
Product:  Crc32-Maker (Freeware)

Version:  1.0

Author:   Andre Morales-Bahnik
          Schneidemuehler Str. 25
          D-76139 Karlsruhe
          Germany


Files:    Crc32.pas     unit with the CRC32-routines
          Crcunit1.dfm  form-file for the example application
          Crctest.dpr   project-file for the example application
          Crcunit1.pas  main unit for the example application
                   
CIS:      100102,1210

Internet: 100102.1210@compuserve.com
          morales-bahnik@t-online.de
          
           
1. Description

While i was looking for interesting code in CompuServe's Borland
Delphi 32 forum i found a routine for calculating a CRC-32 on a
file written by Pierre A. Damas (CIS 100023,221) in Object Pascal.

I changed his code, so it works now with different buffer sizes.
I also added a 32-bit assembler version of the routine. It works
in the same way as the Pascal version and i found for my surprise,
that both versions work with approximate the same speed.

In addition i have written two routines for calculating a CRC-32 on
MemoryStreams, the one in Pascal and the other using assembler.
While testing this routines, i found the assembler version working
about five times faster as the Pascal version.

You can use this routines in your programs without restrictions be-
cause they are Freeware. To see the routines in action you can com-
pile the example application with Delphi 2. With this application
you can see the influence of buffer size on execution speed and
you can compare the different routines directly.


Enjoy it!

Andre "Carlos" Morales-Bahnik

}

unit tiCRC32;

interface

uses Windows, SysUtils, Classes;

function tiCRC32FromFile(const AFilename: string): Longword;
function tiCRC32FromStream(AStream: TStream): Longword;
function tiCRC32FromStreamFirstNBytes(AStream: TStream; ACountOfBytesToCheck : Longint): Longword;
function tiCRC32FromString( const AString : string ) : Longword;

const
 BufferSize: integer = 65536;

implementation

type
 TBuffer = array[0..1048576] of byte;
 TCrc32Table = array[0..255] of Longword;

const
 Crc32Table: TCrc32Table =
 ($0,$77073096,$EE0E612C,$990951BA,
 $76DC419,$706AF48F,$E963A535,$9E6495A3,
 $EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,
 $9B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
 $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,
 $1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
 $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,
 $14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
 $3B6E20C8,$4C69105E,$D56041E4,$A2677172,
 $3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
 $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,
 $32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
 $26D930AC,$51DE003A,$C8D75180,$BFD06116,
 $21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
 $2802B89E,$5F058808,$C60CD9B2,$B10BE924,
 $2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
 $76DC4190,$1DB7106,$98D220BC,$EFD5102A,
 $71B18589,$6B6B51F,$9FBFE4A5,$E8B8D433,
 $7807C9A2,$F00F934,$9609A88E,$E10E9818,
 $7F6A0DBB,$86D3D2D,$91646C97,$E6635C01,
 $6B6B51F4,$1C6C6162,$856530D8,$F262004E,
 $6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
 $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,
 $62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
 $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,
 $4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
 $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,
 $44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
 $5005713C,$270241AA,$BE0B1010,$C90C2086,
 $5768B525,$206F85B3,$B966D409,$CE61E49F,
 $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,
 $59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
 $EDB88320,$9ABFB3B6,$3B6E20C,$74B1D29A,
 $EAD54739,$9DD277AF,$4DB2615,$73DC1683,
 $E3630B12,$94643B84,$D6D6A3E,$7A6A5AA8,
 $E40ECF0B,$9309FF9D,$A00AE27,$7D079EB1,
 $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,
 $F762575D,$806567CB,$196C3671,$6E6B06E7,
 $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,
 $F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
 $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,
 $D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
 $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,
 $DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
 $CB61B38C,$BC66831A,$256FD2A0,$5268E236,
 $CC0C7795,$BB0B4703,$220216B9,$5505262F,
 $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,
 $C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
 $9B64C2B0,$EC63F226,$756AA39C,$26D930A,
 $9C0906A9,$EB0E363F,$72076785,$5005713,
 $95BF4A82,$E2B87A14,$7BB12BAE,$CB61B38,
 $92D28E9B,$E5D5BE0D,$7CDCEFB7,$BDBDF21,
 $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,
 $81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
 $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,
 $8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
 $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,
 $A7672661,$D06016F7,$4969474D,$3E6E77DB,
 $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,
 $A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
 $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,
 $BAD03605,$CDD70693,$54DE5729,$23D967BF,
 $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,
 $B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);

function UpdateCrc32(Value: integer; var Buffer: array of byte; Count: integer): Longword;
var
 i: integer;
begin
 Result:=Value;
 for i:=0 to Count-1 do
 begin
  Result:=((Result shr 8) and $00FFFFFF) xor Crc32Table[(Result xor Buffer[i]) and $000000FF];
 end;
end;

function AsmUpdateCrc32(Value: integer; Buffer: pointer; Count: integer): integer; assembler;
asm
 {Input = eax: Value, edx: Points to Buffer, ecx: Count}
 {Output= eax: CRC32}
 push ebx
 push edi
 push esi
 mov  esi, edx                    {esi: Points to Buffer}
 mov  edx, eax                    {edx: Result}
 mov  edi, Offset Crc32Table      {edi: Points to Crc32Table}
 xor  eax, eax
 cld
 @@Loop:
 mov  ebx, edx                    {Save Result in ebx}
 shr  edx, 8
 and  edx, 00FFFFFFh
 lodsb                            {Load next Buffer entry}
 xor  ebx, eax
 and  ebx, 000000FFh
 xor  edx, dword ptr [edi+4*ebx]
 dec  ecx                         {Dec Count}
 jnz  @@Loop                      {if Count<>0 goto @@Loop}
 mov  eax, edx                    {Save Result in eax}
 pop  esi
 pop  edi
 pop  ebx
end;

function tiCRC32FromFile(const AFileName: string): Longword;
var
 Buffer: Pointer;
 Handle,Loaded: integer;
begin
 Result:=$FFFFFFFF;
 GetMem(Buffer,BufferSize);
 Handle:=FileOpen(AFileName,fmOpenRead);
 repeat
  Loaded:=FileRead(Handle,Buffer^,BufferSize);
  Result:=UpdateCrc32(Result,TBuffer(Buffer^),Loaded);
 until Loaded<>BufferSize;
 FileClose(Handle);
 FreeMem(Buffer);
 Result:=not Result;
end;

function tiAsmGetFileCrc32(FileName: string): Longword;
var
 Buffer: Pointer;
 Handle,Loaded: integer;
begin
 Result:=$FFFFFFFF;
 GetMem(Buffer,BufferSize);
 Handle:=FileOpen(FileName,fmOpenRead);
 repeat
  Loaded:=FileRead(Handle,Buffer^,BufferSize);
  Result:=AsmUpdateCrc32(Result,Buffer,Loaded);
 until Loaded<>BufferSize;
 FileClose(Handle);
 FreeMem(Buffer);
 Result:=not Result;
end;

function tiCRC32FromStream(AStream: TStream): Longword;
var
  Value: byte;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Result:=$FFFFFFFF;
  while (AStream.Read(Value,1)=1) do
    Result:=((Result shr 8) and $00FFFFFF) xor Crc32Table[(Result xor Value) and $000000FF];
  Result:=not Result;
end;

function tiCRC32FromStreamFirstNBytes(AStream: TStream; ACountOfBytesToCheck : Longint): Longword;
var
  Value: byte;
  LPos: Longint;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(ACountOfBytesToCheck > 0, 'ACountOfBytesToCheck <= 0');
  Result:=$FFFFFFFF;
  LPos:= 1;
  while (AStream.Read(Value,1)=1) and
        (LPos <= ACountOfBytesToCheck) do
  begin
    Result:=((Result shr 8) and $00FFFFFF) xor Crc32Table[(Result xor Value) and $000000FF];
    Inc(LPos);
  end;
  Result:=not Result;
end;

function tiAsmGetMemoryStreamCrc32(Stream: TMemoryStream): Longword;
begin
 Result:=$FFFFFFFF;
 Result:=AsmUpdateCrc32(Result,Stream.Memory,Stream.Size);
 Result:=not Result;
end;

function tiCRC32FromString( const AString : string ) : Longword;
var
  lStringStream : TStringStream ;
begin
  lStringStream := TStringStream.Create(AString);
  try
    lStringStream.Position := 0 ;
    result := tiCRC32FromStream( lStringStream ) ;
  finally
    lStringStream.Free ;
  end ;
end ;

end.
