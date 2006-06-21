{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

  Purpose:
    Provide String, Stream and File (via TStrings) encryption & decryption.

  Classes:
    TEncryptSimple - Simple encryption

  Revision History:
    ???,  ????, Scott Maskiel, Created

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

// Turn off writable const
(*$J-*)

unit tiEncryptDES;

interface
uses
  tiEncryptBlockCipher
 ;


const
  cgsEncryptionDES = 'EncryptionDES' ;


type

  TtiDESKeyScheduleRange = 0..15;
  TtiSixBitArray = array [0..7] of DWORD;
  TtiDESKeySchedule = array [TtiDESKeyScheduleRange] of TtiSixBitArray;

  TEncryptDES = class(TtiEncryptBlockCipher)
  private
    FKeySchedule: TtiDESKeySchedule;
  protected
    procedure SetIntSeed(const Value: Int64 ); override ;
  public
    procedure EncryptBlock(var pPlaintext); override;
    procedure DecryptBlock(var pCiphertext); override;
    function EncryptedBlock(const pPlaintext: Int64): Int64; override;
    function DecryptedBlock(const pCiphertext: Int64): Int64; override;
  end;


implementation
uses
  tiEncrypt
 ;

type
  TInitialPermutation = array [Byte] of DWORD;
const
  IP: TInitialPermutation = (
   $00000000, $00000080, $00000008, $00000088, $00008000, $00008080, $00008008, $00008088,
   $00000800, $00000880, $00000808, $00000888, $00008800, $00008880, $00008808, $00008888,
   $00800000, $00800080, $00800008, $00800088, $00808000, $00808080, $00808008, $00808088,
   $00800800, $00800880, $00800808, $00800888, $00808800, $00808880, $00808808, $00808888,
   $00080000, $00080080, $00080008, $00080088, $00088000, $00088080, $00088008, $00088088,
   $00080800, $00080880, $00080808, $00080888, $00088800, $00088880, $00088808, $00088888,
   $00880000, $00880080, $00880008, $00880088, $00888000, $00888080, $00888008, $00888088,
   $00880800, $00880880, $00880808, $00880888, $00888800, $00888880, $00888808, $00888888,
   $80000000, $80000080, $80000008, $80000088, $80008000, $80008080, $80008008, $80008088,
   $80000800, $80000880, $80000808, $80000888, $80008800, $80008880, $80008808, $80008888,
   $80800000, $80800080, $80800008, $80800088, $80808000, $80808080, $80808008, $80808088,
   $80800800, $80800880, $80800808, $80800888, $80808800, $80808880, $80808808, $80808888,
   $80080000, $80080080, $80080008, $80080088, $80088000, $80088080, $80088008, $80088088,
   $80080800, $80080880, $80080808, $80080888, $80088800, $80088880, $80088808, $80088888,
   $80880000, $80880080, $80880008, $80880088, $80888000, $80888080, $80888008, $80888088,
   $80880800, $80880880, $80880808, $80880888, $80888800, $80888880, $80888808, $80888888,
   $08000000, $08000080, $08000008, $08000088, $08008000, $08008080, $08008008, $08008088,
   $08000800, $08000880, $08000808, $08000888, $08008800, $08008880, $08008808, $08008888,
   $08800000, $08800080, $08800008, $08800088, $08808000, $08808080, $08808008, $08808088,
   $08800800, $08800880, $08800808, $08800888, $08808800, $08808880, $08808808, $08808888,
   $08080000, $08080080, $08080008, $08080088, $08088000, $08088080, $08088008, $08088088,
   $08080800, $08080880, $08080808, $08080888, $08088800, $08088880, $08088808, $08088888,
   $08880000, $08880080, $08880008, $08880088, $08888000, $08888080, $08888008, $08888088,
   $08880800, $08880880, $08880808, $08880888, $08888800, $08888880, $08888808, $08888888,
   $88000000, $88000080, $88000008, $88000088, $88008000, $88008080, $88008008, $88008088,
   $88000800, $88000880, $88000808, $88000888, $88008800, $88008880, $88008808, $88008888,
   $88800000, $88800080, $88800008, $88800088, $88808000, $88808080, $88808008, $88808088,
   $88800800, $88800880, $88800808, $88800888, $88808800, $88808880, $88808808, $88808888,
   $88080000, $88080080, $88080008, $88080088, $88088000, $88088080, $88088008, $88088088,
   $88080800, $88080880, $88080808, $88080888, $88088800, $88088880, $88088808, $88088888,
   $88880000, $88880080, $88880008, $88880088, $88888000, $88888080, $88888008, $88888088,
   $88880800, $88880880, $88880808, $88880888, $88888800, $88888880, $88888808, $88888888
  );

  IPInv: TInitialPermutation = (
   $00000000, $02000000, $00020000, $02020000, $00000200, $02000200, $00020200, $02020200,
   $00000002, $02000002, $00020002, $02020002, $00000202, $02000202, $00020202, $02020202,
   $01000000, $03000000, $01020000, $03020000, $01000200, $03000200, $01020200, $03020200,
   $01000002, $03000002, $01020002, $03020002, $01000202, $03000202, $01020202, $03020202,
   $00010000, $02010000, $00030000, $02030000, $00010200, $02010200, $00030200, $02030200,
   $00010002, $02010002, $00030002, $02030002, $00010202, $02010202, $00030202, $02030202,
   $01010000, $03010000, $01030000, $03030000, $01010200, $03010200, $01030200, $03030200,
   $01010002, $03010002, $01030002, $03030002, $01010202, $03010202, $01030202, $03030202,
   $00000100, $02000100, $00020100, $02020100, $00000300, $02000300, $00020300, $02020300,
   $00000102, $02000102, $00020102, $02020102, $00000302, $02000302, $00020302, $02020302,
   $01000100, $03000100, $01020100, $03020100, $01000300, $03000300, $01020300, $03020300,
   $01000102, $03000102, $01020102, $03020102, $01000302, $03000302, $01020302, $03020302,
   $00010100, $02010100, $00030100, $02030100, $00010300, $02010300, $00030300, $02030300,
   $00010102, $02010102, $00030102, $02030102, $00010302, $02010302, $00030302, $02030302,
   $01010100, $03010100, $01030100, $03030100, $01010300, $03010300, $01030300, $03030300,
   $01010102, $03010102, $01030102, $03030102, $01010302, $03010302, $01030302, $03030302,
   $00000001, $02000001, $00020001, $02020001, $00000201, $02000201, $00020201, $02020201,
   $00000003, $02000003, $00020003, $02020003, $00000203, $02000203, $00020203, $02020203,
   $01000001, $03000001, $01020001, $03020001, $01000201, $03000201, $01020201, $03020201,
   $01000003, $03000003, $01020003, $03020003, $01000203, $03000203, $01020203, $03020203,
   $00010001, $02010001, $00030001, $02030001, $00010201, $02010201, $00030201, $02030201,
   $00010003, $02010003, $00030003, $02030003, $00010203, $02010203, $00030203, $02030203,
   $01010001, $03010001, $01030001, $03030001, $01010201, $03010201, $01030201, $03030201,
   $01010003, $03010003, $01030003, $03030003, $01010203, $03010203, $01030203, $03030203,
   $00000101, $02000101, $00020101, $02020101, $00000301, $02000301, $00020301, $02020301,
   $00000103, $02000103, $00020103, $02020103, $00000303, $02000303, $00020303, $02020303,
   $01000101, $03000101, $01020101, $03020101, $01000301, $03000301, $01020301, $03020301,
   $01000103, $03000103, $01020103, $03020103, $01000303, $03000303, $01020303, $03020303,
   $00010101, $02010101, $00030101, $02030101, $00010301, $02010301, $00030301, $02030301,
   $00010103, $02010103, $00030103, $02030103, $00010303, $02010303, $00030303, $02030303,
   $01010101, $03010101, $01030101, $03030101, $01010301, $03010301, $01030301, $03030301,
   $01010103, $03010103, $01030103, $03030103, $01010303, $03010303, $01030303, $03030303
  );

const
  LowSixBits = $3f;
  LowTwoBits = $03;

function CircularSHL28(const X: DWORD; const Amount: Byte): DWORD;
{
  Pre: Amount < BitsInX.
  Post: Result is an unsigned circular left shift of X by Amount bytes.
}
const
  BitLength = 28;
  { The high nibble needs to be cleared. }
  Mask = not (Pred(1 shl (SizeOf(X) - BitLength)) shl BitLength);
begin
  Result := X shl Amount and Mask or X shr (BitLength - Amount);
end;

type
  TPBox = array [Byte] of DWORD;

const
  PBox1: TPBox = (
    $00000000, $00004000, $40000000, $40004000, $00000010, $00004010,
    $40000010, $40004010, $00080000, $00084000, $40080000, $40084000,
    $00080010, $00084010, $40080010, $40084010, $00000002, $00004002,
    $40000002, $40004002, $00000012, $00004012, $40000012, $40004012,
    $00080002, $00084002, $40080002, $40084002, $00080012, $00084012,
    $40080012, $40084012, $00000200, $00004200, $40000200, $40004200,
    $00000210, $00004210, $40000210, $40004210, $00080200, $00084200,
    $40080200, $40084200, $00080210, $00084210, $40080210, $40084210,
    $00000202, $00004202, $40000202, $40004202, $00000212, $00004212,
    $40000212, $40004212, $00080202, $00084202, $40080202, $40084202,
    $00080212, $00084212, $40080212, $40084212, $00008000, $0000C000,
    $40008000, $4000C000, $00008010, $0000C010, $40008010, $4000C010,
    $00088000, $0008C000, $40088000, $4008C000, $00088010, $0008C010,
    $40088010, $4008C010, $00008002, $0000C002, $40008002, $4000C002,
    $00008012, $0000C012, $40008012, $4000C012, $00088002, $0008C002,
    $40088002, $4008C002, $00088012, $0008C012, $40088012, $4008C012,
    $00008200, $0000C200, $40008200, $4000C200, $00008210, $0000C210,
    $40008210, $4000C210, $00088200, $0008C200, $40088200, $4008C200,
    $00088210, $0008C210, $40088210, $4008C210, $00008202, $0000C202,
    $40008202, $4000C202, $00008212, $0000C212, $40008212, $4000C212,
    $00088202, $0008C202, $40088202, $4008C202, $00088212, $0008C212,
    $40088212, $4008C212, $00800000, $00804000, $40800000, $40804000,
    $00800010, $00804010, $40800010, $40804010, $00880000, $00884000,
    $40880000, $40884000, $00880010, $00884010, $40880010, $40884010,
    $00800002, $00804002, $40800002, $40804002, $00800012, $00804012,
    $40800012, $40804012, $00880002, $00884002, $40880002, $40884002,
    $00880012, $00884012, $40880012, $40884012, $00800200, $00804200,
    $40800200, $40804200, $00800210, $00804210, $40800210, $40804210,
    $00880200, $00884200, $40880200, $40884200, $00880210, $00884210,
    $40880210, $40884210, $00800202, $00804202, $40800202, $40804202,
    $00800212, $00804212, $40800212, $40804212, $00880202, $00884202,
    $40880202, $40884202, $00880212, $00884212, $40880212, $40884212,
    $00808000, $0080C000, $40808000, $4080C000, $00808010, $0080C010,
    $40808010, $4080C010, $00888000, $0088C000, $40888000, $4088C000,
    $00888010, $0088C010, $40888010, $4088C010, $00808002, $0080C002,
    $40808002, $4080C002, $00808012, $0080C012, $40808012, $4080C012,
    $00888002, $0088C002, $40888002, $4088C002, $00888012, $0088C012,
    $40888012, $4088C012, $00808200, $0080C200, $40808200, $4080C200,
    $00808210, $0080C210, $40808210, $4080C210, $00888200, $0088C200,
    $40888200, $4088C200, $00888210, $0088C210, $40888210, $4088C210,
    $00808202, $0080C202, $40808202, $4080C202, $00808212, $0080C212,
    $40808212, $4080C212, $00888202, $0088C202, $40888202, $4088C202,
    $00888212, $0088C212, $40888212, $4088C212
  );

  PBox2: TPBox = (
   $00000000, $80000000, $00400000, $80400000, $00001000, $80001000, $00401000, $80401000,
   $00000040, $80000040, $00400040, $80400040, $00001040, $80001040, $00401040, $80401040,
   $04000000, $84000000, $04400000, $84400000, $04001000, $84001000, $04401000, $84401000,
   $04000040, $84000040, $04400040, $84400040, $04001040, $84001040, $04401040, $84401040,
   $00000004, $80000004, $00400004, $80400004, $00001004, $80001004, $00401004, $80401004,
   $00000044, $80000044, $00400044, $80400044, $00001044, $80001044, $00401044, $80401044,
   $04000004, $84000004, $04400004, $84400004, $04001004, $84001004, $04401004, $84401004,
   $04000044, $84000044, $04400044, $84400044, $04001044, $84001044, $04401044, $84401044,
   $00010000, $80010000, $00410000, $80410000, $00011000, $80011000, $00411000, $80411000,
   $00010040, $80010040, $00410040, $80410040, $00011040, $80011040, $00411040, $80411040,
   $04010000, $84010000, $04410000, $84410000, $04011000, $84011000, $04411000, $84411000,
   $04010040, $84010040, $04410040, $84410040, $04011040, $84011040, $04411040, $84411040,
   $00010004, $80010004, $00410004, $80410004, $00011004, $80011004, $00411004, $80411004,
   $00010044, $80010044, $00410044, $80410044, $00011044, $80011044, $00411044, $80411044,
   $04010004, $84010004, $04410004, $84410004, $04011004, $84011004, $04411004, $84411004,
   $04010044, $84010044, $04410044, $84410044, $04011044, $84011044, $04411044, $84411044,
   $00000100, $80000100, $00400100, $80400100, $00001100, $80001100, $00401100, $80401100,
   $00000140, $80000140, $00400140, $80400140, $00001140, $80001140, $00401140, $80401140,
   $04000100, $84000100, $04400100, $84400100, $04001100, $84001100, $04401100, $84401100,
   $04000140, $84000140, $04400140, $84400140, $04001140, $84001140, $04401140, $84401140,
   $00000104, $80000104, $00400104, $80400104, $00001104, $80001104, $00401104, $80401104,
   $00000144, $80000144, $00400144, $80400144, $00001144, $80001144, $00401144, $80401144,
   $04000104, $84000104, $04400104, $84400104, $04001104, $84001104, $04401104, $84401104,
   $04000144, $84000144, $04400144, $84400144, $04001144, $84001144, $04401144, $84401144,
   $00010100, $80010100, $00410100, $80410100, $00011100, $80011100, $00411100, $80411100,
   $00010140, $80010140, $00410140, $80410140, $00011140, $80011140, $00411140, $80411140,
   $04010100, $84010100, $04410100, $84410100, $04011100, $84011100, $04411100, $84411100,
   $04010140, $84010140, $04410140, $84410140, $04011140, $84011140, $04411140, $84411140,
   $00010104, $80010104, $00410104, $80410104, $00011104, $80011104, $00411104, $80411104,
   $00010144, $80010144, $00410144, $80410144, $00011144, $80011144, $00411144, $80411144,
   $04010104, $84010104, $04410104, $84410104, $04011104, $84011104, $04411104, $84411104,
   $04010144, $84010144, $04410144, $84410144, $04011144, $84011144, $04411144, $84411144
  );

  PBox3: TPBox = (
   $00000000, $00002000, $00200000, $00202000, $00000008, $00002008, $00200008, $00202008,
   $10000000, $10002000, $10200000, $10202000, $10000008, $10002008, $10200008, $10202008,
   $20000000, $20002000, $20200000, $20202000, $20000008, $20002008, $20200008, $20202008,
   $30000000, $30002000, $30200000, $30202000, $30000008, $30002008, $30200008, $30202008,
   $00000080, $00002080, $00200080, $00202080, $00000088, $00002088, $00200088, $00202088,
   $10000080, $10002080, $10200080, $10202080, $10000088, $10002088, $10200088, $10202088,
   $20000080, $20002080, $20200080, $20202080, $20000088, $20002088, $20200088, $20202088,
   $30000080, $30002080, $30200080, $30202080, $30000088, $30002088, $30200088, $30202088,
   $00040000, $00042000, $00240000, $00242000, $00040008, $00042008, $00240008, $00242008,
   $10040000, $10042000, $10240000, $10242000, $10040008, $10042008, $10240008, $10242008,
   $20040000, $20042000, $20240000, $20242000, $20040008, $20042008, $20240008, $20242008,
   $30040000, $30042000, $30240000, $30242000, $30040008, $30042008, $30240008, $30242008,
   $00040080, $00042080, $00240080, $00242080, $00040088, $00042088, $00240088, $00242088,
   $10040080, $10042080, $10240080, $10242080, $10040088, $10042088, $10240088, $10242088,
   $20040080, $20042080, $20240080, $20242080, $20040088, $20042088, $20240088, $20242088,
   $30040080, $30042080, $30240080, $30242080, $30040088, $30042088, $30240088, $30242088,
   $01000000, $01002000, $01200000, $01202000, $01000008, $01002008, $01200008, $01202008,
   $11000000, $11002000, $11200000, $11202000, $11000008, $11002008, $11200008, $11202008,
   $21000000, $21002000, $21200000, $21202000, $21000008, $21002008, $21200008, $21202008,
   $31000000, $31002000, $31200000, $31202000, $31000008, $31002008, $31200008, $31202008,
   $01000080, $01002080, $01200080, $01202080, $01000088, $01002088, $01200088, $01202088,
   $11000080, $11002080, $11200080, $11202080, $11000088, $11002088, $11200088, $11202088,
   $21000080, $21002080, $21200080, $21202080, $21000088, $21002088, $21200088, $21202088,
   $31000080, $31002080, $31200080, $31202080, $31000088, $31002088, $31200088, $31202088,
   $01040000, $01042000, $01240000, $01242000, $01040008, $01042008, $01240008, $01242008,
   $11040000, $11042000, $11240000, $11242000, $11040008, $11042008, $11240008, $11242008,
   $21040000, $21042000, $21240000, $21242000, $21040008, $21042008, $21240008, $21242008,
   $31040000, $31042000, $31240000, $31242000, $31040008, $31042008, $31240008, $31242008,
   $01040080, $01042080, $01240080, $01242080, $01040088, $01042088, $01240088, $01242088,
   $11040080, $11042080, $11240080, $11242080, $11040088, $11042088, $11240088, $11242088,
   $21040080, $21042080, $21240080, $21242080, $21040088, $21042088, $21240088, $21242088,
   $31040080, $31042080, $31240080, $31242080, $31040088, $31042088, $31240088, $31242088
  );

  PBox4: TPBox = (
   $00000000, $00000800, $00020000, $00020800, $00000020, $00000820, $00020020, $00020820,
   $08000000, $08000800, $08020000, $08020800, $08000020, $08000820, $08020020, $08020820,
   $02000000, $02000800, $02020000, $02020800, $02000020, $02000820, $02020020, $02020820,
   $0A000000, $0A000800, $0A020000, $0A020800, $0A000020, $0A000820, $0A020020, $0A020820,
   $00000400, $00000C00, $00020400, $00020C00, $00000420, $00000C20, $00020420, $00020C20,
   $08000400, $08000C00, $08020400, $08020C00, $08000420, $08000C20, $08020420, $08020C20,
   $02000400, $02000C00, $02020400, $02020C00, $02000420, $02000C20, $02020420, $02020C20,
   $0A000400, $0A000C00, $0A020400, $0A020C00, $0A000420, $0A000C20, $0A020420, $0A020C20,
   $00100000, $00100800, $00120000, $00120800, $00100020, $00100820, $00120020, $00120820,
   $08100000, $08100800, $08120000, $08120800, $08100020, $08100820, $08120020, $08120820,
   $02100000, $02100800, $02120000, $02120800, $02100020, $02100820, $02120020, $02120820,
   $0A100000, $0A100800, $0A120000, $0A120800, $0A100020, $0A100820, $0A120020, $0A120820,
   $00100400, $00100C00, $00120400, $00120C00, $00100420, $00100C20, $00120420, $00120C20,
   $08100400, $08100C00, $08120400, $08120C00, $08100420, $08100C20, $08120420, $08120C20,
   $02100400, $02100C00, $02120400, $02120C00, $02100420, $02100C20, $02120420, $02120C20,
   $0A100400, $0A100C00, $0A120400, $0A120C00, $0A100420, $0A100C20, $0A120420, $0A120C20,
   $00000001, $00000801, $00020001, $00020801, $00000021, $00000821, $00020021, $00020821,
   $08000001, $08000801, $08020001, $08020801, $08000021, $08000821, $08020021, $08020821,
   $02000001, $02000801, $02020001, $02020801, $02000021, $02000821, $02020021, $02020821,
   $0A000001, $0A000801, $0A020001, $0A020801, $0A000021, $0A000821, $0A020021, $0A020821,
   $00000401, $00000C01, $00020401, $00020C01, $00000421, $00000C21, $00020421, $00020C21,
   $08000401, $08000C01, $08020401, $08020C01, $08000421, $08000C21, $08020421, $08020C21,
   $02000401, $02000C01, $02020401, $02020C01, $02000421, $02000C21, $02020421, $02020C21,
   $0A000401, $0A000C01, $0A020401, $0A020C01, $0A000421, $0A000C21, $0A020421, $0A020C21,
   $00100001, $00100801, $00120001, $00120801, $00100021, $00100821, $00120021, $00120821,
   $08100001, $08100801, $08120001, $08120801, $08100021, $08100821, $08120021, $08120821,
   $02100001, $02100801, $02120001, $02120801, $02100021, $02100821, $02120021, $02120821,
   $0A100001, $0A100801, $0A120001, $0A120801, $0A100021, $0A100821, $0A120021, $0A120821,
   $00100401, $00100C01, $00120401, $00120C01, $00100421, $00100C21, $00120421, $00120C21,
   $08100401, $08100C01, $08120401, $08120C01, $08100421, $08100C21, $08120421, $08120C21,
   $02100401, $02100C01, $02120401, $02120C01, $02100421, $02100C21, $02120421, $02120C21,
   $0A100401, $0A100C01, $0A120401, $0A120C01, $0A100421, $0A100C21, $0A120421, $0A120C21
  );
  
type
  TSBox = array [0..63] of Integer;

const
  SBox1: TSBox = (
    224,   0,  64, 240, 208, 112,  16,  64,  32, 224, 240,  32, 176, 208, 128,  16,
     48, 160, 160,  96,  96, 192, 192, 176,  80, 144, 144,  80,   0,  48, 112, 128,
     64, 240,  16, 192, 224, 128, 128,  32, 208,  64,  96, 144,  32,  16, 176, 112,
    240,  80, 192, 176, 144,  48, 112, 224,  48, 160, 160,   0,  80,  96,   0, 208
  );

  SBox2: TSBox = (
    15,  3,  1, 13,  8,  4, 14,  7,  6, 15, 11,  2,  3,  8,  4, 14,
     9, 12,  7,  0,  2,  1, 13, 10, 12,  6,  0,  9,  5, 11, 10,  5,
     0, 13, 14,  8,  7, 10, 11,  1, 10,  3,  4, 15, 13,  4,  1,  2,
     5, 11,  8,  6, 12,  7,  6, 12,  9,  0,  3,  5,  2, 14, 15,  9
  );

  SBox3: TSBox = (
    160, 208,   0, 112, 144,   0, 224, 144,  96,  48,  48,  64,  240,  96,  80, 160,
     16,  32, 208, 128, 192,  80, 112, 224, 176, 192,  64, 176,   32, 240, 128,  16,
    208,  16,  96, 160,  64, 208, 144,   0, 128,  96, 240, 144,   48, 128,   0, 112,
    176,  64,  16, 240,  32, 224, 192,  48,  80, 176, 160,  80, 224,   32, 112, 192
  );

  SBox4: TSBox = (
     7, 13, 13,  8, 14, 11,  3,  5,  0,  6,  6, 15,  9,  0, 10,  3,
     1,  4,  2,  7,  8,  2,  5, 12, 11,  1, 12, 10,  4, 14, 15,  9,
    10,  3,  6, 15,  9,  0,  0,  6, 12, 10, 11,  1,  7, 13, 13,  8,
    15,  9,  1,  4,  3,  5, 14, 11,  5, 12,  2,  7,  8,  2,  4, 14
  );

  SBox5: TSBox = (
     32, 224, 192, 176,  64,  32,  16, 192, 112,  64, 160, 112, 176, 208,  96,  16,
    128,  80,  80,   0,  48, 240, 240, 160, 208,  48,   0, 144, 224, 128, 144,  96,
     64, 176,  32, 128,  16, 192, 176, 112, 160,  16, 208, 224, 112,  32, 128, 208,
    240,  96, 144, 240, 192,   0,  80, 144,  96, 160,  48,  64,   0,  80, 224,  48
  );

  SBox6: TSBox = (
    12, 10,  1, 15, 10,  4, 15,  2,  9,  7,  2, 12,  6,  9,  8,  5,
     0,  6, 13,  1,  3, 13,  4, 14, 14,  0,  7, 11,  5,  3, 11,  8,
     9,  4, 14,  3, 15,  2,  5, 12,  2,  9,  8,  5, 12, 15,  3, 10,
     7, 11,  0, 14,  4,  1, 10,  7,  1,  6, 13,  0, 11,  8,  6, 13
  );

  SBox7: TSBox = (
     64, 208, 176,   0,  32, 176, 224, 112, 240,  64,   0, 144, 128,  16, 208, 160,
     48, 224, 192,  48, 144,  80, 112, 192,  80,  32, 160, 240,  96, 128,  16,  96,
     16,  96,  64, 176, 176, 208, 208, 128, 192,  16,  48,  64, 112, 160, 224, 112,
    160, 144, 240,  80,  96,   0, 128, 240,   0, 224,  80,  32, 144,  48,  32, 192
  );

  SBox8: TSBox = (
    13,  1,  2, 15,  8, 13,  4,  8,  6, 10, 15,  3, 11,  7,  1,  4,
    10, 12,  9,  5,  3,  6, 14, 11,  5,  0,  0, 14, 12,  9,  7,  2,
     7,  2, 11,  1,  4, 14,  1,  7,  9,  4, 12, 10, 14,  8,  2, 13,
     0, 15,  6, 12, 10,  9, 13,  0, 15,  3,  3,  5,  5,  6,  8, 11
  );

function ExpandedSubstitutedAndPermutedDWORD(const D: DWORD;
                                             const K: TtiSixBitArray): DWORD;
var
  X, Y: DWORD;
begin
  { First row takes bits 32, and bits 1 - 5. }
  X := K[0] xor (D shl 5 and $20 or D shr 27 and $1F);
  { Next row takes bits 4 - 9. }
  Y := K[1] xor D shr 23 and LowSixBits;
  Result := PBox1(.SBox1[X] or SBox2[Y].);

  { Next row takes bits 8 - 13. }
  X := K[2] xor D shr 19 and LowSixBits;
  { Next row takes bits 12 - 17. }
  Y := K[3] xor D shr 15 and LowSixBits;
  Result := Result or PBox2(.SBox3[X] or SBox4[Y].);

  { Next row takes bits 16 - 21. }
  X := K[4] xor D shr 11 and LowSixBits;
  { Bits 20 - 25. }
  Y := K[5] xor D shr 7 and LowSixBits;
  Result := Result or PBox3(.SBox5[X] or SBox6[Y].);

  { Bits 24 - 29. }
  X := K[6] xor D shr 3 and LowSixBits;
  {              Bits 28 - 32,        bit 1. }
  Y := K[7] xor (D shl 1 and $3e or D shr 31 and $3f);
  Result := Result or PBox4(.SBox7[X] or SBox8[Y].);
end;

function BitSelection(const Block: Int64; const A; const ASize: Integer): DWORD;
var
  I: Integer;
  ShiftAmount: Integer;
  H, L: DWORD;
  PA: ^Integer;
begin
  Result := 0;
  PA := Addr(A);
  H := TDoubleDWORD(Block).R;
  L := TDoubleDWORD(Block).L;
  for I := Pred(ASize) downto 0 do begin
    ShiftAmount := PA^;
    if ShiftAmount > 31 then
      Result := Result or H shr (ShiftAmount - 32) and 1 shl I
    else begin
      Result := Result or L shr ShiftAmount and 1 shl I;
    end;
    Inc(PA);
  end;
end;

function PC2(const C, D: DWORD): Int64;
const
  MapL: array [0..31] of Byte = (
   24, 27, 20,  6, 14, 10,  3, 22,
    0, 17,  7, 12,  8, 23, 11,  5,
   16, 26,  1,  9, 19, 25,  4, 15,
   26, 15,  8,  1, 21, 12, 20,  2
  );
  MapH: array [0..15] of Byte = (
   24, 16,  9,  5, 18,  7, 22, 13,
    0, 25, 23, 27,  4, 17, 11, 14
  );
var
  I: Integer;
  ResultHL: TDoubleDWORD absolute Result;
begin
  {
   C and D are 28-bit halves. Thus if bit needed is more than 28, we
   need to go to the high DWORD, namely C. Fortunately, all bits
   greater than 28 occur between Map[1] and Map[24] inclusive, so we can
   optimize this by breaking up the loops.
  }
  Result := 0;
  { First fill in the high 16 bits, which are the low 16 bits of KeyHL.H. }
  for I := High(MapH) downto Low(MapH) do begin
    ResultHL.R := ResultHL.R or C shr MapH[I] and 1 shl I;
  end;
  { Now fill in the next 8 bits, which are the high 8 bits of KeyHL.L. }
  for I := High(MapL) downto High(MapL) - 7 do begin
    ResultHL.L := ResultHL.L or C shr MapL[I] and 1 shl I;
  end;
  { Finally fill in the low 24 bits, which are the low 24 bits of KeyHL.L. }
  for I := High(MapL) - 8 downto Low(MapL) do begin
    ResultHL.L := ResultHL.L or D shr MapL[I] and 1 shl I;
  end;
end;


{ TEncryptDES }

procedure TEncryptDES.DecryptBlock(var pCiphertext);
var
  H, L, R, DH, DL: DWORD;
begin
  L := TDoubleDWORD(pCiphertext).L;
  H := TDoubleDWORD(pCiphertext).R;
  DL := L and $55555555 or H and $55555555 shl 1;
  DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
  L :=    IP[DL shr 24        ] shr 3
       or IP[DL shr 16 and $FF] shr 2
       or IP[DL shr  8 and $FF] shr 1
       or IP[DL        and $FF];
  R :=    IP[DH shr 24        ] shr 3
       or IP[DH shr 16 and $FF] shr 2
       or IP[DH shr  8 and $FF] shr 1
       or IP[DH        and $FF];

  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 0]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 1]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 2]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 3]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 4]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 5]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 6]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 7]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 8]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 9]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[10]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[11]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[12]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[13]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[14]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[15]);

  DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
  DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
  TDoubleDWORD(pCiphertext).R :=    IPInv[DL shr 24        ] shl 6
                                 or IPInv[DL shr 16 and $FF] shl 4
                                 or IPInv[DL shr  8 and $FF] shl 2
                                 or IPInv[DL and        $FF];
  TDoubleDWORD(pCiphertext).L :=    IPInv[DH shr 24        ] shl 6
                                 or IPInv[DH shr 16 and $FF] shl 4
                                 or IPInv[DH shr  8 and $FF] shl 2
                                 or IPInv[DH        and $FF];
end;

function TEncryptDES.DecryptedBlock(const pCiphertext: Int64): Int64;
{ See comments for EncryptedBlock. }
var
  H, L, R, DH, DL: DWORD;
begin
  L := TDoubleDWORD(pCiphertext).L;
  H := TDoubleDWORD(pCiphertext).R;
  DL := L and $55555555 or H and $55555555 shl 1;
  DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
  L :=    IP[DL shr 24        ] shr 3
       or IP[DL shr 16 and $FF] shr 2
       or IP[DL shr  8 and $FF] shr 1
       or IP[DL        and $FF];
  R :=    IP[DH shr 24        ] shr 3
       or IP[DH shr 16 and $FF] shr 2
       or IP[DH shr  8 and $FF] shr 1
       or IP[DH        and $FF];

  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 0]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 1]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 2]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 3]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 4]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 5]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 6]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 7]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 8]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 9]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[10]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[11]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[12]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[13]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[14]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[15]);

  DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
  DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
  TDoubleDWORD(Result).R :=    IPInv[DL shr 24        ] shl 6
                            or IPInv[DL shr 16 and $FF] shl 4
                            or IPInv[DL shr  8 and $FF] shl 2
                            or IPInv[DL and        $FF];
  TDoubleDWORD(Result).L :=    IPInv[DH shr 24        ] shl 6
                            or IPInv[DH shr 16 and $FF] shl 4
                            or IPInv[DH shr  8 and $FF] shl 2
                            or IPInv[DH        and $FF];
end;

procedure TEncryptDES.EncryptBlock(var pPlaintext);
var
  H, L, R, DH, DL: DWORD;
begin
  L := TDoubleDWORD(pPlaintext).L;
  H := TDoubleDWORD(pPlaintext).R;
  DL := L and $55555555 or H and $55555555 shl 1;
  DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
  L :=    IP[DL shr 24        ] shr 3
       or IP[DL shr 16 and $FF] shr 2
       or IP[DL shr  8 and $FF] shr 1
       or IP[DL        and $FF];
  R :=    IP[DH shr 24        ] shr 3
       or IP[DH shr 16 and $FF] shr 2
       or IP[DH shr  8 and $FF] shr 1
       or IP[DH        and $FF];

  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[15]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[14]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[13]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[12]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[11]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[10]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 9]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 8]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 7]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 6]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 5]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 4]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 3]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 2]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 1]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 0]);

  { Exchange final blocks L16, R16. }
  { Run 'em through the inverse of IP and put 'em back without swapping. }
  DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
  DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
  TDoubleDWORD(pPlaintext).R := IPInv[DL shr 24        ] shl 6
                             or IPInv[DL shr 16 and $FF] shl 4
                             or IPInv[DL shr  8 and $FF] shl 2
                             or IPInv[DL and        $FF];
  TDoubleDWORD(pPlaintext).L := IPInv[DH shr 24        ] shl 6
                             or IPInv[DH shr 16 and $FF] shl 4
                             or IPInv[DH shr  8 and $FF] shl 2
                             or IPInv[DH        and $FF];
end;

function TEncryptDES.EncryptedBlock(const pPlaintext: Int64): Int64;
var
  H, L, R, DH, DL: DWORD;
begin
  L := TDoubleDWORD(pPlaintext).L;
  H := TDoubleDWORD(pPlaintext).R;
  DL := L and $55555555 or H and $55555555 shl 1;
  DH := H and $AAAAAAAA or L and $AAAAAAAA shr 1;
  L :=    IP[DL shr 24        ] shr 3
       or IP[DL shr 16 and $FF] shr 2
       or IP[DL shr  8 and $FF] shr 1
       or IP[DL        and $FF];
  R :=    IP[DH shr 24        ] shr 3
       or IP[DH shr 16 and $FF] shr 2
       or IP[DH shr  8 and $FF] shr 1
       or IP[DH        and $FF];

  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[15]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[14]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[13]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[12]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[11]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[10]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 9]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 8]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 7]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 6]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 5]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 4]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 3]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 2]);
  L := L xor ExpandedSubstitutedAndPermutedDWORD(R, FKeySchedule[ 1]);
  R := R xor ExpandedSubstitutedAndPermutedDWORD(L, FKeySchedule[ 0]);

  { Exchange final blocks L16, R16. }
  { Run 'em through the inverse of IP and put 'em back without swapping. }
  DL := L and $0F0F0F0F or R and $0F0F0F0F shl 4;
  DH := R and $F0F0F0F0 or L and $F0F0F0F0 shr 4;
  TDoubleDWORD(Result).R :=    IPInv[DL shr 24        ] shl 6
                            or IPInv[DL shr 16 and $FF] shl 4
                            or IPInv[DL shr  8 and $FF] shl 2
                            or IPInv[DL and        $FF];
  TDoubleDWORD(Result).L :=    IPInv[DH shr 24        ] shl 6
                            or IPInv[DH shr 16 and $FF] shl 4
                            or IPInv[DH shr  8 and $FF] shl 2
                            or IPInv[DH        and $FF];
end;

procedure TEncryptDES.SetIntSeed(const Value: Int64 );
// Moved from original TDESCipher.CreateKeySchedule
type
  THalfArray = array [0..27] of Integer;
const
  V: array [TtiDESKeyScheduleRange] of Byte = (
    1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1
  );
  PC1C: THalfArray = (
     7, 15, 23, 31, 39, 47, 55,
    63,  6, 14, 22, 30, 38, 46,
    54, 62,  5, 13, 21, 29, 37,
    45, 53, 61,  4, 12, 20, 28
  );
  PC1D: THalfArray = (
     1,  9, 17, 25, 33, 41, 49,
    57,  2, 10, 18, 26, 34, 42,
    50, 58,  3, 11, 19, 27, 35,
    43, 51, 59, 36, 44, 52, 60
  );
var
  C, D: DWORD;
  I, J: Integer;
  {
   I could just use a TDoubleDWORD, but doing this caused an internal error
   C3677 in D2. Using an absolute, ironically, caused this to go away.
   (Normally absolutes create tons of internal errors.)
  }
  K1: Int64;
  K: TDoubleDWORD absolute K1;
begin
  inherited;
  C := BitSelection(Value, PC1C, High(PC1C) - Low(PC1C) + 1);
  D := BitSelection(Value, PC1D, High(PC1D) - Low(PC1D) + 1);
  J := High(V);
  for I := Low(V) to High(V) do begin
    C := CircularSHL28(C, V[I]);
    D := CircularSHL28(D, V[I]);
    { Select 48 bits from the concatenation of C and D. (C is the high DWORD.) }
    K1 := PC2(C, D);
    { Pre-calc the six-bit chunks and store them. }
    FKeySchedule[J][0] := K.R shr 10 and LowSixBits;
    FKeySchedule[J][1] := K.R shr  4 and LowSixBits;
    FKeySchedule[J][2] := K.R shl  2 and LowSixBits or
                          K.L shr 30 and LowTwoBits;
    FKeySchedule[J][3] := K.L shr 24 and LowSixBits;
    FKeySchedule[J][4] := K.L shr 18 and LowSixBits;
    FKeySchedule[J][5] := K.L shr 12 and LowSixBits;
    FKeySchedule[J][6] := K.L shr  6 and LowSixBits;
    FKeySchedule[J][7] := K.L        and LowSixBits;
    Dec(J);
  end;
end;


initialization
  // Register the TtiEncrypt with the EncryptFactory
  gEncryptFactory.RegisterClass( cgsEncryptionDES, TEncryptDES ) ;
  gtiEncryptClass := TEncryptDES ;


end.
