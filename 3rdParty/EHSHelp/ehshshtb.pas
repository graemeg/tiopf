{ EC Software Help Suite

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by copyright laws and
  international copyright treaties, as well as other intellectual property
  laws and treaties. The product is licensed, not sold.

  The source code and sample programs in this package or parts hereof
  as well as the documentation shall not be copied, modified or redistributed
  without permission, explicit or implied, of the author.


  EMail: info@ec-software.com
  Internet: http://www.ec-software.com

  Disclaimer of Warranty
  ----------------------

  THIS SOFTWARE AND THE ACCOMPANYING FILES ARE PROVIDED "AS IS" AND
  WITHOUT WARRANTIES OF ANY KIND WHETHER EXPRESSED OR IMPLIED.

  In no event shall the author be held liable for any damages whatsoever,
  including without limitation, damages for loss of business profits,
  business interruption, loss of business information, or any other loss
  arising from the use or inability to use the software. }


unit ehshshtb;

interface

function GetHashValue(TopicID: string): longint;

implementation

{$Q-}
function GetHashValue(TopicID: string): longint;
{ With credits for M. Winterhoff - this hash table is based on his
  excellent documentation of the - officially undocumented - HLP file format }
const
   Table: array [0..255] of longint = (
    $00, $D1, $D2, $D3, $D4, $D5, $D6, $D7,     //0-7
    $D8, $D9, $DA, $DB, $DC, $DD, $DE, $DF,     //8-15
    $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7,     //16-23
    $E8, $E9, $EA, $EB, $EC, $ED, $EE, $EF,     //24-31
    -16, $0B, -14, $F3, $F4, $F5, -10, -9,      //32-39
    -8,  -7,  $FA, $FB, -4,  -3,  $0C, -1,      //40-47
    $0A, $01, $02, $03, $04, $05, $06, $07,     //48-55
    $08, $09, $0A, $0B, $0C, $0D, $0E, $0F,     //56-63
    $10, $11, $12, $13, $14, $15, $16, $17,     //64-71
    $18, $19, $1A, $1B, $1C, $1D, $1E, $1F,     //72-79
    $20, $21, $22, $23, $24, $25, $26, $27,     //80-87
    $28, $29, $2A, $0B, $0C, $0D, $0E, $0D,     //88-95
    $10, $11, $12, $13, $14, $15, $16, $17,     //96-103
    $18, $19, $1A, $1B, $1C, $1D, $1E, $1F,     //104-111
    $20, $21, $22, $23, $24, $25, $26, $27,     //112-119
    $28, $29, $2A, $2B, $2C, $2D, $2E, $2F,     //120-127
   -176,-175,-174,-173,-172,-171,-170,-169,     //128-135
   -168,-167,-166,-165,-164,-163,-162,-161,     //136-143
   -160, $61, $62, $63, $64, $65, $66, $67,     //144-151
   -152,-151,-150,-149,-148,-147,-146,-145,     //152-159
   -144,-143,-142,-141,-140,-139,-138,-137,     //160-167
   -136,-135,-134,-133,-132,-131,-130,-129,     //168-175
   -128,-127,-126,-125,-124,-123,-122,-121,     //176-183
   -120,-119,-118,-117,-116,-115,-114,-113,     //184-191
   -112,-111,-110,-109,-108,-107,-106,-105,     //192-199
   -104,-103,-102,-101,-100, -99, -98, -97,     //200-207
    -96, -95, -94, -93, -92, -91, -90, -89,     //208-215
    -88, -87, -86, -85, -84, -83, -82, -81,     //216-223
    -80, -79, -78, -77, -76, -75, -74, -73,     //224-231
    -72, -71, -70, -69, -68, -67, -66, -65,     //232-239
    -64, -63, -62, -61, -60, -59, -58, -57,     //240-247
    -56, -55, -54, -53, -52, -51, -50, -49  );  //248-255
var
   i: integer;
begin
     result := 0;
     try
       for I := 1 to length(TopicID) do result := (result * 43) + Table[ ord(TopicID[i]) ];
     except
     end;
end;
{$Q+}

end.
