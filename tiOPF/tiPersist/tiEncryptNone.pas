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
    TEncryptNone - Simple encryption

  Revision History:
    ???,  ????, Scott Maskiel, Created

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiEncryptNone;

interface

uses
  Classes
  ,tiEncryptAbs
  ;

const
  cgsEncryptionNone = 'EncryptionNone' ;

type

  TEncryptNone = class( TtiEncryptAbs )
  public
    function    EncryptString( const pData : string ) : string ; override ;
    function    DecryptString( const pData : string ) : string ; override ;
    procedure   EncryptStream( const pSrc, pDest : TStream ) ; override ;
    procedure   DecryptStream( const pSrc, pDest : TStream ) ; override ;
  end ;


implementation
{$IFDEF MSWINDOWS}
uses
  Windows
  ;
{$ENDIF MSWINDOWS}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TEncryptNone
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TEncryptNone.EncryptString( const pData : string ) : string ;
begin
  result := pData ;
end ;

// -----------------------------------------------------------------------------
function TEncryptNone.DecryptString( const pData : string ) : string ;
begin
  result := pData ;
end ;

// -----------------------------------------------------------------------------
procedure TEncryptNone.EncryptStream( const pSrc, pDest : TStream ) ;
begin
  pSrc.Position := 0 ;
  pDest.Size := 0 ;
  pDest.CopyFrom( pSrc, pSrc.Size ) ;
end ;

// -----------------------------------------------------------------------------
procedure TEncryptNone.DecryptStream( const pSrc, pDest : TStream ) ;
begin
  pSrc.Position := 0 ;
  pDest.Size := 0 ;
  pDest.CopyFrom( pSrc, pSrc.Size ) ;
end ;

initialization
  // Register the TtiEncrypt with the EncryptFactory
  gEncryptFactory.RegisterClass( cgsEncryptionNone, TEncryptNone) ;
  gtiEncryptClass := TEncryptNone ;

end.
