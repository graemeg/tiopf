unit tiEncryptNone;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiEncrypt
 ;

const
  cgsEncryptionNone = 'EncryptionNone';

type

  TEncryptNone = class(TtiEncryptAbs)
  public
    function    EncryptString(const AData : AnsiString): AnsiString; override;
    function    DecryptString(const AData : AnsiString): AnsiString; override;
    procedure   EncryptStream(const pSrc, pDest : TStream); override;
    procedure   DecryptStream(const pSrc, pDest : TStream); override;
  end;


implementation


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TEncryptNone
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TEncryptNone.EncryptString(const AData : AnsiString): AnsiString;
begin
  result := AData;
end;


function TEncryptNone.DecryptString(const AData : AnsiString): AnsiString;
begin
  result := AData;
end;


procedure TEncryptNone.EncryptStream(const pSrc, pDest : TStream);
begin
  pSrc.Position := 0;
  pDest.Size := 0;
  pDest.CopyFrom(pSrc, pSrc.Size);
end;


procedure TEncryptNone.DecryptStream(const pSrc, pDest : TStream);
begin
  pSrc.Position := 0;
  pDest.Size := 0;
  pDest.CopyFrom(pSrc, pSrc.Size);
end;


initialization
  // Register the TtiEncrypt with the EncryptFactory
  gEncryptFactory.RegisterClass(cgsEncryptionNone, TEncryptNone);
  gtiEncryptClass := TEncryptNone;

end.
