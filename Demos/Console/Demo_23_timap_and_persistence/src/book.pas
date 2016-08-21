unit book;

{$ifdef FPC}
  {$mode objfpc}{$h+}
{$endif}

interface

uses
  Classes,
  book_autogen;

type

  TBook = class(TBaseBook)
  protected
    procedure SetImageAsStream(const AValue: TStream); override;
    function  GetImageAsStream: TStream; override;
  end;


  TBookList = class(TBaseBookList)
  end;


implementation

uses
  FPimage, FPReadBMP, FPReadJPEG, FPReadPNG, FPWritePNG;


{ TBook }

procedure TBook.SetImageAsStream(const AValue: TStream);
var
  Img: TFPCustomImage;
  Reader: TFPCustomImageReader;
begin
  if AValue.Size = 0 then
    Image := nil
  else
  begin
    Reader := ImageHandlers.ImageReader['Portable Network Graphics'].Create;
    Img := Reader.ImageRead(AValue, nil);
    Reader.Free;
    Image := Img;
  end;
end;

function TBook.GetImageAsStream: TStream;
var
  Writer: TFPCustomImageWriter;
begin
  Result := TMemoryStream.Create; // must not be nil.
  if (Image = nil) or (Image.Width = 0) or (Image.Height = 0) then
    Exit;
  Writer := ImageHandlers.ImageWriter['Portable Network Graphics'].Create;
  Writer.ImageWrite(Result, FImage);
  Writer.Free;
end;

initialization
  TBaseBookList.ItemClass := TBook;

end.
