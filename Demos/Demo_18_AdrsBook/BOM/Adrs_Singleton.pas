unit Adrs_Singleton;

{.$I tiDefines.inc}

interface
uses
  Adrs_BOM;

function GAdrsBook: TAdrsBook;
procedure FreeAndNilAdrsBook;

implementation
uses
  SysUtils;
  
var
  UAdrsBook: TAdrsBook;

function GAdrsBook: TAdrsBook;
begin
  if UAdrsBook = nil then
  begin
    UAdrsBook:= TAdrsBook.Create;
    UAdrsBook.Read;
  end;
  result:= UAdrsBook;
end;

procedure FreeAndNilAdrsBook;
begin
  FreeAndNil(UAdrsBook);
end;

initialization

finalization
  UAdrsBook.Free;

end.








