unit Adrs_Manager;

interface
uses
   tiBaseObject
  ,Adrs_BOM
  ;

type

  TAdrsManager = class(TtiBaseObject)
  private
    FAdrsBook: TAdrsBook;
    function GetAdrsBook: TAdrsBook;
  public
    destructor Destroy; override;
    property AdrsBook: TAdrsBook read GetAdrsBook;
  end;

function GAdrsManager: TAdrsManager;

implementation
uses
  SysUtils
  ;
  
var
  uAdrsManager: TAdrsManager;

function GAdrsManager: TAdrsManager;
begin
  if uAdrsManager = nil then
    uAdrsManager:= TAdrsManager.Create;
  result:= uAdrsManager;
end;

{ TAdrsManager }

destructor TAdrsManager.Destroy;
begin
  FAdrsBook.Free;
  inherited;
end;

function TAdrsManager.GetAdrsBook: TAdrsBook;
begin
  if FAdrsBook = nil then
  begin
    FAdrsBook:= TAdrsBook.Create;
    FAdrsBook.Read;
  end;
  result:= FAdrsBook;
end;

initialization

finalization
  FreeAndNil(uAdrsManager);

end.
