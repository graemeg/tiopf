unit tiPerObjAbs;

//DEFINEMACRO:TMyClass=The collectable class owned by TMyClasses
//DEFINEMACRO:TMyClasses=The collection class

interface

type
TCodeTemplate = class( TPerObjAbs )
  private
    { Put private fields here
      Ex.: FsLastName : String;}
  protected
    function GetOwner: <!TMyClasses!>; reintroduce ;
    procedure SetOwner(const Value: <!TMyClasses!>); reintroduce ;
  public
    property Owner : <!TMyClasses!> read GetOwner write SetOwner ;
  published
    {Put the properties here
     Ex.: property LastName  : string read FsLastName  write FsLastName ;}
end ;

implementation

function TCodeTemplate.GetOwner: <!TMyClasses!>;
begin
  result := <!TMyClasses!>( inherited GetOwner );
end;

procedure TCodeTemplate.SetOwner(const Value: <!TMyClasses!>);
begin
  inherited SetOwner( Value ) ;
end;

end.
