unit DemoTIPerBlobs_BOM;

interface
uses
  tiPtnVisPerObj
  ,Classes
  ,Graphics
  ;

type

  TDemoImages = class;
  TDemoImage = class;

  TDemoImages = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TDemoImage ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TDemoImage); reintroduce ;
  public
    property    Items[i:integer] : TDemoImage read GetItems write SetItems ;
    procedure   Add( pObject : TDemoImage   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  published
  end ;

  TDemoImage = class( TPerObjAbs )
  private
    FDescription: string;
    FImage: TBitmap;
  protected
    function    GetOwner: TDemoImages; reintroduce ;
    procedure   SetOwner(const Value: TDemoImages); reintroduce ;
  public
    constructor Create ; override ;
    constructor CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TDemoImages             read GetOwner      write SetOwner ;
  published
    property    Description : string read FDescription write FDescription ;
    // Having this as a TImage descandant makes it easier to work
    // in the GUI (because the image edit control mapps to a TGraphic),
    // but harder to work at the database layer (because the persitence
    // layers require a TStream to save the data)
    property    Image : TBitMap read FImage ;
  end ;


implementation

{ TDemoImages }

procedure TDemoImages.Add(pObject: TDemoImage; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TDemoImages.GetItems(i: integer): TDemoImage;
begin
  result := TDemoImage( inherited GetItems( i )) ;
end;

procedure TDemoImages.SetItems(i: integer; const Value: TDemoImage);
begin
   inherited SetItems( i, Value ) ;
end;

{ TDemoImage }

constructor TDemoImage.Create;
begin
  inherited;
  FImage := TBitMap.Create ;
end;

constructor TDemoImage.CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ;
begin
  inherited;
  Description := '<Enter description>' ;
end;

destructor TDemoImage.Destroy;
begin
  FImage.Free ;
  inherited;
end;

function TDemoImage.GetOwner: TDemoImages;
begin
  result := TDemoImages( inherited GetOwner ) ;
end;

procedure TDemoImage.SetOwner(const Value: TDemoImages);
begin
  inherited SetOwner( Value ) ;
end;

end.
