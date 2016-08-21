

unit book_autogen;
// ---------------------------------------------------------
// Automatically generated on 21-8-16 21:13:54
// Warning:
//   If you rerun timap, your changes in this file will be lost
// ---------------------------------------------------------


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}


interface


uses
  SysUtils
  ,tiObject
  ,tiAutoMap
  ,tiOPFManager
  ,tiVisitorDB
  ,tiVisitorCriteria
  ,tiCriteria
  ,tiSQLParser
  ,mapper
  ,Classes
  ,fpImage
  ;

type


  // ---------------------------------------------
  // Generated Classes
  // ---------------------------------------------


  TBaseBookClass = class of TBaseBook;
  { Generated Class: TBaseBook}
  TBaseBook = class(TtiObject)
  protected
    FName: string;
    FAuthor: string;
    FSomeIntProp: integer;
    FProcessedDate: TDateTime;
    FImage: TfpCustomImage;
    procedure   SetName(const AValue: string); virtual;
    procedure   SetAuthor(const AValue: string); virtual;
    procedure   SetSomeIntProp(const AValue: integer); virtual;
    procedure   SetProcessedDate(const AValue: TDateTime); virtual;
    procedure   SetImage(const AValue: TfpCustomImage); virtual;
    function    GetImageAsStream: TStream; virtual; abstract;
    procedure   SetImageAsStream(const AValue: TStream); virtual; abstract;
  public
    destructor  Destroy; override;
    procedure   Read; override;
    procedure   Save; override;
  published
    property    Name: string read FName write SetName;
    property    Author: string read FAuthor write SetAuthor;
    property    SomeIntProp: integer read FSomeIntProp write SetSomeIntProp;
    property    ProcessedDate: TDateTime read FProcessedDate write SetProcessedDate;
    property    Image: TfpCustomImage read FImage write SetImage;
  end;

  { List of TBaseBook.  TtiMappedFilteredObjectList descendant. }
  TBaseBookList = class(TtiMappedFilteredObjectList)
  private
    class var FItemClass: TBaseBookClass;
  protected
    procedure   SetItems(i: integer; const AValue: TBaseBook); reintroduce;
    function    GetItems(i: integer): TBaseBook; reintroduce;
  public
    property    Items[i:integer] : TBaseBook read GetItems write SetItems;
    procedure   Add(AObject: TBaseBook); reintroduce;
    procedure   Read; override;
    procedure   Save; override;
    class property ItemClass: TBaseBookClass read FItemClass write FItemClass;
    { Return count (1) if successful. }
    function    FindByOID(const AOID: string): integer;
  end;

  { Read Visitor for TBaseBook }
  TBaseBook_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;

  { Create Visitor for TBaseBook }
  TBaseBook_Create = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;

  { Update Visitor for TBaseBook }
  TBaseBook_Update = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;

  { Delete Visitor for TBaseBook }
  TBaseBook_Delete = class(TtiVisitorUpdate)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
  end;

  { List Read Visitor for TBaseBookList }
  TBaseBookList_Read = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: Boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;


  { Visitor Manager Registrations }
  procedure RegisterVisitors;

  { Register Auto Mappings }
  procedure RegisterMappings;


implementation


uses
  tiUtils
  ,tiLog
  ;

procedure RegisterMappings;
begin
  { Automap registrations for TBaseBook }
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK', 'OID', 'BK_OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK','Name', 'BK_NAME');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK','Author', 'BK_AUTHOR');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK','SomeIntProp', 'BK_INTPROP');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK','ProcessedDate', 'BK_DATE_PROCESSED');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TBaseBook,
    'BOOK','Image', 'BK_IMAGE');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TBaseBookList, TBaseBook);
end;

procedure RegisterVisitors;
begin
  { NOTE: The most reliable order of registering visitors are
          Read, Delete, Update, Create }
  GTIOPFManager.VisitorManager.RegisterVisitor('LoadBaseBookList', TBaseBookList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('LoadBaseBook', TBaseBook_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor('SaveBaseBook', TBaseBook_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor('SaveBaseBook', TBaseBook_Update);
  GTIOPFManager.VisitorManager.RegisterVisitor('SaveBaseBook', TBaseBook_Create);

end;

destructor TBaseBook.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TBaseBook.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  BeginUpdate;
  FName := AValue;
  EndUpdate;
end;

procedure TBaseBook.SetAuthor(const AValue: string);
begin
  if FAuthor = AValue then
    Exit;
  BeginUpdate;
  FAuthor := AValue;
  EndUpdate;
end;

procedure TBaseBook.SetSomeIntProp(const AValue: integer);
begin
  if FSomeIntProp = AValue then
    Exit;
  BeginUpdate;
  FSomeIntProp := AValue;
  EndUpdate;
end;

procedure TBaseBook.SetProcessedDate(const AValue: TDateTime);
begin
  if FProcessedDate = AValue then
    Exit;
  BeginUpdate;
  FProcessedDate := AValue;
  EndUpdate;
end;

procedure TBaseBook.SetImage(const AValue: TfpCustomImage);
begin
  if FImage = AValue then
    Exit;
  BeginUpdate;
  FImage := AValue;
  EndUpdate;
end;

procedure TBaseBook.Read;
begin
  GTIOPFManager.VisitorManager.Execute('LoadBaseBook', self);
end;

procedure TBaseBook.Save;
begin
  GTIOPFManager.VisitorManager.Execute('SaveBaseBook', self);
end;

 {TBaseBookList }

procedure TBaseBookList.Add(AObject: TBaseBook);
begin
  inherited Add(AObject);
end;

function TBaseBookList.GetItems(i: integer): TBaseBook;
begin
  result := inherited GetItems(i) as TBaseBook;
end;

procedure TBaseBookList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('LoadBaseBookList', self);
end;

procedure TBaseBookList.Save;
begin
  GTIOPFManager.VisitorManager.Execute('SaveBaseBook', self);
end;

procedure TBaseBookList.SetItems(i: integer; const AValue: TBaseBook);
begin
  inherited SetItems(i, AValue);
end;

function TBaseBookList.FindByOID(const AOID: string): integer;
begin
  if self.Count > 0 then
    self.Clear;

  Criteria.ClearAll;
  Criteria.AddEqualTo('BK_OID', AOID);
  Read;
  result := Count;
end;

{ TBaseBook_Create }
function TBaseBook_Create.AcceptVisitor: Boolean;
begin
  result := (Visited is TBaseBook) and (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TBaseBook_Create.Init;
begin
  Query.SQLText :=
    'INSERT INTO BOOK(' +
    ' BK_OID, ' +
    ' BK_NAME, ' +
    ' BK_AUTHOR, ' +
    ' BK_INTPROP, ' +
    ' BK_DATE_PROCESSED, ' +
    ' BK_IMAGE' +
    ') VALUES (' +
    ' :BK_OID, ' +
    ' :BK_NAME, ' +
    ' :BK_AUTHOR, ' +
    ' :BK_INTPROP, ' +
    ' :BK_DATE_PROCESSED, ' +
    ' :BK_IMAGE' +
    ') ';
end;

procedure TBaseBook_Create.SetupParams;
var
  lObj: TBaseBook;
  lStream: TStream;
  lStreamFree: Boolean = True;
begin
  lObj := TBaseBook(Visited);
  lObj.OID.AssignToTIQuery('BK_OID',Query);
  Query.ParamAsString['BK_NAME'] := lObj.Name;
  Query.ParamAsString['BK_AUTHOR'] := lObj.Author;
  Query.ParamAsInteger['BK_INTPROP'] := lObj.SomeIntProp;
  Query.ParamAsString['BK_DATE_PROCESSED'] := tiDateTimeAsIntlDateStor(lObj.ProcessedDate);
  try
    lStream := lObj.GetImageAsStream();
    if Assigned(lStream) then
    begin
      lStream.Position := 0;
      Query.AssignParamFromStream('BK_IMAGE', lStream);
    end;
  finally
    if lStreamFree then
      lStream.Free;
  end;
end;

{ TBaseBook_Update }
function TBaseBook_Update.AcceptVisitor: Boolean;
begin
  result := (Visited is TBaseBook) and (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TBaseBook_Update.Init;
begin
  Query.SQLText :=
    'UPDATE BOOK SET ' +
    ' BK_NAME = :BK_NAME, ' +
    ' BK_AUTHOR = :BK_AUTHOR, ' +
    ' BK_INTPROP = :BK_INTPROP, ' +
    ' BK_DATE_PROCESSED = :BK_DATE_PROCESSED, ' +
    ' BK_IMAGE = :BK_IMAGE ' +
    'WHERE BK_OID = :BK_OID' ;
end;

procedure TBaseBook_Update.SetupParams;
var
  lObj: TBaseBook;
  lStream: TStream;
  lStreamFree: Boolean = True;
begin
  lObj := TBaseBook(Visited);
  lObj.OID.AssignToTIQuery('BK_OID',Query);
  Query.ParamAsString['BK_NAME'] := lObj.Name;
  Query.ParamAsString['BK_AUTHOR'] := lObj.Author;
  Query.ParamAsInteger['BK_INTPROP'] := lObj.SomeIntProp;
  Query.ParamAsString['BK_DATE_PROCESSED'] := tiDateTimeAsIntlDateStor(lObj.ProcessedDate);
  try
    lStream := lObj.GetImageAsStream();
    if Assigned(lStream) then
    begin
      lStream.Position := 0;
      Query.AssignParamFromStream('BK_IMAGE', lStream);
    end;
  finally
    if lStreamFree then
      lStream.Free;
  end;
end;

{ TBaseBook_Read }
function TBaseBook_Read.AcceptVisitor: Boolean;
begin
  result := (Visited is TBaseBook) and ((Visited.ObjectState = posPK) OR (Visited.ObjectState = posClean));
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TBaseBook_Read.Init;
begin
  Query.SQLText :=
    'SELECT ' +
    ' BK_OID, ' +
    ' BK_NAME, ' +
    ' BK_AUTHOR, ' +
    ' BK_INTPROP, ' +
    ' BK_DATE_PROCESSED, ' +
    ' BK_IMAGE ' +
    'FROM BOOK WHERE BK_OID = :BK_OID' ;
end;

procedure TBaseBook_Read.SetupParams;
var
  lObj: TBaseBook;
begin
  lObj := TBaseBook(Visited);
  lObj.OID.AssignToTIQuery('BK_OID',Query);
end;

procedure TBaseBook_Read.MapRowToObject;
var
  lObj: TBaseBook;
  lStream: TStream;
  lStreamFree: Boolean = True;
begin
  lObj := TBaseBook(Visited);
  lObj.OID.AssignFromTIQuery('BK_OID',Query);
  lObj.Name := Query.FieldAsString['BK_NAME'];
  lObj.Author := Query.FieldAsString['BK_AUTHOR'];
  lObj.SomeIntProp := Query.FieldAsInteger['BK_INTPROP'];
  lObj.ProcessedDate := tiIntlDateStorAsDateTime(Query.FieldAsString['BK_DATE_PROCESSED']);
  lStream := TMemoryStream.Create;
  try
    Query.AssignFieldAsStream('BK_IMAGE', lStream);
    lStream.Position := 0;
    lObj.SetImageAsStream(lStream);
  finally
    if lStreamFree then
      lStream.Free
  end;
end;

{ TBaseBook_Delete }
function TBaseBook_Delete.AcceptVisitor: Boolean;
begin
  result := (Visited is TBaseBook) and (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TBaseBook_Delete.Init;
begin
  Query.SQLText :=
    'DELETE FROM BOOK ' +
    'WHERE BK_OID = :BK_OID';
end;

procedure TBaseBook_Delete.SetupParams;
var
  lObj: TBaseBook;
begin
  lObj := TBaseBook(Visited);
  lObj.OID.AssignToTIQuery('BK_OID',Query);
end;

{ TBaseBookList_Read }
function TBaseBookList_Read.AcceptVisitor: Boolean;
begin
  result := (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TBaseBookList_Read.Init;
var
  lFiltered: ItiFiltered;
  lWhere: string;
  lOrder: string;
  lSQL: string;
begin
  if Supports(Visited, ItiFiltered, lFiltered) then
  begin
    if lFiltered.GetCriteria.HasCriteria then
      lWhere := ' WHERE ' + tiCriteriaAsSQL(lFiltered.GetCriteria)
    else
      lWhere := '';
    if lFiltered.GetCriteria.hasOrderBy then
      lOrder := tiCriteriaOrderByAsSQL(lFiltered.GetCriteria)
    else
      lOrder := '';
  end;

  lSQL :=
    'SELECT ' +
    ' BK_OID, ' +
    ' BK_NAME, ' +
    ' BK_AUTHOR, ' +
    ' BK_INTPROP, ' +
    ' BK_DATE_PROCESSED, ' +
    ' BK_IMAGE ' +
    'FROM  BOOK %s %s ;';

  Query.SQLText := gFormatSQL(Format(lSQL, [lWhere, lOrder]), TBaseBook);
end;

procedure TBaseBookList_Read.MapRowToObject;
var
  lObj: TBaseBook;
  lItemClass : TBaseBookClass = TBaseBook;
  lStream: TStream;
  lStreamFree: Boolean = True;
begin
  if Assigned(TBaseBookList.ItemClass) then
    lItemClass := TBaseBookList.ItemClass;
  lObj := lItemClass.Create;
  lObj.OID.AssignFromTIQuery('BK_OID',Query);
  lObj.Name := Query.FieldAsString['BK_NAME'];
  lObj.Author := Query.FieldAsString['BK_AUTHOR'];
  lObj.SomeIntProp := Query.FieldAsInteger['BK_INTPROP'];
  lObj.ProcessedDate := tiIntlDateStorAsDateTime(Query.FieldAsString['BK_DATE_PROCESSED']);
  lStream := TMemoryStream.Create;
  try
    Query.AssignFieldAsStream('BK_IMAGE', lStream);
    lStream.Position := 0;
    lObj.SetImageAsStream(lStream);
  finally
    if lStreamFree then
      lStream.Free
  end;
  lObj.ObjectState := posClean;
  TtiObjectList(Visited).Add(lObj);
end;

initialization
  RegisterVisitors;
  RegisterMappings;


end.
