
{$I tiDefines.inc}

unit tiQueryXML;

interface
uses
   tiQuery
  ,tiXML
  ,Classes
  ,tiAutoMap
  ,tiObject
  ,tiDBConnectionPool
  ,MSXML_TLB
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
  ,Windows
 ;

type



  TtiDBConnectionPoolDataXML = Class(TtiDBConnectionPoolDataAbs)
  public
    procedure InitDBConnectionPool; override;
  end;

  TtiDatabaseXML = class(TtiDatabase)
  private
    FXMLDomDoc : IXMLDOMDocument;
    FConnected : boolean;
    FCurrentThreadID: DWord;
    FInTransaction : boolean;
    FTerminateOnConnectError: boolean;
    FXMLRCTrans: IXMLReservedCharsTranslator;

    function DOMFindTable(const AName : string): IXMLDomElement;
    function GetXMLWhereClause(const AWhere: TtiQueryParams): string;
    function GetXMLDomDoc: IXMLDOMDocument;

  protected
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    property  XMLDomDoc: IXMLDOMDocument read GetXMLDomDoc;

  public
    constructor Create; override;
    destructor  Destroy; override;

    class function DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;

    procedure   ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    procedure   DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
    property    TerminateOnConnectError : boolean read FTerminateOnConnectError write FTerminateOnConnectError;
    function    Test : boolean; override;
    function    TIQueryClass: TtiQueryClass; override;

  end;

  TtiQueryXML = class(TtiQueryNonSQL)
  private
    FQueryString : TStringList;
    FTableName: string;
    FNodes : IXMLDOMNodeList;
    FCurrentRecordIndex : integer;
    function CurrentRow : IXMLDomElement;

  protected

    function    GetParamAsString(const AName: string): string; override;
    procedure   SetParamAsString(const AName, AValue: string); override;

    function    GetFieldAsString(const AName: string): string  ; override;
    function    GetFieldAsFloat(const AName: string): extended     ; override;
    function    GetFieldAsBoolean(const AName: string): boolean   ; override;
    function    GetFieldAsInteger(const AName: string): Int64; override;
    function    GetFieldAsDateTime(const AName: string):TDateTime; override;
    function    GetFieldIsNull(const AName: string): Boolean; override;

    function    GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function    GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
    function    GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function    GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function    GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function    GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    function    GetActive: boolean; override;
    procedure   SetActive(const AValue: boolean); override;
    function    GetEOF: boolean; override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   Next   ; override;
    procedure   ExecSQL; override;

    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase;  override;
    procedure   Reset; override;

    function    FieldCount : integer; override;
    function    FieldName(AIndex : integer): string; override;
    function    FieldIndex(const AName : string): integer; override;
    function    FieldKind(AIndex : integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex : integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   InsertRow(const ATableName : string; const AParams : TtiQueryParams); override;
    procedure   DeleteRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); override;

  end;


implementation
uses
   tiLog
  ,tiUtils
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  ,tiWin32
  ,tiExcept
  ,SysUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
 ;

var
  // ToDo: Move uXMLTags to the TtiDatabaseXML so settings can be changed at
  //       runtime on a per-xml file basis.
  uXMLTags : TtiXMLTags;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryXML
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryXML.Create;
begin
  inherited;
  FQueryString := TStringList.Create;
end;

destructor TtiQueryXML.Destroy;
begin
  FQueryString.Free;
  inherited;
end;

procedure TtiQueryXML.Close;
begin
  Active := false;
end;

procedure TtiQueryXML.ExecSQL;
begin
  Assert(false, 'Not implemented in ' + ClassName);
end;

function TtiQueryXML.GetFieldAsBoolean(const AName: string): boolean;
var
  Ls : string;
begin
  try
    Ls := CurrentRow.GetAttribute(LowerCase(AName));
    result :=
       (SameText(Ls, 'T') or
         SameText(Ls, 'TRUE') or
         SameText(Ls, '1') or
         SameText(Ls, 'Y') or
         SameText(Ls, 'YES'));
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field <' + AName + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsDateTime(const AName: string): TDateTime;
var
  Ls : string;
begin
  try
    Ls := CurrentRow.GetAttribute(LowerCase(AName));
    result := tiXMLStringToDateTime(Ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field <' + AName + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.message);
  end;
end;

function TtiQueryXML.GetFieldAsFloat(const AName: string): extended;
var
  Ls : string;
begin
//   ToDo: Fix null management
  try
    Ls := CurrentRow.GetAttribute(LowerCase(AName));
    if Ls = '' then
      result := 0
    else
      result := StrToFloat(ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field <' + AName + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsInteger(const AName: string): Int64;
var
  ls : string;
begin
  try
    ls := CurrentRow.GetAttribute(LowerCase(AName));
    result := StrToInt64(ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field <' + AName + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsString(const AName: string): string;
var
  ls: string;
begin
  try
    ls := CurrentRow.GetAttribute(LowerCase(AName));
    result := (Database as TtiDatabaseXML).FXMLRCTrans.InsertReserved(rcMSXML, ls);
  except
    on e:exception do
    begin
      result := ''; // To shut the compiler up
      raise EtiOPFDataException.Create(
        'Error reading field <' + AName + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
    end;
  end;
end;

function TtiQueryXML.GetActive: boolean;
begin
  result := FNodes <> nil;
end;

function TtiQueryXML.GetEOF: boolean;
begin
  result := FCurrentRecordIndex = FNodes.Length;
end;

function TtiQueryXML.GetSQL: TStrings;
begin
  result := FQueryString;
end;

procedure TtiQueryXML.Next;
begin
  Inc(FCurrentRecordIndex);
end;

procedure TtiQueryXML.Open;
begin
  Active := true;
end;

procedure TtiQueryXML.SetActive(const AValue: boolean);
begin
  if Active = AValue then
    Exit; //==>

  if Active and Not AValue then
  begin
    FCurrentRecordIndex := 0;
    FNodes := nil;
    Exit;
  end;
  Assert(false, 'Can not call ' + ClassName + '.SetActive(true)');
end;

procedure TtiQueryXML.SetSQL(const AValue: TStrings);
begin
  FQueryString.Assign(AValue);
end;

procedure TtiQueryXML.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
end;

procedure TtiQueryXML.DetachDatabase;
begin
  inherited DetachDatabase;
end;

function TtiQueryXML.FieldCount: integer;
begin
  result := CurrentRow.Attributes.Length;
end;

function TtiQueryXML.FieldName(AIndex: integer): string;
begin
  result := CurrentRow.Attributes[AIndex].NodeName;
end;

procedure TtiQueryXML.Reset;
begin
//  Active := false;
//  FQuery.SQL.Clear;
//  FQuery.Params.Clear;
end;

function TtiQueryXML.FieldIndex(const AName: string): integer;
var
  lTable    : IXMLDomElement;
  lXMLFields : IXMLDomElement;
  lXMLField : IXMLDomElement;
  i : integer;
begin
  result := -1;
  lTable := (Database as TtiDatabaseXML).DOMFindTable(FTableName);
  if lTable = nil then
    Exit; //==>
  lXMLFields := lTable.SelectSingleNode(uXMLTags.Fields) as IXMLDomElement;
  for i := 0 to lXMLFields.childNodes.length - 1 do
  begin
    lXMLField := lXMLFields.childNodes[i] as IXMLDomElement;
    if SameText(AName, lXMLField.GetAttribute(uXMLTags.FieldName)) then
    begin
      result := i;
      Exit; //==>
    end;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseXML
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseXML.Create;
begin
  inherited;
  FCurrentThreadID:= 0;
  XMLDomDoc;
  FTerminateOnConnectError := true;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
end;

destructor TtiDatabaseXML.Destroy;
begin
  // There is an AV here under some conditions:
  // a) Text unit test & Win2K;
  // b) GUIUnit tests on Win2K is OK;
  // c) Text unit tests on WinXP is OK.
  // Added exception handling to suppress AV until we find a fix
  // Suspect a dangling reference to XMLDOMDoc
  try  FXMLDomDoc := nil;
  except end;
  tiWin32CoUnInitialize;
  inherited;
end;

procedure TtiDatabaseXML.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FXMLDomDoc.Save(DatabaseName);
  FInTransaction := false;
end;

function TtiDatabaseXML.InTransaction: boolean;
begin
  result := FInTransaction;
end;

procedure TtiDatabaseXML.RollBack;
begin
  Connected := true;
  FInTransaction := false;
end;

procedure TtiDatabaseXML.StartTransaction;
begin
  XMLDomDoc;
  FInTransaction := true;
end;

procedure TtiDatabaseXML.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lXMLTable : IXMLDomElement;
  lXMLFields : IXMLDomElement;
  lXMLField : IXMLDomElement;

  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;

  i : integer;

begin

  lTable := TtiDBMetaDataTable(AData);
  lXMLTable := DOMFindTable(lTable.Name);
  if lXMLTable = nil then
    Exit; //==>

  lXMLFields := lXMLTable.SelectSingleNode(uXMLTags.Fields) as IXMLDomElement;
  for i := 0 to lXMLFields.childNodes.length - 1 do
  begin
    lXMLField := lXMLFields.childNodes[i] as IXMLDomElement;

    lField             := TtiDBMetaDataField.Create;
    lField.Name       := lXMLField.GetAttribute(uXMLTags.FieldName);
    lField.KindAsStr  := lXMLField.GetAttribute(uXMLTags.FieldKind);
    lField.Width      := lXMLField.GetAttribute(uXMLTags.FieldSize);
    lField.ObjectState := posClean;
    lTable.Add(lField);
  end;
end;

procedure TtiDatabaseXML.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lTables : IXMLDOMNodeList;
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  i : integer;
begin
  lTables := FXMLDomDoc.selectNodes('//' + uXMLTags.Tables + '/' + uXMLTags.Table);
  lMetaData := (AData as TtiDBMetaData);
  for i := 0 to lTables.Length - 1 do
  begin
    lTable := TtiDBMetaDataTable.Create;
    lTable.Name := (lTables.item[i] as IXMLDomElement).GetAttribute(uXMLTags.TableName);
    lTable.ObjectState := posPK;
    lMetaData.Add(lTable);
    lMetaData.ObjectState := posClean;
  end;

end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryXML.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lTable   : IXMLDomElement;
  lMetaData : IXMLDomElement;
  lKind : string;
begin
  result := qfkString; // To shut the compiler up
  lTable := (Database as TtiDatabaseXML).DOMFindTable(FTableName);
  if lTable = nil then
    Exit; //==>
  lMetaData := lTable.SelectSingleNode(uXMLTags.Fields) as IXMLDomElement;
  lKind := (lMetaData.ChildNodes[AIndex] as IXMLDomElement).GetAttribute(uXMLTags.FieldKind);
  result := StrToQueryFieldKind(lKind);
end;

function TtiQueryXML.FieldSize(AIndex: integer): integer;
var
  lTable   : IXMLDomElement;
  lMetaData : IXMLDomElement;
  lSize : string;
begin
  if FieldKind(AIndex) in [ qfkInteger, qfkLogical, qfkFloat, qfkDateTime, qfkLongString ] then
    result := 0
  else
  begin
    result := 0; // To shut the compiler up
    lTable := (Database as TtiDatabaseXML).DOMFindTable(FTableName);
    if lTable = nil then
      Exit; //==>
    lMetaData := lTable.SelectSingleNode(uXMLTags.Fields) as IXMLDomElement;
    lSize := (lMetaData.ChildNodes[AIndex] as IXMLDomElement).GetAttribute(uXMLTags.FieldSize);
    result := StrToInt(lSize);
  end;
end;

function TtiDatabaseXML.GetConnected: boolean;
begin
  // ToDo: There must be a way of getting this info out of IXMLDOMDocument
  result := FConnected;
end;

procedure TtiDatabaseXML.SetConnected(AValue: boolean);
var
  lError : IXMLDomParseError;
  lErrorMessage : string;
begin

  // ToDo: Require a better way of doing this
  if Connected and (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FConnected := false;
    FXMLDomDoc := nil;
    FXMLDomDoc := CoDomDocument.Create;
    Exit;
  end;

  try
    FXMLDomDoc.Load(DatabaseName);

    While FXMLDomDoc.readyState <> 4 do
    begin
      Log('%s Waiting for XML doc to load');
      Sleep(50);
    end;

    lError := FXMLDomDoc.ParseError;
    if lError.ErrorCode <> 0 then
    begin
      lErrorMessage :=
        Format('XML load error: %s' + Cr +
                'Line: %s' + Cr +
                'Position: %s' + Cr +
                'Details %s' + Cr +
                'XML %s',
                [VarToStr(lError.Reason),
                 VarToStr(lError.line),
                 VarToStr(lError.linePos),
                 VarToStr(lError.SrcText),
                 DatabaseName ]);
      EtiOPFDBExceptionCanNotConnect.Create(cTIPersistXML, DatabaseName, UserName, Password, lErrorMessage);
    end;
  except
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create(cTIPersistXML, DatabaseName, UserName, Password, e.message);
  end;

  FConnected := true;

end;

function TtiQueryXML.GetFieldIsNull(const AName: string): Boolean;
//var
//  i : integer;
begin
  Result := GetFieldAsString(AName) = '';
//  result := true;
//  for i := 0 to CurrentRow.Attributes.Length - 1 do
//    if SameText(CurrentRow.Attributes[i].NodeName, AName) then
//    begin
//      result := false;
//      Break; //==>
//    end;
end;

procedure TtiDatabaseXML.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lTables : IXMLDomElement;
  lTable : IXMLDomElement;
  lFields : IXMLDomElement;
  lField : IXMLDomElement;
  lRows  : IXMLDomElement;
  i : integer;
begin

  lTable := DOMFindTable(ATableMetaData.Name);
  if lTable <> nil then
    raise EtiOPFInternalException.Create('Attempt to create duplicate table name <' +
                    ATableMetaData.Name + '>');


  StartTransaction;
  try
    lTable := FXMLDomDoc.CreateElement(uXMLTags.Table);

    lTable.setAttribute(uXMLTags.TableName, LowerCase(ATableMetaData.Name));

    lFields      := FXMLDomDoc.CreateElement(uXMLTags.Fields);
    lTable.AppendChild(lFields);

    for i := 0 to ATableMetaData.Count - 1 do
    begin
      lField       := FXMLDomDoc.CreateElement(uXMLTags.Field);
      lField.SetAttribute(uXMLTags.FieldName, LowerCase(ATableMetaData.Items[i].Name));
      lField.SetAttribute(uXMLTags.FieldKind, LowerCase(ATableMetaData.Items[i].KindAsStr));
      lField.SetAttribute(uXMLTags.FieldSize, ATableMetaData.Items[i].Width);
      lFields.AppendChild(lField);
    end;

    lRows        := FXMLDomDoc.CreateElement(uXMLTags.Rows);
    lTable.AppendChild(lRows);

    lTables := FXMLDomDoc.SelectSingleNode('//' + uXMLTags.Tables) as IXMLDomElement;
    if lTables = nil then
      raise EtiOPFInternalException.Create('Cant find XML <' +  uXMLTags.Tables + '> element');

    lTables.appendChild(lTable);
    Commit;
  except
    on e:exception do
    begin
      RollBack;
      raise;
    end;
  end;
end;

procedure TtiDatabaseXML.DropTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lTables : IXMLDomElement;
  lTable : IXMLDomElement;
begin
  lTable := DOMFindTable(ATableMetaData.Name);
  if lTable <> nil then
  begin
    StartTransaction;
    try
      lTables := FXMLDomDoc.SelectSingleNode('//' + uXMLTags.Tables) as IXMLDomElement;
      if lTables = nil then
        raise EtiOPFInternalException('Unable to find XML Element <' + uXMLTags.Tables + '>');
      lTables.RemoveChild(lTable);
      Commit;
    except
      on e:exception do
      begin
        RollBack;
        raise;
      end;
    end;
  end;
end;

function TtiDatabaseXML.DOMFindTable(const AName: string): IXMLDomElement;
var
  ls : string;
begin
  ls := '//' + uXMLTags.Tables +
        '/' + uXMLTags.Table + '[@' +
        uXMLTags.TableName + '="' +
        LowerCase(AName) + '"]';
  result := FXMLDomDoc.selectSingleNode(ls) as IXMLDomElement;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBConnectionPoolDataXML
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBConnectionPoolDataXML.InitDBConnectionPool;
begin
  DBConnectionPool.MinPoolSize := 0;
  DBConnectionPool.MaxPoolSize := 1;
end;

function TtiDatabaseXML.GetXMLWhereClause(const AWhere: TtiQueryParams): string;
var
  i : integer;
begin
  result := '';
  if AWhere = nil then
    Exit;
  for i := 0 to AWhere.Count - 1 do
  begin
    result := tiAddTrailingValue(result, '$and$');
    result :=
      result +
      LowerCase(AWhere.Items[i].Name) + '=' +
      '"' + FXMLRCTrans.RemoveReserved(rcMSXML, AWhere.Items[i].GetValueAsString)  + '"';
  end;

  if result <> '' then
    result := '[@' + result + ']';

end;


procedure TtiQueryXML.SelectRow(const ATableName: string;const AWhere: TtiQueryParams);
var
  lTable : IXMLDomElement;
  lQuery : string;
begin
  Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  FTableName := lowerCase(ATableName);

  lTable := (Database as TtiDatabaseXML).DOMFindTable(FTableName);
  if lTable = nil then
    raise EtiOPFInternalException.Create(
          'Can not find table <' + ATableName +
          '> in <' + Database.DatabaseName + '>');
  lQuery :=
          uXMLTags.Rows + '/' + uXMLTags.Row +
          (Database as TtiDatabaseXML).GetXMLWhereClause(AWhere);

  FNodes := lTable.SelectNodes(lQuery);

  FCurrentRecordIndex := 0;
end;

function TtiQueryXML.CurrentRow: IXMLDomElement;
begin
  result := (FNodes.Item[FCurrentRecordIndex] as IXMLDomElement);
end;

procedure TtiQueryXML.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lTable : IXMLDomElement;
  lRows : IXMLDomNodeList;
  lWhere : string;
  i : integer;
begin
  lTable := (Database as TtiDatabaseXML).DOMFindTable(ATableName);
  if lTable <> nil then
  begin
    lWhere :=
            uXMLTags.Rows + '/' + uXMLTags.Row +
            (Database as TtiDatabaseXML).GetXMLWhereClause(AWhere);

    lRows := lTable.SelectNodes(lWhere);
    for i := 0 to lRows.Length - 1 do
      lTable.SelectSingleNode(uXMLTags.Rows).RemoveChild(lRows.item[i]);
  end;
end;

procedure TtiQueryXML.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
var
  lTable : IXMLDomElement;
  lRow   : IXMLDomElement;
  lRows  : IXMLDomNode;
  lName  : string;
  lValue : string;
  i : integer;
  lMetaDataTable : TtiDBMetaDataTable;
  lParam: TtiQueryParamAbs;
begin

  lTable := (Database as TtiDatabaseXML).DOMFindTable(ATableName);
  if lTable = nil then
    raise EtiOPFInternalException.Create(
          'Can not find table <' + ATableName +
          '> in <' + Database.DatabaseName + '>');

  lRow := (Database as TtiDatabaseXML).XMLDomDoc.CreateElement(uXMLTags.Row);

  lMetaDataTable := TtiDBMetaDataTable.Create;
  try
    lMetaDataTable.Name := ATableName;
    (Database as TtiDatabaseXML).ReadMetaDataFields(lMetaDataTable);
    for i := 0 to lMetaDataTable.Count - 1 do
    begin
      lParam := AParams.FindParamByName(lMetaDataTable.Items[i].Name);
      if lParam <> nil then
      begin
        lName := LowerCase(lParam.Name);
        lValue := (Database as TtiDatabaseXML).FXMLRCTrans.RemoveReserved(rcMSXML, lParam.GetValueAsString);
        lRow.SetAttribute(lName, lValue);
      end else
        lRow.setAttribute(lMetaDataTable.Items[i].Name, '');
    end;
  finally
    lMetaDataTable.Free;
  end;

  lRows := lTable.SelectSingleNode(uXMLTags.Rows);
  if lRows = nil then
    raise EtiOPFInternalException.Create('Can not find XML tag <' + uXMLTags.Rows + '>');

  lRows.AppendChild(lRow);

end;

procedure TtiQueryXML.UpdateRow(const ATableName: string; const AParams, AWhere : TtiQueryParams);
var
  lTable : IXMLDomElement;
  lRows : IXMLDomNodeList;
  lRow   : IXMLDomElement;
  lWhere : string;
  i, j : integer;        
  lValue : string;
  lName : string;
begin
  lTable := (Database as TtiDatabaseXML).DOMFindTable(ATableName);
  if lTable <> nil then
  begin
    lWhere :=
            uXMLTags.Rows + '/' + uXMLTags.Row +
            (Database as TtiDatabaseXML).GetXMLWhereClause(AWhere);
    lRows := lTable.SelectNodes(lWhere);
    for i := 0 to lRows.Length - 1 do
    begin
      lRow := (lRows.item[i] as IXMLDomElement);
      for j := 0 to AParams.Count - 1 do
      begin
        lName := LowerCase(AParams.Items[j].Name);
        lValue := (Database as TtiDatabaseXML).FXMLRCTrans.RemoveReserved(rcMSXML, AParams.Items[j].GetValueAsString);
        lRow.SetAttribute(lName, lValue);
      end;
    end;
  end;
end;

class procedure TtiDatabaseXML.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  lXML : string;
  lPath : string;
begin
  lXML :=
      uXMLTags.DocHeader +
    tiXMLTag(uXMLTags.DocData) +
    uXMLTags.DatabaseDetails +
    tiXMLTag(uXMLTags.Tables) +
    tiXMLTagEnd(uXMLTags.Tables) +
    tiXMLTagEnd(uXMLTags.DocData);

  lPath := ExtractFilePath(ADatabaseName);
  if (lPath <> '') and not DirectoryExists(lPath) then
    ForceDirectories(lPath);
  tiStringToFile(lXML, ADatabaseName);
end;

class function TtiDatabaseXML.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
begin
  result := FileExists(ADatabaseName);
end;

function TtiQueryXML.HasNativeLogicalType: boolean;
begin
  result := true;
end;

function TtiQueryXML.GetParamAsString(const AName: string): string;
begin
  Result := Params.GetValueAsString(AName);
end;

procedure TtiQueryXML.SetParamAsString(const AName, AValue: string);
begin
  Params.SetValueAsString(AName, AValue);
end;

function TtiDatabaseXML.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseXML.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryXML;
end;

function TtiQueryXML.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  ls: string;
begin
  try
    ls := CurrentRow.Attributes[AIndex].nodeValue;
    result :=
       (SameText(ls, 'T') or
         SameText(ls, 'TRUE') or
         SameText(ls, '1') or
         SameText(ls, 'Y') or
         SameText(ls, 'YES'));
  except
    on e:exception do
      raise EtiOPFInternalException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
var
  ls : string;
begin
  try
    ls := CurrentRow.Attributes[AIndex].nodeValue;
    result := tiXMLStringToDateTime(ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Mesage: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsFloatByIndex(AIndex: Integer): extended;
var
  ls : string;
begin
  try
    ls := CurrentRow.Attributes[AIndex].nodeValue;
    if ls = '' then
      result := 0
    else
      result := StrToFloat(ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
var
  ls: string;
begin
  try
    ls := CurrentRow.Attributes[AIndex].nodeValue;
    result := StrToInt64(ls);
  except
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiQueryXML.GetFieldAsStringByIndex(AIndex: Integer): string;
var
  Ls: string;
begin
  try
    Ls := CurrentRow.Attributes[AIndex].nodeValue;
    result := (Database as TtiDatabaseXML).FXMLRCTrans.InsertReserved(rcMSXML, Ls);
  except
    on e:exception do
    begin
      result := ''; // To shut the compiler up
      raise EtiOPFDataException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
    end;
  end;
end;

function TtiQueryXML.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  try
    result := GetFieldAsStringByIndex(AIndex) = '';
  except 
    on e:exception do
      raise EtiOPFDataException.Create(
        'Error reading field index <' + IntToStr(AIndex) + '> ' + Cr +
        'From row <' + IntToStr(FCurrentRecordIndex) + '> ' + Cr +
        'In table <' + FTableName + '> ' + Cr +
        'XML: <' + CurrentRow.XML + '> Message: ' + e.Message);
  end;
end;

function TtiDatabaseXML.GetXMLDomDoc: IXMLDOMDocument;
var
  LCurrentThreadID: DWord;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  if LCurrentThreadID <> FCurrentThreadID then
  begin
    FXMLDomDoc:= nil;
    FCurrentThreadID:= LCurrentThreadID;
    tiWin32CoInitialize;
    FXMLDomDoc := CoDomDocument.Create;
    FConnected := false;
    FInTransaction := false;
  end;
  Result:= FXMLDomDoc;
end;

Initialization
  uXMLTags := TtiXMLTags.Create;
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
              cTIPersistXML,
              TtiDBConnectionPoolDataXML,
              TtiQueryXML,
              TtiDatabaseXML);

finalization
  uXMLTags.Free;
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistXML);

end.



