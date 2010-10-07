unit tiXML;

{$I tiDefines.inc}


interface
uses
  Classes
  ,tiConstants
  ,tiBaseObject
 ;

{$IFDEF OPTIMISE_XMLDB_SIZE}
  OPTIMISE_XMLDB_SIZE is no longer a valid compiler directive
{$ENDIF}


const
  cTIOPFXMLVersion= '2.0';
  cErrorInvalidOptXMLDBSizeValue = 'Invalid OptXMLDBSize value';
  cErrorInvalidOptXMLDBSizeStringValue = 'Invalid OptXMLDBSize value <%s>';
  cErrorInvalidParamToCompressEncode   = 'Invalid param to tiCompressEncode <%s>';
  cErrorInvalidTXMLAttributeParseState = 'Invalid TXMLAttributeParseState';
  cErrorInvalidTXMLNodeParseState = 'Invalid TXMLNodeParseState';


type
  TtiXMLNodeParseState      = (xnpsWaitForBlock, xnpsInBlock);
  TtiXMLAttributeParseState = (xapsWaitForAttr, xapsInAttrName, xapsWaitForAttrValue, xapsInAttrValue);

  TtiOnXMLNodeMethod = procedure (const AStr : string) of object;
  TtiOnXMLAttributeMethod = procedure (const AName, AValue : string) of object;


  TtiXMLParser = class(TtiBaseObject)
  private
  public
    procedure  ParseForNode(const AStr, pStartToken, pEndToken: string; pOnFind: TtiOnXMLNodeMethod);
    procedure  ParseForAttributes(const AStr: string; pOnFind: TtiOnXMLAttributeMethod);
  end;


  TtiReservedChar = (rcUnassigned, rcXML, rcMSXML, rcCSV, rcTAB);


  IXMLReservedCharsTranslator = interface
    ['{615B2B52-DDDA-40A8-B7AE-D68E1704CC13}']
    function RemoveReserved(const ACharKind: TtiReservedChar;
      const AString: string): string;
    function InsertReserved(const ACharKind: TtiReservedChar;
      const AString: string): string;
  end;


  TtiOptXMLDBSize = (optDBSizeOff, optDBSizeOn );


const
  cOptXMLDBSize : array[TtiOptXMLDBSize] of string =
    ('optDBSizeOff', 'optDBSizeOn');


  function tiStringToOptXMLDBSize(const AValue: string): TtiOptXMLDBSize;


type
  TtiXMLTags = class(TtiBaseObject)
  private
    FOptXMLDBSize: TtiOptXMLDBSize;
    FRow: string;
    FLenRowStart: Byte;
    FRowStart: string;
    FTables: string;
    FLenTablesStart: Byte;
    FTablesStart: string;
    FTableEnd: string;
    FLenTableEnd: Byte;
    FTablesEnd: string;
    FLenTablesEnd: Byte;
    FDocData: string;
    FTable: string;
    FLenTableStart: Byte;
    FTableStart: string;
    FTableName: string;
    FFields: string;
    FLenFieldsEnd: Byte;
    FLenFieldsStart: Byte;
    FFieldsEnd: string;
    FFieldsStart: string;
    FField: string;
    FLenFieldStart: Byte;
    FLenFieldEnd: Byte;
    FFieldStart: string;
    FFieldEnd: string;
    FFieldKind: string;
    FFieldName: string;
    FFieldSize: string;
    FRows: string;
    FLenRowsStart: Byte;
    FRowsStart: string;
    FLenRowsEnd: Byte;
    FRowsEnd: string;
    FRowsStartEnd: string;
    FLenRowsStartEnd: Byte;
    FRowEnd: string;
    FLenRowEnd: Byte;
    FValue: string;
    FDatabaseDetails: string;
    FDocHeader: string;
    FXMLVersion: string;
    FDatabaseFooter: string;
    FDatabaseHeader: string;
    FFieldNameMetaDataFieldName: string;
    FFieldNameCommandType: string;
    FFieldNameQuerySQL: string;
    FFieldNameTableName: string;
    FTableNameQuery: string;
    FTableNameQueryParam: string;
    FFieldNameMetaDataFieldWidth: string;
    FTableNameMetaData: string;
    FFieldNameParamName: string;
    FFieldNameComputerName: string;
    FTableNameResultMessage: string;
    FFieldNameMetaDataTableName: string;
    FFieldNameMetaDataFieldKind: string;
    FTableNameResultSet: string;
    FFieldNameResultError: string;
    FFieldNameParamValue: string;
    FFieldNameTransactionID: string;
    FFieldNameResultRowCount: string;
    FFieldNameParamKind: string;
    FFieldNameUserName: string;
    procedure   SetOptXMLDBSize(AValue: TtiOptXMLDBSize);
    procedure   DeriveInternalValues;
    procedure   SetOptXMLDBSizeAsString(const AValue: string);
    function    GetOptXMLDBSizeAsString: string;
  public
    constructor Create;
    class function DefaultDocHeader : string; 
    property    OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write SetOptXMLDBSize;
    property    OptXMLDBSizeAsString: string read GetOptXMLDBSizeAsString Write SetOptXMLDBSizeAsString;
    property    DocData: string read FDocData;
    property    Tables : string read FTables;
    property    TablesStart: string read FTablesStart;
    property    LenTablesStart: Byte read FLenTablesStart;
    property    TablesEnd:     string read FTablesEnd;
    property    LenTablesEnd: Byte read FLenTablesEnd;
    property    Table: string read FTable;
    property    TableStart: string read FTableStart;
    property    LenTableStart: Byte read FLenTableStart;
    property    TableEnd: string read FTableEnd;
    property    LenTableEnd: Byte read FLenTableEnd;
    property    TableName: string read FTableName;
    property    Fields: string read FFields;
    property    FieldsStart: string read FFieldsStart;
    property    LenFieldsStart: Byte read FLenFieldsStart;
    property    FieldsEnd: string read FFieldsEnd;
    property    LenFieldsEnd: Byte read FLenFieldsEnd;
    property    Field: string read FField;
    property    FieldStart: string read FFieldStart;
    property    LenFieldStart: Byte read FLenFieldStart;
    property    FieldEnd: string read FFieldEnd;
    property    LenFieldEnd: Byte read FLenFieldEnd;
    property    FieldName: string read FFieldName;
    property    FieldKind: string read FFieldKind;
    property    FieldSize: string read FFieldSize;
    property    Rows: string read FRows;
    property    RowsStart: string read FRowsStart;
    property    LenRowsStart: Byte read FLenRowsStart;
    property    RowsEnd: string read FRowsEnd;
    property    LenRowsEnd: Byte read FLenRowsEnd;
    property    RowsStartEnd: string read FRowsStartEnd;
    property    LenRowsStartEnd: Byte read FLenRowsStartEnd;

    property    Row   : string read FRow;
    property    RowStart : string read FRowStart;
    property    RowEnd: string read FRowEnd;
    property    LenRowStart: Byte read FLenRowStart;
    property    LenRowEnd: Byte read FLenRowEnd;

    property    AValue : string read FValue;
    property    DocHeader : string read FDocHeader;
    property    DatabaseDetails : string read FDatabaseDetails;
    property    XMLVersion : string read FXMLVersion;
    property    DatabaseHeader: string read FDatabaseHeader;
    property    DatabaseFooter: string read FDatabaseFooter;

    function    MakeXMLDatabase: string;
    function    MakeXMLDoc(const AString: string): string;

    property    TableNameQuery : string read FTableNameQuery;
    property    FieldNameQuerySQL : string read FFieldNameQuerySQL;
    property    FieldNameTransactionID : string read FFieldNameTransactionID;
    property    FieldNameCommandType : string read FFieldNameCommandType;
    property    FieldNameComputerName : string read FFieldNameComputerName;
    property    FieldNameUserName : string read FFieldNameUserName;

    // Request Query_Param table
    property    TableNameQueryParam : string read FTableNameQueryParam;
    property    FieldNameTableName : string read FFieldNameTableName;
    property    FieldNameParamName : string read FFieldNameParamName;
    property    FieldNameParamKind : string read FFieldNameParamKind;
    property    FieldNameParamValue : string read FFieldNameParamValue;

    // Response message table
    property    TableNameResultMessage : string read FTableNameResultMessage;
    property    FieldNameResultError : string read FFieldNameResultError;
    property    FieldNameResultRowCount : string read FFieldNameResultRowCount;

    // MetaData structure
    property    TableNameMetaData : string read FTableNameMetaData;
    property    FieldNameMetaDataTableName : string read FFieldNameMetaDataTableName;
    property    FieldNameMetaDataFieldName : string read FFieldNameMetaDataFieldName;
    property    FieldNameMetaDataFieldKind : string read FFieldNameMetaDataFieldKind;
    property    FieldNameMetaDataFieldWidth : string read FFieldNameMetaDataFieldWidth;

    // Response set table
    property    TableNameResultSet : string read FTableNameResultSet;
  end;


const
  cgXMLTagOptXMLDBSize    = 'optxmldbsize';
  cgXMLFieldNameStyle     = 'xmlfieldnamestyle';
  cDefaultXMLDocHeader       = '<?xml version="1.0"?>';

  // These may be passed as the document name in a HTTP.Post()
  // ToDo: Improve the way the simple XML packets are build up
  cgTIDBProxy                = 'tidbproxy';
  cgTIDBProxyTestHTML        = 'test';
  cgTIDBProxyTestXML         = 'testxml';
  // For a legacy system. Use cgTIDBProxyTestAlive1 in new systems.
  cgTIDBProxyTestAlive       = 'tidbproxytestalive';
  cgTIDBProxyTestAlive1      = 'tidbproxytestalive1';
  cgTIDBProxyServerVersion   = 'tidbproxyserverversion';
  cgTIDBProxyServerException = 'tidbproxyserverexception';

  cgXMLNodeTIOPF             = 'tiopf';
  cgXMLAttrTIOPFVersion      = 'version'; // <tiopf version="9.999"/>
  cgXMLNodeDBProxyTestAlive  = 'proxytestalive';
  cgXMLAttrDBProxyTestAlive  = 'status'; // <tidbproxytestalive status="passed"/>
  cgXMLAttrDBProxyTestPassed = 'passed';
  cgXMLAttrDBProxyTestFailed = 'failed';


function tiXMLTagEnd(const psTag : String): string;
function tiXMLTag(const psTag : String): string;
function tiXMLDataTag(const psTag : String; psData : string): string;
function tiXMLNodeAttrData(const ANode, AAttr, AData: string): string;
function CreateXMLReservedCharsTranslator: IXMLReservedCharsTranslator;
function tiDeCompressDecode(const AValue: string; const pCompression : string = cgsCompressZLib): string;
function tiCompressEncode(const AValue: string; const pCompression : string = cgsCompressZLib): string;
function tiParseForSingleNode(const pXMLFragment, pNodeName: string): string;
function tiParseForSingleAttribute(const pXMLFragment, AAttrName: string): string;
function tiParseForSingleNodeAttribute(const pXMLFragment, pNodeName, AAttrName: string): string;


var
  gDefaultOptXMLDBSize : TtiOptXMLDBSize;


implementation
uses
   tiUtils
  ,tiCompress
  ,tiCompressZLib // Required to force linking
  ,tiExcept
  ,tiStreams
  ,SysUtils
  ,TypInfo
  ,Math
 ;


function tiXMLTagEnd(const psTag : String): string;
var
  pResult: PChar;
begin
  SetLength(result, Length(psTag) + 3);
  pResult := Pointer(result);
  pResult := StrECopy(pResult, PChar('</'));
  pResult := StrECopy(pResult, Pointer(psTag));
  pResult^:= '>';
//  result := '</' + psTag + '>';
end;


function tiXMLTag(const psTag : String): string;
var
  pResult: PChar;

begin
//  result := '<' + psTag + '>';
  SetLength(result, Length(psTag) + 2);
  pResult := Pointer(result);
  pResult^:= '<';
  Inc(pResult);
  pResult := StrECopy(pResult, Pointer(psTag));
  pResult^:= '>';
end;


function tiXMLDataTag(const psTag : String; psData : string): string;
begin
  result := '<' + psTag + ' ' + psData + '/>';
end;

function tiXMLNodeAttrData(const ANode, AAttr, AData: string): string;
begin
  result:=
    '<' + ANode +
    ' ' + AAttr +
    '="' + AData +
    '"/>';
end;

function tiStringToOptXMLDBSize(const AValue: string): TtiOptXMLDBSize;
var
  i : TtiOptXMLDBSize;
begin
//  Result:= Low(TtiOptXMLDBSize); // To shut the compiler up
  for i := Low(TtiOptXMLDBSize) to High(TtiOptXMLDBSize) do
    if SameText(AValue, cOptXMLDBSize[i]) then
    begin
      result := i;
      Exit; //==>
    end;
  raise Exception.CreateFmt(cErrorInvalidOptXMLDBSizeStringValue, [AValue]);
end;


function tiDeCompressDecode(const AValue: string; const pCompression : string): string;
var
  lCompress : TtiCompressAbs;
  ls : string;
begin
  Result := '';   // lets keep the compiler happy
  // Should be pooling these, or using the one instance (is that thread safe?)
  lCompress := gCompressFactory.CreateInstance(pCompression);
  try
    ls := MimeDecodeString(AValue);
    lCompress.DeCompressString(ls, Result);
  finally
    lCompress.Free;
  end;
end;


function tiCompressEncode(const AValue: string; const pCompression : string): string;
var
  lCompress : TtiCompressAbs;
  ls : string;
begin
  try
    lCompress := gCompressFactory.CreateInstance(pCompression);
    try
      lCompress.CompressString(AValue,ls);
      result := MimeEncodeString(ls);
    finally
      lCompress.Free;
    end;
  except
    on e:Exception do
      raise EtiOPFInternalException.CreateFmt(cErrorInvalidParamToCompressEncode, [AValue]);
  end;
end;


type
  TtiXMLParseForSingleNode = class(TtiXMLParser)
  private
    FNodeString: string;
    FTargetAttributeName: string;
    FAttributeValue: string;
  public
    procedure DoOnXMLNode(const AStr : string);
    procedure DoOnXMLAttribute(const AName, AValue : string);
    property  TargetAttributeName: string read FTargetAttributeName Write FTargetAttributeName;
    property  AttributeValue: string read FAttributeValue Write FAttributeValue;
    property  NodeString: string read FNodeString Write FNodeString;
  end;


procedure TtiXMLParseForSingleNode.DoOnXMLNode(const AStr : string);
begin
  FNodeString := AStr;
end;


procedure TtiXMLParseForSingleNode.DoOnXMLAttribute(const AName, AValue : string);
begin
  if AName = FTargetAttributeName then
    FAttributeValue := AValue;
end;


function tiParseForSingleNode(const pXMLFragment, pNodeName: string): string;
var
  lXMLParser: TtiXMLParseForSingleNode;
begin
  lXMLParser:= TtiXMLParseForSingleNode.Create;
  try
    lXMLParser.ParseForNode(pXMLFragment, '<' + pNodeName, '/>', lXMLParser.DoOnXMLNode);
    Result := lXMLParser.NodeString;
  finally
    lXMLParser.Free;
  end;
end;


function tiParseForSingleAttribute(const pXMLFragment, AAttrName: string): string;
var
  lXMLParser: TtiXMLParseForSingleNode;
begin
  lXMLParser:= TtiXMLParseForSingleNode.Create;
  try
    lXMLParser.TargetAttributeName := AAttrName;
    lXMLParser.ParseForAttributes(pXMLFragment, lXMLParser.DoOnXMLAttribute);
    Result := lXMLParser.AttributeValue;
  finally
    lXMLParser.Free;
  end;
end;


function tiParseForSingleNodeAttribute(const pXMLFragment, pNodeName, AAttrName: string): string;
var
  lNode : string;
begin
  lNode := tiParseForSingleNode(pXMLFragment, pNodeName);
  if lNode <> '' then
    Result := tiParseForSingleAttribute(lNode, AAttrName)  
  else
    Result := '';
end;


type
  // note change of type for EscWith
  TtiXMLReservedChar1 = record
    ResChar : Char;
    EscWith : string;
    EscLen : Byte;
  end;

// redeclare these arrays (local to this unit) to pick up redefinition above..

const cTIXMLReservedChr: array [0..6] of TtiXMLReservedChar1 =
  ((ResChar: '&';  EscWith: '&amp;';  EscLen: 5),
   (ResChar: '<';  EscWith: '&lt;';   EscLen: 4),
   (ResChar: '>';  EscWith: '&gt;';   EscLen: 4),
   (ResChar: '"';  EscWith: '&quot;'; EscLen: 6),
   (ResChar: ''''; EscWith: '&apos;'; EscLen: 6),
   (ResChar: #10;  EscWith: '&lf;';   EscLen: 4),
   (ResChar: #13;  EscWith: '&cr;';   EscLen: 4));

const cTIMSXMLReservedChr: array [0..1] of TtiXMLReservedChar1 =
  ((ResChar: #10;  EscWith: '&lf;';   EscLen: 4),
   (ResChar: #13;  EscWith: '&cr;';   EscLen: 4));

const cTICSVReservedChr: array [0..2] of TtiXMLReservedChar1 =
  ((ResChar: #10;  EscWith: '&lf;';   EscLen: 4),
   (ResChar: #13;  EscWith: '&cr;';   EscLen: 4),
   (ResChar: ',';  EscWith: '&com;';  EscLen: 5));

const cTITABReservedChr: array [0..2] of TtiXMLReservedChar1 =
  ((ResChar: #10;  EscWith: '&lf;';   EscLen: 4),
   (ResChar: #13;  EscWith: '&cr;';   EscLen: 4),
   (ResChar: #9;   EscWith: '&tab;';  EscLen: 5));


type
  PReservedLookupEntry = ^TReservedLookupEntry;
  TReservedLookupEntry = record
    Replacement: string;
    ReplacementLength: integer;
  end;

  PReplacementLookup = ^TReplacementLookup;
  TReplacementLookup = array[char] of PReservedLookupEntry;


  TtiXMLReservedCharsTranslator = class(TInterfacedObject, IXMLReservedCharsTranslator)
  private
    FXMLReservedLookup,
    FMSXMLReservedLookup,
    FCSVReservedLookup,
    FTABReservedLookup: TReplacementLookup;
    procedure   InitialiseLookup(var ALookup: TReplacementLookup;
        const AReplacements: array of TtiXMLReservedChar1);
    procedure   FinaliseLookup(var ALookup: TReplacementLookup);
    function    Remove(const ASource: string;
        const ALookup: TReplacementLookup; const ASubstMaxLength: integer): string;
    function    Insert(const ASource: string;
        const AReplacements: array of TtiXMLReservedChar1): string;
    // declared private to ensure access is only via an interface instance...
    function    RemoveReserved(const ACharKind: TtiReservedChar;
        const AString: string): string;
    function    InsertReserved(const ACharKind: TtiReservedChar;
        const AString: string): string;
  public
    constructor Create;
    destructor  Destroy; override;
 end;


function CreateXMLReservedCharsTranslator: IXMLReservedCharsTranslator;
begin
  result := TtiXMLReservedCharsTranslator.Create;
end;


(*
procedure TtiXMLParser.ParseForAttributes(const AStr:string; pOnFind: TtiOnXMLAttributeMethod);
const
  attrNameSet = ['a'..'z', 'A'..'Z', '0'..'9' ];
  nodeStart = '<';
  space = #32;
  endPStr = #0;
  endAttrName = '=';
  valueDelimiter = '"';

var
  pSrc, pToken: PChar;
  len: cardinal;
  name, value: string;

begin
  // Notes:
  // This method assumes attributes will *always* be completely formed and
  // syntactically correct and of the form: name="value" with no whitespace
  // allowed either side of the "=", and only (1 or more) spaces (#32) between
  // successive attributes
  // As in PH's implementation, this method allows for a leading node tag
  // and node closure tag

  pSrc := Pointer(AStr);

  if (pSrc = nil) or (not Assigned(pOnFind)) then
    exit;

  // skip any starting node
  if pSrc^ = nodeStart then
  repeat
    Inc(pSrc);
  until
    pSrc^ = space;

  // advance over whitespace
  while pSrc^ = space do
    Inc(pSrc);

  // loop over attribute name/value pairs...

  while (pSrc <> nil) and (pSrc^ <> endPStr)
    and (pSrc^ in attrNameSet) do
  begin
    // build attr name
    pToken := pSrc;

    while pSrc^ <> endAttrName do
      Inc(pSrc);

    len := cardinal(pSrc) - cardinal(pToken);
    SetLength(name, len);
    StrLCopy(Pointer(name), pToken, len);

    // advance to attr value
    while pSrc^ <> valueDelimiter do
      Inc(pSrc);

    // move past delimiter to start of value text
    Inc(pSrc);
    pToken := pSrc;

    // search for end of value text
    while pSrc^ <> valueDelimiter do
      Inc(pSrc);

    len := cardinal(pSrc) - cardinal(pToken);
    SetLength(value, len);
    StrLCopy(Pointer(value), pToken, len);
    // name and value captured - call handler
    pOnFind(name, value);
    // advance beyond value delimiter
     Inc(pSrc);

    // advance over whitespace
    while pSrc^ = space do
      Inc(pSrc);

  end;
end;
*)


procedure TtiXMLParser.ParseForAttributes(const AStr:string; pOnFind: TtiOnXMLAttributeMethod);
var
  i         : integer;
  lStrSize  : integer;
  lState    : TtiXMLAttributeParseState;
  lStart    : integer;
  lChar     : Char;
  lName     : string;
  lValue    : string;
  lContainsNodeTag : Boolean;
begin
  lStrSize  := Length(AStr);
  lState    := xapsWaitForAttr;
  lStart    := 0; // To fix a compiler warning
  lContainsNodeTag := (lStrSize >= 1) and (AStr[1] = '<');

  for i := 1 to lStrSize do
  begin
    lChar := AStr[i];
    case lState of
    xapsWaitForAttr : begin
                        if ((lContainsNodeTag) and (lChar = ' ')) or
                           ((not lContainsNodeTag) and (lChar in ['a'..'z', 'A'..'Z', '0'..'9' ])) then
                        begin
                          Inc(lState);
                          if (not lContainsNodeTag) then
                            lStart := i-1
                          else
                            lStart := i;
                        end;
                      end;

    xapsInAttrName : begin
                        if lChar = '=' then
                        begin
                          Inc(lState);
                          lName := Copy(AStr, lStart+1, i - lStart - 1);
                        end;
                      end;
    xapsWaitForAttrValue : begin
                        if (lChar = '"') then
                        begin
                          Inc(lState);
                          lStart := i;
                        end;
                      end;
    xapsInAttrValue : begin
                        if (lChar = '"') then
                        begin
                          lState := Low(TtiXMLAttributeParseState);
                          lValue := Copy(AStr, lStart+1, i - lStart - 1);
                          pOnFind(lName, lValue);
                        end;
                      end;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
    end;
  end;
end;


procedure TtiXMLParser.ParseForNode(const AStr, pStartToken, pEndToken : string; pOnFind : TtiOnXMLNodeMethod);
var
  i         : integer;
  lStrSize  : integer;
  lBlockSize : integer;
  lState    : TtiXMLNodeParseState;
  lNode     : string;
  lPos      : integer;
  lStartTokenLen : integer;
  lEndTokenLen  : integer;
begin
  lStartTokenLen := Length(pStartToken);
  lEndTokenLen  := Length(pEndToken);
  lStrSize := Length(AStr);
  lPos := 0;
  lState   := Low(TtiXMLNodeParseState);
  lBlockSize := 0;
  for i := 1 to lStrSize do
  begin
    case lState of
    xnpsWaitForBlock : begin
                        if Copy(AStr, lPos, lStartTokenLen) = pStartToken then
                        begin
                          Inc(lState);
                          lBlockSize := 0;
                        end;
                      end;
    xnpsInBlock       : begin
                          if Copy(AStr, lPos, lEndTokenLen) = pEndToken then
                          begin
                            lState   := Low(TtiXMLNodeParseState);

                            lNode := Copy(AStr,
                                           lPos - lBlockSize - 1,
                                           lBlockSize + lEndTokenLen + 1);
                            lNode := Copy(lNode,
                                           lStartTokenLen+1,
                                           Length(lNode) - lStartTokenLen - lEndTokenLen);
                            pOnFind(Trim(lNode));
                            lBlockSize := 0;
                          end else
                            Inc(lBlockSize);
                        end;
    else
      raise EtiOPFProgrammerException.Create(cErrorInvalidTXMLNodeParseState);
    end;
    Inc(lPos);
  end;
end;


procedure TtiXMLReservedCharsTranslator.InitialiseLookup(
  var ALookup: TReplacementLookup;
  const AReplacements: array of TtiXMLReservedChar1);
var
  idx: integer;
  pEntry: PReservedLookupEntry;
begin
  // zero out array
  FillChar(ALookup, Sizeof(ALookup), 0);
  for idx := Low(AReplacements) to High(AReplacements) do
  begin
    New(pEntry);
    ALookup[AReplacements[idx].ResChar]:= pEntry;
    pEntry^.Replacement := AReplacements[idx].EscWith;
    pEntry^.ReplacementLength := AReplacements[idx].EscLen;
  end;
end;


procedure TtiXMLReservedCharsTranslator.FinaliseLookup(
  var ALookup: TReplacementLookup);
var
  idx: char;
  pEntry: PReservedLookupEntry;
begin
  for idx := Low(char) to High(char) do
  begin
    pEntry := ALookup[idx];
    if pEntry <> nil then
      Dispose(pEntry);
  end;
end;


function TtiXMLReservedCharsTranslator.InsertReserved(
  const ACharKind: TtiReservedChar; const AString: string): string;
begin
  case ACharKind of
    rcXML: Result := Insert(AString, cTIXMLReservedChr);
    rcMSXML: Result := Insert(AString, cTIMSXMLReservedChr);
    rcCSV: Result := Insert(AString, cTICSVReservedChr);
    rcTAB: Result := Insert(AString, cTITABReservedChr);
  else
    raise Exception.Create('Invalid ACharKind');
  end;
end;


function TtiXMLReservedCharsTranslator.RemoveReserved(
  const ACharKind: TtiReservedChar; const AString: string): string;
begin
  case ACharKind of
    rcXML: Result := Remove(AString, FXMLReservedLookup, 6);
    rcMSXML: Result := Remove(AString, FMSXMLReservedLookup, 2);
    rcCSV: Result := Remove(AString, FCSVReservedLookup, 5);
    rcTAB: Result := Remove(AString, FTABReservedLookup, 5);
  else
    raise Exception.Create('Invalid ACharKind');
  end;
end;


function TtiXMLReservedCharsTranslator.Remove(const ASource: string;
  const ALookup: TReplacementLookup;
  const ASubstMaxLength: integer): string;
var
  pSrc, pResult: PChar;
begin
  if ASource = '' then begin
    Result := '';
    Exit; //==>
  end;

  // max possible length = every char in ASource requires substitution
  // with maximum length escaped char
  SetLength(result, Length(ASource) * ASubstMaxLength * sizeof(char));
  pSrc := PChar(ASource);
  pResult := PChar(Result);

  // repeat till null terminator encountered
  while pSrc^ <> #0 do
  begin
    // lookup replacement for current char
    if ALookup[pSrc^] <> nil then
      // use "with" here as it creates a local (register) ref rather than
      // redirecting each time
      with ALookup[pSrc^]^ do
      begin
        // found a replacement - copy to result
        Move(Pointer(Replacement)^, pResult^, ReplacementLength  * sizeof(char));
        Inc(pResult, ReplacementLength);
      end
    else
    begin
      // copy current char
      pResult^:= pSrc^;
      Inc(pResult);
    end;

    Inc(pSrc);
  end;

  // add null terminator
  pResult^:= #0;
  SetLength(result, pResult - PChar(result));
end;


function TtiXMLReservedCharsTranslator.Insert(const ASource: string;
  const AReplacements: array of TtiXMLReservedChar1): string;
var
  pMatch, pResult, pRemainder, pNull: PChar;
  idx: integer;
begin
  if ASource = '' then begin
    Result := '';
    Exit; //==>
  end;

  // copy of source as starting point
  result := ASource;
  UniqueString(result); // Force unique copy so we don't change source
  // pointer to null terminator at end of result
  pNull := PChar(result) + Length(result);

  for idx := High(AReplacements) downto Low(AReplacements) do
    with AReplacements[idx] do
    begin
      pResult := PChar(result);
      pRemainder := PChar(result);

      // look for occurence of EscWith - returns non-nil pointer
      pMatch := tiStrPos(pRemainder, PChar(EscWith));
      while pMatch <> nil do
      begin
        // Append previous remainder up to match
        if pRemainder <> pResult then
          Move(pRemainder^, pResult^, SizeOf(Char) * (pMatch - pRemainder));
        Inc(pResult, pMatch - pRemainder);

        // insert ResChar
        pResult^:= ResChar;
        Inc(pResult);

        // pointer to portion of string beyond this instance of EscWith
        pRemainder := pMatch + EscLen;

        // search for next EscWith...
        pMatch := tiStrPos(pRemainder, PChar(EscWith));
      end;

      if pRemainder <> pResult then
      begin
        if pRemainder <> pNull then
        begin
          // Append remainder after last match
          Move(pRemainder^, pResult^, SizeOf(Char) * (pNull - pRemainder));
          Inc(pResult, pNull - pRemainder);
        end;
        // apply null terminator at end of "moved" block
        pResult^:= #0;
        pNull := pResult;
      end;
    end;

  SetLength(result, pNull - PChar(result));
end;


constructor TtiXMLReservedCharsTranslator.Create;
begin
  inherited Create;
  InitialiseLookup(FXMLReservedLookup, cTIXMLReservedChr);
  InitialiseLookup(FMSXMLReservedLookup, cTIMSXMLReservedChr);
  InitialiseLookup(FCSVReservedLookup, cTICSVReservedChr);
  InitialiseLookup(FTABReservedLookup, cTITABReservedChr);
end;


destructor TtiXMLReservedCharsTranslator.Destroy;
begin
  FinaliseLookup(FTABReservedLookup);
  FinaliseLookup(FCSVReservedLookup);
  FinaliseLookup(FMSXMLReservedLookup);
  FinaliseLookup(FXMLReservedLookup);
  inherited;
end;


{ TtiXMLTags }

class function TtiXMLTags.DefaultDocHeader: string;
begin
  result := cDefaultXMLDocHeader;
end;
// Make DocHeader visible from the meta class

constructor TtiXMLTags.Create;
begin
  inherited Create;
  FDocHeader      := DefaultDocHeader;
  FXMLVersion        := 'xmlversion';
  SetOptXMLDBSize(gDefaultOptXMLDBSize);
end;


procedure TtiXMLTags.DeriveInternalValues;
begin

  FTablesStart   := '<'  + fTables + '>';
  FLenTablesStart := Length(FTablesStart);
  FTablesEnd     := '</' + FTables + '>';
  FLenTablesEnd  := Length(FTablesEnd);

  FTableStart    := '<' + FTable + ' ';
  FLenTableStart := Length(FTableStart);
  FTableEnd      := '</' + FTable + '>';
  FLenTableEnd   := Length(FTableEnd);

  FFieldsStart   := '<' + FFields + '>' ;
  FLenFieldsStart := Length(FFieldsStart);
  FFieldsEnd     := '</' + FFields + '>';
  FLenFieldsEnd  := Length(FFieldsEnd);

  FFieldStart    := '<' + FField + ' '  ;
  FLenFieldStart := Length(FFieldStart);
  FFieldEnd      := '/>';
  FLenFieldEnd   := Length(FFieldEnd);

  FRowsStart     := '<' + FRows + '>'   ;
  FLenRowsStart  := Length(FRowsStart);
  FRowsEnd       := '</' + FRows + '>'  ;
  FLenRowsEnd    := Length(FRowsEnd);
  FRowsStartEnd  := '<' + FRows + '/>'   ;
  FLenRowsStartEnd:= Length(FRowsStartEnd);

  FRowStart := '<'  + FRow   + ' ';
  FRowEnd   := '/>';
  FLenRowEnd := Length(FRowEnd);

  FLenRowStart := Length(FRowStart);

  FDatabaseDetails := '<' + cgXMLNodeTIOPF + ' ' + cgXMLAttrTIOPFVersion +
                      '="' + cTIOPFXMLVersion + '"/>';

  FDatabaseHeader := FDocHeader + '<' + FDocData + '>' + FDatabaseDetails +
                     '<' + FTables + '>';

  FDatabaseFooter:= '</' + FTables + '>' + '</' + FDocData + '>';

end;

procedure TtiXMLTags.SetOptXMLDBSize(AValue: TtiOptXMLDBSize);
begin
  FOptXMLDBSize := AValue;
  case FOptXMLDBSize of
  optDBSizeOff : begin
                   FDocData        := 'xmldocdata';
                   FTables         := 'tables'    ;
                   FTable          := 'table'     ;
                   FTableName      := 'table_name';
                   FFields         := 'fields'    ;
                   FField          := 'field'     ;
                   FFieldName      := 'field_name';
                   FFieldKind      := 'field_kind';
                   FFieldSize      := 'field_Size';
                   FRows           := 'rows'      ;
                   FRow            := 'row'       ;
                   FValue          := 'value'     ;

                   FTableNameQuery             := 'query';
                   FFieldNameQuerySQL          := 'query_sql';
                   FFieldNameTransactionID     := 'transaction_id';
                   FFieldNameCommandType       := 'command_type';
                   FFieldNameComputerName      := 'computer_name';
                   FFieldNameUserName          := 'user_name';

                   // Request Query_Param table
                   FTableNameQueryParam        := 'query_param';
                   FFieldNameTableName         := 'table_name';
                   FFieldNameParamName         := 'param_name';
                   FFieldNameParamKind         := 'param_kind';
                   FFieldNameParamValue        := 'param_value';

                   // Response message table
                   FTableNameResultMessage     := 'result_message';
                   FFieldNameResultError       := 'error_message';
                   FFieldNameResultRowCount    := 'row_count';

                   // MetaData structure
                   FTableNameMetaData          := 'table_metadata';
                   FFieldNameMetaDataTableName := 'md_table_name';
                   FFieldNameMetaDataFieldName := 'md_field_name';
                   FFieldNameMetaDataFieldKind := 'md_field_kind';
                   FFieldNameMetaDataFieldWidth := 'md_field_width';

                   // Response set table
                   FTableNameResultSet         := 'result_set';
                 end;
  optDBSizeOn : begin
                   FDocData        := 'a';
                   FTables         := 'b';
                   FTable          := 'c';
                   FTableName      := 'd';
                   FFields         := 'e';
                   FField          := 'f';
                   FFieldName      := 'g';
                   FFieldKind      := 'h';
                   FFieldSize      := 'i';
                   FRows           := 'j';
                   FRow            := 'k';
                   FValue          := 'l';
                   FTableNameQuery             := 'm';
                   FFieldNameQuerySQL          := 'n';
                   FFieldNameTransactionID     := 'o';
                   FFieldNameCommandType       := 'p';
                   FFieldNameComputerName      := 'q';
                   FFieldNameUserName          := 'r';

                   // Request Query_Param table
                   FTableNameQueryParam        := 's';
                   FFieldNameTableName         := 't';
                   FFieldNameParamName         := 'u';
                   FFieldNameParamKind         := 'v';
                   FFieldNameParamValue        := 'w';

                   // Response message table
                   FTableNameResultMessage     := 'x';
                   FFieldNameResultError       := 'y';
                   FFieldNameResultRowCount    := 'z';

                   // MetaData structure
                   FTableNameMetaData          := 'aa';
                   FFieldNameMetaDataTableName := 'ab';
                   FFieldNameMetaDataFieldName := 'ac';
                   FFieldNameMetaDataFieldKind := 'ad';
                   FFieldNameMetaDataFieldWidth := 'ae';

                   // Response set table
                   FTableNameResultSet         := 'af';
                 end;
  else
    raise Exception.Create(cErrorInvalidOptXMLDBSizeValue);
  end;
  DeriveInternalValues;
end;


function TtiXMLTags.MakeXMLDoc(const AString : string): string;
begin
  result :=
    FDocHeader +
    tiXMLTag(FDocData) +
    FDatabaseDetails +
    AString +
    tiXMLTagEnd(FDocData);
end;

function TtiXMLTags.MakeXMLDatabase : string;
begin
  Result :=
    MakeXMLDoc(
    tiXMLTag(FTables) +
    tiXMLTagEnd(FTables));
end;


procedure TtiXMLTags.SetOptXMLDBSizeAsString(const AValue: string);
begin
  FOptXMLDBSize := tiStringToOptXMLDBSize(AValue);
end;


function TtiXMLTags.GetOptXMLDBSizeAsString: string;
begin
  Result := cOptXMLDBSize[FOptXMLDBSize];
end;


initialization
  gDefaultOptXMLDBSize := optDBSizeOff;


end.
