program TestParseFile;

{$APPTYPE CONSOLE}

uses
  libxml2,
  SysUtils;

function myParseFile(aFileName: PxmlChar): xmlDocPtr;
var
  ctxt: xmlParserCtxtPtr;
begin
  Result := nil;
  xmlInitParser();
  ctxt := xmlCreateFileParserCtxt(aFileName);
  if (ctxt = nil) then exit;
  xmlParseDocument(ctxt);
  if (ctxt.wellFormed<>0) then begin
    Result := ctxt.myDoc;
  end else begin
    xmlFreeDoc(ctxt.myDoc);
    ctxt.myDoc := nil;
  end;
  xmlFreeParserCtxt(ctxt);
end;


procedure TestXmlDocParseFile(aFileName: string);
var
  doc: xmlDocPtr;
begin
  Write(aFilename, ': ');
  doc := myParseFile(PChar(aFileName));
  if (doc=nil) then begin
    WriteLN('File is NOT well-formed');
  end else begin
    WriteLN('File is well-formed');
    xmlFreeDoc(doc);
  end;
end;

var
  i: integer;
begin
  WriteLN('TestXmlDocParseFile');
  if (ParamCount=0) then begin
    WriteLN('Usage: ', ParamStr(0),' <filename> [<filename>]');
  end else begin
    for i:=1 to ParamCount do begin
      TestXmlDocParseFile(ParamStr(i));
    end;
  end;
end.

