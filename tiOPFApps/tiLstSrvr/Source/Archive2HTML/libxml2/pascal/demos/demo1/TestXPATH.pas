unit TestXPATH;

{
Author:
Uwe Fechner <ufechner@4commerce.de>
copyright:
4commerce technologies AG
kamerbalken 10-14
22525 Hamburg
}

interface

procedure test1(filename, expr: string);

implementation

uses libxml2,MicroTime,conapp,SysUtils;

procedure test1(filename, expr: string);
// An Example Programm for the use of XPATH-expressions

var doc: xmlDocPtr;
    node: xmlNodePtr;
    ctxt: xmlXPathContextPtr;
    res:  xmlXPathObjectPtr;
    temp: string;
    type_: integer;
    number: double;
begin
  // read the input file
  //filename:='..\data\'+filename;
  StartTimer;
  doc:=xmlParseFile(pchar(filename));
  if doc<>nil then begin
    outLog('Parsed file ok!');
    outLog('Elapsed time: '+format('%8.1f',[EndTime*1000])+' ms');
  end
  else exit;
  node:=xmlDocGetRootElement(doc);
  if node=nil then exit;
  // xpath
  ctxt:=xmlXPathNewContext(doc);
  ctxt.node:=node;
  res:=xmlXPathEvalExpression(pchar(expr),ctxt);
  if res=nil then exit;
  type_:=res.type_;
  case type_ of
    XPATH_UNDEFINED:
      begin
        temp:='Undefined';
      end;
    XPATH_NODESET:
      begin
        temp:='Nodeset of '+inttostr(res.nodesetval.nodeNr)+' nodes';
      end;
    XPATH_BOOLEAN:
      begin
        if res.boolval=0
          then temp:='Boolean: false'
          else temp:='Boolean: true';
      end;
    XPATH_NUMBER:
      begin
        number:=res.floatval;
        temp:='Number: '+floattostr(number);
      end;
    XPATH_STRING:
      begin
        temp:='String: '+res.stringval;
      end;
    else
      begin
        temp:='???';
      end;
  end;
  outLog(temp);
  xmlXPathFreeObject(res);
  xmlXPathFreeContext(ctxt);
end;

end.
