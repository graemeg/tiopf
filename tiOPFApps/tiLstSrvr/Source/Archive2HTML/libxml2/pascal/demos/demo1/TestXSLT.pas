unit TestXSLT;

{
    ------------------------------------------------------------------------------
    Copyright:
        4commerce technologies AG
        Kamerbalken 10-14
        22525 Hamburg, Germany
    Published under a double license:
    a) the GNU Library General Public License: 
       http://www.gnu.org/copyleft/lgpl.html
    b) the Mozilla Public License:
       http://www.mozilla.org/MPL/MPL-1.1.html
    Please send corrections to: ufechner@4commerce.de
    ------------------------------------------------------------------------------
}

interface

// Parses an xml file
Procedure test1(filename:string);

// Transforms the xmlfile with the xslfile (stylesheet) into the outputfile
// The strings are the filenames
procedure test2(xmlfile,xslfile,outputfile: string);

implementation

uses
  libxml2,libxslt,MicroTime,conapp,SysUtils;

procedure test2(xmlfile,xslfile,outputfile: string);
var
  cur: xsltStylesheetPtr;
  doc: xmlDocPtr;
  res: xmlDocPtr;
  node: xmlNodePtr;
  params: ppchar;
  ok: integer;
  temp: string;
  compression:longint;
begin
  StartTimer;
  xmlSubstituteEntitiesDefault(1);
  params:=nil;
  cur:=xsltParseStylesheetFile(pchar(xslfile));
  if cur<>nil then begin
    outLog('Parsed Stylesheet ok!');
    outLog('Elapsed time: '+format('%8.1f',[EndTime*1000])+' ms');
  end
  else exit;
  doc:=xmlParseFile(pchar(xmlfile));
  if doc<>nil then begin
    outLog('Parsed File ok!');
    outLog('Elapsed time: '+format('%8.1f',[EndTime*1000])+' ms');
  end
  else exit;
  res:=xsltApplyStylesheet(cur,doc,params);
  if res<>nil then begin
    outLog('Applied Stylesheet ok!');
    outLog('Elapsed time: '+format('%8.1f',[EndTime*1000])+' ms');
  end
  else exit;
  node:=xmlDocGetRootElement(res);
  if node<>nil then begin
    temp:=node.name;
    outLog('name of root element: '+temp);
  end;
  //ok:=xmlSaveFile(pchar(outputfile),res);
  compression:=0;
  ok:=xsltSaveResultToFileName(pchar(outputfile), res, cur,compression);
  if ok<>-1 then begin
    outLog('Saved result ok!');
    outLog('Elapsed time: '+format('%8.1f',[EndTime*1000])+' ms');
  end
  else exit;
end;

Procedure test1(filename:string);
// An Example Programm for the direct use of libxml2
// (without the xmldom.pas interface)
var doc: xmlDocPtr;
    temp: string;
    //name: PGdomeDOMString;
    node: xmlNodePtr;
begin
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
  temp:=node.name;
  outLog('name of root element: '+temp);
  temp:=inttostr(node.type_);
  outLog('type of root element: '+temp);
  //xmlFreeNode(node);
  xmlFreeDoc(doc);
  outLog('');
  doc:=xmlNewDoc('1.0');
  if doc<>nil then begin
    outLog('Created empty document!');
  end;
  doc.children:=xmlNewDocNode(doc,nil,pchar('test'),nil);
  node:=xmlDocGetRootElement(doc);
  temp:=node.name;
  outLog('name of root element: '+temp);
  xmlFreeDoc(doc);
  xsltCleanupGlobals();
  xmlCleanupParser();
end;

end.
