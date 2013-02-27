unit libxml2i;

interface

uses
  libxml2;

type
  (**
   * This interface is intended for libxml2 wrappers. It provides a way
   * back - i.e. from the wrapper object to the libxml2 node.
   *)
  ILibXml2Node = interface ['{1D4BD646-0AB9-4810-B4BD-7277FB0CFA30}']
    function  LibXml2NodePtr: xmlNodePtr;
  end;

implementation

end.
