The AdomParserEx unit implements the TXmlToDomParserEx component, a TXmlToDomParser descendant that implement convenient parsing methods for a couple of input formats, such as strings, WideStrings, and Streams.


METHOD DESCRIPTIONS
-------------------

TXmlToDomParserEx.StreamToDom(const Stream: TStream;
                              const SysId: WideString;
                              const CodecClass: TUnicodeCodecClass;
                              const InclDecl: Boolean): TDomDocument; virtual;

      Reads an XML document from a stream and parses
      it into a Document Object Model tree.

      Parameters:
      - Stream
         The stream to be parsed.
      - SysId
         The system identifier of the document, or an
         empty string if none is available.
      - CodecClass
         The codec class for the input stream if known, or 
         nil if unkown. If nil, the effective codec class 
         is determined with the help of the input stream's 
         byte order mark, its XML declaration, or is set
         to the default TUTF8Codec class, if no byte order 
         mark or encoding label was found.
      - InclDecl
         If True, location information includes any XML
         declaration at the beginning of the associated 
         stream.  If False, the location information is 
         calculated as if no XML declaration had been 
         found.

      Return Value:
         A newly created TDomDocument node containing the 
         resulting document tree of the parsing process.  
         The DocumentUri property of this document node is 
         set to the specified SysId value.  The document 
         node is attached to the TDomImplementation object 
         specified by the DOMImpl property at the time the 
         function is called -- which means that this
         document node is automatically freed, when the 
         TDomImplementation object is being destroyed. 
         However the document can be freed explicitly by 
         calling its Free method.

      Exceptions:
      - EAccessViolation
         Raised if there is no TDomImplementation component 
         associated with this TXmlToDomParser component, or
         if no stream is specified.
      - EParserException
         Raised if the parser stumbles over a non-wellformed
         entity in the XML document.  Note that not every
         violation of a wellformedness constraint as defined
         in [XML 1.0] can be detected by parsing a document.
         Some of them can only be discovered by validating
         the document.



TXmlToDomParserEx.StringToDom(const S: string;
                              const SysId: WideString;
                              const CodecClass: TUnicodeCodecClass;
                              const InclDecl: Boolean): TDomDocument; virtual;

      Reads an XML document from a string and parses
      it into a Document Object Model tree.

      Parameters:
      - S
         The string to be parsed.
      - SysId
         The system identifier of the document, or an
         empty string if none is available.
      - CodecClass
         The codec class for the input stream if known, or 
         nil if unkown. If nil, the effective codec class 
         is determined with the help of the input stream's 
         byte order mark, its text declaration or is set
         to the default TUTF8Codec class, if no byte order 
         mark was found.
      - InclDecl
         If True, location information includes any XML
         declaration at the beginning of the string. If 
         False, the location information is calculated as 
         if no XML declaration had been found.

      Return Value:
         A newly created TDomDocument node containing the 
         resulting document tree of the parsing process.  
         The DocumentUri property of this document node is 
         set to the specified SysId value.  The document 
         node is attached to the TDomImplementation object 
         specified by the DOMImpl property at the time the 
         function is called -- which means that this
         document node is automatically freed, when the 
         TDomImplementation object is being destroyed. 
         However the document can be freed explicitly by 
         calling its Free method.

      Exceptions:
      - EAccessViolation
         Raised if there is no TDomImplementation component 
         associated with this TXmlToDomParser component, or
         if the specified string is empty.
      - EParserException
         Raised if the parser stumbles over a non-wellformed
         entity in the XML document.  Note that not every
         violation of a wellformedness constraint as defined
         in [XML 1.0] can be detected by parsing a document.
         Some of them can only be discovered by validating
         the document.



TXmlToDomParserEx.WideStringToDom(const S: WideString;
                                  const SysId: WideString;
                                  const CodecClass: TUnicodeCodecClass;
                                  const InclDecl: Boolean): TDomDocument; virtual;

      Reads an XML document from a WideString and
      parses it into a Document Object Model tree.

      Parameters:
      - S
         The WideString to be parsed.
      - SysId
         The system identifier of the document, or an
         empty string if none is available.
      - CodecClass
         The codec class for the input stream if known, or 
         nil if unkown. If nil, the effective codec class 
         is determined with the help of the input stream's 
         byte order mark, its text declaration or is set
         to the default TUTF8Codec class, if no byte order 
         mark was found.  
         Hint: If an application wants to parse a 
         WideString encoded in UTF-16LE (the usual encoding
         used for WideStrings in ADOM), it must either
         set the CodecClass to TUTF16LECodec or S must 
         start with an UTF-16LE byte order mark (#$FEFF).         
      - InclDecl
         If True, location information includes any XML
         declaration at the beginning of the WideString.
         If False, the location information is calculated 
         as if no XML declaration had been found.

      Return Value:
         A newly created TDomDocument node containing the 
         resulting document tree of the parsing process.  
         The DocumentUri property of this document node is 
         set to the specified SysId value.  The document 
         node is attached to the TDomImplementation object 
         specified by the DOMImpl property at the time the 
         function is called -- which means that this
         document node is automatically freed, when the 
         TDomImplementation object is being destroyed. 
         However the document can be freed explicitly by 
         calling its Free method.

      Exceptions:
      - EAccessViolation
         Raised if there is no TDomImplementation component 
         associated with this TXmlToDomParser component, or
         if the specified WideString is empty.
      - EParserException
         Raised if the parser stumbles over a non-wellformed
         entity in the XML document.  Note that not every
         violation of a wellformedness constraint as defined
         in [XML 1.0] can be detected by parsing a document.
         Some of them can only be discovered by validating
         the document.