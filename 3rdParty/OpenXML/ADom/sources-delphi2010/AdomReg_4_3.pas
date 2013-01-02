unit AdomReg_4_3;

  {$IFDEF WIN32}
    {$IFNDEF VER140}
      {$DEFINE MSWINDOWS}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN16}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
  {$IFDEF VER140}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER150}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER160}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER170}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER180}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER185}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER190}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER200}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER210}
    {$DEFINE VER140+}
  {$ENDIF}
  {$IFDEF VER220}
    {$DEFINE VER140+}
  {$ENDIF}

interface
uses
  {$IFDEF LINUX}
    DesignIntf, DesignEditors,
  {$ELSE}
    {$IFDEF VER140+} // Delphi 7 up
      {$IFDEF CLR}
        Borland.Vcl.Design.DesignIntf, Borland.Vcl.Design.DesignEditors,
        // If you encounter a compile error here, you must manually add
        // Borland.Studio.Vcl.Design.dll to the 'Requires' section of the
        // ADOM package by right-clicking on the 'Requires' folder and
        // selecting 'Add Reference...', etc.
      {$ELSE}
        DesignIntf, DesignEditors,
      {$ENDIF}
    {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  AdomPropertyEditor_4_3, dkAdomCore_4_3, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ADOM 4.3',[TDomImplementation,
                                 TStandardResourceResolver,
                                 TXmlToDomParser,
                                 TDomToXmlParser,
                                 TDtdToDtdModelParser,
                                 TXmlStandardDocReader,
                                 TXmlStandardDtdReader,
                                 TXmlStandardDomReader,
                                 TXmlStandardHandler,
                                 TXmlDistributor,
                                 TXmlWFTestHandler,
                                 TXmlNamespaceSignalGenerator,
                                 TXmlDomBuilder,
                                 TXmlDtdModelBuilder,
                                 TXmlStreamBuilder,
                                 TXPathExpression ]);
  RegisterPropertyEditor(TypeInfo(TXmlHandlers), TXmlDistributor, '', THandlerListProperty);
end;

end.
