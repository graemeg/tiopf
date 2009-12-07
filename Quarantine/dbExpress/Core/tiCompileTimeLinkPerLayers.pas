unit tiCompileTimeLinkPerLayers;

interface
uses
  // These defines are cloned in tiDefines.inc
  tiQuery
  {$IFDEF LINK_XML}          ,tiQueryXML          {$ENDIF}
  {$IFDEF LINK_IBX}          ,tiQueryIBX          {$ENDIF}
  {$IFDEF LINK_BDEPARADOX}   ,tiQueryBDEParadox   {$ENDIF}
  {$IFDEF LINK_ADOACCESS}    ,tiQueryADOAccess    {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER} ,tiQueryADOSQLServer {$ENDIF}
  {$IFDEF LINK_CSV}          ,tiQueryCSV          {$ENDIF}
  {$IFDEF LINK_TAB}          ,tiQueryTAB          {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}     ,tiQueryXMLLight     {$ENDIF}
  {$IFDEF LINK_DOA}          ,tiQueryDOA          {$ENDIF}
  {$IFDEF LINK_REMOTE}       ,tiQueryRemote       {$ENDIF}
  {$IFDEF LINK_DBEPOSTGRESQL},tiQueryDBEPostgreSQL{$ENDIF}
  ;

implementation
uses
   tiOPFManager
  ,tiConstants
  ;

initialization
  // This works if there is only one persistence layer linked in via a compiler
  // directive, but not if there are more.
  // Have added this code to solve the problem of forcing the correct default per layer
  // when the remote layer is pulled in from code, and a SQL layer is required as well.
  {$IFDEF LINK_XML}          gTIOPFManager.DefaultPerLayerName := cTIPersistXML;          {$ENDIF}
  {$IFDEF LINK_IBX}          gTIOPFManager.DefaultPerLayerName := cTIPersistIBX;          {$ENDIF}
  {$IFDEF LINK_BDEPARADOX}   gTIOPFManager.DefaultPerLayerName := cTIPersistBDEParadox;   {$ENDIF}
  {$IFDEF LINK_ADOACCESS}    gTIOPFManager.DefaultPerLayerName := cTIPersistADOAccess;    {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER} gTIOPFManager.DefaultPerLayerName := cTIPersistADOSQLServer; {$ENDIF}
  {$IFDEF LINK_CSV}          gTIOPFManager.DefaultPerLayerName := cTIPersistCSV;          {$ENDIF}
  {$IFDEF LINK_TAB}          gTIOPFManager.DefaultPerLayerName := cTIPersistTAB;          {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}     gTIOPFManager.DefaultPerLayerName := cTIPersistXMLLight;     {$ENDIF}
  {$IFDEF LINK_DOA}          gTIOPFManager.DefaultPerLayerName := cTIPersistDOA;          {$ENDIF}
  {$IFDEF LINK_REMOTE}       gTIOPFManager.DefaultPerLayerName := cTIPersistRemote;       {$ENDIF}
  {$IFDEF LINK_DBEPOSTGRESQL}gTIOPFManager.DefaultPerLayerName := cTIPersistDBEPostgreSQL;{$ENDIF}
end.
