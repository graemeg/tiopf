{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit ctiPersist;

interface
const
  cTIOPFVersion          = '1.300' ;
  cTIPersistIBX          = 'IBX' ;
  cTIPersistBDEParadox   = 'BDEParadox' ;
  cTIPersistXML          = 'XML' ; // ReName this MSXML
  cTIPersistXMLLight     = 'XMLLight' ;
  cTIPersistCSV          = 'CSV' ;
  cTIPersistTAB          = 'TAB' ;
  cTIPersistIBO          = 'IBO' ;
  cTIPersistADOAccess    = 'ADOAccess' ;
  cTIPersistADOSQLServer = 'ADOSQLServer';
  cTIPersistDOA          = 'DOA' ;
  cTIPersistRemote       = 'Remote' ;
  cTIPersistSQLite       = 'SQLite' ;

  cDBProxyServerTimeOut  = 1 ; // Minute

  cTIPersistPackageRootName = 'tiPersist' ;
  // When multiple database names are provided to gTIPerMgr.DetectAndConnect()
  // they will be separated by this character.
  cDatabaseNameDelim        = ';' ;

  cTIInternalError = 'tiOPF Internal Error: ' ;
  cTIInvalidObjectError = cTIInternalError + 'Object.TestValid failed' ;

  // Constants for the remote persistence layer
  cLocalHost = 'http://localhost:80' ;
  cRemoteServerMainFormCaption = 'TechInsite DB Proxy Server' ;
  cTIDBProxyServiceName = 'tiDBProxyServer';
  cSecToTimeOutLocked = 999999 ;

{$IFDEF DELPHI5}
  cPackageSuffix = '' ;
{$ENDIF}

{$IFDEF DELPHI6}
  cPackageSuffix = '60' ;
{$ENDIF}

{$IFDEF DELPHI7}
  cPackageSuffix = '70' ;
{$ENDIF}

{$IFDEF KYLIX3}
  cPackageSuffix = 'K3' ;
{$ENDIF}

  // Compression constants
  cgsCompressNone = 'No compression' ;
  cgsCompressZLib = 'ZLib compression' ;

{$IFDEF MSWINDOWS}
  { example filename:  tiPersistCore60.bpl }
  cPackagePrefix       = '';
  cPackageExtension    = '.bpl';
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  { example filename:  bpltiPersistCoreK3.so }
  cPackagePrefix       = 'bpl';
  cPackageExtension    = '.so';
{$ENDIF LINUX}

  // Some parameter keys and values that may be used by TtiQueryRemote
  cINISystem              = 'system' ;
  cHTTPIndy               = 'HTTPIndy' ;
//  cHTTPWinINet            = 'HTTPWinINet';
  cHTTPMSXML              = 'HTTPMSXML';
  cHTTPURL                = 'url';
  cHTTPConnectWith        = 'connectwith';
  cHTTPProxyServer        = 'proxyserver';
  cHTTPProxyServeractive  = 'proxyserveractive';
  cHTTPProxyServerName    = 'proxyservername';
  cHTTPProxyPort          = 'proxyport';

  cTIOPFExcMsgConnectionConfirmationFormatWrong = 'Server did not return correctly formatted connection confirmation Expected <%s> Found <%s>';
  cTIOPFExcMsgWrongServerVersion = 'The client and server are incompatable versions.'#13#13 +
                                   'Please upgrade your software.';
  cTIOPFExcMsgErrorOnServer = 'There was an unknown error on the application server. Please try again later.';

implementation

end.
