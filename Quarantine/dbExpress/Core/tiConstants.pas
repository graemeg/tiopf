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

unit tiConstants;

interface
const
  cTIOPFVersion          = '1.405' ;
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
  cTIPersistDBEPostgreSQL = 'DBEPostgreSQL';

  cDBProxyServerTimeOut  = 1 ; // Minute

  cTIPersistPackageRootName = 'tiPersist' ;
  // When multiple database names are provided to gTIOPFManager.DetectAndConnect()
  // they will be separated by this character.
  cDatabaseNameDelim        = ';' ;

  cTIInternalError = 'tiOPF Internal Error: ' ;
  // Don't use this one, it's to allow old code to compile
  cTIInvalidObjectError = cTIInternalError + ' TtiBaseObject.TestValid failed' ;

  // Constants for the remote persistence layer
  cLocalHost = 'http://localhost:80' ;
  cRemoteServerMainFormCaption = 'TechIniste DB Proxy Server' ;
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

  // Compression constants
  cgsCompressNone = 'No compression' ;
  cgsCompressZLib = 'ZLib compression' ;

{$IFDEF KYLIX3}
  cPackageSuffix = 'K3' ;
{$ENDIF}

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
  cINISystem              = 'System' ;
  cINIAppServer           = 'System'; // This can't be changed without changing the application launcher too
  cHTTPIndy               = 'HTTPIndy' ;
  cHTTPMSXML              = 'HTTPMSXML';
  cHTTPURL                = 'url';
  cHTTPConnectWith        = 'connectwith';
  cHTTPProxyServer        = 'proxyserver';
  cHTTPProxyServeractive  = 'proxyserveractive';
  cHTTPProxyServerName    = 'proxyservername';
  cHTTPProxyPort          = 'proxyport';

  cTIOPFExcMsgConnectionConfirmationFormatWrong = 'Server did not return correctly formatted connection confirmation Expected <%s> Found <%s>';
  cTIOPFExcMsgWrongServerVersion = 'The client and server are incompatable versions.'#13#13 +
                                   'Please upgrade your software by following the'#13 +
                                   'instructions at %s'#13#13+
                                   '(When you click <OK>, your web browser will be launched pointing to the application''s web site.)';
  cTIOPFExcMsgErrorOnServer = 'There was an unknown error on the application server. Please try again later.';
  cNullOIDInteger = 0 ;

  cNextOIDReadHigh = 'NextOIDReadHigh' ; // Visitor constant

  {$IFDEF BOOLEAN_CHAR_1}
  cTrueDB  = 'T';
  cFalseDB = 'F';
  {$ELSE}
  cTrueDB  = 'TRUE';
  cFalseDB = 'FALSE';
  {$ENDIF}

  cTrueGUI = 'True';
  cFalseGUI = 'False';

  ciOK                = 0 ;
  ciRetry             = 1 ;
  ciAbort             = 2 ;

  cdtOneDay        = 1 ;
  cdtOneHour       = 1/24 ;
  cdtOneMinute     = 1/24/60 ;
  cdtOneSecond     = 1/24/60/60 ;
  cdtOneMiliSecond = 1/24/60/60/1000 ;

  cComma          = ',' ;
  cBackSpace      = #8 ;

  crMaxReal       = 9999999999 ;

  cNullDate       = 0.0 ;
  cNullDateTime   = 0.0 ;
  cgNullDate      = 0.0 ;
  cgNullDateTime  = 0.0 ;
  cgMinDateTime   = 0.0 ;
  cNullSQLDate    = '12/30/1899' ;
  cgdtMaxDateTime = 2958465.99998843 ; // 31/12/9999 23:59:59
  cgMaxDateTime   = cgdtMaxDateTime ;
  cMaxDateTime    = cgdtMaxDateTime ;
  cgdtMaxDate     = 2958465.0 ;
  crMaxDate       = cgdtMaxDate ;
  cMaxDate        = cgdtMaxDate ;
  cgMinDate       = 0.0 ;
  cgMaxDate       = 2958465.0 ;
  cgsComma        = ',' ;
  {$IFDEF DELPHI5} // These are defined from Delphi 5
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
  {$ENDIF}
  crZeroLimit      = 0.005 ;
  cCurrencyFormat0 = '$#,##0' ;
  cCurrencyFormat2 = '$#,##0.00' ;
  cgsNA            = 'N/A' ;

  cPI              = 3.14159265358979 ;

implementation

end.
