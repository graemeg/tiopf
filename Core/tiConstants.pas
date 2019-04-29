unit tiConstants;

{$I tiDefines.inc}

interface

const
  cTIOPFVersion           = '3.000';
  // There are quite a few of these now, so keep them sorted in alapha order
  cTIPersistADOAccess     = 'ADOAccess';
  cTIPersistADOSQLServer  = 'ADOSQLServer';
  cTIPersistBDEParadox    = 'BDEParadox';
  cTIPersistCrSdac        = 'CrSdac';
  cTIPersistCSV           = 'CSV';
  cTIPersistDOA           = 'DOA'; CtiQueryOptionDOAReadBuffer = 'ReadBuffer';
  cTIPersistFBL           = 'FBL';          // Free Pascal only
  cTIPersistFIBP          = 'FIBP';         // FIBPlus
  cTIPersistIBO           = 'IBO';
  cTIPersistIBX           = 'IBX';
  cTIPersistRemote        = 'Remote';
  cTIPersistSqldbIB       = 'Sqldb_IB';     // Free Pascal equivalent to Delphi dbExpress
  cTIPersistSqldbPQ       = 'Sqldb_PQ';
  cTIPersistSqldbOracle   = 'Sqldb_Oracle';
  cTIPersistSqldbSQLite3  = 'Sqldb_SQLite3';
  cTIPersistSqldbODBC     = 'Sqldb_ODBC';
  cTIPersistSqldbMySQL40  = 'Sqldb_MySQL40';
  cTIPersistSqldbMySQL41  = 'Sqldb_MySQL41';
  cTIPersistSqldbMySQL50  = 'Sqldb_MySQL50';
//  cTIPersistSqldbMySQL51  = 'Sqldb_MySQL51';
  cTIPersistSqldbMySQL55  = 'Sqldb_MySQL55';
  cTIPersistTAB           = 'TAB';
  cTIPersistXML           = 'XML';          // ToDo: Rename this MSXML
  cTIPersistXMLLight      = 'XMLLight';
  cTIPersistZeosFB        = 'Zeos_FB';
  cTIPersistZeosMySQL41   = 'Zeos_MySQL41';
  cTIPersistZeosMySQL50   = 'Zeos_MySQL50';
  cTIPersistDBISAM4       = 'DBISAM4';
  cTIPersistAsqlite3      = 'Asqlite3';
  cTIPersistUIB_EB        = 'UIB_EB';  // Embedded server
  cTIPersistUIB_FB        = 'UIB_FB';  // Firebird server
  cTIPersistUIB_IB        = 'UIB_IB';  // Interbase server

  cTIPersistPackageRootName = 'tiPersist';
  // When multiple database names are provided to GTIOPFManager.DetectAndConnect()
  // they will be separated by this character.
  cDatabaseNameDelim        = ';';
  CPasswordMasked = 'Password masked from view';

  CTIErrorInternal = 'tiOPF Internal Error: ';
  CTIErrorInvalidObject = CTIErrorInternal + ' TtiBaseObject.TestValid failed';

  // Constants for the remote persistence layer
  CDefaultPort                            = '80';
  CLocalHost                              = 'http://localhost:' + CDefaultPort;
  CRemoteServerMainFormCaption            = 'TechInsite DB Proxy Server';
  CTIDBProxyServiceName                   = 'tiDBProxyServer';
  CDefaultStatefulDBConnectionPoolTimeOut = 1; // Minute
  CSecToTimeOutLocked                     = 999999;

  CTIProtocolHTTP = 'http';
  CTIProtocolFile = 'file';
  CTIProtocolSelf = 'self';
  CTIProtocolMailTo = 'mailto';

{$IFDEF DELPHIXE103}
  cPackageSuffix = '330';
  cCompilerName = 'Delphi XE10.3 Rio';
{$ENDIF}

{$IFDEF DELPHIXE102}
  cPackageSuffix = '320';
  cCompilerName = 'Delphi XE10.2 Tokyo';
{$ENDIF}

{$IFDEF DELPHIXE101}
  cPackageSuffix = '310';
  cCompilerName = 'Delphi XE10.1 Berlin';
{$ENDIF}

{$IFDEF DELPHIXE10}
  cPackageSuffix = '300';
  cCompilerName = 'Delphi XE10 Seattle';
{$ENDIF}

{$IFDEF DELPHIXE8}
  cPackageSuffix = '290';
  cCompilerName = 'Delphi XE8';
{$ENDIF}

{$IFDEF DELPHIXE7}
  cPackageSuffix = '280';
  cCompilerName = 'Delphi XE7';
{$ENDIF}

{$IFDEF DELPHIXE6}
  cPackageSuffix = '270';
  cCompilerName = 'Delphi XE6';
{$ENDIF}

{$IFDEF DELPHIXE5}
  cPackageSuffix = '260';
  cCompilerName = 'Delphi XE5';
{$ENDIF}

{$IFDEF DELPHIXE4}
  cPackageSuffix = '250';
  cCompilerName = 'Delphi XE4';
{$ENDIF}

{$IFDEF DELPHIXE3}
  cPackageSuffix = '240';
  cCompilerName = 'Delphi XE3';
{$ENDIF}

{$IFDEF DELPHIXE2}
  cPackageSuffix = '230';
  cCompilerName = 'Delphi XE2';
{$ENDIF}

{$IFDEF DELPHIXE}
  cPackageSuffix = '220';
  cCompilerName = 'Delphi XE';
{$ENDIF}

{$IFDEF DELPHI2010}
  cPackageSuffix = '210';
  cCompilerName = 'Delphi 2010';
{$ENDIF}

{$IFDEF DELPHI2009}
  cPackageSuffix = '200';
  cCompilerName = 'Delphi 2009';
{$ENDIF}

{$IFDEF FPC}
  cPackageSuffix = {$I %fpcversion%};
  cCompilerName = 'Free Pascal';
{$ENDIF}

  // Compression constants
  cgsCompressNone = 'No compression';
  cgsCompressZLib = 'ZLib compression';

{$IFDEF MSWINDOWS}
  { example filename:  tiPersistCore60.bpl }
  cPackagePrefix       = '';
  cPackageExtension    = '.bpl';
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
  { example filename:  bpltiPersistCore.so }
  cPackagePrefix       = 'bpl';
  cPackageExtension    = '.so';
{$ENDIF}

  // Some parameter keys and values that may be used by TtiQueryRemote
  cINISystem              = 'System';
  cINIAppServer           = 'System'; // This can't be changed without changing the application launcher too
  cHTTPIndy               = 'HTTPIndy';
  cHTTPMSXML              = 'HTTPMSXML';
  cHTTPURL                = 'url';
  cHTTPConnectWith        = 'connectwith';
  cHTTPProxyServer        = 'proxyserver';
  cHTTPProxyServeractive  = 'proxyserveractive';
  cHTTPProxyServerName    = 'proxyservername';
  cHTTPProxyPort          = 'proxyport';
  CHTTPBlockSize          = 'blocksize';
  CHTTPRetryLimit         = 'retrylimit';
  CHTTPRetryWaitMS        = 'retrywaitms';
  CHTTPResolveTimeout     = 'resolvetimeout';
  CHTTPConnectTimeout     = 'connecttimeout';
  CHTTPSendTimeout        = 'sendtimeout';
  CHTTPReceiveTimeout     = 'receivetimeout';
  CHTTPSSLLibraryPath     = 'ssllibrarypath';

  CPathToCachedDataRoot       = 'CachedData';

  cTIOPFExcMsgConnectionConfirmationFormatWrong = 'Server did not return correctly formatted connection confirmation Expected <%s> Found <%s>';
  cTIOPFExcMsgWrongServerVersion = 'The client and server are incompatible versions.'#13#13 +
                                   'Please upgrade your client software or contact technical support.';
  cTIOPFExcMsgErrorOnServer = 'There was an unknown error on the application server. Please try again later.';
  cNullOIDInteger = 0;

  cNextOIDReadHigh = 'NextOIDReadHigh'; // Visitor constant

  {$IFDEF BOOLEAN_CHAR_1}
  cTrueDB  = 'T';
  cFalseDB = 'F';
  {$ELSE}
  cTrueDB  = 'TRUE';
  cFalseDB = 'FALSE';
  {$ENDIF}

  cTrueGUI = 'True';
  cFalseGUI = 'False';

  ciOK                = 0;
  ciRetry             = 1;
  ciAbort             = 2;

  cdtOneDay        = 1;
  cdtOneHour       = 1/24;
  cdtOneMinute     = 1/24/60;
  cdtOneSecond     = 1/24/60/60;
  cdtOneMiliSecond = 1/24/60/60/1000;

  cComma          = ',';
  cBackSpace      = #8;

  crMaxReal       = 9999999999;

  CNullDate       = 0.0;
  CNullDateTime   = 0.0;
  CMaxDateTime    = 2958465.99998843;  // 31/12/9999 23:59:59
  CMaxDate        = 2958465;
  CNullSQLDate    = 0;                 // 12/30/1899

  cgNullDate      = CNullDateTime;
  cgNullDateTime  = CNullDateTime;
  cgMinDateTime   = CNullDateTime;
  cgdtMaxDateTime = CMaxDateTime;  // 31/12/9999 23:59:59
  cgMaxDateTime   = cgdtMaxDateTime;
  cgdtMaxDate     = CMaxDate;
  crMaxDate       = CMaxDate;
  cgMinDate       = 0.0;
  cgMaxDate       = CMaxDate;

  { Summary of ISO 8601  http://www.cl.cam.ac.uk/~mgk25/iso-time.html }
  cIntlDateTimeStor = 'yyyymmdd"T"hhnnss';    // for storage
  CIntlDateStor     = 'yyyymmdd';             // for storage
  cIntlDateTimeDisp = 'yyyy-mm-dd hh:nn:ss';  // for display
  CIntlDateDisp     = 'yyyy-mm-dd';           // for display

  cgsComma        = ',';
  {$IFDEF DELPHI5} // These are defined from Delphi 5
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
  {$ENDIF}
  crZeroLimit      = 0.005;
  cCurrencyFormat0 = '$#,##0';
  cCurrencyFormat2 = '$#,##0.00';
  cgsNA            = 'N/A';

  cPI              = 3.14159265358979;
  cGoldenRatio     = 1618; // Forms should be sized in the ratio 1618 x 1000
  cCommandLineParamPause = 'pause'; // Handy in Console apps

  // Some INI file constants
  cINISectionSystem     = 'System';
  cINIIdentDatabaseName = 'DatabaseName';
  cINIIdentUserName     = 'UserName';
  cINIIdentPassword     = 'Password';
  cINIIdentPort         = 'Port';

  { Moved from tiUtils }
  csWinDateFormat     = 'dd/MM/yyyy';
  csWinTimeFormat     = 'hh:mm:ss';
  csWinDateTimeFormat = 'dd/MM/YYYY hh:mm:ss';
  {$IFDEF UNIX}
  AllFilesWildCard    = '*';
  {$ELSE}
  AllFilesWildCard    = '*.*';
  {$ENDIF}
  {$IFDEF UNIX}
  cLineEnding         = #10;    // Unix type OSes (Linux, *BSD, MacOSX ...)
  CMinFileDate        = 29221; {$MESSAGE 'ToDo: Supply correct value for min file date on Unix'}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  cLineEnding         = #13#10; // Windows
  CMinFileDate        = 29221;
  {$ENDIF}


  // Error messages
  CErrorDecodeNumError          = 'tiDecodeNum: <%s> does not represent a number in base %d.';
  CErrorUnableToCreateDirectory = 'Unable to create directory <%s>';
  CErrorRemoveDirectory         = 'Error removing <%s> from <%s>';
  CErrorInvalidVariantType      = 'Invalid variant type';
  CErrorXMLStringToDate         = 'Error converting string to date. String <%s> Error: %s';
  CErrorCanNotDeleteFile        = 'Can not delete file <%s>. It may be locked by another application.';
  CErrorCanNotCopyFile          = 'Unable to copy "%s" to "%s". System error code: "%d". System error message: "%s"';
  CErrorSettingFileDate         = 'Unable to set file date on: "%s" to "%s" System error code: "%d". System error message: "%s"';

  CUnknownGUI = 'Unknown' ;
  CUnknownDB = 'UNKNOWN';

  cBase           = 26;
  cZeroChar       = 'A';

  cuiBorder       = 16;
  cuiBtnBorder    =  8;
  cuiBtnHeight    = 25;
  cuiBtnWidth     = 75;
  cuiImageWidth   = 32;

  {: The maximum length a command passed to an console EXE can be. This length
     includes the exe path & name, and any parameters.}
  CMaximumCommandLineLength = 32768 - 5;
  {The 5 is a magic value to allow for pace between exe name and params,
   a null terminator and something else I don't quite understand.}
  {: When the command line parameter to be passed to a CGI extension excedes
     CMaximumCommandLineLength, then it's passed via a file. This flag is
     used to indicate a file name has been passed.}
  CCGIExtensionLargeParamFlag = 'CGIExtParamViaFile';

  // Structured CSV line prefixes
  CStructCSVPrefixI = 'I';
  CStructCSVPrefixD = 'D';

  CNoPathEntered = 'Please enter a path to save to.';
  CDirsEndingInSpace = 'Folder names cannot end in a space';
  CInvalidDrive = '%s is not a valid drive.';
  CInvalidDirName = '%s is not a valid directory name or you do not have write permission.';
  CInvalidFileName = '%s is not a valid file name.';
  CFileAccessDenied = '%s is locked by another application or you do not have permission to create it.';

implementation

end.
