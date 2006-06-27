unit tiExcept;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ;

const
  cTIOPFExcMsgCanNotConnectToDatabase = 'Can not connect to database' ;
  cTIOPFExcMsgCanNotFindDatabase      = 'Can not find database' ;
  cTIOPFExcMsgInvalidUserNamePassword = 'Invalid username and password combination' ;
  cTIOPFExcMsgCanNotCreateDatabase    = 'Can not create database' ;
  cTIOPFExcMsgDatabaseAlreadyExists   = 'Database already exists' ;
  cTIOPFExcCanNotDetermineSQLType     = 'Can not determine the type of the SQL (SELECT, UPDATE, ALTER, etc)'#13#13'%s';
  cTIOPFExcMsgTIQueryType             = 'Invalid TtiQueryType' ;

type

  // Abstract tiOPF Exception
  EtiOPFException = class( Exception )
  ;

  // Abstract exception for tiOPF internal errors
  EtiOPFInternalException = class( ETIOPFException )
  ;

  // Abstract exception for tiOPF errors, caused by a programmer using the framework
  EtiOPFProgrammerException = class( ETIOPFException )
  ;

  // Abstract exception for errors caused by bad user input
  EtiOPFUserException = class( ETIOPFException )
  ;

  // Abstract exception for errors caused by bad data
  EtiOPFDataException = class( ETIOPFException )
  ;

  // Error in the file system
  EtiOPFFileSystemException = class( ETIOPFException )
  ;

  // For providing user feedback. Not to be trapped by MadExcept
  EtiOPFUserFeedbackException = class( ETIOPFException )
  ;

  EtiOPFDBException = class( EtiOPFException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; virtual ;
  end ;

  // Can not connect to database, reason unknown
  EtiOPFDBExceptionCanNotConnect = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Can not find the database
  EtiOPDDBCanNotFindException = class( EtiOPFDBExceptionCanNotConnect )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Invalid user name or password
  EtiOPFDBExceptionUserNamePassword = class( EtiOPFDBExceptionCanNotConnect )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Could not create a database
  EtiOPFDBExceptionCanNotCreateDatabase = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Could not create the database because it already exists
  EtiOPFDBExceptionAlreadyExists = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  EtiOPFDBExceptionWrongServerVersion = class( EtiOPFDBException );

  procedure tiMailBugReport(E: Exception); overload ;
  procedure tiMailBugReport(const pText: string); overload ;

implementation
uses
  tiUtils
  {$IFDEF madexcept}
  ,MadExcept
  ,Windows
  {$ENDIF}
  ;

{ EtiDBAlreadyExists }

constructor EtiOPFDBExceptionAlreadyExists.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgDatabaseAlreadyExists + Cr +
    Message ;
end;

{ EtiDBFailedCreatingDatabase }

constructor EtiOPFDBExceptionCanNotCreateDatabase.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotCreateDatabase + Cr +
    Message ;
end;

{ EtiDBException }

constructor EtiOPFDBException.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  Message :=
    'Database name:       ' + pDatabaseName + Cr +
    'User name:           ' + pUserName     + Cr +
    'Password:            ' + tiReplicate( 'X', Length( pPassword )) + Cr +
    'Persistence layer:   ' + pPerLayerName ;
  if pMessage <> '' then
    Message := Message + Cr(2) +
      'Message:' + Cr+ pMessage ;
end;

{ EtiDBCanNotConnectToDBException }

constructor EtiOPFDBExceptionCanNotConnect.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotConnectToDatabase + Cr(2) +
    Message ;
end;

{ EtiDBCanNotFindException }

constructor EtiOPDDBCanNotFindException.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotFindDatabase + Cr +
    Message ;
end;

{ EtiDBUserNamePasswordException }

constructor EtiOPFDBExceptionUserNamePassword.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgInvalidUserNamePassword + Cr +
    Message ;
end;

procedure tiMailBugReport(E: Exception);
{$IFDEF madexcept}
  var
    ls: string;
{$ENDIF}
begin
  {$IFDEF madexcept}
  ls := MadExcept.CreateBugReport(False, e, nil, GetCurrentThreadID, 0, 0, False, nil);
  tiMailBugReport(ls);
  {$ENDIF}
end;

procedure tiMailBugReport(const pText: string);
begin
  {$IFDEF madexcept}
  MadExcept.AutoMailBugReport(pText, '');
  {$ENDIF}
end;

end.
