unit tiExcept;

{$I tiDefines.inc}

interface
uses
  SysUtils
 ;

const
  cTIOPFExcMsgCanNotConnectToDatabase = 'Can not connect to database';
  cTIOPFExcMsgCanNotFindDatabase      = 'Can not find database';
  cTIOPFExcMsgInvalidUserNamePassword = 'Invalid username and password combination';
  cTIOPFExcMsgCanNotCreateDatabase    = 'Can not create database';
  cTIOPFExcMsgDatabaseAlreadyExists   = 'Database already exists';
  cTIOPFExcCanNotDetermineSQLType     = 'Can not determine the type of the SQL (SELECT, UPDATE, ALTER, etc)'#13#13'%s';
  cTIOPFExcMsgTIQueryType             = 'Invalid TtiQueryType';
  cTIOPFExcMsgInvalidExecutionPath    = 'Invalid execution path. You should not be in this part of the program. Code is being executed that has not been implemented.';

type

  // Abstract tiOPF Exception
  EtiOPFException = class(Exception)
 ;

  // Abstract exception for tiOPF internal errors
  EtiOPFInternalException = class(ETIOPFException)
 ;

  // Abstract exception for tiOPF errors, caused by a programmer using the framework
  EtiOPFProgrammerException = class(ETIOPFException)
 ;

  // Abstract exception for errors caused by bad user input
  EtiOPFUserException = class(ETIOPFException)
 ;

  // Abstract exception for errors caused by bad data
  EtiOPFDataException = class(ETIOPFException)
 ;

  // Error in the file system
  EtiOPFFileSystemException = class(ETIOPFException)
 ;

  // For providing user feedback. Not to be trapped by MadExcept
  EtiOPFUserFeedbackException = class(ETIOPFException)
 ;

  EtiOPFDBException = class(EtiOPFException)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); virtual;
  end;

  // Can not connect to database, reason unknown
  EtiOPFDBExceptionCanNotConnect = class(EtiOPFDBException)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); override;
  end;

  // Can not find the database
  EtiOPDDBCanNotFindException = class(EtiOPFDBExceptionCanNotConnect)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); override;
  end;

  // Invalid user name or password
  EtiOPFDBExceptionUserNamePassword = class(EtiOPFDBExceptionCanNotConnect)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); override;
  end;

  // Could not create a database
  EtiOPFDBExceptionCanNotCreateDatabase = class(EtiOPFDBException)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); override;
  end;

  // Could not create the database because it already exists
  EtiOPFDBExceptionAlreadyExists = class(EtiOPFDBException)
    constructor Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = ''); override;
  end;

  EtiOPFDBExceptionWrongServerVersion = class(EtiOPFDBException);

  procedure tiMailBugReport(E: Exception); overload;
  procedure tiMailBugReport(const AText: string); overload;

implementation
uses
  tiUtils
  {$IFDEF madexcept}
  ,MadExcept
  ,Windows
  {$ENDIF}
 ;

{ EtiDBAlreadyExists }

constructor EtiOPFDBExceptionAlreadyExists.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  inherited Create(APersistenceLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message :=
    cTIOPFExcMsgDatabaseAlreadyExists + Cr +
    Message;
end;

{ EtiDBFailedCreatingDatabase }

constructor EtiOPFDBExceptionCanNotCreateDatabase.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  inherited Create(APersistenceLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message :=
    cTIOPFExcMsgCanNotCreateDatabase + Cr +
    Message;
end;

{ EtiDBException }

constructor EtiOPFDBException.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  Message :=
    'Database name:       ' + ADatabaseName + Cr +
    'User name:           ' + AUserName     + Cr +
    'Password:            ' + tiReplicate('X', Length(APassword)) + Cr +
    'Persistence layer:   ' + APersistenceLayerName;
  if AMessage <> '' then
    Message := Message + Cr(2) +
      'Message:' + Cr+ AMessage;
end;

{ EtiDBCanNotConnectToDBException }

constructor EtiOPFDBExceptionCanNotConnect.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  inherited Create(APersistenceLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message :=
    cTIOPFExcMsgCanNotConnectToDatabase + Cr(2) +
    Message;
end;

{ EtiDBCanNotFindException }

constructor EtiOPDDBCanNotFindException.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  inherited Create(APersistenceLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message :=
    cTIOPFExcMsgCanNotFindDatabase + Cr +
    Message;
end;

{ EtiDBUserNamePasswordException }

constructor EtiOPFDBExceptionUserNamePassword.Create(const APersistenceLayerName, ADatabaseName, AUserName, APassword : string; const AMessage: string = '');
begin
  inherited Create(APersistenceLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message :=
    cTIOPFExcMsgInvalidUserNamePassword + Cr +
    Message;
end;

procedure tiMailBugReport(E: Exception);
{$IFDEF madexcept}
  var
    ls: string;
{$ENDIF}
begin
  Assert(E = E);  // Getting rid of compiler hints, param not used.
  {$IFDEF madexcept3}
  ls := MadExcept.CreateBugReport(etNormal, e, nil, GetCurrentThreadID);
  tiMailBugReport(ls);
  {$ELSE}
  {$IFDEF madexcept}
  ls := MadExcept.CreateBugReport(False, e, nil, GetCurrentThreadID, 0, 0, False, nil);
  tiMailBugReport(ls);
  {$ENDIF}
  {$ENDIF}
end;

procedure tiMailBugReport(const AText: string);
begin
  Assert(AText = AText);  // Getting rid of compiler hints, param not used.
  {$IFDEF madexcept3}
  MadExcept.AutoSendBugReport(AText, nil);
  {$ELSE}
  {$IFDEF madexcept}
  MadExcept.AutoMailBugReport(AText, '');
  {$ENDIF}
  {$ENDIF}
end;

end.


