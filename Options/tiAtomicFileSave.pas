unit tiAtomicFileSave;

{$I tiDefines.inc}

interface

uses
  Classes,
  Contnrs,
  tiBaseObject,
  tiStreams,
  tiExcept;

type

  TtiAtomicFileSaveResourceAbs = class;

  {:@summary Write 1..n files as an atomic action
    @desc If one of the files can not be written, then non shall be written.
          Any of the files exist and can not be overridden, no files shall be written.
          If files exist, then they shall not be modified unless all the files can be written.
    @longCode(#
procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_CodeSample;
var
  LAtomicFileSave: TtiAtomicFileSave;
  LMemoryStream: TMemoryStream;
  LPreSizedStream: TtiPreSizedStream;
begin
  LAtomicFileSave:= nil;
  LMemoryStream:= nil;
  LPreSizedStream:= nil;
  try
    LAtomicFileSave:= TtiAtomicFileSave.Create;
    LMemoryStream:= TMemoryStream.Create;
    LAtomicFileSave.AddResource('MemoryStreamFile.txt', LMemoryStream);

    LPreSizedStream:= TtiPreSizedStream.Create(256, 256);
    LAtomicFileSave.AddResource('PreSizedStreamFile.txt', LPreSizedStream);

    // If one of the files is inaccessable or locked by another application, an
    // exception will be raised here.
    // If the file exists, then StartTransaction will place a lock on the file
    // so it can't be accessed by another application. If the file does not
    // exist, StartTransaction will create it with a lock.
    LAtomicFileSave.StartTransaction;

    // Write to the memory stream, or presized stream as you would normally.
    // Data is written to an in-memory buffer
    tiStringToStream('Testing the memory stream within a TtiAtomicFileSave(er)', LMemoryStream);
    LPreSizedStream.Write('Testing the presized stream within a TtiAtomicFileSave(er)');

    // Commit will write from the memory buffer to the on disk file.
    LAtomicFileSave.Commit;

  finally

    // If you call Free on the TtiAtomicFileSave without calling Commit, any
    // stub files created for the purpose of locking are deleted.
    LAtomicFileSave.Free;

    // The resources used for the writing (memory stream or presized stream) are
    // managed outside the TtiAtomicFileSave so you must free them your self.
    LMemoryStream.Free;
    LPreSizedStream.Free;
  end;

end;
     #)}
  TtiAtomicFileSave = class(TtiBaseObject)
  private
    FList: TObjectList;
    FInTransaction: boolean;
    function Error: boolean;
    function ErrorMessage: string;
    function SingleLineErrorMessage: string;
    function MultiLineErrorMessage: string;
    procedure Rollback;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddResource(const AFileName: string; const AStream: TMemoryStream); overload;
    procedure   AddResource(const AFileName: string; const AStream: TtiPreSizedStream); overload;
    function    FindByFileName(const AFileName: string): TtiAtomicFileSaveResourceAbs;
    function    IsLocked(const AFileName: string): boolean;
    procedure   StartTransaction;
    procedure   Commit;
  end;

  EtiOPFUserFeedbackExceptionCanNotLockFiles = class(EtiOPFUserFeedbackException);

  TtiAtomicFileSaveResourceAbs = class(TtiBaseObject)
  private
    FFileName: string;
    FLockStream: TFileStream;
    FHadToCreateFile: boolean;
    FError: boolean;
  private
    function  GetDirectoryName: string;
    procedure CreateDirectory;
    procedure LockExisting;
    procedure LockNew;
  public
    constructor Create;
    destructor Destroy; override;
    property  FileName: string read FFileName write FFileName;
    property  DirectoryName: string read GetDirectoryName;
    property  LockStream: TFileStream read FLockStream;
    property  Error: boolean read FError write FError;
    procedure StartTransaction;
    procedure Commit; virtual; abstract;
    procedure Rollback;
    procedure UnLock;
  end;

  TtiAtomicFileSaveResourceStream = class(TtiAtomicFileSaveResourceAbs)
  private
    FStream: TMemoryStream; // Owned outside TtiAtomicFileSave
  public
    property Stream: TMemoryStream read FStream write FStream;
    procedure Commit; override;
  end;

  TtiAtomicFileSaveResourcePreSizedStream = class(TtiAtomicFileSaveResourceAbs)
  private
    FStream: TtiPreSizedStream; // Owned outside TtiAtomicFileSave
  public
    property Stream: TtiPreSizedStream read FStream write FStream;
    procedure Commit; override;
  end;

const
  CErrorDescriptionSingleFile = 'Sorry, I can not write to the file "%s". It looks like you don''t have write access to the location or the file may be open with another application.';
  CErrorDescriptionMultiFile = 'Sorry, I can not write to the files below. It looks like you don''t have write access to the location or the file may be open with another application:';

implementation
uses
  tiUtils,
  SysUtils;

{ TtiAtomicFileSave }

procedure TtiAtomicFileSave.AddResource(const AFileName: string;
  const AStream: TMemoryStream);
var
  LItem: TtiAtomicFileSaveResourceStream;
begin
  LItem:= TtiAtomicFileSaveResourceStream.Create;
  LItem.FileName:= AFileName;
  LItem.Stream:= AStream;
  FList.Add(LItem);
end;

procedure TtiAtomicFileSave.AddResource(const AFileName: string;
  const AStream: TtiPreSizedStream);
var
  LItem: TtiAtomicFileSaveResourcePreSizedStream;
begin
  LItem:= TtiAtomicFileSaveResourcePreSizedStream.Create;
  LItem.FileName:= AFileName;
  LItem.Stream:= AStream;
  FList.Add(LItem);
end;

procedure TtiAtomicFileSave.Commit;
var
  i: integer;
begin
  for i := 0 to FList.Count-1 do
    (FList.Items[i] as TtiAtomicFileSaveResourceAbs).Commit;
  if Error then
  begin
    raise EtiOPFUserFeedbackException.Create(ErrorMessage);
    // ToDo: Implement rollback
  end;
  for i := 0 to FList.Count-1 do
    (FList.Items[i] as TtiAtomicFileSaveResourceAbs).Unlock;
  FInTransaction:= false;
end;

constructor TtiAtomicFileSave.Create;
begin
  inherited;
  FList:= TObjectList.Create(True);
  FInTransaction:= false;
end;

destructor TtiAtomicFileSave.Destroy;
begin
  if FInTransaction then
    Rollback;
  FList.Free;
  inherited;
end;

function TtiAtomicFileSave.Error: boolean;
var
  i: integer;
begin
  result:= false;
  for i := 0 to FList.Count-1 do
    if (FList.Items[i] as TtiAtomicFileSaveResourceAbs).Error then
    begin
      result:= true;
      Exit; //==>
    end;
end;

function TtiAtomicFileSave.ErrorMessage: string;
begin
  if FList.Count = 1 then
    result:= SingleLineErrorMessage
  else
    result:= MultiLineErrorMessage;
end;

function TtiAtomicFileSave.FindByFileName(
  const AFileName: string): TtiAtomicFileSaveResourceAbs;
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
    if SameText((FList.Items[i] as TtiAtomicFileSaveResourceAbs).FileName, AFileName) then
    begin
      result:= FList.Items[i] as TtiAtomicFileSaveResourceAbs;
      Exit; //==>
    end;
  result:= nil;
end;

function TtiAtomicFileSave.IsLocked(const AFileName: string): boolean;
begin
  result:= FindByFileName(AFileName) <> nil;
end;

function TtiAtomicFileSave.MultiLineErrorMessage: string;
var
  i: integer;
  LFileNameList: string;
begin
  Assert(FList.Count > 1, 'FList.Count <= 1');
  LFileNameList:= '';
  for i := 0 to FList.Count-1 do
    if (FList.Items[i] as TtiAtomicFileSaveResourceAbs).Error then
    begin
      if LFileNameList <> '' then
        LFileNameList:= LFileNameList + CrLf;
      LFileNameList:= LFileNameList + '  ' + (FList.Items[i] as TtiAtomicFileSaveResourceAbs).FileName;
    end;
  result:=
    CErrorDescriptionMultiFile + CrLf +
      LFileNameList;
end;

procedure TtiAtomicFileSave.Rollback;
var
  i: integer;
begin
  for i := 0 to FList.Count-1 do
    (FList.Items[i] as TtiAtomicFileSaveResourceAbs).Rollback;
  if Error then
    raise EtiOPFUserFeedbackException.Create(ErrorMessage);
  FInTransaction:= false;
end;

function TtiAtomicFileSave.SingleLineErrorMessage: string;
begin
  Assert(FList.Count = 1, 'FList.Count <> 1');
  result:=
    Format(CErrorDescriptionSingleFile, [(FList.Items[0] as TtiAtomicFileSaveResourceAbs).FileName]);
end;

procedure TtiAtomicFileSave.StartTransaction;
var
  i: integer;
begin
  for i := 0 to FList.Count-1 do
    (FList.Items[i] as TtiAtomicFileSaveResourceAbs).StartTransaction;
  if Error then
    raise EtiOPFUserFeedbackException.Create(ErrorMessage);
  FInTransaction:= true;
end;

{ TtiAtomicFileSaveResourceAbs }

constructor TtiAtomicFileSaveResourceAbs.Create;
begin
  inherited;
  FError:= false;
end;

procedure TtiAtomicFileSaveResourceAbs.CreateDirectory;
begin
  try
    tiForceDirectories(DirectoryName);
  except
    on e:Exception do
      Error:= True;
  end;
end;

destructor TtiAtomicFileSaveResourceAbs.Destroy;
begin
  FLockStream.Free;
  inherited;
end;

function TtiAtomicFileSaveResourceAbs.GetDirectoryName: string;
begin
  result:= ExtractFilePath(FileName);
end;

procedure TtiAtomicFileSaveResourceAbs.LockExisting;
begin
  try
    FLockStream:= TFileStream.Create(FileName, fmOpenWrite or fmShareExclusive);
    FHadToCreateFile:= False;
  except
    on e:Exception do
      Error:= True;
  end;
end;

procedure TtiAtomicFileSaveResourceAbs.LockNew;
begin
  try
    FLockStream:= TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    FHadToCreateFile:= True;
  except
    on e:EFCreateError do
      Error:= True;
  end;
end;

procedure TtiAtomicFileSaveResourceAbs.Rollback;
begin
  UnLock;
  if FHadToCreateFile then
    tiDeleteFile(FileName);
end;

procedure TtiAtomicFileSaveResourceAbs.StartTransaction;
begin
  if not DirectoryExists(DirectoryName) then
    CreateDirectory;
  if not Error then
  begin
    if FileExists(FileName) then
      LockExisting
    else
      LockNew;
  end;
end;

procedure TtiAtomicFileSaveResourceAbs.UnLock;
begin
  FreeAndNil(FLockStream);
end;

{ TtiAtomicFileSaveResourceStream }

procedure TtiAtomicFileSaveResourceStream.Commit;
begin
  try
    tiCopyStream(Stream, LockStream);
  except
    on E: EWriteError do
      Error := True;
  end;
end;

{ TtiAtomicFileSaveResourcePreSizedStream }

procedure TtiAtomicFileSaveResourcePreSizedStream.Commit;
begin
  try
    Stream.AssignTo(LockStream);
  except
    on E: EWriteError do
      Error := True;
  end;
end;

end.

