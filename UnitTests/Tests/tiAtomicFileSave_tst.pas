unit tiAtomicFileSave_tst;

{$I tiDefines.inc}

interface
uses
  tiTestFramework;

type

  TtiAtomicFileSaveTestCase = class(TtiTestCase)
  private
    function CanLock(const AFileName: string): boolean;
  published
    procedure AtomicFileSaveTestCase_AddResourceMemoryStream;
    procedure AtomicFileSaveTestCase_AddResourcePreSizedStream;
    procedure AtomicFileSaveTestCase_FindByFileName;
    procedure AtomicFileSaveTestCase_IsLocked;
    procedure AtomicFileSaveTestCase_StartTransactionMemoryStream;
    procedure AtomicFileSaveTestCase_StartTransactionPreSizedStream;

    procedure AtomicFileSaveTestCase_CommitMemoryStreamFileExists;
    procedure AtomicFileSaveTestCase_CommitMemoryStreamFileNotExists;
    procedure AtomicFileSaveTestCase_CommitPreSizedStreamFileExists;
    procedure AtomicFileSaveTestCase_CommitPreSizedStreamFileNotExists;

    procedure AtomicFileSaveTestCase_RollbackMemoryStreamFileExists;
    procedure AtomicFileSaveTestCase_RollbackMemoryStreamFileNotExists;
    procedure AtomicFileSaveTestCase_RollbackPreSizedStreamFileExists;
    procedure AtomicFileSaveTestCase_RollbackPreSizedStreamFileNotExists;
  end;

procedure RegisterTests;

implementation
uses
  tiTestDependencies,
  tiAtomicFileSave,
  tiUtils,
  tiStreams,
  Classes,
  SysUtils;

const
  CTestFileName1 = 'tiAtomicFileSaveTest1.txt';
  CTestFileName2 = 'tiAtomicFileSaveTest2.txt';

{ TAtomicFileSaveTestCase }

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiAtomicFileSaveTestCase);
end;

{ TtiAtomicFileSaveTestCase }

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_AddResourceMemoryStream;
var
  LO: TtiAtomicFileSave;
  LResource: TMemoryStream;
  LFileName: string;
  LItem: TtiAtomicFileSaveResourceAbs;
begin
  LFileName:= TempFileName(CTestFileName1);
  LO:= nil;
  LResource:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResource:= TMemoryStream.Create;
    LO.AddResource(LFileName, LResource);
    LItem:= LO.FindByFileName(LFileName);
    CheckNotNull(LItem);
    CheckIs(LItem, TtiAtomicFileSaveResourceStream);
    CheckEquals(LFileName, LItem.FileName);
    CheckSame(LResource, (LItem as TtiAtomicFileSaveResourceStream).Stream);
  finally
    LO.Free;
    LResource.Free;
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_AddResourcePreSizedStream;
var
  LO: TtiAtomicFileSave;
  LResource: TtiPreSizedStream;
  LFileName: string;
  LItem: TtiAtomicFileSaveResourceAbs;
begin
  LFileName:= TempFileName(CTestFileName1);
  LO:= nil;
  LResource:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResource:= TtiPreSizedStream.Create(2, 2);
    LO.AddResource(LFileName, LResource);
    LItem:= LO.FindByFileName(LFileName);
    CheckNotNull(LItem);
    CheckIs(LItem, TtiAtomicFileSaveResourcePreSizedStream);
    CheckEquals(LFileName, LItem.FileName);
    CheckSame(LResource, (LItem as TtiAtomicFileSaveResourcePreSizedStream).Stream);
  finally
    LO.Free;
    LResource.Free;
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_CommitMemoryStreamFileExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TMemoryStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  tiStringToFile('XXXXXXXXX', LFileName);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TMemoryStream.Create;
      LO.AddResource(LFileName, LResource);

      LO.StartTransaction;
      tiStringToStream('test', LResource);
      LO.Commit;

    finally
      LO.free;
      LResource.Free;
    end;
    CheckEquals('test', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_CommitMemoryStreamFileNotExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TMemoryStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TMemoryStream.Create;
      LO.AddResource(LFileName, LResource);

      LO.StartTransaction;
      tiStringToStream('test', LResource);
      LO.Commit;

    finally
      LO.free;
      LResource.Free;
    end;
    CheckEquals('test', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_CommitPreSizedStreamFileExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TtiPreSizedStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  tiStringToFile('XXXXXXXXX', LFileName);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TtiPreSizedStream.Create(2, 2);
      LO.AddResource(LFileName, LResource);

      LO.StartTransaction;
      LResource.Write('test');
      LO.Commit;

    finally
      LO.free;
      LResource.Free;
    end;
    CheckEquals('test', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_CommitPreSizedStreamFileNotExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TtiPreSizedStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TtiPreSizedStream.Create(2, 2);
      LO.AddResource(LFileName, LResource);

      LO.StartTransaction;
      LResource.Write('test');
      LO.Commit;

    finally
      LO.free;
      LResource.Free;
    end;
    CheckEquals('test', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_FindByFileName;
var
  LO: TtiAtomicFileSave;
  LResourcePreSizedStream: TtiPreSizedStream;
  LFileNamePreSizedStream: string;
  LItemPreSizedStream: TtiAtomicFileSaveResourceAbs;
  LResourceMemoryStream: TMemoryStream;
  LFileNameMemoryStream: string;
  LItemMemoryStream: TtiAtomicFileSaveResourceAbs;
begin
  LFileNamePreSizedStream:= TempFileName(CTestFileName1);
  LFileNameMemoryStream:= TempFileName(CTestFileName2);
  LO:= nil;
  LResourcePreSizedStream:= nil;
  LResourceMemoryStream:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResourcePreSizedStream:= TtiPreSizedStream.Create(2, 2);
    LResourceMemoryStream:= TMemoryStream.Create;
    LO.AddResource(LFileNamePreSizedStream, LResourcePreSizedStream);
    LO.AddResource(LFileNameMemoryStream, LResourceMemoryStream);

    LItemPreSizedStream:= LO.FindByFileName(LFileNamePreSizedStream);
    CheckNotNull(LItemPreSizedStream);
    CheckEquals(LFileNamePreSizedStream, LItemPreSizedStream.FileName);

    LItemMemoryStream:= LO.FindByFileName(LFileNameMemoryStream);
    CheckNotNull(LItemMemoryStream);
    CheckEquals(LFileNameMemoryStream, LItemMemoryStream.FileName);

  finally
    LO.free;
    LResourcePreSizedStream.free;
    LResourceMemoryStream.free;
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_IsLocked;
var
  LO: TtiAtomicFileSave;
  LResourcePreSizedStream: TtiPreSizedStream;
  LFileNamePreSizedStream: string;
  LResourceMemoryStream: TMemoryStream;
  LFileNameMemoryStream: string;
begin
  LFileNamePreSizedStream:= TempFileName(CTestFileName1);
  LFileNameMemoryStream:= TempFileName(CTestFileName2);

  LO:= nil;
  LResourcePreSizedStream:= nil;
  LResourceMemoryStream:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResourcePreSizedStream:= TtiPreSizedStream.Create(2, 2);
    LResourceMemoryStream:= TMemoryStream.Create;
    LO.AddResource(LFileNamePreSizedStream, LResourcePreSizedStream);
    LO.AddResource(LFileNameMemoryStream, LResourceMemoryStream);

    Check(LO.IsLocked(LFileNamePreSizedStream));
    Check(LO.IsLocked(LFileNameMemoryStream));
    Check(not LO.IsLocked('filethatsnotlocked.txt'));
  finally
    LO.free;
    LResourcePreSizedStream.free;
    LResourceMemoryStream.free;
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_RollbackMemoryStreamFileExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TMemoryStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  tiStringToFile('before XXX', LFileName);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TMemoryStream.Create;
      LO.AddResource(LFileName, LResource);
      // There is no actual rollback method, not calling Commit implies Rollback;
      tiStringToStream('after', LResource);
    finally
      LO.free;
      LResource.Free;
    end;
    CheckEquals('before XXX', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_RollbackMemoryStreamFileNotExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TMemoryStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  LO:= nil;
  LResource:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResource:= TMemoryStream.Create;
    LO.AddResource(LFileName, LResource);

    LO.StartTransaction;
    tiStringToStream('after', LResource);

  finally
    LO.free;
    LResource.Free;
  end;

  // There is no actual rollback method, not calling Commit implies Rollback;
  Check(not FileExists(LFileName));

end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_RollbackPreSizedStreamFileExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TtiPreSizedStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  tiStringToFile('before XXX', LFileName);
  try
    LO:= nil;
    LResource:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResource:= TtiPreSizedStream.Create(2, 2);
      LO.AddResource(LFileName, LResource);

      LO.StartTransaction;
      LResource.Write('after');

    finally
      LO.free;
      LResource.Free;
    end;
    // There is no actual rollback method, not calling Commit implies Rollback;
    CheckEquals('before XXX', tiFileToString(LFileName));
  finally
    tiDeleteFile(LFileName);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_RollbackPreSizedStreamFileNotExists;
var
  LO: TtiAtomicFileSave;
  LFileName: string;
  LResource: TtiPreSizedStream;
begin
  LFileName:= TempFileName(CTestFileName1);
  LO:= nil;
  LResource:= nil;
  try
    LO:= TtiAtomicFileSave.Create;
    LResource:= TtiPreSizedStream.Create(2, 2);
    LO.AddResource(LFileName, LResource);

    LO.StartTransaction;
    LResource.Write('test');

  finally
    LO.free;
    LResource.Free;
  end;
  // There is no actual rollback method, not calling Commit implies Rollback;
  Check(not FileExists(LFileName));
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_StartTransactionMemoryStream;
var
  LO: TtiAtomicFileSave;
  LFileNameFileExists: string;
  LFileNameFileNotExists: string;
  LResourceFileExists: TMemoryStream;
  LResourceFileNotExists: TMemoryStream;
begin
  LFileNameFileExists:= TempFileName(CTestFileName1);
  LFileNameFileNotExists:= TempFileName(CTestFileName2);
  tiStringToFile('', LFileNameFileExists);
  try
    LO:= nil;
    LResourceFileExists:= nil;
    LResourceFileNotExists:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResourceFileExists:= TMemoryStream.Create;
      LResourceFileNotExists:= TMemoryStream.Create;
      LO.AddResource(LFileNameFileExists, LResourceFileExists);
      LO.AddResource(LFileNameFileNotExists, LResourceFileNotExists);

      Check(FileExists(LFileNameFileExists));
      Check(CanLock(LFileNameFileExists));
      Check(not FileExists(LFileNameFileNotExists));
      Check(CanLock(LFileNameFileNotExists));

      LO.StartTransaction;
      Check(FileExists(LFileNameFileExists));
      Check(not CanLock(LFileNameFileExists));
      Check(FileExists(LFileNameFileNotExists));
      Check(not CanLock(LFileNameFileNotExists));

    finally
      LO.free;
      LResourceFileExists.Free;
      LResourceFileNotExists.Free;
    end;
  finally
    tiDeleteFile(LFileNameFileExists);
    tiDeleteFile(LFileNameFileNotExists);
  end;
end;

procedure TtiAtomicFileSaveTestCase.AtomicFileSaveTestCase_StartTransactionPreSizedStream;
var
  LO: TtiAtomicFileSave;
  LFileNameFileExists: string;
  LFileNameFileNotExists: string;
  LResourceFileExists: TtiPreSizedStream;
  LResourceFileNotExists: TtiPreSizedStream;
begin
  LFileNameFileExists:= TempFileName(CTestFileName1);
  LFileNameFileNotExists:= TempFileName(CTestFileName2);
  tiStringToFile('', LFileNameFileExists);
  try
    LO:= nil;
    LResourceFileExists:= nil;
    LResourceFileNotExists:= nil;
    try
      LO:= TtiAtomicFileSave.Create;
      LResourceFileExists:= TtiPreSizedStream.Create(2, 2);
      LResourceFileNotExists:= TtiPreSizedStream.Create(2, 2);
      LO.AddResource(LFileNameFileExists, LResourceFileExists);
      LO.AddResource(LFileNameFileNotExists, LResourceFileNotExists);

      Check(FileExists(LFileNameFileExists));
      Check(CanLock(LFileNameFileExists));
      Check(not FileExists(LFileNameFileNotExists));
      Check(CanLock(LFileNameFileNotExists));

      LO.StartTransaction;
      Check(FileExists(LFileNameFileExists));
      Check(not CanLock(LFileNameFileExists));
      Check(FileExists(LFileNameFileNotExists));
      Check(not CanLock(LFileNameFileNotExists));

    finally
      LO.free;
      LResourceFileExists.Free;
      LResourceFileNotExists.Free;
    end;
  finally
    tiDeleteFile(LFileNameFileExists);
    tiDeleteFile(LFileNameFileNotExists);
  end;
end;

function TtiAtomicFileSaveTestCase.CanLock(const AFileName: string): boolean;
var
  LStream: TFileStream;
begin
  result:= not FileExists(AFileName);
  if not result then
  begin
    LStream:= nil;
    try
      try
        LStream:= TFileStream.Create(AFileName, fmOpenRead or fmShareExclusive);
        result:= True;
      except
        on e:exception do
          result:= False;
      end;
    finally
      LStream.Free;
    end;
  end;
end;

end.











