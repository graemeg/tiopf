unit tiDUnitUtils;

{$I tiDefines.inc}

interface
uses
   Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,tiExcept
  ,tiObject
  ,tiConstants
  ,tiBaseObject
  ,TypInfo
  {$IFDEF FPC}
  ,fpcunit
  ,testregistry
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$define read_interface}
{$undef read_implementation}


type

  TtiPerformanceCounterResultType = Cardinal;

  // PH: Sorry Linux guys. Not sure what to do about this:
  TtiPerformanceCounter = class(TtiBaseObject)
  private
    FInitialized: boolean;
    FRunning: boolean;
    FStart: TtiPerformanceCounterResultType;
    FStop: TtiPerformanceCounterResultType;
    FPerformanceFactor: TtiPerformanceCounterResultType;
    procedure Initialize;
    function  GetTickCount: TtiPerformanceCounterResultType;
    function  GetPerformanceValue: TtiPerformanceCounterResultType;
  public
    constructor Create;
    procedure   Start;
    procedure   Stop;
    property    TickCount: TtiPerformanceCounterResultType read GetTickCount;
  end;

  EtiOPFDUnitException = class(EtiOPFException);

  TtiTestCase = class(TTestCase)
  private
    FPerformanceCounter: TtiPerformanceCounter;
  public
    destructor Destroy; override;
    function  PerformanceCounter: TtiPerformanceCounter;
    procedure CheckObjectState(AObjectState : TPerObjectState; AData : TtiObject; const AMessage: string = '');
    procedure CheckNearEnough(pExpected, pActual: Double); overload;
    procedure CheckNearEnough(pExpected, pActual: Double; const AMessage: string); overload;
    procedure CheckNearEnough(pExpected, pActual: Double; const AMessage: string; pArgs: array of const); overload;
    procedure CheckNearEnough(pExpected, pActual: Double; pFEPS: Double); overload;
    procedure CheckNearEnough(pExpected, pActual: Double; pFEPS: Double; const AString: string); overload;
    procedure CheckNearEnough(AExpected: TDateTime; AField: TtiFieldDateTime); overload;
    procedure CheckNearEnough(AExpected: Real; AField: TtiFieldFloat); overload;
    procedure CheckNearEnough(AExpected, AActual: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldString); overload;
    procedure TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldInteger); overload;
    procedure TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldDateTime); overload;
    procedure TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldBoolean); overload;
    procedure TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldString); overload;
    procedure TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldInteger); overload;
    procedure TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldFloat); overload;
    procedure TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldDateTime); overload;
    procedure TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldBoolean); overload;
    procedure CheckEquals(AExpected, AActual: TtiFieldString); overload;
    procedure CheckEquals(AExpected, AActual: TtiFieldInteger); overload;
    procedure CheckEquals(AExpected, AActual: TtiFieldFloat); overload;
    procedure CheckEquals(AExpected, AActual: TtiFieldBoolean); overload;
    procedure CheckEquals(AExpected, AActual: TtiFieldDateTime); overload;
    procedure CheckEquals(AValue: string;    AField: TtiFieldString); overload;
    procedure CheckEquals(AValue: Integer;   AField: TtiFieldInteger); overload;
    procedure CheckEquals(AValue: Real;      AField: TtiFieldFloat); overload;
    procedure CheckEquals(AValue: Boolean;   AField: TtiFieldBoolean); overload;
    procedure CheckEquals(AValue: TDateTime; AField: TtiFieldDateTime); overload;
    procedure CheckEquals(AValue: TStream;   AField: TStream); overload;
    procedure CheckEquals(AValue: String;   AField: TtiFieldInteger); overload;
    procedure CheckEquals(const AExpected: string; AActual: integer); overload;
    {: Check a TtiObject's Equals Method by calling AData1.Equals(AData2), then changing APropName to ANewValue and trying again.
       APropName must be a string property.}
    procedure CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName, ANewValue: string); overload;
    {: Check a TtiObject's Equals Method by calling AData1.Equals(AData2), then changing APropName to ANewValue and trying again.
       APropName must be a real property.}
    procedure CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName: string; const ANewValue: Real); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a string property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: String; const AErrorProperty: string = ''); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a integer property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: Int64; const AErrorProperty: string = ''); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a real property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: Real; const AErrorProperty: string = ''); overload;
    {: Check a delete visitor is working by creating an instance of TtiObjectClass (which must be a list), then calling it's read method.
       There must be one object in the list. This object will be marked as deleted, then saved and the list will be re-read. The
       list must be empty on the second read.}
    procedure CheckDeletionFromDatabase(const AListClass: TtiObjectClass);

    {$IFDEF FPC}
      // DUnit compatibility interface
      {$I DUnitCompatableInterface.inc}
    {$ENDIF}
  end;

function  tiCreateStringOfSize(pSize : LongInt): string;
procedure tiCreateTextFileOfSize(AFileName : string; pSize : LongInt);
procedure tiDUnitForceRemoveDir(const pDirectory : string);
// Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
procedure tiFillTestingStream(AStream : TStream; ASize : integer);
procedure tiFillTestingStreamASCII(AStream : TStream; ASize : integer);
function  tiIncStr(const AValue: string; AInc: Integer=1; AMaxLength : integer = 999999): String;
function  tiIncStr1(const AValue: string; var AInc: Integer): String;

const
  cDUnitTestFloatPrecision = 0.000001;       
  
implementation
uses
  SysUtils
  ,tiUtils
  {$IFDEF LINUX}
  ,libc
  {$ENDIF LINUX}
 ;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}


function  tiCreateStringOfSize(pSize : LongInt): string;
var
  ls : Char;
begin
  result := '';
  While Length(Result)< pSize do
  begin
    ls := Chr(Random(126 - 32) + 32);
    result := result + ls;
    if (Length(Result)< pSize) and
       (Length(Result) mod 60 = 0) then
      Result := Result + cLineEnding;
  end;
end;

procedure tiCreateTextFileOfSize(AFileName : string; pSize : LongInt);
var
  lFileStream : TFileStream;
  lBuffer  : PChar;
  lLen     : integer;
  ls : string;
begin
  ls := tiCreateStringOfSize(pSize);

  if FileExists(AFileName) then
    tiDeleteFile(AFileName);
  lFileStream := TFileStream.Create(AFileName,
                                     fmCreate or fmShareDenyNone);
  try
    lBuffer := PChar(ls);
    lLen := length(ls);
    lFileStream.write(lBuffer^, lLen);
  finally
    lFileStream.Free;
  end;
end;

procedure tiDUnitForceRemoveDir(const pDirectory : string);
begin
  // The easiest way I could think of to delete a directory and all
  // it's child directories and files.
  try
    {$IFDEF MSWINDOWS}
    tiUtils.tiShellExecute('cmd.exe',
                            '/X /C "rd ' + pDirectory + ' /S /Q"');
    Sleep(500);
    // With no sleep, system will return before the shell call has finished.
    // Tried using tiRunEXEAndWait, but kept getting error "System can not find
    // the file specified."
    // Sleep(100) makes the problem go away, so made it Sleep(500) to be sure.
    {$ENDIF MSWINDOWS}
    
    {$IFDEF LINUX}
    tiUtils.tiRunEXEAndWait('rm -f -R ' + pDirectory);
    {$ENDIF LINUX}
  except
    on e:exception do
      raise exception.Create('Error in ForceRemoveDirectory(''' +
            pDirectory + '> Message: ' + e.message);
  end;
end;

procedure tiFillTestingStream(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LWord : word;
  LChar : Char;
begin
  for LCount := 1 to (ASize div 2) do
    begin
    LWord := Random($7FFF);
    AStream.WriteBuffer(LWord, sizeof(Word));
    end;
  if ASize mod 2 = 1 then
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

procedure tiFillTestingStreamASCII(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LChar : Char;
begin
  for LCount := 1 to ASize do
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

function tiIncStr(const AValue: string; AInc: integer=1; AMaxLength: integer = 999999): String;
var
  LLen: integer;
begin
  result:= IntToStr(StrToInt(AValue)+AInc);
  LLen:= Length(Result);
  if LLen > AMaxLength then
    Result:= Copy(Result, LLen - AMaxLength, AMaxLength);
end;

function  tiIncStr1(const AValue: string; var AInc: Integer): String;
begin
  result:= tiIncStr(AValue, AInc);
  Inc(AInc);
end;

procedure TtiTestCase.CheckObjectState(
  AObjectState: TPerObjectState;
  AData: TtiObject;
  const AMessage: string = '');
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  Check(AObjectState = AData.ObjectState,
        'ObjectState: Expected <' +
        GetEnumName(TypeInfo(TPerObjectState), Ord(AObjectState)) +
        '> but got <' +
        AData.ObjectStateAsString +
        '> on ' + AData.ClassName + '. ' + AMessage);
end;

destructor TtiTestCase.Destroy;
begin
  FreeAndNil(FPerformanceCounter);
  inherited;
end;

function TtiTestCase.PerformanceCounter: TtiPerformanceCounter;
begin
  if FPerformanceCounter=nil then
    FPerformanceCounter:= TtiPerformanceCounter.Create;
  result:= FPerformanceCounter;  
end;

procedure TtiTestCase.CheckNearEnough(pExpected, pActual: Double);
begin
  CheckNearEnough(pExpected, pActual, 0.0001);
end;

procedure TtiTestCase.CheckNearEnough(pExpected, pActual: Double; const AMessage: string);
begin
  CheckNearEnough(pExpected, pActual, 0.0001, AMessage);
end;

procedure TtiTestCase.CheckNearEnough(pExpected, pActual: Double; pFEPS: Double);
begin
  CheckNearEnough(pExpected, pActual, pFEPS, '');
end;

procedure TtiTestCase.CheckEquals(AValue: Integer; AField: TtiFieldInteger);
begin
  CheckEquals(AValue, AField.AsInteger);
end;

procedure TtiTestCase.CheckEquals(AValue: string; AField: TtiFieldString);
begin
  CheckEquals(AValue, AField.AsString);
end;

procedure TtiTestCase.CheckEquals(AValue: Real; AField: TtiFieldFloat);
begin
  CheckNearEnough(AValue, AField.AsFloat);
end;

procedure TtiTestCase.CheckEquals(AValue: TDateTime; AField: TtiFieldDateTime);
begin
  CheckNearEnough(AValue, AField.AsDateTime);
end;

procedure TtiTestCase.CheckEquals(AExpected, AActual: TtiFieldFloat);
begin
  CheckNearEnough(AExpected.AsFloat, AActual.AsFloat, 'Failed on %s', [AExpected.FieldName]);
end;

procedure TtiTestCase.CheckEquals(AExpected, AActual: TtiFieldInteger);
begin
  CheckEquals(AExpected.AsInteger, AActual.AsInteger, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(AExpected, AActual: TtiFieldDateTime);
begin
  CheckNearEnough(AExpected.AsDateTime, AActual.AsDateTime, 'Failed on %s', [AExpected.FieldName]);
end;

procedure TtiTestCase.CheckEquals(AExpected, AActual: TtiFieldBoolean);
begin
  CheckEquals(AExpected.AsBoolean, AActual.AsBoolean, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(AExpected, AActual: TtiFieldString);
begin
  CheckEquals(AExpected.AsString, AActual.AsString, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(AValue: Boolean; AField: TtiFieldBoolean);
begin
  CheckEquals(AValue, AField.AsBoolean);
end;

procedure TtiTestCase.CheckNearEnough(pExpected, pActual: Double; pFEPS: Double; const AString: string);
begin
  if not tiIsNearEnough(pExpected, pActual, pFEPS) then
    FailNotEquals(FloatToStr(pExpected), FloatToStr(pActual), Format('(feps: %g) %s', [pFEPS, AString]), {$IFDEF FPC} nil {$ELSE} CallerAddr {$ENDIF});
end;

procedure TtiTestCase.CheckNearEnough(pExpected, pActual: Double; const AMessage: string; pArgs: array of const);
begin
  CheckNearEnough(pExpected, pActual, Format(AMessage, pArgs));
end;

procedure TtiTestCase.TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldString);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AObj2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField2.TestValid, cErrorTIPerObjAbsTestValid);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsString := AField2.AsString + '1';
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldFloat);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AObj2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField2.TestValid, cErrorTIPerObjAbsTestValid);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsFloat := AField2.AsFloat + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldInteger);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AObj2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField2.TestValid, cErrorTIPerObjAbsTestValid);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsInteger := AField2.AsInteger + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldBoolean);
begin
  Assert(AObj1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AObj2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField2.TestValid, cErrorTIPerObjAbsTestValid);

  Check(AObj1.Equals(AObj2));
  AField1.AsBoolean := not AField2.AsBoolean;
  Check(not AObj1.Equals(AObj2));
  AField1.AsBoolean := AField2.AsBoolean;
  Check(AObj1.Equals(AObj2));
end;

procedure TtiTestCase.TestTIObjectEquals(AObj1, AObj2: TtiObject; AField1, AField2: TtiFieldDateTime);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AObj2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField2.TestValid, cErrorTIPerObjAbsTestValid);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsDateTime := AField2.AsDateTime + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldInteger);
var
  LFieldName: string;
  LSaved: Integer;
begin
  Assert(AObj.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField.TestValid, cErrorTIPerObjAbsTestValid);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsInteger;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsInteger := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldString);
var
  LFieldName: string;
  LSaved: string;
begin
  Assert(AObj.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField.TestValid, cErrorTIPerObjAbsTestValid);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsString;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsString := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldFloat);
var
  LFieldName: string;
  LSaved: real;
begin
  Assert(AObj.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField.TestValid, cErrorTIPerObjAbsTestValid);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsFloat;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsFloat := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldBoolean);
var
  LFieldName: string;
  LSaved: Boolean;
begin
  Assert(AObj.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField.TestValid, cErrorTIPerObjAbsTestValid);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsBoolean;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsBoolean := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(AObj: TtiObject; AField: TtiFieldDateTime);
var
  LFieldName: string;
  LSaved: TDateTime;
begin
  Assert(AObj.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AField.TestValid, cErrorTIPerObjAbsTestValid);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsDateTime;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsDateTime := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.CheckDeletionFromDatabase(const AListClass: TtiObjectClass);
var
  LList: TtiObjectList;
begin
  LList:= AListClass.Create as TtiObjectList;
  try
    LList.Read;
    CheckEquals(1, LList.Count);
    LList.Items[0].Deleted:= True;
    LList.Items[0].Save;
    CheckObjectState(posDeleted, LList.Items[0]);
  finally
    LList.Free;
  end;

  LList:= AListClass.Create as TtiObjectList;
  try
    LList.Read;
    CheckEquals(0, LList.Count);
  finally
    LList.Free;
  end;
end;

procedure TtiTestCase.CheckEquals(const AExpected: string; AActual: integer);
begin
  CheckEquals(StrToInt(AExpected), AActual);
end;

procedure TtiTestCase.CheckEquals(AValue: TStream; AField: TStream);
var
  LMessage: string;
begin
  if not tiTestStreamsIdentical(AValue, AField, LMessage) then
    Fail(LMessage);
end;

procedure TtiTestCase.CheckEquals(AValue: String; AField: TtiFieldInteger);
begin
  CheckEquals(AValue, AField.AsString);
end;

procedure TtiTestCase.CheckNearEnough(AExpected: TDateTime; AField: TtiFieldDateTime);
begin
  CheckNearEnough(AExpected, AField.AsDateTime, AField.FieldName);
end;

procedure TtiTestCase.CheckNearEnough(AExpected: Real; AField: TtiFieldFloat);
begin
  CheckNearEnough(AExpected, AField.AsFloat, AField.FieldName);
end;

procedure TtiTestCase.CheckNearEnough(AExpected, AActual: TtiFieldFloat);
begin
  CheckNearEnough(AExpected.AsFloat, AActual.AsFloat);
end;

procedure TtiTestCase.CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName, ANewValue: string);
var
  LSaved: string;
begin
  Assert(AData1.TestValid, cTIInvalidObjectError);
  Assert(AData2.TestValid, cTIInvalidObjectError);
  CheckEquals(True, AData1.Equals(AData2));
  LSaved:= AData1.PropValue[APropName];
  AData1.PropValue[APropName]:= ANewValue;
  CheckEquals(False, AData1.Equals(AData2));
  AData1.PropValue[APropName]:= LSaved;
  CheckEquals(True, AData1.Equals(AData2));
end;

procedure TtiTestCase.CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName: string; const ANewValue: Real); 
var
  LSaved: Real;
begin
  Assert(AData1.TestValid, cTIInvalidObjectError);
  Assert(AData2.TestValid, cTIInvalidObjectError);
  CheckEquals(True, AData1.Equals(AData2));
  LSaved:= AData1.PropValue[APropName];
  AData1.PropValue[APropName]:= ANewValue;
  CheckEquals(False, AData1.Equals(AData2));
  AData1.PropValue[APropName]:= LSaved;
  CheckEquals(True, AData1.Equals(AData2));
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: String;
  const AErrorProperty: string);
var
  LSaved: String;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: Int64; const AErrorProperty: string = '');
var
  LSaved: Int64;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: Real; const AErrorProperty: string = '');
var
  LSaved: real;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

{$IFDEF FPC}
  // DUnit compatibility interface
  {$I DUnitCompatableInterface.inc}
{$ENDIF}


{ TtiPerformanceCounter }

constructor TtiPerformanceCounter.Create;
begin
  inherited;
  FInitialized:= false;
  FRunning:= false;
end;

procedure TtiPerformanceCounter.Initialize;
var
  LStart: TtiPerformanceCounterResultType;
  LStop: TtiPerformanceCounterResultType;
  LTime: TtiPerformanceCounterResultType;
  i: Cardinal;
  LDummy: TtiPerformanceCounterResultType;
  LLoopCount: TtiPerformanceCounterResultType;
begin
  LTime:= 0;
  LLoopCount:= 100;
  while LTime <= 500 do
  begin
    LLoopCount:= LLoopCount * 10;
    LDummy:= 1;
    LStart:= GetPerformanceValue;
    for i := 1 to LLoopCount do
      Inc(LDummy, LDummy);
    LStop:= GetPerformanceValue;
    LTime:= LStop-LStart;
  end;
  FPerformanceFactor:= LLoopCount div LTime div 10000000;
end;

procedure TtiPerformanceCounter.Start;
begin
  Assert(not FRunning, 'Currently running');
  Initialize;
  FStart:= GetPerformanceValue;
  FRunning:= true;
end;

procedure TtiPerformanceCounter.Stop;
begin
  Assert(FRunning, 'Not running');
  FStop:= GetPerformanceValue;
  FRunning:= false;
end;

function TtiPerformanceCounter.GetPerformanceValue: TtiPerformanceCounterResultType;
begin
  result:= tiGetTickCount;
end;

function TtiPerformanceCounter.GetTickCount: TtiPerformanceCounterResultType;
begin
  Assert(not FRunning, 'Still running');
  result:= (FStop - FStart) * FPerformanceFactor;
end;



end.
