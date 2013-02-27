Unit StdStuff;
{$IFDEF VER150}
  {$WARN UNIT_PLATFORM OFF }
{$ENDIF}
Interface

Uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, FileCtrl, Buttons;

Type
  TOSVersion = (ovWIN311, ovWIN95, ovWIN98, ovNT351, ovNT40, ovWin2K, ovWinXP, ovUnknown);

  TDebugLog = Class
  Private
    PFileName : String;
    PFileOpen : Boolean;
    PFirstLog : Boolean;
    FLogSize : Integer;
    PLogFile : Text;
    {FTempFile : File Of Byte;
    Procedure CheckSize;}
  Public
    FLogging : Boolean;
    Procedure AddEntry(Const LogText : String);
    Procedure AddEntryFmt(Const LogText : String; Const Args : Array Of Const);
    Procedure OpenLogFile;
    Procedure CloseLogFile;
    Procedure ClearLogFile;
    Constructor Create;
    Constructor CreateOnPath(Const FName : String);
    Destructor Destroy; Override;
    Property Logging : Boolean Read FLogging Write FLogging;
    Property LogSize : Integer Read FLogSize Write FLogSize;
  End;

  TStopWatch = Class
  Private
    FStarted : DWord;
    FStopped : DWord;
    FRunning : Boolean;
    Function GetElapsedTime : DWord;
  Public
    Procedure StartTiming;
    Procedure StopTiming;
    Constructor Create; 
    Property ElapsedTime : DWord Read GetElapsedTime;
    Property Running : Boolean Read FRunning;
  End;

// User Interaction Stuff.
Procedure ErrorBox(Const Msg : String);
Procedure WarnBox(Const Msg : String);
Procedure ShowInfo(Const Msg : String);
Function UserAgrees(Const Msg : String) : Boolean;
Function PasswordOK(Const pPassword : String) : Boolean;

// System Routines.
Function OSVersion : TOSVersion;
Function EnablePrivilege(PrivStr : PChar) : Boolean;
Function SetPCDateAndTime(NewDateTime : TDateTime) : Boolean;
Function GetProgramVersion : String;
Function PrinterInstalled : Boolean;

// String Routines.
Function LastPosCh(aCh : AnsiChar; Const pStr : String) : Integer;
Function MyPosCh(aCh : AnsiChar; Const pStr : String) : Integer;
Procedure OverwriteString(Var Orig : String; Rep : String; Start : Integer);
Function IsLetter(Ch : Char) : Boolean;
Function IsNumber(Ch : Char) : Boolean;
Function RemoveNonAlphas(Const pString : String) : String;
Function IsAllCharacter(Const S : String) : Boolean;
Function PrettyText2(Const Str : String) : String;
Function PrettyText(Str : String) : String;
Function EncryptString(Const StrToEncrypt : String) : String;
Function DecryptString(Const StrToDecrypt : String) : String;
Function LeftPadCh(Const TheStr : String; L : Byte; TheChar : Char) : String;
Function PadCh(Const TheStr : String; L : Byte; TheChar : Char) : String;
Function Real2Str(TheReal : Double; TheWidth, Decimals : Byte) : String;
Function String2DelimChars(Const Str : String) : String;
Function DelimChars2String(Const pStr : String) : String;
Function LastChar(Const pStr : String) : Char;
Function HexB(B : Byte) : String;
Function ReplaceChar(Const pStr : String; pSource, pDest : Char) : String;

//Financial Routines
Function Nett(pGross : Currency; pTaxRate : Double) : Currency;
Function Gross(pNett : Currency; pTaxRate : Double) : Currency;

// Miscellaneous Routines.
Procedure Debug(Const DebugStr : String);
Function TodaysPassword : String; // returns 444 + day of month as password.
Function GetMDACVersion : String; // Returns the installed version of MDAC.

// Date Routines.
Function StartOfWeek(TheDate : TDateTime) : TDateTime;
Function EndOfWeek(TheDate : TDateTime) : TDateTime;
Function StartOfMonth(TheDate : TDateTime) : TDateTime;
//Function EndOfMonth(TheDate : TDateTime) : TDateTime;
Function StartOfYear(TheDate : TDateTime) : TDateTime;
Function EndOfYear(TheDate : TDateTime) : TDateTime;
Function TradingDate(TheDate : TDateTime; EndTime : TDateTime = 0.25) : TDateTime;
Function AgeInYears(DOB : TDateTime) : Integer;
Function MeaningfulDate(pDate : TDateTime) : String;

//  Bit manipulation routines.
Function BitIsSet(Value : Integer; Bit : Byte) : Boolean;
Function BitIsClear(Value : Integer; Bit : Byte) : Boolean;
Function TurnBitOn(Value : Integer; Bit : Byte) : Integer;
Function TurnBitOff(Value : Integer; Bit : Byte) : Integer;

Var
  DebugLog : TDebugLog;
  Stopwatch : TStopWatch;

Const
  Hrs6 = 6 / 24;
  Hrs6Secs = Trunc(Hrs6 * SecsPerDay);
  Mins1 = 1 / (24 * 60);

Implementation

Uses Windows, Printers, Registry;

Var
  DayNumber : Byte;

{: According to
 http://support.microsoft.com/default.aspx?scid=KB;EN-US;Q301202&#3
 the MDAC version number is stored in
 HKEY_LOCAL_MACHINE\Software\Microsoft\DataAccess\FullInstallVer }

Function GetMDACVersion : String;
Const
  MDACRootKey = 'Software\Microsoft\DataAccess';
  MDACVersionKey = 'FullInstallVer';
Var
  Reg : TRegistry;
Begin
  Result := 'MDAC not installed.';
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    If Reg.OpenKey(MDACRootKey, False) Then
    Begin
      If Reg.ValueExists(MDACVersionKey) Then
      Begin
        Result := Reg.ReadString(MDACVersionKey);
        Reg.CloseKey;
      End;
    End;
  Finally
    Reg.Free;
  End;
End;

Function IsLetter(Ch : Char) : Boolean;
Begin
  Result := (Ch In ['a'..'z', 'A'..'Z']);
End;

Function IsNumber(Ch : Char) : Boolean;
Begin
  Result := (Ch In ['0'..'9']);
End;

Function RemoveNonAlphas(Const pString : String) : String;
Var
  I : Integer;
  TempCh : Char;
Begin
  Result := '';
  For I := 1 To Length(pString) Do
  Begin
    TempCh := pString[I];
    If (IsLetter(TempCh) Or IsNumber(TempCh)) Then
      Result := Result + TempCh;
  End; { Loop }
End;

Function IsAllCharacter(Const S : String) : Boolean;
Var
  J : Integer;
Begin
  Result := True;
  For J := 1 To Length(S) Do
  Begin
    If S[J] In ['0'..'9'] Then
    Begin
      Result := False;
      Exit;
    End;
  End;
End;

Function PrettyText2(Const Str : String) : String;
Var
  J : Integer;
  lResult : String;
Begin
  If (Length(Str) = 0) Then
    Exit;
  lResult := Str;
  If Not IsAllCharacter(Str) Then
  Begin
    lResult := Uppercase(Str);
    Exit;
  End;
  If (Length(lResult) > 1) Then
  Begin
    lResult := Lowercase(lResult);
    lResult[1] := Upcase(lResult[1]);
    For J := 2 To Length(lResult) Do
      If (lResult[Pred(J)] In [' ', '-', '''']) Then
        lResult[J] := Upcase(lResult[J]);
  End;
  Result := lResult;
End;

{:Finds the last position of aCh in pStr. }

Function LastPosCh(aCh : AnsiChar; Const pStr : String) : Integer;
Var
  I : Integer;
Begin
  Result := 0;
  For I := Length(pStr) Downto 1 Do
  Begin
    If (pStr[I] = aCh) Then
    Begin
      Result := I;
      Exit;
    End;
  End; { Loop }
End;

{:Used for checking the position of a char in a string. MyPosCH is 5 times
 faster than the VCL POS function.}

Function MyPosCh(aCh : AnsiChar; Const pStr : String) : Integer;
Var
  I : Integer;
Begin
  Result := 0;
  For I := 1 To Length(pStr) Do
  Begin
    If (pStr[I] = aCh) Then
    Begin
      Result := I;
      Exit;
    End;
  End; { Loop }
End;

Procedure OverwriteString(Var Orig : String; Rep : String; Start : Integer);
Var
  J, N, L : Integer;
Begin
  L := Length(Orig);
  If ((Start < 1) Or (Start > L)) Then
    Exit;
  N := Length(Rep);
  J := 1;
  While (Start <= L) And (J <= N) Do
  Begin
    Orig[Start] := Rep[J];
    Inc(Start);
    Inc(J);
  End;
End;

Function PrettyText(Str : String) : String;
Var
  S : String;
  J, K : Integer;
Begin
  Result := Str;
  If (Length(Str) = 0) Then
    Exit;
  J := 1;
  S := '';
  K := J;
  Repeat
    S := S + Str[J];
    If (J >= Length(Str)) Or (Str[J] In [' ', ',']) Then
    Begin
      S := PrettyText2(S);
      OverwriteString(Str, S, K);
      K := Succ(J);
      S := '';
    End;
    Inc(J);
  Until (J > Length(Str));
  Result := Str;
End;

Function EncryptString(Const StrToEncrypt : String) : String;
Var
  L, I : Integer;
  TempResult : String;
Begin
  L := Length(StrToEncrypt);
  TempResult := '';
  For I := 1 To L Do
    TempResult := TempResult + Chr(Ord(StrToEncrypt[I]) + L);
  Result := TempResult;
End;

Function DecryptString(Const StrToDecrypt : String) : String;
Var
  L, I : Integer;
  TempResult : String;
Begin
  L := Length(StrToDecrypt);
  TempResult := '';
  For I := 1 To L Do
    TempResult := TempResult + Chr(Ord(StrToDecrypt[I]) - L);
  Result := TempResult;
End;

Function LeftPadCh(Const TheStr : String; L : Byte; TheChar : Char) : String;
Var
  TempRes : String;
Begin
  TempRes := TheStr;
  While (Length(TempRes) < L) Do
    TempRes := TheChar + TempRes;
  Result := TempRes;
End;

Function PadCh(Const TheStr : String; L : Byte; TheChar : Char) : String;
Var
  TempRes : String;
Begin
  TempRes := TheStr;
  While (Length(TempRes) < L) Do
    TempRes := TempRes + TheChar;
  Result := TempRes;
End;

Function Real2Str(TheReal : Double; TheWidth, Decimals : Byte) : String;
Var
  TempRes : String;
Begin
  Str(TheReal : TheWidth : Decimals, TempRes);
  Result := TempRes;
End;

Function ReplaceChar(Const pStr : String; pSource, pDest : Char) : String;
Var
  lStr : String;
  I : Integer;
Begin
  lStr := pStr;
  I := MyPosCh(pSource, pStr);
  While (I <> 0) Do
  Begin
    lStr[I] := pDest;
    I := MyPosCh(pSource, lStr);
  End;
  Result := lStr;
End;

Function HexB(B : Byte) : String;
Const
  lDigits : Array[0..$F] Of Char = '0123456789ABCDEF';
Var
  lTempResult : String;
Begin
  lTempResult := '  ';
  lTempResult[1] := lDigits[B Shr 4];
  lTempResult[2] := lDigits[B And $F];
  Result := lTempResult;
End;

Function LastChar(Const pStr : String) : Char;
Var
  L : Integer;
Begin
  L := Length(pStr);
  If (L > 0) Then
    Result := pStr[L]
  Else
    Result := #0;
End;

Function DelimChars2String(Const pStr : String) : String;
Var
  lWorkStr,
  lByteStr : String;
  B, lCommaPos : Byte;
Begin
  lWorkStr := pStr;
  Result := '';
  While (Length(lWorkStr) > 1) Do
  Begin
    lCommaPos := MyPosCh(',', lWorkStr);
    lByteStr := Copy(lWorkStr, 1, lCommaPos - 1);
    B := StrToInt(lByteStr);
    Result := Result + Chr(B);
    Delete(lWorkStr, 1, lCommaPos);
  End;
End;

Function String2DelimChars(Const Str : String) : String;
Var
  I : Integer;
Begin
  Result := '';
  For I := 1 To Length(Str) Do
    Result := Result + IntToStr(Ord(Str[I])) + ',';
  Delete(Result, Length(Result), 1);
End;

Function Nett(pGross : Currency; pTaxRate : Double) : Currency;
Begin
  Result := pGross / (1 + (pTaxRate / 100));
End;

Function Gross(pNett : Currency; pTaxRate : Double) : Currency;
Begin
  Result := pNett + (pNett * (pTaxRate / 100));
End;


Procedure ErrorBox(Const Msg : String);
Begin
  MessageBeep(MB_ICONHAND);
  MessageDlg(Msg, mtError, [mbOK], 0);
End;

Procedure WarnBox(Const Msg : String);
Begin
  MessageBeep(MB_ICONEXCLAMATION);
  MessageDlg(Msg, mtWarning, [mbOK], 0);
End;

Procedure ShowInfo(Const Msg : String);
Begin
  MessageBeep(MB_OK);
  MessageDlg(Msg, mtInformation, [mbOK], 0);
End;

Function UserAgrees(Const Msg : String) : Boolean;
Begin
  MessageBeep(MB_ICONQUESTION);
  Result := (MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
End;

Function PasswordOK(Const pPassword : String) : Boolean;
Var
  lAttempt : String;
  lOK : Boolean;
Begin
  lOK := InputQuery('Enter Password', 'Password', lAttempt); // Need to use password mask
  Result := lOK And (UpperCase(lAttempt) = UpperCase(pPassword));
End;

Function OSVersion : TOSVersion;
Var
  MyVersionInfoRec : TOSVersionInfoA;
Begin
  Result := ovUnknown;
  FillChar(MyVersionInfoRec, SizeOf(MyVersionInfoRec), #0);
  With MyVersionInfoRec Do
  Begin
    dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    If GetVersionEx(MyVersionInfoRec) Then
    Begin
      Case dwPlatformID Of
        VER_PLATFORM_WIN32_WINDOWS :
          Begin
            Case dwMajorVersion Of
              4 : Result := ovWin95;
              5 : Result := ovWin98;
            End; { Case }
          End;
        VER_PLATFORM_WIN32_NT :
          Begin
            Case dwMajorVersion Of
              3 : Result := ovNT351;
              4 : Result := ovNT40;
              5 : Result := ovWin2K;
              6 : Result := ovWinXP;
            End; { Case }
          End;
        VER_PLATFORM_WIN32s :
          Begin
            Result := ovWIN311;
          End;
      End; { Case }
    End;
  End;
End;

Function EnablePrivilege(PrivStr : PChar) : Boolean;
Var
  MyTokenHandle : THandle;
  Junk,
    MyPrivilege : TTokenPrivileges;
  WorkInt : DWord;
Begin
  If OSVersion In [ovNT351, ovNT40, ovWin2K, ovWinXP] Then
  Begin
    Result := False;
    If Not (OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, MyTokenHandle)) Then
      ShowMessage('Error ' + IntToStr(GetLastError))
    Else
    Begin
      LookupPrivilegeValue('', PrivStr, MyPrivilege.Privileges[0].LUID);
      With MyPrivilege Do
      Begin
        PrivilegeCount := 1;
        Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      End;
      FillChar(Junk, SizeOf(Junk), #0);
      WorkInt := 0;
      Windows.AdjustTokenPrivileges(MyTokenHandle, False, MyPrivilege, 0, Junk, WorkInt);
      If (GetLastError <> ERROR_SUCCESS) Then
        ErrorBox('Error after AdjustPrivileges')
      Else
        Result := True;
    End;
  End
  Else
    Result := True;
End;

Function SetPCDateAndTime(NewDateTime : TDateTime) : Boolean;
Var
  NewSystemDateTime : TSystemTime;
Begin
  DateTimeToSystemTime(NewDateTime, NewSystemDateTime);
  EnablePrivilege('SE_SYSTEMTIME_NAME');
  Result := SetSystemTime(NewSystemDateTime);
End;

Function GetProgramVersion : String;
Var
  lSize, I : Integer;
  dwdDummy : DWord;
  uinSize : UINT;
  lInfo : PChar;
  pntValor : Pointer;
  pntTranslation : Pointer;
  strBegin : String;
Begin
  Result := '';
  lSize := GetFileVersionInfoSize(PChar(ParamStr(0)), dwdDummy);
  If (lSize > 0) Then
  Begin
    GetMem(lInfo, lSize);
    Try
      GetFileVersionInfo(PChar(ParamStr(0)), 0, lSize, lInfo);
      VerQueryValue(lInfo, '\\VarFileInfo\\Translation', pntTranslation, uinSize);
      strBegin := '\\StringFileInfo\\' + IntToHex(LoWord(LongInt(pntTranslation^)), 4) + IntToHex(HiWord(LongInt(pntTranslation^)), 4) + '\\';
      For I := 1 To 11 Do
        If VerQueryValue(lInfo, PChar(strBegin + 'FileVersion'), pntValor, uinSize) Then
          If (uinSize > 0) And (I = 3) Then
          Begin
            Result := String(PChar(pntValor));
          End;
    Finally
      FreeMem(lInfo, lSize);
    End;
  End;
End;

Function PrinterInstalled : Boolean;
Begin
  Result := (Printer.Printers.Count > 0);
End;

{Procedure InitDates;
Begin
  TempDate := Now;
  TodayStr := DateToStr(Now);
  LongTodayStr := ovcIntlSup.DateToDateString('wwwwwwwww dd nnnnnnnnnn yyyy', CurrentDate, True);
End;}

Procedure Debug(Const DebugStr : String);
Var
  TheStr : String;
Begin
  TheStr := DateTimeToStr(Now) + ' ' + DebugStr + #13;
  OutputDebugString(PChar(TheStr));
End;

Function StartOfWeek(TheDate : TDateTime) : TDateTime;
Begin
  DayNumber := SysUtils.DayOfWeek(TheDate);
  While (DayNumber <> 1) Do
  Begin
    TheDate := TheDate - 1;
    DayNumber := SysUtils.DayOfWeek(TheDate);
  End;
  Result := TheDate;
End;

Function EndOfWeek(TheDate : TDateTime) : TDateTime;
Begin
  DayNumber := SysUtils.DayOfWeek(TheDate);
  While (DayNumber <> 7) Do
  Begin
    TheDate := TheDate + 1;
    DayNumber := SysUtils.DayOfWeek(TheDate);
  End;
  Result := TheDate;
End;

Function StartOfMonth(TheDate : TDateTime) : TDateTime;
Var
  NowStr,
    StartStr : String;
Begin
  NowStr := DateToStr(TheDate);
  StartStr := '01' + Copy(NowStr, 3, Length(NowStr));
  Result := StrToDate(StartStr);
End;

{Function EndOfMonth(TheDate : TDateTime) : TDateTime;
Var
  LastDay : Integer;
  NowStr,
    EndStr : String;
  NowDay,
    NowMonth,
    NowYear : Word;
Begin
  DecodeDate(TheDate, NowYear, NowMonth, NowDay);
  LastDay := DaysInMonth(NowMonth, NowYear, 0);
  NowStr := DateToStr(TheDate);
  EndStr := IntToStr(LastDay) + Copy(NowStr, 3, Length(NowStr));
  Result := StrToDate(EndStr);
End;}

Function StartOfYear(TheDate : TDateTime) : TDateTime;
Var
  NowDay,
    NowMonth,
    NowYear : Word;
Begin
  DecodeDate(TheDate, NowYear, NowMonth, NowDay);
  Result := StrToDate('01/01/' + IntToStr(NowYear));
End;

Function EndOfYear(TheDate : TDateTime) : TDateTime;
Var
  NowDay,
    NowMonth,
    NowYear : Word;
Begin
  DecodeDate(TheDate, NowYear, NowMonth, NowDay);
  Result := StrToDate('31/12/' + IntToStr(NowYear));
End;

Procedure TDebugLog.AddEntry(Const LogText : String);
Var
  LogLength : Integer;
Begin
  If FLogging Then
  Begin
    Try
      LogLength := Length(DateTimeToStr(Now) + ' ' + LogText);
      If Not PFileOpen Then
        OpenLogFile;
      If Not PFileOpen Then
        ErrorBox('Unable To Open The Log File.' + #13 + '(Is there another application using it?)');
      If PFirstLog Then
      Begin
        WriteLn(PLogFile, DateTimeToStr(Now) + ' ** LOG SESSION STARTED **');
        LogLength := Length(DateTimeToStr(Now) + ' ** LOG SESSION STARTED **');
        Inc(FLogSize, LogLength);
        System.Flush(PLogFile);
        PFirstLog := False;
      End;
      WriteLn(PLogFile, DateTimeToStr(Now) + ' ' + LogText);
      Inc(FLogSize, LogLength);
      System.Flush(PLogFile);
    Except
      On E : Exception Do
        ErrorBox('Error writing log file :- ' + #13 + E.Message);
    End;
  End;
End;

Procedure TDebugLog.AddEntryFmt(Const LogText : String; Const Args : Array Of Const);
Begin
  AddEntry(Format(LogText, Args));
End;

(*Procedure TDebugLog.CheckSize;
Var
  TempRes : Integer;
Begin
  If FileExists(PFileName) Then
  Begin
    AssignFile(FTempFile, PFileName);
{$I-}
    Reset(FTempFile);
    TempRes := IOResult;
    If (TempRes <> 0) Then
    Begin
      ShowMessage(Format('Reset returned %d', [TempRes]));
      Rewrite(FTempFile);
      FLogSize := 0;
    End
    Else
      FLogSize := FileSize(FTempFile);
{$I+}
    CloseFile(FTempFile);
  End;
End; *)

Procedure TDebugLog.ClearLogFile;
Begin
  CloseFile(PLogFile);
  Rewrite(PLogFile);
  Append(PLogFile);
End;

Procedure TDebugLog.CloseLogFile;
Begin
  System.Flush(PLogFile);
  CloseFile(PLogFile);
  PFileOpen := False;
End;

Constructor TDebugLog.Create;
Begin
  Inherited;
  PFirstLog := True;
  FLogging := False;
  FLogSize := 0;
  PFileName := 'Log\Error.Log';
End;

Constructor TDebugLog.CreateOnPath(Const FName : String);
Begin
  Inherited;
  PFirstLog := True;
  FLogging := False;
  PFileOpen := False;
  PFileName := FName;
  FLogSize := 0;
  //CheckSize;
  AssignFile(PLogFile, FName);
  Try
    If Not FileExists(FName) Then
      Rewrite(PLogFile)
    Else
      Append(PLogFile);
    PFileOpen := True;
  Except
    PFileOpen := False;
  End;
End;

Destructor TDebugLog.Destroy;
Begin
  If Not PFirstLog Then
    AddEntry('** LOG SESSION ENDED **');
  If PFileOpen Then
    CloseFile(PLogFile);
  Inherited;
End;

Procedure TDebugLog.OpenLogFile;
Begin
  PFileOpen := False;
  //CheckSize;
  If Not DirectoryExists('Log') Then
    MkDir('Log');
  AssignFile(PLogFile, PFileName);
  Try
    If Not FileExists(PFileName) Then
      Rewrite(PLogFile);
    Append(PLogFile);
    PFileOpen := True;
  Except
    PFileOpen := False;
  End;
End;

Function TodaysPassword : String; // returns 444 + day of month as password.
Var
  BasePw : Integer;
  NowDay,
    NowMonth,
    NowYear : Word;
Begin
  DecodeDate(Now, NowYear, NowMonth, NowDay);
  BasePw := 444 + NowDay;
  Result := IntToStr(BasePw);
End;

Function TradingDate(TheDate : TDateTime; EndTime : TDateTime = 0.25) : TDateTime;
Begin
  If (Frac(TheDate) <= EndTime) Then
    Result := Trunc(TheDate - 1)
  Else
    Result := Trunc(TheDate);
End;

Function AgeInYears(DOB : TDateTime) : Integer;
Var
  Age,
    BDay, NowDay,
    BYear, NowYear,
    BMonth, NowMonth : Word;
Begin
  If (DOB <> 0) Then
  Begin
    DecodeDate(Now, NowYear, NowMonth, NowDay);
    DecodeDate(DOB, BYear, BMonth, BDay);
    Age := NowYear - BYear;
    If (NowMonth < BMonth) Then
      Dec(Age);
    If (NowMonth = BMonth) Then
    Begin
      If (NowDay < BDay) Then
        Dec(Age);
    End;
  End
  Else
    Age := 0;
  Result := Age;
End;

Function MeaningfulDate(pDate : TDateTime) : String;
Var
  lDelta : Integer;
Begin
  lDelta := Trunc(pDate) - Trunc(Now);
  Case lDelta Of
    -1 : Result := 'Yesterday';
    0 : Result := 'Today';
    1 : Result := 'Tomorrow';
  Else
    Result := DateToStr(pDate);
  End; { Case }
End;


Function BitIsSet(Value : Integer; Bit : Byte) : Boolean;
Begin
  Result := ((Value And (1 Shl Bit)) <> 0);
End;

Function BitIsClear(Value : Integer; Bit : Byte) : Boolean;
Begin
  Result := Not BitIsSet(Value, Bit);
End;

Function TurnBitOn(Value : Integer; Bit : Byte) : Integer;
Begin
  Result := Value Or (1 Shl Bit);
End;

Function TurnBitOff(Value : Integer; Bit : Byte) : Integer;
Begin
  Result := Value And Not (1 Shl Bit);
End;

{ TStopWatch }

Constructor TStopWatch.Create;
Begin
  Inherited;
  FRunning := False;
  FStopped := 0;
  FStarted := 0;
End;

Function TStopWatch.GetElapsedTime : DWord;
Begin
  If FRunning Then
    Result := GetTickCount - FStarted
  Else
    Result := FStopped - FStarted;
End;

Procedure TStopWatch.StartTiming;
Begin
  FStarted := GetTickCount;
  FRunning := True;
End;

Procedure TStopWatch.StopTiming;
Begin
  FStopped := GetTickCount;
  FRunning := False;
End;

Initialization
//  InitDates;
  DebugLog := TDebugLog.Create;
  Stopwatch := TStopWatch.Create;
Finalization
  DebugLog.Free;
  Stopwatch.Free;
End.

