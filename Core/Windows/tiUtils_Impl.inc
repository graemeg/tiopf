{ Implementation section of tiUtils for platform specific code only.

  Platform: MSWINDOWS }


function tiGetTempFile(const AFileNameExtension : string): string;
const
  cMaxPathLen = 255;
var
  pcTemp : array[0..cMaxPathLen] of char;
  pcApp : array[0..cMaxPathLen] of char;
  pcPath : array[0..cMaxPathLen] of char;
begin
  strPCopy(pcApp, copy(extractFileName(ParamStr(0)), 1, 3));
  getTempPath(cMaxPathLen, pcPath);
  getTempFileName(pcPath, pcApp, 0, pcTemp);
  deleteFile(pcTemp); // This is using the Window deleteFile, not Delphi's
  result := strPas(pcTemp);

  if pos('.', result) <> 0 then
  begin
    if AFileNameExtension = '' then
      result := tiRemoveExtension(result)
    else
      result := copy(result, 1, tiPosR('.', result)) + AFileNameExtension;
  end;
end;


function tiGetTempDir : string;
begin
  result := ExtractFilePath(tiGetTempFile('tmp'));
end;


function tiGetWindowsSysDir : string;
const
  cMaxPathLen = 255;
var
  pcDir : array[0..cMaxPathLen] of char;
begin
  GetSystemDirectory(pcDir, cMaxPathLen);
  result := String(pcDir);
end;


function tiShellExecute(const AEXE : string;
                         const AParameters : string = '';
                         const AWinState : integer = SW_SHOWNORMAL): integer;
var
  lFileName  : array[0..255] of char;
  lParameters : array[0..255] of char;
  lHandle : THandle;
begin;

  strPCopy(lFileName,   AEXE);
  strPCopy(lParameters, AParameters);

  // Screen.ActiveForm.Handle is not thread safe
  //lHandle := screen.activeForm.handle;
  lHandle := 0;
  result := _tiShellExecute(lHandle,
                             nil,
                             lFileName,
                             lParameters,
                             nil,
                             AWinState);
end;


function _tiShellExecute(AHwnd : integer;
                         AOpperation, AFile, AParameters, ADirectory : PChar;
                         AShowCmd : integer): integer;
var sMessage    : string;
begin

  result := ShellExecute(AHwnd,
                          AOpperation,
                          AFile,
                          AParameters,
                          ADirectory,
                          AShowCmd);

  { These error messages were pasted from the WINAPI help on shellExecute() }
  case result of
    0 : sMessage := ('System was out of memory, executable file was corrupt, or ' +
                       'relocations were invalid.');
    2 : sMessage := ('File was not found.');
    3 : sMessage := ('Path was not found.');
    5 : sMessage := ('Attempt was made to dynamically link to a task, or there ' +
                       'was a sharing or network-protection error.');
    6 : sMessage := ('Library required separate data segments for each task.');
    8 : sMessage := ('There was insufficient memory to start the application.');
    10 : sMessage := ('Windows version was incorrect.');
    11 : sMessage := ('Executable file was invalid. Either it was not a Windows ' +
                       'application or there was an error in the .EXE image.');
    12 : sMessage := ('Application was designed for a different operating system.');
    13 : sMessage := ('Application was designed for MS-DOS 4.0.');
    14 : sMessage := ('Type of executable file was unknown.');
    15 : sMessage := ('Attempt was made to load a real-mode application (developed ' +
                       'for an earlier version of Windows).');
    16 : sMessage := ('Attempt was made to load a second instance of an executable ' +
                       'file containing multiple data segments that were not marked ' +
                       'read-only.');
    19 : sMessage := ('Attempt was made to load a compressed executable file. The ' +
                       'file must be decompressed before it can be loaded.');
    20 : sMessage := ('Dynamic-link library (DLL) file was invalid. One of the ' +
                       'DLLs required to run this application was corrupt.');
    21 : sMessage := ('Application requires Windows 32-bit extensions.');
    else
      sMessage := '';
      { ShellExe ran OK, do nothing. }
    end;

    if sMessage <> '' then
      raise Exception.Create('Error executing external application.' + Cr +
                              'Error: ' + sMessage);
end;


procedure tiOpenFile(const AFileName: string);
const
  CKnownFileTypes: array[0..10] of string = (
    'txt'
    ,'htm'
    ,'html'
    ,'csv'
    ,'exe'
    ,'doc'
    ,'docx'
    ,'xls'
    ,'xlsx'
    ,'vsd'
    ,'pdf'
  );
var
  LExt: string;
  i: integer;
begin
  // Is it a known file type?
  LExt := LowerCase(tiExtractExtension(AFileName));
  for i := 0 to Length(CKnownFileTypes) - 1 do
    if LExt = CKnownFileTypes[i] then
    begin
      tiShellExecute(AFileName);
      Exit; //==>
    end;

  // Is it a directory?
  if DirectoryExists(AFileName) then
  begin
    tiShellExecute(AFileName);
    Exit; //==>
  end;

  tiShellExecute('notepad.exe', AFileName);
end;


procedure tiCopyFile(const AFrom, ATo : string);
var
  LResultCode : Longword;
begin
  copyFile(pChar(AFrom), pChar(ATo), false);
  LResultCode := getLastError();
  if LResultCode <> 0 then
    raise EtiOPFFileSystemException.CreateFmt(
      CErrorCanNotCopyFile,
      [AFrom, ATo, LResultCode, sysErrorMessage(LResultCode)]);
end;



function tiGetUserName : string;
begin
  Result := tiWin32GetUserName;
end;


function tiGetComputerName : string;
begin
  Result := tiWin32GetComputerName;
end;



procedure tiSetFileReadOnly(AFileName: string; AReadOnly: boolean);
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
      lBoolReadOnly : boolean;
begin
  lBoolReadOnly := tiIsFileReadOnly(AFileName);
  if lBoolReadOnly = AReadOnly then exit; //==>

  iCurrentState := tiWin32FileGetAttr(AFileName);
  if AReadOnly then begin
    tiWin32FileSetAttr(AFileName, iCurrentState or cReadOnly);
  end else begin
   tiWin32FileSetAttr(AFileName, iCurrentState xor cReadOnly);
  end;
end;


function tiIsFileReadOnly(AValue: string): boolean;
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
begin
  iCurrentState := tiWin32FileGetAttr(AValue);
  result := tiIsBitSet(iCurrentState, 0);
end;


function tiGetModuleFileName: string;
var
  Path: array[0..MAX_PATH - 1] of Char;
begin
  if IsLibrary then
  begin
    if Windows.GetModuleFileName(HInstance, Path, SizeOf(Path)) = 0 then
      Result := ''
    else
      Result := Path;
  end
  else
    Result := Paramstr(0);
end;


function tiGetEXEPath : string;
var
  path: array[0..MAX_PATH - 1] of char;
begin
  if IsLibrary then
    SetString(Result, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
    result := Paramstr(0);
  result := tiRemoveTrailingSlash(ExtractFilePath(Result));
end;



{ This allows us to always use a \ as a path separator. For Win32 it will
  do nothing, but for *Unix it will replace all \'s with /'s.  Now we don't have
  to have so many IFDEFs in the Unit Tests! }
function tiFixPathDelim(const AText: string): string;
begin
  result := AText;
end;

function tiGetAppDataDirPrivate: string;
begin
  result := tiWin32GetAppConfigDir(False);
end;

function tiGetAppDataDirPublic: string;
begin
  result := tiWin32GetAppConfigDir(True);
end;


procedure tiRunEXEAndWait(const AEXE : string; const AParams: string = '';
                          AInheritParentStartInfo: boolean = true);
var
  LCommand: string;
begin
  if AParams <> '' then
    LCommand:= AEXE + ' ' + AParams
  else
    LCommand:= AEXE;
  tiWin32RunEXEAndWait(LCommand, AInheritParentStartInfo);
end;



procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
var
  LFileDate: Integer;
  LFileHandle: Integer;
  LExitCode: Integer;
begin
  LFileDate := DateTimeToFileDate(ADateTime);
  {$WARN SYMBOL_PLATFORM OFF}
  LFileHandle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    if LFileHandle > 0 then
    begin
      LExitCode := FileSetDate(LFileHandle, LFileDate);
      if LExitCode <> 0 then
        raise EtiOPFFileSystemException.CreateFmt(
            CErrorSettingFileDate, [
            AFileName, tiDateTimeToStr(ADateTime),
            LExitCode, SysErrorMessage(LExitCode)]);
    end
    else
      raise exception.Create('Unable to set file date on <' + AFileName + '>');
  finally
    FileClose(LFileHandle);
  end;
end;


function tiStrPos(const AString, ASubString: PChar): PChar;
  {
  From Paul Spain. paul@xpro.com.au
  You may remember during the XML optimisation project that there was a
  method where I couldn't get any performance benefit over a string-based
  method that was using quite a few Copy() calls? What, it's not just like
  yesterday for you?;-)

  Well, I finally figured out why last night after trying to optimise
  StringReplace(). It turns out that the RTL implementation of StrPos() is
  a dog. Written in assembler, but a dog nonetheless. If you find any
  calls to StrPos() in the XML code, you can crank some more from it using
  this implementation:
 }
var
 sub, str: PChar;
begin
  if (ASubString <> nil) and (ASubString^ <> #0) and (AString <> nil) then
  begin
    str := AString;
    while str^ <> #0 do
    begin
      sub := ASubString;
      // look for start of sub in str or end of str
      while (str^ <> sub^) and (str^ <> #0) do
        Inc(str);
      // check end of str
      if str^ <> #0 then
      begin
        // remember start of sub in str
        Result := str;
        // look for end of sub in str or end of sub or end of str
        while (str^ = sub^) and (str^ <> #0) do
        begin
          Inc(str);
          Inc(sub);
        end;
        // success if sub^ = #0
        if sub^ = #0 then
          exit;
      end;
    end;
  end;
  Result := nil;
end;


function tiGMTOffset: TDateTime;
var
  TZI: TTimeZoneInformation;
begin
  Result := 0; // Removes 'return value might be undefined' warning.
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD: Result := (TZI.Bias + TZI.StandardBias) * cdtOneMinute;
    TIME_ZONE_ID_DAYLIGHT: Result := (TZI.Bias + TZI.DaylightBias) * cdtOneMinute;
    TIME_ZONE_ID_UNKNOWN: Result := TZI.Bias * cdtOneMinute;
  else
    RaiseLastOSError;
  end;
end;

function tiOffsetFromGMT: TDateTime;
begin
  Result := -tiGMTOffset;
end;

function tiTimeToGMT(const ALocalTime: TDateTime; const AOffsetFromGMTHours: Integer): TDateTime;
begin
  Result := ALocalTime - (AOffsetFromGMTHours / 24);
end;

function tiLocalTimeToGMT(const ALocalTime: TDateTime): TDateTime;
begin
  Result := ALocalTime + tiGMTOffset;
end;

function tiGMTToTime(const AGMTTime: TDateTime; const AOffsetFromGMTHours: Integer): TDateTime;
begin
  Result := AGMTTime + (AOffsetFromGMTHours / 24);
end;

function tiGMTToLocalTime(const AGMTTime: TDateTime): TDateTime;
begin
  Result := AGMTTime + tiOffsetFromGMT;
end;


function tiCreateGUIDString: string;
begin
  Result := tiWin32CoCreateGUID;
end;

function tiGetTickCount: Cardinal;
begin
  Result := tiWin32GetTickCount;
end;

function tiApplicationName: string;
begin
  Result := ChangeFileExt(ExtractFileName(Paramstr(0)),'');
end;

function tiGetCommonAppDataDir(const AAppDataSubDir: string): string;
begin
  result := tiWin32GetCommonAppDir + AAppDataSubDir;
end;

function tiGetUserLocalAppDataDir(const AAppDataSubDir: string): string;
begin
  result := tiWin32GetUserLocalAppDir + AAppDataSubDir;
end;

function tiGetCurrentUserPersonalDir: string;
begin
  result := tiWin32GetCurrentUserPersonalDir;
end;


