{ Implementation section of tiUtils for platform specific code only.

  Platform: MACOS }


function tiGetTempFile(const AFileNameExtension : string): string;
begin
  Result := TPath.GetTempFilename;
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
  result := TPath.GetTempPath;
end;


procedure tiCopyFile(const AFrom, ATo : string);
var
  LResultCode : Longword;
begin
  TFile.Copy(AFrom, ATo);
  LResultCode := getLastError();

  if LResultCode <> 0 then
    raise EtiOPFFileSystemException.CreateFmt(
      CErrorCanNotCopyFile,
      [AFrom, ATo, LResultCode, sysErrorMessage(LResultCode)]);
end;



function tiGetUserName : string;
begin
//  {$IFDEF MSWINDOWS}
//    Result := tiWin32GetUserName;
//  {$ENDIF}
//  {$IFDEF UNIX}
//    Result := tiUnixGetUserName;
//  {$ENDIF}

//TODO - TO BE TESTED

  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
  // Used it program is run from cron jobs
  if Result = '' then
    Result := GetEnvironmentVariable('LOGNAME');
end;


function tiGetComputerName : string;
begin
//  {$IFDEF MSWINDOWS}
//  Result := tiWin32GetComputerName;
//  {$ENDIF MSWINDOWS}
//  {$IFDEF UNIX}
//  Result := tiUnixGetComputerName;
//  {$ENDIF}

//TODO

  Result := '';
end;


//TODO - TO BE TESTED
procedure tiSetFileReadOnly(AFileName: string; AReadOnly: boolean);
var
  lAttr: TFileAttributes;
begin
  lAttr := TFile.GetAttributes(AFileName);
  if AReadOnly <> (TFileAttribute.faOwnerWrite in lAttr) then
  begin
    if AReadOnly then
      Exclude(lAttr, TFileAttribute.faOwnerWrite)
    else
      Include(lAttr, TFileAttribute.faOwnerWrite);
    TFile.SetAttributes(AFileName, lAttr);
  end;
end;


{$IFDEF UNIX}
function tiIsFileReadOnly(AValue: string): boolean;
begin
  Result := FileIsReadOnly(AValue);
end;
{$ENDIF UNIX}


function tiGetModuleFileName: string;
begin
  Result := Paramstr(0);
end;


function tiGetEXEPath : string;
begin
  result := Paramstr(0);
  result := tiRemoveTrailingSlash(ExtractFilePath(Result));
end;



{ This allows us to always use a \ as a path separator. For Win32 it will
  do nothing, but for *Unix it will replace all \'s with /'s.  Now we don't have
  to have so many IFDEFs in the Unit Tests! }
function tiFixPathDelim(const AText: string): string;
begin
//  {$IFDEF MSWINDOWS}
//  result := AText;
//  {$ENDIF}
//  {$IFDEF UNIX}
//  result := SetDirSeparators(AText);
//  {$ENDIF}

//TODO
  result := AText;
end;

function tiGetAppDataDirPrivate: string;
begin
//TODO (may not be the required result)
  result := TPath.GetHomePath;
end;

function tiGetAppDataDirPublic: string;
begin
//TODO (may not be the required result)
  result := TPath.GetHomePath;
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
//{$IFDEF MSWINDOWS}
//  tiWin32RunEXEAndWait(LCommand, AInheritParentStartInfo);
//{$ENDIF MSWINDOWS}
//{$IFDEF UNIX}
//  tiUnixRunEXEAndWait(LCommand);
//{$ENDIF}

  Assert(False, 'tiRunEXEAndWait() not implemented');

end;



procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
var
  LFileDate: Integer;
  LExitCode: Integer;
begin
  LFileDate := DateTimeToFileDate(ADateTime);
//  {$IFDEF MSWINDOWS}
//  {$WARN SYMBOL_PLATFORM OFF}
//  LFileHandle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
//  try
//    if LFileHandle > 0 then
//    begin
//      LExitCode := FileSetDate(LFileHandle, LFileDate);
//      if LExitCode <> 0 then
//        raise EtiOPFFileSystemException.CreateFmt(
//            CErrorSettingFileDate, [
//            AFileName, tiDateTimeToStr(ADateTime),
//            LExitCode, SysErrorMessage(LExitCode)]);
//    end
//    else
//      raise exception.Create('Unable to set file date on <' + AFileName + '>');
//  finally
//    FileClose(LFileHandle);
//  end;
//  {$ENDIF MSWINDOWS}

  {$IFDEF UNIX}
    LExitCode := FileSetDate(AFileName, LFileDate);
    if LExitCode <> 0 then
      raise Exception.Create('Unable to set file date on <' + AFileName + '>');
  {$ENDIF UNIX}
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
  {$IFDEF FPC}
  { Free Pascal's implementation of StrPos is even faster than the tiStrPos
    version, so we rather call the Free Pascal one. }
  result := StrPos(AString, ASubString);
  {$ELSE}
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
  {$ENDIF}
end;



function tiCreateGUIDString: string;
var
  lGUID: TGUID;
begin
  CreateGUID(lGUID);
  Result := GUIDToString(lGUID);
end;

function tiGetTickCount: Cardinal;
begin
//TODO - TO BE TESTED
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function tiApplicationName: string;
begin
  {$IFDEF FPC}
  // FPC adds a few more features we don't want to loose, even under Windows
  Result := ApplicationName;
  {$ELSE}
  Result := ChangeFileExt(ExtractFileName(Paramstr(0)),'');
  {$ENDIF}
end;

function tiGetCommonAppDataDir(const AAppDataSubDir: string): string;
begin
//TODO (may not be the required result)
  result := TPath.GetHomePath;
end;

function tiGetUserLocalAppDataDir(const AAppDataSubDir: string): string;
begin
//TODO (may not be the required result)
  result := TPath.GetHomePath;
end;

function tiGetCurrentUserPersonalDir: string;
begin
//TODO (may not be the required result)
  result := TPath.GetHomePath;
end;

function IsCharAlpha(AChar: Char): Boolean;
begin
  // This is very primitive and doesn't take special chars or unicode in
  // consideration.
  if AChar in ['a'..'z', 'A'..'Z'] then
    Result := True
  else
    Result := False;
end;

