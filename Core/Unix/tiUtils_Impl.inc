{ Implementation section of tiUtils for platform specific code only.

  Platform: Unix }


function tiGetTempFile(const AFileNameExtension : string): string;
{$IFNDEF FPC}
const
  cMaxPathLen = 255;
var
  pcTemp : array[0..cMaxPathLen] of char;
  pcApp : array[0..cMaxPathLen] of char;
  pcPath : array[0..cMaxPathLen] of char;
{$ENDIF FPC}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetTempFileName('', '');    // prefix of TMP is default
  {$ELSE}
  strPCopy(pcApp, copy(extractFileName(ParamStr(0)), 1, 3));
  getTempPath(cMaxPathLen, pcPath);
  getTempFileName(pcPath, pcApp, 0, pcTemp);
  deleteFile(pcTemp); // This is using the Window deleteFile, not Delphi's
  result := strPas(pcTemp);
  {$ENDIF}
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
  {$IFDEF FPC}
  result := GetTempDir;
  {$ELSE}
  result := ExtractFilePath(tiGetTempFile('tmp'));
  {$ENDIF}
end;



procedure tiCopyFile(const AFrom, ATo : string);
var
  LResultCode : Longword;
  {$IFDEF FPC}
    function fpcCopyFile(Org, Dest:string): boolean;
    var
      Source, Target:TFileStream;
    begin
      Result := false;
      try
        try
          Source := TFileStream.Create(Org, fmShareDenyNone or fmOpenRead);
          try
            Target := TFileStream.Create(Dest, fmOpenWrite or fmCreate);
            Target.CopyFrom(Source, Source.Size);
            Result := true;
          finally
            Target.Free;
          end;
        finally
          Source.Free;
        end;
      except
      end;
    end;
  {$ENDIF}
begin
{$IFNDEF FPC}
  copyFile(pChar(AFrom), pChar(ATo), false);
  LResultCode := getLastError();
{$ELSE}
  if fpcCopyFile(AFrom, ATo) then
    LResultCode := 0
  else
    LResultCode := GetLastOSError;
{$ENDIF}
  if LResultCode <> 0 then
    raise EtiOPFFileSystemException.CreateFmt(
      CErrorCanNotCopyFile,
      [AFrom, ATo, LResultCode, sysErrorMessage(LResultCode)]);
end;



function tiGetUserName : string;
begin
  Result := tiUnixGetUserName;
end;


function tiGetComputerName : string;
begin
  Result := tiUnixGetComputerName;
end;



{ Works on Owner rights only. Unix has 3 sets of rights: Owner, Group and Global }
procedure tiSetFileReadOnly(AFileName: string; AReadOnly: boolean);
var
  lAttr: LongInt;
  Info: Stat;
begin
  lAttr := FileGetAttr(AFileName);
  if lAttr <> -1 then
  begin
    if FpStat(AFileName, Info) < 0 then
      raise Exception.Create('Unable to set ReadOnly attribute');

    if AReadOnly then
      FpChmod(AFileName, Info.st_mode xor S_IWUSR)   // remove write access. Owner rights only.
    else
      FpChmod(AFileName, Info.st_mode or S_IWUSR);   // add write access.
  end
  else
    raise Exception.Create('Unable to set ReadOnly attribute');
end;

function tiIsFileReadOnly(AValue: string): boolean;
begin
  Result := FileIsReadOnly(AValue);
end;


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
  result := SetDirSeparators(AText);
end;

function tiGetAppDataDirPrivate: string;
begin
  {$IFDEF FPC}
  result := GetAppConfigDir(False);
  {$ELSE}
  result := tiWin32GetAppConfigDir(False);
  {$ENDIF}
end;

function tiGetAppDataDirPublic: string;
begin
  {$IFDEF FPC}
  result := GetAppConfigDir(True);
  {$ELSE}
  result := tiWin32GetAppConfigDir(True);
  {$ENDIF}
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
  tiUnixRunEXEAndWait(LCommand);
end;



procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
var
  LFileDate: Integer;
  LExitCode: Integer;
begin
  LFileDate := DateTimeToFileDate(ADateTime);
  LExitCode := FileSetDate(AFileName, LFileDate);
  if LExitCode <> 0 then
     raise Exception.Create('Unable to set file date on <' + AFileName + '>');
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
{$IFDEF FPC}
var
  lGUID: TGUID;
{$ENDIF}
begin
{$IFDEF FPC}
  CreateGUID(lGUID);
  Result := GUIDToString(lGUID);
{$ELSE}
  Result := tiWin32CoCreateGUID;
{$ENDIF}
end;

function tiGetTickCount: Cardinal;
begin
  Result := tiUnixGetTickCount;
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
  {$IFDEF FPC}
  //TODO: Change to point at the correct subfolder
  result := GetAppConfigDir(True);
  {$ELSE}
  result := tiWin32GetCommonAppDir + AAppDataSubDir;
  {$ENDIF}
end;

function tiGetUserLocalAppDataDir(const AAppDataSubDir: string): string;
begin
  {$IFDEF FPC}
  //TODO: Change to point at the correct subfolder
  result := GetAppConfigDir(False);
  {$ELSE}
  result := tiWin32GetUserLocalAppDir + AAppDataSubDir;
  {$ENDIF}
end;

function tiGetCurrentUserPersonalDir: string;
begin
  {$IFDEF FPC}
  //TODO
  result := '';
  {$ELSE}
  result := tiWin32GetCurrentUserPersonalDir;
  {$ENDIF}
end;

