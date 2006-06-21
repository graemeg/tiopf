unit tiWin32;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ;

  procedure tiWin32RunEXEAndWait( pStrEXE : string ) ;
  function  tiWin32FileGetAttr( const pFileName : string ) : integer ;
  function  tiWin32FileSetAttr(const pFileName: string; pAttr: Integer): Integer;
  function  tiWin32FindFirstFile(const Path: string; var  F: TSearchRec): Integer;
  function  tiWin32CoCreateGUID : String ;
  procedure tiWin32CoInitialize;
  procedure tiWin32CoUnInitialize;

implementation
uses
  Windows
  ,ComObj
  ,ActiveX
  ,Classes
  ;

var
  uCoInitializeList : TList ;

//{$ifdef DELPHI6ORABOVE}
//  {$WARN SYMBOL_PLATFORM OFF}
//{$endif}

procedure tiWin32RunEXEAndWait( pStrEXE : string ) ;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  GetStartupInfo(SI);
  Win32Check(
    CreateProcess(
      nil, PChar(pStrEXE), nil, nil,
      False, 0, nil, nil, SI, PI));
  WaitForInputIdle(PI.hProcess, Infinite);
  WaitForSingleObject(PI.hProcess, Infinite);
end ;

function tiWin32FileGetAttr( const pFileName : string ) : integer ;
begin
  result := fileGetAttr( pFileName ) ;
end;

function tiWin32FileSetAttr(const pFileName: string; pAttr: Integer): Integer;
begin
  result := FileSetAttr( pFileName, pAttr);
end;

function tiWin32FindFirstFile(const Path: string; var  F: TSearchRec): Integer;
begin
  result := FindFirst(Path, faAnyFile-faVolumeID-faSYSFile-faDirectory, F);
end ;

function tiWin32CoCreateGUID : string ;
var
  lGuid : TGUID;
begin
  tiWin32CoInitialize;
  CoCreateGuid(lGUID);
  Result := GuidToString(lGUID);
end;

procedure tiWin32CoInitialize ;
var
  i : integer ;
  liThreadID : integer ;
begin
  if uCoInitializeList = nil then
    uCoInitializeList := TList.Create ;
  liThreadID := GetCurrentThreadID ;
  for i := 0 to uCoInitializeList.Count - 1 do
  begin
    if Integer( TObject( uCoInitializeList.Items[i] )) = liThreadID then
      Exit ; //==>
  end ;
  // If you get here, then CoInitialize for this thread has not been called
  CoInitialize( nil ) ;
  uCoInitializeList.Add( TObject( liThreadID )) ;
end ;

procedure tiWin32CoUnInitialize ;
var
  i : integer ;
  liThreadID : integer ;
begin
  liThreadID := GetCurrentThreadID ;
  for i := 0 to uCoInitializeList.Count - 1 do
    if Integer( TObject( uCoInitializeList.Items[i] )) = liThreadID then
    begin
      CoUnInitialize ;
      uCoInitializeList.Delete(i) ;
      Exit ; //==>
    end ;
end ;


//{$ifdef DELPHI6ORABOVE}
//  {$WARN SYMBOL_PLATFORM ON}
//{$endif}

initialization
  // Have moved tiCoInitialize to tiWin32CoInitialize, which is not in
  // tiPersistCore. Not sure about calling CoInitialize in the initialization
  // section as it will be called even when it is not needed. Must be
  // strict about calling it when required in the code that uses COM
  // tiCoInitialize ;

finalization
  //tiCoUnInitialize ;
  // Should call tiWin32CoUnInitialize?
  uCoInitializeList.Free ;


end.
