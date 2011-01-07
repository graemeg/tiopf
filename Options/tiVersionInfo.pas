{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  (c) TechInsite Pty. Ltd.
  PO Box 429, Abbotsford, Melbourne. 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au
  EMail: peter_hinrichsen@techinsite.com.au

  Created: Mid 1998

  Purpose: Read a file's version inforation.
           This component was downloaded from ???

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiVersionInfo;

{$I tiDefines.inc}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Windows, ExtCtrls, StdCtrls, tiBaseObject
  ;

type

  TtiVersionInfo = class(TtiBaseObject)
  private
    fFileAge: Integer;
    fVersionData: PAnsiChar;
    fVerStrings: TStringList;
    fExecutableFileName: String;
    FMemo : TMemo ;
    procedure ReadOnlyStringProperty (Index: Integer; const Value: String);
    procedure ReadOnlyIntegerProperty (Index, Value: Integer);
    procedure SetVersionStrings (Value: TStringList);
    function GetIndexStringProperty (Index: Integer): String;
    function GetIndexIntegerProperty (Index: Integer): Integer;
    procedure ParseVersionData;
    function GetKey (const KeyName: String): String;
    function GetExecutableDate : string ;
    procedure SetMemo( value : TMemo ) ;

  protected
      { Protected declarations }

    property ExecutableFileName: String index 0 read GetIndexStringProperty write ReadOnlyStringProperty;
    property ExecutableFileDate: String index 1 read GetIndexStringProperty write ReadOnlyStringProperty;
    property CompanyName       : String index 2 read GetIndexStringProperty write ReadOnlyStringProperty;
    property FileDescription   : String index 3 read GetIndexStringProperty write ReadOnlyStringProperty;
    property FileVersion       : String index 4 read GetIndexStringProperty write ReadOnlyStringProperty;
    property InternalName      : String index 5 read GetIndexStringProperty write ReadOnlyStringProperty;
    property LegalCopyright    : String index 6 read GetIndexStringProperty write ReadOnlyStringProperty;
    property LegalTrademarks   : String index 7 read GetIndexStringProperty write ReadOnlyStringProperty;
    property OriginalFilename  : String index 8 read GetIndexStringProperty write ReadOnlyStringProperty;
    property ProductName       : String index 9 read GetIndexStringProperty write ReadOnlyStringProperty;
    property ProductVersion    : String index 10 read GetIndexStringProperty write ReadOnlyStringProperty;
    property Comments          : String index 11 read GetIndexStringProperty write ReadOnlyStringProperty;

    property Keys: TStringList read fVerStrings write SetVersionStrings;
    property FileVersionHigh: Integer index $30 read GetIndexIntegerProperty write ReadOnlyIntegerProperty;
    property FileVersionLow: Integer index $34 read GetIndexIntegerProperty write ReadOnlyIntegerProperty;
    property ProductVersionHigh: Integer index $38 read GetIndexIntegerProperty write ReadOnlyIntegerProperty;
    property ProductVersionLow: Integer index $3C read GetIndexIntegerProperty write ReadOnlyIntegerProperty;
    property Memo : TMemo read FMemo write SetMemo ;

  public
    constructor Create;
    destructor Destroy; override;
    property Key [const KeyName: String]: String read GetKey;
    procedure DataToStrings( pStrings : TStrings ) ;
    procedure Refresh;

  end;

function tiGetEXEVersionNumber : string ;

implementation

function tiGetEXEVersionNumber : string ;
var
  lVersionInfo : TtiVersionInfo ;
begin
  lVersionInfo := TtiVersionInfo.Create;
  try
    lVersionInfo.Refresh ;
    result := lVersionInfo.FileVersion ;
  finally
    lVersionInfo.Free ;
  end ;
end ;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//*  TtiVersionInfo
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiVersionInfo.Create;
begin
    Inherited Create;
    fFileAge := -1;
    fVersionData := Nil;
    fVerStrings := TStringList.Create;
    // Run-time case
    fExecutableFileName := Application.ExeName;
end;

//------------------------------------------------------------------------------
destructor TtiVersionInfo.Destroy;
begin
    if fVersionData <> Nil then FreeMem (fVersionData);
    fVerStrings.Free;
    Inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.Refresh;
var
    pSrc: PAnsiChar;
    hMod: hModule;
    Res: hRsrc;
    lRes: hGlobal;
begin
  // Trash the existing version data buffer
  if fVersionData <> Nil then FreeMem (fVersionData);
  fVersionData := Nil;

  // Now get the updated stuff....
  if fExecutableFileName <> '' then begin
      hMod := LoadLibraryEx (PChar (fExecutableFileName), 0, Load_Library_As_DataFile);
      if hMod <> 0 then try
          Res := FindResource (hMod, PChar (1), rt_Version);
          if Res <> 0 then begin
              lRes := LoadResource (hMod, Res);
              if lRes <> 0 then begin
                  pSrc := LockResource (lRes);
                  if pSrc <> Nil then begin
                      // Sanity check time!
                      if PWideChar(pSrc + 6) = 'VS_VERSION_INFO' then begin
                          GetMem (fVersionData, SizeofResource (hmod, Res));
                          Move (pSrc^, fVersionData^, SizeofResource (hmod, Res));
                          ParseVersionData;

                          // Added as part of not refreshing bug fix
                          if FMemo <> nil then
                            DataToStrings( Memo.Lines ) ;

                      end;
                  end;
              end;
          end;
      finally
          FreeLibrary (hMod);
      end;
  end;
  if FMemo <> nil then
    DataToStrings( Memo.Lines ) ;
end;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.ReadOnlyStringProperty (Index: Integer; const Value: String);
begin
    // Read-only property
end;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.ReadOnlyIntegerProperty (Index, Value: Integer);
begin
    // Read-only property
end;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.SetVersionStrings (Value: TStringList);
begin
    // Read-only property
end;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.ParseVersionData;
const
    //---------------------------------------------------
    // ACHTUNG!  Don't change these constants unless the
    // format of the VERSION resource is altered.
    //---------------------------------------------------
    vSFIStart = $5C;              // Start of StringFileInfo block
    vSTStart  = vSFIStart + $24;  // Start of String table block
    vSStart   = vSTStart + $18;   // Start of String table proper
var
    p: PAnsiChar;
    pw: PWord absolute p;
    StringFileInfoLen, ThisEntryLen: Word;
    Key, Val: String;

    function Align32 (p: PAnsiChar): PAnsiChar;
    var
        pp: LongInt absolute p;
    begin
        pp := (pp + 3) and $fffffffc;
        Result := p;
    end;

begin
    // You can never have too many sanity checks...
    if PWideChar (fVersionData + vSFIStart + 6) <> 'StringFileInfo' then
        raise Exception.Create ('Unrecognised version block');
    // Looks good - parse the version strings
    fVerStrings.Clear;
    p := fVersionData + vSTStart;
    StringFileInfoLen := pw^;
    // Point at first entry
    p := fVersionData + vSStart;
    while p < (fVersionData + vSTStart + StringFileInfoLen) do begin
        ThisEntryLen := pw^;
        Key := PWideChar (p + 6);
        Val := PWideChar ((Align32 (p + 6 + ((Length (Key) + 1) * 2))));
        fVerStrings.Add (Key + '=' + Val);
        p := Align32 (p + ThisEntryLen);
    end ;
end ;

//------------------------------------------------------------------------------
function TtiVersionInfo.GetKey (const KeyName: String): String;
var
    S: String;
    Index, nPos: Integer;
begin
    if fVersionData = Nil then Result := '--not available--' else begin
       for Index := 0 to fVerStrings.Count - 1 do begin
           S := fVerStrings [Index];
           nPos := Pos ('=', S);
           if Copy (S, 1, nPos - 1) = KeyName then begin
               Result := Copy (S, nPos + 1, MaxInt);
               Exit;  //==>
           end;
       end;

       Result := '';
    end;
end;

//------------------------------------------------------------------------------
function TtiVersionInfo.GetIndexStringProperty (Index: Integer): String;
const
    PropName: array [2..11] of String = (

              'CompanyName',
              'FileDescription',
              'FileVersion',
              'InternalName',
              'LegalCopyright',
              'LegalTrademarks',
              'OriginalFilename',
              'ProductName',
              'ProductVersion',
              'Comments'                );
begin
    case Index of
        0:     Result := fExecutableFileName;
        1:     Result := getExecutableDate ;
        2..11: Result := GetKey (PropName [Index]);
    end;
end;

//------------------------------------------------------------------------------
function TtiVersionInfo.GetIndexIntegerProperty (Index: Integer): Integer;
begin
    if fVersionData = Nil then Result := -1 else
       Result := PInteger (fVersionData + Index)^;
end;

//------------------------------------------------------------------------------
function TtiVersionInfo.GetExecutableDate : string ;
var
  LFileAge: TDateTime;
begin
  try
    fileAge(FExecutableFileName, LFileAge);
    result := DateTimeToStr(LFileAge) ;
  except
    result := 'Unable to get executable date.' ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.SetMemo( value : TMemo ) ;
begin
  if FMemo <> nil then
    FMemo.Clear ;
  FMemo := Value ;
  if FMemo <> nil then begin
    DataToStrings( Memo.Lines ) ;
    FMemo.WordWrap := false ;
    FMemo.Font.Name := 'Courier New' ;
    FMemo.Font.Size := 8 ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure TtiVersionInfo.DataToStrings(pStrings: TStrings ) ;
begin
  with pStrings do begin
    Clear ;
    add( 'Comments:           ' + Comments           ) ;
    add( 'CompanyName:        ' + CompanyName        ) ;
    add( 'ExecutableFileDate: ' + ExecutableFileDate ) ;
    add( 'ExecutableFileName: ' + ExecutableFileName ) ;
    add( 'FileDescription:    ' + FileDescription    ) ;
    add( 'FileVersion:        ' + FileVersion        ) ;
    add( 'InternalName:       ' + InternalName       ) ;
    add( 'ProductName:        ' + ProductName        ) ;
    add( 'ProductVersion:     ' + ProductVersion     ) ;
  end ;
end;

end.

