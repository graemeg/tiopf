{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Created: Mid 1998

  Purpose: Custom controls and components found on the TechInsite component
           pallet

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerAwareDirectoryCombos;

interface
uses
  tiPerAwareCombosAbs
  ,Classes
  ,tiPerAwareCtrls
  ;

type

  //----------------------------------------------------------------------------
  TtiPickDirectory = class( TtiPickerAbs )
  private
    FbMustExist : boolean ;
    FbCreateDir : boolean;
    procedure   PickDirectoryOnExit( sender : TObject ) ;
    function    GetClosestValidDirectory(const pPath: string): string;
  protected
    procedure   DoButtonClick( sender : TObject ) ; override ;
  published
    property    MustExist : boolean read FbMustExist write FbMustExist ;
    property    CreateDir : boolean read FbCreateDir write FbCreateDir default false ;
  public
    constructor create( owner : TComponent ) ; override ;
  end ;

  TtiPerAwarePickDirectory = class( TtiPerAwarePickAbs )
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

implementation
uses
  FileCtrl
  ,Dialogs
  ,Controls
  ,SysUtils
  ,tiUtils
  ,tiJVBrowseFolder
  ,tiDialogs
  ,tiFocusPanel
//  ,Graphics
  ;

{ TtiPerAwarePickDirectory }

constructor TtiPerAwarePickDirectory.Create(Owner: TComponent);
begin
  WinControl := TtiPickDirectory.Create( self ) ;
  TtiPickDirectory( WinControl ).OnChange := DoChange ;
  OnDblClick := TtiPickDirectory( WinControl ).DoButtonClick;
  inherited;
  FLabel.OnDblClick := OnDblClick;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPickDirectory
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//------------------------------------------------------------------------------
constructor TtiPickDirectory.create( Owner : TComponent ) ;
begin
  inherited create( Owner ) ;
  FbCreateDir := false ;
  self.onExit := pickDirectoryOnExit ;
  OnDblClick := DoButtonClick ;
end ;

//------------------------------------------------------------------------------
procedure TtiPickDirectory.DoButtonClick( sender : TObject ) ;
var
  lBFD : TtiJVBrowseForFolderDialog;
begin
  lBFD := TtiJVBrowseForFolderDialog.Create(nil);
  try
    lBFD.Options := [odOnlyDirectory, odFileSystemDirectoryOnly,
                     odEditBox, odNewDialogStyle, odUsageHint,
                     odValidate];
    lBFD.RootDirectory:= fdMyComputer;
    lBFD.Position:= fpFormCenter;
    lBFD.Directory := GetClosestValidDirectory(Edit.Text) ;
    //lBFD.DisplayName:= Caption;
    //lBFD.Title:= Caption;
    //lBFD.StatusText:= 'Status text';
    if lBFD.Execute then
    begin
      Self.text := lBFD.directory ;
      DoOnChange( Self ) ;
    end;
  finally
    lBFD.Free;
  end;
  inherited DoButtonClick(Sender);
  ( Owner as TtiFocusPanel ).DoDrawFocusRect(True);
end ;

//------------------------------------------------------------------------------
procedure TtiPickDirectory.PickDirectoryOnExit( sender : TObject ) ;
var sDirectory : string ;
begin

  if not FbCreateDir then
    exit ;

  sDirectory := self.text ;
  if not DirectoryExists( sDirectory ) then begin
    if not messageDlg( 'Directory <' + sDirectory +
                       '> does not exist.' + #13 +
                       'Do you want to create it ?',
                       mtConfirmation, [mbYes, mbNo], 0 ) = mrYes then begin
      self.setFocus ;
      exit ; //==>
    end ;
    ForceDirectories( sDirectory ) ;
    if not DirectoryExists( sDirectory ) then begin
      raise exception.create( 'Can not create directory <' +
                              sDirectory + '>' ) ;
    end ;
  end ;
end ;

function TtiPickDirectory.GetClosestValidDirectory(const pPath: string): string;
var
  lPos : Integer ;
  lPath: string ;
begin
  if not DirectoryExists(pPath) then
  begin
    lPos := tiPosR('\', pPath);
    if lPos <> 0 then
    begin
      lPath := Copy(pPath, 0, lPos-1);
      Result := GetClosestValidDirectory(lPath);
    end else
      Result := 'C:\';
  end else
    Result := pPath ;
end;

end.
