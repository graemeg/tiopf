unit tiPerAwareDirectoryCombos;

{$I tiDefines.inc}

interface
uses
  Classes,
  tiPerAwareCombosAbs,
  tiPerAwareCtrls,
  tiSpeedButton;

const
  CErrorDirectoryNotFound = 'Sorry, the directory "%s" does not appear to exist.';

type

  TtiPickDirectory = class(TtiPickerAbs)
  private
    FSBShowDirectory: TtiSpeedButton;
    FbMustExist : boolean;
    FbCreateDir : boolean;
    procedure   PickDirectoryOnExit(sender : TObject);
  protected
    procedure   SetName(const NewName: TComponentName); override;
    procedure   DoResize; override;
    procedure   DoButtonClick(sender : TObject); override;
    procedure   DoShowDirectoryButtonClick(sender : TObject);
  published
    property    MustExist : boolean read FbMustExist write FbMustExist;
    property    CreateDir : boolean read FbCreateDir write FbCreateDir default false;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TtiPerAwarePickDirectory = class(TtiPerAwarePickAbs)
  public
    constructor Create(AOwner : TComponent); override;
  end;

implementation
uses
  Controls,
  SysUtils,
  Dialogs,
  tiUtils,
{$IFNDEF FPC}
  tiJVBrowseFolder,
{$ENDIF}
  tiFocusPanel,
  tiResources,
  tiDialogs,
  tiGUIUtils;

{ TtiPerAwarePickDirectory }

constructor TtiPerAwarePickDirectory.Create(AOwner: TComponent);
begin
  WinControl := TtiPickDirectory.Create(self);
  TtiPickDirectory(WinControl).OnChange := DoChange;
  OnDblClick := TtiPickDirectory(WinControl).DoButtonClick;
  inherited;
  FLabel.OnDblClick := OnDblClick;
end;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPickDirectory
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPickDirectory.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FbCreateDir := false;
  self.onExit := pickDirectoryOnExit;
  OnDblClick := DoButtonClick;

  FSBShowDirectory := TtiSpeedButton.Create(self);
  FSBShowDirectory.top := 0;
  FSBShowDirectory.width := 22;
  FSBShowDirectory.parent := self;
  FSBShowDirectory.ParentFont := true;
  {$IFNDEF FPC}
  FSBShowDirectory.glyph.LoadFromResourceName(HInstance, cResTI_FileOpen16N);
  FSBShowDirectory.glyphHot.LoadFromResourceName(HInstance, cResTI_FileOpen16H);
  FSBShowDirectory.GlyphDisabled.LoadFromResourceName(HInstance, cResTI_FileOpen16D);
  {$ELSE}
  FSBShowDirectory.Layout := blGlyphRight;
  FSBShowDirectory.glyph.LoadFromLazarusResource(cResTI_FileOpen16N);
  FSBShowDirectory.glyphHot.LoadFromLazarusResource(cResTI_FileOpen16H);
  FSBShowDirectory.GlyphDisabled.LoadFromLazarusResource(cResTI_FileOpen16D);
  {$ENDIF}
  FSBShowDirectory.onClick := DoShowDirectoryButtonClick;

end;

procedure TtiPickDirectory.DoButtonClick(sender : TObject);
var
  lBFD : TtiJVBrowseForFolderDialog;
  LDirectory: string;
begin
  LDirectory := Edit.Text;

  lBFD := TtiJVBrowseForFolderDialog.Create(nil);
  try
    if lBFD.ShowDialog(LDirectory) then
    begin
      Edit.Text := LDirectory;
      DoOnChange(Self);
    end;
  finally
    lBFD.Free;
  end;
  inherited DoButtonClick(Sender);
end;

procedure TtiPickDirectory.DoResize;
var
 LHeight: Integer;
 LWidth : Integer;
begin
  if (Edit=nil) or (SpeedButton=nil) then Exit;
  LWidth := self.clientWidth - SpeedButton.Width - FSBShowDirectory.Width;
  LHeight := self.ClientHeight;
  if (LWidth<=1) or (LHeight<=0) then Exit;
  SpeedButton.left := LWidth - 1;
  SpeedButton.height := LHeight;
  FSBShowDirectory.Left:= LWidth + SpeedButton.Width;
  FSBShowDirectory.Height:= LHeight;
  Edit.height := LHeight;
  Edit.width := LWidth - 2;
end;

procedure TtiPickDirectory.DoShowDirectoryButtonClick(sender: TObject);
begin
  if DirectoryExists(Edit.Text) then
    tiShellExecute(Edit.Text)
  else
    tiAppError(Format(CErrorDirectoryNotFound, [Edit.Text]));
end;

procedure TtiPickDirectory.PickDirectoryOnExit(sender : TObject);
var sDirectory : string;
begin

  if not FbCreateDir then
    exit;

  sDirectory := self.text;
  if not DirectoryExists(sDirectory) then begin
    if messageDlg('Directory <' + sDirectory +
                       '> does not exist.' + #13 +
                       'Do you want to create it ?',
                       mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin
      self.setFocus;
      exit; //==>
    end;
    ForceDirectories(sDirectory);
    if not DirectoryExists(sDirectory) then begin
      raise exception.Create('Can not create directory <' +
                              sDirectory + '>');
    end;
  end;
end;

procedure TtiPickDirectory.SetName(const NewName: TComponentName);
begin
  inherited;
  if Assigned(FSBShowDirectory) then
    FSBShowDirectory.Name := tiGetUniqueComponentNameFromParent(Self, 'sbShowDirectory');
end;

end.
