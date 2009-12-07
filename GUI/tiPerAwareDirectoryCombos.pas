unit tiPerAwareDirectoryCombos;

{$I tiDefines.inc}

interface
uses
  tiPerAwareCombosAbs
  ,Classes
  ,tiPerAwareCtrls
 ;

type

  TtiPickDirectory = class(TtiPickerAbs)
  private
    FbMustExist : boolean;
    FbCreateDir : boolean;
    procedure   PickDirectoryOnExit(sender : TObject);
  protected
    procedure   DoButtonClick(sender : TObject); override;
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
  FileCtrl
  ,Dialogs
  ,Controls
  ,SysUtils
  ,tiUtils
{$IFNDEF FPC}
  ,tiJVBrowseFolder
{$ENDIF}
  ,tiFocusPanel
//  ,Graphics
 ;

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

end.
