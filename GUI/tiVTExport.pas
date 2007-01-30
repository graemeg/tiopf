unit tiVTExport;

interface

uses
  Classes
  ,tiRegINI
  ,tiVTListView
  ,tiStreams
  ,tiObject
  ;

type
  TtiVTExport = class(TtiObject)
  protected
    FOutputStr: string;
    FSourceTree: TtiCustomVirtualEditTree;
    FStream: TtiPreSizedStream;
    procedure WriteToOutput(const AText: string); virtual;
    function CreateOutput: boolean; virtual;
    procedure WriteHeader; virtual;
    procedure WriteTitles; virtual;
    procedure WriteDetail; virtual;
    procedure WriteFooter; virtual;
    procedure CloseOutput; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function FormatName: string; virtual; abstract;
    procedure Execute(const ATree: TtiCustomVirtualEditTree);
  end;

  TtiVTExportFile = class(TtiVTExport)
  protected
    FFileName: string;
    FDefaultExt: string;
    function GetFileName: Boolean; virtual;
    function CreateOutput: boolean; override;
    procedure CloseOutput; override;
    property FileName: string read FFileName write FFileName;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;

  TtiVTExportClass = class of TtiVTExport;

  TtiVTExportFileNameStore = class(TtiObject)
  public
    function GetFileName(const AVT: TComponent; const AExportFormatName: string): string; virtual; abstract;
    procedure SetFileName(const AVT: TComponent; const AExportFormatName: string; const AFileName: string); virtual; abstract;
  end;

  TtiVTIniExportFileNameStore = class (TtiVTExportFileNameStore)
  private
    FIniFile: TtiINIFile;
    procedure GetID(const AVT: TComponent; const AExportFormatName: string;
      out ASection, AIdent: string);
  public
    constructor CreateExt(const AIniFile: TtiINIFile);
    function GetFileName(const AVT: TComponent;
      const AExportFormatName: string): string; override;
    procedure SetFileName(const AVT: TComponent;
      const AExportFormatName: string; const AFileName: string); override;
  end;

  TtiVTExportRegistry = class(TTiObject)
  private
    FExportMappings: TStrings;
    FExportFileNameStore: TtiVTExportFileNameStore;
    function GetExportCount: integer;
    procedure SetExportFileNameStore(const AValue: TtiVTExportFileNameStore);
    function GetExportFileNameStore: TtiVTExportFileNameStore;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterExport(const AVTExportClass: TtiVTExportClass);
    function GetExport(const AFormatName: string): TtiVTExportClass; overload;
    function GetExport(const AIndex: integer): TtiVTExportClass; overload;
    property ExportCount: integer read GetExportCount;
    property ExportFileNameStore: TtiVTExportFileNameStore
      read GetExportFileNameStore write SetExportFileNameStore;
  end;

function tiVTExportRegistry: TtiVTExportRegistry;

implementation

uses
  SysUtils
  ,Dialogs
  ;

var
  utiVTExportRegistry: TtiVTExportRegistry;

function tiVTExportRegistry: TtiVTExportRegistry;
begin

  if (utiVTExportRegistry = nil) then
    utiVTExportRegistry := TtiVTExportRegistry.Create;

  Result := utiVTExportRegistry;
end;

  { TtiVTExport }

procedure TtiVTExport.CloseOutput;
begin
  FreeAndNil(FStream);
end;

constructor TtiVTExport.Create;
begin
  inherited Create;
end;

function TtiVTExport.CreateOutput: boolean;
begin
  FStream := TtiPreSizedStream.Create(0, cStreamGrowBy);
  Result := true;
end;

destructor TtiVTExport.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TtiVTExport.Execute(const ATree: TtiCustomVirtualEditTree);
begin
  FSourceTree := ATree;
  Assert(Assigned(FSourceTree),
    'Internal error: TtiVTExport.Execute - unassignsed ATree');

  if CreateOutput then
  begin
    WriteHeader;
    WriteTitles;
    WriteDetail;
    WriteFooter;
    CloseOutput;
  end;
end;

procedure TtiVTExport.WriteDetail;
begin

end;

procedure TtiVTExport.WriteFooter;
begin

end;

procedure TtiVTExport.WriteHeader;
begin

end;

procedure TtiVTExport.WriteTitles;
begin

end;

procedure TtiVTExport.WriteToOutput(const AText: string);
begin
  FStream.WriteLn(AText);
end;

{ TtiVTExportFile }

procedure TtiVTExportFile.CloseOutput;
begin

  try
    FStream.SaveToFile(FFileName);
  except
    on E: Exception do
    begin
      ShowMessage(Format('Error creating output file %s (%s).', [FFileName,
        E.Message]));
    end;
  end;

end;

function TtiVTExportFile.CreateOutput: boolean;
begin
  Result := inherited CreateOutput and GetFileName;
end;

function TtiVTExportFile.GetFileName: Boolean;
const
  fmtFilter = '%s Files|*.%s|All Files|*.*';
var
  LSaveDialog: TSaveDialog;

begin
  Result := false;
  FFileName := tiVTExportRegistry.ExportFileNameStore.GetFileName(
    FSourceTree, FormatName);

  if Assigned(FSourceTree.OnExportFileName) then
    FSourceTree.OnExportFileName(FormatName, FFileName, Result)
  else
  begin
    LSaveDialog := TSaveDialog.Create(nil);

    try
      LSaveDialog.DefaultExt := FDefaultExt;
      LSaveDialog.FileName := FFileName;
      LSaveDialog.Filter := Format(fmtFilter, [FormatName, FDefaultExt]);

      if LSaveDialog.Execute then
      begin
        FFileName := LSaveDialog.FileName;
        tiVTExportRegistry.ExportFileNameStore.SetFileName(FSourceTree,
          FormatName, FFileName);
        Result := True;
      end;

    finally
      LSaveDialog.Free;
    end;

  end;

  if Result then
    tiVTExportRegistry.ExportFileNameStore.SetFileName(FSourceTree, FormatName, FFileName);

end;

{ TtiVTExportRegistry }

constructor TtiVTExportRegistry.Create;
begin
  inherited Create;
  FExportMappings := TStringList.Create;
  (FExportMappings as TStringList).Sorted := true;
  (FExportMappings as TStringList).Duplicates := dupError;
end;

destructor TtiVTExportRegistry.Destroy;
begin
  FExportMappings.Free;
  SetExportFileNameStore(nil);
  inherited;
end;

function TtiVTExportRegistry.GetExport(const AFormatName: string): TtiVTExportClass;
var
  I: Integer;
begin
  I := FExportMappings.IndexOf(UpperCase(AFormatName));
  if (I = -1) then
    ShowMessage('Request for invalid export class "' + AFormatName + '"');
  Result := TtiVTExportClass(FExportMappings.Objects[I]);
end;

function TtiVTExportRegistry.GetExport(const AIndex: integer): TtiVTExportClass;
begin
  Assert( (AIndex >= 0) and (AIndex < FExportMappings.Count),
    'Internal error: bad export index');
  Result := TtiVTExportClass(FExportMappings.Objects[AIndex]);
end;

function TtiVTExportRegistry.GetExportCount: integer;
begin
  Result := FExportMappings.Count;
end;

function TtiVTExportRegistry.GetExportFileNameStore: TtiVTExportFileNameStore;
begin
  // Do this assignment as late as possible (ie, here!) so user has a chance to
  // register a custom filename store after creating registry without clobbering
  // default store.
  if not Assigned(FExportFileNameStore) then
    SetExportFileNameStore(TtiVTIniExportFileNameStore.CreateExt(gINI));

  Result := FExportFileNameStore;
end;

procedure TtiVTExportRegistry.RegisterExport(const AVTExportClass: TtiVTExportClass);
var
  LFormatName: string;

begin
  LFormatName := UpperCase(AVTExportClass.FormatName);
  try
    FExportMappings.AddObject(LFormatName, TObject(AVTExportClass));
  except
    on EStringListError do
    begin
      ShowMessage('Attempt to register duplicate export "'
        + AVTExportClass.FormatName + '"');
      Exit;
    end;
  end;

end;

procedure TtiVTExportRegistry.SetExportFileNameStore(
  const AValue: TtiVTExportFileNameStore);
begin
  if Assigned(FExportFileNameStore) then
    FExportFileNameStore.Free;

  FExportFileNameStore := AValue;
end;

{ TtiVTIniExportFileNameStore }

constructor TtiVTIniExportFileNameStore.CreateExt(const AIniFile: TtiINIFile);
begin
  inherited Create;
  FIniFile := AIniFile;
end;

function TtiVTIniExportFileNameStore.GetFileName(const AVT: TComponent;
  const AExportFormatName: string): string;
var
  LSection, LIdent: string;

begin
  GetID(AVT, AExportFormatName, LSection, LIdent);
  Result :=  FIniFile.ReadString(LSection, LIdent, '');
end;

procedure TtiVTIniExportFileNameStore.GetID(const AVT: TComponent;
  const AExportFormatName: string; out ASection, AIdent: string);
begin
  Assert(Assigned(AVT) and (AVT.Name <> ''),
    'Internal error: TExportFileNamePersister.GetID() - AVT or AVT.Name unassigned');

  if Assigned(AVT.Owner) and (AVT.Owner.Name <> '') then
    ASection := AVT.Owner.Name
  else
    ASection := 'nil';

  FmtStr(AIdent, '%s.%s', [AVT.Name, AExportFormatName]);
end;

procedure TtiVTIniExportFileNameStore.SetFileName(const AVT: TComponent;
   const AExportFormatName: string; const AFileName: string);
var
  LSection, LIdent: string;

begin
  GetID(AVT, AExportFormatName, LSection, LIdent);
  FIniFile.WriteString(LSection, LIdent, AFileName);
end;

initialization
finalization

  utiVTExportRegistry.Free;
end.

