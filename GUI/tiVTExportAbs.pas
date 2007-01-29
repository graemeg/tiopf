unit tiVTExportAbs;

interface

uses
  Classes
  , tiVTListView
  , tiStreams
  ;

type
  TtiVTExportAbs = class
  protected
    FOutputStr: string;
    FSourceTree: TtiCustomVirtualTree;
    FStream: TtiPreSizedStream;
    procedure WriteToOutput(const AText: string); virtual;
    function CreateOutput: boolean; virtual;
    procedure WriteHeader; virtual;
    procedure WriteTitles; virtual;
    procedure WriteDetail; virtual;
    procedure WriteFooter; virtual;
    procedure CloseOutput; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function FormatName: string; virtual; abstract;
    procedure Execute;
    property SourceTree: TtiCustomVirtualTree
      read FSourceTree write FSourceTree;
  end;

  TtiVTExportFile = class(TtiVTExportAbs)
  protected
    FFileName: string;
    FDefaultExt: string;
    function GetFileName: Boolean; virtual;
    function CreateOutput: boolean; override;
    procedure CloseOutput; override;
    property FileName: string read FFileName write FFileName;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;

implementation

uses
  tiLog
  , SysUtils
  , Dialogs
  , tiVTExportFactory
  ;

const
  fmtFilter = '%s Files|*.%s|All Files|*.*';

  { TtiVTExportAbs }

procedure TtiVTExportAbs.CloseOutput;
begin
  FreeAndNil(FStream);
end;

constructor TtiVTExportAbs.Create;
begin
  inherited Create;
end;

function TtiVTExportAbs.CreateOutput: boolean;
begin
  FStream := TtiPreSizedStream.Create(0, cStreamGrowBy);
  Result := true;
end;

destructor TtiVTExportAbs.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TtiVTExportAbs.Execute;
begin
  if CreateOutput then
  begin
    WriteHeader;
    WriteTitles;
    WriteDetail;
    WriteFooter;
    CloseOutput;
  end;
end;

procedure TtiVTExportAbs.WriteDetail;
begin

end;

procedure TtiVTExportAbs.WriteFooter;
begin

end;

procedure TtiVTExportAbs.WriteHeader;
begin

end;

procedure TtiVTExportAbs.WriteTitles;
begin

end;

procedure TtiVTExportAbs.WriteToOutput(const AText: string);
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
var
  LSaveDialog: TSaveDialog;
begin
  Result := false;
  FFileName := tiVTExportRegistry.GetExport(FormatName).LastSavedFileName;
  LSaveDialog := TSaveDialog.Create(nil);

  try
    LSaveDialog.DefaultExt := Self.DefaultExt;
    LSaveDialog.FileName := Self.FileName;
    LSaveDialog.Filter := Format(fmtFilter, [FormatName, FDefaultExt]);
    if LSaveDialog.Execute then
    begin
      FFileName := LSaveDialog.FileName;
      tiVTExportRegistry.GetExport(FormatName).LastSavedFileName := FFileName;
      Result := True;
    end;
  finally
    LSaveDialog.Free;
  end;

end;

end.

