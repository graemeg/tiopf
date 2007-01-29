unit tiVTExportFactory;

interface

uses
  Classes
  , tiVTExportAbs
  ;

type
  TtiVTExportClass = class of TtiVTExportAbs;

  TtiVTExport = class
  private
    FExportClass: TtiVTExportClass;
    FLastSavedFileName: string;
  published
  public
    property ExportClass: TtiVTExportClass read FExportClass write FExportClass;
    property LastSavedFileName: string read FLastSavedFileName write FLastSavedFileName;
  end;

  TtiVTExportRegistry = class
  private
    FExportMappings: TStrings;
    function GetExportCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterExport(const AVTExportClass: TtiVTExportClass);
    function GetExport(const AFormatName: string): TtiVTExport; overload;
    function GetExport(const AIndex: integer): TtiVTExport; overload;
    property ExportCount: integer read GetExportCount;
  end;

function tiVTExportRegistry: TtiVTExportRegistry;

implementation

uses
  SysUtils
  , Dialogs
  ;

var
  utiVTExportRegistry: TtiVTExportRegistry;

function tiVTExportRegistry: TtiVTExportRegistry;
begin

  if (utiVTExportRegistry = nil) then
    utiVTExportRegistry := TtiVTExportRegistry.Create;

  Result := utiVTExportRegistry;
end;

{ TtiVTExportRegistry }

constructor TtiVTExportRegistry.Create;
begin
  inherited;
  FExportMappings := TStringList.Create;
  (FExportMappings as TStringList).Sorted := true;
  (FExportMappings as TStringList).Duplicates := dupError;
end;

destructor TtiVTExportRegistry.Destroy;
begin

  while FExportMappings.Count > 0 do
  begin
    FExportMappings.Objects[0].Free;
    FExportMappings.Delete(0);
  end;

  FExportMappings.Free;
  inherited;
end;

function TtiVTExportRegistry.GetExport(const AFormatName: string): TtiVTExport;
var
  I: Integer;
begin
  I := FExportMappings.IndexOf(UpperCase(AFormatName));
  if (I = -1) then
    ShowMessage('Request for invalid export class "' + AFormatName + '"');
  Result := TtiVTExport(FExportMappings.Objects[I]);
end;

function TtiVTExportRegistry.GetExport(const AIndex: integer): TtiVTExport;
begin
  Assert( (AIndex >= 0) and (AIndex < FExportMappings.Count),
    'Internal error: bad export index');
  Result := TtiVTExport(FExportMappings.Objects[AIndex]);  
end;

function TtiVTExportRegistry.GetExportCount: integer;
begin
  Result := FExportMappings.Count;
end;

procedure TtiVTExportRegistry.RegisterExport(const AVTExportClass: TtiVTExportClass);
var
  LFormatName: string;
  LExport: TtiVTExport;

begin
  LFormatName := UpperCase(AVTExportClass.FormatName);
  LExport := TtiVTExport.Create;
  LExport.FExportClass := AVTExportClass;
  try
    FExportMappings.AddObject(LFormatName, LExport);
  except
    on EStringListError do
    begin
      ShowMessage('Attempt to register duplicate export "'
        + AVTExportClass.FormatName + '"');
      Exit;
    end;
  end;

end;

initialization
finalization

  utiVTExportRegistry.Free;
end.

