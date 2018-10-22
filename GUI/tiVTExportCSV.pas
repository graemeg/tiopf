unit tiVTExportCSV;

interface

{$IFDEF VIRTUAL_TREEVIEW}

uses
  tiVTListView
  ;

type
  TtiVTExportCSV = class(TtiVTExportFile)
  protected
    function GetFileName: Boolean; override;
    procedure WriteDetail; override;
    procedure WriteHeader; override;
  public
    class function FormatName: string; override;
    class function ImageName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

uses
  tiResources
  ;

resourcestring
  CSVDesc = 'CSV';

const
  CSVFieldSeparator = ',';
  CSVFieldDelimiter = '"';

  { TtiVTExportCSV }

function TtiVTExportCSV.GetFileName: Boolean;
begin
  FDefaultExt := 'csv';
  Result := inherited GetFileName;
end;

class function TtiVTExportCSV.ImageName: string;
begin
  Result := cResTI_ExportToCSV;
end;

class function TtiVTExportCSV.FormatName: string;
begin
  Result := CSVDesc;
end;

procedure TtiVTExportCSV.WriteDetail;
begin
  WriteDetailEx(CSVFieldSeparator, CSVFieldDelimiter);
end;

procedure TtiVTExportCSV.WriteHeader;
begin
  WriteHeaderEx(CSVFieldSeparator, CSVFieldDelimiter);
end;

initialization

  tiVTExportRegistry.RegisterExport(TtiVTExportCSV);

{$ENDIF}

end.

