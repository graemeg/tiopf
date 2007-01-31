unit tiVTExportCSV;

interface

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

  TTiVTExportClipboard = class(TtiVTExportCSV)
  protected
    function GetFileName: Boolean; override;
    procedure CloseOutput; override;
  public
    class function FormatName: string; override;
    class function ImageName: string; override;
  end;

implementation

uses
  SysUtils
  ,tiVirtualTrees
  ,Clipbrd           // global Clipboard object
  ,tiResources
  ,tiObject
  ;

resourcestring
  CSVDesc = 'CSV';
  ClipboardDesc = 'Clipboard';

const
  CSVFieldSeparator = ',';
  CSVFieldDelimiter = '"';

  { TtiVTExportCSV }

function TtiVTExportCSV.GetFileName: Boolean;
begin
  FDefaultExt := 'CSV';
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
var
  lNode: PVirtualNode;
  I: integer;
  LColumn: TtiVTColumn;
  LLastColumnIndex: integer;
  LObject: TtiObject;

begin
  lNode := FSourceTree.VT.GetFirst;

  while (lNode <> nil) do
  begin
    SetLength(FOutputStr, 0);
    LObject := FSourceTree.GetObjectFromNode(lNode);
    LLastColumnIndex := FSourceTree.VT.Header.Columns.Count - 1;

    for I := 0 to LLastColumnIndex do
    begin
      LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

      if (not LColumn.Derived) or Assigned(LColumn.OnDeriveColumn) then
        if LColumn.DataType = vttkString then
          FmtStr(FOutputStr, '%s%s%s%s%s', [FOutputStr, CSVFieldSeparator,
            CSVFieldDelimiter, FSourceTree.GetTextFromObject(LObject, I),
              CSVFieldDelimiter])
        else
          FmtStr(FOutputStr, '%s%s%s', [FOutputStr, CSVFieldSeparator,
            FSourceTree.GetTextFromObject(LObject, I)]);

    end;

    if LLastColumnIndex >= 0 then
      Delete(FOutputStr, 1, Length(CSVFieldSeparator));

    WriteToOutput(FOutputStr);
    lNode := FSourceTree.VT.GetNext(lNode);
  end;
end;

procedure TtiVTExportCSV.WriteHeader;
var
  I: integer;
  LColumn: TtiVTColumn;
  LLastColumnIndex: integer;

begin
  SetLength(FOutputStr, 0);
  LLastColumnIndex := FSourceTree.VT.Header.Columns.Count - 1;

  for I := 0 to LLastColumnIndex do
  begin
    LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

    if (not LColumn.Derived) or Assigned(LColumn.OnDeriveColumn) then
        FmtStr(FOutputStr, '%s%s%s%s%s', [FOutputStr, CSVFieldSeparator,
          CSVFieldDelimiter, LColumn.Text, CSVFieldDelimiter])

  end;

  if LLastColumnIndex >= 0 then
    Delete(FOutputStr, 1, Length(CSVFieldSeparator));

  WriteToOutput(FOutputStr);
end;

{ TTiVTExportClipboard }

procedure TTiVTExportClipboard.CloseOutput;
begin
  Clipboard.AsText := FStream.AsString;
end;

class function TTiVTExportClipboard.FormatName: string;
begin
  Result := ClipboardDesc;
end;

function TTiVTExportClipboard.GetFileName: Boolean;
begin
  Result := true;
end;

class function TTiVTExportClipboard.ImageName: string;
begin
  Result := cResTI_CopyToClipboard;
end;

initialization
  tiVTExportRegistry.RegisterExport(TtiVTExportCSV);
  tiVTExportRegistry.RegisterExport(TTiVTExportClipboard);
end.

