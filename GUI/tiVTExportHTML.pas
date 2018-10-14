unit tiVTExportHTML;

interface

{$IFDEF VIRTUAL_TREEVIEW}

uses
  tiVTListView
  ;

type
  TtiVTExportHTML = class(TtiVTExportFile)
  private
    function FormatFieldHeader(const AFieldHeader: string): string;
    function FormatField(const AField: string): string;
    function FormatRow(const ARow: string): string;
  protected
    function GetFileName: Boolean; override;
    procedure WriteHeader; override;
    procedure WriteTitles; override;
    procedure WriteDetail; override;
    procedure WriteFooter; override;
  public
    class function FormatName: string; override;
    class function ImageName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

uses
  SysUtils
  ,VirtualTrees
  ,tiVirtualTreesNEW
  ,tiResources
  ,tiObject
  ;

resourcestring
  HTMLDesc = 'HTML';

  { TtiVTExportHTML }

function TtiVTExportHTML.GetFileName: Boolean;
begin
  FDefaultExt := 'HTM';
  Result := inherited GetFileName;
end;

class function TtiVTExportHTML.ImageName: string;
begin
  Result := cResTI_ExportToHTML;
end;

function TtiVTExportHTML.FormatFieldHeader(const AFieldHeader: string): string;
begin
  FmtStr(Result,'<TH>%s</TH>', [AFieldHeader]);
end;

class function TtiVTExportHTML.FormatName: string;
begin
  Result := HTMLDesc;
end;

function TtiVTExportHTML.FormatField(const AField: string): string;
begin
  FmtStr(Result,'<TD>%s</TD>', [AField]);
end;

function TtiVTExportHTML.FormatRow(const ARow: string): string;
begin
  FmtStr(Result,'<TR>%s</TR>', [ARow]);
end;


procedure TtiVTExportHTML.WriteDetail;
var
  lNode: PVirtualNode;
  I: integer;
  LLastColumnIndex: integer;
  LColumn: TtiVTColumn;
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
        FOutputStr := FOutputStr + FormatField(
          FSourceTree.GetTextFromObject(LObject, I));

    end;

    WriteToOutput(FormatRow(FOutputStr));
    lNode := FSourceTree.VT.GetNext(lNode);
  end;
end;

procedure TtiVTExportHTML.WriteFooter;
begin
  WriteToOutput('</TABLE>');
  WriteToOutput('</BODY>');
  WriteToOutput('</HTML>');
end;

procedure TtiVTExportHTML.WriteHeader;
begin
  WriteToOutput('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">');
  WriteToOutput('<html>');
  WriteToOutput('  <head>');
  WriteToOutput('    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">');
  WriteToOutput('  </head>');
  WriteToOutput('<body>');
end;

procedure TtiVTExportHTML.WriteTitles;
var
  I, LColumnCount, LHeaderRow: integer;
  LLastColumnIndex: integer;
  LColumn: TtiVTColumn;

begin
  LLastColumnIndex := FSourceTree.VT.Header.Columns.Count - 1;
//  WriteToOutput('<table>');
  WriteToOutput('<table BORDER=1 ALIGN="CENTER">');
  LColumnCount := 0;

  for I := 0 to LLastColumnIndex do
  begin
    LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

    if (not LColumn.Derived) or Assigned(LColumn.OnDeriveColumn) then
      Inc(LColumnCount);

  end;

  WriteToOutput('<COL ALIGN="LEFT" SPAN=' +
    IntToStr(LColumnCount) + '>');

  for LHeaderRow := 0 to (FSourceTree.VT.Header as TtiVSTHeader).RowCount - 1 do
  begin
    SetLength(FOutputStr, 0);

    for I := 0 to LLastColumnIndex do
    begin
      LColumn := FSourceTree.VT.Header.Columns[I] as TtiVTColumn;

      if (not LColumn.Derived) or Assigned(LColumn.OnDeriveColumn) then
        FOutputStr := FOutputStr + FormatFieldHeader(LColumn.DisplayNames[LHeaderRow]);

    end;

    WriteToOutput(FormatRow(FOutputStr));
  //  WriteToOutput(FormatRow('<B>' + FOutputStr + '</B>'));
  end;
end;

initialization
  tiVTExportRegistry.RegisterExport(TtiVTExportHTML);

{$ENDIF}

end.

