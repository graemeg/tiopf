unit tiGUIUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpg_grid,
  tiDataBuffer_BOM;

  // Mouse cursor routines
//  function tiAutoWaitCursor: IUnknown;
//  function tiAutoCursor(ACursor: TCursor = crHourglass): IUnknown;

//  procedure tiDataSetToListView(pDataSet: TtiDataBuffer; pLV: TListView);
  procedure tiDataSetToGridColumns(pDataSet: TtiDataBuffer; pGrid: TfpgStringGrid);
  procedure tiDataSetToGridContents(pDataSet: TtiDataBuffer; pGrid: TfpgStringGrid);
  

implementation

uses
  Math,
  fpg_main;

procedure tiDataSetToGridColumns(pDataSet: TtiDataBuffer; pGrid: TfpgStringGrid);
var
  i: integer;
begin
  Assert(pGrid <> nil, 'StringGrid not assigned');
  pGrid.Clear;

  if pDataSet = nil then
    Exit; //==>

  if pDataSet.Fields.Count > 0 then
    pGrid.ColumnCount := pDataSet.Fields.Count;
    
  for i := 0 to pDataSet.Fields.Count-1 do
  begin
    pGrid.Columns[i].Title := pDataSet.Fields.Items[i].Name;
    pGrid.Columns[i].Width := Trunc(fpgStyle.DefaultFont.TextWidth(pDataSet.Fields.Items[i].Name) * 1.2);
  end;

  pGrid.RowCount := pDataSet.Count;
end;

procedure tiDataSetToGridContents(pDataSet: TtiDataBuffer; pGrid: TfpgStringGrid);
var
  r,c: integer;
  lsValue: string;
begin
  writeln('pDataSet.Count = ', pDataSet.Count);
  writeln('pDataSet.Fields.Count = ', pDataSet.Fields.Count);

  for r := 0 to pDataSet.Count - 1 do
  begin
    for c := 0 to pDataSet.Fields.Count-1 do
    begin
      // Setup the Grid contents for columns 0..n
      lsValue := pDataSet.Items[r].Items[c].ValueAsString;
      pGrid.Cells[c, r] := lsValue;
      pGrid.Columns[c].Width := Max(pGrid.Columns[c].Width, Trunc(fpgStyle.DefaultFont.TextWidth(lsValue)*1.1));
    end;
  end;
end;

end.

