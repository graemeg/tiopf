unit tiDataSet_Reg;

interface

uses tiDataset, Classes;

procedure Register;

implementation

{$r *.res}

procedure Register;
begin
  RegisterComponents('TechInsite Dataset',[TTiDataset, TTiNestedDataset, TTiRecordDataset, TTiNestedRecordDataset]);
end;

end.
