program DemoTIListView;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FtiListViewDemo in 'FtiListViewDemo.pas' {FormListViewDemo},
  FtiListViewPlusDemo in 'FtiListViewPlusDemo.pas' {FormListViewPlusDemo},
  DemoData in 'DemoData.pas',
  FEdit in 'FEdit.pas' {FormEdit},
  FtiListViewCtrlsDemo in 'FtiListViewCtrlsDemo.pas' {FormListViewCtrlsDemo},
  FtiListViewMultiSelect in 'FtiListViewMultiSelect.pas' {FormTIListViewMultiSelect},
  FtiListViewDif in 'FtiListViewDif.pas' {FormTIListViewDif};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
