unit DtiDefaultActionValues;

interface
uses
  SysUtils, Classes, ActnList

  ,tiPerAwareCtrls
  ;

type
  TtidmDefaultActionValues = class(TDataModule)
    ActionList1: TActionList;
  private
  public
  end;

var
  tidmDefaultActionValues: TtidmDefaultActionValues;

implementation

{$R *.dfm}

end.
