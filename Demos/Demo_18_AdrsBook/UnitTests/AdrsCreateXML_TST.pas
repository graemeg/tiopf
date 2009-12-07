unit AdrsCreateXML_TST;

interface
uses
  tiTestFramework;

type

  TAdrsCreateXMLTestCase = class(TtiTestCase)
  published
    procedure CreateXMLDatabase;
  end;


procedure RegisterTests;

implementation
uses
  TestFramework,
  Adrs_CreateXML,
  SysUtils;

{ TTestAdrs }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TAdrsCreateXMLTestCase.Suite);
end;

{ TAdrsCreateXMLTestCase }

procedure TAdrsCreateXMLTestCase.CreateXMLDatabase;
begin
  TAdrsCreateXML.Execute('Adrs.XMLLight');
  Check(FileExists('Adrs.XMLLight'));
end;

end.
