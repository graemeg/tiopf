unit Adrs_Dependencies;

{$I tiDefines.inc}

interface
uses
   tiOIDGUID
  ,Adrs_BOM
//  ,Adrs_SrvAutoGenSQL  // For auto generated SQL
  ,tiOPFManager
;

procedure ConnectToDatabase;

implementation

procedure ConnectToDatabase;
begin
  gTIOPFManager.ConnectDatabase('..\Data\AdrsBookSimple.fdb', 'sysdba', 'masterkey');
end;

end.
