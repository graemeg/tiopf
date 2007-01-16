{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit tiOPFHelpIntegration; 

interface

uses
  PkgHelpIntegration, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('PkgHelpIntegration', @PkgHelpIntegration.Register); 
end; 

initialization
  RegisterPackage('tiOPFHelpIntegration', @Register); 
end.
