{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit tiOPFGUIDsgn; 

interface

uses
  tiOPFControlsReg, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('tiOPFControlsReg', @tiOPFControlsReg.Register); 
end; 

initialization
  RegisterPackage('tiOPFGUIDsgn', @Register); 
end.
