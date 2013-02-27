unit tiDUnitINI;

interface
uses
  INIFiles
  ,Registry
  ;

const
  cErrorCanNotSetINIFileToReadWrite = 'INI file <%s> is read-only and can not be set to read-write.' ;

function gDUnitINI : TINIFile ;
function gDUnitReg : TRegINIFile ;

implementation
uses
  tiUtils
  ,tiDialogs
  ,SysUtils
  ;

var
  uINI : TINIFile ;
  uReg : TRegINIFile ;

function gDUnitINI : TINIFile ;
var
  lFileName : string ;
begin
  if uINI = nil then
  begin
    lFileName := tiGetEXEPath + '\DUnitTIOPF.INI' ;
    if tiIsFileReadOnly(lFileName) then
      tiSetFileReadOnly(lFileName,false);
    if tiIsFileReadOnly(lFileName) then
      tiAppError(Format(cErrorCanNotSetINIFileToReadWrite,
                 [lFileName]));
    uINI := TINIFile.Create( lFileName ) ;
  end ;
  result := uINI;
end;

function gDUnitReg : TRegINIFile ;
begin                      
  if uReg = nil then
    uReg := TRegINIFile.Create( 'DUnitTIOPF' ) ;
  result := uReg;
end ;

initialization

finalization
  uINI.Free ;
  uReg.Free ;
end.
