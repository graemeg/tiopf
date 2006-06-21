program tiGroupMailArchive2HTML;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiCommandLineParams,
  Indice_BOM in 'Indice_BOM.pas',
  xml_util in 'xml_util.pas',
  atConsts in 'atConsts.pas',
  Exp2xml in 'Exp2xml.pas',
  IteradorAbs in 'IteradorAbs.pas',
  ListaVariables in 'ListaVariables.pas',
  It_Orden in 'It_Orden.pas',
  It_Secuencia in 'It_Secuencia.pas',
  Correo_BOM in 'Correo_BOM.pas',
  Exp2xml_Index in 'Exp2xml_Index.pas',
  tiPerObjOIDGUID in '..\..\..\TechInsite\tiPersist\tiPerObjOIDGUID.pas';

var
  lSource: string;
  lTarget: string;
begin
  lSource := ExpandFileName(gCommandLineParams.GetParam('source'));
  lTarget := ExpandFileName(gCommandLineParams.GetParam('target'));
  WriteLn('Source: ' + lSource);
  WriteLn('Target: ' + lTarget);
  WriteLn;

  try
    TtiGroupMailArchive2HTML.I.ProcesarArchivos( lSource, lTarget );
  finally
    TtiGroupMailArchive2HTML.I( True );
  end;

  if gCommandLineParams.IsParam('i') then
  begin
    WriteLn;
    WriteLn('Press <Enter> to continue');
    ReadLn;
  end;
end.

