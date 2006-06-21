program DeleteHistoryListsInDofFiles;
{$APPTYPE CONSOLE}
uses
  SysUtils
  ,Classes
  ,tiUtils
  ,tiDialogs
  ,INIFiles
  ;

var
  lFiles : TStringList ;
  i, j : integer ;
  lINI : TINIFile ;
  lSections : TStringList ;
  lFirstCall : boolean ;
begin
  lFiles := TStringList.Create;
  try
    tiFilesToStringList( ParamStr(1), '*.dof', lFiles, true );
    for i := 0 to lFiles.Count - 1 do
    begin
      lFirstCall := true ;
      lINI := TINIFile.Create(lFiles.Strings[i]);
      try
        lSections := TStringList.Create;
        try
          lINI.ReadSections(lSections);
          for j := 0 to lSections.Count - 1 do
            if Pos( 'HISTORYLISTS\', UpperCase(lSections.Strings[j])) = 1 then
            begin
              if lFirstCall then
              begin
                Writeln('Deleting history lists from ' + lFiles.Strings[i]);
                lFirstCall := false ;
              end ;
              WriteLn('  Erasing ' + lSections.Strings[j]);
              lINI.EraseSection(lSections.Strings[j]);
            end ;
          lINI.WriteInteger( 'Compiler', 'ShowHints', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ShowWarnings', 1 ) ;
          lINI.WriteString( 'Compiler', 'UnitAliases','WinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE;');
          //NamespacePrefix=
          lINI.WriteInteger( 'Compiler', 'SymbolDeprecated', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'SymbolLibrary', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'SymbolPlatform', 0 ) ;
          lINI.WriteInteger( 'Compiler', 'UnitLibrary', 0 ) ;
          lINI.WriteInteger( 'Compiler', 'UnitPlatform', 0 ) ;
          lINI.WriteInteger( 'Compiler', 'UnitDeprecated', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'HResultCompat', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'HidingMember', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'HiddenVirtual', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'Garbage', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'BoundsError', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ZeroNilCompat', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'StringConstTruncated', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ForLoopVarVarPar', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'TypedConstVarPar', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'AsgToTypedConst', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'CaseLabelRange', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ForVariable', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ConstructingAbstract', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ComparisonFalse', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ComparisonTrue', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ComparingSignedUnsigned', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'CombiningSignedUnsigned', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'UnsupportedConstruct', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'FileOpen', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'FileOpenUnitSrc', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'BadGlobalSymbol', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'DuplicateConstructorDestructor', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'InvalidDirective', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'PackageNoLink', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'PackageThreadVar', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ImplicitImport', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'HPPEMITIgnored', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'NoRetVal', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'UseBeforeDef', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ForLoopVarUndef', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'UnitNameMismatch', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'NoCFGFileFound', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'MessageDirective', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ImplicitVariants', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'UnicodeToLocale', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'LocaleToUnicode', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'ImagebaseMultiple', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'SuspiciousTypecast', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'PrivatePropAccessor', 1 ) ;
          lINI.WriteInteger( 'Compiler', 'UnsafeType', 0 ) ;
          lINI.WriteInteger( 'Compiler', 'UnsafeCode', 0 ) ;
          lINI.WriteInteger( 'Compiler', 'UnsafeCast', 0 ) ;
        finally
          lSections.Free;
        end;
      finally
        lINI.Free;
      end;
    end ;
  finally
    lFiles.Free;
  end ;
end.
