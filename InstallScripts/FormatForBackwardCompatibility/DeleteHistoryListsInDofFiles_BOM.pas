unit DeleteHistoryListsInDofFiles_BOM;

interface
uses
   tiBaseObject
  ,FormatForBackwardCompatibility_BOM
  ;

type

  TDeleteHistoryListsInDofFiles = class(TFormatForBackwardCompatibility)
  protected
    procedure ProcessOne(const AFileName: string); override;
    function  FileSpec: string; override;
  end;

implementation
uses
   tiUtils
  ,SysUtils
  ,Classes
  ,INIFiles
  ;

{ TDeleteHistoryListsInDofFiles }


function TDeleteHistoryListsInDofFiles.FileSpec: string;
begin
  result:= '*.dof';
end;

procedure TDeleteHistoryListsInDofFiles.ProcessOne(const AFileName: string);
var
  j : integer;
  lINI : TINIFile;
  lSections : TStringList;
begin
  lINI := TINIFile.Create(AFileName);
  try
    lSections := TStringList.Create;
    try
      lINI.ReadSections(lSections);
      for j := 0 to lSections.Count - 1 do
        if Pos('HISTORYLISTS\', UpperCase(lSections.Strings[j])) = 1 then
        begin
          WriteLn('    Erasing ' + lSections.Strings[j]);
          lINI.EraseSection(lSections.Strings[j]);
        end;

      lINI.EraseSection('Excluded Packages');
      lINI.WriteInteger('Compiler', 'ShowHints', 1);
      lINI.WriteInteger('Compiler', 'ShowWarnings', 1);
      lINI.WriteString( 'Compiler', 'UnitAliases','WinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE;');
      //NamespacePrefix=
      lINI.WriteInteger('Compiler', 'SymbolDeprecated', 1);
      lINI.WriteInteger('Compiler', 'SymbolLibrary', 1);
      lINI.WriteInteger('Compiler', 'SymbolPlatform', 0);
      lINI.WriteInteger('Compiler', 'UnitLibrary', 0);
      lINI.WriteInteger('Compiler', 'UnitPlatform', 0);
      lINI.WriteInteger('Compiler', 'UnitDeprecated', 1);
      lINI.WriteInteger('Compiler', 'HResultCompat', 1);
      lINI.WriteInteger('Compiler', 'HidingMember', 1);
      lINI.WriteInteger('Compiler', 'HiddenVirtual', 1);
      lINI.WriteInteger('Compiler', 'Garbage', 1);
      lINI.WriteInteger('Compiler', 'BoundsError', 1);
      lINI.WriteInteger('Compiler', 'ZeroNilCompat', 1);
      lINI.WriteInteger('Compiler', 'StringConstTruncated', 1);
      lINI.WriteInteger('Compiler', 'ForLoopVarVarPar', 1);
      lINI.WriteInteger('Compiler', 'TypedConstVarPar', 1);
      lINI.WriteInteger('Compiler', 'AsgToTypedConst', 1);
      lINI.WriteInteger('Compiler', 'CaseLabelRange', 1);
      lINI.WriteInteger('Compiler', 'ForVariable', 1);
      lINI.WriteInteger('Compiler', 'ConstructingAbstract', 1);
      lINI.WriteInteger('Compiler', 'ComparisonFalse', 1);
      lINI.WriteInteger('Compiler', 'ComparisonTrue', 1);
      lINI.WriteInteger('Compiler', 'ComparingSignedUnsigned', 1);
      lINI.WriteInteger('Compiler', 'CombiningSignedUnsigned', 1);
      lINI.WriteInteger('Compiler', 'UnsupportedConstruct', 1);
      lINI.WriteInteger('Compiler', 'FileOpen', 1);
      lINI.WriteInteger('Compiler', 'FileOpenUnitSrc', 1);
      lINI.WriteInteger('Compiler', 'BadGlobalSymbol', 1);
      lINI.WriteInteger('Compiler', 'DuplicateConstructorDestructor', 1);
      lINI.WriteInteger('Compiler', 'InvalidDirective', 1);
      lINI.WriteInteger('Compiler', 'PackageNoLink', 1);
      lINI.WriteInteger('Compiler', 'PackageThreadVar', 1);
      lINI.WriteInteger('Compiler', 'ImplicitImport', 1);
      lINI.WriteInteger('Compiler', 'HPPEMITIgnored', 1);
      lINI.WriteInteger('Compiler', 'NoRetVal', 1);
      lINI.WriteInteger('Compiler', 'UseBeforeDef', 1);
      lINI.WriteInteger('Compiler', 'ForLoopVarUndef', 1);
      lINI.WriteInteger('Compiler', 'UnitNameMismatch', 1);
      lINI.WriteInteger('Compiler', 'NoCFGFileFound', 1);
      lINI.WriteInteger('Compiler', 'MessageDirective', 1);
      lINI.WriteInteger('Compiler', 'ImplicitVariants', 1);
      lINI.WriteInteger('Compiler', 'UnicodeToLocale', 1);
      lINI.WriteInteger('Compiler', 'LocaleToUnicode', 1);
      lINI.WriteInteger('Compiler', 'ImagebaseMultiple', 1);
      lINI.WriteInteger('Compiler', 'SuspiciousTypecast', 1);
      lINI.WriteInteger('Compiler', 'PrivatePropAccessor', 1);
      lINI.WriteInteger('Compiler', 'UnsafeType', 0);
      lINI.WriteInteger('Compiler', 'UnsafeCode', 0);
      lINI.WriteInteger('Compiler', 'UnsafeCast', 0);

      lINI.WriteInteger('Version Info', 'IncludeVerInfo', 0);
      lINI.WriteInteger('Version Info', 'AutoIncBuild', 0);

      lINI.WriteString('Version Info Keys', 'FileVersion', '0.0.0.0');
      lINI.WriteString('Version Info Keys', 'ProductVersion', '');

    finally
      lSections.Free;
    end;
  finally
    lINI.Free;
  end;
end;

end.
