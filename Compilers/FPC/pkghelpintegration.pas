unit PkgHelpIntegration;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, HelpFPDoc;

procedure Register;

implementation

procedure Register;
begin
 // for Online help files
//  RegisterFPDocHTMLHelpForPackage('tiOPF3 Help','tiOPF3 Help Database',
//              'http://opensoft.homeip.net/tiopf/','tiOPF');

 // for local help files
 RegisterFPDocHTMLHelpForPackage('tiOPF3Core','tiOPF3 Core Help',
             'file://$PkgDir(tiOPF)/html/core','tiOPF','../../Core;../../Options;../../GUI');
 RegisterFPDocHTMLHelpForPackage('tiOPF3Options','tiOPF3 Options Help',
             'file://$PkgDir(tiOPF)/html/options','tiOPFGUI','../../Core;../../Options;../../GUI');
// RegisterFPDocHTMLHelpForPackage('tiOPF3Gui','tiOPF3 GUI Help',
//             'file://$PkgDir(tiOPF)/html/gui','tiOPFGUI','../../Core;../../Options;../../GUI');

 // also tried
{
 RegisterFPDocHTMLHelpForPackage('tiOPF3 Help','tiOPF3 Help Database',
             'file://$PkgDir(tiOPF)/html','tiOPF');
}
end;

end.
