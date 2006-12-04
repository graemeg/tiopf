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
//  RegisterFPDocHTMLHelpForPackage('tiOPF2 Help','tiOPF2 Help Database',
//              'http://opensoft.homeip.net/tiopf/','tiOPF');

 // for local help files
 RegisterFPDocHTMLHelpForPackage('tiOPF2Core','tiOPF2 Core Help',
             'file://$PkgDir(tiOPF)/html/core','tiOPF','../../Core;../../Options;../../GUI');
 RegisterFPDocHTMLHelpForPackage('tiOPF2Options','tiOPF2 Options Help',
             'file://$PkgDir(tiOPF)/html/options','tiOPFGUI','../../Core;../../Options;../../GUI');
// RegisterFPDocHTMLHelpForPackage('tiOPF2GUI','tiOPF2 Help Database',
//             'file://$PkgDir(tiOPF)/html','tiOPF','../../Core;../../Options;../../GUI');

 // also tried
{
 RegisterFPDocHTMLHelpForPackage('tiOPF2 Help','tiOPF2 Help Database',
             'file://$PkgDir(tiOPF)/html','tiOPF');
}
end;

end.
