rem * You will need to install the subversion command line client
rem * from http://subversion.tigris.org/
rem *
rem * If you are behind a proxy server, you will need a file
rem * called servers in the "Your private directory\Subversion\bin" directory containing
rem * the following text:
rem * 
rem * [groups]
rem * sourceforge = svn.sourceforge.net
rem * 
rem * [sourceforge]
rem * http-proxy-host = enter-value-here
rem * http-proxy-port = enter-value-here

set path_to_svn="C:\Program Files\Subversion\bin\svn" 
set svn_url_tiopf=https://svn.sourceforge.net/svnroot/tiopf/tiOPF3/Trunk
set check_out_to=C:\temp\tiOPF3

%path_to_svn% co %svn_url_tiopf% %check_out_to%

rem pause
