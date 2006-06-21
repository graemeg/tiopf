
I M P O R T A N T  
------------------

If you are using an earlier version of Lazarus 0.9.13 (Revision: 8965) dated 2006-03-20 you need to follow the instructions below.  If you have Revision 8965 and later, you can ignore the rest of this ReadMe file.

Before you can use this package, it must be copied to to the root of the 
Source directory. 2 directories back from this one!

eg:
  \Source\Compilers\fpc\tiOPF.*			<= original location
  \Source\tiOPF.*				<= where it should be copied to


The reason for this, is due to the fact that relative paths prior to where the
Lazarus Packages (*.lpk) lives, can not be used.  The tiOPF.lpk file will 
automatically add the compiled lib path and correct source paths to whatever
project/package depends on it.  This only works if the paths to be added are in
the same directory as the .lpk file or deeper.

Regards,
  - Graeme Geldenhuys -

                            ------------  oOo  ---------------

