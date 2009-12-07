
This readme file will contain any FAQ's from the newsgroups.


Question:
----------
> How do I use tiOPF with Lazarus?

See the following wiki page:  http://wiki.freepascal.org/tiOPF


Question:
----------
> Which of the two fpcUnitTIOPFGui files with extents .lpr .lpi noramlly 
> load the project? Either seems to work.

If you go 'Project | Open Project', you normally open the .lpi file. lpi 
= Lazarus Project Information.  It's similar to the Delphi .bproj file I 
believe.  The .lpr is equivalent to the .dpr file, which is the start 
'program' unit of you project.

Like you mentioned, Lazarus would auto detect that you opened a 
'program' unit and automatically load the .lpi instead in it exists.


Question:
----------
> When started I see an error message
> "The following package failed to load:
> 
> tiOPFGUI(>=2.0)

Lazarus projects resolves dependencies via Lazarus Packages. No need to 
ever edit any project unit paths or include paths like Delphi. Simply 
select 'Package | Open package file (*.lpk)' and open the tiOPF.lpk and 
tiOPFGUI.lpk files located in the Compilers/FPC/ directory. Once you 
opened each package simply click the 'Compile' button in that window (no 
need to click install, as they are runtime packages).

Once you've open a Lazarus Packages, it's automatically register with 
Lazarus, so Lazarus would know where to find it the next time.

Oh, I think you would need to compile the FPCUnitTestRunner.lpk package 
as well. Located in <lazarus>/components/fpcunit/ directory. This 
packages contains the GUI Test Runner.

Now try and recompile the fpcUnitTIOPFGui project again.


Question:
----------
> Why does the tiOPFGUIDsgn package not work, or the tiOPF GUI 
> components give errors?

The tiOPF custom GUI components, based on the Lazarus LCL framework
is currently not maintained by anybody. I don't use the LCL anymore and
there was simply to many differences between the LCL and VCL at the
time.  Instead I now use the fpGUI Toolkit and the Model-GUI-Mediator
pattern to make any standard GUI components 'object-aware'. No need
to create descendant (custom) components anymore.



   -------------.oO0Oo.__ The End __.oO0Oo.---------------



