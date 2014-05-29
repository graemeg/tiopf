Changes submitted 28-May-2014 by Ian Krigsman
---------------------------------------------

Changes below originally appeared in the DS branch (on svn at least, if not GIT)
which evolved over time. The originating revision was r666 in tiOPF2. 
Local changes were then carried forward into tiOPF3. The nature of some changes 
made the prospect of a merge back to the trunk inconvenient.

Below is a list of relevant changes as of 02-Mar-2012 but not committed to the 
trunk until May 2014.

Update notes and discussion with Peter H left as is.

Side note: Some unused (by me) tiQueryXXXX units (cf #19-20 below) had their 
methods reordered to assist with a source comparison based on tiQueryIBX.
Thus, they will appear completely mashed when diff'ed against their own history.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

UPDATED: 26-Oct-2012 after clean compile in Delphi XE3 with (partial success of DUnit tests)

2012/11/05 PH
2012/11/05 IK

UPDATED: 28-May-2014 after upgrading to Delphi XE6.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1) Core\tiAutoMap.pas

--------
procedure TVisAutoUpdate.DoExecuteQuery;
begin
  if FParams.Count > 0 then <<<< ADDED
    Query.UpdateRow(FWhereAttrColMaps.TableName, FParams, FWhere);
end;
--------

Ie. TtiQuerySQL.UpdateRow prepared an invalid UPDATE statement if there were no parameters.
On reflection, the problem should probably be addressed there. It's feasible, although
potentially dangerous, to have a parameterless update.

PH: This exact problem tripped me up last week. I was calling a TVisAutoUpdate 
    descendant with no parameters. This was a programmer problem (and hence the
    code was bad) and should not have been checked in. In the case you describe
    above, I think I would rather:
    
      Assert(FParams.Count > 0, 'No parameters assigned in update query');
      Query.UpdateRow(FWhereAttrColMaps.TableName, FParams, FWhere);
    
    I don't think we should swallow bad code and keep executing. I think we 
    should fix the bad code.
	
	IK: I need to remind myself of the original problem again but I'm quite sure
	    it should not raise an exception.
	    IIRC it was OK *not* to have params - just don't call the update.
		If Query.UpdateRow was called with no params, it would generate an error
		(incorrect SQL syntax from memory). Thus my suspicion that .UpdateRow()
		should perhaps handle it(?).
		I'll see if I can refresh my memory after inspecting some code and report back.
			    
	IK: IIRC, registering mappings with two classes:
			TMyClass = class(TtiObject)
			// params
			TMySubClass = class(TMyClass)
			// No additional params
			
			==> Causes error. An update with only an OID would attempt to occur twice - or something like that.
			Can't recall exactly but sure it is valid to arrive in DoExecuteQuery with no params

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
2) Core\tiObject.pas

a) Added TtiObject.PropToCSV()

I think this was previously available in tiOPF2.
 
  PH: Looks like this code is cloned in tiListToStream.
      How about if we chain a call into tiListToStream() then use 
      tiStreamToString() to get the result set. 
	  
	  IK: That'll be better.
	  
          UPDATE: Committed as is.

b) Added optional param AColsHeader to tiListToStream and tiListToCSV.

AColsHeader allows an alternative header line to be written.
ie. different column names to the corresponding property names

  PH: Yes, good idea.

  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
3) Core\tiOPFManager.pas

Added TtiOPFManager.Execute() as a wrapper for FVisitorManager.Execute()

  PH: OK.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
4) Core\tiQuery.pas

Added properties RowCount and RowsAffected with abstract getters.
NB: This will produce "Constructing instance of TtiQueryXXXX containing abstract method..." warnings.
    Depending on your POV, this is either a good or bad thing.
    To me the msgs serve as a to-do reminder, but empty virtual methods can be created if required.

  PH: OK, so we have a problem here. What if the persistence layer we are 
      working with does not support RowCount or RowsAffected?
      
      a) Rough out the method but put an Assert(false, 'Not implemented')?
      b) Return zero?
      c) Leave with a compiler warning? or
      d) Come up with another method for getting special data out of a query
         that may not be available in all persistence layers.
         
      (We kind-of have this problem with SQL and Non SQL persistence layers in
       multi threaded support is not possible with most of the non SQL layers)
	   
	  IK: TtiQuery has a boolean property SupportsRowsAffected. This defaults to false
	      but overridden in TtiQueryIBX.Create() to True.
          Inside tiVisitorDB.TtiVisitorUpdate.Execute(const AData: TtiVisited)
		  there is a line:
          if FQuery.SupportsRowsAffected then AfterExecSQL(lRowsAffected);
		  
		  Does that help address the questions?
		  
		  A generic method to extract special data out of a query would be nice,
		  but I would have thought that the no. of rows affected by an update,
		  or as a minimum, 'were any rows affected?' warrants a TtiQuery property/method.
		  
		  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
5) Core\tiVisitor.pas

a) Made Iterate() virtual.

  PH: I remember this was a breaking change for much of the Iris code. Right?
      I don't recall the exact reason for doing this - but was pretty sure it
      should stay as non-virtual when I made the change. Here is the comment in 
      the code:
      
    {** Iterate will cause an instance of TtiVisitor to be passed over all
        objects that are accessible  by RTTI as published.
        Note: Do not override Iterate to change the behaviour of your object.
        Override IterateAssignTouched Instead. Iterate is not called by the
        VisitorManager so you overriden method will not execute. Alternatively,
        you can set the IterationStyle property on the Visitor to change
        iteration behaviour.
        @param AVisitor: An instance of the TtiVisitor to be passed over the
        object graph.}  
        
      So, while making Iterate() virtual will get the older code to compile,
      it may not execute as expected.
      
      Take care.
	  
	  IK: You remember correctly. I didn't realize it was delicate issue.
	      If/when IRIS is next updated, I may need to hire your services
		  to help review it.
	  

b) Cosmetic: Added more info to ProcessVisitors() logging.
 
  PH: OK.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
6) GUI\VCL\tiListMediators.pas

Unit change for VirtualTrees. See below

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
7) GUI\tiListView.pas


a) Surfaced published property: TabOrder;

  PH: OK

b) Wrap ConnectToData in a Begin/EndUpdate block

  PH: OK

c) TtiCustomListView.GetColWidth() was causing an AV.
Changed to answer arbitrary fixed value (50).
Temporary hack. Can't offer a better solution, but have not
encountered any side effects.

  PH: Probably some kind of "Parent Not Assigned" or "Owner not assigned"
      problem. How about if we test for Parent / Owner (assuming this is the 
      problem) and only returning the hard wired value if we know TextWidth()
      will fail.
	  
	  IK: Quite possibly. Prefer your suggestion as long as if Assigned(owner-thingy)
	      indeed returns false if not valid. thing.Free; ==> FreeAndNil(thing); anyone?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
8) GUI\tiListViewCtrls.pas

Surfaced published property: TabOrder;
  
  PH: OK

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
9) GUI\tiLogToGUI.pas

Removed:
--------
initialization
  if gCommandLineParams.IsParam(csLogVisual) then
    GLog.RegisterLog(TtiLogToGUI);
--------

For some reason registering the GUI log here was causing problems
when other logging features (eg. LogToFile) were being registered 
in the DPR.

The above is taken care of in tiLogReg.pas.

  PH: OK. Well spotted.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
10) GUI\tiPerAwareCtrls.pas
  
a) Property ShowError defaults to TRUE.

  PH: This will change the behaviour of lots of existing code (mine included).
      How about if ShowError defaults to False, but is set via an assignable
      constant along the lines of cost CDefaultShowError = false that can 
      be overridden at startup by your code.
	  
	  IK: Ummm It should break anyones code.
	      ShowError only comes into effect if Error also set.
		  ~~~~~~~~~~~~~~~~~~~~~~
		  procedure TtiPerAwareAbs.SetControlColor;
          begin
          // the control is in error
            if ShowError and Error then
		  ~~~~~~~~~~~~~~~~~~~~~~
          
		  Do you use the 'Error' property? 
		  If so, I'd like to be enlightened as to why ShowError is not best 
		  left as True.
		  If not, then it will have no affect.
		  
		  
b) Improved ReadOnly painting for ComboBox

  PH: OK. Tks.

c) Improved change event triggering

  PH: OK. Tks.

d) Added simple filtering to ComboBox using a user defined mask character or string.

  PH: OK. Reading your notes, you are not sure of the best approach to 
      implementing this requirement. How about if it's not published and only 
      ever assigned in code. This way, we can change the implementation (and 
      signature) as our ideas develop and the compiler will trap any issues
      (rather than the run-time supprised we would get with a published 
      property.)
	  
	  IK: Changing from published to public should not be too painful.
	      IIRC I tend to assign it in code anyway.
	      It appeared a bit of a hack when I implemented it but it works perfectly.
		  Eg. we can update obsolete code descriptions as '{obs} blah blah' which
		  will appear if historical data is retrieved from the database,
		  but the selection will be hidden for new data entry.
      
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
11) GUI\tiThreadProgress.pas

Htmlview is used (in WINDOWS, not FPC) to create the 'hot' mouse over behaviour on the 
label (that is clicked). Since I use my own version of Htmlview, the unit name was causing conflict.

Change: Added conditional define USEHTMLVIEW so this can be suppressed in Windows (as for FPC).

  PH: Yuck (but understandable)
  
      IK: Ditto.

And now the biggy...

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
12) GUI\tiVirtualTrees.pas

This is currently a modified clone of an old version of VirtualTrees.pas.

As discussed numerous times on the newsgroup, this:
a) Is old
b) Conflicts with users wanting a later version
c) Conflicts with other components, such as EasyListView and VirtualShellTools from MustangPeak.

The solution? Despite initial appearances, it is possible to refactor this into a genuine 'tiVT' version that inherits from the real thing.

Eg. Class declaration looks like this:

------------------------------------------------
  TtiVirtualStringTree = class(TVirtualStringTree)
  private
    FOnAdvancedPaintText: TVTAdvancedPaintText;  // triggered before either normal or fixed text is painted to allow
                                                 // even finer customization (kind of sub cell painting). Allows
                                                 // setting of color when node is disabled or unfocused.
    function IsDisabled(Node: PVirtualNode): boolean;
    function IsSelectedAndUnfocused(Node: PVirtualNode; Column: TColumnIndex): boolean;
  protected
    procedure PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer; Text: UnicodeString); override;
    procedure PaintStaticText(const PaintInfo: TVTPaintInfo; TextOutFlags: Integer; const Text: UnicodeString); override;
    procedure DoAdvancedPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType; NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean); virtual;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetHeaderClass: TVTHeaderClass; override;
  published
    property OnAdvancedPaintText: TVTAdvancedPaintText read FOnAdvancedPaintText write FOnAdvancedPaintText;
  end;
------------------------------------------------


This is achieved, thanks largely to the GetColumnClass and GetHeaderClass functions.
We can implement our own subclasses and add (most of) the behaviour from the original modifications.


tiVirtualTreesNEW.pas, with the deliberately obnoxious name, has the goods.

Other units have been modifled to refer to (the real) VirtualTrees.pas and tiVirtualTreesNEW in place of tiVirtualTrees.
cf tiListMediators.pas, tiVTAccessibilityFactory.pas, tiVTExportHTML.pas, tiVTListView.pas, tiVTTreeView.pas

NB tiVTAccessibilityFactory.pas looks like it is dependent on the current tiVirtualTrees.pas, so may be redundant.

Thus, *replacing* tiVirtualTrees.pas with this new version should provide a way forward.

Caveats:
i) Some minor modifications had to be made to the real VirtualTrees.pas in order for the subclassing to be complete.
eg. private properties to protected. All of these have been tagged with [tiOPF]. This implies a merging of the VT
code whenever it is updated. About 30 mins work.

ii) PaintNormalText and PaintStaticText had to be overridden completely. Any future modifications in the real 
VirtualTrees.pas has to be merged here.

iii) There might be one or two features in the tiVirtualTrees.pas version that could not be sufficently catered for.
(I can't remember specifics.)

The end result of this is (hopefully):
a) We place a copy of virtual-treeview from googlecode into 3rdParty.
b) Make the few required local modifications to this code
c) Ditch tiVirtualTrees.pas
d) Rename tiVirtualTreesNEW.pas to tiVirtualTrees.pas

  PH: I feel your pain... but do you feel mine. We have lots of critical 
      behaviour wrapped up in that dang tiVirtualTree with no resource 
      (developer or tester) to change. If we change and break (which I'm sure
      we will), it will get ugly.
      
      Here's my plan. All AEMO code gets changed to use AEMOVirtualTree.pas
      We rename units & classes and let the compiler catch the problems.
      
      When that's done, you can make the changes you propose.
	  
	  IK: Pain shared... and I can't offer a less painful alternative.
	      This probably applies to other 3rd party components as well, but with VT,
		  the author does update it quite regularly (with Delphi version and bug fixes)
		  so it would be a shame to not take advantage of that.
		  If you had a developer/tester you might be surprised how little effort
		  there was to update it. I'm using it across the board with CPMS and IRIS.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
13) GUI\tiVTListView.pas

a) VirtualTrees change as per 11)

b) Surfaced a few paint events

c) Minor mods. eg. Made OnHeaerClick available outside of HeaderClickSorting test

  PH: All good once the AEMO coded is isolated.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
14) Options\tiFactory.pas

Taken from tiOPF2 because it was missing in tiOPF3. Is this somewhere else and I can't see it? I notice some factory behaviour elsewhere but I had used the v2 version and needed the same thing here.

  PH: TPerObjFactory is in tiObject.pas (Still need the name tidy up we made tiOPF1 -> tiOPF2)
  
      IK: Will check out. Thanks.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
15) Options\tiQueryFBL.pas & tiQueryFIBP.pas

Refactor AssignParams()

  PH: Remove AssignParams() Right?
  
      IK: Yup. Similar to IBX.(Haven't done testing on those layers - only IBX)
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
16) Options\tiQueryIBO.pas

Refactor, which essentially was a reorder of the methods.
(In an attempt to bring consistency to all tiQueryXXXX.pas units.)
Yes, a Diff will look *really* messy.

  PH: OK.
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
17) Options\tiQueryIBX.pas

Implements RecordCount and RowsAffected.

NB Users of other tiQuery layers are encouraged to do the same.

  PH: See note above regarding how we should handle these special result values.
     This may do the trick in a more generic way:
     
       GetSpecialResultValueAsInteger(const AResultPropertyName: string): integer;
       GetSpecialResultValueAsString(const AResultPropertyName: string): string;
     Just an idea... 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
18) Options\tiQueryTXTAbs.pas

Modify TTXTToTIDataSetAbs.ReadRow to allow empty tokens.
NB This bug took me hours to find!!

  PH: That's why you get the big bucks.
  
      IK: :-) If only.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
19) Options\tiQueryUIBAbs.pas

Refactor, which essentially was a reorder of the methods.

  PH: OK. (Not my area of knowledge however)
  
      IK: Would have otherwise stayed away from these other Firebird layers, but
	      found myself exploring options to IBX for fear it had 'become' incompatible
		  with FB + Unicode. Fear unwarranted in the end, but not before
		  messing with the code after hours of beyond-compare-ing.
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
20) Options\tiQueryZeosXXXX.pas

Refactored Zeos family of units, but found out these have been replaced
by single tiQueryZeosIBFB.pas, so my version may not be correct.

  PH: OK. (Not my area of knowledge however)

 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
21) Mediators

Various tweaks and enhancements to the mediator classes. Eg. Cater for IOS (preliminary testing)

tiBaseMediator
tiMediators

NB Some new units created under subfolder of \GUI


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
22) Miscellaneous Changes

Examples:
- Changing {$IFNDEF FPC} to {$IFDEF WINDOWS} for platform compatibility
- Updated compiler constants for XE3-XE6
- Removed MSWINDOWS declaration in tiDefines.inc (it already is defined)
- Support for IOS. (Experimental - Initial tests working! - but with considerable chunks of code disabled)
- Refactored MIME logic from tiStreams to new unit tiMime
- tiUtils now has a {$I tiUtils_impl.inc} to separate large chunks of platform specific logic
  These are stored in subfolders of \Core. eg \Core\OSX with appropriate path added to project options.
  I'm not convinced this is the best way to do it.
- Updated Demo #19 to test FMX, IOS and OSX (with mediators)
- Demo_18_AdrsBook\BOM\Adrs_Dependencies.pas had URL with \\ instead of // (might have worked for IIS)
- Added FMX units under GUI\FMX
