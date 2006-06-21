A wizard for creating a unit with a TPerObjList descendent that owns a list of
TperObjAbs descendents. The list is owned by another TPerObjAbs descendent.

Revision history:

1.0: March 2002 - Original release

This wizard makes creating a list of items for the tiOPF easier.

To install the wizard, open tiOPFListItemWiz.dpk in Delphi and click the
Install button. Once you have it installed, the wizard will appear in 
the File->New->Other (or when you press the New button) selections 
in the tiOPF tab. 

Copy the template files to C:\TechInsite\Wizards - these files are header.txt and tiOPFListItem.txt.

To create a new unit with a List-Item definition, do the following:

Press the New toolbutton or choose the menu File->New->Other
Go to the TechInsite tab and double click the tiOPF List-Item Wizard
Then follow the instructions in the wizard. 
You need to specify:
- the name of the List Owner class (TPerObjAbs descendent)
- the name of the List class (TPerObjList descendent)
- the name of the Item class (TPerObjAbs descendent)
For any of the above options, you may also specify that the class is defined 
in another unit, and this definition will be used instead.
- any properties of the Item class. The default values for your properties are set in Wizards.INI (The default settings are not case sensitive)
Then press Finish, and your classes are created in a new unit file.

How it works:

The tiOPF List-Item Wizard works on templates. 

Templates are simply text files, being your code, with place holders 
for standard text replacements and conditional blocks for conditional 
code.

The first template is header.txt, which is normally your licence, 
file description, or other standard header that you include when you 
create a new unit. The second template is tiOPFListItem.txt being the 
source code of the unit, with place holders in the places where you want 
your new names to go. 

Place holders are words surrounded by % signs, like so: %PlaceHolder%
Place holders are not case sensitive, and multiple place holders can be 
used per line.

The following place holders are defined:

Place Holder     | Purpose
-----------------+-----------------------------------------------------
ListOwner        | The name of the list owner. This is the class name
                 |   without the leading T.
ListOwnerCaption | The caption for the list owner, if defined in this 
                 |   unit. Blank otherwise.
ListOwnerUnit    | The unit the list owner class is defined in, if it
                 |   isn't defined in this unit. Blank otherwise.
List             | The name of the list. This is the class name
                 |   without the leading T.
ListUnit         | The unit the list class is defined in, if it
                 |   isn't defined in this unit. Blank otherwise.
Item             | The name of the item. This is the class name
                 |   without the leading T.
ItemUnit         | The unit the item class is defined in, if it
                 |   isn't defined in this unit. Blank otherwise.
Author           | The name of the person writing this unit. This is loaded
                 |   automatically from Wizards.ini
Date FORMAT      | The current Date, optionally followed by a format.
                 |   If the format is included, the date is inserted 
                 |   using that format, otherwise the short date format
                 |   for the current computer is used. For example,
                 |   %Date MMMM YYYY% inserts the full month name
                 |   followed by the four digit year.

Conditional code blocks are surrounded in {$IFDEF} {$ENDIF} pairs.
The {$IFDEF} and {$ENDIF} commands must start in the first column.
The {$IFDEF} starts the conditional block, and the block of code 
up to the corresponding {$ENDIF} is either included or excluded 
from the unit depending on whether the variable tested in the {$IFDEF} 
has a value. The variables are the place holders discussed above. 

For example, consider the following template:

//--------------------------------------------------------
interface

uses
  tiPtnVisPerObj
{$IFDEF %ListOwnerUnit%}
  , %ListOwnerUnit%
{$ENDIF}
  ;
//--------------------------------------------------------
  

If %ListOwnerUnit% has the value "SomeUnit", then the following code 
will be output:
//--------------------------------------------------------
interface

uses
  tiPtnVisPerObj
  , SomeUnit
  ;
//--------------------------------------------------------

Otherwise, if %ListOwnerUnit% is blank, then the following code is output:
//--------------------------------------------------------
interface

uses
  tiPtnVisPerObj
  ;
//--------------------------------------------------------


Conditional code blocks can be nested. In this case, the inner blocks 
are only output if their condition and all other outer conditional 
blocks are true. For example:

//--------------------------------------------------------
type
{$IFDEF %ListOwner%}
{$IFDEF %List%}
  T%List% = class;

{$ENDIF}
  T%ListOwner% = class(TPerObjAbs)
  private
    F%List%: T%List%;
  protected
    function    GetCaption: string; override;
  published
    property    %List%: T%List% read F%List%;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;
{$ENDIF}
//--------------------------------------------------------

The line "T%List% = class;" will only be output if both 
%List% and %ListOwner% have a value. 



Enjoy.

Chris Latta
Data Solutions Pty Ltd

This code is released under the Mozilla Public Licence for the tiOPF.
See source code comments for details.
