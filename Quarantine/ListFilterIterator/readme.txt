

== ABSTRACT ==

Problem: How to filter TtiObjectLists that have already been populated from the 
backend data store?

Increasingly, I have had the need to filter TtiObjectLists that have already been 
populated.  These lists were already populated using either AutoMap or Hard Code
Visitors, but once fetched, I would often need to further filter the objects 
contained in the list.

The best solutions that I have come across was the Iterator Pattern:
http://en.wikipedia.org/wiki/Iterator_Pattern

What I liked most about this pattern was that it was agnostic of the composite 
structure that contained the objects that I wanted to get at.  Also, it seemed
perfect to support a Filtering process of the objects that it provided access to.

== SOLUTION ==

Enter, TtiObjectListFilterIterator.pas unit.  This unit contains notably: 

TtiListFilterIterator
This object is a base class for iterators used to iterate through a TtiObjectList.  It
is considered an "External Iterator" which infers that the client controls the iteration.

TtiIteratorManager
This object is exposed as a global, singleton object "gListIteratorMgr".  Most notably, 
TtiIteratorManager exposes the following methods: 

- RegisterIterator(AListClass: TtiListClass; AIteratorClass: TtiIteratorClass);

This method allows you to register a specific descendant TtiListFilterIterator with 
a specific TtiObjectList class.  The effect is that the specific iterator class that
you register is returned for the specific TtiObjectList that you register it for.


- GetIteratorI(const AFilter: string; AObjectList: TtiObjectList;
      const AStopFirstFail: Boolean = False): IListFilterIterator;

Notice the "I" suffix?  This method returns a reference counted generic 
TtiListFilterIterator which can be used and left to free itself.  The draw backs of 
the GetIteratorI is that the Interface returns a TtiObject in the "Current" function,
returning which would have to be cast to the specific object you're working with.

With GetIteratorI, the TtiIteratorManager checks to see if there is an iterator 
registered for the specific TtiObjectList passed in and uses it, if found.  If not,
it returns the base TtiListFilterIterator class.

- GetIterator(const AFilter: string; AObjectList: TtiObjectList;
      const AStopFirstFail: Boolean = False): TtiListFilterIterator;

This version is essentially the same as GetIteratorI with the following exceptions:

1. If a registered iterator is not found for the TtiObjectList passed in, an Assert
exception is raised.  Therefor you should create a descendant TtiListFilterIterator object
and override its .Current function to return the type of TtiObject the list will contain.

2. It returns a concreate object (instead of an Interface) which must be freed by the client.

== FILTERING ==

I've decided to use simple filtering which supports the following operators at this time:

=, <>, >, >=, <, <=, and "LIKE".  There are no single quotes required for string fields.  

Examples are: 

var 
  lFilter: string;
beign

  // Notice no single quotes required around "Johnson".'
  lFilter := 'LastName = Johnson'; 

  // Multiple filters are comma separated
  lFilter := 'LastName LIKE Jo*, Age > 18'; 
  
end;


I'd like to include support for AND'ing and OR'ing statements at some point as well as support
for IN(List|of|Possible|Choices|separated|By|Pipes|) and 
BETWEEN(ThisValue|ThatValue) at some point in the future.

--------------------------------------------------------------------------------------


==> 2008/9/1

+ Added Protected Method, DoTestFilterFields().

Tests the fields in the Filter expression to ensure they are published members of the
object contained in the ObjectList to be filtered before applying the filter.
  
+ Finished Support for AStopOnFirstFail parameter of TtiListFilterIterator constructor.

This is intended for lists that are sorted by properties
to be filtered by, usually a single field.  For instance,
if AListToCopy has objects with property "FirstName" and the
list is sorted by "FirstName" and AStopOnFirstFail is true,
then the iterator's filter method will wait until it finds the first
match and then set a flag.  When the first non-match is encountered
after the first match is found, searching stops since the list is
assumed to be sorted by that property and no other matches are expected to be found.


==> 2008/8/29

+ Set AutoSetItemOwner to false in TtiListFilterIterator constructor.

I learn something new about tiOPF everyday.  I wasn't aware of it but when 
assigning a TtiObject from one list to another, its owner gets set to the 
list the object was copied to if AutoSetItemOwner is not set to false.  
Can cause some funky side effects for tiObjectList's that are long lived.

+ Added TtiIteratorManager.

Added a global singleton manager object to register TtiObjectLists with a 
particular TtiListFilterIterator descendant to avoid having to cast the 
.Current property of the iterator when its used.  Instead, one cast is 
necessary when retrieving the iterator from the manager.

For example in a BOM unit:

interface

{: Iterator to register. }
  TPersonListIterator = class(TtiListFilterIterator)
  public
    function    Current: TPerson; reintroduce;
  end;

implementation

{ TPersonListIterator }

function TPersonListIterator.Current: TPerson;
begin
  result := TPerson(inherited Current);
end;

initialization;
  gListIteratorMgr.RegisterIterator(TPersonList,
    TPersonListIterator); // <-- Descendant Iterator


then you can do this:

var
  lIterator: TPersonListIterator;
  lFilter: string;
begin

  lFilter := 'LastName LIKE John*';

  lIterator :=
    TPersonListIterator(gListIteratorMgr.GetIterator(lFilter, FPersonList));

  try
    while lIterator.Next do
      begin
        memOutput.Lines.Add(lIterator.Current.FirstName + ' ' +
          lIterator.Current.LastName);
      end;
  finally
    lIterator.free;
  end;

--------------------[ end ]---------------------------
