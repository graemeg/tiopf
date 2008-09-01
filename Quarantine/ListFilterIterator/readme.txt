Subject: tiObjectListFilterIterator
From: Lee Jenkins <Lee@nospam.com>
Date: Fri, 29 Aug 2008 22:06:37 -0400
Newsgroups: tiopf.binary



Made a couple of changes.

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


Its still a work in progress so any suggestions or critiques are most welcome.

-- 
Warm Regards,

Lee

--------------------[ end ]---------------------------
