unit tiTypes;

{$i vtfDefines.inc}

interface

type
  TtiNotifyType = (
    // Observer/Subject
    ntAttached,     // Attach observer to subject
    ntDetached,     // Detach observer from subject

    // Object/Member State
    ntLoaded,       // loaded from persister
    ntDeleting,     // marked to Delete
    ntDeleted,      // already deleted from persister - to propagate to objectlist
    ntModified,     // member or object changed
    ntPersisted,    // saved to persister

    // ObjectList/Object/Member
    ntClear,        // list clear/set null all members/set member to null and clear value

    //ObjectList
    ntAdd,          // object added to List
    ntRemove,       // object removed from list
    ntBeginUpdate,  // start to update list in batch
    ntEndUpdate,    // end of update list in batch
    ntSorted,       // after sort
    ntUpdating      // updating list
  );

implementation

end.

