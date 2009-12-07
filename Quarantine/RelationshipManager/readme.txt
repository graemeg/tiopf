Date: Wed, 21 Mar 2007 10:51:04 +0100
From: Stephane Carre <stephane at optimaconsulting dot lu>
Newsgroups: tiopf.binary
Subject: Relationship Manager

Well, finally we found some time to play with the RM concepts in tiOPF. 
And since we promised that we would provide feedback with some example code,
here it is (attached). 

It is pretty much operational although it should still be considerd beta 
from the architectural point of view. We have used it sucessfully in this 
state in one real application only, so I am sure that there are usage models 
that have not been explored (and may be problematic). 

You will note that all RM handled objects descend from TBusinessObject, 
which provides access to the RM. An alternative is to make the RM globally 
accessible, but we prefer the first approach for its ease of use (never 
forget to register the RM again) and flexibility (especially when it comes 
to unit testing). 

The unit tests are written for DUnit, so unfortunately there will be some
 work to do if you want to run them in FPCUnit. 

Things we like with the RM: 

* Creating new BO classes at the DB and OPF visitor level is very flexible. 
You just need to take care of the non-object properties, the RM will do the 
rest when it comes to (any kind of) object relations. 

* Traversing complex object structures is really easy, no need to take care 
of back-pointers to parents etc. One-to-one, one-to-many and many-to-many 
relationships are supported in the same way. 

Things we do not like with (our current implementation of) the RM: 

* Association object visitors need to be aware of the lists owning the 
objects in the relationships. This forces these lists to be read into 
memory before the RM relationships are read. Potentially this means a lot of 
objects in memory! Additionally, these lists must typically be globally 
accessible (although our TListStrategy - see example code - is an attempt to 
mitigate this). 

* When the RM is used for multiple complex relation types, it can quickly 
become the main bottleneck in an application due to the great number of 
association objects. 

* Additionally, the RM handling of multiple relation types may be a problem 
if, for example, database access rights are attributed at user level: users 
that need read/write access to a certain type of relations will also be 
(unnecessarily) granted potential read/write access to all other types of 
relations held in the same database table. This could even become a blocking 
point if some relationships should not be read by some users! 

* Object properties relying on the RM must be called in 'with' statements or 
via temporary storage variables. 
For example, the following code will trigger two calls to the RM (and two 
searches through the relationship list): 

if Assigned(Order.Customer) then 
  Display(Order.Customer.Name); 

To avoid the double call we must use a temporary variable: 

tmpCustomer := Order.Customer; 
if Assigned(tmpCustomer) then 
  Display(tmpCustomer.Name); 

* All objectlist type properties rely on the fact that the containing list 
is a field of the parent object, non-owning and passed as a parameter to be 
'filled' upon request. We should improve this in such a way that, at least, 
typecasts are not required when accessing the returned elements. 

* We have not yet dealt with multi-user access to the back-end database, but 
this may be problematic in case the same transaction deals with both 
relationship additions and removals. 

Please don't hesitate to comment/amend the attached code! We will be happy 
to hear any suggestion to improve the usability, architecture or implementation! 

Stéphane 

-----------------------

Stéphane Carre wrote: 
> All objectlist type properties rely on the fact that the containing 
> list is a field of the parent object, non-owning and passed as a 
> parameter to be 'filled' upon request. We should improve this in such 
> a way that, at least, typecasts are not required when accessing the 
> returned elements. 

In attachment you will find a slightly improved version of the Relationship 
Manager that handles TtiObjectList (and descendants) as containers for 
collection properties, thus removing the need for typecasting when accessing 
the collection elements. 

I've also included a separate DDL file for the generation of the test 
DB structure (Firebird). 

Stéphane 

---------------[ end ]------------------
