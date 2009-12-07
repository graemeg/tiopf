
The following units are contributed by Carlo Marona. They show how one can 
use the Firebird DB's generators for sequential integer OIDs.

------------------[ copy ]----------------------
Date: Sun, 09 Nov 2008 16:35:05 +0100
From: Carlo Marona <c.marona@tiscali.it>
Newsgroups: tiopf.development

Hi,
I have some corrections, additions and suggestions.

[...snip...]

-In my application I add a tiOIDIntFBGen.pas unit that permit to use 
 Firebird generators as OID and you can use many OIDGen for different 
 tiObject descendant classes. It can be used with the OIDManager class 
 defined in tiOIDManager.pas.

To use the code last described, you need to make some changes to tiObject 
class. I like to see this changes added to the official code so we doesn't 
need to make changes every time a code update is made. I think this changes
doesn't break the existsing code.

In tiObject.pas, in the class TtiObject, I moved the FOID field from 
private to protected, added a OIDGenerator property declared as below:
    property OIDGenerator: TtiOIDGenerator read GetOIDGenerator;
and renamed the OIDGenerator function to GetOIDGenerator.


Cheers.
  Carlo Marona
------------------[ end ]----------------------


