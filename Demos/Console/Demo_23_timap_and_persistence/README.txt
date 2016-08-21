
       DEMO 23 - Using timap and data persistence
       ==========================================

This demo is quite small, but it is packed full of features.

Demo 23 shows how to use the tiOPFMapper (timap.exe) tool from the
"tiopf_apps" code repository. The object mapping (properties to database
fields) are defined in a XML file in the "docs" directory. The timap tool
is used to generate the "src/book_autogen.pas" unit. This is a a fully
functional unit with business object and list object, including the
four hard-coded visitors for Create, Read, Update and Delete functionality.

The reason the XML defines "Base" objects, is because the application only
uses descendants (see book.pas unit) of those classes. This allows use to
tweak the XML schema file and regenerate the book_autogen.pas unit, without
us looking any local modifications in TBook and TBookList.

This demo also shows how multiple persistence layers can be used in a single
project, and only a change in Compiler Defines switches between those
persistence layers. No other code changes are needed.

Another feature this demo shows is how to create a database and table
if needed. This is again persistence layer independent.

NOTE:
  This demo uses some Free Pascal specific units - more specifically the
  image loading units to demo timap's TStream (Blob field) support. If
  you don't use Free Pascal, simply edit the schema file and remove the
  "Image" field and mapping information, then regenerate the autogen
  unit.

                 -------------[ end ]--------------
