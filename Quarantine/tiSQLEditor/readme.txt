
   tiOPF SQL Editor
   ----------------

This is a SQL Editor that greatly helps with the creation of
hard coded SQL Visitors.  Execute a query using the F8 key. 
A new window will display the results, if any are returned. Use
the SQL menu option to generate code that will automatically
be inserted into the clipboard, ready for you to copy into your
source code.

Database connections are controlled with three parameters:
 -d <database>
 -u <username>
 -p <password>

Example:
  ./tiSQLEditor.exe -d '192.168.0.1|M2' -u 'sysdba' -p 'masterkey'


The default persistence layer is controlled with a compiler conditional 
define. The available options are shown in the tiOPFManager.pas file,
near the bottom of the unit.

Example:
  To enable the Firebird FBLib persistence layer pass the compiler
  directive LINK_FBL to the compiler.

