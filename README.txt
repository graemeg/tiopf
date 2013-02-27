
Welcome to the tiOPF project
============================

tiOPF is a Object Persistent  Framework written in Object Pascal,
for  use  with Delphi  and  Free  Pascal (FPC)  compilers.  tiOPF
simplifies the mapping of an  object oriented business model into
a  relational  database.  Persistence layers  are  available  for
Firebird,  Oracle,  MS  SQL Server,  MySQL,  PostgreSQL,  SQLite,
NexusDB, XML, CSV, TAB, Remote (via  HTTP) and many more. It also
allows you to use your  choice of database connection components,
like IBX,  dbExpress, DOA, SqlDB,  FBLib etc. When using  FPC you
can also target 32-bit or 64-bit platforms.

We pride ourselves  on the stability of the framework,  and it is
backed by  1600+ unit tests that  run multiple times per  day, on
our various build servers. tiOPF  is currently tested on Windows,
Linux and FreeBSD.

We also give you the freedom to choose how you want to build your
graphical desktop  applications. tiOPF has custom  components for
VCL  and  LCL. It also  has a Model-GUI-Mediator (MGM — something
like Model-View-Controller)  implementation that  allows standard
GUI  components  to  become  “object aware” without  creating yet
more descendant components. With MGM  we support the VCL, LCL and
fpGUI toolkits.


Where is the source code?
-------------------------
Recently the  tiOPF repository  was converted from  SubVersion to
Git. The old SubVersion  repository contained everything plus the
kitchen  sink  —  all  in  one  massive  repository.   A  bit  of
a  mess  really. While  doing  the  conversion  to Git,  we  took
the  opportunity  to  restructure  the repository  a  bit  better
by  using branches  and sub-repositories  (smaller more  specific
repositories, like the one now holding our website source).

For tiOPF2 code, simply switch to the 'tiopf2' branch:

  git checkout tiopf2

If you want the tiOPF3 code, then switch to the 'tiopf3' branch:

  git checkout tiopf3


Is tiOPF2 and tiOPF3 both still maintained?
-------------------------------------------
Yes.  If  you  use  Free Pascal  or Delphi 7–2007, then  you must
use  tiOPF2.  If you use Delphi 2009 or newer,  then you must use
tiOPF3.


Why does FPC not work in tiOPF3?
--------------------------------
tiOPF3  was primarily  started to  clean up  some code,  and take
advantage of newer language  features and Unicode string support.
As soon as  these language features have stabilised  in FPC, then
we will add FPC support to tiOPF3 as well.

No  worries  though,  tiOPF2  and  tiOPF3  are  near identical in
features.  Any  new changes since recent weeks are synced between
both  versions.  I  am  also working though the commit history to
merge  any other fixes or improvements that has been committed in
either  branch. This will be a ongoing process until FPC supports
tiOPF3.


               --------------------------------

