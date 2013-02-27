Hi Peter,

Here is the state of play with the Wizards at the moment. As part of
implementing a TreeView Form Wizard I realized that I could refactor my
previous solution to be more generally applicable. I also had to package the
core functionality of the wizards into its own package for sharing across
the various wizards. This resulted in tiOPFWizardCore.dpk which has to be
installed before the "real" wizards.

I am also attempting to do some further implementation hiding, which will
make creating a wizard as simple as creating a tiOPFWizard descendent,
setting some properties in the Create constructor, implementing the abstract
Execute method, chucking a couple of bitmaps into a resource, writing a
template file and you're done. You can see my initial attempt at this
approach in the tiOPFWizard.pas file (to be part of tiOPFWizardCore.dpk) and
tiOPFListItemWiz.pas (which would replace tiOPFListItemWizardClass in
tiOPFListItemWizard.dpk). However, I'm getting some resource problem in my
implementation of this, so I have backed off from this approach for the
moment - I will hopefully be able to implement this properly when I return
from Europe.

tiOPFTreeViewFormWizard.dpk is not complete yet - I'm getting some problem
creating the form template.

In addition to the refactoring, I have added an image to the packages and
added an Action and Toolbutton so you can add this to your toolbars. This
works very nicely, until you exit Delphi - when you re-start, the toolbutton
no longer has an image or an action for some reason, although its
implemented exactly as the help says to do it.

I have tried to create the dpk5 versions of these packages (using that
wonderful IDE: Notepad), which will hopefully compile for you in D5.

So basically, only tiOPFListItemWizard works at the moment still, but the
code base has been fairly extensively overhauled. I am unlikely to find any
more time to work on it before I leave, but I wanted to make sure you had
the most up to date version before I disappeared so you can see the
direction I am heading with these wizards.

Even though it gone well in my tests, you might want to keep a backup of the
previous version of the wizard in case there are any dramas with this
version.

Cheers,
Chris Latta
