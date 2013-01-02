Patching ADOM in Delphi XE
==========================

Delphi XE natively includes the Open XML Utilities library and a runtime version of ADOM 4.3. Completely replacing this units and packages is tricky and error-prone. Thus I recommend one of the following methods to use the ADOM update side by side with the one that is already installed in Delphi XE. 

First you need to install the latest version of the Open XML Utilities library in your Delphi system. You may obtain it from <http://www.philo.de/xml/>. This update of the Utilities library uses a "dk" prefix for its unit names to avoid name clashes with the version that comes with Delphi. If you directly use the Utilities library in your applications you should update your uses clauses accordingly.

Option A: Place the AdomCore_4_3.pas file directly in the folder of your project and add it explicitly to your project (use the "Project/Add to project ..." menu entry). Note that this way ADOM is still only available at runtime for you.

Option B: Use the package included in this folder. This installs both a designtime and a runtime package of ADOM 4.3 parallel to the old runtime version that comes with Delphi. The difference is that the update uses the unit name "dkAdomCore_4_3" instead of "AdomCore_4_3". Thus to use the update you need to change all uses clauses in your applications that read "AdomCore_4_3" into "dkAdomCore_4_3". Note that you need to repeat this for all add-ons or third-party software as well.

In both cases it is recommended to change the uses clauses for the Utilities library update as well. The unit names of the update all have a "dk" prefix now.