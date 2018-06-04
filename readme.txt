Code Shell Controls Readme
--------------------------

Current Version: v0.9.0.1
 
Contents
--------
 
1) File and Directory Listing
2) Install Guide
3) Quick Start Guide
5) Recent Changes
4) Support
 
File and Directory Listing
--------------------------
 
Files in this package:
 
readme.txt      This file
manual.htm      The shell controls programmer's guide
snapshop.jpg    An screen capture of the controls from the demo program
 
Directories in this package:
 
demo            The source to the shell controls demo program
images          Images for the manual
packages        Delphi runtime and design time packages
units           The source files for the shell controls
 
Install Guide
-------------
 
1. Copy the zip contents to a development computer
2. Add the path to the units folder to the Delphi environment path
3. Open the runtime package for your version of Delphi (d12botshellrun.dpk)
4. Build the package and the close the project
5. Open the design time package for your version of Delphi (d12botshelldsgn.dpk)
4. Install the package and the close the project
 
You should now have a tab on the component palette named Codebot Shell containing
these components: TShellBubbles, TShellEdit, TShallPathEditBar, TShellTree,
TShellView, TSmallShellImages, TLargeShellImages, TShellBinding
 
Quick Start Guide
-----------------
 
1. Open Delphi and create a new VCL form project
2. Select the Codebot Controls tab from the component pallet and drop the
following components on your form: TShellPathEditBar, TShellTree, TShellView, and
TShellBinding.
3. Double click TShellBinding, select all the controls, and press OK
4. Press F9 to run the project
5. You've just built a functional version of Windows Explorer
 
Recent Changes
--------------
 
Below is a list of recent changes to the Shell Controls
 
Last Fixes

* Changed first shell path edit control to remove default button
* Fixed shell path edit to use the correct font color and styles
* Fixed shell path edit to use font color for drop down arrows
* Fixed shell tree and shell view to allow for custom popup menus

Prior Fixes

* Delphi 2009 compatibility
* Added shell binding control to simply development
* Added mouse wheel scrolling to the drop down menus
* Added bolded default items to the path edit bar drop down menus
* Added shell change notification to detect and reflect changes to the shell
* Changed control borders to use Vista hot, normal, and disabled visual states
* Changed drawing of the drop menu to better match vista rendering
* Changed drawing of the button areas to better match vista rendering
* Changed shell bubbles to not prevent dragging of bubble items
* Reduced shell path edit bar arrow sizes
* Removed references to units with little or no relevance to the shell controls
 
* Added a programming manual
 
Support
-------
 
Questions may be answered by reading the included manual.
 
More in depth programming information is available in this article
http://www.codebot.org/delphi/?doc=9418
 
A video walk through of using these controls is provided here.
http://www.codebot.org/delphi/preview
 
If questions are not answered in the manual or by using demo or watching the
above video, you may contact the author at sysrpl@gmail.com. Please include words
"Shell Controls Support" in the subject line.