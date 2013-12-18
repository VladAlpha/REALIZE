REALIZE was written adn made available by Chris@timeguy.com on his website.  I mearly am creating a github repo of it to keep track of it, psosibly update and make it readily available for others.

FROM THE ORIGINAL PAGE AT timeguy.com/cradek/autocad

//***************************
//		REALIZE for AutoCAD
//***************************

I use AutoCAD to generate tool paths for 2.5D milling. This simple AutoLISP program named REALIZE generates G Code from lines, circles, polylines, and points in the drawing. The program supports tool changes; tools are represented by different layers in the drawing. Feeds are given using labels in the drawing. This seems awkward but a design goal was to require no editing of the generated file because all information needed to generate the output properly is stored in the drawing. REALIZE is distributed under the terms of the GNU General Public License (GPL).

This program originally generated G Code suited for MaxNC's software, but I've long since ditched that in favor of LinuxCNC's Enhanced Machine Controller, so currently it generates standard RS274 G Code.

//***************************
//		How to use REALIZE
//***************************

This is the documentation that is found at the top of the program file:

REALIZE supports converting LINE, CIRCLE, POLYLINE and POINT entities to RS-274 GCODE for use with LinuxCNC's Enhanced Machine Controller.

The layer of the drawing entity determines the drill or mill description. Entities on layer 0 are ignored.

The x and y work in the obvious way. The z dimension determines the cutting depth. Therefore the z of all entities NOT on layer 0 must be negative.

All cuts made with a particular tool are made in least-depth-first order.

Tools are used in decreasing alphabetic order, so if you have paths on layers named 250-MILL and 125-MILL, it will ask for the 1/4" before the 1/8".

A text of the form "vfeed: [real number]" or "hfeed: [real number]" on a tool layer will set the vertical/horizontal feed for that tool.

A text of the form "dwell: [real number]" on a drill layer will set the dwell at the bottom of the drill cycle.

In summary this means if you want to mill a 1" square path .1" deep with a 1/4" mill, draw a RECTANG from 0,0,-.1 to 1,1,-.1 on a layer named 250-MILL. If you want a 1/8" hole 1/2" deep in the center, put a POINT at .5,.5,-.5 on a layer named 125-DRILL. If you want the drill to feed at 10 ipm, put a text "vfeed: 10" on the 125-DRILL layer. Now run REALIZE (by typing "realize" at the prompt) and enter your desired safety height, output filename, and then select all.


//***************************
//		Helpful hints
//***************************

    Since REALIZE only exports POLYLINES, not LWPOLYLINES, you may have to convert to "Heavy" representation before running REALIZE, using the autocad CONVERTPOLY function. This is only a problem in R14 and newer. REALIZE is now confirmed to run on Autocad 2005.

    Texts used to specify feeds, tools, etc. need to be object type TEXT. Beware that using the icon/toolbar to add text may give you an MTEXT object. If so, just run the TEXT command at the command line instead.

    Use AutoCAD's OFFSET command to generate a tool path outside or inside a POLYLINE contour.

    Use AutoCAD's HATCH command (and then EXPLODE the hatch) to cut out an area.

    If you need to cut material in several Z passes, rotate AutoCAD's UCS so you can use ARRAY to copy your POLYLINE downward in Z.

The code of the program is pretty simple - you should be able to tweak the output for your (or your machine's) own needs, or add support for other entities. 