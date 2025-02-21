This folder contains images in svg format that can be used as a 
basis for create component modern images for SynEdit components 
and highlighters.

You can use the SVG Text Editor which is part 
[of SVG Shell Extensions](https://github.com/EtheaDev/SVGShellExtensions) to
create/edit svg files for the various components and then generate the required png
files.  See `TSYNSPELLCHECK.svg' for an example.  The png files need to be
trasparent and use colors that display reasonably well in both dark
and light backgrounds.

The generated png files then need to be inserted into the resource file 
`SynEditReg.dcr'.  You can use [Resource Hacker](https://www.angusj.com/resourcehacker/) 
to do that.

This [article](https://blogs.embarcadero.com/new-in-10-2-2-component-icons/) 
by David Millington contains information about the use of png files as
component images.
