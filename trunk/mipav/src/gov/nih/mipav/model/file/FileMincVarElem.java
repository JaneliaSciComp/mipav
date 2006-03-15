package gov.nih.mipav.model.file;

import java.util.*;
import gov.nih.mipav.model.structures.ModelSerialCloneable;

/**
*   This class represents a MINC variable element.  MINC variables consist of
*   a name, a dimid array (often empty), an attribute array, a type, a begin, and
*   a size.  The attribute array contains important image information.  The "type"
*   indicates the actual type of the variable (not the attributes).  The begin is
*   the location in the file where the variable is first written; the size is how
*   many bytes the variable takes up.  So for example the variable image has a bunch
*   of attributes such as signtype, vmin, and vmax that are necessary for proper
*   read.  Then the type is the type of the image (often short) and the begin is
*   where the image data actually begins.  The size is extents[0]*extents[1]*extents[2]*byte size.
*/
public class FileMincVarElem extends ModelSerialCloneable{
    public String name;
    public int nelems;
    public int[] dimid;
    public FileMincAttElem[] vattArray;
    public int nc_type;
    public int vsize;
    public int begin;
    public Vector values;

    public String   signtype;
    public double   vmin;
    public double   vmax;
    public String   units;
    public double   resolution;
    public String   comments;
    public double   start;
    public double   trueStart;
    public float[]  cosines;

    /**
    *   Constructor for the MINC variable element.
    *	@param name		The name of the variable (e.g., image, image-min, image-max).
    *	@param nelems	The number of elements in this variable.
    *	@param dimid	Array of dimension ids; often empty.
    */
    public FileMincVarElem(String name, int nelems, int[] dimid) {
        this.name = name;
        this.nelems = nelems;
        this.dimid = dimid;
    }

    /**
    *	Creates a new variable attribute array.  For example, the "image" variable has
    *	important information about the image recorded in the variables attribute array.
    *	@param length	The size of this variable attribute array.
    */
    public void createVattArray(int length) {
        vattArray = new FileMincAttElem[length];
    }

    /**
    *	Adds an attribute element to the variable attribute array.
    *	@param name		Name of the attribute.
    *	@param length	Length of the attribute.
    *	@param index	Where to put the element in the variable attribute array.
    */
    public void addVattElem(String name, int type, int length, int index) {
        FileMincAttElem elem = new FileMincAttElem(name, type, length);
        vattArray[index] = elem;
    }

    /**
    *	Sets the value of a variable attribute element.  If the element is
    *	image, xspace, yspace, or zspace, sets important image information.
    *	@param elem		Element to set value of.
    *	@param value	Value to set element to.
    *	@param index	Index in attribute element to set value to.
	*/
    public void addVattValue(FileMincAttElem elem, Object value, int index) {
        if (this.name.equals("xspace") ||
            this.name.equals("yspace") ||
            this.name.equals("zspace")) {
            if (elem.name.equals("units") && index == elem.values.length-1) {
                String s = "";
                for (int i=0; i<elem.values.length; i++) {
                    if (elem.values[i] != null) s+=((Character)elem.values[i]).charValue();
                }
                units = s.trim();
            }
            else if (elem.name.equals("step")) {
                resolution = ((Double)value).doubleValue();
            }
            else if (elem.name.equals("start")) {
            	start = ((Double)value).doubleValue();
            	trueStart = start;
            }
            else if (elem.name.equals("comments")) {
                String s = "";
                for (int i=0; i<elem.values.length; i++) {
                    if (elem.values[i] != null) s+=((Character)elem.values[i]).charValue();
                }
                comments = s.trim();
            }
            else if (elem.name.equals("direction_cosines")) {
                cosines = new float[3];
                if (elem.values[0] != null) cosines[0] = (float)(((Double)elem.values[0]).doubleValue());
                if (elem.values[1] != null) cosines[1] = (float)(((Double)elem.values[1]).doubleValue());
                if (elem.values[2] != null) cosines[2] = (float)(((Double)elem.values[2]).doubleValue());
            }
        }
        else if (this.name.equals("image")) {
            if (elem.name.equals("signtype") && index == elem.values.length-1) {
                String s = "";
                for (int i=0; i<elem.values.length; i++) {
                    if (elem.values[i] != null) s+=((Character)elem.values[i]).charValue();
                }
                signtype = s;
            }
        }
        elem.setValue(value, index);
    }

    /**
    *	Returns the variable attribute located at <code>index</code> in the array.
    *	@param index	Where in the variable attribute array the attribute is located.
    *	@return			The attribute.
    */
    public FileMincAttElem getVattElem(int index) {
        return vattArray[index];
    }

    /**
    *	Adds value to values vector.
    *	@param value	Value to add.
    */
    public void setValue(Object value) {
        values.addElement(value);
    }

    /**
    *	Sets the information in the variable that comes after the attribute array.
    *	@param nc_type	Type of data (byte, short, float, etc.)
    *	@param vsize	Size of the variable.
    *	@param begin	Where this variable's data starts.
    */
    public void setOther(int nc_type, int vsize, int begin) {
        this.nc_type = nc_type;
        this.vsize = vsize;
        this.begin = begin;
        values = new Vector();
    }

    /**
    *	Converts information contained in variable to a readable form.  This is
    *	displayed in displayAboutInfo.
    *	@return		String version of information contained in this variable.
    */
    public String toString() {
        String s;
        String typeStr;

        switch (nc_type) {
          case 1:
            typeStr = " Type: NC_BYTE ";
            break;
          case 2:
              typeStr = " Type: NC_CHAR ";
            break;
          case 3:
              typeStr = " Type:NC_SHORT ";
            break;
          case 4:
              typeStr = " Type: NC_INT ";
            break;
          case 5:
              typeStr = " Type: NC_FLOAT ";
            break;
          case 6:
              typeStr = " Type: NC_DOUBLE ";
            break;
          default:
              typeStr = " Type: ???? ";
        }

        s="Name: " + name + " VSize: "+ vsize + typeStr +" Begin: "+begin+ "\n  Attributes:\n";
        for(int i=0; i<vattArray.length; i++) {
            s+="\t"+vattArray[i] + "\n";
        }
        if (!name.equals("image")) {
            s+="  Variable value: ";
            if (values.size() > 1) s+="\n";
            for (int i=0; i<values.size(); i++) {
                s+="\t"+values.elementAt(i) + "\n";
            }
        }
        return s;
    }

    /**
    *	Creates a new version of this variable, copying all the important information.
    *	@return		An exact copy of this variable.
    */
    public Object clone(){
        FileMincVarElem elem = new FileMincVarElem(this.name, this.nelems, this.dimid);
        elem.vattArray       = new FileMincAttElem[this.vattArray.length];
        for (int i=0; i<this.vattArray.length; i++)
            elem.vattArray[i] = (FileMincAttElem)this.vattArray[i].clone();
        elem.nc_type = this.nc_type;
        elem.vsize = this.vsize;
        elem.begin = this.begin;
        elem.values = (Vector)this.values.clone();
        elem.resolution = this.resolution;
        elem.start = this.start;
        elem.trueStart = this.trueStart;
        elem.signtype = this.signtype;
        elem.units = this.units;
        elem.vmin = this.vmin;
        elem.vmax = this.vmax;
        return elem;
    }
}
