package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;


/**
 * This class represents a MINC dimension element. As far as I can tell, MINC dimension elements are always descriptors
 * of the three dimensions of the image file. Thus they contain a name and a length; the name is "xspace", "yspace", or
 * "zspace", and the length is the size of that dimension. So an axial image that was 256x256 and had 34 slices would
 * have "xspace" with length 256, "yspace" with length 256, and "zspace" with length 34. On the other hand, a coronal
 * image with the same dimensions would have "xspace" with length 256, "yspace" with length 34, and "zspace" with length
 * 256. This is because MINC always refers to the patient when assigning these dimension variables.
 */
public class FileMincDimElem extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8677079575179802672L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public int length;

    /** DOCUMENT ME! */
    public String name;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that creates a MINC dimension element with the given name and length.
     *
     * @param  name    Name of this dimension element (usually xspace, yspace, or zspace).
     * @param  length  Size of the dimension.
     */
    public FileMincDimElem(String name, int length) {
        this.name = name;
        this.length = length;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns a new MincDimElem with the same values as this one.
     *
     * @return  The new MincDimElem element.
     */
    public Object clone() {
        FileMincDimElem elem = new FileMincDimElem(this.name, this.length);

        return elem;
    }

    /**
     * Creates a readable version of this element; used in displayAboutInfo.
     *
     * @return  A string listing the name and length of this dimension element.
     */
    public String toString() {
        return "Name: " + name + " Length: " + length + "\n";
    }
}
