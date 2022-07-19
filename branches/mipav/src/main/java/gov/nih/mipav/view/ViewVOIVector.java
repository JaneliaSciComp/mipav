package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * A simple class that extends Vector mostly to clean-up syntax access to VOI info. in ViewComponentEditImage <code>
 * ((VOI)(VOIs.elementAt(i))).isActive()</code> would be a good example can be re-written as as: <code>
 * VOIs.VOIAt(i).isActive()</code>
 *
 * @version  Feb 2, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/view/ViewVOIVector.java $ $Revision: 10 $ $Date: 3/31/03 3:57p $</p>
 */

public class ViewVOIVector extends Vector <VOI> {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6003086288473157573L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an empty vector so that its internal data array has size 10 and its standard capacity increment is
     * zero.
     *
     * <p><i>Copied from the definition of <code>java.util.Vector</code></i></p>
     */
    public ViewVOIVector() {
        super();
    }

    /**
     * Constructs an empty vector with the specified initial capacity and with its capacity increment equal to zero.
     *
     * @param  initialsize  initial capacity of the vector
     */
    public ViewVOIVector(int initialsize) {
        super(initialsize);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Override the Vector method to ensure that object is a voi, and that the new voi's name is unique.
     *
     * @param      o  index of the VOI
     *
     * @exception  IllegalArgumentException  for any argument <code>o</code> which is not an instance of <code>
     *                                       gov.nih.mipav.model.structures.VOI</code>
     */
    public void addElement(VOI newVOI) {

        // check the voi name, fix if necessary
        while (contains(newVOI)) {
        	newVOI.setName(buildName(newVOI.getName()));
        }

        // add the voi to the vector
        super.addElement(newVOI);

        Preferences.debug("Add voi: name = " + newVOI.getName() + "\n");

    }

    /**
     * Returns the VOI at the index.
     *
     * @param   i  index of the VOI
     *
     * @return  the VOI at the index
     */
    public final VOI VOIAt(int i) {
        return ((VOI) (elementAt(i)));
    }

    /**
     * Builds a new voi name by incrementing the given name.
     *
     * @param   name  current name
     *
     * @return  the new name
     */
    protected String buildName(String name) {

        String newName = null;

        if (name == null) {
            newName = new String("newVOI1");

            return newName;
        }

        // get base name -- substring without a number at end
        int numIndex = name.length();

        for (int i = name.length() - 1; i >= 0; i--) {

            if (!Character.isDigit(name.charAt(i))) {
                numIndex = i + 1;

                break;
            }
        }

        String baseName = name.substring(0, numIndex);

        int num = 0;

        if (numIndex < name.length()) {
            num = Integer.parseInt(name.substring(numIndex));
        }

        num += 1;

        newName = new String(baseName + num);

        return newName;
    }
    
    public int getUniqueID()
    {
        int[] aiIDs = new int[size()];
        for ( int i = 0; i < size(); i++ )
        {
            String name = elementAt(i).getName();
            // get base name -- substring without a number at end
            int numIndex = name.length();

            for (int j = name.length() - 1; j >= 0; j--) {

                if (!Character.isDigit(name.charAt(j))) {
                    numIndex = j + 1;

                    break;
                }
            }
            int num = 0;
            if (numIndex < name.length()) {
                num = Integer.parseInt(name.substring(numIndex));
            }
            aiIDs[i] = num;
        }
        for ( int i = 0; i < size(); i++ )
        {
            boolean bFound = false;
            for ( int j = 0; j < size(); j++ )
            {
                if ( aiIDs[j] == i )
                {
                    bFound = true;
                    break;
                }
            }
            if ( !bFound )
            {
                return i;
            }
        }
        return size();
    }

}
