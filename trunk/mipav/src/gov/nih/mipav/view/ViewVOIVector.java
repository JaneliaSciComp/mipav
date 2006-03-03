package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.*;
import java.util.*;

/** 
*       A simple class that extends Vector mostly to clean-up
*       syntax access to VOI info. in ViewComponentEditImage
*
*       <code>((VOI)(VOIs.elementAt(i))).isActive()</code>
*       would be a good example can be re-written as as:
*       <code>VOIs.VOIAt(i).isActive()</code>
*
*		@version    Feb 2,  1997
*		@author     Matthew J. McAuliffe, Ph.D.
*
*       $Logfile: /mipav/src/gov/nih/mipav/view/ViewVOIVector.java $
*       $Revision: 10 $
*       $Date: 3/31/03 3:57p $
*
*/

public class ViewVOIVector extends Vector {
    /** Constructs an empty vector so that 
    *   its internal data array has size 10 
    *   and its standard capacity increment 
    *   is zero. 
    *   <p>
    *   <i>Copied from the definition of 
    *   <code>java.util.Vector</code></i>
    */
    public ViewVOIVector() {
        super();
    }
    
    /** Constructs an empty vector with the 
    *   specified initial capacity and with 
    *   its capacity increment equal to zero.  
    *   @param initialsize initial capacity of 
    *           the vector
    *   @exception IllegalArgumentException - 
    *           if the specified initial capacity 
    *           is negative
    *   <p>
    *   <i>Copied from the definition of 
    *   <code>java.util.Vector</code></i>
    */
    public ViewVOIVector(int initialsize) {
        super(initialsize);
    }
    /**
    *  Returns the VOI at the index
    *  @param i   index of the VOI
    *  @return    the VOI at the index
    */
    public final VOI VOIAt(int i) {
        return ((VOI)(elementAt(i)));
    }
    
    /**
    *  Override the Vector method to ensure that 
    *  object is a voi, and that the new voi's name is unique.
    *  @param i   index of the VOI
    *  @return    the VOI at the index
    *   @exception IllegalArgumentException for any argument <code>o</code> 
    *               which is not an instance of 
    *               <code>gov.nih.mipav.model.structures.VOI</code>
    */
    public void addElement(Object o) {
        VOI voi = null;
        
        // check that object is a VOI
        if (!(o instanceof VOI))
            throw new IllegalArgumentException();
        voi = (VOI)o;
        // check the voi name, fix if necessary
        while (contains(voi)) {
            voi.setName(buildName(voi.getName()));
        }
        
        // add the voi to the vector
        super.addElement(voi);
        
        Preferences.debug ("Add voi: name = " + voi.getName() + "\n");

    }
    
    /**
    *  Builds a new voi name by incrementing the given name.
    *  @param name   current name
    *  @return       the new name
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
        
}