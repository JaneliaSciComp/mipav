package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This class clones (copies) by serializing the object (providing it can be serialized).
 */
public class ModelSerialCloneable implements Cloneable, Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7436115594993339188L;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Copies the object that extends this class through use of Serialization.
     *
     * @return  The cloned object. Null if there was an error.
     */
    public Object clone() {

        try {
            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ObjectOutputStream     objOut = new ObjectOutputStream(byteOut);
            objOut.writeObject(this);

            ByteArrayInputStream byteIn = new ByteArrayInputStream(byteOut.toByteArray());
            ObjectInputStream     objIn = new ObjectInputStream(byteIn);

            Object clonedObj = objIn.readObject();
            
            objIn.close();
            objOut.close();
            byteOut.close();
            byteIn.close();

            return clonedObj;

        } catch (Exception e) {
        	e.printStackTrace();
            MipavUtil.displayError("Error cloning " + this.getClass().getName() + " :\n" + e);
            Preferences.debug("Clone() exception:\n", Preferences.DEBUG_MINOR);

            for (int i = 0; i < e.getStackTrace().length; i++) {
                Preferences.debug("\t" + e.getStackTrace()[i] + "\n", Preferences.DEBUG_MINOR);
            }

            return null;
        }
    }
    
    /**
     * Does not use serialization for cloning, exists for performance comparison and shallow copies.
     * 
     * @throws CloneNotSupportedException 
     * 
     */
    public Object nativeClone() {
    	try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
			return null;
		}
    }
}
