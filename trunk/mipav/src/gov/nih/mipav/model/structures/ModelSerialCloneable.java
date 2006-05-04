package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This class clones (copies) by serializing the object (providing it can be serialized). See page 66 of Core Java 1.1
 * Vol. II by Horstmann, Cornell.
 */

public class ModelSerialCloneable implements Cloneable, Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7436115594993339188L;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Copies the object that extends this class. Can be slow sometimes because it actually copies (streams) to the hard
     * drive.
     *
     * @return  DOCUMENT ME!
     */
    public Object clone() {

        try {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(bout);
            out.writeObject(this);

            ByteArrayInputStream bin = new ByteArrayInputStream(bout.toByteArray());
            ObjectInputStream in = new ObjectInputStream(bin);

            Object ret = in.readObject();
            in.close();
            out.close();

            bout.close();
            bin.close();

            return ret;

        } catch (Exception e) {
            MipavUtil.displayError("Error cloning " + this.getClass().getName() + " :\n" + e);
            Preferences.debug("Clone() exception:\n", Preferences.DEBUG_MINOR);
            for (int i = 0; i < e.getStackTrace().length; i++) {
                Preferences.debug("\t" + e.getStackTrace()[i] + "\n", Preferences.DEBUG_MINOR);
            }
            return null;
        }
    }

    /**
     * Clone itself in order to save memory.
     *
     * @return  Object
     */
    public Object cloneItself() {

        try {
            return super.clone();
        } catch (CloneNotSupportedException e) {
            MipavUtil.displayError("\nException reported :\n" + e + "\n        in: \n" + this.getClass().getName());

            return null;
        }
    }
}
