package gov.nih.mipav.model.structures;

import gov.nih.mipav.view.MipavUtil;

import java.io.*;


/**
 *  This class clones (copies) by serializing the object (providing
 *  it can be serialized). See page 66 of Core Java 1.1 Vol. II by
 *  Horstmann, Cornell.
 *
 */

public class ModelSerialCloneable implements Cloneable, Serializable {


    /**
    *   Copies the object that extends this class. Can be slow sometimes
    *   because it actually copies (streams) to the hard drive.
    *
    */
    public Object clone() {

        try {
            ByteArrayOutputStream bout  = new ByteArrayOutputStream();
            ObjectOutputStream    out   = new ObjectOutputStream(bout);
            out.writeObject(this);
            ByteArrayInputStream  bin   = new ByteArrayInputStream(bout.toByteArray());
            ObjectInputStream      in   = new ObjectInputStream(bin);

            Object ret = in.readObject();
            in.close();
            out.close();

            bout.close();
            bin.close();

            return ret;

        }
        catch(Exception e) {
            MipavUtil.displayError("\nException reported :\n"
                                        + e
                                        + "\n        in: \n"
                                        + this.getClass().getName());
            return null;
        }
    }

    /**
     * Clone itself in order to save memory
     * @return Object
     */
    public Object cloneItself() {
       try {
         return super.clone();
       } catch ( CloneNotSupportedException e ) {
           MipavUtil.displayError( "\nException reported :\n" + e + "\n        in: \n" + this.getClass().getName() );
           return null;
       }
    }
}
