package gov.nih.mipav.view.srb;


import edu.sdsc.grid.io.srb.SRBFile;
import javax.swing.*;

/**
 * <code>SRBFileView</code is the SRB version of the javax.swing.filechooser.FileView.
 * 
 * @see javax.swing.JFileChooser
 *
 * @version 1.0 04/04/06
 * @author Hailong Wang
 *
 */
public abstract class SRBFileView {
    /**
     * The name of the file. Normally this would be simply
     * <code>f.getName()</code>.
     */
    public String getName(SRBFile f) {
        return null;
    };

    /**
     * A human readable description of the file. For example,
     * a file named <i>jag.jpg</i> might have a description that read:
     * "A JPEG image file of James Gosling's face".
     */
    public String getDescription(SRBFile f) {
        return null;
    }

    /**
     * A human readable description of the type of the file. For
     * example, a <code>jpg</code> file might have a type description of:
     * "A JPEG Compressed Image SRBFile"
     */
    public String getTypeDescription(SRBFile f) {
        return null;
    }

    /**
     * The icon that represents this file in the <code>JFileChooser</code>.
     */
    public Icon getIcon(SRBFile f) {
        return null;
    }

    /**
     * Whether the directory is traversable or not. This might be
     * useful, for example, if you want a directory to represent
     * a compound document and don't want the user to descend into it.
     */
    public Boolean isTraversable(SRBFile f) {
        return null;
    }

}

