package gov.nih.mipav.view.srb;

import edu.sdsc.grid.io.srb.SRBFile;

/**
 * <code>SRBFile</code> is the SRB version of the javax.swing.filechooser.FileFilter.
 * @see javax.swing.filechooser.FileFilter
 *
 * @version 1.0 04/04/06
 * @author Hailong Wang
 */
public abstract class SRBFileFilter {
    /**
     * Whether the given file is accepted by this filter.
     */
    public abstract boolean accept(SRBFile f);

    /**
     * The description of this filter. For example: "JPG and GIF Images"
     * @see FileView#getName
     */
    public abstract String getDescription();
}
