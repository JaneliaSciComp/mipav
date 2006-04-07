package gov.nih.mipav.view.srb;

import edu.sdsc.grid.io.srb.SRBFile;
import javax.swing.plaf.ComponentUI;

/**
 * Pluggable look and feel interface for <code>SRBFileChooser</code>.
 *
 * @version 1.0 04/04/06
 * @author Hailong Wang
 */

public abstract class SRBFileChooserUI extends ComponentUI
{
    public abstract SRBFileFilter getAcceptAllFileFilter(SRBFileChooser fc);
    public abstract SRBFileView getFileView(SRBFileChooser fc);

    public abstract String getApproveButtonText(SRBFileChooser fc);
    public abstract String getDialogTitle(SRBFileChooser fc);

    public abstract void rescanCurrentDirectory(SRBFileChooser fc);
    public abstract void ensureFileIsVisible(SRBFileChooser fc, SRBFile f);
}

