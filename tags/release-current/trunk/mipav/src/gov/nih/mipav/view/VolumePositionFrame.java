package gov.nih.mipav.view;


import java.awt.*;

import java.io.*;

import javax.swing.*;


/**
 * VolumePositionFrame, used by the ViewJFrameTriImage class to display the volume position data.
 */
public class VolumePositionFrame extends JFrame {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3115465399315063208L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Reference to the parentFrame, to notify when this window closes:. */
    protected ViewJFrameTriImage parentFrame;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new VolumePositionFrame object.
     *
     * @param  parentFrame  DOCUMENT ME!
     * @param  tabbedPane   DOCUMENT ME!
     */
    public VolumePositionFrame(ViewJFrameTriImage parentFrame, JTabbedPane tabbedPane) {
        super("Coordinates window");
        this.parentFrame = parentFrame;
        setSize(250, 225);
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        try {
            setIconImage(MipavUtil.getIconImage("3plane_16x16.gif"));
        } catch (FileNotFoundException fnfe) { }

        MipavUtil.centerOnScreen(this);

        getContentPane().setLayout(new GridLayout(1, 1));
        getContentPane().add(tabbedPane);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void dispose() {
        getContentPane().removeAll();
        parentFrame.addTabbedPane();
        setVisible(false);
    }
}
