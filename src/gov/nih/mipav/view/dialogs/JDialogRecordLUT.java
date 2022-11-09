package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * DOCUMENT ME!
 */
public class JDialogRecordLUT extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2240336763327716473L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Offset to draw the tick mark and value label. */
    protected int offsetX = 90;

    /** DOCUMENT ME! */
    protected int offsetY = 20;

    /** control points coordinate of LUT transfer function . */
    protected float[] x = new float[100];

    /** DOCUMENT ME! */
    protected int[] xN = new int[100];

    /** DOCUMENT ME! */
    protected float[] y = new float[100];

    /** DOCUMENT ME! */
    protected int[] yN = new int[100];

    /** DOCUMENT ME! */
    protected float[] z = new float[100];

    /** DOCUMENT ME! */
    protected int[] zN = new int[100];

    /** Lookup table of image. The rectangular LUT table. */
    private ViewJComponentLUTTable compLUT;

    /** ModelImage reference. */
    private ModelImage image;

    /** LUT table renference. */
    private ModelLUT lut;

    /** LUT table panel. */
    private JPanel lutTablePanel;

    /** Panel to hold the toolbar. */
    private JPanel tBarPanel;

    /** Toolbar. */
    private JToolBar toolBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create the LUT recorder dialog to record LUT.
     *
     * @param  _image  ModelImage model image renferencce
     * @param  _lut    ModelLUT LUT table reference
     */
    public JDialogRecordLUT(ModelImage _image, ModelLUT _lut) {
        super(_image.getParentFrame(), false);

        /*
         * try { _image.getParentFrame().setIconImage(MipavUtil.getIconImage("histolut.gif")); } catch
         * (FileNotFoundException error) { Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
         * ">.  Check that this file is available.\n"); System.err.println("Exception ocurred while getting <" +
         * error.getMessage() + ">.  Check that this file is available.\n"); }
         */

        image = _image;
        lut = _lut;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Only perform the save LUT action.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Save")) {
            writeImage();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Construct the panel LUT table viewing panel.
     */
    public void buildLUTPanel() {
        int borderSize = 3;

        lutTablePanel = new JPanel();
        lutTablePanel.setBorder(new EtchedBorder());

        compLUT = new ViewJComponentLUTTable(image, lut, new Dimension(130, lut.getExtents()[1]),
                                             ViewJComponentLUTTable.VERTICAL);
        compLUT.show(null);
        compLUT.setLocation(borderSize, borderSize + 10);

        // compLUT.addMouseMotionListener(this);
        // compLUT.addMouseListener(this);
        compLUT.setToolTipText("Lookup Table (LUT)");

        lutTablePanel.setLayout(null);
        lutTablePanel.add(compLUT);
        lutTablePanel.setPreferredSize(new Dimension(145 + 20, lut.getExtents()[1] + 20));
        lutTablePanel.setBounds(34, 20, compLUT.getSize().width + (2 * borderSize),
                                compLUT.getSize().height + (2 * borderSize));
        updateLUT(lut);

    }

    /**
     * Update the LUT view when drag the control points.
     *
     * @param  _lut  ModelLUT LUT reference with LUT changes.
     */
    public void dragPoint(ModelLUT _lut) {
        lut = _lut;
        compLUT.show(_lut);
    }

    /**
     * Build the dialog panel.
     */
    public void init() {
        setTitle("LUT: " + image.getImageName());
        setLocation(300, 400);
        buildToolbar();
        buildLUTPanel();

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(tBarPanel);
        contentBox.add(lutTablePanel);
        getContentPane().add(contentBox);
        setResizable(false);
        pack();
    }

    /**
     * Switching the different types of LUT view invoke this method to update the the LUT view.
     *
     * @param  _lut  ModelLUT LUT reference with LUT changes.
     */
    public void updateLUT(ModelLUT _lut) {
        compLUT.show(_lut);
        dragPoint(_lut);
    }


    /**
     * Build the toolbar, for now, only one save button.
     */
    private void buildToolbar() {
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;

        toolBar = new JToolBar();
        toolBar.setBorderPainted(true);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);

        JButton button = new JButton(MipavUtil.getIcon("save.gif"));

        button.addActionListener(this);
        button.setActionCommand("Save");
        button.setToolTipText("Save LUT table");
        button.setBorderPainted(false);
        button.setFocusPainted(true);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon("saveroll.gif"));
        button.setMargin(new Insets(0, 0, 0, 0));

        toolBar.add(button);

        tBarPanel = new JPanel(new BorderLayout());
        tBarPanel.add(toolBar, BorderLayout.WEST);
    }

    /**
     * Writes the image captured from the screen, using currentRectangle as the bounding box of the capture. Converts
     * pixels grabbed using a Robot to an RGB TIFF file.
     *
     * @return  DOCUMENT ME!
     */
    private boolean writeImage() {
        int[] pixels;
        int bufferSize, xDim, yDim;
        short[] buffer = null;
        ModelImage testImage = null;
        Robot robot;
        boolean save = true;
        ViewUserInterface userInterface = ViewUserInterface.getReference();

        Dimension d = new Dimension();
        Point p = new Point();

        p.x = 17;
        p.y = -1;
        SwingUtilities.convertPointToScreen(p, compLUT);

        d.width = compLUT.getWidth() - 12;
        d.height = compLUT.getHeight() + 4;

        Rectangle currentRectangle = new Rectangle(p, d);

        try {
            robot = new Robot();

            Image imagePix = robot.createScreenCapture(currentRectangle);

            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            bufferSize = 4 * xDim * yDim;
            pixels = new int[xDim * yDim];

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);

            pgTest.grabPixels();
        } catch (InterruptedException e) {
            Preferences.debug("Interrupted waiting for pixels!");

            return false;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException error) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        }

        try {
            int[] extents = new int[2];

            extents[0] = xDim; // RGB
            extents[1] = yDim;
            testImage = new ModelImage(ModelStorageBase.ARGB, extents, "Screen capture");
            buffer = new short[bufferSize];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("JDialogRecordLUT: unable to allocate enough memory for RGB image");

            return false;
        }

        int i, k;

        for (i = 0, k = 0; i < (xDim * yDim); i++, k += 4) {
            buffer[k] = (short) (255); // alpha
            buffer[k + 1] = (short) ((pixels[i] >> 16) & 0xFF); // Red
            buffer[k + 2] = (short) ((pixels[i] >> 8) & 0xFF); // Green
            buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
        }

        try {
            testImage.importData(0, buffer, true);
        } catch (IOException error) {
            MipavUtil.displayError("JDialogRecordLUT: Problems grabbing image!");
        }

        testImage.getFileInfo()[0].setPhotometric((short) 2); // Indicates RGB tiff file format

        if (save) {
            String fileName;
            String directory;
            FileIO fileIO = new FileIO();

            JFileChooser chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            File f = new File(chooser.getCurrentDirectory() + File.separator + image.getImageName() + "_LUT.tif");
            chooser.setSelectedFile(f);

            int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                fileIO.writeImage(testImage, new FileWriteOptions(fileName, directory, true));
                userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);
            } else {
                return false;
            }
        } else {
            new ViewJFrameImage(testImage);
        }

        return true;
    }


}
