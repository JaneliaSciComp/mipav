package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.AlgorithmHistogram;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.ScriptRunner;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.Preferences.DefaultDisplay;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This is an abstract class used to display images in a 2D planar format.
 * 
 * @version 0.1 Oct 1, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * @see ViewJComponentEditImage
 */
public abstract class ViewJFrameBase extends JFrame implements ViewImageUpdateInterface, ActionListener,
        WindowListener, ComponentListener, ChangeListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3040123414501128141L;

    /** Display mode image A. */
    public static final int IMAGE_A = 0;

    /** Display mode image B. */
    public static final int IMAGE_B = 1;

    /** Display mode image A and B. */
    public static final int IMAGE_A_B = 2;

    /** DOCUMENT ME! */
    protected int zoomMode = ViewJComponentEditImage.LINEAR_ZOOM;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Labels for the current absolute position:. */
    protected JLabel absoluteLabel = null;

    /** Labels for the current absolute position values:. */
    protected JLabel[] absoluteLabelVals = null;

    /** JPanel containing the absoulte position labels:. */
    protected JPanel absolutePanel = new JPanel(new GridBagLayout());

    /** Indicates the amount of blending when two images are loaded in the image frame. */
    protected float alphaBlend = 0.5f;

    /** Indicates which image is to be acted upon when two images are displayed. */
    protected int displayMode = ViewJFrameBase.IMAGE_A;

    /** Reference to the image A of this frame. */
    protected ModelImage imageA = null;

    /** Reference to the image B of this frame. */
    protected ModelImage imageB = null;

    /** Holds a reference (link) to another frame so that it be updated with this frame. */
    protected ViewJFrameImage linkFrame;

    /** Holds a reference (link) to another frame so that it can be updated with this frame. */
    protected ViewJFrameTriImage linkTriFrame;

    /** Reference to LUT for image A and to the default LUT for image A. */
    protected ModelLUT defaultLUTa, LUTa;

    /** Reference to LUT for image B and to the default LUT for image B. */
    protected ModelLUT defaultLUTb, LUTb;

    /** Reference to progress bar. */
    protected ViewJProgressBar progressBar;

    /** Labels for the current scanner position:. */
    protected JLabel scannerLPSLabel = null;

    /** Labels for the current scanner position:. */
    protected JLabel scannerRASLabel = null;

    /** Labels for the current scanner position values:. */
    protected JLabel[] scannerLPSLabelVals = null;

    /** Labels for the current scanner position values:. */
    protected JLabel[] scannerRASLabelVals = null;

    /** JPanel containing the scanner position labels:. */
    protected JPanel scannerLPSPanel = new JPanel(new GridBagLayout());

    /** JPanel containing the scanner position labels:. */
    protected JPanel scannerRASPanel = new JPanel(new GridBagLayout());

    /** The main tabbed pane in the volume view frame. */
    protected JTabbedPane tabbedPane;

    /** The tabbed pane to hold teh LPS, RAS and absolution coordinate panels. */
    protected JTabbedPane scannerTabbedPane;

    /** Reference to the user interface. */
    protected ViewUserInterface userInterface;
    
    /** Whether the frame's close() method is being called. */
    protected boolean isClosing = false;

    /** Blue channel value of the paint color. */
    private int blue;

    /** Tells whether the ViewMenuBuilder should allow Close Image(B) after loading image B. */
    private boolean enableCloseImageB = true;
    
    private boolean newFrameCreated = false;

    /** Green channel value of the paint color. */
    private int green;

    /** Save if last state was iconified or normal. */
    private int lastState = Frame.NORMAL;

    /** Opacity value of the paint color. */
    private float opacity;

    /** Reference to the BitSet map used in painting the image. */
    private BitSet paintBitmap;

    /** Red channel value of the paint color. */
    private int red;

    /** DOCUMENT ME! */
    private String voiSavedFileName = null;

    /** Variable indicating if images of like dimensions should be scrolled simult. */
    protected boolean linkedScrolling = false;

    /** voxel absolute goto components* */
    protected JLabel absoluteXLabel, absoluteYLabel, absoluteZLabel;

    protected JTextField absoluteXTextField, absoluteYTextField, absoluteZTextField;

    protected JButton absoluteGoToButton;

    protected JPanel absoluteGoToPanel;

    /** scanner lps goto components* */
    protected JLabel scannerLPS_RLLabel, scannerLPS_APLabel, scannerLPS_ISLabel;

    protected JTextField scannerLPS_RLTextField, scannerLPS_APTextField, scannerLPS_ISTextField;

    protected JButton scannerLPS_GoToButton;

    protected JPanel scannerLPS_GoToPanel;

    /** scanner ras goto components* */
    protected JLabel scannerRAS_RLLabel, scannerRAS_APLabel, scannerRAS_ISLabel;

    protected JTextField scannerRAS_RLTextField, scannerRAS_APTextField, scannerRAS_ISTextField;

    protected JButton scannerRAS_GoToButton;

    protected JPanel scannerRAS_GoToPanel;

    /** talairach goto components* */
    protected JLabel talXLabel, talYLabel, talZLabel;

    protected JTextField talXTextField, talYTextField, talZTextField;

    protected JButton talGoToButton;

    protected JPanel talGoToPanel;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Makes an image frame.
     * 
     * @param _imageA model image A
     * @param _imageB model image B
     */
    public ViewJFrameBase(final ModelImage _imageA, final ModelImage _imageB) {
        imageA = _imageA;
        imageB = _imageB;
        displayMode = ViewJFrameBase.IMAGE_A;

        userInterface = ViewUserInterface.getReference();
        getContentPane().setLayout(new BorderLayout());
        addWindowListener(this);

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************
    /**
     * Method to handle action events generated by the main UI when the current frame is selected. Implemented by
     * specific frame classes.
     * 
     * @param event the event from the UI
     */
    public abstract void actionPerformed(ActionEvent event);

    /**
     * Gets the control widgets for the frame.
     * 
     * @return the frame's image controls
     */
    public abstract ViewControlsImage getControls();

    /**
     * Accessor that returns the imageA.
     * 
     * @return imageA
     */
    public abstract ModelImage getImageA();

    /**
     * Accessor that returns the imageB.
     * 
     * @return imageB
     */
    public abstract ModelImage getImageB();

    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls. Abstract and must be extended.
     */
    public abstract void removeControls();

    /**
     * Sets the active image (image that should be processed for drawing VOIs and applying algorithms.
     * 
     * @param active IMAGE_A or IMAGE_B
     */
    public abstract void setActiveImage(int active);

    /**
     * Sets the alpha blending parameter for two image display.
     * 
     * @param value amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to be
     *            displayed
     */
    public abstract void setAlphaBlend(int value);

    /**
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts only the menus and controls needed to
     * controls the operations of this frame. Different image frames have different menu and controls.
     */
    public abstract void setControls();

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     * 
     * @param flag if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public abstract void setEnabled(boolean flag);

    /**
     * Accessor that sets the imageB.
     * 
     * @param imageB The image to make image-B in this frame.
     */
    public abstract void setImageB(ModelImage imageB);

    /**
     * If true do not getMask on a setActiveImage command so as to keep the mask from the old active image.
     * 
     * @param flag if true do not getMask on a setActiveImage command
     */
    public abstract void setPaintBitmapSwitch(boolean flag);

    /**
     * Set the RGB table for image A.
     * 
     * @param RGBT the RGB table to use for image A
     */
    public abstract void setRGBTA(ModelRGB RGBT);

    /**
     * Set the RGB table for image B.
     * 
     * @param RGBT the RGB table to use for image B
     */
    public abstract void setRGBTB(ModelRGB RGBT);

    /**
     * Sets the Title bar of the frame.
     */
    public abstract void setTitle();

    // ************************************************************************
    // **************************** ViewImageUpdateInterface ******************
    // ************************************************************************

    /**
     * The extents on this image have changed, so the extents need to be read in again and menus, panes and slide bars
     * adjusted accordingly.
     * 
     * @return true if the update was successful, false otherwise
     * 
     * @see ViewImageUpdateInterface
     */
    public abstract boolean updateImageExtents();

    /**
     * Makes an aboutDialog box that displays information about the image slice.
     * 
     * @param zSlice index to slice in z-Plane about the specific image slice
     * @param tSlice index to slice in time about the specific image slice
     */
    public void about(final int zSlice, final int tSlice) {
        boolean geSigna = false;
        boolean geSigna4x = false;
        boolean dicom = false;

        // DICOM images are special--handle separately
        boolean analyze = false; // currently displaying analyze differentl;y. update soon

        // does NOT NOT NOT NOT NOT handle ANALYZE_MULTIFILE files.
        boolean mgh = false;
        @SuppressWarnings("unused")
        boolean nifti = false;
        boolean nrrd = false;
        boolean spm = false;
        boolean xml = false; // special handling for XML files
        boolean minc = false;
        boolean mincHDF = false;
        boolean vista = false;

        if (imageA.getFileInfo()[0] != null) {

            if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                dicom = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.XML)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                xml = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.RAW)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                xml = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.JIMI)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                xml = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.ANALYZE)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                analyze = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.MGH)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                mgh = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.NIFTI)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                nifti = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.NRRD)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                nrrd = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.SPM)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                spm = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.GE_GENESIS)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                geSigna = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.GE_SIGNA4X)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                geSigna4x = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.MINC)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                minc = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.MINC_HDF)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                mincHDF = true;
            } else if ( ( (imageA.getFileInfo()[0]).getFileFormat() == FileUtility.VISTA)
                    && (displayMode == ViewJFrameBase.IMAGE_A)) {
                vista = true;
            } else if ( (imageB != null) && (imageB.getFileInfo()[0] != null)) {

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    dicom = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.XML)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    xml = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.RAW)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    xml = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.JIMI)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    xml = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.ANALYZE)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    analyze = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.MGH)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    mgh = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.NIFTI)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    nifti = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.NRRD)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    nrrd = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.SPM)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    spm = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.GE_GENESIS)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    geSigna = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.GE_SIGNA4X)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    geSigna4x = true;
                }

                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.MINC)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    minc = true;
                }
                
                if ( ( (imageB.getFileInfo()[0]).getFileFormat() == FileUtility.VISTA)
                        && (displayMode == ViewJFrameBase.IMAGE_B)) {
                    vista = true;
                }
            }
        }

        JDialogBase aboutDialog;

        if (dicom) {
            aboutDialog = new JDialogFileInfoDICOM(this, "Image information");
        } else if (xml) {
            aboutDialog = new JDialogFileInfoXML(this, "Image information", imageA);
        } else {

            if (analyze) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageA);
                } else {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageB);
                }
            } else if (mgh) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageA);
                } else {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageB);
                }
            } else if (nrrd) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageA);
                } else {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageB);
                }
            } else if (spm) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageA);
                } else {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageB);
                }
            } else if (vista) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageA);
                } else {
                    aboutDialog = new JDialogFileInfo(this, "Image Information", imageB);
                }
            }else if (geSigna) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogTextGE(this, "Image Information", imageA, zSlice);
                } else {
                    aboutDialog = new JDialogTextGE(this, "Image Information", imageB, zSlice);
                }
            } else if (geSigna4x) {

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    aboutDialog = new JDialogTextGE4X(this, "Image Information", imageA, zSlice);
                } else {
                    aboutDialog = new JDialogTextGE4X(this, "Image Information", imageB, zSlice);
                }
            } else if (minc) {
                aboutDialog = new JDialogFileInfoMinc(this, "Image Information");
            } else if (mincHDF) {
                aboutDialog = new JDialogFileInfoMincHDF(this, "Image Information");
            } else {
                aboutDialog = new JDialogText(this, "Image information");
            }
        }

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            aboutDialog.setTitle("Info: " + imageA.getImageName());
            imageA.displayAboutInfo(aboutDialog, zSlice, tSlice, dicom, xml);
        } else {
            aboutDialog.setTitle("Info: " + imageB.getImageName());
            imageB.displayAboutInfo(aboutDialog, zSlice, tSlice, dicom, xml);
        }

        if (aboutDialog != null) {
            aboutDialog.setLocation(100, 100);
            aboutDialog.setVisible(true);
        }

        if ( !dicom) {

            try {
                ((JDialogText) aboutDialog).setScrollPaneTop();
            } catch (final ClassCastException cce) {
                Preferences.debug("ClassCastException in ViewJFrameBase.about\n", Preferences.DEBUG_MINOR);
            }

            aboutDialog.validate();
        }

    }

    /**
     * The label panel of the x, y, z slider position.
     */
    public void buildLabelPanel() {

        if (scannerLPSLabel != null) {
            return;
        }

        if (scannerRASLabel != null) {
            return;
        }

        scannerLPSLabel = new JLabel("Scanner LPS Coordinates");
        scannerLPSLabel.setForeground(Color.black);
        scannerLPSLabel.setFont(MipavUtil.font14B);
        scannerLPSLabelVals = new JLabel[4];
        scannerLPSLabelVals[0] = new JLabel("R-L: ");
        scannerLPSLabelVals[1] = new JLabel("A-P: ");
        scannerLPSLabelVals[2] = new JLabel("I-S: ");
        scannerLPSLabelVals[3] = new JLabel("Intensity: ");

        scannerLPS_RLLabel = new JLabel("R-L:");
        scannerLPS_RLLabel.setForeground(Color.black);
        scannerLPS_RLLabel.setFont(MipavUtil.font14B);
        scannerLPS_RLTextField = new JTextField(4);
        scannerLPS_APLabel = new JLabel("A-P:");
        scannerLPS_APLabel.setForeground(Color.black);
        scannerLPS_APLabel.setFont(MipavUtil.font14B);
        scannerLPS_APTextField = new JTextField(4);
        scannerLPS_ISLabel = new JLabel("I-S:");
        scannerLPS_ISLabel.setForeground(Color.black);
        scannerLPS_ISLabel.setFont(MipavUtil.font14B);
        scannerLPS_ISTextField = new JTextField(4);
        scannerLPS_GoToButton = new JButton("Go to");
        scannerLPS_GoToButton.setForeground(Color.black);
        scannerLPS_GoToButton.setFont(MipavUtil.font14B);
        scannerLPS_GoToPanel = new JPanel();
        scannerLPS_GoToPanel.add(scannerLPS_RLLabel);
        scannerLPS_GoToPanel.add(scannerLPS_RLTextField);
        scannerLPS_GoToPanel.add(scannerLPS_APLabel);
        scannerLPS_GoToPanel.add(scannerLPS_APTextField);
        scannerLPS_GoToPanel.add(scannerLPS_ISLabel);
        scannerLPS_GoToPanel.add(scannerLPS_ISTextField);
        scannerLPS_GoToPanel.add(scannerLPS_GoToButton);

        scannerRASLabel = new JLabel("Scanner RAS Coordinates");
        scannerRASLabel.setForeground(Color.black);
        scannerRASLabel.setFont(MipavUtil.font14B);
        scannerRASLabelVals = new JLabel[4];
        scannerRASLabelVals[0] = new JLabel("L-R: ");
        scannerRASLabelVals[1] = new JLabel("P-A: ");
        scannerRASLabelVals[2] = new JLabel("I-S: ");
        scannerRASLabelVals[3] = new JLabel("Intensity: ");

        scannerRAS_RLLabel = new JLabel("L-R:");
        scannerRAS_RLLabel.setForeground(Color.black);
        scannerRAS_RLLabel.setFont(MipavUtil.font14B);
        scannerRAS_RLTextField = new JTextField(4);
        scannerRAS_APLabel = new JLabel("P-A:");
        scannerRAS_APLabel.setForeground(Color.black);
        scannerRAS_APLabel.setFont(MipavUtil.font14B);
        scannerRAS_APTextField = new JTextField(4);
        scannerRAS_ISLabel = new JLabel("I-S:");
        scannerRAS_ISLabel.setForeground(Color.black);
        scannerRAS_ISLabel.setFont(MipavUtil.font14B);
        scannerRAS_ISTextField = new JTextField(4);
        scannerRAS_GoToButton = new JButton("Go to");
        scannerRAS_GoToButton.setForeground(Color.black);
        scannerRAS_GoToButton.setFont(MipavUtil.font14B);
        scannerRAS_GoToPanel = new JPanel();
        scannerRAS_GoToPanel.add(scannerRAS_RLLabel);
        scannerRAS_GoToPanel.add(scannerRAS_RLTextField);
        scannerRAS_GoToPanel.add(scannerRAS_APLabel);
        scannerRAS_GoToPanel.add(scannerRAS_APTextField);
        scannerRAS_GoToPanel.add(scannerRAS_ISLabel);
        scannerRAS_GoToPanel.add(scannerRAS_ISTextField);
        scannerRAS_GoToPanel.add(scannerRAS_GoToButton);

        absoluteLabel = new JLabel("Absolute Volume Coordinates");
        absoluteLabel.setToolTipText("Coordinates in 3D image space");
        absoluteLabel.setForeground(Color.black);
        absoluteLabel.setFont(MipavUtil.font14B);
        absoluteLabelVals = new JLabel[5];
        absoluteLabelVals[0] = new JLabel("X: ");
        absoluteLabelVals[1] = new JLabel("Y: ");
        absoluteLabelVals[2] = new JLabel("Z: ");
        absoluteLabelVals[3] = new JLabel("Intensity: ");
        absoluteLabelVals[4] = new JLabel("1D index: ");

        absoluteXLabel = new JLabel("X:");
        absoluteXLabel.setForeground(Color.black);
        absoluteXLabel.setFont(MipavUtil.font14B);
        absoluteXTextField = new JTextField(4);
        absoluteYLabel = new JLabel("Y:");
        absoluteYLabel.setForeground(Color.black);
        absoluteYLabel.setFont(MipavUtil.font14B);
        absoluteYTextField = new JTextField(4);
        absoluteZLabel = new JLabel("Z:");
        absoluteZLabel.setForeground(Color.black);
        absoluteZLabel.setFont(MipavUtil.font14B);
        absoluteZTextField = new JTextField(4);
        absoluteGoToButton = new JButton("Go to");
        absoluteGoToButton.setForeground(Color.black);
        absoluteGoToButton.setFont(MipavUtil.font14B);
        absoluteGoToPanel = new JPanel();
        absoluteGoToPanel.add(absoluteXLabel);
        absoluteGoToPanel.add(absoluteXTextField);
        absoluteGoToPanel.add(absoluteYLabel);
        absoluteGoToPanel.add(absoluteYTextField);
        absoluteGoToPanel.add(absoluteZLabel);
        absoluteGoToPanel.add(absoluteZTextField);
        absoluteGoToPanel.add(absoluteGoToButton);

        for (int i = 0; i < 4; i++) {
            scannerLPSLabelVals[i].setForeground(Color.black);
            scannerLPSLabelVals[i].setFont(MipavUtil.font14B);

            scannerRASLabelVals[i].setForeground(Color.black);
            scannerRASLabelVals[i].setFont(MipavUtil.font14B);

            absoluteLabelVals[i].setForeground(Color.black);
            absoluteLabelVals[i].setFont(MipavUtil.font14B);
        }

        absoluteLabelVals[4].setForeground(Color.black);
        absoluteLabelVals[4].setFont(MipavUtil.font14B);

        final GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.anchor = GridBagConstraints.NORTHWEST;
        gbc2.fill = GridBagConstraints.BOTH;
        // gbc2.weightx = 1;
        // gbc2.weighty = 1;

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;

        scannerLPSPanel.add(scannerLPSLabel, gbc2);
        scannerRASPanel.add(scannerRASLabel, gbc2);
        absolutePanel.add(absoluteLabel, gbc2);

        gbc2.gridy++;
        scannerLPSPanel.add(new JLabel(), gbc2);
        scannerRASPanel.add(new JLabel(), gbc2);
        absolutePanel.add(new JLabel(), gbc2);

        for (int i = 0; i < 4; i++) {
            gbc2.gridy++;
            scannerLPSPanel.add(scannerLPSLabelVals[i], gbc2);
            scannerRASPanel.add(scannerRASLabelVals[i], gbc2);
            absolutePanel.add(absoluteLabelVals[i], gbc2);
        }

        gbc2.gridy++;
        absolutePanel.add(absoluteLabelVals[4], gbc2);

        gbc2.gridy++;
        absolutePanel.add(absoluteGoToPanel, gbc2);
        scannerLPSPanel.add(scannerLPS_GoToPanel, gbc2);
        scannerRASPanel.add(scannerRAS_GoToPanel, gbc2);
    }

    /**
     * Returns whether or not the close image B option should appear after loading.
     * 
     * @return whether the &quot;Close image B&quot; option should appear after image B is loaded
     */
    public boolean canCloseImageBAfterLoad() {
        return enableCloseImageB;
    }
    
    public boolean newFrameBeenCreated() {
        return newFrameCreated;
    }

    /**
     * Closes both image A and image B (if it exists). It ensures the images are un-registered from the main-frame then
     * removes any display listeners.
     */
    public void close() {
    	isClosing = true;
    	
        setVisible(false);

        userInterface.unregisterFrame(this);

        if (imageA != null) {
            imageA.removeImageDisplayListener(this);
        }

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        dispose();
        
        isClosing = false;
    }

    /**
     * Removes image B from the frame and resets image A to be the active frame for all image controls. Ensures the
     * image is removed, disconnected from the frame and un-registered. Also informs the garbage-collector to release
     * any held memory.
     */
    public void closeImageB() {

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);

            if ( (imageB.getImageFrameVector() != null) && imageB.getImageFrameVector().isEmpty()) {
                imageB.disposeLocal();
                userInterface.unRegisterImage(imageB);
            }

            imageB = null;

            if (linkTriFrame != null) {
                linkTriFrame.setActiveImage(ViewJFrameBase.IMAGE_A);
                linkTriFrame.closeImageB();
            }
        }

        System.gc();
    }

    /**
     * Does nothing.
     * 
     * @param event the component event
     */
    public void componentHidden(final ComponentEvent event) {}

    /**
     * Does nothing.
     * 
     * @param event the component event
     */
    public void componentMoved(final ComponentEvent event) {}

    // ************************************************************************
    // ************************* Component Events *****************************
    // ************************************************************************

    /**
     * Does nothing.
     * 
     * @param event the component event
     */
    public synchronized void componentResized(final ComponentEvent event) {}

    /**
     * Does nothing.
     * 
     * @param event the component event
     */
    public void componentShown(final ComponentEvent event) {}

    /**
     * Returns a default alphaBlend value for blending of two images.
     * 
     * @return a default alphaBlend value
     */
    public float getAlphaBlend() {
        return 0.5f;
    }

    /**
     * Accessor that returns displayMode.
     * 
     * @return displayMode
     */
    public int getDisplayMode() {
        return displayMode;
    }

    /**
     * Accessor that returns the title of image A.
     * 
     * @return frame title for Image A
     */
    public String getImageNameA() {
        return imageA.getImageName();
    }

    /**
     * Accessor that returns the title of image B.
     * 
     * @return frame title for Image B
     */
    public String getImageNameB() {
        return imageB.getImageName();
    }

    /**
     * getLastState.
     * 
     * @return lastState Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public int getLastState() {
        return lastState;
    }

    /**
     * Accessor that returns LUTa.
     * 
     * @return LUTa
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Accessor that returns LUTb.
     * 
     * @return LUTb
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }
    
    /**
     * Accessor that returns the default LUTa for the frame.
     * 
     * @return defaultLUTa
     */
    public ModelLUT getDefaultLUTa() {
		return defaultLUTa;
	}

    /**
     * Accessor that returns the default LUTb for the frame.
     * 
     * @return defaultLUTb
     */
	public ModelLUT getDefaultLUTb() {
		return defaultLUTb;
	}

	/**
     * Get the RGB table for image A.
     * 
     * @return null (may be overridden by inheriting classes)
     */
    public ModelRGB getRGBTA() {
        return null;
    }

    /**
     * Get the RGB table for image B.
     * 
     * @return null (may be overridden by inheriting classes)
     */
    public ModelRGB getRGBTB() {
        return null;
    }

    /**
     * Get the ViewJFrameTriImage reference.
     * 
     * @return linkTriFrame ViewJFrameTriImage.
     */
    public ViewJFrameTriImage getTriImg() {
        return linkTriFrame;
    }

    /**
     * Accessor that returns the user interface.
     * 
     * @return the user interface
     */
    public ViewUserInterface getUserInterface() {
        return userInterface;
    }

	/**
     * This method loads all VOIs to the active image from the default VOI directory for that image.
     * 
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadAllVOIs(boolean quietMode) {

        String fileDir;
        String imageName;
        String voiDir;
        ModelImage img;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();
        } else if (displayMode == ViewJFrameBase.IMAGE_B) {
            img = this.getImageB();
        } else {

            if ( !quietMode) {
                MipavUtil.displayError(" Cannot open VOIs when viewing both images");
            }

            return;
        }

        fileDir = img.getFileInfo(0).getFileDirectory();

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            imageName = img.getFileInfo(0).getFileName();

            final int index = imageName.lastIndexOf(".");

            if (index > 0) {
                imageName = imageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = imageName.length();

            for (int i = imageName.length() - 1; i >= 0; i--) {
                final char myChar = imageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                imageName = new String("DICOM");
            } else {
                imageName = imageName.substring(0, newIndex);
            }
        } else {
            imageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = imageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        loadAllVOIsFrom(voiDir, quietMode);

    } // end loadAllVOIs()

    /**
     * This method loads all VOIs to the active image from a given directory.
     * 
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadAllVOIsFrom(final String voiDir, boolean quietMode) {

        int i, j;
        VOI[] VOIs;
        FileVOI fileVOI;
        ModelImage currentImage;

        try {

            if (displayMode == ViewJFrameBase.IMAGE_A) {
                currentImage = imageA;
            } else if (displayMode == ViewJFrameBase.IMAGE_B) {
                currentImage = imageB;
            } else {

                if ( !quietMode) {
                    MipavUtil.displayError(" Cannot open VOIs when viewing both images");
                }

                return;
            }

            // if voiDir does not exist, then return
            // if voiDir exists, then get list of voi's from directory (*.voi)
            final File voiFileDir = new File(voiDir);
            final Vector<String> filenames = new Vector<String>();
            final Vector<Boolean> isLabel = new Vector<Boolean>();

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // get list of files
                final File[] files = voiFileDir.listFiles();

                for (final File element : files) {

                    if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
                        filenames.add(element.getName());
                        isLabel.add(false);
                    } else if (element.getName().endsWith(".lbl")) {
                        filenames.add(element.getName());
                        isLabel.add(true);
                    }
                }
            } else { // voiFileDir either doesn't exist, or isn't a directory

                if ( !quietMode) {
                    MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
                }

                return;
            }

            // open each voi array, then register voi array to this image
            for (i = 0; i < filenames.size(); i++) {

                fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, currentImage);

                VOIs = fileVOI.readVOI(isLabel.get(i));

                for (j = 0; j < VOIs.length; j++) {
                    currentImage.registerVOI(VOIs[j]);
                }
            }

            // when everything's done, notify the image listeners
            currentImage.notifyImageDisplayListeners();

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
            }
        }

    } // end loadAllVOIsFrom()

    /**
     * Loads an image into imageB slot of frame. Matches the imageB resolutions and dimensions to imageA, and can
     * optionally match the images' origins and orientations.
     * 
     * @param obj an object which is either a ModelImage to be loaded into the modelimage (as say, imageB, imported from
     *            another frame) or a File to be read in via FileIO.readImage(...)
     * @param compImage image component where image is set so that it can be displayed.
     * @param stackFlag flag indicating multi file
     * @param doOrigins when calling MatchImages to match resolutions and dimensions, should origins be matched too?
     * @param doOrients when calling MatchImages to match resolutions and dimensions, should orientations be matched
     *            too?
     * 
     * @return true if the load was successful, false otherwise
     * 
     * @see JDialogLoadImage
     */
    public boolean loadImage(final Object obj, final ViewJComponentEditImage compImage, final boolean stackFlag,
            final boolean doOrigins, final boolean doOrients, final double defaultValue, final double defaultRed,
            final double defaultGreen, final double defaultBlue, boolean isQuiet)
    {
        boolean success = false;

        // get the model image for the image location
        // image B may be loaded from disk, or imported from another frame.
        try {

            // get rid of the previous imageB if one exists
            if (imageB != null) {
                imageB.disposeLocal();
                imageB = null;
            }

            if (obj instanceof File) {
                final FileIO fileIO = new FileIO();
                final File file = (File) obj;
                userInterface.setDefaultDirectory(file.getParent());
                userInterface.setLoad(true);
                fileIO.setQuiet(isQuiet);
                fileIO.setImageA(imageA);
                
                imageB = fileIO.readImage(file.getName(), file.getParent() + File.separator, stackFlag, imageA.getFileInfo(0), true); // read
                this.setLUTb(fileIO.getModelLUT());
                // image!
            } else if (obj instanceof ModelImage) {
                imageB = (ModelImage) obj;
            }

            if (imageB == null) {
                return false;
            }

            if (imageB.isColorImage() && (getRGBTB() == null)) {
                final int[] RGBExtents = new int[2];
                RGBExtents[0] = 4;
                RGBExtents[1] = 256;

                final ModelRGB rgb = new ModelRGB(RGBExtents);

                this.setRGBTB(rgb);
            }
            else if ( !imageB.isColorImage() && (getLUTb() == null)) {
            	this.setLUTb(new ModelLUT() );
            }

            if ( (imageA.getNDims() == imageB.getNDims()) || ( (imageA.getNDims() == 4) && (imageB.getNDims() == 3))) {
                matchImages(imageA, imageB, doOrigins, doOrients, defaultValue, defaultRed, defaultGreen, defaultBlue);
            } else {
                MipavUtil
                        .displayError("Image loading failed because the image dimensions do not match. Images must both be 2D, 3D or 4D. Or ImageA must be 4D and imageB 3D.");

                return false;
            }

            if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

                // load any luts
                displayMode = ViewJFrameBase.IMAGE_B; // tell loadLUT to load LUT for image B
                this.loadLUT(true, true);

                // load any vois
                this.loadAllVOIs(true);
                displayMode = ViewJFrameBase.IMAGE_A; // Reset to default.
            }

            success = true;
        } catch (final Throwable t) {
            t.printStackTrace();
            success = false;
        } finally {

            if ( !success && (imageB != null)) {
                imageB.disposeLocal();
                imageB = null;
            }
        }

        return success;
    }

    private void matchImages(ModelImage imageA, ModelImage imageB, final boolean doOrigins, final boolean doOrients,
            final double defaultValue, final double defaultRed, final double defaultGreen, final double defaultBlue) {
        final ModelImage imageA_back = imageA;
        ModelImage imageB_back = imageB;

        final AlgorithmMatchImages algoMatch = new AlgorithmMatchImages(imageA, imageB, doOrigins, doOrients);
        if ( !imageB.isColorImage()) {
            algoMatch.setPadValue((float) defaultValue);
        } else {
            algoMatch.setPadValue((float) defaultRed, (float) defaultGreen, (float) defaultBlue);
        }
        algoMatch.setRunningInSeparateThread(false);
        algoMatch.run();
        imageA = algoMatch.getImageA();
        imageB = algoMatch.getImageB();
        if (imageB_back != imageB) {
            imageB_back.disposeLocal();
            imageB_back = null;
        }

        if (imageA != imageA_back) {
            // Create new frame with imageA
            // Put commands for newFrame here since commands in ViewJFrameImage under loadImage work on the
            // original frame and not the newFrame.

            final ViewJFrameImage newFrame = new ViewJFrameImage(imageA, null, null, false);
            newFrame.setImageB(imageB);
            newFrame.enableImageB(true);
            newFrameCreated = true;
            ViewControlsImage controls = new ViewControlsImage(newFrame); // Build controls used in this frame
            ViewMenuBuilder menuBuilder = new ViewMenuBuilder(newFrame);
            ViewJComponentEditImage componentImage = newFrame.getComponentImage();
            componentImage.setTimeSlice(0);
            componentImage.setSlice( (imageB.getExtents()[2] - 1) / 2);
            ButtonGroup VOIGroup = new ButtonGroup();
            VOIManagerInterface voiManager = new VOIManagerInterface(newFrame, imageA, imageB, 1, false, VOIGroup);
            voiManager.getVOIManager(0).init(newFrame, imageA, imageB, componentImage, componentImage,
                    componentImage.getOrientation());
            voiManager.getToolBar().setVisible(true);
            componentImage.setVOIManager(voiManager.getVOIManager(0));
            boolean showImage = true;
            boolean showPaint = true;
            boolean showScript = true;
            controls.buildToolbar(showImage, voiManager.getToolBar(), VOIGroup, voiManager.getPointerButton(),
                    showPaint, showScript);
            menuBuilder.setMenuItemEnabled("Close image(B)", true);
            menuBuilder.setMenuItemEnabled("Extract image(B)", true);
            newFrame.updateImages(true);
            newFrame.setActiveImage(ViewJFrameBase.IMAGE_B); // set image B to active by default, for convenience of user
            newFrame.controls.setActiveImage(ViewJFrameBase.IMAGE_B); // set the controls to show that image B is active
            newFrame.setControls();
            newFrame.setTitle();
        } else {
            // imgA is not new, so keep the same ViewJFrameImage, which is imgA's frame
            // because image A was not changed, we will just set either the untouched
            // or transformed image B and return
            setImageB(imageB);
        }
    }

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     * 
     * @param loadAll boolean indicating that both lut and transfer functions should be loaded. If false, then only
     *            transfer functions are loaded.
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadLUT(final boolean loadAll, final boolean quietMode) {

        ModelImage img;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();
        } else {
            img = this.getImageB();
        }

        // build filename for lut
        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        String imageName = null;

        if (img.isDicomImage()) {
            imageName = img.getFileInfo(0).getFileName();

            final int index = imageName.lastIndexOf(".");

            if (index > 0) {
                imageName = imageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = imageName.length();

            for (int i = imageName.length() - 1; i >= 0; i--) {
                final char myChar = imageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                imageName = new String("DICOM");
            } else {
                imageName = imageName.substring(0, newIndex);
            }
        } else {
            imageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = imageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        final String filename = new String(imageName + ".lut");
        final String dirName = img.getFileInfo(0).getFileDirectory();

        loadLUTandTransferFunctionFrom(loadAll, filename, dirName, quietMode);

    } // end loadLUT()

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     * 
     * @param loadAll boolean indicating that both lut and transfer functions should be loaded. If false, then only
     *            transfer functions are loaded.
     * @param filename filename to save LUT as
     * @param dirName directory to save LUT to
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    public void loadLUTandTransferFunctionFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTA();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTa();
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTB();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTb();
            }
        }

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if ( !useLUT) {
            loadAll = false;
        }

        if ( (filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            final JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (loadAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    this.setLUTa(lut);
                } else {
                    this.setLUTb(lut);
                }
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    this.setRGBTA(rgb);
                } else {
                    this.setRGBTB(rgb);
                }
            }

            img.notifyImageDisplayListeners(lut, true);

        } catch (final IOException error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    

    /**
     * Reads the input LUT (ModelLUT or ModelRGB) from file.
     * @param image
     * @param lut
     * @param loadAll
     * @param filename
     * @param dirName
     * @param quietMode
     */
    public static void loadLUTandTransferFunctionFrom(ModelImage image, ModelStorageBase lut, boolean loadAll, String filename, String dirName, boolean quietMode) {

        boolean useLUT = !image.isColorImage();

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if ( !useLUT )
        {
            loadAll = false;
        }

        if ( (filename == null) || (dirName == null)) {
            dirName = image.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            final JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (loadAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            final int returnVal = chooser.showOpenDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);
                return;
            }
        }

        try {

            if (useLUT) {
            	FileHistoLUT fileHistoLUT = new FileHistoLUT(filename, dirName, image, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }
            } else {
            	FileHistoLUT fileHistoLUT = new FileHistoLUT(filename, dirName, image, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }
            }

        } catch (final IOException error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    /**
     * This method opens an existing VOI.
     * 
     * @param quietMode if true indicates that warnings should not be displayed.
     * @param doLabels DOCUMENT ME!
     * 
     * @return whether a VOI was successfully opened (ie - the dialog wasn't cancelled)
     */
    public boolean openVOI(boolean quietMode, final boolean doLabels) {
        ViewOpenVOIUI openVOI = null;

        try {
            openVOI = new ViewOpenVOIUI();

            if (displayMode == ViewJFrameBase.IMAGE_A) {

                if (openVOI.open(imageA, doLabels) == null) {
                    return false;
                }
            } else if (displayMode == ViewJFrameBase.IMAGE_B) {

                if (openVOI.open(imageB, doLabels) == null) {
                    return false;
                }
            } else {

                if ( !quietMode) {
                    MipavUtil.displayError(" Cannot open VOI when viewing both images");
                }

                return false;
            }
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.openVOI");
            }

            return false;
        }

        return true;
    }

    /**
     * This method opens an existing VOI.
     * 
     * @param image image where VOI(s) are to registered
     * @param quietMode if true indicates that warnings should not be displayed.
     * 
     * @return the VOI(s)
     */
    public VOI[] openVOI(final ModelImage image, boolean quietMode) {
        ViewOpenVOIUI openVOI = null;
        VOI[] voi = null;

        try {
            openVOI = new ViewOpenVOIUI();
            voi = openVOI.open(image, false);
        } catch (final OutOfMemoryError error) {

            if ( !quietMode) {
                MipavUtil.displayError("Out of memory: ViewJFrameBase.openVOI");
            }

            return voi;
        }

        return voi;
    }

    /**
     * Brings up a FileChooser to pick a file.
     * 
     * @return File the File the chooser picks.
     */
    public File pickImageFile() {

        File f = null;

        // set the filter type to the preferences saved filter
        int filter = 0;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't use it!
            filter = -1;
        }

        try {
            final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(userInterface.getLastStackFlag());

            final JFileChooser chooser = fileChooser.getFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            // make sure that this filter has been added to chooser
            if (filter != -1) {

                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                final javax.swing.filechooser.FileFilter found = findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                userInterface.setLastStackFlag(fileChooser.isMulti());
                f = chooser.getSelectedFile();
            } else {
                return null;
            }

        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameBase.loadImage");

            return null;
        }

        userInterface.setDefaultDirectory(f.getParent());

        return f;
    }

    /**
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     * 
     * @param options File-write options.
     * @param filterType only used if >= 0
     */
    public void save(final FileWriteOptions options, final int filterType) {
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ModelImage img = null;
        ViewImageFileFilter vFilter = null;
        int i;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = imageA;
        } else if (displayMode == ViewJFrameBase.IMAGE_B) {
            img = imageB;
        } else {
            MipavUtil.displayError(" Cannot save images when viewing both images.");

            return;
        }

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, true);

                    try {

                        // try to prefill the "save as" text area
                        if (img.getFileInfo(0).getFileDirectory() != null) {
                            fileChooser.getFileChooser().setSelectedFile(
                                    new File(img.getFileInfo(0).getFileDirectory() + img.getImageFileName()));
                        } else {
                            fileChooser.getFileChooser().setSelectedFile(new File(img.getImageFileName()));
                        }
                    } catch (final Throwable t) {
                        // if prefill fails, do nothing
                    }

                    final JFileChooser chooser = fileChooser.getFileChooser();

                    // chooser.setName("Save image as");
                    if (userInterface.getDefaultDirectory() != null) {
                        chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                    }

                    if (filterType >= 0) {
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                    } else {
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                    }

                    final int returnVal = chooser.showSaveDialog(this);

                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        fileName = chooser.getSelectedFile().getName();

                        if (filterType >= 0) {
                            i = fileName.lastIndexOf('.');

                            if ( (i > 0) && (i < (fileName.length() - 1))) {
                                extension = fileName.substring(i + 1).toLowerCase();
                                vFilter = new ViewImageFileFilter(filterType);

                                if ( !vFilter.accept(extension)) {
                                    MipavUtil.displayError("Extension does not match filter type");

                                    return;
                                }
                            } // if ( i > 0 && i < fileName.length() - 1 )
                            else if (i < 0) {

                                switch (filterType) {

                                    case ViewImageFileFilter.AVI:
                                        fileName = fileName + ".avi";
                                        break;

                                    case ViewImageFileFilter.VOI:
                                        fileName = fileName + ".voi";
                                        break;

                                    case ViewImageFileFilter.FUNCT:
                                        fileName = fileName + ".fun";
                                        break;

                                    case ViewImageFileFilter.LUT:
                                        fileName = fileName + ".lut";
                                        break;

                                    case ViewImageFileFilter.PLOT:
                                        fileName = fileName + ".plt";
                                        break;

                                    case ViewImageFileFilter.CLASS:
                                        fileName = fileName + ".class";
                                        break;

                                    case ViewImageFileFilter.SCRIPT:
                                        fileName = fileName + ".sct";
                                        break;

                                    case ViewImageFileFilter.SURFACE:
                                        fileName = fileName + ".sur";
                                        break;

                                    case ViewImageFileFilter.FREESURFER:
                                        fileName = fileName + ".asc";
                                        break;
                                }
                            } // else if (i < 0)
                        } // if (filterType >= 0)

                        directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        userInterface.setDefaultDirectory(directory);
                    } else {
                        return;
                    }

                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return;
                }
            }

        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

        if ( !options.isScript() && Preferences.is(Preferences.PREF_SAVE_PROMPT_OVERWRITE)
                && new File(directory + File.separator + fileName).exists()) {
            final int response = JOptionPane.showConfirmDialog(this, directory + fileName + " exists.  Overwrite?",
                    "File exists", JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.NO_OPTION) {
                options.setSaveAs(true);
                userInterface.setDefaultDirectory(directory);
                this.save(options, filterType);

                return;
            }
        }

        /*
         * I'm not sure why this wasn't done before.... if we do a save-as we should also update the name of the file
         */
        // if (options.isSaveAs()) {
        // img.setImageName(fileName.substring(0, fileName.length()-4));
        // }
        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if ( !options.isSaveAs()) {

            if (img.getNDims() == 3) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
            } else if (img.getNDims() == 4) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
                options.setBeginTime(0);
                options.setEndTime(img.getExtents()[3] - 1);
            }
        }

        if ( (fileName != null)
                && ( (fileName.endsWith(".avi")) || (fileName.endsWith(".AVI")) || (fileName.endsWith(".mov")) || (fileName
                        .endsWith(".MOV")))) {

            ModelImage imageAvi = imageA;
            boolean converted = false;

            // must convert to ARGB if ARGB float or ARGB ushort
            if ( (imageA.getType() == ModelStorageBase.ARGB_FLOAT)
                    || (imageA.getType() == ModelStorageBase.ARGB_USHORT)) {
                int response = JOptionPane.NO_OPTION;

                if ( !options.isScript()) {
                    response = JOptionPane.showConfirmDialog(userInterface.getMainFrame(), new String(
                            "Image must be converted to ARGB to save as .avi"), "Convert?", JOptionPane.YES_NO_OPTION,
                            JOptionPane.QUESTION_MESSAGE);
                } else {
                    response = JOptionPane.YES_OPTION;
                }

                if (response == JOptionPane.NO_OPTION) {
                    return;
                }

                imageAvi = new ModelImage(ModelStorageBase.ARGB, imageA.getExtents(), fileName + "ARGB");

                AlgorithmChangeType algoChange = new AlgorithmChangeType(imageAvi, imageA, (float) imageA.getMin(),
                        (float) imageA.getMax(), 0, 255, false);

                algoChange.setRunningInSeparateThread(false);
                algoChange.run();

                algoChange.finalize();
                algoChange = null;
                converted = true;
            }

            try {
                suffix = new String(".avi");
                fileType = FileUtility.AVI;

                FileAvi aviFile;

                aviFile = new FileAvi(fileName, directory);

                if (fileName.endsWith(".mov") || fileName.endsWith(".MOV")) {
                    aviFile.setWriteQT(true);

                }

                aviFile.setIsScript(options.isScript());

                if ( !aviFile.writeImage(imageAvi, imageB, LUTa, LUTb, getRGBTA(), getRGBTB(), red, green, blue,
                        opacity, alphaBlend, paintBitmap, options.getAVICompression())) {

                    System.err.println("AVI image write cancelled");
                }

                if (converted && (imageAvi != null)) {
                    imageAvi.disposeLocal();
                }

                imageAvi = null;
            } catch (final IOException error) {

                if (converted && (imageAvi != null)) {
                    imageAvi.disposeLocal();
                }

                MipavUtil.displayError("ViewJFrameBase: " + error);

                return;
            } catch (final OutOfMemoryError error) {

                if (converted && (imageAvi != null)) {
                    imageAvi.disposeLocal();
                }

                MipavUtil.displayError("ViewJFrameBase: " + error);

                return;
            }
        } else {

            if (fileName != null) {
                final FileIO fileIO = new FileIO();

                if (displayMode == ViewJFrameBase.IMAGE_A) {
                    fileIO.setModelLUT(this.getLUTa());
                } else {
                    fileIO.setModelLUT(this.getLUTb());
                }

                if (img.isColorImage()) {

                    if (displayMode == ViewJFrameBase.IMAGE_A) {
                        options.setRGBTa(this.getRGBTA());
                    } else {
                        options.setRGBTa(this.getRGBTB());
                    }
                }

                fileIO.writeImage(img, options);
            }
        }

        // if the SaveAllOnSave preference flag is set, then
        // save all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            // Since the options may have changed the filename
            // and the directory --- get new fileName and directory
            // from options
            final String fName = options.getFileName(); // if you use the name from img, then DICOM has funny names
            final String dirName = img.getFileInfo(0).getFileDirectory();
            String filebase;
            final int ind = fName.lastIndexOf(".");

            if (ind > 0) {
                filebase = fName.substring(0, fName.lastIndexOf("."));
            } else {
                filebase = new String(fName);
            }

            if (options.getFileType() == FileUtility.DICOM) {
                int newIndex = filebase.length();

                for (i = filebase.length() - 1; i >= 0; i--) {
                    final char myChar = filebase.charAt(i);

                    if (Character.isDigit(myChar)) {
                        newIndex = i;
                    } else {
                        break;
                    } // as soon as something is NOT a digit, leave loop
                }

                if (newIndex > 0) {
                    filebase = filebase.substring(0, newIndex);
                }
            }

            // save any luts
            final String lutName = new String(filebase + ".lut");

            saveLUTAs(true, lutName, dirName);

            // save any vois
            final String voiName = filebase.replace('^', '_');
            final String voiDir = new String(dirName + File.separator + "defaultVOIs_" + voiName + File.separator);

            saveAllVOIsTo(voiDir);
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        final FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);

            if (suffix.equals("")) {
                fileName = options.getFileName();
                suffix = FileUtility.getExtension(fileName);
            }

            boolean zerofunused[] = new boolean[1];
            fileType = FileUtility.getFileType(fileName, directory, false, false, zerofunused);
        }

        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            final int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                final char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if ( !directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
            // fileInfo[i].setFileFormat (fileType);
        }

    }

    /**
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     * 
     * @param img DOCUMENT ME!
     * @param options DOCUMENT ME!
     * @param filterType only used if value >= 0
     */
    public void save(final ModelImage img, final FileWriteOptions options, final int filterType) {
        this.save(img, options, filterType, false);
    }

    /**
     * Creates save dialog so that the image can be saved // This should be moved to imageModel.save();
     * 
     * @param img DOCUMENT ME!
     * @param options DOCUMENT ME!
     * @param filterType only used if value >= 0
     * @param operateQuiet informs the FileIO to not notify user of errors or interrogate for more information.
     */
    public void save(final ModelImage img, final FileWriteOptions options, final int filterType, boolean operateQuiet) {
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ViewImageFileFilter vFilter = null;
        int i;

        // System.err.println( "Save image (base) saveAs: " + options.isSaveAs() + " is set: " + options.isSet());

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    final JFileChooser chooser = new JFileChooser();

                    if (userInterface.getDefaultDirectory() != null) {
                        chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                    }

                    if (filterType >= 0) {
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                    } else {
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                    }

                    final int returnVal = chooser.showSaveDialog(this);

                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        fileName = chooser.getSelectedFile().getName();

                        if (options.isAVI()) {

                            // force the name to be .avi
                            if ( !fileName.endsWith("avi") && !fileName.endsWith("AVI")) {
                                fileName += ".avi";
                            }
                        }

                        if (filterType >= 0) {
                            i = fileName.lastIndexOf('.');

                            if ( (i > 0) && (i < (fileName.length() - 1))) {
                                extension = fileName.substring(i + 1).toLowerCase();
                                vFilter = new ViewImageFileFilter(filterType);

                                if ( !vFilter.accept(extension)) {
                                    MipavUtil.displayError("Extension does not match filter type");

                                    return;
                                }
                            } // if ( i > 0 && i < fileName.length() - 1 )
                            else if (i < 0) {

                                switch (filterType) {

                                    case ViewImageFileFilter.AVI:
                                        fileName = fileName + ".avi";
                                        break;

                                    case ViewImageFileFilter.VOI:
                                        fileName = fileName + ".voi";
                                        break;

                                    case ViewImageFileFilter.FUNCT:
                                        fileName = fileName + ".fun";
                                        break;

                                    case ViewImageFileFilter.LUT:
                                        fileName = fileName + ".lut";
                                        break;

                                    case ViewImageFileFilter.PLOT:
                                        fileName = fileName + ".plt";
                                        break;

                                    case ViewImageFileFilter.CLASS:
                                        fileName = fileName + ".class";
                                        break;

                                    case ViewImageFileFilter.SCRIPT:
                                        fileName = fileName + ".sct";
                                        break;

                                    case ViewImageFileFilter.SURFACE:
                                        fileName = fileName + ".sur";
                                        break;

                                    case ViewImageFileFilter.FREESURFER:
                                        fileName = fileName + ".asc";
                                        break;
                                }
                            } // else if (i < 0)
                        } // if (filterType >= 0)

                        directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        userInterface.setDefaultDirectory(directory);
                    } else {
                        return;
                    }
                } catch (final OutOfMemoryError error) {

                    if ( !operateQuiet) {
                        MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    }

                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return;
                }
            }
        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if ( !options.isSaveAs()) {

            if (img.getNDims() == 3) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
            } else if (img.getNDims() == 4) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
                options.setBeginTime(0);
                options.setEndTime(img.getExtents()[3] - 1);
            }
        }

        if (fileName != null) {

            if ( ( (fileName.endsWith(".avi")) || (fileName.endsWith(".AVI")) || (fileName.endsWith(".mov")) || (fileName
                    .endsWith(".MOV")))) {
                // if ( (imageA.getType() != ModelStorageBase.ARGB) && (imageA.getType() != ModelStorageBase.UBYTE)) {
                // System.err.println("TYPE: " + imageA.getType()); MipavUtil.displayError("Must convert image to ARGB
                // or UBYTE for AVI"); return; }

                options.setPaintBitmap(paintBitmap);
                options.setRed(red);
                options.setGreen(green);
                options.setBlue(blue);
                options.setLUTa(getLUTa());
                options.setLUTb(getLUTb());
                options.setRGBTa(getRGBTA());
                options.setRGBTb(getRGBTB());
                options.setOpacity(opacity);
                options.setAlphaBlend(alphaBlend);
                options.setImageB(imageB);
            }

            final FileIO fileIO = new FileIO();

            fileIO.setQuiet(operateQuiet);

            if (displayMode == ViewJFrameBase.IMAGE_A) {
                fileIO.setModelLUT(this.getLUTa());
            } else {
                fileIO.setModelLUT(this.getLUTb());
            }

            fileIO.writeImage(img, options);
        }

        // if the SaveAllOnSave preference flag is set, then
        // save all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            // Since the options may have changed the filename
            // and the directory --- get new fileName and directory
            // from options
            final String fName = options.getFileName(); // if you use the name from img, then DICOM has funny names
            final String dirName = img.getFileInfo(0).getFileDirectory();
            String filebase;
            final int ind = fName.lastIndexOf(".");

            if (ind > 0) {
                filebase = fName.substring(0, fName.lastIndexOf("."));
            } else {
                filebase = new String(fName);
            }

            if (options.getFileType() == FileUtility.DICOM) {
                int newIndex = filebase.length();

                for (i = filebase.length() - 1; i >= 0; i--) {
                    final char myChar = filebase.charAt(i);

                    if (Character.isDigit(myChar)) {
                        newIndex = i;
                    } else {
                        break;
                    } // as soon as something is NOT a digit, leave loop
                }

                if (newIndex > 0) {
                    filebase = filebase.substring(0, newIndex);
                }
            }

            // save any luts
            final String lutName = new String(filebase + ".lut");

            saveLUTAs(true, lutName, dirName);

            // save any vois
            final String voiName = filebase.replace('^', '_');
            final String voiDir = new String(dirName + File.separator + "defaultVOIs_" + voiName + File.separator);

            saveAllVOIsTo(voiDir);
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        final FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);
            boolean zerofunused[] = new boolean[1];
            fileType = FileUtility.getFileType(fileName, directory, false, operateQuiet, zerofunused);
        }

        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            final int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                final char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if ( !directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
        }
    }

    /**
     * This method saves all VOIs for the active image to the default VOI directory for that image.
     */
    public void saveAllVOIs() {

        String fileDir;
        String tmpImageName;
        String imageName;
        String voiDir;
        ModelImage img;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();
        } else if (displayMode == ViewJFrameBase.IMAGE_B) {
            img = this.getImageB();
        } else {
            MipavUtil.displayError(" Cannot save VOIs when viewing both images");

            return;
        }

        fileDir = img.getFileInfo(0).getFileDirectory();

        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            tmpImageName = img.getFileInfo(0).getFileName();

            final int index = tmpImageName.lastIndexOf(".");

            if (index > 0) {
                tmpImageName = tmpImageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = tmpImageName.length();

            for (int i = tmpImageName.length() - 1; i >= 0; i--) {
                final char myChar = tmpImageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                tmpImageName = new String("DICOM");
            } else {
                tmpImageName = tmpImageName.substring(0, newIndex);
            }
        } else {
            tmpImageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = tmpImageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        voiDir = new String(fileDir + File.separator + "defaultVOIs_" + imageName + File.separator);

        saveAllVOIsTo(voiDir);

    } // end saveAllVOIs()

    /**
     * This method saves all VOIs for the active image to a given directory.
     * 
     * @param voiDir directory that contains VOIs for this image.
     */
    public void saveAllVOIsTo(final String voiDir) {

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        FileVOI fileVOI;
        ModelImage currentImage;

        try {

            if (displayMode == ViewJFrameBase.IMAGE_A) {
                currentImage = imageA;
                VOIs = imageA.getVOIs();
            } else if (displayMode == ViewJFrameBase.IMAGE_B) {
                currentImage = imageB;
                VOIs = imageB.getVOIs();
            } else {
                MipavUtil.displayError(" Cannot save VOIs when viewing both images");

                return;
            }

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            nVOI = VOIs.size();

            System.err.println("Number of VOIs: " + nVOI);

            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, currentImage);
                    fileVOI.writeVOI(VOIs.VOIAt(i), true);
                } else {
                    fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, currentImage);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()

    /**
     * DOCUMENT ME!
     * 
     * @param saveAll DOCUMENT ME!
     */
    public void saveLabels(final boolean saveAll) {
        String fileName;
        String directory;
        JFileChooser chooser;

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean foundLabel = false;

        if (displayMode == ViewJFrameBase.IMAGE_A) {

            VOIs = imageA.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if ( (VOIs.VOIAt(i).isActive() || saveAll) && (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION)) {
                    foundLabel = true;
                }
            }

            if ( !foundLabel) {
                MipavUtil.displayWarning("There are no labels on the image.");

                return;
            }

            chooser = new JFileChooser();
            chooser.setDialogTitle("Save label(s) as");

            if (userInterface.getDefaultDirectory() != null) {
                final File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".lbl"}));

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                if ( !fileName.endsWith(".lbl")) {
                    fileName += ".lbl";
                }
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);

                this.voiSavedFileName = directory + fileName;

            } else {
                return;
            }

            try {

                final FileVOI fileVOI = new FileVOI(fileName, directory, imageA);

                fileVOI.writeAnnotationXML(saveAll);

            } catch (final IOException error) {
                MipavUtil.displayError("Error writing labels");
            }

        } else if (displayMode == ViewJFrameBase.IMAGE_B) {

            VOIs = imageB.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if ( (VOIs.VOIAt(i).isActive() || saveAll) && (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION)) {
                    foundLabel = true;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI.");

                return;
            }

            if ( !foundLabel) {
                MipavUtil.displayWarning("There are no labels on the image.");

                return;
            }

            chooser = new JFileChooser();
            chooser.setDialogTitle("Save label(s) as");

            if (userInterface.getDefaultDirectory() != null) {
                final File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {"lbl"}));

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                if ( !fileName.endsWith(".lbl")) {
                    fileName += ".lbl";
                }
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return;
            }

            try {

                final FileVOI fileVOI = new FileVOI(fileName, directory, imageB);
                fileVOI.writeAnnotationXML(true);

            } catch (final IOException error) {
                MipavUtil.displayError("Error writing label(s)");
            }
        } else {
            MipavUtil.displayError(" Cannot save images when viewing both images.");
        }
    }

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     * 
     * @param saveAll - boolean indicating that both lut and transfer functions should be saved. If false, then only
     *            transfer functions are saved.
     */
    public void saveLUT(final boolean saveAll) {

        ModelImage img;
        String imageName;
        String tmpImageName;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();
        } else {
            img = this.getImageB();
        }

        // build filename for lut
        // if the image is a dicom image, then base the new directory name
        // on the actual filename, not the image name
        if (img.isDicomImage()) {
            tmpImageName = img.getFileInfo(0).getFileName();

            final int index = tmpImageName.lastIndexOf(".");

            if (index > 0) {
                tmpImageName = tmpImageName.substring(0, index);
            }

            // now, get rid of any numbers at the end of the name (these
            // are part of the dicom file name, but we only want the 'base'
            // part of the name
            int newIndex = tmpImageName.length();

            for (int i = tmpImageName.length() - 1; i >= 0; i--) {
                final char myChar = tmpImageName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex == 0) {

                // give the base name a generic name
                tmpImageName = new String("DICOM");
            } else {
                tmpImageName = tmpImageName.substring(0, newIndex);
            }
        } else {
            tmpImageName = img.getImageName();
        }

        // get rid of any '^' and ',' which may exist in dicom images
        imageName = tmpImageName.replace('^', '_');
        imageName = imageName.replace(',', '_');

        final String filename = new String(imageName + ".lut");
        final String dirName = img.getFileInfo(0).getFileDirectory();

        saveLUTAs(saveAll, filename, dirName);

    } // end saveLUT()

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     * 
     * @param saveAll boolean indicating that both lut and transfer functions should be saved. If false, then only
     *            transfer functions are saved.
     * 
     * <p>
     * If either filename or directory is null, then the user will be prompted for a filename.
     * </p>
     * @param filename filename to save LUT as
     * @param dirName directory to save LUT to
     */
    public void saveLUTAs(boolean saveAll, String filename, String dirName) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTA();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTa();
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTB();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTb();
            }
        }

        // if not using a lut (i.e. rgb only), then you
        // can't saveAll.... there are only functions, so
        // reset the saveAll variable
        if ( !useLUT) {
            saveAll = false;
        }

        // if filename and/or dirName is null, then get it from user
        if ( (filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            final JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (saveAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }

        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            if (saveAll) {
                fileHistoLUT.writeAll();
            } else {
                fileHistoLUT.writeFunctions();
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()


    /**
     * Saves the LUT associated with the input image (either a ModelLUT or ModelRGB).
     * @param image
     * @param lut
     * @param saveAll
     */
    public static void saveLUTAs(ModelImage image, ModelStorageBase lut, boolean saveAll)
    {
    	// if filename and/or dirName is null, then get it from user
    	String dirName = image.getFileInfo(0).getFileDirectory();

    	if (dirName == null) {
    		dirName = System.getProperties().getProperty("user.dir");
    	}

    	final JFileChooser chooser = new JFileChooser();
    	chooser.setCurrentDirectory(new File(dirName));
        if (saveAll) {
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
        } else {
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
        }

    	final int returnVal = chooser.showSaveDialog(null);
    	String filename = null;
    	if (returnVal == JFileChooser.APPROVE_OPTION) {
    		filename = chooser.getSelectedFile().getName();
    		dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
    	} else if (returnVal == JFileChooser.CANCEL_OPTION) {
    		chooser.setVisible(false);
    		return;
    	}

        try {

        	FileHistoLUT fileHistoLUT = new FileHistoLUT(filename, dirName, image, lut);
        	fileHistoLUT.writeAll();

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }
    }
    
    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     * 
     * @param filename filename to save LUT as
     * @param dirName directory to save LUT to
     */
    public void saveLUTandTransferFunction(final String filename, final String dirName) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTA();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTa();
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = this.getRGBTB();
                lut = null;
            } else {
                useLUT = true;
                rgb = null;
                lut = this.getLUTb();
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            fileHistoLUT.writeLUTandTransferFunction();
            Preferences.setDefaultDisplay(DefaultDisplay.LUT);
            
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()
    

    /**
     * Saves the ModelLUT and transfer function.
     * @param image
     * @param lut
     * @param filename
     * @param dirName
     */
    public static void saveLUTandTransferFunction(ModelImage image, ModelLUT lut, final String filename, final String dirName)
    {
    	FileHistoLUT fileHistoLUT;
    	try {
    		fileHistoLUT = new FileHistoLUT(filename, dirName, image, lut);
    		fileHistoLUT.writeLUTandTransferFunction();
    		Preferences.setDefaultDisplay(DefaultDisplay.LUT);

    	} catch (final IOException error) {
    		MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
    	}

    }

    /**
     * This method saves a selected VOI - should this be in VOI structure ??!!!
     * 
     * @param saveAllContours if true all contours are saved
     */
    public void saveVOI(final boolean saveAllContours) {

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        FileVOI fileVOI;
        String extension = ".xml";

        try {

            if (displayMode == ViewJFrameBase.IMAGE_A) {
                VOIs = imageA.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive()) {
                        break;
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select a VOI.");

                    return;
                }

                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    if (VOIs.VOIAt(i).getExtension().equals(".voi")) {
                        extension = ".voi";
                    }
                    fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + extension,
                            imageA.getFileInfo(0).getFileDirectory(), imageA);
                    fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
                } else {
                    fileVOI = new FileVOI(VOIs.VOIAt(i) + ".lbl", imageA.getFileInfo(0).getFileDirectory(), imageA);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
                }
            } else if (displayMode == ViewJFrameBase.IMAGE_B) {
                VOIs = imageB.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive()) {
                        break;
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select a VOI.");

                    return;
                }

                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                    fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", imageA.getFileInfo(0).getFileDirectory(),
                            imageB);
                    fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
                } else {
                    fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", imageA.getFileInfo(0).getFileDirectory(),
                            imageB);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
                }
            } else {
                MipavUtil.displayError(" Cannot open VOI when viewing both images");
            }
        } catch (final IOException error) {
            MipavUtil.displayError("Error writing VOI" + error);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public String saveVOIAs() {
        saveVOIAs(true);

        return this.voiSavedFileName;
    }

    /**
     * This method allows the user to choose how to save the VOI.
     * 
     * @param saveAllContours if true all contours are saved
     */
    public void saveVOIAs(final boolean saveAllContours) {
        String fileName;
        String directory;
        JFileChooser chooser;

        int nVOI;
        int i;
        ViewVOIVector VOIs;
        boolean doPoint = false, doAnnotation = false;

        if (displayMode == ViewJFrameBase.IMAGE_A) {

            VOIs = imageA.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    break;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI.");

                return;
            }

            chooser = new JFileChooser();
            chooser.setDialogTitle("Save VOI as");

            if (userInterface.getDefaultDirectory() != null) {
                final File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".xml"}));

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                if ( !fileName.endsWith(".xml")) {
                    MipavUtil.displayError("VOI files must end in .xml");
                    return;
                }
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);

                this.voiSavedFileName = directory + fileName;

            } else {
                return;
            }

            try {

                if (fileName.endsWith(".voi") && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                    doPoint = true;
                } else if (fileName.endsWith(".lbl") || VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION) {
                    doAnnotation = true;
                    if (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION && !fileName.endsWith(".lbl")) {
                        fileName += ".lbl";
                    }
                } else if ( !fileName.endsWith(".voi") && !fileName.endsWith(".xml")) {
                    fileName += ".xml";
                }

                final FileVOI fileVOI = new FileVOI(fileName, directory, imageA);

                if ( !doPoint && !doAnnotation) {

                    // use the MIPAV VOI format (not Nauges) since we
                    // need to save the curveType in order to correctly
                    // rebuild the VOIs when reading the VOI files.
                    fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
                } else if (doPoint) {
                    fileVOI.writePointVOI(VOIs.VOIAt(i));
                } else if (doAnnotation) {
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
                }
            } catch (final IOException error) {
                MipavUtil.displayError("Error writing VOI");
            }

        } else if (displayMode == ViewJFrameBase.IMAGE_B) {

            VOIs = imageB.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    break;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI.");

                return;
            }

            chooser = new JFileChooser();
            chooser.setDialogTitle("Save VOI as");

            if (userInterface.getDefaultDirectory() != null) {
                final File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".xml", ".voi"}));

            final int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return;
            }

            try {

                if (fileName.endsWith(".voi") && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                    doPoint = true;
                } else if (fileName.endsWith(".lbl") || VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION) {
                    doAnnotation = true;
                    if (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION && !fileName.endsWith(".lbl")) {
                        fileName += ".lbl";
                    }
                } else if ( !fileName.endsWith(".voi") && !fileName.endsWith(".xml")) {
                    fileName += ".xml";
                }

                final FileVOI fileVOI = new FileVOI(fileName, directory, imageB);

                if ( !doPoint && !doAnnotation) {

                    // use the MIPAV VOI format (not Nauges) since we
                    // need to save the curveType in order to correctly
                    // rebuild the VOIs when reading the VOI files.
                    fileVOI.writeVOI(VOIs.VOIAt(i), saveAllContours);
                } else if (doPoint) {
                    fileVOI.writePointVOI(VOIs.VOIAt(i));
                } else if (doAnnotation) {
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), saveAllContours);
                }
            } catch (final IOException error) {
                MipavUtil.displayError("Error writing VOI");
            }
        } else {
            MipavUtil.displayError(" Cannot save images when viewing both images.");
        }
    }

    /**
     * Save intensities in VOI to a text file of format x,y,z,intensity on each line if not color or complex. If color
     * use format x,y,z,a,r,g,b on each line and if complex use format x,y,z,real,imaginary on each line.
     * 
     */
    public void saveVOIIntensities() {
        String fileName;
        String directory;
        JFileChooser chooser;
        File textFile;
        RandomAccessFile raFile;
        ModelImage selectedImage = null;
        int imageSize;
        int nDims;
        BitSet mask;
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;

        int nVOI;
        int i, j, k;
        ViewVOIVector VOIs = null;
        int x;
        int y;
        int z;
        double buffer[];
        byte entryBytes[];

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            selectedImage = imageA;
        } else if (displayMode == ViewJFrameBase.IMAGE_B) {
            selectedImage = imageB;
        } else {
            MipavUtil.displayError("Cannot save VOI intensities when viewing both images");
            return;
        }
        VOIs = selectedImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }

        nDims = selectedImage.getNDims();
        xDim = selectedImage.getExtents()[0];
        yDim = selectedImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageSize = sliceSize;
        if (nDims > 2) {
            zDim = selectedImage.getExtents()[2];
            imageSize *= zDim;
        } else {
            zDim = 1;
        }
        mask = new BitSet(imageSize);
        VOIs.VOIAt(i).createBinaryMask3D(mask, xDim, yDim, false, false);
        if (selectedImage.isColorImage()) {
            buffer = new double[4 * imageSize];
        } else if (selectedImage.isComplexImage()) {
            buffer = new double[2 * imageSize];
        } else {
            buffer = new double[imageSize];
        }

        try {
            selectedImage.exportData(0, buffer.length, buffer);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on selectedImage.exportData");
            return;
        }
        
        if (ScriptRunner.getReference().isRunning()) {
        	if (userInterface.getDefaultDirectory() != null) {
                final File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    directory = userInterface.getDefaultDirectory();
                } else {
                    directory = System.getProperty("user.dir");
                }
            } else {
            	directory = System.getProperty("user.dir");    
            }
        	fileName = selectedImage.getImageName() + "_" + VOIs.VOIAt(i).getName() + "_intensities.txt";
        	userInterface.setDefaultDirectory(directory);
        	
            this.voiSavedFileName = directory + fileName;
        }
        else {

	        chooser = new JFileChooser();
	        chooser.setDialogTitle("Save intensities in VOI as");
	
	        if (userInterface.getDefaultDirectory() != null) {
	            final File file = new File(userInterface.getDefaultDirectory());
	
	            if (file != null) {
	                chooser.setCurrentDirectory(file);
	            } else {
	                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	            }
	        } else {
	            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
	        }
	
	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".txt"}));
	
	        final int returnVal = chooser.showSaveDialog(this);
	
	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	            fileName = chooser.getSelectedFile().getName();
	            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	            userInterface.setDefaultDirectory(directory);
	
	            this.voiSavedFileName = directory + fileName;
	
	        } else {
	            return;
	        }
        }

        if ( !fileName.endsWith(".txt")) {
            fileName += ".txt";
        }

        textFile = new File(directory + fileName);
        try {
            raFile = new RandomAccessFile(textFile, "rw");
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile = new RandomAccessFile");
            return;
        }
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            raFile.setLength(0);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.setLength(0)");
            return;
        }

        if (selectedImage.isColorImage()) {
            entryBytes = new String("x,y,z,a,red,green,blue\n").getBytes();
        } else if (selectedImage.isComplexImage()) {
            entryBytes = new String("x,y,z,real,imaginary\n").getBytes();
        } else {
            entryBytes = new String("x,y,z,intensity\n").getBytes();
        }
        try {
            raFile.write(entryBytes);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.write(entryBytes) for header line");
            return;
        }

        for (z = 0; z < zDim; z++) {
            k = z * sliceSize;
            for (y = 0; y < yDim; y++) {
                j = k + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = j + x;
                    if (mask.get(i)) {
                        if (selectedImage.isColorImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[4 * i]) + ","
                                    + Double.toString(buffer[4 * i + 1]) + "," + Double.toString(buffer[4 * i + 2])
                                    + "," + Double.toString(buffer[4 * i + 3]) + "\n").getBytes();
                        } else if (selectedImage.isComplexImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[2 * i]) + ","
                                    + Double.toString(buffer[2 * i + 1]) + "\n").getBytes();
                        } else {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[i]) + "\n").getBytes();
                        }
                        try {
                            raFile.write(entryBytes);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on raFile.write(entryBytes");
                            return;
                        }
                    }
                }
            }
        }
        try {
            raFile.close();
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.close()");
        }

    }
    
    /**
     * Save intensities in VOI to a text file of format x,y,z,intensity on each line if not color or complex. If color
     * use format x,y,z,a,r,g,b on each line and if complex use format x,y,z,real,imaginary on each line.
     * 
     */
    public void saveVOIIntensitiesTo(final String voiIntensitiesDir) {
        String fileName;
        String directory;
        JFileChooser chooser;
        File textFile;
        RandomAccessFile raFile;
        ModelImage selectedImage = null;
        int imageSize;
        int nDims;
        BitSet mask;
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;

        int nVOI;
        int i, j, k;
        ViewVOIVector VOIs = null;
        int x;
        int y;
        int z;
        double buffer[];
        byte entryBytes[];

        if (displayMode == ViewJFrameBase.IMAGE_A) {
            selectedImage = imageA;
        } else if (displayMode == ViewJFrameBase.IMAGE_B) {
            selectedImage = imageB;
        } else {
            MipavUtil.displayError("Cannot save VOI intensities when viewing both images");
            return;
        }
        VOIs = selectedImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }

        nDims = selectedImage.getNDims();
        xDim = selectedImage.getExtents()[0];
        yDim = selectedImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imageSize = sliceSize;
        if (nDims > 2) {
            zDim = selectedImage.getExtents()[2];
            imageSize *= zDim;
        } else {
            zDim = 1;
        }
        mask = new BitSet(imageSize);
        VOIs.VOIAt(i).createBinaryMask3D(mask, xDim, yDim, false, false);
        if (selectedImage.isColorImage()) {
            buffer = new double[4 * imageSize];
        } else if (selectedImage.isComplexImage()) {
            buffer = new double[2 * imageSize];
        } else {
            buffer = new double[imageSize];
        }

        try {
            selectedImage.exportData(0, buffer.length, buffer);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on selectedImage.exportData");
            return;
        }
        
        final File voiFileIntensitiesDir = new File(voiIntensitiesDir);
        
        if (voiFileIntensitiesDir.exists() && voiFileIntensitiesDir.isDirectory()) { // do nothing
        	
        } else if (voiFileIntensitiesDir.exists() && !voiFileIntensitiesDir.isDirectory()) {
        	// voiFileIntensitiesDir.delete();
        } else {
        	voiFileIntensitiesDir.mkdir();
        }
        
        if (ScriptRunner.getReference().isRunning()) {
        	
        	fileName = selectedImage.getImageName() + "_" + VOIs.VOIAt(i).getName() + "_intensities.txt";
        	directory = String.valueOf(voiIntensitiesDir);
        	userInterface.setDefaultDirectory(voiIntensitiesDir);
        	
            this.voiSavedFileName = voiIntensitiesDir + fileName;
        }
        else {

	        chooser = new JFileChooser();
	        chooser.setDialogTitle("Save intensities in VOI as");
	        chooser.setCurrentDirectory(voiFileIntensitiesDir);
	
	        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {".txt"}));
	
	        final int returnVal = chooser.showSaveDialog(this);
	
	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	            fileName = chooser.getSelectedFile().getName();
	            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	            userInterface.setDefaultDirectory(directory);
	
	            this.voiSavedFileName = directory + fileName;
	
	        } else {
	            return;
	        }
        }

        if ( !fileName.endsWith(".txt")) {
            fileName += ".txt";
        }

        textFile = new File(directory + fileName);
        try {
            raFile = new RandomAccessFile(textFile, "rw");
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile = new RandomAccessFile");
            return;
        }
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        try {
            raFile.setLength(0);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.setLength(0)");
            return;
        }

        if (selectedImage.isColorImage()) {
            entryBytes = new String("x,y,z,a,red,green,blue\n").getBytes();
        } else if (selectedImage.isComplexImage()) {
            entryBytes = new String("x,y,z,real,imaginary\n").getBytes();
        } else {
            entryBytes = new String("x,y,z,intensity\n").getBytes();
        }
        try {
            raFile.write(entryBytes);
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.write(entryBytes) for header line");
            return;
        }

        for (z = 0; z < zDim; z++) {
            k = z * sliceSize;
            for (y = 0; y < yDim; y++) {
                j = k + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = j + x;
                    if (mask.get(i)) {
                        if (selectedImage.isColorImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[4 * i]) + ","
                                    + Double.toString(buffer[4 * i + 1]) + "," + Double.toString(buffer[4 * i + 2])
                                    + "," + Double.toString(buffer[4 * i + 3]) + "\n").getBytes();
                        } else if (selectedImage.isComplexImage()) {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[2 * i]) + ","
                                    + Double.toString(buffer[2 * i + 1]) + "\n").getBytes();
                        } else {
                            entryBytes = new String(Integer.toString(x) + "," + Integer.toString(y) + ","
                                    + Integer.toString(z) + "," + Double.toString(buffer[i]) + "\n").getBytes();
                        }
                        try {
                            raFile.write(entryBytes);
                        } catch (final IOException e) {
                            MipavUtil.displayError("IOException on raFile.write(entryBytes");
                            return;
                        }
                    }
                }
            }
        }
        try {
            raFile.close();
        } catch (final IOException e) {
            MipavUtil.displayError("IOException on raFile.close()");
        }

    }

    /**
     * Sets whether the close image B option should appear after loading.
     * 
     * @param enable whether the &quot;Close image B&quot; option should appear after image B is loaded
     */
    public void setEnableCloseImageBAfterLoad(final boolean enable) {
        this.enableCloseImageB = enable;
    }

    /**
     * Accessor that sets the imageA.
     * 
     * @param image The image to make image-A in this frame.
     */
    public void setImageA(final ModelImage image) {
        imageA = image;
    }

    /**
     * setLastState.
     * 
     * @param state Should be either Frame.NORMAL or Frame.ICONIFIED
     */
    public void setLastState(final int state) {
        lastState = state;
    }

    /**
     * Accessor that sets the link Frame.
     * 
     * @param linkFrame set frame to link to.
     */
    public void setLinkedFrame(final ViewJFrameImage linkFrame) {
        this.linkFrame = linkFrame;
    }

    /**
     * Accessor that sets the link Frame.
     * 
     * @param linkTriFrame set frame to link to.
     */
    public void setLinkedTriFrame(final ViewJFrameTriImage linkTriFrame) {
        this.linkTriFrame = linkTriFrame;
    }

    /**
     * Accessor that sets LUTa.
     * 
     * @param lut DOCUMENT ME!
     */
    public void setLUTa(final ModelLUT lut) {
        LUTa = lut;
        if(defaultLUTa == null) {
        	defaultLUTa = lut;
        }
    }

    /**
     * Accessor that sets LUTb.
     * 
     * @param lut DOCUMENT ME!
     */
    public void setLUTb(final ModelLUT lut) {
        LUTb = lut;
        if(defaultLUTb == null) {
        	defaultLUTb = lut;
        }
    }

    /**
     * Sets LUTa and LUTb.
     * 
     * @param _LUTa DOCUMENT ME!
     * @param _LUTb DOCUMENT ME!
     */
    public void setLUTs(final ModelLUT _LUTa, final ModelLUT _LUTb) {
        LUTa = _LUTa;
        LUTb = _LUTb;
        if(defaultLUTa == null) {
        	defaultLUTa = _LUTa;
        }
        if(defaultLUTb == null) {
        	defaultLUTb = _LUTb;
        }
    }

    /**
     * Accessor to set the text of the main frame's text field.
     * 
     * @param str string to be display in the main frame's text field
     */
    public final void setMessageText(final String str) {
        userInterface.setMessageText(str);
    }

    /**
     * Sets RGB Information.
     * 
     * @param _OPACITY DOCUMENT ME!
     * @param _alphaBlend DOCUMENT ME!
     */
    public void setOpacityInfo(final float _OPACITY, final float _alphaBlend) {

        try {
            red = getControls().getTools().getPaintColor().getRed();
            green = getControls().getTools().getPaintColor().getGreen();
            blue = getControls().getTools().getPaintColor().getBlue();
        } catch (final Exception e) {
            e.printStackTrace();
        }

        opacity = _OPACITY;
        alphaBlend = _alphaBlend;
    }

    /**
     * Sets the paint Bitmap.
     * 
     * @param _paintBitmap DOCUMENT ME!
     */
    public void setpaintBitmap(final BitSet _paintBitmap) {
        paintBitmap = _paintBitmap;
    }

    /**
     * Does nothing.
     * 
     * @param event the change event
     */
    public void stateChanged(final ChangeEvent event) {}

    /**
     * Tells the UI that this frame is the currently active one.
     * 
     * @param event the window event
     */
    public void windowActivated(final WindowEvent event) {

        // userInterface.getMainFrame().toFront();
        // userInterface.getMessageFrame().toFront();
        userInterface.setActiveFrame(this);
    }

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowClosed(final WindowEvent event) {}

    /**
     * Cleans up the frame before closing.
     * 
     * @param event the window event that triggered this method
     */
    public void windowClosing(final WindowEvent event) {
        close();
    }

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowDeactivated(final WindowEvent event) {}

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowDeiconified(final WindowEvent event) {}

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowIconified(final WindowEvent event) {}

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Does nothing.
     * 
     * @param event the window event
     */
    public void windowOpened(final WindowEvent event) {}

    /**
     * Cleans memory.
     * 
     * @throws Throwable the <code>Exception</code> raised by this method
     */
    protected void finalize() throws Throwable {

        if ( (imageA != null) && (imageA.getImageFrameVector() != null)) {

            if (imageA.getImageFrameVector().isEmpty()) {
                imageA.disposeLocal();
                imageA = null;
                linkFrame = null;
                linkTriFrame = null;
            }
        }

        if ( (imageB != null) && (imageB.getImageFrameVector() != null)) {

            if (imageB.getImageFrameVector().isEmpty()) {
                imageB.disposeLocal();
                imageB = null;
                linkFrame = null;
                linkTriFrame = null;
            }
        }

        super.finalize();
    }

    /**
     * Makes a string of a float with a specific number of decimal points.
     * 
     * @param number number to be converted to a string
     * @param decPts the number of decimal points
     * 
     * @return string representation of the number
     */
    protected String makeString(final float number, final int decPts) {
        String subStr = null;
        String str = null;

        try {
            subStr = new String();
            str = new String(String.valueOf(number));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("FrameBase.makeString: out of memory");

            return null;
        }

        final int index = str.indexOf(".");
        final int length = str.length();

        if ( (index + decPts) < length) {
            subStr = str.substring(0, index + decPts + 1);
        } else {
            subStr = str;
        }

        return subStr;
    }

    /**
     * Sets the Absolute position label.
     * 
     * @param position DOCUMENT ME!
     */
    protected void setAbsPositionLabels(final Vector3f position) {

        if (absoluteLabelVals == null) {
            return;
        }

        absoluteLabelVals[0].setText("X: " + (int) position.X);
        absoluteLabelVals[1].setText("Y: " + (int) position.Y);
        absoluteLabelVals[2].setText("Z: " + (int) position.Z);

        absoluteXTextField.setText(String.valueOf((int) position.X));
        absoluteYTextField.setText(String.valueOf((int) position.Y));
        absoluteZTextField.setText(String.valueOf((int) position.Z));

        final int[] dimExtents = imageA.getExtents();
        final int index = (int) ( (position.Z * dimExtents[0] * dimExtents[1]) + (position.Y * dimExtents[0]) + position.X);

        final int iBuffFactor = imageA.isColorImage() ? 4 : 1;
        if ( (index * iBuffFactor > imageA.getSize()) || (index < 0)) {
            return;
        }
        absoluteLabelVals[3].setText("Intensity: " + imageA.getFloat(index * iBuffFactor));
        absoluteLabelVals[4].setText("1D index: " + index);
    }

    /**
     * Sets the Scanner position label.
     * 
     * @param position DOCUMENT ME!
     */
    protected void setScannerPosition(final Vector3f position) {

        if (scannerLPSLabelVals == null) {
            return;
        }
        if (scannerRASLabelVals == null) {
            return;
        }

        final String[] LPS_labelContents = ViewJComponentEditImage.getScannerPositionLabels(imageA, position);
        final String[] RAS_labelContents = ViewJComponentEditImage.getRASScannerPositionLabels(imageA, position);
        final int[] dimExtents = imageA.getExtents();
        final int index = (int) ( (position.Z * dimExtents[0] * dimExtents[1]) + (position.Y * dimExtents[0]) + position.X);

        final int iBuffFactor = imageA.isColorImage() ? 4 : 1;
        if ( (index * iBuffFactor > imageA.getSize()) || (index < 0)) {
            return;
        }
        for (int i = 0; i < LPS_labelContents.length; i++) {
            scannerLPSLabelVals[i].setText(LPS_labelContents[i]);
        }
        for (int i = 0; i < RAS_labelContents.length; i++) {
            scannerRASLabelVals[i].setText(RAS_labelContents[i]);
        }
        scannerLPSLabelVals[3].setText("Intensity: " + imageA.getFloat(index * iBuffFactor));
        scannerRASLabelVals[3].setText("Intensity: " + imageA.getFloat(index * iBuffFactor));

        scannerLPS_RLTextField.setText(LPS_labelContents[0].substring(LPS_labelContents[0].lastIndexOf(":") + 1,
                LPS_labelContents[0].length()));
        scannerLPS_APTextField.setText(LPS_labelContents[1].substring(LPS_labelContents[1].lastIndexOf(":") + 1,
                LPS_labelContents[1].length()));
        scannerLPS_ISTextField.setText(LPS_labelContents[2].substring(LPS_labelContents[2].lastIndexOf(":") + 1,
                LPS_labelContents[2].length()));

        scannerRAS_RLTextField.setText(RAS_labelContents[0].substring(RAS_labelContents[0].lastIndexOf(":") + 1,
                RAS_labelContents[0].length()));
        scannerRAS_APTextField.setText(RAS_labelContents[1].substring(RAS_labelContents[1].lastIndexOf(":") + 1,
                RAS_labelContents[1].length()));
        scannerRAS_ISTextField.setText(RAS_labelContents[2].substring(RAS_labelContents[2].lastIndexOf(":") + 1,
                RAS_labelContents[2].length()));

    }

    /**
     * DOCUMENT ME!
     * 
     * @param component DOCUMENT ME!
     * @param event DOCUMENT ME!
     */
    protected void handleZoomPopupMenu(final Component component, final MouseEvent event) {
        final JPopupMenu popupMenu = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem("Use linear zoom increment");
        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_ZOOM_LINEAR, "true");
            }
        });

        boolean doLinear = Preferences.is(Preferences.PREF_ZOOM_LINEAR);

        if (doLinear) {
            menuItem.setFont(MipavUtil.font12B);
        } else {
            menuItem.setFont(MipavUtil.font12);
        }
        popupMenu.add(menuItem);

        menuItem = new JMenuItem("Use exponential zoom increment");

        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_ZOOM_LINEAR, "false");
            }
        });
        if ( !doLinear) {
            menuItem.setFont(MipavUtil.font12B);
        } else {
            menuItem.setFont(MipavUtil.font12);
        }
        popupMenu.add(menuItem);

        popupMenu.show(component, event.getX(), event.getY());
    }

    /**
     * Handles the popup menu for the Quick Mask buttons
     * 
     * @param component DOCUMENT ME!
     * @param event DOCUMENT ME!
     */
    public static void handleMaskPopupMenu(final Component component, final MouseEvent event) {
        final JPopupMenu popupMenu = new JPopupMenu();

        boolean currentState = Preferences.is(Preferences.PREF_QUICK_MASK_NEW);

        JMenuItem menuItem = new JMenuItem("Perform mask in place");

        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_QUICK_MASK_NEW, "false");
            }
        });

        if (currentState) {
            menuItem.setFont(MipavUtil.font12);
        } else {
            menuItem.setFont(MipavUtil.font12B);
        }

        popupMenu.add(menuItem);

        menuItem = new JMenuItem("Perform mask into new image");

        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_QUICK_MASK_NEW, "true");
            }
        });

        if ( !currentState) {
            menuItem.setFont(MipavUtil.font12);
        } else {
            menuItem.setFont(MipavUtil.font12B);
        }
        popupMenu.add(menuItem);

        popupMenu.show(component, event.getX(), event.getY());
    }

    /**
     * Handles the pop-up menu for Paint to Mask buttons (inside/outside)
     * 
     * @param component DOCUMENT ME!
     * @param event DOCUMENT ME!
     */
    protected void handlePaintToMaskPopupMenu(final Component component, final MouseEvent event) {
        final JPopupMenu popupMenu = new JPopupMenu();

        boolean currentState = Preferences.is(Preferences.PREF_PAINT_TO_MASK_NEW);

        JMenuItem menuItem = new JMenuItem("Perform paint to mask in place");
        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_PAINT_TO_MASK_NEW, "false");
            }
        });

        if (currentState) {
            menuItem.setFont(MipavUtil.font12);
        } else {
            menuItem.setFont(MipavUtil.font12B);
        }

        popupMenu.add(menuItem);

        menuItem = new JMenuItem("Perform paint to mask into new image");

        menuItem.addActionListener(new MipavUtil.ActionAdapter() {
            public void actionPerformed(final ActionEvent e) {
                Preferences.setProperty(Preferences.PREF_PAINT_TO_MASK_NEW, "true");
            }
        });
        if ( !currentState) {
            menuItem.setFont(MipavUtil.font12);
        } else {
            menuItem.setFont(MipavUtil.font12B);
        }
        popupMenu.add(menuItem);

        popupMenu.show(component, event.getX(), event.getY());
    }

    /**
     * Get the extents of an image. (don't know why this is really needed...)
     * 
     * @param img the image
     * 
     * @return the image's extents
     */
    protected static int[] initExtents(final ModelImage img) {
        int[] extents = null;

        if (img.getNDims() == 2) {
            extents = new int[2];
            extents[0] = Math.round(img.getExtents()[0]);
            extents[1] = Math.round(img.getExtents()[1]);
        } else if (img.getNDims() == 3) {
            extents = new int[3];
            extents[0] = Math.round(img.getExtents()[0]);
            extents[1] = Math.round(img.getExtents()[1]);
            extents[2] = Math.round(img.getExtents()[2]);
        } else if (img.getNDims() == 4) {
            extents = new int[4];
            extents[0] = Math.round(img.getExtents()[0]);
            extents[1] = Math.round(img.getExtents()[1]);
            extents[2] = Math.round(img.getExtents()[2]);
            extents[3] = Math.round(img.getExtents()[3]);
        }

        return extents;
    }

    /**
     * Create the intensity buffer for an image.
     * 
     * @param extents the extents of the image
     * @param isColor whether the image is in color
     * 
     * @return a buffer which is big enough to contain the image intensity data
     */
    protected static float[] initImageBuffer(final int[] extents, final boolean isColor, final int imageType) {
        int bufferFactor = 1;

        if (isColor) {
            bufferFactor = 4;
        } else if (imageType == ModelStorageBase.COMPLEX || imageType == ModelStorageBase.DCOMPLEX) {
            bufferFactor = Preferences.getComplexDisplay().getNumParts();
        }

        return new float[bufferFactor * extents[0] * extents[1]];
    }

    /**
     * Get the total number of time slices and volume slices.
     * 
     * @param img the image to get the slices of
     * 
     * @return an array containing the number of volume slices (in the first element) and the number of time slices in
     *         the image (in the second element)
     */
    protected static int[] initNumSlices(final ModelImage img) {
        final int[] numImages = new int[2];

        if (img.getNDims() == 4) {
            numImages[0] = img.getExtents()[2];
            numImages[1] = img.getExtents()[3];
        } else if (img.getNDims() == 3) {
            numImages[0] = img.getExtents()[2];
            numImages[1] = 0;
        } else {
            numImages[0] = 1;
            numImages[1] = 0;
        }

        return numImages;
    }

    /**
     * Create the pixel buffer for an image.
     * 
     * @param extents the extents of the image
     * 
     * @return a buffer which is big enough to contain the image pixel data
     */
    protected static int[] initPixelBuffer(final int[] extents) {
        return new int[extents[0] * extents[1]];
    }

    /**
     * Get the resolution correction needed for non-isotropic images.
     * 
     * @param imgResols the image resolution
     * @param imgUnits the image units of measure
     * 
     * @return the resolution correction factor in the x (the first element) and y (the second element) dimensions
     */
    protected static float[] initResFactor(final float[] imgResols, final int[] imgUnits) {
        final float[] resFactor = new float[2];

        resFactor[0] = 1.0f;
        resFactor[1] = 1.0f;

        if ( (imgResols[1] >= imgResols[0]) && (imgResols[1] < (20.0f * imgResols[0])) && (imgUnits[0] == imgUnits[1])) {
            resFactor[1] = imgResols[1] / imgResols[0];
        } else if ( (imgResols[0] > imgResols[1]) && (imgResols[0] < (20.0f * imgResols[1]))
                && (imgUnits[0] == imgUnits[1])) {
            resFactor[0] = imgResols[0] / imgResols[1];
        }

        return resFactor;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param img DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected static float[] initResolutions(final ModelImage img) {
        final float[] res = img.getFileInfo(0).getResolutions();

        for (int r = 0; r < img.getNDims(); r++) {

            if (res[r] < 0) {
                res[r] = Math.abs(res[r]);
            } else if (res[r] == 0) {
                res[r] = 1.0f;
            }
        }

        return res;
    }

    /**
     * Get the initial time and volume slice positions.
     * 
     * @param img the image to get the slice positions of
     * 
     * @return an array containing the slice in the volume (in the first element) and the time slice (in the second
     *         element)
     */
    protected static int[] initSlicePositions(final ModelImage img) {
        final int[] slices = new int[2];

        if (img.getNDims() == 4) {
            slices[0] = (img.getExtents()[2] - 1) / 2;
            slices[1] = 0;
        } else if (img.getNDims() == 3) {
            slices[0] = (img.getExtents()[2] - 1) / 2;
            slices[1] = 0;
        } else {
            slices[0] = 0;
            slices[1] = 0;
        }

        return slices;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param img DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected static int[] initUnits(final ModelImage img) {
        return img.getFileInfo(0).getUnitsOfMeasure();
    }

    /**
     * Creates and initializes the LUT for an image.
     * 
     * @param img the image to create a LUT for
     * 
     * @return a LUT for the image <code>img</code> (null if a color image)
     * 
     * @throws OutOfMemoryError if enough memory cannot be allocated for this method
     */
    public static ModelLUT initLUT(final ModelImage img) throws OutOfMemoryError {
        ModelLUT newLUT = null;

        if(img == null) {
        	return null;
        }
        
        // only make a lut for non color images
        if (img.isColorImage() == false) {
            final int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            newLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            float min, max;

            if (img.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (img.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) img.getMin();
                max = (float) img.getMax();
            }

            float imgMin = (float) img.getMin();
            float imgMax = (float) img.getMax();
            
            float[] x = new float[4];
            float[] y = new float[4];
            
            switch(Preferences.getDefaultDisplay()) {
            
            case LUT:
                try {
                    ModelLUT subLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
                    FileHistoLUT fileLUT = new FileHistoLUT("userdefine.lut", Preferences.getPreferencesDir(), subLUT);
                    fileLUT.setImg(img);
                    fileLUT.readLUTandTransferFunction(true);
                    newLUT.resetTransferLine(min, imgMin, max, imgMax);
                    float[] xSub = new float[subLUT.getTransferFunction().size()];
                    float[] ySub = new float[subLUT.getTransferFunction().size()];
                    subLUT.getTransferFunction().exportArrays(xSub, ySub);
                    newLUT.getTransferFunction().importArrays(xSub, ySub, subLUT.getTransferFunction().size());
                    break;
                } catch(Exception e) {
                    Preferences.debug("Default LUT could not be loaded", Preferences.DEBUG_FILEIO);
                    newLUT.resetTransferLine(min, imgMin, max, imgMax);
                    
                    break;
                }
                
            
            case ImageJ:
                int bins = 1024;
                
                int[] dimExtents = new int[]{bins};
    
                ModelHistogram histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtents);
    
                AlgorithmHistogram histoAlgo = new AlgorithmHistogram(histogram, img, true);
    
                histoAlgo.setRunningInSeparateThread(false);
                histoAlgo.run();
                
                int[] histoBuffer = histoAlgo.getHistoBuffer();
                int totalPix = histogram.getTotalPixels();
                boolean secondPeak = false;
                int belowTwoLoc = -1;
                for(int i=0; i<histoBuffer.length; i++) {
                	if(histoBuffer[i] < .002*totalPix) {
                		belowTwoLoc = i;
                		break;
                	}
                }
                
                if(belowTwoLoc != -1) {
                	for(int i=belowTwoLoc; i<histoBuffer.length; i++) {
                		if(histoBuffer[i] > .02*totalPix) {
                			secondPeak = true;
                			break;
                		}
                	}
                }
                
                if(!secondPeak) {
                	belowTwoLoc = 0;
                }
                
                double pixCache = 0;
                for(int i=belowTwoLoc; i<histoBuffer.length; i++) {
                    pixCache += histoBuffer[i];
                    if(pixCache/totalPix > .05) {
                        imgMin = imgMin + i*((imgMax-imgMin)/bins);
                        break;
                    }
                }
                
                pixCache = 0;
                for(int i=histoBuffer.length-1; i>=0; i--) {
                    pixCache += histoBuffer[i];
                    if(pixCache/totalPix > .05) {
                        imgMax = imgMax - (bins-i)*((imgMax-imgMin)/bins);
                        break;
                    }
                }
                
                newLUT.resetTransferLine(min, imgMin, max, imgMax);
                break;
                
            case MinMax:
                float minPref = min;
                float maxPref = max;
                
                try {
                    final String minString = Preferences.getProperty(Preferences.PREF_MIN);
                    final String maxString = Preferences.getProperty(Preferences.PREF_MAX);
                    
                    minPref = Float.valueOf(minString);
                    maxPref = Float.valueOf(maxString);
                    
                    if(minPref < min || maxPref > max || minPref > max || maxPref < min) {
                    	Preferences.debug("Default min/max are not applicable to this dataset", Preferences.DEBUG_FILEIO);
                    	newLUT.resetTransferLine(min, imgMin, max, imgMax);
                    	break;
                    }
                } catch(Exception e) {
                    Preferences.debug("Default min/max could not be loaded", Preferences.DEBUG_FILEIO);
                    newLUT.resetTransferLine(min, imgMin, max, imgMax);
                    
                    break;
                }
                
                y[0] = 255.0f;
                x[0] = minPref;

                y[1] = 255.0f;
                x[1] = minPref;

                y[2] = 0;
                x[2] = maxPref;
                
                y[3] = 0;
                x[3] = maxPref;

                newLUT.getTransferFunction().importArrays(x, y, 4);
                
                break;
                
            case WindowLevel:
                float level = min;
                float window = 1;
                
                try {
                    final String levelText = Preferences.getProperty(Preferences.PREF_LEVEL);
                    final String windowText = Preferences.getProperty(Preferences.PREF_WINDOW);
                   
                    level = Float.valueOf(levelText);
                    window = Float.valueOf(windowText);
                } catch(Exception e) {
                    Preferences.debug("Default win/level could not be loaded", Preferences.DEBUG_FILEIO);
                    newLUT.resetTransferLine(min, imgMin, max, imgMax);
                    
                    break;
                }
                
                JDialogWinLevel.calcWinLevTransferFunction(img, window, level, x, y);

                newLUT.getTransferFunction().importArrays(x, y, 4);
                break;
                
            case Default:
            case Mipav:

                newLUT.resetTransferLine(min, imgMin, max, imgMax);
                
                break;
            }
        }

        return newLUT;
    }

    /**
     * Creates and initializes the ModelRGB for an image.
     * 
     * @param img the image to create a ModelRGB for
     * 
     * @return a ModelRGB for the image <code>img</code> (null if NOT a color image)
     * 
     * @throws OutOfMemoryError if enough memory cannot be allocated for this method
     */
    public static ModelRGB initRGB(final ModelImage img) throws OutOfMemoryError {
        ModelRGB newRGB = null;

        if (img.isColorImage()) {
            final float[] x = new float[4];
            final float[] y = new float[4];
            final Dimension dim = new Dimension(256, 256);

            // Set ModelRGB min max values;
            x[0] = 0;
            y[0] = dim.height - 1;

            x[1] = 255 * 0.333f;
            y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

            x[2] = 255 * 0.667f;
            y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

            x[3] = 255;
            y[3] = 0;

            final int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;
            newRGB = new ModelRGB(RGBExtents);
            newRGB.getRedFunction().importArrays(x, y, 4);
            newRGB.getGreenFunction().importArrays(x, y, 4);
            newRGB.getBlueFunction().importArrays(x, y, 4);
            newRGB.makeRGB( -1);
        }

        return newRGB;
    }

    /**
     * Initializes the zoom variables for an image.
     * 
     * @param img the image to use to initialize the zoom
     * @param xResFactor correction factor in the x direction for images that are do not have isotropic voxels
     * @param yResFactor correction factor in the x direction for images that are do not have isotropic voxels
     * 
     * @return the initial zoom to use for this image
     */
    protected static float initZoom(final ModelImage img, final float xResFactor, final float yResFactor,
            final int screenX, final int screenY) {

        float imgZoom;
        float zoomX = 1, zoomY = 1;

        if ( (img.getExtents()[0] * xResFactor) > (screenX - 300)) {
            zoomX = (screenX - 300.0f) / (img.getExtents()[0] * xResFactor);
        }

        if ( (img.getExtents()[1] * yResFactor) > (screenY - 300)) {
            zoomY = (screenY - 300.0f) / (img.getExtents()[1] * yResFactor);
        }

        imgZoom = Math.min(zoomX, zoomY);

        if (imgZoom < 1) {

            if (imgZoom > 0.5) {
                imgZoom = 0.5f;
            } else if (imgZoom > 0.25) {
                imgZoom = 0.25f;
            } else if (imgZoom > 0.125) {
                imgZoom = 0.125f;
            } else if (imgZoom > 0.0625) {
                imgZoom = 0.0625f;
            } else if (imgZoom > 0.03125) {
                imgZoom = 0.03125f;
            } else {
                imgZoom = 0.015625f;
            }
        }

        return imgZoom;

    } // end initZoom()

    /**
     * Finds the FileFilter that matches the file type.
     * 
     * @param chooser JFileChooser
     * @param filterType int the file filter type
     * 
     * @return FileFilter the filter that matches the filter type.
     */
    private javax.swing.filechooser.FileFilter findFilter(final JFileChooser chooser, final int filterType) {

        // get the list of file filters held in chooser
        final javax.swing.filechooser.FileFilter[] filters = chooser.getChoosableFileFilters();

        for (final FileFilter element : filters) {

            if (ViewImageFileFilter.matches(element, filterType)) {
                return element;
            }
        }

        return null;

    } // end findFilter()

    /**
     * Returns whether an image can be resampled to match imageA for loading into the imageB slot of the frame.
     * 
     * @param image the image to check
     * 
     * @return true if the image is either 3D or 4D (but not if the 4th dim differs)
     */
    @SuppressWarnings("unused")
    private boolean isImageResampleable(final ModelImage image) {
        final int minDims = Math.min(imageB.getNDims(), imageA.getNDims());

        if ( (imageB.getNDims() == imageA.getNDims())
                || ( ( (imageB.getNDims() == 3) || (imageB.getNDims() == 4)) && ( (imageA.getNDims() == 3) || (imageA
                        .getNDims() == 4)))) {

            if (minDims == 4) {

                // Resampling will work for two 3D images or one 3D and one 4D image.
                // However, AlgorithmTransform is not currently equipped to deal with
                // differences in the fourth dimension between two 4D images
                if ( (imageB.getExtents()[3] != imageA.getExtents()[3])
                        || (imageB.getFileInfo(0).getResolutions()[3] != imageA.getFileInfo(0).getResolutions()[3])) {
                    MipavUtil.displayError("Images of unequal fourth dimensions");

                    return false;
                }
            }

            return true;
        } else {
            MipavUtil.displayError("Images of unequal dimensions");

            return false;
        }
    }

    /**
     * Returns whether a image should be resampled to match the extents and resolutions of imageA.
     * 
     * @param image the image to check against imageA
     * 
     * @return true if a resampling of the image is required
     */
    @SuppressWarnings("unused")
    private boolean isResampleNeeded(final ModelImage image) {
        final int minDims = Math.min(image.getNDims(), imageA.getNDims());
        Preferences.debug("minDims = " + minDims, Preferences.DEBUG_MINOR);

        if (image.getExtents() == null) {
            Preferences.debug("image.getExtents == null", Preferences.DEBUG_MINOR);
        } else {
            Preferences.debug("image.getExtents().length = " + image.getExtents().length, Preferences.DEBUG_MINOR);
        }

        if (image.getFileInfo(0) == null) {
            Preferences.debug("image.getFileInfo(0) == null", Preferences.DEBUG_MINOR);
        } else if (image.getFileInfo(0).getResolutions() == null) {
            Preferences.debug("image.getFileInfo(0).getResolutions() == null", Preferences.DEBUG_MINOR);
        } else {
            Preferences.debug("image.getFileInfo(0).getResolutions().length = "
                    + image.getFileInfo(0).getResolutions().length, Preferences.DEBUG_MINOR);
        }

        if (imageA.getExtents() == null) {
            Preferences.debug("imageA.getExtents == null", Preferences.DEBUG_MINOR);
        } else {
            Preferences.debug("imageA.getExtents().length = " + imageA.getExtents().length, Preferences.DEBUG_MINOR);
        }

        if (imageA.getFileInfo(0) == null) {
            Preferences.debug("imageA.getFileInfo(0) == null", Preferences.DEBUG_MINOR);
        } else if (imageA.getFileInfo(0).getResolutions() == null) {
            Preferences.debug("imageA.getFileInfo(0).getResolutions() == null", Preferences.DEBUG_MINOR);
        } else {
            Preferences.debug("imageA.getFileInfo(0).getResolutions().length = "
                    + imageA.getFileInfo(0).getResolutions().length, Preferences.DEBUG_MINOR);
        }

        for (int f = 0; f < minDims; f++) {

            if ( (image.getExtents()[f] != imageA.getExtents()[f])
                    || (image.getFileInfo(0).getResolutions()[f] != imageA.getFileInfo(0).getResolutions()[f])) {
                return true; // one or more dim or res != A's
            }
        }

        return false;
    }

    /**
     * Resamples an image and loads it into the imageB slot. This image should already be matched against imageA.
     * 
     * @param image the image to resample and load as imageB
     */
    @SuppressWarnings("unused")
    private void loadResampledImage(ModelImage image) {

        if ( (image.getNDims() == 3) || (image.getNDims() == 4)) {
            Preferences.debug("Resampling.", Preferences.DEBUG_FILEIO);

            // imageLocation == IMAGE_B ; resample into first 3 dimensions of A's grid
            final TransMatrix xfrm = image.getMatrix();
            float oXres, oYres, oZres;
            int oXdim, oYdim, oZdim;

            oXres = imageA.getFileInfo(0).getResolutions()[0];
            oYres = imageA.getFileInfo(0).getResolutions()[1];
            oZres = imageA.getFileInfo(0).getResolutions()[2];
            oXdim = imageA.getExtents()[0];
            oYdim = imageA.getExtents()[1];
            oZdim = imageA.getExtents()[2];

            AlgorithmTransform transformVol = new AlgorithmTransform(image, xfrm, AlgorithmTransform.TRILINEAR, oXres,
                    oYres, oZres, oXdim, oYdim, oZdim, false, true, false);

            // transformVol.setActiveImage(false);
            transformVol.run();

            final ModelImage resampledImage = transformVol.getTransformedImage();

            if (transformVol != null) {
                transformVol.disposeLocal();
                transformVol = null;
            }

            resampledImage.calcMinMax();

            // System.out.println(image.getMatrix());
            image.disposeLocal();
            image = null;
            setImageB(resampledImage);
        } else if (image.getNDims() == 2) { // imageLocation == IMAGE_B ; resample into A's grid

            final TransMatrix xfrm = image.getMatrix();
            float oXres, oYres;
            int oXdim, oYdim;

            oXres = imageA.getFileInfo(0).getResolutions()[0];
            oYres = imageA.getFileInfo(0).getResolutions()[1];
            oXdim = imageA.getExtents()[0];
            oYdim = imageA.getExtents()[1];

            AlgorithmTransform transformVol = new AlgorithmTransform(image, xfrm, AlgorithmTransform.BILINEAR, oXres,
                    oYres, oXdim, oYdim, false, true, false);

            transformVol.setRunningInSeparateThread(false);
            transformVol.run();

            final ModelImage resampledImage = transformVol.getTransformedImage();

            if (transformVol != null) {
                transformVol.disposeLocal();
                transformVol = null;
            }

            resampledImage.calcMinMax();

            // System.out.println(image.getMatrix());
            if ( !image.isColorImage()) {
                Preferences.debug("Done resampling image: min = " + resampledImage.getMin() + "max = "
                        + resampledImage.getMax() + "\n");
            } else {
                Preferences.debug("Done resampling image: minR = " + resampledImage.getMinR() + "minG = "
                        + resampledImage.getMinG() + "minB = " + resampledImage.getMinB() + "maxR = "
                        + resampledImage.getMaxR() + "maxG = " + resampledImage.getMaxG() + "maxB = "
                        + resampledImage.getMaxB() + "\n");
            }

            setImageB(resampledImage);
        } else { // imageLocation == IMAGE_B ; no resampling necessary
            setImageB(image);
            Preferences.debug("imageLocation == IMAGE_B ; no resampling necessary", Preferences.DEBUG_FILEIO);
        }
    }

    /**
     * Reorders the AFNI image based on the axis orientations. B0 to A1 indicates changing x to y; B0 to A2 indicates a
     * change from x to z.
     * 
     * @param image Image to reorder.
     * @param axisA axis indices orientation of reordered image
     * @param axisB axis indices orientation of original image
     * 
     * @return true if the reordering of the Afni file is successful, false otherwise
     */
    @SuppressWarnings("unused")
    private boolean reorderAfni(ModelImage image, final int[] axisA, final int[] axisB) {
        int[] resUnit = null;
        int[] newResUnit = null;
        int[] newExtents = null;
        float[] resol = null;
        float[] newResol = null;
        int newXDim, newYDim, newZDim;
        int newX = 0;
        int newY = 0;
        int newZ = 0;
        int i;
        boolean xInvert = false;
        boolean yInvert = false;
        boolean zInvert = false;
        boolean ytox = false;
        boolean ytoxInvert = false;
        boolean ztox = false;
        boolean ztoxInvert = false;
        boolean xtoy = false;
        boolean xtoyInvert = false;
        boolean ztoy = false;
        boolean ztoyInvert = false;
        boolean xtoz = false;
        boolean xtozInvert = false;
        boolean ytoz = false;
        boolean ytozInvert = false;

        int sliceSize, newSliceSize, volSize, newVolSize;
        double minimum, maximum;

        // AFNI functional images often have a zero filled gap with no collected data
        // planeGap = -1 if no gap is present, 0 = x axis, 1 = y axis, 2 = z axis
        int planeGap = -1;
        int[] gapArray = null;
        boolean unfoundGap;
        final int xDimB = image.getExtents()[0];
        final int yDimB = image.getExtents()[1];
        final int zDimB = image.getExtents()[2];
        int tDimB;
        float lowXmmB = 0, lowYmmB = 0, lowZmmB = 0, highXmmB = 0, highYmmB = 0, highZmmB = 0;
        float newLowXmmB = 0, newLowYmmB = 0, newLowZmmB = 0;
        float newHighXmmB = 0, newHighYmmB = 0, newHighZmmB = 0;
        FileInfoAfni[] newFileInfo;
        FileInfoBase fileInfo;
        int xyztSize;
        int x, y, z, t;
        float[] imgBuffer;
        float[] imgBuffer2;

        planeGap = ((FileInfoAfni) image.getFileInfo(0)).getPlaneGap();
        gapArray = ((FileInfoAfni) image.getFileInfo(0)).getGapArray();
        lowXmmB = ((FileInfoAfni) image.getFileInfo(0)).getLowXmm();
        lowYmmB = ((FileInfoAfni) image.getFileInfo(0)).getLowYmm();
        lowZmmB = ((FileInfoAfni) image.getFileInfo(0)).getLowZmm();
        highXmmB = ((FileInfoAfni) image.getFileInfo(0)).getHighXmm();
        highYmmB = ((FileInfoAfni) image.getFileInfo(0)).getHighYmm();
        highZmmB = ((FileInfoAfni) image.getFileInfo(0)).getHighZmm();

        if (image.getNDims() > 3) {
            tDimB = image.getExtents()[3];
            newResol = new float[4];
            newResol[3] = 1.0f;
            newResUnit = new int[4];
            newExtents = new int[4];
        } else {
            tDimB = 1;
            newResol = new float[3];
            newResUnit = new int[3];
            newExtents = new int[3];
        }

        resUnit = image.getFileInfo(0).getUnitsOfMeasure();
        resol = image.getFileInfo(0).getResolutions();

        if (FileInfoBase.sameAxis(axisA[1], axisB[0])) {

            if (axisB[0] == axisA[1]) {
                xtoy = true;
            } else {
                xtoyInvert = true;
            }
        } else if (FileInfoBase.sameAxis(axisB[0], axisA[2])) {

            if (axisB[0] == axisA[2]) {
                xtoz = true;
            } else {
                xtozInvert = true;
            }
        } else if (axisB[0] != axisA[0]) {
            xInvert = true;
        }

        if (FileInfoBase.sameAxis(axisB[1], axisA[0])) {

            if (axisB[1] == axisA[0]) {
                ytox = true;
            } else {
                ytoxInvert = true;
            }
        } else if (FileInfoBase.sameAxis(axisB[1], axisA[2])) {

            if (axisB[1] == axisA[2]) {
                ytoz = true;
            } else {
                ytozInvert = true;
            }
        } else if (axisB[1] != axisA[1]) {
            yInvert = true;
        }

        if (FileInfoBase.sameAxis(axisB[2], axisA[0])) {

            if (axisB[2] == axisA[0]) {
                ztox = true;
            } else {
                ztoxInvert = true;
            }
        } else if (FileInfoBase.sameAxis(axisB[2], axisA[1])) {

            if (axisB[2] == axisA[1]) {
                ztoy = true;
            } else {
                ztoyInvert = true;
            }
        } else if (axisB[2] != axisA[2]) {
            zInvert = true;
        }

        unfoundGap = true;

        if (ytox || ytoxInvert) {
            newResol[0] = resol[1];
            newResUnit[0] = resUnit[1];
            newXDim = yDimB;

            if ( (planeGap == 1) && (unfoundGap)) {
                planeGap = 0;
                unfoundGap = false;

                if (ytoxInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = yDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 1) && (foundGap))

            newLowXmmB = lowYmmB;
            newHighXmmB = highYmmB;
        } // if (ytox || ytoxInvert)
        else if (ztox || ztoxInvert) {
            newResol[0] = resol[2];
            newResUnit[0] = resUnit[2];
            newXDim = zDimB;

            if ( (planeGap == 2) && (unfoundGap)) {
                planeGap = 0;
                unfoundGap = false;

                if (ztoxInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = zDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 2)

            newLowXmmB = lowZmmB;
            newHighXmmB = highZmmB;
        } // else if (ztox || ztoxInvert)
        else {
            newResol[0] = resol[0];
            newResUnit[0] = resUnit[0];
            newXDim = xDimB;

            if ( (planeGap == 0) && (unfoundGap)) {
                unfoundGap = false;

                if (xInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = xDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 0) && (unfoundGap))

            newLowXmmB = lowXmmB;
            newHighXmmB = highXmmB;
        } // else for (x || xInvert)

        if (xtoy || xtoyInvert) {
            newResol[1] = resol[0];
            newResUnit[1] = resUnit[0];
            newYDim = xDimB;

            if ( (planeGap == 0) && (unfoundGap)) {
                planeGap = 1;
                unfoundGap = false;

                if (xtoyInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = xDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 0) && (unfoundGap))

            newLowYmmB = lowXmmB;
            newHighYmmB = highXmmB;
        } // if (xtoy || xtoyInvert)
        else if (ztoy || ztoyInvert) {
            newResol[1] = resol[2];
            newResUnit[1] = resUnit[2];
            newYDim = zDimB;

            if ( (planeGap == 2) && (unfoundGap)) {
                planeGap = 1;
                unfoundGap = false;

                if (ztoyInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = zDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 2) && (unfoundGap))

            newLowYmmB = lowZmmB;
            newHighYmmB = highZmmB;
        } // else if (ztoy || ztoyInvert)
        else {
            newResol[1] = resol[1];
            newResUnit[1] = resUnit[1];
            newYDim = yDimB;

            if ( (planeGap == 1) && (unfoundGap)) {
                unfoundGap = false;

                if (yInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = yDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 1) && (unfoundGap))

            newLowYmmB = lowYmmB;
            newHighYmmB = highYmmB;
        } // else for (y || yInvert)

        if (xtoz || xtozInvert) {
            newResol[2] = resol[0];
            newResUnit[2] = resUnit[0];
            newZDim = xDimB;

            if ( (planeGap == 0) && (unfoundGap)) {
                planeGap = 2;
                unfoundGap = false;

                if (xtozInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = xDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 0) && (unfoundGap))

            newLowZmmB = lowXmmB;
            newHighZmmB = highXmmB;
        } // if (xtoz || xtozInvert)
        else if (ytoz || ytozInvert) {
            newResol[2] = resol[1];
            newResUnit[2] = resUnit[1];
            newZDim = yDimB;

            if ( (planeGap == 1) && (unfoundGap)) {
                planeGap = 2;
                unfoundGap = false;

                if (ytozInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = yDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planeGap == 1) && (unfoundGap))

            newLowZmmB = lowYmmB;
            newHighZmmB = highYmmB;
        } // else if (ytoz || ytozInvert)
        else {
            newResol[2] = resol[2];
            newResUnit[2] = resUnit[2];
            newZDim = zDimB;

            if ( (planeGap == 2) && (unfoundGap)) {
                unfoundGap = false;

                if (zInvert) {

                    for (i = 0; i < gapArray.length; i++) {
                        gapArray[i] = zDimB - 1 - gapArray[i];
                    }
                }
            } // if ((planegap == 2) && (unfoundGap))

            newLowZmmB = lowZmmB;
            newHighZmmB = highZmmB;
        } // else for (z || zInvert)

        sliceSize = xDimB * yDimB;
        newSliceSize = newXDim * newYDim;
        volSize = sliceSize * zDimB;
        newVolSize = newSliceSize * newZDim;
        image.calcMinMax();
        minimum = image.getMin();
        maximum = image.getMax();

        progressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(), "Reordering imageB to imageA ...", 0,
                100, false, null, null);

        final int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        final int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;

        progressBar.setLocation(xScreen / 2, yScreen / 2);
        progressBar.setVisible(true);
        progressBar.updateValue(0, true);

        xyztSize = xDimB * yDimB * zDimB * tDimB;
        imgBuffer = new float[xyztSize];

        try {
            image.exportData(0, xyztSize, imgBuffer);
        } catch (final IOException e) {
            MipavUtil.displayError("Error on image.exportData(0,xyztSize,imgBuffer)");
            progressBar.dispose();

            if (image != null) {
                image.disposeLocal();
            }

            image = null;

            return false;
        }

        newExtents[0] = newXDim;
        newExtents[1] = newYDim;
        newExtents[2] = newZDim;

        if (image.getNDims() > 3) {
            newExtents[3] = tDimB;
        }

        fileInfo = image.getFileInfo(0);

        try {
            image.changeExtents(newExtents);
        } catch (final OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("ViewJFrameBase: Out of memory on image.changeExtents(newExtents)");
            progressBar.dispose();

            if (image != null) {
                image.disposeLocal();
            }

            image = null;

            return false;
        }

        newFileInfo = new FileInfoAfni[newZDim * tDimB];

        for (i = 0; i < (newZDim * tDimB); i++) {
            newFileInfo[i] = (FileInfoAfni) fileInfo.clone();
            newFileInfo[i].setModality(fileInfo.getModality());
            newFileInfo[i].setFileDirectory(fileInfo.getFileDirectory());
            newFileInfo[i].setDataType(fileInfo.getDataType());
            newFileInfo[i].setEndianess(fileInfo.getEndianess());
            newFileInfo[i].setUnitsOfMeasure(newResUnit);
            newFileInfo[i].setResolutions(newResol);
            newFileInfo[i].setExtents(newExtents);
            newFileInfo[i].setAxisOrientation(axisA);
            newFileInfo[i].setMax(maximum);
            newFileInfo[i].setMin(minimum);
            newFileInfo[i].setPixelPadValue(fileInfo.getPixelPadValue());
            newFileInfo[i].setPhotometric(fileInfo.getPhotometric());
            (newFileInfo[i]).setAFNIViewType( ((FileInfoAfni) fileInfo).getAFNIViewType());
            (newFileInfo[i]).setAFNITypeString( ((FileInfoAfni) fileInfo).getAFNITypeString());
            (newFileInfo[i]).setPlaneGap(planeGap);

            if (planeGap != -1) {
                (newFileInfo[i]).setGapArray(gapArray);
            }

            (newFileInfo[i]).setLowXmm(newLowXmmB);
            (newFileInfo[i]).setLowYmm(newLowYmmB);
            (newFileInfo[i]).setLowZmm(newLowZmmB);
            (newFileInfo[i]).setHighXmm(newHighXmmB);
            (newFileInfo[i]).setHighYmm(newHighYmmB);
            (newFileInfo[i]).setHighZmm(newHighZmmB);
        }

        image.setFileInfo(newFileInfo);

        imgBuffer2 = new float[xyztSize];

        for (t = 0; t < tDimB; t++) {

            for (x = 0; x < xDimB; x++) {
                progressBar.updateValue(Math.round(100 * (x + (t * xDimB)) / ( ( (tDimB - 1) * xDimB) + xDimB - 1)),
                        true);

                if (xtoy) {
                    newY = x;
                } else if (xtoyInvert) {
                    newY = xDimB - 1 - x;
                } else if (xtoz) {
                    newZ = x;
                } else if (xtozInvert) {
                    newZ = xDimB - 1 - x;
                } else if (xInvert) {
                    newX = xDimB - 1 - x;
                } else {
                    newX = x;
                }

                for (y = 0; y < yDimB; y++) {

                    if (ytox) {
                        newX = y;
                    } else if (ytoxInvert) {
                        newX = yDimB - 1 - y;
                    } else if (ytoz) {
                        newZ = y;
                    } else if (ytozInvert) {
                        newZ = yDimB - 1 - y;
                    } else if (yInvert) {
                        newY = yDimB - 1 - y;
                    } else {
                        newY = y;
                    }

                    for (z = 0; z < zDimB; z++) {

                        if (ztox) {
                            newX = z;
                        } else if (ztoxInvert) {
                            newX = zDimB - 1 - z;
                        } else if (ztoy) {
                            newY = z;
                        } else if (ztoyInvert) {
                            newY = zDimB - 1 - z;
                        } else if (zInvert) {
                            newZ = zDimB - 1 - z;
                        } else {
                            newZ = z;
                        }

                        imgBuffer2[newX + (newXDim * newY) + (newSliceSize * newZ) + (newVolSize * t)] = imgBuffer[x
                                + (xDimB * y) + (sliceSize * z) + (volSize * t)];
                    } // for (z = 0; z < zDimB; z++)
                } // for (y = 0; y < yDimB; y++)
            } // for (x = 0; x < xDimB; x++)

        } // for (t = 0; t < tDimB; t++)

        imgBuffer = null;

        try {
            image.importData(0, imgBuffer2, true);
        } catch (final IOException e) {
            MipavUtil.displayError("Error on image.importData(0,imgBuffer2,true)");
            progressBar.dispose();

            if (image != null) {
                image.disposeLocal();
            }

            image = null;

            return false;
        }

        imgBuffer2 = null;
        progressBar.dispose();

        return true;
    }

    /**
     * Sets image B to an AFNI image, performing the appropriate transformations.
     * 
     * @param imageA Image A.
     * @param image AFNI image to set to image A.
     * 
     * @return -1 if failure; 0 if no tranformation needed; and 1 if transformation and set successful.
     */
    @SuppressWarnings("unused")
    private int setImageBAfni(final ModelImage imageA, ModelImage image) {
        final FileInfoAfni imageAInfo = (FileInfoAfni) imageA.getFileInfo(0);
        final FileInfoAfni imageBInfo = (FileInfoAfni) image.getFileInfo(0);
        final int AFNIViewTypeA = imageAInfo.getAFNIViewType();
        final int AFNIViewTypeB = imageBInfo.getAFNIViewType();
        final int AFNITypeStringA = imageAInfo.getAFNITypeString();
        final int AFNITypeStringB = imageBInfo.getAFNITypeString();
        final int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        final int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;
        ViewJProgressBar afniProgressBar = null;
        FileInfoAfni[] newAfniInfo;
        int xDimA, yDimA, zDimA, xDimB, yDimB, zDimB, tDimB;
        float xResA, yResA, zResA, xResB, yResB, zResB;
        float lowXmmA = 0, lowYmmA = 0, lowZmmA = 0, highXmmA = 0, highYmmA = 0, highZmmA = 0;
        float lowXmmB = 0, lowYmmB = 0, lowZmmB = 0, highXmmB = 0, highYmmB = 0, highZmmB = 0;
        float scaleX, scaleY, scaleZ;

        boolean doNN; // use nearest neighbor interpolation on threshold data in functional images

        // AFNI functional images often have a zero filled gap with no collected data
        // planeGap = -1 if no gap is present, 0 = x axis, 1 = y axis, 2 = z axis
        int planeGap = -1;
        int[] gapArray = null;

        // Coordinates values on on planeGap axis at which gaps are present
        int[] tmpArray = null;
        int gapNumber;
        int[] newResUnit;
        int[] newExtents;
        float[] newResol;
        float[] imgBuffer;
        int bufferSize;
        double minimum = 0;
        double maximum = 0;
        int viewType = AFNIViewTypeA;

        if ( (AFNIViewTypeA == FileInfoAfni.AFNI_ORIG)
                && (imageA.getNDims() == 3)
                && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG)
                && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                        .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                        .getNDims() == 4)))))) {
            xDimA = imageAInfo.getExtents()[0];
            yDimA = imageAInfo.getExtents()[1];
            zDimA = imageAInfo.getExtents()[2];
            xResA = imageAInfo.getResolutions()[0];
            yResA = imageAInfo.getResolutions()[1];
            zResA = imageAInfo.getResolutions()[2];
        } else if ( ( (AFNIViewTypeA == FileInfoAfni.AFNI_ACPC) && (imageA.getNDims() == 3)
                && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG) && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                .getNDims() == 4))))))
                || ( (AFNIViewTypeA == FileInfoAfni.AFNI_TLRC) && (imageA.getNDims() == 3)
                        && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG) && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                        .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                        .getNDims() == 4))))))) {
            xDimA = imageAInfo.getAFNIOrigExtents()[0];
            yDimA = imageAInfo.getAFNIOrigExtents()[1];
            zDimA = imageAInfo.getAFNIOrigExtents()[2];
            xResA = imageAInfo.getAFNIOrigResolutions()[0];
            yResA = imageAInfo.getAFNIOrigResolutions()[1];
            zResA = imageAInfo.getAFNIOrigResolutions()[2];
        } else {
            return 0;
        }

        lowXmmA = imageAInfo.getLowXmm();
        lowYmmA = imageAInfo.getLowYmm();
        lowZmmA = imageAInfo.getLowZmm();
        highXmmA = imageAInfo.getHighXmm();
        highYmmA = imageAInfo.getHighYmm();
        highZmmA = imageAInfo.getHighZmm();
        xDimB = image.getExtents()[0];
        yDimB = image.getExtents()[1];
        zDimB = image.getExtents()[2];
        xResB = imageBInfo.getResolutions()[0];
        yResB = imageBInfo.getResolutions()[1];
        zResB = imageBInfo.getResolutions()[2];
        lowXmmB = imageBInfo.getLowXmm();
        lowYmmB = imageBInfo.getLowYmm();
        lowZmmB = imageBInfo.getLowZmm();
        highXmmB = imageBInfo.getHighXmm();
        highYmmB = imageBInfo.getHighYmm();
        highZmmB = imageBInfo.getHighZmm();
        planeGap = imageBInfo.getPlaneGap();
        gapArray = imageBInfo.getGapArray();
        Preferences.debug("lowXmmA = " + lowXmmA + " lowYmmA = " + lowYmmA + " lowZmmA = " + lowZmmA + "\n");
        Preferences.debug("highXmmA = " + highXmmA + " highYmmA = " + highYmmA + " highZmmA = " + highZmmA + "\n");
        Preferences.debug("lowXmmB = " + lowXmmB + " lowYmmB = " + lowYmmB + " lowZmmB = " + lowZmmB + "\n");
        Preferences.debug("highXmmB = " + highXmmB + " highYmmB = " + highYmmB + " highZmmB = " + highZmmB + "\n");

        if ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE)) {
            doNN = true;
        } else {
            doNN = false;
        }

        if (image.getNDims() > 3) {
            tDimB = image.getExtents()[3];
        } else {
            tDimB = 1;
        }

        if (image.getNDims() > 3) {
            newExtents = new int[4];
            newResol = new float[4];
            newResol[3] = 0.0f;
            newResUnit = new int[4];
            newResUnit[3] = 0;
        } else {
            newExtents = new int[3];
            newResol = new float[3];
            newResUnit = new int[3];
        }

        bufferSize = xDimB * yDimB * zDimB * tDimB;

        try {
            imgBuffer = new float[bufferSize];
            image.exportData(0, bufferSize, imgBuffer);
        } catch (final IOException error) {
            MipavUtil.displayError("ViewJFrameBase: IOException error on exportData");

            return -1;
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("ViewJFrameBase: Out of memory on new image buffer.");

            return -1;
        }

        newExtents[0] = xDimA;
        newExtents[1] = yDimA;
        newExtents[2] = zDimA;

        if (image.getNDims() > 3) {
            newExtents[3] = tDimB;
        }

        newResUnit[0] = imageAInfo.getUnitsOfMeasure()[0];
        newResUnit[1] = imageAInfo.getUnitsOfMeasure()[1];
        newResUnit[2] = imageAInfo.getUnitsOfMeasure()[2];

        final int imageType = image.getType();
        userInterface = ViewUserInterface.getReference();

        image.disposeLocal();
        image = null;
        System.gc();

        try {
            image = new ModelImage(imageType, newExtents, imageBInfo.getFileName());
        } catch (final OutOfMemoryError e) {
            System.gc();
            MipavUtil.displayError("ViewJFrameBase: Out of memory on new ModelImage");

            return -1;
        }

        if ( (AFNIViewTypeA == FileInfoAfni.AFNI_ORIG)
                && (imageA.getNDims() == 3)
                && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG)
                && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                        .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                        .getNDims() == 4)))))) {

            // convert IMAGEB from FileInfoAfni.AFNI_ORIG to rescaled FileInfoAfni.AFNI_ORIG
            // I am skipping the transform() section of AlgorithmTransform which has the line
            // xfrm = matrixtoInverseArray(transMatrix);
            afniProgressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(),
                    "Transforming to match AFNI anatomical image ...", 0, 100, false, null, null);
            afniProgressBar.setLocation(xScreen / 2, yScreen / 2);
            afniProgressBar.setVisible(true);
            afniProgressBar.updateValue(0, true);

            newResol[0] = xResA;
            newResol[1] = yResA;
            newResol[2] = zResA;

            image.setImageOrientation(imageA.getImageOrientation());

            transformAFNI(image, imgBuffer, xResB, yResB, zResB, xDimB, yDimB, zDimB, tDimB, lowXmmB, lowYmmB, lowZmmB,
                    highXmmB, highYmmB, highZmmB, planeGap, gapArray, doNN, xResA, yResA, zResA, xDimA, yDimA, zDimA,
                    lowXmmA, lowYmmA, lowZmmA, highXmmA, highYmmA, highZmmA);
            image.calcMinMax();
            minimum = image.getMin();
            maximum = image.getMax();

            // Determine the new gapArray if slices of uncollected data represented by zeroes
            // are present
            if (planeGap == 0) {
                gapNumber = 0;
                tmpArray = new int[xDimA];
                scaleX = xResA / xResB;

                final boolean[] found = new boolean[xDimA];

                for (final int element : gapArray) {

                    for (int x = 0; x < xDimA; x++) {

                        if ( ( (x * scaleX) >= element) && ( (x * scaleX) < (element + 1)) && ( !found[x])) {
                            tmpArray[gapNumber++] = x;
                            found[x] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // if (planeGap ==0)
            else if (planeGap == 1) {
                gapNumber = 0;
                tmpArray = new int[yDimA];
                scaleY = yResA / yResB;

                final boolean[] found = new boolean[yDimA];

                for (final int element : gapArray) {

                    for (int y = 0; y < yDimA; y++) {

                        if ( ( (y * scaleY) >= element) && ( (y * scaleY) < (element + 1)) && ( !found[y])) {
                            tmpArray[gapNumber++] = y;
                            found[y] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planegap == 1)
            else if (planeGap == 2) {
                gapNumber = 0;
                tmpArray = new int[zDimA];
                scaleZ = zResA / zResB;

                final boolean[] found = new boolean[zDimA];

                for (final int element : gapArray) {

                    for (int z = 0; z < zDimA; z++) {

                        if ( ( (z * scaleZ) >= element) && ( (z * scaleZ) < (element + 1)) && ( !found[z])) {
                            tmpArray[gapNumber++] = z;
                            found[z] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planeGap == 2)

            viewType = FileInfoAfni.AFNI_ORIG;
        } // end of convert IMAGEB from FileInfoAfni.AFNI_ORIG to rescaled FileInfoAfni.AFNI_ORIG
        else if ( (AFNIViewTypeA == FileInfoAfni.AFNI_ACPC)
                && (imageA.getNDims() == 3)
                && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG)
                && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                        .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                        .getNDims() == 4)))))) {

            // convert IMAGEB from FileInfoAfni.AFNI_ORIG to FileInfoAfni.AFNI_ACPC
            // I am skipping the transform() section of AlgorithmTransform which has the line
            // xfrm = matrixtoInverseArray(transMatrix);
            afniProgressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(),
                    "Transforming to match original anatomical image ...", 0, 100, false, null, null);
            afniProgressBar.setLocation(xScreen / 2, yScreen / 2);
            afniProgressBar.setVisible(true);
            afniProgressBar.updateValue(0, true);

            final Vector3f alpha = imageAInfo.getAlpha();
            final Vector3f beta = imageAInfo.getBeta();
            final Vector3f gamma = imageAInfo.getGamma();
            final Vector3f TalairachCenter = imageAInfo.getTalairachCenter();
            final Vector3f rr = imageAInfo.getrr();

            // Change rr into mm distance
            rr.X = rr.X * xResA;
            rr.Y = rr.Y * yResA;
            rr.Z = rr.Z * zResA;

            transformAFNI(image, imgBuffer, xResB, yResB, zResB, xDimB, yDimB, zDimB, tDimB, lowXmmB, lowYmmB, lowZmmB,
                    highXmmB, highYmmB, highZmmB, planeGap, gapArray, doNN, xResA, yResA, zResA, xDimA, yDimA, zDimA,
                    lowXmmA, lowYmmA, lowZmmA, highXmmA, highYmmA, highZmmA);
            image.calcMinMax();

            // Determine the new gapArray if slices of uncollected data represented by zeroes
            // are present
            if (planeGap == 0) {
                gapNumber = 0;
                tmpArray = new int[xDimA];
                scaleX = xResA / xResB;

                final boolean[] found = new boolean[xDimA];

                for (final int element : gapArray) {

                    for (int x = 0; x < xDimA; x++) {

                        if ( ( (x * scaleX) >= element) && ( (x * scaleX) < (element + 1)) && ( !found[x])) {
                            tmpArray[gapNumber++] = x;
                            found[x] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // if (planeGap ==0)
            else if (planeGap == 1) {
                gapNumber = 0;
                tmpArray = new int[yDimA];
                scaleY = yResA / yResB;

                final boolean[] found = new boolean[yDimA];

                for (final int element : gapArray) {

                    for (int y = 0; y < yDimA; y++) {

                        if ( ( (y * scaleY) >= element) && ( (y * scaleY) < (element + 1)) && ( !found[y])) {
                            tmpArray[gapNumber++] = y;
                            found[y] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planegap == 1)
            else if (planeGap == 2) {
                gapNumber = 0;
                tmpArray = new int[zDimA];
                scaleZ = zResA / zResB;

                final boolean[] found = new boolean[zDimA];

                for (final int element : gapArray) {

                    for (int z = 0; z < zDimA; z++) {

                        if ( ( (z * scaleZ) >= element) && ( (z * scaleZ) < (element + 1)) && ( !found[z])) {
                            tmpArray[gapNumber++] = z;
                            found[z] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planeGap == 2)

            xDimA = imageA.getExtents()[0];
            yDimA = imageA.getExtents()[1];
            zDimA = imageA.getExtents()[2];
            xResA = imageAInfo.getResolutions()[0];
            yResA = imageAInfo.getResolutions()[1];
            zResA = imageAInfo.getResolutions()[2];
            xDimB = imageAInfo.getAFNIOrigExtents()[0];
            yDimB = imageAInfo.getAFNIOrigExtents()[1];
            zDimB = imageAInfo.getAFNIOrigExtents()[2];
            xResB = imageAInfo.getAFNIOrigResolutions()[0];
            yResB = imageAInfo.getAFNIOrigResolutions()[1];
            zResB = imageAInfo.getAFNIOrigResolutions()[2];
            newExtents[0] = xDimA;
            newExtents[1] = yDimA;
            newExtents[2] = zDimA;

            if (image.getNDims() > 3) {
                newExtents[3] = tDimB;
            }

            newResol[0] = xResA;
            newResol[1] = yResA;
            newResol[2] = zResA;

            // convert talairach center from coordinates to millimeters
            TalairachCenter.X = TalairachCenter.X * xResA;
            TalairachCenter.Y = TalairachCenter.Y * yResA;
            TalairachCenter.Z = TalairachCenter.Z * zResA;

            final Vector3f center = image.getImageCentermm(false);

            bufferSize = xDimB * yDimB * zDimB * tDimB;
            imgBuffer = new float[bufferSize];

            try {
                image.exportData(0, bufferSize, imgBuffer);
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameBase: IOException error on exportData");
            }

            image.disposeLocal();
            image = null;
            System.gc();
            afniProgressBar.setMessage("Transforming to match ACPC image ...");

            try {
                image = new ModelImage(imageType, newExtents, imageBInfo.getFileName());
            } catch (final OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("ViewJFrameBase: Out of memory on new ModelImage for ACPC rotation");
                afniProgressBar.dispose();

                return -1;
            }

            final TransMatrix xfrm = new TransMatrix(4);

            xfrm.setTranslate(center.X, center.Y, center.Z);

            xfrm.setRotate(alpha, beta, gamma);

            // double[][] M = xfrm.getMatrix();

            // Remember this is an output to input mapping so find the translation needed
            // to map the transformed Talairach center to the original dicom ordered functional
            // image rr
            final double Tr03 = rr.X - (TalairachCenter.X * xfrm.get(0, 0)) - (TalairachCenter.Y * xfrm.get(0, 1))
                    - (TalairachCenter.Z * xfrm.get(0, 2));
            final double Tr13 = rr.Y - (TalairachCenter.X * xfrm.get(1, 0)) - (TalairachCenter.Y * xfrm.get(1, 1))
                    - (TalairachCenter.Z * xfrm.get(1, 2));
            final double Tr23 = rr.Z - (TalairachCenter.X * xfrm.get(2, 0)) - (TalairachCenter.Y * xfrm.get(2, 1))
                    - (TalairachCenter.Z * xfrm.get(2, 2));

            /*
             * Tr03 = xfrm.Get(0, 0) * Tx + xfrm.Get(0, 1) * Ty + xfrm.Get(0, 2) * Tz + xfrm.Get(0, 3) Tr13 =
             * xfrm.Get(1, 0) * Tx + xfrm.Get(1, 1) * Ty + xfrm.Get(1, 2) Tz + xfrm.Get(1, 3) Tr23 = xfrm.Get(2, 2) * Tx +
             * xfrm.Get(2, 1) * Ty + xfrm.Get(2, 2) * Tz + xfrm.Get(2, 3)
             */
            final JamaMatrix A = new JamaMatrix(3, 3);

            A.set(0, 0, xfrm.get(0, 0));
            A.set(0, 1, xfrm.get(0, 1));
            A.set(0, 2, xfrm.get(0, 2));
            A.set(1, 0, xfrm.get(1, 0));
            A.set(1, 1, xfrm.get(1, 1));
            A.set(1, 2, xfrm.get(1, 2));
            A.set(2, 0, xfrm.get(2, 0));
            A.set(2, 1, xfrm.get(2, 1));
            A.set(2, 2, xfrm.get(2, 2));

            final JamaMatrix b = new JamaMatrix(3, 1);

            b.set(0, 0, Tr03 - xfrm.get(0, 3));
            b.set(1, 0, Tr13 - xfrm.get(1, 3));
            b.set(2, 0, Tr23 - xfrm.get(2, 3));

            final JamaMatrix X = A.solve(b);
            final double Tx = X.get(0, 0);
            final double Ty = X.get(1, 0);
            final double Tz = X.get(2, 0);

            xfrm.setTranslate(Tx, Ty, Tz);

            transformACPC(image, imgBuffer, xfrm, xResB, yResB, zResB, xDimB, yDimB, zDimB, tDimB, planeGap, gapArray,
                    doNN, xResA, yResA, zResA, xDimA, yDimA, zDimA);
            image.calcMinMax();
            minimum = image.getMin();
            maximum = image.getMax();

            // After AFNI rotation to form ACPC image can no longer use gap information based on a
            // single x,y, or z location. Could mark individual pixels, but this would be very tedious
            // Simply stop using the gap information in further interpolation for now
            planeGap = -1;
            viewType = FileInfoAfni.AFNI_ACPC;
        } // end of convert IMAGEB from FileInfoAfni.AFNI_ORIG to FileInfoAfni.AFNI_ACPC
        else if ( (AFNIViewTypeA == FileInfoAfni.AFNI_TLRC)
                && (imageA.getNDims() == 3)
                && (AFNIViewTypeB == FileInfoAfni.AFNI_ORIG)
                && ( ( (AFNITypeStringA == FileInfoAfni.HEAD_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.HEAD_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.HEAD_ANAT_TYPE) && (image
                        .getNDims() == 4)))) || ( (AFNITypeStringA == FileInfoAfni.GEN_ANAT_TYPE) && ( (AFNITypeStringB == FileInfoAfni.GEN_FUNC_TYPE) || ( (AFNITypeStringB == FileInfoAfni.GEN_ANAT_TYPE) && (image
                        .getNDims() == 4)))))) {

            // convert IMAGEB from FileInfoAfni.AFNI_ORIG to FileInfoAfni.AFNI_TLRC
            // I am skipping the transform() section of AlgorithmTransform which has the line
            // xfrm = matrixtoInverseArray(transMatrix);
            afniProgressBar = new ViewJProgressBar(image.getFileInfo()[0].getFileName(),
                    "Transforming to match original anatomical image ...", 0, 100, false, null, null);

            afniProgressBar.setLocation(xScreen / 2, yScreen / 2);
            afniProgressBar.setVisible(true);
            afniProgressBar.updateValue(0, true);

            final Vector3f[] alphaArray = imageAInfo.getAlphaArray();
            final Vector3f[] betaArray = imageAInfo.getBetaArray();
            final Vector3f[] gammaArray = imageAInfo.getGammaArray();
            final Vector3f[] rrArray = imageAInfo.getrrArray();
            final Vector3f TalairachCenter = imageAInfo.getTalairachCenter();
            final int[] botX = imageAInfo.getBotX();
            final int[] botY = imageAInfo.getBotY();
            final int[] botZ = imageAInfo.getBotZ();
            final int[] topX = imageAInfo.getTopX();
            final int[] topY = imageAInfo.getTopY();
            final int[] topZ = imageAInfo.getTopZ();

            // Change rr into mm distance
            for (int i = 0; i < 12; i++) {
                rrArray[i].X = rrArray[i].X * xResA;
                rrArray[i].Y = rrArray[i].Y * yResA;
                rrArray[i].Z = rrArray[i].Z * zResA;
            }

            transformAFNI(image, imgBuffer, xResB, yResB, zResB, xDimB, yDimB, zDimB, tDimB, lowXmmB, lowYmmB, lowZmmB,
                    highXmmB, highYmmB, highZmmB, planeGap, gapArray, doNN, xResA, yResA, zResA, xDimA, yDimA, zDimA,
                    lowXmmA, lowYmmA, lowZmmA, highXmmA, highYmmA, highZmmA);
            image.calcMinMax();

            // Determine the new gapArray if slices of uncollected data represented by zeroes
            // are present
            if (planeGap == 0) {
                gapNumber = 0;
                tmpArray = new int[xDimA];
                scaleX = xResA / xResB;

                final boolean[] found = new boolean[xDimA];

                for (final int element : gapArray) {

                    for (int x = 0; x < xDimA; x++) {

                        if ( ( (x * scaleX) >= element) && ( (x * scaleX) < (element + 1)) && ( !found[x])) {
                            tmpArray[gapNumber++] = x;
                            found[x] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // if (planeGap ==0)
            else if (planeGap == 1) {
                gapNumber = 0;
                tmpArray = new int[yDimA];
                scaleY = yResA / yResB;

                final boolean[] found = new boolean[yDimA];

                for (final int element : gapArray) {

                    for (int y = 0; y < yDimA; y++) {

                        if ( ( (y * scaleY) >= element) && ( (y * scaleY) < (element + 1)) && ( !found[y])) {
                            tmpArray[gapNumber++] = y;
                            found[y] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planegap == 1)
            else if (planeGap == 2) {
                gapNumber = 0;
                tmpArray = new int[zDimA];
                scaleZ = zResA / zResB;

                final boolean[] found = new boolean[zDimA];

                for (final int element : gapArray) {

                    for (int z = 0; z < zDimA; z++) {

                        if ( ( (z * scaleZ) >= element) && ( (z * scaleZ) < (element + 1)) && ( !found[z])) {
                            tmpArray[gapNumber++] = z;
                            found[z] = true;
                        }
                    }
                } // for (i = 0; i < gapArray[i]; i++)

                gapArray = new int[gapNumber];

                for (int i = 0; i < gapNumber; i++) {
                    gapArray[i] = tmpArray[i];
                }

                tmpArray = null;
            } // else if (planeGap == 2)

            xDimA = imageA.getExtents()[0];
            yDimA = imageA.getExtents()[1];
            zDimA = imageA.getExtents()[2];
            xResA = imageAInfo.getResolutions()[0];
            yResA = imageAInfo.getResolutions()[1];
            zResA = imageAInfo.getResolutions()[2];
            xDimB = imageAInfo.getAFNIOrigExtents()[0];
            yDimB = imageAInfo.getAFNIOrigExtents()[1];
            zDimB = imageAInfo.getAFNIOrigExtents()[2];
            xResB = imageAInfo.getAFNIOrigResolutions()[0];
            yResB = imageAInfo.getAFNIOrigResolutions()[1];
            zResB = imageAInfo.getAFNIOrigResolutions()[2];
            newExtents[0] = xDimA;
            newExtents[1] = yDimA;
            newExtents[2] = zDimA;

            if (image.getNDims() > 3) {
                newExtents[3] = tDimB;
            }

            newResol[0] = xResA;
            newResol[1] = yResA;
            newResol[2] = zResA;

            // Change TalairachCenter in millimeters
            TalairachCenter.X = TalairachCenter.X * xResA;
            TalairachCenter.Y = TalairachCenter.Y * yResA;
            TalairachCenter.Z = TalairachCenter.Z * zResA;

            newResUnit[0] = imageAInfo.getUnitsOfMeasure()[0];
            newResUnit[1] = imageAInfo.getUnitsOfMeasure()[1];
            newResUnit[2] = imageAInfo.getUnitsOfMeasure()[2];

            final Vector3f center = image.getImageCentermm(false);

            bufferSize = xDimB * yDimB * zDimB * tDimB;
            imgBuffer = new float[bufferSize];

            try {
                image.exportData(0, bufferSize, imgBuffer);
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameBase: IOException error on exportData");
            }

            try {
                image.disposeLocal();
                image = new ModelImage(imageType, newExtents, imageBInfo.getFileName());
            } catch (final OutOfMemoryError e) {
                System.gc();
                MipavUtil.displayError("ViewJFrameBase: Out of memory on new ModelImage for Talairach conversion");
                afniProgressBar.dispose();

                return -1;
            }

            final TransMatrix xfrm = new TransMatrix(4);

            for (int i = 0; i < 12; i++) {
                final int j = i + 1;

                afniProgressBar.setMessage("Talairach transformation pass #" + j);

                xfrm.identity();
                xfrm.setTranslate(center.X, center.Y, center.Z);
                xfrm.setRotate(alphaArray[i], betaArray[i], gammaArray[i]);

                final double Tr03 = rrArray[i].X - (TalairachCenter.X * xfrm.get(0, 0))
                        - (TalairachCenter.Y * xfrm.get(0, 1)) - (TalairachCenter.Z * xfrm.get(0, 2));
                final double Tr13 = rrArray[i].Y - (TalairachCenter.X * xfrm.get(1, 0))
                        - (TalairachCenter.Y * xfrm.get(1, 1)) - (TalairachCenter.Z * xfrm.get(1, 2));
                final double Tr23 = rrArray[i].Z - (TalairachCenter.X * xfrm.get(2, 0))
                        - (TalairachCenter.Y * xfrm.get(2, 1)) - (TalairachCenter.Z * xfrm.get(2, 2));

                /*
                 * Tr03 = xfrm.Get(0, 0) * Tx + xfrm.Get(0, 1) * Ty + xfrm.Get(0, 2) * Tz + xfrm.Get(0, 3) Tr13 =
                 * xfrm.Get(1, 0) * Tx + xfrm.Get(1, 1) * Ty + xfrm.Get(1, 2) * Tz + xfrm.Get(1, 3) Tr23 = xfrm.Get(2,
                 * 2) * Tx + xfrm.Get(2, 1) * Ty + xfrm.Get(2, 2) * Tz + xfrm.Get(2, 3)
                 */
                final JamaMatrix A = new JamaMatrix(3, 3);

                A.set(0, 0, xfrm.get(0, 0));
                A.set(0, 1, xfrm.get(0, 1));
                A.set(0, 2, xfrm.get(0, 2));
                A.set(1, 0, xfrm.get(1, 0));
                A.set(1, 1, xfrm.get(1, 1));
                A.set(1, 2, xfrm.get(1, 2));
                A.set(2, 0, xfrm.get(2, 0));
                A.set(2, 1, xfrm.get(2, 1));
                A.set(2, 2, xfrm.get(2, 2));

                final JamaMatrix b = new JamaMatrix(3, 1);

                b.set(0, 0, Tr03 - xfrm.get(0, 3));
                b.set(1, 0, Tr13 - xfrm.get(1, 3));
                b.set(2, 0, Tr23 - xfrm.get(2, 3));

                final JamaMatrix X = A.solve(b);
                final double Tx = X.get(0, 0);
                final double Ty = X.get(1, 0);
                final double Tz = X.get(2, 0);

                xfrm.setTranslate(Tx, Ty, Tz);
                transformTalairach(image, imgBuffer, xfrm, xResB, yResB, zResB, xDimB, yDimB, zDimB, tDimB, planeGap,
                        gapArray, doNN, xResA, yResA, zResA, xDimA, yDimA, zDimA, botX[i], botY[i], botZ[i], topX[i],
                        topY[i], topZ[i]);
            } // for (i = 0; i < 12; i++)

            image.calcMinMax();
            minimum = image.getMin();
            maximum = image.getMax();

            // After AFNI transformation to form Talairach image can no longer use gap information based on a
            // single x,y, or z location. Could mark individual pixels, but this would be very tedious
            // Simply stop using the gap information in further interpolation for now
            planeGap = -1;
            viewType = FileInfoAfni.AFNI_TLRC;

        }

        newAfniInfo = new FileInfoAfni[zDimA * tDimB];

        for (int i = 0; i < (zDimA * tDimB); i++) {
            newAfniInfo[i] = (FileInfoAfni) imageBInfo.clone();
            newAfniInfo[i].setUnitsOfMeasure(newResUnit);
            newAfniInfo[i].setResolutions(newResol);
            newAfniInfo[i].setExtents(newExtents);
            newAfniInfo[i].setMax(maximum);
            newAfniInfo[i].setMin(minimum);
            newAfniInfo[i].setPlaneGap(planeGap);
            newAfniInfo[i].setAFNIViewType(viewType);

            if (planeGap != -1) {
                newAfniInfo[i].setGapArray(gapArray);
            }
        }

        image.setFileInfo(newAfniInfo);
        afniProgressBar.dispose();
        setImageB(image);

        return 1;
    }

    /**
     * This routine is designed to transform AFNI functional images which have already been transformed to match AFNI
     * original images to match AFNI +acpc images.
     * 
     * <p>
     * AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by Robert W. Cox
     * states:
     * </p>
     * 
     * <blockquote>"In some applications, gaps are present between the functional slices. For example, some
     * investigators using sagittal functional images do not collect data that spans the longitudinal fissure, but
     * instead leave a 3-5 mm gap there. The auxiliary program abut can provide zero-filled images to fill in the gaps,
     * and can resample the nonzero slices to a finer mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm
     * functional slices). Resampling in the slice select direction between contiguous input slices can be done using
     * nearest- neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done across the
     * boundaries between the gap-filling zero images and the nonzero user-supplied images; that is, interpolation is
     * only done inside spatially contiguous blocks of actual input data."</blockquote>
     * 
     * <p>
     * A second special consideration exists in the interpolation of functional data. The MCW AFNI - User Manual states:
     * "In this version of AFNI, the threshold data (e.g., correlation coefficient) is always resampled using the
     * nearest neighbor method. This is because thresholding with an interpolated nonlinear statistic is a somewhat
     * dubious procedure." At another point the manual states: "Threshold data in functional datasets is always
     * resampled using the nearest neighbor mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear
     * statistic (such as correlation coefficient) between voxels, and then to interpret this statistic using
     * probabilistic models that assume independence."
     * </p>
     * 
     * <p>
     * Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is interpolated with
     * nearest neighbor interpolation.
     * </p>
     * 
     * @param image image being created
     * @param imgBuffer source image array
     * @param xfrm transformation matrix to be applied
     * @param iXres input x resolution
     * @param iYres input y resolution
     * @param iZres input z resolution
     * @param iXdim input x dimesnion
     * @param iYdim input y dimension
     * @param iZdim input z dimension
     * @param iTdim input t dimension
     * @param planeGap -1 if no zero filled gaps, 0 for x axis gaps, 1 for y axis, 2 for z axis
     * @param gapArray array of coordinates at which the zero filled planes occur
     * @param doNN true if functional, false if anatomical
     * @param oXres output x resolution
     * @param oYres output y resolution
     * @param oZres output z resolution
     * @param oXdim output x dimension
     * @param oYdim output y dimension
     * @param oZdim output z dimension
     */
    private void transformACPC(final ModelImage image, final float[] imgBuffer, final TransMatrix xfrm,
            final float iXres, final float iYres, final float iZres, final int iXdim, final int iYdim, final int iZdim,
            final int iTdim, final int planeGap, final int[] gapArray, boolean doNN, final float oXres,
            final float oYres, final float oZres, final int oXdim, final int oYdim, final int oZdim) {
        // This routine is designed to transform AFNI functional images which have already been
        // transformed to match AFNI original images to match AFNI +acpc images.

        // AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by
        // Robert W. Cox states: "In some applications, gaps are present between the functional slices.
        // For example, some investigators using sagittal functional images do not collect data that spans
        // the longitudinal fissure, but instead leave a 3-5 mm gap there. The auxiliary program abut can
        // provide zero-filled images to fill in the gaps, and can resample the nonzero slices to a finer
        // mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm functional slices). Resampling
        // in the slice select direction between contiguous input slices can be done using nearest-
        // neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done
        // across the boundaries between the gap-filling zero images and the nonzero user-supplied images;
        // that is, interpolation is only done inside spatially contiguous blocks of actual input data."

        // A second special consideration exists in the interpolation of functional data. The MCW AFNI -
        // User Manual states: "In this version of AFNI, the threshold data (e.g., correlation coefficient)
        // is always resampled using the nearest neighbor method. This is because thresholding with an
        // interpolated nonlinear statistic is a somewhat dubious procedure." At another point the manual
        // states: "Threshold data in functional datasets is always resampled using the nearest neighbor
        // mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear statistic (such
        // as correlation coefficient) between voxels, and then to interpret this statistic using
        // probabilistic models that assume independence."
        // Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is
        // interpolated with nearest neighbor interpolation.

        int i, j, k, t, g;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        float value;
        int sliceSize;
        int volSize;
        float imm, jmm, kmm;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3, temp4, temp5, temp6, temp7;
        int roundX, roundY, roundZ;

        sliceSize = iXdim * iYdim;
        volSize = sliceSize * iZdim;

        int tOffset;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        boolean interp;
        int xOffset, yOffset, zOffset;

        final int mod = oXdim / 50;
        int tLast;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        tLast = Math.max(1, iTdim);

        if ( !doNN) {

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = 0; i < oXdim; i++) {

                    if ( (i % mod) == 0) {
                        progressBar
                                .updateValue(
                                        (int) ( ( ((float) ( (t * oXdim) + i) / ( ( (iTdim - 1) * oXdim) + (oXdim - 1))) * 100) + .5),
                                        true);
                    }

                    imm = i * oXres;
                    i1 = (imm * T00) + T03;
                    i2 = (imm * T10) + T13;
                    i3 = (imm * T20) + T23;

                    for (j = 0; j < oYdim; j++) {
                        jmm = j * oYres;
                        j1 = jmm * T01;
                        j2 = jmm * T11;
                        j3 = jmm * T21;
                        temp1 = i3 + j3;
                        temp2 = i2 + j2;
                        temp3 = i1 + j1;

                        for (k = 0; k < oZdim; k++) {

                            // transform i,j,k
                            value = 0; // remains zero if voxel is transformed out of bounds
                            kmm = k * oZres;
                            X = (temp3 + (kmm * T02)) / iXres;
                            roundX = (int) (X + 0.5f);

                            if ( (X >= 0) && (roundX < iXdim)) {
                                Y = (temp2 + (kmm * T12)) / iYres;
                                roundY = (int) (Y + 0.5f);

                                if ( (Y >= 0) && (roundY < iYdim)) {
                                    Z = (temp1 + (kmm * T22)) / iZres;
                                    roundZ = (int) (Z + 0.5f);

                                    if ( (Z >= 0) && (roundZ < iZdim)) {

                                        if ( ((int) X < (iXdim - 1)) && ((int) Y < (iYdim - 1))
                                                && ((int) Z < (iZdim - 1))) {

                                            // set intensity of i,j,k to new transformed coordinate if
                                            // x,y,z is w/in dimensions of image
                                            x0 = X - (int) X;
                                            y0 = Y - (int) Y;
                                            z0 = Z - (int) Z;
                                            x1 = 1 - x0;
                                            y1 = 1 - y0;
                                            z1 = 1 - z0;
                                            X0pos = (int) X;
                                            Y0pos = (int) Y * iXdim;
                                            Z0pos = (int) Z * sliceSize;
                                            X1pos = X0pos + 1;
                                            Y1pos = Y0pos + iXdim;
                                            Z1pos = Z0pos + sliceSize;
                                            interp = true;

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            if (planeGap != -1) {

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == X0pos)
                                                                || ( (gapArray[g] == X1pos) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                temp4 = y1 * z1;
                                                temp5 = y0 * z1;
                                                temp6 = y1 * z0;
                                                temp7 = y0 * z0;
                                                value = (x1 * temp4 * imgBuffer[Z0pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp4 * imgBuffer[Z0pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp5 * imgBuffer[Z0pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp5 * imgBuffer[Z0pos + Y1pos + X1pos + tOffset])
                                                        + (x1 * temp6 * imgBuffer[Z1pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp6 * imgBuffer[Z1pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp7 * imgBuffer[Z1pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp7 * imgBuffer[Z1pos + Y1pos + X1pos + tOffset]);
                                            } // if (interp)
                                        } else {

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            interp = true;

                                            if (planeGap != -1) {
                                                x0 = X - (int) X;
                                                y0 = Y - (int) Y;
                                                z0 = Z - (int) Z;

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) X)
                                                                || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                X0pos = roundX;
                                                Y0pos = roundY * iXdim;
                                                Z0pos = roundZ * sliceSize;
                                                value = imgBuffer[Z0pos + Y0pos + X0pos + tOffset];
                                            } // if (interp)
                                        }
                                    } // end if Z in bounds
                                } // end if Y in bounds
                            } // end if X in bounds

                            if (image.getNDims() == 3) {
                                image.set(i, j, k, value);
                            } else {
                                image.set(i, j, k, t, value);
                            }
                        } // end for k
                    } // end for j
                } // end for i
            } // for (t = 0; t < tLast; t++)
        } // if (!doNN)
        else { // doNN

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = 0; i < oXdim; i++) {

                    if ( (i % mod) == 0) {
                        progressBar
                                .updateValue(
                                        (int) ( ( ((float) ( (t * oXdim) + i) / ( ( (iTdim - 1) * oXdim) + (oXdim - 1))) * 100) + .5),
                                        true);
                    }

                    imm = i * oXres;

                    for (j = 0; j < oYdim; j++) {
                        jmm = j * oYres;

                        for (k = 0; k < oZdim; k++) {
                            value = 0.0f; // remains zero if voxel is transformed out of bounds

                            // transform i,j,k
                            kmm = k * oZres;

                            X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            X = X / iXres;
                            Y = Y / iYres;
                            Z = Z / iZres;

                            roundX = (int) (X + 0.5f);
                            roundY = (int) (Y + 0.5f);
                            roundZ = (int) (Z + 0.5f);

                            if ( (roundX < 0) || (roundX > (iXdim - 1)) || (roundY < 0) || (roundY > (iYdim - 1))
                                    || (roundZ < 0) || (roundZ > (iZdim - 1))) {} else {
                                interp = true;

                                // If values have not been collected in the original functional image
                                // and the gaps are filled in with zeroes, then set the value equal
                                // to zero if any of the values are in the area of no data collection.
                                if (planeGap != -1) {
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    z0 = Z - (int) Z;

                                    if (planeGap == 0) {

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) X)
                                                    || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } else if (planeGap == 1) { // if (planeGap == 0)

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Y)
                                                    || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } else { // else if (planeGap == 1), planeGap == 2

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Z)
                                                    || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } // else for planegap == 2
                                } // if (planeGap != -1)

                                if (interp) {
                                    xOffset = roundX;
                                    yOffset = roundY * iXdim;
                                    zOffset = roundZ * sliceSize;
                                    value = imgBuffer[xOffset + yOffset + zOffset + tOffset];
                                }
                            }

                            if (image.getNDims() == 3) {
                                image.set(i, j, k, value);
                            } else {
                                image.set(i, j, k, t, value);
                            }
                        } // for (k=0; k < oZdim; k++)
                    } // for (j=0; j < oYdim; j++)
                } // for (i=0; i < oXdim; i++)
            } // for (t = 0; t < tLast; t++)
        } // else doNN
    }

    /**
     * This routine is designed to transform AFNI functional images and AFNI 3D + time anatomical images to match AFNI
     * original 3D anatomical images. Here the 3 axes of the 2 images are parallel and the spatial orientation of the 2
     * sets of axes are identical.
     * 
     * <p>
     * AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by Robert W. Cox
     * states:
     * </p>
     * 
     * <blockquote>&quot;In some applications, gaps are present between the functional slices. For example, some
     * investigators using sagittal functional images do not collect data that spans the longitudinal fissure, but
     * instead leave a 3-5 mm gap there. The auxiliary program abut can provide zero-filled images to fill in the gaps,
     * and can resample the nonzero slices to a finer mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm
     * functional slices). Resampling in the slice select direction between contiguous input slices can be done using
     * nearest- neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done across the
     * boundaries between the gap-filling zero images and the nonzero user-supplied images; that is, interpolation is
     * only done inside spatially contiguous blocks of actual input data.&quot;</blockquote>
     * 
     * <p>
     * A second special consideration exists in the interpolation of functional data. The MCW AFNI - User Manual states:
     * &quot;In this version of AFNI, the threshold data (e.g., correlation coefficient) is always resampled using the
     * nearest neighbor method. This is because thresholding with an interpolated nonlinear statistic is a somewhat
     * dubious procedure.&quot;
     * </p>
     * 
     * <p>
     * At another point the manual states: &quot;Threshold data in functional datasets is always resampled using the
     * nearest neighbor mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear statistic (such as
     * correlation coefficient) between voxels, and then to interpret this statistic using probabilistic models that
     * assume independence.&quot;
     * </p>
     * 
     * <p>
     * Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is interpolated with
     * nearest neighbor interpolation.
     * </p>
     * 
     * @param image image being created
     * @param imgBuffer source image array
     * @param iXres input x resolution
     * @param iYres input y resolution
     * @param iZres input z resolution
     * @param iXdim input x dimesnion
     * @param iYdim input y dimension
     * @param iZdim input z dimension
     * @param iTdim input t dimension
     * @param ilowXmm lowest valid input x location in space in millimeters
     * @param ilowYmm lowest valid input y location in space in millimeters
     * @param ilowZmm lowest valid input z location in space in millimeters
     * @param ihighXmm highest valid input x location in space in millimeters
     * @param ihighYmm highest valid input y location in space in millimeters
     * @param ihighZmm highest valid input z location in space in millimeters
     * @param planeGap -1 if no zero filled gaps, 0 for x axis gaps, 1 for y axis, 2 for z axis
     * @param gapArray array of coordinates at which the zero filled planes occur
     * @param doNN true if functional, false if anatomical
     * @param oXres output x resolution
     * @param oYres output y resolution
     * @param oZres output z resolution
     * @param oXdim output x dimension
     * @param oYdim output y dimension
     * @param oZdim output z dimension
     * @param olowXmm lowest valid output x location in space in millimeters
     * @param olowYmm lowest valid output y location in space in millimeters
     * @param olowZmm lowest valid output z location in space in millimeters
     * @param ohighXmm highest valid output x location in space in millimeters
     * @param ohighYmm highest valid output y location in space in millimeters
     * @param ohighZmm highest valid output z location in space in millimeters
     */
    private void transformAFNI(final ModelImage image, final float[] imgBuffer, final float iXres, final float iYres,
            final float iZres, final int iXdim, final int iYdim, final int iZdim, final int iTdim, final float ilowXmm,
            final float ilowYmm, final float ilowZmm, final float ihighXmm, final float ihighYmm, final float ihighZmm,
            final int planeGap, final int[] gapArray, boolean doNN, final float oXres, final float oYres,
            final float oZres, final int oXdim, final int oYdim, final int oZdim, final float olowXmm,
            final float olowYmm, final float olowZmm, final float ohighXmm, final float ohighYmm, final float ohighZmm) {
        // This routine is designed to transform AFNI functional images and AFNI 3D + time anatomical images to match
        // AFNI original 3D anatomical images. Here the 3 axes of the 2 images are parallel and the spatial orientation
        // of the 2 sets of axes are identical.

        // AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by
        // Robert W. Cox states: "In some applications, gaps are present between the functional slices.
        // For example, some investigators using sagittal functional images do not collect data that spans
        // the longitudinal fissure, but instead leave a 3-5 mm gap there. The auxiliary program abut can
        // provide zero-filled images to fill in the gaps, and can resample the nonzero slices to a finer
        // mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm functional slices). Resampling
        // in the slice select direction between contiguous input slices can be done using nearest-
        // neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done
        // across the boundaries between the gap-filling zero images and the nonzero user-supplied images;
        // that is, interpolation is only done inside spatially contiguous blocks of actual input data."

        // A second special consideration exists in the interpolation of functional data. The MCW AFNI -
        // User Manual states: "In this version of AFNI, the threshold data (e.g., correlation coefficient)
        // is always resampled using the nearest neighbor method. This is because thresholding with an
        // interpolated nonlinear statistic is a somewhat dubious procedure." At another point the manual
        // states: "Threshold data in functional datasets is always resampled using the nearest neighbor
        // mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear statistic (such
        // as correlation coefficient) between voxels, and then to interpret this statistic using
        // probabilistic models that assume independence."
        // Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is
        // interpolated with nearest neighbor interpolation.

        int i, j, k, t, g;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        float value;
        int sliceSize;
        int volSize;
        float imm, jmm, kmm;
        float temp4, temp5, temp6, temp7;
        int roundX, roundY, roundZ;

        sliceSize = iXdim * iYdim;
        volSize = sliceSize * iZdim;

        int tOffset;
        boolean interp;
        int xOffset, yOffset, zOffset;

        // absolute output locations in space in millimeters
        float iloc, jloc, kloc;
        float xloc, yloc, zloc;
        float xloc0, yloc0, zloc0;
        float xloc1, yloc1, zloc1;
        int tLast;

        final int mod = oXdim / 50;

        tLast = Math.max(1, iTdim);

        if ( !doNN) {

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = 0; i < oXdim; i++) {

                    if ( (i % mod) == 0) {
                        progressBar
                                .updateValue(
                                        (int) ( ( ((float) ( (t * oXdim) + i) / ( ( (iTdim - 1) * oXdim) + (oXdim - 1))) * 100) + .5),
                                        true);
                    }

                    imm = i * oXres;
                    iloc = olowXmm + imm;

                    for (j = 0; j < oYdim; j++) {
                        jmm = j * oYres;
                        jloc = olowYmm + jmm;

                        for (k = 0; k < oZdim; k++) {

                            // transform i,j,k
                            value = 0; // remains zero if voxel is transformed out of bounds
                            kmm = k * oZres;
                            kloc = olowZmm + kmm;
                            X = (iloc - ilowXmm) / iXres;
                            roundX = (int) (X + 0.5f);
                            xloc = (roundX * iXres) + ilowXmm;
                            xloc0 = (X * iXres) + ilowXmm;
                            xloc1 = xloc0 + iXres;

                            if ( (X >= 0) && (roundX < iXdim)) {
                                Y = (jloc - ilowYmm) / iYres;
                                roundY = (int) (Y + 0.5f);
                                yloc = (roundY * iYres) + ilowYmm;
                                yloc0 = (Y * iYres) + ilowYmm;
                                yloc1 = yloc0 + iYres;

                                if ( (Y >= 0) && (roundY < iYdim)) {
                                    Z = (kloc - ilowZmm) / iZres;
                                    roundZ = (int) (Z + 0.5f);
                                    zloc = (roundZ * iZres) + ilowZmm;
                                    zloc0 = (Z * iZres) + ilowZmm;
                                    zloc1 = zloc0 + iZres;

                                    if ( (Z >= 0) && (roundZ < iZdim)) {

                                        if ( ((int) X < (iXdim - 1)) && ((int) Y < (iYdim - 1))
                                                && ((int) Z < (iZdim - 1)) && (xloc0 >= olowXmm) && (xloc1 <= ohighXmm)
                                                && (yloc0 >= olowYmm) && (yloc1 <= ohighYmm) && (zloc0 >= olowZmm)
                                                && (zloc1 <= ohighZmm)) {

                                            // set intensity of i,j,k to new transformed coordinate if
                                            // x,y,z is w/in dimensions of image
                                            x0 = X - (int) X;
                                            y0 = Y - (int) Y;
                                            z0 = Z - (int) Z;
                                            x1 = 1 - x0;
                                            y1 = 1 - y0;
                                            z1 = 1 - z0;
                                            X0pos = (int) X;
                                            Y0pos = (int) Y * iXdim;
                                            Z0pos = (int) Z * sliceSize;
                                            X1pos = X0pos + 1;
                                            Y1pos = Y0pos + iXdim;
                                            Z1pos = Z0pos + sliceSize;
                                            interp = true;

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            if (planeGap != -1) {

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == X0pos)
                                                                || ( (gapArray[g] == X1pos) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                temp4 = y1 * z1;
                                                temp5 = y0 * z1;
                                                temp6 = y1 * z0;
                                                temp7 = y0 * z0;
                                                value = (x1 * temp4 * imgBuffer[Z0pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp4 * imgBuffer[Z0pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp5 * imgBuffer[Z0pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp5 * imgBuffer[Z0pos + Y1pos + X1pos + tOffset])
                                                        + (x1 * temp6 * imgBuffer[Z1pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp6 * imgBuffer[Z1pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp7 * imgBuffer[Z1pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp7 * imgBuffer[Z1pos + Y1pos + X1pos + tOffset]);
                                            } // if (interp)
                                        } else if ( ( (roundX == (iXdim - 1)) || (roundY == (iYdim - 1)) || (roundZ == (iZdim - 1)))
                                                && ( (xloc >= olowXmm) && (xloc <= ohighXmm) && (yloc >= olowYmm)
                                                        && (yloc <= olowYmm) && (zloc >= olowYmm) && (zloc <= ohighZmm))) {

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            interp = true;

                                            if (planeGap != -1) {
                                                x0 = X - (int) X;
                                                y0 = Y - (int) Y;
                                                z0 = Z - (int) Z;

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) X)
                                                                || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                X0pos = roundX;
                                                Y0pos = roundY * iXdim;
                                                Z0pos = roundZ * sliceSize;
                                                value = imgBuffer[Z0pos + Y0pos + X0pos + tOffset];
                                            } // if (interp)
                                        }
                                    } // end if Z in bounds
                                } // end if Y in bounds
                            } // end if X in bounds

                            if (image.getNDims() == 3) {
                                image.set(i, j, k, value);
                            } else {
                                image.set(i, j, k, t, value);
                            }
                        } // end for k
                    } // end for j
                } // end for i
            } // for (t = 0; t < tLast; t++)
        } // if (!doNN)
        else { // doNN

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = 0; i < oXdim; i++) {

                    if ( (i % mod) == 0) {
                        progressBar
                                .updateValue(
                                        (int) ( ( ((float) ( (t * oXdim) + i) / ( ( (iTdim - 1) * oXdim) + (oXdim - 1))) * 100) + .5),
                                        true);
                    }

                    imm = i * oXres;
                    iloc = imm + olowXmm;

                    for (j = 0; j < oYdim; j++) {
                        jmm = j * oYres;
                        jloc = jmm + olowYmm;

                        for (k = 0; k < oZdim; k++) {
                            value = 0.0f; // remains zero if voxel is transformed out of bounds

                            // transform i,j,k
                            kmm = k * oZres;
                            kloc = kmm + olowZmm;

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            X = (iloc - ilowXmm) / iXres;
                            Y = (jloc - ilowYmm) / iYres;
                            Z = (kloc - ilowZmm) / iZres;

                            roundX = (int) (X + 0.5f);
                            roundY = (int) (Y + 0.5f);
                            roundZ = (int) (Z + 0.5f);
                            xloc = (roundX * iXres) + ilowXmm;
                            yloc = (roundY * iYres) + ilowYmm;
                            zloc = (roundZ * iZres) + ilowZmm;

                            if ( (roundX < 0) || (roundX > (iXdim - 1)) || (roundY < 0) || (roundY > (iYdim - 1))
                                    || (roundZ < 0) || (roundZ > (iZdim - 1)) || (xloc < olowXmm) || (xloc > ohighXmm)
                                    || (yloc < olowYmm) || (yloc > ohighYmm) || (zloc < olowZmm) || (zloc > ohighZmm)) {} else {
                                interp = true;

                                // If values have not been collected in the original functional image
                                // and the gaps are filled in with zeroes, then set the value equal
                                // to zero if any of the values are in the area of no data collection.
                                if (planeGap != -1) {
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    z0 = Z - (int) Z;

                                    if (planeGap == 0) {

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) X)
                                                    || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } else if (planeGap == 1) { // if (planeGap == 0)

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Y)
                                                    || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } else { // else if (planeGap == 1), planeGap == 2

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Z)
                                                    || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                interp = false;
                                            }
                                        }
                                    } // else for planegap == 2
                                } // if (planeGap != -1)

                                if (interp) {
                                    xOffset = roundX;
                                    yOffset = roundY * iXdim;
                                    zOffset = roundZ * sliceSize;
                                    value = imgBuffer[xOffset + yOffset + zOffset + tOffset];
                                }
                            }

                            if (image.getNDims() == 3) {
                                image.set(i, j, k, value);
                            } else {
                                image.set(i, j, k, t, value);
                            }
                        } // for (k=0; k < oZdim; k++)
                    } // for (j=0; j < oYdim; j++)
                } // for (i=0; i < oXdim; i++)
            } // for (t = 0; t < tLast; t++)
        } // else doNN
    }

    /**
     * This routine is designed to transform AFNI functional images which have already been transformed to match AFNI
     * original images to match AFNI +tlrc images.
     * 
     * <p>
     * AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by Robert W. Cox
     * states:
     * </p>
     * 
     * <blockquote>"In some applications, gaps are present between the functional slices. For example, some
     * investigators using sagittal functional images do not collect data that spans the longitudinal fissure, but
     * instead leave a 3-5 mm gap there. The auxiliary program abut can provide zero-filled images to fill in the gaps,
     * and can resample the nonzero slices to a finer mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm
     * functional slices). Resampling in the slice select direction between contiguous input slices can be done using
     * nearest- neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done across the
     * boundaries between the gap-filling zero images and the nonzero user-supplied images; that is, interpolation is
     * only done inside spatially contiguous blocks of actual input data."</blockquote>
     * 
     * <p>
     * A second special consideration exists in the interpolation of functional data. The MCW AFNI - User Manual states:
     * "In this version of AFNI, the threshold data (e.g., correlation coefficient) is always resampled using the
     * nearest neighbor method. This is because thresholding with an interpolated nonlinear statistic is a somewhat
     * dubious procedure." At another point the manual states: "Threshold data in functional datasets is always
     * resampled using the nearest neighbor mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear
     * statistic (such as correlation coefficient) between voxels, and then to interpret this statistic using
     * probabilistic models that assume independence."
     * </p>
     * 
     * <P>
     * Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is interpolated with
     * nearest neighbor interpolation.
     * </P>
     * 
     * @param image image being created
     * @param imgBuffer source image array
     * @param xfrm transformation matrix to be applied
     * @param iXres input x resolution
     * @param iYres input y resolution
     * @param iZres input z resolution
     * @param iXdim input x dimesnion
     * @param iYdim input y dimension
     * @param iZdim input z dimension
     * @param iTdim input t dimension
     * @param planeGap -1 if no zero filled gaps, 0 for x axis gaps, 1 for y axis, 2 for z axis
     * @param gapArray array of coordinates at which the zero filled planes occur
     * @param doNN true if functional, false if anatomical
     * @param oXres output x resolution
     * @param oYres output y resolution
     * @param oZres output z resolution
     * @param oXdim output x dimension
     * @param oYdim output y dimension
     * @param oZdim output z dimension
     * @param botX lowest x output value in this 1 of the 12 Talairach regions
     * @param botY lowest y output value in this 1 of the 12 Talairach regions
     * @param botZ lowest z output value in this 1 of the 12 Talairach regions
     * @param topX highest x output value in this 1 of the 12 Talairach regions
     * @param topY highest y output value in this 1 of the 12 Talairach regions
     * @param topZ highest z output value in this 1 of the 12 Talairach regions
     */
    private void transformTalairach(final ModelImage image, final float[] imgBuffer, final TransMatrix xfrm,
            final float iXres, final float iYres, final float iZres, final int iXdim, final int iYdim, final int iZdim,
            final int iTdim, final int planeGap, final int[] gapArray, boolean doNN, final float oXres,
            final float oYres, final float oZres, final int oXdim, final int oYdim, final int oZdim, final int botX,
            final int botY, final int botZ, final int topX, final int topY, final int topZ) {
        // This routine is designed to transform AFNI functional images which have already been
        // transformed to match AFNI original images to match AFNI +tlrc images.

        // AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by
        // Robert W. Cox states: "In some applications, gaps are present between the functional slices.
        // For example, some investigators using sagittal functional images do not collect data that spans
        // the longitudinal fissure, but instead leave a 3-5 mm gap there. The auxiliary program abut can
        // provide zero-filled images to fill in the gaps, and can resample the nonzero slices to a finer
        // mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm functional slices). Resampling
        // in the slice select direction between contiguous input slices can be done using nearest-
        // neighbor, linear, or cubic interpolation, at the user's discretion. Interpolation is not done
        // across the boundaries between the gap-filling zero images and the nonzero user-supplied images;
        // that is, interpolation is only done inside spatially contiguous blocks of actual input data."

        // A second special consideration exists in the interpolation of functional data. The MCW AFNI -
        // User Manual states: "In this version of AFNI, the threshold data (e.g., correlation coefficient)
        // is always resampled using the nearest neighbor method. This is because thresholding with an
        // interpolated nonlinear statistic is a somewhat dubious procedure." At another point the manual
        // states: "Threshold data in functional datasets is always resampled using the nearest neighbor
        // mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear statistic (such
        // as correlation coefficient) between voxels, and then to interpret this statistic using
        // probabilistic models that assume independence."
        // Thus, the intensity data is interpolated with trilinear interpolation and the threshold data is
        // interpolated with nearest neighbor interpolation.
        int i, j, k, t, g;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        float value;
        int sliceSize;
        int volSize;
        float imm, jmm, kmm;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3, temp4, temp5, temp6, temp7;
        int roundX, roundY, roundZ;

        sliceSize = iXdim * iYdim;
        volSize = sliceSize * iZdim;

        int tOffset;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        boolean doTransform;
        boolean interp;
        int xOffset, yOffset, zOffset;

        final int mod = oXdim / 50;
        int tLast;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        tLast = Math.max(1, iTdim);

        if ( !doNN) {

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = botX; i <= topX; i++) {

                    if ( (i % mod) == 0) {
                        progressBar.updateValue((int) ( ((float) ( (t * (topX - botX + 1)) + (i - botX))
                                / ( ( (iTdim - 1) * (topX - botX + 1)) + (topX - botX)) * 100) + .5), true);
                    }

                    imm = i * oXres;
                    i1 = (imm * T00) + T03;
                    i2 = (imm * T10) + T13;
                    i3 = (imm * T20) + T23;

                    for (j = botY; j <= topY; j++) {
                        jmm = j * oYres;
                        j1 = jmm * T01;
                        j2 = jmm * T11;
                        j3 = jmm * T21;
                        temp1 = i3 + j3;
                        temp2 = i2 + j2;
                        temp3 = i1 + j1;

                        for (k = botZ; k <= topZ; k++) {

                            // transform i,j,k
                            value = 0; // remains zero if voxel is transformed out of bounds
                            doTransform = false;
                            kmm = k * oZres;
                            X = (temp3 + (kmm * T02)) / iXres;
                            roundX = (int) (X + 0.5f);

                            if ( (X >= 0) && (roundX < iXdim)) {
                                Y = (temp2 + (kmm * T12)) / iYres;
                                roundY = (int) (Y + 0.5f);

                                if ( (Y >= 0) && (roundY < iYdim)) {
                                    Z = (temp1 + (kmm * T22)) / iZres;
                                    roundZ = (int) (Z + 0.5f);

                                    if ( (Z >= 0) && (roundZ < iZdim)) {

                                        if ( ((int) X < (iXdim - 1)) && ((int) Y < (iYdim - 1))
                                                && ((int) Z < (iZdim - 1))) {

                                            // set intensity of i,j,k to new transformed coordinate if
                                            // x,y,z is w/in dimensions of image
                                            x0 = X - (int) X;
                                            y0 = Y - (int) Y;
                                            z0 = Z - (int) Z;
                                            x1 = 1 - x0;
                                            y1 = 1 - y0;
                                            z1 = 1 - z0;
                                            X0pos = (int) X;
                                            Y0pos = (int) Y * iXdim;
                                            Z0pos = (int) Z * sliceSize;
                                            X1pos = X0pos + 1;
                                            Y1pos = Y0pos + iXdim;
                                            Z1pos = Z0pos + sliceSize;
                                            interp = true;

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            if (planeGap != -1) {

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == X0pos)
                                                                || ( (gapArray[g] == X1pos) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                temp4 = y1 * z1;
                                                temp5 = y0 * z1;
                                                temp6 = y1 * z0;
                                                temp7 = y0 * z0;
                                                value = (x1 * temp4 * imgBuffer[Z0pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp4 * imgBuffer[Z0pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp5 * imgBuffer[Z0pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp5 * imgBuffer[Z0pos + Y1pos + X1pos + tOffset])
                                                        + (x1 * temp6 * imgBuffer[Z1pos + Y0pos + X0pos + tOffset])
                                                        + (x0 * temp6 * imgBuffer[Z1pos + Y0pos + X1pos + tOffset])
                                                        + (x1 * temp7 * imgBuffer[Z1pos + Y1pos + X0pos + tOffset])
                                                        + (x0 * temp7 * imgBuffer[Z1pos + Y1pos + X1pos + tOffset]);
                                                doTransform = true;
                                            } // if (interp)
                                        } else {

                                            // If values have not been collected in the original functional image
                                            // and the gaps are filled in with zeroes, then set the value equal
                                            // to zero if any of the values are in the area of no data collection.
                                            interp = true;

                                            if (planeGap != -1) {
                                                x0 = X - (int) X;
                                                y0 = Y - (int) Y;
                                                z0 = Z - (int) Z;

                                                if (planeGap == 0) {

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) X)
                                                                || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } else if (planeGap == 1) { // if (planeGap == 0)

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Y)
                                                                || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } else { // else if (planeGap == 1), planeGap == 2

                                                    for (g = 0; (g < gapArray.length) && interp; g++) {

                                                        if ( (gapArray[g] == (int) Z)
                                                                || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                            interp = false;
                                                            value = 0.0f;
                                                            doTransform = true;
                                                        }
                                                    }
                                                } // else for planegap == 2
                                            } // if (planeGap != -1)

                                            if (interp) {
                                                X0pos = roundX;
                                                Y0pos = roundY * iXdim;
                                                Z0pos = roundZ * sliceSize;
                                                value = imgBuffer[Z0pos + Y0pos + X0pos + tOffset];
                                                doTransform = true;
                                            } // if (interp)
                                        }
                                    } // end if Z in bounds
                                } // end if Y in bounds
                            } // end if X in bounds

                            if (doTransform) {

                                if (image.getNDims() > 3) {
                                    image.set(i, j, k, t, value);
                                } else {
                                    image.set(i, j, k, value);
                                }
                            }
                        } // for (k=botZ; k <= topZ; k++)
                    } // for (j=botY; j <= topY; j++)
                } // for (i=botX; i <= topX; i++)
            } // for (t = 0; t < tLast; t++)
        } // if (!doNN)
        else { // doNN

            for (t = 0; t < tLast; t++) {
                tOffset = t * volSize;

                for (i = botX; i <= topX; i++) {

                    if ( (i % mod) == 0) {
                        progressBar.updateValue((int) ( ((float) ( (t * (topX - botX + 1)) + (i - botX))
                                / ( ( (iTdim - 1) * (topX - botX + 1)) + (topX - botX)) * 100) + .5), true);
                    }

                    imm = i * oXres;

                    for (j = botY; j <= topY; j++) {
                        jmm = j * oYres;

                        for (k = botZ; k <= topZ; k++) {
                            value = 0.0f; // remains zero if voxel is transformed out of bounds
                            doTransform = false;

                            // transform i,j,k
                            kmm = k * oZres;

                            X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            X = X / iXres;
                            Y = Y / iYres;
                            Z = Z / iZres;

                            roundX = (int) (X + 0.5f);
                            roundY = (int) (Y + 0.5f);
                            roundZ = (int) (Z + 0.5f);

                            if ( (roundX < 0) || (roundX > (iXdim - 1)) || (roundY < 0) || (roundY > (iYdim - 1))
                                    || (roundZ < 0) || (roundZ > (iZdim - 1))) {} else {
                                interp = true;

                                // If values have not been collected in the original functional image
                                // and the gaps are filled in with zeroes, then set the value equal
                                // to zero if any of the values are in the area of no data collection.
                                if (planeGap != -1) {
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    z0 = Z - (int) Z;

                                    if (planeGap == 0) {

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) X)
                                                    || ( (gapArray[g] == (int) (X + 1)) && (x0 != 0.0f))) {
                                                interp = false;
                                                doTransform = true;
                                            }
                                        }
                                    } else if (planeGap == 1) { // if (planeGap == 0)

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Y)
                                                    || ( (gapArray[g] == (int) (Y + 1)) && (y0 != 0.0f))) {
                                                interp = false;
                                                doTransform = true;
                                            }
                                        }
                                    } else { // else if (planeGap == 1), planeGap == 2

                                        for (g = 0; (g < gapArray.length) && interp; g++) {

                                            if ( (gapArray[g] == (int) Z)
                                                    || ( (gapArray[g] == (int) (Z + 1)) && (z0 != 0.0f))) {
                                                interp = false;
                                                doTransform = true;
                                            }
                                        }
                                    } // else for planegap == 2
                                } // if (planeGap != -1)

                                if (interp) {
                                    xOffset = roundX;
                                    yOffset = roundY * iXdim;
                                    zOffset = roundZ * sliceSize;
                                    value = imgBuffer[xOffset + yOffset + zOffset + tOffset];
                                    doTransform = true;
                                }
                            }

                            if (doTransform) {

                                if (image.getNDims() > 3) {
                                    image.set(i, j, k, t, value);
                                } else {
                                    image.set(i, j, k, value);
                                }
                            }
                        } // for (k=botZ; k <= topZ; k++)
                    } // for (j=botY; j <= topY; j++)
                } // for (i=botX; i <= topX; i++)
            } // for (t = 0; t < tLast; t++)
        } // else doNN
    }

}
