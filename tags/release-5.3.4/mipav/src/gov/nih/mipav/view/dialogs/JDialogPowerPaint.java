package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog box for the paint power tools: morphology operations, object delete, etc.
 *
 * @version  May 2005
 * @author   Pierre-Louis Bazin
 * @see      JDialogBase
 * @see      AlgorithmInterface
 */
public class JDialogPowerPaint extends JDialogBase
        implements MouseListener, MouseWheelListener, KeyListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6175130808453006120L;

    /** DOCUMENT ME! */
    private static final int MaxObject = 500000;

    /** DOCUMENT ME! */
    private static int NONE = 0;

    /** DOCUMENT ME! */
    private static int BACKGROUND = 1;

    /** DOCUMENT ME! */
    private static int ALLBACKGROUNDS = 2;

    /** DOCUMENT ME! */
    private static int REMOVE = 3;

    /** DOCUMENT ME! */
    private static int REMOVEALL = 4;

    /** DOCUMENT ME! */
    private static int GROWREGION = 5;

    /** DOCUMENT ME! */
    private static int XY = 0;
    // private static int XY = ViewJComponentBase.AXIAL;

    /** DOCUMENT ME! */
    private static int XZ = 1;
    // private static int XZ = ViewJComponentBase.CORONAL;

    /** DOCUMENT ME! */
    private static int ZY = 2;
    // private static int ZY = ViewJComponentBase.SAGITTAL;

    /** autosave elements. */
    private static int delay = 10;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int backgroundDim = 2;

    /** DOCUMENT ME! */
    private int backgroundsDim = 2;

    /** DOCUMENT ME! */
    private JPanel botPanel;

    /** DOCUMENT ME! */
    private JButton buttonDilate;

    /** DOCUMENT ME! */
    private JButton buttonErode;

    /** DOCUMENT ME! */
    private JButton buttonExportToMask;

    /** DOCUMENT ME! */
    private JButton buttonExportToVOI;

    /** DOCUMENT ME! */
    private JButton buttonFillBackground;

    /** DOCUMENT ME! */
    private JButton buttonFillBackgrounds;

    /** DOCUMENT ME! */
    private JButton buttonGrowRegion;

    /** DOCUMENT ME! */
    private JButton buttonImportFromMask;

    /** DOCUMENT ME! */
    private JButton buttonImportFromVOI;

    /** DOCUMENT ME! */
    private JButton buttonRevert;

    /** DOCUMENT ME! */
    private JButton buttonRmObject;

    /** DOCUMENT ME! */
    private JButton buttonRmObjects;

    /** DOCUMENT ME! */
    private JToggleButton buttonShortkeys;

    /** DOCUMENT ME! */
    private int c2x, c2y, c2z; // sturucturing element dimensions (2D)

    /** DOCUMENT ME! */
    private int c3x, c3y, c3z; // sturucturing element dimensions (3D)

    /** DOCUMENT ME! */
    private JCheckBox checkSave;

    /** handling of intensity threshold. */
    private JCheckBox checkThreshold;

   /** DOCUMENT ME! */
    private JComboBox comboConnectType;

    /** DOCUMENT ME! */
    private JComboBox comboDilateDimType;

    /** DOCUMENT ME! */
    private JComboBox comboErodeDimType;

    /** DOCUMENT ME! */
    private JComboBox comboStructureType;

    /** DOCUMENT ME! */
    private String connectType = "6/26";

    /** DOCUMENT ME! */
    private String[] connectTypes = { "6/18", "6/26", "18/6", "26/6" };

    /** DOCUMENT ME! */
    private String dilateDimType = "3D";

    /** DOCUMENT ME! */
    private String[] dimTypes = {
        "3D", "2.5D", "2D", "-triplanar-", "2.5D(XY)", "2.5D(XZ)", "2.5D(YZ)", "2D(XY)", "2D(XZ)", "2D(YZ)"
    };

    /** DOCUMENT ME! */
    private String erodeDimType = "3D";

    /** DOCUMENT ME! */
    private JPanel exportPanel;

    /** DOCUMENT ME! */
    private int getMouseInput = NONE;

    /** DOCUMENT ME! */
    private ButtonGroup groupBackground;

    /** DOCUMENT ME! */
    private ButtonGroup groupBackgrounds;

    /** DOCUMENT ME! */
    private ButtonGroup groupGrowRegion;

    /** DOCUMENT ME! */
    private ButtonGroup groupObject;

    /** DOCUMENT ME! */
    private ButtonGroup groupObjects;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelConnectType;

    /** DOCUMENT ME! */
    private JLabel labelO2D;

    /** DOCUMENT ME! */
    private JLabel labelO3D;

    /** DOCUMENT ME! */
    private JLabel labelStructureType;

    /** DOCUMENT ME! */
    private JLabel labelStructuring;

    /** handling of intensity threshold. */
    private float lowerThreshold = 0.0f;

    /** dialog elements. */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JPanel morphoPanel;

    /** DOCUMENT ME! */
    private JPanel movePanel;

    /** DOCUMENT ME! */
    private int nx, ny, nz; // image dimensions

    /** DOCUMENT ME! */
    private JPanel objectPanel;

    /** DOCUMENT ME! */
    private JPanel panelThreshold;

    /** internal objects. */
    private BitSet previous; // previous mask

    /** DOCUMENT ME! */
    private JRadioButton radioBackground2D;

    /** DOCUMENT ME! */
    private JRadioButton radioBackground3D;

    /** DOCUMENT ME! */
    private JRadioButton radioBackgrounds2D;

    /** DOCUMENT ME! */
    private JRadioButton radioBackgrounds3D;

    /** DOCUMENT ME! */
    private JRadioButton radioGrowRegion2D;

    /** DOCUMENT ME! */
    private JRadioButton radioGrowRegion3D;

    /** DOCUMENT ME! */
    private JRadioButton radioObject2D;

    /** DOCUMENT ME! */
    private JRadioButton radioObject3D;

    /** DOCUMENT ME! */
    private JRadioButton radioObjects2D;

    /** DOCUMENT ME! */
    private JRadioButton radioObjects3D;

    /** parameters. */
    private int regionGrowDim = 3;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int rmObjDim = 2;

    /** DOCUMENT ME! */
    private int rmObjsDim = 2;

    /** DOCUMENT ME! */
    private PaintAutoSave save;

    /** DOCUMENT ME! */
    private java.util.Timer saver;

    /** DOCUMENT ME! */
    private BitSet se2xy, se2yz, se2xz, se3; // structuring elements

    /** handling of intensity threshold. */
    //private JSlider slideLower;
	private JSpinner spinLower;

    /** DOCUMENT ME! */
    //private JSlider slideUpper;
	private JSpinner spinUpper;

    /** DOCUMENT ME! */
    private float structureSize = 5.0f;

    /** DOCUMENT ME! */
    private String structureType = "ball";

    /** DOCUMENT ME! */
    private String[] structureTypes = { "ball", "diamond", "cube" };

    /** DOCUMENT ME! */
    private JTextField textSave;

    /** DOCUMENT ME! */
    private JTextField textStructuring;

    /** handling of intensity threshold. */
    private float upperThreshold = 255.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates dialog for plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogPowerPaint(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        // reset the button texts
        buttonGrowRegion.setText("Grow Region");
        buttonFillBackground.setText("Fill Background");
        buttonFillBackgrounds.setText("Fill All Background");
        buttonRmObject.setText("Remove Object");
        buttonRmObjects.setText("Remove All Objects");

        if ((command.equals("GrowRegion")) || (command.equals("Background")) || (command.equals("Backgrounds")) ||
                (command.equals("RmObject")) || (command.equals("RmObjects"))) {

            // listen to mouse motion in the original image
            image.getParentFrame().getComponentImage().addMouseListener(this);

            // check for the triplanar image
            if (image.getTriImageFrame() != null) {
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).addMouseListener(this);
                image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).addMouseListener(this);
            }
        }

        if (command.equals("Close")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("PT0007");
        } else if (command.equals("GrowRegion")) {

            if (getMouseInput != GROWREGION) {
                buttonGrowRegion.setText("click to grow");
                getMouseInput = GROWREGION;
            } else {
                getMouseInput = NONE;
            }
        } else if (command.equals("Background")) {

            if (getMouseInput != BACKGROUND) {
                buttonFillBackground.setText("click to remove");
                getMouseInput = BACKGROUND;
            } else {
                getMouseInput = NONE;
            }
        } else if (command.equals("Backgrounds")) {

            if (getMouseInput != ALLBACKGROUNDS) {
                buttonFillBackgrounds.setText("click to keep");
                getMouseInput = ALLBACKGROUNDS;
            } else {
                getMouseInput = NONE;
            }
        } else if (command.equals("RmObject")) {

            if (getMouseInput != REMOVE) {
                buttonRmObject.setText("click to remove");
                getMouseInput = REMOVE;
            } else {
                getMouseInput = NONE;
            }
        } else if (command.equals("RmObjects")) {

            if (getMouseInput != REMOVEALL) {
                buttonRmObjects.setText("click to keep");
                getMouseInput = REMOVEALL;
            } else {
                getMouseInput = NONE;
            }
        } else if (command.equals("Dilate")) {
            dilateImage();
        } else if (command.equals("Erode")) {
            erodeImage();
        } else if (command.equals("PropagUp")) {
            propagateUp();
        } else if (command.equals("PropagDown")) {
            propagateDown();
        } else if (command.equals("PropagAll")) {
            propagateAll();
        } else if (command.equals("ExportToVOI")) {
            exportToVOI();
        } else if (command.equals("ImportFromVOI")) {
            importFromVOI();
        } else if (command.equals("ExportToMask")) {
            exportToMask();
        } else if (command.equals("ImportFromMask")) {
            importFromMask();
        } else if (command.equals("Revert")) {
            revertImage();
        } else if (command.equals("Autosave")) {

            if (checkSave.isSelected()) {

                // start the auto-save option
                delay = Integer.valueOf(textSave.getText()).intValue();
                save = new PaintAutoSave(image);
                saver = new java.util.Timer();
                saver.schedule(save, new Date(), delay * 60 * 1000);
            } else {

                // stop the auto-save option
                saver.cancel();
                save = null;
                saver = null;
            }
        } else if (command.equals("Shortkeys")) {

            if (buttonShortkeys.isSelected()) {
                setFocusable(true);
                addKeyListener(this);
                requestFocusInWindow();
                image.getParentFrame().getComponentImage().setFocusable(true);
                image.getParentFrame().getComponentImage().addKeyListener(this);
                image.getParentFrame().getComponentImage().requestFocusInWindow();
            } else {
                removeKeyListener(this);
                image.getParentFrame().getComponentImage().removeKeyListener(this);
            }
        } else if (command.equals("Threshold")) {

            if (checkThreshold.isSelected()) {
                image.getParentFrame().getComponentImage().addMouseListener(this);

                if (image.getTriImageFrame() != null) {
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).addMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).addMouseListener(this);
                }
            } else {
                image.getParentFrame().getComponentImage().removeMouseListener(this);

                if (image.getTriImageFrame() != null) {
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).removeMouseListener(this);
                    image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).removeMouseListener(this);
                }
            }
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Handle the key pressed event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Handle the key released event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) {
        System.out.print("kr-");
    }

    /**
     * Handle the key typed event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) {
        String key = Character.toString(e.getKeyChar());
        //System.out.println("key: " + key);

        if (key.equals("d")) {
            actionPerformed(new ActionEvent(this, 0, "Dilate"));
        } else if (key.equals("e")) {
            actionPerformed(new ActionEvent(this, 0, "Erode"));
        } else if (key.equals("g")) {
            actionPerformed(new ActionEvent(this, 0, "GrowRegion"));
        } else if (key.equals("f")) {
            actionPerformed(new ActionEvent(this, 0, "Background"));
        } else if (key.equals("r")) {
            actionPerformed(new ActionEvent(this, 0, "RmObject"));
        }
    }

    /**
     * Listening to mouse events when updating paint.
     *
     * @param  mouseEvent  MouseEvent
     */
    public void mouseClicked(MouseEvent mouseEvent) {
        int xS = 0, yS = 0, zS = 0, sliceDir = XY;

        if (getMouseInput == NONE) {
            return;
        }

        if (mouseEvent.getComponent().equals(image.getParentFrame().getComponentImage())) {

            // get the point coordinates : image frame
            xS = Math.round((mouseEvent.getX() /
                                 (image.getParentFrame().getComponentImage().getZoomX() *
                                      image.getParentFrame().getComponentImage().getResolutionX())) - 0.5f); // zoomed x.  Used as cursor
            yS = Math.round((mouseEvent.getY() /
                                 (image.getParentFrame().getComponentImage().getZoomY() *
                                      image.getParentFrame().getComponentImage().getResolutionY())) - 0.5f); // zoomed y.  Used as cursor
            zS = image.getParentFrame().getComponentImage().getSlice();
            sliceDir = XY;
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A))) {

            // triplanar image : XY panel
            int slice = image.getTriImageFrame().getAxialComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                        mouseEvent.getY(),
                                                                                                        slice),
                                                                                           patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.AXIAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XZ;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A))) {

            // triplanar image : XZ panel
            int slice = image.getTriImageFrame().getCoronalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                          mouseEvent.getY(),
                                                                                                          slice),
                                                                                             patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.CORONAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = ZY;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A))) {

            // triplanar image : ZY panel
            int slice = image.getTriImageFrame().getSagittalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                           mouseEvent.getY(),
                                                                                                           slice),
                                                                                              patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.SAGITTAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XY;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B))) {

            // triplanar image : XY panel
            int slice = image.getTriImageFrame().getAxialComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                        mouseEvent.getY(),
                                                                                                        slice),
                                                                                           patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.AXIAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XZ;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B))) {

            // triplanar image : XZ panel
            int slice = image.getTriImageFrame().getCoronalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                          mouseEvent.getY(),
                                                                                                          slice),
                                                                                             patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.CORONAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = ZY;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B))) {

            // triplanar image : ZY panel
            int slice = image.getTriImageFrame().getSagittalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                           mouseEvent.getY(),
                                                                                                           slice),
                                                                                              patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.SAGITTAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XY;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB))) {

            // triplanar image : XY panel
            int slice = image.getTriImageFrame().getAxialComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                        mouseEvent.getY(),
                                                                                                        slice),
                                                                                           patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.AXIAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XZ;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB))) {

            // triplanar image : XZ panel
            int slice = image.getTriImageFrame().getCoronalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                          mouseEvent.getY(),
                                                                                                          slice),
                                                                                             patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.CORONAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = XZ;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = XY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = ZY;
            }
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB))) {

            // triplanar image : ZY panel
            int slice = image.getTriImageFrame().getSagittalComponentSlice();
            Vector3f patientMousePoint = new Vector3f();
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).ScreenToLocal(new Vector3f(mouseEvent.getX(),
                                                                                                           mouseEvent.getY(),
                                                                                                           slice),
                                                                                              patientMousePoint);

            Vector3f pt = new Vector3f();
            MipavCoordinateSystems.patientToFile(patientMousePoint, pt, image, FileInfoBase.SAGITTAL);
            xS = (int) pt.X;
            yS = (int) pt.Y;
            zS = (int) pt.Z;

            int origDir = image.getImageOrientation();

            if (origDir == FileInfoBase.AXIAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.CORONAL) {
                sliceDir = ZY;
            } else if (origDir == FileInfoBase.SAGITTAL) {
                sliceDir = XY;
            }
        }

        Preferences.debug("<" + xS + ", " + yS + ", " + zS + " :" + sliceDir + ">", Preferences.DEBUG_MINOR);

        // Check for validity
        if ((xS < 0) || (xS >= nx) || (yS < 0) || (yS >= ny) || (zS < 0) || (zS >= nz)) {
            return;
        }

        // remove the listeners
        image.getParentFrame().getComponentImage().removeMouseListener(this);

        if (image.getTriImageFrame() != null) {
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).removeMouseListener(this);
            image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).removeMouseListener(this);
        }

        if (getMouseInput == GROWREGION) {
            buttonGrowRegion.setText("growing...");
            buttonGrowRegion.repaint();
            growRegion(xS, yS, zS, sliceDir);
            buttonGrowRegion.setText("Grow Region");
        } else if (getMouseInput == BACKGROUND) {
            buttonFillBackground.setText("filling...");
            buttonFillBackground.repaint();
            fillBackground(xS, yS, zS, sliceDir);
            buttonFillBackground.setText("Fill Background");
        } else if (getMouseInput == ALLBACKGROUNDS) {
            buttonFillBackgrounds.setText("filling...");
            buttonFillBackgrounds.repaint();
            fillAllBackgrounds(xS, yS, zS, sliceDir);
            buttonFillBackgrounds.setText("Fill All Background");
        } else if (getMouseInput == REMOVE) {
            buttonRmObject.setText("removing...");
            buttonRmObject.repaint();
            removeObject(xS, yS, zS, sliceDir);
            buttonRmObject.setText("Remove Object");
        } else if (getMouseInput == REMOVEALL) {
            buttonRmObjects.setText("removing...");
            buttonRmObjects.repaint();
            removeAllObjects(xS, yS, zS, sliceDir);
            buttonRmObjects.setText("Remove All Objects");
        }

        // release the mouse
        getMouseInput = NONE;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { 
	}

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { 
	}

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { 
	}

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        if (image.getParentFrame().getComponentImage().getCursorMode() == ViewJComponentEditImage.PAINT_VOI) {

            // if (image.getParentFrame().getComponentImage().getMode()==ViewJComponentEditImage.PAINT_VOI) {
            if (checkThreshold.isSelected()) {

                // System.out.print("1-");
                if (mouseEvent.getComponent().equals(image.getParentFrame().getComponentImage()) ||
                        ((image.getTriImageFrame() != null) &&
                             (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A)) ||
							  mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B)) ||
							  mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB)) ||
                              mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB))))) {

                    // System.out.print("2-");

                    BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

                    if (obj == null) {
                        MipavUtil.displayError("paint mask not found");

                        return;
                    }
                    // System.out.print("3-");
					trimIntensityThreshold(image, obj, previous);
                    
                    // save it to previous
                    previous = (BitSet) obj.clone();

                    refreshImagePaint(image, obj);

                    // System.out.print("6-");

                }

            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseWheelMoved(MouseWheelEvent mouseEvent) {

        if (image.getTriImageFrame() == null) {
            return;
        }

        // find the position
        if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A))) {

            // triplanar image : XY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(), tri.getCoronalComponentSlice(),
                          tri.getAxialComponentSlice() + mouseEvent.getWheelRotation());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A))) {

            // triplanar image : XZ panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(),
                          tri.getCoronalComponentSlice() + mouseEvent.getWheelRotation(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A))) {

            // triplanar image : ZY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice() + mouseEvent.getWheelRotation(),
                          tri.getCoronalComponentSlice(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B))) {

            // triplanar image : XY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(), tri.getCoronalComponentSlice(),
                          tri.getAxialComponentSlice() + mouseEvent.getWheelRotation());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B))) {

            // triplanar image : XZ panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(),
                          tri.getCoronalComponentSlice() + mouseEvent.getWheelRotation(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B))) {

            // triplanar image : ZY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice() + mouseEvent.getWheelRotation(),
                          tri.getCoronalComponentSlice(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB))) {

            // triplanar image : XY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(), tri.getCoronalComponentSlice(),
                          tri.getAxialComponentSlice() + mouseEvent.getWheelRotation());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB))) {

            // triplanar image : XZ panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice(),
                          tri.getCoronalComponentSlice() + mouseEvent.getWheelRotation(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        } else if (mouseEvent.getComponent().equals(image.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB))) {

            // triplanar image : ZY panel
            ViewJFrameTriImage tri = image.getTriImageFrame();
            tri.setSlices(tri.getSagittalComponentSlice() + mouseEvent.getWheelRotation(),
                          tri.getCoronalComponentSlice(), tri.getAxialComponentSlice());
            tri.updateImages(true);
        }

    }

    /**
     * state change listener for the sliders.
     *
     * @param  e  DOCUMENT ME!
     */
    public void stateChanged(ChangeEvent e) {
		/*
        if (e.getSource().equals(slideLower)) {

            if (!slideLower.getValueIsAdjusting()) {
                lowerThreshold = (float) (slideLower.getValue() / 1000.0f);
            }
        } else if (e.getSource().equals(slideUpper)) {

            if (!slideUpper.getValueIsAdjusting()) {
                upperThreshold = (float) (slideUpper.getValue() / 1000.0f);
            }
        }
		*/
        if (e.getSource().equals(spinLower)) {
			lowerThreshold = (float)((SpinnerNumberModel)spinLower.getModel()).getNumber().floatValue();
        } else if (e.getSource().equals(spinUpper)) {
			upperThreshold = (float)((SpinnerNumberModel)spinUpper.getModel()).getNumber().floatValue();
        }
		//System.out.print("kr-");
    }

    /**
     * 3D images: 18-neighborhood.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     * @param   nz   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[][][] connected18Object3D(boolean[][][] img, int nx, int ny, int nz) {
        int Nlabel = 0;
        int[][][] label = new int[nx][ny][nz];
        int[] lb = new int[MaxObject];
        int lbMin;
        int x, y, z, i, j, k, l, c;
        int Nlb;
        int[] connect = new int[18];
        int Nconnect;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = 0;
                }
            }
        }

        lb[0] = 0;
        Nlabel = 1;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {

                    if (img[x][y][z]) {

                        // object point: neighbors ?
                        Nconnect = 0;

                        for (i = -1; i <= 1; i++) {

                            for (j = -1; j <= 1; j++) {

                                for (k = -1; k <= 1; k++) {

                                    if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny) &&
                                            ((z + k) >= 0) && ((z + k) < nz)) {

                                        if (((i * i) + (j * j) + (k * k)) < 3) {

                                            if (label[x + i][y + j][z + k] > 0) {
                                                connect[Nconnect] = lb[label[x + i][y + j][z + k]];
                                                Nconnect++;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // if connected values, find the smallest lb label and attribute it
                        // to all others (-> join labels)
                        if (Nconnect > 0) {

                            // printf("c:%d",Nconnect);
                            lbMin = lb[connect[0]];

                            for (l = 1; l < Nconnect; l++) {
                                lbMin = Math.min(lbMin, lb[connect[l]]);
                            }

                            for (l = 0; l < Nconnect; l++) {
                                lb[connect[l]] = lbMin;
                            }

                            label[x][y][z] = lbMin;
                        } else {

                            // new, unconnected region
                            label[x][y][z] = Nlabel;
                            lb[Nlabel] = Nlabel;

                            // printf("l:%d", Nlabel);
                            Nlabel++;
                        }
                    }
                }
            }
        }

        // only one level of labels
        for (k = 1; k < Nlabel; k++) {
            c = k;

            while (lb[c] != c) {
                c = lb[c];
            }

            lb[k] = c;
        }

        // count the valid labels and rearrange labels to have regular increment
        Nlb = 0;

        int[] lb2 = new int[Nlabel];
        lb2[0] = 0;

        for (k = 1; k < Nlabel; k++) {

            if (lb[k] == k) {
                Nlb++;
                lb2[k] = Nlb;
            }
        }

        // copy on label image
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = lb2[lb[label[x][y][z]]];
                }
            }
        }

        // clean up
        lb = null;
        lb2 = null;
        connect = null;

        return label;
    }

    /**
     * 3D images: 26-neighborhood.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     * @param   nz   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[][][] connected26Object3D(boolean[][][] img, int nx, int ny, int nz) {
        int Nlabel = 0;
        int[][][] label = new int[nx][ny][nz];
        int[] lb = new int[MaxObject];
        int lbMin;
        int x, y, z, i, j, k, l, c;
        int Nlb;
        int[] connect = new int[26];
        int Nconnect;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = 0;
                }
            }
        }

        lb[0] = 0;
        Nlabel = 1;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {

                    if (img[x][y][z]) {

                        // object point: neighbors ?
                        Nconnect = 0;

                        for (i = -1; i <= 1; i++) {

                            for (j = -1; j <= 1; j++) {

                                for (k = -1; k <= 1; k++) {

                                    if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny) &&
                                            ((z + k) >= 0) && ((z + k) < nz)) {

                                        if (label[x + i][y + j][z + k] > 0) {
                                            connect[Nconnect] = lb[label[x + i][y + j][z + k]];
                                            Nconnect++;
                                        }
                                    }
                                }
                            }
                        }

                        // if connected values, find the smallest lb label and attribute it
                        // to all others (-> join labels)
                        if (Nconnect > 0) {

                            // printf("c:%d",Nconnect);
                            lbMin = lb[connect[0]];

                            for (l = 1; l < Nconnect; l++) {
                                lbMin = Math.min(lbMin, lb[connect[l]]);
                            }

                            for (l = 0; l < Nconnect; l++) {
                                lb[connect[l]] = lbMin;
                            }

                            label[x][y][z] = lbMin;
                        } else {

                            // new, unconnected region
                            label[x][y][z] = Nlabel;
                            lb[Nlabel] = Nlabel;

                            // printf("l:%d", Nlabel);
                            Nlabel++;
                        }
                    }
                }
            }
        }

        // only one level of labels
        for (k = 1; k < Nlabel; k++) {
            c = k;

            while (lb[c] != c) {
                c = lb[c];
            }

            lb[k] = c;
        }

        // count the valid labels and rearrange labels to have regular increment
        Nlb = 0;

        int[] lb2 = new int[Nlabel];
        lb2[0] = 0;

        for (k = 1; k < Nlabel; k++) {

            if (lb[k] == k) {
                Nlb++;
                lb2[k] = Nlb;
            }
        }

        // copy on label image
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = lb2[lb[label[x][y][z]]];
                }
            }
        }

        // clean up
        lb = null;
        lb2 = null;
        connect = null;

        return label;
    }

    /**
     * 2D images: 4-connectivity.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[][] connected4Object2D(boolean[][] img, int nx, int ny) {
        int Nlabel;
        int[][] label = new int[nx][ny];
        int[] lb = new int[MaxObject];
        int lbMin;
        int x, y, c, i, j, k;
        int Nlb;
        int[] connect = new int[4];
        int Nconnect;

        // the input is a 3x3 binary image (0 out, 1 in)
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {
                label[x][y] = 0;
            }
        }

        lb[0] = 0;
        Nlabel = 1;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                if (img[x][y]) {

                    // object point: neighbors ?
                    Nconnect = 0;

                    for (i = -1; i <= 1; i++) {

                        for (j = -1; j <= 1; j++) {

                            if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny)) {

                                if (((i * i) + (j * j)) < 2) {

                                    if (label[x + i][y + j] > 0) {
                                        connect[Nconnect] = lb[label[x + i][y + j]];
                                        Nconnect++;
                                    }
                                }
                            }
                        }
                    }

                    // if connected values, find the smallest lb label and attribute it
                    // to all others (-> join labels)
                    if (Nconnect > 0) {
                        lbMin = lb[connect[0]];

                        for (k = 1; k < Nconnect; k++) {
                            lbMin = Math.min(lbMin, lb[connect[k]]);
                        }

                        for (k = 0; k < Nconnect; k++) {
                            lb[connect[k]] = lbMin;
                        }

                        label[x][y] = lbMin;
                    } else {

                        // new, unconnected region
                        label[x][y] = Nlabel;
                        lb[Nlabel] = Nlabel;
                        Nlabel++;
                    }
                }
            }
        }

        // only one level of labels
        for (k = 1; k < Nlabel; k++) {
            c = k;

            while (lb[c] != c) {
                c = lb[c];
            }

            lb[k] = c;
        }

        // count the valid labels and rearrange labels to have regular increment
        Nlb = 0;

        int[] lb2 = new int[Nlabel];
        lb2[0] = 0;

        for (k = 1; k < Nlabel; k++) {

            if (lb[k] == k) {
                Nlb++;
                lb2[k] = Nlb;
            }
        }

        // copy on label image
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {
                label[x][y] = lb2[lb[label[x][y]]];
            }
        }

        // clean up
        lb = null;
        lb2 = null;
        connect = null;

        return label;
    }

    /**
     * 3D images: 6-neighborhood.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     * @param   nz   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[][][] connected6Object3D(boolean[][][] img, int nx, int ny, int nz) {
        int Nlabel = 0;
        int[][][] label = new int[nx][ny][nz];
        int[] lb = new int[MaxObject];
        int lbMin;
        int x, y, z, i, j, k, l, c;
        int Nlb;
        int[] connect = new int[6];
        int Nconnect;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = 0;
                }
            }
        }

        lb[0] = 0;
        Nlabel = 1;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {

                    if (img[x][y][z]) {

                        // object point: neighbors ?
                        Nconnect = 0;

                        for (i = -1; i <= 1; i++) {

                            for (j = -1; j <= 1; j++) {

                                for (k = -1; k <= 1; k++) {

                                    if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny) &&
                                            ((z + k) >= 0) && ((z + k) < nz)) {

                                        if (((i * i) + (j * j) + (k * k)) < 2) {

                                            if (label[x + i][y + j][z + k] > 0) {
                                                connect[Nconnect] = lb[label[x + i][y + j][z + k]];
                                                Nconnect++;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // if connected values, find the smallest lb label and attribute it
                        // to all others (-> join labels)
                        if (Nconnect > 0) {

                            // printf("c:%d",Nconnect);
                            lbMin = lb[connect[0]];

                            for (l = 1; l < Nconnect; l++) {
                                lbMin = Math.min(lbMin, lb[connect[l]]);
                            }

                            for (l = 0; l < Nconnect; l++) {
                                lb[connect[l]] = lbMin;
                            }

                            label[x][y][z] = lbMin;
                        } else {

                            // new, unconnected region
                            label[x][y][z] = Nlabel;
                            lb[Nlabel] = Nlabel;

                            // printf("l:%d", Nlabel);
                            Nlabel++;
                        }
                    }
                }
            }
        }

        // only one level of labels
        for (k = 1; k < Nlabel; k++) {
            c = k;

            while (lb[c] != c) {
                c = lb[c];
            }

            lb[k] = c;
        }

        // count the valid labels and rearrange labels to have regular increment
        Nlb = 0;

        int[] lb2 = new int[Nlabel];
        lb2[0] = 0;

        for (k = 1; k < Nlabel; k++) {

            if (lb[k] == k) {
                Nlb++;
                lb2[k] = Nlb;
            }
        }

        // copy on label image
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                for (z = 0; z < nz; z++) {
                    label[x][y][z] = lb2[lb[label[x][y][z]]];
                }
            }
        }

        // clean up
        lb = null;
        lb2 = null;
        connect = null;

        return label;
    }

    /**
     * 2D images: 8-neighborhood.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[][] connected8Object2D(boolean[][] img, int nx, int ny) {
        int Nlabel;
        int[][] label = new int[nx][ny];
        int[] lb = new int[MaxObject];
        int lbMin;
        int x, y, c, i, j, k;
        int Nlb;
        int[] connect = new int[4];
        int Nconnect;

        // the input is a 3x3 binary image (0 out, 1 in)
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {
                label[x][y] = 0;
            }
        }

        lb[0] = 0;
        Nlabel = 1;

        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {

                if (img[x][y]) {

                    // object point: neighbors ?
                    // object point: neighbors ?
                    Nconnect = 0;

                    for (i = -1; i <= 1; i++) {

                        for (j = -1; j <= 1; j++) {

                            if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny)) {

                                if (((i * i) + (j * j)) < 3) {

                                    if (label[x + i][y + j] > 0) {
                                        connect[Nconnect] = lb[label[x + i][y + j]];
                                        Nconnect++;
                                    }
                                }
                            }
                        }
                    }

                    // if connected values, find the smallest lb label and attribute it
                    // to all others (-> join labels)
                    if (Nconnect > 0) {
                        lbMin = lb[connect[0]];

                        for (k = 1; k < Nconnect; k++) {
                            lbMin = Math.min(lbMin, lb[connect[k]]);
                        }

                        for (k = 0; k < Nconnect; k++) {
                            lb[connect[k]] = lbMin;
                        }

                        label[x][y] = lbMin;
                    } else {

                        // new, unconnected region
                        label[x][y] = Nlabel;
                        lb[Nlabel] = Nlabel;
                        Nlabel++;
                    }
                }
            }
        }

        // only one level of labels
        for (k = 1; k < Nlabel; k++) {
            c = k;

            while (lb[c] != c) {
                c = lb[c];
            }

            lb[k] = c;
        }

        // count the valid labels and rearrange labels to have regular increment
        Nlb = 0;

        int[] lb2 = new int[Nlabel];
        lb2[0] = 0;

        for (k = 1; k < Nlabel; k++) {

            if (lb[k] == k) {
                Nlb++;
                lb2[k] = Nlb;
            }
        }

        // copy on label image
        for (x = 0; x < nx; x++) {

            for (y = 0; y < ny; y++) {
                label[x][y] = lb2[lb[label[x][y]]];
            }
        }

        // clean up
        lb = null;
        lb2 = null;
        connect = null;

        return label;
    }

    /**
     * make the structuring element for morphology.
     */
    private void createStructuringElement2D() {
        float rx, ry, rz;
        int dmx, dmy, dmz;

        // build the structuring element for erosion and dilatation
        // along the three dimensions
        rx = image.getFileInfo()[0].getResolutions()[0];
        ry = image.getFileInfo()[0].getResolutions()[1];
        rz = image.getFileInfo()[0].getResolutions()[2];

        // ideally: use a spherical ball of radius seSize
        dmx = (int) Math.floor(structureSize / rx);
        dmy = (int) Math.floor(structureSize / ry);
        dmz = (int) Math.floor(structureSize / rz);

        if ((dmx % 2) == 0) {
            dmx++;
        }

        if ((dmy % 2) == 0) {
            dmy++;
        }

        if ((dmz % 2) == 0) {
            dmz++;
        }

        c2x = (dmx - 1) / 2;
        c2y = (dmy - 1) / 2;
        c2z = (dmz - 1) / 2;

        se2xy = new BitSet(dmx * dmy);
        se2xz = new BitSet(dmx * dmz);
        se2yz = new BitSet(dmx * dmy);

        if (structureType.equals("ball")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {

                    if ((((i - c2x) * (i - c2x) * rx * rx) + ((j - c2y) * (j - c2y) * ry * ry)) <
                            (0.25f * structureSize * structureSize)) {
                        se2xy.set(i + (dmx * j), true);
                    } else {
                        se2xy.set(i + (dmx * j), false);
                    }
                }
            }

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmz; j++) {

                    if ((((i - c2x) * (i - c2x) * rx * rx) + ((j - c2z) * (j - c2z) * rz * rz)) <
                            (0.25f * structureSize * structureSize)) {
                        se2xz.set(i + (dmx * j), true);
                    } else {
                        se2xz.set(i + (dmx * j), false);
                    }
                }
            }

            for (int i = 0; i < dmy; i++) {

                for (int j = 0; j < dmz; j++) {

                    if ((((i - c2y) * (i - c2y) * ry * ry) + ((j - c2z) * (j - c2z) * rz * rz)) <
                            (0.25f * structureSize * structureSize)) {
                        se2yz.set(i + (dmy * j), true);
                    } else {
                        se2yz.set(i + (dmy * j), false);
                    }
                }
            }
        } else if (structureType.equals("diamond")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {

                    if (((Math.abs(i - c2x) * rx) + (Math.abs(j - c2y) * ry)) < (0.5f * structureSize)) {
                        se2xy.set(i + (dmx * j), true);
                    } else {
                        se2xy.set(i + (dmx * j), false);
                    }
                }
            }

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmz; j++) {

                    if (((Math.abs(i - c2x) * rx) + (Math.abs(j - c2z) * rz)) < (0.5f * structureSize)) {
                        se2xz.set(i + (dmx * j), true);
                    } else {
                        se2xz.set(i + (dmx * j), false);
                    }
                }
            }

            for (int i = 0; i < dmy; i++) {

                for (int j = 0; j < dmz; j++) {

                    if (((Math.abs(i - c2y) * ry) + (Math.abs(j - c2z) * rz)) < (0.5f * structureSize)) {
                        se2yz.set(i + (dmy * j), true);
                    } else {
                        se2yz.set(i + (dmy * j), false);
                    }
                }
            }
        } else if (structureType.equals("cube")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {
                    se2xy.set(i + (dmx * j), true);
                }
            }

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmz; j++) {
                    se2xz.set(i + (dmx * j), true);
                }
            }

            for (int i = 0; i < dmy; i++) {

                for (int j = 0; j < dmz; j++) {
                    se2yz.set(i + (dmy * j), true);
                }
            }
        }

        return;
    }

    /**
     * make the structuring element for morphology.
     */
    private void createStructuringElement3D() {
        float rx, ry, rz;
        int dmx, dmy, dmz;

        // build the structuring element for erosion and dilatation
        // different ones for different resolutions ?
        rx = image.getFileInfo()[0].getResolutions()[0];
        ry = image.getFileInfo()[0].getResolutions()[1];
        rz = image.getFileInfo()[0].getResolutions()[2];

        // ideally: use a spherical ball of radius seSize
        dmx = (int) Math.floor(structureSize / rx);
        dmy = (int) Math.floor(structureSize / ry);
        dmz = (int) Math.floor(structureSize / rz);

        if ((dmx % 2) == 0) {
            dmx++;
        }

        if ((dmy % 2) == 0) {
            dmy++;
        }

        if ((dmz % 2) == 0) {
            dmz++;
        }

        c3x = (dmx - 1) / 2;
        c3y = (dmy - 1) / 2;
        c3z = (dmz - 1) / 2;

        se3 = new BitSet(dmx * dmy * dmz);

        if (structureType.equals("ball")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {

                    for (int k = 0; k < dmz; k++) {

                        if ((((i - c3x) * (i - c3x) * rx * rx) + ((j - c3y) * (j - c3y) * ry * ry) +
                                 ((k - c3z) * (k - c3z) * rz * rz)) < (0.25f * structureSize * structureSize)) {
                            se3.set(i + (dmx * j) + (dmx * dmy * k), true);
                        } else {
                            se3.set(i + (dmx * j) + (dmx * dmy * k), false);
                        }
                    }
                }
            }
        } else if (structureType.equals("diamond")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {

                    for (int k = 0; k < dmz; k++) {

                        if (((Math.abs(i - c3x) * rx) + (Math.abs(j - c3y) * ry) + (Math.abs(k - c3z) * rz)) <
                                (0.5f * structureSize)) {
                            se3.set(i + (dmx * j) + (dmx * dmy * k), true);
                        } else {
                            se3.set(i + (dmx * j) + (dmx * dmy * k), false);
                        }
                    }
                }
            }
        } else if (structureType.equals("cube")) {

            for (int i = 0; i < dmx; i++) {

                for (int j = 0; j < dmy; j++) {

                    for (int k = 0; k < dmz; k++) {
                        se3.set(i + (dmx * j) + (dmx * dmy * k), true);
                    }
                }
            }
        }

        return;
    }

    /**
     * dilation.
     */
    private void dilateImage() {
        //System.out.println("dilate");

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // create morphology mask if needed
        String tmpStr = textStructuring.getText();
        float size = 0.0f;

        if (testParameter(tmpStr, 0.0, 100.0)) {
            size = Float.valueOf(tmpStr).floatValue();
        } else {
            textStructuring.requestFocus();
            textStructuring.selectAll();

            return;
        }

        String shape = (String) comboStructureType.getSelectedItem();

        if ((size != structureSize) || (shape != structureType)) {
            structureSize = size;
            structureType = shape;
            createStructuringElement2D();
            createStructuringElement3D();
        }

        // get the dimension
        String dilateType = (String) comboDilateDimType.getSelectedItem();

        // dilation
        if (dilateType.equals("3D")) {
            obj = dilateObject(obj, nx, ny, nz, se3, c3x, c3y, c3z);
        } else if (dilateType.equals("2.5D")) {
            obj = dilateObject(obj, nx, ny, nz, se2xy, c2x, c2y, 0);
        } else if (dilateType.equals("2.5D(XY)")) {
            obj = dilateObject(obj, nx, ny, nz, se2xy, c2x, c2y, 0);
        } else if (dilateType.equals("2.5D(XZ)")) {
            obj = dilateObject(obj, nx, ny, nz, se2xz, c2x, 0, c2z);
        } else if (dilateType.equals("2.5D(YZ)")) {
            obj = dilateObject(obj, nx, ny, nz, se2yz, 0, c2y, c2z);
        } else if (dilateType.equals("2D")) {

            // extract slice
            int zS = image.getParentFrame().getComponentImage().getSlice();
            int nxnyzS = nx * ny * zS;
            int nxnyzSp = nx * ny * (zS + 1);
            BitSet img = new BitSet(nx * ny);

            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {
                img.set(index - nxnyzS, true);
            }

            // dilate
            img = dilateObject(img, nx, ny, 1, se2xy, c2x, c2y, 0);

            // copy the result
            for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {

                if (img.get(index)) {
                    obj.set(index + nxnyzS, true);
                }
            }
        } else if (dilateType.equals("2D(XY)")) {

            // extract slice
            int zS = image.getTriImageFrame().getAxialComponentSlice();
            int nxnyzS = nx * ny * zS;
            int nxnyzSp = nx * ny * (zS + 1);
            BitSet img = new BitSet(nx * ny);

            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {
                img.set(index - nxnyzS, true);
            }

            // dilate
            img = dilateObject(img, nx, ny, 1, se2xy, c2x, c2y, 0);

            // copy the result
            for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {

                if (img.get(index)) {
                    obj.set(index + nxnyzS, true);
                }
            }
        } else if (dilateType.equals("2D(XZ)")) {

            // extract slice
            int yS = image.getTriImageFrame().getCoronalComponentSlice();
            BitSet img = new BitSet(nx * nz);
            int nxny = nx * ny;
            int x, z;

            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (yS == ((index % nxny) / nx)) {
                    x = ((index % nxny) % nx);
                    z = index / nxny;
                    img.set(x + (nx * z), true);
                }
            }

            // dilate
            img = dilateObject(img, nx, nz, 1, se2xz, c2x, c2z, 0);

            // copy the result
            for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {
                x = index % nx;
                z = index / nx;
                obj.set(x + (nx * yS) + (nxny * z), true);
            }
        } else if (dilateType.equals("2D(YZ)")) {

            // extract slice
            int xS = image.getTriImageFrame().getSagittalComponentSlice();
            BitSet img = new BitSet(nx * ny);
            int nxny = nx * ny;
            int y, z;

            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (xS == ((index % nxny) % nx)) {
                    y = (index % nxny) / nx;
                    z = index / nxny;
                    img.set(y + (ny * z), true);
                }
            }

            // dilate
            img = dilateObject(img, ny, nz, 1, se2yz, c2y, c2z, 0);

            // copy the result
            for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {
                y = index % ny;
                z = index / ny;
                obj.set(xS + (nx * y) + (nxny * z), true);
            }
        }

		trimIntensityThreshold(image, obj, previous);
                    
        refreshImagePaint(image, obj);
    }

    /**
     * dilate binary object with a custom kernel using the BitSet structure with indexing convention index = x + nx*y +
     * nx*ny*z.
     *
     * @param   img   DOCUMENT ME!
     * @param   nx    DOCUMENT ME!
     * @param   ny    DOCUMENT ME!
     * @param   nz    DOCUMENT ME!
     * @param   mask  DOCUMENT ME!
     * @param   dx    DOCUMENT ME!
     * @param   dy    DOCUMENT ME!
     * @param   dz    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private BitSet dilateObject(BitSet img, int nx, int ny, int nz, BitSet mask, int dx, int dy, int dz) {
        BitSet dilated = new BitSet(nx * ny * nz);
        int ndx = (2 * dx) + 1;
        int ndxndy = ndx * ((2 * dy) + 1);
        int nxny = nx * ny;
        int x, y, z;

        // dx,dy,dz describe the structuring element ( x+/-dx, y+/-dy, z+/-dz )
        for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {
            z = index / nxny;
            y = (index % nxny) / nx;
            x = ((index % nxny) % nx);

            for (int i = -dx; i <= dx; i++) {

                for (int j = -dy; j <= dy; j++) {

                    for (int k = -dz; k <= dz; k++) {

                        if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny) && ((z + k) >= 0) &&
                                ((z + k) < nz)) {

                            if ((mask.get(i + dx + (ndx * (j + dy)) + (ndxndy * (k + dz))))) {
                                dilated.set(x + i + (nx * (y + j)) + (nxny * (z + k)));
                            }
                        }
                    }
                }
            }
        }

        return dilated;
    }

    /**
     * erosion.
     */
    private void erodeImage() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // create morphology mask if needed
        String tmpStr = textStructuring.getText();
        float size = 0.0f;

        if (testParameter(tmpStr, 0.0, 100.0)) {
            size = Float.valueOf(tmpStr).floatValue();
        } else {
            textStructuring.requestFocus();
            textStructuring.selectAll();

            return;
        }

        String shape = (String) comboStructureType.getSelectedItem();

        if ((size != structureSize) || (shape != structureType)) {
            structureSize = size;
            structureType = shape;
            createStructuringElement2D();
            createStructuringElement3D();
        }

        // get the dimension
        String erodeType = (String) comboErodeDimType.getSelectedItem();

        // erosion
        if (erodeType.equals("3D")) {
            obj = erodeObject(obj, nx, ny, nz, se3, c3x, c3y, c3z);
        } else if (erodeType.equals("2.5D")) {
            obj = erodeObject(obj, nx, ny, nz, se2xy, c2x, c2y, 0);
        } else if (erodeType.equals("2.5D(XY)")) {
            obj = erodeObject(obj, nx, ny, nz, se2xy, c2x, c2y, 0);
        } else if (erodeType.equals("2.5D(XZ)")) {
            obj = erodeObject(obj, nx, ny, nz, se2xz, c2x, 0, c2z);
        } else if (erodeType.equals("2.5D(YZ)")) {
            obj = erodeObject(obj, nx, ny, nz, se2yz, 0, c2y, c2z);
        } else if (erodeType.equals("2D")) {

            // extract slice
            int zS = image.getParentFrame().getComponentImage().getSlice();
            int nxnyzS = nx * ny * zS;
            int nxnyzSp = nx * ny * (zS + 1);
            BitSet img = new BitSet(nx * ny);

            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {
                img.set(index - nxnyzS, true);
            }

            // erode
            img = erodeObject(img, nx, ny, 1, se2xy, c2x, c2y, 0);

            // copy the result
            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {

                if (!img.get(index - nxnyzS)) {
                    obj.set(index, false);
                }
            }
        } else if (erodeType.equals("2D(XY)")) {

            // extract slice
            int zS = image.getTriImageFrame().getAxialComponentSlice();
            int nxnyzS = nx * ny * zS;
            int nxnyzSp = nx * ny * (zS + 1);
            BitSet img = new BitSet(nx * ny);

            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {
                img.set(index - nxnyzS, true);
            }

            // erode
            img = erodeObject(img, nx, ny, 1, se2xy, c2x, c2y, 0);

            // copy the result
            for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                     index = obj.nextSetBit(index + 1)) {

                if (!img.get(index - nxnyzS)) {
                    obj.set(index, false);
                }
            }
        } else if (erodeType.equals("2D(XZ)")) {

            // extract slice
            int yS = image.getTriImageFrame().getCoronalComponentSlice();
            BitSet img = new BitSet(nx * nz);
            int nxny = nx * ny;
            int x, z;

            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (yS == ((index % nxny) / nx)) {
                    x = ((index % nxny) % nx);
                    z = index / nxny;
                    img.set(x + (nx * z), true);
                }
            }

            // erode
            img = erodeObject(img, nx, nz, 1, se2xz, c2x, c2z, 0);

            // copy the result
            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (yS == ((index % nxny) / nx)) {
                    x = ((index % nxny) % nx);
                    z = index / nxny;

                    if (!img.get(x + (nx * z))) {
                        obj.set(index, false);
                    }
                }
            }
        } else if (erodeType.equals("2D(YZ)")) {

            // extract slice
            int xS = image.getTriImageFrame().getSagittalComponentSlice();
            BitSet img = new BitSet(nx * ny);
            int nxny = nx * ny;
            int y, z;

            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (xS == ((index % nxny) % nx)) {
                    y = (index % nxny) / nx;
                    z = index / nxny;
                    img.set(y + (ny * z), true);
                }
            }

            // erode
            img = erodeObject(img, ny, nz, 1, se2yz, c2y, c2z, 0);

            // copy the result
            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {

                if (xS == ((index % nxny) % nx)) {
                    y = (index % nxny) / nx;
                    z = index / nxny;

                    if (!img.get(y + (ny * z))) {
                        obj.set(index, false);
                    }
                }
            }
        }

        refreshImagePaint(image, obj);
    }

    /**
     * erode binary object with a custom kernel using the BitSet structure with indexing convention index = x + nx*y +
     * nx*ny*z.
     *
     * @param   img   DOCUMENT ME!
     * @param   nx    DOCUMENT ME!
     * @param   ny    DOCUMENT ME!
     * @param   nz    DOCUMENT ME!
     * @param   mask  DOCUMENT ME!
     * @param   dx    DOCUMENT ME!
     * @param   dy    DOCUMENT ME!
     * @param   dz    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private BitSet erodeObject(BitSet img, int nx, int ny, int nz, BitSet mask, int dx, int dy, int dz) {
        BitSet eroded = new BitSet(nx * ny * nz);
        int ndx = (2 * dx) + 1;
        int ndxndy = ndx * ((2 * dy) + 1);
        int nxny = nx * ny;
        int x, y, z;

        // dx,dy,dz describe the structuring element ( x+/-dx, y+/-dy, z+/-dz )
        for (int index = img.nextSetBit(0); index >= 0; index = img.nextSetBit(index + 1)) {
            z = index / nxny;
            y = (index % nxny) / nx;
            x = ((index % nxny) % nx);
            eroded.set(index, true);

            for (int i = -dx; i <= dx; i++) {

                for (int j = -dy; j <= dy; j++) {

                    for (int k = -dz; k <= dz; k++) {

                        if (((x + i) >= 0) && ((x + i) < nx) && ((y + j) >= 0) && ((y + j) < ny) && ((z + k) >= 0) &&
                                ((z + k) < nz)) {

                            if ((mask.get(i + dx + (ndx * (j + dy)) + (ndxndy * (k + dz)))) &&
                                    (!img.get(x + i + (nx * (y + j)) + (nxny * (z + k))))) {
                                eroded.set(index, false);
                            }
                        }
                    }
                }
            }
        }

        return eroded;
    }

    /**
     * export to Mask.
     */
    private void exportToMask() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // select the ID: if not new, update the mask
        if (image.getParentFrame().getImageB() == null) {

            // create the mask image
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
        }

        // record selected image; set to image B
        ModelImage active = image.getParentFrame().getActiveImage();
        image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);

        // call the paint to mask program
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "CommitPaint"));

        // reset the active image
        if (!active.equals(image.getParentFrame().getActiveImage())) {
            image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
        }

        refreshImagePaint(image, obj);
    }

    /**
     * export to VOI.
     */
    private void exportToVOI() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // call the paint to VOI program
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "PaintToVOI"));

        // clear the mask
        obj.clear();
        refreshImagePaint(image, obj);
    }

    /**
     * background filling algorithm.
     *
     * @param  xS        ModelImage-Space x coordinate
     * @param  yS        ModelImage-Space y coordinate
     * @param  zS        ModelImage-Space z coordinate
     * @param  sliceDir  (XY, XZ, ZY)
     */
    private void fillAllBackgrounds(int xS, int yS, int zS, int sliceDir) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // get parameters
        connectType = (String) comboConnectType.getSelectedItem();

        if (radioBackgrounds3D.isSelected()) {
            backgroundsDim = 3;
        } else {
            backgroundsDim = 2;
        }

        // find main object
        if ((backgroundsDim == 2) && (sliceDir == XY)) {
            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nx][ny];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    img[x][y] = (!obj.get(x + (nx * y) + (nx * ny * zS)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nx, ny);
            } else {
                label = connected4Object2D(img, nx, ny);
            }

            // remove all but the selected background
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    if (label[x][y] != label[xS][yS]) {
                        obj.set(x + (nx * y) + (nx * ny * zS), true);
                    }
                }
            }
        } else if ((backgroundsDim == 2) && (sliceDir == XZ)) {
            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nx][nz];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    img[x][z] = (!obj.get(x + (nx * yS) + (nx * ny * z)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nx, nz);
            } else {
                label = connected4Object2D(img, nx, nz);
            }

            // remove all but the selected background
            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {

                    if (label[x][z] != label[xS][zS]) {
                        obj.set(x + (nx * yS) + (nx * ny * z), true);
                    }
                }
            }
        } else if ((backgroundsDim == 2) && (sliceDir == ZY)) {
            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nz][ny];

            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {
                    img[z][y] = (!obj.get(xS + (nx * y) + (nx * ny * z)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nz, ny);
            } else {
                label = connected4Object2D(img, nz, ny);
            }

            // remove all but the selected background
            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {

                    if (label[z][y] != label[zS][yS]) {
                        obj.set(xS + (nx * y) + (nx * ny * z), true);
                    }
                }
            }
        } else {
            int[][][] label;

            // extract the mask and invert
            boolean[][][] img = new boolean[nx][ny][nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
                        img[x][y][z] = (!obj.get(x + (nx * y) + (nx * ny * z)));
                    }
                }
            }

            // connected components on background
            if (connectType.equals("6/26")) {
                label = connected26Object3D(img, nx, ny, nz);
            } else if (connectType.equals("6/18")) {
                label = connected18Object3D(img, nx, ny, nz);
            } else {
                label = connected6Object3D(img, nx, ny, nz);
            }

            // remove all but the selected background
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {

                        if (label[x][y][z] != label[xS][yS][zS]) {
                            obj.set(x + (nx * y) + (nx * ny * z), true);
                        }
                    }
                }
            }
        }

        refreshImagePaint(image, obj);
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * background filling algorithm.
     *
     * @param  xS        ModelImage-Space x coordinate
     * @param  yS        ModelImage-Space y coordinate
     * @param  zS        ModelImage-Space z coordinate
     * @param  sliceDir  (XY, XZ, ZY)
     */
    private void fillBackground(int xS, int yS, int zS, int sliceDir) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // get parameters
        connectType = (String) comboConnectType.getSelectedItem();

        if (radioBackground3D.isSelected()) {
            backgroundDim = 3;
        } else {
            backgroundDim = 2;
        }

        // find main object
        if ((backgroundDim == 2) && (sliceDir == XY)) {
            //System.out.println("XY");

            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nx][ny];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    img[x][y] = (!obj.get(x + (nx * y) + (nx * ny * zS)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nx, ny);
            } else {
                label = connected4Object2D(img, nx, ny);
            }

            // remove the selected background
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    if (label[x][y] == label[xS][yS]) {
                        obj.set(x + (nx * y) + (nx * ny * zS), true);
                    }
                }
            }
        } else if ((backgroundDim == 2) && (sliceDir == XZ)) {
            //System.out.println("XZ");

            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nx][nz];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    img[x][z] = (!obj.get(x + (nx * yS) + (nx * ny * z)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nx, nz);
            } else {
                label = connected4Object2D(img, nx, nz);
            }

            // remove the selected background
            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {

                    if (label[x][z] == label[xS][zS]) {
                        obj.set(x + (nx * yS) + (nx * ny * z), true);
                    }
                }
            }
        } else if ((backgroundDim == 2) && (sliceDir == ZY)) {
            //System.out.println("ZY");

            int[][] label;

            // extract the slice and invert
            boolean[][] img = new boolean[nz][ny];

            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {
                    img[z][y] = (!obj.get(xS + (nx * y) + (nx * ny * z)));
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected8Object2D(img, nz, ny);
            } else {
                label = connected4Object2D(img, nz, ny);
            }

            // remove the selected background
            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {

                    if (label[z][y] == label[zS][yS]) {
                        obj.set(xS + (nx * y) + (nx * ny * z), true);
                    }
                }
            }
        } else {
            int[][][] label;

            // extract the mask and invert
            boolean[][][] img = new boolean[nx][ny][nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
                        img[x][y][z] = (!obj.get(x + (nx * y) + (nx * ny * z)));
                    }
                }
            }

            // connected components on background
            if (connectType.equals("6/26")) {
                label = connected26Object3D(img, nx, ny, nz);
            } else if (connectType.equals("6/18")) {
                label = connected18Object3D(img, nx, ny, nz);
            } else {
                label = connected6Object3D(img, nx, ny, nz);
            }

            // remove the selected background
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {

                        if (label[x][y][z] == label[xS][yS][zS]) {
                            obj.set(x + (nx * y) + (nx * ny * z), true);
                        }
                    }
                }
            }
        }

        refreshImagePaint(image, obj);
    }

    /**
     * region growing algorithm.
     *
     * @param  xS        ModelImage-Space x coordinate
     * @param  yS        ModelImage-Space y coordinate
     * @param  zS        ModelImage-Space z coordinate
     * @param  sliceDir  (XY, XZ, ZY)
     */
    private void growRegion(int xS, int yS, int zS, int sliceDir) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // get parameters
        connectType = (String) comboConnectType.getSelectedItem();

        if (radioGrowRegion3D.isSelected()) {
            regionGrowDim = 3;
        } else {
            regionGrowDim = 2;
        }

        //float imgValue = image.get(xS, yS, zS).floatValue();

        // find main object
        if ((regionGrowDim == 2) && (sliceDir == XY)) {
            int[][] label;

            // find potential regions
            boolean[][] img = new boolean[nx][ny];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    img[x][y] = isInsideIntensityThreshold(x,y,zS,image,xS,yS,zS);
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, ny);
            } else {
                label = connected8Object2D(img, nx, ny);
            }

            // add to the paint mask
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    if (label[x][y]>0) {
						// only if inside an object
						if (label[x][y] == label[xS][yS]) {
							obj.set(x + (nx * y) + (nx * ny * zS), true);
						}
					}
                }
            }
        } else if ((backgroundDim == 2) && (sliceDir == XZ)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nx][nz];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    img[x][z] = isInsideIntensityThreshold(x,yS,z,image,xS,yS,zS);
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, nz);
            } else {
                label = connected8Object2D(img, nx, nz);
            }

            // add to paint
            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {

                    if (label[x][z]>0) {
						// only if inside an object
						if (label[x][z] == label[xS][zS]) {
							obj.set(x + (nx * yS) + (nx * ny * z), true);
						}
					}
                }
            }
        } else if ((backgroundDim == 2) && (sliceDir == ZY)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nz][ny];

            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {
                    img[z][y] = isInsideIntensityThreshold(xS,y,z,image,xS,yS,zS);
				}
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nz, ny);
            } else {
                label = connected8Object2D(img, nz, ny);
            }

            // remove the selected background
            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {

					if (label[z][y]>0) {
						// only if inside an object
					   if (label[z][y] == label[zS][yS]) {
							obj.set(xS + (nx * y) + (nx * ny * z), true);
						}
					}
                }
            }
        } else {
            int[][][] label;

            // extract the mask and invert
            boolean[][][] img = new boolean[nx][ny][nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
                        img[x][y][z] = isInsideIntensityThreshold(x,y,z,image,xS,yS,zS);
					}
                }
            }

            // connected components on background
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected6Object3D(img, nx, ny, nz);
            } else if (connectType.equals("18/6")) {
                label = connected18Object3D(img, nx, ny, nz);
            } else {
                label = connected26Object3D(img, nx, ny, nz);
            }

            // remove the selected background
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
						if (label[x][y][z]>0) {
							// only if inside an object
							if (label[x][y][z] == label[xS][yS][zS]) {
								obj.set(x + (nx * y) + (nx * ny * z), true);
							}
                        }
                    }
                }
            }
        }
		//trimIntensityThreshold(image, obj, previous);
                    
        refreshImagePaint(image, obj);
    }

    /**
     * import from Mask.
     */
    private void importFromMask() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // call the paint to mask program
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "MaskToPaint"));

        refreshImagePaint(image, obj);
    }

    /**
     * import from VOI.
     */
    private void importFromVOI() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // call the VOI to paint program
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "PaintMask"));

        // remove the VOI
        image.getParentFrame().getComponentImage().getVOIHandler().deleteSelectedVOI(true);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Paint Power Tools");

        // global parameters
        nx = image.getExtents()[0];
        ny = image.getExtents()[1];

        if (image.getNDims() < 3) {
            nz = 1;
        } else {
            nz = image.getExtents()[2];
        }

        createStructuringElement2D();
        createStructuringElement3D();

        // remember current paint
        previous = image.getParentFrame().getComponentImage().getPaintMask();

        // object processing
        buttonGrowRegion = new JButton("Grow Region");
        buttonGrowRegion.addActionListener(this);
        buttonGrowRegion.setActionCommand("GrowRegion");
        buttonGrowRegion.setFont(serif12);
        buttonGrowRegion.setToolTipText("Grow paint in the selected region");

        buttonFillBackground = new JButton("Fill Background");
        buttonFillBackground.addActionListener(this);
        buttonFillBackground.setActionCommand("Background");
        buttonFillBackground.setFont(serif12);
        buttonFillBackground.setToolTipText("Fill the selected background");

        buttonFillBackgrounds = new JButton("Fill All Background");
        buttonFillBackgrounds.addActionListener(this);
        buttonFillBackgrounds.setActionCommand("Backgrounds");
        buttonFillBackgrounds.setFont(serif12);
        buttonFillBackgrounds.setToolTipText("Fill all but the selected background");

        buttonRmObject = new JButton("Remove Object");
        buttonRmObject.addActionListener(this);
        buttonRmObject.setActionCommand("RmObject");
        buttonRmObject.setFont(serif12);
        buttonRmObject.setToolTipText("Remove the selected object");

        buttonRmObjects = new JButton("Remove All Objects");
        buttonRmObjects.addActionListener(this);
        buttonRmObjects.setActionCommand("RmObjects");
        buttonRmObjects.setFont(serif12);
        buttonRmObjects.setToolTipText("Remove all but the selected object");

        labelConnectType = new JLabel("connectivity: ");
        labelConnectType.setForeground(Color.black);
        labelConnectType.setFont(serif12);
        labelConnectType.setToolTipText("Set the object/background connectivity object processing (default: 6/26)");

        comboConnectType = new JComboBox(connectTypes);
        comboConnectType.setSelectedItem(connectType);
        comboConnectType.setFont(serif12);
        comboConnectType.setBackground(Color.white);

        labelO2D = new JLabel("2D");
        labelO2D.setForeground(Color.black);
        labelO2D.setFont(serif12);

        labelO3D = new JLabel("3D");
        labelO3D.setForeground(Color.black);
        labelO3D.setFont(serif12);

        radioGrowRegion2D = new JRadioButton();
        radioGrowRegion3D = new JRadioButton();

        radioBackground2D = new JRadioButton();
        radioBackground3D = new JRadioButton();
        radioBackgrounds2D = new JRadioButton();
        radioBackgrounds3D = new JRadioButton();

        radioObject2D = new JRadioButton();
        radioObject3D = new JRadioButton();
        radioObjects2D = new JRadioButton();
        radioObjects3D = new JRadioButton();

        groupGrowRegion = new ButtonGroup();
        groupGrowRegion.add(radioGrowRegion2D);
        groupGrowRegion.add(radioGrowRegion3D);

        if (regionGrowDim == 2) {
            radioGrowRegion2D.setSelected(true);
        } else {
            radioGrowRegion3D.setSelected(true);
        }

        groupBackground = new ButtonGroup();
        groupBackground.add(radioBackground2D);
        groupBackground.add(radioBackground3D);

        if (backgroundDim == 2) {
            radioBackground2D.setSelected(true);
        } else {
            radioBackground3D.setSelected(true);
        }

        groupBackgrounds = new ButtonGroup();
        groupBackgrounds.add(radioBackgrounds2D);
        groupBackgrounds.add(radioBackgrounds3D);

        if (backgroundsDim == 2) {
            radioBackgrounds2D.setSelected(true);
        } else {
            radioBackgrounds3D.setSelected(true);
        }

        groupObject = new ButtonGroup();
        groupObject.add(radioObject2D);
        groupObject.add(radioObject3D);

        if (rmObjDim == 2) {
            radioObject2D.setSelected(true);
        } else {
            radioObject3D.setSelected(true);
        }

        groupObjects = new ButtonGroup();
        groupObjects.add(radioObjects2D);
        groupObjects.add(radioObjects3D);

        if (rmObjsDim == 2) {
            radioObjects2D.setSelected(true);
        } else {
            radioObjects3D.setSelected(true);
        }

        // morphology operations
        buttonDilate = new JButton("Dilation");
        buttonDilate.addActionListener(this);
        buttonDilate.setActionCommand("Dilate");
        buttonDilate.setFont(serif12);
        buttonDilate.setToolTipText("Dilate the object with a ball");

        buttonErode = new JButton("Erosion");
        buttonErode.addActionListener(this);
        buttonErode.setActionCommand("Erode");
        buttonErode.setFont(serif12);
        buttonErode.setToolTipText("Erode the object with a ball");

        comboErodeDimType = new JComboBox(dimTypes);
        comboErodeDimType.setSelectedItem(erodeDimType);
        comboErodeDimType.setFont(serif12);
        comboErodeDimType.setBackground(Color.white);

        comboDilateDimType = new JComboBox(dimTypes);
        comboDilateDimType.setSelectedItem(dilateDimType);
        comboDilateDimType.setFont(serif12);
        comboDilateDimType.setBackground(Color.white);

        labelStructureType = new JLabel("Element: ");
        labelStructureType.setForeground(Color.black);
        labelStructureType.setFont(serif12);
        labelStructureType.setToolTipText("Set the shape of the erosion/dilation element");

        comboStructureType = new JComboBox(structureTypes);
        comboStructureType.setSelectedItem(structureType);
        comboStructureType.setFont(serif12);
        comboStructureType.setBackground(Color.white);

        labelStructuring = new JLabel("Scale (mm) ");
        labelStructuring.setForeground(Color.black);
        labelStructuring.setFont(serif12);
        labelStructuring.setToolTipText("Set the scale of the erosion/dilation element (default: 5mm)");

        textStructuring = new JTextField(5);
        textStructuring.setText(String.valueOf(structureSize));
        textStructuring.setFont(serif12);

        checkSave = new JCheckBox("Auto save :");
        checkSave.addActionListener(this);
        checkSave.setActionCommand("Autosave");
        checkSave.setSelected(false);
        checkSave.setToolTipText("Enable automatic saving of the paint mask every x minutes");

        textSave = new JTextField(5);
        textSave.setText(String.valueOf(delay));
        textSave.setFont(serif12);

        // import/export
        buttonExportToVOI = new JButton("Paint to VOI");
        buttonExportToVOI.addActionListener(this);
        buttonExportToVOI.setActionCommand("ExportToVOI");
        buttonExportToVOI.setFont(serif12);

        buttonImportFromVOI = new JButton("VOI to Paint");
        buttonImportFromVOI.addActionListener(this);
        buttonImportFromVOI.setActionCommand("ImportFromVOI");
        buttonImportFromVOI.setFont(serif12);

        buttonExportToMask = new JButton("Paint to Mask");
        buttonExportToMask.addActionListener(this);
        buttonExportToMask.setActionCommand("ExportToMask");
        buttonExportToMask.setFont(serif12);

        buttonImportFromMask = new JButton("Mask to Paint");
        buttonImportFromMask.addActionListener(this);
        buttonImportFromMask.setActionCommand("ImportFromMask");
        buttonImportFromMask.setFont(serif12);

        buttonRevert = new JButton("Undo last");
        buttonRevert.addActionListener(this);
        buttonRevert.setActionCommand("Revert");
        buttonRevert.setFont(serif12);

        buttonShortkeys = new JToggleButton("Use shortkeys");
        buttonShortkeys.addActionListener(this);
        buttonShortkeys.setActionCommand("Shortkeys");
        buttonShortkeys.setFont(serif12);
        buttonShortkeys.setToolTipText("Use shortkeys for all commands: g: grow, f: fill, r: remove, d: dilate, e:erode");

        checkThreshold = new JCheckBox("Threshold");
        checkThreshold.addActionListener(this);
        checkThreshold.setActionCommand("Threshold");
        checkThreshold.setToolTipText("Enable the use of intensity thresholds when painting");

		
		lowerThreshold = (float)image.getMin();
		upperThreshold = (float)image.getMax();
		float step = (upperThreshold-lowerThreshold)/100.0f;
		spinLower = new JSpinner(new SpinnerNumberModel(lowerThreshold,lowerThreshold,upperThreshold,step));
		spinLower.addChangeListener(this);
		spinLower.setToolTipText("Minimum intensity value for painting");

		spinUpper = new JSpinner(new SpinnerNumberModel(upperThreshold,lowerThreshold,upperThreshold,step));
		spinUpper.addChangeListener(this);
		spinUpper.setToolTipText("Maximum intensity value for painting");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 2, 2, 2);

        panelThreshold = new JPanel(new GridBagLayout());
        panelThreshold.setForeground(Color.black);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        panelThreshold.add(checkThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        panelThreshold.add(spinLower, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        panelThreshold.add(spinUpper, gbc);

        objectPanel = new JPanel(new GridBagLayout());
        objectPanel.setBorder(buildTitledBorder("Object Processing"));

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(labelO2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(labelO3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(buttonGrowRegion, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioGrowRegion2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioGrowRegion3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(buttonFillBackground, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioBackground2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioBackground3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(buttonFillBackgrounds, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioBackgrounds2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioBackgrounds3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(buttonRmObject, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioObject2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioObject3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(buttonRmObjects, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioObjects2D, gbc);
        gbc.gridx = 2;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(radioObjects3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(labelConnectType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.NONE;
        objectPanel.add(comboConnectType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        objectPanel.add(panelThreshold, gbc);

        morphoPanel = new JPanel(new GridBagLayout());
        morphoPanel.setBorder(buildTitledBorder("Morphology"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        morphoPanel.add(buttonErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        morphoPanel.add(comboErodeDimType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        morphoPanel.add(buttonDilate, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        morphoPanel.add(comboDilateDimType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        morphoPanel.add(labelStructureType, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        morphoPanel.add(comboStructureType, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        morphoPanel.add(labelStructuring, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        morphoPanel.add(textStructuring, gbc);

        movePanel = new JPanel(new GridBagLayout());
        movePanel.setBorder(buildTitledBorder("Misc."));

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        movePanel.add(checkSave, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        movePanel.add(textSave, gbc);

        exportPanel = new JPanel(new GridBagLayout());
        exportPanel.setBorder(buildTitledBorder("Import / Export"));

        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        exportPanel.add(buttonExportToVOI, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        exportPanel.add(buttonImportFromVOI, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        exportPanel.add(buttonExportToMask, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        exportPanel.add(buttonImportFromMask, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(objectPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(morphoPanel, gbc);
        gbc.gridy = 2;
        gbc.insets = new Insets(2, 2, 2, 2);
        mainPanel.add(buttonRevert, gbc);
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.gridy = 3;
        mainPanel.add(movePanel, gbc);
        gbc.gridy = 4;
        mainPanel.add(exportPanel, gbc);
		gbc.gridy = 6;
        mainPanel.add(buttonShortkeys, gbc);

        botPanel = new JPanel();
        botPanel.add(buildCloseButton());
        botPanel.add(buildHelpButton());

        getContentPane().add(mainPanel);
        getContentPane().add(botPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        //setResizable(false);
        System.gc();
		
		image.getParentFrame().getComponentImage().addMouseListener(this);

    } // end init()

    /**
     * propagate to all slices.
     */
    private void propagateAll() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // extract slice
        int zS = image.getParentFrame().getComponentImage().getSlice();
        int nxny = nx * ny;
        int nxnyzS = nxny * zS;
        int nxnyzSp = nxnyzS + nxny;
        int indx;

        // copy the paint mask one slice down
        for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                 index = obj.nextSetBit(index + 1)) {
            indx = index % nxny;

            for (int i = 0; i < nz; i++) {
                obj.set(indx + (i * nxny), true);
            }
        }

        return;
    }

    /**
     * propagate to the next slice.
     */
    private void propagateDown() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // extract slice
        int zS = image.getParentFrame().getComponentImage().getSlice();

        if (zS == 0) {
            return;
        }

        int nxny = nx * ny;
        int nxnyzS = nxny * zS;
        int nxnyzSp = nxnyzS + nxny;

        // copy the paint mask one slice down
        for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                 index = obj.nextSetBit(index + 1)) {
            obj.set(index - nxny, true);
        }

        // move to next slice
        image.getParentFrame().getComponentImage().setSlice(zS - 1);
        image.getParentFrame().setSlice(zS - 1);
        image.getParentFrame().updateImages(true);

        return;
    }

    /**
     * propagate to the next slice.
     */
    private void propagateUp() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // extract slice
        int zS = image.getParentFrame().getComponentImage().getSlice();

        if (zS == (nz - 1)) {
            return;
        }

        int nxny = nx * ny;
        int nxnyzS = nxny * zS;
        int nxnyzSp = nxnyzS + nxny;

        // copy the paint mask one slice up
        for (int index = obj.nextSetBit(nxnyzS); ((index >= 0) && (index < nxnyzSp));
                 index = obj.nextSetBit(index + 1)) {
            obj.set(index + nxny, true);
        }

        // move to next slice
        image.getParentFrame().getComponentImage().setSlice(zS + 1);
        image.getParentFrame().setSlice(zS + 1);
        image.getParentFrame().updateImages(true);

        return;
    }

    /**
     * refresh the displayed mask.
     *
     * @param  img  DOCUMENT ME!
     * @param  obj  DOCUMENT ME!
     */
    private void refreshImagePaint(ModelImage img, BitSet obj) {

        // replace it by previous
        img.getParentFrame().getComponentImage().setPaintMask(obj);
        img.setMask(obj);

        // show result
        img.getParentFrame().updateImages(true);

        if (img.getTriImageFrame() != null) {
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_A).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_A).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_A).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_B).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_B).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_B).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.AXIAL_AB).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.SAGITTAL_AB).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(ViewJFrameTriImage.CORONAL_AB).setPaintMask(obj);
            img.getTriImageFrame().updateImages(true);
        }
    }

    /**
     * check if point (x,y,z) has same intensity as (xS,yS,zS)
     *
     */
    private boolean isInsideIntensityThreshold(int x, int y, int z, ModelImage img, int xS, int yS, int zS) {
		int index = x+nx*y+nx*ny*z;
		double val;
		if (img.isColorImage()) {
			// color: use intensity ?
			val = Math.sqrt( img.getDouble(4*index+1)*img.getDouble(4*index+1)
							+img.getDouble(4*index+2)*img.getDouble(4*index+2)
							+img.getDouble(4*index+3)*img.getDouble(4*index+3) );
		} else {
			val = img.getDouble(index);
		}	
		if (checkThreshold.isSelected()) {
			return ( (val>=lowerThreshold) && (val<=upperThreshold));	
		} else {
			int indexS = xS+nx*yS+nx*ny*zS;
			double valS;
			
			if (img.isColorImage()) {
				// color: use intensity
				valS = Math.sqrt( img.getDouble(4*indexS+1)*img.getDouble(4*indexS+1)
								 +img.getDouble(4*indexS+2)*img.getDouble(4*indexS+2)
								 +img.getDouble(4*indexS+3)*img.getDouble(4*indexS+3) );
			} else {
				valS = img.getDouble(indexS);
			}	
			
			return (val==valS);
		}
	}

    /**
     * trim the mask using the intensity
     *
     * @param  img  DOCUMENT ME!
     * @param  obj  DOCUMENT ME!
     * @param  prev  DOCUMENT ME!
     */
    private void trimIntensityThreshold(ModelImage img, BitSet obj, BitSet prev) {
		if (checkThreshold.isSelected()) {

			double min = lowerThreshold;
			double max = upperThreshold;
					
            for (int index = obj.nextSetBit(0); index >= 0; index = obj.nextSetBit(index + 1)) {
				// check the previous paint first: no change there
                if (!prev.get(index)) {
					double val = 0;
					if (img.isColorImage()) {
						// color: use intensity
						val = Math.sqrt( img.getDouble(4*index+1)*img.getDouble(4*index+1)
										+img.getDouble(4*index+2)*img.getDouble(4*index+2)
										+img.getDouble(4*index+3)*img.getDouble(4*index+3) );
					} else {
						// grayscale images
						val = img.getDouble(index);
					}
					
                    if ((val < min) || (val > max)) {
						obj.set(index, false);
                    }
                }
			}
		}
		return;
	}

    /**
     * object removal algorithm.
     *
     * @param  xS        ModelImage-Space x coordinate
     * @param  yS        ModelImage-Space y coordinate
     * @param  zS        ModelImage-Space z coordinate
     * @param  sliceDir  (XY, XZ, ZY)
     */
    private void removeAllObjects(int xS, int yS, int zS, int sliceDir) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // get parameters
        connectType = (String) comboConnectType.getSelectedItem();

        if (radioObjects3D.isSelected()) {
            rmObjsDim = 3;
        } else {
            rmObjsDim = 2;
        }

        // find main object
        if ((rmObjsDim == 2) && (sliceDir == XY)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nx][ny];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    img[x][y] = obj.get(x + (nx * y) + (nx * ny * zS));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, ny);
            } else {
                label = connected8Object2D(img, nx, ny);
            }

            // remove all but the selected object
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    if (label[x][y] != label[xS][yS]) {
                        obj.set(x + (nx * y) + (nx * ny * zS), false);
                    }
                }
            }
        } else if ((rmObjsDim == 2) && (sliceDir == XZ)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nx][nz];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    img[x][z] = obj.get(x + (nx * yS) + (nx * ny * z));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, nz);
            } else {
                label = connected8Object2D(img, nx, nz);
            }

            // remove all but the selected object
            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {

                    if (label[x][z] != label[xS][zS]) {
                        obj.set(x + (nx * yS) + (nx * ny * z), false);
                    }
                }
            }
        } else if ((rmObjsDim == 2) && (sliceDir == ZY)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nz][ny];

            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {
                    img[z][y] = obj.get(xS + (nx * y) + (nx * ny * z));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nz, ny);
            } else {
                label = connected8Object2D(img, nz, ny);
            }

            // remove all but the selected object
            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {

                    if (label[z][y] != label[zS][yS]) {
                        obj.set(xS + (nx * y) + (nx * ny * z), false);
                    }
                }
            }
        } else {
            int[][][] label;

            // extract the slice
            boolean[][][] img = new boolean[nx][ny][nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
                        img[x][y][z] = obj.get(x + (nx * y) + (nx * ny * z));
                    }
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected6Object3D(img, nx, ny, nz);
            } else if (connectType.equals("18/6")) {
                label = connected18Object3D(img, nx, ny, nz);
            } else {
                label = connected26Object3D(img, nx, ny, nz);
            }

            // remove all but the selected object
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {

                        if (label[x][y][z] != label[xS][yS][zS]) {
                            obj.set(x + (nx * y) + (nx * ny * z), false);
                        }
                    }
                }
            }
        }

        refreshImagePaint(image, obj);
    }

    /**
     * object removal algorithm.
     *
     * @param  xS        ModelImage-Space x coordinate
     * @param  yS        ModelImage-Space y coordinate
     * @param  zS        ModelImage-Space z coordinate
     * @param  sliceDir  (XY, XZ, ZY)
     */
    private void removeObject(int xS, int yS, int zS, int sliceDir) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("paint mask not found");

            return;
        }

        // save it to previous
        previous = (BitSet) obj.clone();

        // get parameters
        connectType = (String) comboConnectType.getSelectedItem();

        if (radioObject3D.isSelected()) {
            rmObjDim = 3;
        } else {
            rmObjDim = 2;
        }

        // find main object
        if ((rmObjDim == 2) && (sliceDir == XY)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nx][ny];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    img[x][y] = obj.get(x + (nx * y) + (nx * ny * zS));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, ny);
            } else {
                label = connected8Object2D(img, nx, ny);
            }

            // remove the selected object
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    if (label[x][y] == label[xS][yS]) {
                        obj.set(x + (nx * y) + (nx * ny * zS), false);
                    }
                }
            }
        } else if ((rmObjDim == 2) && (sliceDir == XZ)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nx][nz];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    img[x][z] = obj.get(x + (nx * yS) + (nx * ny * z));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nx, nz);
            } else {
                label = connected8Object2D(img, nx, nz);
            }

            // remove the selected object
            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {

                    if (label[x][z] == label[xS][zS]) {
                        obj.set(x + (nx * yS) + (nx * ny * z), false);
                    }
                }
            }
        } else if ((rmObjDim == 2) && (sliceDir == ZY)) {
            int[][] label;

            // extract the slice
            boolean[][] img = new boolean[nz][ny];

            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {
                    img[z][y] = obj.get(xS + (nx * y) + (nx * ny * z));
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected4Object2D(img, nz, ny);
            } else {
                label = connected8Object2D(img, nz, ny);
            }

            // remove the selected object
            for (int z = 0; z < nz; z++) {

                for (int y = 0; y < ny; y++) {

                    if (label[z][y] == label[zS][yS]) {
                        obj.set(xS + (nx * y) + (nx * ny * z), false);
                    }
                }
            }
        } else {
            int[][][] label;

            // extract the slice
            boolean[][][] img = new boolean[nx][ny][nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {
                        img[x][y][z] = obj.get(x + (nx * y) + (nx * ny * z));
                    }
                }
            }

            // connected components
            if (connectType.equals("6/26") || connectType.equals("6/18")) {
                label = connected6Object3D(img, nx, ny, nz);
            } else if (connectType.equals("18/6")) {
                label = connected18Object3D(img, nx, ny, nz);
            } else {
                label = connected26Object3D(img, nx, ny, nz);
            }

            // remove the selected object
            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {

                    for (int z = 0; z < nz; z++) {

                        if (label[x][y][z] == label[xS][yS][zS]) {
                            obj.set(x + (nx * y) + (nx * ny * z), false);
                        }
                    }
                }
            }
        }

        refreshImagePaint(image, obj);
    }

    /**
     * revert to previous mask.
     */
    private void revertImage() {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // no previous image
        if (previous == null) {
            return;
        }

        // replace it by previous
        refreshImagePaint(image, previous);

        // put current into previous
        previous = (BitSet) obj.clone();
    }
}

/**
 * DOCUMENT ME!
 */
class PaintAutoSave extends TimerTask {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    ModelImage image;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PaintAutoSave object.
     *
     * @param  img  DOCUMENT ME!
     */
    public PaintAutoSave(ModelImage img) {
        image = img;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void run() {

        // transfer the paint to a ModelImage
        //System.out.println("saving the paint");

        ModelImage tmp = new ModelImage(ModelImage.BOOLEAN, image.getExtents(), "paint_autosave");

        try {
            tmp.importData(0, image.getParentFrame().getComponentImage().getPaintMask(), true);

            FileImageXML file = new FileImageXML("paint_autosave", image.getFileInfo(0).getFileDirectory());

            FileWriteOptions opt = new FileWriteOptions(true);
            opt.setBeginSlice(0);

            if (image.getNDims() > 2) {
                opt.setEndSlice(image.getExtents()[2] - 1);
            } else {
                opt.setEndSlice(0);
            }

            file.writeHeader(tmp, opt, "paint_autosave", image.getFileInfo(0).getFileDirectory(), true);
            file.writeImage(tmp, opt);
            file = null;
        } catch (IOException io) { }

        tmp = null;
    }
}
