package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogFillObjects extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3464349858386521845L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** use later. */
    private ButtonGroup destinationGroup;

    /** panel that hold the new image radio button. */
    private JPanel destinationPanel;

    /** Flag indicating if a new image is to be generated. */
    private int displayLoc;

    /** Morphology2D algorithm reference. */
    private AlgorithmMorphology2D idObjectsAlgo2D = null;
    
    /** Morphology25D algorithm reference. */
    private AlgorithmMorphology25D idObjectsAlgo25D = null;

    /** Morphology3D algorithm reference. */
    private AlgorithmMorphology3D idObjectsAlgo3D = null;

    /** source image. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JCheckBox image25D;
    
    /** DOCUMENT ME! */
    private boolean do25D = false; // do a 2.5D erode on a 3D image

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** used later. */
    private JPanel imageVOIPanel;

    /** new image radio button. */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private boolean regionFlag = false;

    /** use later. */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private String[] titles;

    /** Usigned byte image, the actual image to do the morphology calculation. */
    private ModelImage ubyteImage = null;

    /** user interface renference. */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFillObjects() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogFillObjects(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
            dispose();

            return;
        }

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("Mor009FH");
        	MipavUtil.showWebHelp("Morphology#Applying_fill_holes");
        }
    }
    
    /**
     * Process the image in 2.5D.
     *
     * @param  b  whether to do 2.5D morphology
     */
    public void setImage25D(boolean b) {
        do25D = b;
    }


    /**
     * When the morphology algorithm finish running, this method is invoked.
     *
     * @param  algorithm  AlgorithmBase reference
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((idObjectsAlgo2D.isCompleted() == true) && (ubyteImage != null)) {
                updateFileInfo(image, ubyteImage);
                ubyteImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(ubyteImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (ubyteImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (ubyteImage != null) {

                // algorithm failed but result image still has garbage
                ubyteImage.disposeLocal(); // clean up memory
                ubyteImage = null;
            }
        } else if (algorithm instanceof AlgorithmMorphology25D) {
            image.clearMask();

            if ((idObjectsAlgo25D.isCompleted() == true) && (ubyteImage != null)) {
                updateFileInfo(image, ubyteImage);
                ubyteImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    new ViewJFrameImage(ubyteImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (ubyteImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (ubyteImage != null) {

                // algorithm failed but result image still has garbage
                ubyteImage.disposeLocal(); // clean up memory
                ubyteImage = null;
            }
        } else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((idObjectsAlgo3D.isCompleted() == true) && (ubyteImage != null)) {
                updateFileInfo(image, ubyteImage);
                ubyteImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(ubyteImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (ubyteImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (ubyteImage != null) {

                // algorithm failed but result image still has garbage
                ubyteImage.disposeLocal(); // clean up memory
                ubyteImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
    }

    /**
     * Accessor that returns the image after filling holes.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return ubyteImage;
    }

    /**
     * When OK button is clicked, this method is invoked.
     */
    protected void callAlgorithm() {

        int kernel = 0;
        String name = makeImageName(image.getImageName(), "_FillObjects");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (displayLoc == NEW) {

                try {
                    ubyteImage = new ModelImage(ModelImage.UBYTE, image.getExtents(), name);

                    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(ubyteImage, image, 0, 1, 0, 1, false);

                    changeTypeAlgo.run();

                    // Make algorithm
                    idObjectsAlgo2D = new AlgorithmMorphology2D(ubyteImage, kernel, 0, AlgorithmMorphology2D.FILL_HOLES,
                                                                0, 0, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        idObjectsAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    idObjectsAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo2D);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (idObjectsAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo2D.run();
                    }
                    // new ViewJFrameImage( tempImage, null, new Dimension( 300, 300 ), tempImage.getUserInterface(),
                    // false );
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog ID objects: unable to allocate enough memory");

                    if (ubyteImage != null) {
                        ubyteImage.disposeLocal(); // Clean up memory of result image
                        ubyteImage = null;
                    }

                    return;
                }
            } else { // the replace image part might be done later

                /*
                 * try { // No need to make new image space because the user has choosen to replace the source image //
                 * Make the algorithm class // dilateAlgo2D = new AlgorithmMorphology2D( image, kernel, kernelSize,
                 * AlgorithmMorphology2D.DILATE, //       itersD, 0, 0, 0, regionFlag ); // idObjectsAlgo2D = new
                 * AlgorithmMorphology2D(resultImage, kernel, 0, //  AlgorithmMorphology2D.FILL_OBJECTS, 0, 0, 0, 0,
                 * regionFlag); resultImage = (ModelImage) image.clone(); resultImage.setImageName(name);
                 *
                 * ubyteImage = new ModelImage( ModelImage.UBYTE, resultImage.getExtents(), name,
                 * resultImage.getUserInterface() );
                 *
                 *
                 * AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType( ubyteImage, resultImage, 0, 1, 0, 1,
                 * false ); changeTypeAlgo.run();
                 *
                 * // Make algorithm idObjectsAlgo2D = new AlgorithmMorphology2D(ubyteImage, kernel, 0,
                 * AlgorithmMorphology2D.FILL_HOLES, 0, 0, 0, 0, regionFlag);
                 *
                 * if ( regionFlag == false ) { idObjectsAlgo2D.setMask( image.generateVOIMask() ); } // This is very
                 * important. Adding this object as a listener allows the algorithm to // notify this object when it has
                 * completed or failed. See algorithm performed event. // This is made possible by implementing
                 * AlgorithmedPerformed interface idObjectsAlgo2D.addListener( this ); // Hide the dialog since the
                 * algorithm is about to run. setVisible( false );
                 *
                 * // These next lines set the titles in all frames where the source image is displayed to // "locked - "
                 * image name so as to indicate that the image is now read/write locked! // The image frames are
                 * disabled and then unregisted from the userinterface until the // algorithm has completed. Vector
                 * imageFrames = image.getImageFrameVector();
                 *
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) ); }
                 *
                 * if ( isRunInSeparateThread() ) { // Start the thread as a low priority because we wish to still have
                 * user interface. if ( idObjectsAlgo2D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                 * MipavUtil.displayError( "A thread is already running on this object" ); } } else {
                 * idObjectsAlgo2D.setActiveImage( isActiveImage ); if ( !userInterface.isAppFrameVisible() ) {
                 * idObjectsAlgo2D.setProgressBarVisible( false ); } idObjectsAlgo2D.run(); } } catch ( OutOfMemoryError
                 * x ) { MipavUtil.displayError( "Dialog dilate: unable to allocate enough memory" ); return; }
                 */
            }
        } else if (image.getNDims() == 3 && !do25D) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (displayLoc == NEW) {

                try {
                    ubyteImage = new ModelImage(ModelImage.UBYTE, image.getExtents(), name);

                    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(ubyteImage, image, 0, 1, 0, 1, false);

                    changeTypeAlgo.run();

                    // Make algorithm
                    idObjectsAlgo3D = new AlgorithmMorphology3D(ubyteImage, kernel, 0, AlgorithmMorphology3D.FILL_HOLES,
                                                                0, 0, 0, 0, regionFlag);

                    // idObjectsAlgo3D.setMinMax(min, max);
                    if (regionFlag == false) {
                        idObjectsAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    idObjectsAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo3D);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Fill objects: unable to allocate enough memory");

                    if (ubyteImage != null) {
                        ubyteImage.disposeLocal(); // Clean up image memory
                        ubyteImage = null;
                    }

                    return;
                }
            } else { // replace image part is done later

                /*
                 * try { // Make algorithm idObjectsAlgo3D = new AlgorithmMorphology3D(image, kernel, 0,
                 * AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, regionFlag); // idObjectsAlgo3D.setMinMax(min, max); if
                 * (regionFlag == false) { idObjectsAlgo3D.setMask(image.generateVOIMask()); // This is very important.
                 * Adding this object as a listener allows the algorithm to // notify this object when it has completed
                 * of failed. See algorithm performed event. // This is made possible by implementing
                 * AlgorithmedPerformed interface } idObjectsAlgo3D.addListener(this); // Hide dialog setVisible(false);
                 *
                 * // These next lines set the titles in all frames where the source image is displayed to // "locked - "
                 * image name so as to indicate that the image is now read/write locked! // The image frames are
                 * disabled and then unregisted from the userinterface until the // algorithm has completed. Vector
                 * imageFrames = image.getImageFrameVector(); titles = new String[imageFrames.size()]; for (int i = 0; i
                 * < imageFrames.size(); i++) { titles[i] = ( (ViewJFrameBase) (imageFrames.elementAt(i))).getTitle(); (
                 * (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]); ( (ViewJFrameBase)
                 * (imageFrames.elementAt(i))).setEnabled(false); userInterface.unregisterFrame( (Frame)
                 * (imageFrames.elementAt(i))); }
                 *
                 * if (isRunInSeparateThread()) { // Start the thread as a low priority because we wish to still have user
                 * interface work fast if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                 * MipavUtil.displayError( "A thread is already running on this object"); } } else {
                 * idObjectsAlgo3D.setActiveImage(isActiveImage); if (!userInterface.isAppFrameVisible()) {
                 * idObjectsAlgo3D.setProgressBarVisible(false); } idObjectsAlgo3D.run(); } } catch (OutOfMemoryError x)
                 * { MipavUtil.displayError( "Dialog ID objects: unable to allocate enough memory"); return; } */
            }
        } else if (do25D) {

            

            if (displayLoc == NEW) {

                try {
                    ubyteImage = new ModelImage(ModelImage.UBYTE, image.getExtents(), name);
                    
                    
                    

                    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(ubyteImage, image, 0, 1, 0, 1, true);

                    changeTypeAlgo.run();
                   

                    
                    // Make algorithm
                    idObjectsAlgo25D = new AlgorithmMorphology25D(ubyteImage, kernel, 0,
                                                              AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        idObjectsAlgo25D.setMask(image.generateVOIMask());
                    }
                    
                   

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    idObjectsAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo25D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (idObjectsAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

                    if (ubyteImage != null) {
                        ubyteImage.disposeLocal(); // Clean up memory of result image
                        ubyteImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    idObjectsAlgo25D = new AlgorithmMorphology25D(image, kernel, 0,
                                                              AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        idObjectsAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    idObjectsAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), idObjectsAlgo25D);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (idObjectsAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        idObjectsAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Fill objects: unable to allocate enough memory");

                    return;
                }
            }
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if ((image.getType() != ModelImage.BOOLEAN) && (image.getType() != ModelImage.UBYTE) &&
                (image.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
            dispose();

            return;
        }

        displayLoc = NEW;
        setImage25D(scriptParameters.getParams().getBoolean("do_25D"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_25D", do25D));
    }


    /**
     * Initial control panel.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Fill holes");

        destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        // following part may be used later sometimes
        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Process"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        image25D = new JCheckBox("Process image in 2.5D", false);
        image25D.setFont(serif12);

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        //imageVOIPanel.add(wholeImage, gbc);
        //gbc.gridy = 1;
        //imageVOIPanel.add(VOIRegions, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridy = 0;
        imageVOIPanel.add(image25D, gbc);

        if (image.getNDims() == 3) {
            image25D.setEnabled(true);
        } else {
            image25D.setEnabled(false);
        }

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        JPanel controlPanel = new JPanel();

        controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS));
        controlPanel.add(imageVOIPanel);
        //controlPanel.add(destinationPanel);

        controlPanel.add(buttonPanel);
        getContentPane().add(controlPanel);
        pack();
        setVisible(true);

    }

    /**
     * Set the variable when the OK button is clicked.
     *
     * @return  boolean always true
     */
    private boolean setVariables() {
        System.gc();

        //if (replaceImage.isSelected()) {
        //    displayLoc = REPLACE;
        //} else if (newImage.isSelected()) {
        //    displayLoc = NEW;

        //}
        displayLoc = NEW;

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }
        
        do25D = image25D.isSelected();

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("2D flood fill that fills the holes insize the cell region block.");
            }

            public String getDescriptionLong() {
                return new String("2D flood fill that fills the holes insize the cell region block.");
            }

            public String getShortLabel() {
                return new String("FillObjects");
            }

            public String getLabel() {
                return new String("Fill Objects");
            }

            public String getName() {
                return new String("Fill Objects");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
   public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean("do_25D", false));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
