package gov.nih.mipav.view.dialogs;


import WildMagic.LibFoundation.Curves.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterString;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 July 28, 2004
 * @author   William Gandler TransformBSpline dialog This dialog takes a source image and uses information read in from
 *           a .nlt file to perform a nonlinear B-Spline transformation on the image. The dimensions of the transformed
 *           image, the degree of the B-Spline(ranging from 1 to 4), the number of control points, and the values of the
 *           control points are obtained from the .nlt file. For the .nlt file to be usable, the dimensions of the
 *           source image used in the .nlt file must be the same as the dimensions of the source image used in the
 *           algorithm.
 */

public class JDialogTransformBSpline extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener,
                                                                              ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3877682545436641450L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTransformBSpline algoTrans;

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private float[][] controlMat;

    /** DOCUMENT ME! */
    private float[][][] controlMat25D;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private int destMinExtent;

    /** DOCUMENT ME! */
    private boolean have25D = false;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private int nDims;

    /** DOCUMENT ME! */
    private String nltName;

    /** DOCUMENT ME! */
    private int numberSlices;

    /** DOCUMENT ME! */
    private int numControlPoints;

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private float[] resolutions;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // transformed image

    /** DOCUMENT ME! */
    private int splineDegree;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private String directory;
    
    private File nltFile;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    public JDialogTransformBSpline() {
    	
    }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present. This constructor is used by the script parser because it doesn't have the parent frame.
     *
     * @param  im  Source image.
     */
    public JDialogTransformBSpline(ModelImage im) {
        super();
        setForeground(Color.black);
        image = im;
        this.userInterface = ViewUserInterface.getReference();

        init();
    }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTransformBSpline(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        setForeground(Color.black);
        image = im;
        userInterface = ViewUserInterface.getReference();

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Choose")) {

            if (open()) {
                model.addElement(nltName);
                removeButton.setEnabled(true);
                chooserButton.setEnabled(false);
            }
        } // if (command.equals("Choose"))
        else if (command.equals("Remove")) {
            model.removeElement(nltName);
            removeButton.setEnabled(false);
            chooserButton.setEnabled(true);
        } // else if ((command.equals("Remove"))

        if (command.equals("OK")) {
            callAlgorithm();
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
        else if (command.equals("Help")) {
            //MipavUtil.showHelp("TransNon10");
            MipavUtil.showWebHelp("Transform_nonlinear");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************


    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmTransformBSpline) {
            resultImage = algoTrans.getTransformedImage();

            if ((algoTrans.isCompleted() == true) && (resultImage != null)) {
                resultImage.calcMinMax();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
            
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }

        System.gc();

        // Update frames
        image.notifyImageDisplayListeners(null, true);

        // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (algoTrans != null) {
            algoTrans.disposeLocal();
            algoTrans = null;
        }

        dispose();
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) { }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            if (have25D) {
                algoTrans = new AlgorithmTransformBSpline(image, resolutions, splineDegree, numControlPoints,
                                                          controlMat25D);

            } else {
                algoTrans = new AlgorithmTransformBSpline(image, resolutions, destExtents, splineDegree,
                                                          numControlPoints, controlMat);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoTrans.addListener(this);

            createProgressBar(image.getImageName(), algoTrans);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoTrans.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoTrans.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Makes the GUI elements of the dialog.
     */
    private void init() {
        setTitle("Nonlinear B-Spline Transformation");
        getContentPane().setLayout(new BorderLayout());

        JPanel nltPanel = new JPanel(new BorderLayout());
        nltPanel.setBorder(buildTitledBorder("Open NLT file"));

        model = new DefaultListModel();

        JList nltList = new JList(model);
        nltList.setVisibleRowCount(1);
        nltList.setPreferredSize(new Dimension(300, 30));
        nltList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        nltList.addListSelectionListener(this);
        nltPanel.add(nltList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        nltPanel.add(chooserPanel, BorderLayout.SOUTH);
        getContentPane().add(nltPanel);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());
        
        //buildOKButton();
        //buttonPanel.add(OKButton);
        //buildCancelButton();
        //buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);
    }

    /**
     * Open an image based on the suffix of the file. Read in the values of the parameters
     *
     * @return  boolean indicasting success of file read
     */
    private boolean open() {
        JFileChooser chooser = null;
        boolean success;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.NLT));

            chooser.setDialogTitle("Open B-Spline parameter file");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                nltFile = chooser.getSelectedFile();
                nltName = nltFile.getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return false;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return false;
        }

        success = openNLT();
        return success;

    }
    
    private boolean openNLT() {
    	RandomAccessFile in;
        String str = null;
        StringTokenizer stoken = null;
        int i, j, k;
        int srcMinExtent;
        int iNumControlPointsMax;
    	// open the file containing B-Spline parameters

        if (nltName.endsWith("nlt")) {

            try {
                in = new RandomAccessFile(nltFile, "r");

                // read number of dimensions
                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                float fDims = Float.valueOf(str).floatValue();

                if (2.5f == fDims) {
                    nDims = 3;
                    have25D = true;
                } else {
                    nDims = (int) fDims;
                    have25D = false;
                }

                if (image.getNDims() != nDims) {
                    MipavUtil.displayError("Error! " + image.getImageName() + " is " + image.getNDims() +
                                           "D, while parameter file nDims is " + nDims + "D");
                    in.close();

                    return false;
                }

                // read resolutions for output image
                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                stoken = new StringTokenizer(str);
                resolutions = new float[nDims];
                srcMinExtent = Integer.MAX_VALUE;

                for (i = 0; i < nDims; i++) {
                    resolutions[i] = Float.valueOf(stoken.nextToken()).floatValue();

                    if ((image.getExtents()[i] < srcMinExtent) && ((!have25D) || (i < 2))) {
                        srcMinExtent = image.getExtents()[i];
                    }
                }

                // If 2D/3D, read dimensions for target image
                if (!have25D) {

                    do {
                        str = in.readLine().trim();
                    } while (str.substring(0, 1).equals("#"));

                    stoken = new StringTokenizer(str);
                    destExtents = new int[nDims];
                    destMinExtent = Integer.MAX_VALUE;

                    for (i = 0; i < nDims; i++) {
                        destExtents[i] = Integer.valueOf(stoken.nextToken()).intValue();

                        if (destExtents[i] < destMinExtent) {
                            destMinExtent = destExtents[i];
                        }
                    }
                } else {
                    numberSlices = image.getExtents()[2];
                }

                // read B-spline degree
                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                stoken = new StringTokenizer(str);
                splineDegree = Integer.valueOf(stoken.nextToken()).intValue();

                if ((splineDegree < 1) || (splineDegree > 4)) {
                    MipavUtil.displayError("Error! Spline degree has an illegal value = " + splineDegree);
                    in.close();

                    return false;
                }

                // read number of control points
                do {
                    str = in.readLine().trim();
                } while (str.substring(0, 1).equals("#"));

                stoken = new StringTokenizer(str);
                numControlPoints = Integer.valueOf(stoken.nextToken()).intValue();

                int iNumControlPointsMin = BSplineBasisf.GetMinNumControlPoints(splineDegree);

                if (have25D) {
                    iNumControlPointsMax = srcMinExtent / 2;
                } else {
                    iNumControlPointsMax = destMinExtent / 2;
                }

                if (numControlPoints < iNumControlPointsMin) {
                    MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints +
                                           " control points, but " + iNumControlPointsMin + " are required");
                    in.close();

                    return false;
                }

                if (numControlPoints > iNumControlPointsMax) {
                    MipavUtil.displayError("Error! The parameter file specifies " + numControlPoints +
                                           " control points, but no more than " + iNumControlPointsMax +
                                           " are allowed");
                    in.close();

                    return false;
                }

                if (!have25D) {
                    int allDimControlPoints = (nDims == 2) ? (numControlPoints * numControlPoints)
                                                           : (numControlPoints * numControlPoints * numControlPoints);

                    controlMat = new float[allDimControlPoints][nDims];

                    for (i = 0; i < allDimControlPoints; i++) {

                        do {
                            str = in.readLine().trim();
                        } while (str.substring(0, 1).equals("#"));

                        stoken = new StringTokenizer(str);

                        for (j = 0; j < nDims; j++) {
                            controlMat[i][j] = Float.valueOf(stoken.nextToken()).floatValue();
                        }
                    } // for (i = 0; i < allDimControlPoints; i++)
                } // if (!have25D)
                else { // have25D

                    int allDimControlPoints = numControlPoints * numControlPoints;
                    controlMat25D = new float[numberSlices][allDimControlPoints][2];

                    for (k = 0; k < numberSlices; k++) {

                        for (i = 0; i < allDimControlPoints; i++) {

                            do {
                                str = in.readLine().trim();
                            } while (str.substring(0, 1).equals("#"));

                            stoken = new StringTokenizer(str);
                            controlMat25D[k][i][0] = Float.valueOf(stoken.nextToken()).floatValue();
                            controlMat25D[k][i][1] = Float.valueOf(stoken.nextToken()).floatValue();
                        } // for (i = 0; i < allDimControlPoints; i++)
                    } // for (k = 0; k < numberSlices; k++)
                } // else have25D

                in.close();

                return true;
            } catch (IOException e) {
                MipavUtil.displayError("Read Error on " + nltName + "  +" + e.getMessage());

                return false;
            }
        } else { // nltName does not end with .nlt
            return false;
        }	
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
        	//System.out.println("beginning input params");
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            
            table.put(new ParameterString("nlt_name"));
            table.put(new ParameterString("nlt_directory"));
            //System.out.println("ending input params");
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
        	return resultImage.getImageName();
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
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.TransformBSpline");
            }

            public String getDescription() {
                return new String("Nonlinear BSpline transformation.");
            }

            public String getDescriptionLong() {
                return new String("Nonlinear BSpline transformation.");
            }

            public String getShortLabel() {
                return new String("NLT");
            }

            public String getLabel() {
                return new String("NLT");
            }

            public String getName() {
                return new String("NLT");
            }
        };
    }
    
    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        nltName = scriptParameters.getParams().getString("nlt_name");
        directory = scriptParameters.getParams().getString("nlt_directory");
        if (nltName != null) {
	        nltFile = new File(directory + nltName); 	
        }
        else {
        	MipavUtil.displayError("nltName is null");
        	return;
        }
        
        openNLT();
    }
    
    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("nlt_name", nltName));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nlt_directory", directory));
    }

}
