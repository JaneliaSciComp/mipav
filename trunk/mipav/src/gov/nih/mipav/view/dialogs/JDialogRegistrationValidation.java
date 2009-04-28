package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmRegValidation. Selects image is match image, the image that gets transformed
 * until it is registered to the base image. Algorithms are executed in their own thread.
 * 
 * @author   senseneyj
 */
public class JDialogRegistrationValidation extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2171665599919057675L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Base image - register match image to base image. */
    private ModelImage baseImage;

    /** Combo box with image names for choosing base image. */
    private JComboBox comboBoxImage;

    /** Number of dimensions in match image. */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean fromOAR3D = false;

    /** DOCUMENT ME! */
    private boolean lsCompleted = false;

    /** Algorithm to run from this dialog. */
    private AlgorithmRegLeastSquares LSMatch = null;

    /** Match image - register match image to base image. */
    private ModelImage matchImage;

    /** Result image - image returned from registration algorithm. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private TransMatrix resultMatrix = null;

    /** Used to lock and unlock images. */
    private String[] titles;

    /** Reference to userface. */
    private ViewUserInterface userInterface;

    /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;

    /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;
    
    private JLabel outOfBoundsLabel;
    
    private JComboBox outOfBoundsComboBox;
    
    private JLabel valueLabel;
    
    private JTextField valueText;
    
    private double imageMin;
    
    private double imageMax;
    
    private int dataType;
    
    /**
     * Tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image maximum
     */
    private int outOfBoundsIndex = 0;
    
    private float fillValue = 0.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationValidation() { }

    /**
     * Creates new registration dialog to get base image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationValidation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    /**
     * Creates a new JDialogRegistrationValidation object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  _mi             DOCUMENT ME!
     * @param  _ri             DOCUMENT ME!
     */
    public JDialogRegistrationValidation(Frame theParentFrame, ModelImage _mi, ModelImage _ri) {
        matchImage = _mi;
        baseImage = _ri;
        DIM = 3;
        userInterface = ViewUserInterface.getReference();
        setSeparateThread(false);
        fromOAR3D = true;
        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, set variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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
            MipavUtil.showHelp("10040");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegLeastSquares) {

            if (LSMatch.isCompleted() == true) {
                lsCompleted = true;
                matchImage.setMatrix(LSMatch.getTransformBtoA());
                Preferences.debug(matchImage.getMatrix().toString());
                LSMatch.getTransformBtoA().saveMatrix(userInterface.getDefaultDirectory() + matchImage.getImageName() +
                                                      "_To_" + baseImage.getImageName() + ".mtx");
                LSMatch.calculateResiduals();
                xdimA = baseImage.getExtents()[0];
                ydimA = baseImage.getExtents()[1];

                String name = makeImageName(matchImage.getImageName(), "_register");

                if (DIM == 2) {
                    int[] extents = new int[] { xdimA, ydimA };
                    float[] resolutions = new float[] { xresA, yresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name);
                    resultImage.getFileInfo(0).setResolutions(resolutions);

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformBilinear(matchImage, resultImage, LSMatch.getTransformBtoA(), null, true,
                                                             fillValue);
                    } else {
                        AlgorithmTransform.transformBilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              xdimA, ydimA, xresA, yresA, fillValue);
                    }

                } else if (DIM == 3) {

                    if (fromOAR3D) {
                        resultMatrix = LSMatch.getTransformBtoA();
                    }

                    zdimA = baseImage.getExtents()[2];

                    int[] extents = new int[] { xdimA, ydimA, zdimA };
                    float[] resolutions = new float[] { xresA, yresA, zresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name);

                    for (int i = 0; i < zdimA; i++) {
                        resultImage.getFileInfo(i).setResolutions(resolutions);
                    }

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformTrilinear(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              null, true, fillValue);
                    } else {
                        AlgorithmTransform.transformTrilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                               xdimA, ydimA, zdimA, xresA, yresA, zresA, fillValue);
                    }
                }

                resultImage.calcMinMax();

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = matchImage.getImageFrameVector();

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

                matchImage.notifyImageDisplayListeners(null, true);

                if (resultImage != null) {

                    try {
                        resultImage.setImageName("LS Transformed image");
                        updateFileInfo(baseImage, resultImage);

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                    
                    resultImage.getMatrixHolder().replaceMatrices(baseImage.getMatrixHolder().getMatrices());

                } else {
                    MipavUtil.displayError("Result Image is null");
                }

                insertScriptLine();
            }
        }

        if (!fromOAR3D) {
            dispose();
        }
    }

    /**
     * Accessor that returns whether or not the algorithm successfully completed.
     *
     * @return  boolean
     */
    public boolean getLSCompleted() {
        return lsCompleted;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Gets the result matrix (only used by OAR3D).
     *
     * @return  TransMatrix
     */
    public TransMatrix getResultMatrix() {
        return resultMatrix;
    }

    /**
     * Sets arrays appropriately and calls registration algorithm, running it in it's own thread.
     */
    protected void callAlgorithm() {
        int nPtsA = 0; // = baseImage.getVOIs().size();
        int nPtsB = 0; // = matchImage.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i, s, ptNum;
        Vector[] curves;

        try {

            if (baseImage.getVOIs().size() == 0) {
                MipavUtil.displayError("Select points before clicking OK");

                return;
            } else {

                if ((baseImage.getNDims() == 3) && (matchImage.getNDims() == 3)) {
                    curves = baseImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s

                    for (s = 0; s < baseImage.getExtents()[2]; s++) {
                        nPtsA += curves[s].size();
                    }

                    Preferences.debug("nPtsA = " + nPtsA + "\n");
                    ptA = new Vector3f[nPtsA];

                    for (s = 0; s < baseImage.getExtents()[2]; s++) {
                        tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(s);

                        for (i = 0; i < tmpptA.length; i++) {
                            ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                                    1;
                            ptA[ptNum] = tmpptA[i];
                        }
                    }

                    curves = matchImage.getVOIs().VOIAt(0).getCurves();

                    for (s = 0; s < matchImage.getExtents()[2]; s++) {
                        nPtsB += curves[s].size();
                    }

                    if (nPtsA != nPtsB) {
                        MipavUtil.displayError("Both images must have the same number of points");

                        return;
                    }

                    Preferences.debug("nPtsB = " + nPtsB + "\n");
                    ptB = new Vector3f[nPtsB];

                    for (s = 0; s < matchImage.getExtents()[2]; s++) {
                        tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(s);

                        for (i = 0; i < tmpptB.length; i++) {
                            ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                                    1;

                            // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                            ptB[ptNum] = tmpptB[i];
                        }
                    }

                    if ((nPtsA < 4) || (nPtsB < 4)) {
                        MipavUtil.displayError("Must select at least " + (DIM + 1) + " points.");

                        return;
                    }
                } else if ((baseImage.getNDims() == 2) && (matchImage.getNDims() == 2)) {
                    curves = baseImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s
                    nPtsA = curves[0].size();
                    Preferences.debug("nPtsA = " + nPtsA + "\n");
                    ptA = new Vector3f[nPtsA];
                    tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(0);

                    for (i = 0; i < tmpptA.length; i++) {
                        ptNum = (int) (Short.valueOf(((VOIPoint) curves[0].elementAt(i)).getLabel()).shortValue()) - 1;
                        ptA[ptNum] = tmpptA[i];
                    }

                    curves = matchImage.getVOIs().VOIAt(0).getCurves();
                    nPtsB += curves[0].size();

                    if (nPtsA != nPtsB) {
                        MipavUtil.displayError("Both images must have the same number of points");

                        return;
                    }

                    Preferences.debug("nPtsB = " + nPtsB + "\n");
                    ptB = new Vector3f[nPtsB];
                    tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(0);

                    for (i = 0; i < tmpptB.length; i++) {
                        ptNum = (int) (Short.valueOf(((VOIPoint) curves[0].elementAt(i)).getLabel()).shortValue()) - 1;
                        ptB[ptNum] = tmpptB[i];
                    }

                    if ((nPtsA < 3) || (nPtsB < 3)) {
                        MipavUtil.displayError("Must select at least " + (DIM + 1) + " points.");

                        return;
                    }
                }
            }

            Vector3f[] ptAmm = new Vector3f[nPtsA];
            Vector3f[] ptBmm = new Vector3f[nPtsB];
            zresA = 1;
            xresA = baseImage.getFileInfo(0).getResolutions()[0];
            yresA = baseImage.getFileInfo(0).getResolutions()[1];

            if (baseImage.getNDims() == 3) {
                zresA = baseImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsA; i++) {
                ptAmm[i] = new Vector3f((ptA[i].X * xresA), (ptA[i].Y * yresA),
                                        (ptA[i].Z * zresA));
                Preferences.debug(ptAmm[i].X + ", " + ptAmm[i].Y + ", " + ptAmm[i].Z + "\n");
            }

            xresB = matchImage.getFileInfo(0).getResolutions()[0];
            yresB = matchImage.getFileInfo(0).getResolutions()[1];

            if (matchImage.getNDims() == 3) {
                zresB = matchImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsB; i++) {
                ptBmm[i] = new Vector3f((ptB[i].X * xresB), (ptB[i].Y * yresB),
                                        (ptB[i].Z * zresB));
                Preferences.debug(ptBmm[i].X + ", " + ptBmm[i].Y + ", " + ptBmm[i].Z + "\n");
            }

            LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Register Least Squares: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        LSMatch.addListener(this);

        createProgressBar(baseImage.getImageName(), LSMatch);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        // Start the thread as a low priority because we wish to still have
        // user interface work fast
        // if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false){
        // MipavUtil.displayError("A thread is already running on this object", "Error");

        if (isRunInSeparateThread()) {

            if (LSMatch.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            LSMatch.run();
        }


    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }
    
    /**
     * tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image max 
     * @param outOfBoundsIndex
     */
    public void setOutOfBoundsIndex(int outOfBoundsIndex) {
        this.outOfBoundsIndex = outOfBoundsIndex;
    }
    
    /**
     * Accessor to set intensity value for out of bounds data
     * @param fillValue
     */
    public void setFillValue(float fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
        baseImage = scriptParameters.retrieveImage("reference_image");

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = matchImage.getParentFrame();
        setOutOfBoundsIndex(scriptParameters.getParams().getInt("out_of_bounds_index"));
        switch(outOfBoundsIndex) {
            case 0: 
                setFillValue((float)imageMin);
                break;
            case 1: 
                if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                    setFillValue(Float.NaN);
                }
                else {
                    setFillValue(0.0f);
                }
                break;
            case 2:
                setFillValue(scriptParameters.getParams().getFloat("fill_value"));
                break;
            case 3:
                setFillValue((float)imageMax);
                break;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(baseImage, "reference_image");

        scriptParameters.storeImageInRecorder(getResultImage());
        scriptParameters.getParams().put(ParameterFactory.newParameter("out_of_bounds_index", outOfBoundsIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));
    }

    /**
     * Initializes GuserInterface components and displays dialog.
     */
    private void init() {
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
        setForeground(Color.black);
        setTitle("Least Squares Registration");

        String matchName = matchImage.getImageName();

        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImageComboBox(matchImage);

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(comboBoxImage);
        
        JPanel outPanel = new JPanel(new GridBagLayout());
        outOfBoundsLabel = new JLabel("Out of bounds data:");
        outOfBoundsLabel.setForeground(Color.black);
        outOfBoundsLabel.setFont(serif12);
        outOfBoundsLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox = new JComboBox();
        outOfBoundsComboBox.setFont(serif12);
        outOfBoundsComboBox.setBackground(Color.white);
        outOfBoundsComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox.addItem("Image minimum");
        outOfBoundsComboBox.addItem("If float NaN, else 0");
        outOfBoundsComboBox.addItem("User defined");
        outOfBoundsComboBox.addItem("Image maximum");
        outOfBoundsComboBox.setSelectedIndex(0);
        outOfBoundsComboBox.addItemListener(this);
        
        valueLabel = new JLabel("Out of bounds intensity value:");
        valueLabel.setForeground(Color.black);
        valueLabel.setFont(serif12);
        valueLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        valueText = new JTextField(String.valueOf(imageMin));
        valueText.setFont(serif12);
        valueText.setEnabled(false);
        
        GridBagConstraints gbc = new GridBagConstraints();
        Insets insets = new Insets(2, 5, 2, 5);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        outPanel.add(outOfBoundsLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(outOfBoundsComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(valueLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(valueText, gbc);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(outPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        if (event.getSource() == outOfBoundsComboBox) {
            switch (outOfBoundsComboBox.getSelectedIndex()) {
                case 0: // image minimum
                    valueText.setText(String.valueOf(imageMin));
                    valueText.setEnabled(false);
                    break;
                case 1: // If float NaN, else 0
                    if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        valueText.setText(String.valueOf(Float.NaN)); 
                    }
                    else {
                        valueText.setText(String.valueOf(0));
                    }
                    valueText.setEnabled(false);
                    break;
                case 2: // User defined;
                    valueText.setEnabled(true);
                    break;
                case 3: // Image maximum
                    valueText.setText(String.valueOf(imageMax));
                    valueText.setEnabled(false);
                    break;
            } // switch (outOfBoundsComboBox.getSelectedIndex())
        } // if (event.getSource() == outOfBoundsComboBox)
    }


    /**
     * Sets the variables needed for calling the algorithm.
     *
     * @return  <code>true</code> if successful in setting variables.
     */
    private boolean setVariables() {

        // assign baseImage to image selected in comboBox
        String selectedName = (String) comboBoxImage.getSelectedItem();

        baseImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
            
        }
        
        fillValue = Float.valueOf(valueText.getText()).floatValue();
        outOfBoundsIndex = outOfBoundsComboBox.getSelectedIndex();
        if (outOfBoundsIndex == 2) {
            // user defined value
            boolean success = testType(dataType, fillValue);
            if (!success) {
                MipavUtil.displayError("User defined value is out of the data type range");
                valueText.requestFocus();
                valueText.selectAll();
                return false;
            }
        }

        return true;
    }
    
    /**
     * Determine if the value is in the image type range and
     * within the float range since AlgorithmTransform does
     * not use double buffers.
     *
     * @param   type    image type
     * @param   value   value tested
     *
     * @return  true if value is within acceptable range
     */
    private boolean testType(int type, float value) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ((value < 0) || (value > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ((value < -128) || (value > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ((value < -32768) || (value > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ((value < Integer.MIN_VALUE) || (value > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ((value < 0) || (value > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ((value < Long.MIN_VALUE) || (value > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            // Float buffers are used in the AlgorithmTransform routines
            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

}
