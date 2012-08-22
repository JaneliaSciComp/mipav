package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Selected image is match image, the image that gets transformed until it is registered to the
 * base image. Thin plate spline algorithm is used for matching.
 *
 * <p>The mouse is used to put a set of point VOIs on the base image. Then, the mouse is used to put the same number of
 * point VOIs at the corresponding positions on the match image. For 2D images 3 or more points are required and the
 * algorithm may fail if the points all nearly fall on the same line. For 3D images 4 or more points are required and
 * the algorithm may fail if the points nearly all fall on the same plane. The base image is selected from a combo box
 * containing the names of images other than the selected match image.</p>
 *
 * <p>The dimensions or image type of the match image need not be the same as the dimensions or image type of the base
 * image. The registered resultImage will have the same image type as the match image and the same extents as the base
 * image.</p>
 *
 * <p>The spline matching points sets are used to obtain spline interpolation coefficients. These spline interpolation
 * coefficents are used to transform all the xorg,yorg grid positions in base image space to xnew, ynew grid positions
 * in match space. Then, at every xorg,yorg grid position in the base space, the program checks to see if the
 * corresponding xnew,ynew grid position in the match space is within the image bounds of the match image. If xnew, ynew
 * is within the match space bounds, then the data value at the xnew,ynew grid position in the match space is assigned
 * to be the registered value at the xorg,yorg position in the base space. Since xnew, ynew is a floating point number
 * and the data in the match image is only contained at integer grid points, interpolation must be used. For a 2D image
 * the data value at xnew,ynew in the match space is obtained by bilinear interpolation from its 4 nearest neighbors.
 * For a 3D image the data value at xnew,ynew in the match space is obtained by trilinear interpolation from its 8
 * nearest neighbors. If the xnew, ynew is outside the match space bounds, then a zero is assigned to the xorg, yorg
 * position in the base space.</p>
 *
 * <p>This software does not yet provide a general coplanar solution for 3D images. However, special handling does exist
 * for the case where the z values of the corresponding point landmarks are identical.</p>
 */
public class JDialogRegistrationTPSpline extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5973812592305647222L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage baseImage;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private boolean coplanar = false;

    /** DOCUMENT ME! */
    private int DIM;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register match image to baseImage

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmTPSpline spline = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private double[] xSource;

    /** DOCUMENT ME! */
    private double[] xTar;

    /** DOCUMENT ME! */
    private double[] ySource;

    /** DOCUMENT ME! */
    private double[] yTar;

    /** DOCUMENT ME! */
    private double[] zSource;

    /** DOCUMENT ME! */
    private double[] zTar;
    
    private JLabel matrixLabel;
    
    private JComboBox matrixComboBox;
    
    private String matrixDirectory;
    
    private JLabel userDirectoryLabel;
    
    private JTextField userDirectoryText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor for scripts.
     */
    public JDialogRegistrationTPSpline() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRegistrationTPSpline(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        UI = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        matchImage = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }

        } else if (source == cancelButton) {
            dispose();
        } else if (source == helpButton) {
            //MipavUtil.showHelp("10033");
            MipavUtil.showWebHelp("Registration:_Landmark-TPSpline#Applying_the_Registration:_Landmark-TPSpline_algorithm");
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
        int i;

        if (algorithm instanceof AlgorithmTPSpline) {

            if (spline.isCompleted() == true) {
                spline.saveMatrix(matrixDirectory + matchImage.getImageName() +
                        "_To_" + baseImage.getImageName() + ".tps", null);
                Preferences.debug("Saved " + matrixDirectory + matchImage.getImageName() +
                        "_To_" + baseImage.getImageName() + ".tps\n",Preferences.DEBUG_FILEIO);
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();

                for (i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        UI.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    UI.registerFrame(parentFrame);
                    matchImage.notifyImageDisplayListeners(null, true);
                }

                resultImage = spline.getResultImage();

                if (resultImage != null) {

                    try {

                        // resultImage.setImageName("Transformed image");
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                    
                    if (resultImage != null) {
                        resultImage.getMatrixHolder().replaceMatrices(baseImage.getMatrixHolder().getMatrices());
        
                        if (DIM == 3) {
                            for (i = 0; i < resultImage.getExtents()[2]; i++) {
                                resultImage.getFileInfo(i).setOrigin(baseImage.getFileInfo(i).getOrigin());
                            }
                        }
                        else {
                            resultImage.getFileInfo(0).setOrigin(baseImage.getFileInfo(0).getOrigin());    
                        }
                    }
                } else {
                    MipavUtil.displayError("result Image is null");
                }

                Preferences.debug("Done.",Preferences.DEBUG_ALGORITHM);
                insertScriptLine();
            }

        }

        dispose();
    }

    /**
     * Gets the result image.
     *
     * @return  ModelImage result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        if (event.getSource() == comboBoxImage) {
        	baseImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        	
        	matrixComboBox.removeAllItems();
            
            if (baseImage != null) {
                matrixComboBox.addItem(baseImage.getImageDirectory());	
            }
            
            if ((matchImage.getImageDirectory() != null) && 
                	(!baseImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
                	matrixComboBox.addItem(matchImage.getImageDirectory());
            }
            
            if ((UI.getDefaultDirectory() != null) && 
            	(!UI.getDefaultDirectory().equals(baseImage.getImageDirectory())) &&
            	(!UI.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
            	matrixComboBox.addItem(UI.getDefaultDirectory());
            }
            matrixComboBox.addItem("User specified matrix directory");
            matrixComboBox.setSelectedIndex(0);
        }
    }

    /**
     * Runs the algorithm.
     */
    protected void callAlgorithm() {

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
            
        }

        // Hide dialog
        setVisible(false);

        int nPtsA = 0; // = baseImage.getVOIs().size();
        int nPtsB = 0; // = matchImage.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i;
        int j;
        int num;
        Vector<VOIBase> curves;
        VOIVector voiVector;
        VOI presentVOI;
        
        if (baseImage.getVOIs().size() == 0) {
            MipavUtil.displayError("Select points before clicking OK");

            return;
        }
        
        if (baseImage.getNDims() != matchImage.getNDims()) {
        	MipavUtil.displayError("baseImage and matchImage must have the same number of dimensions");
        }
    
    	voiVector = baseImage.getVOIs();
        for (i = 0; i < voiVector.size(); i++) {
            presentVOI = baseImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	curves = presentVOI.getCurves();
            	nPtsA += curves.size();
            }
        }

        Preferences.debug("nPtsA = " + nPtsA + "\n",Preferences.DEBUG_ALGORITHM);
        ptA = new Vector3f[nPtsA];
        for (i = 0, num = 0; i < voiVector.size(); i++) {
            presentVOI = baseImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	tmpptA = presentVOI.exportAllPoints();
            	for (j = 0; j < tmpptA.length; j++) {
            		ptA[num++] = tmpptA[j];
            	}
            }
        }

        voiVector = matchImage.getVOIs();
        for (i = 0; i < voiVector.size(); i++) {
            presentVOI = matchImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	curves = presentVOI.getCurves();
            	nPtsB += curves.size();
            }
        }

        if (nPtsA != nPtsB) {
            MipavUtil.displayError("Both images must have the same number of points");

            return;
        }

        Preferences.debug("nPtsB = " + nPtsB + "\n",Preferences.DEBUG_ALGORITHM);
        ptB = new Vector3f[nPtsB];
        for (i = 0, num = 0; i < voiVector.size(); i++) {
            presentVOI = matchImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	tmpptB = presentVOI.exportAllPoints();
            	for (j = 0; j < tmpptB.length; j++) {
            		ptB[num++] = tmpptB[j];
            	}
            }
        }
    
        

        if ((nPtsA < (DIM+1)) || (nPtsB < (DIM+1))) {
            MipavUtil.displayError("Must select at least " + (DIM + 1) + " points.");

            return;
        }
        
        if (DIM == 3) {
        	coplanar = true;

            for (i = 0; (i < ptA.length) && coplanar; i++) {

                if (ptA[i].Z != ptB[i].Z) {
                    coplanar = false;
                }
            }
        }

        

        if ((DIM == 2) || (coplanar)) {

            // Calculate the reverse direction to find the values of the grid positions in x',y' space in
            // terms of x, y values in the original space
            try {
                xSource = new double[nPtsA];
                ySource = new double[nPtsA];
                xTar = new double[nPtsB];
                yTar = new double[nPtsB];
            } catch (OutOfMemoryError error) {
                xSource = null;
                ySource = null;
                xTar = null;
                yTar = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on xSource");

                return;
            }

            for (i = 0; i < nPtsA; i++) {
                xSource[i] = ptA[i].X;
                ySource[i] = ptA[i].Y;
            }

            for (i = 0; i < nPtsB; i++) {
                xTar[i] = ptB[i].X;
                yTar[i] = ptB[i].Y;
            }

            // 0.0f for no smoothing, with smoothing interpolation is not exact
            try {
                spline = new AlgorithmTPSpline(xSource, ySource, xTar, yTar, 0.0f, baseImage, matchImage);
            } catch (OutOfMemoryError error) {
                spline = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");

                return;
            }

        } // if (DIM == 2)
        else { // DIM == 3

            // Calculate the reverse direction to find the values of the grid positions in x',y',z' space in
            // terms of x, y, z values in the original space
            try {
                xSource = new double[nPtsA];
                ySource = new double[nPtsA];
                zSource = new double[nPtsA];

                xTar = new double[nPtsB];
                yTar = new double[nPtsB];
                zTar = new double[nPtsB];
            } catch (OutOfMemoryError error) {
                xSource = null;
                ySource = null;
                zSource = null;

                xTar = null;
                yTar = null;
                zTar = null;

                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory.");

                return;
            }

            for (i = 0; i < nPtsA; i++) {
                xSource[i] = ptA[i].X;
                ySource[i] = ptA[i].Y;
                zSource[i] = ptA[i].Z;
            }

            for (i = 0; i < nPtsB; i++) {
                xTar[i] = ptB[i].X;
                yTar[i] = ptB[i].Y;
                zTar[i] = ptB[i].Z;
            }

            // 0.0f for no smoothing, with smoothing interpolation is not exact
            try {
                spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTar, yTar, zTar, 0.0f, baseImage,
                                               matchImage);
            } catch (OutOfMemoryError error) {
                spline = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");

                return;
            }

        } // else DIM == 3

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        spline.addListener(this);

        createProgressBar(matchImage.getImageName(), spline);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            UI.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        if (isRunInSeparateThread()) {

            if (spline.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            spline.run();
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }
    
    /**
     * Accessor to set directory in which the matrix file is stored
     * @param matrixDirectory
     */
    public void setMatrixDirectory(String matrixDirectory) {
    	this.matrixDirectory = matrixDirectory;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        baseImage = scriptParameters.retrieveImage("reference_image");

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
        }

        UI = ViewUserInterface.getReference();
        parentFrame = matchImage.getParentFrame();
        setMatrixDirectory(scriptParameters.getParams().getString("matrix_directory"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(baseImage, "reference_image");

        scriptParameters.storeImageInRecorder(getResultImage());
        scriptParameters.getParams().put(ParameterFactory.newParameter("matrix_directory", matrixDirectory));
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Thin Plate Spline Registration");

        JPanel imagePanel = buildImagePanel();
        JPanel outputPanel = buildOutputPanel();

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(outputPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    protected JPanel buildImagePanel() {
    	String matchName = matchImage.getImageName();

        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImgComboBox(matchImage);

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(comboBoxImage);
        
        return imagePanel;
    }
    
    protected JPanel buildOutputPanel() {
    	JPanel outputPanel = new JPanel(new GridBagLayout());
        
        matrixLabel = new JLabel("Matrix file directory");
        matrixLabel.setForeground(Color.black);
        matrixLabel.setFont(serif12);
        matrixLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        matrixComboBox = new JComboBox();
        matrixComboBox.setFont(serif12);
        matrixComboBox.setBackground(Color.white);
        matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        baseImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        
        if (baseImage != null) {
            matrixComboBox.addItem(baseImage.getImageDirectory());	
        }
        if ((matchImage.getImageDirectory() != null) && 
        	(!baseImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
        	matrixComboBox.addItem(matchImage.getImageDirectory());
        }
        if ((UI.getDefaultDirectory() != null) && 
        	(!UI.getDefaultDirectory().equals(baseImage.getImageDirectory())) &&
        	(!UI.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
        	matrixComboBox.addItem(UI.getDefaultDirectory());
        }
        matrixComboBox.addItem("User specified matrix directory");
        matrixComboBox.setSelectedIndex(0);
        
        userDirectoryLabel = new JLabel("User specified matrix directory");
        userDirectoryLabel.setForeground(Color.black);
        userDirectoryLabel.setFont(serif12);
        userDirectoryLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        userDirectoryText = new JTextField();
        userDirectoryText.setFont(serif12);
        userDirectoryText.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        Insets insets = new Insets(2, 5, 2, 5);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = insets;
        outputPanel.add(matrixLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outputPanel.add(matrixComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outputPanel.add(userDirectoryLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outputPanel.add(userDirectoryText, gbc);
        
        return outputPanel;
    }
    
    /**
     * Builds a list of images. Returns combobox.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildImgComboBox(ModelImage image) {
        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();

            if (!name.equals(image.getImageName())) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if ((image.getNDims() == img.getNDims()) && (image.isColorImage() == img.isColorImage()) &&
                        (UI.getFrameContainingImage(img) != null)) {
                    comboBox.addItem(name);
                }
            }
        }
        comboBox.addItemListener(this);

        return comboBox;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {
        UI = ViewUserInterface.getReference();

        String selectedName = (String) comboBoxImage.getSelectedItem();
        baseImage = UI.getRegisteredImageByName(selectedName);

        if (baseImage.getVOIs().size() == 0) {
            MipavUtil.displayError("Error! No VOIs were present in the base image");

            return false;
        } else if (matchImage.getVOIs().size() == 0) {
            MipavUtil.displayError("Error! No VOIs were present in the match image");

            return false;
        }
        
        matrixDirectory = (String)matrixComboBox.getSelectedItem();
        if (matrixDirectory != null) {
	        if (matrixDirectory.equals("User specified matrix directory")) {
	            matrixDirectory = userDirectoryText.getText();	
	        }
        }

        return true;
    }
}
