import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import javax.swing.*;


/**
 * @version  December 26, 2006
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogKidneySegmentation.java $ $Revision: 21 $ $Date: 1/25/06 4:59p $
 *           </p>
 */
public class PlugInDialogKidneySegmentation extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = -2063809955712228256L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** Number of iterations performed for both erosions and dilations */
    private int iters;

    /** DOCUMENT ME! */
    private JLabel itersLabel;

    /** DOCUMENT ME! */
    private JTextField itersText;
    
    private JLabel middleLabel;
    
    private JTextField middleText;
    
    private int middleSlice;
    
    private JCheckBox areaBox;
    
    /** If true, when a kidney curve in a slice encompasses less than 0.8 of the area
     *  of both the previous and following kidney slice curves, then lower the
     *  threshold and resegment to obtain a new larger area kidney curve for the slice.
     */
    private boolean areaCorrect;

    /** DOCUMENT ME! */
    private PlugInAlgorithmKidneySegmentation kidneySegAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogKidneySegmentation() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogKidneySegmentation(Frame theParentFrame, ModelImage im) {
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        
        

        if (algorithm instanceof PlugInAlgorithmKidneySegmentation) {
            Preferences.debug("Kidney segmentation Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((kidneySegAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += middleSlice;
        str += iters;
        str += areaCorrect;

        return str;
    }

    /**
     * Accessor that sets the slice containing the middle of the kidneys
     * @param middleSlice
     */
    public void setMiddleSlice(int middleSlice) {
        this.middleSlice = middleSlice;
    }

    /**
     * Accessor that sets the number of erosion and dilation iterations.
     *
     * @param  iters  int
     */
    public void setIters(int iters) {
        this.iters = iters;
    }
    
    /**
     * Accessor that sets whether curves in a slice are increased in area
     * if previous and following curves have consierably larger area
     * @param areaCorrect
     */
    public void setAreaCorrect(boolean areaCorrect) {
        this.areaCorrect = areaCorrect;
    }

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_kidneys");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            kidneySegAlgo = new PlugInAlgorithmKidneySegmentation(resultImage, image, middleSlice, iters, areaCorrect);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            kidneySegAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", kidneySegAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (kidneySegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                kidneySegAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Kidney segmentation: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
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
        parentFrame = image.getParentFrame();

        setMiddleSlice(scriptParameters.getParams().getInt(" middle_slice"));
        setIters(scriptParameters.getNumIterations());
        setAreaCorrect(scriptParameters.getParams().getBoolean("area_correct"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("middle_slice", middleSlice));
        scriptParameters.storeNumIterations(iters);
        scriptParameters.getParams().put(ParameterFactory.newParameter("area_correct", areaCorrect));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Kidney segmentation 12/28/06");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));

        JLabel labelVOI = new JLabel("In one slice place a contour VOI in each kidney");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        
        itersLabel = new JLabel("Erosion and dilation iterations");
        itersLabel.setForeground(Color.black);
        itersLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(itersLabel, gbc);

        itersText = new JTextField(5);

        if (image.getNDims() > 2) {
            itersText.setText("5");
        } else {
            itersText.setText("5");
        }

        itersText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(itersText, gbc);
        
        middleLabel = new JLabel("Slice with middle of kidneys");
        middleLabel.setForeground(Color.black);
        middleLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(middleLabel, gbc);

        middleText = new JTextField(5);
        if (image.getNDims() > 2) {
            // Math.floor in article but use Math.ceil to use same slice as initial MIPAV display
            middleText.setText(Integer.toString((int)Math.ceil(image.getExtents()[2]/2.0)));
        }
        else {
            middleText.setText("1");
        }
        middleText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(middleText, gbc);
        
        areaBox = new JCheckBox("Correct curves with small area", true);
        areaBox.setForeground(Color.black);
        areaBox.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(areaBox, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int i;
        
        ViewVOIVector VOIs = image.getVOIs();
        int nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Image must have a VOI with 2 curves");

            return false;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("The VOI must be a contour VOI");

            return false;
        }
        
        
        Vector<VOIBase> curves = VOIs.VOIAt(i).getCurves();
        boolean haveTwoCurves = (curves.size() == 2);

        if (!haveTwoCurves) {
            MipavUtil.displayError("One slice must have a contour VOI with 2 curves");

            return false;
        }
        
        tmpStr = itersText.getText();
        iters = Integer.parseInt(tmpStr);

        if (iters < 0) {
            MipavUtil.displayError("iters must be at least 0");
            itersText.requestFocus();
            itersText.selectAll();

            return false;
        } else if (iters > 100) {
            MipavUtil.displayError("iters must not exceed 100");
            itersText.requestFocus();
            itersText.selectAll();

            return false;
        }
        
        tmpStr = middleText.getText();
        // Change from 1 based to 0 based slice numbering
        middleSlice = Integer.parseInt(tmpStr) - 1;
        if (middleSlice < 0) {
            MipavUtil.displayError("Middle slice must be at least 1");
            middleText.requestFocus();
            middleText.selectAll();

            return false;
        } else if ((image.getNDims() > 2) && (middleSlice > image.getExtents()[2] - 1)) {
            MipavUtil.displayError("Middle slice must not exceed " + image.getExtents()[2]);
            middleText.requestFocus();
            middleText.selectAll();

            return false;
        } else if ((image.getNDims() == 2) && (middleSlice > 1)) {
            MipavUtil.displayError("Middle slice must not exceed 1");
            middleText.requestFocus();
            middleText.selectAll();
            
            return false;
        }
        
        areaCorrect = areaBox.isSelected();

        return true;
    } // end setVariables()

}
