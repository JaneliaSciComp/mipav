import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * PlugIn dialog that controls the OAI segmentation for a single MR 
 * image set containing only one thigh. 
 *
 * @version  1.0, July 27, 2007
 * @author   Lydia Beasley, Agatha Monzon, Paul Hemler, Matthew J. McAuliffe, Ph.D.
 */

// Main dialog the OAI segmentation algorithm.  Controls image
// cropping and starts segmentation on both cropped thigh images.
public class PlugInDialogOAISegOneThigh extends JDialogBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The input image that contains both thighs */
    private ModelImage imageA = null; // source image

    /** outer boundary mask image, used to seperate the input image into images containing
     *  a single thigh */
    private ModelImage obMaskA = null;

    /** DOCUMENT ME! */
    private PlugInAlgorithmOAISegOneThigh segThighAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    public boolean leftThigh = false;
    
    public String leftRightString = "Right";
    
    public static String patientID;
    
    public static int xDim, yDim, zDim, sliceSize;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ModelImage destImageA, ModelImage destImageB, ModelImage obMaskA, ModelImage obMaskB, ModelImage srcImage Sets
     * variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public PlugInDialogOAISegOneThigh(Frame theParentFrame, ModelImage imA) {
    	super(theParentFrame, false);
        imageA = imA;
        resultImage = (ModelImage)imA.clone();
        obMaskA = (ModelImage)imA.clone();
        
        setSeparateThread(true);
        
        userInterface = ViewUserInterface.getReference();
        init();
 //       userInterface.getMessageFrame().addTab("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID);

    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI   The user interface, needed to create the image frame.
     * @param  imA  Source image.
     */
    public PlugInDialogOAISegOneThigh(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        imageA = imA;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    
    
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            resultImage.setVOIs(imageA.getVOIs());
            // user has said okay to having input the VOIs
            if (imageA.getVOIs() == null || imageA.getVOIs().size() == 0) {
            	JOptionPane.showMessageDialog(null,"OAI plugin requires VOIs!", "Warning", JOptionPane.WARNING_MESSAGE);
                return;
            }
     
            setVisible(false);
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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        imageA.clearMask();

       if (algorithm instanceof PlugInAlgorithmOAISegOneThigh) {

                // The algorithm has completed and produced a new image to be displayed.
                resultImage = segThighAlgo.getResultImageA();

                resultImage.setImageName(leftRightString+" Thigh Final Segmentation");

                updateFileInfo(imageA, resultImage);

                ViewJFrameImage f1 = new ViewJFrameImage(resultImage);
               
                // section putin fri feb10
                // myFrame.getLUTa().setColor( index, Color);
                // Where index is the static value for FAT etc, and color is the new color.
                // Then Do myFrame.updateImages(true);

                f1.getLUTa().setColor(PlugInAlgorithmOAISegThighs.MUSCLE, Color.GREEN);
                f1.getLUTa().setColor(PlugInAlgorithmOAISegThighs.FAT, Color.MAGENTA);
                f1.getLUTa().setColor(PlugInAlgorithmOAISegThighs.BONE, Color.RED);
                f1.getLUTa().setColor(PlugInAlgorithmOAISegThighs.BONE_MARROW, Color.PINK);
                f1.getLUTa().setColor(PlugInAlgorithmOAISegThighs.SUB_CUT_FAT, Color.YELLOW);
                f1.updateImages(true);

                segThighAlgo.finalize();
            }
    }

    /**
     * Calls the algorithm.
     *
     * @param  doStageOne  DOCUMENT ME!
     */
    protected void callAlgorithm() {

//            userInterface.getMessageFrame().addTab(leftRightString+ " Thigh");
        userInterface.getMessageFrame().addTab("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID);

            // Make algorithm
            segThighAlgo = new PlugInAlgorithmOAISegOneThigh(resultImage, obMaskA, leftThigh);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            segThighAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (segThighAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
            	segThighAlgo.run();
            }
        }

    public void thighPrompt(){
    	Object[] selectionValues = { "Left Thigh", "Right Thigh" };
        String initialSelection = "Left Thigh";
        Object selection = JOptionPane.showInputDialog(null, "Which thigh will be analyzed?",
            "Select A Thigh", JOptionPane.QUESTION_MESSAGE, null, selectionValues, initialSelection);
   
    		if(selection.equals("Left Thigh")){
    			leftThigh = true;
    		}else if(selection.equals("Right Thigh")){
    			leftThigh = false;
    		}
    		
    		if(leftThigh)
        		leftRightString = "Left";
        	else
        		leftRightString = "Right";
    }
    
    /**
     * Will display (make visible) the dialog so that the user can input the needed VOIs.
     */
    private void init() {
        this.setSize(100, 80);

        thighPrompt();
        
        setTitle("PLEASE INPUT VOIs");

        JLabel instruction = new JLabel("Please enter VOIs, then press OK.");
        JPanel mainPanel = new JPanel(new BorderLayout());
        
        mainPanel.add(instruction, BorderLayout.NORTH);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(this.buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

}

