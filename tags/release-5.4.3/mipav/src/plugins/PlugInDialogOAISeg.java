import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

/*
import java.util.*;
import javax.swing.event.*;
import javax.swing.border.*;
import gov.nih.mipav.model.file.*;
 */


/**
 * PlugIn dialog that controls the OAI segmentation for a single MR 
 * image set containing both the left and right thigh 
 *
 * @version  1.0, June 22, 2006
 * @author   Agatha Monzon, Paul Hemler, and Matthew J. McAuliffe, Ph.D.
 */

// Main dialog the OAI segmentation algorithm.  Controls image
// cropping and starts segmentation on both cropped thigh images.
public class PlugInDialogOAISeg extends JDialogBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The input image that contains both thighs */
    private ModelImage imageA = null; // source image

    /** outer boundary mask image, used to seperate the input image into images containing
     *  a single thigh */
    private ModelImage obMaskA = null;

    /** DOCUMENT ME! */
    private ModelImage obMaskB = null;

    /** DOCUMENT ME! */
    private PlugInAlgorithmOAICropImage cropImageAlgo;

    /** DOCUMENT ME! */
    private PlugInAlgorithmOAISegThighs segThighsAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImageA = null;

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JCheckBox n3Box=null;
    
    private boolean useN3 = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ModelImage destImageA, ModelImage destImageB, ModelImage obMaskA, ModelImage obMaskB, ModelImage srcImage Sets
     * variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public PlugInDialogOAISeg(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, false);
    	System.out.println("name of image: " + imA.getImageName());
        imageA = imA;
        userInterface = ViewUserInterface.getReference();
        callAlgorithm(true);
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI   The user interface, needed to create the image frame.
     * @param  imA  Source image.
     */
    public PlugInDialogOAISeg(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        imageA = imA;
        callAlgorithm(true);
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
            
            // user has said okay to having input the VOIs
            if (resultImageA.getVOIs() == null || resultImageA.getVOIs().size() == 0 ||
                resultImageB.getVOIs() == null || resultImageB.getVOIs().size() == 0    ) {
                JOptionPane.showMessageDialog(null, "OAI plugin requires VOIs!", "Warning",
                        JOptionPane.WARNING_MESSAGE);
                return;
            }
        	setVariables();
            setVisible(false);
            callAlgorithm(false);

        } else if (command.equals("Cancel")) {
            dispose();
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
        System.out.println("PlugInAlgorithmPipeline completed");

        imageA.clearMask();

        if (algorithm instanceof PlugInAlgorithmOAICropImage) {

            if (cropImageAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a new image to be displayed.
                resultImageA = cropImageAlgo.getResultImageA();
                resultImageB = cropImageAlgo.getResultImageB();

                obMaskA = cropImageAlgo.getMaskA();
                obMaskB = cropImageAlgo.getMaskB();

                updateFileInfo(imageA, resultImageA);
                updateFileInfo(imageA, resultImageB);

                ViewJFrameImage f1 = new ViewJFrameImage(resultImageA);
                new ViewJFrameImage(resultImageB, null, new Dimension((int) f1.getLocation().getX() + 250,
                                                                      (int) f1.getLocation().getY()));

                // go to stage 2
                init();
            }
        } else if (algorithm instanceof PlugInAlgorithmOAISegThighs) {

            if (segThighsAlgo.isCompleted() == true) {

                System.err.println("GOT PASSED THE SECOND STAGE");

                // The algorithm has completed and produced a new image to be displayed.
                resultImageA = segThighsAlgo.getResultImageA();
                resultImageB = segThighsAlgo.getResultImageB();

                resultImageA.setImageName("Right Thigh Final Segmentation");
                resultImageB.setImageName("Left Thigh Final Segmentation");

                updateFileInfo(imageA, resultImageA);
                updateFileInfo(imageA, resultImageB);

                ViewJFrameImage f1 = new ViewJFrameImage(resultImageA);
                ViewJFrameImage f2 = new ViewJFrameImage(resultImageB, null,
                                                         new Dimension((int) f1.getLocation().getX() + 250,
                                                                       (int) f1.getLocation().getY()));

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

                f2.getLUTa().setColor(PlugInAlgorithmOAISegThighs.MUSCLE, Color.GREEN);
                f2.getLUTa().setColor(PlugInAlgorithmOAISegThighs.FAT, Color.MAGENTA);
                f2.getLUTa().setColor(PlugInAlgorithmOAISegThighs.BONE, Color.RED);
                f2.getLUTa().setColor(PlugInAlgorithmOAISegThighs.BONE_MARROW, Color.PINK);
                f2.getLUTa().setColor(PlugInAlgorithmOAISegThighs.SUB_CUT_FAT, Color.YELLOW);
                f2.updateImages(true);

                segThighsAlgo.finalize();
            }
        }
    }

    /**
     * Calls the algorithm.
     *
     * @param  doStageOne  DOCUMENT ME!
     */
    protected void callAlgorithm(boolean doStageOne) {

        if (doStageOne) {

            System.out.println("Cropping Image");

            try {

                // Make algorithm
            	cropImageAlgo = new PlugInAlgorithmOAICropImage(resultImageA, resultImageB, obMaskA, obMaskB, imageA);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
            	cropImageAlgo.addListener(this);

            	createProgressBar(imageA.getImageName(), " ...", cropImageAlgo);
            	
                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cropImageAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                	cropImageAlgo.run();

                }
            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("AlgorithmAbsoluteValue: unable to allocate enough memory");

                return;
            }

        }
        // finished stage one, has VOIs, go to stage 2
        else {
             
            userInterface.getMessageFrame().addTab("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID);

            // Make algorithm
            segThighsAlgo = new PlugInAlgorithmOAISegThighs(resultImageA, resultImageB, obMaskA, obMaskB, useN3);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            segThighsAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (segThighsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
            	segThighsAlgo.run();
            }
        }
    }

    /**
     * Will display (make visible) the dialog so that the user can input the needed VOIs.
     */
    private void init() {
        this.setSize(100, 80);

        setTitle("PLEASE INPUT VOIs");

        JLabel instruction = new JLabel("Please enter VOIs, then press OK.");
        JPanel mainPanel = new JPanel(new BorderLayout());
        n3Box = new JCheckBox("Apply N3 to cropped image");
        n3Box.setFont(serif12);

        
        mainPanel.add(instruction, BorderLayout.NORTH);
        mainPanel.add(n3Box, BorderLayout.SOUTH);
        
        

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(this.buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    private boolean setVariables() {
    	useN3 = n3Box.isSelected();
    	
    	return true;
    }
    
}

