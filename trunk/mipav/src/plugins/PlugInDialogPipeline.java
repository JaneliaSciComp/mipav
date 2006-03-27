import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.*;

/*
import java.util.*;
import javax.swing.event.*;
import javax.swing.border.*;
import gov.nih.mipav.model.file.*;
 */

/**
 *
 *
 *		@version    0.1 Nov 17, 1998
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 */
public class PlugInDialogPipeline
    extends JDialogBase implements AlgorithmInterface {

    private PlugInAlgorithmPipeline PipelineAlgo;
    private PlugInAlgorithmPipelineB PipelineAlgoB;
    private ModelImage imageA = null; // source image

    private ModelImage resultImageA = null;
    private ModelImage resultImageB = null;
    private ModelImage obMaskA = null;
    private ModelImage obMaskB = null;

    private ViewUserInterface userInterface;

    /**
     *
     * ModelImage destImageA, ModelImage destImageB,
                                     ModelImage obMaskA, ModelImage obMaskB,
                                     ModelImage srcImage

     *  Sets variables needed to call algorithm.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public PlugInDialogPipeline(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, false);
        imageA = imA;
        userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
        System.err.println("in constructor of dialog pipe");
        callAlgorithm( true );
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param imA	Source image.
     */
    public PlugInDialogPipeline(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        imageA = imA;
        callAlgorithm( true );
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            //user has said okay to having input the VOIs
            setVisible(false);
            callAlgorithm( false );

        }
        else if (command.equals("Cancel")) {
            dispose();
        }
    }

    /**
     *    Calls the algorithm.
     */
    public void callAlgorithm( boolean doStageOne) {

    if (doStageOne) {
    	
    	System.err.println("doing stage one");
            try {
                // Make algorithm
                PipelineAlgo = new PlugInAlgorithmPipeline(resultImageA, resultImageB,
                    obMaskA, obMaskB, imageA);
                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                PipelineAlgo.addListener(this);
                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (PipelineAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                }
                else {
                    PipelineAlgo.setActiveImage(isActiveImage);
                    PipelineAlgo.run();

                }
            }
            catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("AlgorithmAbsoluteValue: unable to allocate enough memory");
                return;
            }

        }
        //finished stage one, has VOIs, go to stage 2
        else {
             userInterface.getMessageFrame().addTab("Segmented Images - Results:  "+PlugInAlgorithmPipeline.patientID);
            // Make algorithm
                PipelineAlgoB = new PlugInAlgorithmPipelineB(resultImageA, resultImageB,
                    obMaskA, obMaskB);
                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                PipelineAlgoB.addListener(this);
                // Hide dialog
                setVisible(false);

                if (runInSeparateThread) {
                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (PipelineAlgoB.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                }
                else {
                    PipelineAlgoB.setActiveImage(isActiveImage);
                    PipelineAlgoB.run();
                }
        }
    }

    /**
     * Will display (make visible) the dialog so that the user
     * can input the needed VOIs
     */
    private void init() {
        this.setSize(100, 80);

        setTitle("VOI INPUT PLEASE");
        JLabel instruction = new JLabel(" Please enter VOIs, then press OK.");
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(instruction, BorderLayout.CENTER);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(this.buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    //************************************************************************
     //************************** Algorithm Events ****************************
      //************************************************************************

       /**
        *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
        *   algorithms when it has completed or failed to to complete, so that the dialog can be display
        *   the result image and/or clean up.
        *   @param algorithm   Algorithm that caused the event.
        */
       public void algorithmPerformed(AlgorithmBase algorithm) {
           System.err.println("Algorithm performed");
           ViewJFrameImage imageFrame = null;
           imageA.clearMask();

           if (algorithm instanceof PlugInAlgorithmPipeline) {
               if (PipelineAlgo.isCompleted() == true) {
                   //The algorithm has completed and produced a new image to be displayed.
                   resultImageA = PipelineAlgo.getResultImageA();
                   resultImageB = PipelineAlgo.getResultImageB();

                   obMaskA = PipelineAlgo.getMaskA();
                   obMaskB = PipelineAlgo.getMaskB();

                   updateFileInfo(imageA, resultImageA);
                   updateFileInfo(imageA, resultImageB);
                   float[] res1 = imageA.getFileInfo()[0].getResolutions();
                   float[] resA = resultImageA.getFileInfo()[0].getResolutions();
                   float[] resB = resultImageB.getFileInfo()[0].getResolutions();

                   ViewJFrameImage f1 = new ViewJFrameImage(resultImageA);
                   ViewJFrameImage f2 = new ViewJFrameImage(resultImageB, null, new Dimension ((int)f1.getLocation().getX()+250, (int)f1.getLocation().getY()));

                   //go to stage 2
                   init();
               }
           }
        else if (algorithm instanceof PlugInAlgorithmPipelineB) {
            if (PipelineAlgoB.isCompleted() == true) {

                System.err.println("GOT PASSED THE SECOND STAGE");

                //The algorithm has completed and produced a new image to be displayed.
                resultImageA = PipelineAlgoB.getResultImageA();
                resultImageB = PipelineAlgoB.getResultImageB();

                resultImageA.setImageName("Right Thigh Final Segmentation");
                resultImageB.setImageName("Left Thigh Final Segmentation");

                updateFileInfo(imageA, resultImageA);
                updateFileInfo(imageA, resultImageB);

                ViewJFrameImage f1 = new ViewJFrameImage(resultImageA);
                ViewJFrameImage f2 = new ViewJFrameImage(resultImageB, null, new Dimension ((int)f1.getLocation().getX()+250, (int)f1.getLocation().getY()));

                //section putin fri feb10
                //myFrame.getLUTa().setColor( index, Color);
                //Where index is the static value for FAT etc, and color is the new color.
                //Then Do myFrame.updateImages(true);

                f1.getLUTa().setColor(PlugInAlgorithmPipelineB.MUSCLE, Color.GREEN);
                f1.getLUTa().setColor(PlugInAlgorithmPipelineB.FAT, Color.MAGENTA);
                f1.getLUTa().setColor(PlugInAlgorithmPipelineB.BONE, Color.RED);
                f1.getLUTa().setColor(PlugInAlgorithmPipelineB.BONE_MARROW, Color.PINK);
                f1.getLUTa().setColor(PlugInAlgorithmPipelineB.SUB_CUT_FAT, Color.YELLOW);
                f1.updateImages(true);

                f2.getLUTa().setColor(PlugInAlgorithmPipelineB.MUSCLE, Color.GREEN);
                f2.getLUTa().setColor(PlugInAlgorithmPipelineB.FAT, Color.MAGENTA);
                f2.getLUTa().setColor(PlugInAlgorithmPipelineB.BONE, Color.RED);
                f2.getLUTa().setColor(PlugInAlgorithmPipelineB.BONE_MARROW, Color.PINK);
                f2.getLUTa().setColor(PlugInAlgorithmPipelineB.SUB_CUT_FAT, Color.YELLOW);
                f2.updateImages(true);

                PipelineAlgoB.finalize();
            }
        }
    }
}
