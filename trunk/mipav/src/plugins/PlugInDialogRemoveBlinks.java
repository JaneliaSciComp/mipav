import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;


public class PlugInDialogRemoveBlinks extends JDialogScriptableBase implements AlgorithmInterface {

    private PlugInAlgorithmRemoveBlinks icgAlgo;
    private ModelImage image = null; // source image
    private ModelImage resultImage = null;

    /**
     *  Sets variables needed to call algorithm.
     *  @param  theParentFrame  Parent frame
     *  @param  imA             Source image
     */
    public PlugInDialogRemoveBlinks(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        image = imA;
        callAlgorithm();
    }

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogRemoveBlinks() { }

    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), true);
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Script")) {
            callAlgorithm();
        }
        else if (command.equals("Cancel")) {
            dispose();
        }
    }

    /**
     *    Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm
            icgAlgo = new PlugInAlgorithmRemoveBlinks(image, true);
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            icgAlgo.addListener(this);
            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (icgAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            }
            else {
               // icgAlgo.setActiveImage(isActiveImage);
                icgAlgo.run();
            }
        }
        catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError(
                "Dialog RGB to Gray: unable to allocate enough memory");
            return;
        }
    }

    /**
    *  Accessor that returns the image.
    *  @return  The result image.
    */
   public ModelImage getResultImage() {
       return resultImage;
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

           if (algorithm instanceof PlugInAlgorithmRemoveBlinks) {
               if (icgAlgo.isCompleted() == true) {

                   resultImage = icgAlgo.getResultImage();

                   insertScriptLine();

                   new ViewJFrameImage(icgAlgo.getResultImage());

               }
           }
       }
}
