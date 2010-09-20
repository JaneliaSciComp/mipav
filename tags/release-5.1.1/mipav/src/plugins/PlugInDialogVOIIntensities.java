import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;
import java.io.File;

import javax.swing.JFileChooser;




public class PlugInDialogVOIIntensities
    extends JDialogScriptableBase implements AlgorithmInterface {

   private File file = null;
   private VOI voi = null;
   private ModelImage image = null; // source image
   private PlugInAlgorithmVOIIntensities voiAlgo = null;
   	
   
    /**
     * Empty constructor required for dynamic instantiation during script execution.
     */
    public PlugInDialogVOIIntensities() {}

    /**
     *  Sets variables needed to call algorithm.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public PlugInDialogVOIIntensities(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        image = imA;
        init();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param imA	Source image.
     */
    public PlugInDialogVOIIntensities(ViewUserInterface UI, ModelImage imA) {
        super();

        image = imA;
        init();
    }
    
    private void init() {
    	JFileChooser chooser;
          
          
    	int nVOI;
    	int i;
    	ViewVOIVector VOIs;
    	
    	VOIs = (ViewVOIVector) image.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
            	voi = VOIs.VOIAt(i);
                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI.");

            return;
        }

        chooser = new JFileChooser();
        chooser.setDialogTitle("[Plugin v1.0] Save VOI Intensities to: ");

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        
        int returnVal = chooser.showSaveDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
        	file = chooser.getSelectedFile();
        	callAlgorithm();
        } else {
            return;
        }
        
    }
    
    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();

        parentFrame = image.getParentFrame();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
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
        	voiAlgo = new PlugInAlgorithmVOIIntensities(image, voi, file);
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
        	voiAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), " ...", voiAlgo);
            
            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {
                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (voiAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            }
            else {
            	voiAlgo.run();
            }
        }
        catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError(
                "PlugInDialogVOIIntensities: unable to allocate enough memory");
            return;
        }
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

           if (algorithm instanceof PlugInAlgorithmVOIIntensities) {
               if (voiAlgo.isCompleted() == true) {
                   //insertScriptLine();
               }
           }
       }
}
