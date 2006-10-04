import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;




public class PlugInDialogRemoveBlinks
    extends JDialogBase implements AlgorithmInterface {

    private PlugInAlgorithmRemoveBlinks icgAlgo;
    private ModelImage image = null; // source image
    private ModelImage resultImage = null;

    private ViewUserInterface userInterface;

    /**
     *  Sets variables needed to call algorithm.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public PlugInDialogRemoveBlinks(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        image = imA;
        userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
        callAlgorithm();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param imA	Source image.
     */
    public PlugInDialogRemoveBlinks(ViewUserInterface UI, ModelImage imA) {
        super();

        userInterface = UI;
        image = imA;
        callAlgorithm();
    }

    public PlugInDialogRemoveBlinks() { }

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun() throws IllegalArgumentException
    {
        String srcImageKey = null;
        String destImageKey = null;

        try
        {
    //        srcImageKey = parser.getNextString();
        }
        catch (Exception e)
        {
            throw new IllegalArgumentException();
        }
 //       ModelImage im = parser.getImage(srcImageKey);

    //    image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try
        {
   //         destImageKey = parser.getNextString();
        }
        catch (Exception e)
        {
            throw new IllegalArgumentException();
        }

        //setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();
        if (!srcImageKey.equals(destImageKey))
        {
      //      parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo)
    {
        if (algo.isCompleted())
        {
            if (userInterface.isScriptRecording())
            {
                //check to see if the match image is already in the ImgTable
         //       if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null)
          ////      {
         //           if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null)
         //           {
         //               userInterface.getScriptDialog().putActiveVar(image.getImageName());
         //           }
        //        }

        //        userInterface.getScriptDialog().append("PlugInDialogRemoveBlinks " +
        //            userInterface.getScriptDialog().
       //             getVar(image.getImageName()) +
       //             " ");

      //          userInterface.getScriptDialog().putVar(resultImage.getImageName());
      //          userInterface.getScriptDialog().append(userInterface.
      //              getScriptDialog().getVar(resultImage.getImageName()) + "\n");
            }
        }
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
   public ModelImage getResultImage()
   {
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

                   insertScriptLine(icgAlgo);

                   new ViewJFrameImage(icgAlgo.getResultImage());

               }
           }
       }
}
