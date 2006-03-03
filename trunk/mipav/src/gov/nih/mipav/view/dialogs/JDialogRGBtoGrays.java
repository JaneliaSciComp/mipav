package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import java.awt.event.*;
import java.awt.*;


/**
 *   Dialog to call a algorithm to convert an RGB to a grayscale images. This
 *   dialog will not be visible because it does not require user input at
     *   this time. It was made a dialog object because it may in the future require
 *   user input and to be consistent with the dialog/algorithm
 *   paradigm. In should be noted, that the algorithms are executed in
 *   their own thread.
 *
 *		@version    0.1 Nov 17, 1998
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 */
public class JDialogRGBtoGrays
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

  private AlgorithmRGBtoGrays RGBAlgo;
  private ModelImage imageA = null;
  private ModelImage resultImageR = null; // result image
  private ModelImage resultImageG = null; // result image
  private ModelImage resultImageB = null; // result image
  private ViewUserInterface userInterface;

  /**
   *  Sets variables needed to call algorithm.
   *  @param theParentFrame    Parent frame
   *  @param imA               Source image
   */
  public JDialogRGBtoGrays(Frame theParentFrame, ModelImage imA) {
    super(theParentFrame, true);
    imageA = imA;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
  }

  /**
       *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param imA	Source image.
   */
  public JDialogRGBtoGrays(ViewUserInterface UI, ModelImage imA) {
    super();
    userInterface = UI;
    imageA = imA;
    parentFrame = imageA.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogRGBtoGrays() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;
      String imageRKey = null;
      String imageGKey = null;
      String imageBKey = null;

      try {
          srcImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(srcImageKey);

      imageA = im;
      userInterface = imageA.getUserInterface();
      parentFrame = imageA.getParentFrame();

      // the result image
      try {
          imageRKey = parser.getNextString();
          imageGKey = parser.getNextString();
          imageBKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);
      callAlgorithm();
      parser.putVariable(imageRKey, getResultImageR().getImageName());
      parser.putVariable(imageGKey, getResultImageG().getImageName());
      parser.putVariable(imageBKey, getResultImageB().getImageName());
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the match image is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(imageA.getImageName());
                  }
              }

              userInterface.getScriptDialog().append("RGBtoGrays " +
                                                     userInterface.getScriptDialog().
                                                     getVar(imageA.getImageName()) +
                                                     " ");
              userInterface.getScriptDialog().putVar(resultImageR.getImageName());
              userInterface.getScriptDialog().append(userInterface.getScriptDialog().
                                                     getVar(resultImageR.
                                                            getImageName()) + " ");
              userInterface.getScriptDialog().putVar(resultImageG.getImageName());
              userInterface.getScriptDialog().append(userInterface.getScriptDialog().
                                                     getVar(resultImageG.
                                                            getImageName()) + " ");
              userInterface.getScriptDialog().putVar(resultImageB.getImageName());
              userInterface.getScriptDialog().append(userInterface.getScriptDialog().
                                                     getVar(resultImageB.
                                                            getImageName()) + "\n");
          }
      }
  }

  /**
   *   Accessor that returns the Red result image.
   *   @return resultImageR
   */
  public ModelImage getResultImageR() {
    return resultImageR;
  }

  /**
   *   Accessor that returns the Green result image.
   *   @return resultImageG
   */
  public ModelImage getResultImageG() {
    return resultImageG;
  }

  /**
   *   Accessor that returns the Blue result image.
   *   @return resultImageB
   */
  public ModelImage getResultImageB() {
    return resultImageB;
  }

  /**
   *    Calls the algorithm from the script parser.
   *    @param event  Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
  }

  /**
   *    Calls the algorithm.
   */
  public void callAlgorithm() {
    try {
      if (imageA.getType() == ModelStorageBase.ARGB) {
        resultImageR = new ModelImage(ModelImage.UBYTE, imageA.getExtents(),
                                      "GrayR",
                                      userInterface);
        resultImageG = new ModelImage(ModelImage.UBYTE, imageA.getExtents(),
                                      "GrayG",
                                      userInterface);
        resultImageB = new ModelImage(ModelImage.UBYTE, imageA.getExtents(),
                                      "GrayB",
                                      userInterface);
      }
      else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
        resultImageR = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                      "GrayR",
                                      userInterface);
        resultImageG = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                      "GrayG",
                                      userInterface);
        resultImageB = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                      "GrayB",
                                      userInterface);
      }
      else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
        resultImageR = new ModelImage(ModelImage.FLOAT, imageA.getExtents(),
                                      "GrayR",
                                      userInterface);
        resultImageG = new ModelImage(ModelImage.FLOAT, imageA.getExtents(),
                                      "GrayG",
                                      userInterface);
        resultImageB = new ModelImage(ModelImage.FLOAT, imageA.getExtents(),
                                      "GrayB",
                                      userInterface);
      }
      // Make algorithm
      RGBAlgo = new AlgorithmRGBtoGrays(resultImageR, resultImageG,
                                        resultImageB, imageA);
      // This is very important. Adding this object as a listener allows the algorithm to
      // notify this object when it has completed of failed. See algorithm performed event.
      // This is made possible by implementing AlgorithmedPerformed interface
      RGBAlgo.addListener(this);
      // Hide dialog
      setVisible(false);
      if (runInSeparateThread) {
        // Start the thread as a low priority because we wish to still have user interface work fast.
        if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
          MipavUtil.displayError("A thread is already running on this object");
        }
      }
      else {
        RGBAlgo.setActiveImage(isActiveImage);
        if (!userInterface.isAppFrameVisible()) {
          RGBAlgo.setProgressBarVisible(false);
        }
        RGBAlgo.run();
      }
    }
    catch (OutOfMemoryError x) {

      if (resultImageR != null) {
        resultImageR.disposeLocal(); // Clean up memory of result image
        resultImageR = null;
      }
      if (resultImageG != null) {
        resultImageG.disposeLocal(); // Clean up memory of result image
        resultImageG = null;
      }
      if (resultImageB != null) {
        resultImageB.disposeLocal(); // Clean up memory of result image
        resultImageB = null;
      }
      System.gc();
      MipavUtil.displayError(
          "Dialog RGB to Grays: unable to allocate enough memory");
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
       ViewJFrameImage imageFrameR = null;
       ViewJFrameImage imageFrameG = null;
       ViewJFrameImage imageFrameB = null;

       if (algorithm instanceof AlgorithmRGBtoGrays) {
         if (RGBAlgo.isCompleted() == true && resultImageB != null) {
           //The algorithm has completed and produced a new image to be displayed.

           updateFileInfo(imageA, resultImageR);
           updateFileInfo(imageA, resultImageG);
           updateFileInfo(imageA, resultImageB);

           try {
             imageFrameR = new ViewJFrameImage(resultImageR, null, new Dimension(610, 200));
             imageFrameG = new ViewJFrameImage(resultImageG, null, new Dimension(650, 250));
             imageFrameB = new ViewJFrameImage(resultImageB, null, new Dimension(690, 300));
           }
           catch (OutOfMemoryError error) {
             System.gc();
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImageR != null) {
           //algorithm failed but result image still has garbage
           resultImageR.disposeLocal(); // clean up memory
           resultImageR = null;
           if (resultImageG != null) {
             resultImageG.disposeLocal(); // clean up memory
             resultImageG = null;
           }
           if (resultImageB != null) {
             resultImageB.disposeLocal(); // clean up memory
             resultImageB = null;
           }

           System.gc();
         }
       }
       // Update frame
       if (parentFrame != null)
         ( (ViewJFrameBase) parentFrame).updateImages(true);

       insertScriptLine(algorithm);

       RGBAlgo.finalize();
       RGBAlgo = null;
     }
}
