package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.file.*;

/**
 *      Algorithm that can match:
 *			the orientation (based on the images saved Transform Matrix),
 *			the origin (based on the image's saved origin, assumed as of Nov, 2004 to
 *			be ordered according to the GE's Left-Posterior-Superior ordering
 *			and not according to the image's X-Y-Z directions),
 *			the resolution,
 *			and
 *			the dimensions
 *			of a pair of images
 *			so that they can be easily compared.
 *
 *		@author Zohara A Cohen, Ph.D.
 */
public class AlgorithmMatchImages
    extends AlgorithmBase {

  private ViewUserInterface UI;
  private float eps = 1.0e-6f;

  // Image info
  private ModelImage sourceImgA, sourceImgB; // Source images
  private ModelImage resultImgA, resultImgB; // Result images
  String nameA, nameB;
  private int unitsOfMeasureA[], unitsOfMeasureB[];
  private int nDims;
  private int dimA[], dimB[];
  private double resA[], resB[];
  private double[] oDiffD;
  private FileInfoBase fileInfo;
  private boolean newA, newB; // Indicates whether new images were created.

  // Image origin and end location
  private double origLPS_A[], origLPS_B[];
  private double origImg_A[], origImg_B[];
  private double endImg_A[], endImg_B[];
  private double endLPS_A[], endLPS_B[];
  private double switchEnd2Orig[];

  // Orientation matrices and ordering indices
  private Matrix LPS2img_A, LPS2img_B;
  private Matrix img2LPS_A, img2LPS_B;
  private TransMatrix alignImgB2ImgA;
  private AlgorithmTransform algoTransform;
  private int reorderB2A[];
  private int sign2LPS_A[], sign2LPS_B[];

  // boolean variable that determine what matchings are done
  private boolean changeUnits = false;
  private boolean resByRef = false;
  private boolean doOrigins = true;
  private boolean doDimensions = true;
  private boolean stopped = false;

  private boolean doOrients = true;
  private double padValue = 0.0;

  /**
   *   Creates new algorithm.
   *   @param sourceImgA_       image A
   *   @param sourceImgB_       image B
   *   @param doOrigins_
   *   @param doDimensions_
   *   @param resByRef_
   */
  public AlgorithmMatchImages(ModelImage sourceImgA_, ModelImage sourceImgB_,
                              boolean doOrigins_, boolean doDimensions_,
                              boolean resByRef_) {
    super(null, sourceImgA_);
    this.sourceImgA = sourceImgA_;
    this.sourceImgB = sourceImgB_;
    this.doOrigins = doOrigins_;
    this.doDimensions = doDimensions_;
    this.resByRef = resByRef_;
    this.UI = sourceImgA.getUserInterface();
    nDims = sourceImgA.getNDims();

    try {
      dimA = new int[nDims];
      resA = new double[nDims];
      dimB = new int[nDims];
      resB = new double[nDims];
      for (int i = 0; i < nDims; i++) {
          resA[i] = (double) sourceImgA.getFileInfo(0).getResolutions()[i];
          dimA[i] = sourceImgA.getExtents()[i];
          resB[i] = (double) sourceImgB.getFileInfo(0).getResolutions()[i];
          dimB[i] = sourceImgB.getExtents()[i];
      }
      oDiffD = new double[nDims];
      origLPS_A = new double[nDims];
      origLPS_B = new double[nDims];
      origImg_A = new double[nDims];
      origImg_B = new double[nDims];
      endLPS_A = new double[nDims];
      endLPS_B = new double[nDims];
      endImg_A = new double[nDims];
      endImg_B = new double[nDims];
    }
    catch (OutOfMemoryError e) {
      System.gc();
      displayError("Algorithm reports: Out of memory");
      setCompleted(false);
      return;
    }

    if (sourceImgA.getNDims() != sourceImgB.getNDims()) {
      displayError("Images don't have the number of dimensions.");
      setCompleted(false);
      return;
    }
  }

  public void runAlgorithm() {
    /**
     *  Runs the program
     */

    // Create result images.
    String tag = new String("_matched2_");
    nameA = JDialogBase.makeImageName(sourceImgA.getImageName(),
                                      tag.concat(sourceImgB.getImageName()));
    nameB = JDialogBase.makeImageName(sourceImgB.getImageName(),
                                      tag.concat(sourceImgA.getImageName()));
    newA = false;
    newB = false;

    constructLog();

    // Get data
    getData();

    // Match image colors
    matchColors();

    if (doOrients) {
      // The only way this can be false is if it's explicity set to be.  JDialogMatchImages
      // do not given an option not to match orientations.
      // Match orientations.
      if (!alignImgB2ImgA.isIdentity()) {
        Preferences.debug("\n Matching orientations...\n");
        matchOrients();
      }
      else {
        Preferences.debug("Orientations already match.\n");
      }

      // Stop, if necessary
      if (stopped) {
        doStop();
        return;
      }
    }
    // Match resolutions
    checkUnits();
    boolean doResols = false;
    for (int i = 0; i < nDims; i++) {
      if (resB[i] != resA[i]) {
        if ( (resB[i] >= resA[i] - eps) && (resB[i] <= resA[i] + eps)) {
          resB[i] = resA[i];
        }
        else {
          doResols = true;
        }
      }
    }
    if (doResols) {
      Preferences.debug("\nMatching resolutions...\n");
      matchResolutions();
    }
    else {
        Preferences.debug("Resolutions already match.\n");
    }

    // Stop, if necessary
    if (stopped) {
      doStop();
      return;
    }

    // Match origins.
    if (doOrigins) {
      Preferences.debug("\nMatching origins...\n");
      matchOrigins();
    }

    // Stop, if necessary
    if (stopped) {
      doStop();
      return;
    }

    // Match dimensions.
    if (doDimensions) {
      doDimensions = false;
      for (int i = 0; i < nDims; i++) {
        if (dimA[i] != dimB[i]) {
          doDimensions = true;
        }
      }
    }
    if (doDimensions) {
      Preferences.debug("\nMatching dimensions...\n");
      matchDimensions();
    }

    setCompleted(true);
  }

  private void getData() {
    /**
     *   Get origin, transformation data, and field of view.
     *   This has to be done regardless of which other options the user chose.
     */
    double fovA[], fovB[], fovA_LPS[], fovB_LPS[];
    TransMatrix trans;
    TransMatrix trans2D;
    Matrix img2;
    double rotX, rotY, rotZ;
    boolean success;
    double tempA;
    double tempB;
    int i, j;

    try {
      LPS2img_A = new Matrix(nDims+1, nDims+1);
      LPS2img_B = new Matrix(nDims+1, nDims+1);
      img2LPS_A = new Matrix(nDims+1, nDims+1);
      img2LPS_B = new Matrix(nDims+1, nDims+1);
      alignImgB2ImgA = new TransMatrix(nDims+1);
      reorderB2A = new int[nDims];
      sign2LPS_A = new int[nDims];
      sign2LPS_B = new int[nDims];
      switchEnd2Orig = new double[nDims];
      fovA = new double[nDims];
      fovB = new double[nDims];
      fovA_LPS = new double[nDims];
      fovB_LPS = new double[nDims];
    }
    catch (OutOfMemoryError e) {
      System.gc();
      displayError("AlgorithmMatchImages.getData() reports: Out of memory");
      setCompleted(false);
      stopped = true;
      return;
    }

    Preferences.debug("\n PRELIMINARY DATA: \n");

    // Find transformation matrix to reorient Image B.
    if (sourceImgA.getNDims() > 2) {
        trans = sourceImgA.getMatrix();
    }
    else {
        trans2D = sourceImgA.getMatrix();
        trans = new TransMatrix(4);
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 3; j++) {
                trans.set(i, j, trans2D.get(i, j));
            }
        }
    }
    success = trans.decomposeMatrix(trans);
    if (!success) {
        MipavUtil.displayError("Failure on decompose imageA matrix");
        stopped = true;
        setCompleted(false);
        return;  
    }
    rotZ = trans.getRotateZ() * Math.PI/180.0;
    if (sourceImgA.getNDims() > 2) {
        rotX = trans.getRotateX() * Math.PI/180.0;
        rotY = trans.getRotateY() * Math.PI/180.0;
        img2LPS_A.set(0, 0, 1.0);
        img2LPS_A.set(1, 1, Math.cos(rotX));
        img2LPS_A.set(1, 2, -Math.sin(rotX));
        img2LPS_A.set(2, 1, Math.sin(rotX));
        img2LPS_A.set(2, 2, Math.cos(rotX));
        img2LPS_A.set(3, 3, 1.0);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotY));
        img2.set(0, 2, Math.sin(rotY));
        img2.set(1, 1, 1.0);
        img2.set(2, 0, -Math.sin(rotY));
        img2.set(2, 2, Math.cos(rotY));
        img2.set(3, 3, 1.0);
        img2LPS_A.timesEquals(img2);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotZ));
        img2.set(0, 1, -Math.sin(rotZ));
        img2.set(1, 0, Math.sin(rotZ));
        img2.set(1, 1, Math.cos(rotZ));
        img2.set(2, 2, 1.0);
        img2.set(3, 3, 1.0);
        img2LPS_A.timesEquals(img2);
    }
    else {
        img2LPS_A.set(0, 0, Math.cos(rotZ));
        img2LPS_A.set(0, 1, -Math.sin(rotZ));
        img2LPS_A.set(1, 0, Math.sin(rotZ));
        img2LPS_A.set(1, 1, Math.cos(rotZ));
        img2LPS_A.set(2, 2, 1.0);
    }
    img2LPS_A = img2LPS_A.getMatrix(0, nDims, 0, nDims);
    LPS2img_A = img2LPS_A.inverse();
    
    if (sourceImgB.getNDims() > 2) {
        trans = sourceImgB.getMatrix();
    }
    else {
        trans2D = sourceImgB.getMatrix();
        trans = new TransMatrix(4);
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 3; j++) {
                trans.set(i, j, trans2D.get(i, j));
            }
        }
    }
    success = trans.decomposeMatrix(trans);
    if (!success) {
        MipavUtil.displayError("Failure on decompose imageB matrix");
        stopped = true;
        setCompleted(false);
        return;  
    }
    rotZ = trans.getRotateZ() * Math.PI/180.0;
    if (sourceImgB.getNDims() > 2) {
        rotX = trans.getRotateX() * Math.PI/180.0;
        rotY = trans.getRotateY() * Math.PI/180.0;
        img2LPS_B.set(0, 0, 1.0);
        img2LPS_B.set(1, 1, Math.cos(rotX));
        img2LPS_B.set(1, 2, -Math.sin(rotX));
        img2LPS_B.set(2, 1, Math.sin(rotX));
        img2LPS_B.set(2, 2, Math.cos(rotX));
        img2LPS_B.set(3, 3, 1.0);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotY));
        img2.set(0, 2, Math.sin(rotY));
        img2.set(1, 1, 1.0);
        img2.set(2, 0, -Math.sin(rotY));
        img2.set(2, 2, Math.cos(rotY));
        img2.set(3, 3, 1.0);
        img2LPS_B.timesEquals(img2);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotZ));
        img2.set(0, 1, -Math.sin(rotZ));
        img2.set(1, 0, Math.sin(rotZ));
        img2.set(1, 1, Math.cos(rotZ));
        img2.set(2, 2, 1.0);
        img2.set(3, 3, 1.0);
        img2LPS_B.timesEquals(img2);
    }
    else {
        img2LPS_B.set(0, 0, Math.cos(rotZ));
        img2LPS_B.set(0, 1, -Math.sin(rotZ));
        img2LPS_B.set(1, 0, Math.sin(rotZ));
        img2LPS_B.set(1, 1, Math.cos(rotZ));
        img2LPS_B.set(2, 2, 1.0);
    }
    img2LPS_B = img2LPS_B.getMatrix(0, nDims, 0, nDims);
    LPS2img_B = img2LPS_B.inverse();

    alignImgB2ImgA.identity();

    boolean matricesAreEqual = true;
    if (!img2LPS_A.equals(img2LPS_B)) {matricesAreEqual = false;}

    if (!matricesAreEqual) {
      alignImgB2ImgA.timesEquals(LPS2img_B);
      alignImgB2ImgA.timesEquals(img2LPS_A);
    }
    Preferences.debug("Transformation matrix to align img B to img A = \n"
                   +alignImgB2ImgA.toString());
    UI.setDataText("Transformation matrix to align img B to img A = \n"
                   +alignImgB2ImgA.toString());

    // Create index arrays that store the reordering of image data.
    for (i = 0; i < nDims; i++) { // i's are the rows
      tempA = 0.0;
      tempB = 0.0;  
      for (j = 0; j < nDims; j++) { // j's are the columns
        if (Math.abs(alignImgB2ImgA.get(i, j)) > Math.cos(Math.PI / 4.0)) {
          reorderB2A[i] = j;
        }
        tempA = tempA + LPS2img_A.get(i, j);
        tempB = tempB + LPS2img_B.get(i, j);
        switchEnd2Orig[j] = switchEnd2Orig[j] + alignImgB2ImgA.get(i, j);
        // note: adding all rows for a single column
      }
      if (tempA > 0.0) {
          sign2LPS_A[i] = 1;
      }
      else {
          sign2LPS_A[i] = -1;
      }
      if (tempB > 0.0) {
          sign2LPS_B[i] = 1;
      }
      else {
          sign2LPS_B[i] = -1;
      }
    }

    if (nDims == 3) {
        Preferences.debug("Indices to reorder ImgB to ImgA: " + reorderB2A[0] + ", " +
                          reorderB2A[1] + ", " + reorderB2A[2] + "\n");
        Preferences.debug("Original ImageA signs for LPS: " + sign2LPS_A[0] + ", " +
                          sign2LPS_A[1] + ", " + sign2LPS_A[2] + "\n");
        Preferences.debug("Original ImageB signs for LPS: " + sign2LPS_B[0] + ", " +
                          sign2LPS_B[1] + ", " + sign2LPS_B[2] + "\n");
    } // if (nDims == 3)
    else { // nDims == 2
        Preferences.debug("Indices to reorder ImgB to ImgA: " + reorderB2A[0] + ", " +
                          reorderB2A[1] + "\n");
        Preferences.debug("Original ImageA signs for LPS: " + sign2LPS_A[0] + ", " +
                          sign2LPS_A[1]  + "\n");
        Preferences.debug("Original ImageB signs for LPS: " + sign2LPS_B[0] + ", " +
                          sign2LPS_B[1] + "\n");
    } // else nDims == 2
    // Get image origin before transformation.
    for (i = 0; i < nDims; i++) {
      origLPS_A[i] = (double) sourceImgA.getFileInfo(0).getOrigin(i);
      origLPS_B[i] = (double) sourceImgB.getFileInfo(0).getOrigin(i);
    }

    if (nDims == 3) {
        Preferences.debug("Original ImageA startLocation: " + (float) origLPS_A[0] + ", "
                          + (float) origLPS_A[1] + ", " + (float) origLPS_A[2] + "\n");
        Preferences.debug("Original ImageB startLocation: " + (float) origLPS_B[0] + ", "
                          + (float) origLPS_B[1] + ", " + (float) origLPS_B[2] + "\n");
    }
    else { // nDims == 2
        Preferences.debug("Original ImageA startLocation: " + (float) origLPS_A[0] + ", "
                          + (float) origLPS_A[1] + "\n");
        Preferences.debug("Original ImageB startLocation: " + (float) origLPS_B[0] + ", "
                          + (float) origLPS_B[1] + "\n");
    } // else nDims == 2
    origImg_A = originLPS2Img(origLPS_A, sourceImgA);
    origImg_B = originLPS2Img(origLPS_B, sourceImgB);

    if (nDims == 3) {
        Preferences.debug("ImageA origins in image order: " + (float) origImg_A[0] + ", "
                          + (float) origImg_A[1] + ", " + (float) origImg_A[2] + "\n");
        Preferences.debug("ImageB origins in image order: " + (float) origImg_B[0] + ", "
                          + (float) origImg_B[1] + ", " + (float) origImg_B[2] + "\n");
        Preferences.debug("ImageB origins in ImageA order: " + (float) origLPS_B[reorderB2A[0]] +
                          ", " + (float) origLPS_B[reorderB2A[1]] + ", " +
                          (float) origLPS_B[reorderB2A[2]] + "\n");
    } // if (nDims == 3
    else { // nDims == 2
        Preferences.debug("ImageA origins in image order: " + (float) origImg_A[0] + ", "
                          + (float) origImg_A[1] + "\n");
        Preferences.debug("ImageB origins in image order: " + (float) origImg_B[0] + ", "
                          + (float) origImg_B[1] + "\n");
        Preferences.debug("ImageB origins in ImageA order: " + (float) origLPS_B[reorderB2A[0]] +
                          ", " + (float) origLPS_B[reorderB2A[1]] + "\n");

    } // else nDims == 2
    // Get image end locations
    for (i = 0; i < nDims; i++) {
      fovA[i] = resA[i] * (dimA[i] - 1); // field of view in this dimension
      fovB[i] = resB[i] * (dimB[i] - 1);
    }
    //Preferences.debug(
    //    "\nImageA field-of-view image order: " + fovA[0] + ", " + fovA[1] +
    //    ", " + fovA[2] + "\n");
    //Preferences.debug(
    //   "ImageB field-of-view image order: " + fovB[0] + ", " + fovB[1] + ", " +
    //    fovB[2] + "\n");

    for (i = 0; i < nDims; i++) { // i's are the rows
      for (j = 0; j < nDims; j++) { // j's are the columns
        fovA_LPS[i] = fovA_LPS[i] + (float) img2LPS_A.get(i, j) * fovA[j];
        fovB_LPS[i] = fovB_LPS[i] + (float) img2LPS_B.get(i, j) * fovB[j];
      }
    }
    //Preferences.debug(
    //    "ImageA field-of-view in LPS order: " + fovA_LPS[0] + ", " + fovA_LPS[1] +
    //    ", " + fovA_LPS[2] + "\n");
    //Preferences.debug(
    //    "ImageB field-of-view in LPS order: " + fovB_LPS[0] + ", " + fovB_LPS[1] +
    //    ", " + fovB_LPS[2] + "\n");

    for (i = 0; i < nDims; i++) {
      endLPS_A[i] = origLPS_A[i] + fovA_LPS[i];
      endLPS_B[i] = origLPS_B[i] + fovB_LPS[i];
    }

    //Preferences.debug
    //    ("ImageA end locations in LPS order: " + (float) endLPS_A[0] + ", " +
    //     (float) endLPS_A[1] + ", " + (float) endLPS_A[2] + "\n");
    //Preferences.debug
    //    ("ImageB end locations in LPS order: " + (float) endLPS_B[0] + ", " +
    //     (float) endLPS_B[1] + ", " + (float) endLPS_B[2] + "\n");

    endImg_A = originLPS2Img(endLPS_A, sourceImgA);
    endImg_B = originLPS2Img(endLPS_B, sourceImgB);
    if (nDims == 3) {
        Preferences.debug
                ("ImageA end locations in image order: " + (float) endImg_A[0] + ", " +
                 (float) endImg_A[1] + ", " + (float) endImg_A[2] + "\n");
        Preferences.debug
                ("ImageB end locations in image order: " + (float) endImg_B[0] + ", " +
                 (float) endImg_B[1] + ", " + (float) endImg_B[2] + "\n");

        // Get image orientation
        sign2LPS_A = AlgorithmTransform.getImageOrient(sourceImgA);
    } // if (nDims == 3)
    else { // nDims == 2
        Preferences.debug
            ("ImageA end locations in image order: " + (float) endImg_A[0] + ", " +
             (float) endImg_A[1] + "\n");
        Preferences.debug
            ("ImageB end locations in image order: " + (float) endImg_B[0] + ", " +
             (float) endImg_B[1] + "\n");
        // Get image orientation
        int direct[] = new int[3];
        direct = AlgorithmTransform.getImageOrient(sourceImgA);
        sign2LPS_A[0] = direct[0];
        sign2LPS_A[1] = direct[1];
    } // else nDims == 2
  }

  private void constructLog() {
    /**
     *   Constructs a string of the construction parameters and
     *   outputs the string to the messsage frame if the logging
     *   procedure is turned on.
     */
    historyString = new String("Matching image properties()\n");
  }

  private void matchColors() {
    /**
     *   Match color types.  If imageA is grayscale, make imageB grayscale.  If imageA is RGB...
     */

    // Check to see if the color types match up
    if (sourceImgA.isColorImage() && !sourceImgB.isColorImage()) {
      // If resultImgA is color and resultImgB isn't, change resultImgB to a color image.
      // Run algorithm AlgorithmRGBConcat
      if (!newB) {
        resultImgB = (ModelImage) sourceImgB.clone();
        newB = true;
      }
      resultImgB.setImageName("imageB_rgb");
      AlgorithmRGBConcat rgbAlgo = new AlgorithmRGBConcat(resultImgB,
          resultImgB, resultImgB, true);
      rgbAlgo.setActiveImage(activeImage);
      rgbAlgo.run();
      resultImgB = rgbAlgo.getImageR();

      // Clean up
      rgbAlgo.finalize();
      rgbAlgo = null;
    }
    else if (!sourceImgA.isColorImage() && sourceImgB.isColorImage()) {
      // If resultImgA is not color and resultImgB is, change resultImgB to a grayscale image.
      float redValue = 1.0f / 3.0f;
      float greenValue = 1.0f / 3.0f;
      float blueValue = 1.0f / 3.0f;
      float threshold = 1.0f;

      // Run algorithm AlgorithmRGBtoGray
      if (!newB) {
        resultImgB = (ModelImage) sourceImgB.clone();
        newB = true;
      }
      resultImgB.setImageName("imageB_gray");
      AlgorithmRGBtoGray rgbAlgo = new AlgorithmRGBtoGray(resultImgB, redValue,
          greenValue, blueValue,
          true, threshold, true);
      rgbAlgo.setActiveImage(activeImage);
      rgbAlgo.run();
      resultImgB = rgbAlgo.getSrcImage();

      rgbAlgo.finalize();
      rgbAlgo = null;

    }
  }

  private void matchOrients() {
    /**
     *   Reorient ImageB so that directions will coincide with ImageA.
     */
    int tID;
    int[] oldDims = new int[nDims];
    AlgorithmCrop algoCrop;

    //Call AlgorithmTransform with padding.
    resultImgB = (ModelImage) sourceImgB.clone();
    newB = true;
    resultImgB.setImageName("tempB");
    oldDims = resultImgB.getExtents();

    if (nDims == 3) {
        algoTransform = new AlgorithmTransform(resultImgB, alignImgB2ImgA,
                                               AlgorithmTransform.TRILINEAR,
                                               (float) resB[reorderB2A[0]],
                                               (float) resB[reorderB2A[1]],
                                               (float) resB[reorderB2A[2]],
                                               dimB[reorderB2A[0]],
                                               dimB[reorderB2A[1]],
                                               dimB[reorderB2A[2]], false, false, true);
    } // if (nDims == 3)
    else { // nDims == 2
        algoTransform = new AlgorithmTransform(resultImgB, alignImgB2ImgA,
                                               AlgorithmTransform.BILINEAR,
                                               (float) resB[reorderB2A[0]],
                                               (float) resB[reorderB2A[1]],
                                               dimB[reorderB2A[0]],
                                               dimB[reorderB2A[1]],
                                               false, false, true);
    } // else nDims == 2
    algoTransform.setActiveImage(activeImage);
    algoTransform.setPadValue((int)padValue);

    algoTransform.run();
    if (algoTransform.isCompleted() == false) {
      Preferences.debug("algoTransform in matchOrients failed.\n");
      algoTransform.finalize();
      algoTransform = null;
      stopped = true;
      setCompleted(false);
      return;
    }
    resultImgB.disposeLocal();
    resultImgB = algoTransform.getTransformedImage();
    resultImgB.setImageName(nameB);
    resultImgB.setMatrix(sourceImgA.getMatrix());

    algoTransform.finalize();
    algoTransform = null;

    // Get updated resolutions and dimensions.
    double[] tempRes = new double[nDims];
    for (int i = 0; i < nDims; i++) {
      tempRes[i] = resB[reorderB2A[i]];
    }
    for (int i = 0; i < nDims; i++) {
      resB[i] = tempRes[i];
      dimB[i] = resultImgB.getExtents()[i];
    }

    if (nDims == 3) {
        Preferences.debug("ImageB resolution in matchOrients after transform: " +
                          (float) resB[0] + ", " + (float) resB[1] + ", "
                          + (float) resB[2] + "\n");
        Preferences.debug("ImageB dimensions in matchOrient after transform: " +
                          dimB[0] + ", " + dimB[1] + ", " + dimB[2] + "\n");
    } // if (nDims == 3)
    else { // nDims == 2
        Preferences.debug("ImageB resolution in matchOrients after transform: " +
                          (float) resB[0] + ", " + (float) resB[1] + "\n");
        Preferences.debug("ImageB dimensions in matchOrient after transform: " +
                          dimB[0] + ", " + dimB[1] + "\n");
    } // else nDims == 2
    // When necessary, shift start location to opposite end of image.
    // This version assumes that origin is stored in coord in the LPS order
    double[] temp = new double[nDims];

    for (int i = 0; i < nDims; i++) {
      if (switchEnd2Orig[i] < 0) {
        Preferences.debug("Shifting origin for direction " + i +
                          " to other side of image (i.e. adding fov)\n");
        temp[i] = origImg_B[i];
        origImg_B[i] = endImg_B[i];
        endImg_B[i] = temp[i];
      }
    }

    // Reorder image B origins, to image A order.
    for (int i = 0; i < nDims; i++) {
      temp[i] = origImg_B[i];
    }
    for (int i = 0; i < nDims; i++) {
      origImg_B[i] = temp[reorderB2A[i]];
    }

    if (nDims == 3) {
        Preferences.debug("ImageB origins (in image order) after switching to other side: "
                + (float) origImg_B[0] + ", " + (float) origImg_B[1] + ", "
                + (float) origImg_B[2] + "\n");
        UI.setDataText("New Image B origins (in image order): "
                       + (float) origImg_B[0] + ", " + (float) origImg_B[1] + ", " +
                       (float) origImg_B[2] + "\n");
    } // if (nDims == 3)
    else { // nDims == 2
        Preferences.debug("ImageB origins (in image order) after switching to other side: "
            + (float) origImg_B[0] + ", " + (float) origImg_B[1] + "\n");
        UI.setDataText("New Image B origins (in image order): "
               + (float) origImg_B[0] + ", " + (float) origImg_B[1] + "\n");
    } // else nDims == 2
    // Crop image on side opposite padding
    int[] cropAmt = new int[nDims];
    int[] boundX = new int[2];
    int[] boundY = new int[2];
    int[] boundZ = new int[2];
    int[] newDims = new int[nDims];

    for (int i = 0; i < nDims; i++) {
      if (dimB[i] > oldDims[reorderB2A[i]]) {
        cropAmt[i] = dimB[i] - oldDims[reorderB2A[i]];
      }
      newDims[i] = oldDims[reorderB2A[i]];
    }
    boundX[0] = 0;
    boundX[1] = dimB[0] - cropAmt[0] - 1;
    boundY[0] = 0;
    boundY[1] = dimB[1] - cropAmt[1] - 1;
    boundZ[0] = 0;
    if (nDims == 3) {
        boundZ[1] = dimB[2] - cropAmt[2] - 1;

        Preferences.debug("Original image dimensions: " + oldDims[reorderB2A[0]] +
                          " " + oldDims[reorderB2A[1]]
                          + " " + oldDims[reorderB2A[2]] + ".\n");
        Preferences.debug("OrientedB dimensions: " + dimB[0] + " " + dimB[1] + " " +
                          dimB[2] + ".\n");
        Preferences.debug("Amount to crop " + cropAmt[0] + ", " + cropAmt[1] +
                          ", and " + cropAmt[2] + " pixels.\n");
        Preferences.debug("New dimensions for reoriented Img B: " + newDims[0] +
                          " " + newDims[1] + " " + newDims[2] + ".\n");
        UI.setDataText("New dimension for reoriented Img B: " + newDims[0] +
                       " " + newDims[1] + " " + newDims[2] + ".\n");
    } // if (nDims == 3)
    else { // nDims == 2
        boundZ[1] = 0;
        Preferences.debug("Original image dimensions: " + oldDims[reorderB2A[0]] +
                          " " + oldDims[reorderB2A[1]] + ".\n");
        Preferences.debug("OrientedB dimensions: " + dimB[0] + " " + dimB[1] + ".\n");
        Preferences.debug("Amount to crop " + cropAmt[0] + " and " + cropAmt[1] + " pixels.\n");
        Preferences.debug("New dimensions for reoriented Img B: " + newDims[0] +
                          " " + newDims[1] + ".\n");
        UI.setDataText("New dimension for reoriented Img B: " + newDims[0] +
                       " " + newDims[1] + ".\n");
    } // else nDims == 2

    resultImgB.setImageName("tempB");
    algoCrop = new AlgorithmCrop(resultImgB, 0, boundX, boundY, boundZ);
    algoCrop.setActiveImage(activeImage);
    algoCrop.run();
    if (algoCrop.isCompleted() == false) {
      Preferences.debug("algoCrop in matchOrients failed.");
      algoCrop.finalize();
      algoCrop = null;
      setCompleted(false);
      stopped = true;
      return;
    }
    resultImgB.disposeLocal();
    resultImgB = algoCrop.getSrcImage();
    resultImgB.setImageName(nameB);

    algoCrop.finalize();
    algoCrop = null;

   // This should be changed so that it happens within updateFileInfo.  Write a method
   // that takes the transMatrix and updates the imageOrientation and axisOrientation.
   FileInfoBase fileInfoB = resultImgB.getFileInfo(0);
   fileInfoB.setAxisOrientation(sourceImgA.getFileInfo(0).getAxisOrientation());
   resultImgB.setFileInfo(fileInfoB, 0);

   for (int i = 0; i < nDims; i++) {
     dimB[i] = resultImgB.getExtents()[i];
   }
   if (nDims == 3) {
       Preferences.debug("After crop, dimB = " + dimB[0] + ", " + dimB[1] + ", " +
                         dimB[2] + "\n");
   }
   else { // nDims == 2
       Preferences.debug("After crop, dimB = " + dimB[0] + ", " + dimB[1] + "\n");
   } // else nDims == 2
   origLPS_B = originImg2LPS(origImg_B, resultImgB);
   if (nDims == 3) {
       Preferences.debug("Final ImageB origins in LPS order: " + (float) origLPS_B[0] + ", "
               + (float) origLPS_B[1] + ", " + (float) origLPS_B[2] + "\n");
       UI.setDataText("Image B origins (in LPS order) after matching orientation: "
               + (float) origLPS_B[0] + ", " + (float) origLPS_B[1] + ", " + (float) origLPS_B[2]
               + "\n");
       resultImgB.setImageOrientation(sourceImgA.getImageOrientation());
   } // if (nDims == 3)
   else { // nDims == 2
       Preferences.debug("Final ImageB origins in LPS order: " + (float) origLPS_B[0] + ", "
               + (float) origLPS_B[1] + "\n");
       UI.setDataText("Image B origins (in LPS order) after matching orientation: "
               + (float) origLPS_B[0] + ", " + (float) origLPS_B[1] + "\n");
   } // else nDims == 2
    // Update file info for result images.
    tID = sourceImgA.getFileInfo(0).getTransformID();
    updateFileInfo(resultImgB, dimB, resB, origLPS_B, tID);
 }

 private void matchResolutions() {
   /**
    *   Match resolutions - resample both images so that they have the higher of the two
    *   resolutions (i.e. lower pixel size) in each direction.
    *   If resByRef is set to true, then resolution will match reference image (image A),
    *   regardless of which resolution is higher.
    */

   double fovA, fovB;
   int tIDA, tIDB;
   boolean newResA = false;
   boolean newResB = false;

   for (int i = 0; i < nDims; i++) {
     fovA = resA[i] * dimA[i]; // field of view in this dimension
     fovB = resB[i] * dimB[i];
     if (resA[i] != resB[i]) {
       // can overwrite the resolution and dimension variables b/c no longer need originals.
       if (resA[i] < resB[i] || resByRef) {
         resB[i] = resA[i]; // lower pixel size in this dimension
          newResB = true;
          dimB[i] = (int) Math.round(fovB / resB[i]);
        }
        else {
          resA[i] = resB[i];
          newResA = true;
          dimA[i] = (int) Math.round(fovA / resA[i]);
        }
      }
    }

    TransMatrix identMatrix = new TransMatrix(nDims+1); // constructor sets matrix to identity
    TransMatrix tMatBkp;

    if (newResA) {
      newA = true;
      // resultImgA definitely has not been created at this point.
      resultImgA = (ModelImage) sourceImgA.clone();
      resultImgA.setImageName("tempA");
      if (nDims == 3) {
          Preferences.debug(
                  "Creating new version of Image A with resolutions: " + (float) resA[0] +
                  ", " + (float) resA[1] + ", " + (float) resA[2] +
                  " and dimensions: " + dimA[0] + ", " + dimA[1] + ", " + dimA[2] + "\n");
          //Call AlgorithmTransform with padding.
          algoTransform = new AlgorithmTransform(resultImgA, identMatrix,
                                                 AlgorithmTransform.TRILINEAR,
                                                 (float) resA[0], (float) resA[1],
                                                 (float) resA[2],
                                                 dimA[0], dimA[1], dimA[2], false, false, true);
      } // if (nDims == 3)
      else { // nDims == 2
          Preferences.debug(
              "Creating new version of Image A with resolutions: " + (float) resA[0] +
              ", " + (float) resA[1] +
              " and dimensions: " + dimA[0] + ", " + dimA[1] + "\n");
          //Call AlgorithmTransform with padding.
          algoTransform = new AlgorithmTransform(resultImgA, identMatrix,
                                                 AlgorithmTransform.BILINEAR,
                                                 (float) resA[0], (float) resA[1],
                                                 dimA[0], dimA[1], false, false, true);
      }
      algoTransform.setActiveImage(activeImage);
      algoTransform.setPadValue((int)padValue);
      algoTransform.run();
      if (algoTransform.isCompleted() == false) {
        Preferences.debug("algoTransform in matchResolutions failed - imageA.");
        algoTransform.finalize();
        algoTransform = null;
        setCompleted(false);
        stopped = true;
        return;
      }
      resultImgA.disposeLocal();
      resultImgA = algoTransform.getTransformedImage();
      algoTransform.finalize();
      algoTransform = null;

      // Update file info for result images.
      resultImgA.setImageName(nameA);
      resultImgA.setMatrix(sourceImgA.getMatrix());
      for (int i = 0; i < nDims; i++) {
        origLPS_A[i] = (double) resultImgA.getFileInfo(0).getOrigin(i);
      }
      tIDA = sourceImgA.getFileInfo(0).getTransformID();
      updateFileInfo(resultImgA, dimA, resA, origLPS_A, tIDA);
    }
    else {
      Preferences.debug(
          "No need to resample imageA.  Already at minimum resolution.\n");
    }

    if (newResB) {
      if (!newB) {
        resultImgB = (ModelImage) sourceImgB.clone();
        newB = true;
      }
      tIDB = resultImgB.getFileInfo(0).getTransformID();
      tMatBkp = resultImgB.getMatrix();
      resultImgB.setImageName("tempB");
      if (nDims == 3) {
          Preferences.debug(
                  "Creating new version of Image B with resolutions: " +
                  (float) resB[0] + ", " + (float) resB[1] + ", " + (float) resB[2] +
                  " and dimensions: " + dimB[0] + ", " + dimB[1] + ", " + dimB[2] +
                  "\n");
          //Call AlgorithmTransform with padding.
          algoTransform = new AlgorithmTransform(resultImgB, identMatrix,
                                                 AlgorithmTransform.TRILINEAR,
                                                 (float) resB[0], (float) resB[1],
                                                 (float) resB[2],
                                                 dimB[0], dimB[1], dimB[2], false, false, true);
      } // if (nDims == 3)
      else { // nDims == 2
          Preferences.debug(
              "Creating new version of Image B with resolutions: " +
              (float) resB[0] + ", " + (float) resB[1] +
              " and dimensions: " + dimB[0] + ", " + dimB[1] + "\n");
          //Call AlgorithmTransform with padding.
          algoTransform = new AlgorithmTransform(resultImgB, identMatrix,
                                                 AlgorithmTransform.BILINEAR,
                                                 (float) resB[0], (float) resB[1],
                                                 dimB[0], dimB[1], false, false, true);
      } // else nDims == 2
      algoTransform.setActiveImage(activeImage);
      algoTransform.setPadValue((int)padValue);
      algoTransform.run();
      if (algoTransform.isCompleted() == false) {
        Preferences.debug("algoTransform in matchResolutions failed - imageB.");
        algoTransform.finalize();
        algoTransform = null;
        setCompleted(false);
        stopped = true;
        return;
      }
      resultImgB.disposeLocal();
      resultImgB = algoTransform.getTransformedImage();
      algoTransform.finalize();
      algoTransform = null;

      // Update file info for result images.
      resultImgB.setImageName(nameB);
      resultImgB.setMatrix(tMatBkp);
      for (int i = 0; i < nDims; i++) {
        origLPS_B[i] = (double) resultImgB.getFileInfo(0).getOrigin(i);
      }
      updateFileInfo(resultImgB, dimB, resB, origLPS_B, tIDB);
    }
    else {
      Preferences.debug(
          "No need to resample imageB.  Already at minimum resolution.\n\n");
    }
  }

  private void matchOrigins() {
    /**
     *   Add margins to images A and B so that their origins match.
     */

    int addA[], addB[], tID;
    double eps[];
    TransMatrix tMatBkp;
    AlgorithmAddMargins algoMarginsA, algoMarginsB;
    boolean newOriginA = false;
    boolean newOriginB = false;

    try {
      addA = new int[nDims];
      addB = new int[nDims];
      eps = new double[nDims];
    }
    catch (OutOfMemoryError e) {
      System.gc();
      displayError("AlgorithmMatchImages in matchOrigins reports: Out of memory");
      setCompleted(false);
      stopped = true;
      return;
    }

    // If all the start locations for Image A are 0.0, assume that start locations
    // aren't known and shouldn't be used in the registration.
    // Likewise for Image B.  Set the diff to zero.
    if (((nDims == 3) && ( (origLPS_A[0] == 0.0 && origLPS_A[1] == 0.0 && origLPS_A[2] == 0.0)
        || (origLPS_B[0] == 0.0 && origLPS_B[1] == 0.0 && origLPS_B[2] == 0.0))) ||
        ((nDims == 2) && ((origLPS_A[0] == 0.0 && origLPS_A[1] == 0.0) ||
           (origLPS_B[0] == 0.0 && origLPS_B[1] == 0.0)))) {
      Preferences.debug(
          "Start locations for either Image A or Image B are all zeros.  Start location won't be used "
          + " in registration.\n");
    }
    else {
      // Convert origins to current image coordinate systems.
      //origImg_A = originLPS2Img(origLPS_A, resultImgA);
      //origImg_B = originLPS2Img(origLPS_B, resultImgB);
      doOrigins = false;

      // Find differences in image coordinate system and add appropriately.
      for (int i = 0; i < nDims; i++) {
        oDiffD[i] = sign2LPS_A[i] * (origImg_B[i] - origImg_A[i]);
        eps[i] = resA[i];
        if (Math.abs(oDiffD[i]) > eps[i]) {
          doOrigins = true;
          if (oDiffD[i] > 0) {
            // add pixels to the start (left, top, front) of imageB
            addB[i] = (int) Math.round(oDiffD[i] / resB[i]);
            // convert to pixels from mm
            origImg_B[i] = origImg_B[i] - sign2LPS_A[i] * addB[i] * resB[i];
            if (!newB) {
              resultImgB = (ModelImage) sourceImgB.clone();
              newB = true;
            }
            newOriginB = true;
            // The reason to set the origin this way and not simply set it equal to the origin from
            // imageA is that the rounding to get the number of pixels may make the new origin for
            // imageB not exactly equal to that of imageA.
          }
          else {
            // add pixels to the start (left, top, front) of imageA
            addA[i] = -1 * (int) Math.round(oDiffD[i] / resA[i]);
            // convert to pixels from mm
            origImg_A[i] = origImg_A[i] - sign2LPS_A[i] * addA[i] * resA[i];
            if (!newA) {
              resultImgA = (ModelImage) sourceImgA.clone();
              newA = true;
            }
            newOriginA = true;
          }
        }
      }
      if (doOrigins) Preferences.debug("Origins will be matched.\n");

      //Call AlgorithmAddMargins.
      if (newOriginA) {
        if (nDims == 3) {
            Preferences.debug(
                    "Adding margins to imageA: " + addA[0] + ", " + addA[1] + ", " + addA[2] + "\n");
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug(
                    "Adding margins to imageA: " + addA[0] + ", " + addA[1] + "\n");
        } // else nDims == 2
        // update resolution and dimension variables
        for (int i = 0; i < nDims; i++) {
          dimA[i] = dimA[i] + addA[i];
        }
        if (nDims == 3) {
            Preferences.debug("New ImageA dimensions (matchOrigins): "
                              + dimA[0] + ", " + dimA[1] + ", " + dimA[2] + "\n");
        } // if (nDims == 3)
        else { // else nDims == 2
            Preferences.debug("New ImageA dimensions (matchOrigins): "
                              + dimA[0] + ", " + dimA[1] + "\n");
        } // else nDims == 2
        //resultImgA.setImageName("tempA");
        tID = resultImgA.getFileInfo(0).getTransformID();
        tMatBkp = resultImgA.getMatrix();

        if (nDims == 3) {
            algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, addA[0],
                                                   0, addA[1], 0, addA[2], 0);
        } // if (nDims == 3)
        else { // nDims == 2
            algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, addA[0],
                                                   0, addA[1], 0);
        } // else nDims == 2
        algoMarginsA.setActiveImage(activeImage);

        algoMarginsA.run();
        if (algoMarginsA.isCompleted() == false) {
          Preferences.debug("algoMarginsA in matchOrigins failed.");
          algoMarginsA.finalize();
          algoMarginsA = null;
          setCompleted(false);
          stopped = true;
          return;
        }
        resultImgA.disposeLocal();
        resultImgA = algoMarginsA.getSrcImage();
        resultImgA.setImageName(nameA);
        algoMarginsA.finalize();
        algoMarginsA = null;

        resultImgA.setMatrix(tMatBkp);
        origLPS_A = originImg2LPS(origImg_A, resultImgA);
        updateFileInfo(resultImgA, dimA, resA, origLPS_A, tID);
      }

      if (newOriginB) {
        if (nDims == 3) {
            Preferences.debug("Adding margins to imageB: " + addB[0] + ", "
                              + addB[1] + ", " + addB[2] + "\n");
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug("Adding margins to imageB: " + addB[0] + ", "
                              + addB[1] + "\n");
        } // else nDims == 2
        // update resolution and dimension variables
        for (int i = 0; i < nDims; i++) {
          dimB[i] = dimB[i] + addB[i];
        }
        if (nDims == 3) {
            Preferences.debug("New ImageB dimensions (matchOrigins): "
                    + dimB[0] + ", " + dimB[1] + ", " + dimB[2] + "\n");
        } // if (nDims == 3)
        else { // nDims == 2
            Preferences.debug("New ImageB dimensions (matchOrigins): "
                    + dimB[0] + ", " + dimB[1] + "\n");
        } // else nDims == 2
        resultImgB.setImageName("tempB");
        tID = resultImgB.getFileInfo(0).getTransformID();
        tMatBkp = resultImgB.getMatrix();

        if (nDims == 3) {
            algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, addB[0],
                                                   0, addB[1], 0, addB[2], 0);
        } // if (nDims == 3)
        else { // nDims == 2
            algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, addB[0],
                                                   0, addB[1], 0);
        } // else nDims == 2
        algoMarginsB.setActiveImage(activeImage);
        algoMarginsB.run();
        if (algoMarginsB.isCompleted() == false) {
          Preferences.debug("algoMarginsB in matchOrigins failed.");
          algoMarginsB.finalize();
          algoMarginsB = null;
          setCompleted(false);
          stopped = true;
          return;
        }
        resultImgB.disposeLocal();
        resultImgB = algoMarginsB.getSrcImage();
        resultImgB.setImageName(nameB);
        algoMarginsB.finalize();
        algoMarginsB = null;

        resultImgB.setMatrix(tMatBkp);
        origLPS_B = originImg2LPS(origImg_B, resultImgB);
        updateFileInfo(resultImgB, dimB, resB, origLPS_B, tID);
      }
    }
  }

  private void matchDimensions() {
    /**
     *   Append pixels to end (right, bottom, back) of whichever image has smaller dimension.
     *   Only gets called when matchDimensions is true but matchOrigins is not.  If matchOrigins
     *   is also true, this will get done within that algorithm.  Doesn't affect resolution.
     */

    int[] appendA = new int[nDims];
    int[] appendB = new int[nDims];
    int tIDA, tIDB;
    TransMatrix matA, matB;
    AlgorithmAddMargins algoMarginsA, algoMarginsB;
    boolean newDimA = false;
    boolean newDimB = false;

    // Find difference
    for (int i = 0; i < nDims; i++) {
      if (dimA[i] > dimB[i]) {
        appendB[i] = dimA[i] - dimB[i];
        dimB[i] = dimA[i];
        newDimB = true;
      }
      else if (dimB[i] > dimA[i]) {
        appendA[i] = dimB[i] - dimA[i];
        dimA[i] = dimB[i];
        newDimA = true;
      }
    }

    if (newDimA) {
      if (!newA) {
        resultImgA = (ModelImage) sourceImgA.clone();
        newA = true;
      }
      resultImgA.setImageName("tempA");
      tIDA = resultImgA.getFileInfo(0).getTransformID();
      matA = resultImgA.getMatrix();

      if (nDims == 3) {
          algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, 0, appendA[0],
                                                 0, appendA[1], 0, appendA[2]);
      } // if (nDims == 3)
      else { // nDims == 2
          algoMarginsA = new AlgorithmAddMargins(resultImgA, padValue, 0, appendA[0],
                                                 0, appendA[1]);
      } // else nDims == 2
      algoMarginsA.setActiveImage(activeImage);
      algoMarginsA.run();
      if (algoMarginsA.isCompleted() == false) {
        Preferences.debug(
            "algoMarginsA or algoMarginsB in matchDimensions failed.");
        algoMarginsA.finalize();
        algoMarginsA = null;
        setCompleted(false);
        stopped = true;
        return;
      }
      resultImgA.disposeLocal();
      resultImgA = algoMarginsA.getSrcImage();
      resultImgA.setImageName(nameA);
      algoMarginsA.finalize();
      algoMarginsA = null;
      if (nDims == 3) {
          Preferences.debug(
                  "New ImageA dimensions (matchDimensions): " + dimA[0] + ", " + dimA[1] +
                  ", " + dimA[2] + "\n");
      } // if (nDims == 3)
      else { // nDims == 2
          Preferences.debug(
              "New ImageA dimensions (matchDimensions): " + dimA[0] + ", " + dimA[1] + "\n");
      } // else nDims == 2
      //update file info
      resultImgA.setMatrix(matA);
      updateFileInfo(resultImgA, dimA, resA, origLPS_A, tIDA);
    }

    if (newDimB) {
      if (!newB) {
        resultImgB = (ModelImage) sourceImgB.clone();
        newB = true;
      }
      resultImgB.setImageName("tempB");
      tIDB = resultImgB.getFileInfo(0).getTransformID();
      matB = resultImgB.getMatrix();

      if (nDims == 3) {
          algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, 0, appendB[0],
                                                 0, appendB[1], 0, appendB[2]);
      } // if (nDims == 3)
      else { // nDims == 2
          algoMarginsB = new AlgorithmAddMargins(resultImgB, padValue, 0, appendB[0],
                                                 0, appendB[1]);
      } // else nDims == 2
      algoMarginsB.setActiveImage(activeImage);
      algoMarginsB.run();
      if (algoMarginsB.isCompleted() == false) {
        Preferences.debug(
            "algoMarginsA or algoMarginsB in matchDimensions failed.");
        algoMarginsB.finalize();
        algoMarginsB = null;
        setCompleted(false);
        stopped = true;
        return;
      }
      if (nDims == 3) {
          Preferences.debug(
                  "New ImageB dimensions (matchDimensions): " + dimB[0] + ", " + dimB[1] +
                  ", " + dimB[2] + "\n");
      } // if (nDims == 3)
      else { // nDims == 2
          Preferences.debug(
              "New ImageB dimensions (matchDimensions): " + dimB[0] + ", " + dimB[1] + "\n");
      } // else nDims == 2
      resultImgB.disposeLocal();
      resultImgB = algoMarginsB.getSrcImage();
      resultImgB.setImageName(nameB);
      algoMarginsB.finalize();
      algoMarginsB = null;

      //update file info
      resultImgB.setMatrix(matB);
      updateFileInfo(resultImgB, dimB, resB, origLPS_B, tIDB);
    }
  }

  /**
   * Exits from run method
   */
  public void doStop() {
    Preferences.debug(
        "AlgorithmMatchImages stopped by another algorithm that it called.");
  }

  /**
   *   Prepares this class for destruction.
   */
  public void finalize() {
    disposeLocal();
    super.finalize();
  }

  public void disposeLocal() {
    /**
     *   Get rid of space hogs.
     */
    LPS2img_A = null;
    img2LPS_A = null;
    img2LPS_B = null;
    LPS2img_B = null;
    alignImgB2ImgA = null;
    sourceImgA = null;
    sourceImgB = null;
  }


  /**
   *    Returns first of new images.
   *    @return resultImgA
   */
  public ModelImage getResultA() {
    resultImgA.calcMinMax();
    return this.resultImgA;
  }

  /**
   *    Returns second of new images.
   *    @return resultImgB
   */
  public ModelImage getResultB() {
    resultImgB.calcMinMax();
    return this.resultImgB;
  }

  /**
   * Tells if new imageA was created
   * @return newA
   */
  public boolean isNewA() {
    return this.newA;
  }

  /**
   * Tells if new imageB was created
   * @return newB
   */
  public boolean isNewB() {
    return this.newB;
  }

  /**
   *   Switch origin order from image order to LPS order.
   */
  private double[] originImg2LPS(double[] origImg, ModelImage img) {
    double[] origLPS = new double[nDims];
    Matrix img2LPS = new Matrix(nDims+1, nDims+1);
    TransMatrix trans;
    TransMatrix trans2D;
    boolean success;
    double rotX, rotY, rotZ;
    Matrix img2;
    int i, j;

    if (img.getNDims() > 2) {
        trans = img.getMatrix();
    }
    else {
        trans2D = img.getMatrix();
        trans = new TransMatrix(4);
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 3; j++) {
                trans.set(i, j, trans2D.get(i, j));
            }
        }
    }
    trans = img.getMatrix();
    success = trans.decomposeMatrix(trans);
    if (!success) {
        MipavUtil.displayError("Failure on decompose matrix in originImg2LPS");
        stopped = true;
        setCompleted(false);
        return null;  
    }
    rotZ = trans.getRotateZ() * Math.PI/180.0;
    if (img.getNDims() > 2) {
        rotX = trans.getRotateX() * Math.PI/180.0;
        rotY = trans.getRotateY() * Math.PI/180.0;
        img2LPS.set(0, 0, 1.0);
        img2LPS.set(1, 1, Math.cos(rotX));
        img2LPS.set(1, 2, -Math.sin(rotX));
        img2LPS.set(2, 1, Math.sin(rotX));
        img2LPS.set(2, 2, Math.cos(rotX));
        img2LPS.set(3, 3, 1.0);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotY));
        img2.set(0, 2, Math.sin(rotY));
        img2.set(1, 1, 1.0);
        img2.set(2, 0, -Math.sin(rotY));
        img2.set(2, 2, Math.cos(rotY));
        img2.set(3, 3, 1.0);
        img2LPS.timesEquals(img2);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotZ));
        img2.set(0, 1, -Math.sin(rotZ));
        img2.set(1, 0, Math.sin(rotZ));
        img2.set(1, 1, Math.cos(rotZ));
        img2.set(2, 2, 1.0);
        img2.set(3, 3, 1.0);
        img2LPS.timesEquals(img2);
    }
    else {
        img2LPS.set(0, 0, Math.cos(rotZ));
        img2LPS.set(0, 1, -Math.sin(rotZ));
        img2LPS.set(1, 0, Math.sin(rotZ));
        img2LPS.set(1, 1, Math.cos(rotZ));
        img2LPS.set(2, 2, 1.0);
    }
    
    for (i = 0; i < nDims; i++) { // i's are the rows
      for (j = 0; j < nDims; j++) { // j's are the columns
        if (Math.abs(img2LPS.get(i, j)) > Math.cos(Math.PI / 4.0))
        // plane is within 45 degrees of that direction
        {
          origLPS[i] = origImg[j];
        }
      }
    }
    return origLPS;
  }

  /***
   *   Switch origin order from LPS order to Img order.
   */
  private double[] originLPS2Img(double[] origLPS, ModelImage img) {
    double[] origImg = new double[nDims];
    Matrix LPS2img = new Matrix(nDims+1, nDims+1);
    TransMatrix trans;
    TransMatrix trans2D;
    boolean success;
    double rotX, rotY, rotZ;
    Matrix img2;
    int i,j;

    if (img.getNDims() > 2) {
        trans = img.getMatrix();
    }
    else {
        trans2D = img.getMatrix();
        trans = new TransMatrix(4);
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 3; j++) {
                trans.set(i, j, trans2D.get(i, j));
            }
        }
    }
    success = trans.decomposeMatrix(trans);
    if (!success) {
        MipavUtil.displayError("Failure on decompose matrix in originLPS2Img");
        stopped = true;
        setCompleted(false);
        return null;  
    }
    rotZ = trans.getRotateZ() * Math.PI/180.0;
    if (img.getNDims() > 2) {
        rotX = trans.getRotateX() * Math.PI/180.0;
        rotY = trans.getRotateY() * Math.PI/180.0;
        LPS2img.set(0, 0, 1.0);
        LPS2img.set(1, 1, Math.cos(rotX));
        LPS2img.set(1, 2, -Math.sin(rotX));
        LPS2img.set(2, 1, Math.sin(rotX));
        LPS2img.set(2, 2, Math.cos(rotX));
        LPS2img.set(3, 3, 1.0);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotY));
        img2.set(0, 2, Math.sin(rotY));
        img2.set(1, 1, 1.0);
        img2.set(2, 0, -Math.sin(rotY));
        img2.set(2, 2, Math.cos(rotY));
        img2.set(3, 3, 1.0);
        LPS2img.timesEquals(img2);
        img2 = new Matrix(nDims+1, nDims+1);
        img2.set(0, 0, Math.cos(rotZ));
        img2.set(0, 1, -Math.sin(rotZ));
        img2.set(1, 0, Math.sin(rotZ));
        img2.set(1, 1, Math.cos(rotZ));
        img2.set(2, 2, 1.0);
        img2.set(3, 3, 1.0);
        LPS2img.timesEquals(img2);
    }
    else {
        LPS2img.set(0, 0, Math.cos(rotZ));
        LPS2img.set(0, 1, -Math.sin(rotZ));
        LPS2img.set(1, 0, Math.sin(rotZ));
        LPS2img.set(1, 1, Math.cos(rotZ));
        LPS2img.set(2, 2, 1.0);
    }
    
    LPS2img = LPS2img.getMatrix(0, nDims, 0, nDims).inverse();
    for (i = 0; i < nDims; i++) { // i's are the rows
      for (j = 0; j < nDims; j++) { // j's are the columns
        if (Math.abs(LPS2img.get(i, j)) > Math.cos(Math.PI / 4.0))
        // plane is within 45 degrees of that direction
        {
          origImg[i] = origLPS[j];
        }
      }
    }
    return origImg;
  }

  /**
   * Update fileInfoBase
   *
   */
  void updateFileInfo(ModelImage image, int[] dim, double[] res, double[] start,
                      int tID) {
      int[] tempI3 = new int[nDims];
      float[] tempF3 = new float[nDims];
      float slicePos;
      int lastSlice;
      FileInfoBase fileInfoB;

    if (nDims == 3) {
        lastSlice = dim[2];
        slicePos = (float) start[2];
    }
    else {
        lastSlice = 1;
        slicePos = 0.0f;
    }
    //Set file properties of result image.
    for (int i = 0; i < nDims; i++) {
      tempI3[i] = dim[i];
    }
    for (int i = 0; i < nDims; i++) {
      tempF3[i] = (float) res[i];
    }

    image.setExtents(tempI3);
    for (int i = 0; i < lastSlice; i++) {
      fileInfoB = (FileInfoBase) image.getFileInfo(i);
      fileInfoB.setResolutions(tempF3);
      if (nDims == 3) {
          fileInfoB.setSliceSpacing(tempF3[2]);
      }
      fileInfoB.setOrigin( (float) start[0], 0);
      fileInfoB.setOrigin( (float) start[1], 1);
      fileInfoB.setOrigin(slicePos, 2);
      fileInfoB.setTransformID(tID);
      if (nDims == 3) {
          slicePos += (float) (sign2LPS_A[2] * res[2]);
      } // if (nDims == 3)
    }
  }

  /**
   *   Returns orgthogonal vector
   *   @param a    vector
   *   @param b    vector
   *   @return     vector orthogonal to a & b
   */
  /*double[] vectorCrossProduct(double[] a, double[] b) {
    double[] c = new double[3];
    //cross product c = axb = <a2b3-a3b2,a3b1-a1b3,a1b2-a2b1>
    c[0] = a[1] * b[2] - a[2] * b[1];
    c[1] = a[2] * b[0] - a[0] * b[2];
    c[2] = a[0] * b[1] - a[1] * b[0];
    return c;
  }*/

  /** Set value for image padding during transformation.
   * Not set in constructor because for most applications it will be 0.
   */
  public void setPadValue( int pad ) {
      padValue = (double)pad;
  }

  /** Set value for doOrients. Not set in constructor because for most applications it will be true.
   */
  public void setOrients( boolean doOr ) {
      doOrients = doOr;
  }

  public void checkUnits() {
    /**
     * Checks if units of measure from two images match.
     */
    int i;
    int lastSlice;
    try {
      unitsOfMeasureA = new int[nDims];
      unitsOfMeasureB = new int[nDims];
      for (i = 0; i < nDims; i++) {
          unitsOfMeasureA[i] = sourceImgA.getFileInfo(0).getUnitsOfMeasure()[i];
          unitsOfMeasureB[i] = sourceImgB.getFileInfo(0).getUnitsOfMeasure()[i];
      }
    }
    catch (OutOfMemoryError e) {
      System.gc();
      displayError("AlgorithmRegPatientPos.checkUnits() reports: Out of memory");
      setCompleted(false);
      stopped = true;
      return;
    }

    changeUnits = false;
    for (i = 0; i < nDims; i++) {
      if (unitsOfMeasureA[i] != unitsOfMeasureB[i]) {
        // Check if both are units of length
        if ( ( (unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS)
              || (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS)
              || (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS)
              || (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS)
              || (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS)
              || (unitsOfMeasureA[i] == FileInfoBase.METERS)
              || (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS)
              || (unitsOfMeasureA[i] == FileInfoBase.INCHES) ||
              (unitsOfMeasureA[i] == FileInfoBase.MILES))
            && ( (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS)
                || (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS)
                || (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS)
                || (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS)
                || (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS)
                || (unitsOfMeasureB[i] == FileInfoBase.METERS)
                || (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS)
                || (unitsOfMeasureB[i] == FileInfoBase.INCHES)
                || (unitsOfMeasureB[i] == FileInfoBase.MILES))) {
          changeUnits = true;
          // Change units of B into units of A
          if (unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 10.0 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e8 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e10 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e13 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e8 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e13 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.ANGSTROMS;
          } // if (unitsOfMeasureA[i] == FileInfoBase.ANGSTROMS)
          else if (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 0.1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e12 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e12 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.NANOMETERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.NANOMETERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 1.0e-4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e9 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MICROMETERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MICROMETERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 1.0e-7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 1.0e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e6 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MILLIMETERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MILLIMETERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 1.0e-8 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 1.0e-7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e-4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e-1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e2 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e5 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e5 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.CENTIMETERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.CENTIMETERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.METERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 1.0e-10 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 1.0e-9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e-2 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e-2 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093e3 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.METERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.METERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 1.0e-13 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 1.0e-12 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 1.0e-9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 1.0e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 1.0e-5 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 2.54e-5 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 1.6093 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.KILOMETERS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.KILOMETERS)
          else if (unitsOfMeasureA[i] == FileInfoBase.INCHES) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 3.937e-9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 3.937e-8 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 3.937e-5 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 3.937e-2 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 3.937e-1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 39.37 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 3.937e4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILES) {
              resB[i] = 63360.0 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.INCHES;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.INCHES)
          else if (unitsOfMeasureA[i] == FileInfoBase.MILES) {
            if (unitsOfMeasureB[i] == FileInfoBase.ANGSTROMS) {
              resB[i] = 6.214e-14 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.NANOMETERS) {
              resB[i] = 6.214e-13 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROMETERS) {
              resB[i] = 6.214e-10 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLIMETERS) {
              resB[i] = 6.214e-7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.CENTIMETERS) {
              resB[i] = 6.214e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.METERS) {
              resB[i] = 6.214e-4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.KILOMETERS) {
              resB[i] = 6.214e-1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.INCHES) {
              resB[i] = 1.57828e-5 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MILES;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MILES)
        } // if both unitsOfMeasure are length units
        else if ( ( (unitsOfMeasureA[i] == FileInfoBase.NANOSEC)
                   || (unitsOfMeasureA[i] == FileInfoBase.MICROSEC)
                   || (unitsOfMeasureA[i] == FileInfoBase.MILLISEC)
                   || (unitsOfMeasureA[i] == FileInfoBase.SECONDS)
                   || (unitsOfMeasureA[i] == FileInfoBase.MINUTES)
                   || (unitsOfMeasureA[i] == FileInfoBase.HOURS))
                 && ( (unitsOfMeasureB[i] == FileInfoBase.NANOSEC)
                     || (unitsOfMeasureB[i] == FileInfoBase.MICROSEC)
                     || (unitsOfMeasureB[i] == FileInfoBase.MILLISEC)
                     || (unitsOfMeasureB[i] == FileInfoBase.SECONDS)
                     || (unitsOfMeasureB[i] == FileInfoBase.MINUTES)
                     || (unitsOfMeasureB[i] == FileInfoBase.HOURS))) {
          changeUnits = true;
          if (unitsOfMeasureA[i] == FileInfoBase.NANOSEC) {
            if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
              resB[i] = 1.0e6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
              resB[i] = 1.0e9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
              resB[i] = 6.0e10 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
              resB[i] = 3.6e12 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.NANOSEC;
          } // if (unitsOfMeasureA[i] == FileInfoBase.NANOSEC)
          else if (unitsOfMeasureA[i] == FileInfoBase.MICROSEC) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
              resB[i] = 1.0e6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
              resB[i] = 6.0e7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
              resB[i] = 3.6e9 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MICROSEC;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MICROSEC)
          else if (unitsOfMeasureA[i] == FileInfoBase.MILLISEC) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
              resB[i] = 1.0e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
              resB[i] = 1.0e3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
              resB[i] = 6.0e4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
              resB[i] = 3.6e6 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MILLISEC;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MILLISEC)
          else if (unitsOfMeasureA[i] == FileInfoBase.SECONDS) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
              resB[i] = 1.0e-9 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
              resB[i] = 1.0e-6 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
              resB[i] = 1.0e-3 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
              resB[i] = 6.0e1 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
              resB[i] = 3.6e3 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.SECONDS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.SECONDS)
          else if (unitsOfMeasureA[i] == FileInfoBase.MINUTES) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
              resB[i] = 1.66666666e-11 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
              resB[i] = 1.66666666e-8 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
              resB[i] = 1.66666666e-5 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
              resB[i] = 1.66666666e-2 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.HOURS) {
              resB[i] = 60.0 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.MINUTES;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.MINUTES)
          else if (unitsOfMeasureA[i] == FileInfoBase.HOURS) {
            if (unitsOfMeasureB[i] == FileInfoBase.NANOSEC) {
              resB[i] = 2.77777777e-13 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MICROSEC) {
              resB[i] = 2.77777777e-10 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MILLISEC) {
              resB[i] = 2.77777777e-7 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.SECONDS) {
              resB[i] = 2.77777777e-4 * resB[i];
            }
            else if (unitsOfMeasureB[i] == FileInfoBase.MINUTES) {
              resB[i] = 1.66666666e-2 * resB[i];
            }
            unitsOfMeasureB[i] = FileInfoBase.HOURS;
          } // else if (unitsOfMeasureA[i] == FileInfoBase.HOURS)
        } // if both unitsOfMeasure are time units
      } // if (unitsOfMeasureA[i] != unitsOfMeasureB[i])
    } // for (int i=0; i<3; i++)
    // If units need to be changed, change them here.

    if (changeUnits) {
      float[] resolutionB = new float[nDims];
      for (i = 0; i < nDims; i++) {
          resolutionB[i] = (float) resB[i];
      }
      if (nDims == 3) {
          lastSlice = dimB[2];
      }
      else {
          lastSlice = 1;
      }
      for (i = 0; i < lastSlice; i++) {
        fileInfo = sourceImgB.getFileInfo()[i];
        fileInfo.setResolutions(resolutionB);
        fileInfo.setUnitsOfMeasure(unitsOfMeasureB);
        sourceImgB.setFileInfo(fileInfo, i);
      } // for (int i = 0; i < lastSlice; i++)
    } // if (changeUnits)
  }

}
