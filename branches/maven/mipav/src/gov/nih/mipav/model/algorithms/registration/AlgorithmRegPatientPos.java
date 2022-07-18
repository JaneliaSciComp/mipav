package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * Use origin and image orientations to align images based on patient position.
 *
 * @version  1.0 Sept 2000
 * @author   Delia McGarry
 * @author   Zohara Cohen
 */
public class AlgorithmRegPatientPos extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    String name;

    /** DOCUMENT ME! */
    private TransMatrix alignmentXfrm;

    /** DOCUMENT ME! */
    private int[] axisDirA, axisDirB;

    /** DOCUMENT ME! */
    private double[] coordA;

    /** DOCUMENT ME! */
    private double[] coordB;

    /** private double dimA[], resA[];. */
    private double[] dimB, resB;

    /** DOCUMENT ME! */
    private boolean doMatch;

    /** DOCUMENT ME! */
    private ModelImage imageA, imageB, orientedImgB, shiftedImgB, resultImg;

    /** DOCUMENT ME! */
    private int[] index2Axial, index2ImgA, sign2Axial, sign2ImgA;

    /** DOCUMENT ME! */
    private TransMatrix orientA, orientB, orientA_inv, orientB_inv;

    /** DOCUMENT ME! */
    private double[][] orientMatrixA, orientMatrixB;

    /** DOCUMENT ME! */
    private double[] xOrientA, yOrientA, zOrientA;

    /** DOCUMENT ME! */
    private double[] xOrientB, yOrientB, zOrientB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithm.
     *
     * @param  imageA   image A
     * @param  imageB   image B, to be registered to imageA
     * @param  doMatch  whether to match the image origins
     */
    public AlgorithmRegPatientPos(ModelImage imageA, ModelImage imageB, boolean doMatch) {
        super(null, imageA);
        this.imageA = imageA;
        this.imageB = imageB;
        this.doMatch = doMatch;

        try {
            axisDirA = new int[] { 1, 1, 1 };
            axisDirB = new int[] { 1, 1, 1 };
            dimB = new double[3];
            resB = new double[3];
            resB[0] = (double) imageB.getFileInfo(0).getResolutions()[0];
            resB[1] = (double) imageB.getFileInfo(0).getResolutions()[1];
            resB[2] = (double) imageB.getFileInfo(0).getResolutions()[2];
            dimB[0] = (double) imageB.getExtents()[0];
            dimB[1] = (double) imageB.getExtents()[1];
            dimB[2] = (double) imageB.getExtents()[2];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm reports: Out of memory");
            setCompleted(false);

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        orientA = null;
        orientA_inv = null;
        orientB = null;
        orientB_inv = null;
        super.finalize();
    }

    /**
     * Extract orientation info from file header.
     *
     * @param  xOrient  3D array of directional cosines in x
     * @param  yOrient  3D array of directional cosines in y
     * @param  image    image
     */
    public void getPatientOrientation(double[] xOrient, double[] yOrient, ModelImage image) {

        if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
            FileInfoDicom fileInfo = (FileInfoDicom) (image.getFileInfo(0));
            String orientation = (String) fileInfo.getTagTable().getValue("0020,0037");

            if (orientation == null) {
                MipavUtil.displayError("Patient Orientation string = null");

                return;
            }

            int index1, index2, index3, index4, index5;
            int notSet = -1;
            index1 = index2 = index3 = index4 = notSet = index5 = notSet;

            for (int i = 0; i < orientation.length(); i++) {

                if (orientation.charAt(i) == '\\') {

                    if (index1 == notSet) {
                        index1 = i;
                    } else if (index2 == notSet) {
                        index2 = i;
                    } else if (index3 == notSet) {
                        index3 = i;
                    } else if (index4 == notSet) {
                        index4 = i;
                    } else {
                        index5 = i;
                    }
                }
            }

            xOrient[0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
            xOrient[1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
            xOrient[2] = Double.valueOf(orientation.substring(index2 + 1, index3)).doubleValue();
            yOrient[0] = Double.valueOf(orientation.substring(index3 + 1, index4)).doubleValue();
            yOrient[1] = Double.valueOf(orientation.substring(index4 + 1, index5)).doubleValue();
            yOrient[2] = Double.valueOf(orientation.substring(index5 + 1)).doubleValue();
        } else {
            FileInfoBase fileInfo = (FileInfoBase) (image.getFileInfo(0));
            int[] iOrient = new int[2];
            double[][] dOrient = new double[2][3];
            
            for (int i = 0; i < 2; i++) {
                if (fileInfo.getAxisOrientation(i) == FileInfoBase.ORI_UNKNOWN_TYPE) {
                    MipavUtil.displayError(image.getImageName() + " has unknown orientation for axis = " + i + "\n");
                    setCompleted(false);
                    return;
                }
            }

            for (int i = 0; i < 2; i++) {
                iOrient[i] = fileInfo.getAxisOrientation(i);

                if (iOrient[i] >= 3) {
                    dOrient[i][0] = 0;
                } else if (iOrient[i] == 1) {
                    dOrient[i][0] = 1;
                } else {
                    dOrient[i][0] = -1;
                } // iOrient[i]=2

                if ((iOrient[i] <= 2) || (iOrient[i] >= 5)) {
                    dOrient[i][1] = 0;
                } else if (iOrient[i] == 4) {
                    dOrient[i][1] = 1;
                } else {
                    dOrient[i][1] = -1;
                } // iOrient[i]=3

                if (iOrient[i] <= 4) {
                    dOrient[i][2] = 0;
                } else if (iOrient[i] == 5) {
                    dOrient[i][2] = 1;
                } else {
                    dOrient[i][2] = -1;
                } // iOrient[i]=6
            }

            xOrient[0] = dOrient[0][0];
            xOrient[1] = dOrient[0][1];
            xOrient[2] = dOrient[0][2];
            yOrient[0] = dOrient[1][0];
            yOrient[1] = dOrient[1][1];
            yOrient[2] = dOrient[1][2];
        }
    }

    /**
     * Extracts the origin from image file info.
     *
     * @param  coord  3D array of patient position x,y,z *
     * @param  image  image
     */
    public void getPatientPosition(double[] coord, ModelImage image) {

        if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
            FileInfoDicom fileInfo = (FileInfoDicom) (image.getFileInfo(0));
            String orientation = (String) fileInfo.getTagTable().getValue("0020,0032");

            if (orientation == null) {
            	//BEN:  I guess DICOM doesn't necessarily have this tag...?
                return;
            }

            int index1 = -1, index2 = -1;

            for (int i = 0; i < orientation.length(); i++) {

                if (orientation.charAt(i) == '\\') {

                    if (index1 == -1) {
                        index1 = i;
                    } else {
                        index2 = i;
                    }
                }
            }

            coord[0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
            coord[1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
            coord[2] = Double.valueOf(orientation.substring(index2 + 1)).doubleValue();
        } else {
            FileInfoBase fileInfo = (FileInfoBase) (image.getFileInfo(0));
            coord[0] = (double) fileInfo.getOrigin(0);
            coord[1] = (double) fileInfo.getOrigin(1);
            coord[2] = (double) fileInfo.getOrigin(2);
        }
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  result image
     */
    public ModelImage getResultImage() {
        return resultImg;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        

        reOrient();

        if (doMatch) {
            matchOrigin();
        }

        finalize();
    }

    /**
     * Returns orgthogonal vector.
     *
     * @param   a  vector
     * @param   b  vector
     *
     * @return  vector orthogonal to a & b
     */
    double[] vectorCrossProduct(double[] a, double[] b) {
        double[] c = new double[3];

        // cross product c = axb = <a2b3-a3b2,a3b1-a1b3,a1b2-a2b1>
        c[0] = (a[1] * b[2]) - (a[2] * b[1]);
        c[1] = (a[2] * b[0]) - (a[0] * b[2]);
        c[2] = (a[0] * b[1]) - (a[1] * b[0]);

        return c;
    }
  
    /**
     * Get indices for reordering data from Image B to the Axial coordinate system (LPS) and to the coordinate system of
     * Image A.
     */
    private void getIndices() {
        double[][] orderB2A, orderB2Ax, orderAx2A;
        TransMatrix tmpTransMat;
        TransMatrix matAx2A;

        try {
            orderB2Ax = new double[4][4];
            orderAx2A = new double[4][4];
            orderB2A = new double[4][4];
            tmpTransMat = new TransMatrix(4);
            index2Axial = new int[] { 0, 1, 2 };
            index2ImgA = new int[] { 0, 1, 2 };
            sign2Axial = new int[] { 1, 1, 1 };
            sign2ImgA = new int[] { 1, 1, 1 };
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmRegPatientPos.getIndices() reports: Out of memory");
            setCompleted(false);

            return;
        }

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                orderB2Ax[i][j] = orientB_inv.get(i, j);
                orderAx2A[i][j] = orientA.get(i, j);
                
            }
        }
        matAx2A = new TransMatrix(orientA);

        for (int i = 0; i < 3; i++) {

            for (int j = 0; j < 3; j++) {

                if (orderB2Ax[i][j] != 0) {
                    index2Axial[i] = j;

                    if (orderB2Ax[i][j] < 0) {
                        sign2Axial[i] = -1;
                    }
                }
            }
        }
        //System.out.println("\n" +"Sign to axial: " +sign2Axial[0] +" " +sign2Axial[1] +" " +sign2Axial[2]);
        //System.out.println("Order to axial: " +index2Axial[0] +" " +index2Axial[1] +" " +index2Axial[2]);

        tmpTransMat.Copy(orientB_inv);
        
        tmpTransMat.mult(matAx2A);
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                orderB2A[i][j] = tmpTransMat.get(i, j);
                
            }
        }

        for (int i = 0; i < 3; i++) {

            for (int j = 0; j < 3; j++) {

                if (orderB2A[i][j] != 0) {
                    index2ImgA[i] = j;

                    if (orderB2A[i][j] < 0) {
                        sign2ImgA[i] = -1;
                    }
                }
            }
        }
        //System.out.println("Sign to ImgA: " +sign2ImgA[0] +" " +sign2ImgA[1] +" " +sign2ImgA[2]);
        //System.out.println("Order to ImgA: " +index2ImgA[0] +" " +index2ImgA[1] +" " +index2ImgA[2] +".\n");
    }

    /**
     * Get transformation matrix from Image B to Image A.
     */
    private void getTransform() {

        try {
            xOrientA = new double[] { 0, 0, 0 };
            yOrientA = new double[] { 0, 0, 0 };
            zOrientA = new double[] { 0, 0, 0 };
            orientA = new TransMatrix(4);
            orientA_inv = new TransMatrix(4);

            xOrientB = new double[] { 0, 0, 0 };
            yOrientB = new double[] { 0, 0, 0 };
            zOrientB = new double[] { 0, 0, 0 };
            orientB = new TransMatrix(4);
            orientB_inv = new TransMatrix(4);

            alignmentXfrm = new TransMatrix(4);
            coordB = new double[] { 0, 0, 0 };
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmRegPatientPos reports: Out of memory");
            setCompleted(false);

            return;
        }

        /* Find transformation matrix */
        // ImageA (reference image): Get image orientation
        getPatientOrientation(xOrientA, yOrientA, imageA);
        zOrientA = vectorCrossProduct(xOrientA, yOrientA);
        orientMatrixA = new double[][] {
                            { xOrientA[0], xOrientA[1], xOrientA[2], 0 },
                            { yOrientA[0], yOrientA[1], yOrientA[2], 0 },
                            { zOrientA[0], zOrientA[1], zOrientA[2], 0 },
                            { 0, 0, 0, 1 }
                        };

        Preferences.debug("A: patientOrientX = " + xOrientA[0] + ", " + xOrientA[1] + ", " + xOrientA[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("A: patientOrientY = " + yOrientA[0] + ", " + yOrientA[1] + ", " + yOrientA[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("A: patientOrientZ = " + zOrientA[0] + ", " + zOrientA[1] + ", " + zOrientA[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);

        // ImageB (moving image):  Get image orientation
        getPatientOrientation(xOrientB, yOrientB, imageB);
        zOrientB = vectorCrossProduct(xOrientB, yOrientB);
        orientMatrixB = new double[][] {
                            { xOrientB[0], xOrientB[1], xOrientB[2], 0 },
                            { yOrientB[0], yOrientB[1], yOrientB[2], 0 },
                            { zOrientB[0], zOrientB[1], zOrientB[2], 0 },
                            { 0, 0, 0, 1 }
                        };

        Preferences.debug("B: patientOrientX = " + xOrientB[0] + ", " + xOrientB[1] + ", " + xOrientB[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("B: patientOrientY = " + yOrientB[0] + ", " + yOrientB[1] + ", " + yOrientB[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("B: patientOrientZ = " + zOrientB[0] + ", " + zOrientB[1] + ", " + zOrientB[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);

        // Figure out the transformation to reorient Image B.
        alignmentXfrm.identity();
        orientA.copyMatrix(orientMatrixA);  
        orientA_inv.Copy(orientA);
        orientA_inv.Inverse();
        alignmentXfrm.mult(orientA);
        orientB.copyMatrix(orientMatrixB);
        orientB_inv.Copy(orientB);
        orientB_inv.Inverse();
        alignmentXfrm.mult(orientB_inv);

        Preferences.debug("Transformation Matrix = \n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug(alignmentXfrm.toString(),Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Add image margins to image B, or crop to match Image A.
     */
    private void matchOrigin() {
        double diffD[];
        int[] destExtentsA, destExtentsC;
        int[] diffI, add, cropAmt, boundX, boundY, boundZ;

        try {
            coordA = new double[] { 0, 0, 0 };
            diffD = new double[] { 0, 0, 0 };
            diffI = new int[] { 0, 0, 0 };
            destExtentsA = new int[] { 0, 0, 0 };
            destExtentsC = new int[] { 0, 0, 0 };
            add = new int[] { 0, 0, 0 };
            cropAmt = new int[] { 0, 0, 0 };
            boundX = new int[] { 0, 0 };
            boundY = new int[] { 0, 0 };
            boundZ = new int[] { 0, 0 };
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmRegPatientPos reports: Out of memory");
            setCompleted(false);

            return;
        }

        // Get image origins (i.e. start locations)
        getPatientPosition(coordA, imageA);
        Preferences.debug("ImageA startLocation in MatchOrigin: " + (float) coordA[0] + ", " + (float) coordA[1] +
                          ", " + (float) coordA[2] + "\n",Preferences.DEBUG_ALGORITHM);
        getPatientPosition(coordB, resultImg);
        Preferences.debug("ImageB startLocation in order in MatchOrigin: " + (float) coordB[0] + ", " +
                          (float) coordB[1] + ", " + (float) coordB[2] + "\n",Preferences.DEBUG_ALGORITHM);

        // If all the origin for Image A are 0.0, assume that start locations aren't known and shouldn't be used
        // in the registration.  Likewise for Image B.  Set the diff to zero.
        if (((coordA[0] == 0.0) && (coordA[1] == 0.0) && (coordA[2] == 0.0)) ||
                ((coordB[0] == 0.0) && (coordB[1] == 0.0) && (coordB[2] == 0.0))) {
            MipavUtil.displayInfo("Start locations for either Image A or Image B are all zeros.  The origin won't be used " +
                                  " in registration.");
        } else {

            for (int i = 0; i < 3; i++) {
                diffD[i] = coordB[i] - coordA[i];
            }

            // Call AlgorithmAddMargins.
            for (int i = 0; i < 3; i++) {
                diffI[i] = (int) (diffD[i] / resB[i]); // convert to pixels from mm

                if ((axisDirA[i] * diffI[i]) < 0) {
                    cropAmt[i] = (int) Math.abs(diffI[i]);
                    add[i] = 0;
                } else {
                    add[i] = (int) Math.abs(diffI[i]);
                    cropAmt[i] = 0;
                }

                destExtentsA[i] = resultImg.getExtents()[i] + add[i];
                destExtentsC[i] = destExtentsA[i] - cropAmt[i];
                if (destExtentsC[i] < 1) {
                    MipavUtil.displayError("Error! Required cropping would make destExtentsC[" + i + "] = " + destExtentsC[i]);
                    setCompleted(false);
                    return;
                }
                // System.out.println("Original dimension is " +orientedImgB.getExtents()[i] +", amount to add is "
                // +add[i] +", and destExtents[" +i +"] is " +destExtents[i] +".");
            }

            name = JDialogBase.makeImageName(imageB.getImageName(), "_shifted");
            shiftedImgB = new ModelImage(imageB.getType(), destExtentsA, imageB.getImageName());


            int[] marginX = new int[]{0,add[0]};
            int[] marginY = new int[]{0,add[1]};
            int[] marginZ = new int[]{0,add[2]};
            
            AlgorithmAddMargins algoMargins = new AlgorithmAddMargins(resultImg, shiftedImgB, 
                    marginX, marginY, marginZ );
            algoMargins.setPadValue( new float[]{0,0,0} );

            /* constructor looks like:
             * AlgorithmAddMargins(ModelImage srcImage, ModelImage destImage, double n,             int leftWidth, int
             * rightWidth, int height, int front, int back)
             */

            algoMargins.setRunningInSeparateThread(runningInSeparateThread);
            algoMargins.run();

            if (algoMargins.isCompleted() == false) {
                setCompleted(false);

                return;
            }

            boundX[0] = cropAmt[0];
            boundY[0] = cropAmt[1];
            boundZ[0] = cropAmt[2];
            boundX[1] = destExtentsA[0] - 1;
            boundY[1] = destExtentsA[1] - 1;
            boundZ[1] = destExtentsA[2] - 1;
            name = JDialogBase.makeImageName(imageB.getImageName(), "_aligned");
            resultImg = new ModelImage(imageB.getType(), destExtentsC, imageB.getImageName());

            AlgorithmCrop algoCrop = new AlgorithmCrop(resultImg, shiftedImgB, 0, boundX, boundY, boundZ);

            /* constructor looks like:
             * AlgorithmCrop(ModelImage destImg, ModelImage srcImg, int _cushion, int x[], int y[], int z[]) where x, y,
             * and z are int arrays with 2 elements representing lower and upper bounds.
             */
            algoCrop.setRunningInSeparateThread(runningInSeparateThread);
            algoCrop.run();

            if (algoCrop.isCompleted() == false) {
                setCompleted(false);

                return;
            }
        }

        setCompleted(true);
    }

    /**
     * Reorient ImageB so that directions will coincide with ImageA.
     */
    private void reOrient() {
        double[] fieldOfView;
        int switchOrig;
        int[] tempI3 = new int[3];
        double[] tempD1 = new double[3];
        float[] tempF3 = new float[3];

        try {
            fieldOfView = new double[] { 0, 0, 0 };
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmRegPatientPos reports: Out of memory");
            setCompleted(false);

            return;
        }

        /*
         * Call getTransform to get transformation matrix from orientation of ImageB to orientation of ImageA.
         * alignmentXfrm will be created and defined.
         */
        getTransform();

        /*
         * Create index arrays that store the reordering of Image B data (i.e. origin, resolution, dimension) to the
         * ordering of Image A. index2Axial, index2ImgA, sign2Axial, and sign2ImgA will be created.
         */
        getIndices();

        // Get image origin before transformation.
        getPatientPosition(coordB, imageB);
        Preferences.debug("Original ImageB startLocation: " + (float) coordB[0] + ", " + (float) coordB[1] + ", " +
                          (float) coordB[2] + "\n",Preferences.DEBUG_ALGORITHM);

        // Call AlgorithmTransform with padding.
        AlgorithmTransform algoTransform = new AlgorithmTransform(imageB, alignmentXfrm, AlgorithmTransform.TRILINEAR,
                                                                  (float) resB[index2ImgA[0]],
                                                                  (float) resB[index2ImgA[1]],
                                                                  (float) resB[index2ImgA[2]],
                                                                  (int) dimB[index2ImgA[0]], (int) dimB[index2ImgA[1]],
                                                                  (int) dimB[index2ImgA[2]], false, false, true);
        algoTransform.setRunningInSeparateThread(runningInSeparateThread);
        algoTransform.run();

        if (algoTransform.isCompleted() == false) {
            setCompleted(false);

            return;
        }

        orientedImgB = algoTransform.getTransformedImage(); // AlgorithmTransform creates new ModelImage destImage
        

        if (algoTransform != null) {
            algoTransform.finalize();
        }

        // Get updated resolutions and dimensions.
        for (int i = 0; i < 3; i++) {
            resB[i] = (double) orientedImgB.getFileInfo(0).getResolutions()[i];
            dimB[i] = (double) orientedImgB.getExtents()[i];
        }

        Preferences.debug("ImageB resolution in reOrient after transform: " + (float) resB[0] + ", " + (float) resB[1] +
                          ", " + (float) resB[2] + "\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ImageB dimensions in reOrient after transform: " + (int) dimB[0] + ", " + (int) dimB[1] +
                          ", " + (int) dimB[2] + "\n",Preferences.DEBUG_ALGORITHM);

        // Reorder origin.
        for (int i = 0; i < 3; i++) {
            tempD1[i] = coordB[index2ImgA[i]];
        }

        for (int i = 0; i < 3; i++) {
            coordB[i] = tempD1[i];
        }

        Preferences.debug("ImageB startLocation in A order: " + (float) coordB[0] + ", " + (float) coordB[1] + ", " +
                          (float) coordB[2] + "\n",Preferences.DEBUG_ALGORITHM);

        // When necessary, shift origin to opposite end of image.
        int[] oldDims = new int[3];
        oldDims = imageB.getExtents();

        float[] oldRes = new float[3];
        oldRes = imageB.getFileInfo(0).getResolutions();

        for (int i = 0; i < 3; i++) {
            fieldOfView[i] = oldRes[index2ImgA[i]] * (oldDims[index2ImgA[i]] - 1);
            Preferences.debug("Field of view in " + i + " direction is " + (float) fieldOfView[i] + ".\n",
            		Preferences.DEBUG_ALGORITHM);
        }

        /* Read the direction vector from MipavCoordinateSystems class: */
        axisDirA = MipavCoordinateSystems.getModelDirections(imageA);
        axisDirB = MipavCoordinateSystems.getModelDirections(imageB);

        for (int i = 0; i < 3; i++) {
            switchOrig = axisDirB[index2ImgA[i]] * axisDirA[i];

            if (switchOrig < 0) {
                coordB[i] = coordB[i] + (axisDirB[index2ImgA[i]] * fieldOfView[i]);
            }
        }

        Preferences.debug("New image origin: " + (float) coordB[0] + ", " + (float) coordB[1] + ", " +
                          (float) coordB[2] + "\n",Preferences.DEBUG_ALGORITHM);

        // Crop image on side opposite padding
        int[] cropAmt = new int[] { 0, 0, 0 };
        int[] boundX = new int[] { 0, 0 };
        int[] boundY = new int[] { 0, 0 };
        int[] boundZ = new int[] { 0, 0 };
        int[] newNewDims = new int[3];

        for (int i = 0; i < 3; i++) {

            if (dimB[i] > oldDims[index2ImgA[i]]) {
                cropAmt[i] = (int) dimB[i] - oldDims[index2ImgA[i]];
            }

            newNewDims[i] = oldDims[index2ImgA[i]];
        }

        boundX[1] = (int) dimB[0] - cropAmt[0] - 1;
        boundY[1] = (int) dimB[1] - cropAmt[1] - 1;
        boundZ[1] = (int) dimB[2] - cropAmt[2] - 1;

        Preferences.debug("Original image dimensions: " + oldDims[index2ImgA[0]] + " " + oldDims[index2ImgA[1]] + " " +
                          oldDims[index2ImgA[2]] + ".\n",Preferences.DEBUG_ALGORITHM);
        Preferences.debug("OrientedB dimensions: " + dimB[0] + " " + dimB[1] + " " + dimB[2] + ".\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Amount to crop " + cropAmt[0] + ", " + cropAmt[1] + ", and " + cropAmt[2] + " pixels.\n",
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("New orientedB (tempImg) dimensions: " + newNewDims[0] + " " + newNewDims[1] + " " +
                          newNewDims[2] + ".\n",Preferences.DEBUG_ALGORITHM);

        name = JDialogBase.makeImageName(imageB.getImageName(), "_cropped");

        ModelImage tempImg = new ModelImage(imageB.getType(), newNewDims, imageB.getImageName());
        AlgorithmCrop algoCrop = new AlgorithmCrop(tempImg, orientedImgB, 0, boundX, boundY, boundZ);
        algoCrop.setRunningInSeparateThread(runningInSeparateThread);
        algoCrop.run();

        if (algoCrop.isCompleted() == false) {
            setCompleted(false);

            return;
        }

        orientedImgB.disposeLocal();
        orientedImgB = tempImg;

        for (int i = 0; i < 3; i++) {
            dimB[i] = (double) orientedImgB.getExtents()[i];
        }

        Preferences.debug("After crop, dimB = " + dimB[0] + ", " + dimB[1] + ", " + dimB[2] + "\n",
        		Preferences.DEBUG_ALGORITHM);

        // Set file properties of result image.
        FileInfoBase fileInfoB, fileInfoA;
        float slicePos = (float) coordB[2];
        fileInfoA = imageA.getFileInfo(0);

        for (int i = 0; i < 3; i++) {
            tempI3[i] = (int) dimB[i];
        }

        for (int i = 0; i < 3; i++) {
            tempF3[i] = (float) resB[i];
        }

        for (int i = 0; i < orientedImgB.getExtents()[2]; i++) {
            fileInfoB = orientedImgB.getFileInfo(i);
            fileInfoB.setResolutions(tempF3);
            fileInfoB.setAxisOrientation(fileInfoA.getAxisOrientation());
            fileInfoB.setImageOrientation(fileInfoA.getImageOrientation());
            fileInfoB.setOrigin((float) coordB[0], 0);
            fileInfoB.setOrigin((float) coordB[1], 1);
            fileInfoB.setOrigin(slicePos, 2);
            slicePos += (float) (axisDirA[2] * resB[2]);
        }

        // clean up this method
        alignmentXfrm = null;

        resultImg = orientedImgB;
        tempImg = null;
        setCompleted(true);
    }
}
