package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.util.*;

public class AlgorithmIsophoteCurvature extends AlgorithmBase implements AlgorithmInterface {
    
    private static final int xOp = 1;
    
    private static final int yOp = 2;
    
    private static final int zOp = 3;
    
    private static final int xxOp = 4;
    
    private static final int xyOp = 5;
    
    private static final int yyOp = 6;
    
    /** Storage location of the first derivative of the Gaussian in the X direction. */
    private float[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;

    /** Storage location of the first derivative of the Gaussian in the Z direction. */
    private float[] GzData;
    
    private float[] GxxData;
    
    private float[] GxyData;
    
    private float[] GyyData;
    
    /** Dimensionality of the kernel. */
    private int[] kExtents;
    
    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
    
    private boolean entireImage = true;
    
    /** Stores output of AlgorithmConvolver */
    private float[] oX = null;
    private float[] oY = null;
    private float[] oZ = null;
    private float[] oXX = null;
    private float[] oXY = null;
    private float[] oYY = null;
    
    private int operationType = xOp;
    
    public AlgorithmIsophoteCurvature(ModelImage srcImg, float[] sigmas, boolean entireImage, boolean image25D) {
        this(null, srcImg, sigmas, entireImage, image25D);
    }
    
    public AlgorithmIsophoteCurvature(ModelImage destImg, ModelImage srcImg, float[] sigmas, 
                                      boolean entireImage, boolean image25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        this.entireImage = entireImage;
        this.image25D = image25D;
        
        if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
    }
    
    public void cleanUp() {
        GxData = null;
        GyData = null;
        GzData = null;
        kExtents = null;
        sigmas = null;
        srcImage = null;
    }

    /**
     * finalize - sets class storages arrays to null so that System.gc() can free the memory.
     */
    public void finalize() {
        cleanUp();
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        fireProgressStateChanged(0, null, "Calculating the Isophote Curvature ...");
        
        if ((srcImage.getNDims() == 2) || image25D) {
            makeKernels2D();
            calc25D();
        } else {
            makeKernels3D();
            calc3D();
        }

    }
        
        
     private void calc25D() {
         AlgorithmConvolver convolver;
         int zDim;
         int i;
         int length = srcImage.getSliceSize();
         double buffer[] = null;
         double curvature[];
         int totalLength;
         double num;
         double denom;
         double oxSq;
         double oySq;
         double typeMin;
         double typeMax;
         double a;
         double resultMin;
         double resultMax;
         ModelImage resultImage = null;
         
         
         if (srcImage.getNDims() == 2) {
             zDim = 1;
         }
         else {
             zDim = srcImage.getExtents()[2];
         }
         totalLength = length * zDim;
         
         curvature = new double[totalLength];
         if (!entireImage) {
             buffer = new double[totalLength];
             try {
                 srcImage.exportData(0, totalLength, buffer);
             }
             catch (IOException e) {
                 errorCleanUp("IOException " + e + " on srcImage.exportData(0, totalLength, buffer)", true); 
                 setCompleted(false);
                 return;
             }
         }
         
         convolver = new AlgorithmConvolver(srcImage, GxData, kExtents, entireImage, image25D);
         convolver.setMinProgressValue(0);
         convolver.setMaxProgressValue(15);
         linkProgressToAlgorithm(convolver);
         convolver.addListener(this);
         if (!entireImage) {
             convolver.setMask(mask);
         }
         operationType = xOp;
         convolver.run();
         
         convolver = new AlgorithmConvolver(srcImage, GyData, kExtents, entireImage, image25D);
         convolver.setMinProgressValue(16);
         convolver.setMaxProgressValue(30);
         linkProgressToAlgorithm(convolver);
         convolver.addListener(this);
         if (!entireImage) {
             convolver.setMask(mask);
         }
         operationType = yOp;
         convolver.run();
         
         convolver = new AlgorithmConvolver(srcImage, GxxData, kExtents, entireImage, image25D);
         convolver.setMinProgressValue(31);
         convolver.setMaxProgressValue(45);
         linkProgressToAlgorithm(convolver);
         convolver.addListener(this);
         if (!entireImage) {
             convolver.setMask(mask);
         }
         operationType = xxOp;
         convolver.run();
         
         convolver = new AlgorithmConvolver(srcImage, GxyData, kExtents, entireImage, image25D);
         convolver.setMinProgressValue(46);
         convolver.setMaxProgressValue(60);
         linkProgressToAlgorithm(convolver);
         convolver.addListener(this);
         if (!entireImage) {
             convolver.setMask(mask);
         }
         operationType = xyOp;
         convolver.run();
         
         convolver = new AlgorithmConvolver(srcImage, GyyData, kExtents, entireImage, image25D);
         convolver.setMinProgressValue(61);
         convolver.setMaxProgressValue(75);
         linkProgressToAlgorithm(convolver);
         convolver.addListener(this);
         if (!entireImage) {
             convolver.setMask(mask);
         }
         operationType = yyOp;
         convolver.run();
         
         resultMin = Double.MAX_VALUE;
         resultMax = -Double.MAX_VALUE;
         for (i = 0; i < totalLength; i++) {
             if (entireImage || mask.get(i)) {
                 if ((oX[i] != 0.0) || (oY[i] != 0.0)) {
                     oxSq = oX[i] * oX[i];
                     oySq = oY[i] * oY[i];
                     num = 2.0 * oX[i] * oY[i] * oXY[i] - oxSq * oYY[i] - oySq * oXX[i];
                     denom = Math.pow((oxSq + oySq), 1.5);
                     curvature[i] = num/denom;
                 }
                 else {
                     curvature[i] = 0.0;
                 }
             }
             else {
                 curvature[i] = buffer[i];
             }
             if (curvature[i] < resultMin) {
                 resultMin = curvature[i];
             }
             if (curvature[i] > resultMax) {
                 resultMax = curvature[i];
             }
         }
         
         if (destImage == null) {
             resultImage = srcImage;
         }
         else {
             resultImage = destImage;
         }
         
         switch(resultImage.getType()) {
             case ModelStorageBase.BOOLEAN:
                 typeMin = 0;
                 typeMax = 1;
                 break;
             case ModelStorageBase.BYTE:
                 typeMin = -128;
                 typeMax = 127;
                 break;
             case ModelStorageBase.UBYTE:
                 typeMin = 0;
                 typeMax = 255;
                 break;
             case ModelStorageBase.SHORT:
                 typeMin = -32768;
                 typeMax = 32767;
                 break;
             case ModelStorageBase.USHORT:
                 typeMin = 0;
                 typeMax = 65535;
                 break;
             case ModelStorageBase.INTEGER:
                 typeMin = Integer.MIN_VALUE;
                 typeMax = Integer.MAX_VALUE;
                 break;
             case ModelStorageBase.UINTEGER:
                 typeMin = 0;
                 typeMax = 4294967295L;
                 break;
             case ModelStorageBase.LONG:
                 typeMin = Long.MIN_VALUE;
                 typeMax = Long.MAX_VALUE;
                 break;
             case ModelStorageBase.FLOAT:
                 typeMin = -Float.MAX_VALUE;
                 typeMax = Float.MAX_VALUE;
                 break;
             case ModelStorageBase.DOUBLE:
                 typeMin = -Double.MAX_VALUE;
                 typeMax = Double.MAX_VALUE;
                 break;
             default:
                 typeMin = -Double.MAX_VALUE;
                 typeMax = Double.MAX_VALUE;
         }
         
         if ((resultMin < typeMin) || (resultMax > typeMax)) {
             // Don't shift offset
             if (resultMin >= 0) {
                 a = typeMax/resultMax;
             }
             else {
                 a = Math.max(typeMax/resultMax, typeMin/resultMin);
             }
             Preferences.debug("Rescaling to " + a + " * curvature + " + "\n", Preferences.DEBUG_ALGORITHM);
             for (i = 0; i < totalLength; i++) {
                 curvature[i] = a * curvature[i];
             }
         }

         
         if (destImage != null) {
             try {
                 destImage.importData(0, curvature, true);
             }
             catch (IOException e) {
                 errorCleanUp("IOException " + e + " on destImage.importData(0, curvature, true)", true); 
                 setCompleted(false);
                 return;    
             }
         } 
         else {
             try {
                 srcImage.importData(0, curvature, true);
             }
             catch (IOException e) {
                 errorCleanUp("IOException " + e + " on srcImage.importData(0, curvature, true)", true); 
                 setCompleted(false);
                 return;        
             }
         }
         
         setCompleted(true);
         return;
         
     }
     
     private void calc3D() {
         
     }
    
    /**
     * makeKernals2D - creates the derivative kernels used to calculate the gradient magnitude.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        derivOrder[0] = 1;
        derivOrder[1] = 0;
        GxData = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.calc(true);
        
        derivOrder[0] = 2;
        derivOrder[1] = 0;
        GxxData = new float[xkDim * ykDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);
        Gxx.calc(false);
        
        derivOrder[0] = 1;
        derivOrder[1] = 1;
        GxyData = new float[xkDim * ykDim];

        GenerateGaussian Gxy = new GenerateGaussian(GxyData, kExtents, sigmas, derivOrder);
        Gxy.calc(false);
        
        derivOrder[0] = 0;
        derivOrder[1] = 2;
        GyyData = new float[xkDim * ykDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);
        Gyy.calc(false);
    }
    
    /**
     * makeKernals3D - creates the derivative kernels used to calculate the gradient magnitude.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(5 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;

        GxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.calc(true);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);

        Gz.calc(true);
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            if (operationType == xOp) {
                oX = convolver.getOutputBuffer();
            }
            else if (operationType == yOp) {
                oY = convolver.getOutputBuffer();
            }
            else if (operationType == zOp) {
                oZ = convolver.getOutputBuffer();
            }
            else if (operationType == xxOp) {
                oXX = convolver.getOutputBuffer();
            }
            else if (operationType == xyOp) {
                oXY = convolver.getOutputBuffer();
            }
            else if (operationType == yyOp) {
                oYY = convolver.getOutputBuffer();
            }
        }
    }
}