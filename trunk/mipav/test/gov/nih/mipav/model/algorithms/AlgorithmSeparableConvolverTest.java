package gov.nih.mipav.model.algorithms;

/**
 * Test case for AlgorithmSeparableConvolver.java
 * 
 * @author Hailong Wang, Ph.D
 * @version 0.1 04/04/2008
 */
import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.file.FileBase;
import junit.framework.Assert;
import junit.framework.TestCase;



public class AlgorithmSeparableConvolverTest extends TestCase {
    /**
     * The test 2d image with 512x512 dimension
     */
    public static final String testImageFileName = "test\\r_M00176_Anatomic_slice2.raw";
    
    /**
     * The x derivative image of the test image with the kernel of 1.00, 1.00
     */
    public static final String xDerivativeImageFileName = "test\\r_M00176_Anatomic_slice2_x_der_100_100.raw";
    
    /**
     * The y derivative image of the test image with the kernel of 1.00, 1.00
     */
    public static final String yDerivativeImageFileName = "test\\r_M00176_Anatomic_slice2_y_der_100_100.raw";

    private int imgLength;
    private AlgorithmSeparableConvolver sepConvolver;
    private float[] srcBuffer;
    private float[] refBufferXDer;
    private float[] refBufferYDer;
    private float[] refBufferZDer;
    private float[] testBuffer;
    private GaussianKernelFactory gkf;
    protected void setUp(){
        float[] sigmas = new float[] {1.0f, 1.0f};
        srcBuffer = FileBase.readRawFileFloat(testImageFileName, false);
        refBufferXDer = FileBase.readRawFileFloat(xDerivativeImageFileName, false);
        refBufferYDer = FileBase.readRawFileFloat(yDerivativeImageFileName, false);
//        refBufferZDer = FileBase.readRawFileFloat("test\\r_M00176_Anatomic_slice2_z_der.raw", false);
        imgLength = srcBuffer.length;
        gkf = GaussianKernelFactory.getInstance(sigmas);
    }
    public void testPerform() {
        gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
        Kernel xDerivativeKernel = gkf.createKernel();
        sepConvolver = new AlgorithmSeparableConvolver(srcBuffer, new int[]{512, 512}, xDerivativeKernel.getData(), false, false);;
        sepConvolver.run();
        testBuffer = sepConvolver.getOutputBuffer();
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBuffer[i], refBufferXDer[i]);
        }
        
        gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
        Kernel yDerivativeKernel = gkf.createKernel();
        sepConvolver = new AlgorithmSeparableConvolver(srcBuffer, new int[]{512, 512}, yDerivativeKernel.getData(), false, false);;
        sepConvolver.run();
        testBuffer = sepConvolver.getOutputBuffer();
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBuffer[i], refBufferYDer[i]);
        }

//        gkf.setKernelType(GaussianKernelFactory.Z_DERIVATIVE_KERNEL);
//        Kernel zDerivativeKernel = gkf.createKernel();
//        sepConvolver = new AlgorithmSeparableConvolver(testBuffer, srcBuffer, new int[]{512, 512}, zDerivativeKernel.getData(), false, false);;
//        sepConvolver.run();
////        FileBase.writeRawFileFloat("test\\r_M00176_Anatomic_slice2_z_der.raw", false, testBuffer);
//        for(int i = 0; i < imgLength; i++){
//            Assert.assertEquals(testBuffer[i], refBufferZDer[i]);
//        }
    }

}
