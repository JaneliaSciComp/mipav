package gov.nih.mipav.model.algorithms.filters;

/**
 * This class is used to create test cases to test AlgorithmGradientMagnitudeSep.java
 * 
 * @author Hailong Wang, Ph.D
 * @version 0.1, 04/04/2008
 */
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.TestingFileUtil;
import junit.framework.Assert;
import junit.framework.TestCase;

public class AlgorithmGradientMagnitudeSepTest extends TestCase {
    /**
     * This is the test 2d image with 512x512 dimension 
     */
    public static String testImageFileName = "images\\r_M00176_Anatomic_slice2.raw";
    
    /**
     * The gradient magnitude image with the kernel parameters of 1.75, 1.75 for above test image
     */
    public static String gmagImageFileName = "images\\r_M00176_Anatomic_slice2_gmag_175_175.raw";
    
    /**
     * The normalized x directions of x derivative image.
     */
    public static String xDerivativeDirectionImageFileName = "images\\r_M00176_Anatomic_slice2_x_dir_175_175.raw";
    
    /**
     * The normalized y directions of y derivative image.
     */
    public static String yDerivativeDirectionImageFileName = "images\\r_M00176_Anatomic_slice2_y_dir_175_175.raw";
    
    private ModelImage srcImage;
    private int imgLength;
    private AlgorithmGradientMagnitudeSep gmagSep;
    private float[] imageBuffer;
    private float[] refBufferMag;
    private float[] refBufferXDir;
    private float[] refBufferYDir;

    private float[] testBufferMag;
    private float[] testBufferXDir;
    private float[] testBufferYDir;
    
    protected void setUp() throws Exception{
        srcImage = new ModelImage(ModelStorageBase.FLOAT, new int[]{512, 512}, "");
        float[] sigmas = new float[] {1.75f, 1.75f};
        imageBuffer = TestingFileUtil.readRawFileFloat(testImageFileName, false);
        srcImage.importData(0, imageBuffer, true);
        refBufferMag = TestingFileUtil.readRawFileFloat(gmagImageFileName, false);
        refBufferXDir = TestingFileUtil.readRawFileFloat(xDerivativeDirectionImageFileName, false);
        refBufferYDir = TestingFileUtil.readRawFileFloat(yDerivativeDirectionImageFileName, false);
        
        imgLength = imageBuffer.length;
        gmagSep = new AlgorithmGradientMagnitudeSep(srcImage, sigmas, true, false);
    }

    public void testExecute() {
        gmagSep.setDirectionNeeded(true);
        gmagSep.setNormalized(true);
        
        /**
         * Single-threading gradient magnitude algorithm
         */
        gmagSep.setMultiThreadingEnabled(false);
        gmagSep.execute();
        testBufferMag = gmagSep.getResultBuffer();
        testBufferXDir = gmagSep.getXDerivativeDirections();
        testBufferYDir = gmagSep.getYDerivativeDirections();
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferMag[i], refBufferMag[i]);
         }
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferXDir[i], refBufferXDir[i]);
         }
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferYDir[i], refBufferYDir[i]);
         }
        
        /**
         * Multi-threading gradient magnitude algorithm
         */
        gmagSep.setMultiThreadingEnabled(true);
        gmagSep.execute();
        testBufferMag = gmagSep.getResultBuffer();
        testBufferXDir = gmagSep.getXDerivativeDirections();
        testBufferYDir = gmagSep.getYDerivativeDirections();
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferMag[i], refBufferMag[i]);
         }
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferXDir[i], refBufferXDir[i]);
         }
        for(int i = 0; i < imgLength; i++){
            Assert.assertEquals(testBufferYDir[i], refBufferYDir[i]);
         }
    }

}
