package gov.nih.mipav.model.algorithms.registration;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.TestingFileUtil;

import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

public class AlgorithmRegOAR2DTest extends TestCase {
    private static final double[][] levelEightMinimas = new double[][] { {24.0, -3.048101954150095, 3.213989784315583, 0.9975384104257081, 0.9975384104257081, 0.0, 0.0} };;

    private static final double[][] levelEightOptMinimas = new double[][] { {25.984392846879377, -3.1527437360924235, 3.213989784315583, 0.9969396587669149, 0.9969396587669149, 0.0, 0.0} };

    private static final double[][] levelFourMinimas = new double[][] {{25.981874537315036, -6.305487472184847, 6.427979568631166, 0.9995063996809996, 0.9995063996809996, 0.0, 0.0},
        {25.98327349166584, -6.305487472184847, 6.427979568631166, 0.9995063996809996, 0.9995063996809996, 0.0, 0.0},
        {25.97968281464621, -6.305487472184847, 6.427979568631166, 0.9995063996809996, 0.9995063996809996, 0.0, 0.0},
        {25.970903490105133, -6.2868253871520094, 6.427979568631166, 0.9987254779004654, 0.9987254779004654, 0.0, 0.0},
        {25.96786355716043, -6.2868253871520094, 6.427979568631166, 0.9987254779004654, 0.9987254779004654, 0.0, 0.0},
        {25.98156691552056, -6.2868253871520094, 6.427979568631166, 0.9987254779004654, 0.9987254779004654, 0.0, 0.0},
        {22.65237238601483, -4.973411989984255, 6.261752035724767, 0.9860422581081923, 0.9860422581081923, 0.0, 0.0},
        {22.541229116273655, -4.917498169517612, 6.255905192086004, 0.984463054200308, 0.984463054200308, 0.0, 0.0},
        {24.89589971842676, -8.017523461449436, 8.24468334134367, 1.0355586207771232, 1.0355586207771232, 0.0, 0.0},
        {24.90588432507933, -8.07513474535741, 8.362541109374467, 1.03718191911983, 1.03718191911983, 0.0, 0.0},
        {22.079659665135768, -9.981557454732492, 14.252423861150326, 1.1309291072858303, 1.1309291072858303, 0.0, 0.0},
        {22.287400316731627, -10.017429884185498, 14.328167722563474, 1.1306565615073672, 1.1306565615073672, 0.0, 0.0},
        {20.36877421900837, -1.2214869425363657, 3.918902076919138, 0.9229763204640202, 0.9229763204640202, 0.0, 0.0},
        {20.102524951739277, -1.0940322570184597, 3.700915107127271, 0.9193062005352353, 0.9193062005352353, 0.0, 0.0}};

    private static final double[] levelTwoMinima = new double[] {26.020860298512456, -12.610974944369694, 12.75501013295583, 1.0000361756069127, 1.0000361756069127, -3.15963123304584E-4, -6.642174648060562E-4};

    private static final double[] levelOneMinima = new double[] {26.020860298512456, -25.10563215874716, 25.337382842456766, 1.0000361756069127, 1.0000361756069127, 1.2119486889885757E-4, -6.642174648060562E-4};

    /**
     * The original image file
     */
    private static final String refImageFileName = "images\\r_M00176_Anatomic_slice2.raw";
    
    /**
     * The transformed image file: translation (20, 20), rotation 26 about the center of mass  
     */
    private static final String matchImageFileName = "images\\r_M00176_Anatomic_slice2_20_20_26.raw";

    private AlgorithmRegOAR2D reg;
    private ModelImage refImage;
    private ModelImage matchImage;
    
    private ModelSimpleImage refImageLevelEight;
    private ModelSimpleImage matchImageLevelEight;
    
    private ModelSimpleImage refImageLevelFour;
    private ModelSimpleImage matchImageLevelFour;
    
    private ModelSimpleImage refImageLevelTwo;
    private ModelSimpleImage matchImageLevelTwo;
    
    private ModelSimpleImage refImageLevelOne;
    private ModelSimpleImage matchImageLevelOne;
    
    private int cost;
    private int dof;
    private int interpolation;
    private float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;
    private boolean doSubsample;
    private boolean doMultiThread;
    private int bracketBound;
    private int maxIterations;
    private int numMinima;
    
    private Vector<MatrixListItem>[] inputLevelFour;
    private Vector<MatrixListItem> inputLevelTwo;
    private MatrixListItem inputLevelOne;

    private float[] resolution = {2.5f, 2.5f};

    @Before
    protected void setUp() throws Exception {
        /**
         * Correlation ratio
         */
        cost = 1;
        dof = 6;
        /**
         * biliear interpolation.
         */
        interpolation = 1;
        rotateBeginZ = -30;
        rotateEndZ = 30;
        coarseRateZ = 15;
        fineRateZ = 6;
        
        doSubsample = true;
        doMultiThread = true;
        maxIterations = 2;
        numMinima = 3;
        
        float[] data = TestingFileUtil.readRawFileFloat(refImageFileName, false);
        refImage = new ModelImage(ModelStorageBase.FLOAT, new int[]{512, 512}, "r_M00176_Anatomic_slice2");
        refImage.importData(0, data, true);
        refImage.setResolutions(0, resolution);
        data = TestingFileUtil.readRawFileFloat(matchImageFileName, false);
        matchImage = new ModelImage(ModelStorageBase.FLOAT, new int[]{512, 512}, "r_M00176_Anatomic_slice2_20_20_26");
        matchImage.importData(0, data, true);
        matchImage.setResolutions(0, resolution);

        reg = new AlgorithmRegOAR2D(refImage, matchImage, cost, dof, interpolation, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, doSubsample, doMultiThread, maxIterations, numMinima);
        reg.setLevel1Factor(2.0f);
        reg.setLevel2Factor(2.0f);
        reg.setLevel4Factor(2.0f);
        reg.setMaxDim(512);
        refImageLevelOne = new ModelSimpleImage(refImage.getExtents(), refImage.getFileInfo(0).getResolutions(), refImage);
        matchImageLevelOne = new ModelSimpleImage(matchImage.getExtents(), matchImage.getFileInfo(0).getResolutions(), matchImage);
        refImageLevelTwo = refImageLevelOne.subSample2dBy2(false);
        matchImageLevelTwo = matchImageLevelOne.subSample2dBy2(false);
        refImageLevelFour = refImageLevelTwo.subSample2dBy2(false);
        matchImageLevelFour = matchImageLevelTwo.subSample2dBy2(false);
        refImageLevelEight = refImageLevelFour.subSample2dBy2(false);
        matchImageLevelEight = matchImageLevelFour.subSample2dBy2(false);
        inputLevelFour = new Vector[2];
        inputLevelFour[0] = new Vector<MatrixListItem>();
        TransMatrix tMatrix = null;
        for(int i = 0; i < levelEightMinimas.length; i++){
            MatrixListItem m = new MatrixListItem(Double.MAX_VALUE, tMatrix, levelEightMinimas[i]);
            inputLevelFour[0].add(m);
        }
        inputLevelFour[1] = new Vector<MatrixListItem>();
        for(int i = 0; i < levelEightOptMinimas.length; i++){
            MatrixListItem m = new MatrixListItem(Double.MAX_VALUE, tMatrix, levelEightOptMinimas[i]);
            inputLevelFour[1].add(m);
        }

        inputLevelTwo = new Vector<MatrixListItem>();
        for(int i = 0; i < levelFourMinimas.length; i++){
            MatrixListItem m = new MatrixListItem(Double.MAX_VALUE, tMatrix, levelFourMinimas[i]);
            inputLevelTwo.add(m);
        }
        inputLevelOne = new MatrixListItem(Double.MAX_VALUE, tMatrix, levelTwoMinima);
 
    }

    @Test
    public void testLevelEight() {
        reg.setMultiThreadingEnabled(true);
        Vector<MatrixListItem>[] minimas = reg.levelEight(refImageLevelEight, matchImageLevelEight);
        double[] point = minimas[0].get(0).initial;
        for(int i = 0; i < point.length; i++){
            Assert.assertEquals(point[i], levelEightMinimas[0][i]);
        }
        point = minimas[1].get(0).initial;
        for(int i = 0; i < point.length; i++){
            Assert.assertEquals(point[i], levelEightOptMinimas[0][i]);
        }
    }

    @Test
    public void testLevelFour() {
        reg.setMultiThreadingEnabled(false);
        Vector<MatrixListItem> minimas = reg.levelFour(refImageLevelFour, matchImageLevelFour, inputLevelFour[0], inputLevelFour[1]);
        
        for(int i = 0; i < minimas.size(); i++){
            double[] point = minimas.get(i).initial;
            for(int j = 0; j < point.length; j++){
                Assert.assertEquals(point[j], levelFourMinimas[i][j]);
            }
        }
    }

    @Test
    public void testLevelTwo() {
        reg.setMultiThreadingEnabled(true);
        MatrixListItem minima = reg.levelTwo(refImageLevelTwo, matchImageLevelTwo, inputLevelTwo);
        for(int i = 0; i < levelTwoMinima.length; i++){
            Assert.assertEquals(levelTwoMinima[i], minima.initial[i]);
        }
    }
    @Test
    public void testLevelOne(){
        reg.setMultiThreadingEnabled(true);
        MatrixListItem minima = reg.levelOne(refImageLevelOne, matchImageLevelOne, inputLevelOne);
        for(int i = 0; i < levelOneMinima.length; i++){
            Assert.assertEquals(levelOneMinima[i], minima.initial[i]);
        }

    }
}
