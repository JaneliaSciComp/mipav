package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.TestingFileUtil;

import java.util.Vector;

import junit.framework.Assert;
import junit.framework.TestCase;

public class AlgorithmRegOAR3DTest extends TestCase{
    /**
     * The original image file
     */
    private static final String refImageFileName = "images\\33175_3_concat_with_same_resolution.raw";
    
    /**
     * The transformed image file: translation (20, 20, 20), rotation (30, 30, 30) about origin  
     */
    private static final String matchImageFileName = "images\\33175_3_concat_with_same_resolution_20_20_20_30_30_30.raw";

    private static final double[][] levelEightMinima = {{18.0, 30.0, 12.0, 1.4434867473621933, 4.823719361146728, 0.20856171408275567, 0.9895530951408948, 0.9895530951408948, 0.9895530951408948, 0.0, 0.0, 0.0}};
    private static final double[][] levelEightOptMinima = {{17.024217614124804, 38.16824198608171, 15.457694197136354, 1.1689118633853433, 5.032017889211978, 0.07037082058473915, 0.9969916979084693, 0.9969916979084693, 0.9969916979084693, 0.0, 0.0, 0.0}};
    private static final double[][] levelFourMinima = {{16.048014816696508, 38.63652668710674, 16.142624502896318, 2.282815672128066, 10.159695467174647, -0.06335629254306226, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {16.287634900838754, 38.52438597540046, 15.962432438748094, 2.3378237267706865, 10.159695467174647, -0.012127080568940449, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {16.30666376870725, 38.52438597540046, 15.939755598277944, 2.3378237267706865, 10.159695467174647, -0.012127080568940449, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {16.304003288098418, 38.52438597540046, 15.962432438748094, 2.3378237267706865, 10.159695467174647, -0.012127080568940449, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {16.369370340052573, 38.534292252296815, 15.931198937668055, 2.3378237267706865, 10.159695467174647, -0.012127080568940449, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {15.440673748182824, 38.71818332906329, 16.358519149456278, 2.2690595326898344, 10.180763809061325, -0.15878438929649075, 0.999350335258078, 0.999350335258078, 0.999350335258078, 0.0, 0.0, 0.0},
        {16.76822412252333, 38.484373043234854, 15.741887596649336, 2.36632192089675, 10.194709581914799, 0.005162018292291216, 1.0000447581863803, 1.0000447581863803, 1.0000447581863803, 0.0, 0.0, 0.0},
        {12.76893282136443, 39.88580863586576, 17.244068197324797, 2.17432089651949, 9.755144301181403, -0.6086137093785475, 0.9932157071841604, 0.9932157071841604, 0.9932157071841604, 0.0, 0.0, 0.0},
        {17.224505684373256, 40.214093155633876, 17.484477440926945, 2.810226608035258, 11.704921604254707, -0.06456746889303436, 1.0248916371489738, 1.0248916371489738, 1.0248916371489738, 0.0, 0.0, 0.0},
        {15.912257131813327, 52.646079112692874, 20.201921487309438, 2.88680779537026, 17.057752663011858, -0.22127342077489298, 1.1525962975605801, 1.1525962975605801, 1.1525962975605801, 0.0, 0.0, 0.0},
        {27.308485126765767, 70.2206096316455, 14.126136026196498, 4.459922481483129, 5.451639903049167, -1.84128959614465, 0.9179234285897924, 0.9179234285897924, 0.9179234285897924, 0.0, 0.0, 0.0}};
    private static final double[] levelTwoMinima = {16.048014816696508, 38.61310205059656, 16.165147529038997, 4.65316891788926, 20.383057906592267, -0.1267125850861245, 0.9998846006697798, 0.9998846006697798, 0.9998846006697798, 5.206433828768211E-4, 0.0016438243991700528, -0.001355782466873768};
    private static final double[] levelOneMinima = {16.09360543303537, 38.61310205059656, 16.110888499244282, 19.78552451471642, 20.116579820336806, 20.03086187350004, 0.9998846006697798, 0.9993827488025857, 1.0006822058199183, 5.206433828768211E-4, 0.0016438243991700528, -0.001355782466873768};

    private AlgorithmRegOAR3D reg;
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
    private int interpolate;
    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;
    private boolean doSubsample;
    private boolean doMultiThread;
    private boolean fastMode;
    private boolean maxOfMinResol;
    private int maxIterations;
    private int numMinima;
    
    private Vector<MatrixListItem>[] inputLevelFour;
    private Vector<MatrixListItem> inputLevelTwo;
    private MatrixListItem inputLevelOne;

    private float[] resolution = {0.9375f, 0.9375f, 0.9375f};

    @SuppressWarnings("unchecked")
    protected void setUp() throws Exception {
        /**
         * correlation ratio
         */
        cost = 1;
        /**
         * 12 degrees of freedom
         */
        dof = 12;
        rotateBeginX = -30.0f;
        rotateEndX = 30.0f;
        coarseRateX = 15.0f;
        fineRateX = 6.0f;
        rotateBeginY = -30.0f;
        rotateEndY = 30.0f;
        coarseRateY = 15.0f;
        fineRateY = 6.0f;
        rotateBeginZ = -30.0f;
        rotateEndZ = 30.0f;
        coarseRateZ = 15.0f;
        fineRateZ = 6.0f;
        
        maxOfMinResol = true;
        doSubsample = true;
        doMultiThread = true;
        fastMode = false;
        maxIterations = 2;
        numMinima = 3;
        
        short[] data = TestingFileUtil.readRawFileShort(refImageFileName, false);
        refImage = new ModelImage(ModelStorageBase.SHORT, new int[]{256, 256, 198}, "33175_3_concat_with_same_resolution");
        refImage.importData(0, data, true);
        refImage.setResolutions(0, resolution);
        data = TestingFileUtil.readRawFileShort(matchImageFileName, false);
        matchImage = new ModelImage(ModelStorageBase.SHORT, new int[]{256, 256, 198}, "33175_3_concat_with_same_resolution_20_20_20_30_30_30");
        matchImage.importData(0, data, true);
        matchImage.setResolutions(0, resolution);

        reg = new AlgorithmRegOAR3D(refImage, matchImage, cost, dof, interpolate, rotateBeginX, rotateEndX,
                coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
                rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol,
                doSubsample, doMultiThread, fastMode, maxIterations, numMinima);
        reg.setLevel1FactorXY(2.0f);
        reg.setLevel1FactorZ(2.0f);
        reg.setLevel2FactorXY(2.0f);
        reg.setLevel2FactorZ(2.0f);
        reg.setLevel4FactorXY(2.0f);
        reg.setLevel4FactorZ(2.0f);
        refImageLevelOne = new ModelSimpleImage(refImage.getExtents(), refImage.getFileInfo(0).getResolutions(), refImage);
        matchImageLevelOne = new ModelSimpleImage(matchImage.getExtents(), matchImage.getFileInfo(0).getResolutions(), matchImage);
        refImageLevelTwo = refImageLevelOne.subsample3dBy2(false);
        matchImageLevelTwo = matchImageLevelOne.subsample3dBy2(false);
        refImageLevelFour = refImageLevelTwo.subsample3dBy2(false);
        matchImageLevelFour = matchImageLevelTwo.subsample3dBy2(false);
        refImageLevelEight = refImageLevelFour.subsample3dBy2(false);
        matchImageLevelEight = matchImageLevelFour.subsample3dBy2(false);
        inputLevelFour = new Vector[2];
        inputLevelFour[0] = new Vector<MatrixListItem>();
        TransMatrix tMatrix = null;
        for(int i = 0; i < levelEightMinima.length; i++){
            MatrixListItem m = new MatrixListItem(0, tMatrix, levelEightMinima[0]);
            inputLevelFour[0].add(m);
        }
        inputLevelFour[1] = new Vector<MatrixListItem>();
        for(int i = 0; i < levelEightOptMinima.length; i++){
            MatrixListItem m = new MatrixListItem(0, tMatrix, levelEightOptMinima[0]);
            inputLevelFour[1].add(m);
        }

        inputLevelTwo = new Vector<MatrixListItem>();
        for(int i = 0; i < levelFourMinima.length; i++){
            MatrixListItem m = new MatrixListItem(0, tMatrix, levelFourMinima[0]);
            inputLevelTwo.add(m);
        }
        inputLevelOne = new MatrixListItem(0, tMatrix, levelTwoMinima);
    }

    public void testLevelEight(){
        reg.setMultiThreadingEnabled(true);
        Vector<MatrixListItem>[] minima = reg.levelEight(refImageLevelEight, matchImageLevelEight, 0f, 100f);
        double[] point = minima[0].get(0).initial;
        for(int i = 0; i < point.length; i++){
            Assert.assertEquals(point[i], levelEightMinima[0][i]);
        }
        point = minima[1].get(0).initial;
        for(int i = 0; i < point.length; i++){
            Assert.assertEquals(point[i], levelEightOptMinima[0][i]);
        }
//        reg.setMultiThreadingEnabled(true);
//        minima = reg.levelEight(refImageLevelEight, matchImageLevelEight, 0f, 100f);
//        point = minima[0].get(0).initial;
//        for(int i = 0; i < point.length; i++){
//            Assert.assertEquals(point[i], levelEightMinima[0][i]);
//        }
//        point = minima[1].get(0).initial;
//        for(int i = 0; i < point.length; i++){
//            Assert.assertEquals(point[i], levelEightOptMinima[0][i]);
//        }
    }


    public void testLevelFour(){
        reg.setMultiThreadingEnabled(true);
        Vector<MatrixListItem> minima = reg.levelFour(refImageLevelFour, matchImageLevelFour, inputLevelFour[0], inputLevelFour[1], 0f, 100f);
        for(int i = 0; i < levelFourMinima.length; i++){
            MatrixListItem mli = minima.get(i);
            for(int j = 0; j < levelFourMinima[i].length; j++){
                Assert.assertEquals(levelFourMinima[i][j], mli.initial[j]);
            }
        }
//        reg.setMultiThreadingEnabled(true);
//        minima = reg.levelFour(refImageLevelFour, matchImageLevelFour, inputLevelFour[0], inputLevelFour[1], 0f, 100f);
//        for(int i = 0; i < levelFourMinima.length; i++){
//            MatrixListItem mli = minima.get(i);
//            for(int j = 0; j < levelFourMinima[i].length; j++){
//                Assert.assertEquals(levelFourMinima[i][j], mli.initial[j]);
//            }
//        }
    }

    public void testLevelTwo(){
        MatrixListItem minima = reg.levelTwo(refImageLevelTwo, matchImageLevelTwo, inputLevelTwo, 0f, 100f);
        for(int i = 0; i < levelTwoMinima.length; i++){
            Assert.assertEquals(levelTwoMinima[i], minima.initial[i]);
        }
    }


    public void testLevelOne(){
        MatrixListItem minima = reg.levelOne(refImageLevelOne, matchImageLevelOne, inputLevelOne, maxIterations, 0f, 100f);
        for(int i = 0; i < levelOneMinima.length; i++){
            Assert.assertEquals(levelOneMinima[i], minima.initial[i]);
        }
    }
}
