package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransMatrixd;

import gov.nih.mipav.util.TestingFileUtil;
import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;

public class AlgorithmPowellOpt3DTest extends TestCase {
    private static final double[][] initial4 = new double[][]{{-30.0, -30.0, -30.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0},
        {-30.0, -30.0, -15.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0},
        {-30.0, -30.0, 0.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0},
        {-30.0, -30.0, 15.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0},
        {-30.0, -30.0, 30.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0},
        {-30.0, -15.0, -30.0, 2.183237304872879, 3.079412488232716, -0.13668425991973976, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0}};
    private static final double[][] result4 = new double[][]{{0.9680426734657889, 2.8887031189635546, 3.7487743555311512, -0.7057110909274593},
        {0.9714825117529581, 2.334424588583379, 4.057825217004962, -0.2848749189156946},
        {0.9885536828288917, 2.04687751235868, 4.639763236834399, -1.6343197171273436},
        {1.0190843675714074, 1.0934692825627221, 4.849232764647468, -3.84843248803882},
        {1.0642552985378082, 0.5287293704964868, 4.912689233541724, -3.6413996897854095},
        {0.9426003903440052, 3.111665768422462, 3.8300172129573293, -0.6520777028750657}};
    
    

    /**
     * The original image file
     */
    private static final String refImageFileName = "images\\33175_3_concat_with_same_resolution.raw";
    
    /**
     * The transformed image file: translation (20, 20, 20), rotation (30, 30, 30) about origin  
     */
    private static final String matchImageFileName = "images\\33175_3_concat_with_same_resolution_20_20_20_30_30_30.raw";
    private ModelImage refImage;
    private ModelImage matchImage;
    private ModelSimpleImage refSimpleImage8;
    private ModelSimpleImage matchSimpleImage8;
    private ModelSimpleImage refSimpleImage4;
    private ModelSimpleImage matchSimpleImage4;
    private ModelSimpleImage refSimpleImage2;
    private ModelSimpleImage matchSimpleImage2;
    private ModelSimpleImage refSimpleImage;
    private ModelSimpleImage matchSimpleImage;
    private AlgorithmPowellOpt3D powell;
    private AlgorithmCostFunctions costFunc;
    private float[] resolution = {0.9375f, 0.9375f, 0.9375f};
    private int dof;
    private int maxIter = 4;
    //private int interpolation = 0;
    private Vector3f cog;
    
    private TransMatrixd toOrigin;
    private TransMatrixd fromOrigin;
    @Before
    protected void setUp() throws Exception {
        toOrigin = new TransMatrixd(4);
        toOrigin.Set(1.0f, 0.0f, 0.0f, 17.314208192569254f,
                     0.0f, 1.0f, 0.0f, 21.140626155237523f,
                     0.0f, 0.0f, 1.0f, 11.527512442322395f,
                     0.0f, 0.0f, 0.0f, 1.0f);
        fromOrigin = new TransMatrixd(toOrigin);
        fromOrigin.Set(0, 3, -toOrigin.Get(0,3));
        fromOrigin.Set(1, 3, -toOrigin.Get(1,3));
        fromOrigin.Set(2, 3, -toOrigin.Get(2,3));

        short[] data = TestingFileUtil.readRawFileShort(refImageFileName, false);
        refImage = new ModelImage(ModelStorageBase.SHORT, new int[]{256, 256, 198}, "33175_3_concat_with_same_resolution");
        refImage.importData(0, data, true);
        refImage.setResolutions(0, resolution);
        data = TestingFileUtil.readRawFileShort(matchImageFileName, false);
        matchImage = new ModelImage(ModelStorageBase.SHORT, new int[]{256, 256, 198}, "33175_3_concat_with_same_resolution_20_20_20_30_30_30");
        matchImage.importData(0, data, true);
        matchImage.setResolutions(0, resolution);
        
        refSimpleImage = new ModelSimpleImage(refImage.getExtents(), refImage.getFileInfo()[0].getResolutions(), refImage);
        matchSimpleImage = new ModelSimpleImage(matchImage.getExtents(), matchImage.getFileInfo()[0].getResolutions(), matchImage);
        refSimpleImage2 = refSimpleImage.subsample3dBy2(false);
        matchSimpleImage2 = matchSimpleImage.subsample3dBy2(false);
        refSimpleImage4 = refSimpleImage2.subsample3dBy2(false);
        matchSimpleImage4 = matchSimpleImage2.subsample3dBy2(false);
        refSimpleImage8 = refSimpleImage4.subsample3dBy2(false);
        matchSimpleImage8 = matchSimpleImage4.subsample3dBy2(false);
        cog = AlgorithmRegOAR3D.calculateCenterOfMass3D(matchSimpleImage8, (ModelSimpleImage)null, false);
        costFunc = new AlgorithmCostFunctions(refSimpleImage8, matchSimpleImage8, 1, 32, 1);
        dof = 4;
        powell = new AlgorithmPowellOpt3D(null, cog, dof, costFunc, getTolerance(dof, 256), maxIter);

    }

    public void testConvertToMatrix(){
        double[] point = {-30.0, -30.0, -30.0, 2.1839247649369415, 3.0813184180912216, -0.1371298864664574, 1.05, 1.05, 1.05, 0.0, 0.0, 0.0};
        TransMatrixd refMatrix = new TransMatrixd(4);
        refMatrix.Set(0.7875000000000002f, 0.45466333698683026f, -0.5249999999999999f, 2.303270404345806f, 
                      -0.22733166849341516f, 0.9187500000000002f, 0.45466333698683026f, 3.4939248560798895f, 
                      0.65625f, -0.22733166849341516f, 0.7875000000000002f, -4.2440488019808456f, 
                      0.0f, 0.0f, 0.0f, 1.0f);
        TransMatrixd testMatrix = powell.convertToMatrix(toOrigin, fromOrigin, point);
        // TODO does TransMatrix define a useful equals method?
        Assert.assertTrue(refMatrix.equals(testMatrix));
    }

    public void testOptimize() {
        /**
         * Test 4 degree of freedom and single threading powell's method
         */
        dof = 4;
        powell = new AlgorithmPowellOpt3D(null, cog, dof, costFunc, getTolerance(dof, 256), maxIter);
        for(int i = 0; i < initial4.length; i++){
            Vectornd v = new Vectornd(initial4[i], true);
            powell.optimize(v);
            double[] point = powell.extractPoint(v.getPoint());
            for(int j = 0; j < result4[i].length; j++){
                Assert.assertEquals(point[j], result4[i][j]);
            }
        }
    }

    public static double[] getTolerance(int DOF, int maxDim) {
        double[] tols = new double[DOF];
        int i;

        if (DOF == 3) {

            for (i = 0; i < 3; i++) {
                tols[i] = 0.5;
            } // translation
        } else if (DOF == 4) {
            tols[0] = 0.005; // global scaling

            for (i = 1; i < 4; i++) {
                tols[i] = 0.5;
            } // translation
        } else if (DOF >= 6) {

            for (i = 0; i < DOF; i++) {

                if ((i / 3) == 0) {
                    tols[i] = ((180. / Math.PI) / maxDim);
                } // rotation tolerances
                else if ((i / 3) == 1) { // translation tolerances
                    tols[i] = 0.5;
                } else if ((i / 3) == 2) { // scaling tolerances
                    tols[i] = 0.005;
                } else if ((i / 3) == 3) { // skewing tolerances
                    tols[i] = 0.001;
                }
            }
        }

        return tols;
    }

}
