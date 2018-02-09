package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.TestingFileUtil;
import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import WildMagic.LibFoundation.Mathematics.GMatrixd;


public class AlgorithmDWI2DTITest extends TestCase{
    private final String MASK_IMAGE_FILENAME = "images\\dti_data\\904300_BOS_20050629_minc_dicom_proc\\904300_BOS_20050629_minc_dicom_MASK.raw";
    private final String PATH_FILENAME = "images\\dti_data\\904300_BOS_20050629_minc_dicom_proc\\904300_BOS_20050629_minc_dicom.path";
    private final String BMATRIX_FILENAME = "images\\dti_data\\904300_BOS_20050629_minc_dicom_proc\\904300_BOS_20050629_minc_dicom.BMTXT";
    private final String DTI_IMAGE_FILENAME = "images\\dti_data\\904300_BOS_20050629_minc_dicom_proc\\904300_BOS_20050629_minc_dicom.dti";
    
    private final int[] matrixEntries = new int[]{0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 7, 8, 9, 10, 11, 12, 0, 7, 8, 9, 10, 11, 12};
    private String[][] dwiFileNameList = null;
    private AlgorithmDWI2DTI dti;
    private ModelImage maskImage;
    private int nslices;
    private int nweights;
    private int xDim;
    private int yDim;
    private GMatrixd BMatrix;
    private float[] dtiData;
    /**
     * The number of different B matrix rows.
     */
    private int nrows;
    @Before
    protected void setUp() throws Exception {
        nslices = 51;
        xDim = 256;
        yDim = 256;
        nweights = 42;
        nrows = 13;
        maskImage = new ModelImage(ModelStorageBase.SHORT, new int[]{xDim, yDim, nslices}, "904300_BOS_20050629_minc_dicom_MASK");
        short[] maskImageData = TestingFileUtil.readRawFileShort(MASK_IMAGE_FILENAME, false);
        maskImage.importData(0, maskImageData, true);
        dwiFileNameList = TestingFileUtil.readPathFile(PATH_FILENAME, nslices, nweights);

        BMatrix = TestingFileUtil.readDTIBMatrixFile(BMATRIX_FILENAME, nweights, matrixEntries);
        dti = new AlgorithmDWI2DTI(maskImage, false, nslices, xDim, yDim, nrows, nweights, 0.0f, 
                dwiFileNameList,  matrixEntries, BMatrix, "dicom");
        dtiData = TestingFileUtil.readRawFileFloat(DTI_IMAGE_FILENAME, false);
    }

    @Test
    public void testCreateDWIImage() {
        float[] dtiImage = dti.createDWIImage();
        for(int i = 0; i < dtiImage.length; i++){
            Assert.assertEquals(dtiData[i], dtiImage[i]);
        }
    }

}
