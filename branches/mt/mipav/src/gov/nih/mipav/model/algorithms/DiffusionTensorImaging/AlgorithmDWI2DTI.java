package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.*;
import java.awt.*;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.view.dialogs.JDialogBrainSurfaceExtractor;


/**
 * Algorithm calculates a Diffusion Tensor Image from a series of Diffusion Wieghted Images.
 * 
 * See: Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDWI2DTI extends AlgorithmBase
    implements ViewImageUpdateInterface
{
    /** Mask Image for masking brain regions during tensor calculation: */
    private ModelImage m_kMaskImage = null;
    /** B0 weighted image: */
    private ModelImage m_kB0Image = null;
    /** ViewJFrameImage for displaying the B0 weighted image: */
    private ViewJFrameImage m_kB0Frame = null;
    /** Whether to just display the B0 weighted image and return, or to do a full calculation: */
    private boolean m_bDisplayB0 = false;
    /** Number of slices in the input images: */
    private int m_iSlices;
    /** X Dimension of the input images: */
    private int m_iDimX;
    /** Y Dimension of the input images: */
    private int m_iDimY;
    /** Number of different entries in the BMatrix: */
    private int m_iBOrig;
    /** Number of images in the weighted series: */
    private int m_iWeights;
    /** The mean noise value: */
    private float m_fMeanNoise;
    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;
    /** List of file names for the Diffusion Weighted Images, from the .path file. */
    private String[][] m_aakDWIList = null;
    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;
    /** Format of the raw data: (float, int, dicom, etc.) */
    private String m_kRawImageFormat = null;
    /** Output DTI Image:*/
    private ModelImage m_kDTIImage = null;


    /** Create a new AlgorithmDWI2DTI 
     * @param kMaskImage, mask image masking non-brain regions, if null it is calculated.
     * @param bDisplayB0, when true open and display the B0 weighted image and return.
     * @param iSlices, number of slices in the raw data.
     * @param iDimX, x-dimensions of the raw data.
     * @param iDimY, y-dimensions of the raw data.
     * @param iBOrig, number of different entries in the BMatrix.
     * @param iWeights, number of weighted series.
     * @param fMeanNoise, mean noise value.
     * @param aakDWIList, list of file names in the weighted series.
     * @param repidx,
     * @param kBMatrix, BMatrix values.
     * @param kRawFormat, format string for the raw data.
     */
    public AlgorithmDWI2DTI( ModelImage kMaskImage, boolean bDisplayB0, int iSlices, int iDimX, int iDimY,
                             int iBOrig, int iWeights, float fMeanNoise, String[][] aakDWIList, int[] aiMatrixEntries, GMatrixf kBMatrix,
                             String kRawFormat )
    {
        m_kMaskImage = kMaskImage;
        m_iSlices = iSlices;
        m_iDimX = iDimX;
        m_iDimY = iDimY;
        m_iBOrig = iBOrig;
        m_iWeights = iWeights;
        m_fMeanNoise = fMeanNoise;
        m_aakDWIList = aakDWIList;
        m_aiMatrixEntries = aiMatrixEntries;
        m_kBMatrix = kBMatrix;
        m_kRawImageFormat = kRawFormat;
        
        m_bDisplayB0 = bDisplayB0;
    }

    public void disposeLocal()
    {
        m_kMaskImage = null;
        m_kB0Image = null;
        m_kBMatrix = null;
        m_aakDWIList = null;
        m_aiMatrixEntries = null;
        m_kRawImageFormat = null;
        m_kDTIImage = null;
    }

    /** Calculate the DTI image. If the mask image is null, calculate the mask image first. */
    public void runAlgorithm()
    {
        if ( m_kMaskImage == null )
        {
            createMaskImage();
        }
        else
        {
            createDWIImage();
        }
    }

    /** Return the DTI Image. 
     * @return the DTI Image. 
     */
    public ModelImage getDTIImage()
    {
        return m_kDTIImage;
    }

    /** Get the slice data for the image with the given slice and weight.
     * @param iSlice, slice to read.
     * @param iWeight, weight to read.
     * @return float[] containing the data.
     */
    private float[] readDicomWeight( int iSlice, int iWeight )
    {
        int length = m_iDimX * m_iDimY;
        float[] buffer = new float[length];

        FileDicom fileDicom = null;
        FileInfoDicom refFileInfo = null;
        FileInfoBase fileInfo = null;
        
        String kPath = m_aakDWIList[ iSlice ][iWeight];
        String kDir = kPath.substring(0,kPath.lastIndexOf(File.separator)) + File.separator;
        String kFileName = kPath.substring(kPath.lastIndexOf(File.separator) + 1, kPath.length());
        
        try {
            fileDicom = new FileDicom(kFileName, kDir);
            fileDicom.setQuiet(true);
            fileDicom.readHeader(true);
            refFileInfo = (FileInfoDicom) fileDicom.getFileInfo();
            fileInfo = fileDicom.getFileInfo();
            fileDicom.readImage(buffer, ModelStorageBase.SHORT, 0);
        } catch ( IOException e ) {
            buffer = null;
            fileDicom = null;
        }
        fileDicom = null;
        return buffer;
    }

    /** Get the slice data for the image with the given slice and weight.
     * @param iSlice, slice to read.
     * @param iWeight, weight to read.
     * @return float[] containing the data.
     */
    private float[] readFloatWeight( int iSlice, int iWeight )
    {
        File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
        if ( !kFile.exists() || !kFile.canRead() )
        {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        int iLength = (int)kFile.length();
        if ( iLength <= 0 )
        {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        byte[] abSliceData = new byte[iLength];
        try {
            FileInputStream kFileReader = new FileInputStream(kFile);
            kFileReader.read(abSliceData,0,iLength);
            kFileReader.close();
            kFile = null;
        } catch (IOException e ) {}

        int length = m_iDimX * m_iDimY;
        float[] afResult = new float[length];
        for ( int iY = 0; iY < m_iDimY; iY++ )
        {
            for ( int iX = 0; iX < m_iDimX; iX++ )
            {
                int iIndex = (iY * m_iDimX) + iX;
                afResult[iIndex] = readFloat( abSliceData, iIndex );
            }
        }
        abSliceData = null;
        return afResult;
    }
    

    /** Get the slice data for the image with the given slice and weight.
     * @param iSlice, slice to read.
     * @param iWeight, weight to read.
     * @return float[] containing the data.
     */
    private float[] readIntegerWeight( int iSlice, int iWeight )
    {
        File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
        if ( !kFile.exists() || !kFile.canRead() )
        {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        int iLength = (int)kFile.length();
        if ( iLength <= 0 )
        {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        byte[] abSliceData = new byte[iLength];
        try {
            FileInputStream kFileReader = new FileInputStream(kFile);
            kFileReader.read(abSliceData,0,iLength);
            kFileReader.close();
        } catch (IOException e ) {}

        int length = m_iDimX * m_iDimY;
        float[] afResult = new float[length];
        for ( int iY = 0; iY < m_iDimY; iY++ )
        {
            for ( int iX = 0; iX < m_iDimX; iX++ )
            {
                int iIndex = (iY * m_iDimX) + iX;
                afResult[iIndex] = (float)readInteger( abSliceData, iIndex );
            }
        }
        return afResult;
    }


    /** Get the slice data for the image with the given slice and weight.
     * @param iSlice, slice to read.
     * @param iWeight, weight to read.
     * @return float[] containing the data.
     */
    private float[] readSliceWeight( int iSlice, int iWeight )
    {
        float[] buffer = null;
        if ( m_kRawImageFormat.equals( "dicom" ) ) 
        {
            buffer = readDicomWeight( iSlice, iWeight );
        }
        else if ( m_kRawImageFormat.equals( "float" ) ) 
        {
            buffer = readFloatWeight( iSlice, iWeight );
        }
        else
        {
            buffer = readIntegerWeight( iSlice, iWeight );
        }
        return buffer;
    }

    /** Create the mask image.  Create an image with the B0 weighted data, if
     * one has not already been created. Next use the
     * JDialogBrainSurfaceExtractor to extract only the brain regions. */
    private void createMaskImage()
    {
        if ( m_kB0Frame == null )
        {
            int iWeight = 0;
            int[] imageExtents = new int[]{m_iDimX, m_iDimY, m_iSlices};
            m_kB0Image = new ModelImage(ModelStorageBase.FLOAT, imageExtents, new String( "BrainImage" ) );

            int length = m_iDimX * m_iDimY;
            for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
            {
                float[] buffer = readSliceWeight(iSlice, iWeight);
                try {
                    m_kB0Image.importData(length * iSlice, buffer, false);
                } catch (IOException e) {}
            }

            m_kB0Image.addImageDisplayListener(this);
        }
        m_kB0Frame = new ViewJFrameImage(m_kB0Image, null, new Dimension(610, 200), false);

        if ( m_bDisplayB0 )
        {
            return;
        }

        JDialogBrainSurfaceExtractor kExtractBrain = new JDialogBrainSurfaceExtractor( m_kB0Frame, m_kB0Image );
        kExtractBrain.setFillHoles(false);
        kExtractBrain.setExtractPaint(true);
        kExtractBrain.callAlgorithm();

    }


    /** Creates the weighted data for the tensor calculation from the diffusion weighted images.
     * @return float[][][] containing the weights used in the tensor calculation.
     */
    private float[][][] createTensorWeights() {

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "creating weights mask...", 0, 100, true);

        float[][][] norm = new float[m_iBOrig][m_iSlices][m_iDimX*m_iDimY];
        float[][][] wmask = new float[m_iWeights][m_iSlices][m_iDimX*m_iDimY];

        for (int i=0; i<m_iBOrig; i++)
        {
            for (int j=0; j< m_iWeights; j++)
            {
                if ( m_aiMatrixEntries[j] == i )
                {                  
                    for (int slice = 0; slice < m_iSlices; slice++)
                    { 
                        float[] buffer = readSliceWeight(slice, j);
                        for ( int iY = 0; iY < m_iDimY; iY++ )
                        {
                            for ( int iX = 0; iX < m_iDimX; iX++ )
                            {
                                int iIndex = (iY * m_iDimX) + iX;
                                if ( m_kMaskImage.getBoolean( slice*m_iDimY*m_iDimX + iIndex) )
                                {
                                    float fValue = buffer[iIndex];
                                    norm[i][slice][iIndex] += fValue;
                                }
                                else
                                {
                                    norm[i][slice][iIndex] = 0;
                                }
                            }
                        }
                    }
                }
            }
            int iValue = (int)(100 * (float)(i+1)/(float)m_iBOrig);
            kProgressBar.updateValueImmed( iValue );
        }		

        kProgressBar.dispose();
        kProgressBar = null;
        return norm;
    }

    /** Second step in processing the diffusion weighted images. This
     * function is called once the brain extractor is complete.  The
     * brain image is transformed into a mask image, which is passed
     * to this function. The mask image limits where the tensor
     * calculations are performed.  The tensor is calculated, then the
     * eigen vectors and functional anisotropy images. The
     * DialogDTIColorDisplay is then launched.
     * @param kMaskImage, mask image representing the brain.
     */
    private void createDWIImage( )
    {
        float[][][] aaafWeights = createTensorWeights( );
        
        int iLen = m_iDimX * m_iDimY * m_iSlices;
        float[] afTensorData = new float[iLen * 6];


        int vol = m_iWeights;
        int a0 = 1;
        
        double[][] bmatrix = new double[vol][6+a0];

        Matrix B = new Matrix( m_kBMatrix.GetRows(), 6+1 );
        for ( int iR = 0; iR < m_kBMatrix.GetRows(); iR++ )
        {
            for ( int iC = 0; iC < 6+1; iC++ )
            {
                B.set(iR, iC, m_kBMatrix.Get(iR,iC));
            }
        }
        
        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "calculating tensor...", 0, 100, true);

        for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
        {
            float[][] buffer = new float[m_iWeights][];
            for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
            {
                buffer[iWeight] = readSliceWeight(iSlice, iWeight);
            }
            for ( int iY = 0; iY < m_iDimY; iY++ )
            {
                for ( int iX = 0; iX < m_iDimX; iX++ )
                {
                    int iIndex = (iY * m_iDimX) + iX;
                    int index = iSlice * (m_iDimY * m_iDimX) + (iY * m_iDimX) + iX;
                    if ( m_kMaskImage.getBoolean( index ) )
                    {
                        Matrix SIGMA = Matrix.identity(vol, vol);
                        double[][] x = new double[vol][1];
                        Matrix X = new Matrix(x);
                        int[] r = new int[vol];
                        int[] c = new int[vol];
                        int idx=0;

                        for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
                        {
                            double p = buffer[iWeight][iIndex];
                            double w = 0; //aaafWeights[iWeight][iSlice][iIndex];
                            if ( aaafWeights[m_aiMatrixEntries[iWeight]][iSlice][iIndex]  != 0 )
                            {
                                w = p/aaafWeights[m_aiMatrixEntries[iWeight]][iSlice][iIndex];
                            }
                            if (w>0.196)
                            {
                                r[idx]=iWeight;
                                c[idx]=iWeight;
                                idx++;
                            }
                            
                            //SIGMA is a diagonal matrix and its inverse would be diag(1/S(i,i))
                            if ( p<m_fMeanNoise )
                            {
                                p=m_fMeanNoise;
                            }

                            X.set(iWeight,0,Math.log(p));
                            SIGMA.set(iWeight,iWeight,(p*p*w)); // SIGMA here becomes SIGMA.inverse
                        }
                        Matrix B2 = B.getMatrix(r, 0, 6+a0-1);
                        Matrix SIGMA2 = SIGMA.getMatrix(r, c);
                        Matrix X2 = X.getMatrix(r, 0, 0);
                        Matrix A = ((B2.transpose()).times( SIGMA2 )).times(B2);
                        Matrix Y = ((B2.transpose()).times( SIGMA2 )).times(X2);
                        
                        Matrix D = new Matrix(7,1);
                        SingularValueDecomposition SVD = new SingularValueDecomposition(A);
                        Matrix S = SVD.getS();
                        for (int i=0; i<S.getRowDimension(); i++)
                            S.set(i,i,1/(S.get(i,i)));
                        D = (((SVD.getV()).times(S)).times(SVD.getU().transpose())).times(Y);
                        
                        // D = [Dxx, Dxy, Dxz, Dyy, Dyz, Dzz, Amplitude] 
                        float[] tensor = new float[10+vol];
                        for (int i=0; i<6; i++) { 
                            tensor[i] = (float)(-D.get(i,0)*1000000); // um^2/sec
                            if (i==0 && tensor[0]<0) { tensor[0]=(float)0.01; }
                            if (i==3 && tensor[3]<0) { tensor[3]=(float)0.01; }
                            if (i==5 && tensor[5]<0) { tensor[5]=(float)0.01; }
                        }
                    

                        float[] newTensor = new float[6];
                        newTensor[0] = tensor[0];
                        newTensor[1] = tensor[3];
                        newTensor[2] = tensor[5];
                        newTensor[3] = tensor[1];
                        newTensor[4] = tensor[2];
                        newTensor[5] = tensor[4];
                        for ( int iT = 0; iT < 6; iT++ )
                        {
                            afTensorData[index + iT*iLen] = newTensor[iT];
                        }
                    }
                    else
                    {
                        for ( int iT = 0; iT < 6; iT++ )
                        {
                            afTensorData[index + iT*iLen] = 0;
                        }
                    }
                }
            }
            int iValue = (int)(100 * (float)(iSlice+1)/(float)m_iSlices);
            kProgressBar.updateValueImmed( iValue );
        }
        kProgressBar.dispose();
        aaafWeights = null;

        int[] extents = new int[]{m_iDimX, m_iDimY, m_iSlices, 6};
        m_kDTIImage = new ModelImage( ModelStorageBase.FLOAT, extents, new String( "DiffusionTensorImage" ) );
        try {
            m_kDTIImage.importData(0, afTensorData, true );
        } catch (IOException e) {}
        setCompleted(true);
    }

    /** Translates the byte[] into float values at the given indes iIndex.
     * @param abData, byte[] containing float values.
     * @param iIndex, index into the array to get the float from.
     * @return float value representing 4 bytes starting at abData[iIndex*4].
     */
    private float readFloat( byte[] abData, int iIndex )
    {
        int b1 = abData[iIndex*4 + 0] & 0xff;
        int b2 = abData[iIndex*4 + 1] & 0xff;
        int b3 = abData[iIndex*4 + 2] & 0xff;
        int b4 = abData[iIndex*4 + 3] & 0xff;
        int tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        float fValue = Float.intBitsToFloat(tmpInt);
        return fValue;
    }


    /** Translates the byte[] into integer values at the given indes iIndex.
     * @param abData, byte[] containing integer values.
     * @param iIndex, index into the array to get the float from.
     * @return integer value representing 4 bytes starting at abData[iIndex*4].
     */
    private int readInteger( byte[] abData, int iIndex )
    {
        int b1 = abData[iIndex*4 + 0] & 0xff;
        int b2 = abData[iIndex*4 + 1] & 0xff;
        int b3 = abData[iIndex*4 + 2] & 0xff;
        int b4 = abData[iIndex*4 + 3] & 0xff;
        int iValue = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        return iValue;
    }



    /** ViewImageUpdateInterface : stub */
    public void setSlice(int slice) {}

    /** ViewImageUpdateInterface : stub */
    public void setTimeSlice(int tSlice) {}

    /** ViewImageUpdateInterface : stub */
    public boolean updateImageExtents() {
        return false;
    }

    /** ViewImageUpdateInterface : called when the JDialogBrainSurfaceExtractor finishes. Calls processDWI. */
    public boolean updateImages() {
        if ( !m_bDisplayB0 )
        {
            m_kMaskImage = ViewUserInterface.getReference().getRegisteredImageByName(m_kB0Frame.getComponentImage().commitPaintToMask());
            m_kB0Image.removeImageDisplayListener(this);
            m_kB0Image = null;
            run();
        }
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

}