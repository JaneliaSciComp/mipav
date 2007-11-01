package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.*;
import java.awt.*;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
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


public class AlgorithmDWI2DTI extends AlgorithmBase
    implements ViewImageUpdateInterface
{

    private ModelImage m_kMaskImage = null;
    /** Mask of the image that shows only the brain: */
    private ModelImage m_kBrainImage = null;
    /** ViewJFrameImage for displaying the Mask of the image that shows only the brain: */
    private ViewJFrameImage m_kBrainFrame = null;
    private int m_iSlices;
    private int m_iDimX;
    private int m_iDimY;
    private int m_iBOrig;
    private int m_iWeights;
    private float m_fMeanNoise;
    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;
    /** List of file names for the Diffusion Weighted Images, from the .path file. */
    private String[][] m_aakDWIList = null;
    private int[] m_repidx;
    
    private ModelImage m_kDTIImage = null;


    public AlgorithmDWI2DTI( ModelImage kMaskImage, int iSlices, int iDimX, int iDimY,
    int iBOrig, int iWeights, float fMeanNoise, String[][] aakDWIList, int[] repidx, GMatrixf kBMatrix       
    )
    {
        m_kMaskImage = kMaskImage;
        m_iSlices = iSlices;
        m_iDimX = iDimX;
        m_iDimY = iDimY;
        m_iBOrig = iBOrig;
        m_iWeights = iWeights;
        m_fMeanNoise = fMeanNoise;
       m_aakDWIList = aakDWIList;
       m_repidx = repidx;
       m_kBMatrix = kBMatrix;
    }

    public void disposeLocal()
    {
        //m_kDTIImage = null;
    }

    /** */
    public void runAlgorithm()
    {
        if ( m_kBrainImage == null )
        {
            createMaskImage();
        }
        else
        {
            createDWIImage();
        }
    }

    public ModelImage getDTIImage()
    {
        return m_kDTIImage;
    }

    
    private void createMaskImage()
    {
        int[] imageExtents = new int[]{m_iDimX, m_iDimY, m_iSlices};
        m_kBrainImage = new ModelImage( ModelStorageBase.FLOAT, imageExtents, new String( "BrainImage" ) );
        for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
        {
            int iWeight = 0;
            File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
            if ( !kFile.exists() || !kFile.canRead() )
            {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return;
            }
            int iLength = (int)kFile.length();
            if ( iLength <= 0 )
            {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return;
            }
            byte[] abSliceData = new byte[iLength];
            try {
                FileInputStream kFileReader = new FileInputStream(kFile);
                kFileReader.read(abSliceData,0,iLength);
                kFileReader.close();
            } catch (IOException e ) {}
            
            for ( int iY = 0; iY < m_iDimY; iY++ )
            {
                for ( int iX = 0; iX < m_iDimX; iX++ )
                {
                    int iIndex = (iY * m_iDimX) + iX;
                    float p = readFloat( abSliceData, iIndex );
                    
                    m_kBrainImage.set(iSlice*(m_iDimX*m_iDimY) + iIndex, p );
                }
            }
        }

        m_kBrainImage.addImageDisplayListener(this);
        m_kBrainFrame = new ViewJFrameImage(m_kBrainImage, null, new Dimension(610, 200), false);
        JDialogBrainSurfaceExtractor kExtractBrain = new JDialogBrainSurfaceExtractor( m_kBrainFrame, m_kBrainImage );
        kExtractBrain.setFilterGaussianStdDev(5);
        kExtractBrain.setFillHoles(false);
        kExtractBrain.setExtractPaint(true);
        kExtractBrain.callAlgorithm();
    }


    /** Creates the weighted mask image for the tensor calculation from the diffusion weighted images.
     * @param kMaskImage, the image masking the diffusion weighted images so only the brain portions are used.
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
                if ( m_repidx[j] == i )
                {
                    for (int slice = 0; slice < m_iSlices; slice++)
                    { 
                        File kFile = new File( m_aakDWIList[slice][j] );
                        if ( !kFile.exists() || !kFile.canRead() )
                        {
                            MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                            return null;
                        }
                        int iLength = (int)kFile.length();
                        if ( iLength <= 0 )
                        {
                            MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                            return null;
                        }
                        byte[] abSliceData = new byte[iLength];
                        try {
                            abSliceData = new byte[iLength];
                            FileInputStream kFileReader = new FileInputStream(kFile);
                            kFileReader.read( abSliceData,0,iLength);
                            kFileReader.close();
                        } catch (IOException e ) {}


                        for ( int iY = 0; iY < m_iDimY; iY++ )
                        {
                            for ( int iX = 0; iX < m_iDimX; iX++ )
                            {
                                int iIndex = (iY * m_iDimX) + iX;
                                if ( m_kMaskImage.getBoolean( slice*m_iDimY*m_iDimX + iIndex) )
                                {
                                    float fValue = readFloat( abSliceData, iIndex );
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

        kProgressBar.getProgressBar().setMaximum(m_iWeights);
        for (int j=0; j< m_iWeights; j++)
        {
            for (int slice = 0; slice < m_iSlices; slice++)
            { 
                File kFile = new File( m_aakDWIList[slice][j] );
                if ( !kFile.exists() || !kFile.canRead() )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                    return null;
                }
                int iLength = (int)kFile.length();
                if ( iLength <= 0 )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                    return null;
                }
                byte[] abSliceData = new byte[iLength];
                try {
                    abSliceData = new byte[iLength];
                    FileInputStream kFileReader = new FileInputStream(kFile);
                    kFileReader.read( abSliceData,0,iLength);
                    kFileReader.close();
                } catch (IOException e ) {}


                for ( int iY = 0; iY < m_iDimY; iY++ )
                {
                    for ( int iX = 0; iX < m_iDimX; iX++ )
                    {
                        int iIndex = (iY * m_iDimX) + iX;

                        if ( norm[m_repidx[j]][slice][iIndex] == 0 )
                        {
                            wmask[j][slice][iIndex]= 0;
                        }
                        else
                        {
                            if ( m_kMaskImage.getBoolean( slice*m_iDimY*m_iDimX + iIndex) )
                            {
                                float fValue = readFloat( abSliceData, iIndex );
                                wmask[j][slice][iIndex]= fValue/norm[m_repidx[j]][slice][iIndex];
                            }
                        }
                    }
                }
            }
            int iValue = (int)(100 * (float)(j+1)/(float)m_iWeights);
            kProgressBar.updateValueImmed( iValue );
        }

        kProgressBar.dispose();
        return wmask;	
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
            byte[][] aabSliceData = new byte[m_iWeights][ ];

            for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
            {
                File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
                if ( !kFile.exists() || !kFile.canRead() )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                    return;
                }
                int iLength = (int)kFile.length();
                aabSliceData[iWeight] = new byte[iLength];
                if ( iLength <= 0 )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                    return;
                }
                try {
                    FileInputStream kFileReader = new FileInputStream(kFile);
                    kFileReader.read(aabSliceData[iWeight],0,iLength);
                    kFileReader.close();
                } catch (IOException e ) {}
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
                            double p = (double)readFloat( aabSliceData[iWeight], iIndex );

                            //SIGMA is a diagonal matrix and its inverse would be diag(1/S(i,i))
                            if ( p<m_fMeanNoise )
                            {
                                p=m_fMeanNoise;
                            }
                        
                            double w = aaafWeights[iWeight][iSlice][iIndex];
                            if (w>0.196)
                            {
                                r[idx]=iWeight;
                                c[idx]=iWeight;
                                idx++;
                            }
                            X.set(iWeight,0,Math.log(p));
                            SIGMA.set(iWeight,iWeight,(p*p*w)); // SIGMA here becomes SIGMA.inverse

                        }
                        Matrix B2 = B.getMatrix(r, 0, 6+a0-1);
                        Matrix SIGMA2 = SIGMA.getMatrix(r, c);
                        Matrix X2 = X.getMatrix(r, 0, 0);
                        //Matrix A = ((B.transpose()).times( SIGMA )).times(B);
                        //Matrix Y = ((B.transpose()).times( SIGMA )).times(X);
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
            aabSliceData = null;

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
        m_kMaskImage = ViewUserInterface.getReference().getRegisteredImageByName(m_kBrainFrame.getComponentImage().commitPaintToMask());
        m_kBrainImage.removeImageDisplayListener(this);
        //m_kBrainImage.disposeLocal();
        //m_kBrainImage = null;
        run();
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
