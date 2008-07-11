package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.*;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJProgressBar;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.jama.*;


/**
 * Algorithm calculates an Eigen Vector Image and Functional Anisotropy Image
 * from the input Diffusion Tensor Image.
 * 
 * See: Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDTI2EGFA extends AlgorithmBase
{

    /** Input Diffusion Tensor Image: */
    private ModelImage m_kDTIImage = null;
    /** Output EigenVector Image: */
    private ModelImage m_kEigenImage = null;
    /** Output Functional Anisotropy Image: */
    private ModelImage m_kFAImage = null;

    /** Initialize the Algorithm with the input DTI Image:
     * @param kDTIImage, input DTI Image.
     */
    public AlgorithmDTI2EGFA( ModelImage kDTIImage )
    {
        m_kDTIImage = kDTIImage;
    }

    /** Clean up memory. */
    public void disposeLocal()
    {
        m_kDTIImage = null;
        m_kEigenImage = null;
        m_kFAImage = null;
    }

    /** Run the DTI -> EigenVector Functional Anisotropy algorithm. */
    public void runAlgorithm()
    {
        if ( m_kDTIImage == null )
        {
            return;
        }
        calcEigenVectorFA();
    }

    /** Returns the Eigen Vector Image. 
     * @return the Eigen Vector Image. 
     */
    public ModelImage getEigenImage()
    {
        return m_kEigenImage;
    }

    /** Returns the Functional Anisotropy Image. 
     * @return the Functional Anisotropy Image. 
     */
    public ModelImage getFAImage()
    {
        return m_kFAImage;
    }


    /** 
     * Calculates the eigen vector data from the dtiImage.
     */
    private void calcEigenVectorFA( )
    {
        if ( m_kDTIImage == null )
        {
            return;
        }
        int iLen = m_kDTIImage.getExtents()[0] * m_kDTIImage.getExtents()[1] * m_kDTIImage.getExtents()[2];
        int iZDim = m_kDTIImage.getExtents()[2];
        int iSliceSize = m_kDTIImage.getExtents()[0] * m_kDTIImage.getExtents()[1];
        float[] afData = new float[iLen];
        float[] afDataCM = new float[iLen*9];
        float[] afTensorData = new float[6];

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating Eigen Vectors ",
                                                             "calculating eigen vectors...", 0, 100, true);
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        Vector3f kV3 = new Vector3f();
        Matrix3f kEigenValues = new Matrix3f();
        for ( int i = 0; i < iLen; i++ )
        {
            boolean bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = m_kDTIImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.Set( afTensorData[0], afTensorData[3], afTensorData[4],
                                                 afTensorData[3], afTensorData[1], afTensorData[5], 
                                                 afTensorData[4], afTensorData[5], afTensorData[2] );
                
                Matrix3f.EigenDecomposition( kMatrix, kEigenValues );
                float fLambda1 = kEigenValues.M22;
                float fLambda2 = kEigenValues.M11;
                float fLambda3 = kEigenValues.M00;
                kMatrix.GetColumn(2,kV1);
                kMatrix.GetColumn(1,kV2);
                kMatrix.GetColumn(0,kV3);
                
                afData[i] = (float)(Math.sqrt(1.0/2.0) *
                        ( ( Math.sqrt( (fLambda1 - fLambda2)*(fLambda1 - fLambda2) +
                                (fLambda2 - fLambda3)*(fLambda2 - fLambda3) +
                                (fLambda3 - fLambda1)*(fLambda3 - fLambda1)   ) ) /
                                ( Math.sqrt( fLambda1*fLambda1 + fLambda2*fLambda2 + fLambda3*fLambda3 ) ) ) );
            }
            else
            {
                afData[i] = 0;
            }

            afDataCM[i + 0*iLen] = kV1.X;
            afDataCM[i + 1*iLen] = kV1.Y;
            afDataCM[i + 2*iLen] = kV1.Z;

            afDataCM[i + 3*iLen] = kV2.X;
            afDataCM[i + 4*iLen] = kV2.Y;
            afDataCM[i + 5*iLen] = kV2.Z;

            afDataCM[i + 6*iLen] = kV3.X;
            afDataCM[i + 7*iLen] = kV3.Y;
            afDataCM[i + 8*iLen] = kV3.Z;

            if ( (i%iSliceSize) == 0 )
            {
                int iValue = (int)(100 * (float)(i+1)/(float)iLen);
                kProgressBar.updateValueImmed( iValue );
            }
        }
        kMatrix = null;
        kV1 = null;
        kV2 = null;
        kV3 = null;
        kEigenValues = null;

        kProgressBar.dispose();
        kProgressBar = null;

        int[] extentsEV = new int[]{m_kDTIImage.getExtents()[0], m_kDTIImage.getExtents()[1], m_kDTIImage.getExtents()[2], 9};
        int[] extentsA = new int[]{m_kDTIImage.getExtents()[0], m_kDTIImage.getExtents()[1], m_kDTIImage.getExtents()[2]};


        m_kFAImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     new String( m_kDTIImage.getFileInfo(0).getFileName() + "FA") );
        try {
            m_kFAImage.importData(0, afData, true);
        } catch (IOException e) {
            m_kFAImage.disposeLocal();
            m_kFAImage = null;
        }

        m_kEigenImage = new ModelImage( ModelStorageBase.FLOAT,
                                        extentsEV,
                                        new String( m_kDTIImage.getFileInfo(0).getFileName() + "EG") );
        try {
            m_kEigenImage.importData(0, afDataCM, true);
        } catch (IOException e) {
            m_kEigenImage.disposeLocal();
            m_kEigenImage = null;
        }

        extentsEV = null;
        extentsA = null;
        
        afData = null;
        afDataCM = null;
        afTensorData = null;

    }


}
