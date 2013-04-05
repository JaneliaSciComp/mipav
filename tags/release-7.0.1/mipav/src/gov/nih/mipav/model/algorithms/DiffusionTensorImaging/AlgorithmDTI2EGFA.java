package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;


import java.io.IOException;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Algorithm requires input of a Diffusion Tensor Image to calculate an Apparent Diffusion Coefficient Image, 
 * Functional Anisotropy Image, Color Image, Eigen Value Image, Eigen Vector Image, Relative Anisotropy Image,
 * Trace Image, and Volume Ratio Image
 * 
 * This algorithm works in conjunction with AlgorithmDTITract to create the 
 * MIPAV DTI Fiber Tracking/ Statistics Dialog
 *
 * See: Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDTI2EGFA extends AlgorithmBase
{

    /** Input Diffusion Tensor Image: */
    private ModelImage m_kDTI = null;
    /** Output EigenVector Image: */
    private ModelImage m_kEigenVectorImage = null;
    /** Output EigenValue Image: */
    private ModelImage m_kEigenValueImage = null;
    /** Output Functional Anisotropy Image: */
    private ModelImage m_kFAImage = null;
    /** Output Trace (ADC) Image (before diagonalization of diffusion tensor): */
    private ModelImage m_kADCImage = null;
    /** Output Trace Image (before diagonalization of diffusion tensor): */
    private ModelImage m_kTraceImage = null;
    /** Output RA (Relative Anisotropy) Image: */
    private ModelImage m_kRAImage = null;
    /** Output VR (Volume Radio) Image: */
    private ModelImage m_kVRImage = null;
    /** Output Color mapped Image: */
    private ModelImage m_kColorImage = null;

    /** Initialize the Algorithm with the input DTI Image:
     * @param kDTI input DTI Image.
     */
    public AlgorithmDTI2EGFA( ModelImage kDTI )
    {
        m_kDTI = kDTI;
    }

    /** Clean up memory. */
    public void disposeLocal()
    {
        m_kDTI = null;
        m_kEigenVectorImage = null;
        m_kEigenValueImage = null;
        m_kFAImage = null;
        m_kADCImage = null;
        m_kVRImage = null;
        m_kTraceImage = null;
        m_kRAImage = null;
        System.gc();
    }

    /** Returns the Color Map Image. 
     * @return the Color Map Image. 
     */
    public ModelImage getColorImage()
    {
        return m_kColorImage;
    }

    /** Returns the Apparent Diffusion Coefficient Image. 
     * @return the Apparent Diffusion Coefficient Image. 
     */
    public ModelImage getADCImage()
    {
        return m_kADCImage;
    }

    /** Returns the Eigen Vector Image. 
     * @return the Eigen Vector Image. 
     */
    public ModelImage getEigenVectorImage()
    {
        return m_kEigenVectorImage;
    }


    /** Returns the Eigen Value Image. 
     * @return the Eigen Value Image. 
     */
    public ModelImage getEigenValueImage()
    {
        return m_kEigenValueImage;
    }

    /** Returns the Functional Anisotropy Image. 
     * @return the Functional Anisotropy Image. 
     */
    public ModelImage getFAImage()
    {
        return m_kFAImage;
    }

    /** Returns the Relative Anisotropy Image. 
     * @return the Relative Anisotropy Image. 
     */
    public ModelImage getRAImage()
    {
        return m_kRAImage;
    }

    /** Returns the Trace Image. 
     * @return the Trace Image. 
     */
    public ModelImage getTraceImage()
    {
        return m_kTraceImage;
    }

    /** Returns the Volume Ratio Image. 
     * @return the Volume Ratio Image. 
     */
    public ModelImage getVRImage()
    {
        return m_kVRImage;
    }

    /** Run the DTI -> EigenVector Functional Anisotropy algorithm. */
    public void runAlgorithm()
    {
        if ( m_kDTI == null )
        {
            return;
        }
        calcEigenVectorFA();
    }


    /** 
     * Calculates the eigen vector data from the DTI.
     */
    private void calcEigenVectorFA( )
    {
        if ( m_kDTI == null )
        {
            return;
        }
        int iLen = m_kDTI.getExtents()[0] * m_kDTI.getExtents()[1] * m_kDTI.getExtents()[2];
        //int iZDim = m_kDTI.getExtents()[2];
        int iSliceSize = m_kDTI.getExtents()[0] * m_kDTI.getExtents()[1];
        float[] afData = new float[iLen];       // functional anisotropy
        float[] afTraceData = new float[iLen];  // trace data
        float[] afADCData = new float[iLen];    // ADC data
        float[] afRAData = new float[iLen];     // RA
        float[] afVRData = new float[iLen];     // VR
        float[] afEigenValues = new float[iLen*4]; // eigen values
        float[] afDataCM = new float[iLen*9];      // engen vectors
        float[] afTensorData = new float[6];       // tensor (single voxel)
        float[] afColorImageData = new float[iLen*4];  // color image
        float gamma = 1.8f;

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating Eigen Vectors ",
                                                             "Calculating Eigen Vectors...", 0, 100, true);
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        Vector3f kV3 = new Vector3f();
        Matrix3f kEigenValues = new Matrix3f();
        
        float minL = Float.MAX_VALUE;
        float maxL = -Float.MAX_VALUE;
        for ( int i = 0; i < iLen; i++ )
        {
            boolean bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = m_kDTI.getFloat(i + j*iLen);
                if ( afTensorData[j] == Float.NaN )
                {
                	System.err.println( "nan" );
                	afTensorData[j] = 0;
                }
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {                
                kMatrix.set( afTensorData[0], afTensorData[1], afTensorData[2],
                        afTensorData[1], afTensorData[3], afTensorData[4], 
                        afTensorData[2], afTensorData[4], afTensorData[5] );


                afTraceData[i] = kMatrix.M00 + kMatrix.M11 + kMatrix.M22;
                afADCData[i] = afTraceData[i]/3.0f;
                
                if ( Matrix3f.eigenDecomposition( kMatrix, kEigenValues ) )
                {
                    float fLambda1 = kEigenValues.M22;
                    float fLambda2 = kEigenValues.M11;
                    float fLambda3 = kEigenValues.M00;
                    if ( (fLambda1 <= 0) || (fLambda2 <= 0) || (fLambda3 <= 0) )
                    {
                        bAllZero = true;
                    }
                    else
                    {
                    	if ( fLambda1 < minL )
                    	{
                    		minL = fLambda1;
                    	}
                    	if ( fLambda2 < minL )
                    	{
                    		minL = fLambda2;
                    	}
                    	if ( fLambda3 < minL )
                    	{
                    		minL = fLambda3;
                    	}

                    	if ( fLambda1 > maxL )
                    	{
                    		maxL = fLambda1;
                    	}
                    	if ( fLambda2 > maxL )
                    	{
                    		maxL = fLambda2;
                    	}
                    	if ( fLambda3 > maxL )
                    	{
                    		maxL = fLambda3;
                    	}



                    	afEigenValues[i*4 + 0] = 0;
                    	afEigenValues[i*4 + 1] = fLambda1;
                    	afEigenValues[i*4 + 2] = fLambda2;
                    	afEigenValues[i*4 + 3] = fLambda3;
                    	kMatrix.getColumn(2,kV1);
                    	kMatrix.getColumn(1,kV2);
                    	kMatrix.getColumn(0,kV3);


                    	afData[i] = (float)(Math.sqrt(1.0/2.0) *
                    			( ( Math.sqrt( (fLambda1 - fLambda2)*(fLambda1 - fLambda2) +
                    					(fLambda2 - fLambda3)*(fLambda2 - fLambda3) +
                    					(fLambda3 - fLambda1)*(fLambda3 - fLambda1)   ) ) /
                    					( Math.sqrt( fLambda1*fLambda1 + fLambda2*fLambda2 + fLambda3*fLambda3 ) ) ) );

                    	float fLambda = (fLambda1 + fLambda2 + fLambda3)/3.0f;
                    	afRAData[i] = (float)(Math.sqrt((fLambda1 - fLambda)*(fLambda1 - fLambda) + 
                    			(fLambda2 - fLambda)*(fLambda2 - fLambda) +
                    			(fLambda3 - fLambda)*(fLambda3 - fLambda)   ) / 
                    			Math.sqrt( 3.0f * fLambda ));
                    	afVRData[i] = (fLambda1*fLambda2*fLambda3)/(fLambda*fLambda*fLambda);                

                    	afDataCM[i + 0*iLen] = kV1.X;
                    	afDataCM[i + 1*iLen] = kV1.Y;
                    	afDataCM[i + 2*iLen] = kV1.Z;

                    	afDataCM[i + 3*iLen] = -kV2.X;
                    	afDataCM[i + 4*iLen] = -kV2.Y;
                    	afDataCM[i + 5*iLen] = -kV2.Z;

                    	afDataCM[i + 6*iLen] = kV3.X;
                    	afDataCM[i + 7*iLen] = kV3.Y;
                    	afDataCM[i + 8*iLen] = kV3.Z;

                    	
                    	kV1.normalize();
                    	afColorImageData[i* 4 + 0] = 255;
                    	
                    	afColorImageData[i* 4 + 1] = Math.abs(kV1.X) * afData[i];
                    	afColorImageData[i* 4 + 1] = (float) Math.pow(afColorImageData[i* 4 + 1], (1 / gamma));
                    	afColorImageData[i* 4 + 1] = afColorImageData[i* 4 + 1] * 255;
                    	

                    	afColorImageData[i* 4 + 2] = Math.abs(kV1.Y) * afData[i];
                    	afColorImageData[i* 4 + 2] = (float) Math.pow(afColorImageData[i* 4 + 2], (1 / gamma));
                    	afColorImageData[i* 4 + 2] = afColorImageData[i* 4 + 2] * 255;
                    	

                    	afColorImageData[i* 4 + 3] = Math.abs(kV1.Z) * afData[i];
                    	afColorImageData[i* 4 + 3] = (float) Math.pow(afColorImageData[i* 4 + 3], (1 / gamma));
                    	afColorImageData[i* 4 + 3] = afColorImageData[i* 4 + 3] * 255;
                    }
                }
                else
                {
                    bAllZero = true;
                }
            }
            if ( bAllZero )
            {
                afData[i] = 0;
                afTraceData[i] = 0;
                afRAData[i] = 0;
                afVRData[i] = 0;
                afEigenValues[i*4 + 0] = 0;
                afEigenValues[i*4 + 1] = 0;
                afEigenValues[i*4 + 2] = 0;
                afEigenValues[i*4 + 3] = 0;

                afDataCM[i + 0*iLen] = 0;
                afDataCM[i + 1*iLen] = 0;
                afDataCM[i + 2*iLen] = 0;

                afDataCM[i + 3*iLen] = 0;
                afDataCM[i + 4*iLen] = 0;
                afDataCM[i + 5*iLen] = 0;

                afDataCM[i + 6*iLen] = 0;
                afDataCM[i + 7*iLen] = 0;
                afDataCM[i + 8*iLen] = 0;
                

                afColorImageData[i*4 + 0] = 0;
                afColorImageData[i*4 + 1] = 0;
                afColorImageData[i*4 + 2] = 0;
                afColorImageData[i*4 + 3] = 0;
            }

            if ( (i%iSliceSize) == 0 )
            {
                int iValue = (int)(100 * (float)(i+1)/iLen);
                kProgressBar.updateValueImmed( iValue );
            }
        }
        
        System.err.println( minL + " " + maxL );
        
        kMatrix = null;
        kV1 = null;
        kV2 = null;
        kV3 = null;
        kEigenValues = null;

        kProgressBar.dispose();
        kProgressBar = null;

        int[] extentsEV = new int[]{m_kDTI.getExtents()[0], m_kDTI.getExtents()[1], m_kDTI.getExtents()[2], 9};
        int[] extentsA = new int[]{m_kDTI.getExtents()[0], m_kDTI.getExtents()[1], m_kDTI.getExtents()[2]};


        /** Fractional Anisotropy Image */
        m_kFAImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "FA") );
        try {
            m_kFAImage.importData(0, afData, true);
        } catch (IOException e) {
            m_kFAImage.disposeLocal();
            m_kFAImage = null;
        }
        if ( m_kFAImage != null )
        {           
            //new ViewJFrameImage(m_kFAImage, null, new Dimension(610, 200), false);
            m_kFAImage.setExtents(extentsA);
            m_kFAImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kFAImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }

        
        
        /** Trace Image */
        m_kTraceImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "Trace") );
        try {
            m_kTraceImage.importData(0, afTraceData, true);
        } catch (IOException e) {
            m_kTraceImage.disposeLocal();
            m_kTraceImage = null;
        }
        if ( m_kTraceImage != null )
        {
            //new ViewJFrameImage(m_kTraceImage, null, new Dimension(610, 200), false);
            m_kTraceImage.setExtents(extentsA);
            m_kTraceImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kTraceImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }
        
        

        /** RA Image */
        m_kRAImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "RA") );
        try {
            m_kRAImage.importData(0, afRAData, true);
        } catch (IOException e) {
            m_kRAImage.disposeLocal();
            m_kRAImage = null;
        }
        if ( m_kRAImage != null )
        {
            //new ViewJFrameImage(m_kRAImage, null, new Dimension(610, 200), false);
            m_kRAImage.setExtents(extentsA);
            m_kRAImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kRAImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }
        
        

        /** Volume Ratio Image */
        m_kVRImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "VR") );
        try {
            m_kVRImage.importData(0, afVRData, true);
        } catch (IOException e) {
            m_kVRImage.disposeLocal();
            m_kVRImage = null;
        }

        if ( m_kVRImage != null )
        {
           //new ViewJFrameImage(m_kVRImage, null, new Dimension(610, 200), false);
            m_kVRImage.setExtents(extentsA);
            m_kVRImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kVRImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }
        

        /** ADC Image */
        m_kADCImage = new ModelImage( ModelStorageBase.FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "ADC") );
        try {
            m_kADCImage.importData(0, afADCData, true);
        } catch (IOException e) {
            m_kADCImage.disposeLocal();
            m_kADCImage = null;
        }
        if ( m_kADCImage != null )
        {
           //new ViewJFrameImage(m_kADCImage, null, new Dimension(610, 200), false);
            m_kADCImage.setExtents(extentsA);
            m_kADCImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kADCImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }
   
        
        /** Eigen Vector Image */
        m_kEigenVectorImage = new ModelImage( ModelStorageBase.FLOAT,
                                        extentsEV,
                                        ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "EG") );
        try {
            m_kEigenVectorImage.importData(0, afDataCM, true);
        } catch (IOException e) {
            m_kEigenVectorImage.disposeLocal();
            m_kEigenVectorImage = null;
        }
        if ( m_kEigenVectorImage != null )
        {
           //new ViewJFrameImage(m_kEigenVectorImage, null, new Dimension(610, 200), false);
            m_kEigenVectorImage.setExtents(extentsEV);
            m_kEigenVectorImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kEigenVectorImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsEV);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
        }
        else
        {
            System.err.println( "null" );
        }
        

        /** Eigen Value Image */
        m_kEigenValueImage = new ModelImage( ModelStorageBase.ARGB_FLOAT,
                extentsA,
                                        ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "EV") );
        try {
            m_kEigenValueImage.importData(0, afEigenValues, true);
        } catch (IOException e) {
            m_kEigenValueImage.disposeLocal();
            m_kEigenValueImage = null;
        }
        if ( m_kEigenValueImage != null )
        {
            // looks good...
            m_kEigenValueImage.setExtents(extentsA);
            m_kEigenValueImage.copyFileTypeInfo(m_kDTI);        
            //copy core file info over
            FileInfoBase[] fileInfoBases = m_kEigenValueImage.getFileInfo();
            for (int i=0;i<fileInfoBases.length;i++) {
                fileInfoBases[i].setExtents(extentsA);
                fileInfoBases[i].setFileDirectory( m_kDTI.getFileInfo(0).getFileDirectory());
            }
            
           //new ViewJFrameImage(m_kEigenValueImage, null, new Dimension(610, 200), false);
        }
        else
        {
            System.err.println( "null" );
        }
        


        /** Fractional Anisotropy Image */
        m_kColorImage = new ModelImage( ModelStorageBase.ARGB_FLOAT,
                                     extentsA,
                                     ModelImage.makeImageName( m_kDTI.getFileInfo(0).getFileName(), "ColorMap") );
        try {
        	m_kColorImage.importData(0, afColorImageData, true);
        } catch (IOException e) {
        	m_kColorImage.disposeLocal();
        	m_kColorImage = null;
        }
        if ( m_kColorImage != null )
        {           
    		JDialogBase.updateFileInfo( m_kDTI, m_kColorImage );
        }
        
        
        extentsEV = null;
        extentsA = null;

        afData = null;
        afTraceData = null;
        afADCData = null;
        afVRData = null;
        afRAData = null;
        afDataCM = null;
        afTensorData = null;
        afColorImageData = null;
    }


}