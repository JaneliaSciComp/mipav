package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.*;
import java.util.Vector;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import WildMagic.LibFoundation.Mathematics.*;


/**
 * Algorithm requires input of previous calculated FA, EigenVector and EigenValue images from
 * AlgorithmDTI2EGFA to calculate fiber bundle tracts
 * 
 * This algorithm works in conjunction with AlgorithmDTI2EGFA to create the 
 * MIPAV DTI Fiber Tracking/ Statistics Dialog
 * 
 * See:Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDTITract extends AlgorithmBase
{

    /** Input Diffusion Tensor Image: */
    private ModelImage m_kDTI = null;
    /** Input FA Image: */
    private ModelImage m_kFAImage = null;
    /** Input EigenVector Image: */
    private ModelImage m_kEigenVectorImage = null;
    /** Input EigenValue Image: */
    private ModelImage m_kEigenValueImage = null;
    /** The name of the tract output file, specified by the user. */
    private String m_kOutputFile = null;

    /** Which tensor nodes are already on the fiber bundle tract */
    private boolean[] m_abVisited = null;
    /** When saving the fiber bundle tracts. */
    private boolean m_bFirstWrite = true;
    private boolean m_bNegX = false;
    private boolean m_bNegY = false;
    private boolean m_bNegZ = false;
    private float m_fFAMin = 0;
    private float m_fFAMax = 1;
    private float m_fAngleMax = (float)Math.PI/4.0f;
    private int m_iMinLength = 20;



    /** Initialize the Algorithm with the input DTI Image:
     * @param kDTI input DTI Image.
     */
    public AlgorithmDTITract( ModelImage kDTI, ModelImage kEigenVectorImage, ModelImage kEigenValueImage,
                              String kFile, boolean bNegX, boolean bNegY, boolean bNegZ )
    {
        m_kDTI = kDTI;
        m_kEigenVectorImage = kEigenVectorImage;
        m_kEigenValueImage = kEigenValueImage;
        m_kOutputFile = kFile;
        m_bNegX = bNegX;
        m_bNegY = bNegY;
        m_bNegZ = bNegZ;
    }

    /** Initialize the Algorithm with the input DTI Image:
     * @param kDTI input DTI Image.
     */
    public AlgorithmDTITract( ModelImage kDTI, ModelImage kFAImage, ModelImage kEigenVectorImage, ModelImage kEigenValueImage,
                              String kFile, boolean bNegX, boolean bNegY, boolean bNegZ,
                              float fFAMin, float fFAMax, float fMaxAngle, int iMinLength )
    {
        m_kDTI = kDTI;
        m_kFAImage = kFAImage;
        m_kEigenVectorImage = kEigenVectorImage;
        m_kEigenValueImage = kEigenValueImage;
        m_kOutputFile = kFile;
        m_bNegX = bNegX;
        m_bNegY = bNegY;
        m_bNegZ = bNegZ;
        m_fFAMin = fFAMin;
        m_fFAMax = fFAMax;
        m_fAngleMax = (float)(fMaxAngle*Math.PI/180.0f);
        m_iMinLength = iMinLength;
        //System.err.println( m_fFAMin + " " + m_fFAMax + " " + m_fAngleMax + " " + fMaxAngle );
    }

    /** Clean up memory. */
    public void disposeLocal()
    {
        m_kDTI = null;
        m_kEigenVectorImage = null;
        m_abVisited = null;
    }

    /** Run the DTI -> EigenVector Functional Anisotropy algorithm. */
    public void runAlgorithm()
    {
        if ( (m_kDTI == null) ||
             (m_kEigenVectorImage == null) )
        {
            return;
        }
        reconstructTracts();
    }

    
    /** Constructs the Fiber Bundle Tracts from the DTI and the
     * EigenVectorImage parameters. The fiber bundles are output to a file
     * specified by the user.
     * @param DTI Diffusion Tensor Image.
     * @param EigenVectorImage EigenVector Image.
     */
    private void reconstructTracts()
    {
        // save the file version to disk
        File kFile = new File( m_kOutputFile ); 
        FileOutputStream kFileWriter = null;
        try {
            kFileWriter = new FileOutputStream(kFile);
        } catch ( FileNotFoundException e1 ) {}

        int iDimX = m_kDTI.getExtents()[0];
        int iDimY = m_kDTI.getExtents()[1];
        int iDimZ = m_kDTI.getExtents()[2];
        int iLen = m_kDTI.getExtents()[0] *
            m_kDTI.getExtents()[1] * m_kDTI.getExtents()[2];
        
        float[] afVectorData = new float[3];

        int iCount = 0;
        int iTractSize = 0;

        ViewJProgressBar kProgressBar =
            new ViewJProgressBar("Calculating Fiber Bundle Tracts ",
                                 "Calculating tracts...", 0, 100, true);

        m_abVisited  = new boolean[iLen];
        for ( int i = 0; i < iLen; i++ )
        {
            m_abVisited[i] = false;
        }

        Vector<Integer> kTract = new Vector<Integer>();
        Vector3f kPos = new Vector3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        
        for ( int iZ = 0; iZ < iDimZ; iZ++ )
        {
            for ( int iY = 0; iY < iDimY; iY++ )
            {
                for ( int iX = 0; iX < iDimX; iX++ )
                {
                    int i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

                    boolean bAllZero = true;
                    for ( int j = 0; j < 3; j++ )
                    {
                        afVectorData[j] = m_kEigenVectorImage.getFloat(i + j*iLen);
                        if ( afVectorData[j] != 0 )
                        {
                            bAllZero = false;
                        }

                    }
                    if ( !bAllZero )
                    {
                        if ( m_bNegX )
                        {
                            afVectorData[0] *= -1;
                        }
                        if ( m_bNegY )
                        {
                            afVectorData[1] *= -1;
                        }
                        if ( m_bNegZ )
                        {
                            afVectorData[2] *= -1;
                        }
                        
                        //System.out.println("point: iX = " + iX +  " iY = " + iY + " iZ = " + iZ  );
                        kPos.set( iX, iY, iZ );
                        kTract.add(i);

                        kV1.set( afVectorData[0], afVectorData[1], afVectorData[2] );
                        kV2.copy(kV1).neg();

                        kV1.normalize();
                        kV2.normalize();

                        traceTract2( kTract, new Vector3f(kPos), new Vector3f(kV1), m_kEigenVectorImage, m_kEigenValueImage, m_kFAImage, true );
                        m_abVisited[i] = true;
                        traceTract2( kTract, new Vector3f(kPos), new Vector3f(kV2), m_kEigenVectorImage, m_kEigenValueImage, m_kFAImage, false );
                        
                        
                        //traceTract( kTract, kPos, kV1, m_kDTI, true );
                        //m_abVisited[i] = true;
                        //traceTract( kTract, kPos, kV2, m_kDTI, false );

                        iCount++;
                        iTractSize += kTract.size();
                        outputTract( kTract, iDimX, iDimY, iDimZ, kFileWriter );
                        
                        kTract.clear();
                    }
                }
            }
            int iValue = (int)(100 * (float)(iZ+1)/iDimZ);
            kProgressBar.updateValueImmed( iValue );
        }
        kProgressBar.dispose();

        try {
            kFileWriter.close();
        } catch ( IOException e2 ) {}
        Preferences.debug( "\nNumber of tracts " + iCount +"\n", Preferences.DEBUG_ALGORITHM);
    }

    /** Traces a single fiber bundle tract starting at the input
     * position and following the input direction.
     * @param kTract fiber bundle tract, new positions are stored in this tract as the fiber is traced.
     * @param kStart starting position of the tract.
     * @param kDir direction from the position.
     * @param DTI Diffusion Tensor image used to calculate next direction of tract.
     * @param bDir boolean when true the positions are added to the
     * end of the tract (positive direction). When false the positions
     * are added to the beginning of the tract (negative direction).
     */
    /*private void traceTract( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
                             ModelImage DTI, boolean bDir )
    {
        int iDimX = DTI.getExtents()[0];
        int iDimY = DTI.getExtents()[1];
        int iDimZ = DTI.getExtents()[2];
        int iLen = DTI.getExtents()[0] * DTI.getExtents()[1] * DTI.getExtents()[2];

        float[] afTensorData = new float[6];

        boolean bDone = false;
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX;
        int iY;
        int iZ;
        int i;
        boolean bAllZero = true;

        while ( !bDone )
        {
            kNext.Add( kStart, kDir );
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;
            
            if ( (iZ < 0) || (iZ >= iDimZ) ||
                 (iY < 0) || (iY >= iDimY) ||
                 (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                break;
            }

            bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = DTI.getFloat(i + j*iLen);
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
                
                kMatrix.Mult(kDir, kOut);
                kOut.Normalize();
                
                if ( m_abVisited[i] )
                {
                    bDone = true;
                    break;
                }
                m_abVisited[i] = true;
                
                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }

                kStart = kNext;
                kDir = kOut;
            }
            else
            {
                bDone = true;
            }
        }
        kNext = null;
    }*/

    private void traceTract2( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
            ModelImage EigenVectorImage, ModelImage eigenValueImage, ModelImage kFAImage, boolean bDir )
    {
        int iDimX = EigenVectorImage.getExtents()[0];
        int iDimY = EigenVectorImage.getExtents()[1];
        int iDimZ = EigenVectorImage.getExtents()[2];
        int iLen = EigenVectorImage.getExtents()[0] * EigenVectorImage.getExtents()[1] * EigenVectorImage.getExtents()[2];

        float[] afVectorData = new float[6];

        boolean bDone = false;
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX, iY, iZ, i;
        float fLambda1, fLambda2, fLambda3;
        float fDot, fAngle;
        boolean bAllZero = true;

        float fFA;
        while ( !bDone )
        {
            kNext.copy( kStart ).add( kDir );
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

            if ( (iZ < 0) || (iZ >= iDimZ) ||
                    (iY < 0) || (iY >= iDimY) ||
                    (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                break;
            }

            if ( kFAImage != null )
            {
                fFA = kFAImage.getFloat(i);
                if ( (fFA < m_fFAMin) || (fFA > m_fFAMax) )
                {
                    bDone = true;
                    break;
                }
            }
            
            bAllZero = true;
            for ( int j = 0; j < 3; j++ )
            {
                afVectorData[j] = EigenVectorImage.getFloat(i + j*iLen);
                if ( afVectorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( m_bNegX )
            {
                afVectorData[0] *= -1;
            }
            if ( m_bNegY )
            {
                afVectorData[1] *= -1;
            }
            if ( m_bNegZ )
            {
                afVectorData[2] *= -1;
            }

            fLambda1 = eigenValueImage.getFloat(i*4+1);
            fLambda2 = eigenValueImage.getFloat(i*4+2);
            fLambda3 = eigenValueImage.getFloat(i*4+3);
            

            if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
            {
                bDone = true;
                break;
            }
            else if ( !bAllZero && (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0) )
            {
                kOut.set( afVectorData[0], afVectorData[1], afVectorData[2] );
                fDot = kDir.dot( kOut );
                if ( fDot < 0 )
                {
                    kOut.neg();
                }
                fAngle = Vector3f.angle(kDir,kOut);
                if ( fAngle > m_fAngleMax )
                {
                    bDone = true;
                    break;
                }

                if ( m_abVisited[i] )
                {
                    bDone = true;
                    break;
                }
                m_abVisited[i] = true;

                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }
                kStart.copy(kNext);
                kDir.copy(kOut);
            }
            else
            {
                bDone = true;
                break;
            }
        }
        kNext = null;
    }    
    
    

    
    
    /** Writes the fiber bundle tract to disk.
     * @param kTract the fiber bundle tract.
     * @param iDimX x-dimension of the diffusion tensor image.
     * @param iDimY y-dimension of the diffusion tensor image.
     * @param iDimZ z-dimension of the diffusion tensor image.
     * @param kFileWrite FileOutputStream.
     */
    private void outputTract( Vector<Integer> kTract, int iDimX, int iDimY, int iDimZ,
                  FileOutputStream kFileWriter )
    {
        int iVQuantity = kTract.size();
        if ( iVQuantity < m_iMinLength )
        {
        	return;
        }

        int iBufferSize = iVQuantity*4 + 4;
        if ( m_bFirstWrite )
        {
            iBufferSize += (3*4 + 3);
        }
        ByteArrayOutputStream acBufferOut = new ByteArrayOutputStream( iBufferSize );
        DataOutputStream acDataOut = new DataOutputStream( acBufferOut );
        if ( kFileWriter != null )
        {
            try {
                if ( m_bFirstWrite )
                {
                    acDataOut.writeInt(iDimX);
                    acDataOut.writeInt(iDimY);
                    acDataOut.writeInt(iDimZ);
                    acDataOut.writeBoolean(m_bNegX);
                    acDataOut.writeBoolean(m_bNegY);
                    acDataOut.writeBoolean(m_bNegZ);
                    m_bFirstWrite = false;
                }
                acDataOut.writeInt(iVQuantity);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < iVQuantity; i++)
        {
            m_abVisited[kTract.get(i).intValue()] = false;

            if ( kFileWriter != null )
            {
                try {
                    acDataOut.writeInt(kTract.get(i));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        if ( kFileWriter != null )
        {
            byte[] acBuffer = acBufferOut.toByteArray();
            try {
                kFileWriter.write(acBuffer,0,iBufferSize);
            } catch ( IOException e2 ) {
                acBuffer = null;
            }
            acBuffer = null;
        }
        try {
            acBufferOut.close();
            acDataOut.close();
        } catch (IOException e) {}
        acBufferOut = null;
        acDataOut = null;
    }
}

