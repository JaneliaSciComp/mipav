package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;

import java.io.*;
import java.util.Vector;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.jama.*;


/**
 * Algorithm calculates an Eigen Vector Image and Functional Anisotropy Image
 * from the input Diffusion Tensor Image.
 * 
 * See: Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDTITract extends AlgorithmBase
{

    /** Input Diffusion Tensor Image: */
    private ModelImage m_kDTIImage = null;
    /** Input EigenVector Image: */
    private ModelImage m_kEigenImage = null;
    /** The name of the tract output file, specified by the user. */
    private String m_kOutputFile = null;

    /** Which tensor nodes are already on the fiber bundle tract */
    private boolean[] m_abVisited = null;
    /** When outputing the fiber bundle tracts. */
    private boolean m_bFirstWrite = true;



    /** Initialize the Algorithm with the input DTI Image:
     * @param kDTIImage, input DTI Image.
     */
    public AlgorithmDTITract( ModelImage kDTIImage, ModelImage kEigenImage,
                              String kFile )
    {
        m_kDTIImage = kDTIImage;
        m_kEigenImage = kEigenImage;
        m_kOutputFile = kFile;
    }

    /** Clean up memory. */
    public void disposeLocal()
    {
        m_kDTIImage = null;
        m_kEigenImage = null;
        m_abVisited = null;
    }

    /** Run the DTI -> EigenVector Functional Anisotropy algorithm. */
    public void runAlgorithm()
    {
        if ( (m_kDTIImage == null) ||
             (m_kEigenImage == null) )
        {
            return;
        }
        reconstructTracts();
    }

    /** Constructs the Fiber Bundle Tracts from the dtiImage and the
     * eigenImage parameters. The fiber bundles are output to a file
     * sepecified by the user.
     * @param dtiImage, Diffusion Tensor Image.
     * @param eigenImage, EigenVector Image.
     */
    private void reconstructTracts()
    {
        // save the file version to disk
        File kFile = new File( m_kOutputFile ); 
        FileOutputStream kFileWriter = null;
        try {
            kFileWriter = new FileOutputStream(kFile);
        } catch ( FileNotFoundException e1 ) {
            kFileWriter = null;
        }

        int iDimX = m_kDTIImage.getExtents()[0];
        int iDimY = m_kDTIImage.getExtents()[1];
        int iDimZ = m_kDTIImage.getExtents()[2];
        int iLen = m_kDTIImage.getExtents()[0] *
            m_kDTIImage.getExtents()[1] * m_kDTIImage.getExtents()[2];

        float[] afVectorData = new float[3];

        int iCount = 0;
        int iTractSize = 0;

        ViewJProgressBar kProgressBar =
            new ViewJProgressBar("Calculating Fiber Bundle Tracts ",
                                 "calculating tracts...", 0, 100, true);

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
                        afVectorData[j] = m_kEigenImage.getFloat(i + j*iLen);
                        if ( afVectorData[j] != 0 )
                        {
                            bAllZero = false;
                        }

                    }
                    if ( !bAllZero )
                    {
                        kPos.SetData( iX, iY, iZ );
                        kTract.add(i);

                        kV1.SetData( afVectorData[0], afVectorData[1], afVectorData[2] );
                        kV2.SetData(kV1);
                        kV2.negEquals();

                        kV1.Normalize();
                        kV2.Normalize();

                        traceTract( kTract, kPos, kV1, m_kDTIImage, true );
                        m_abVisited[i] = true;
                        traceTract( kTract, kPos, kV2, m_kDTIImage, false );

                        if ( kTract.size() > 1 )
                        {
                            iCount++;
                            iTractSize += kTract.size();
                            outputTract( kTract, iDimX, iDimY, iDimZ, kFileWriter );
                        }
                        kTract.clear();
                    }
                }
            }
            int iValue = (int)(100 * (float)(iZ+1)/(float)iDimZ);
            kProgressBar.updateValueImmed( iValue );
        }
        kProgressBar.dispose();

        try {
            kFileWriter.close();
        } catch ( IOException e2 ) {}
    }

    /** Traces a single fiber bundle tract starting at the input
     * position and following the input direction.
     * @param kTract, fiber bundle tract, new positions are stored in this tract as the fiber is traced.
     * @param kStart, starting positon of the tract.
     * @param kDir, direction from the position.
     * @param dtiImage, Diffusion Tensor image used to calculate next direction of tract.
     * @param bDir, boolean when true the positions are added to the
     * end of the tract (positive direction). When false the positions
     * are added to the beginning of the tract (negative direction).
     */
    private void traceTract( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
                             ModelImage dtiImage, boolean bDir )
    {
        int iDimX = dtiImage.getExtents()[0];
        int iDimY = dtiImage.getExtents()[1];
        int iDimZ = dtiImage.getExtents()[2];
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];

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
            kStart.add( kDir, kNext );
            iX = Math.round(kNext.X());
            iY = Math.round(kNext.Y());
            iZ = Math.round(kNext.Z());
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
                afTensorData[j] = dtiImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.SetData( afTensorData[0], afTensorData[3], afTensorData[4],
                                 afTensorData[3], afTensorData[1], afTensorData[5], 
                                 afTensorData[4], afTensorData[5], afTensorData[2] );
                
                kMatrix.mult(kDir, kOut);
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
    }

    /** Writes the fiber bundle tract to disk.
     * @param kTract, the fiber bundle tract.
     * @param iDimX, x-dimension of the diffusion tensor image.
     * @param iDimY, y-dimension of the diffusion tensor image.
     * @param iDimZ, z-dimension of the diffusion tensor image.
     * @param kFileWrite, FileOutputStream.
     */
    private void outputTract( Vector<Integer> kTract, int iDimX, int iDimY, int iDimZ,
			      FileOutputStream kFileWriter )
    {
        int iVQuantity = kTract.size();

        int iBufferSize = iVQuantity*4 + 4;
        if ( m_bFirstWrite )
        {
            iBufferSize += 3*4;
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
