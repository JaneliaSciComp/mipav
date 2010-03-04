package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.nio.ByteBuffer;
import java.util.Vector;

import WildMagic.LibGraphics.Rendering.GraphicsImage;


public class SurfaceExtractImage extends GraphicsImage
{
    /**  */
    private static final long serialVersionUID = -8778385831984000534L;
    public Vector<int[]> TriTable;
    
    public int[] Data;

    public int Max, Min;
    /** Construct a 3D image (byte)
     * @param eFormat image format
     * @param iBound0 image dimension 1.
     * @param iBound1 image dimension 2.
     * @param iBound2 image dimension 3.
     * @param aucData image data.
     * @param acImageName image name.
     */
    public SurfaceExtractImage (FormatMode eFormat, int iBound0, int iBound1, int iBound2,
                          byte[] aucData, String acImageName)
    {
        super(eFormat,iBound0,iBound1,iBound2,aucData,acImageName);
        TriTable = new Vector<int[]>();
        Data = new int[iBound0*iBound1*iBound2];
        Min = Integer.MAX_VALUE;
        Max = Integer.MIN_VALUE;
    }
    public void dispose()
    {
        Data = null;
        TriTable.clear();
        TriTable = null;
        super.dispose();
    }
    public void SetData ( ByteBuffer kBuffer, int iZ )
    {
        byte[] afData = kBuffer.array();
        int iType = 0;
        int iDataIndex;
        if ( (m_aucData != null) && (afData != null) )
        {
            for ( int y = 0; y < m_aiBound[1]; y++ )
            {
                for ( int x = 0; x < m_aiBound[0]; x++ )
                {
                    iDataIndex = iZ * m_aiBound[0] * m_aiBound[1] + y * m_aiBound[0] + x;
                    Data[iDataIndex] = afData[y * m_aiBound[0] * 4 + x*4 ] & 0xff;
                    if (Data[iDataIndex] > Max)
                    {
                        Max = Data[iDataIndex];
                    }
                    if (Data[iDataIndex] < Min)
                    {
                        Min = Data[iDataIndex];
                    }
                    iType = afData[y * m_aiBound[0] * 4 + x*4 + 3] & 0xff;
                    if ( iType != 0 )
                    {
                        //System.err.println( iType  );
                        TriTable.add( new int[]{x,y,iZ,iType} );
                    }

                    m_aucData[iDataIndex] = 
                        afData[y * m_aiBound[0] * 4 + x*4];
                }
            }
        }
    }
}
