package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.nio.ByteOrder;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;

public class CMRImage implements Cloneable {

	public CMRImage() {
		init();
	}

    public Object clone()
    {
        Object o = null;
        try {
            o = super.clone();
        }
        catch( CloneNotSupportedException e) {
            System.out.println("CMRImage can not clone");
        }
        return o;
    }

	public void init() 
	{
			m_sImageId = new byte[ 16 ];
			m_sSiteDataSet = new byte[ 32 ];
			m_sReserve1 = new byte[ 4 ];
			m_sBuffer1 = new byte[ 28 ];
			m_sSiteDataSet = new byte[ 32 ];
			m_sSlicePosString = new byte[ 1 ];
			m_sBuffer2 = new byte[ 115 ];
	}
	
	public void write( ImageOutputStream ldos ) 
	{
		try {
			ldos.write(m_sImageId, 0, 16);        // 0
			ldos.writeShort(m_nImageVersion);     // 16
			ldos.writeShort(m_nImageSubVersion);  // 18
			ldos.write(m_sSiteDataSet, 0, 32);    // 20
			ldos.writeInt(m_nFileCount);          // 52
			ldos.write(m_sReserve1, 0, 4);        // 56
			ldos.writeInt(m_nImageDateTime);      // 60
			ldos.writeInt(m_nScanId);             // 64
			ldos.write(m_sBuffer1, 0, 28);        // 68
			ldos.writeInt(m_nTE);                 // 96
			ldos.writeInt(m_nTR);                 // 100
			ldos.writeInt(m_nTI);                 // 104
			ldos.write(m_sSlicePosString, 0, 1);  // 108
			ldos.writeFloat(m_fSlicePosition);    // 109
			ldos.writeInt(m_nPixelSizeX);         // 113
			ldos.writeInt(m_nPixelSizeY);         // 117
			ldos.writeShort(Xdims);               // 121
			ldos.writeShort(Ydims);               // 123
			ldos.writeShort(m_nBytesPerPixel);    // 125
			ldos.writeInt(m_nSliceThickness);     // 127
			ldos.writeShort(m_nHeartRate);        // 131
			ldos.writeInt(m_nTriggerDelay);       // 133
			ldos.writeFloat(m_fBandwidth);        // 137
			ldos.write(m_sBuffer2, 0, 115);       // 141
		}
		catch( IOException e ) {
			System.out.println("CMRImage: Could not read in file");
			System.exit(-1);
		}
	}

	public void read( ImageInputStream ldis ) 
	{
		try {
			ldis.readFully(m_sImageId, 0, 16);
			m_nImageVersion = ldis.readShort();
			m_nImageSubVersion = ldis.readShort();
			ldis.readFully(m_sSiteDataSet, 0, 32);
			m_nFileCount = ldis.readInt();
			ldis.readFully(m_sReserve1, 0, 4);
			m_nImageDateTime = ldis.readInt();
			m_nScanId = ldis.readInt();
			ldis.readFully(m_sBuffer1, 0, 28);
			m_nTE = ldis.readInt();
			m_nTR = ldis.readInt();
			m_nTI = ldis.readInt();
			ldis.readFully(m_sSlicePosString, 0, 1);
			m_fSlicePosition = ldis.readFloat();
			m_nPixelSizeX = ldis.readInt();
			m_nPixelSizeY = ldis.readInt();
			Xdims = ldis.readShort();
			Ydims = ldis.readShort();
			m_nBytesPerPixel = ldis.readShort();
			m_nSliceThickness = ldis.readInt();
			m_nHeartRate = ldis.readShort();
			m_nTriggerDelay = ldis.readInt();
			m_fBandwidth = ldis.readFloat();
			ldis.readFully(m_sBuffer2, 0, 115);
		}
		catch( IOException e ) {
			System.out.println("CMRImage: Could not read in file");
			System.exit(-1);
		}
	}

	public void show() {
		gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format f = new gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format("");

		f.print(System.out, "%+20s: " + new String(m_sImageId) + "\n", "m_sImageId");
		f.print(System.out, "%+20s: " + m_nImageVersion + "\n", "m_nImageVersion");
		f.print(System.out, "%+20s: " + m_nImageSubVersion + "\n", "m_nImageSubVersion");
		f.print(System.out, "%+20s: " + new String(m_sSiteDataSet) + "\n", "m_sSiteDataSet");
		f.print(System.out, "%+20s: " + m_nFileCount + "\n", "m_nFileCount");
		f.print(System.out, "%+20s: " + new String(m_sReserve1) + "\n", "m_sReserve1");
		f.print(System.out, "%+20s: " + m_nImageDateTime + "\n", "m_nImageDateTime");
		f.print(System.out, "%+20s: " + m_nScanId + "\n", "m_nScanId");
		f.print(System.out, "%+20s: " + new String(m_sBuffer1) + "\n", "m_sBuffer1");
		f.print(System.out, "%+20s: " + m_nTR + "\n", "m_nTR");
		f.print(System.out, "%+20s: " + m_nTE + "\n", "m_nTE");
		f.print(System.out, "%+20s: " + m_nTI + "\n", "m_nTI");
		f.print(System.out, "%+20s: " + new String(m_sSlicePosString) + "\n", "m_sSlicePosString");
		f.print(System.out, "%+20s: " + m_fSlicePosition + "\n", "m_fSlicePosition");
		f.print(System.out, "%+20s: " + m_nPixelSizeX + "\n", "m_nPixelSizeX");
		f.print(System.out, "%+20s: " + m_nPixelSizeY + "\n", "m_nPixelSizeY");
		f.print(System.out, "%+20s: " + m_nBytesPerPixel + "\n", "m_nBytesPerPixel");
		f.print(System.out, "%+20s: " + m_nSliceThickness + "\n", "m_nSliceThickness");
		f.print(System.out, "%+20s: " + m_nHeartRate + "\n", "m_nHeartRate");
		f.print(System.out, "%+20s: " + m_nTriggerDelay + "\n", "m_nTriggerDelay");
		f.print(System.out, "%+20s: " + m_fBandwidth + "\n", "m_fBandwidth");
		f.print(System.out, "%+20s: " + new String(m_sBuffer2) + "\n", "m_sBuffer2");
	}

	/** Return the echo time in ms.
	 *
	 */
	public final double getTE()
	{
		return (double)m_nTE / 1000.0;
	}

	private byte[]  m_sImageId = null;
	private int   m_nImageVersion;
	private int   m_nImageSubVersion;
	private byte[]  m_sSiteDataSet = null;
	private int    m_nFileCount;
	private byte[]  m_sReserve1 = null; 
	private int    m_nImageDateTime;
	private int    m_nScanId;
	private byte[]  m_sBuffer1 = null;
	private int    m_nTE;
	private int    m_nTR;
	private int    m_nTI;
	int    Xdims;
	int    Ydims;
	private byte[]  m_sSlicePosString = null;
	private float   m_fSlicePosition;
	int    m_nPixelSizeX;
	int    m_nPixelSizeY;
	private int   m_nBytesPerPixel;
	int    m_nSliceThickness;
	private int   m_nHeartRate;
	private int    m_nTriggerDelay;
	private float   m_fBandwidth;
	private byte[]  m_sBuffer2 = null;
}
