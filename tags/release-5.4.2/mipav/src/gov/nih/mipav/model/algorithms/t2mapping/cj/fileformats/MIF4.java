package gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats;

import java.io.*;
import java.lang.*;
import java.lang.Float;
import java.util.*;
import java.text.*;
import java.util.zip.*;
import java.nio.ByteOrder;
import javax.imageio.stream.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.CMRImage;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.Format;

public class MIF4 extends MRIFile {

    public MIF4(String filename) {
		initialize();
        read(filename);
    }

	public void write( String filename ) 
	{
		ImageOutputStream ios = null;

		ios = AppKit.openWrite(filename);

        if (ios == null) {
            System.out.println("MIF4: Could not open file "+filename+" to write.");
            System.exit(0);
        }

		ios.setByteOrder( ByteOrder.LITTLE_ENDIAN );
		writeHeader(ios);

		try {
			for( int slice=0; slice<m_nSlices; slice++) {
				for( int echo=0; echo<m_nEchoes; echo++) {
					imageTail[slice*m_nEchoes + echo].write(ios);
					data.write( ios, slice, echo, datatype );
				}
			}

			ios.close();
		}
		catch( IOException e ) {
			System.out.println("MIF4: Could not write file "+filename);
			System.exit(-1);
		}

	}

    public void writeHeader( ImageOutputStream ios ) 
	{
        try {
			ios.write(m_sMIFId, 0, 20);            // 0
			ios.write(m_sBuffer1, 0, 24);          // 20
			ios.writeShort(m_nMIFVersion);         // 44
			ios.writeShort(m_nMIFSubVersion);      // 46
			ios.write(m_sMIFName, 0, 80);          // 48
			ios.write(m_sReserve1, 0, 4);          // 128
			ios.writeInt(m_nFileDateTime);         // 132
			ios.write(m_sReserve2, 0, 4);          // 136
			ios.writeInt(m_nConversionExeTime);    // 140
			ios.writeShort(m_nConversionVersion);  // 144
			ios.write(m_sSiteDataSet, 0, 32);      // 146
			ios.write(m_sBuffer2, 0, 14);          // 178
//			ios.writeInt(m_nModDateTime);
			ios.write(m_sPatientNumber, 0, 25);    // 192
			ios.write(m_sPatientName, 0, 25);      // 217
			ios.write(m_sPatientInitials, 0, 8);   // 242
			ios.write(m_sPatientBirthDate, 0, 12); // 250
			ios.write(m_sSex, 0, 1);               // 262
			ios.write(m_sHospitalName, 0, 33);     // 263
			ios.write(m_sReserve3, 0, 4);          // 296
			ios.writeInt(m_nScanDateTime);         // 300
			ios.writeInt(m_nScanId);               // 304
			ios.write(m_sBuffer3, 0, 24);          // 308
			ios.writeShort(m_nImages);             // 332
			ios.writeShort(m_nSlices);             // 334
			ios.writeShort(m_nEchoes);             // 336
			ios.writeShort(m_nImageDimX);          // 338
			ios.writeShort(m_nImageDimY);          // 340
			ios.writeShort(m_nBytesPerPixel);      // 342
			ios.writeInt(m_nDefaultSliceGap);      // 344
			ios.writeFloat(m_nNumAverages);        // 348
			ios.write(m_sBuffer4,  0, 4);          // 352
			ios.write(m_sScanOrientation, 0, 1);   // 356
			ios.writeShort(m_nSeriesNum);          // 357
			ios.write(m_sProjectName, 0, 10);      // 359
			ios.writeShort(m_nContrast);           // 369
			ios.writeShort(m_nSwapPF);             // 371
			ios.writeInt(m_nNumPhaseEncodes);      // 373
			ios.writeInt(m_nNumFreqEncodes);       // 377
			ios.write(m_sPulseSeqName, 0, 33);     // 381
			ios.write(m_sMTPulseType, 0, 6);       // 414
			ios.writeFloat(m_nMTPulseWidth);       // 420
			ios.writeFloat(m_nMTPulseAmplitude);   // 424
			ios.writeShort(m_nMTPulse);            // 428
			ios.writeFloat(m_nMTPulseDelay);       // 430
			ios.writeFloat(m_nMTPulseFreqOffset);  // 434
			ios.writeInt(m_nMagnetStrength);       // 438
			ios.writeInt(m_nAutoCentreFreq);       // 442
			ios.writeShort(m_nTransmitGain);       // 446
			ios.writeShort(m_nReceiveGain1);       // 448
			ios.writeShort(m_nReceiveGain2);       // 450
			ios.write(m_sFilters, 0, 32);          // 452
			ios.writeInt(m_nTranslationX);         // 484
			ios.writeInt(m_nTranslationY);         // 488
			ios.writeInt(m_nTranslationZ);         // 492
			ios.writeFloat(m_nRotationX);          // 496
			ios.writeFloat(m_nRotationY);          // 500
			ios.writeFloat(m_nRotationZ);          // 504
			ios.write(m_sBuffer6, 0, 4);           // 508
		}
		catch( EOFException e ) {
			System.out.println(e.getMessage());
			System.out.println("Unexpected end of file");
			System.exit(0);
		}
		catch( IOException e ) {
			System.out.println(e.getMessage());
			System.out.println("Could not read header");
			System.exit(0);
		}
	}

	public void initialize() {
		endian = ByteOrder.LITTLE_ENDIAN;
		datatype = MRIFile.DATATYPE_USHORT;

		m_sMIFId = new byte[ 20 ];
		m_sBuffer1 = new byte[ 24 ];
		m_sMIFName = new byte[ 80 ];
		m_sReserve1 = new byte[ 4 ];
		m_sReserve2 = new byte[ 5 ];
		m_sSiteDataSet = new byte[ 32 ];
		m_sBuffer2 = new byte[ 14 ];
		m_sPatientNumber = new byte[ 25 ];
		m_sPatientName = new byte[ 25 ];
		m_sPatientInitials = new byte[ 8 ];
		m_sPatientBirthDate = new byte[ 12 ];
		m_sSex = new byte[ 1 ];
		m_sHospitalName = new byte[ 33 ];
		m_sReserve3 = new byte[ 4 ];
		m_sBuffer3 = new byte[ 24 ];
		m_sBuffer4 = new byte[ 4 ];
		m_sScanOrientation = new byte[ 1 ];
		m_sProjectName = new byte[ 10 ];
		m_sPulseSeqName = new byte[ 33 ];
		m_sMTPulseType = new byte[ 6 ];
		m_sFilters = new byte[ 32 ];
		m_sBuffer6 = new byte[ 4 ];
	}

    public void read( String filename ) {
		readHeader( filename );
	}

    public void readHeader( String filename ) {

		ImageInputStream ldis = null;
		ldis = AppKit.openRead(filename);

        if (ldis == null) {
            System.out.println("Could not open file "+filename);
            System.exit(0);
        }

		ldis.setByteOrder( ByteOrder.LITTLE_ENDIAN );

		readHeader(ldis);
	}

    public void readHeader( ImageInputStream ldis ) {
        try {
			ldis.readFully(m_sMIFId, 0, 20);
			ldis.readFully(m_sBuffer1, 0, 24);
			m_nMIFVersion = ldis.readShort();
			m_nMIFSubVersion = ldis.readShort();
			ldis.readFully(m_sMIFName, 0, 80);
			ldis.readFully(m_sReserve1, 0, 4);
			m_nFileDateTime = (int)ldis.readInt();
			ldis.readFully(m_sReserve2, 0, 4);
			m_nConversionExeTime = (int)ldis.readInt();
			m_nConversionVersion = (int)ldis.readShort();
			ldis.readFully(m_sSiteDataSet, 0, 32);
			ldis.readFully(m_sBuffer2, 0, 14);
//			m_nModDateTime = (int)ldis.readInt();
			ldis.readFully(m_sPatientNumber, 0, 25);
			ldis.readFully(m_sPatientName, 0, 25);
			ldis.readFully(m_sPatientInitials, 0, 8);
			ldis.readFully(m_sPatientBirthDate, 0, 12);
			ldis.readFully(m_sSex, 0, 1);
			ldis.readFully(m_sHospitalName, 0, 33);
			ldis.readFully(m_sReserve3, 0, 4);
			m_nScanDateTime = ldis.readInt();
			m_nScanId = ldis.readInt();
			ldis.readFully(m_sBuffer3, 0, 24);
			m_nImages = (int)ldis.readShort();
			m_nSlices = (int)ldis.readShort();
			m_nEchoes = (int)ldis.readShort();
			m_nImageDimX = (int)ldis.readShort();
			m_nImageDimY = (int)ldis.readShort();
			m_nBytesPerPixel = (int)ldis.readShort();
			m_nDefaultSliceGap = ldis.readInt();
			m_nNumAverages = ldis.readFloat();
			ldis.readFully(m_sBuffer4,  0, 4);
			ldis.readFully(m_sScanOrientation, 0, 1);
			m_nSeriesNum = (int)ldis.readShort();
			ldis.readFully(m_sProjectName, 0, 10);
			m_nContrast = (int)ldis.readShort();
			m_nSwapPF = (int)ldis.readShort();
			m_nNumPhaseEncodes = ldis.readInt();
			m_nNumFreqEncodes = ldis.readInt();
			ldis.readFully(m_sPulseSeqName, 0, 33);
			ldis.readFully(m_sMTPulseType, 0, 6);
			m_nMTPulseWidth = ldis.readFloat();
			m_nMTPulseAmplitude = ldis.readFloat();
			m_nMTPulse = (int)ldis.readShort(); 
			m_nMTPulseDelay = ldis.readFloat();
			m_nMTPulseFreqOffset = ldis.readFloat();
			m_nMagnetStrength = ldis.readInt();
			m_nAutoCentreFreq = ldis.readInt();
			m_nTransmitGain = (int)ldis.readShort();
			m_nReceiveGain1 = (int)ldis.readShort();
			m_nReceiveGain2 = (int)ldis.readShort();
			ldis.readFully(m_sFilters, 0, 32);
			m_nTranslationX = ldis.readInt();
			m_nTranslationY = ldis.readInt();
			m_nTranslationZ = ldis.readInt();
			m_nRotationX = ldis.readFloat();
			m_nRotationY = ldis.readFloat();
			m_nRotationZ = ldis.readFloat();
			ldis.readFully(m_sBuffer6, 0, 4);
		}
		catch( EOFException e ) {
			System.out.println(e.getMessage());
			System.out.println("End of file for some reason");
			System.exit(0);
		}
		catch( IOException e ) {
			System.out.println(e.getMessage());
			System.out.println("Could not read header");
			System.exit(0);
		}

		imageTail = new CMRImage[m_nImages];

		//
		//  Now read in the data.
		//
		data = new MCVolumeFile(m_nImageDimY, m_nImageDimX, m_nSlices, m_nEchoes);

		try {
			for( int slice=0; slice<m_nSlices; slice++) {
				for( int echo=0; echo<m_nEchoes; echo++) {
					imageTail[slice*m_nEchoes + echo] = new CMRImage();
					imageTail[slice*m_nEchoes + echo].read(ldis);
					data.read( ldis, slice, echo, datatype);
				}
			}

			ldis.close();
		}
		catch( IOException e ) {
			System.out.println("Could not read data from MIF file.");
			System.exit(-1);
		}
	}

	public void updateHeader(int flags) {
		if ((flags&UPDATE_ROWCOLS) != 0)
		{
			SetImageDimX(data.getNCols());
			SetImageDimY(data.getNRows());
		}
 		if ((flags&UPDATE_SLICES) != 0)
		{
			int newSlices = data.getNSlices();
			int newImages = m_nEchoes * newSlices;
			CMRImage[] newTail = new CMRImage[newImages];

			for(int i=0;i<newSlices;i++)
			{
				for(int j=0;j<m_nEchoes;j++)
				{
					if (i < m_nSlices)
						newTail[i*m_nEchoes+j] = imageTail[i*m_nEchoes+j];
					else
						newTail[i*m_nEchoes+j] = imageTail[(m_nSlices-1)*m_nEchoes+j];
				}
			}

			imageTail = newTail;
			m_nSlices = newSlices;
			m_nImages = newImages;
		}
		if ((flags&UPDATE_CHANNELS) != 0)
		{
			int newEchoes = data.getNChannels();
			int newImages = m_nSlices * newEchoes;
			CMRImage[] newTail = new CMRImage[newImages];

			for(int i=0;i<m_nSlices;i++)
			{
				for(int j=0;j<newEchoes;j++)
				{
					if (j < m_nEchoes)
						newTail[i*newEchoes+j] = imageTail[i*m_nEchoes+j];
					else
						newTail[i*newEchoes+j] = imageTail[(i+1)*m_nEchoes-1];
				}
			}

			imageTail = newTail;
			m_nEchoes = newEchoes;
			m_nImages = newImages;
		}
	}

	public void show() {

		Format f = new Format("");
		f.print(System.out, "%+20s: " + new String(m_sMIFId) + "\n", "m_sMIFId");
		f.print(System.out, "%+20s: " + new String(m_sBuffer1) + "\n", "m_sBuffer1");

		f.print(System.out, "%+20s: " + m_nMIFVersion + "\n", "m_nMIFVersion");
		f.print(System.out, "%+20s: " + m_nMIFSubVersion + "\n", "m_nMIFSubVersion");
		f.print(System.out, "%+20s: " + new String(m_sMIFName) + "\n", "m_sMIFName");
		f.print(System.out, "%+20s: " + new String(m_sReserve1) + "\n", "m_sReserve1");
		f.print(System.out, "%+20s: " + m_nFileDateTime + "\n", "m_nFileDateTime");
		f.print(System.out, "%+20s: " + new String(m_sReserve2) + "\n", "m_sReserve2");
		f.print(System.out, "%+20s: " + m_nConversionExeTime + "\n", "m_nConversionExeTime");
		f.print(System.out, "%+20s: " + m_nConversionVersion + "\n", "m_nConversionVersion");
		f.print(System.out, "%+20s: " + new String(m_sSiteDataSet) + "\n", "m_sSiteDataSet");
		f.print(System.out, "%+20s: " + new String(m_sBuffer2) + "\n", "m_sBuffer2");
		
		f.print(System.out, "%+20s: " + new String(m_sPatientNumber) + "\n", "m_sPatientNumber");
		f.print(System.out, "%+20s: " + new String(m_sPatientName) + "\n", "m_sPatientName");
		f.print(System.out, "%+20s: " + new String(m_sPatientInitials) + "\n", "m_sPatientInitials");
		f.print(System.out, "%+20s: " + new String(m_sPatientBirthDate) + "\n", "m_sPatientBirthDate");
		f.print(System.out, "%+20s: " + new String(m_sSex) + "\n", "m_sSex");
		f.print(System.out, "%+20s: " + new String(m_sHospitalName) + "\n", "m_sHospitalName");
		f.print(System.out, "%+20s: " + new String(m_sReserve3) + "\n", "m_sReserve3");
		f.print(System.out, "%+20s: " + m_nScanDateTime + "\n", "m_nScanDateTime");
		f.print(System.out, "%+20s: " + m_nScanId + "\n", "m_nScanId");
		f.print(System.out, "%+20s: " + new String(m_sBuffer3) + "\n", "m_sBuffer3");

		f.print(System.out, "%+20s: " + m_nImages + "\n", "m_nImages");
		f.print(System.out, "%+20s: " + m_nSlices + "\n", "m_nSlices");
		f.print(System.out, "%+20s: " + m_nEchoes + "\n", "m_nEchoes");
		f.print(System.out, "%+20s: " + m_nImageDimX + "\n", "m_nImageDimX");
		f.print(System.out, "%+20s: " + m_nImageDimY + "\n", "m_nImageDimY");
		f.print(System.out, "%+20s: " + m_nBytesPerPixel + "\n", "m_nBytesPerPixel");
		f.print(System.out, "%+20s: " + m_nDefaultSliceGap + "\n", "m_nDefaultSliceGap");
		f.print(System.out, "%+20s: " + m_nNumAverages + "\n", "m_nNumAverages");
		f.print(System.out, "%+20s: " + new String(m_sBuffer4) + "\n", "m_sBuffer4");

		f.print(System.out, "%+20s: " + new String(m_sScanOrientation) + "\n", "m_sScanOrientation");
		f.print(System.out, "%+20s: " + m_nSeriesNum + "\n", "m_nSeriesNum");
		f.print(System.out, "%+20s: " + new String(m_sProjectName) + "\n", "m_sProjectName");
		f.print(System.out, "%+20s: " + m_nContrast + "\n", "m_nContrast");
		f.print(System.out, "%+20s: " + m_nSwapPF + "\n", "m_nSwapPF");
		f.print(System.out, "%+20s: " + m_nNumPhaseEncodes + "\n", "m_nNumPhaseEncodes");
		f.print(System.out, "%+20s: " + m_nNumFreqEncodes + "\n", "m_nNumFreqEncodes");
		f.print(System.out, "%+20s: " + new String(m_sPulseSeqName) + "\n", "m_sPulseSeqName");	
		f.print(System.out, "%+20s: " + new String(m_sMTPulseType) + "\n", "m_sMTPulseType");	
		f.print(System.out, "%+20s: " + m_nMTPulseWidth + "\n", "m_nMTPulseWidth");
		f.print(System.out, "%+20s: " + m_nMTPulseAmplitude + "\n", "m_nMTPulseAmplitude");
		f.print(System.out, "%+20s: " + m_nMTPulse + "\n", "m_nMTPulse");
		f.print(System.out, "%+20s: " + m_nMTPulseDelay + "\n", "m_nMTPulseDelay");
		f.print(System.out, "%+20s: " + m_nMTPulseFreqOffset + "\n", "m_nMTPulseFreqOffset");
		f.print(System.out, "%+20s: " + m_nMagnetStrength + "\n", "m_nMagnetStrength");
		f.print(System.out, "%+20s: " + m_nAutoCentreFreq + "\n", "m_nAutoCentreFreq");
		f.print(System.out, "%+20s: " + m_nTransmitGain + "\n", "m_nTransmitGain");
		f.print(System.out, "%+20s: " + m_nReceiveGain1 + "\n", "m_nReceiveGain1");
		f.print(System.out, "%+20s: " + m_nReceiveGain2 + "\n", "m_nReceiveGain2");

		f.print(System.out, "%+20s: " + new String(m_sFilters) + "\n", "m_sFilters");

		f.print(System.out, "%+20s: " + m_nTranslationX + "\n", "m_nTranslationX");
		f.print(System.out, "%+20s: " + m_nTranslationY + "\n", "m_nTranslationY");
		f.print(System.out, "%+20s: " + m_nTranslationZ + "\n", "m_nTranslationZ");
		f.print(System.out, "%+20s: " + m_nRotationX + "\n", "m_nRotationX");
		f.print(System.out, "%+20s: " + m_nRotationY + "\n", "m_nRotationY");
		f.print(System.out, "%+20s: " + m_nRotationZ + "\n", "m_nRotationZ");
		f.print(System.out, "%+20s: " + new String(m_sBuffer6) + "\n", "m_sBuffer6");

		for(int ii=0; ii<m_nImages; ii++) {
			System.out.println("-----------------------------");
			System.out.println("Image " + (ii+1) );
			imageTail[ii].show();
		}
		
	}

	public double[] getAllTE()
	{
		double[] teout = new double[ GetEchoes() ];

		for(int ii=0; ii<GetEchoes(); ii++)
		{
			teout[ii] = imageTail[ii].getTE();
		}

		return teout;
	}

	public double[] getTE()
	{
		System.out.println("MIF4: Not sure if the TE's are right!");
		double[] telist = new double[ m_nEchoes ];
		telist[0] = imageTail[0].getTE();
		telist[1] = imageTail[1].getTE();
		return telist;
	}

	public String GetMIFId() { return new String(m_sMIFId); };
	public String GetBuffer1() { return new String(m_sBuffer1);};
	public int GetMIFVersion() { return m_nMIFVersion;};
	public int GetMIFSubVersion() { return m_nMIFSubVersion;};
	public String GetMIFName() { return new String(m_sMIFName);};
	public String GetReserve1() { return new String(m_sReserve1);};
	public int GetFileDateTime() { return m_nFileDateTime;};
	public String GetReserve2() { return new String(m_sReserve2);};
	public int GetConversionExeTime() { return m_nConversionExeTime;};
	public int GetConversionVersion() { return m_nConversionVersion;};
	public String GetSiteDataSet() { return new String(m_sSiteDataSet);};
	public String GetBuffer2() { return new String(m_sBuffer2);};
	public String GetPatientNumber() { return new String(m_sPatientNumber);};
	public String GetPatientName() { return new String(m_sPatientName);};
	public String GetPatientInitials() { return new String(m_sPatientInitials);};
	public String GetPatientBirthDate() { return new String(m_sPatientBirthDate);}
	public String GetSex() { return new String(m_sSex);};
	public String GetHospitalName() { return new String(m_sHospitalName);};
	public String GetReserve3() { return new String(m_sReserve3);};
	public int GetScanDateTime() { return m_nScanDateTime;};
	public int GetScanId() { return m_nScanId;};
	public String GetBuffer3() { return new String(m_sBuffer3);};
	public int GetImages_size() { return m_nImages;};
	public int GetSlices() { return m_nSlices;};
	public int GetEchoes() { return m_nEchoes;};
	public int GetImageDimX() { return m_nImageDimX;};
	public int GetImageDimY() { return m_nImageDimY;};
	public int GetBytesPerPixel() { return m_nBytesPerPixel;};
	public int GetDefaultSliceGap() { return m_nDefaultSliceGap;};
	public float GetNumAverages() { return m_nNumAverages;};
	public String GetBuffer4() { return new String(m_sBuffer4);};
	public String GetScanOrientation() { return new String(m_sScanOrientation);};
	public int GetSeriesNum() { return m_nSeriesNum;};
	public String GetProjectName() { return new String(m_sProjectName);};
	public int GetContrast() { return m_nContrast;};
	public int GetSwapPF() { return m_nSwapPF;};
	public int GetNumPhaseEncodes() { return m_nNumPhaseEncodes;};
	public int GetNumFreqEncodes() { return m_nNumFreqEncodes;};
	public String GetPulseSeqName() { return new String(m_sPulseSeqName);};
	public String GetMTPulseType() { return new String(m_sMTPulseType);};
	public float GetMTPulseWidth() { return m_nMTPulseWidth;};
	public float GetMTPulseAmplitude() { return m_nMTPulseAmplitude;};
	public int GetMTPulse() { return m_nMTPulse;};
	public float GetMTPulseDelay() { return m_nMTPulseDelay;};
	public float GetMTPulseFreqOffset() { return m_nMTPulseFreqOffset;};
	public int GetMagnetStrength() { return m_nMagnetStrength;};
	public int GetAutoCentreFreq() { return m_nAutoCentreFreq;};
	public int GetTransmitGain() { return m_nTransmitGain;};
	public int GetReceiveGain1() { return m_nReceiveGain1;};
	public int GetReceiveGain2() { return m_nReceiveGain2;};
	public String GetFilters() { return new String(m_sFilters);};
	public int GetTranslationX() { return m_nTranslationX;};
	public int GetTranslationY() { return m_nTranslationY;};
	public int GetTranslationZ() { return m_nTranslationZ;};
	public float GetRotationX() { return m_nRotationX;};
	public float GetRotationY() { return m_nRotationY;};
	public float GetRotationZ() { return m_nRotationZ;};
	public String GetBuffer6() { return new String(m_sBuffer6);};

	private void SetImageDimX(int in)
	{
		m_nImageDimX = in;
		for(int i=0;i<imageTail.length;i++) imageTail[i].Xdims = in;
	}

	private void SetImageDimY(int in)
	{
		m_nImageDimY = in;
		for(int i=0;i<imageTail.length;i++) imageTail[i].Ydims = in;
	}

	public int GetPixelX() { return imageTail[0].m_nPixelSizeX; }
	public int GetPixelY() { return imageTail[0].m_nPixelSizeY; }
	public int GetSliceThick() { return imageTail[0].m_nSliceThickness; }

	public void SetPixelX(int in)
	{
		for(int i=0;i<imageTail.length;i++) imageTail[i].m_nPixelSizeX = in;
	}

	public void SetPixelY(int in)
	{
		for(int i=0;i<imageTail.length;i++) imageTail[i].m_nPixelSizeY = in;
	}

	public void SetSliceThick(int in)
	{
		for(int i=0;i<imageTail.length;i++) imageTail[i].m_nSliceThickness = in;
	}

	// #<<<<<<<MIFS>>>>>>>#
	private byte[] 	m_sMIFId;	

	// Buffer
	private byte[] 	m_sBuffer1;	

	// MIF Version
	private int 	m_nMIFVersion;	

	// MIF Sub Version
	private int 	m_nMIFSubVersion;	

	// MIF file name
	private byte[] 	m_sMIFName;	

	// Reserved for possible increase in time_t
	private byte[]	m_sReserve1;

	// File Date and Time
	private int 	m_nFileDateTime;	

	// Reserved for possible increase in time_t
	private byte[]	m_sReserve2;

	// Conversion Executable Time
	private int 	m_nConversionExeTime;	

	// Site Data Set
	private byte[] m_sSiteDataSet;

	// Conversion Executable Version
	private int 	m_nConversionVersion;	

	// Buffer
	private byte[] 	m_sBuffer2;	

	// Patient Number
	private byte[] 	m_sPatientNumber;	

	// Patient Name
	private byte[] 	m_sPatientName;	

	// Patient Initials
	private byte[] 	m_sPatientInitials;	

	// Patient Birthdate
	private byte[] 	m_sPatientBirthDate;	

	private int m_nModDateTime;

	// Patient Sex
	private byte[] 	m_sSex;

	// Hospital Name
	private byte[]	m_sHospitalName;

	// Reserved for possible increase in time_t
	private byte[]	m_sReserve3;

	// Scan Date and Time
	private int 	m_nScanDateTime;	

	// Scan ID (Exam # in GE parlance
	private int 	m_nScanId;	

	// Buffer
	private byte[] 	m_sBuffer3;	

	// Number of images
	private int 	m_nImages;

	// Number of slices
	private int	m_nSlices;

	// Number of Echoes
	private int 	m_nEchoes;

	// Dimensions X
	private int 	m_nImageDimX;	

	// Dimensions Y
	private int 	m_nImageDimY;

	// Bytes per pixel
	private int 	m_nBytesPerPixel;	

	// Slice Gap (um
	private int 	m_nDefaultSliceGap;	

	// Number of averages
	private float 	m_nNumAverages;	

	// Buffer
	private byte[] 	m_sBuffer4;	

	// Scan Orientation
	private byte[] 	m_sScanOrientation;	

	// Series Number
	private int 	m_nSeriesNum;	

	// Project Name
	private byte[] 	m_sProjectName;	

	// Contrast Enhanced?
	private int 	m_nContrast;	

	// Swap Phase and Frequency
	private int 	m_nSwapPF;	

	// Number of phase encodes
	private int 	m_nNumPhaseEncodes;	

	// Number of frequency encodes
	private int 	m_nNumFreqEncodes;	

	// Name of the pulse sequence (Research
	private byte[] 	m_sPulseSeqName;	

	// type of MT pulse (BINOM, SINC
	private byte[] 	m_sMTPulseType;	

	// width of MT pulse (us
	private float 	m_nMTPulseWidth;	

	// amplitude of MT pulse
	private float 	m_nMTPulseAmplitude;	

	// number of MT pulses
	private int 	m_nMTPulse;	

	// delay of MT pulse (us
	private float 	m_nMTPulseDelay;	

	// frequency offset of MT pulse 
	private float 	m_nMTPulseFreqOffset;

	// main magnetic field strength
	private int		m_nMagnetStrength;

	// auto centre frequency
	private int		m_nAutoCentreFreq;

	// transmit gain
	private int	m_nTransmitGain;

	// receive gain 1
	private int	m_nReceiveGain1;

	// receive gain 2
	private int	m_nReceiveGain2;

	// Filter Name
	private byte[] 	m_sFilters;	

	// Translation X
	private int 	m_nTranslationX;	

	// Translation Y
	private int	m_nTranslationY;	

	// Translation Z
	private int 	m_nTranslationZ;	

	// Rotation X
	private float 	m_nRotationX;	

	// Rotation Y
	private float 	m_nRotationY;	

	// Rotation Z
	private float 	m_nRotationZ;	

	// Buffer
	private byte[] 	m_sBuffer6;	


	private CMRImage[] imageTail;
}
