package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * Reads a .trk track file as a MIPAV mask image.
 * 
 * @author justinsenseney
 *
 */
public class FileTrackVis extends FileBase {

	/** Fileinfo for image */
	private FileInfoTrackVis fileInfo;
	/** Number of scalars for each datapoint */
	private short nScalars;
	/** Number of properties associated with each track */
	private short nProperties;
	/** If 0 then # of tracks is unknown */
	private int numTracks;
	/** Created image */
	private ModelImage image;
	/** Transformation matrix for image */
	private TransMatrix trans;
	
	public FileTrackVis(String fileName, String fileDir) throws IOException {
		this.fileNames = new String[1];
		fileNames[0] = fileDir + File.separator + fileName;
	}
	
	/**
     * Reads TrackVis header, first 1000 bytes of TRK file.
     *
     * @exception  IOException  if there is an error reading the file
     */
	public boolean readHeader() throws IOException {
		try {
			File file = new File(fileNames[0]);
			raFile = new RandomAccessFile(file, "r");
			
			fileInfo = new FileInfoTrackVis(file.getName(), file.getParent(), FileUtility.TRACKVIS);

			boolean bigEndian = false;
			
			String name = this.readString(6);
			if(!name.substring(0, 5).toUpperCase().equals("TRACK")) { 
				return false;
			}
			
			int[] extents = new int[4];
			extents[0] = this.readShort(bigEndian);
			extents[1] = this.readShort(bigEndian);
			extents[2] = this.readShort(bigEndian);
			
			float[] res = new float[3];
			res[0] = this.readFloat(bigEndian);
			res[1] = this.readFloat(bigEndian);
			res[2] = this.readFloat(bigEndian);
			fileInfo.setResolutions(res);
			
			float[] origin = new float[3]; //TODO: TrackVis always uses 0,0,0 for origin, test when this is
			origin[0] = this.readFloat(bigEndian); //better implemented by TrackVis
			origin[1] = this.readFloat(bigEndian);
			origin[2] = this.readFloat(bigEndian);
			fileInfo.setOrigin(origin);
			
			nScalars = this.readShort(bigEndian);
			fileInfo.setNumScalar(nScalars);
			extents[3] = nScalars;
			int extentLength = extents[3] != 0 ? 4 : 3;
			fileInfo.setExtents(Arrays.copyOf(extents, extentLength));
			
			int scalarNameNum = 10;//nScalars <= 10 ? nScalars : 10;
			String[] scalarName = new String[scalarNameNum];
			for(int i=0; i<scalarName.length; i++) {
				scalarName[i] = this.readString(20);
			}
			fileInfo.setScalarNames(scalarName);
			
			nProperties = this.readShort(bigEndian);
			fileInfo.setNumProp(nProperties);
			
			int propertyNameNum = 10;
			String[] propertyName = new String[propertyNameNum];
			for(int i=0; i<propertyName.length; i++) {
				propertyName[i] = this.readString(20);
			}
			fileInfo.setPropNames(propertyName);
			
			int size = 4;
			float[][] transMatrix = new float[size][]; //In RAS space
			
			for(int i=0; i<size; i++) {
				transMatrix[i] = new float[size];
				for(int j=0; j<size; j++) {
					transMatrix[i][j] = this.readFloat(bigEndian);
				}
			}
			
			trans = new TransMatrix(4, 4);
			for(int i=0; i<size; i++) {
				for(int j=0; j<size; j++) {
					trans.set(i,  j, transMatrix[i][j]);
				}
			}
			if(transMatrix[3][3] == 0) {  //transmatrix not set
				trans.MakeIdentity();
			}
			
			String reserved = this.readString(444);
			fileInfo.setReserved(reserved);
			
			String voxelOrder = this.readString(4); //often LAS
			fileInfo.setVoxelOrder(voxelOrder);
			
			String pad2 = this.readString(4); //often LAS
			fileInfo.setPad2(pad2);
			
			float[] imageOrientation = new float[6];  //meets DICOM standard, usually {1, 0, 0, 0, 1, 0}
			for(int i=0; i<imageOrientation.length; i++) {
				imageOrientation[i] = this.readFloat(bigEndian);
			}
			
			String pad1 = this.readString(2); 
			fileInfo.setPad1(pad1);
			
			String invertXStr = this.readString(1);
			String invertYStr = this.readString(1);
			String invertZStr = this.readString(1);
			fileInfo.setInvertXStr(invertXStr);
			fileInfo.setInvertYStr(invertYStr);
			fileInfo.setInvertZStr(invertZStr);
			
			String swapXY = this.readString(1);
			String swapYZ = this.readString(1);
			String swapZX = this.readString(1);
			fileInfo.setSwapXY(swapXY);
			fileInfo.setSwapYZ(swapYZ);
			fileInfo.setSwapZX(swapZX);
			
			numTracks = this.readInt(bigEndian);
			fileInfo.setNumTracks(numTracks);
			
			int version = this.readInt(bigEndian);
			fileInfo.setVersion(version);
			
			int hdrSize = this.readInt(bigEndian);
			fileInfo.setHdrSize(hdrSize);
			if(hdrSize < 1000) {
				fileInfo.setHdrSize(1000);
			}
			
			if(raFile.getFilePointer() != 1000 && hdrSize != raFile.getFilePointer()) {
				Preferences.debug("TrackVis file reading error incorrect location", Preferences.DEBUG_FILEIO);
			}
			
			fileInfo.setModality(FileInfoBase.MAGNETIC_RESONANCE);
			
			System.out.println("Current location: "+raFile.getFilePointer());
			
		} catch(IOException io) {
			MipavUtil.displayError("Error reading trackvis file.");
		}
		
		return true;
	}
	
	/**
	 * Reads TrackVis series of tracks and creates masks from them, based on stored fileInfo.
	 */
	public ModelImage readImage() {
		boolean bigEndian = false;
		
		if(image != null) {
			image.disposeLocal();
		}
		
		image = new ModelImage(ModelStorageBase.USHORT, Arrays.copyOf(fileInfo.getExtents(), fileInfo.getExtents().length), fileInfo.getFileName());
		for(int i=0; i<image.getFileInfo().length; i++) {
			image.setFileInfo(fileInfo, i);
		}
		image.setMatrix(trans);
		
		try {
			ArrayList<float[]> trackProps = new ArrayList<float[]>(); 
			raFile.seek(fileInfo.getHdrSize());
			if(numTracks < 1) {
				numTracks = Integer.MAX_VALUE;
			}
			
			int numData = 3 + nScalars;
			float x, y, z;
			int xInt, yInt, zInt;
			int[] points = new int[fileInfo.getExtents().length];
			float data;
			int trackCounter = 0;
		
			float xRes = image.getResolutions(0)[0];
			float yRes = image.getResolutions(0)[1];
			float zRes = image.getResolutions(0)[2];
			
			VOI v = new VOI((short)4, "Tracts");
			
			
			
			for(int i=0; i<numTracks && raFile.getFilePointer() < raFile.length(); i++) {
				if(numTracks > 0) {
					fireProgressStateChanged((int) ((((double)i) / (numTracks))*100));
				}
				
				//System.out.println("Track "+i);
				VOIContour contour = new VOIContour(false);
				int numPoints = this.readInt(bigEndian);
				for(int j=0; j<numPoints; j++) {
					x = this.readFloat(bigEndian);
					y = this.readFloat(bigEndian);
					z = this.readFloat(bigEndian);
					
					x = x / xRes;
					y = y / yRes;
					z = z / zRes;
					
					contour.add(new Vector3f(x, y, z));
					
					xInt = (int)x; //TODO: Import as VOIContours to preserve float specification
					yInt = (int)y;
					zInt = (int)z;
					points[0] = xInt;
					points[1] = yInt;
					points[2] = zInt;
					if(points.length > 3) {
						points[3] = 0;
					}
					
					int[] extents = image.getExtents();
					int dataPoint = 0, subPoint = 0;
					for(int n=0; n<points.length; n++) {
						subPoint = points[n];
						for(int m=1; m<n+1; m++) {
							subPoint *= extents[m-1];
						}
						dataPoint += subPoint;
					}
					
					image.set(dataPoint, i+1);
					
					short s = image.get(points).shortValue();
					short sTry = image.getShort(xInt, yInt, zInt);
					//System.out.println("S: "+s);
					
					for(int k=3; k<numData; k++) { //in this case must be a 4D dataset
						points[3] = k-3;
						data = this.readFloat(bigEndian);
						//System.out.println("Data at point : "+data);
						image.set(points, (short)data);
					}
				}
				
				long time = System.currentTimeMillis();
				
				v.importCurve(contour);
				
				System.out.println("Time: "+(System.currentTimeMillis() - time));
				
				float[] trackSpecific  = new float[nProperties];
				for(int j=0; j<nProperties; j++) {
					trackSpecific[j] = this.readFloat(bigEndian);
				}
				trackProps.add(trackSpecific);
				
				trackCounter++;
			}	
			
			image.registerVOI(v);
			
			if(trackCounter != numTracks) {
				fileInfo.setNumTracks(trackCounter);
			}
			
			fileInfo.setProperties(trackProps.toArray(new float[trackProps.size()][]));
		
		} catch (IOException e) {
			MipavUtil.displayError("Error reading TrackVis image, IOException occurred");
			e.printStackTrace();
		}
		
		
		
		return image;
	}

	public FileInfoTrackVis getFileInfo() {
		return fileInfo;
	}

	public void setFileInfo(FileInfoTrackVis fileInfo) {
		this.fileInfo = fileInfo;
	}
	
}
