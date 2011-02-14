package gov.nih.mipav.model.file.rawjp2;

import java.io.*;

import jj2000.j2k.io.*;

//import pluginJP2.jj2000.j2k.io.BEBufferedRandomAccessFile;
/** 3D JP2 image header reader/writer.
 * @version 0.01, Mar *, 2007
 * @author Dzung Nguyen
 *
 */
public class RAWJP2Header {
	protected boolean bigEndian = true;
	protected int numOfSlices;
	protected int[] imgExtents = {256,256,124,1};
	protected float[] imgResolution ={1,1,1} ;
	protected int imgType;
	protected int imgModality;
	protected int imgOrientation;
	protected int[] sizeArray;
	private int[] ptArray;
	private int arrayPos;
	protected boolean is2D = false;

	protected void calPtArr(){
//		ptArray = null;
		if(is2D) {
			numOfSlices = 1;
		}else {
			numOfSlices = imgExtents[2];
		}
		ptArray = new int[numOfSlices];
//		JOptionPane.showMessageDialog(null, "calPtArr 1", "Info", JOptionPane.INFORMATION_MESSAGE);
		ptArray[0] = arrayPos + numOfSlices * 4;
		for (int i=1;i<numOfSlices;i++){
			ptArray[i] = ptArray[i-1] + sizeArray[i-1];
		}
		
	}

	public RAWJP2Header() {
		if(is2D) {
			numOfSlices = 1;
		}else {
			numOfSlices = imgExtents[2];
		}
//		imgExtents = new int[4];
//		imgExtents = ;
//		imgExtents = {256,256,124,0};
		sizeArray = new int[numOfSlices];
		ptArray = new int[numOfSlices];
		arrayPos = 1+1+1+7*4;//size of imgExtents + imgResolutions + imgType+imOrientation+imgModality
	}
	public int[] getImgExtents() {
		return imgExtents;
	}
	public int setImgExtents(int[] s) {
		if (s.length < 3){
	//		return -1;
		}
				
		imgExtents[0] = s[0];imgExtents[1] = s[1];
		
		if (s.length >= 3)
		imgExtents[2] = s[2];
		
		numOfSlices= imgExtents[2];
		
//		calPtArr();
//		JOptionPane.showMessageDialog(null, "setImgExtents 3", "Info", JOptionPane.INFORMATION_MESSAGE);		
		return 0;
	}
	public void setNumOfSlices(int numOfSlices) {
		this.numOfSlices = numOfSlices;
		this.imgExtents[2] = numOfSlices;
//		calPtArr();
	}
	public int[] getSizeArray() {
		return sizeArray;
	}
	public int setSizeArray(int[] p) {
		numOfSlices = p.length;
		imgExtents[2] = numOfSlices;
		this.sizeArray = p;
		return 0;
	}
	public int getNumOfSlices() {
		return numOfSlices;
	}

	public int setSize(int p, int idx) {
		if(!is2D) {
			if (idx>=numOfSlices || idx<0) return -1;
		}
		this.sizeArray[idx] = p;
		return 0;
	}
	public int getSize(int idx) {
		return this.sizeArray[idx];
	}

	public boolean isBigendian() {
		return bigEndian;
	}
	public void setBigendian(boolean bigendian) {
		this.bigEndian = bigendian;
	}
	public void writeRawJP2Header(BEBufferedRandomAccessFile f) throws IOException{
		int curPos = f.getPos();
		f.seek(0);
		f.writeByte(imgType);
		f.writeInt(imgExtents[0]);
		f.writeInt(imgExtents[1]);
		if(is2D) {
			f.writeInt(0);
			f.writeInt(0);
		}else {
			f.writeInt(imgExtents[2]);
			f.writeInt(imgExtents[3]);
		}
		f.writeFloat(imgResolution[0]);
		f.writeFloat(imgResolution[1]);
		if(is2D) {
			f.writeFloat(0);
		}else {
			f.writeFloat(imgResolution[2]);
		}
		f.writeByte(imgOrientation);
		f.writeByte(imgModality);

//		f.writeInt(numOfSlices);
		for( int i=0;i<numOfSlices;i++) {
			f.writeInt(sizeArray[i]);
		}
		int intSize = 4;
//		if (curPos >= 4*(numOfSlices+1)) f.seek(curPos);
		if (curPos >= intSize*(numOfSlices)+arrayPos) f.seek(curPos);
	}
	public /*BEBufferedRandomAccessFile*/FileInputStream readRawJP2Header(String fName){
		/*		try {
			File f1 = new File(fName);
			FileInputStream f = new FileInputStream(f1);
			numOfSlices = 1;
			sizeArray = new int[numOfSlices];
			sizeArray[0]= (int)f1.length();
			return f;
		} catch (IOException e) {
			System.out.println("Error reading header of the compressed file!");
			return null;
		}
*/		

		try {
			FileInputStream f = new FileInputStream(fName);
			imgType = (int)f.read();
			imgExtents[0] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();
			imgExtents[1] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();
			imgExtents[2] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();
			imgExtents[3] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();
			imgResolution[0] = Float.intBitsToFloat((f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read());
			imgResolution[1] = Float.intBitsToFloat((f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read());
			imgResolution[2] = Float.intBitsToFloat((f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read());
			if(imgExtents[2] == 0 && imgExtents[3] == 0 && imgResolution[2] == 0) {
				is2D = true;
				numOfSlices = 1;
			}else {
				is2D = false;
				numOfSlices =imgExtents[2];
			}
			imgOrientation = (int)f.read();
			imgModality = (int)f.read();
			
//			numOfSlices = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();//f.readInt();
			sizeArray = null;
			if(is2D) {
				sizeArray = new int[1];
				sizeArray[0] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();//f.readInt();
			}else {
				sizeArray = new int[numOfSlices];
				for (int i=0;i<numOfSlices;i++){
					sizeArray[i] = (f.read()<<24) | (f.read()<<16) | (f.read()<<8) | f.read();//f.readInt();
				}
			}
//			JOptionPane.showMessageDialog(null, "Reading size arr OK!", "Debug", JOptionPane.INFORMATION_MESSAGE);		
			calPtArr();
			return f; 
		} catch (IOException e) {
			System.out.println("Error reading header of the compressed file!");
			return null;
		}
//*/
	}
	public int getSlicePos(int idx) {
		return ptArray[idx];
	}

	public int getImgType() {
		return imgType;
	}

	public void setImgType(int imgType) {
		this.imgType = imgType;
	}

	public float[] getImgResolution() {
		return imgResolution;
	}

	public void setImgResolution(float[] imgResolution) {
		this.imgResolution = imgResolution;
	}

	public int getImgModality() {
		return imgModality;
	}

	public void setImgModality(int imgModality) {
		this.imgModality = imgModality;
	}

	public int getImgOrientation() {
		return imgOrientation;
	}

	public void setImgOrientation(int imgOrientation) {
		this.imgOrientation = imgOrientation;
	}
	
	public void setIs2D(boolean is2D) {
		this.is2D = is2D;
	}
	
	public boolean getIs2D() {
		return is2D;
	}
}
