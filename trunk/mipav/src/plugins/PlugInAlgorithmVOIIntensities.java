import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import java.io.*;
import java.text.*;
import java.util.Vector;

public class PlugInAlgorithmVOIIntensities
    extends AlgorithmBase {
    
    private File file = null;
    private VOI voi = null;
    private RandomAccessFile raFile;
    
    /**
     *   @param destImg   image model where result image is to stored
     *   @param srcImage  source image model
     */
    public PlugInAlgorithmVOIIntensities(ModelImage srcImg, VOI voi, File file) {
        this.srcImage = srcImg;
        this.voi = voi;
        this.file = file;
    }

    /**
     *   Prepares this class for destruction
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     *   Starts the program
     */
    public void runAlgorithm() {
        writeIntensities();
    }

    private void writeIntensities() {
    	int i, j, k;
        int length;
        int nContours;

        Vector[] contours;
    
        try {
        
        if (file.exists() == true) {
        	String path = file.getPath();
            file.delete();
            file = new File(path);
            raFile = new RandomAccessFile(file, "rw");
        } else {
            raFile = new RandomAccessFile(file, "rw");
        }

        if (srcImage.isColorImage()) {
        	raFile.writeBytes("x\t\ty\t\tz\t\tr\t\tg\t\tb\n");
        } else {
        	raFile.writeBytes("x\t\ty\t\tz\t\tintensity\n");
        }
        contours = voi.getCurves();
        length = voi.getCurves().length;

        int numPts;
        
        Vector3f currentPt;
        
        DecimalFormat df = new DecimalFormat("###.##");
        
        int colorFactor = 1;
        if (srcImage.isColorImage()) {
        	colorFactor = 4;
        }
        
        float [] imageBuffer = new float[srcImage.getSliceSize() * colorFactor];
        int bufLen = imageBuffer.length;
        int xDim = srcImage.getExtents()[0];
        int x, y;
        int colorLoc;
        for (i = 0; i < length; i++) {
            nContours = contours[i].size();

            if (nContours > 0) {
            	if (srcImage.isColorImage()) {
            		srcImage.exportData(bufLen * i, bufLen, imageBuffer);
            	} else {
            		srcImage.exportSliceXY(i, imageBuffer);
            	}
            	for (j = 0; j < nContours; j++) {

                	numPts = ((VOIBase) contours[i].elementAt(j)).size();
                	for (k = 0; k < numPts; k++) {
                		currentPt = (Vector3f)((VOIBase) contours[i].elementAt(j)).elementAt(k);
                		x = (int)currentPt.X;
                		y = (int)currentPt.Y;
                		raFile.writeBytes((int)currentPt.X + "\t\t" + (int)currentPt.Y + "\t\t" + (i+1));
                		if (srcImage.isColorImage()) {
                			colorLoc = 4 * ((y * xDim) + x);
                			raFile.writeBytes("\t\t" +  df.format(imageBuffer[colorLoc + 1]) + "\t\t" +
                					df.format(imageBuffer[colorLoc + 2]) + "\t\t" +
                					df.format(imageBuffer[colorLoc + 3]) + "\n");
                		} else {
                			raFile.writeBytes("\t\t" +  df.format(imageBuffer[(y * xDim) + x]) +  "\n");
                		}
                	}
                }
            }           
        }
    
        raFile.close();
        }
        
        catch (IOException e) {
        	
        }
        
        setCompleted(true);
        notifyListeners(this);
        
    }
}
