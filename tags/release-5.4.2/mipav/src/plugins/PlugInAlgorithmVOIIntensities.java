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
    	int j, k;
        int nContours;

        Vector<VOIBase> contours;
    
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

        int numPts;
        
        Vector3f currentPt;

        DecimalFormat df = new DecimalFormat("###.##");

        nContours = contours.size();

        if (nContours > 0) {        
            for (j = 0; j < nContours; j++) {
                numPts = ((VOIBase) contours.elementAt(j)).size();
                for (k = 0; k < numPts; k++) {
                    currentPt = ((VOIBase) contours.elementAt(j)).elementAt(k);
                    int x = (int)currentPt.X;
                    int y = (int)currentPt.Y;
                    int z = (int)currentPt.Z;
                    raFile.writeBytes(x + "\t\t" + y + "\t\t" + z);
                    if (srcImage.isColorImage()) {
                        raFile.writeBytes("\t\t" +  df.format(srcImage.getFloatC(x,y,z,1)) + "\t\t" +
                                df.format(srcImage.getFloatC(x,y,z,2)) + "\t\t" +
                                df.format(srcImage.getFloatC(x,y,z,3)) + "\n");
                    } else {
                        raFile.writeBytes("\t\t" +  df.format(srcImage.getFloat(x,y,z)) +  "\n");
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
