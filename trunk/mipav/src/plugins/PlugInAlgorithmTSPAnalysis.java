//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Collection;
import java.util.Vector;



public class PlugInAlgorithmTSPAnalysis extends AlgorithmBase {
    
    private int xDim;

    private int yDim;

    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    private Collection<ModelImage> resultImageList;
    
    private String pwiImageFileDirectory;
    
    // Thresholding to mask out image pixels not corresponding to brain tissue
    private int masking_threshold = 800;
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmTSPAnalysis(String pwiImageFileDirectory, int masking_threshold) {
        //super(resultImage, srcImg);
    	this.pwiImageFileDirectory = pwiImageFileDirectory;
    	this.masking_threshold = masking_threshold;
    }

    
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int tDim;
    	int length;
    	int volume;
    	int dataSize;
    	int extents[] = new int[4];
    	String tDimString = null;
    	String t0String = null;
    	float t0;
    	String t1String = null;
    	float t1;
    	float delT;
    	int buffer[];
    	int data[][][][];
    	int x, y, z, t;
    	File folder = new File(pwiImageFileDirectory);
    	int selectedFileNumber = 0;
    	for (File fileEntry : folder.listFiles()) {
    		if (!fileEntry.isDirectory()) {
    			if (fileEntry.getName().length() > 2) {
    			    String startName = fileEntry.getName().substring(0,2);
    			    if (startName.equalsIgnoreCase("IM")) {
    			    	selectedFileNumber++;
    			    }
    			}
    		}
    	}
    	String fileList[] = new String[selectedFileNumber];
    	int index = 0;
    	for (File fileEntry : folder.listFiles()) {
    		if (!fileEntry.isDirectory()) {
    			if (fileEntry.getName().length() > 2) {
    			    String startName = fileEntry.getName().substring(0,2);
    			    if (startName.equalsIgnoreCase("IM")) {
    			    	fileList[index++] = fileEntry.getName();
    			    }
    			}
    		}
    	}
    	String selectedFileName = fileList[0];
    	FileIO fileIO = new FileIO(); 
    	fileIO.setFileDir(pwiImageFileDirectory + File.separator);
    	boolean performSort = true;
    	fileIO.setQuiet(false);
    	ModelImage image3D = fileIO.readDicom(selectedFileName, fileList, performSort);
    	image3D.calcMinMax();
    	//int imageIndex = 0;
    	//new ViewJFrameImage(vols2, null, new Dimension(610, 200 + (imageIndex++ * 20)));
    	int extents3D[] = image3D.getExtents();
    	length = extents[0] * extents[1];
    	xDim = extents[0];
    	yDim = extents[1];
    	FileInfoDicom dicomInfo = (FileInfoDicom) image3D.getFileInfo(0);
    	FileDicomTagTable tagTable = dicomInfo.getTagTable();
        if (tagTable.getValue("0020,0105") != null) {
        	// Number of temporal positions
            FileDicomTag tag = tagTable.get(new FileDicomKey("0020,0105"));
            tDimString = (String)tag.getValue(false);     
        }
        else {
        	MipavUtil.displayError("Tag (0020,0105) for Number of Temporal Positions is null");
        	setCompleted(false);
        	return;
        }
        tDim = Integer.valueOf(tDimString.trim()).intValue();
        extents[3] = tDim;
        zDim = extents3D[2]/tDim;
        extents[2] = zDim;
        //System.out.println("zDim = " + zDim + " tDim = " + tDim);
        if (tagTable.getValue("0018,1060") != null) {
        	// Trigger time
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1060"));
        	t0String = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,1060) for Trigger Time is null");
        	setCompleted(false);
        	return;
        }
        t0 = Float.valueOf(t0String.trim()).floatValue();
        dicomInfo = (FileInfoDicom) image3D.getFileInfo(1);
        tagTable = dicomInfo.getTagTable();
        if (tagTable.getValue("0018,1060") != null) {
        	// Trigger time
        	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1060"));
        	t1String = (String)tag.getValue(false);
        }
        else {
        	MipavUtil.displayError("Tag (0018,1060) for Trigger Time is null");
        	setCompleted(false);
        	return;
        }
        t1 = Float.valueOf(t1String.trim()).floatValue();
        delT = t1 - t0;
        //System.out.println("delT = " + delT);
        volume = zDim * length;
        dataSize = volume * tDim;
        buffer = new int[dataSize];
    	setCompleted(true);
    	try {
    		image3D.exportData(0,  dataSize, buffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException on image3D.exportData");
    		setCompleted(false);
    		return;
    	}
    	data = new int[xDim][yDim][zDim][tDim];
    	for (t = 0; t < tDim; t++) {
    		for (z = 0; z < zDim; z++) {
    			for (y = 0; y < yDim; y++) {
    				for (x = 0; x < xDim; x++) {
    					data[x][y][z][t] = buffer[x + y*xDim + t*length +z*tDim*length];
    				}
    			}
    		}
    	}
    	
    } // end runAlgorithm()
    
    

//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }

}
