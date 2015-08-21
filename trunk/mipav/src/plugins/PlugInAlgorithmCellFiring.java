import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JTextArea;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmCellFiring extends AlgorithmBase {
    
    
    private int xDim;
    
    private int yDim;
    
    private int zDim;
    
    private int length;
    
    private boolean alreadyDisplayed;
    
    private boolean displayInputImage;
    
    private float downSampleXY;
    
    private float downSampleZ;
    
    private boolean displayDownSampleImage;
    
    private boolean saveDownSampleImage;

    private final JTextArea outputTextArea;
    

    public PlugInAlgorithmCellFiring(ModelImage image, boolean alreadyDisplayed, boolean displayInputImage, 
    		float downSampleXY, float downSampleZ, boolean displayDownSampleImage, 
    		boolean saveDownSampleImage, final JTextArea outputTextArea) {
    	super(null, image);
    	this.alreadyDisplayed = alreadyDisplayed;
    	this.displayInputImage = displayInputImage;
    	this.downSampleXY = downSampleXY;
    	this.downSampleZ = downSampleZ;
    	this.displayDownSampleImage = displayDownSampleImage;
    	this.saveDownSampleImage = saveDownSampleImage;
        this.outputTextArea = outputTextArea;
    }

    @Override
    public void runAlgorithm() {
    	
        outputTextArea.append("Running Algorithm v1.0" + "\n");

        final long begTime = System.currentTimeMillis();
        
        if (displayInputImage && (!alreadyDisplayed)) {
        	new ViewJFrameImage(srcImage);
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        zDim = srcImage.getExtents()[2];
        
        if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f)) {
        	int interp = AlgorithmTransform.TRILINEAR;
        	TransMatrix xfrm = new TransMatrix(4);
        	xfrm.identity();
            int newExtents[] = new int[3];
            newExtents[0] = Math.round(downSampleXY * xDim);
            newExtents[1] = Math.round(downSampleXY * yDim);
            newExtents[2] = Math.round(downSampleZ * zDim);
            float oXres = srcImage.getFileInfo(0).getResolutions()[0] * xDim / newExtents[0];
            float oYres = srcImage.getFileInfo(0).getResolutions()[1] * yDim / newExtents[1];
            float oZres = srcImage.getFileInfo(0).getResolutions()[2] * zDim / newExtents[2];
            int units[] = srcImage.getUnitsOfMeasure();
            final boolean doClip = true;
            final boolean doPad = false;
            final boolean doVOI = false;
            final boolean doRotateCenter = false;
            final Vector3f center = new Vector3f();
            final float fillValue = 0.0f;
            final boolean doUpdateOrigin = false;
            final boolean isSATransform = false;
            AlgorithmTransform algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oZres,
            		newExtents[0], newExtents[1], newExtents[2], units,
                    doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.setUseScannerAnatomical(isSATransform);
            algoTrans.setSuppressProgressBar(true);

            algoTrans.run();
            
            ModelImage downSampleImage = algoTrans.getTransformedImage();
            downSampleImage.calcMinMax();
            algoTrans.disposeLocal();
            algoTrans = null;
            
            if (displayDownSampleImage) {
            	new ViewJFrameImage(downSampleImage);
            }
            
            if (saveDownSampleImage) {
            	final FileIO io = new FileIO();
	            io.setQuiet(true);
	            io.setSuppressProgressBar(true);
	            final FileWriteOptions options = new FileWriteOptions(null, null, true);
	            options.setFileType(FileUtility.TIFF);
	
	            options.setIsScript(true);
	            options.setOptionsSet(true);
	            
	            options.setFileDirectory(srcImage.getImageDirectory() + File.separator);
	            int index = srcImage.getImageFileName().indexOf(".");
	            String baseName;
	            if (index > 0) {
	                baseName = srcImage.getImageFileName().substring(0, index);	
	            }
	            else {
	            	baseName = srcImage.getImageFileName();
	            }
	            options.setBeginSlice(0);
                options.setEndSlice(downSampleImage.getExtents()[2] - 1);
                options.setFileName(baseName + "_XY" + String.valueOf(downSampleXY) + "_Z" +
	                                 String.valueOf(downSampleZ) + ".tif");
                boolean allowScriptRecording = false;
                io.writeImage(downSampleImage, options, false, allowScriptRecording);
            }
        } // if ((downSampleXY < 1.0f) || (downSampleZ < 1.0f))
        

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

    }
    


}
