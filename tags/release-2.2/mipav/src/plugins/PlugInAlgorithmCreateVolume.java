import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.IOException;

/**
 *  Algorithm to repeat images from original image set and
 *  insert blank images, so that in the final image volume
 *  all images will have the same slice thickness and the image
 *  volume will be to proper scale.  Resorting of images occurs
 *  here too.
 */

public class PlugInAlgorithmCreateVolume extends AlgorithmBase {

    /** X dimension of the image.                              */
    private   int           Xdim;
    /**  Y dimension of the image.                             */
    private   int           Ydim;
    /** Original Z dimension of the image.                     */
    private   int           oldZdim;
    /** Area of a slice (Xdim * Ydim).                         */
    private   int           sliceArea;
    /** Number of images per position from the original set. Also, number of planes. */
    private	  int			nPlanes;
    /** Number of images in new image volumes, once gap and thickness correction is applied. */
    private	  int			newNPlanes;

    /** Import variables */
    /** Result Image.  Will be reused. */
    private	ModelImage  	resultImage;
    /* Current position number */
    private	int	curPos;
    /* Total number of images originally -- for whole volume. */
    private int TNI;
    /* Number of images per position, originally. eg. 24*/
    private int	NIPP;
    /* Number of repeated images, to compensate for gap. */
    private int numRepIm;
    /* Number of blank images, to make up gap. */
    private int	numBlanks;

    /**
    *   Import source image into the class
    *   @param  srcImage        source image (image to clip from)
    */
    public PlugInAlgorithmCreateVolume(ModelImage srcImage, ModelImage resultImage, int curPos, int TNI,
    		int NIPP, int numRepIm, int numBlanks) {
        super(resultImage, srcImage);
		this.resultImage=resultImage;
		this.curPos=curPos;
		this.TNI=TNI;
		this.NIPP=NIPP;
		this.numRepIm=numRepIm;
		this.numBlanks=numBlanks;

        // set global variables for this class

	    Xdim        = resultImage.getExtents()[0];
        Ydim        = resultImage.getExtents()[1];
        sliceArea   = Xdim*Ydim;
        oldZdim = srcImage.getExtents()[2];

	    nPlanes = TNI/NIPP;
	    newNPlanes = nPlanes*(numRepIm+numBlanks);
	}


    /**
    *   Constructs a string of the contruction parameters and
    *   outputs the string to the messsage frame if the logging
    *   procedure is turned on.
    */
    private void constructLog() {
        String createVolStr = new String();
        createVolStr = " with " +numRepIm +" repeats of image slices and " +numBlanks
        	+" blanks between images.  Total number of new images is " +newNPlanes
        	+".";
        historyString = new String( "Creating volume for " + srcImage.getImageName() + createVolStr +"\n");
    }

    /**
    *   Runs algorithm.
    */
    public void runAlgorithm() {
    	int 	j,k;
    	int     z, Z;       // z is slice-depth of srcImage; Z is slice-depth of destination
        float   imageBuffer[];
        FileInfoXML	fileInfo;
        ViewJFrameImage imageFrame;

        constructLog();

        try {
            imageBuffer = new float[sliceArea]; // assuming here 3D image set and grayscale
        }
        catch (OutOfMemoryError e) {
            imageBuffer = null;
            errorCleanUp("Algorithm Create Volume reports: Out of memory", true);
            return;
        }

        // main part of algorithm
    	Z = 0;
    	// Z is the slice number in the resultImage
        for (z=0; z<TNI && !threadStopped; z++){
        	// z is the slice number in the srcImage
            if (((z+1)%NIPP) == curPos || (curPos == NIPP && (z+1)%NIPP==0) ) {
            	try {
                    srcImage.exportData(z*sliceArea, sliceArea,imageBuffer);
        			fileInfo = (FileInfoXML) srcImage.getFileInfo(z).clone();
        			fileInfo.setFileName(resultImage.getImageFileName());
    				fileInfo.setFileFormat(FileBase.XML);
                    for (k=0; k < numRepIm; k++, Z++) {
	                    resultImage.importData(Z*sliceArea, imageBuffer, false);
                        resultImage.setFileInfo(fileInfo, Z);
	                }
	                for (j = 0; j < sliceArea; j++) {
            			imageBuffer[j] = 0.0f;
            		}
	                for (k=0; k < numBlanks; k++, Z++) {
	                	resultImage.importData(Z*sliceArea, imageBuffer, false);
                        resultImage.setFileInfo(fileInfo, Z);
	                }
            	}
            	catch (IOException error) {
            		displayError("Algorithm Create Volume reports: Destination image already locked.");
                    setCompleted(false);
                    return;
                }
			}
		}

		if (threadStopped) {
		    imageBuffer  = null;
		    setCompleted(false);
        	finalize();
			return;
		}

        // Clean up and let the calling dialog know that algorithm did its job
        setCompleted(true);
    }

    /* Accessor to current position number. */
    public int getCurrentPosition() {
    	return this.curPos;
    }
}
