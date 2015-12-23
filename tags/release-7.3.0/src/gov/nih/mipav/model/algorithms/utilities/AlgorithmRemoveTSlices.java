package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Dimension;
import java.io.*;


/**
 * Algorithm to run the remove T slices. Algorithm verifies the t-th slice should be in included in the destination
 * image, as defined in the list, remove. It copies the slice from the src img to a buffer, and then from the buffer
 * into the destination img. Copies the src file info to a buffer and makes it conform to the new img, then copies it
 * into the dest file.
 */

public class AlgorithmRemoveTSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Original T dimension of the image. */
    private int oldTdim;

    /** List of slices to remove from source image. */
    private boolean[] remove;

    /** Area of a slice (Xdim * Ydim * Zdim). */
    private int volume;

    /** X dimension of the image. */
    private int Xdim;

    /** Y dimension of the image. */
    private int Ydim;

    /** Z dimension of the image. */
    private int Zdim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * import source and destination images into the class.
     *
     * @param  srcImage      source image (image to clip from)
     * @param  destImage     destination image (image to paste to)
     * @param  removeSlices  list of boolean indicating which slices in source should *not* be in the destination
     */
    public AlgorithmRemoveTSlices(ModelImage srcImage, ModelImage destImage, boolean[] removeSlices) {
        super(destImage, srcImage);
        remove = removeSlices;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        Zdim = srcImage.getExtents()[2];
        volume = Xdim * Ydim * Zdim;
        oldTdim = srcImage.getExtents()[3];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Where algorithm calculates the final output.
     */
    public void runAlgorithm() {
        int z;
        float[] imageBuffer;
        int t, T; // t is time-depth of srcImage; T is time-depth of destination
        int colorFactor;
        

        try {

            if (srcImage.isColorImage()) {
                colorFactor = 4;
            } else if (srcImage.isComplexImage()) {
                colorFactor = 2;	
            } else {
                colorFactor = 1;
            }
            imageBuffer = new float[colorFactor * volume];

            fireProgressStateChanged(srcImage.getImageName(), "Removing Selected Time Slices...");
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm Remove Time Slices reports: Out of memory");
            setCompleted(false);
            

            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.
        

        T = 0; // start counting the slices of the destination image at the first slice.

        for (t = 0; (t < oldTdim) && !threadStopped; t++) { // for all slices in the old image

            // let user know something is happening by updating the progressbar
            fireProgressStateChanged(Math.round((float) (t) / (oldTdim - 1) * 100));

            // so long as the slice has not been marked for removal, copy it all over.
            if (!remove[t]) {

                try {

                    // try copying the zth slice out of srcImage, making it the Zth in destImage
                	srcImage.exportData(t * colorFactor * volume, colorFactor * volume, imageBuffer);
                    destImage.importData(T * colorFactor * volume, imageBuffer, false);
                } catch (IOException error) {
                    displayError("Algorithm RemoveTSlices reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }

                FileInfoBase fileInfoBuffer; // buffer of any old type



                T++; // next time slice position in the new image.
            } // if (!remove[t])
            // else {do nothing; goto next t-slice;}
        } // for (t = 0; t < oldTdim; t++)
        destImage.calcMinMax(); // calculate the minimum & maximum intensity values for the destImage-image
        
        if (srcImage.getFileInfo()[0] instanceof FileInfoDicom) {
            //4-D Destination dicom images
                T = 0;
                FileInfoBase destFileInfo[] = null;
                FileInfoDicom oldDicomInfo = null;   
                int j;
                destFileInfo = new FileInfoBase[srcImage.getExtents()[0] * srcImage.getExtents()[1] ];
                double sliceResolution = 0.0;
                int sliceCounter = 0; //Keeps track of every slice to populate tag

           for (t = 0; t < oldTdim; t++) {
               if (!remove[t]) {
                   for (z = 0; z < Zdim ; z++) {
                       j = (T * Zdim) + z;
                       oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo((t * Zdim) + z);
                           destFileInfo[j] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                           oldDicomInfo.getFileFormat());
                           ((FileInfoDicom)destFileInfo[j]).setVr_type(oldDicomInfo.getVr_type());     
    
                        FileDicomTagTable newTagTable = ((FileInfoDicom) destFileInfo[j]).getTagTable();
                        if (newTagTable.getValue("0018,0088") != null) {
                            String sliceGapString = ((String) ((FileInfoDicom) destFileInfo[j]).getTagTable().getValue("0018,0088")).trim();
                            sliceResolution = new Double(sliceGapString.trim()).doubleValue();
                        }                    
                            //fireProgressStateChanged((((100 * (t*2)))/(destImage.getExtents()[2]+1)));
                            destFileInfo[j].setResolutions(srcImage.getFileInfo(0).getResolutions());
                            destFileInfo[j].setExtents(destImage.getExtents());
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[0], 0);
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[1], 1);
                            destFileInfo[j].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation()[2], 2);
                            destFileInfo[j].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());    
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().importTags((FileInfoDicom) oldDicomInfo);
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0011", new Short((short) Xdim), 2); // columns
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0028,0010", new Short((short) Ydim), 2); // rows                 
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().setValue("0020,0013", Short.toString((short) (t + 1)),
                                                                     Short.toString((short) (t + 1)).length()); // instance number
                            ((FileInfoDicom) destFileInfo[j]).getTagTable().removeTag("0019,100A");// Removes NumberofImages in Mosaic Tag
                            sliceCounter++;  
                                                     
                   }
                   T++;
               }
           }
           destImage.setFileInfo(destFileInfo);
        }
        
        else {
            FileInfoBase fileInfoBuffer;
            T = 0;
            for (t = 0; (t < oldTdim) && !threadStopped; t++) { 
                if (!remove[t]) {
                    for (z = 0; (z < Zdim) && !threadStopped; z++) {
                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * Zdim) + z).clone();
                        fileInfoBuffer.setExtents(destImage.getExtents());
                        destImage.setFileInfo(fileInfoBuffer, (T * Zdim) + z);
                    }
                    T++;
                }
               
            }

            
        }

        if (threadStopped) {
            imageBuffer = null;
            finalize();

            return;
        }



        // Clean up and let the calling dialog know that algorithm did its job
        
        setCompleted(true);
    }

}
