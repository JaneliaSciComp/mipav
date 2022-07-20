package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Algorithm to adjust image volume for cases when the slice spacing is not equal to the slice thickness. When spacing &gt;
 * thickness: repeat images from original image set insert blank images (so that in the final image volume, all images
 * will have the same slice thickness and the image volume will be to proper scale. When spacing &lt; thickness: set
 * thickness = spacing. Only works for DICOM or XML files, since they include the sliceSpacing field.
 */
public class JDialogCorrectSpacing extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6267255098982835652L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmCorrectSpacing correctSpaceAlgo;

    /** DOCUMENT ME! */
    private int[] extents;

    /** DOCUMENT ME! */
    //private int fileFormat;

    /** DOCUMENT ME! */
    private FileInfoBase fileInfoBuffer;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int numIm, numRepIm, numBlank, newNumIm;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private float thick, space, gap, newThick, zStart, newZStart;

    /** DOCUMENT ME! */
    private String[] titles;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCorrectSpacing() { }

    /**
     * Constructor that creates new dialog for Correct Spacing. No dialog will be displayed.
     *
     * @param  parent  Parent frame of this dialog.
     * @param  image   Active image in parent frame.
     */
    public JDialogCorrectSpacing(JFrame parent, ModelImage image) {
        super(parent, false);
        this.image = image;
        setVisible(false);

        fileInfoBuffer = (FileInfoBase) image.getFileInfo(0).clone();
        space = fileInfoBuffer.getResolution(2);
        thick = fileInfoBuffer.getSliceThickness();
        
        
        if (image.getNDims() == 3) {
            extents = new int[4];
        } else if (image.getNDims() == 4) {
            extents = new int[3];
        } else {
            MipavUtil.displayError("This utility only works with 3 and 4 D files.");
            
            return;
        }

        //if (fileFormat != FileUtility.XML) {
        //    MipavUtil.displayError("This utility only works with XML files.");

       //     return;
       //}

        if (space > thick) {
            extents = image.getExtents();
            numIm = extents[2];
            gap = space - thick;
            Preferences.debug("\nIn original image set, slice thickness is " + thick + " and gap between slices is " +
                              gap + ".");
            zStart = fileInfoBuffer.getOrigin(2);
            getNewInfo();
            Preferences.debug("New slice thickness: " + newThick +
                              ".  To create new images, each original image is repeated " + numRepIm + " times and " +
                              numBlank + " blanks are inserted.");
            generateNewImages();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method is required if the AlgorithmInterface is implemented.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmCorrectSpacing) {

            // resultImage = correctSpaceAlgo.getResultImage();
            if ((correctSpaceAlgo.isCompleted() == true) && (resultImage != null)) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);
                }

                resultImage.calcMinMax();
                resultImage.setExtents(extents);
                fileInfoBuffer.setExtents(extents);
                fileInfoBuffer.setResolutions(newThick, 2);
                fileInfoBuffer.setSliceThickness(newThick);
                fileInfoBuffer.setOrigin(newZStart, 2);

                for (int i = 0; i < newNumIm; i++) {
                    resultImage.setFileInfo(fileInfoBuffer, i);
                }

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (parentFrame != null) {
            ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame(parentFrame);
            ((ViewJFrameBase) (parentFrame)).close();
        }

        correctSpaceAlgo.finalize();
        correctSpaceAlgo = null;
        dispose();
    }

    /**
     * Call algorithm to copy appropriate images from original, and double where necessary.
     */
    public void callAlgorithm() {

        if (space == 0.0) {
            MipavUtil.displayError("Spacing information is not available for this data set.");

            return;
        } else if (thick == 0.0) {
        	MipavUtil.displayError("Slice thickness information is not available for this data set.");
        	
        	return;
        } else if (space == thick) {
            MipavUtil.displayError("Slice spacing equals slice thickness.  No need for correction.");

            return;
        } else if (space < thick) {
            correctSpaceLTthick(image, space);
        } else {

            try {
                correctSpaceAlgo = new AlgorithmCorrectSpacing(image, resultImage, numRepIm, numBlank);
                correctSpaceAlgo.addListener(this);

                createProgressBar(image.getImageName(), correctSpaceAlgo);

                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (correctSpaceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("Correct Spacing reports: A thread is already running on this object [correctSpaceAlgo]");
                    }
                } else {

                    correctSpaceAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Correct Spacing reports: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        fileInfoBuffer = (FileInfoBase) image.getFileInfo(0).clone();
        space = fileInfoBuffer.getResolution(2);
        thick = fileInfoBuffer.getSliceThickness();

        if (image.getNDims() == 3) {
            extents = new int[4];
        } else if (image.getNDims() == 4) {
            extents = new int[3];
        } else {
            MipavUtil.displayError("This utility only works with 3D and 4D files.");

            return;
        }
        
        //This utility should work for all file types, not just XML. 
        //if (fileFormat != FileUtility.XML) {
        //    MipavUtil.displayError("This utility only works with XML files.");

        //    return;
        //}

        if (space > thick) {
            extents = image.getExtents();
            numIm = extents[2];
            gap = space - thick;
            Preferences.debug("\nIn original image set, slice thickness is " + thick + " and gap between slices is " +
                              gap + ".");
            zStart = fileInfoBuffer.getOrigin(2);
            getNewInfo();
            Preferences.debug("New slice thickness: " + newThick +
                              ".  To create new images, each original image is repeated " + numRepIm + " times and " +
                              numBlank + " blanks are inserted.");
            generateNewImages();
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());
    }

    /**
     * This method corrects spacing for cases where the space is less than the thickness.
     *
     * @param  im           DOCUMENT ME!
     * @param  newThickVal  DOCUMENT ME!
     */
    private void correctSpaceLTthick(ModelImage im, float newThickVal) {
        FileInfoBase[] arrayOfFileInfo;
        arrayOfFileInfo = (FileInfoBase[]) im.getFileInfo();

        int zExtent = im.getExtents()[2];

        for (int i = 0; i < zExtent; i++) {
            arrayOfFileInfo[i].setSliceThickness(newThickVal);
        }

        MipavUtil.displayInfo("Since spacing is less than thickness, setting thickness to spacing value (" +
                              newThickVal + ").");
        dispose();

        return;
    }

    /**
     * Generates new images. Calculates the new total number of images (newNumIm) and then generates new images with
     * that dimension. Calls callAgorithm, passing it the resultImage.
     */
    private void generateNewImages() {
        newNumIm = numIm * (numRepIm + numBlank);
        extents = (int[]) image.getExtents().clone();
        extents[2] = newNumIm;
        // System.out.println("Dimensions of new image will be " +extents[0] +", " +extents[1] +", " +extents[2]);

        try {
            String newName = makeImageName(image.getImageName(), "CorrSpc");

            // makeImageName is defined in JDialogBase
            resultImage = new ModelImage(image.getType(), extents, newName);
        } catch (OutOfMemoryError e) {
            resultImage = null;
            MipavUtil.displayError("JDialogCorrectSpacing reports: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Calculates the new slice thickness (newThick), number of times to repeat each original image (numRepIm), the
     * number of blanks to insert between the image duplicates (numBlank), and the new z coordinate origin for the image
     * volume (newZStart).
     */
    private void getNewInfo() {
        float sign = 1.f;

        if (zStart < 0) {
            sign = -1.f;
        }

        /* don't have to handle gap==0 or gap<0 b/c dialog will have exited already */
        if (((gap / thick) % 1) == 0.0) {
            newThick = thick;
            numRepIm = 1;
            numBlank = 1 * (int) (gap / thick);

            // newNumIm = (int)(1+gap/thick)*numIm;
            newZStart = zStart;
        } else if (((thick / gap) % 1) == 0.0) {
            newThick = gap;
            numRepIm = 1 * (int) (thick / gap);
            numBlank = 1;

            // newNumIm = (int)(1+thick/gap)*numIm;
            newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 2.f)));
        } else if ((2 * (gap / thick) % 1) == 0.0) {
            newThick = thick / 2;
            numRepIm = 2;
            numBlank = 2 * (int) (gap / thick);

            // newNumIm = (int)(2+2*gap/thick)*numIm;
            newZStart = zStart + (sign * (-thick / 4.f));
        } else if ((2 * (thick / gap) % 1) == 0.0) {
            newThick = gap / 2;
            numRepIm = 2 * (int) (thick / gap);
            numBlank = 2;

            // newNumIm = (int)(2+2*thick/gap)*numIm;
            newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 4.f)));
        } else if ((3 * (gap / thick) % 1) == 0.0) {
            newThick = thick / 3;
            numRepIm = 3;
            numBlank = 3 * (int) (gap / thick);

            // newNumIm = (int)(3+3*gap/thick)*numIm;
            newZStart = zStart + (sign * (-thick / 3.f));
        } else if ((3 * (thick / gap) % 1) == 0.0) {
            newThick = gap / 3;
            numRepIm = 3 * (int) (thick / gap);
            numBlank = 3;

            // newNumIm = (int)(3+3*thick/gap)*numIm;
            newZStart = zStart + (sign * ((-thick / 2.f) + (gap / 6.f)));
        } else {
            MipavUtil.displayError("Option not implemented for that ratio of thickness to space.");
        }

        return;
    }
}
