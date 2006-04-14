package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import javax.swing.*;
import java.awt.event.*;
import java.awt.image.*;
import java.awt.*;
import java.io.*;
/**
*	Dialog (invisible) for capturing slices of a 3d/4d image into a new image
*/
public class JDialogCaptureScreens extends JDialogBase {

    /** Pointer to the user interface.                    */
    private ViewUserInterface userInterface;

    /** The rectangle that will be captured from the screen to save to a file.*/
    private Rectangle		currentRectangle;

    /** The parent image frame */
    private ViewJFrameImage imageFrame;

    /** Number of slices in the image */
    private int             numSlices;

    /**
     * Invisible dialog used through script or from the Main frame
     * that captures slices of a 3d/4d Image into a new frame
     * @param parent the parent frame
     * @param UI the user interface
     */
    public JDialogCaptureScreens(ViewJFrameImage parent, ViewUserInterface UI) {
        super(parent, false);
        userInterface = UI;
        this.imageFrame = parent;
        if (imageFrame.getImageA().getExtents().length < 3) {
            MipavUtil.displayError("JDialogCaptureScreens is for 3D and 4D images only");
            return;
        }
        this.numSlices = imageFrame.getImageA().getExtents()[2];
    }

    /**
     *  Handles the action events (called from script)
     * @param event
     */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
        if (command.equals("Script")) {
            if (writeImage()) {
               // System.err.println("successful");
            }
        }
	}

    /**
     *  Scrolls through all z slices of a 3d/4d image and captures them into
     *  a new ARGB ModelImage, then puts the ModelImage in a ViewJFrameImage
     * @return
     */
	private boolean writeImage() {
    	int pixels[];
	    int bufferSize, xDim, yDim;
    	short buffer[] = null;
        int extents[] = new int[3];
    	ModelImage testImage = null;
        Robot robot = null;
        Image imagePix = null;

        /**
         * Create a Robot to capture the screen at the given location and dimension
         *  (the region of interest is the ViewJFrameImage)
         */
        try {
            robot = new Robot();
            Point p = new Point();
            p.x = 0;
            p.y = 0;
            SwingUtilities.convertPointToScreen(p, imageFrame.getContentPane());
            p.x++;
            p.y++;

            Dimension d = new Dimension();
            d.width = imageFrame.getContentPane().getWidth() - 3;
            d.height = imageFrame.getContentPane().getHeight() - 3;
            currentRectangle = new Rectangle(p, d);


            extents[0] = currentRectangle.width; // RGB
            extents[1] = currentRectangle.height;
            extents[2] = numSlices;
            pixels = new int[extents[0] * extents[1]];
            bufferSize = 4 * extents[0] * extents[1];
            testImage = new ModelImage(ModelStorageBase.ARGB, extents, "Screen capture", userInterface);
            buffer    = new short[bufferSize];
        }
        catch (OutOfMemoryError error){
            MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");
            return false;
        }
        catch (AWTException aex) {
            MipavUtil.displayError("Platform doesn't support screen capture.");
        	return false;
        }


        //Turn off image slice #'s
        imageFrame.getComponentImage().setShowSliceNumber(false);
        imageFrame.getComponentImage().useHighlight(false);

        /**
         * Scroll through each slice, grabbing the screen with the robot and exporting pixels into
         * a buffer for ARGB
         */
        for (int slice = 0; slice < numSlices; slice++) {
            imageFrame.setSlice(slice);
            try {
                imagePix = robot.createScreenCapture(currentRectangle);
                PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, extents[0], extents[1], pixels, 0, extents[0]);
                pgTest.grabPixels();
            }
            catch (InterruptedException e) {
                Preferences.debug("Interrupted waiting for pixels!");
                return false;
            }
            catch (OutOfMemoryError error) {
                MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");
                return false;
            }

            int i, k;
            for (i = 0, k = 0; i < (extents[0] * extents[1]); i++, k += 4) {
                buffer[k] = (short) (255); // alpha
                buffer[k + 1] = (short) (pixels[i] >> 16 & 0xFF); // Red
                buffer[k + 2] = (short) (pixels[i] >> 8 & 0xFF); // Green
                buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
            }

            /**
             * Import the ARGB buffer into the model image
             */
            try {
                testImage.importData((buffer.length * slice), buffer, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
            }
            testImage.getFileInfo()[0].setPhotometric( (short) 2); // Indicates RGB tiff file format



        }
        imageFrame.getComponentImage().setShowSliceNumber(true);

        testImage.calcMinMax();
        testImage.getFileInfo()[0].setResolutions(imageFrame.getImageA().getFileInfo()[0].getResolutions());
        testImage.getFileInfo()[0].setUnitsOfMeasure(imageFrame.getImageA().getFileInfo()[0].getUnitsOfMeasure());
        testImage.getFileInfo()[0].setAxisOrientation(imageFrame.getImageA().getFileInfo()[0].getAxisOrientation());
        testImage.getFileInfo()[0].setImageOrientation(imageFrame.getImageA().getFileInfo()[0].getImageOrientation());
        testImage.getFileInfo()[0].setSliceSpacing(imageFrame.getImageA().getFileInfo()[0].getSliceSpacing());
        testImage.getFileInfo()[0].setOrigin(imageFrame.getImageA().getFileInfo()[0].getOrigin());

        for (int m = 1; m < extents[2]; m++) {
            testImage.setFileInfo((FileInfoBase)testImage.getFileInfo()[0].clone(), m);
        }


        new ViewJFrameImage(testImage, null, new Dimension(610,200));

    	return true;
	}

}
