package gov.nih.mipav.view;

import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;

public class ViewJComponentPedsAtlasIconImage extends ViewJComponentEditImage {

	
	public ViewJComponentPedsAtlasIconImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
            int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay, int _orientation) {

        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay,
                _orientation);
        this.imageA = _imageA;


    }
	
	
	 /**
     * Shows the image and the VOI(s).
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null
     * @param _LUTb LUTb - to change to new LUT for imageB else null
     * @param forceShow forces this method to import image and recalculate java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(final int tSlice, final int zSlice, final ModelLUT _LUTa, final ModelLUT _LUTb,
            final boolean forceShow) {
    	
    	
    	
    	return show(tSlice, zSlice, null, null, forceShow, interpMode);
    }
	
	 /**
     * Shows the image and the VOI(s).
     * 
     * @param tSlice t (time) slice to show
     * @param zSlice z slice to show
     * @param _LUTa LUTa - to change to new LUT for imageA else null
     * @param _LUTb LUTb - to change to new LUT for imageB else null
     * @param forceShow forces this method to import image and recalculate java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean to indicate if the show was successful
     */
    public boolean show(final int tSlice, final int zSlice, final ModelLUT _LUTa, final ModelLUT _LUTb,
            final boolean forceShow, final int interpMode) {

        if (interpMode > -1) {
            setInterpolationMode(interpMode);
        }

        m_kPatientSlice.setLUTa(_LUTa);
        m_kPatientSlice.setLUTb(_LUTb);
        m_kPatientSlice.updateSlice(zSlice);

        if (cleanImageBufferB == null) {
            cleanImageBufferB = new int[imageExtents[0] * imageExtents[1]];
        }

        if (m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false, 0,
                false)) {

            slice = zSlice;
            //setSliceString(String.valueOf(slice));
            Graphics g = getGraphics();
            paintComponent(g);

            return true;
        } else {
            return false;
        }
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)
	
	
	/**
     * mouse clicked
     */
    public void mouseClicked(MouseEvent mouseEvent) {

    }

    /**
     * mouse dragged
     */
    public void mouseDragged(MouseEvent mouseEvent) {

    }

    /**
     * mouse entered
     */
    public void mouseEntered(MouseEvent mouseEvent) {

    }

    /**
     * mouse exited
     */
    public void mouseExited(MouseEvent mouseEvent) {

    }

    /**
     * mouse moved
     */
    public void mouseMoved(MouseEvent mouseEvent) {

    }

    /**
     * mouse pressed
     */
    public void mousePressed(MouseEvent mouseEvent) {

    }

    /**
     * mouse pressed paint
     */
    protected void mousePressedPaint(MouseEvent mouseEvent) {

    }

    /**
     * mouse released
     */
    public void mouseReleased(MouseEvent mouseEvent) {

    }

    /**
     * mouse wheel moved
     */
    public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {

    }
	
	
	
}
