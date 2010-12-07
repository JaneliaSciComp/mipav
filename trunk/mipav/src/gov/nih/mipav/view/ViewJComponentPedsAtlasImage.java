package gov.nih.mipav.view;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;

public class ViewJComponentPedsAtlasImage extends ViewJComponentEditImage {

	public ViewJComponentPedsAtlasImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
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
            setSliceString(String.valueOf(slice));
            paintComponent(getGraphics());

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

    /*public void mouseDragged(final MouseEvent mouseEvent) {

        final int mouseMods = mouseEvent.getModifiers();

        int xS, yS;
        Color dropperColor;
        int diffX = Math.round((lastMouseX - mouseEvent.getX()) * getZoomX());
        int diffY = Math.round((lastMouseY - mouseEvent.getY()) * getZoomY());
        lastMouseX = mouseEvent.getX();
        lastMouseY = mouseEvent.getY();

        if ( (pixBuffer == null) || (imageBufferActive == null) || (modifyFlag == false)) {
            return;
        }

        xS = getScaledX(mouseEvent.getX()); // zoomed x. Used as cursor
        yS = getScaledY(mouseEvent.getY()); // zoomed y. Used as cursor

        final int xDim = imageActive.getExtents()[0];
        final int yDim = imageActive.getExtents()[1];

        if ( (xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        processDefaultMouseDrag(mouseEvent, xS, yS);
        
        
        
    }*/
    
    
    

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
