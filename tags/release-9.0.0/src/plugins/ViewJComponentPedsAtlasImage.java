

import java.awt.Color;

import java.awt.Dimension;

import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;


import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameBase;

public class ViewJComponentPedsAtlasImage extends ViewJComponentEditImage {
	
	private String modality;
	

	private int orient;
	
	public ViewJComponentPedsAtlasImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
            int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay, int _orientation, String modality) {

        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay,
                _orientation);
        
        this.modality = modality;
        this.orient = _orientation;
        interpMode = ViewJComponentBase.INTERPOLATE_A;


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
    	
    	
    	
    	return show(tSlice, zSlice, _LUTa, _LUTb, forceShow, interpMode);
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

        if(_LUTa != null) {
        	m_kPatientSlice.setLUTa(_LUTa);
        }
        if(_LUTb != null) {
        	m_kPatientSlice.setLUTb(_LUTb);
        }
        m_kPatientSlice.updateSlice(zSlice);
        
        

        if (cleanImageBufferB == null) {
            cleanImageBufferB = new int[imageExtents[0] * imageExtents[1]];
        }

        if (m_kPatientSlice.showUsingOrientation(tSlice, cleanImageBufferA, cleanImageBufferB, forceShow, false)) {

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
    
    
    /**
     * DOCUMENT ME!
     * 
     * @param LUT DOCUMENT ME!
     * @param image DOCUMENT ME!
     */
    public void resetLUT(final ModelLUT LUT, final ModelImage image) {
        float min, max;
        final float[] x = new float[4];
        final float[] y = new float[4];
        final Dimension dim = new Dimension(256, 256);

        // Set LUT min max values;
        if (image.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (image.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else {
            min = (float) image.getMin();
            max = (float) image.getMax();
        }

        x[0] = min;
        y[0] = dim.height - 1;

        x[1] = (min + ( (max - min) / 3.0f));
        y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

        x[2] = (min + ( (max - min) * 0.67f));
        y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

        x[3] = max;
        y[3] = 0;
        LUT.getTransferFunction().importArrays(x, y, 4);

    }

    public void mouseDragged(final MouseEvent mouseEvent) {

        //final int mouseMods = mouseEvent.getModifiers();

        int xS, yS;
        //Color dropperColor;
        //int diffX = Math.round((lastMouseX - mouseEvent.getX()) * getZoomX());
        //int diffY = Math.round((lastMouseY - mouseEvent.getY()) * getZoomY());
        //lastMouseX = mouseEvent.getX();
        //lastMouseY = mouseEvent.getY();

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
     * mouse pressed
     */
    public void mousePressed(MouseEvent mouseEvent) {

        
        
    	if(mouseEvent.getButton() == MouseEvent.BUTTON3	) {

    		float fMinImageWin = (float)imageActive.getMin();
	        float fMaxImageWin = (float)imageActive.getMax();
	        float fX = 0;
	        float fY = 0;
	    	//fWindow = 2.0f * fX * (fMaxImageWin - fMinImageWin)
	        //fLevel = fY * (fMaxImageWin - fMinImageWin);
	    	
	    	if(modality.equals("t1")) {
	    		fX = PlugInDialogPedsAtlas.t1WindowPreset/(2.0f*(fMaxImageWin-fMinImageWin));
	    		fY = PlugInDialogPedsAtlas.t1LevelPreset/((fMaxImageWin-fMinImageWin));
	 		}else if(modality.equals("t2")) {
	 			fX = PlugInDialogPedsAtlas.t2WindowPreset/(2.0f*(fMaxImageWin-fMinImageWin));
	    		fY = PlugInDialogPedsAtlas.t2LevelPreset/((fMaxImageWin-fMinImageWin));
	 		}else if(modality.equals("pd")) {
	 			fX = PlugInDialogPedsAtlas.pdWindowPreset/(2.0f*(fMaxImageWin-fMinImageWin));
	    		fY = PlugInDialogPedsAtlas.pdLevelPreset/((fMaxImageWin-fMinImageWin));
	 		}
	    	
	    	int xDim = imageActive.getExtents()[0];
	        int yDim = imageActive.getExtents()[1];
	        
	        int xS, yS;
	        
	        xS = (int) (xDim * fX);
	        yS = (int) (yDim * fY);
	
	        //System.out.println(xS);
	        //System.out.println(yS);
    		
    		
    		float xVal = xDim - xS;
    		float yVal = yS;
    		
    		if(orient == FileInfoBase.SAGITTAL) {
    			xVal = xS;
    		}
    		
    		//System.out.println("xVal is " + xVal);
    		//System.out.println("yVal is " + yVal);
    		//System.out.println("--------------");

    		String name = "";
    		String label = "";
    		
    		if(modality.equals("t1")) {
    			name = "T1";
    			label = "T1";
    		}else if(modality.equals("t2")) {
    			name = "T2";
    			label = "T2";
    		}else if(modality.equals("pd")) {
    			name = "PD";
    			label = "PD";
    		}
    			
    			
    		
    		VOI newPtVOI = null;
            final float[] x = new float[1];
            final float[] y = new float[1];
            final float[] z = new float[1];

            // STANDARD COLUMN IMAGE
            
            
            newPtVOI = new VOI((short) 0, name, VOI.POINT, -1.0f);
            newPtVOI.setUID(newPtVOI.hashCode());
            newPtVOI.setColor(Color.white);
           
            x[0] = 20;
   	     	y[0] = 20;
   	     	z[0] = slice;
   	     	
   	     	
   	     if(modality.equals("t1")) {
   	    	x[0] = xVal;
   	     	y[0] = yVal;
   	     	z[0] = slice;
 		}else if(modality.equals("t2")) {
 			x[0] = xVal;
   	     	y[0] = yVal;
   	     	z[0] = slice;
 		}else if(modality.equals("pd")) {
 			x[0] = xVal;
   	     	y[0] = yVal;
   	     	z[0] = slice;
 		}
   	     	
   	     	VOIBase curve = new VOIPoint();
   	     	curve.importArrays(x, y, z, x.length);
   	     	curve.setLabel(label);
   	     
   	     	newPtVOI.importCurve(curve);

            
            imageActive.registerVOI(newPtVOI);
            
            imageActive.notifyImageDisplayListeners();
            
            
            try {
   			 this.setDrawVOIs(true);
   	         this.paintComponent(this.getGraphics());
   	        } catch (final OutOfMemoryError error) {
   	            System.gc();
   	        }
   	        this.getFrame().repaint();
   	    
   	    
   	    
    	}
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
    	if(mouseEvent.getButton() == MouseEvent.BUTTON3) {
    		VOIVector vois = imageActive.getVOIs();
    		for(int i=0;i<vois.size();i++) {
    			VOI voi = vois.get(i);
    			if(voi.getName().equals("T1") || voi.getName().equals("T2") || voi.getName().equals("PD")) {
    				vois.remove(i);
    				
    			}
    				
    			
    		}
    		
    		imageActive.notifyImageDisplayListeners();
            
            
            try {
   			 this.setDrawVOIs(true);
   	         this.paintComponent(this.getGraphics());
   	        } catch (final OutOfMemoryError error) {
   	            System.gc();
   	        }
   	        this.getFrame().repaint();
    		
    		
    	}
    }

    /**
     * mouse wheel moved
     */
    public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {

    }
	
}
