package gov.nih.mipav.view;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;

public class ViewJComponentDTIImage extends ViewJComponentEditImage implements MouseListener{
	
	/** type of color wheel 	ABSVAL, NOSYMM, ROTATIONALSYMM, MIRRORSYMM  **/
	private String type;
	
	/** model image **/
	private ModelImage imageA;
	
	/** sat vs theta **/
	private float pS = 0.5f;
	
	/** color range **/
	private float pC = .700f;
	
	/** blue saturation **/
	private float pB = .350f;
	
	/** gamma correction **/
	private float gamma = 1.8f;
	
	/** two pi **/
	private float twoPi = 2 * (float)Math.PI;
	
	/** pi div two **/
	private float piDivTwo = (float)Math.PI/2;
	
	/** arry of r,g,b after blue shift **/
	private float blueShiftColors[] = new float[3];

	
	/**
	 * constructor
	 * @param _frame
	 * @param _imageA
	 * @param _LUTa
	 * @param imgBufferA
	 * @param pixelBuffer
	 * @param zoom
	 * @param extents
	 * @param logMagDisplay
	 * @param _orientation
	 */
	public ViewJComponentDTIImage(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
            int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
            int _orientation) {
		
        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay,
                _orientation);
        this.imageA = _imageA;
		
	}
	
	
	
	
	 /**
     * For generating the display of 1 or 2 images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow, String type, float pS, float pB, float pC, float gamma) {
    	this.type = type;
    	this.pS = pS;
    	this.pB = pB;
    	this.pC = pC;
    	this.gamma = gamma;
        return show(tSlice, zSlice, null, null, forceShow, interpMode);
    }
    
    
    
    /**
     * Shows the image and the VOI(s).
     *
     * @param   tSlice      t (time) slice to show
     * @param   zSlice      z slice to show
     * @param   _LUTa       LUTa - to change to new LUT for imageA else null
     * @param   _LUTb       LUTb - to change to new LUT for imageB else null
     * @param   forceShow   forces this method to import image and recalculate java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, int interpMode) {
    
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

            if ((pixBuffer == null) || (pixBuffer.length != cleanImageBufferA.length)) {
                pixBuffer = new int[cleanImageBufferA.length];
            }
            
            
            //now we need to show depending on the color wheel selected
            //since this is ARGB image....it is alpha, red, green, blue...so we mult by 4
        	int length = imageA.getSliceSize() * 4;
        	int start = zSlice * length;
        	float[] buff = new float[length];
        	try {
        		imageA.exportData(start, length, buff);
        	}
        	catch(IOException e) {
        			
        	}
            if(type.equals("ABSVAL")) {
            	absoluteValue(zSlice,buff);	
            }
            if(type.equals("NOSYMM")) {
            	noSymm(zSlice,buff);	
            }
            if(type.equals("ROTATIONALSYMM")) {
            	rotationalSymm(zSlice,buff);	
            }
            if(type.equals("MIRRORSYMM")) {
            	mirrorSymm(zSlice,buff);	
            }

            System.arraycopy(cleanImageBufferA, 0, pixBuffer, 0, cleanImageBufferA.length);
            slice = zSlice;
            setSliceString(String.valueOf(slice + 1));
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    } 
	
	
    /**
     * absolute value
     * @param zSlice
     * @param buff
     */
	public void absoluteValue(int zSlice, float[] buff) {
    	float r,g,b;
    	int red,green,blue;
    	int val,index;
    	for(int i=0;i<=(buff.length-4);i=i+4) {
    		
    		r = Math.abs(buff[i+1]);
    		g = Math.abs(buff[i+2]);
    		b = Math.abs(buff[i+3]);
    		
    		//shift blue
			blueShiftColors = shiftBlue(r,g,b);
			r = blueShiftColors[0];
			g = blueShiftColors[1];
			b = blueShiftColors[2];
			
			//gamma correction
			r = (float)Math.pow(r,(1/gamma));
			g = (float)Math.pow(g,(1/gamma));
			b = (float)Math.pow(b,(1/gamma));
			
			
			//now get r,g,b to 0 - 255 range
			red = Math.round(r * 255);
    		green = Math.round(g * 255);
    		blue = Math.round(b * 255);

    		//now do the orring
    		val = 0xff000000 | (red << 16) | (green << 8) | (blue);
    		
    		//now put replace in cleanImageBufferA
    		index = i/4;
    		cleanImageBufferA[index] = val;

    	}
	}
	
	
	/**
	 * no symmetry
	 * @param zSlice
	 * @param buff
	 */
	public void noSymm(int zSlice, float[] buff) {
		float vx,vy,vz;
		float hue, sat;
		float theta,phi;
		float xylength;
    	int val,index;
    	float r,g,b;
    	int red,green,blue;
    	for(int i=0;i<=(buff.length-4);i=i+4) {
    		vx = buff[i+1];
    		vy = buff[i+2];
    		vz = buff[i+3];
    		if(vx == 0 && vy == 0 && vz == 0) {
    			red = 0;
    			green = 0;
    			blue = 0;
    		}
    		else {
    			if(vz < 0) {
    				vx = -vx;
    				vy = -vy;
    				vz = -vz;
    			}
    			
    			xylength = (float)Math.sqrt((vx * vx) + (vy * vy));

	    		theta = (float)Math.asin(xylength);
	    		phi = (float)Math.atan2(vy,vx);
	    		hue = ((phi - piDivTwo + twoPi)%twoPi)/twoPi;
	    		if(pS < .001) {
					pS = .001f;
				}
	    		sat = (float)((Math.sin(pS * theta))/(Math.sin(pS * piDivTwo)));
	    		
	    		Color c = Color.getHSBColor(hue, sat, 1f);
				red = c.getRed();
				green = c.getGreen();
				blue = c.getBlue();
				
				//normalize red,green,blue to between 0 and 1
				r = red/255f;
				g = green/255f;
				b = blue/255f;
				
				//shift blue
				blueShiftColors = shiftBlue(r,g,b);
				r = blueShiftColors[0];
				g = blueShiftColors[1];
				b = blueShiftColors[2];
				
				//gamma correction
				r = (float)Math.pow(r,(1/gamma));
				g = (float)Math.pow(g,(1/gamma));
				b = (float)Math.pow(b,(1/gamma));
				
				//now get r,g,b to 0 - 255 range
				red = Math.round(r * 255);
	    		green = Math.round(g * 255);
	    		blue = Math.round(b * 255);
    		}
    		
			//now do the orring
    		val = 0xff000000 | ( red << 16) | (green << 8) | (blue);
    		
    		//now put replace in cleanImageBufferA
    		index = i/4;
    		cleanImageBufferA[index] = val;
    	}
	}
	
	
	
	/**
	 * rotationsl symmetry
	 * @param zSlice
	 * @param buff
	 */
	public void rotationalSymm(int zSlice, float[] buff) {
		float vx,vy,vz;
		float hue, sat;
		float theta,phi;
		float xylength;
    	int val,index;
    	int red,green,blue;
    	float r,g,b;
    	for(int i=0;i<=(buff.length-4);i=i+4) {
    		vx = buff[i+1];
    		vy = buff[i+2];
    		vz = buff[i+3];
    		if(vx == 0 && vy == 0 && vz == 0) {
    			red = 0;
    			green = 0;
    			blue = 0;
    		}
    		else {
    			if(vz < 0) {
    				vx = -vx;
    				vy = -vy;
    				vz = -vz;
    			}
    			xylength = (float)Math.sqrt((vx * vx) + (vy * vy));

	    		theta = (float)Math.asin(xylength);
	    		phi = (float)Math.atan2(vy,vx);
	    	
	    		hue = ((2 * (phi - piDivTwo + twoPi))%twoPi)/twoPi;
	    		if(pS < .001) {
					pS = .001f;
				}
	    		sat = (float)((Math.sin(pS * theta))/(Math.sin(pS * piDivTwo)));
	    		Color c = Color.getHSBColor(hue, sat, 1f);
				red = c.getRed();
				green = c.getGreen();
				blue = c.getBlue();
				
				//normalize red,green,blue to between 0 and 1
				r = red/255f;
				g = green/255f;
				b = blue/255f;
				
				//shift blue
				blueShiftColors = shiftBlue(r,g,b);
				r = blueShiftColors[0];
				g = blueShiftColors[1];
				b = blueShiftColors[2];
				
				//gamma correction
				r = (float)Math.pow(r,(1/gamma));
				g = (float)Math.pow(g,(1/gamma));
				b = (float)Math.pow(b,(1/gamma));
				
				//now get r,g,b to 0 - 255 range
				red = Math.round(r * 255);
	    		green = Math.round(g * 255);
	    		blue = Math.round(b * 255);
				
				
				
    		}
    		
			//now do the orring
    		val = 0xff000000 | (red << 16) | (green << 8) | (blue);
    		
    		//now put replace in cleanImageBufferA
    		index = i/4;
    		cleanImageBufferA[index] = val;
    	}
	}
	
	
	
	/**
	 * mirror symmetry
	 * @param zSlice
	 * @param buff
	 */
	public void mirrorSymm(int zSlice, float[] buff) {
		float vx,vy,vz;
		float hue, sat;
		float theta,phi,phi_deg;
		float xylength;
    	int val,index;
    	int red,green,blue;
    	float r,g,b;
    	for(int i=0;i<=(buff.length-4);i=i+4) {
    		vx = buff[i+1];
    		vy = buff[i+2];
    		vz = buff[i+3];
    		if(vx == 0 && vy == 0 && vz == 0) {
    			red = 0;
    			green = 0;
    			blue = 0;
    		}
    		else {
    			if(vz < 0) {
    				vx = -vx;
    				vy = -vy;
    				vz = -vz;
    			}
    			xylength = (float)Math.sqrt((vx * vx) + (vy * vy));

	    		theta = (float)Math.asin(xylength);
	    		
	    		phi = (float)Math.atan2(vy,vx);
	    		phi_deg =(float)Math.toDegrees(phi);
	    	
	    		float mirrorAngle;
				if(phi_deg > 90 && phi_deg <= 180) {
					mirrorAngle = 180 - phi_deg;
					hue = ((mirrorAngle - 45 + 180)%180)/180f; 
				}
				else if (phi_deg > 180 && phi_deg <= 270) {
					mirrorAngle = 540 - phi_deg;
					hue = ((mirrorAngle - 45 + 180)%180)/180f;
				}
				else {
					hue = ((phi_deg - 45 + 180)%180)/180f; 
				}
	    		if(pS < .001) {
					pS = .001f;
				}
	    		sat = (float)((Math.sin(pS * theta))/(Math.sin(pS * piDivTwo)));
	    		Color c = Color.getHSBColor(hue, sat, 1f);
				red = c.getRed();
				green = c.getGreen();
				blue = c.getBlue();
				
				//normalize red,green,blue to between 0 and 1
				r = red/255f;
				g = green/255f;
				b = blue/255f;
				
				//shift blue
				blueShiftColors = shiftBlue(r,g,b);
				r = blueShiftColors[0];
				g = blueShiftColors[1];
				b = blueShiftColors[2];
				
				//gamma correction
				r = (float)Math.pow(r,(1/gamma));
				g = (float)Math.pow(g,(1/gamma));
				b = (float)Math.pow(b,(1/gamma));
				
				//now get r,g,b to 0 - 255 range
				red = Math.round(r * 255);
	    		green = Math.round(g * 255);
	    		blue = Math.round(b * 255);
				
				
				
    		}
    		
			//now do the orring
    		val = 0xff000000 | (red << 16) | (green << 8) | (blue);
    		
    		//now put replace in cleanImageBufferA
    		index = i/4;
    		cleanImageBufferA[index] = val;
    	}
	}
	
	
	
	
	
	/**
	 * blue shift
	 * @param r1
	 * @param g1
	 * @param b1
	 * @return
	 */
	public float[] shiftBlue(float r1, float g1, float b1) {
		float colors[] = new float[3];
		
		float b = b1/(r1+g1+b1);
		float cB = Math.max((3/2) * pB * (b-(1/3)) * pC, 0);
		float rS = (cB*b1) + ((1-cB)*r1);
		float gS = (cB*b1) + ((1-cB)*g1);
		float bS = b1;
		
		colors[0] = rS;
		colors[1] = gS;
		colors[2] = bS;
		
		return colors;
	}

	
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
