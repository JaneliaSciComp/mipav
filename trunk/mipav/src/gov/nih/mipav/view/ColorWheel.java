package gov.nih.mipav.view;


import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;

/**
 * @author pandyan
 * 
 * This is the Color Wheel class for the DTI Color Display Plugin
 * 
 * References: Developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group,
 * Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS from the
 * the NIH/NICHD/LIMB/STBB group and Olga Vogt from the NIH/CIT/DCB/ISL/BIRSS group:
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL)
 * Biomedical Imaging Research Services Section (BIRSS)
 * Imaging Sciences Laboratory (ISL)
 * Division of Cumputational Bioscience (DCB)
 * Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 * 
 * 
 * Publication Reference:
 * 
 * S. Pajevic and C. Pierpaoli, "Color Schemes to Represent the Orientation of Anisotropic Tissues from Diffusion Tensor Data: Application to White Matter Fiber Tract Mapping in the Human Brain," Magnetic Resonance in Medicine, vol. 42, no. 3, pp. 526-540, 1999
 *
 */ 
public class ColorWheel extends Canvas {

	/** offscreen image **/
	private Image offImage;
	
	/** offscreen graphics handle **/
	private Graphics offGraphics;
	
	/** radii of all the circles...r6 is the outermost radius **/
	private int r1,r2,r3,r4,r5,r6;
	
	/** type of color wheel 	ABSVAL, NOSYMM, ROTATIONALSYMM, MIRRORSYMM  **/
	private String type;
	
	/** sat vs theta **/
	private float pS = 0.5f;
	
	/** color range **/
	private float pC = .700f;
	
	/** blue saturation **/
	private float pB = .350f;
	
	/** gamma correction **/
	private float gamma = 1.8f;
	
	/** green adj **/
	private float pG = .800f;
	
	/** Stevens Beta **/
	private float stevensBeta = .4f;

	/** array of r,g,b values after blue shifting **/
	private float[] blueShiftColors = new float[3];
	
	/** array of r,g,b values after red shifting **/
	private float[] redShiftColors = new float[3];
	
	/** array of r,g,b values after green adj **/
	private float[] greenAdjColors = new float[3];
	
	
	/**
	 * constructor
	 * 
	 * @param type
	 * @param radius
	 */
	public ColorWheel(String initialType, int radius) {
		setSize(radius*2,radius*2);
		setBackground(Color.BLACK);
		this.r6 = radius;
		this.type = initialType;
		//calculates radii of interior circles using Lambertian equal areas
		calculateRadii();
	}

	
	/**
	 * paint method
	 */
	public void paint(Graphics g) {
		offImage = createImage(r6*2,r6*2);
		offGraphics = offImage.getGraphics();
		
		int r6_x = 0;
		int r6_y = 0;

		//draw main circle
		offGraphics.setColor(Color.BLACK);
		offGraphics.drawOval(r6_x, r6_y, r6*2, r6*2);
		offGraphics.setColor(Color.WHITE);
		offGraphics.fillOval(0, 0, r6*2, r6*2);

		//paint No Symmetry Color Wheel
		if(type.equals("NOSYMM")) {
			paintNoSymmColorWheel();
		}

		//paint Absolute Value Color Wheel
		if(type.equals("ABSVAL")) {
			paintAbsValColorWheel();
		}
		
		//paint Rotational Symmetry Color Wheel
		if(type.equals("ROTATIONALSYMM")) {
			paintRotationalSymmColorWheel();
		}
		
		//paint Mirror Symmetry Color Wheel
		if(type.equals("MIRRORSYMM")) {
			paintMirrorSymmColorWheel();
		}
		
		//x and y starting coordinates for remaining circles
		int r5_x = r6_x + (r6 - r5);
		int r5_y = r6_y + (r6 - r5);
		int r4_x = r6_x + (r6 - r4);
		int r4_y = r6_y + (r6 - r4);
		int r3_x = r6_x + (r6 - r3);
		int r3_y = r6_y + (r6 - r3);
		int r2_x = r6_x + (r6 - r2);
		int r2_y = r6_y + (r6 - r2);
		int r1_x = r6_x + (r6 - r1);
		int r1_y = r6_y + (r6 - r1);
		
		//draw rest of circles
		offGraphics.setColor(Color.BLACK);
		offGraphics.drawOval(r5_x, r5_y, r5*2, r5*2);
		offGraphics.drawOval(r4_x, r4_y, r4*2, r4*2);
		offGraphics.drawOval(r3_x, r3_y, r3*2, r3*2);
		offGraphics.drawOval(r2_x, r2_y, r2*2, r2*2);
		offGraphics.drawOval(r1_x, r1_y, r1*2, r1*2);
		
		//draw the perpendicular lines
		offGraphics.drawLine(r6, 0, r6, r6*2);
		offGraphics.drawLine(0, r6, r6*2, r6);
		
		//draw the rest of the angle lines
		int px1 = 0;
		int py1 = 0;
		int px2 = 0;
		int py2 = 0;
		double tanPiDiv4 = Math.tan(Math.PI/4);
		double tanPiDiv12 = Math.sin(Math.PI/12);
		double tanPiDiv6 = (float)Math.tan(Math.PI/6);
		
		
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		py1 = 0;
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(15))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(15))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		py1 = r6*2;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(15))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(15))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		py1 = 0;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(15))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(15))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		py1 = r6*2;
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(15))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(15))));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		py1 = 0;
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(30))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(30))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		py1 = r6*2;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(30))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(30))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		py1 = 0;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(30))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(30))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		py1 = r6*2;
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(30))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(30))));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv4));
		py1 = 0;
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(45))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(45))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv4));
		py1 = r6*2;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(45))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(45))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = 0;
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(45))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(45))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv4));
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(45))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(45))));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		
		px1 = r6*2;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(60))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(60))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(60))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(60))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(60))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(60))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(60))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(60))));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		
		px1 = r6*2;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(75))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(75))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(75))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(75))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		px2 = (int)(r6 + (r1 * Math.sin(Math.toRadians(75))));
		py2 = (int)(r6 + (r1 * Math.cos(Math.toRadians(75))));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		px2 = (int)(r6 - (r1 * Math.sin(Math.toRadians(75))));
		py2 = (int)(r6 - (r1 * Math.cos(Math.toRadians(75))));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		g.drawImage(offImage,0,0,this);
	}
	
	
	/**
	 * calculates radii of the interior circles 
	 * using Lambertian equal areas
	 * 
	 * equation used: r = 2 |sin(theta/2)|
	 *
	 */
	public void calculateRadii() {

		//first determine constant...we can calculate C becasue we know r6 since that is a parameter in the constructor
		//then get unscaled radius based on above equation and scale using constant

		double radians = Math.toRadians(90f/2f);
		float r_unscaled = (float)(2 * Math.sin(radians));
		float constant = r6/r_unscaled;
		radians = Math.toRadians(15f/2f);
		r_unscaled = (float)(2 * Math.sin(radians));
		r1 = Math.round(r_unscaled * constant);
		radians = Math.toRadians(30f/2f);
		r_unscaled = (float)(2 * Math.sin(radians));
		r2 = Math.round(r_unscaled * constant);
		radians = Math.toRadians(45f/2f);
		r_unscaled = (float)(2 * Math.sin(radians));
		r3 = Math.round(r_unscaled * constant);
		radians = Math.toRadians(60f/2f);
		r_unscaled = (float)(2 * Math.sin(radians));
		r4 = Math.round(r_unscaled * constant);
		radians = Math.toRadians(75f/2f);
		r_unscaled = (float)(2 * Math.sin(radians));
		r5 = Math.round(r_unscaled * constant);

		
	}
	
	
	//since Java's circle coordinate system has 0 at 3pm and goes counterclockwise
	//and the color circles need to be such that 0 is at 12pm and go clockwise,
	//the calculation of color wheels' hues are slightly different than those that
	//are cited in the colour paper
	//in addition, some of the hues have a + 90 in the equations....this is so that the red
	//appears at the 12pm position like they appear in the colour paper
	
	
	/**
	 * Paints the Absolute Value Color Wheel
	 */
	public void paintAbsValColorWheel() {
		Color c;
		float gammaInv= 1/gamma;
		for(int r=r6;r>=0;r-=1) {
			int arcw = 2*r;
			int arch = 2*r;
			int x = r6-arcw/2;
			int y = r6-arch/2;
			double ang = r/(r6*Math.sqrt(2));
			double theta = 2 * Math.asin(ang);
			for(int angle=0;angle<=360;angle++) {
				float phi = angle * ((float)Math.PI/180);
				float red = (Math.abs((float)Math.sin(theta) * (float)Math.cos(phi)));
				float green = (Math.abs((float)Math.sin(theta) * (float)Math.sin(phi)));
				float blue = (Math.abs((float)Math.cos(theta)));
				
				//shift blue
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//shift red
				redShiftColors = shiftRed(red,green,blue);
				red = redShiftColors[0];
				green = redShiftColors[1];
				blue = redShiftColors[2];
				
				//adjust green
				greenAdjColors = adjustGreen(red,green,blue);
				red = greenAdjColors[0];
				green = greenAdjColors[1];
				blue = greenAdjColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,gammaInv);
				green = (float)Math.pow(green,gammaInv);
				blue = (float)Math.pow(blue,gammaInv);
				
				c = new Color(red,green,blue);
				offGraphics.setColor(c);
				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
			}
		}
	}
	
	
	/**
	 * Paints the No Symmetry Color Wheel
	 */
	public void paintNoSymmColorWheel() {
		Color c;
		float gammaInv= 1/gamma;
		float rotConst = 1/360f;
		float normConst = 1/255f;
		double piDiv2 = Math.PI/2.0d;
		double sinConst = 1/Math.sin(pS * piDiv2);
		for(int r=r6;r>=0;r-=1) {
			int arcw = 2*r;
			int arch = 2*r;
			int x = r6-arcw/2;
			int y = r6-arch/2;
			double ang = r/(r6*Math.sqrt(2));
			double theta = 2 * Math.asin(ang);
			if(pS < .001) {
				pS = .001f;
			}
			float sat = (float)(Math.sin(pS*theta) * sinConst); 
			
			int hueAngle = 360;
			for(int angle=0;angle<=360;angle++) {
				float hue = ((hueAngle + 90 + 360)%360) * rotConst;
				c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed() * normConst;
				float green = c.getGreen() * normConst;
				float blue = c.getBlue() * normConst;

				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//shift red
				redShiftColors = shiftRed(red,green,blue);
				red = redShiftColors[0];
				green = redShiftColors[1];
				blue = redShiftColors[2];
				
				//adjust green
				greenAdjColors = adjustGreen(red,green,blue);
				red = greenAdjColors[0];
				green = greenAdjColors[1];
				blue = greenAdjColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,gammaInv);
				green = (float)Math.pow(green,gammaInv);
				blue = (float)Math.pow(blue,gammaInv);
				
				c = new Color(red,green,blue);
				offGraphics.setColor(c);

				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
				hueAngle--;
			}
		}
	}
	
	
	/**
	 * Paints the Rotational Symmetry Color Wheel
	 */
	public void paintRotationalSymmColorWheel() {
		Color c;
		float gammaInv= 1/gamma;
		float rotConst = 1/360f;
		float normConst = 1/255f;
		double piDiv2 = Math.PI/2.0d;
		double sinConst = 1/Math.sin(pS * piDiv2);
		for(int r=r6;r>=0;r-=1) {
			int arcw = 2*r;
			int arch = 2*r;
			int x = r6-arcw/2;
			int y = r6-arch/2;
			double ang = r/(r6*Math.sqrt(2));
			double theta = 2 * Math.asin(ang);
			if(pS < .001) {
				pS = .001f;
			}
			float sat = (float)(Math.sin(pS*theta) * sinConst); 
			int hueAngle = 360;
			for(int angle=0;angle<360;angle++) {
				//float hue = ((2 * (angle - 90 + 360))%360)/360f;
				float hue = ((2 * (hueAngle + 90 + 360))%360) * rotConst;
				c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed() * normConst;
				float green = c.getGreen() * normConst;
				float blue = c.getBlue() * normConst;
				
				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//shift red
				redShiftColors = shiftRed(red,green,blue);
				red = redShiftColors[0];
				green = redShiftColors[1];
				blue = redShiftColors[2];
				
				//adjust green
				greenAdjColors = adjustGreen(red,green,blue);
				red = greenAdjColors[0];
				green = greenAdjColors[1];
				blue = greenAdjColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,gammaInv);
				green = (float)Math.pow(green,gammaInv);
				blue = (float)Math.pow(blue,gammaInv);
				
				c = new Color(red,green,blue);
				
				offGraphics.setColor(c);
				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
				
				hueAngle--;
			}
		}
	}
	
	
	/**
	 * Paints the Mirror Symmetry Color Wheel
	 */
	public void paintMirrorSymmColorWheel() {
		Color c;
		float gammaInv= 1/gamma;
		float rotConst = 1/180f;
		float normConst = 1/255f;
		double piDiv2 = Math.PI/2.0d;
		double sinConst = 1/Math.sin(pS * piDiv2);
		for(int r=r6;r>=0;r-=1) {
			int arcw = 2*r;
			int arch = 2*r;
			int x = r6-arcw/2;
			int y = r6-arch/2;
			double ang = r/(r6*Math.sqrt(2));
			double theta = 2 * Math.asin(ang);
			if(pS < .001) {
				pS = .001f;
			}
			float sat = (float)(Math.sin(pS*theta) * sinConst); 
			for(int angle=0;angle<=360;angle++) {
				float hue;
				float mirrorAngle;
				if(angle > 90 && angle <= 180) {
					mirrorAngle = 180 - angle;
					hue = ((mirrorAngle - 45 + 180)%180) * rotConst;
				}
				else if (angle > 180 && angle <= 270) {
					mirrorAngle = 540 - angle;
					hue = ((mirrorAngle - 45 + 180)%180) * rotConst;
				}
				else {
					hue = ((angle - 45 + 180)%180) * rotConst;
				}
				
				c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed() * normConst;
				float green = c.getGreen() * normConst;
				float blue = c.getBlue() * normConst;

				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//shift red
				redShiftColors = shiftRed(red,green,blue);
				red = redShiftColors[0];
				green = redShiftColors[1];
				blue = redShiftColors[2];
				
				//adjust green
				greenAdjColors = adjustGreen(red,green,blue);
				red = greenAdjColors[0];
				green = greenAdjColors[1];
				blue = greenAdjColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,gammaInv);
				green = (float)Math.pow(green,gammaInv);
				blue = (float)Math.pow(blue,gammaInv);
				
				c = new Color(red,green,blue);
				
				offGraphics.setColor(c);
				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
			}
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
		
		float cB = Math.max((3/2f) * pB * (b-(1/3f)) * pC, 0f);
		
		float rS = (cB*b1) + ((1-cB)*r1);
		float gS = (cB*b1) + ((1-cB)*g1);
		float bS = b1;
		
		colors[0] = rS;
		colors[1] = gS;
		colors[2] = bS;
		
		return colors;
	}
	
	
	/**
	 * red shift
	 * @param r1
	 * @param g1
	 * @param b1
	 * @return
	 */
	public float[] shiftRed(float r1, float g1, float b1) {
		float colors[] = new float[3];
		float pR = pB/4f;
		
		float b = b1/(r1+g1+b1);
		
		float cB = Math.max((3/2f) * pR * (b-(1/3f)) * pC, 0f);
		
		float rS = (cB*b1) + ((1-cB)*r1);
		float gS = (cB*b1) + ((1-cB)*g1);
		float bS = b1;
		
		colors[0] = rS;
		colors[1] = gS;
		colors[2] = bS;
		
		
		return colors;
	}
	
	
	/**
	 * adjust green intensity
	 * @param r1
	 * @param g1
	 * @param b1
	 * @return
	 */
	public float[] adjustGreen(float r1, float g1, float b1) {
		float colors[] = new float[3];
		float max1 = Math.max(r1, g1);
		float max2 = Math.max(max1, b1);
		float maxVal = Math.max(max2, .0000001f);
		r1 = r1/maxVal;
		g1 = g1/maxVal;
		b1 = b1/maxVal;
		float thrd = 1/3f;
		float c1 = thrd - (pG/25f);
		float c2 = thrd + (pG/4f);
		float leql = 0.7f;
		float totalVal = (float)(((c1*r1) + (c2*g1) + ((1 - c2 - stevensBeta) * b1))/Math.pow(leql, (1/stevensBeta)));
		if(totalVal < 1) {
			totalVal = 1;
		}
		r1 = r1/(pC * totalVal + (1 - pC));
		g1 = g1/(pC * totalVal + (1 - pC));
		b1 = b1/(pC * totalVal + (1 - pC));

		colors[0] = r1;
		colors[1] = g1;
		colors[2] = b1;
		return colors;
	}	
	
	
	
	
	/**
     * Calls paint without erasing background - this reduces flicker!
     *
     * @param  g  Graphics handle
     */
    public void update(Graphics g) {
        paint(g);
    }


	public void setGamma(float gamma) {
		this.gamma = gamma;
	}


	public void setPB(float pb) {
		pB = pb;
	}


	public void setPC(float pc) {
		pC = pc;
	}


	public void setPG(float pg) {
		pG = pg;
	}


	public void setPS(float ps) {
		pS = ps;
	}


	public void setStevensBeta(float stevensBeta) {
		this.stevensBeta = stevensBeta;
	}


	public void setType(String type) {
		this.type = type;
	}


	public String getType() {
		return type;
	}
    
	
    
    
    
    
}
