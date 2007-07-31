import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;

/**
 * @author pandyan
 * 
 * This is the main dialog for the DTI Color Display
 * 
 * References: This algorithm was developed in concert with Sinisa Pajevic from the NIH/CIT/DCB/MSCL group and
 * Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * 
 * Mathematical and Statistical Computing Laboratory (MSCL)
 * Division of Cumputational Bioscience (DCB)
 * Center for Informational Technology (CIT)
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
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

	/** array of r,g,b values after blue sgifting **/
	private float blueShiftColors[] = new float[3];
	
	
	/**
	 * constructor
	 * 
	 * @param type
	 * @param radius
	 */
	public ColorWheel(String type, int radius) {
		setSize(radius*2,radius*2);
		setBackground(Color.BLACK);
		this.r6 = radius;
		this.type = type;
		
		//calculates radii of interior circles using Lambertian equal areas
		calculateRadii();
		
	}
	
	
	
	/**
	 * constructor
	 * 
	 * @param type
	 * @param radius
	 */
	public ColorWheel(String type, int radius, float pB, float pC, float pS, float gamma ) {
		setSize(radius*2,radius*2);
		setBackground(Color.BLACK);
		this.r6 = radius;
		this.type = type;
		this.pB = pB;
		this.pC = pC;
		this.pS = pS;
		this.gamma = gamma;
		
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
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv4));
		py1 = 0;
		px2 = (int)Math.round(r6 - (r6 * tanPiDiv4));
		py2 = r6*2;
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = 0;
		py1 = 0;
		px2 = r6*2;
		py2 = (int)Math.round(r6 + (r6 * tanPiDiv4));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		double tanPiDiv12 = Math.sin(Math.PI/12);
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		py1 = 0;
		px2 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		py2 = r6*2;
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		py1 = 0;
		px2 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		py2 = r6*2;
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		px2 = 0;
		py2 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv12));
		px2 = 0;
		py2 = (int)Math.round(r6 - (r6 * tanPiDiv12));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		double tanPiDiv6 = (float)Math.tan(Math.PI/6);
		px1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		py1 = 0;
		px2 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		py2 = r6*2;
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		py1 = 0;
		px2 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		py2 = r6*2;
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		px2 = 0;
		py2 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		offGraphics.drawLine(px1, py1, px2, py2);
		px1 = r6*2;
		py1 = (int)Math.round(r6 + (r6 * tanPiDiv6));
		px2 = 0;
		py2 = (int)Math.round(r6 - (r6 * tanPiDiv6));
		offGraphics.drawLine(px1, py1, px2, py2);
		
		g.drawImage(offImage,0,0,this);

		
	}
	
	/**
	 * calculates radii of the interior circles 
	 * using Lambertian equal areas
	 *
	 */
	public void calculateRadii() {
		float r1_f = (float)Math.sqrt((r6 * r6)/6);
		r1 = Math.round(r1_f);
		float r5_f = (float)Math.sqrt((r1 * r1) * 5);
		r5 = Math.round(r5_f);
		float r4_f = (float)Math.sqrt((r1 * r1) * 4);
		r4 = Math.round(r4_f);
		float r3_f = (float)Math.sqrt((r1 * r1) * 3);
		r3 = Math.round(r3_f);
		float r2_f = (float)Math.sqrt((r1 * r1) * 2);
		r2 = Math.round(r2_f);	
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
				
				//gamma correction
				red = (float)Math.pow(red,(1/gamma));
				green = (float)Math.pow(green,(1/gamma));
				blue = (float)Math.pow(blue,(1/gamma));
				
				Color c = new Color(red,green,blue);
				offGraphics.setColor(c);
				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
			}
		}
		
	}
	
	/**
	 * Paints the No Symmetry Color Wheel
	 */
	public void paintNoSymmColorWheel() {
		
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
			float sat = (float)(Math.sin(pS*theta)/Math.sin(pS * (Math.PI/2))); 
			
			int hueAngle = 360;
			for(int angle=0;angle<=360;angle++) {
				//float hue = ((angle - 90 + 360)%360)/360f; (cited in paper, but resulted in incorrect colour wheel b/c of mismatch between coordinate systems
				float hue = ((hueAngle + 90 + 360)%360)/360f; 
				Color c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed()/255f;
				float green = c.getGreen()/255f;
				float blue = c.getBlue()/255f;

				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,(1/gamma));
				green = (float)Math.pow(green,(1/gamma));
				blue = (float)Math.pow(blue,(1/gamma));
				
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
			float sat = (float)(Math.sin(pS*theta)/Math.sin(pS * (Math.PI/2))); 
			int hueAngle = 360;
			for(int angle=0;angle<=360;angle++) {
				//float hue = ((2 * (angle - 90 + 360))%360)/360f;
				float hue = ((2 * (hueAngle + 90 + 360))%360)/360f;
				Color c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed()/255f;
				float green = c.getGreen()/255f;
				float blue = c.getBlue()/255f;

				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,(1/gamma));
				green = (float)Math.pow(green,(1/gamma));
				blue = (float)Math.pow(blue,(1/gamma));
				
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
			float sat = (float)(Math.sin(pS*theta)/Math.sin(pS * (Math.PI/2))); 
			//int hueAngle = 360;
			for(int angle=0;angle<=360;angle++) {
				float hue;
				float mirrorAngle;
				if(angle > 90 && angle <= 180) {
					mirrorAngle = 180 - angle;
					hue = ((mirrorAngle - 45 + 180)%180)/180f; 
				}
				else if (angle > 180 && angle <= 270) {
					mirrorAngle = 540 - angle;
					hue = ((mirrorAngle - 45 + 180)%180)/180f;
				}
				else {
					hue = ((angle - 45 + 180)%180)/180f; 
				}
				//float hue = ((2 * (hueAngle + 90 + 360 ))%360)/360f;
				Color c = Color.getHSBColor(hue, sat, 1f);
				
				float red = c.getRed()/255f;
				float green = c.getGreen()/255f;
				float blue = c.getBlue()/255f;

				//blue shift
				blueShiftColors = shiftBlue(red,green,blue);
				red = blueShiftColors[0];
				green = blueShiftColors[1];
				blue = blueShiftColors[2];
				
				//gamma correction
				red = (float)Math.pow(red,(1/gamma));
				green = (float)Math.pow(green,(1/gamma));
				blue = (float)Math.pow(blue,(1/gamma));
				
				c = new Color(red,green,blue);
				
				offGraphics.setColor(c);
				offGraphics.fillArc(x, y, arcw, arch, angle, 1);
				//hueAngle--;
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
		
		float cB = Math.max((3/2) * pB * (b-(1/3)) * pC, 0);
		
		float rS = (cB*b1) + ((1-cB)*r1);
		float gS = (cB*b1) + ((1-cB)*g1);
		float bS = b1;
		
		colors[0] = rS;
		colors[1] = gS;
		colors[2] = bS;
		
		return colors;
	}
	
	
	
	
	
}
