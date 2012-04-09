package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.appkit.AppKit;
import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.process.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import java.util.*;

public class MCVolumePanel extends JPanel implements KeyListener, MouseListener,MouseMotionListener {
	double imAspect=1.0;
    boolean isPacked, normMode;
	protected Image	imgNew;
	protected int 	imWidth,imHeight,imArea,imDepth,imChannels,imSlices;
	int invert;
	int	lastx,lasty;  // in image coordinates
	MediaTracker	tracker;
	ImageFilter   colorfilter ;
	MemoryImageSource mi = null;
	String todraw;
	MCVolume v = null;
    protected short[] packedData = null; // packed data
	protected byte[] scaledData = null; // scaled data
	protected int sliceIndex = 0;
	protected int channelIndex = 0;
	int	windowWidth=128, windowLevel=128 ;
	ColorModel cm = getColorModel();
	Vector overlays = new Vector();
	Vector images = new Vector();

    // normMode = true for slice-by-slice, false for whole volume

    public MCVolumePanel(MCVolume b, boolean normMode) {
		this(b, normMode, false);
	}

	public MCVolumePanel(MCVolume b, boolean normMode, boolean scaleMode) {

		//-------------------------------------------------------------------
		//
		//  Read in the data. 
		//
		imHeight= b.getNRows();
		imWidth = b.getNCols();
		imSlices = b.getNSlices();
		imChannels = b.getNChannels();
		imArea = imHeight * imWidth;
		imDepth = imSlices * imChannels;
		imAspect = (double)imWidth/(double)imHeight;

        this.isPacked = !scaleMode;
		this.normMode = normMode;

        if (!isPacked)
			{ v = b; scaledData = scaleBasic(); }
		else
			{ packedData = packVolume(b); scaledData = scalePercentile(); }

		//-------------------------------------------------------------------
		//
		//  Create the images
		//
		mi = new MemoryImageSource(imWidth, imHeight, cm, scaledData, 0, imWidth);
		//mi.newPixels(scaledData, getColorModel(), 0, imWidth);
		imgNew = createImage( mi );

		//-------------------------------------------------------------------
		//
		//  Create the media tracker (WHY??)
		//
		/* tracker = new MediaTracker (this);
		tracker.addImage(imgNew,0);
		try{
			tracker.waitForID(0);
		} catch (InterruptedException e){}

		if(tracker.isErrorID (0) ){
			System.out.println("Error fetching image");
			return;
		} // image loaded */

		//-------------------------------------------------------------------
		//
		//  Misc 
		//
		lastx = imWidth;
		lasty = imHeight/2;
		invert = 0;

		setSize(imWidth,imHeight); 

		colorfilter = new wwImgFilter(windowWidth,windowLevel,invert);
		imgNew=createImage(new FilteredImageSource(mi,colorfilter));
		repaint();
	}

	/** 
	 *  Set a row of data (used primarily by t2map to display the 
	 *  myelin water map as we are calculating it).
	 */
	public void setRow(float[] val, int row, int slice, int ch)
	{
//		v.setRow(val, row, slice, ch);
//		scaledData = scaleBasic();
//		mi = new MemoryImageSource(imWidth, imHeight, cm, scaledData, 0, imWidth);
//		imgNew=createImage(new FilteredImageSource(mi,colorfilter));
//		repaint();

		v.setRow(val, row, slice, ch);
		scaledData = scaleBasic();
		mi.newPixels(scaledData, getColorModel(), 0, imWidth);
		imgNew.flush();
		repaint();
	}

	protected byte[] scaleBasic()
	{
		byte[] s = new byte[imArea*imDepth];

		double m = Statistics.min(v), M = Statistics.max(v);

		int ii=0;

		for(int si=0; si<imSlices; si++) {
			for(int ei=0; ei<imChannels; ei++) {
				if (normMode)
				{
					m = Statistics.min(v.getSlice(si,ei));
					M = Statistics.max(v.getSlice(si,ei));
				}
				for(int ri=0; ri<imHeight; ri++) {
					float[] t = v.getRow(ri,si,ei);
					for(int ci=0; ci<imWidth; ci++) {
						s[ii++] = (byte)((t[ci]-m)*255.0/(M-m));
					}
				}
			}
		}

		return s;
	}

	protected byte[] scalePercentile()
	{
		byte[] s = new byte[imArea*imDepth];
		//-------------------------------------------------------------------
		//
		//  Compute the percentile
		//
		int[] si_prctl = new int[imDepth];

		if (normMode)
		{
			for(int i=0;i<imDepth;i++)
			{
				si_prctl[i] = percentile(99.9, imArea*i, imArea*(i+1));
			}
		}
		else
		{
			int p = percentile(99.99, 0, imArea*imDepth);
			java.util.Arrays.fill(si_prctl, p);
		}

		//-------------------------------------------------------------------
		//
		//  Scale the data to be between 0 and 255
		//
		int ii, jj, slice;

		for(slice=0;slice<imDepth;slice++)
		{
			int offset = slice * imArea;

			for(ii=0; ii<imArea; ii++) {
				int u = unpack(packedData[ii+offset]);

				if( u >= si_prctl[slice] ) {
					s[ii+offset] = (byte)0xff;
				}
				else if( u < 0 ) {
					s[ii+offset] = 0;
				}
				else {
					jj = ((int)((double)u / (double)(si_prctl[slice]) * 255.0));
					s[ii+offset] = (byte)jj;
				}
			}
		}

		return s;
	}

	protected short[] packVolume(MCVolume b)
	{
		short[] t = new short[imArea*imDepth];

		int ii=0;
		for(int si=0; si<imSlices; si++) {
			for(int ei=0; ei<imChannels; ei++) {
				for(int ri=0; ri<imHeight; ri++) {
					float[] t2 = b.getRow(ri,si,ei);
					for(int ci=0; ci<imWidth; ci++) {
						t[ii++] = pack((int)t2[ci]);
					}
				}
			}
		}

		return t;
	}

	public void addOverlay(RegionList r, Color c, char ch, int nrows, int ncols, 
		int nslices)
	{
		addOverlay(r, c, ch, nrows, ncols, nslices, "");
	}

	public void addOverlay(RegionList r, Color c, char ch, int nrows, int ncols, 
		int nslices, String name)
	{
		Overlay o = new Overlay(r,c,ch,nrows, ncols,nslices, name); 
		overlays.add(o); 
		images.add(o.getImage(0));
	}

	/** 
	 *  Add an overlay without saying what the string was.
	 */
	public void addOverlay(Volume v, Color c, char ch)
	{
		addOverlay(v, c, ch, "");
	}

	/** 
	 *  Add an overlay with the name of the overlay.
	 */
	public void addOverlay(Volume v, Color c, char ch, String fn)
	{ 	
		Overlay o = new Overlay(v,c,ch,fn); 
		overlays.add(o); 
		images.add(o.getImage(0)); 
	}

	protected short pack(int x)
	  { if (x<(1<<15)) return (short)x; else return (short)(x-(1<<16)); }

	protected int unpack(short x)
	  { if (x<0) return ((int)x+(1<<16)); else return x; }

	public float getData(int x, int y, int z )
	{  
		if (isPacked)
			return unpack(packedData[(sliceIndex*imHeight+y)*imWidth+x]);
		else
			return v.getData(y, x, z/imChannels, z%imChannels);
	}

	public void mouseStuff(MouseEvent e) {
		//-------------------------------------------------------------------
		//
		//  Compute the X and Y location in IMAGE coordinates.
		//
		int w = getWidth(), h = getHeight(), s = Math.min(w,h);

		lastx = (int)(((double)(e.getX()-(w-s)/2)/s)*(double)imWidth); 
		lasty = (int)(((double)(e.getY()-(h-s)/2)/s)*(double)imHeight);

		//-------------------------------------------------------------------
		//
		//  See if it is BUTTON 1
		//
		if( (e.getModifiers() & MouseEvent.BUTTON1_MASK) == MouseEvent.BUTTON1_MASK ) {
			lastx = Math.max(lastx,0); lastx = Math.min(lastx,imWidth-1);
			lasty = Math.max(lasty,0); lasty = Math.min(lasty,imHeight-1);

			todraw = "(" + lastx + ", " + lasty + ") = " + 
				getData(lastx, lasty, sliceIndex);

			repaint();
		}
		//-------------------------------------------------------------------
		//
		//  See if it is BUTTON 2
		//
		else if( (e.getModifiers() & MouseEvent.BUTTON2_MASK) == MouseEvent.BUTTON2_MASK ) {
		}
		//-------------------------------------------------------------------
		//
		//  See if it is BUTTON 3
		//
		else if( (e.getModifiers() & MouseEvent.BUTTON3_MASK) == MouseEvent.BUTTON3_MASK ) {

			// The window width and level should be scaled to be between 
			// 0 and 255.
			windowWidth = (int)(((double)e.getX()/(double)super.getWidth())*255.0); 
			windowLevel = (int)(((double)e.getY()/(double)super.getHeight())*255.0); 

			colorfilter = new wwImgFilter(windowWidth,windowLevel,invert);
			imgNew=createImage(new FilteredImageSource(mi,colorfilter));
			repaint();
		}
	}

	public void mouseDragged(MouseEvent e) {
		mouseStuff(e);
	}

	public void mouseMoved(MouseEvent e) {
		//lastx = e.getX();
		//lasty = e.getY();
	}

    public void mouseReleased(MouseEvent e) {}
    public void mousePressed(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseClicked(MouseEvent e) {
		mouseStuff(e);
	}

	/**
	 *  Overloaded function of what to do when a key is released.
	 *
	 *  @param e - keyEvent 
	 *  @return none
	 */
	public void keyReleased(KeyEvent e) {}

	/**
	 *  Overloaded function of what to do when a key is pressed.
	 *
	 *  @param e - keyEvent 
	 *  @return none
	 */
	public void keyTyped(KeyEvent e) {}

	/**
	 *  Overloaded function of what to do when a key is typed.
	 *
	 *  @param e - keyEvent 
	 *  @return none
	 */
	public void keyPressed(KeyEvent e)
	{
		boolean unknownKey = false;
		int c = (char) e.getKeyChar();
			
		if( e.getKeyCode() == KeyEvent.VK_DOWN )
			c = 'p';
		else if ( e.getKeyCode() == KeyEvent.VK_LEFT )
			c = 800;
		else if( e.getKeyCode() == KeyEvent.VK_UP )
			c = 'n';
		else if ( e.getKeyCode() == KeyEvent.VK_RIGHT )
			c = 801;

		switch(c) {
			//
			//  invert the imae
			//
			case 'i':
				if( invert == 1) { 
					invert = 0;
				}
				else {
					invert = 1;
				}
			break;
			//
			//  quit 
			//
			case 'q':
				System.exit(0);
			break;
			//
			//  move up 
			//
			case 'n':
				if( sliceIndex < imDepth-imChannels ) sliceIndex+=imChannels;
				displaySlice(sliceIndex);
			break;
			//
			//  move down 
			//
			case 'p':
				if( sliceIndex >= imChannels ) sliceIndex-=imChannels;
				displaySlice(sliceIndex);
			break;

			case 800:
				if (sliceIndex % imChannels == 0) sliceIndex+=imChannels;
				sliceIndex--;
				displaySlice(sliceIndex);
			break;

			case 801:
			case 'c':
				sliceIndex++;
				if (sliceIndex % imChannels == 0) sliceIndex-=imChannels;
				displaySlice(sliceIndex);
			break;
			//
			//  return to normal colomap 
			//
			case 'r':
				invert = 0;
				windowWidth=128; 
				windowLevel=128;
			break;

            case 'M':
				AppKit.memStatus(System.out, true);
            break;

			default:
				boolean hit=false;
				for(int i=0;i<overlays.size();i++)
				{
					Overlay o = (Overlay) overlays.get(i);
					if (o.respondTo(c)) hit=true;
				}
				if (hit) { displaySlice(sliceIndex); }
				else { unknownKey = !otherKeyTyped(e); }
			break;
		}

		//-------------------------------------------------------------------
		//
		//  Create the new image filter.
		//
		if( !unknownKey ) {
			colorfilter = new wwImgFilter(windowWidth,windowLevel,invert);
			imgNew=createImage(new FilteredImageSource(mi,colorfilter));
			repaint();
		}
	}

    /**
     *  Handle additional keystrokes (for subclasses).
     *  @param e - key event
     *  @return True if the key event was recognised
     */
	protected boolean otherKeyTyped(KeyEvent e) { return false; }

	/**
	 *  Display the slice given the slice index.
	 *
	 *  @param sliceNumber - the _index_ of the slice
	 *  @return none
	 */
	public void displaySlice(int sliceNumber) {
		mi.newPixels(scaledData, cm, sliceNumber*imArea, imWidth);
		imgNew = createImage(mi);

		for(int i=overlays.size()-1;i>=0;i--)
		{
			Overlay o = (Overlay)overlays.get(i);
			if (o.inUse())
				images.set(i,o.getImage(sliceIndex/imChannels));
		}
	}

	/**
	 *  Overloaded udpate.
	 *
	 *  @param g - The graphics context 
	 *  @return none
	 */
	public void update(Graphics g) {
		paint(g);
	}

	/**
	 *  Overloaded paintContent.
	 *
	 *  @param g - The graphics context 
	 *  @return none
	 */
	public void paintComponent(Graphics g) {
		int sw = (int)getWidth();
		int sh = (int)getHeight();
		int sm = (int)Math.min(sw/imAspect,sh);
		int sm2 = (int)(sm*imAspect);

		super.paintComponent(g);

		g.setColor(Color.black); g.fillRect(0,0,sw,sh);

		g.drawImage(imgNew, (sw-sm2)/2, (sh-sm)/2, sm2, sm, this);

		// FontMetrics used to right justify the overlay name.
		FontMetrics fm = g.getFontMetrics();

		// Draw the overlays and the names.
		for(int i=overlays.size()-1;i>=0;i--)
		{
			Overlay o = (Overlay)overlays.get(i);
			if (o.inUse())
			{ 
				// Draw the overlay.
				g.drawImage((Image)images.get(i), (sw-sm2)/2, (sh-sm)/2, sm2, sm, this);

				// Draw name if non-empty.
				if( o.getName().length() > 0 )
				{
					g.setColor(o.getColor());
					g.drawString(o.getName(), 
						sw-fm.stringWidth(o.getName())-5, (overlays.size()-i)*10);
				}
			}
		}

		// Draw the coordinates and pixel value.
		if( todraw != null ) {
			g.setColor(Color.green);
			g.drawString(todraw, 10,10);
		}

		// Draw the slice number.
		g.setColor(Color.green);

		String s = "Slice " + (sliceIndex/imChannels+1);
		if (imChannels > 1) s += " Channel "+(sliceIndex%imChannels + 1);
		g.drawString(s,10,sh-10);
	}

	private int percentile(double prc, int start, int end)
	{
		int maxnum, minnum;
		maxnum = minnum = unpack(packedData[start]);

		for(int ii=start; ii<end; ii++)
		{
			int u = unpack(packedData[ii]);
			if (u > maxnum) maxnum = u;
			else if (u < minnum) minnum = u;
		}

		int target = (int)( (double)prc / (double)100.0 * (double)(end-start) );
		int range = maxnum - minnum;
		
		if (range < 1) return maxnum;

		int[] counts = new int[range + 1];

		for(int ii=0; ii<range+1; ii++) {
			counts[ii] = 0;
		}

		for(int ii=start; ii<end; ii++) {
			counts[ unpack(packedData[ii])-minnum]++;
		}

		int match = 0;
		for(int ii=1; ii <= range; ii++) {
			counts[ii] += counts[ii-1];
			if( counts[ii] <= target ) match = ii; else break;
		}

		return match + minnum;
	}
}

class wwImgFilter extends RGBImageFilter {
	//
	//  Everything here assumes that we are going from 
	//  0 to 255.
	//
	int windowWidth,windowLevel,invert;
	int halfWidth;

	public wwImgFilter( int windowWidth_in, int windowLevel_in, int invert_in) {
		//-------------------------------------------------------------------
		//
		//  Copy the parameters internally
		//
		invert = invert_in;
		windowWidth = 2*windowWidth_in;
		windowLevel = windowLevel_in;
		halfWidth = (int)( (double)windowWidth / 2.0 );

		canFilterIndexColorModel = true;
	}

	public int filterRGB(int x, int y, int rgb) {
		int r = rgb & 0xff;
		int r_out = r;
		
		if( invert == 1 ) { r = 255-r; }

		if( r < windowLevel-halfWidth ) {
			r_out = 0;
		}
		else if( r > windowLevel+halfWidth ) {
			r_out = 255;
		}
		else {
			r_out = (int)((double)(r- (windowLevel-halfWidth) ) / (double)windowWidth * 255.0);
		}

		return (rgb & 0xff000000) | (r_out << 16) | (r_out << 8) | (r_out << 0);
	}
}

class Overlay extends Component {
	Color c;
	char ch;
	boolean visible;
	int nCols,nRows,nSlices,sliceSize;
	byte[] src;
	byte[][] colors;
	ColorModel cm;
	MemoryImageSource mi;
	Image[] images;
	String name = "";

	Overlay(RegionList r, Color c, char ch, int nRows, int nCols, 
			int nSlices, String name)
	{
		this.c = c;
		this.ch = ch;
		this.visible = true;
		this.nRows = nRows;
		this.nCols = nCols;
		this.nSlices = nSlices;
		this.sliceSize = nRows*nCols;
		this.name = name;

		src = new byte[sliceSize*nSlices];

		int i=0;
		for(int sl=0;sl<nSlices;sl++)
			for(int row=0;row<nRows;row++)
				for(int col=0;col<nCols;col++)
					src[i++] = (byte)(255*r.getData(row,col,sl));

		colors = new byte[4][256];

		Arrays.fill(colors[0],(byte)c.getRed());
		Arrays.fill(colors[1],(byte)c.getGreen());
		Arrays.fill(colors[2],(byte)c.getBlue());
		int x = c.getAlpha(); for(i=0;i<256;i++) colors[3][i]=(byte)(i*x/255);

		cm = new IndexColorModel(8,256,colors[0],colors[1],colors[2],colors[3]);
		mi = new MemoryImageSource(nCols, nRows, cm, src, 0, nCols);
	}

	Overlay(Volume v, Color c, char ch, String name)
	{
		this.c = c;
		this.ch = ch;
		this.visible = true;
		this.nRows = v.getNRows();
		this.nCols = v.getNCols();
		this.nSlices = v.getNSlices();
		this.sliceSize = nRows*nCols;
		this.name = name;

		src = new byte[sliceSize*nSlices];

		int i=0;
		for(int sl=0;sl<nSlices;sl++)
			for(int row=0;row<nRows;row++)
				for(int col=0;col<nCols;col++)
					src[i++] = (byte)(255*v.getData(row,col,sl));

		colors = new byte[4][256];

		Arrays.fill(colors[0],(byte)c.getRed());
		Arrays.fill(colors[1],(byte)c.getGreen());
		Arrays.fill(colors[2],(byte)c.getBlue());
		int x = c.getAlpha(); for(i=0;i<256;i++) colors[3][i]=(byte)(i*x/255);

		cm = new IndexColorModel(8,256,colors[0],colors[1],colors[2],colors[3]);
		mi = new MemoryImageSource(nCols, nRows, cm, src, 0, nCols);
	}

	final public Color getColor()
	{
		Color toreturn = new Color(c.getRed(), c.getGreen(), c.getBlue());
		return toreturn;
	}

	final public String getName()
	{
		return name;
	}

	boolean respondTo(int ch)
	{
		if (this.ch == ch) { visible = !visible; return true; }
		return false;
	}

	boolean inUse() { return this.visible; }

	Image getImage(int slice)
	{
		mi.newPixels(src,cm,slice*sliceSize,nCols);
		return createImage(mi);
	}
}
