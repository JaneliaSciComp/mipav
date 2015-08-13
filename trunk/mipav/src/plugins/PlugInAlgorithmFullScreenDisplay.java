import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.PatientSlice;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.WindowLevel;

import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;

import javax.swing.JTextArea;


public class PlugInAlgorithmFullScreenDisplay extends AlgorithmBase implements MouseWheelListener, MouseMotionListener {
    private final BufferedImage inputImage;

    private final Image cornerImage;
    
    private int[][] imageData;
    
    private int zOffset;
    
    private boolean isColorImage;
    
    private int inputWidth;
    
    private int inputHeight;
    
    private int zDim;
    
    private int bufferData[] = null;
    
    private int length;
    
    private Frame frame;

    private final JTextArea outputTextArea;
    
    /** User invokes window and level adjustment with right mouse drag in DEFAULT mode. */
    private boolean winLevelSet = false;
    
    /** PatientSlice contains all the Patient Coordinate system view-specific data for rendering this component:. */
    protected PatientSlice m_kPatientSlice;
    
    /**
     * Member variables used to adjust the window and level (contrast and
     * brightness) by dragging with the right-mouse button:.
     */
    private float[] m_afXWin = new float[4];

    /**
     * Member variables used to adjust the window and level (contrast and
     * brightness) by dragging with the right-mouse button:.
     */
    private float[] m_afYWin = new float[4];

    /** image max value */
    private float m_fMax = Float.MIN_VALUE;
    /** image min value */
    private float m_fMin = Float.MAX_VALUE;

    /** previous mouse x-position */
    private float m_fOldX;

    /** previous mouse y-position */
    private float m_fOldY;
    
    /** If true, window/level adjusted relative to the current transfer function values
     *  If false, window/level adjusted to the absolute image position values
     */
    private boolean doRelative = true;
    
    private float old_fWindow;
    
    private float old_fLevel;
    
    private ModelLUT LUTa = null;
    
    private ModelRGB RGBa = null;


    public PlugInAlgorithmFullScreenDisplay(ModelImage image, final BufferedImage inputImage, final Image cornerImage, 
    		int imageData[][], int zOffset, final JTextArea outputTextArea) {
    	super(null, image);
        this.inputImage = inputImage;
        this.cornerImage = cornerImage;
        this.imageData = imageData;
        this.zOffset = zOffset;
        this.outputTextArea = outputTextArea;
        this.isColorImage = image.isColorImage();
    }

    @Override
    public void runAlgorithm() {
        outputTextArea.append("Running Algorithm v1.0" + "\n");

        final long begTime = System.currentTimeMillis();
        inputWidth = inputImage.getWidth();
        inputHeight = inputImage.getHeight();
        length = inputWidth * inputHeight;
        zDim = imageData.length;
        if (isColorImage) {
        	RGBa = ViewJFrameBase.initRGB(srcImage);
        }
        else {
            LUTa = ViewJFrameBase.initLUT(srcImage);
        }

        frame = new Frame("Test");
        frame.setUndecorated(true);
        frame.add(new Component() {
            @Override
            public void paint(final Graphics g) {
                super.paint(g);
                final double widthRatio = (double) (getWidth() - 160) / (double) inputWidth;
                final double heightRatio = (double) (getHeight() - 158) / (double) inputHeight;
                final BufferedImage backgroundImage = new BufferedImage(getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB);
                final int screenLength = getWidth() * getHeight();
                final int[] screenData = new int[screenLength * 4];
                for (int i = 0; i < screenLength; i++) {
                    screenData[i * 4 + 0] = 0;
                    screenData[i * 4 + 1] = 0;
                    screenData[i * 4 + 2] = 0;
                    screenData[i * 4 + 3] = 255;
                }
                backgroundImage.getRaster().setPixels(0, 0, getWidth(), getHeight(), screenData);
                g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
                if (widthRatio > heightRatio) {
                    // Can only expand by the heightRatio
                    final int expWidth = (int) Math.floor(inputWidth * heightRatio);
                    final int leftPadding = (getWidth() - 160 - expWidth) / 2;
                    g.drawImage(inputImage, 80 + leftPadding, 79, expWidth, getHeight() - 158, this);
                } else {
                    // Can only expand by the widthRatio
                    final int expHeight = (int) Math.floor(inputHeight * widthRatio);
                    final int topPadding = (getHeight() - 158 - expHeight) / 2;
                    g.drawImage(inputImage, 80, 79 + topPadding, getWidth() - 160, expHeight, this);
                }
                if (cornerImage != null) {
                    g.drawImage(cornerImage, 0, 0, 80, 79, this);
                    g.drawImage(cornerImage, getWidth() - 80, 0, 80, 79, this);
                    g.drawImage(cornerImage, 0, getHeight() - 79, 80, 79, this);
                    g.drawImage(cornerImage, getWidth() - 80, getHeight() - 70, 80, 79, this);
                } // if (cornerImage != null)
            }
        });
        frame.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(final MouseEvent e) {
            	if (e.getButton() == MouseEvent.BUTTON1) {
                    System.exit(0);
            	}
            }

            @Override
            public void mousePressed(final MouseEvent e) {}

            @Override
            public void mouseReleased(final MouseEvent e) {
            	//updates winlevel if right mouse button was pressed and user's preferences indicate this should occur
                if ( (e.getModifiers() & InputEvent.BUTTON3_MASK) != 0) {
                    if (winLevelSet) {
                        winLevelSet = false;
                    }
                    frame.setCursor(MipavUtil.defaultCursor);
                    
                }
            }

            @Override
            public void mouseEntered(final MouseEvent e) {}

            @Override
            public void mouseExited(final MouseEvent e) {}
        });
        bufferData = new int[4 * length];
        if (zDim > 1) {
        	frame.addMouseWheelListener(this);
        } // if (zDim > 1)
        frame.addMouseMotionListener(this);
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice gs = ge.getDefaultScreenDevice();
        gs.setFullScreenWindow(frame);
        frame.validate();

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

    }
    
public void mouseWheelMoved(final MouseWheelEvent mouseWheelEvent) {
	int i;
        final int wheelRotation = mouseWheelEvent.getWheelRotation();
        if ((wheelRotation < 0) && (zOffset < zDim - 1)) {
        	// Increment slice
        	zOffset++;
        	if (isColorImage) {
        		int RGBIndexBufferA[] = RGBa.exportIndexedRGB();
                int redMapped;
                int greenMapped;
                int blueMapped;
        	    for (i = 0; i < length; i++) {
        	        if (RGBa.getROn()) {
        	            redMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 1]] & 0x00ff0000) >> 16;
        	        } else {
        	            redMapped = 0;
        	        }
        	
        	        if (RGBa.getGOn()) {
        	            greenMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 2]] & 0x0000ff00) >> 8;
        	        } else {
        	            greenMapped = 0;
        	        }
        	
        	        if (RGBa.getBOn()) {
        	            blueMapped = (RGBIndexBufferA[imageData[zOffset][4 * i + 3]] & 0x000000ff);
        	        } else {
        	            blueMapped = 0;
        	        }
        	        bufferData[i * 4 + 0] = redMapped;
        	        bufferData[i * 4 + 1] = greenMapped;
        	        bufferData[i * 4 + 2] = blueMapped;
        	        bufferData[i * 4 + 3] = 255;
        	    } // for (i = 0; i < length; i++)
        	} // if (isColorImage)
        	else {
        		int lutBufferRemapped[] = LUTa.exportIndexedLUT();
            	int lutWrite[][] = new int[3][256];
            	for (i = 0; i < lutBufferRemapped.length; i++) {
                    int value = lutBufferRemapped[i];

                    lutWrite[2][i] = (value & 0x000000ff); // blue
                    lutWrite[1][i] =  ((value & 0x0000ff00) >> 8); // green
                    lutWrite[0][i] =  ((value & 0x00ff0000) >> 16); // red
                }
            	for (i = 0; i < length; i++) {
            		bufferData[i * 4 + 0] = lutWrite[0][imageData[zOffset][i]];
                    bufferData[i * 4 + 1] = lutWrite[1][imageData[zOffset][i]];
                    bufferData[i * 4 + 2] = lutWrite[2][imageData[zOffset][i]];
                    bufferData[i * 4 + 3] = 255;	
            	}
        	}
        	inputImage.getRaster().setPixels(0, 0, inputWidth, inputHeight, bufferData);
        	frame.repaint();
        } // if ((wheelRotation < 0) && (zOffset < zDim - 1))
        else if ((wheelRotation > 0) && (zOffset > 0)) {
        	// Decrement slice
        	zOffset--;
        	if (isColorImage) {
        		int RGBIndexBufferA[] = RGBa.exportIndexedRGB();
                int redMapped;
                int greenMapped;
                int blueMapped;
        	    for (i = 0; i < length; i++) {
        	        if (RGBa.getROn()) {
        	            redMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 1]] & 0x00ff0000) >> 16;
        	        } else {
        	            redMapped = 0;
        	        }
        	
        	        if (RGBa.getGOn()) {
        	            greenMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 2]] & 0x0000ff00) >> 8;
        	        } else {
        	            greenMapped = 0;
        	        }
        	
        	        if (RGBa.getBOn()) {
        	            blueMapped = (RGBIndexBufferA[imageData[zOffset][4 * i + 3]] & 0x000000ff);
        	        } else {
        	            blueMapped = 0;
        	        }
        	        bufferData[i * 4 + 0] = redMapped;
        	        bufferData[i * 4 + 1] = greenMapped;
        	        bufferData[i * 4 + 2] = blueMapped;
        	        bufferData[i * 4 + 3] = 255;
        	    } // for (i = 0; i < length; i++)	
        	} // if (isColorImage)
        	else {
        		int lutBufferRemapped[] = LUTa.exportIndexedLUT();
            	int lutWrite[][] = new int[3][256];
            	for (i = 0; i < lutBufferRemapped.length; i++) {
                    int value = lutBufferRemapped[i];

                    lutWrite[2][i] = (value & 0x000000ff); // blue
                    lutWrite[1][i] =  ((value & 0x0000ff00) >> 8); // green
                    lutWrite[0][i] =  ((value & 0x00ff0000) >> 16); // red
                }
            	for (i = 0; i < length; i++) {
            		bufferData[i * 4 + 0] = lutWrite[0][imageData[zOffset][i]];
                    bufferData[i * 4 + 1] = lutWrite[1][imageData[zOffset][i]];
                    bufferData[i * 4 + 2] = lutWrite[2][imageData[zOffset][i]];
                    bufferData[i * 4 + 3] = 255;	
            	}
        	}
        	inputImage.getRaster().setPixels(0, 0, inputWidth, inputHeight, bufferData);
        	frame.repaint();
        } // else if ((wheelRotation > 0) && (zOffset > 0))
}

public void mouseDragged(final MouseEvent mouseEvent) {
    if ((mouseEvent.getModifiers() & InputEvent.BUTTON3_MASK) == 0)  {
    	return;
    }

    int xS, yS;
    int i;
    final double widthRatio = (double) (frame.getWidth() - 160) / (double) inputWidth;
    final double heightRatio = (double) (frame.getHeight() - 158) / (double) inputHeight;
    if (widthRatio > heightRatio) {
        // Can only expand by the heightRatio
        final int expWidth = (int) Math.floor(inputWidth * heightRatio);
        final int leftPadding = (frame.getWidth() - 160 - expWidth) / 2;
        double zoomX = (double)expWidth/(double)inputWidth;
        double zoomY = (double)(frame.getHeight() - 158)/(double)inputHeight;
        xS = (int)((mouseEvent.getX() - (80 + leftPadding))/zoomX);
        yS = (int)((mouseEvent.getY() - 79)/zoomY);
        //g.drawImage(inputImage, 80 + leftPadding, 79, expWidth, getHeight() - 158, this);
    } else {
        // Can only expand by the widthRatio
        final int expHeight = (int) Math.floor(inputHeight * widthRatio);
        final int topPadding = (frame.getHeight() - 158 - expHeight) / 2;
        double zoomX = (double)(frame.getWidth() - 160)/(double)inputWidth;
        double zoomY = (double)expHeight/(double)inputHeight;
        xS = (int)((mouseEvent.getX() - 80)/zoomX);
        yS = (int)((mouseEvent.getY() - (79 + topPadding))/zoomY);
        //g.drawImage(inputImage, 80, 79 + topPadding, getWidth() - 160, expHeight, this);
    }
    if ( (xS < 0) || (xS >= inputWidth) || (yS < 0) || (yS >= inputHeight)) {
        return;
    }
    
    // Dragging the mouse with the right mouse button pressed
    // increases the window when going from left to right.
    // Dragging the mouse with the right mouse button pressed
    // increases the level when going from up to down.
    
    final float fX = xS / (float) inputWidth;
    final float fY = yS / (float) inputHeight;
    
    if (isColorImage) {
        updateWinLevel(fX, fY, !winLevelSet, RGBa, srcImage);
        int RGBIndexBufferA[] = RGBa.exportIndexedRGB();
        int redMapped;
        int greenMapped;
        int blueMapped;
	    for (i = 0; i < length; i++) {
	        if (RGBa.getROn()) {
	            redMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 1]] & 0x00ff0000) >> 16;
	        } else {
	            redMapped = 0;
	        }
	
	        if (RGBa.getGOn()) {
	            greenMapped = (RGBIndexBufferA[imageData[zOffset][i * 4 + 2]] & 0x0000ff00) >> 8;
	        } else {
	            greenMapped = 0;
	        }
	
	        if (RGBa.getBOn()) {
	            blueMapped = (RGBIndexBufferA[imageData[zOffset][4 * i + 3]] & 0x000000ff);
	        } else {
	            blueMapped = 0;
	        }
	        bufferData[i * 4 + 0] = redMapped;
	        bufferData[i * 4 + 1] = greenMapped;
	        bufferData[i * 4 + 2] = blueMapped;
	        bufferData[i * 4 + 3] = 255;
        } // for (i = 0; i < length; i++)
    }
    else {
    	updateWinLevel(fX, fY, !winLevelSet, LUTa, srcImage);
    	int lutBufferRemapped[] = LUTa.exportIndexedLUT();
    	int lutWrite[][] = new int[3][256];
    	for (i = 0; i < lutBufferRemapped.length; i++) {
            int value = lutBufferRemapped[i];

            lutWrite[2][i] = (value & 0x000000ff); // blue
            lutWrite[1][i] =  ((value & 0x0000ff00) >> 8); // green
            lutWrite[0][i] =  ((value & 0x00ff0000) >> 16); // red
        }
    	for (i = 0; i < length; i++) {
    		bufferData[i * 4 + 0] = lutWrite[0][imageData[zOffset][i]];
            bufferData[i * 4 + 1] = lutWrite[1][imageData[zOffset][i]];
            bufferData[i * 4 + 2] = lutWrite[2][imageData[zOffset][i]];
            bufferData[i * 4 + 3] = 255;	
    	}
    }
    inputImage.getRaster().setPixels(0, 0, inputWidth, inputHeight, bufferData);
	frame.repaint();
    frame.setCursor(MipavUtil.winLevelCursor);
    
    if ( !winLevelSet) {
        //setCursor(MipavUtil.winLevelCursor);
        winLevelSet = true;
    }
}

public void mouseMoved(final MouseEvent mouseEvent) {
	
}

/**
 * updateWinLevel updates the window-level for the input lookup table
 * based on two normalized parameters (fX, fY). These parameters may be
 * derived from a normalized x,y mouse position, from slider values, or
 * from any variable.
 *
 * In ViewJComponentEditImage and PlaneRender classesL If the right mouse
 * button is pressed and dragged. updateWinLevel updates the HistoLUT
 * window and level (contrast and brightness) for the
 * ViewJComponentEditImage and PlaneRender classes. The input parameters
 * fX and fY must be in normalized screen space (0-1).
 *
 *
 * @param fX the normalized window parameter (0-1)
 * @param fY the normalized level parameter (0-1)
 * @param bFirstUpdate when true initialize the WindowLevel function
 * @param kLookupTable either the ModelLUT or the ModelRGB being modified
 * @param kImage the ModelImage the lookup table describes. 
 * @return true when the lookup table changes, false when no change
 */
public boolean updateWinLevel( float fX, float fY, boolean bFirstUpdate,
                               ModelStorageBase kLookupTable,
                               ModelImage kImage )
{
    /* If this is the first time the kLookupTable is updated for
     * window-level control, setup the member variables to change the
     * HistoLUT. */
    if (bFirstUpdate)
    {
        if (Preferences.getProperty(Preferences.PREF_RELATIVE_WINDOW_LEVEL) != null) {
            doRelative = Preferences.is(Preferences.PREF_RELATIVE_WINDOW_LEVEL);
        }
        
        if (doRelative) {
            if ( kImage.isColorImage() )
            {
                winLevelRGB( (ModelRGB)kLookupTable, kImage );
            }
            else
            {
                winLevelGray( (ModelLUT)kLookupTable, kImage );
            }    
        } // if (doRelative)
        else { // absolute
            if ( kImage.isColorImage() )
            {
                initWinLevelRGB( (ModelRGB)kLookupTable, kImage );
            }
            else
            {
                initWinLevelGray( (ModelLUT)kLookupTable, kImage );
            }
        } // else absolute
        
        /* Keep track if the mouse position changed: */
        m_fOldX = fX;
        m_fOldY = fY;
    }
    /* Updating window-level has been initialized on the previous call,
     * this changes the HistoLUT: */
    else if ((kImage != null) && (kLookupTable != null) &&
             ((m_fOldX != fX) || (m_fOldY != fY)))
    {
        /* Determine the HistoLUT window image size based on the
         * ModelImage: */
        float fMinImageWin = m_fMin;
        float fMaxImageWin = m_fMax;
        float fWindow;
        float fLevel;

        if (doRelative) {
            fWindow = old_fWindow + 2.0f * (fX - m_fOldX) * (fMaxImageWin - fMinImageWin);    
        }
        else {
        /* The new window value is based on the fX parameter: */
            fWindow = 2.0f * fX * (fMaxImageWin - fMinImageWin);
        }

        if (fWindow > (2.0f * (fMaxImageWin - fMinImageWin))) {
            fWindow = 2.0f * (fMaxImageWin - fMinImageWin);
        } else if (fWindow < 0) {
            fWindow = 0;
        }
        
        if (doRelative) {
            fLevel = old_fLevel + (fY - m_fOldY) * (fMaxImageWin - fMinImageWin);
        }
        else {
            /* The new level value is based on the fY parameter: */
            fLevel = fY * (fMaxImageWin - fMinImageWin);
        }

        if ( fLevel > fMaxImageWin) {
            fLevel = fMaxImageWin;
        } else if ( fLevel < fMinImageWin) {
            fLevel = fMinImageWin;
        }
        
        
        //System.out.println("flevel is " + fLevel);
        //System.out.println("fwindow is " + fWindow); 

        /* The new x positions, and y positions of the middle points on
         * the transfer line: */
        m_afXWin[2] = fLevel + (fWindow / 2.0f);
        m_afXWin[1] = fLevel - (fWindow / 2.0f);
        m_afYWin[2] = m_afYWin[3];
        m_afYWin[1] = m_afYWin[0];

        if (m_afXWin[2] > fMaxImageWin) {
            m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

            if (m_afYWin[2] > 255.0f) {
                m_afYWin[2] = 255.0f;
            }
            m_afXWin[2] = fMaxImageWin;
        }

        if (m_afXWin[1] < fMinImageWin) {
            m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

            if (m_afYWin[1] < 0.0f) {
                m_afYWin[1] = 0.0f;
            }
            m_afXWin[1] = fMinImageWin;
        }
        
        if ( kImage.isColorImage() )
        {
            updateWinLevelRGB( (ModelRGB)kLookupTable, kImage, m_afXWin, m_afYWin );
        }
        else
        {
            updateWinLevelGray( (ModelLUT)kLookupTable, kImage, m_afXWin, m_afYWin );
        }

        /* Store old change in fX,fY positions: */
        m_fOldX = fX;
        m_fOldY = fY;
        old_fWindow = fWindow;
        old_fLevel = fLevel;
        
        return true;
    }
    return false;
}

/**
 * initWinLevelRGB, initializes the ModelRGB for window-level changes. The
 * transfer function is set to have four control points, and is reset to
 * the default linear.
 * @param kRGBT the ModelRGB to be initialized
 * @param kImage the ModelImage that the min/max values are derived from
 * for initializing the ModelRGB.
 */
private void initWinLevelRGB( ModelRGB kRGBT,
                              ModelImage kImage )
{
    m_fMin = (float)Math.min( kImage.getMinR(), kImage.getMinG() );
    m_fMin = (float)Math.min( m_fMin, kImage.getMinB() );
    m_fMax = (float)Math.max( kImage.getMaxR(), kImage.getMaxG() );
    m_fMax = (float)Math.max( m_fMax, kImage.getMaxB() );
    if ( kImage.getType() == ModelStorageBase.ARGB ) {
        m_afXWin[1] = m_fMin;
        m_afXWin[2] = m_fMax;
    }
    else {
        m_afXWin[1] = m_fMin * 255 / m_fMax;
        m_afXWin[2] = 255;
    }
    m_afXWin[0] = 0;
    m_afXWin[3] = 255;
    
    m_afYWin[0] = 255;
    m_afYWin[1] = 255;
    m_afYWin[2] = 0;
    m_afYWin[3] = 0;

    updateWinLevelRGB( kRGBT, kImage, m_afXWin, m_afYWin );
}

/**
 * winLevelRGB finds old_fWindow and old_fLevel.
 * @param kRGBT the ModelRGB 
 * @param kImage the ModelImage
 */
private void winLevelRGB( ModelRGB kRGBT,
        ModelImage kImage )
{
    m_fMin = (float)Math.min( kImage.getMinR(), kImage.getMinG() );
    m_fMin = (float)Math.min( m_fMin, kImage.getMinB() );
    m_fMax = (float)Math.max( kImage.getMaxR(), kImage.getMaxG() );
    m_fMax = (float)Math.max( m_fMax, kImage.getMaxB() );
    
    if (kRGBT != null) {
        if ( kRGBT.getROn() )
        {
            kRGBT.getRedFunction().exportArrays( m_afXWin, m_afYWin);
        }
        if ( kRGBT.getGOn() )
        {
            kRGBT.getGreenFunction().exportArrays(m_afXWin, m_afYWin);
        }
        if ( kRGBT.getBOn() )
        {
            kRGBT.getBlueFunction().exportArrays(m_afXWin, m_afYWin);
        }
        
        if (m_afYWin[2] != m_afYWin[3]) {
            m_afXWin[2] = m_afXWin[3] - 1;
            m_afYWin[2] = m_afYWin[3];
        }
        if (m_afYWin[1] != m_afYWin[0]) {
            m_afXWin[1] = m_afXWin[0] + 1;
            m_afYWin[1] = m_afYWin[0];
        }
        
        old_fWindow = m_afXWin[2] - m_afXWin[1];
        if (old_fWindow > (2.0f * (m_fMax - m_fMin))) {
            old_fWindow = 2.0f * (m_fMax - m_fMin);
        } else if (old_fWindow < 0) {
            old_fWindow = 0;
        }
        old_fLevel = (m_afXWin[1] + m_afXWin[2])/2.0f;
        if ( old_fLevel > m_fMax) {
            old_fLevel = m_fMax;
        } else if ( old_fLevel < m_fMin) {
            old_fLevel = m_fMin;
        }
    } // if (kRGBT != null)
}

/** 
 * initWinLevelGray, initializes the ModelLUT for gray-scale images before
 * window-level operations. The transfer function is set to have four
 * control points, and is reset to the default linear.
 * @param kLUT the ModelLUT to be initialized.
 * @param kImage the ModelImage attached to kLUT
 */
private void initWinLevelGray( ModelLUT kLUT,
                               ModelImage kImage )
{
    m_fMin = (float) kImage.getMin();
    m_fMax = (float) kImage.getMax();
    
    if ( kImage.getType() == ModelStorageBase.UBYTE ) {
        m_fMin = 0;
        m_fMax = 255;
    } else if ( kImage.getType() == ModelStorageBase.BYTE ) {
        m_fMin = -128;
        m_fMax = 127;
    }

    /* Reset the transferline: */
    if ((kImage != null) && (kLUT != null))
    {
        kLUT.resetTransferLine(m_fMin, m_fMax);
        kLUT.getTransferFunction().exportArrays(m_afXWin, m_afYWin);
        
        m_afXWin[1] = m_afXWin[0];
        m_afXWin[2] = m_afXWin[3];
        m_afYWin[1] = m_afYWin[0];
        m_afYWin[2] = m_afYWin[3];
        
        updateWinLevelGray( kLUT, kImage, m_afXWin, m_afYWin);
    }
}

/** 
 * winLevelGray finds old_fWindow and old_fLevel
 * @param kLUT the ModelLUT
 * @param kImage the ModelImage attached to kLUT
 */
private void winLevelGray( ModelLUT kLUT,
        ModelImage kImage )
{
    m_fMin = (float) kImage.getMin();
    m_fMax = (float) kImage.getMax();
    
    if ( kImage.getType() == ModelStorageBase.UBYTE ) {
        m_fMin = 0;
        m_fMax = 255;
    } else if ( kImage.getType() == ModelStorageBase.BYTE ) {
       m_fMin = -128;
       m_fMax = 127;
    }
    
    if (kLUT != null) {
       
        kLUT.getTransferFunction().exportArrays(m_afXWin, m_afYWin);
        
        if (m_afYWin[2] != m_afYWin[3]) {
            m_afXWin[2] = m_afXWin[3] - 1;
            m_afYWin[2] = m_afYWin[3];
        }
        if (m_afYWin[1] != m_afYWin[0]) {
            m_afXWin[1] = m_afXWin[0] + 1;
            m_afYWin[1] = m_afYWin[0];
        }
    
        old_fWindow = m_afXWin[2] - m_afXWin[1];
        if (old_fWindow > (2.0f * (m_fMax - m_fMin))) {
            old_fWindow = 2.0f * (m_fMax - m_fMin);
        } else if (old_fWindow < 0) {
            old_fWindow = 0;
        }
        old_fLevel = (m_afXWin[1] + m_afXWin[2])/2.0f;
        if ( old_fLevel > m_fMax) {
            old_fLevel = m_fMax;
        } else if ( old_fLevel < m_fMin) {
            old_fLevel = m_fMin;
        } 
     
    }
    
}

/**
 * updateWinLevelRGB, updates the ModelRGB with the new transfer
 * functions. Updates depend on the activation of the different rgb
 * functions, so if the getROn returns false the red function is not
 * updated.
 * @param kRGBT the ModelRGB being updated
 * @param kImage the ModelImage that the ModelRGB describes
 * @param afXWin the x-transfer function
 * @param afYWin the y-transfer function
 */
private void updateWinLevelRGB( ModelRGB kRGBT, ModelImage kImage,
                                float[] afXWin, float[] afYWin )
{
    if ( kRGBT.getROn() )
    {
        kRGBT.getRedFunction().importArrays( afXWin, afYWin, 4 );
    }
    if ( kRGBT.getGOn() )
    {
        kRGBT.getGreenFunction().importArrays( afXWin, afYWin, 4 );
    }
    if ( kRGBT.getBOn() )
    {
        kRGBT.getBlueFunction().importArrays( afXWin, afYWin, 4 );
    }
    kRGBT.makeRGB( -1 );
}

/**
 * updateWinLevelGray, updates the ModelLUT with the new transfer
 * function. 
 * @param kLUT the ModelLUT being updated
 * @param kImage the ModelImage that the ModelLUT describes
 * @param afXWin the x-transfer function
 * @param afYWin the y-transfer function
 */
private void updateWinLevelGray( ModelLUT kLUT, ModelImage kImage,
                                 float[] afXWin, float[] afYWin )
{
    /* Update the HistoLUT and the renderers: */
    kLUT.getTransferFunction().importArrays( afXWin, afYWin, 4 );
}

}
