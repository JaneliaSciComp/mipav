import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.*;



import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogGaussianBlur;
import gov.nih.mipav.view.dialogs.JDialogLivewire;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;


/**
 * Plugin example class for creating a simple, self-contained frame that extends ViewJFrame Image
 * Contains a subset of the VOI functions, as well as the message frame contained within the frame itself
 * @author linkb
 *
 */
public class PlugInDialogImageVOIDisplay extends ViewJFrameImage implements MouseListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------
   
    /** Reference to the toolbars located in the controls object. */
    protected ViewControlsImage controls;
        
    /**
     * The width resolution factor
     */
    float widthResFactor;
    
    /**
     * The height resolution factor
     */
    float heightResFactor;
    
    /**
     * The zoom level of the image
     */
    float zoom;
    
    /** Width of the display screen. */
    protected static final int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;

    /** Height of the display screen. */
    protected static final int yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;
    
    /**
     * The buffers that hold the current slice dat for imageA and imageB
     */
    float [] imageBufferA, imageBufferB;
    
    /**
     * The buffers that hold the draw-on-screen data for both imageA and imageB
     */
    int [] pixBuffer, pixBufferB;
    
    /** Number of slices in a 3D dataset. */
    protected int nImage;

    /** Number of time sequences in a 4D dataset. */
    protected int nTImage;
    
    /**
     * The current time and z-slice of the displayed image
     */
    protected int tSlice, zSlice;
    

    /** Reference to the two window and level dialogs where [0] is for imageA, [1] for imageB. */
    protected JDialogWinLevel[] windowLevel;
    
    /**
     * Flag indicating whether or not that the image should be displayed in Log scale. Used primarily for displaying the
     * FFT of an image.
     */
    protected boolean logMagDisplay;
    
    /** Reference to the frame's menu bar. */
    protected JMenuBar menuBar;

    /** Constructs the image menu bar. */
    protected ViewMenuBar menuBarMaker;

    /** This object contains a number of useful functions for building a menu and querying the state of menu items. */
    protected ViewMenuBuilder menuBuilder;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------
   
   /**
    * Default constructor
    */
    public PlugInDialogImageVOIDisplay(ModelImage image) {
    	super(image, null, null, false, false);
        init(LUTa);
    }

    
    /**
     * ViewOpenFrameInterface function for opening a model image (result) into a new frame
     */
    public PlugInDialogImageVOIDisplay openFrame(ModelImage image) {
    	return new PlugInDialogImageVOIDisplay(image);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************  
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        System.err.println("command: " + command);
        
        //run through toggle buttons to see if a menu selected one (updates the button status)
        getControls().getTools().setToggleButtonSelected(command);
       
        if (command.equals("Gaussian blur")) {
            new JDialogGaussianBlur(this, getActiveImage());
        } else if (command.equals("Open")) {
        	//ViewUserInterface.getReference().openImageFrame();
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER)) {
            componentImage.setCursorMode(ViewJComponentEditImage.DEFAULT);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_POINT.getActionCommand())) {

        	
            if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.POINT, getControls())) {
                componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            }

            componentImage.setCursorMode(ViewJComponentEditImage.POINT_VOI);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand())) {

            if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.LINE, getControls())) {
                componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            }

            componentImage.setCursorMode(ViewJComponentEditImage.LINE);
        } else if (command.equals("SplitVOI")) {
            componentImage.setCursorMode(ViewJComponentEditImage.SPLIT_VOI);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_POLY_SLICE.getActionCommand())) {

        	 if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.POLYLINE_SLICE, getControls())) {
                 componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
             }

            componentImage.setCursorMode(ViewJComponentEditImage.POLYLINE_SLICE_VOI);
        } else if (command.equals("protractor")) {

        	 if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.PROTRACTOR, getControls())) {
                 componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
             }

            componentImage.setCursorMode(ViewJComponentEditImage.PROTRACTOR);
        } else if (command.equals("Polyline")) {

        	if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.POLYLINE, getControls())) {
                componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            }

            componentImage.setCursorMode(ViewJComponentEditImage.POLYLINE);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_TEXT.getActionCommand())) {

        	componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
           

            componentImage.setCursorMode(ViewJComponentEditImage.ANNOTATION);
        } else if (command.equals("RectVOI")) {

        	if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.CONTOUR, getControls())) {
                componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            }

            componentImage.setCursorMode(ViewJComponentEditImage.RECTANGLE);
        } else if (command.equals("EllipseVOI")) {
        	if (!componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.CONTOUR, getControls())) {
                componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
            }

            componentImage.setCursorMode(ViewJComponentEditImage.ELLIPSE);
        } else if (command.equals("LevelSetVOI")) {
        	componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.CONTOUR, getControls());
            componentImage.setCursorMode(ViewJComponentEditImage.LEVELSET);
        } else if (command.equals("Rect3DVOI")) {
        	componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.CONTOUR, getControls());
            componentImage.setCursorMode(ViewJComponentEditImage.RECTANGLE3D);
        } else if (command.equals("LiveWireVOI")) {
        	componentImage.getVOIHandler().checkForVOICompatibility(getActiveImage().getVOIs(), VOI.CONTOUR, getControls());

            if (componentImage.getVOIHandler().isLivewireNull()) {
                JDialogLivewire dialog = new JDialogLivewire(this);

                if (!dialog.isCancelled()) {
                    componentImage.getVOIHandler().setModeLivewire(dialog.getSelection());
                    componentImage.setCursorMode(ViewJComponentEditImage.LIVEWIRE);
                }
            } else {
                componentImage.setCursorMode(ViewJComponentEditImage.LIVEWIRE);
            }
        } else if (command.equals("NewVOI")) {
            componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);

            int id = (getActiveImage().getVOIs().size() > 0)
                     ? (((VOI) (getActiveImage().getVOIs().lastElement())).getID() + 1) : -1;

            getControls().setVOIColor(id);
        } else if (command.equals("cutVOI")) {

            if (componentImage.getVOIHandler().copyVOItoClipBrd()) {
                componentImage.getVOIHandler().deleteSelectedVOI(true);
            }
        } else if (command.equals("copyVOI")) {
            componentImage.getVOIHandler().copyVOItoClipBrd();
        } else if (command.equals("pasteVOI")) {
            componentImage.getVOIHandler().pasteVOI();
        } else if (command.equals("selectAllVOIs")) {
            componentImage.getVOIHandler().selectAllVOIs(true);
        } else if (event.getActionCommand().equals("voiSelectNone")) {
            componentImage.getVOIHandler().selectAllVOIs(false);
        } else if (command.equals("deleteVOI")) {
            componentImage.getVOIHandler().deleteSelectedVOI(true);
        } else if (command.equals("BringToFront")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOIHandler.FRONT);
        } else if (command.equals("SendToBack")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOIHandler.BACK);
        } else if (command.equals("BringContourToFront")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOIHandler.FRONT);
        } else if (command.equals("SendContourToBack")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOIHandler.BACK);
        } else if (command.equals("PropVOIUp")) {

            // It appears JButtons don't pass key modifiers
            // if((event.getModifiers() & ActionEvent.SHIFT_MASK) != 0) {}
            if (componentImage.getVOIHandler().propVOI(1, false) == true) {
                incSlice();
            }
        } else if (command.equals("PropVOIDown")) {

            if (componentImage.getVOIHandler().propVOI(-1, false) == true) {
                decSlice();
            }
        } else if (command.equals("PropVOIActiveUp")) {

            // It appears JButtons don't pass key modifiers
            // if((event.getModifiers() & ActionEvent.SHIFT_MASK) != 0) {}
            if (componentImage.getVOIHandler().propVOI(1, true) == true) {
                incSlice();
            }
        } else if (command.equals("PropVOIActiveDown")) {

            if (componentImage.getVOIHandler().propVOI(-1, true) == true) {
                decSlice();
            }
        } else if (command.equals("PropVOIAll")) {
            componentImage.getVOIHandler().propVOIAll();
        } else if (command.equals("BringForward")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOIHandler.FORWARD);
        } else if (command.equals("SendBackward")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOIHandler.BACKWARD);
        } else if (command.equals("SendContourForward")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOIHandler.FORWARD);
        } else if (command.equals("SendContourBackward")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOIHandler.BACKWARD);
        } else if (command.equals("VOIProperties")) {

            componentImage.getVOIHandler().showVOIProperties(false);

        } else if (command.equals("VOIPropertiesColor")) {

            if (getActiveImage().getVOIs().size() > 0) {

                ViewVOIVector VOIs = getActiveImage().getVOIs();

                int i;
                int nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if ((VOIs.VOIAt(i).isActive() == true) &&
                            ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                 (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) ||
                                 (VOIs.VOIAt(i).getCurveType() == VOI.POINT) ||
                                 (VOIs.VOIAt(i).getCurveType() == VOI.LINE) ||
                                 (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR))) {
                        break;
                    } else if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION)) {
                        MipavUtil.displayInfo("Double-click annotation to change properties");
                        i = -1;

                        break;
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select VOI");
                } else if (i == -1) { // there was an annotation selected, do nothing
                } else {
                    componentImage.getVOIHandler().showVOIProperties(true);
                }
            } else {
                MipavUtil.displayWarning("Image has no VOIs!");
            }

        }
    }

    /**
     * Can handle actions for the resizing of the frame
     */
    public synchronized void componentResized(ComponentEvent event) {
    	
    }
    
    /**
     * Override MouseListener functions to prevent MouseEvent catching in ViewJFrameImage
     */
    public void mousePressed(MouseEvent e) {}
     public void mouseReleased(MouseEvent e) {}
     public void mouseEntered(MouseEvent e) {}
     public void mouseExited(MouseEvent e) {}
     public void mouseClicked(MouseEvent e) {}

     
     /**
      * Initialize the frame using a lut (can be null)
      * @param LUTa the ModelLUT
      * @throws OutOfMemoryError
      */
     private void init(ModelLUT LUTa) throws OutOfMemoryError {

         try {
             setIconImage(MipavUtil.getIconImage("davinci_32x32.gif"));
         } catch (Exception error) {
             Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
         }

         setResizable(true);

         // initialize logMagDisplay
         this.LUTa = initLUT(imageA);

         initResolutions();
         initZoom();
         System.err.println("zoom: " + zoom);
         
         int[] extents = createBuffers();

         initComponentImage(extents);
         initExtentsVariables(imageA);

         // create and build the menus and controls
         controls = new ViewControlsImage(this); // Build controls used in this frame
         menuBuilder = new ViewMenuBuilder(this);

         // build the menuBar based on the number of dimensions for imageA
         menuBarMaker = new ViewMenuBar(menuBuilder);
         
         //create a custom menu bar using Vectors of UIParams
         JMenuBar menuBar = new JMenuBar();
         
         //add pre-defined UIParams to the vector (will be added to both menu and toolbar)
         Vector<CustomUIBuilder.UIParams> voiParams = new Vector<CustomUIBuilder.UIParams>();
         voiParams.addElement(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER);
         voiParams.addElement(CustomUIBuilder.PARAM_VOI_POINT);
         voiParams.addElement(CustomUIBuilder.PARAM_VOI_ELLIPSE);
         voiParams.addElement(CustomUIBuilder.PARAM_VOI_RECTANGLE);
         
         
         Vector<CustomUIBuilder.UIParams> algoParams = new Vector<CustomUIBuilder.UIParams>();
         algoParams.add(new CustomUIBuilder.UIParams("Gaussian blur", null, null));
         algoParams.add(new CustomUIBuilder.UIParams("Gradient magnitude", null, null));
         
         menuBar.add(menuBarMaker.makeCustomMenu("VOI example", voiParams));
         menuBar.add(menuBarMaker.makeCustomMenu("Simple algorithm menu", algoParams));
                 
         //create a simple toolbar (rather than the default ViewJFrameImage specific toolbar)
         //buttons will be added to the toolbar with the function call .addCustomToolBar()
         controls.buildSimpleToolBar();
         
         controls.addCustomToolBar(voiParams);
         
         setTitle();

         JPanel centerPanel = new JPanel();
         centerPanel.add(componentImage, BorderLayout.CENTER);
         
         // The component image will be displayed in a scrollpane.
       scrollPane = new JScrollPane(centerPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
         
       JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scrollPane, 
    		   ViewUserInterface.getReference().getMessageFrame().getTabbedPane());
       splitPane.setDividerLocation(350);
       
       getContentPane().add(splitPane);
       scrollPane.setBackground(Color.black);

       setBackground(Color.black);



         // MUST register frame to image models
         imageA.addImageDisplayListener(this);

         if (imageB != null) {
             imageB.addImageDisplayListener(this);
         }

         windowLevel = new JDialogWinLevel[2];

         this.setLocation(100, 50);

         setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

         pack();
        
         scrollPane.setPreferredSize(new Dimension(800,800));
         
         setSize(1000,750);
         // User interface will have list of frames
         userInterface.registerFrame(this);
        
        
         this.updateImages(true);
         addComponentListener(this);
         
         this.setJMenuBar(menuBar);
         getContentPane().add(controls, BorderLayout.NORTH);
         
         this.addWindowListener(new WindowAdapter() {
             public void windowClosing(WindowEvent we) {
                 System.exit(0);
             }
         });
         
         setVisible(true);
     } // end init()
     
     /**
      * Creates and initializes the component image for the given image.
      *
      * @param   extents  the image dimensionality.
      *
      * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
      */
     private void initComponentImage(int[] extents) throws OutOfMemoryError {

         componentImage = new ViewJComponentEditImage(this, imageA, LUTa, imageBufferA, null, null, imageBufferB,
                                                      pixBuffer, zoom, extents, logMagDisplay,
                                                      FileInfoBase.UNKNOWN_ORIENT);

         componentImage.setBuffers(imageBufferA, imageBufferB, pixBuffer, pixBufferB);

         if (resols[1] >= resols[0]) {
             componentImage.setResolutions(1, heightResFactor);
         } else {
             componentImage.setResolutions(widthResFactor, 1);
         }

         // if this is a color image, then update the RGB info in the component
         if (imageA.isColorImage()) {

             if (getRGBTA() == null) {
                 setRGBTA(initRGB(imageA));
             }
         } // end if image is an RGB type

     } // end initComponentImage()
     
     /**
      * Initializes the variables based on the image extents. (i.e. number of slices, number of time slices, the initial
      * z-slice, etc.
      *
      * @param  img  the image to set the extent variables for
      */
     public void initExtentsVariables(ModelImage img) {
         int[] slices = null;
         int[] numImages = null;

         slices = initSlicePositions(img);
         numImages = initNumSlices(img);

         zSlice = slices[0];
         tSlice = slices[1];

         nImage = numImages[0];
         nTImage = numImages[1];
     }

     
     /**
      * Create the buffers for imageA and imageB.
      *
      * @return  the extents of the buffers
      *
      * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
      */
     protected int[] createBuffers() throws OutOfMemoryError {
         int[] extents = initExtents(imageA);

         imageBufferA = initImageBuffer(extents, imageA.isColorImage());
         pixBuffer = initPixelBuffer(extents);

         if (imageB != null) {
             imageBufferB = initImageBuffer(imageB.getExtents(), imageB.isColorImage());
             pixBufferB = initPixelBuffer(imageB.getExtents());
         }

         return extents;

     } // end createBuffers()

     /**
      * Initializes the resolutions and units from the image.
      */
     protected void initResolutions() {
         resols = initResolutions(imageA);
         units = initUnits(imageA);

         float[] factor = initResFactor(resols, units);

         widthResFactor = factor[0];
         heightResFactor = factor[1];
     } // end initResolutions()

     /**
      * Initializes the zoom variables for the first image (imageA).
      */
     protected void initZoom() {
         zoom = initZoom(imageA, widthResFactor, heightResFactor, xScreen, yScreen);
     } // end initZoom()
     
     public void incSlice() {
    	 setSlice(zSlice+1);
     }
     
     public void decSlice() {
    	 setSlice(zSlice-1);
     }
     
     public void setSlice(int slice) {

         if (imageA.getNDims() <= 2) {
             return;
         }

         if (zSlice < imageA.getExtents()[2]) {
             zSlice = slice;
             controls.setZSlider(zSlice);
             updateImages(true);            

             // livewire grad mag. should be recalculated for the new slice
             // componentImage.deactivateAllVOI();
             componentImage.getVOIHandler().resetLivewire();
             setTitle();
         }
     }
     
    /**
	 * get controls
	 */
	public ViewControlsImage getControls() {
		return controls;
	}

	
	
	/**
	 * get image a
	 */
	public ModelImage getImageA() {
		return componentImage.getImageA();
	}

	
	
	/**
	 * get image b
	 */
	public ModelImage getImageB() {
		// TODO Auto-generated method stub
		return null;
	}

	
	
	/**
	 * remove controls
	 */
	public void removeControls() {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set active image
	 */
	public void setActiveImage(int active) {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set alpha blend
	 */
	public void setAlphaBlend(int value) {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set controls
	 */
	public void setControls() {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set enabled
	 */
	public void setEnabled(boolean flag) {
		// TODO Auto-generated method stub
		
	}

	
	/**
	 * set image b
	 */
	public void setImageB(ModelImage imageB) {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set paint bitmap switch
	 */
	public void setPaintBitmapSwitch(boolean flag) {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
	 * set rgbtb
	 */
	public void setRGBTB(ModelRGB RGBT) {
		// TODO Auto-generated method stub
		
	}

	/**
	 * Sets the title of the frame
	 */
	public void setTitle() {
		this.setTitle("Simple Image Frame: " + imageA.getImageName());
	}

	
	
	/**
	 * update image extents
	 */
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}
	
	/**
	 * set time slice
	 */
	public void setTimeSlice(int tSlice) {
		// TODO Auto-generated method stub
		
	}

	
	
	/**
     * This methods calls the componentImage's update method to redraw the screen - fastest of the three update methods.
     *
     * @return  boolean confirming successful update
     */
    public synchronized boolean updateImages() {

        if (componentImage == null) {
            return false;
        }

        try {
            componentImage.paintComponent(componentImage.getGraphics());
            // componentImage.repaint(); // problems with this method on some machines seems to eat lots of  memory on
            // JVM 1.3
        } catch (OutOfMemoryError error) {
            System.gc();
        }

        ViewControlsImage myControls = getControls();

        if (myControls != null) {
            myControls.repaint();
        }

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public synchronized boolean updateImages(boolean forceShow) {
        if (componentImage == null) {
            return false;
        }

        if (componentImage.show(tSlice, zSlice, null, null, forceShow, -1) == false) {
            return false;
        }

        ViewControlsImage myControls = getControls();

        if (myControls != null) {
            myControls.repaint();
        }

        return true;
    }

	
	
	/**
	 * update images
	 */
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
		updateImages(true);
		return true;
	}
   
	/**
	    * Gets the RGB LUT table for ARGB image A.
	    * @return  RGBT the new RGB LUT to be applied to the image
	    */
	   public ModelRGB getRGBTA() {
	       return null;
	   }
	   
	   
	   
	   /**
	    * Sets the RGB LUT table for ARGB image A.
	    * @param  RGBT  the new RGB LUT to be applied to the image
	    */
	   public void setRGBTA(ModelRGB RGBT) {

	   }
	   
	    /**
	     * Returns the reference to the currently active image.
	     *
	     * @return  the active image
	     */
	    public ModelImage getActiveImage() {

	        if (componentImage != null) {
	            return componentImage.getActiveImage();
	        } else {
	            return null;
	        }
	    }
	    /**
	     * Returns the frame's component image
	     */
	    public ViewJComponentEditImage getComponentImage() {
	    	return this.componentImage;
	    }
	
}
