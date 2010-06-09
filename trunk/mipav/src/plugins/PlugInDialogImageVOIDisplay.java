import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import javax.swing.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * Plugin example class for creating a simple, self-contained frame that extends ViewJFrame Image
 * Contains a subset of the VOI functions, as well as the message frame contained within the frame itself
 * @author linkb
 *
 */
public class PlugInDialogImageVOIDisplay extends ViewJFrameImage implements MouseListener, AdjustmentListener {

    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------
   
   /**
    * Default constructor
    */
    public PlugInDialogImageVOIDisplay(ModelImage image) {
    	// calls the ViewJFrameBase constructor that will not call ViewJFrameImage's init() function
    	super(image, (ModelImage)null);
        init();
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
    
    public void adjustmentValueChanged(AdjustmentEvent e) {
    	updateImages(true);
    }
    
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
        } else if (command.equals("Gradient magnitude")) {
        	JDialogGradientMagnitude gm = new JDialogGradientMagnitude(this, getActiveImage());
        	gm.setVisible(true);
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
            componentImage.getVOIHandler().changeVOIOrder(false, VOI.FRONT);
        } else if (command.equals("SendToBack")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOI.BACK);
        } else if (command.equals("BringContourToFront")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOI.FRONT);
        } else if (command.equals("SendContourToBack")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOI.BACK);
        } else if (command.equals("PropVOIUp")) {

            // It appears JButtons don't pass key modifiers
            // if((event.getModifiers() & ActionEvent.SHIFT_MASK) != 0) {}
            if (componentImage.getVOIHandler().propVOI(1, false) == true) {
                //incSlice();
            }
        } else if (command.equals("PropVOIDown")) {

            if (componentImage.getVOIHandler().propVOI(-1, false) == true) {
                //decSlice();
            }
        } else if (command.equals("PropVOIActiveUp")) {

            // It appears JButtons don't pass key modifiers
            // if((event.getModifiers() & ActionEvent.SHIFT_MASK) != 0) {}
            if (componentImage.getVOIHandler().propVOI(1, true) == true) {
                //incSlice();
            }
        } else if (command.equals("PropVOIActiveDown")) {

            if (componentImage.getVOIHandler().propVOI(-1, true) == true) {
                //decSlice();
            }
        } else if (command.equals("PropVOIAll")) {
            componentImage.getVOIHandler().propVOIAll();
        } else if (command.equals("BringForward")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOI.FORWARD);
        } else if (command.equals("SendBackward")) {
            componentImage.getVOIHandler().changeVOIOrder(false, VOI.BACKWARD);
        } else if (command.equals("SendContourForward")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOI.FORWARD);
        } else if (command.equals("SendContourBackward")) {
            componentImage.getVOIHandler().changeVOIOrder(true, VOI.BACKWARD);
        } else if (command.equals("VOIProperties")) {

            componentImage.getVOIHandler().showVOIProperties();

        } else if (command.equals("VOIColor")) {

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
                    componentImage.getVOIHandler().showVOIProperties();
                }
            } else {
                MipavUtil.displayWarning("Image has no VOIs!");
            }

        } else if (command.equals("About")) {
            about();
        } else if (command.equals("License")) {
            ViewUserInterface.getReference().showLicense();
        } else if (command.equals("AboutJava")) {
        	ViewUserInterface.getReference().aboutJava();
        } else if (command.equals("DataProvenance")) {
        	ViewUserInterface.getReference().aboutDataProvenance();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp(null);
        } else if (command.equals("MemoryUsage")) {
        	ViewUserInterface.getReference().memoryFrame();
        } else if (command.equals("MemoryAdjust")) {
        	ViewUserInterface.getReference().memoryAllocation();
        } else if (command.equals("ImageRegistryMonitor")) {
        	ViewUserInterface.getReference().imageRegistryMonitoring();
        } else if (command.equals("Options")) {
        	ViewUserInterface.getReference().options();
        } else if (command.equals("Shortcuts")) {
        	ViewUserInterface.getReference().showShortcutEditor(false);
        }
    }

    /**
     * Can handle actions for the resizing of the frame
     */
    public synchronized void componentResized(ComponentEvent event) {
    	// do nothing, this simply catches the ComponentEvent from being handled by ViewJFrameImage
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
     private void init() throws OutOfMemoryError {

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
         
         int[] extents = createBuffers();

         initComponentImage(extents);
         initExtentsVariables(imageA);

         // create and build the menus and controls
         controls = new ViewControlsImage(this); // Build controls used in this frame
         menuBuilder = new ViewMenuBuilder(this);

         // build the menuBar based on the number of dimensions for imageA
         menuBarMaker = new ViewMenuBar(menuBuilder);
         
         JMenuBar mainMenu = new JMenuBar();
         mainMenu = menuBarMaker.getMenuBar(this, imageA.getNDims(), imageA.getType(), imageA.isDicomImage());
         
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
         menuBar.add(menuBarMaker.makeHelpMenu());  
         
         
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
         scrollPane.getVerticalScrollBar().addAdjustmentListener(this);
         scrollPane.getHorizontalScrollBar().addAdjustmentListener(this);
         scrollPane.addComponentListener(this);
         
         setSize(1000,750);
         // User interface will have list of frames
         userInterface.registerFrame(this);
        
        
         this.updateImages(true);
         addComponentListener(this);
         
         this.setJMenuBar(menuBar);
         if(mainMenu != null) {
        	 System.out.println("Menu created");
         }
         //userInterface.getMainFrame().setJMenuBar(mainMenu);
         
         getContentPane().add(controls, BorderLayout.NORTH);
         
         this.addWindowListener(new WindowAdapter() {
             public void windowClosing(WindowEvent we) {
                 System.exit(0);
             }
         });
         
         setVisible(true);
     } // end init()
     

	/**
	 * Sets the title of the frame
	 */
	public void setTitle() {
		this.setTitle("Simple Image Frame: " + imageA.getImageName());
	}


	
}
