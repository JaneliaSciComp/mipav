
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;


/**
 * Creates an interface for working with Iceland CT images.
 * 
 * @author senseneyj
 *
 */
public class PlugInAlgorithmMuscleSegmentation extends AlgorithmBase implements ActionListener, ComponentListener {
    
    //~ Static fields --------------------------------------------------------------------------------------------------

	public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    
    public static final String LUT_IMAGE = "lutImage.tif";
    public static final String VOI_IMAGE = "voiImage.tif";
    
    public static final String NEW_TAB = "New Tab";
    public static final String ADD_VOI = "Add another VOI";
    public static final String LAUNCH = "Launch plugin";
    public static final String SAVE = "Save";
    public static final String DEFAULT_VOI = "Enter VOI name";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /**The current tab.*/
    private int activeTab = 0;
    
    /** denotes the type of srcImg (see enum ImageType) */
    private PlugInMuscleImageDisplay.ImageType imageType; 
    
    /** denotes the symmetry of srcImage */
    private PlugInMuscleImageDisplay.Symmetry symmetry;
    
    /** the parent frame. */
    private Frame parentFrame;
    
    /**Whether multiple slices are contained in srcImg */
    private boolean multipleSlices;
       
    /**voiList created by various set up methods*/
    private PlugInSelectableVOI[][] voiList;
    
    /**Each muscle pane is one VOI in custom mode.*/
    private ArrayList<ArrayList<MusclePane>> customVOI;
    
    /**All of the requested titles*/
    private ArrayList<JTextField> titlesArr;
    
    /**Each particular tab is represented here.*/
    private ArrayList<JPanel> tabs;
    
    /**list of titles used for each pane*/
    private String[] titles;
    
    /**Dialog box used for creating custom types*/
    private JPanel customDialog;
    
    /**Tabbed panes*/
    private JTabbedPane dialogTabs;
    
    /**Initial pane when running in custom mode*/
    private ViewJFrameImage customPane;
	
    /** Number of currently used tabs*/
    private int tabCount = 0;
    
    /**Whether new tabs are in a position to be created. */
    private boolean doEval = false;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMuscleSegmentation(ModelImage srcImg, PlugInMuscleImageDisplay.ImageType imageType, 
    											Frame parentFrame, boolean multipleSlices) {
        super(null, srcImg);
        this.imageType = imageType;
        this.parentFrame = parentFrame;
        this.multipleSlices = multipleSlices;
        
        tabs = new ArrayList();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        
        if (srcImage == null) {
            displayError("Source Image is null");
            return;
        }

        switch (imageType) {
            
            case Abdomen:
                buildAbdomenDialog();
                performDialog();
                break;
                
            case Thigh:
                buildThighDialog();
                performDialog();
                break;
                
            case Custom:
            	customDialog = initDialogBox();
            	customDialog.setVisible(true);
            	doEval = true;
            	break;
                
            default:
                displayError("Image type not supported");
                return;         
        }
    } // end runAlgorithm()
    
    /**
     * For custom muscle types, calls buildCustomDialog
     */
	public void actionPerformed(ActionEvent e) {
		System.out.println("Found a new action: "+e.getSource()+"\t"+e.getActionCommand());
		if(e.getSource() instanceof PlugInMuscleColorButton) {
        	PlugInMuscleColorButton obj = ((PlugInMuscleColorButton)e.getSource());
        	customPane.getComponentImage().getVOIHandler().showColorDialog();
        	System.out.println("Should have showed");
		} else if(e.getSource() instanceof JButton && ((JButton)e.getSource()).getText().equals("OK")) {
			if(checkPanel()) {
				buildCustomDialog();
				performDialog();
			}		
		} else if(e.getActionCommand().equals(ADD_VOI)) {
			int length = 1;
			Component[] comps = tabs.get(activeTab).getComponents();
			for(int i=0; i<comps.length; i++) {
				if(comps[i] instanceof MusclePane)
					length++;
			}
			MusclePane pane = new MusclePane(this);
			pane.setBorder(MipavUtil.buildTitledBorder("VOI #"+(length)));
			customVOI.get(activeTab).add(pane);
			tabs.get(activeTab).add(pane, comps.length-2);
			tabs.get(activeTab).validate();
		} else if(e.getActionCommand().equals(LAUNCH)) {
			if(checkPanel()) {
				buildCustomDialog();
				srcImage = customPane.getImageA();
				performDialog();
			}
		} else if(e.getActionCommand().equals(SAVE)) {
			//Do nothing for now
		} else {
			super.actionPerformed(e);
		}
	}
    
    public void componentHidden(ComponentEvent e) {
    	//Do nothing on hide
	}

	public void componentMoved(ComponentEvent e) {
		//Do nothing on move
	}

	public void componentResized(ComponentEvent e) {
		//Do nothing on resize
	}

	/**
	 * Creates new tab when panel is shown
	 */
	public void componentShown(ComponentEvent e) {
		if(e.getSource() instanceof JPanel && doEval) {
			if(((JPanel)e.getSource()).getName().equals(NEW_TAB)) {
				dialogTabs.removeTabAt(tabCount);
				
				JPanel newPanel = initMuscleTab(tabCount);
		    	
				newPanel.addComponentListener(customPane);
				newPanel.addComponentListener(this);
				newPanel.setName("Tab "+(tabCount+1));
				
				dialogTabs.addTab("Tab "+(tabCount+1), newPanel);
				
				JPanel tempPanel = new JPanel();
				tempPanel.setName(NEW_TAB);
				tempPanel.addComponentListener(customPane);
				tempPanel.addComponentListener(this);
				dialogTabs.addTab("Click to create a new tab...", tempPanel);
				
				dialogTabs.setSelectedIndex(tabCount);
				activeTab = tabCount;
				tabCount++;
			} else if(((JPanel)e.getSource()).getName().indexOf("Tab ") != -1) {
				activeTab = Integer.valueOf(((JPanel)e.getSource()).getName().substring(4)).intValue()-1;
			}
		}
	}

	private void buildAbdomenDialog() {
        
    	voiList = new PlugInSelectableVOI[3][];
    	int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
    	//String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
    	voiList[0] = new PlugInSelectableVOI[3];
    	voiList[0][0] = new PlugInSelectableVOI("Abdomen", true, 1, 0, false, true, imageSize, 0);
    	voiList[0][1] = new PlugInSelectableVOI("Subcutaneous area", true, 1, 0, false, true, imageSize, 1);
    	voiList[0][2] = new PlugInSelectableVOI("Phantom", true, 1, 0, false, false, imageSize);
    	
    	voiList[1] = new PlugInSelectableVOI[5];
    	voiList[1][0] = new PlugInSelectableVOI("Visceral cavity", true, 1, 1, false, true, imageSize, 2);
    	voiList[1][1] = new PlugInSelectableVOI("Liver", true, 1, 1, false, true, imageSize, 3);
    	voiList[1][2] = new PlugInSelectableVOI("Liver cysts", true, 10, 1, true, true, imageSize, 4);
    	voiList[1][3] = new PlugInSelectableVOI("Bone sample", true, 1, 1, false, false, imageSize);
    	voiList[1][4] = new PlugInSelectableVOI("Water sample", true, 1, 1, false, false, imageSize);
    	
    	voiList[2] = new PlugInSelectableVOI[9];
    	voiList[2][0] = new PlugInSelectableVOI("Left Psoas", true, 1, 2, true, true, imageSize, 5);
    	voiList[2][1] = new PlugInSelectableVOI("Right Psoas", true, 1, 2, true, true, imageSize, 6);
    	voiList[2][2] = new PlugInSelectableVOI("Left Lat. obliques", true, 1, 2, true, true, imageSize, 7);
    	voiList[2][3] = new PlugInSelectableVOI("Right Lat. obliques", true, 1, 2, true, true, imageSize, 8);
    	voiList[2][4] = new PlugInSelectableVOI("Left Paraspinous", true, 1, 2, true, true, imageSize, 9);
    	voiList[2][5] = new PlugInSelectableVOI("Right Paraspinous", true, 1, 2, true, true, imageSize, 10);
    	voiList[2][6] = new PlugInSelectableVOI("Left Rectus", true, 1, 2, true, true, imageSize, 11);
    	voiList[2][7] = new PlugInSelectableVOI("Right Rectus", true, 1, 2, true, true, imageSize, 12);
    	voiList[2][8] = new PlugInSelectableVOI("Aortic Calcium", true, 5, 2, true, true, imageSize, 13);
        
        titles = new String[3];
        titles[0] = "Abdomen";
        titles[1] = "Tissue";
        titles[2] = "Muscles"; 
        
        symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay.ImageType.Abdomen;
    }
    
    /**
	 *   Builds thigh dialogue.
	 */
	private void buildThighDialog() {
	    
	    //String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
	    int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
	    
	    voiList = new PlugInSelectableVOI[3][];
	    
	    voiList[0] = new PlugInSelectableVOI[3];
	    voiList[0][0] = new PlugInSelectableVOI("Left Thigh", true, 1, 0, false, true, imageSize, 0);
    	voiList[0][1] = new PlugInSelectableVOI("Right Thigh", true, 1, 0, false, true, imageSize, 1);
    	
    	voiList[0][2] = new PlugInSelectableVOI("Phantom", true, 1, 0, false, false, imageSize);
	    
    	voiList[1] = new PlugInSelectableVOI[5];
	    voiList[1][0] = new PlugInSelectableVOI("Left Bone", true, 1, 1, false, true, imageSize, 2);
    	voiList[1][1] = new PlugInSelectableVOI("Right Bone", true, 1, 1, false, true, imageSize, 3);
	    voiList[1][2] = new PlugInSelectableVOI("Left Marrow", true, 1, 1, true, true, imageSize, 4);
    	voiList[1][3] = new PlugInSelectableVOI("Right Marrow", true, 1, 1, true, true, imageSize, 5);
    	
    	voiList[1][4] = new PlugInSelectableVOI("Bone sample", true, 1, 1, false, false, imageSize);
	    
    	voiList[2] = new PlugInSelectableVOI[11];
	    voiList[2][0] = new PlugInSelectableVOI("Left Fascia", true, 1, 2, false, true, imageSize, 6);
    	voiList[2][1] = new PlugInSelectableVOI("Right Fascia", true, 1, 2, false, true, imageSize, 7);
	    voiList[2][2] = new PlugInSelectableVOI("Left Quads", true, 1, 2, true, true, imageSize, 8);
    	voiList[2][3] = new PlugInSelectableVOI("Right Quads", true, 1, 2, true, true, imageSize, 9);
	    voiList[2][4] = new PlugInSelectableVOI("Left Hamstrings", true, 1, 2, true, true, imageSize, 10);
    	voiList[2][5] = new PlugInSelectableVOI("Right Hamstrings", true, 1, 2, true, true, imageSize, 11);
	    voiList[2][6] = new PlugInSelectableVOI("Left Sartorius", true, 1, 2, true, true, imageSize, 12);
    	voiList[2][7] = new PlugInSelectableVOI("Right Sartorius", true, 1, 2, true, true, imageSize, 13);
	    voiList[2][8] = new PlugInSelectableVOI("Left Adductors", true, 1, 2, true, true, imageSize, 14);
    	voiList[2][9] = new PlugInSelectableVOI("Right Adductors", true, 1, 2, true, true, imageSize, 15);

    	voiList[2][10] = new PlugInSelectableVOI("Water sample", true, 1, 1, false, false, imageSize);
	    
	    titles = new String[3];
	    titles[0] = "Thigh";
	    titles[1] = "Bone";
	    titles[2] = "Muscles";
	     
	    symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay.ImageType.Thigh;
	}
	
	private void buildCustomDialog() {
		//String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
	    int imageSize = 1;
    	if(customPane.getImageA().getNDims() > 2)
    		imageSize = customPane.getImageA().getExtents()[2];
		
		int validPanes = 0;
		for(int i=0; i<customVOI.size(); i++) {
			boolean validEntryFound = false;
			for(int j=0; j<customVOI.get(i).size(); j++) {
				if((!customVOI.get(i).get(j).getName().equals(DEFAULT_VOI)) && customVOI.get(i).get(j).getName().trim().length() > 0) {
					if(!validEntryFound) {
						validEntryFound = true;
						validPanes++;
					}
				}
			}
		}
		int[] validVOI = new int[validPanes];
		
		for(int i=0; i<validPanes; i++) {
			for(int j=0; j<customVOI.get(i).size(); j++) {
				MusclePane temp = customVOI.get(i).get(j);
				if((!temp.getName().equals(DEFAULT_VOI)) && temp.getName().trim().length() > 0) {
					if(temp.getSymmetry().equals(PlugInMuscleImageDisplay.Symmetry.NO_SYMMETRY))
						validVOI[i]++;
					else {
						validVOI[i] = validVOI[i]+2;
						String name = temp.getName();
						customVOI.get(i).add(j+1, customVOI.get(i).get(j).createNew(this));
						customVOI.get(i).get(j).setName(temp.getSymmetry().side1 + " " + name);
						customVOI.get(i).get(j+1).setName(temp.getSymmetry().side2 + " " + name);
						j++;
					}
				}
			}
		}
		
		voiList = new PlugInSelectableVOI[validPanes][];
		int outputLoc = 0;
		
		for(int i=0; i<validPanes; i++) {
			voiList[i] = new PlugInSelectableVOI[validVOI[i]];
			for(int j=0; j<validVOI[i]; j++) {
				MusclePane temp = customVOI.get(i).get(j);
				if(temp.getDoCalc()) {
					voiList[i][j] = new PlugInSelectableVOI(temp.getName(), temp.getIsClosed(), 
							temp.getNumCurves(), i, temp.getDoFill(), temp.getDoCalc(), imageSize, outputLoc++);
				} else {
					voiList[i][j] = new PlugInSelectableVOI(temp.getName(), temp.getIsClosed(), 
							temp.getNumCurves(), i, temp.getDoFill(), temp.getDoCalc(), imageSize);
				}
			}
		}
		
		titles = new String[validPanes];
		for(int i=0; i<validPanes; i++) {
			titles[i] = titlesArr.get(i).getText();
		}
		
		symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay.ImageType.Custom;
	}
	
	/**
	 * Checks that information has been entered correctly.
	 */
	private boolean checkPanel() {
		//always return true until number of curves is replaced with text area
		return true;
	}
	
	/**
	 * Sets mode to CT and sets range to CT presets. Used for any new image.
	 *
	 * @param  preset1  first CT preset
	 * @param  preset2  second CT preset
	 */
	private void ctMode(ModelImage image, int preset1, int preset2) {
		Dimension dim = new Dimension(256, 256);
		
		//stores LUT min max values
		float[] x = new float[4], y = new float[4], z = new float[4];
		
		//reference to the image data currently displayed, used to adjust transfer func
		float[] dataSlice;
		
		float min = Float.MAX_VALUE;
	    float max = -Float.MIN_VALUE;
	    //image's max and min intensities
	    float minImage, maxImage;
	    int i;
	    
	    ModelLUT LUT = customPane.getComponentImage().getLUTa();
	
	    //Stores the maximum and minimum intensity values applicable to this image
	    minImage = (float)image.getMin();
	    maxImage = (float)image.getMax();
	    
	    dataSlice = customPane.getComponentImage().getActiveImageBuffer();
	    min = Float.MAX_VALUE;
	    max = -Float.MAX_VALUE;
	
	    for (i = 0; i < dataSlice.length; i++) {
	
	        if (dataSlice[i] > max) {
	            max = dataSlice[i];
	        }
	
	        if (dataSlice[i] < min) {
	            min = dataSlice[i];
	        }
	    }
	    
	    //Set LUT min max values of the image slice !!
	    x[0] = minImage;
	    y[0] = 255;
	    z[0] = 0;
	    x[1] = min;
	    y[1] = 255;
	    z[1] = 0;
	    x[2] = max;
	    y[2] = 0;
	    z[2] = 0;
	    x[3] = maxImage;
	    y[3] = 0;
	    z[3] = 0;
	    LUT.getTransferFunction().importArrays(x, y, 4);
	    
	    float yVal, m, b;
	    min = (float) image.getMin();
	    max = (float) image.getMax();
	
	    x[0] = min; // -1024;
	    y[0] = dim.height - 1;
	    z[0] = 0;
	
	    if (preset2 < max) {
	        x[2] = preset2;
	    } else {
	        x[2] = max;
	    }
	
	    y[2] = 0;
	    z[2] = 0;
	
	    if (preset1 < min) {
	
	        // y = m * x + b, line equation
	        // Assume given points: pt1 ( preset1, 255 ),  pt2 ( x[2], y[2])
	        // find point: pt3 ( -1024, yVal);
	        m = (255 - y[2]) / (preset1 - x[2]);
	        b = 255 - (m * preset1);
	        yVal = (m * (-1024)) + b;
	        x[1] = -1024;
	        y[1] = yVal;
	        z[1] = 0;
	        Preferences.debug("yVal = " + yVal);
	    } else {
	        x[1] = preset1;
	        y[1] = dim.height - 1;
	        z[1] = 0;
	    }
	
	    if (y[1] > 255) {
	        y[1] = 255;
	    }
	
	    x[3] = max; // 3071;
	    y[3] = 0;
	    z[3] = 0;
	    
	    LUT.getTransferFunction().importArrays(x, y, 4);
	    image.notifyImageDisplayListeners(LUT, true);
	}

	/**
	 * Build the custom dialog box for creating custom muscle types.
	 */
	private JPanel initDialogBox() {
		customPane = new ViewJFrameImage(srcImage);
		titlesArr = new ArrayList();
		
		JPanel mainPanel = initMainPanel();
		
		//if this is standalone (app frame hidden), add the tabbedpane from the messageframe to the bottom of the plugin's frame
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
			customPane.getContentPane().add(mainPanel, BorderLayout.CENTER);
		} else {
			JTabbedPane messageTabs = ViewUserInterface.getReference().getMessageFrame().getTabbedPane();
			messageTabs.setPreferredSize(new Dimension(customPane.getWidth(), 100));
			JSplitPane mainSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, mainPanel, messageTabs );
			mainSplit.setDividerLocation(550);
			customPane.getContentPane().add(mainSplit, BorderLayout.CENTER);
		}
	    
		//removes extra scrollPane from the mipav-loaded plugin
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
			customPane.getContentPane().remove(0);
	    } 
		
		customPane.pack();
		
		ctMode(customPane.getImageA(), -175, 275);
		customPane.updateImages(true);
		customPane.repaint();
		
		return mainPanel;
	}
	
	private JPanel initMainPanel() {
		//The component image will be displayed in a scrollpane.       
	    JScrollPane scrollPane = new JScrollPane(customPane.getComponentImage(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
	                                 ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	    
	    customPane.getComponentImage().useHighlight(false);
	    scrollPane.setFocusable(true);
	    scrollPane.setBackground(Color.black);
	    scrollPane.addKeyListener(customPane);    
	  
	    dialogTabs = new JTabbedPane();
	    dialogTabs.setMinimumSize(new Dimension (370, 532));
	    dialogTabs.setPreferredSize(new Dimension(370, 532));
	    dialogTabs.setMaximumSize(new Dimension (370, 532));
	    
	    JPanel[] tabs = new JPanel[2];
	    
	    JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));
	    
	    JPanel testPanel = new JPanel();
	    testPanel.setLayout(new BorderLayout());
	    testPanel.add(dialogTabs, BorderLayout.WEST);
	    testPanel.add(scrollPane, BorderLayout.CENTER);

	    panelA.add(testPanel);

    	//set up tabs here
    	tabs[0] = initMuscleTab(activeTab);
    	
    	tabs[0].addComponentListener(customPane);
    	tabs[0].addComponentListener(this);
    	tabs[0].setName("Tab 1");
    	dialogTabs.addTab("Tab 1", tabs[0]);
    	tabCount = 1;
    	
    	tabs[1] = new JPanel();
    	tabs[1].addComponentListener(customPane);
    	tabs[1].addComponentListener(this);
    	tabs[1].setName(NEW_TAB);
    	dialogTabs.addTab("Click to create a new tab...", tabs[1]);
    	
    	dialogTabs.addComponentListener(this);
	            
	    return panelA;
	}
	
	private JPanel initMuscleTab(int currentTab) {
		JPanel gridPane = new JPanel();
		//TODO: Use sequential groups in 1.6 to force display of nonedit and 
		//edit text field next to each other
		String editStr;
		JTextField title = new JTextField(editStr = "Enter the title for this panel");
		gridPane.add(title);
		titlesArr.add(title);
		
		if(currentTab == 0)
			customVOI = new ArrayList();
		customVOI.add(new ArrayList());
		for(int i=0; i<4; i++) {
			customVOI.get(currentTab).add(new MusclePane(this));
			customVOI.get(currentTab).get(i).setBorder(MipavUtil.buildTitledBorder("VOI #"+(i+1)));
			gridPane.add(customVOI.get(currentTab).get(i));
		}
		
		JButton incButton = new JButton(ADD_VOI);
		incButton.addActionListener(this);
		JButton launchButton = new JButton(LAUNCH);
		launchButton.addActionListener(this);
		gridPane.add(incButton);
		gridPane.add(launchButton);
		gridPane.setName("Tab "+currentTab);
		tabs.add(gridPane);
		return gridPane;
	}

	private void performDialog() {
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	new PlugInMuscleImageDisplay(srcImage, titles, voiList,  
        			imageType, symmetry, multipleSlices);
        } else {
        	new PlugInMuscleImageDisplay(srcImage, titles, voiList, 
        			imageType, symmetry, true, multipleSlices);
        }
	}
	
	public enum Option {
		Color("Color", java.awt.Color.RED),
		
		Symmetry("Symmetry", PlugInMuscleImageDisplay.Symmetry.NO_SYMMETRY),
		
		Num_Curves("Number of curves", 1),
		
		Do_Calc("Perform calculations", false),
		
		Do_Fill("Fill VOI on custom LUT", false),
		
		Is_Closed("Curves are closed", true);
		
		final String text;
		final Object setting;
		
		Option(String text, Object setting) {
			this.text = text;
			this.setting = setting;
		}
	}
	
	
		
		public static final String START_VOI = "Start VOI";
		public static final String START_PANE = "Start Pane";
		public static final String END_VOI = "End Voi";
		public static final String END_PANE = "End Pane";
		
		
		
		/**
		 * Checks that the file specified by name exists and is the correct extension (txt).  Does
		 * not check validity of file. Returned value indicates success of
		 * checking file.
		 * @param name
		 * @return
		 */
		private boolean fileCheck(String name) {
			File file = new File(name);
			if(!file.exists())
				return false;
			JFileChooser fileChoose = new JFileChooser();
			String fileType = fileChoose.getTypeDescription(file);
			System.out.println(fileType);
			if(!fileType.equals("txt"))
				return false;
			return true;
		}
		
		/**
		 * Reads the information stored in the file identified by name and stores in customVOI
		 * necessary information in standard format. Returned value indicates success of
		 * reading file.
		 * @param name
		 */
		
		private ArrayList<ArrayList<MusclePane>> fileRead(String name) {
			BufferedReader input = null;
			ArrayList<ArrayList<MusclePane>> customVOI = new ArrayList();
			try {
				input = new BufferedReader(new FileReader(name));
				MusclePane currentPane = null;
				String line, title, value, voiName;
				int paneNum = 0;
				boolean inPane = false, inVoi = false;
		 parse: while((line = input.readLine()) != null) {
			 		int breakPoint = line.indexOf(':');
					title = line.substring(0, breakPoint).trim();
					value = line.substring(breakPoint+1).trim();	
				 	if(inPane) {
						if(title.equals(END_PANE))
							inPane = false;
						else {
							if(inVoi) {
								if(title.equals(END_VOI))
									inVoi = false;
								else {
									switch(Option.valueOf(title)) {
									
									case Color:
										String[] colors = value.split(",");
										int[] numCol = new int[colors.length];
										for(int i=0; i<colors.length; i++)
											numCol[i] = Integer.valueOf(colors[i]);
										currentPane.setColorButton(new Color(numCol[0], numCol[1], numCol[2]));
										break;
										
									case Symmetry:
										currentPane.setSymmetry(PlugInMuscleImageDisplay.Symmetry.valueOf(value));
										break;
										
									case Num_Curves:
										currentPane.setNumCurves(Integer.valueOf(value));
										break;
										
									case Do_Calc:
										currentPane.setDoCalc(Boolean.valueOf(value));
										break;
										
									case Do_Fill:
										currentPane.setDoFill(Boolean.valueOf(value));
										break;
										
									case Is_Closed:
										currentPane.setIsClosed(Boolean.valueOf(value));
										break;
									}
									
								}
							} else {
								if(title.equals(START_VOI)) {
									voiName = value;
									currentPane = new MusclePane(this);
									currentPane.setName(voiName);
									customVOI.get(paneNum).add(currentPane);
								} else {
									MipavUtil.displayError("Badly formatted file.  Exiting program.");
									break parse;
								}
							}
						}
					} else {
						if(title.equals(START_PANE)) {
							customVOI.add(new ArrayList<MusclePane>());
							paneNum = customVOI.size()-1;
						} else {
							MipavUtil.displayError("Badly formatted file.  Exiting program.");
							break parse;
						}
					}
				}
			} catch(FileNotFoundException e) {
				MipavUtil.displayError("File not found despite passing internal check system.");
				e.printStackTrace();
				return null;
			} catch(IOException e) {
				MipavUtil.displayError("Error reading file, re-open file.");
				e.printStackTrace();
				return null;
			} finally {
				try {
					if(input != null) {
						input.close();
						return customVOI;
					}
				} catch(IOException e) {
					MipavUtil.displayError("Internal program error, failed to close file.");
					e.printStackTrace();
					return null;
				}
			} 
			return null;
		}
		
		/**
		 * Writes the information stored in customVOI into a text file conatining all
		 * necessary information in standard format. Returned value indicates success of
		 * writing file.
		 * @param name
		 */
		
		private boolean fileWrite(String name) {
			BufferedWriter output = null;
			MusclePane voi = null;
			try {
				output = new BufferedWriter(new FileWriter(name));
				for(int i=0; i<customVOI.size(); i++) {
					output.write(START_PANE+": "+i);
					for(int j=0; j<customVOI.get(i).size(); j++) {
						voi = customVOI.get(i).get(j);
						output.write(START_VOI+": "+voi.getName());
						Color col = null;
						if(!(col = voi.getColorButton()).equals(Option.Color.setting)) {						
							output.write(Option.Color.text+": "+
											col.getRed()+","+col.getGreen()+","+col.getBlue());
						}
						if(!voi.getSymmetry().equals(Option.Symmetry.setting)) {
							output.write(Option.Symmetry.text+": "+voi.getSymmetry());
						}
						if(!Integer.valueOf(voi.getNumCurves()).equals(Option.Num_Curves.setting)) {
							output.write(Option.Num_Curves.text+": "+voi.getNumCurves());
						}
						if(!Boolean.valueOf(voi.getDoCalc()).equals(Option.Do_Calc.setting)) {
							output.write(Option.Do_Calc.text+": "+voi.getDoCalc());
						}
						if(!Boolean.valueOf(voi.getDoFill()).equals(Option.Do_Fill.setting)) {
							output.write(Option.Do_Fill.text+": "+voi.getDoFill());
						}
						if(!Boolean.valueOf(voi.getIsClosed()).equals(Option.Is_Closed.setting)) {
							output.write(Option.Is_Closed.text+": "+voi.getIsClosed());
						}
						output.write(END_VOI);
					}
					output.write("END_PANE");
				}
			} catch(IOException e) {
				MipavUtil.displayError("Error writing file, bad requested file name.");
				e.printStackTrace();
				return false;
			} finally {
				try {
					if(output != null) {
						output.flush();
						output.close();
						return true;
					}
				} catch(IOException e) {
					MipavUtil.displayError("Internal program error, failed to close file.");
					e.printStackTrace();
					return false;
				}
			} 
			return false;
		}
	
	
	private class MusclePane extends JPanel implements Serializable {
		
		/**The color identifier*/
		private PlugInMuscleColorButtonPanel colorButton;
		
		/**The symmetry identifier*/
		private JComboBox symmetry;
		
		/**The name of the VOI*/
		private JTextField name;
		
		/**Number of curves in VOI*/
		private JComboBox numCurves;
		
		/**Whether calculations should be performed (defaults to false).*/
		private JCheckBox doCalc;
		
		/**Whether VOI should be filled when LUT applied (defaults to false. */
		private JCheckBox doFill;
		
		/**Whether the VOI is closed (defaults to true)*/
		private JCheckBox isClosed;
		
		public MusclePane(ActionListener caller) {
			super(new GridBagLayout());
			this.colorButton = new PlugInMuscleColorButtonPanel(Color.black, "VOI", caller);
			int i = 0;
			String[] text = new String[PlugInMuscleImageDisplay.Symmetry.values().length];
			
			for(PlugInMuscleImageDisplay.Symmetry symmetry : PlugInMuscleImageDisplay.Symmetry.values())
				text[i++] = symmetry.text;
			
			this.symmetry = new JComboBox(PlugInMuscleImageDisplay.Symmetry.values());
			this.name = new JTextField(DEFAULT_VOI);
			
			Integer[] numValues = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
			this.numCurves = new JComboBox(numValues);
			
			this.doCalc = new JCheckBox("Perform calc.");
			this.doFill = new JCheckBox("Fill VOI");
			this.isClosed = new JCheckBox("Closed curves");
			isClosed.setSelected(true);
			
			initDialog();
		}
		
		private void initDialog() {
			JLabel curveLabel = new JLabel("# curves");
			JLabel blankLabel = new JLabel("   ");

			GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.gridx = 0;
			c.gridy = 0;
			add(colorButton, c);
			c.gridx = 1;
			c.gridy = 0;
			add(symmetry, c);
			c.gridwidth = 3;
			c.gridx = 2;
			c.gridy = 0;
			c.anchor = GridBagConstraints.LINE_END;
			c.ipadx = 18;
			c.weightx = 1;
			c.fill = GridBagConstraints.BOTH;
			add(name, c);
			c.gridwidth = 2;
			c.gridx = 0;
			c.gridy = 1;
			c.ipadx = 0;
			c.anchor = GridBagConstraints.CENTER;
			c.weightx = 0;
			c.fill = GridBagConstraints.HORIZONTAL;
			add(doCalc, c);
			c.gridwidth = 1;
			c.gridx = 2;
			c.gridy = 1;
			add(doFill, c);
			c.gridwidth = 1;
			c.gridx = 3;
			c.gridy = 1;
			add(blankLabel, c);
			c.gridwidth = 1;
			c.gridx = 4;
			c.gridy = 1;
			c.anchor = GridBagConstraints.LINE_END;
			c.fill = GridBagConstraints.NONE;
			add(curveLabel, c);
			c.gridwidth = 2;
			c.gridx = 0;
			c.gridy = 2;
			c.anchor = GridBagConstraints.CENTER;
			c.fill = GridBagConstraints.HORIZONTAL;
			add(isClosed, c);
			c.gridwidth = 1;
			c.gridx = 4;
			c.gridy = 2;
			c.anchor = GridBagConstraints.LINE_END;
			c.fill = GridBagConstraints.NONE;
			add(numCurves, c);
		}

		public Color getColorButton() {
			return colorButton.getColorButton().getColorIcon().getColor();
		}

		public PlugInMuscleImageDisplay.Symmetry getSymmetry() {
			return ((PlugInMuscleImageDisplay.Symmetry)symmetry.getSelectedItem());
		}

		public String getName() {
			return name.getText();
		}
		
		public int getNumCurves() {
			return ((Integer)numCurves.getSelectedItem()).intValue();
		}

		public boolean getDoCalc() {
			return doCalc.isSelected();
		}

		public boolean getDoFill() {
			return doFill.isSelected();
		}
		
		public boolean getIsClosed() {
			return isClosed.isSelected();
		}
		
		/**
		 * Creates a muscle pane just like this one.
		 */
		public MusclePane createNew(ActionListener listener) {
			MusclePane p = new MusclePane(listener);
			p.setColorButton(getColorButton());
			p.setSymmetry(getSymmetry());
			p.setNumCurves(getNumCurves());
			p.setDoCalc(getDoCalc());
			p.setDoFill(getDoFill());
			p.setIsClosed(getIsClosed());
			p.setName(getName());
			
			return p;
		}

		public void setColorButton(Color c) {
			this.colorButton.getColorButton().getColorIcon().setColor(c);
		}

		public void setSymmetry(PlugInMuscleImageDisplay.Symmetry symmetry) {
			this.symmetry.setSelectedItem(symmetry);
		}

		public void setNumCurves(int numCurves) {
			this.numCurves.setSelectedIndex(numCurves-1);
		}

		public void setDoCalc(boolean doCalc) {
			this.doCalc.setSelected(doCalc);
		}

		public void setDoFill(boolean doFill) {
			this.doFill.setSelected(doFill);
		}

		public void setIsClosed(boolean isClosed) {
			this.isClosed.setSelected(isClosed);
		}

		public void setName(String name) {
			this.name.setText(name);
		}
		
	}
}

