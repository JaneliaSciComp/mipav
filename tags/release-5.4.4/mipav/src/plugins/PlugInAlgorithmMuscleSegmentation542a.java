//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Line2D;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;


/**
 * Creates an interface for working with Iceland CT images.
 * 
 * @author senseneyj
 *
 */
public class PlugInAlgorithmMuscleSegmentation542a extends AlgorithmBase implements ActionListener, ComponentListener {
    
    //~ Static fields --------------------------------------------------------------------------------------------------

	public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    public static final String LUT_IMAGE = "lutImage.tif";
    public static final String VOI_IMAGE = "voiImage.tif";
    
    public static final String NEW_TAB = "New Tab";
    public static final String ADD_VOI = "Add VOI";
    public static final String ADD_PANE = "Add one pane";
    public static final String LAUNCH = "Launch plugin";
    public static final String OPEN_TEMPLATE = "Open custom template";
    public static final String SAVE_TEMPLATE = "Save template";
    public static final String SAVE = "Save";
    public static final String DEFAULT_VOI = "Enter VOI name";
    public static final String DEFAULT_PANE = "Enter the title for this panel";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /**The current tab.*/
    private int activeTab = 0;
    
    /** denotes the type of srcImg (see enum ImageType) */
    private PlugInMuscleImageDisplay542a.ImageType imageType; 
    
    /** denotes the symmetry of srcImage */
    private PlugInMuscleImageDisplay542a.Symmetry symmetry;
    
    /**Whether multiple slices are contained in srcImg */
    private boolean multipleSlices;
       
    /**voiList created by various set up methods*/
    private PlugInSelectableVOI542a[][] voiList;
    
    /**Each muscle pane is one VOI in custom mode.*/
    private ArrayList<ArrayList<MusclePane>> customVOI;
    
    /**All of the requested titles*/
    private ArrayList<JTextField> titlesArr;
    
    /**Each particular tab is represented here.*/
    private ArrayList<JPanel> tabs;
    
    /**ArrayList of scrollPanes to have policies defined on add VOI call*/
    private ArrayList<JScrollPane> verticalPane;
    
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
    
    /**Name of file if image is of type run-time defined*/
    private String fileName;
    
    /**Image directory*/
    private String imageDir;
    
    /**Button to add a VOI*/
    private JButton incButton;

    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMuscleSegmentation542a(ModelImage srcImg, PlugInMuscleImageDisplay542a.ImageType imageType, 
    										 boolean multipleSlices, String fileName) {
        super(null, srcImg);
        this.imageType = imageType;
        this.multipleSlices = multipleSlices;
        //is equal to blank string if image is not run-time defined
        this.fileName = fileName;
        this.imageDir = srcImg.getImageDirectory();
        
        tabs = new ArrayList<JPanel>();
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
            	
            case RunTimeDefined: 
            	customPane = new ViewJFrameImage(srcImage);
            	customPane.setVisible(false);
        		titlesArr = new ArrayList<JTextField>();
        	    
        		//removes extra scrollPane from the mipav-loaded plugin
        		if (ViewUserInterface.getReference().isAppFrameVisible()) {
        			customPane.getContentPane().remove(0);
        	    } 
            	
        		customVOI = new ArrayList<ArrayList<MusclePane>>();
        		FileManager manager = new FileManager(this);
		    	manager.fileRead(srcImage.getImageDirectory()+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator+fileName);	
        		
            	buildCustomDialog();
				srcImage = (ModelImage)customPane.getImageA().clone();
				customPane.dispose();
				performDialog();
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
		if(e.getSource() instanceof JButton && ((JButton)e.getSource()).getText().equals("OK")) {
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
			
			verticalPane.get(activeTab).setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			int lastHeight = (int)tabs.get(activeTab).getPreferredSize().getHeight();
			tabs.get(activeTab).setPreferredSize(new Dimension(370, lastHeight+106));
			
			tabs.get(activeTab).add(pane, comps.length-5);
			tabs.get(activeTab).validate();
		} else if(e.getActionCommand().equals(LAUNCH)) {
			if(checkPanel()) {
				buildCustomDialog();
				srcImage = customPane.getImageA();
				performDialog();
				customPane.dispose();
			}
		} else if(e.getActionCommand().equals(OPEN_TEMPLATE)) {
			JFileChooser chooser = new JFileChooser();
			chooser.setDialogTitle("Open a VOI template");
			chooser.setCurrentDirectory(new File(imageDir+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator));
			chooser.setMultiSelectionEnabled(false);
		    chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".nia" }));
		    
		    int returnVal = chooser.showOpenDialog(customPane);
		    
		    if(returnVal == JFileChooser.APPROVE_OPTION) {
		    	FileManager manager = new FileManager(this);
		    	manager.fileRead(chooser.getSelectedFile().getAbsolutePath());	
		    	customPane.validate();
		    }
		} else if(e.getActionCommand().equals(SAVE_TEMPLATE)) {
			JFileChooser chooser = new JFileChooser();
			chooser.setCurrentDirectory(new File(imageDir+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator));
			chooser.setDialogTitle("Save the current VOI template");
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[]{".nia"}));

            int returnVal = chooser.showSaveDialog(null);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                String fileName = chooser.getSelectedFile().getName();
                String fileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                if(fileName.indexOf(".nia") == -1)
                	fileName += ".nia";
                File file = new File(fileDir + fileName);
                FileManager manager = new FileManager(this);
                manager.fileWrite(file.getAbsolutePath());
               
            } else {
                return;
            }
		} else if(e.getActionCommand().equals(ADD_PANE)) {
				
				JPanel newPanel = initMuscleTab(tabCount);
		    	
				newPanel.addComponentListener(customPane);
				newPanel.addComponentListener(this);
				newPanel.setName("Tab "+(tabCount+1));

				newPanel.setMinimumSize(new Dimension (370, 585));
				newPanel.setPreferredSize(new Dimension(370, 585));
				newPanel.setMaximumSize(new Dimension (370, 8000));
				
				activeTab = tabCount;

				JScrollPane verticalPaneSingle = new JScrollPane(newPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, 
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

		    	verticalPaneSingle.setMinimumSize(new Dimension (370, 585));
		    	verticalPaneSingle.setPreferredSize(new Dimension(370, 585));
		    	verticalPaneSingle.setMaximumSize(new Dimension (370, 585));
		    	verticalPaneSingle.addComponentListener(this);
		    	
		    	verticalPane.add(verticalPaneSingle);
		    	
		    	dialogTabs.addTab("Tab "+(tabCount+1), verticalPaneSingle);
		    	dialogTabs.setSelectedIndex(tabCount);
				
				tabCount++;
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
		if(e.getSource() instanceof JScrollPane && doEval && ((JScrollPane)e.getSource()).getViewport().getView() instanceof JPanel) {
			if(((JPanel)((JScrollPane)e.getSource()).getViewport().getView()).getName().indexOf("Tab ") != -1) {
				activeTab = Integer.valueOf(((JPanel)((JScrollPane)e.getSource()).getViewport().getView()).getName().substring(4)).intValue()-1;
			}
		}
	}

	private void buildAbdomenDialog() {
        
    	voiList = new PlugInSelectableVOI542a[3][];
    	int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
    	//String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
    	voiList[0] = new PlugInSelectableVOI542a[3];
    	voiList[0][0] = new PlugInSelectableVOI542a("Abdomen", true, 1, 0, false, true, imageSize, 0, Color.ORANGE);
    	voiList[0][1] = new PlugInSelectableVOI542a("Subcutaneous area", true, 1, 0, false, true, imageSize, 1, Color.RED);
    	voiList[0][2] = new PlugInSelectableVOI542a("Phantom", true, 1, 0, false, false, imageSize, Color.GREEN);
    	
    	voiList[1] = new PlugInSelectableVOI542a[5];
    	voiList[1][0] = new PlugInSelectableVOI542a("Visceral cavity", true, 1, 1, false, true, imageSize, 2, Color.ORANGE);
    	voiList[1][1] = new PlugInSelectableVOI542a("Liver", true, 1, 1, false, true, imageSize, 3, Color.RED);
    	voiList[1][2] = new PlugInSelectableVOI542a("Liver cysts", true, 10, 1, true, true, imageSize, 4, Color.GREEN);
    	voiList[1][3] = new PlugInSelectableVOI542a("Bone sample", true, 1, 1, false, false, imageSize, Color.CYAN);
    	voiList[1][4] = new PlugInSelectableVOI542a("Water sample", true, 1, 1, false, false, imageSize, Color.MAGENTA);
    	
    	voiList[2] = new PlugInSelectableVOI542a[9];
    	voiList[2][0] = new PlugInSelectableVOI542a("Left Psoas", true, 1, 2, true, true, imageSize, 5, Color.ORANGE);
    	voiList[2][1] = new PlugInSelectableVOI542a("Right Psoas", true, 1, 2, true, true, imageSize, 6, Color.ORANGE);
    	voiList[2][2] = new PlugInSelectableVOI542a("Left Lat. obliques", true, 1, 2, true, true, imageSize, 7, Color.RED);
    	voiList[2][3] = new PlugInSelectableVOI542a("Right Lat. obliques", true, 1, 2, true, true, imageSize, 8, Color.RED);
    	voiList[2][4] = new PlugInSelectableVOI542a("Left Paraspinous", true, 1, 2, true, true, imageSize, 9, Color.GREEN);
    	voiList[2][5] = new PlugInSelectableVOI542a("Right Paraspinous", true, 1, 2, true, true, imageSize, 10, Color.GREEN);
    	voiList[2][6] = new PlugInSelectableVOI542a("Left Rectus", true, 1, 2, true, true, imageSize, 11, Color.CYAN);
    	voiList[2][7] = new PlugInSelectableVOI542a("Right Rectus", true, 1, 2, true, true, imageSize, 12, Color.CYAN);
    	voiList[2][8] = new PlugInSelectableVOI542a("Aortic Calcium", true, 5, 2, true, true, imageSize, 13, Color.MAGENTA);
        
    	//Subcutaneous area has abdomen as child
    	voiList[0][1].addChild(voiList[0][0]);
    	//Liver has liver cysts as child
    	voiList[1][1].addChild(voiList[1][2]);
    	
        titles = new String[3];
        titles[0] = "Abdomen";
        titles[1] = "Tissue";
        titles[2] = "Muscles"; 
        
        symmetry = PlugInMuscleImageDisplay542a.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay542a.ImageType.Abdomen;
	    
	    String extendable = "Start Pane: Abdomen"+
	    	System.getProperty("line.separator")+"Start Voi: Abdomen"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Subcutaneous area"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Phantom"+System.getProperty("line.separator")+"Color: 0,255,0"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane"+
	    	System.getProperty("line.separator")+"Start Pane: Tissue"+
	    	System.getProperty("line.separator")+"Start Voi: Visceral cavity"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Liver"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Liver cysts"+System.getProperty("line.separator")+"Num_Curves: 7"+System.getProperty("line.separator")+"Color: 0,255,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+	System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Bone sample"+System.getProperty("line.separator")+"Color: 0,255,255"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Water sample"+System.getProperty("line.separator")+"Color: 255,0,255"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane"+
	    	System.getProperty("line.separator")+"Start Pane: Muscles"+
	    	System.getProperty("line.separator")+"Start Voi: Psoas"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Lat. obliques"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Paraspinous"+System.getProperty("line.separator")+"Color: 0,255,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Rectus"+System.getProperty("line.separator")+"Color: 0,255,255"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Aortic calcium"+System.getProperty("line.separator")+"Color: 255,0,255"+System.getProperty("line.separator")+"Num_Curves: 5"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane"+System.getProperty("line.separator");
	    
	    performFileSave(extendable, "Abdomen");
	    
    }
    
    /**
	 *   Builds thigh dialogue.
	 */
	private void buildThighDialog() {
	    
	    //String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
	    int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
	    
	    voiList = new PlugInSelectableVOI542a[3][];
	    
	    voiList[0] = new PlugInSelectableVOI542a[3];
	    voiList[0][0] = new PlugInSelectableVOI542a("Left Thigh", true, 1, 0, false, true, imageSize, 0, Color.ORANGE);
    	voiList[0][1] = new PlugInSelectableVOI542a("Right Thigh", true, 1, 0, false, true, imageSize, 1, Color.ORANGE);
    	
    	voiList[0][2] = new PlugInSelectableVOI542a("Phantom", true, 1, 0, false, false, imageSize, Color.RED);
	    
    	voiList[1] = new PlugInSelectableVOI542a[5];
	    voiList[1][0] = new PlugInSelectableVOI542a("Left Bone", true, 1, 1, false, true, imageSize, 2, Color.ORANGE);
    	voiList[1][1] = new PlugInSelectableVOI542a("Right Bone", true, 1, 1, false, true, imageSize, 3, Color.ORANGE);
	    voiList[1][2] = new PlugInSelectableVOI542a("Left Marrow", true, 1, 1, true, true, imageSize, 4, Color.RED);
    	voiList[1][3] = new PlugInSelectableVOI542a("Right Marrow", true, 1, 1, true, true, imageSize, 5, Color.RED);
    	
    	voiList[1][4] = new PlugInSelectableVOI542a("Bone sample", true, 1, 1, false, false, imageSize, Color.GREEN);
	    
    	voiList[2] = new PlugInSelectableVOI542a[11];
	    voiList[2][0] = new PlugInSelectableVOI542a("Left Fascia", true, 1, 2, false, true, imageSize, 6, Color.ORANGE);
    	voiList[2][1] = new PlugInSelectableVOI542a("Right Fascia", true, 1, 2, false, true, imageSize, 7, Color.ORANGE);
	    voiList[2][2] = new PlugInSelectableVOI542a("Left Quads", true, 1, 2, true, true, imageSize, 8, Color.RED);
    	voiList[2][3] = new PlugInSelectableVOI542a("Right Quads", true, 1, 2, true, true, imageSize, 9, Color.RED);
	    voiList[2][4] = new PlugInSelectableVOI542a("Left Hamstrings", true, 1, 2, true, true, imageSize, 10, Color.GREEN);
    	voiList[2][5] = new PlugInSelectableVOI542a("Right Hamstrings", true, 1, 2, true, true, imageSize, 11, Color.GREEN);
	    voiList[2][6] = new PlugInSelectableVOI542a("Left Sartorius", true, 1, 2, true, true, imageSize, 12, Color.CYAN);
    	voiList[2][7] = new PlugInSelectableVOI542a("Right Sartorius", true, 1, 2, true, true, imageSize, 13, Color.CYAN);
	    voiList[2][8] = new PlugInSelectableVOI542a("Left Adductors", true, 1, 2, true, true, imageSize, 14, Color.MAGENTA);
    	voiList[2][9] = new PlugInSelectableVOI542a("Right Adductors", true, 1, 2, true, true, imageSize, 15, Color.MAGENTA);

    	voiList[2][10] = new PlugInSelectableVOI542a("Water sample", true, 1, 1, false, false, imageSize, Color.CYAN);
	    
    	//Left thigh has left bone and left marrow as children
    	voiList[0][0].addChild(voiList[1][0]);
    	voiList[0][0].addChild(voiList[1][2]);
    	
    	//Left bone has left marrow as child
    	voiList[1][0].addChild(voiList[1][2]);
    	
    	//Left thigh has right bone and right marrow as children
    	voiList[0][1].addChild(voiList[1][1]);
    	voiList[0][1].addChild(voiList[1][3]);
    	
    	//Left bone has right marrow as child
    	voiList[1][1].addChild(voiList[1][3]);
    	
	    titles = new String[3];
	    titles[0] = "Thigh";
	    titles[1] = "Bone";
	    titles[2] = "Muscles";
	     
	    symmetry = PlugInMuscleImageDisplay542a.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay542a.ImageType.Thigh;
	    
	    String extendable = "Start Pane: Thigh"+
	    	System.getProperty("line.separator")+"Start Voi: Thigh"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Phantom"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane"+
	    	System.getProperty("line.separator")+"Start Pane: Bone"+
	    	System.getProperty("line.separator")+"Start Voi: Bone"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Marrow"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Bone sample"+System.getProperty("line.separator")+"Color: 0,255,0"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane"+
	    	System.getProperty("line.separator")+"Start Pane: Muscles"+
	    	System.getProperty("line.separator")+"Start Voi: Fascia"+System.getProperty("line.separator")+"Color: 255,200,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Quads"+System.getProperty("line.separator")+"Color: 255,0,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Hamstrings"+System.getProperty("line.separator")+"Color: 0,255,0"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Sartorius"+System.getProperty("line.separator")+"Color: 0,255,255"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Adductors"+System.getProperty("line.separator")+"Color: 255,0,255"+System.getProperty("line.separator")+"Symmetry: Left/Right"+System.getProperty("line.separator")+"Do_Calc: true"+System.getProperty("line.separator")+"Do_Fill: true"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"Start Voi: Water sample"+System.getProperty("line.separator")+"Color: 255,255,0"+System.getProperty("line.separator")+"End Voi"+
	    	System.getProperty("line.separator")+"End Pane";
	    
	    performFileSave(extendable, "Thigh");
	}
	
	private void performFileSave(String extendable, String name) {
		File niaFolder = new File(srcImage.getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator); 
	    BufferedWriter output = null;
	    try {
	    	if(!niaFolder.exists())
	    		niaFolder.mkdir();
	    	File configFile = new File(srcImage.getFileInfo(0).getFileDirectory()+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator+name+".nia");
			output = new BufferedWriter(new FileWriter(configFile));
			output.write(extendable);
	    } catch(IOException e) {
	    	MipavUtil.displayError("Unable to save configuration file.");
	    	e.printStackTrace();
	    } finally {
			try {
				if(output != null) {
					output.flush();
					output.close();
				}
			} catch(IOException e) {
				MipavUtil.displayError("Internal program error, failed to close file.");
				e.printStackTrace();
			}
		} 
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
					if(temp.getSymmetry().equals(PlugInMuscleImageDisplay542a.Symmetry.NO_SYMMETRY))
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
		
		voiList = new PlugInSelectableVOI542a[validPanes][];
		int outputLoc = 0;
		
		for(int i=0; i<validPanes; i++) {
			voiList[i] = new PlugInSelectableVOI542a[validVOI[i]];
			int numVoiFilled = 0;
			for(int j=0; j<customVOI.get(i).size(); j++) {
				MusclePane temp = customVOI.get(i).get(j);
				if((!temp.getName().equals(DEFAULT_VOI)) && temp.getName().trim().length() > 0) {
					Color tempColor = temp.getColorButton();
					if(tempColor.equals(Color.BLACK))
						tempColor = PlugInSelectableVOI542a.INVALID_COLOR;
					if(temp.getDoCalc()) {
						voiList[i][numVoiFilled] = new PlugInSelectableVOI542a(temp.getName(), temp.getIsClosed(), 
								temp.getNumCurves(), i, temp.getDoFill(), temp.getDoCalc(), imageSize, outputLoc++, tempColor);
					} else {
						voiList[i][numVoiFilled] = new PlugInSelectableVOI542a(temp.getName(), temp.getIsClosed(), 
								temp.getNumCurves(), i, temp.getDoFill(), temp.getDoCalc(), imageSize, tempColor);
					}
					numVoiFilled++;
				}
			}
		}
		
		titles = new String[validPanes];
		for(int i=0; i<validPanes; i++) {
			titles[i] = titlesArr.get(i).getText();
		}
		
		symmetry = PlugInMuscleImageDisplay542a.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay542a.ImageType.Custom;
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
		titlesArr = new ArrayList<JTextField>();
		
		JPanel mainPanel = initMainPanel();
		
		//if this is standalone (app frame hidden), add the tabbedpane from the messageframe to the bottom of the plugin's frame
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
			ViewUserInterface.getReference().getMessageFrame().append("Working on GUI2", ViewJFrameMessage.DATA);
			customPane.setBackground(ViewUserInterface.getReference().getMainFrame().getBackground());
			customPane.getContentPane().setBackground(ViewUserInterface.getReference().getMainFrame().getBackground());
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
		
		//TODO: Implement dependent scheme when 1.6 is introduced
		//for(int i=0; i<customVOI.size(); i++) {
		//	for(int j=0; j<customVOI.get(i).size(); j++) {
		//		customVOI.get(i).get(j).initDependents(customVOI.get(i).size());
		//	}
		//}
		
		return mainPanel;
	}
	
	private JPanel initMainPanel() {
		//The component image will be displayed in a scrollpane.       
	    JScrollPane scrollPane = new JScrollPane(customPane.getComponentImage(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
	                                 ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	    
	    customPane.getComponentImage().useHighlight(false);
	    
	    scrollPane.setFocusable(true);
	    scrollPane.setBackground(Color.black);
	    //scrollPane.addKeyListener(customPane);    
	  
	    dialogTabs = new JTabbedPane();
	    dialogTabs.setMinimumSize(new Dimension (370, 585));
	    dialogTabs.setPreferredSize(new Dimension(370, 585));
	    dialogTabs.setMaximumSize(new Dimension (370, 585));
	    
	    JPanel[] tabs = new JPanel[1];
	    
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
    	tabs[0].setMinimumSize(new Dimension (370, 585));
	    tabs[0].setPreferredSize(new Dimension(370, 585));
	    tabs[0].setMaximumSize(new Dimension (370, 8000));
    	
	    verticalPane = new ArrayList<JScrollPane>();
	    JScrollPane verticalPaneSingle = new JScrollPane(tabs[0], ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, 
										ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	    verticalPaneSingle.addComponentListener(this);
    	
    	verticalPaneSingle.setMinimumSize(new Dimension (370, 585));
    	verticalPaneSingle.setPreferredSize(new Dimension(370, 585));
    	verticalPaneSingle.setMaximumSize(new Dimension (370, 585));
    	
    	verticalPane.add(verticalPaneSingle);
    	
    	dialogTabs.addTab("Tab 1", verticalPaneSingle);
    	tabCount = 1;
	
    	dialogTabs.addComponentListener(this);
	            
	    return panelA;
	}
	
	private JPanel initMuscleTab(int currentTab) {
		JPanel gridPane = new JPanel();
		JTextField title = new JTextField(DEFAULT_PANE);
		title.setMinimumSize(new Dimension(180, 25));
		title.setPreferredSize(new Dimension(180, 25));
		title.setMaximumSize(new Dimension(180, 25));
		gridPane.add(title);
		titlesArr.add(title);
		
		JButton incPaneButton = new JButton(ADD_PANE);
		incPaneButton.addActionListener(this);
		incButton = new JButton(ADD_VOI);
		incButton.addActionListener(this);
		JButton launchButton = new JButton(LAUNCH);
		launchButton.addActionListener(this);
		JButton openButton = new JButton(OPEN_TEMPLATE);
		openButton.addActionListener(this);
		JButton saveButton = new JButton(SAVE_TEMPLATE);
		saveButton.addActionListener(this);
		
		if(currentTab == 0)
			customVOI = new ArrayList<ArrayList<MusclePane>>();
		customVOI.add(new ArrayList<MusclePane>());
		for(int i=0; i<4; i++) {
			customVOI.get(currentTab).add(new MusclePane(this));
			customVOI.get(currentTab).get(i).setBorder(MipavUtil.buildTitledBorder("VOI #"+(i+1)));
			gridPane.add(customVOI.get(currentTab).get(i));
		}
		
		
		gridPane.add(incPaneButton);
		gridPane.add(incButton);
		gridPane.add(launchButton);
		gridPane.add(openButton);
		gridPane.add(saveButton);
		gridPane.setName("Tab "+currentTab);
		tabs.add(gridPane);
		return gridPane;
	}

	private void performDialog() {
		ViewJFrameImage i = null;
		    
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	i = new PlugInMuscleImageDisplay542a(srcImage, titles, voiList,  
        			imageType, symmetry, multipleSlices);
        } else {
        	i = new PlugInMuscleImageDisplay542a(srcImage, titles, voiList, 
        			imageType, symmetry, true, multipleSlices);
        }
        
        boolean b = i.requestFocusInWindow();
        System.out.println("Has the focus: "+b);
		System.out.println("Added as a key listener");
		//i.addKeyListener(i);
	}
	
	public enum Option {
		Color("Color", java.awt.Color.BLACK),
		
		Symmetry("Symmetry", PlugInMuscleImageDisplay542a.Symmetry.NO_SYMMETRY),
		
		Num_Curves("Num_Curves", 1),
		
		Do_Calc("Do_Calc", false),
		
		Do_Fill("Do_Fill", false),
		
		Is_Closed("Is_Closed", true);
		
		final String text;
		final Object setting;
		
		Option(String text, Object setting) {
			this.text = text;
			this.setting = setting;
		}
		
		public String toString() {
			return text;
		}
	}
	
	private class FileManager {
		
		public static final String START_VOI = "Start Voi";
		public static final String START_PANE = "Start Pane";
		public static final String END_VOI = "End Voi";
		public static final String END_PANE = "End Pane";
		

		private PlugInAlgorithmMuscleSegmentation542a parent;
		
		public FileManager(PlugInAlgorithmMuscleSegmentation542a parent) {
			this.parent = parent;
		}
		
		/**
		 * Reads the information stored in the file identified by name and stores in customVOI
		 * necessary information in standard format. Returned value indicates success of
		 * reading file.
		 * @param fileName
		 */
		
		private ArrayList<ArrayList<MusclePane>> fileRead(String fileName) {
			BufferedReader input = null;
			try {
				input = new BufferedReader(new FileReader(fileName));
				MusclePane currentPane = null;
				String line, title, value = null, voiName;
				int paneNum = 0, voiNum = 0;
				boolean inPane = false, inVoi = false;
		 parse: while((line = input.readLine()) != null) {
			 		int breakPoint = line.indexOf(':');
			 		if(breakPoint != -1) {
						title = line.substring(0, breakPoint).trim();
						value = line.substring(breakPoint+1).trim();	
			 		} else 
			 			title = line;
				 	if(inPane) {
						if(title.equals(END_PANE)) {
							inPane = false;
							paneNum++;
						} else {
							if(inVoi) {
								if(title.equals(END_VOI)) {
									inVoi = false;
									voiNum++;
								} else {
									switch(Option.valueOf(title)) {
									
									case Color:
										String[] colors = value.split(",");
										int[] numCol = new int[colors.length];
										for(int i=0; i<colors.length; i++)
											numCol[i] = Integer.valueOf(colors[i]);
										currentPane.setColorButton(new Color(numCol[0], numCol[1], numCol[2]));
										break;
										
									case Symmetry:
										if(value.equals(PlugInMuscleImageDisplay542a.Symmetry.LEFT_RIGHT.toString()))
											currentPane.setSymmetry(PlugInMuscleImageDisplay542a.Symmetry.LEFT_RIGHT);
										else if(value.equals(PlugInMuscleImageDisplay542a.Symmetry.TOP_BOTTOM.toString()))
											currentPane.setSymmetry(PlugInMuscleImageDisplay542a.Symmetry.TOP_BOTTOM);
										else if(value.equals(PlugInMuscleImageDisplay542a.Symmetry.NO_SYMMETRY.toString()))
											currentPane.setSymmetry(PlugInMuscleImageDisplay542a.Symmetry.NO_SYMMETRY);
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
									if(voiNum > customVOI.get(paneNum).size()-1) {
										int length = 0;
										Component[] comps = tabs.get(activeTab).getComponents();
										for(int i=0; i<comps.length; i++) {
											if(comps[i] instanceof MusclePane)
												length++;
										}
										
										if(imageType.equals(PlugInMuscleImageDisplay542a.ImageType.Custom))
											verticalPane.get(activeTab).setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
										int lastHeight = (int)tabs.get(activeTab).getPreferredSize().getHeight();
										tabs.get(activeTab).setPreferredSize(new Dimension(370, lastHeight+106));
										
										currentPane = new MusclePane(parent);
										currentPane.setBorder(MipavUtil.buildTitledBorder("VOI #"+(length+1)));
										customVOI.get(activeTab).add(currentPane);
										tabs.get(activeTab).add(currentPane, comps.length-5);
									} else {
										currentPane = customVOI.get(paneNum).get(voiNum);	
									}
									inVoi = true;
									currentPane.setName(voiName);
									
								} else {
									MipavUtil.displayError("Badly formatted file.  Please choose another template.");
									break parse;
								}
							}
						}
					} else {
						if(title.equals(START_PANE)) {
							if(paneNum > customVOI.size()-1) {
								//customVOI.add(new ArrayList<MusclePane>());
								//titlesArr.add(new JTextField(value));
								
								JPanel newPanel = initMuscleTab(tabCount);
						    	
								newPanel.addComponentListener(customPane);
								newPanel.addComponentListener(parent);
								newPanel.setName("Tab "+(tabCount+1));
								newPanel.setMinimumSize(new Dimension (370, 585));
								newPanel.setPreferredSize(new Dimension(370, 585));
								newPanel.setMaximumSize(new Dimension (370, 8000));
								
								if(imageType.equals(PlugInMuscleImageDisplay542a.ImageType.Custom)) {				
									JScrollPane verticalPaneSingle = new JScrollPane(newPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, 
											ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
	    	
							    	verticalPaneSingle.setMinimumSize(new Dimension (370, 585));
							    	verticalPaneSingle.setPreferredSize(new Dimension(370, 585));
							    	verticalPaneSingle.setMaximumSize(new Dimension (370, 5000));
							    	verticalPaneSingle.addComponentListener(parent);
							    	
							    	verticalPane.add(verticalPaneSingle);
							    	
							    	dialogTabs.addTab("Tab "+(tabCount+1), verticalPaneSingle);
							    	dialogTabs.setSelectedIndex(tabCount);
								}
								activeTab = tabCount;
								titlesArr.get(paneNum).setText(value);
								tabCount++;
							} else
								titlesArr.get(paneNum).setText(value);
							voiNum = 0;
							inPane = true;
						} else {
							MipavUtil.displayError("Badly formatted file.  Please choose another template.");
							break parse;
						}
					}
				}
			} catch(FileNotFoundException e) {
				MipavUtil.displayError("File not found despite passing internal check system.");
				e.printStackTrace();
				return null;
			} catch(Exception e) {
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
		 * @param fileName
		 */
		
		private boolean fileWrite(String fileName) {
			BufferedWriter output = null;
			MusclePane voi = null;
			try {
				output = new BufferedWriter(new FileWriter(fileName));
				for(int i=0; i<customVOI.size(); i++) {
					output.write(START_PANE+": "+titlesArr.get(i).getText()+"\n");
			  pane: for(int j=0; j<customVOI.get(i).size(); j++) {
				  		if(customVOI.get(i).get(j).getName().equals(DEFAULT_VOI))
				  			break pane;
				  		if(customVOI.get(i).get(j).getName().length() == 0)
				  			continue pane;
						voi = customVOI.get(i).get(j);
						output.write(START_VOI+": "+voi.getName()+"\n");
						Color col = null;
						if(!(col = voi.getColorButton()).equals(Option.Color.setting) && 
								!(col = voi.getColorButton()).equals(PlugInSelectableVOI542a.INVALID_COLOR)) {						
							output.write(Option.Color.text+": "+
											col.getRed()+","+col.getGreen()+","+col.getBlue()+"\n");
						}
						if(!voi.getSymmetry().equals(Option.Symmetry.setting)) {
							output.write(Option.Symmetry.text+": "+voi.getSymmetry()+"\n");
						}
						if(!Integer.valueOf(voi.getNumCurves()).equals(Option.Num_Curves.setting)) {
							output.write(Option.Num_Curves.text+": "+voi.getNumCurves()+"\n");
						}
						if(!Boolean.valueOf(voi.getDoCalc()).equals(Option.Do_Calc.setting)) {
							output.write(Option.Do_Calc.text+": "+voi.getDoCalc()+"\n");
						}
						if(!Boolean.valueOf(voi.getDoFill()).equals(Option.Do_Fill.setting)) {
							output.write(Option.Do_Fill.text+": "+voi.getDoFill()+"\n");
						}
						if(!Boolean.valueOf(voi.getIsClosed()).equals(Option.Is_Closed.setting)) {
							output.write(Option.Is_Closed.text+": "+voi.getIsClosed()+"\n");
						}
						output.write(END_VOI+"\n");
					}
					output.write(END_PANE+"\n");
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
	}
	
	/**
	 * A muscle pane is an individual VOI, identifying characteristics about the particular VOI.
	 * 
	 * @author senseneyj
	 *
	 */
	private class MusclePane extends JPanel implements Serializable, ActionListener, MouseListener {
		
		/**The color identifier*/
		private PlugInMuscleColorButtonPanel542a colorButton;
		
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
		
		/**ColorChooser for selecting custom VOI colors*/
	    private ViewJColorChooser colorChooser;
	    
	    /**List of potential children for this MusclePane*/
	    //TODO: Implemented for 1.6 Java graphics only
	    //private ArrayList<DependencyNode> lineList;
	    
	    /**Selected dependency node*/
	    //private DependencyNode selected = null;
		
		public MusclePane(ActionListener caller) {
			super(new GridBagLayout());
			this.colorButton = new PlugInMuscleColorButtonPanel542a(Color.black, "VOI", this);
			int i = 0;
			String[] text = new String[PlugInMuscleImageDisplay542a.Symmetry.values().length];
			
			for(PlugInMuscleImageDisplay542a.Symmetry symmetry : PlugInMuscleImageDisplay542a.Symmetry.values())
				text[i++] = symmetry.toString();
			
			this.symmetry = new JComboBox(PlugInMuscleImageDisplay542a.Symmetry.values());
			this.name = new JTextField(DEFAULT_VOI);
			
			Integer[] numValues = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
			this.numCurves = new JComboBox(numValues);
			
			this.doCalc = new JCheckBox("Perform calc.");
			this.doFill = new JCheckBox("Fill VOI");
			this.isClosed = new JCheckBox("Closed curves");
			isClosed.setSelected(true);
			
			//let this MusclePane be aware when a new VOI has been added
			incButton.addActionListener(this);
			
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
			c.ipadx = 25;
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
		
		//TODO: Implement dependency arrows when 1.6 is available, until then this code is not valid
		//public void initDependents(int size) {
		//	lineList = new ArrayList<DependencyNode>(size);
		//	for(int j=0; j<size-1; j++) {
		//		Point p1 = this.getLocationOnScreen();
		//		Point2D.Double p2 = new Point2D.Double(p1.x-10, p1.y);
		//		DependencyNode l;
		//		lineList.add(l = new DependencyNode(p1, p2));	
		//	}
		//}

		public void actionPerformed(ActionEvent e) {
			if(e.getSource() instanceof JButton && ((JButton)e.getSource()).getText().equals("OK")) {
				colorButton.setColor(colorChooser.getColor());
				getGraphics().setColor(colorChooser.getColor());
			} else if(e.getSource() instanceof PlugInMuscleColorButton542a) {
				colorChooser = new ViewJColorChooser(new Frame(), "Pick VOI color", 
						this, this);
			} 
			//TODO: This code relates to 1.6 graphics abilities
			//else if(e.getActionCommand().equals(ADD_VOI)) {
			//		Point p1 = this.getLocationOnScreen();
			//		Point2D.Double p2 = new Point2D.Double(p1.x-10, p1.y);
			//		DependencyNode l;
			//		lineList.add(new DependencyNode(p1, p2));	
			//}
		}

		public void mouseClicked(MouseEvent e) {
			
			
		}

		public void mouseEntered(MouseEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void mouseExited(MouseEvent e) {
			// TODO Auto-generated method stub
			
		}

		public void mousePressed(MouseEvent e) {
			//DependencyNode possiblySelected = null;
			//boolean newLine = false;
			//TODO: This code relates to 1.6 graphics abilities
			//for(int i=0; i<lineList.size(); i++) {
			//	if(!lineList.get(i).isPointing()) {
			//		possiblySelected = lineList.get(i);
			//		newLine = true;
			//	}
			//}
			//if(newLine && possiblySelected.contains(e.getPoint())) {
				//selected = possiblySelected;
			//}
			
		}

		public void mouseReleased(MouseEvent e) {
			
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

		public Color getColorButton() {
			return colorButton.getColorButton().getColorIcon().getColor();
		}

		public PlugInMuscleImageDisplay542a.Symmetry getSymmetry() {
			return ((PlugInMuscleImageDisplay542a.Symmetry)symmetry.getSelectedItem());
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
		
		//public ArrayList<DependencyNode> getLineList() {
		//	return lineList;
		//}

		public void setColorButton(Color c) {
			this.colorButton.getColorButton().getColorIcon().setColor(c);
		}

		public void setSymmetry(PlugInMuscleImageDisplay542a.Symmetry symmetry) {
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
		
		
		private class DependencyNode extends Line2D.Double implements Serializable, MouseMotionListener, ActionListener {

			/**Whether the current node is pointing to another muscle pane.*/
			//private boolean isPointing;
			
			/*public DependencyNode(Point p1, Point2D.Double p2) {
				super(p1,p2);
				isPointing = false;
				
			}*/
			
			/*private boolean isPointing() {
				return isPointing;
			}*/
			
			public void mouseDragged(MouseEvent e) {
				//TODO:Convert e.getLocationOnScreen to 1.5 compatible call
				//setLine(new Point2D.Double(x1, y1), e.getLocationOnScreen());
			}

			public void mouseMoved(MouseEvent e) {
	
			}

			public void actionPerformed(ActionEvent e) { 
				if(e.getSource() instanceof JButton && ((JButton)e.getSource()).getText().equals("OK")) {
					
				}
			}
		}
	}
}

