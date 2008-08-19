
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import java.util.TreeMap;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
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
		} else if(e.getActionCommand().equals("Add another VOI")){
			int length = tabs.get(activeTab).getComponents().length;
			MusclePane pane = new MusclePane(this);
			pane.setBorder(MipavUtil.buildTitledBorder("VOI #"+(length)));
			customVOI.get(activeTab).add(pane);
			//JButton copy = (JButton)tabs.get(activeTab).getComponent(length-1);
			//tabs.get(activeTab).remove(length-1);
			//tabs.get(activeTab).repaint();
			tabs.get(activeTab).add(pane, length-1);
			//pane.setVisible(true);
			//tabs.get(activeTab).repaint();
			
			//tabs.get(activeTab).add(copy);
			//tabs.get(activeTab).repaint();
			
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
		customDialog.setVisible(false);
		MipavUtil.displayError("Custom dialog not yet built. Plugin will now end.");
	}
	
	/**
	 * Checks that information has been entered correctly.
	 */
	private boolean checkPanel() {
		return true;
	}
	
	/**
	 * Build the custom dialog box for creating custom muscle types.
	 */
	private JPanel initDialogBox() {
		customPane = new ViewJFrameImage(srcImage);
		
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
		if(currentTab == 0)
			customVOI = new ArrayList();
		customVOI.add(new ArrayList());
		for(int i=0; i<4; i++) {
			customVOI.get(currentTab).add(new MusclePane(this));
			customVOI.get(currentTab).get(i).setBorder(MipavUtil.buildTitledBorder("VOI #"+(i+1)));
			gridPane.add(customVOI.get(currentTab).get(i));
		}
		
		JButton incButton = new JButton("Add another VOI");
		incButton.addActionListener(this);
		gridPane.add(incButton);
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
	
	private class MusclePane extends JPanel {
		
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
			
			this.symmetry = new JComboBox(text);
			this.name = new JTextField("Enter VOI name");
			
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
			c.weightx = .5;
			c.fill = GridBagConstraints.VERTICAL;
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
			return Integer.valueOf(((String)numCurves.getSelectedItem())).intValue();
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
	}
}

