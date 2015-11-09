package nibib.spim;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;

import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
//MIPAV is freely available from http://mipav.cit.nih.gov
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;

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

public class PlugInDialogStageScan extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

	//private static final long serialVersionUID;
	
	final String initResultLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "Result" + File.separator;
	
	//~ Instance fields ------------------------------------------------------------------------------------------------

    private ModelImage resultImage = null;
    
    /** This is your algorithm */
    private PlugInAlgorithmStageScan stageScanAlgo = null;
    
    private GridBagConstraints gbc;
    
    private JPanel okCancelPanel;
    
    private JTextField AFileDirectoryText;
    
    private String AFileDirectory;
    
    private JTextField AFileDark2DText;
    
    private String AFileDark2D;
    
    private JTextField ALeftShiftText;
    
    private double ALeftShift;
    
    private JTextField ARangeText;
    
    private File[][] AImageAr;
    
    private JTextField BFileDirectoryText;
    
    private String BFileDirectory;
    
    private JTextField BFileDark2DText;
    
    private String BFileDark2D;
    
    private JTextField BLeftShiftText;
    
    private double BLeftShift;
    
    private JTextField BRangeText;
    
    private File[][] BImageAr;
    
    private JTextField subDirectoryRangeText;
    
    private int subDirectoryLowerBound = 0;
    
    private File[] ASubDirectoryAr;
    
    private File[] BSubDirectoryAr;
    
    private JTextField resultDirectoryText;
    
    private String resultDirectoryString;
    
    private File resultDirectory;
    
    private JTextField concurrentNumText;
    
    private int concurrentNum;
    
    private int subDirectoryNumber;
    
//~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogStageScan() { }

    /**
     * Creates new dialog for reference subtraction, slice x shift, and imageA z axis flip using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogStageScan(boolean modal) {
        super(modal);
        
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

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

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            //dispose();
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
            super.actionPerformed(event);
        }
        //System.out.print(this.getSize());
    } // end actionPerformed()
    
    /**
     * Once all the necessary variables are set, call the stage scan algorithm
     */
    protected void callAlgorithm() {

        try {
            
            stageScanAlgo = new PlugInAlgorithmStageScan(AFileDark2D, ALeftShift, AImageAr,
            		BFileDark2D, BLeftShift, BImageAr, subDirectoryLowerBound, resultDirectory, concurrentNum);
            
         // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            stageScanAlgo.addListener(this);
            //createProgressBar("Creating plugin", " ...", stageScanAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (stageScanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                stageScanAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }
    }

	
	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmStageScan) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((stageScanAlgo.isCompleted() == true)) {
                Collection<ModelImage> list = stageScanAlgo.getResultImageList();
                synchronized(list) {
                    Iterator<ModelImage> itr = list.iterator();
                    while(itr.hasNext()) {
                        new ViewJFrameImage(itr.next());
                    }
                }
                insertScriptLine();
            } 

            if (stageScanAlgo != null) {
                stageScanAlgo.finalize();
                stageScanAlgo = null;
            }

            //dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    private void init() {
        setResizable(true);
        setForeground(Color.black);
        setTitle("Stage scan 1");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
        
        GuiBuilder gui = new GuiBuilder(this);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));
        
        gbc.gridwidth = 2;
        AFileDirectoryText = gui.buildFileField("A input directory: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        mainPanel.add(AFileDirectoryText.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        AFileDark2DText = gui.buildFileField("A 2D dark image: ", " ", false, JFileChooser.FILES_ONLY);
        mainPanel.add(AFileDark2DText.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        gbc.gridwidth = 1;
        final JLabel ALeftShiftLabel = new JLabel("   A pixel left shift");
        ALeftShiftLabel.setFont(serif12);
        ALeftShiftLabel.setForeground(Color.black);
        mainPanel.add(ALeftShiftLabel, gbc);
        
        gbc.gridx = 1;
        ALeftShiftText = new JTextField(10);
        ALeftShiftText.setText("4.0");
        ALeftShiftText.setFont(serif12);
        ALeftShiftText.setForeground(Color.black);
        mainPanel.add(ALeftShiftText, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        gbc.gridwidth = 2;
        
        ARangeText = gui.buildField("Range of A slices to use (ex. 20-115): ", " ");
        mainPanel.add(ARangeText.getParent(), gbc);
        gbc.gridy++;
        
        BFileDirectoryText = gui.buildFileField("B input directory: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        mainPanel.add(BFileDirectoryText.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        BFileDark2DText = gui.buildFileField("B 2D dark image: ", " ", false, JFileChooser.FILES_ONLY);
        mainPanel.add(BFileDark2DText.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        gbc.gridwidth = 1;
        final JLabel BLeftShiftLabel = new JLabel("   B pixel left shift");
        BLeftShiftLabel.setFont(serif12);
        BLeftShiftLabel.setForeground(Color.black);
        mainPanel.add(BLeftShiftLabel, gbc);
        
        gbc.gridx = 1;
        BLeftShiftText = new JTextField(10);
        BLeftShiftText.setText("4.0");
        BLeftShiftText.setFont(serif12);
        BLeftShiftText.setForeground(Color.black);
        mainPanel.add(BLeftShiftText, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        
        gbc.gridwidth = 2;
        
        BRangeText = gui.buildField("Range of B slices to use (ex. 20-115): ", " ");
        mainPanel.add(BRangeText.getParent(), gbc);
        gbc.gridy++;
        
        subDirectoryRangeText = gui.buildField("Range of SPIMA and SPIMB subDirectories to use (ex. 1-8): ", " ");
        mainPanel.add(subDirectoryRangeText.getParent(), gbc);
        gbc.gridy++;
        
        resultDirectoryText = gui.buildFileField("Result output location:", initResultLoc, false, JFileChooser.DIRECTORIES_ONLY);
        mainPanel.add(resultDirectoryText.getParent(), gbc);
        gbc.gridy++;
        
        concurrentNumText = gui.buildIntegerField("Number of concurrent fusions: ", 
                (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime.getRuntime().availableProcessors()-2 : 1);
        mainPanel.add(concurrentNumText.getParent(), gbc);
        gbc.gridy++;
        
        okCancelPanel = gui.buildOKCancelPanel();
        mainPanel.add(okCancelPanel, gbc);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        
        getContentPane().setMaximumSize(new Dimension(700, 450));
        getContentPane().setPreferredSize(new Dimension(700, 450));
        
        pack();
        setVisible(true);
        setResizable(true);
        
        
        System.gc();
    }
    
    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
		String tmpStr;
		AFileDirectory = AFileDirectoryText.getText();
		AFileDark2D = AFileDark2DText.getText();
		
		BFileDirectory = BFileDirectoryText.getText();
		BFileDark2D = BFileDark2DText.getText();
		
		tmpStr = ALeftShiftText.getText();
		ALeftShift = Double.valueOf(tmpStr).doubleValue();
		if (ALeftShift < 0.0) {
			MipavUtil.displayError("A left shift cannot be less than zero");
			ALeftShiftText.requestFocus();
			ALeftShiftText.selectAll();
			return false;
		}
		
		String subDirectoryRange = subDirectoryRangeText.getText();
	    HashSet<Integer> includeSubDirectoryRange = new HashSet<Integer>();
	    if(subDirectoryRange != null) {  
	        String[] ranges = subDirectoryRange.split("[,;]");
	        for(int i=0; i<ranges.length; i++) {
	            String[] subset = ranges[i].split("-");
	            subDirectoryLowerBound = -1;
	            int bound = -1;
	            for(int j=0; j<subset.length; j++) {
	                try {
	                    bound = Integer.valueOf(subset[j].trim());
	                    if(subDirectoryLowerBound == -1) {
	                        subDirectoryLowerBound = bound;
	                        includeSubDirectoryRange.add(subDirectoryLowerBound);
	                    } 
	                } catch(NumberFormatException e) {
	                    Preferences.debug("Invalid subDirectory range specified: "+bound, Preferences.DEBUG_ALGORITHM);
	                }
	            }
	            
	            for(int k=subDirectoryLowerBound+1; k<=bound; k++) {
                    includeSubDirectoryRange.add(k);
                }
	        }
	    }
	    
	    
	    if(includeSubDirectoryRange.size() == 0) {
	        includeSubDirectoryRange = null;
	    }
	   
	    if(!populateASubDirectoryLists(includeSubDirectoryRange)) {
	        return false;
	    }
	    
	    if(!populateBSubDirectoryLists(includeSubDirectoryRange)) {
	        return false;
	    }
	    
	    if (ASubDirectoryAr.length != BSubDirectoryAr.length) {
	    	MipavUtil.displayError("Number of A subDirectories = " + ASubDirectoryAr.length + 
	    			" does not equal number of B subDirectories = " + BSubDirectoryAr.length);
	    	return false;
	    }
	    
	    subDirectoryNumber = ASubDirectoryAr.length;
	    AImageAr = new File[subDirectoryNumber][];
	    BImageAr = new File[subDirectoryNumber][];
		
		String ARange = ARangeText.getText();
	    HashSet<Integer> includeARange = new HashSet<Integer>();
	    if(ARange != null) {  
	        String[] ranges = ARange.split("[,;]");
	        for(int i=0; i<ranges.length; i++) {
	            String[] subset = ranges[i].split("-");
	            int lowerBound = -1, bound = -1;
	            for(int j=0; j<subset.length; j++) {
	                try {
	                    bound = Integer.valueOf(subset[j].trim());
	                    if(lowerBound == -1) {
	                        lowerBound = bound;
	                        includeARange.add(lowerBound);
	                    } 
	                } catch(NumberFormatException e) {
	                    Preferences.debug("Invalid A range specified: "+bound, Preferences.DEBUG_ALGORITHM);
	                }
	            }
	            
	            for(int k=lowerBound+1; k<=bound; k++) {
                    includeARange.add(k);
                }
	        }
	    }
	    
	    
	    if(includeARange.size() == 0) {
	        includeARange = null;
	    }
	   
	    if(!populateAFileLists(includeARange)) {
	        return false;
	    }
		
		tmpStr = BLeftShiftText.getText();
		BLeftShift = Double.valueOf(tmpStr).doubleValue();
		if (BLeftShift < 0.0) {
			MipavUtil.displayError("B left shift cannot be less than zero");
			BLeftShiftText.requestFocus();
			BLeftShiftText.selectAll();
			return false;
		}
		
		String BRange = BRangeText.getText();
	    HashSet<Integer> includeBRange = new HashSet<Integer>();
	    if(BRange != null) {  
	        String[] ranges = BRange.split("[,;]");
	        for(int i=0; i<ranges.length; i++) {
	            String[] subset = ranges[i].split("-");
	            int lowerBound = -1, bound = -1;
	            for(int j=0; j<subset.length; j++) {
	                try {
	                    bound = Integer.valueOf(subset[j].trim());
	                    if(lowerBound == -1) {
	                        lowerBound = bound;
	                        includeBRange.add(lowerBound);
	                    } 
	                } catch(NumberFormatException e) {
	                    Preferences.debug("Invalid B range specified: "+bound, Preferences.DEBUG_ALGORITHM);
	                }
	            }
	           
	            for(int k=lowerBound+1; k<=bound; k++) {
                    includeBRange.add(k);
                }
	        }
	    }
	    
	    
	    if(includeBRange.size() == 0) {
	        includeBRange = null;
	    }
	   
	    if(!populateBFileLists(includeBRange)) {
	        return false;
	    }
	    
	    if (AImageAr.length != BImageAr.length) {
	    	MipavUtil.displayError("Number of A slices = " + AImageAr.length + " does not equal number of B slices = " + BImageAr.length);
	    	return false;
	    }
	    
	    resultDirectoryString = resultDirectoryText.getText();
    	if((resultDirectory = createDirectory(resultDirectoryString)) == null) {
            return false;
        }
    	
    	concurrentNum = Integer.valueOf(concurrentNumText.getText()).intValue();
    	
		return true;
	}
	
	private boolean populateASubDirectoryLists(HashSet<Integer> includeRange) {
        ArrayList<File> ASubDirectoryList = new ArrayList<File>();
        try {
            File fA = new File(AFileDirectory);
            for(File fTry : fA.listFiles()) {
            	ASubDirectoryList.add(fTry);
            }
        } catch(Exception e) {
            MipavUtil.displayError("Invalid A directory");
            return false;
        }
        
        if(includeRange != null) {
            int originalSize = ASubDirectoryList.size();
            for(int i=originalSize; i>0; i--) {
                int index = getIndex(ASubDirectoryList.get(i-1));
                if(!includeRange.contains(index)) {
                    ASubDirectoryList.remove(i-1);
                }
            }
        }
        
        
        
        FileCompare f = new FileCompare();
        Collections.sort(ASubDirectoryList, f);
        
        ASubDirectoryAr = ASubDirectoryList.toArray(new File[ASubDirectoryList.size()]);
          
        return true;
    }
	
	private boolean populateBSubDirectoryLists(HashSet<Integer> includeRange) {
        ArrayList<File> BSubDirectoryList = new ArrayList<File>();
        try {
            File fB = new File(BFileDirectory);
            for(File fTry : fB.listFiles()) {
            	BSubDirectoryList.add(fTry);
            }
        } catch(Exception e) {
            MipavUtil.displayError("Invalid B directory");
            return false;
        }
        
        if(includeRange != null) {
            int originalSize = BSubDirectoryList.size();
            for(int i=originalSize; i>0; i--) {
                int index = getIndex(BSubDirectoryList.get(i-1));
                if(!includeRange.contains(index)) {
                    BSubDirectoryList.remove(i-1);
                }
            }
        }
        
        
        
        FileCompare f = new FileCompare();
        Collections.sort(BSubDirectoryList, f);
        
        BSubDirectoryAr = BSubDirectoryList.toArray(new File[BSubDirectoryList.size()]);
          
        return true;
    }
	
	private boolean populateAFileLists(HashSet<Integer> includeRange) {
		for (int j = 0; j < subDirectoryNumber; j++) {
	        ArrayList<File> AImageList = new ArrayList<File>();
	        try {
	            for(File fTry : ASubDirectoryAr[j].listFiles()) {
	            	AImageList.add(fTry);
	            }
	        } catch(Exception e) {
	            MipavUtil.displayError("Invalid A subDirectory");
	            return false;
	        }
	        
	        if(includeRange != null) {
	            int originalSize = AImageList.size();
	            for(int i=originalSize; i>0; i--) {
	                int index = getIndex(AImageList.get(i-1));
	                if(!includeRange.contains(index)) {
	                    AImageList.remove(i-1);
	                }
	            }
	        }
	        
	        
	        
	        FileCompare f = new FileCompare();
	        Collections.sort(AImageList, f);
	        
	        AImageAr[j] = AImageList.toArray(new File[AImageList.size()]);
		} // for (int j = 0; j < subDirectoryNumber; j++)
          
        return true;
    }
	
	private boolean populateBFileLists(HashSet<Integer> includeRange) {
		for (int j = 0; j < subDirectoryNumber; j++) {
	        ArrayList<File> BImageList = new ArrayList<File>();
	        try {
	            for(File fTry : BSubDirectoryAr[j].listFiles()) {
	            	BImageList.add(fTry);
	            }
	        } catch(Exception e) {
	            MipavUtil.displayError("Invalid B subDirectory");
	            return false;
	        }
	        
	        if(includeRange != null) {
	            int originalSize = BImageList.size();
	            for(int i=originalSize; i>0; i--) {
	                int index = getIndex(BImageList.get(i-1));
	                if(!includeRange.contains(index)) {
	                    BImageList.remove(i-1);
	                }
	            }
	        }
	        
	        
	        
	        FileCompare f = new FileCompare();
	        Collections.sort(BImageList, f);
	        
	        BImageAr[j] = BImageList.toArray(new File[BImageList.size()]);
		} // for (int j = 0; j < subDirectoryNumber; j++)
          
        return true;
    }
	
	 private int getIndex(File file) {
	        String name = file.getName();
	        int upper = -1, lower = -1;
	        boolean inRange = false;
	        for(int i=name.length(); i > 0; i--) {
	            if(Character.isDigit(name.charAt(i-1))) {
	                if(!inRange) {
	                    upper = i;
	                    lower = i;
	                    inRange = true;
	                } else {
	                    lower = i;
	                }
	            } else {
	                inRange = false;
	                if(upper != -1 && lower != -1) {
	                    break;
	                }
	            }
	        }
	        
	        try {
	            return Integer.valueOf(name.substring(lower-1, upper)).intValue();
	        } catch(Exception e) {
	            return -1;
	        }
	    }


	    private class FileCompare implements Comparator<File> {
	        public int compare(File arg0, File arg1) {
	            if(arg0.getName().length() != arg1.getName().length()) {
	                return arg0.getName().length() - arg1.getName().length();
	            }
	            
	            return arg0.getName().compareTo(arg1.getName());
	        }
	    }
	    
	    private File createDirectory(String location) {
	        File f = null;
	        try {
	            f = new File(location);
	            if(!f.exists()) {
	                f.mkdirs();
	            }
	        } catch(Exception e) {
	            MipavUtil.displayError("Error making directory "+location);
	        }
	        
	        return f;
	    }
    
    /**
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {
    	 AFileDirectory = scriptParameters.getParams().getString("AFileDirectory");
    	 AFileDark2D = scriptParameters.getParams().getString("AFileDark2D");
         ALeftShift = scriptParameters.getParams().getDouble("A_LEFT_SHIFT");
         BFileDirectory = scriptParameters.getParams().getString("BFileDirectory");
    	 BFileDark2D = scriptParameters.getParams().getString("BFileDark2D");
         BLeftShift = scriptParameters.getParams().getDouble("B_LEFT_SHIFT");
         subDirectoryLowerBound = scriptParameters.getParams().getInt("SUB_DIRECTORY_LOWER_BOUND");
         resultDirectoryString = scriptParameters.getParams().getString("resultDirectoryString");
         concurrentNum = scriptParameters.getParams().getInt("concurrent_num");
         populateASubDirectoryLists(null);
         populateBSubDirectoryLists(null);
         populateAFileLists(null);
         populateBFileLists(null);
    }
    
    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("AFileDirectory", AFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("AFileDark2D", AFileDark2D));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("A_LEFT_SHIFT", ALeftShift));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("BFileDirectory", BFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("BFileDark2D", BFileDark2D));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("B_LEFT_SHIFT", BLeftShift));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("SUB_DIRECTORY_LOWER_BOUND", subDirectoryLowerBound));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("resultDirectoryString", resultDirectoryString));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("concurrent_num", concurrentNum));
    }

}