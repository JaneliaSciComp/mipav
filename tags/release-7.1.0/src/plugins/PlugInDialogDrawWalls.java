import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOILogicalOperations;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMask;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDrawWalls extends JDialogStandalonePlugin implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6523705694712956431L;

	private int counter = 0;
	
	/** File of current image open*/
	private File current;
	
	private JTextField dirText;
	
	private ViewJFrameImage display;
	
	private JFileChooser fileChooser;
	
	/**Contains the wall images that require masks	 */
	private Vector<File> imageList;
	
	private int listLength;
	
	private File maskDir;
	
	private Vector<File> maskList;
	
	private ModelImage rgbImage;
	
	private JRadioButton voiRB;
	
	private JRadioButton paintRB;
	
	private JCheckBox displayMask;
	
	private JCheckBox calcStats;
	
	private PlugInAlgorithmWallNucleiStatsBulk calcAlg;
	
	private JComboBox resUnits;
	
	private JPanel resPanel;
	
	private JTextField xResField;
	
	private File csv;
	
	private FileWriter output;
	
	private JCheckBox nucleiCB;
	
	private JCheckBox wallCB;
	
	private String separator = File.separator;
	
	
	/**
	 * Plugin for the Collin's lab. This accompanies the Wall and Nuclei Statistics plugin
	 * (PlugInWallNucleiStatsBulk). By running this, the user can get the masks required
	 * for the statistic plugin. The plugin is mostly dialogs and user interaction; there
	 * is no true algorithm involved. VOIs drawn on the image are converted to a mask.
	 * 
	 * @author Victor Wang
	 */
	
	public PlugInDialogDrawWalls(){
		
		super();

        imageList = new Vector<File>();
        init();
	}
	
	public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        //Checks to see if no path was provided to the plugin
        boolean changed = !dirText.getText().equals("Path to Directory with Slices");

        if (command.equals("Choose")) openDir();
        else if (command.equals("calc")){
        	if (calcStats.isSelected()) {
        		xResField.setEnabled(true);
        		resUnits.setEnabled(true);
        		nucleiCB.setEnabled(true);
                wallCB.setEnabled(true);
        	}
        	else{
        		xResField.setEnabled(false);
        		resUnits.setEnabled(false);
        		nucleiCB.setEnabled(false);
                wallCB.setEnabled(false);
        	}
        		
        }
        else if (command.equals("ApproveSelection")){
        	//Changes the text field to match the directory selected by the user
        	dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
        else if (command.equals("Prev")){
        	if (counter > 0){
        		counter--;
        		display.close();
        		openImage();
        		String title = "Wall Drawing " + String.valueOf(counter+1) + " of "
        				+ String.valueOf(listLength);
                setTitle(title);
        	}
        	
        }
        else if (command.equals("Next")){
        	//Close the previous image and open the next one
        	counter++;
        	if(counter < listLength){
        		display.close();
        		openImage();
        		String title = "Wall Drawing " + String.valueOf(counter+1) + " of "
        				+ String.valueOf(listLength);
                setTitle(title);
        	}
        	else command = "End";
        }
        else if  (command.equals("OK")){
        	if (changed){
        		//If there are no images, tell the user
	        	if(populateImages(new File(dirText.getText()))) {
	        		openImage();
	        		initDrawer();
	        	}
	        	else 
	        		MipavUtil.displayError("This folder does not contain any images requiring masks");
	        	}
        	else MipavUtil.displayError("Please select a directory.");
        }
        else if (command.equals("Save")) saveMask();
        else super.actionPerformed(event);
        
        //This is at the end so that if "Next" is pushed when there are no images
        //left, it falls to this case
        if(command.equals("End") && calcStats.isSelected()){
        	if (display != null) display.close();
        	imageList.clear();
        	rgbImage = null;
        	if(statsFilter()){
        		calcAlg = new PlugInAlgorithmWallNucleiStatsBulk(imageList, maskList);
        		calcAlg.addListener(this);
        		calcAlg.run();
        	}
        	
        }
        if(command.equals("Cancel")|| (command.equals("End") && !calcStats.isSelected())) {
        	if (display != null) display.close();
        	imageList.clear();
        	rgbImage = null;
        	//dispose();
        	if (isExitRequired()) {
                System.exit(0);
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
            	dispose();
            }
        }
    }

	/**
	 * No algorithm performed, thus nothing to do in that event
	 */
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		//Nothing to do
	}
	
	/**
	 * Opens the next image in the list and displays it for the user to draw VOIs on
	 */
	
	protected void callAlgorithm(){
		try{
			int xRes;
			boolean wall, nuclei;
			
			xRes = Integer.valueOf(xResField.getText());
			wall = wallCB.isSelected();
			nuclei = nucleiCB.isSelected();
			
			calcAlg = new PlugInAlgorithmWallNucleiStatsBulk(imageList, maskList);
			
			calcAlg.setRes(xRes);
			calcAlg.setCSV(output);
			calcAlg.showWall(wall);
			calcAlg.showNuc(nuclei);

			//Progress bar doesn't update smoothly with algorithms
			createProgressBar("Wall/Nuclei statistics calculation", "Initializing..." , calcAlg);
			calcAlg.addListener(this);
			setVisible(false);

			if (isRunInSeparateThread()) {
				if (calcAlg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				calcAlg.run();
			}
			
		}
		catch (OutOfMemoryError x){
			MipavUtil.displayError("Unable to allocate enough memory");
            return;
		}
	}
	
	private void openImage() {
		
		FileIO imLoader = new FileIO();
		
		current = imageList.get(counter);
		rgbImage = imLoader.readImage(current.toString());
		rgbImage.setImageName(current.getName(),false);
		display = new ViewJFrameImage(rgbImage, null, new Dimension(0,300));
		display.setVisible(true);
	}
	
	/**
	 * Method used to populate a vector of wall images that will be used for the algorithm.
	 * Images that already have masks are not included in the list.
	 * 
	 * @return returns false if there are no images to populate the vectors
	 */
	
	private boolean fileFilter(){
		
		File dir = new File(dirText.getText());
		File mask;
		String stripped;
		String dirStr;
		//Populate the mask images first, then use that to get the wall images
		//so that only masked images are processed
		
		dirStr = dirText.getText();
		if(!dirStr.endsWith(separator))
			dirStr = dirStr + separator;
		dirStr = dirStr.concat("Masks" + separator);
		maskDir = new File(dirStr);
		if(!maskDir.exists()) maskDir.mkdir();
		
		FilenameFilter jpgFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".jpg") || name.toLowerCase().endsWith(".jpeg"));
			}
		};
		
		File[] files = dir.listFiles(jpgFilter);
		
		File[] directories = dir.listFiles(new FileFilter() {
			public boolean accept(File path) {
				return path.isDirectory();
			}
		});
		
		for(int i=0;i<directories.length;i++){
			File[] dirFiles = directories[i].listFiles(jpgFilter);
			for(File im : dirFiles){
				stripped = im.getName();
				stripped = stripped.substring(0, stripped.indexOf("."));
				stripped = stripped.concat("_mask.xml");
				mask = new File(dirStr.concat(stripped));
				
		        if (!mask.exists()){
		        	imageList.add(im);
		        }
			}
		}
		
		for(File im : files){
			stripped = im.getName();
			stripped = stripped.substring(0, stripped.indexOf("."));
			stripped = stripped.concat("_mask.xml");
			mask = new File(dirStr.concat(stripped));
			
	        if (!mask.exists()){
	        	imageList.add(im);
	        }
		}
		
		listLength = imageList.size();
		return !imageList.isEmpty();
	}
	
	private boolean populateImages(File dir){
		
		FilenameFilter jpgFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".jpg") || name.toLowerCase().endsWith(".jpeg")||
						name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff"));
			}
		};
		File mask;
		String stripped;
		File[] files = dir.listFiles(jpgFilter);
		String dirStr;
		//Populate the mask images first, then use that to get the wall images
		//so that only masked images are processed
		
		dirStr = dir.toString();
		if(!dirStr.endsWith(separator))
			dirStr = dirStr + separator;
		dirStr = dirStr.concat("Masks" + separator);
		maskDir = new File(dirStr);
		if(!maskDir.exists() && files.length>0) maskDir.mkdir();
		
		for(File im : files){
			stripped = im.getName();
			stripped = stripped.substring(0, stripped.indexOf("."));
			stripped = stripped.concat("_mask.xml");
			mask = new File(dirStr.concat(stripped));
			
	        if (!mask.exists()){
	        	imageList.add(im);
	        }
		}
		
		File[] directories = dir.listFiles(new FileFilter() {
			public boolean accept(File path) {
				return path.isDirectory();
			}
		});
		
		for(int i=0;i<directories.length;i++){
			populateImages(directories[i]);
		}
		
		listLength = imageList.size();
		return !imageList.isEmpty();
	}
	
	private boolean statsFilter(){
		
		File dir = new File(dirText.getText());
		File maskDir;
        imageList = new Vector<File>();
        maskList = new Vector<File>();
		File mask;
		String stripped;
		String dirStr;
		
		dirStr = dirText.getText();
		if(!dirStr.endsWith(separator))
			dirStr = dirStr + separator;
		dirStr = dirStr.concat("Masks" + separator);
		maskDir = new File(dirStr);
		if(!maskDir.exists()) return false;
		
		//Filter out only .jpg/.jpeg images
		File[] files = dir.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".jpg") || name.toLowerCase().endsWith(".jpeg") ||
						name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff"));
			}
		});
		
		//Pair each .jpg image with its associated mask
		for(File im : files){
			stripped = im.getName();
			stripped = stripped.substring(0, stripped.indexOf("."));
			stripped = stripped.concat("_mask.xml");
			stripped = dirStr.concat(stripped);
			mask = new File(stripped);

	        if (mask.exists()){
	        	imageList.add(im);
	        	maskList.add(new File(stripped));
	        }
		}
		
		return !imageList.isEmpty();
	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("Wall Drawing");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains wall sections that require binary masks.</html>";

        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText(Preferences.getImageDirectory());
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);
        
        getContentPane().add(choosePanel, BorderLayout.CENTER);

        JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
	}
	
	/**
	 * Used for setting up and displaying the File Manager for the plugin. Since
	 * we are only interested in multiple files at a time, the user can only choose
	 * directories to open.
	 */
	
	private void openDir(){

		fileChooser = new JFileChooser(dirText.getText());
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.addActionListener(this);
		fileChooser.showOpenDialog(this);
	}
	
	private boolean initCSV(){
		
		String directory = imageList.get(0).getParent();
		csv = new File(directory.concat(separator + "statistics.csv"));
		String units;
		Unit unitStruct;
		String unitAbbr;

		units = resUnits.getSelectedItem().toString();
		unitStruct = Unit.getUnit(units);
		unitAbbr = unitStruct.getAbbrev();
	
		try{
			if(!(csv.exists())){
				output = new FileWriter(csv);
				output.append("Image, ,Wall, , , , , ,Nuclei, ,Units, " + unitAbbr + ", \n");
				output.append(",Min,Max,Average,Std. Dev,Area, ,Number,Min,Max,Average,Std. Dev,Area, \n");
				output.flush();
			}
			else output = new FileWriter(csv, true);
		} catch (IOException x){
			MipavUtil.displayError("Unable to initialize csv file");
	        return false;
		}
		return true;
	}
	
	/**
	 * Initalizes the dialog for when the user needs to draw the VOIS on the image.
	 * Provides instruction and buttons to save/move on to the next image, or even
	 * end the drawing early if they so choose. Once there are no more images left,
	 * the "Next" button acts the same as the "End" button.
	 */
	
	private void initDrawer(){
		
		getContentPane().removeAll();
		
		setForeground(Color.black);
		
		String title = "Wall Drawing " + String.valueOf(counter+1) + " of "
				+ String.valueOf(listLength);
        setTitle(title);
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please use the VOI options to denote the wall sections. One VOI should outline<br>"
        		+ "the outer portion of the wall. Another VOI should outline the inner portion of<br>"
        		+ "the wall. If the wall leaves the image, use a single VOI to outline the inner<br>"
        		+ "and outer portions of the wall. Alternatively, you can use a paint brush to<br>"
        		+ "shade the entire wall area. When done, you can save the outlines and the proceed<br> "
        		+ "to the next image. </html>";
        		
        		/*+ "Please use the VOI options to denote the wall sections. The first VOI should<br>"
        		+ "outline the outer portion of the wall. The second VOI should outline the inner<br>"
        		+ "portion of the wall. When done, you can save the outlines and the proceed to <br>"
        		+ "the next image. </html>";*/
        
        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        getContentPane().add(dirLabel, BorderLayout.NORTH);
        
        voiRB = new JRadioButton("Mask from VOI");
        voiRB.setFont(serif12);
        voiRB.setActionCommand("voi");
        voiRB.setSelected(true);
        
        paintRB = new JRadioButton("Mask from Paint");
        paintRB.setFont(serif12);
        paintRB.setActionCommand("paint");
        
        ButtonGroup group = new ButtonGroup();
        
        group.add(voiRB);
        group.add(paintRB);;
        
        JPanel radioPanel = new JPanel();
        radioPanel.add(voiRB);
        radioPanel.add(paintRB);
        
        JPanel maskPanel = new JPanel();
        maskPanel.setForeground(Color.black);

        displayMask = new JCheckBox("Display Mask once saved", false);
        displayMask.setFont(serif12);
        maskPanel.add(displayMask);
        
        calcStats = new JCheckBox("Calculate Statistics at End", false);
        calcStats.setFont(serif12);
        maskPanel.add(calcStats);
        calcStats.addActionListener(this);
        calcStats.setActionCommand("calc");
        
        resPanel = new JPanel();
        resPanel.setForeground(Color.black);
        resPanel.setBorder(buildTitledBorder("Image Resolutions"));
        
        JLabel xRes = new JLabel("Resolution: ");
        xRes.setForeground(Color.black);
        xRes.setFont(serif12);
        resPanel.add(xRes);
        
        xResField = new JTextField(5);
        xResField.setText("1");
        xResField.setHorizontalAlignment(JTextField.RIGHT);
        xResField.setFont(serif12);
        resPanel.add(xResField);

        Unit[] allSame = UnitType.getUnitsOfType(UnitType.LENGTH);
        int[] allSameMeasure = new int[allSame.length]; 
        for(int i=0; i<allSameMeasure.length; i++) {
            allSameMeasure[i] = allSame[i].getLegacyNum();
        }
        String[] unitArr = new String[allSameMeasure.length];
        for(int i=0; i<allSameMeasure.length; i++) {
        	unitArr[i] = (Unit.getUnitFromLegacyNum(allSameMeasure[i])).toString();
        }
        resUnits = new JComboBox(unitArr);
        resUnits.setFont(serif12);
        resUnits.setSelectedItem("Micrometers");
        resPanel.add(resUnits);
        xResField.setEnabled(false);
        resUnits.setEnabled(false);
        
        JPanel optionsPanel = new JPanel();
        optionsPanel.setForeground(Color.black);
        
        nucleiCB = new JCheckBox("Display nuclei segmentation", false);
        nucleiCB.setFont(serif12);
        wallCB = new JCheckBox("Display midline image", false);
        wallCB.setFont(serif12);
        
        optionsPanel.add(nucleiCB);
        optionsPanel.add(wallCB);
        nucleiCB.setEnabled(false);
        wallCB.setEnabled(false);
        
        PanelManager manager = new PanelManager();
        manager.add(radioPanel);
        manager.addOnNextLine(maskPanel);
        manager.addOnNextLine(resPanel);
        manager.addOnNextLine(optionsPanel);
        
        
        getContentPane().add(manager.getPanel(), BorderLayout.CENTER);
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        
        JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);
        
        JButton prevButton = new JButton("Prev");
        prevButton.setFont(serif12);
        prevButton.addActionListener(this);
        buttonPanel.add(prevButton);
        
        JButton nextButton = new JButton("Next");
        nextButton.setFont(serif12);
        nextButton.addActionListener(this);
        buttonPanel.add(nextButton);
        
        JButton endButton = new JButton("End");
        endButton.setFont(serif12);
        endButton.addActionListener(this);
        buttonPanel.add(endButton);
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        getContentPane();
        setLocation(display.getFrameWidth(), 500);
        setResizable(false);
        System.gc();
	}
	
	/**
	 * Method that converts the VOIs from the displayed image into a mask using an XOR operation
	 * and then saved to disk in the form of IMAGENAME_mask.xml into the same directory.
	 * 
	 * This mask will then be used in the statistics calculations
	 */
	
	private void saveMask(){
		
		FileIO imWriter; //Used to write the binary mask to disk
		FileWriteOptions opt; //Points to the place to save the masks
		ModelImage outImage; //Mask image resulting from the XOR operation
		ModelImage cloned; //Cloned version of the wall image for the XOR operation
		String destName; //Directory and file paths
		boolean voiChecked;
		VOIVector vois;
		
		
		
		voiChecked = voiRB.isSelected();
		//paintChecked = paintRB.isSelected();
		
		/*if(!(voiChecked ^ paintChecked)) {
			MipavUtil.displayError("You must select one checkbox");
			return;
		}*/
		
		cloned = (ModelImage) rgbImage.clone();
		
		if(voiChecked){
			vois = rgbImage.getVOIs();
			if(vois == null){
				MipavUtil.displayError("No VOI was drawn. Please draw the wall boundaries.");
				return;
			}
			//Requires clone, or else errors occur
			AlgorithmVOILogicalOperations xor = 
					new AlgorithmVOILogicalOperations(cloned, vois, AlgorithmVOILogicalOperations.XOR, false);
			xor.run();
			outImage = xor.getFinalMaskImage();
		}
		else{
			BitSet maskBit =  display.getComponentImage().getPaintMask();
			if (maskBit.length()==0) {
				MipavUtil.displayError("No paint region was drawn. Please paint the wall area.");
				return;
			}
			Color paintColor = display.getControls().getTools().getPaintColor();;
			AlgorithmMask maskAlg = new AlgorithmMask(cloned, paintColor, true, false);
			maskAlg.calcInPlace25DCMask((BitSet)maskBit.clone(), paintColor, 0);
			int[] extents = rgbImage.getExtents();
			int length = extents[0]*extents[1];
			boolean[] maskBuffer = new boolean[length];
			int[] imBuffer = new int[4*length];
			outImage = new ModelImage(ModelImage.BOOLEAN, extents, "Mask Image");
			try {
				cloned.exportData(0, 4*length, imBuffer);
				for(int i=0;i<length;i++){
					if(imBuffer[4*i+1] != 0 || imBuffer[4*i+2] !=0 || imBuffer[4*i+3] !=0){
						maskBuffer[i] = true;
					}
				}
				outImage.importData(0, maskBuffer, true);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if(displayMask.isSelected())
			new ViewJFrameImage(outImage, null, new Dimension(50, 300));
		destName = current.getName();
		destName = destName.substring(0, destName.indexOf(".")).concat("_mask.xml");
		opt = new FileWriteOptions(destName, maskDir.toString().concat(separator), true);
		imWriter = new FileIO();
		imWriter.writeImage(outImage, opt, false);
	}
}
