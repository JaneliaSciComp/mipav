import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.BitSet;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;

/**
 * Dialog accompanying the neuron segmentation algorithm for the
 * Giniger lab. This is the generic version of the plugin that
 * allows for both standalone execution and calculating image
 * segmentations over an entire folder (recursively)
 * 
 * @author wangvg
 *
 */
public class PlugInDialogNeuronSegmentationGeneric extends
		JDialogStandalonePlugin implements AlgorithmInterface, ChangeListener, MouseListener {
	
	private static final long serialVersionUID = -829071275308963405L;

	private JRadioButton addRB;
	
	private JCheckBox centroidBox;
	
	private JRadioButton changeRB;
	
	private int changeX;
	
	private int changeY;
	
	private int counter;
	
	/**
	 * The current image that is open
	 */
	private File current;
	
	private JRadioButton deleteRB;
	
	private JTextField dirText;
	
	private JCheckBox excludeBox;
	
	private int[] extents;
	
	private JFileChooser fileChooser;	
	
	private ViewJFrameImage frame;
	
	private JCheckBox imageBox;
	
	/**
	 * List containing the images that need to be segmented 
	 * during this run through
	 */
	private ArrayList<File> images;
	
	/**
	 * Displayed list of images that can be used to jump to
	 * other images
	 */
	private JList list;
	
	private JDialog listDialog;
	
	private boolean listenersOn;
	
	private JButton jumpButton;

	private int numImages;
	
	private JCheckBox polygonalBox;
	
	private JCheckBox saveSkelBox;
	
	private JCheckBox saveVOIBox;
	
	private PlugInAlgorithmNeuronSegmentation seg;
	
	private ViewJFrameImage segFrame;
	
	private JCheckBox segImageBox;
	
	private JSlider sensSlider;
	
	private BitSet skeleton;
	
	private ModelImage srcImage;
	
	private JCheckBox tipBox;
	
	private JButton undoButton;
	
	private int width;
	
	public PlugInDialogNeuronSegmentationGeneric(){
		super();
		images = new ArrayList<File>();
		changeX = -1;
		changeY = -1;
		listenersOn = false;
		init();
	}
	
	public void actionPerformed(ActionEvent e){
		String command = e.getActionCommand();
		
		if (command.equals("ApproveSelection")){
			dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
		else if(command.equals("Cancel")){
			
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}
		else if(command.equals("Choose")) chooseDir();
		else if(command.equals("OK")){
			if(!populateImages(new File(dirText.getText()))){
        		MipavUtil.displayError("No images to segment");
        		chooseDir();
        	}
        	else{
        		counter = 0;
        		openImage();
        		initModifications();
        		callAlgorithm();
        	}
		}
		else if(command.equals("Prev")){
			if (counter > 0){
        		counter--;
        		Point loc = frame.getLocation();
        		frame.close();
        		openImage();
        		frame.setLocation(loc);
        		callAlgorithm();
        		if(segFrame != null){
        			segFrame.close();
        			segFrame = null;
        		}
        		segImageBox.setSelected(false);
        	}
		}
		else if (command.equals("Next")){
        	//Close the previous image and open the next one
        	counter++;
        	if(counter < numImages){
        		Point loc = frame.getLocation();
        		frame.close();
        		openImage();
        		frame.setLocation(loc);
        		callAlgorithm();
        		if(segFrame != null){
        			segFrame.close();
        			segFrame = null;
        		}
        		segImageBox.setSelected(false);
        	}
        	else finalize();
        }
		else if(command.equals("Undo") || command.equals("Redo"))
			undo();
		else if(command.equals("Save")){
			seg.save(saveVOIBox.isSelected(), saveSkelBox.isSelected());
			//seg.saveAsSWC();
		}
		else if(command.equals("End")){
        	finalize();
		}	
		else if(command.equals("Reset")){
			sensSlider.setValue(10);
			changeX = -1;
			changeY = -1;
			callAlgorithm();
		}
		else if(command.equals("Jump")){
			counter = list.getSelectedIndex();
			Point loc = frame.getLocation();
			frame.close();
			openImage();
			frame.setLocation(loc);
			callAlgorithm();
			if(segFrame != null){
				segFrame.close();
				segFrame = null;
			}
			segImageBox.setSelected(false);
		}
		else if(command.equals("CloseList")){
			listDialog.setVisible(false);
			imageBox.setSelected(false);
		}
		else{
			super.actionPerformed(e);
		}
	}
	
	/**
	 * Every time a new image is displayed and the segmentation
	 * routine run, set up the new frame to display everything
	 * correctly
	 */

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		skeleton = seg.getSkeleton();
		
		frame.getComponentImage().getImageA().resetVOIs();
		frame.getControls().getTools().setOpacity(1.0f);
		frame.getControls().getTools().setPaintColor(Color.WHITE);
		
		frame.setCursor(null);
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();

		//Add mouse listener so you can click to add/delete branches
		if(!listenersOn){
			frame.getComponentImage().addMouseListener(this);
			frame.addWindowListener(this);
			listenersOn = true;
		}
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();

	}
	
	public void finalize(){
		
		if (frame != null) frame.close();
		if (listDialog != null) listDialog.dispose();
    	images.clear();
    	srcImage.disposeLocal();
		seg.finalize();
		seg = null;
		skeleton = null;
		
		if (isExitRequired()) {
            System.exit(0);
            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
        	dispose();
        }
	}
	
	/**
	 * Method that is called every time a new image is displayed
	 * (so when Prev or Next is chosen). Initializes the new 
	 * segmentation and adjusts the dialog title to reflect
	 * which image we are currently on
	 */
	
	protected void callAlgorithm(){
		
		extents = srcImage.getExtents();
		if(extents.length > 2){
			actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Next"));
			return;
		}
		width = extents[0];
		
		//Run the initial segmentation on start-up so that
		//it is immediately displayed to the user
		seg = new PlugInAlgorithmNeuronSegmentation(srcImage);
		seg.setCoords(changeX, changeY);
		if(sensSlider == null){
			seg.setSensitivity(0.01f);
		}
		else{
			float sensitivity = 0.001f * (float)sensSlider.getValue();
	        if(sensitivity == 0) sensitivity = 1;
	        seg.setSensitivity(sensitivity);
		}
		
		createProgressBar("Segmenting Neuron", "Creating branches..." , seg);
		progressBar.setVisible(true);
		seg.addListener(this);
		
		if (isRunInSeparateThread()) {
			if (seg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			seg.run();
		}
		
		String title = "Neuron Segmentation " + String.valueOf(counter+1) + " of "
				+ String.valueOf(numImages);
        setTitle(title);
	}
	
	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.showOpenDialog(this);
	}
	
	/**
	 * Very minimal dialog that asks the user to choose
	 * which directory to search through to segment images
	 */
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("Neuron Segmentation");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image File/Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains neurons requiring segmentation<br>"
        		+ "<b>OR</b> choose a single image.</html>";

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
        
        JPanel checkPanel = new JPanel();
        checkPanel.setForeground(Color.black);
        
        excludeBox = new JCheckBox("Exclude images with previous segmentations");
        excludeBox.setFont(serif12);
        excludeBox.setSelected(true);
        checkPanel.add(excludeBox);
        
        PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(checkPanel);
        
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);

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
	 * Method to display the list of images to segment. This will
	 * allow the user to properly account for what images were 
	 * chosen in the given directory. An added feature of this list
	 * is to allow for the user to jump to whichever image they want
	 * in the list
	 */

	private void initList(){
		
		listDialog = new JDialog();
		listDialog.setForeground(Color.black);
		listDialog.addWindowListener(this);
		
		int listHeight;
		if(numImages >= 26) listHeight = 500;
		else if(numImages <= 5) listHeight = 125;
		else listHeight = numImages * 18 + 30;
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.LINE_START;
		
		numImages = images.size();
		String[] imList = new String[numImages];
		String dirString = dirText.getText();
		File dir = new File(dirString);
		if(dir.isDirectory()){
			if(!dirString.endsWith(File.separator))
				dirString += File.separator;
			String next;
			String out;
			for(int i=0;i<numImages;i++){
				next = images.get(i).toString();
				out = next.replace(dirString, "");
				imList[i] = out;
			}
		}
		else{
			imList[0] = images.get(0).getName();
		}
		
		list = new JList(imList);
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.setLayoutOrientation(JList.VERTICAL);
		list.setVisibleRowCount(0);
		list.setSelectedIndex(counter);
		
        JScrollPane listScroller = new JScrollPane(list);
        listScroller.setPreferredSize(new Dimension(250, listHeight));
        listScroller.setAlignmentX(LEFT_ALIGNMENT);
        listScroller.setFont(serif12);
        
        JPanel listPane = new JPanel(new GridBagLayout());
        listPane.add(listScroller, gbc);
        
        JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        
        jumpButton = new JButton("Jump to...");
        jumpButton.setFont(serif12);
        jumpButton.addActionListener(this);
        jumpButton.setActionCommand("Jump");
        
        JButton cancelListButton = new JButton("Close");
        cancelListButton.setFont(serif12);
        cancelListButton.addActionListener(this);
        cancelListButton.setActionCommand("CloseList");
        
        buttonPanel.add(jumpButton);
        buttonPanel.add(cancelListButton);
        list.addMouseListener(new MouseAdapter() {
	            public void mouseClicked(MouseEvent e) {
	                if (e.getClickCount() == 2) {
	                    jumpButton.doClick(); //emulate button click
	                }
	            }
	        });
        listDialog.setTitle("Image List");
        listDialog.add(listScroller, BorderLayout.CENTER);
        listDialog.add(buttonPanel, BorderLayout.SOUTH);
        listDialog.pack();
        //listDialog.setVisible(true);

	}
	
	/**
	 * Initiates the dialog screen for the actual modifications.
	 * This is the exact same as the algorithm plugin with the
	 * exception of the Prev and Next buttons to allow navigation
	 * through the selected images.
	 */
	
	private void initModifications(){
		
		getContentPane().removeAll();
		setForeground(Color.black);
        
        JPanel descPanel = new JPanel();
        descPanel.setForeground(Color.black);
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Choose either add or delete, and then click on the image<br>"
        		+ "to modify the branches.<br>"
        		+ "Choose \"Change Location\" to change where the neuron is <br>"
        		+ "believed to be. <br>"
        		+ "Change sensitivity to change original segmentation.<br><br>"
        		+ "<b>NOTE:</b> Changing sensitivity or location resets any <br>"
        		+ "branches added or deleted previously.</html>";
        
        JLabel descLabel = new JLabel(desc);
        descLabel.setForeground(Color.black);
        descLabel.setFont(serif12);
        descPanel.add(descLabel);
        
        JPanel radioPanel = new JPanel();
        radioPanel.setForeground(Color.black);
        
        addRB = new JRadioButton("Add");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        
        deleteRB = new JRadioButton("Delete");
        deleteRB.setFont(serif12);

        changeRB = new JRadioButton("Change location");
        changeRB.setFont(serif12);
        
        ButtonGroup group = new ButtonGroup();
        
        group.add(addRB);
        group.add(deleteRB);
        group.add(changeRB);
        radioPanel.add(addRB);
        radioPanel.add(deleteRB);
        radioPanel.add(changeRB);
        
        JPanel titlePanel = new JPanel();
        titlePanel.setForeground(Color.black);
        
        JLabel slideLabel = new JLabel("Sensitivity", JLabel.CENTER);
        slideLabel.setFont(serif12B);
        titlePanel.add(slideLabel);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setForeground(Color.black);
        
        sensSlider = new JSlider(JSlider.HORIZONTAL, 0, 30, 10);
        sensSlider.addChangeListener(this);
        sensSlider.setMajorTickSpacing(5);
        sensSlider.setMinorTickSpacing(1);
        sensSlider.setFont(serif12);
        sensSlider.setPaintTicks(true);
        sensSlider.setPaintLabels(true);
        sliderPanel.add(sensSlider);
        
        JPanel checkPanel = new JPanel(new GridLayout(2,2));
        checkPanel.setForeground(Color.black);
        checkPanel.setBorder(buildTitledBorder("Display Options"));
        
        centroidBox = new JCheckBox("Centroid");
        centroidBox.setFont(serif12);
        centroidBox.addItemListener(this);
        checkPanel.add(centroidBox);
        
        tipBox = new JCheckBox("Branch tips");
        tipBox.setFont(serif12);
        tipBox.addItemListener(this);
        checkPanel.add(tipBox);
        
        polygonalBox = new JCheckBox("Polygonal area");
        polygonalBox.setFont(serif12);
        polygonalBox.addItemListener(this);
        checkPanel.add(polygonalBox);
        
        segImageBox = new JCheckBox("Initial segmentation");
        segImageBox.setFont(serif12);
        segImageBox.addItemListener(this);
        checkPanel.add(segImageBox);
        
        JPanel optionsPanel = new JPanel(new GridLayout(0, 2));
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Misc. Options"));

        saveSkelBox = new JCheckBox("Save branches as TIFF");
        saveSkelBox.setFont(serif12);
        saveSkelBox.setSelected(true);
        optionsPanel.add(saveSkelBox);
        
        saveVOIBox = new JCheckBox("Save VOIs");
        saveVOIBox.setFont(serif12);
        optionsPanel.add(saveVOIBox);
        
        imageBox = new JCheckBox("Show image list");
        imageBox.setFont(serif12);
        imageBox.addItemListener(this);
        optionsPanel.add(imageBox);
        
        JPanel boxPanel = new JPanel(new GridLayout(1,2));
        boxPanel.setForeground(Color.black);
        
        JButton prevButton = new JButton("Prev");
        prevButton.setFont(serif12);
        prevButton.addActionListener(this);
        boxPanel.add(prevButton);
        
        JButton nextButton = new JButton("Next");
        nextButton.setFont(serif12);
        nextButton.addActionListener(this);
        boxPanel.add(nextButton);
        
        
        PanelManager manage = new PanelManager();
        manage.add(radioPanel);
        manage.addOnNextLine(titlePanel);
        manage.addOnNextLine(sliderPanel);
        manage.addOnNextLine(checkPanel);
        manage.addOnNextLine(optionsPanel);
        manage.addOnNextLine(boxPanel);
        
        getContentPane().add(descPanel, BorderLayout.NORTH);
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);
        
        JPanel buttonPanel = new JPanel(new GridLayout(1,4));
        buttonPanel.setForeground(Color.black);
        
        JButton resetButton = new JButton("Reset");
        resetButton.setFont(serif12);
        resetButton.addActionListener(this);
        buttonPanel.add(resetButton);
        
        JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);
        
        undoButton = new JButton("Undo");
        undoButton.setFont(serif12);
        undoButton.addActionListener(this);
        buttonPanel.add(undoButton);
        
        JButton endButton = new JButton("End");
        endButton.setFont(serif12);
        endButton.addActionListener(this);
        buttonPanel.add(endButton);
        
        
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
		
	}

	private void openImage() {
	
		FileIO imLoader = new FileIO();
		
		if(srcImage != null) srcImage.disposeLocal();
		if(listDialog != null) list.setSelectedIndex(counter);
		
		current = images.get(counter);
		
		srcImage = imLoader.readImage(current.toString());
		srcImage.setImageName(current.getName(),false);
		frame = new ViewJFrameImage(srcImage, null, new Dimension(0,300));
		frame.setVisible(true);
		listenersOn = false;

	}
	
	/**
	 * Method used to determine which files in the chosen
	 * directory to add, which are then added to the
	 * list <code>images</code>. A filename filter is used
	 * to choose images with filetype tif or lsm. This 
	 * method also works recursively to add candidate images
	 * in folder within the main directory.
	 * 
	 * @param dir the directory to search through for files
	 * @return true if the list is non-empty
	 */
	
	private boolean populateImages(File dir){
		if(dir.isFile()){
			images.add(dir);
			numImages = 1;
			return true;
		}
		
		File skelName;
		String stripped;
		
		FilenameFilter imFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff")
						|| name.toLowerCase().endsWith(".lsm"));
			}
		};
		File[] files = dir.listFiles(imFilter);
		String dirStr = dir.toString();
		if(!dirStr.endsWith(File.separator))
			dirStr = dirStr + File.separator;
		dirStr = dirStr.concat("Branch_Images" + File.separator);
		
		for(File im : files){
			if(excludeBox.isSelected()){
				stripped = im.getName();
				stripped = stripped.substring(0, stripped.indexOf("."));
				stripped = stripped.concat("_branches.swc");
				skelName = new File(dirStr.concat(stripped));
				if(!skelName.exists()){
					images.add(im);
				}
			}
			else{
				images.add(im);
			}
		}
		
		File[] directories = dir.listFiles(new FileFilter() {
			public boolean accept(File path) {
				return (path.isDirectory() && !path.getName().equals("Branch_Images"));
			}
		});
		
		for(int i=0;i<directories.length;i++){
			populateImages(directories[i]);
		}
		
		numImages = images.size();
		return !images.isEmpty();
	}

	/**
	 * Very basic undo functionality is provided in the algorithm.
	 * This could also easily be implemented as only part of the
	 * dialog, which may be more useful for multi-undo/redo.
	 * 
	 * Also allows for redo.
	 */
	
	private void undo(){
		
		seg.undo();
		skeleton = seg.getSkeleton();
		if(skeleton == null){
			MipavUtil.displayError("No modification to undo");
			return;
		}
		
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();
		if(undoButton.getText().equals("Undo"))
			undoButton.setText("Redo");
		else undoButton.setText("Undo");
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();
		
		
	}
	
	/**
	 * Toggles the various VOIs describing the
	 * neuron branches on or off
	 */
	
	public void itemStateChanged(ItemEvent e){
		
		Object source = e.getItemSelectable();
		
		if(source == tipBox){
			if(tipBox.isSelected()){
				seg.displayTips();
			}
			else seg.removeTips();
		}
		else if(source == centroidBox){
			if(centroidBox.isSelected()){
				seg.displayCentroid();
			}
			else seg.removeCentroid();
		}
		else if(source == polygonalBox){
			if(polygonalBox.isSelected()){
				seg.displayPolygonal();
			}
			else seg.removePolygonal();
		}
		else if(source == segImageBox){
			if(segImageBox.isSelected()){
				if(segFrame == null){
					ModelImage segImage = seg.getSegImage();
					segFrame = new ViewJFrameImage(segImage);
					segFrame.addWindowListener(this);
				}
				segFrame.setVisible(true);
			}
			else{
				if(segFrame != null){
					segFrame.setVisible(false);
				}
			}
		}
		else if(source == imageBox){
			if(imageBox.isSelected()){
				if(listDialog == null)
					initList();
				listDialog.setVisible(true);
			}
			else{
				if(listDialog != null)
					listDialog.setVisible(false);
			}
		}
		
	}
	
	/**
	 * When the mouse is clicked on the image (ViewJComponentEditImage),
	 * branches are either added or deleted based on which
	 * radio button is currently selected.
	 */

	@Override
	public void mouseClicked(MouseEvent e) {
		
		float zoomX = frame.getComponentImage().getZoomX();
		float zoomY = frame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX); //- left;
		int y = (int) ((float)e.getY()/zoomY); //- top;

		int i = x + y*width;
		if(changeRB.isSelected()){
			changeX = x;
			changeY = y;
			callAlgorithm();
		}
		else {
			if(addRB.isSelected()) seg.addBranches(i);
			else seg.deleteBranches(i);
			
			skeleton = seg.getSkeleton();
			frame.getComponentImage().setPaintMask(skeleton);
			frame.updateImages();
			
			if(tipBox.isSelected()) seg.displayTips();
			if(centroidBox.isSelected()) seg.displayCentroid();
			if(polygonalBox.isSelected()) seg.displayPolygonal();
			
			undoButton.setText("Undo");
		}
		
	}
	
	//We only care about the click for determining
	//where to see adds or deletions

	@Override
	public void mousePressed(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// Do nothing
		
	}
	
	//Make sure if image window closes, so does the dialog.
	@Override
	public void windowClosing(WindowEvent event) {

		Object source = event.getSource();
		if(source == segFrame){
			segFrame = null;
			segImageBox.setSelected(false);
		}
		else if(source == frame){
			cancelFlag = true;
			images.clear();
			seg.finalize();
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        	if(listDialog != null) 
	        		listDialog.dispose();
	        }
		}
		else if(source == listDialog){
			imageBox.setSelected(false);
		}
		else{
			cancelFlag = true;
			if(seg == null){
				if (isExitRequired()) {
		            System.exit(0);
		            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		        } else {
		        	dispose();
		        	if(listDialog != null) 
		        		listDialog.dispose();
		        }
			}
			else finalize();
		}
        
    }
	
	/**
	 * Allow the user to change how sensitive the initial segmentation
	 * is. This essentially reruns the initial segmentation step, but
	 * with a different threshold to build the skeleton.
	 */

	@Override
	public void stateChanged(ChangeEvent e) {

		JSlider source = (JSlider)e.getSource();
	    if (!source.getValueIsAdjusting()) {
	    	callAlgorithm();
	        /*float sensitivity = 0.001f * (float)sensSlider.getValue();
	        if(sensitivity == 0) sensitivity = 1;
	        seg.setSensitivity(sensitivity);
	        createProgressBar("Segmenting Neuron", "Creating branches..." , seg);
			progressBar.setVisible(true);
			seg.runAlgorithm();
			
			skeleton = seg.getSkeleton();
			frame.getComponentImage().setPaintMask(skeleton);
			frame.updateImages();
			
			if(tipBox.isSelected()) seg.displayTips();
			if(centroidBox.isSelected()) seg.displayCentroid();
			if(polygonalBox.isSelected()) seg.displayPolygonal();*/
	        
	    }
	}
}
