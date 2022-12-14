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
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;

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
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
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
		JDialogStandalonePlugin implements AlgorithmInterface, ChangeListener, MouseListener, MouseMotionListener {
	
	private static final long serialVersionUID = -829071275308963405L;

	private JRadioButton addRB;
	
	private JCheckBox centroidBox;
	
	private JRadioButton changeRB;
	
	private int changeX;
	
	private int changeY;
	
	private int counter;
	
	private JRadioButton deleteRB;
	
	private JTextField dirText;
	
	private JCheckBox editBox;

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
	@SuppressWarnings("rawtypes")
	private JList list;
	
	private JDialog listDialog;
	
	private boolean listenersOn;
	
	private JButton jumpButton;
	
	private int numImages;
	
	private JRadioButton offRB;
	
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
	
	private float zX = 1;
	
	private float zY = 1;
	
	private JCheckBox maskBox;
	
	private ModelImage imStack = null;
	
	private Point loc;
	
	private Dimension dimSize;
	
	private Point intermediatePt;
	
	private ArrayList<Point> segmentList;
	
	public PlugInDialogNeuronSegmentationGeneric(){
		super();
		images = new ArrayList<File>();
		changeX = -1;
		changeY = -1;
		listenersOn = false;
		segmentList = new ArrayList<Point>();
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
        		if(openImage()){
	        		initModifications();
	        		callAlgorithm();
        		} else images.clear();
        	}
		}
		else if(command.equals("Prev")){
			if (counter > 0){
				zX = frame.getComponentImage().getZoomX();
				zY = frame.getComponentImage().getZoomY();
        		counter--;
        		loc = frame.getLocation();
        		dimSize = frame.getSize();
        		frame.close();
        		seg.cleanImages();
        		if(openImage()){
	        		callAlgorithm();
	        		if(segFrame != null){
	        			segFrame.close();
	        			segFrame = null;
	        		}
	        		segImageBox.setSelected(false);
        		}
        	}
		}
		else if (command.equals("Next")){
        	//Close the previous image and open the next one
        	counter++;
        	if(counter < numImages){
        		zX = frame.getComponentImage().getZoomX();
				zY = frame.getComponentImage().getZoomY();
        		loc = frame.getLocation();
        		dimSize = frame.getSize();
        		frame.close();
        		seg.cleanImages();
        		if(openImage()){
	        		callAlgorithm();
	        		if(segFrame != null){
	        			segFrame.close();
	        			segFrame = null;
	        		}
	        		segImageBox.setSelected(false);
        		}
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
			Point loc = frame.getLocation();
			sensSlider.setValue(20);
			frame.close();
    		seg.cleanImages();
    		if(openImage()){
        		frame.setLocation(loc);
        		callAlgorithm();
        		if(segFrame != null){
        			segFrame.close();
        			segFrame = null;
        		}
        		segImageBox.setSelected(false);
    		}
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
		
		//File current = images.get(counter);
		//srcImage.setImageName(current.getName(),false);
		if(loc == null)
			frame = new ViewJFrameImage(srcImage, null, new Dimension(0,300));
		else {
			frame = new ViewJFrameImage(srcImage);
			frame.setLocation(loc);
		}
		if(dimSize != null)
			frame.setSize(dimSize);
		frame.setVisible(true);
		listenersOn = false;
		
		skeleton = seg.getSkeleton();
		int[] chosenSpot = seg.getChosenSpot();
		changeX = chosenSpot[0];
		changeY = chosenSpot[1];
		
		frame.getComponentImage().getImageA().resetVOIs();
		float opacity = 1.0f;
		if(maskBox.isSelected())
			opacity = 0.0f;
		frame.getControls().getTools().setOpacity(opacity);
		frame.getControls().getTools().setPaintColor(Color.GREEN);
		
		frame.setCursor(null);
		frame.getComponentImage().setPaintMask(skeleton);

		//Add mouse listener so you can click to add/delete branches
		if(!listenersOn){
			frame.getComponentImage().addMouseListener(this);
			frame.getComponentImage().addMouseMotionListener(this);
			frame.addWindowListener(this);
			listenersOn = true;
		}
		
		frame.getComponentImage().setZoom(zX, zY);
		//frame.updateFrame(zX, zY);
		frame.updateImages();
		if(isExitRequired()){
			frame.removeWindowListener(frame);
			frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		}
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();

	}
	
	public void finalize(){
		
		if (frame != null) frame.close();
		if (listDialog != null) listDialog.dispose();
		if (segFrame != null) segFrame.close();
		if (imStack != null) imStack.disposeLocal();
		
    	images.clear();
    	srcImage.disposeLocal();
    	seg.cleanImages();
		seg.finalize();
		seg = null;
		skeleton = null;
		
		if(editBox.isSelected()){
			File dir = new File(dirText.getText());
			Preferences.setImageDirectory(dir);
			new PlugInDialogEditNeuron();
			dispose();
		} else{
			if (isExitRequired()) {
	            System.exit(0);
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}

	}
	
	/**
	 * Method that is called every time a new image is displayed
	 * (so when Prev or Next is chosen). Initializes the new 
	 * segmentation and adjusts the dialog title to reflect
	 * which image we are currently on
	 */
	
	protected void callAlgorithm(){
		/*if(extents.length > 2){
			actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Next"));
			return;
		}*/
		
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
	
	private void bresenham(int x0, int y0, int x1, int y1){
		int dx = Math.abs(x1-x0);
		int dy = Math.abs(y1-y0);
		int sx, sy;
		int err, e2;
		if(x0 < x1)
			sx = 1;
		else sx = -1;
		if(y0 < y1)
			sy = 1;
		else sy = -1;
		err = dx - dy;
		
		while(true){
			segmentList.add(new Point(x0,y0));
			int i = x0 + y0*width;
			skeleton.set(i);
			//System.err.printf("%d %d\n", x0, y0);
			if(x0 == x1 && y0 == y1) break;
			e2 = 2*err;
			if(e2 > -dy){
				err -= dy;
				x0 += sx;
			}
			if(e2 < dx){
				err += dx;
				y0 += sy;
			}
		}
	}

	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		FileNameExtensionFilter imFilter = new FileNameExtensionFilter("TIF/LSM Images", "tif", "tiff", "lsm");
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		fileChooser.addActionListener(this);
		fileChooser.addChoosableFileFilter(imFilter);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.setFileFilter(imFilter);
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

	@SuppressWarnings({ "unchecked", "rawtypes" })
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
        		+ "To turn off mouse click actions, select \"No Action\"<br>"
        		+ "Change sensitivity to change original segmentation.<br><br>"
        		+ "<b>NOTE:</b> Changing sensitivity or location resets any <br>"
        		+ "branches added or deleted previously. Changing sensitity <br>"
        		+ "may not change initial segmentation.<br><br>"
        		+ "<b>DISPLAYED IMAGE HAS BEEN FILTERED</b></html>";
        
        JLabel descLabel = new JLabel(desc);
        descLabel.setForeground(Color.black);
        descLabel.setFont(serif12);
        descPanel.add(descLabel);
        
        JPanel radioPanel = new JPanel(new GridLayout(2,2));
        radioPanel.setForeground(Color.black);
        
        addRB = new JRadioButton("Add");
        addRB.setFont(serif12);
        
        deleteRB = new JRadioButton("Delete");
        deleteRB.setFont(serif12);

        changeRB = new JRadioButton("Change Location");
        changeRB.setFont(serif12);
        
        offRB = new JRadioButton("No Action");
        offRB.setFont(serif12);
        offRB.setSelected(true);
        
        
        ButtonGroup group = new ButtonGroup();
        
        group.add(addRB);
        group.add(changeRB);
        group.add(deleteRB);
        group.add(offRB);
        
        radioPanel.add(offRB);
        radioPanel.add(addRB);
        radioPanel.add(changeRB);
        radioPanel.add(deleteRB);
        
        JPanel titlePanel = new JPanel();
        titlePanel.setForeground(Color.black);
        
        JLabel slideLabel = new JLabel("Sensitivity", JLabel.CENTER);
        slideLabel.setFont(serif12B);
        titlePanel.add(slideLabel);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setForeground(Color.black);
        
        sensSlider = new JSlider(JSlider.HORIZONTAL, 0, 50, 20);
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
        optionsPanel.add(saveSkelBox);
        
        saveVOIBox = new JCheckBox("Save VOIs");
        saveVOIBox.setFont(serif12);
        optionsPanel.add(saveVOIBox);
        
        imageBox = new JCheckBox("Show image list");
        imageBox.setFont(serif12);
        imageBox.addItemListener(this);
        optionsPanel.add(imageBox);
        
        editBox = new JCheckBox("Open Editor at end");
        editBox.setFont(serif12);
        optionsPanel.add(editBox);
        
        maskBox = new JCheckBox("Hide trace");
        maskBox.setFont(serif12);
        maskBox.addItemListener(this);
        optionsPanel.add(maskBox);
        
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
	
	private boolean openImage(){
		if(imStack == null){
			return openImageFiles();
		} else
			return openImageStack();
	}

	private boolean openImageFiles() {
	
		FileIO imLoader = new FileIO();
		
		if(srcImage != null) srcImage.disposeLocal();
		if(listDialog != null) list.setSelectedIndex(counter);
		
		File current = images.get(counter);
		
		srcImage = imLoader.readImage(current.toString());
		if(srcImage == null){
			MipavUtil.displayError("Could not read file: " + current.toString());
			return false;
		}
		
		extents = srcImage.getExtents();
		width = extents[0];
		
		return true;

	}
	
	private boolean openImageStack(){
		
		extents = imStack.getExtents();
		numImages = extents.length > 2 ? extents[2] : 1;
		extents = new int[]{extents[0], extents[1]};
		width = extents[0];
		
		if(extents.length < 3){
			srcImage = imStack;
			return true;
		}
		
		String[] slice = new String[]{String.valueOf(counter)};

		srcImage = new ModelImage(imStack.getType(), extents, imStack.getImageName() + "_slice" + counter);
		
		AlgorithmExtractSlices extract = new AlgorithmExtractSlices(imStack, srcImage, slice);
		extract.run();
		
		return true;
		
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
		
		if(!dir.exists()){
			MipavUtil.displayError("Input file/directory does not exist");
			return false;
		}
		if(dir.isFile()){
			String name = dir.toString();
			if(!(name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff")
						|| name.toLowerCase().endsWith(".lsm")))
			{
				MipavUtil.displayError("This is not a proper image file");
				return false;
			}
			//images.add(dir);
			
			FileIO imReader = new FileIO();
			imStack = imReader.readImage(name);

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
		Comparator<File> fileComp = new Comparator<File>(){

			@Override
			public int compare(File o1, File o2) {
				String s1 = o1.getPath();
				String s2 = o2.getPath();
				if(s1.length() > s2.length())
					return 1;
				else if(s1.length() < s2.length())
					return -1;
				else{
					return Integer.signum(s1.compareTo(s2));
				}
			}
			
		};
		
		Arrays.sort(files, fileComp);
		String dirStr = dir.toString();
		if(!dirStr.endsWith(File.separator))
			dirStr = dirStr + File.separator;
		dirStr = dirStr.concat("Branch_Images" + File.separator);
		
		for(File im : files){
			if(im == null)
				continue;
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
		} else if(source == maskBox){
			if(maskBox.isSelected()){
				frame.getControls().getTools().setOpacity(0.0f);
				frame.updateImages();
			} else{
				frame.getControls().getTools().setOpacity(1.0f);
				frame.getControls().getTools().setPaintColor(Color.GREEN);
				frame.updateImages();
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
		
		if(offRB.isSelected() || maskBox.isSelected())
			return;
		
		float zoomX = frame.getComponentImage().getZoomX();
		float zoomY = frame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX); //- left;
		int y = (int) ((float)e.getY()/zoomY); //- top;

		int i = x + y*width;
		if(changeRB.isSelected()){
			changeX = x;
			changeY = y;
			Point loc = frame.getLocation();
	    	frame.close();
    		seg.cleanImages();
    		
    		if(openImage()){
        		frame.setLocation(loc);
        		callAlgorithm();
        		if(segFrame != null){
        			segFrame.close();
        			segFrame = null;
        		}
        		segImageBox.setSelected(false);
    		}
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
		
		//Need to change so that it is only left click that works
		if(e.getButton() != MouseEvent.BUTTON1)
			return;

		float zoomX = frame.getComponentImage().getZoomX();
		float zoomY = frame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX);
		int y = (int) ((float)e.getY()/zoomY);

		intermediatePt = new Point(x,y);
		
		skeleton = (BitSet)seg.getSkeleton().clone();
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();
		//segmentList = new ArrayList<Point>();
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// Do nothing
		if(!addRB.isSelected() || 
				segmentList.size() == 0) return;
		
		intermediatePt = null;
		seg.linkNewSegment(segmentList);
		segmentList.clear();
		//segmentList = null;
		skeleton = seg.getSkeleton();
		frame.getComponentImage().setPaintMask(skeleton);
		frame.updateImages();
		
		if(tipBox.isSelected()) seg.displayTips();
		if(centroidBox.isSelected()) seg.displayCentroid();
		if(polygonalBox.isSelected()) seg.displayPolygonal();
		
		undoButton.setText("Undo");
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		// Do nothing
		
	}

	@Override
	public void mouseExited(MouseEvent e) {
		// Do nothing
		
	}
	
	@Override
	public void mouseDragged(MouseEvent e) {
		
		if(!addRB.isSelected()) return;

		float zoomX = frame.getComponentImage().getZoomX();
		float zoomY = frame.getComponentImage().getZoomY();
		int x = (int) ((float)e.getX()/zoomX);
		int y = (int) ((float)e.getY()/zoomY);
		
		//System.out.printf("%d %d\n", x,y);
		
		bresenham(intermediatePt.x, intermediatePt.y, x, y);
		intermediatePt = new Point(x,y);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		
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
			if (isExitRequired()) {
	            ViewUserInterface.getReference().windowClosing(event);
	        } else {
	        	cancelFlag = true;
	        	images.clear();
	        	seg.finalize();
	        	
	        	if(listDialog != null) 
	        		listDialog.dispose();
	        	if(segFrame != null) segFrame.close();
	        	dispose();
	        }
		}
		else if(source == listDialog){
			imageBox.setSelected(false);
		}
		else if(source == this){
			cancelFlag = true;
			if (isExitRequired()) {
				ViewUserInterface.getReference().windowClosing(event);
			} else {
				
				if(listDialog != null) 
					listDialog.dispose();
				if (frame != null) frame.close();
				if (listDialog != null) listDialog.dispose();
		    	images.clear();
		    	srcImage.disposeLocal();
				seg.finalize();
				seg = null;
				skeleton = null;
				dispose();
			}


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
	    	
	    	Point loc = frame.getLocation();
	    	frame.close();
    		seg.cleanImages();
    		
    		if(openImage()){
        		frame.setLocation(loc);
        		callAlgorithm();
        		if(segFrame != null){
        			segFrame.close();
        			segFrame = null;
        		}
        		segImageBox.setSelected(false);
    		}
	    	
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
