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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
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
import javax.swing.filechooser.FileNameExtensionFilter;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMedian;
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
	
	private JRadioButton deleteRB;
	
	private JTextField dirText;
	
	private JCheckBox editBox;

	private JCheckBox excludeBox;
	
	private int[] extents;
	
	private JFileChooser fileChooser;	
	
	private ViewJFrameImage frame;
	
	private JCheckBox imageBox;
	
	private int[] imBuffer;
	
	/**
	 * List containing the images that need to be segmented 
	 * during this run through
	 */
	private ArrayList<File> images;
	
	private int height;
	
	/**
	 * Displayed list of images that can be used to jump to
	 * other images
	 */
	@SuppressWarnings("rawtypes")
	private JList list;
	
	private JDialog listDialog;
	
	private boolean listenersOn;
	
	private JButton jumpButton;

	private JCheckBox noiseBox;

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
        		if(openImage()){
	        		initModifications();
	        		callAlgorithm();
        		} else images.clear();
        	}
		}
		else if(command.equals("Prev")){
			if (counter > 0){
        		counter--;
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
		}
		else if (command.equals("Next")){
        	//Close the previous image and open the next one
        	counter++;
        	if(counter < numImages){
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
			sensSlider.setValue(20);
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
		frame.getControls().getTools().setPaintColor(Color.GREEN);
		
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
		
		if(editBox.isSelected()){
			File dir = new File(dirText.getText());
			if(dir.isDirectory()){
				new PlugInDialogEditNeuron(dirText.getText());
			} else {
				new PlugInDialogEditNeuron(dir.getParent());
			}
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
		
		//extents = srcImage.getExtents();
		if(extents.length > 2){
			actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Next"));
			return;
		}
		//width = extents[0];
		//height = extents[1];
		
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
	
	/**
	 * Crude filter to get rid of shot noise resulting from the max-projected 
	 * neuron images. It implements a median filter, but only adjusts pixels
	 * that are changing over a certain threshold. The median filter is then
	 * repeated locally around points that were changed until there are no 
	 * longer any differences greater than the given threshold.
	 * 
	 * For 8-bit integer images, the threshold is set to 3, while for 16-bit
	 * integer images the threshold is 66. This does not work with float and
	 * double valued images right now.
	 * 
	 * This filter runs in place. 
	 * 
	 * @param image input image to filter. Is also the result image as this 
	 * filter places the results back into the original image
	 */
	
	private void filterShotNoise(ModelImage image){

		int dataType = image.getType();
		int maxDiff;
		
		if(dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE)
			maxDiff = 3;
		else if(dataType == ModelImage.SHORT || dataType == ModelImage.USHORT)
			maxDiff = 66;
		else 
			return;
		
		ModelImage medianImage = (ModelImage) image.clone();
		AlgorithmMedian median = new AlgorithmMedian(medianImage, 1, 3, AlgorithmMedian.SQUARE_KERNEL,
				0, AlgorithmMedian.STANDARD, 3, true);
		median.run();
		int length = width*height;
		imBuffer = new int[length];
		int[] medBuffer = new int[length];
		int[] outBuffer = new int[length];
		int diff;
		
		
		try{
			image.exportData(0, length, imBuffer);
			medianImage.exportData(0, length, medBuffer);
		} catch(IOException e){
			MipavUtil.displayError("Could not export data from original image");
			e.printStackTrace();
		}
		
		ArrayList<Integer> adjustPts = new ArrayList<Integer>();
		ArrayList<Integer> addPts = new ArrayList<Integer>();
		for(int i=0;i<length;i++){
			diff = Math.abs(imBuffer[i] - medBuffer[i]);
			if(diff >= maxDiff){
				//adjustPts.add(i);
				imBuffer[i] = medBuffer[i];
				int x = i%width;
				int y = i/width;
				for(int nx=x-1;nx<=x+1;nx++){
					if(nx<0 || nx>=width) continue;
					for(int ny=y-1;ny<=y+1;ny++){
						if(ny<0 || ny>=height) continue;
						int ind = nx+ny*width;
						if(!adjustPts.contains(ind))
							adjustPts.add(nx+ny*width);
					}
				}
			}
		}
		
		medBuffer = null;
		
		System.arraycopy(imBuffer, 0, outBuffer, 0, length);
		
		while(adjustPts.size()>0){
			int size = adjustPts.size();
			for(int j = 0;j<size;j++){
				int i = adjustPts.get(j);
				int x = i%width;
				int y = i/width;
				int kMed = findMedian(i);
				if(Math.abs(imBuffer[i] - kMed) >= maxDiff){
					outBuffer[i] = kMed;
					//adjustPts.add(i);
					for(int nx=x-1;nx<=x+1;nx++){
						if(nx<0 || nx>=width) continue;
						for(int ny=y-1;ny<=y+1;ny++){
							if(ny<0 || ny>=height) continue;
							int ind = nx+ny*width;
							if(!addPts.contains(ind))
								addPts.add(nx+ny*width);
						}
					}
				}
			}
			for(int j = 0;j<size;j++){
				int i=adjustPts.remove(0);
				imBuffer[i] = outBuffer[i];
			}
			adjustPts.addAll(addPts);
			addPts.clear();
		}
		
		try {
			image.importData(0, outBuffer, true);
		} catch (IOException e) {
			MipavUtil.displayError("Unable to import filtered image");
			e.printStackTrace();
		}
		
		medianImage.disposeLocal();
	}
	
	/**
	 * Lazy implementation of a median finder. Uses the 
	 * built in quicksort and then finds the middle value
	 * of the sorted array.
	 * @param array the array to find the median of
	 * @return the median value
	 */

	private int findMedian(int[] array){
	
		Arrays.sort(array);
		int middle = array.length/2;
		if(array.length%2 == 0){
			return (array[middle] + array[middle-1])/2;
		} else {
			return array[middle];
		}
	}
	
	/**
	 * Wrapper method to find the median. Extracts the 
	 * values around the given index and puts them into
	 * an array to find the median of.
	 * @param i find the median of the box centered on this index
	 * @return the median value
	 */
	
	private int findMedian(int i){
		int x = i%width;
		int y = i/width;
		int kWidth = Math.min(3, 2 + Math.min(x, width-1-x));
		int kHeight = Math.min(3, 2 + Math.min(y, height-1-y));
		int[] kArray = new int[kWidth*kHeight];
		int cnt = 0;
	
		for(int nx=x-1;nx<=x+1;nx++){
			if(nx<0 || nx>=width) continue;
			for(int ny=y-1;ny<=y+1;ny++){
				if(ny<0 || ny>=height) continue;
				kArray[cnt] = imBuffer[nx + ny*width];
				cnt++;
			}
		}
		int kMed = findMedian(kArray);
		return kMed;
	
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
        
        JPanel noisePanel = new JPanel();
        noisePanel.setForeground(Color.black);
        
        noiseBox = new JCheckBox("Run shot noise filter on images");
        noiseBox.setFont(serif12);
        noisePanel.add(noiseBox);
        
        PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(checkPanel);
        manage.addOnNextLine(noisePanel);
        
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
        saveSkelBox.setSelected(true);
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
        editBox.setSelected(true);
        optionsPanel.add(editBox);
        
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

	private boolean openImage() {
	
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
		height = extents[1];
		
		
		if(noiseBox.isSelected()){
			filterShotNoise(srcImage);
		}
		
		srcImage.setImageName(current.getName(),false);
		frame = new ViewJFrameImage(srcImage, null, new Dimension(0,300));
		frame.setVisible(true);
		listenersOn = false;
		
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
		else if(source == this){
			cancelFlag = true;
			if (isExitRequired()) {
				System.exit(0);
				ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			} else {
				dispose();
				if(listDialog != null) 
					listDialog.dispose();
				if (frame != null) frame.close();
				if (listDialog != null) listDialog.dispose();
		    	images.clear();
		    	srcImage.disposeLocal();
				seg.finalize();
				seg = null;
				skeleton = null;
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
