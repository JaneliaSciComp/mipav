import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Stack;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * Provides a very rudimentary GUI to stitch a series of images together one by one
 * 
 * @author wangvg
 * 
 */
public class PlugInDialogImageStitchingGUI extends JDialogStandalonePlugin implements AlgorithmInterface{

	/**
	 * 
	 */
	private static final long serialVersionUID = 4839106711273262563L;

	private ViewJFrameImage pairFrame;
	
	private ViewJFrameImage stitchFrame;
	
	private ArrayList<Vector3f> relShift;
	
	private JTextField dirText;
	
	private JFileChooser fileChooser;
	
	private ArrayList<ModelImage> images;
	
	private ArrayList<ModelImage> mpImages;
	
	private ArrayList<File> imFiles;
	
	private int counter;
	
	private ModelImage refImage;
	
	private ModelImage targetImage;
	
	private UndoContainer undoer;
	
	private boolean saving;
	
	private JLabel refLabel;
	
	private JLabel targetLabel;
	
	private Point pairSpot;
	
	private Point stitchSpot;
	
	private JCheckBox disableBox;
	
	public PlugInDialogImageStitchingGUI(){
		super();
		
		images = new ArrayList<ModelImage>();
		mpImages = new ArrayList<ModelImage>();
		imFiles = new ArrayList<File>();
		relShift = new ArrayList<Vector3f>();
		
		undoer = new UndoContainer();
		
		pairFrame = null;
		stitchFrame = null;
		saving = false;
		
		counter = 0;
		
		init();
	}
	
	public void actionPerformed(ActionEvent event){
		String command = event.getActionCommand();
		if(command.equals("Choose")){
			chooseDir();
		}else if(command.equals("OK")){
			if(populateImages(new File(dirText.getText())))
				initGUI();
		}else if(command.equals(JFileChooser.APPROVE_SELECTION)){
			dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
		}else if(command.equals("Cancel")){
			if (isExitRequired()) {
	            ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	        } else {
	        	dispose();
	        }
		}else if(command.equals("ref")){
			if(pairFrame != null)
				pairFrame.actionPerformed(new ActionEvent(this, 0, CustomUIBuilder.PARAM_VOI_POINT.getActionCommand()));
		}else if(command.equals("Undo")){
			if(counter > 1){
				undoer.undo();
				createViewUndo();
				
				if(counter > 1){
					// Remove the last image
					ArrayList<ModelImage> shortened = new ArrayList<ModelImage>(mpImages);
					shortened.remove(shortened.size()-1);
					// Recalculate the image and display it
					PlugInAlgorithmImageStitchingGUI alg = new PlugInAlgorithmImageStitchingGUI(shortened, relShift);
					alg.run();
					stitchFrame = new ViewJFrameImage(alg.getDestImage());
					if(stitchSpot != null)
						stitchFrame.setLocation(stitchSpot);
					stitchFrame.setVisible(true);
					stitchFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
				}
			}
		}else if(command.equals("Redo")){
			if(!undoer.isEmpty()){
				// Add the undone image and translation back to recalculate the image
				undoer.redo();
				createViewRedo();
				
				ArrayList<ModelImage> shortened = new ArrayList<ModelImage>(mpImages);
				shortened.remove(shortened.size()-1);
				
				PlugInAlgorithmImageStitchingGUI alg = new PlugInAlgorithmImageStitchingGUI(shortened, relShift);
				alg.run();
				stitchFrame = new ViewJFrameImage(alg.getDestImage());
				if(stitchSpot != null)
					stitchFrame.setLocation(stitchSpot);
				stitchFrame.setVisible(true);
				stitchFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
			}
		}else if(command.equals("register")){
			callAlgorithm();
		}else if(command.equals("Save")){
			saving = true;
			callAlgorithm();
		}
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof PlugInAlgorithmImageStitchingGUI){
			if(saving){
				MipavUtil.displayInfo("Saving has been completed.");
			}else{
				stitchFrame = new ViewJFrameImage(algorithm.getDestImage());
				if(stitchSpot != null)
					stitchFrame.setLocation(stitchSpot);
				stitchFrame.setVisible(true);
				stitchFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
				if(counter < imFiles.size()-1){
					refImage = targetImage;
					createView();
				}else{
					int n = JOptionPane.showConfirmDialog(
							this, "All images have been registered. Do you wish to save?",
							"Registration complete!",
							JOptionPane.YES_NO_OPTION);
					if(n == JOptionPane.YES_OPTION){
						saving = true;
						callAlgorithm();
					}
				}
			}
		}
	}

	protected void callAlgorithm(){
		if(saving){
			String baseDir = imFiles.get(0).getParent();
			File resultDir = new File(baseDir + File.separator + "results");
			if(resultDir.exists()){
				resultDir.delete();
			}
			resultDir.mkdir();
			
			while(counter < images.size()-1){
				images.remove(images.size()-1);
			}
			
			ArrayList<ModelImage> shortened = new ArrayList<ModelImage>(images);
			while(relShift.size() < shortened.size()){
				shortened.remove(shortened.size()-1);
			}
			
			PlugInAlgorithmImageStitchingGUI alg = new PlugInAlgorithmImageStitchingGUI(shortened, relShift);
			createProgressBar("Saving image", "Saving image slices", alg);
			progressBar.setVisible(true);
			alg.setNames(resultDir.getAbsolutePath(), imFiles.get(0).getParentFile().getName());
			alg.addListener(this);
			if (isRunInSeparateThread()) {
				if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				alg.run();
			}
		}else{
			
			ModelImage im = pairFrame.getComponentImage().getImageA();
			// You need to have reference points to stitch together
			VOIVector vec = im.getVOIs();
			if(vec.size() == 0){
				MipavUtil.displayError("No reference points have been added.");
				return;
			}else if(vec.size() > 2){
				MipavUtil.displayError("Only two reference points can be used.");
				return;
			}
			VOI voi = vec.get(0);
			VOIBaseVector base = voi.getCurves();
			// Should only be two points, otherwise something is wrong
			if(base.size()==2){
				
				int width = images.get(counter).getWidth(0);
				
				VOIBase pt1 = base.get(0);
				VOIBase pt2 = base.get(1);
				Vector3f vec1 = pt1.get(0);
				Vector3f vec2 = pt2.get(0);
				
				if((vec1.X < width && vec2.X < width) 
						|| vec1.X > width && vec2.X > width){
					MipavUtil.displayError("There must be a reference point on each image.");
					return;
				}
				
				if(vec1.X > vec2.X){
					Vector3f temp = new Vector3f();
					temp.copy(vec2);
					vec2 = vec1;
					vec1.copy(temp);
				}
				
				vec2.sub(new Vector3f(width, 0, 0));
				
				Vector3f shift = vec1.sub(vec2);
				
				// You can either use a registration algorithm to jiggle the stitching to get a
				// better match, but you can just go with whatever the given translation is as well
				if(disableBox.isSelected()){
					relShift.add(shift);
				}else{
					PlugInAlgorithmPhaseCorrelation pcr = new PlugInAlgorithmPhaseCorrelation(refImage, targetImage, shift, 300, 20, 0);
					pcr.run();
					
					Vector3f trans = pcr.getTranslation();
					
					if(trans == null){
						System.out.println("No max found");
						relShift.add(shift);
					}else{
						System.out.println("Input shift " + shift.X + " " + shift.Y);
						System.out.println("Output shift " + trans.X + " " + trans.Y);
						relShift.add(pcr.getTranslation());
					}
				}
				//relShift.add(shift);
				
			}else{
				MipavUtil.displayError("Please place reference points on both the "
						+ "reference and target image.");
				return;
			}
			
			undoer.clear();
			
			if(pairFrame != null){
				pairSpot = pairFrame.getLocation();
				pairFrame.close();
				pairFrame = null;
			}
			if(stitchFrame != null){
				stitchSpot = stitchFrame.getLocation();
				stitchFrame.close();
			}

			// Stitch the image together based on the various translations provided
			PlugInAlgorithmImageStitchingGUI alg = new PlugInAlgorithmImageStitchingGUI(mpImages, relShift);
			alg.addListener(this);
			if (isRunInSeparateThread()) {
				if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				alg.run();
			}
		}
	}
	
	private ModelImage maxProject(ModelImage image){
		
		int depth = image.getNDims() > 2 ? image.getExtents()[2] : 1;
		double maxPixel = image.getMax();
		double minPixel = image.getMin();
		
		
		AlgorithmMaximumIntensityProjection alg = new AlgorithmMaximumIntensityProjection(
				image, 0, depth-1, depth, minPixel, maxPixel, true, false, AlgorithmMaximumIntensityProjection.Z_PROJECTION);
		alg.run();
		
		return alg.getResultImage().get(0);
	}
	
	/**
	 * Places two images side by side so that it is easier for the user to see both images at the same time.
	 */
	private void createView(){
		
		FileIO io = new FileIO();
		
		if(pairFrame != null){
			pairSpot = pairFrame.getLocation();
			pairFrame.close();
			pairFrame = null;
		}
		//if(stitchFrame != null)
		//	stitchFrame.close();
		
		counter++;
		
		ModelImage targetImage3D;
		if(counter < images.size()){
			targetImage3D = images.get(counter);
		}else{
			targetImage3D = io.readImage(imFiles.get(counter).getAbsolutePath());
			images.add(targetImage3D);
			
		}
		
		targetImage = maxProject(targetImage3D);
		mpImages.add(targetImage);
		
		refLabel.setText(imFiles.get(counter-1).getName());
		targetLabel.setText(imFiles.get(counter).getName());
		
		int width = targetImage.getWidth(0);
		int height = targetImage.getHeight(0);
		int length = width*height;
		
		int[] pairBuffer = new int[length * 2];
		
		int[] buffer1 = new int[length];
		int[] buffer2 = new int[length];
		
		try {
			refImage.exportData(0, length, buffer1);
			targetImage.exportData(0, length, buffer2);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		for(int j=0;j<height;j++){
			System.arraycopy(buffer1, j*width, pairBuffer, 2*j*width, width);
			System.arraycopy(buffer2, j*width, pairBuffer, width*(2*j+1), width);
		}
		
		ModelImage pairImage = new ModelImage(refImage.getType(), new int[]{2*width, height}, "Image pair");
		try {
			pairImage.importData(0, pairBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		pairFrame = new ViewJFrameImage(pairImage);
		if(pairSpot != null)
			pairFrame.setLocation(pairSpot);
		pairFrame.setVisible(true);
		pairFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		
	}
	
	/**
	 * Carries out the undo action and redisplays the correct images side by side as well as the stitched image
	 */
	private void createViewUndo(){
		
		if(pairFrame != null){
			pairSpot = pairFrame.getLocation();
			pairFrame.close();
			pairFrame = null;
		}
		if(stitchFrame != null){
			stitchSpot = stitchFrame.getLocation();
			stitchFrame.close();
		}
		
		counter--;
		
		targetImage.disposeLocal();
		targetImage = refImage;
		refImage = mpImages.get(counter-1);
		
		refLabel.setText(imFiles.get(counter-1).getName());
		targetLabel.setText(imFiles.get(counter).getName());
		
		int width = targetImage.getWidth(0);
		int height = targetImage.getHeight(0);
		int length = width*height;
		
		int[] pairBuffer = new int[length * 2];
		
		int[] buffer1 = new int[length];
		int[] buffer2 = new int[length];
		
		try {
			refImage.exportData(0, length, buffer1);
			targetImage.exportData(0, length, buffer2);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		for(int j=0;j<height;j++){
			System.arraycopy(buffer1, j*width, pairBuffer, 2*j*width, width);
			System.arraycopy(buffer2, j*width, pairBuffer, width*(2*j+1), width);
		}
		
		ModelImage pairImage = new ModelImage(refImage.getType(), new int[]{2*width, height}, "Image pair");
		try {
			pairImage.importData(0, pairBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		pairFrame = new ViewJFrameImage(pairImage);
		pairFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		pairFrame.setVisible(true);
		
	}
	
	/**
	 * Carries out the redo action and displays the correct individual images and stitched image
	 */
	private void createViewRedo(){
		
		if(pairFrame != null){
			pairSpot = pairFrame.getLocation();
			pairFrame.close();
			pairFrame = null;
		}
		if(stitchFrame != null){
			stitchSpot = stitchFrame.getLocation();
			stitchFrame.close();
		}
		counter++;
		
		refLabel.setText(imFiles.get(counter-1).getName());
		targetLabel.setText(imFiles.get(counter).getName());
		
		int width = targetImage.getWidth(0);
		int height = targetImage.getHeight(0);
		int length = width*height;
		
		int[] pairBuffer = new int[length * 2];
		
		int[] buffer1 = new int[length];
		int[] buffer2 = new int[length];
		
		try {
			refImage.exportData(0, length, buffer1);
			targetImage.exportData(0, length, buffer2);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		for(int j=0;j<height;j++){
			System.arraycopy(buffer1, j*width, pairBuffer, 2*j*width, width);
			System.arraycopy(buffer2, j*width, pairBuffer, width*(2*j+1), width);
		}
		
		ModelImage pairImage = new ModelImage(refImage.getType(), new int[]{2*width, height}, "Image pair");
		try {
			pairImage.importData(0, pairBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		pairFrame = new ViewJFrameImage(pairImage);
		if(pairSpot != null)
			pairFrame.setLocation(pairSpot);
		pairFrame.setVisible(true);
		pairFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		
	}
	
	private void chooseDir(){
		String dirText = Preferences.getImageDirectory();
		fileChooser = new JFileChooser(dirText);
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.addActionListener(this);
		fileChooser.setAcceptAllFileFilterUsed(false);
		fileChooser.showOpenDialog(this);
	}
	
	/**
	 * Initial GUI that prompts user for the directory containing the images
	 */
	private void init(){
		
		setForeground(Color.black);
        setTitle("Manual Image Stitching");

        JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains the images to register</html>";

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
	 * Actual GUI used for the stitching. Provides instructions and actionable buttons to get the stitching to work.
	 */
	private void initGUI(){
		
		getContentPane().removeAll();

		refLabel = new JLabel();
		refLabel.setFont(serif12B);
		targetLabel = new JLabel();
		targetLabel.setFont(serif12B);
		
		FileIO io = new FileIO();
		ModelImage refImage3D = io.readImage(imFiles.get(0).getAbsolutePath());
		images.add(refImage3D);
		refImage = maxProject(refImage3D);
		mpImages.add(refImage);
		
		createView();
		
		relShift.add(new Vector3f());
		
		String instructions = "<html><b>"
				+ "Place reference points on the reference and target images to<br>"
				+ "assist in the registration of the full image."
				+ "</b></html>";
		
		JLabel instructionLabel = new JLabel(instructions);
		instructionLabel.setFont(serif12B);
		
		JLabel refLabelName = new JLabel("Reference image (on left): ");
		refLabelName.setFont(serif12B);
		
		JLabel targetLabelName = new JLabel("Target image (on right): ");
		targetLabelName.setFont(serif12B);
		
		JButton refButton = new JButton("Place reference point");
		refButton.setFont(serif12);
		refButton.setActionCommand("ref");
		refButton.addActionListener(this);
		
		JButton registerButton = new JButton("Register images");
		registerButton.setFont(serif12);
		registerButton.setActionCommand("register");
		registerButton.addActionListener(this);
		
		JButton undoButton = new JButton("Undo");
		undoButton.setFont(serif12);
		undoButton.addActionListener(this);
		
		JButton redoButton = new JButton("Redo");
		redoButton.setFont(serif12);
		redoButton.addActionListener(this);
		
		JButton saveButton = new JButton("Save");
		saveButton.setFont(serif12);
		saveButton.addActionListener(this);
		
		JButton endButton = new JButton("End");
		endButton.setFont(serif12);
		endButton.setActionCommand("Cancel");
		endButton.addActionListener(this);
		
		disableBox = new JCheckBox("Disable local registration");
		disableBox.setFont(serif12B);
		
		JPanel disablePanel = new JPanel(new BorderLayout());
		disablePanel.setForeground(Color.black);
		disablePanel.setBorder(new EmptyBorder(10, 0, 10, 0));
		disablePanel.add(disableBox, BorderLayout.CENTER);
		
		JPanel topPanel = new JPanel(new GridBagLayout());
		topPanel.setForeground(Color.black);
		topPanel.setBorder(new EmptyBorder(5, 5, 0, 5));
		
		GridBagConstraints gbc = new GridBagConstraints();
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 4;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.anchor = GridBagConstraints.WEST;
		
		topPanel.add(instructionLabel, gbc);
		
		gbc.gridy++;
		gbc.gridwidth = 2;
		gbc.insets = new Insets(10, 0, 0, 0);
		
		topPanel.add(refLabelName, gbc);
		
		gbc.gridx = 2;
		gbc.weightx = 1;
		topPanel.add(refLabel, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		gbc.weightx = 0;
		topPanel.add(targetLabelName, gbc);
		
		gbc.gridx = 2;
		gbc.weightx = 1;
		topPanel.add(targetLabel, gbc);
		
		getContentPane().add(topPanel, BorderLayout.NORTH);
		
		getContentPane().add(disablePanel, BorderLayout.CENTER);
		
		JPanel bottomPanel = new JPanel(new GridBagLayout());
		bottomPanel.setForeground(Color.black);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 2;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0, 0, 0, 0);
		
		bottomPanel.add(refButton, gbc);
		
		gbc.gridx = 2;
		bottomPanel.add(registerButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy++;
		gbc.gridwidth = 1;
		
		bottomPanel.add(undoButton, gbc);
		gbc.gridx = 1;
		bottomPanel.add(redoButton, gbc);
		gbc.gridx = 2;
		bottomPanel.add(saveButton, gbc);
		gbc.gridx = 3;
		bottomPanel.add(endButton, gbc);
		
		getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
		System.gc();
		
	}
	
	private boolean populateImages(File dir){
		
		if(!dir.exists()){
			MipavUtil.displayError("Input directory does not exist");
			return false;
		}
		
		FilenameFilter imFilter = new FilenameFilter(){
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff"));
			}
		};
		File[] files = dir.listFiles(imFilter);
		Comparator<File> fileComp = new Comparator<File>(){

			@Override
			public int compare(final File o1, final File o2) {
				final String s1 = o1.getName();
				final String s2 = o2.getName();

				final String s1NoNum = s1.replaceAll("[0-9]", "");
				final String s2NoNum = s2.replaceAll("[0-9]", "");

				final int compare = s1NoNum.compareTo(s2NoNum);

				if (compare == 0) {
					// Without numbers, the two are the same
					final String s1Num = s1.replaceAll("[^0-9]", "");
					final String s2Num = s2.replaceAll("[^0-9]", "");
					final String s1NumFinal;
					final String s2NumFinal;

					// Truncate so that you aren't growing too large
					int length = String.valueOf(Integer.MAX_VALUE).length() - 1;
					if (s1Num.length() > length) {
						s1NumFinal = s1Num.substring(s1Num.length() - length);
					} else {
						s1NumFinal = s1Num;
					}

					if (s2Num.length() > length) {
						s2NumFinal = s2Num.substring(s2Num.length() - length);
					} else {
						s2NumFinal = s2Num;
					}

					// Compare the left over numbers
					final int s1Int = Integer.valueOf(s1NumFinal);
					final int s2Int = Integer.valueOf(s2NumFinal);

					return Integer.valueOf(s1Int).compareTo(Integer.valueOf(s2Int));
				} else {
					return compare;
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
			imFiles.add(im);
			
		}
		
		if(imFiles.isEmpty()){
			MipavUtil.displayError("There are no images in this directory");
			return false;
		}
		
		return true;
		
	}
	
	/**
	 * Convenience class used to keep track of any actions so that they are easier to undo/redo
	 * 
	 * @author wangvg
	 * 
	 */
	private class UndoContainer{
		
		private Stack<ModelImage> imageStack;
		
		private Stack<Vector3f> shiftStack;
		
		private UndoContainer(){
			imageStack = new Stack<ModelImage>();
			shiftStack = new Stack<Vector3f>();
		}
		
		private void clear(){
			imageStack.clear();
			shiftStack.clear();
		}
		
		private boolean isEmpty(){
			return imageStack.isEmpty();
		}
		
		private void undo(){
			imageStack.push(mpImages.remove(mpImages.size()-1));
			shiftStack.push(relShift.remove(relShift.size()-1));
		}
		
		private void redo(){
			mpImages.add(imageStack.pop());
			relShift.add(shiftStack.pop());
		}
		
	}
	
}
