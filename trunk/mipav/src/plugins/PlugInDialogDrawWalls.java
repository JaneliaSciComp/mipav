import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOILogicalOperations;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogDrawWalls extends JDialogBase implements AlgorithmInterface {

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
	
	private ModelImage rgbImage;
	
	
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
        else if (command.equals("ApproveSelection")){
        	//Changes the text field to match the directory selected by the user
        	dirText.setText(fileChooser.getSelectedFile().toString());
        	Preferences.setImageDirectory(fileChooser.getSelectedFile());
        }
        else if (command.equals("Next")){
        	//Close the previous image and open the next one
        	counter++;
        	display.close();
        	if(counter < listLength){
        		callAlgorithm();
        		String title = "Wall Drawing " + String.valueOf(counter+1) + " of "
        				+ String.valueOf(listLength);
                setTitle(title);
        	}
        	else command = "End";
        }
        else if  (command.equals("OK")){
        	if (changed){
        		//If there are no images, tell the user
	        	if(fileFilter()) {
	        		callAlgorithm();
	        		initDrawer();
	        	}
	        	else 
	        		MipavUtil.displayError("This folder does not contain any mask images");
	        	}
        	else MipavUtil.displayError("Please select a directory.");
        }
        else if (command.equals("Save")) saveMask();
        else super.actionPerformed(event);
        
        //This is at the end so that if "Next" is pushed when there are no images
        //left, it falls to this case
        if(command.equals("Cancel")||command.equals("End")) {
        	display.close();
        	imageList.clear();
        	rgbImage = null;
        	dispose();
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
	
	protected void callAlgorithm() {
		
		FileIO imLoader = new FileIO();
		
		current = imageList.get(counter);
		rgbImage = imLoader.readImage(current.toString());
		rgbImage.setImageName(current.getName(),false);
		display = new ViewJFrameImage(rgbImage, null, new Dimension(0,300));
	}
	
	/**
	 * Method used to populate a vector of wall images that will be used for the algorithm.
	 * Images that already have masks are not included in the list.
	 * 
	 * @return returns false if there are no images to populate the vectors
	 */
	
	private boolean fileFilter(){
		
		Path dir = Paths.get(dirText.getText());
		File mask;
		String stripped;
		
		//Populate the mask images first, then use that to get the wall images
		//so that only masked images are processed
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir, "*.{jpg,jpeg}")) {
		    for (Path file: stream) {
		        stripped = file.toString();
		        stripped = stripped.substring(0, stripped.indexOf("."));
		        stripped = stripped.concat("_mask.xml");
		        mask = new File(stripped);
		        if (!mask.exists()) imageList.add(file.toFile());
		    }
		} catch (IOException | DirectoryIteratorException x) {
		    MipavUtil.displayError("Directory Iterator not available");
		}
		
		listLength = imageList.size();
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
        		+ "Please use the VOI options to denote the wall sections. The first VOI should<br>"
        		+ "outline the outer portion of the wall. The second VOI should outline the inner<br>"
        		+ "portion of the wall. When done, you can save the outlines and the proceed to <br>"
        		+ "the next image. </html>";
        
        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        getContentPane().add(dirLabel, BorderLayout.NORTH);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        
        JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);
        
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
		String destPath, destName; //Directory and file paths
		
		VOIVector vois = rgbImage.getVOIs();
		if(vois == null){
			MipavUtil.displayError("No VOI was drawn. Please draw the wall boundaries.");
			return;
		}
		//Requires clone, or else errors occur
		cloned = (ModelImage) rgbImage.clone();
		AlgorithmVOILogicalOperations xor = 
				new AlgorithmVOILogicalOperations(cloned, vois, AlgorithmVOILogicalOperations.XOR, false);
		xor.run();
		outImage = xor.getFinalMaskImage();
		destPath = current.getParent().concat("\\");
		destName = current.getName();
		destName = destName.substring(0, destName.indexOf(".")).concat("_mask.xml");
		opt = new FileWriteOptions(destName, destPath, true);
		imWriter = new FileIO();
		imWriter.writeImage(outImage, opt, false);
	}
}
