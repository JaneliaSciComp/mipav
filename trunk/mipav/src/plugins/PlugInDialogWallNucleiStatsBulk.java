import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.io.*;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import javax.swing.*;

/**
 * Plugin for the Collins' Lab to calculate statistics for histology slides. 
 * This is the second iteration of the plugin. In this, the user is required 
 * to manually segment the wall from the histology slide, and pass that in 
 * to the plugin, which will calculate statistics for both the wall section 
 * and the nuclei in the wall. There is minimal error checking at the current 
 * juncture, and the progress bar is not updating smoothly. Further iterations 
 * will contain more error checking, while the progress bar is a lower priority 
 * issue. 
 * 
 * For this second iteration, the plugin is now a bulk processor. The user must
 * provide a directory where the images are. The file filter will then populate
 * vectors that contain the image paths for processing, which are then sent to the
 * batch processing algorithm
 * 
 * @author Victor Wang
 *
 *
 */

public class PlugInDialogWallNucleiStatsBulk extends JDialogBase implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2021668025631889124L;
	
	private File csv;
	
	private JTextField dirText;
	
	private JFileChooser fileChooser;
	
	/**Contains the wall images for batch processing	 */
	private Vector<File> imageList;
	
	/**Contains the mask images fo batch processing */
	private Vector<File> maskList;
	
	private PlugInAlgorithmWallNucleiStatsBulk midAlg = null;
	
	/**Writer for CSV*/
	private FileWriter output;
	
	private JComboBox resUnits;
	
	private JTextField xResField;
	
	public PlugInDialogWallNucleiStatsBulk() {
       
		super();
        init();
    }
	
	public void actionPerformed(ActionEvent event) {
		
        String command = event.getActionCommand();
        File dir;

        if(command.equals("Cancel")) dispose();
        else if (command.equals("Choose")) openDir();
        //Error checking for when the file explorer dialog is completed
        //Make sure the selected folder actually exists
        else if (command.equals("ApproveSelection")){
        	dir = fileChooser.getSelectedFile();
        	if (dir.exists()){
        		Preferences.setImageDirectory(fileChooser.getSelectedFile());
        		dirText.setText(fileChooser.getSelectedFile().toString());
        	}
        	else {
        		MipavUtil.displayError("Please select a valid directory");
        		openDir();
        	}
        }
        //Error check before the algorithm is called
        else if  (command.equals("OK")){
        	dir = new File(dirText.getText());
        	if (dir.exists()){//Check to see directory exists
	        	if(fileFilter()) {//Check to see if csv is capable of writing to
	        		if(initCSV()) callAlgorithm();
	        	}
	        	else 
	        		MipavUtil.displayError("This folder does not contain any mask images");
	        	}
        	else {
        		MipavUtil.displayError("Please select a valid directory.");
        		openDir();
        	}
        }
        else super.actionPerformed(event);
    }

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
		if (algorithm instanceof PlugInAlgorithmWallNucleiStats){
			if (midAlg.isCompleted() == true ){
				MipavUtil.displayInfo("Batch processing has completed. Statistics\n"
						+ "have been exported to CSV.");
				try {
					//Open the folder containing the CSV once the algorithm has completed
					Desktop.getDesktop().open(new File(dirText.getText()));
				} catch (IOException e) {
					MipavUtil.displayError("Problem opening folder to explore");
				}
			}
			else MipavUtil.displayInfo("Unable to complete the statistics processing");
		}
		dispose();
	}
	
	protected void callAlgorithm() {
		
		try{
			int xRes;
			
			xRes = Integer.valueOf(xResField.getText());
			
			midAlg = new PlugInAlgorithmWallNucleiStatsBulk(imageList, maskList);
			
			midAlg.setRes(xRes);
			midAlg.setCSV(output);

			//Progress bar doesn't update smoothly with algorithms
			createProgressBar("Wall/Nuclei statistics calculation", "Initializing..." , midAlg);
			midAlg.addListener(this);
			setVisible(false);

			if (isRunInSeparateThread()) {
				if (midAlg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				midAlg.run();
			}
			
		}
		catch (OutOfMemoryError x){
			MipavUtil.displayError("Unable to allocate enough memory");
            return;
		}
	}
	
	/**
	 * Method used to populate a vector of wall images that will be used for the algorithm.
	 * Images without masks are not included in the list. The user should run the accompanying
	 * plugin to create the masks. 
	 * 
	 * @return returns false if there are no images to populate the vectors
	 */
	
	private boolean fileFilter(){
		
		File dir = new File(dirText.getText());
        imageList = new Vector<File>();
        maskList = new Vector<File>();
		File mask;
		String stripped;
		
		//Filter out only .jpg/.jpeg images
		File[] files = dir.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return (name.toLowerCase().endsWith(".jpg") || name.toLowerCase().endsWith(".jpeg") ||
						name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff"));
			}
		});
		
		//Pair each .jpg image with its associated mask
		for(File im : files){
			stripped = im.toString();
			stripped = stripped.substring(0, stripped.indexOf("."));
	        stripped = stripped.concat("_mask.xml");
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
        setTitle("Wall/Nuclei Statistics");

        
        JPanel dirPanel = new JPanel(new BorderLayout());
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image Directory"));
        
        String desc = "<html><b>Directions: </b><br>"
        		+ "Please choose a folder that contains wall sections as well as binary masks<br>"
        		+ "for each slice. Masks should be binary images saved as XML files, with<br>"
        		+ "pattern IMAGENAME_mask.xml. Wall images should be JPEGs or TIFFs. <br></html>";
        
        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText(Preferences.getImageDirectory());
        dirText.setFont(serif12);
        choosePanel.add(dirText);
        
        JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);

        dirPanel.add(choosePanel, BorderLayout.SOUTH);
        
        getContentPane().add(dirPanel, BorderLayout.NORTH);
        
        JPanel resPanel = new JPanel();
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
        
        getContentPane().add(resPanel, BorderLayout.CENTER);
        
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
	 * 
	 * Initializes the csv file for statistics output. If the file does not already exist,
	 * the first couple of rows are added as information header
	 * 
	 * @return true if initialized, false if IO error (i.e. file is locked/can't write)
	 */
	private boolean initCSV(){
		
		String directory = imageList.get(0).getParent();
		csv = new File(directory.concat("\\statistics.csv"));
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
	 * Used for setting up and displaying the File Manager for the plugin. Since
	 * we are only interested in batches of files, the user can only select
	 * directories to open.
	 */
	
	private void openDir(){
		
		 	fileChooser = new JFileChooser(Preferences.getImageDirectory());
	        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        fileChooser.addActionListener(this);
	        fileChooser.showOpenDialog(this);
	}
}
