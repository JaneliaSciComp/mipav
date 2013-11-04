import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.*;
import java.nio.file.*;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.view.MipavUtil;
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
 * @author wangvg
 *
 *
 */

public class PlugInDialogWallNucleiStatsBulk extends JDialogBase implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2021668025631889124L;
	
	private JTextField dirText;
	
	private JFileChooser fileChooser;
	
	/**Contains the wall images for batch processing	 */
	private Vector<File> imageList;
	
	/**Contains the mask images fo batch processing */
	private Vector<File> maskList;
	
	private PlugInAlgorithmWallNucleiStatsBulk midAlg = null;
	
	private JComboBox<String> resUnits;
	
	private ViewUserInterface UI;
	
	private JTextField xResField;
	
	public PlugInDialogWallNucleiStatsBulk() {
        super();

        UI = ViewUserInterface.getReference();
        imageList = new Vector<File>();
        maskList = new Vector<File>();

        init();
    }
	
	public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        boolean changed = !dirText.getText().equals("Path to Directory with Slices");

        if(command.equals("Cancel")) dispose();
        else if (command.equals("Choose")) openDir();
        else if (command.equals("ApproveSelection")){
        	dirText.setText(fileChooser.getSelectedFile().toString());
        }
        else if  (command.equals("OK")){
        	if (changed){
	        	if(fileFilter()) callAlgorithm();
	        	else 
	        		MipavUtil.displayError("This folder does not contain any mask images");
	        	}
        	else MipavUtil.displayError("Please select a directory.");
        }
        else super.actionPerformed(event);

    }

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof PlugInAlgorithmWallNucleiStats){
			if (midAlg.isCompleted() == true ){
					
			}
			else UI.setGlobalDataText("Unable to complete the algorithm");
		}
		dispose();
	}
	
	protected void callAlgorithm() {
		
		try{
			
			int xRes;
			String units;
			
			xRes = Integer.valueOf(xResField.getText());
			units = resUnits.getSelectedItem().toString();
			
			Unit unitStruct = Unit.getUnit(units);
			String unitAbbr = unitStruct.getAbbrev();
			
			midAlg = new PlugInAlgorithmWallNucleiStatsBulk(imageList, maskList);
			
			midAlg.setRes(xRes);
			midAlg.setUnit(unitAbbr);

			//Progress bar is useless right now
			createProgressBar("Wall/Nuclei statistics calculation", "Initializing..." , midAlg);
			midAlg.addListener(this);
			setVisible(false);

			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still have user interface work fast.
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
	 * 
	 * @return returns false if there are no images to populate the vectors
	 */
	
	private boolean fileFilter(){
		
		Path dir = fileChooser.getSelectedFile().toPath();
		String stripped;
		//Populate the mask images first, then use that to get the wall images
		//so that only masked images are processed
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir, "*_mask.xml")) {
		    for (Path file: stream) {
		        maskList.add(file.toFile());
		        stripped = file.toString();
		        stripped = stripped.substring(0, stripped.length()-9);
		        stripped = stripped.concat(".jpg");
		        imageList.add(new File(stripped));
		    }
		} catch (IOException | DirectoryIteratorException x) {
		    MipavUtil.displayError("Directory Iterator not available");
		}
		
		return !imageList.isEmpty();
		
	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("Wall/Nuclei Statistics");

        
        JPanel dirPanel = new JPanel(new BorderLayout());
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image Directory"));
        
        String desc = "<html> Please choose a folder that contains wall sections as well as binary masks<br>"
        		+ "for each slice. Masks should be binary images saved as XML files, with<br>"
        		+ "pattern IMAGENAME_mask.xml. Wall images should have extension .jpg. <br></html>";
        
        JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel, BorderLayout.NORTH);
        
        JPanel choosePanel = new JPanel();
        
        dirText = new JTextField(30);
        dirText.setText("Path to Directory with Slices");
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
        resUnits = new JComboBox<String>(unitArr);
        resUnits.setFont(serif12);
        resUnits.setSelectedItem("Micrometers");
        resPanel.add(resUnits);
        
        getContentPane().add(resPanel, BorderLayout.CENTER);
        
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
		
	}
	
	private void openDir(){
		
		 	fileChooser = new JFileChooser();
	        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	        fileChooser.addActionListener(this);
	        fileChooser.showOpenDialog(this);
	        
	}
	
	
}
