import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.util.Enumeration;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.model.structures.*;

import javax.swing.*;

/**
 * Plugin for the Collins' Lab to calculate statistics for histology slides. 
 * This is the first iteration of the plugin. In this, the user is required 
 * to manually segment the wall from the histology slide, and pass that in 
 * to the plugin, which will calculate statistics for both the wall section 
 * and the nuclei in the wall. There is minimal error checking at the current 
 * juncture, and the progress bar is not updating smoothly. Further iterations 
 * will contain more error checking, while the progress bar is a lower priority 
 * issue. 
 * 
 * @author wangvg
 *
 *
 * PLUGIN INPUT REQUIREMENTS:
 * RGB Image : An image containing the wall/nuclei 
 * Mask Image: A boolean image masking the wall/nuclei. The user must segment this manually. 
 * Resolution Info: Default is 1 um, should be changed by the user depending on imaging parameters
 *
 */

public class PlugInDialogWallNucleiStats extends JDialogBase implements AlgorithmInterface {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2021668025631889124L;

	private ModelImage rgbImage;
	
	private ModelImage boolImage;
	
	private PlugInAlgorithmWallNucleiStats midAlg = null;
	
	//private ViewUserInterface userInterface;
	
	private JTextField xResField;
	
	private JComboBox<String> resUnits;
	
	private JComboBox<String> rgbImages, boolImages;
	
	private ViewUserInterface UI;
	
	public PlugInDialogWallNucleiStats() {
        super();

        UI = ViewUserInterface.getReference();
        
		if(buildImageBox()) init();
		else dispose();
    }
	
	public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if(command.equals("Cancel")) dispose();
        else if (command.equals("OK")) callAlgorithm();
        else super.actionPerformed(event);

    }

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if (algorithm instanceof PlugInAlgorithmWallNucleiStats){
			if (midAlg.isCompleted() == true ){
				
				int xRes, area;
				String units;
				
				xRes = Integer.valueOf(xResField.getText());
				units = resUnits.getSelectedItem().toString();
				
				Unit unitStruct = Unit.getUnit(units);
				String unitAbbr = unitStruct.getAbbrev();

				float[] stats = midAlg.getWallStats();
				float[] nStats = midAlg.getNucleiStats();
				area = midAlg.getArea();
				
				if(midAlg.getMaxIndex() == -1) {
					UI.setGlobalDataText(
							"\nWall thickness Statistics unavailable. Could not find suitable seed point. \n\n");
				}
				else {
					UI.setGlobalDataText(
							"\nWall thickness Statistics (" + unitAbbr + "): \n\n");
					UI.setGlobalDataText(
							"    Minimum Thickness  = " + String.valueOf(stats[0]*xRes)+ " " + unitAbbr +"\n");
					UI.setGlobalDataText(
							"    Maximum Thickness  = " + String.valueOf(stats[1]*xRes)+ " " + unitAbbr+"\n");
					UI.setGlobalDataText(
							"    Average Thickness  = " + String.valueOf(stats[2]*xRes)+ " " + unitAbbr+"\n");
					UI.setGlobalDataText(
							"    Std. Deviation  = " + String.valueOf(stats[3]*xRes)+ " " + unitAbbr+"\n");
					UI.setGlobalDataText(
							"    Area  = " + String.valueOf(area*xRes*xRes) + " " + unitAbbr +"^2\n");
				}
				UI.setGlobalDataText(
						"\nNuclei Statistics (" + unitAbbr + "): \n\n");
				UI.setGlobalDataText(
						"    Number of Nuclei  = " + String.valueOf(nStats[0])+ "\n");
				UI.setGlobalDataText(
						"    Minimum Size  = " + String.valueOf(nStats[4]*xRes*xRes)+ " " + unitAbbr+"^2\n");
				UI.setGlobalDataText(
						"    Maximum Size  = " + String.valueOf(nStats[5]*xRes*xRes)+ " " + unitAbbr+"^2\n");
				UI.setGlobalDataText(
						"    Average Size  = " + String.valueOf(nStats[2]*xRes*xRes)+ " " + unitAbbr+"^2\n");
				UI.setGlobalDataText(
						"    Std. Deviation  = " + String.valueOf(nStats[3]*xRes*xRes)+ " " + unitAbbr+"^2\n");
				UI.setGlobalDataText(
						"    Total Area  = " + String.valueOf(nStats[1]*xRes*xRes)+ " " + unitAbbr+"^2\n");
				
					
			}
			else UI.setGlobalDataText("Unable to complete the algorithm");
		}
		dispose();
	}
	
	protected void callAlgorithm() {
		try{
			//Retrieve selected images from the combo boxes and pass them into the algorithm
			
	        String rgbName = (String) rgbImages.getSelectedItem();
	        String boolName = (String) boolImages.getSelectedItem();
	        rgbImage = UI.getRegisteredImageByName(rgbName);
	        boolImage = UI.getRegisteredImageByName(boolName);
	        
	        int[] rgbExtents = rgbImage.getExtents();
	        int[] boolExtents = boolImage.getExtents();
	        
	        if((rgbExtents[0] != boolExtents[0]) && (rgbExtents[1] != boolExtents[1])){
	        	
	        	MipavUtil.displayError("Image extents are not equal");
	        	return; 
	        }
	        
			midAlg = new PlugInAlgorithmWallNucleiStats(rgbImage, boolImage);
			
			//Progress bar is useless right now
			createProgressBar(rgbImage.getImageName(), "Wall/Nuclei statistics calculation ...", midAlg);
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
	 * Populates the image combo boxes only with images allowed for the algorithm. 
	 * In this iteration: a wall image is (hopefully) and RGB image, while the
	 * mask image should be a boolean image. 
	**/
	private boolean buildImageBox(){

		Enumeration<String> names = UI.getRegisteredImageNames();
		
        rgbImages = new JComboBox<String>();
		boolImages = new JComboBox<String>();

		while (names.hasMoreElements()){

			String name = names.nextElement();
			ModelImage img = UI.getRegisteredImageByName(name);
			
			if (UI.getFrameContainingImage(img) != null) {
				
				if(img.isColorImage()) rgbImages.addItem(name);
				else if(img.getTypeString() == "Boolean") boolImages.addItem(name);
				
			}
			
		}
		
		//For error checking during init(). Need both the wall and mask image to continue
		
		if((rgbImages.getModel().getSize() == 0) && (boolImages.getModel().getSize() == 0)) {
            MipavUtil.displayWarning("Requires an RGB and a boolean mask image to begin");
            return false;
        }
		else if(rgbImages.getModel().getSize() == 0) {
            MipavUtil.displayWarning("Requires an RGB image to begin");
            return false;
        }
		else if(boolImages.getModel().getSize() == 0) {
            MipavUtil.displayWarning("Requires a boolean mask image to begin");
            return false;
        }
		
		return true;

	}
	
	private void init(){
		
		setForeground(Color.black);
        setTitle("Wall/Nuclei Statistics");
        // Build the Panel that holds the OK and CANCEL Buttons
        
        JPanel imPanel = new JPanel(new GridLayout(3, 3));
        imPanel.setForeground(Color.black);
        imPanel.setBorder(buildTitledBorder("Choose Images"));
        
        JLabel rgbNames = new JLabel("Wall Images");
        rgbNames.setForeground(Color.black);
        rgbNames.setFont(serif12);
        imPanel.add(rgbNames);
        
        imPanel.add(rgbImages);
        
        JLabel boolNames = new JLabel("Mask Images");
        boolNames.setForeground(Color.black);
        boolNames.setFont(serif12);
        imPanel.add(boolNames);
        
        imPanel.add(boolImages);
        
        getContentPane().add(imPanel, BorderLayout.NORTH);
        
        JPanel resPanel = new JPanel();
        resPanel.setForeground(Color.black);
        resPanel.setBorder(buildTitledBorder("Image Resolutions"));
        
        JLabel xRes = new JLabel("Resolution: ");
        xRes.setForeground(Color.black);
        xRes.setFont(serif12);
        resPanel.add(xRes);
        
        xResField = new JTextField(5);
        xResField.setText("1");
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

	
	
}
