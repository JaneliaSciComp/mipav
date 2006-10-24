package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;



/**
 * This class is the dialog that allows the user to edit the user defined file extensions filter
 *
 * @author  Nish Pandya
 */

public class JDialogEditUserDefinedFileTypes extends JDialogBase {
	
	/** The panels that make up this Dialog*/
    private JPanel displayPanel,applyClosePanel;
   
    /** The checkboxes for the file extensions that are supported */
    private JCheckBox ascFileTypeCB,aviFileTypeCB,bmpFileTypeCB,bxhFileTypeCB,classFileTypeCB,
    				  dcmFileTypeCB,funFileTypeCB,gifFileTypeCB,headFileTypeCB,icsFileTypeCB,
    				  imaFileTypeCB,imgFileTypeCB,jpgFileTypeCB,jpegFileTypeCB,lsmFileTypeCB,
    				  lutFileTypeCB,matFileTypeCB,mncFileTypeCB,mtxFileTypeCB,niiFileTypeCB,
    				  nltFileTypeCB,pcxFileTypeCB,picFileTypeCB,pictFileTypeCB,pltFileTypeCB,
    				  pngFileTypeCB,psdFileTypeCB,sctFileTypeCB,sigFileTypeCB,stkFileTypeCB,
    				  surFileTypeCB,tgaFileTypeCB,tifFileTypeCB,tiffFileTypeCB,voiFileTypeCB,
    				  wrlFileTypeCB,xbmFileTypeCB,xmlFileTypeCB,xpmFileTypeCB;
    
    /** This is the user input for additional file extensions */
    private JTextField userInput;
    
    /** This is a list of the checkbox names that is used for validation of user input to make sure there are no duplicates */
    private ArrayList checkboxNames = new ArrayList();
    
    /** This arraylist is the list of checked check boxes that is populated when user hits apply */
    private ArrayList checkedFileTypes = new ArrayList();
    
    /** This array is the list of additional file extensions that user typed in that is populated when user hits apply */
    private String[] userInputFileTypes = null;
    
    /** This array is the list of file extensions that is pulled from the Preferences. We need this in order
     * to check the appropriate check boxes and the user input text field when the user opens up this dialog initailly  */
    private String[] preferencesFileTypes = null;
    
    
    
    

	/** constructor */
	public JDialogEditUserDefinedFileTypes() {
		
		setTitle("Edit User Defined Filter");
		
		//get the user defined file types preferences
		if (Preferences.getProperty("userDefinedFileTypes") != null || !Preferences.getProperty("userDefinedFileTypes").trim().equals("")) {
			preferencesFileTypes = Preferences.getProperty("userDefinedFileTypes").split("; ");
		}
		
		displayPanel = new JPanel();
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		displayPanel.setLayout(gbl);
		
		ascFileTypeCB = new JCheckBox("*.asc");
		checkboxNames.add("*.acs");
		if (checkPreference("*.asc")) {
			ascFileTypeCB.setSelected(true);
		}
		renderCheckBox(ascFileTypeCB,gbl,gbc,0,0);
		
		aviFileTypeCB = new JCheckBox("*.avi");
		checkboxNames.add("*.avi");
		if (checkPreference("*.avi")) {
			aviFileTypeCB.setSelected(true);
		}
		renderCheckBox(aviFileTypeCB,gbl,gbc,1,0);
		
		bmpFileTypeCB = new JCheckBox("*.bmp");
		checkboxNames.add("*.bmp");
		if (checkPreference("*.bmp")) {
			bmpFileTypeCB.setSelected(true);
		}
		renderCheckBox(bmpFileTypeCB,gbl,gbc,2,0);
		
		bxhFileTypeCB = new JCheckBox("*.bxh");
		checkboxNames.add("bxh");
		if (checkPreference("*.bxh")) {
			bxhFileTypeCB.setSelected(true);
		}
		renderCheckBox(bxhFileTypeCB,gbl,gbc,3,0);
		
		classFileTypeCB = new JCheckBox("*.class");
		checkboxNames.add("*.class");
		if (checkPreference("*.class")) {
			classFileTypeCB.setSelected(true);
		}
		renderCheckBox(classFileTypeCB,gbl,gbc,4,0);
		
		dcmFileTypeCB = new JCheckBox("*.dcm");
		checkboxNames.add("*.dcm");
		if (checkPreference("*.dcm")) {
			dcmFileTypeCB.setSelected(true);
		}
		renderCheckBox(dcmFileTypeCB,gbl,gbc,0,1);
		
		funFileTypeCB = new JCheckBox("*.fun");
		checkboxNames.add("*.fun");
		if (checkPreference("*.fun")) {
			funFileTypeCB.setSelected(true);
		}
		renderCheckBox(funFileTypeCB,gbl,gbc,1,1);
		
		gifFileTypeCB = new JCheckBox("*.gif");
		checkboxNames.add("*.gif");
		if (checkPreference("*.gif")) {
			gifFileTypeCB.setSelected(true);
		}
		renderCheckBox(gifFileTypeCB,gbl,gbc,2,1);
		
		headFileTypeCB = new JCheckBox("*.head");
		checkboxNames.add("*.head");
		if (checkPreference("*.head")) {
			headFileTypeCB.setSelected(true);
		}
		renderCheckBox(headFileTypeCB,gbl,gbc,3,1);
		
		icsFileTypeCB = new JCheckBox("*.ics");
		checkboxNames.add("*.ics");
		if (checkPreference("*.ics")) {
			icsFileTypeCB.setSelected(true);
		}
		renderCheckBox(icsFileTypeCB,gbl,gbc,4,1);
		
		imaFileTypeCB = new JCheckBox("*.ima");
		checkboxNames.add("*.ima");
		if (checkPreference("*.ima")) {
			imaFileTypeCB.setSelected(true);
		}
		renderCheckBox(imaFileTypeCB,gbl,gbc,0,2);
		
		imgFileTypeCB = new JCheckBox("*.img");
		checkboxNames.add("*.img");
		if (checkPreference("*.img")) {
			imgFileTypeCB.setSelected(true);
		}
		renderCheckBox(imgFileTypeCB,gbl,gbc,1,2);
		
		jpgFileTypeCB = new JCheckBox("*.jpg");
		checkboxNames.add("*.jpg");
		if (checkPreference("*.jpg")) {
			jpgFileTypeCB.setSelected(true);
		}
		renderCheckBox(jpgFileTypeCB,gbl,gbc,2,2);
		
		jpegFileTypeCB = new JCheckBox("*.jpeg");
		checkboxNames.add("*.jpeg");
		if (checkPreference("*.jpeg")) {
			jpegFileTypeCB.setSelected(true);
		}
		renderCheckBox(jpegFileTypeCB,gbl,gbc,3,2);
		
		lsmFileTypeCB = new JCheckBox("*.lsm");
		checkboxNames.add("*.lsm");
		if (checkPreference("*.lsm")) {
			lsmFileTypeCB.setSelected(true);
		}
		renderCheckBox(lsmFileTypeCB,gbl,gbc,4,2);
		
		lutFileTypeCB = new JCheckBox("*.lut");
		checkboxNames.add("*.lut");
		if (checkPreference("*.lut")) {
			lutFileTypeCB.setSelected(true);
		}
		renderCheckBox(lutFileTypeCB,gbl,gbc,0,3);
		
		matFileTypeCB = new JCheckBox("*.mat");
		checkboxNames.add("*.mat");
		if (checkPreference("*.mat")) {
			matFileTypeCB.setSelected(true);
		}
		renderCheckBox(matFileTypeCB,gbl,gbc,1,3);
		
		mncFileTypeCB = new JCheckBox("*.mnc");
		checkboxNames.add("*.mnc");
		if (checkPreference("*.mnc")) {
			mncFileTypeCB.setSelected(true);
		}
		renderCheckBox(mncFileTypeCB,gbl,gbc,2,3);
		
		mtxFileTypeCB = new JCheckBox("*.mtx");
		checkboxNames.add("*.mtx");
		if (checkPreference("*.mtx")) {
			mtxFileTypeCB.setSelected(true);
		}
		renderCheckBox(mtxFileTypeCB,gbl,gbc,3,3);
		
		niiFileTypeCB = new JCheckBox("*.nii");
		checkboxNames.add("*.nii");
		if (checkPreference("*.nii")) {
			niiFileTypeCB.setSelected(true);
		}
		renderCheckBox(niiFileTypeCB,gbl,gbc,4,3);
		
		nltFileTypeCB = new JCheckBox("*.nlt");
		checkboxNames.add("*.nlt");
		if (checkPreference("*.nlt")) {
			nltFileTypeCB.setSelected(true);
		}
		renderCheckBox(nltFileTypeCB,gbl,gbc,0,4);
		
		pcxFileTypeCB = new JCheckBox("*.pcx");
		checkboxNames.add("*.pcx");
		if (checkPreference("*.pcx")) {
			pcxFileTypeCB.setSelected(true);
		}
		renderCheckBox(pcxFileTypeCB,gbl,gbc,1,4);
		
		picFileTypeCB = new JCheckBox("*.pic");
		checkboxNames.add("*.pic");
		if (checkPreference("*.pic")) {
			picFileTypeCB.setSelected(true);
		}
		renderCheckBox(picFileTypeCB,gbl,gbc,2,4);

		pictFileTypeCB = new JCheckBox("*.pict");
		checkboxNames.add("*.pict");
		if (checkPreference("*.pict")) {
			pictFileTypeCB.setSelected(true);
		}
		renderCheckBox(pictFileTypeCB,gbl,gbc,3,4);
		
		pltFileTypeCB = new JCheckBox("*.plt");
		checkboxNames.add("*.plt");
		if (checkPreference("*.plt")) {
			pltFileTypeCB.setSelected(true);
		}
		renderCheckBox(pltFileTypeCB,gbl,gbc,4,4);
		
		pngFileTypeCB = new JCheckBox("*.png");
		checkboxNames.add("*.png");
		if (checkPreference("*.png")) {
			pngFileTypeCB.setSelected(true);
		}
		renderCheckBox(pngFileTypeCB,gbl,gbc,0,5);
		
		psdFileTypeCB = new JCheckBox("*.psd");
		checkboxNames.add("*.psd");
		if (checkPreference("*.psd")) {
			psdFileTypeCB.setSelected(true);
		}
		renderCheckBox(psdFileTypeCB,gbl,gbc,1,5);
		
		sctFileTypeCB = new JCheckBox("*.sct");
		checkboxNames.add("*.sct");
		if (checkPreference("*.sct")) {
			sctFileTypeCB.setSelected(true);
		}
		renderCheckBox(sctFileTypeCB,gbl,gbc,2,5);
		
		sigFileTypeCB = new JCheckBox("*.sig");
		checkboxNames.add("*.sig");
		if (checkPreference("*.sig")) {
			sigFileTypeCB.setSelected(true);
		}
		renderCheckBox(sigFileTypeCB,gbl,gbc,3,5);
		
		stkFileTypeCB = new JCheckBox("*.stk");
		checkboxNames.add("*.stk");
		if (checkPreference("*.stk")) {
			stkFileTypeCB.setSelected(true);
		}
		renderCheckBox(stkFileTypeCB,gbl,gbc,4,5);
		
		surFileTypeCB = new JCheckBox("*.sur");
		checkboxNames.add("*.sur");
		if (checkPreference("*.sur")) {
			surFileTypeCB.setSelected(true);
		}
		renderCheckBox(surFileTypeCB,gbl,gbc,0,6);
		
		tgaFileTypeCB = new JCheckBox("*.tga");
		if (checkPreference("*.tga")) {
			tgaFileTypeCB.setSelected(true);
		}
		renderCheckBox(tgaFileTypeCB,gbl,gbc,1,6);
		
		tifFileTypeCB = new JCheckBox("*.tif");
		checkboxNames.add("*.tif");
		if (checkPreference("*.tif")) {
			tifFileTypeCB.setSelected(true);
		}
		renderCheckBox(tifFileTypeCB,gbl,gbc,2,6);
		
		tiffFileTypeCB = new JCheckBox("*.tiff");
		checkboxNames.add("*.tiff");
		if (checkPreference("*.tiff")) {
			tiffFileTypeCB.setSelected(true);
		}
		renderCheckBox(tiffFileTypeCB,gbl,gbc,3,6);
		
		voiFileTypeCB = new JCheckBox("*.voi");
		checkboxNames.add("*.voi");
		if (checkPreference("*.voi")) {
			voiFileTypeCB.setSelected(true);
		}
		renderCheckBox(voiFileTypeCB,gbl,gbc,4,6);
		
		wrlFileTypeCB = new JCheckBox("*.wrl");
		checkboxNames.add("*.wrl");
		if (checkPreference("*.wrl")) {
			wrlFileTypeCB.setSelected(true);
		}
		renderCheckBox(wrlFileTypeCB,gbl,gbc,0,7);
		
		xbmFileTypeCB = new JCheckBox("*.xbm");
		checkboxNames.add("*.xbm");
		if (checkPreference("*.xbm")) {
			xbmFileTypeCB.setSelected(true);
		}
		renderCheckBox(xbmFileTypeCB,gbl,gbc,1,7);
		
		xmlFileTypeCB = new JCheckBox("*.xml");
		checkboxNames.add("*.xml");
		if (checkPreference("*.xml")) {
			xmlFileTypeCB.setSelected(true);
		}
		renderCheckBox(xmlFileTypeCB,gbl,gbc,2,7);
		
		xpmFileTypeCB = new JCheckBox("*.xpm");
		checkboxNames.add("*.xpm");
		if (checkPreference("*.xpm")) {
			xpmFileTypeCB.setSelected(true);
		}
		renderCheckBox(xpmFileTypeCB,gbl,gbc,3,7);
		
		
		gbc.gridx = 0;
		gbc.gridy = 8;
		gbc.gridwidth = 5;
		gbc.insets = new Insets(10,5,10,5);
		JLabel label1 = new JLabel("Enter additional file extensions below. ( format: *.abc;*.def;*.ghi )");
		gbl.setConstraints(label1,gbc);
		displayPanel.add(label1);
		
		
		
		gbc.gridx = 0;
		gbc.gridy = 9;
		gbc.gridwidth = 5;
		gbc.insets = new Insets(10,5,10,5);
		userInput = new JTextField(30);

		
		if (Preferences.getProperty("userDefinedFileTypes_textField") != null) {
			userInput.setText(Preferences.getProperty("userDefinedFileTypes_textField"));
		}
		gbl.setConstraints(userInput,gbc);
		displayPanel.add(userInput);
		
		
		
		applyClosePanel = new JPanel();
		
		buildOKButton();
		OKButton.setText("Apply");
		applyClosePanel.add(OKButton, BorderLayout.WEST);
	    buildCancelButton();
	    cancelButton.setText("Cancel");
	    cancelButton.setActionCommand("close");
	    applyClosePanel.add(cancelButton, BorderLayout.CENTER);

		
		this.getContentPane().add(displayPanel,BorderLayout.CENTER);
		
		this.getContentPane().add(applyClosePanel,BorderLayout.SOUTH);
		
		
		
		pack();
        this.setResizable(false);
        setVisible(true);
		
	}

	
	
	
	
	/** The actionPerformed method
	 * 
	 * @param   event  the ActionEvent
	 * 
	 * */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		
		if (command.equalsIgnoreCase("apply")) {
			//do a bunch of stuff here
			checkedFileTypes = new ArrayList();
			
			if(ascFileTypeCB.isSelected()) {
				checkedFileTypes.add(ascFileTypeCB.getText());
			}
			if(aviFileTypeCB.isSelected()) {
				checkedFileTypes.add(aviFileTypeCB.getText());
			}
			if(bmpFileTypeCB.isSelected()) {
				checkedFileTypes.add(bmpFileTypeCB.getText());
			}
			if(bxhFileTypeCB.isSelected()) {
				checkedFileTypes.add(bxhFileTypeCB.getText());
			}
			if(classFileTypeCB.isSelected()) {
				checkedFileTypes.add(classFileTypeCB.getText());
			}
			if(dcmFileTypeCB.isSelected()) {
				checkedFileTypes.add(dcmFileTypeCB.getText());
			}
			if(funFileTypeCB.isSelected()) {
				checkedFileTypes.add(funFileTypeCB.getText());
			}
			if(gifFileTypeCB.isSelected()) {
				checkedFileTypes.add(gifFileTypeCB.getText());
			}
			if(headFileTypeCB.isSelected()) {
				checkedFileTypes.add(headFileTypeCB.getText());
			}
			if(icsFileTypeCB.isSelected()) {
				checkedFileTypes.add(icsFileTypeCB.getText());
			}
			if(imaFileTypeCB.isSelected()) {
				checkedFileTypes.add(imaFileTypeCB.getText());
			}
			if(imgFileTypeCB.isSelected()) {
				checkedFileTypes.add(imgFileTypeCB.getText());
			}
			if(jpgFileTypeCB.isSelected()) {
				checkedFileTypes.add(jpgFileTypeCB.getText());
			}
			if(jpegFileTypeCB.isSelected()) {
				checkedFileTypes.add(jpegFileTypeCB.getText());
			}
			if(lsmFileTypeCB.isSelected()) {
				checkedFileTypes.add(lsmFileTypeCB.getText());
			}
			if(lutFileTypeCB.isSelected()) {
				checkedFileTypes.add(lutFileTypeCB.getText());
			}
			if(matFileTypeCB.isSelected()) {
				checkedFileTypes.add(matFileTypeCB.getText());
			}
			if(mncFileTypeCB.isSelected()) {
				checkedFileTypes.add(mncFileTypeCB.getText());
			}
			if(mtxFileTypeCB.isSelected()) {
				checkedFileTypes.add(mtxFileTypeCB.getText());
			}
			if(niiFileTypeCB.isSelected()) {
				checkedFileTypes.add(niiFileTypeCB.getText());
			}
			if(nltFileTypeCB.isSelected()) {
				checkedFileTypes.add(nltFileTypeCB.getText());
			}
			if(pcxFileTypeCB.isSelected()) {
				checkedFileTypes.add(pcxFileTypeCB.getText());
			}
			if(picFileTypeCB.isSelected()) {
				checkedFileTypes.add(picFileTypeCB.getText());
			}
			if(pictFileTypeCB.isSelected()) {
				checkedFileTypes.add(pictFileTypeCB.getText());
			}
			if(pltFileTypeCB.isSelected()) {
				checkedFileTypes.add(pltFileTypeCB.getText());
			}
			if(pngFileTypeCB.isSelected()) {
				checkedFileTypes.add(pngFileTypeCB.getText());
			}
			if(psdFileTypeCB.isSelected()) {
				checkedFileTypes.add(psdFileTypeCB.getText());
			}
			if(sctFileTypeCB.isSelected()) {
				checkedFileTypes.add(sctFileTypeCB.getText());
			}
			if(sigFileTypeCB.isSelected()) {
				checkedFileTypes.add(sigFileTypeCB.getText());
			}
			if(stkFileTypeCB.isSelected()) {
				checkedFileTypes.add(stkFileTypeCB.getText());
			}
			if(surFileTypeCB.isSelected()) {
				checkedFileTypes.add(surFileTypeCB.getText());
			}
			if(tgaFileTypeCB.isSelected()) {
				checkedFileTypes.add(tgaFileTypeCB.getText());
			}
			if(tifFileTypeCB.isSelected()) {
				checkedFileTypes.add(tifFileTypeCB.getText());
			}
			if(tiffFileTypeCB.isSelected()) {
				checkedFileTypes.add(tiffFileTypeCB.getText());
			}
			if(voiFileTypeCB.isSelected()) {
				checkedFileTypes.add(voiFileTypeCB.getText());
			}
			if(wrlFileTypeCB.isSelected()) {
				checkedFileTypes.add(wrlFileTypeCB.getText());
			}
			if(xbmFileTypeCB.isSelected()) {
				checkedFileTypes.add(xbmFileTypeCB.getText());
			}
			if(xmlFileTypeCB.isSelected()) {
				checkedFileTypes.add(xmlFileTypeCB.getText());
			}
			if(xpmFileTypeCB.isSelected()) {
				checkedFileTypes.add(xpmFileTypeCB.getText());
			}
			
			
			
			StringBuffer sb =  new StringBuffer();
			
			//add thre checked box file types to the string buffer
			for(int i=0;i<checkedFileTypes.size();i++) {
				sb.append((String)checkedFileTypes.get(i));
				if (i != checkedFileTypes.size() - 1 ) {
					sb.append("; ");
				}
			}

			//determine if user entered any file extensions in the user input area..if so...validate and if passed, add on to string buffer
			if (!(userInput.getText().trim().equals(""))) {
				if (!validateUserInputString(userInput.getText().trim())) {
					MipavUtil.displayError("Additional file extensions must be unique and in the proper format. \n                                       ex. (*.abc;*.def)");
					userInput.setText("");
					return;
				}
				else {
					if(checkedFileTypes.size()>0) {
						sb.append("; ");
					}
					for(int i=0;i<userInputFileTypes.length;i++) {
						sb.append(userInputFileTypes[i]);
						if(i != userInputFileTypes.length -1) {
							sb.append("; ");
						}
					}
				}
			}
			
		
			//set the udef description
			if (checkedFileTypes.size() == 0 && userInputFileTypes == null) {
				ViewImageFileFilter.setUdefDescription("User Defined");
			}
			else {
				ViewImageFileFilter.setUdefDescription("User Defined (" + sb.toString() + ")");
			}
			
			//set the preferences
			Preferences.setProperty("userDefinedFileTypes",sb.toString());
			Preferences.setProperty("userDefinedFileTypes_textField",userInput.getText().trim());
			
			//set the cancel button text to 'close' since the changes were accepted
            cancelButton.setText("Close");
		} else if (command.equalsIgnoreCase("close")) { // close box
            dispose();
        }

	}
	
	
	
	/** This method renders the checkbox
	 * 
	 * @param   checkBox  the JCheckbox
	 * @param   gbLayout  the GridbagLayout
	 * @param   gbConstraints  the GridbagConstraints
	 * @param   x  the x position
	 * @param   y  the y position
	 *  
	 *  */
	public void renderCheckBox(JCheckBox checkBox, GridBagLayout gbLayout,GridBagConstraints gbConstraints, int x, int y ) {
		gbConstraints.gridx = x;
		gbConstraints.gridy = y;
		gbConstraints.anchor = GridBagConstraints.WEST;
		gbConstraints.insets = new Insets(0,5,0,5);
		gbLayout.setConstraints(checkBox,gbConstraints);
		displayPanel.add(checkBox);
		
	}
	
	
	/** This method validates the user input file extensions.  
	 * It alllows combinations of letters (upper and lowere) and numbers in the file extension
	 * also makes sure that user input names do not duplicate any checkbox names
	 * 
	 * @param inputString the string to validate
	 * */
	public boolean validateUserInputString(String inputString) {
		userInputFileTypes = inputString.split(";");
		for(int i=0;i<userInputFileTypes.length;i++) {
			if (!(userInputFileTypes[i].trim().matches("\\*\\.\\w+?"))) {
				userInputFileTypes = null;
				return false;
			}
			for(int k=0;k<checkboxNames.size();k++) {
				if(userInputFileTypes[i].trim().equals((String)checkboxNames.get(k))) {
					userInputFileTypes = null;
					return false;
				}
			}
			//this is just in case they separate the file extensions with spaces
			userInputFileTypes[i] = userInputFileTypes[i].trim();
		}
		return true;
	}
	
	
	/** This method does the check with the preferencesFileTypes array to determine if the checkbox should be checked initalially
     * 
     * @param   fileType  the file extension to check */
    public boolean checkPreference(String fileType) {

    	if (preferencesFileTypes != null) {
    		for (int i=0;i<preferencesFileTypes.length;i++) {
    			if (fileType.equals(preferencesFileTypes[i])) {
    				return true;
    			}
    		}
    		return false;
    	}
    	else {
    		return false;
    	}

    }
	
	
	
	
	

}
