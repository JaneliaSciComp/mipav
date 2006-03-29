package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;


import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.vecmath.Point3i;

/** 
*   
*   Dialog box for the paint power tools: morphology operations, object delete, etc.
*
*
*	@version    May 2005
*	@author     Pierre-Louis Bazin
*   @see        JDialogBase
*   @see        AlgorithmInterface
*
*
*/  
public class JDialogMultiPaint extends JDialogBase implements 
AlgorithmInterface {
    
    private     ModelImage              image;					// source image
	private     ModelImage              resultImage = null;		// result image
    private     int                     destExtents[];
    private 	ViewUserInterface       userInterface;
    private     String                  title;
	private		boolean					useTriplanar=false;
	private		BitSet					currentMask;	
	private		int						alphaBlend = 50;
	
	private		int			selected = 1;				// id for the selected paint mask
	private		int			nbx = 4, nby = 6;			// number of paint masks
	private		String[]	label;						// labels for the painted objects
	private		boolean[]	preserved;					// check whether the mask can be affected by new paint or not 	
	private		Color[]		color;						// colors to use for the labels 	
	private		boolean		displayMask,displayPaint;	// check whether the mask and paint are displayed	
	private		boolean		editable;					// wether or not you can edit the object names
	private		Color		nullColor = Color.gray;
	
    // dialog elements
	private 	JPanel  	mainPanel;
    
	private		JPanel			multiPanel;
	private		BorderedButton[]	multiButton;
	
	private		JPanel		optionPanel;
	private		JLabel		numberLabel;
	private		JTextField	numberXField;
	private		JTextField	numberYField;
	private		JButton		resizeButton;
	private		JPanel		numberPanel;
	private		JButton		loadLabelsButton;
	private		JButton		saveLabelsButton;
	private		JButton		loadMaskButton;
	private		JButton		saveMaskButton;
	private		JCheckBox	editBox;
	private		JPanel		filePanel;
	private		JPanel		maskPanel;
	
	private		JPanel		 listPanel;
	private		JCheckBox[]	 preserveBox;
	private		JTextField[] labelField;
	private		JToggleButton[]	listButton;
	
	private		JToggleButton	displayModeButton;
	private		JToggleButton	displayPaintButton;
	private		JToggleButton	displayMasksButton;
	private 	JPanel			displayPanel;
	private 	JPanel			displayedPanel;
	
	private 	JPanel		bottomPanel;

	private	JFileChooser			loadDialog;
	private	JFileChooser			saveDialog;
	
	
    /**
    *  Creates dialog for plugin.
    *  @param parent          Parent frame.
    *  @param im              Source image.
    */
    public JDialogMultiPaint(Frame theParentFrame, ModelImage im) {
		super(theParentFrame, false);
		userInterface = ViewUserInterface.getReference();	    
    	image = im;
		init();
	}
    
    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param im	Source image.
     */
    public JDialogMultiPaint(ModelImage im)
    {
    	super();
    	userInterface = ViewUserInterface.getReference();
    	image = im;
    }
	
    /**
    *	@deprecated
    *   Used primarily for the script to store variables and run the algorithm.  No
    *	actual dialog will appear but the set up info and result image will be stored here.
    *	@param UI   The user interface, needed to create the image frame.
    *	@param im	Source image.
    */
    public JDialogMultiPaint(ViewUserInterface UI, ModelImage im) {
        this(im);
    }
    
    /**
    *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
    */
	private void init() {
        setForeground(Color.black);
        setTitle("Multiple Paint Tools");
		
		// multiple masks/paint
		label = new String[nbx*nby+1];
		labelField = new JTextField[nbx*nby+1];
		multiButton = new BorderedButton[nbx*nby+1];
		listButton = new JToggleButton[nbx*nby+1];
		preserved = new boolean[nbx*nby+1];
		preserveBox = new JCheckBox[nbx*nby+1];
		color = new Color[nbx*nby+1];
		for (int n=1;n<nbx*nby+1;n++) {
			label[n] = new String("Label "+n);
			labelField[n] = new JTextField(5);
			labelField[n].setText(label[n]);
			labelField[n].setFont(serif12);
			labelField[n].addActionListener(this);
			labelField[n].setActionCommand("Label "+n);
			
			multiButton[n] = new BorderedButton(String.valueOf(n));
			multiButton[n].addActionListener(this);
			multiButton[n].setActionCommand("PaintMask "+n);
			multiButton[n].setFont(MipavUtil.font10);
			multiButton[n].setSelected(false);
			multiButton[n].setMaximumSize(new Dimension(48,20));
			multiButton[n].setPreferredSize(new Dimension(48,20));		
			multiButton[n].setToolTipText(label[n]);
						
			listButton[n] = new JToggleButton(String.valueOf(n));
			listButton[n].addActionListener(this);
			listButton[n].setActionCommand("PaintMask "+n);
			listButton[n].setFont(MipavUtil.font10);
			listButton[n].setSelected(false);
			listButton[n].setMaximumSize(new Dimension(50,18));
			listButton[n].setPreferredSize(new Dimension(50,18));	
			listButton[n].setBackground(nullColor);	
						
			color[n] = nullColor;
			
			preserved[n] = false;
			preserveBox[n] = new JCheckBox("");
			preserveBox[n].addActionListener(this);
			preserveBox[n].setActionCommand("Preserve "+n);
			preserveBox[n].setToolTipText("Lock the paint mask");
		}	
		multiButton[selected].setSelected(true);
		listButton[selected].setSelected(true);
		
		// edit, load and save palettes
		editBox = new JCheckBox("Edit");
		editBox.addActionListener(this);
		editBox.setActionCommand("Editable");
		editBox.setToolTipText("Edit the labels");
		editBox.setSelected(true);

		loadLabelsButton = new JButton("Load labels");
        loadLabelsButton.addActionListener(this);
        loadLabelsButton.setActionCommand("Load");
		loadLabelsButton.setFont(serif12);
		loadLabelsButton.setToolTipText("Load a palette");       
		
		saveLabelsButton = new JButton("Save labels");
        saveLabelsButton.addActionListener(this);
        saveLabelsButton.setActionCommand("Save");
		saveLabelsButton.setFont(serif12);
		saveLabelsButton.setToolTipText("Save current palette");       
		
		loadMaskButton = new JButton("Load masks");
        loadMaskButton.addActionListener(this);
        loadMaskButton.setActionCommand("LoadMask");
		loadMaskButton.setFont(serif12);
		loadMaskButton.setToolTipText("Load a mask");       
		
		saveMaskButton = new JButton("Save masks");
        saveMaskButton.addActionListener(this);
        saveMaskButton.setActionCommand("SaveMask");
		saveMaskButton.setFont(serif12);
		saveMaskButton.setToolTipText("Save current mask");       
		
		// customize the mask palette
		numberLabel = new JLabel("Number of masks: ");
		numberLabel.setForeground(Color.black);
		numberLabel.setFont(serif12);
		numberLabel.setToolTipText("Specify the number of labels (col x row)");       
		
		numberXField = new JTextField(3);
		numberXField.setText(String.valueOf(nbx));
		numberXField.setFont(serif12);
		
		numberYField = new JTextField(3);
		numberYField.setText(String.valueOf(nby));
		numberYField.setFont(serif12);
		
		resizeButton = new JButton("Resize");
        resizeButton.addActionListener(this);
        resizeButton.setActionCommand("Resize");
		resizeButton.setFont(serif12);
		resizeButton.setToolTipText("Resize the palette to the appropriate dimensions");       
		
		// display options
		displayModeButton = new JToggleButton("Switch Mode");
        displayModeButton.addActionListener(this);
        displayModeButton.setActionCommand("SwitchMode");
		displayModeButton.setFont(serif12);
		displayModeButton.setToolTipText("Switch between the compact palette and the detailed list modes");       
		
		displayPaintButton = new JToggleButton("Hide paint");
        displayPaintButton.addActionListener(this);
        displayPaintButton.setActionCommand("HidePaint");
		displayPaintButton.setFont(serif12);
		displayPaintButton.setToolTipText("Hide the paint mask for visualization");       
		
		displayMasksButton = new JToggleButton("Hide masks");
        displayMasksButton.addActionListener(this);
        displayMasksButton.setActionCommand("HideMasks");
		displayMasksButton.setFont(serif12);
		displayMasksButton.setToolTipText("Hide inactive masks for visualization");       
		
		GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(2, 2, 2, 2);

        multiPanel = new JPanel(new GridBagLayout());
        multiPanel.setBorder(buildTitledBorder("Paint Mask Palette"));

        gbc.insets = new Insets(0, 0, 0, 0);
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridx = i;
			gbc.gridy = j;
			gbc.gridwidth = 1;
			gbc.weightx = 1;
			gbc.fill = gbc.HORIZONTAL;
			multiPanel.add(multiButton[i+nbx*j+1], gbc);
		}
		
		listPanel = new JPanel(new GridBagLayout());
        listPanel.setBorder(buildTitledBorder("Label list"));
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridy = i+nbx*j;
			gbc.gridwidth = 1;
			gbc.gridx = 0;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(listButton[i+nbx*j+1], gbc);
			gbc.gridx = 1;
			gbc.weightx = 1;
			gbc.fill = gbc.HORIZONTAL;
			listPanel.add(labelField[i+nbx*j+1], gbc);
			gbc.gridx = 2;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(preserveBox[i+nbx*j+1], gbc);		
		}
		listPanel.setVisible(false);
		
        optionPanel = new JPanel(new GridBagLayout());
        optionPanel.setBorder(buildTitledBorder("Options"));
		
		numberPanel = new JPanel(new GridBagLayout());
		gbc.gridy =0;
		gbc.gridwidth = 1;
		gbc.gridx = 0;
		gbc.weightx = 1;
		gbc.fill = gbc.HORIZONTAL;
		numberPanel.add(numberLabel, gbc);
		gbc.weightx = 0;
		gbc.fill = gbc.NONE;
		gbc.gridx = 1;
		numberPanel.add(numberXField, gbc);
		gbc.gridx = 2;
		numberPanel.add(numberYField, gbc);
		gbc.gridx = 3;
		numberPanel.add(resizeButton, gbc);
		
		filePanel = new JPanel(new GridBagLayout());
		gbc.gridx = 0;
		gbc.weightx = 1;
		gbc.fill = gbc.HORIZONTAL;
		filePanel.add(loadLabelsButton, gbc);
		gbc.gridx = 1;
		gbc.fill = gbc.HORIZONTAL;
		filePanel.add(saveLabelsButton, gbc);
		gbc.gridx = 2;
		gbc.fill = gbc.HORIZONTAL;
		//filePanel.add(editBox, gbc);
		
		maskPanel = new JPanel(new GridBagLayout());
		gbc.gridx = 0;
		gbc.weightx = 1;
		gbc.fill = gbc.HORIZONTAL;
		maskPanel.add(loadMaskButton, gbc);
		gbc.gridx = 1;
		gbc.fill = gbc.HORIZONTAL;
		maskPanel.add(saveMaskButton, gbc);
		
		displayPanel = new JPanel(new GridBagLayout());
		gbc.gridx = 0;
		gbc.fill = gbc.HORIZONTAL;
		displayPanel.add(displayModeButton, gbc);
		gbc.gridx = 1;
		displayPanel.add(displayPaintButton, gbc);
		gbc.gridx = 2;
		displayPanel.add(displayMasksButton, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		optionPanel.add(numberPanel, gbc);
		gbc.gridy = 1;
		optionPanel.add(filePanel, gbc);
		gbc.gridy = 2;
		optionPanel.add(maskPanel, gbc);
		gbc.gridy = 3;
		optionPanel.add(displayPanel, gbc);
		
		displayedPanel = multiPanel;
		
		mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        
        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = gbc.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
		mainPanel.add(multiPanel, gbc);
		gbc.gridy = 1;
        mainPanel.add(optionPanel, gbc);
		gbc.gridy = 2;
        mainPanel.add(listPanel, gbc);
		
        bottomPanel = new JPanel();
		bottomPanel.add(buildCloseButton());
        bottomPanel.add(buildHelpButton());
		
        getContentPane().add(mainPanel);
        getContentPane().add(bottomPanel, BorderLayout.SOUTH);
		
		initBlankPaint(1);
		
		pack();
        setVisible(true); 
		setResizable(false);
    	System.gc();		
	} // end init()
	
	private void buildLoadDialog() {
        loadDialog = new javax.swing.JFileChooser();
        loadDialog.setDialogTitle("Load label file");
        loadDialog.setDialogType(JFileChooser.OPEN_DIALOG);
        loadDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        loadDialog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadFileActionPerformed(evt);
            }
        });
        loadDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
        loadDialog.showOpenDialog(this);
	}
	private void buildSaveDialog() {
        saveDialog = new javax.swing.JFileChooser();
        saveDialog.setDialogTitle("Save label file");
        saveDialog.setDialogType(JFileChooser.SAVE_DIALOG);
        saveDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        saveDialog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveFileActionPerformed(evt);
            }
        });
        saveDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
        saveDialog.showSaveDialog(this);
	}
	
    /**
    *	Accessor that sets the parameters
    */
    public void setParameters() {
    }
	
    //************************************************************************
    //************************** Event Processing ****************************
    //************************************************************************

	/**
	*  Closes dialog box when the OK button is pressed and calls the algorithm.
	*  @param event       Event that triggers function.
	*/
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		
			
		if (command.equals("Close")) {
 			dispose();
		}  else if (command.equals("Help")) {
            //MipavUtil.showHelp("10027");
        } else if (command.startsWith("PaintMask")) {
			//System.out.println("(selected: "+selected);
			int num = Integer.valueOf(command.substring( command.indexOf(" ")+1 ) ).intValue();
			// convert the paint to previous selection 
			// and the newly selected mask to paint
			switchPaintAndMask(selected,num);
		} else if (command.startsWith("Preserve")) {
			int num = Integer.valueOf(command.substring( command.indexOf(" ")+1 ) ).intValue();
			//System.out.println("preserve "+num);
			if (preserveBox[num].isSelected()) preserved[num] = true;
			else preserved[num] = false;
		} else if (command.startsWith("Label")) {
			int num = Integer.valueOf(command.substring( command.indexOf(" ")+1 ) ).intValue();
			//System.out.println("label "+num);
			label[num] = labelField[num].getText();
			multiButton[num].setToolTipText(label[num]);
		} else if (command.equals("SwitchMode")) {
			if (displayModeButton.isSelected()) { 
				multiPanel.setVisible(false); 
				listPanel.setVisible(true);
			} else {
				// refresh the text tooltips
				for (int n=1;n<nbx*nby+1;n++) {
					label[n] = labelField[n].getText();
					multiButton[n].setToolTipText(label[n]);
				}
				listPanel.setVisible(false);
				multiPanel.setVisible(true); 
			}
			pack();
			repaint();
		} else if (command.equals("Editable")) {
			if (editBox.isSelected()) {
				for (int n=1;n<nbx*nby+1;n++) labelField[n].setEditable(true);
			} else {
				for (int n=1;n<nbx*nby+1;n++) labelField[n].setEditable(false);				
			}
		} else if (command.equals("Resize")) {
			int Nbx = Integer.valueOf(numberXField.getText()).intValue();
			int Nby = Integer.valueOf(numberYField.getText()).intValue();
			
			//resetLabelList(Nbx,Nby);
			newLabelList(Nbx,Nby);
			refreshLabelDisplay();
			
		} else if (command.equals("HidePaint")) {
			if (displayPaintButton.isSelected()) {
				currentMask = (BitSet)image.getParentFrame().getComponentImage().getPaintMask().clone();
				refreshImagePaint(image,new BitSet());
			} else {
				refreshImagePaint(image,currentMask);
			}
		} else if (command.equals("HideMasks")) {
			if (displayMasksButton.isSelected()) {
				alphaBlend = (int)(image.getParentFrame().getComponentImage().getAlphaBlend()*100.0f);
				//System.out.print(" alpha: "+alphaBlend);
				image.getParentFrame().getComponentImage().setAlphaBlend(100);
			} else {
				image.getParentFrame().getComponentImage().setAlphaBlend(alphaBlend);
			}
			refreshImagePaint(image, image.getParentFrame().getComponentImage().getPaintMask());
		} else if (command.equals("Save")) {
			buildSaveDialog();
            saveDialog.setSize(500,326);
		} else if (command.equals("Load")) {
			buildLoadDialog();
            loadDialog.setSize(500,326);
		} else if (command.equals("SaveMask")) {
			int num = selected;
			commitPaintToMask(num);
			image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "SaveMask"));
			selectedMaskToPaint(num);
		} else if (command.equals("LoadMask")) {
			deselectMask();
			image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "OpenMask"));
			
			//if (image.getParentFrame().getImageB() != null)
			//{
				selectedMaskToPaint(1);
			//	System.out.println("imageB is not null");
			//}
			//else System.out.println("imageB is null");
		} 
    }

	public void loadFileActionPerformed(ActionEvent evt) {
		if ( JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand()) ) {
			String filename = loadDialog.getSelectedFile().getAbsolutePath();
			userInterface.setGlobalDataText("data file: "+filename+"\n");
			readLabelsFromFile(filename);
			userInterface.setGlobalDataText("label data loaded from "+filename+"\n");
		}
		//loadDialog.setVisible(false);
	}

 	public void saveFileActionPerformed(ActionEvent evt) {
		if ( JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand()) ) {
			String filename = saveDialog.getSelectedFile().getAbsolutePath();
			writeLabelsToFile(filename);
			userInterface.setGlobalDataText("label data saved to "+filename+"\n");
		}
		//saveDialog.setVisible(false);
	}
	
    /**
     *	read/write to a text file
     */
    public void writeLabelsToFile(String filename) {
        try {
            // open the file for writing
            File f = new File(filename);
            FileWriter fw = new FileWriter(f);
            PrintWriter pw = new PrintWriter(fw);
            // write the parameters
            pw.println("Labels for MultiPaint (edit with care)");
            pw.println("Number of labels: "+nbx+" x "+nby);
            pw.println("Id | name | color");
            
			for (int n=1;n<nbx*nby+1;n++) {
				// make sure the labels are consistent with the display
				label[n] = labelField[n].getText();
				pw.println(""+n+" | "+label[n]+" | "+color[n].getRGB());
			}
            // close the file
            fw.close();
        }
        catch (FileNotFoundException e) {
            Preferences.debug(e.getMessage());
        }
        catch (IOException e) {
            Preferences.debug(e.getMessage());
        }

    }

    /**
     *	read/write to a text file
     */
	public void readLabelsFromFile(String filename) {
		try {
            File f = new File(filename);
            FileReader fr = new FileReader(f);
            BufferedReader br = new BufferedReader(fr);
            String line = br.readLine();
			int num;
            // Exact corresponding template
            if (!line.equals("Labels for MultiPaint (edit with care)")) {
                Preferences.debug("not a proper label file");
                br.close();
                fr.close();
                return;
            }
            line = br.readLine();
            // numbers
            if (line.startsWith("Number of labels: ")) {
                int Nbx = Integer.valueOf(line.substring( line.indexOf(":")+2, line.indexOf("x")-1 ) ).intValue();
                int Nby = Integer.valueOf(line.substring( line.indexOf("x")+2 ) ).intValue();
				numberXField.setText(""+Nbx);
				numberYField.setText(""+Nby);
			
				if (selected>Nbx*Nby) {
					commitPaintToMask(selected);
				}

				newLabelList(Nbx,Nby);
				
				line = br.readLine();
                for (int n=1;n<nbx*nby+1;n++) {
					line = br.readLine();
					num = Integer.valueOf(line.substring( 0, line.indexOf("|")-1 ) ).intValue();
					if (num==n) {
						label[n] = line.substring( line.indexOf("|")+2,line.lastIndexOf("|")-1 );
						color[n] = new Color(Integer.valueOf(line.substring( line.lastIndexOf("|")+2 ) ).intValue());
					}
				}
				
				refreshLabelDisplay();
            }
            br.close();
            fr.close();
			// fire a size change information
			
        }
        catch (FileNotFoundException e) {
            Preferences.debug(e.getMessage());
        }
        catch (IOException e) {
            Preferences.debug(e.getMessage());
        }
        return ;
    }

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************
    
	/** switch Mask and Paint */
	private void switchPaintAndMask(int from, int to) {
		multiButton[from].setSelected(false);
		listButton[from].setSelected(false);
			
		if (image==null) {
			System.gc();
			MipavUtil.displayError("image not found");
			return;
		}
		
		// retrieve the mask
		BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
		if (obj==null) {
			MipavUtil.displayError("paint mask not found");
			return;
		}
		
		// select the ID: if not new, update the mask
		if (image.getParentFrame().getImageB()==null) {
			// create the mask image
			image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
		}
		
		// record selected image; set to image B
		ModelImage active = image.getParentFrame().getActiveImage();
		image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);
		
		// create new color
		color[from] = image.getParentFrame().getLUTb().getColor(from);
		multiButton[from].setBackground(color[from]);
		listButton[from].setBackground(color[from]);
		
		// call the paint to mask program for exiting mask		
		image.getParentFrame().getComponentImage().setIntensityDropper( (float)from );
		image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "CommitPaint"));
		
		// call the mask to paint program for starting mask
		color[to] = image.getParentFrame().getLUTb().getColor(to);
		image.getParentFrame().getComponentImage().setIntensityDropper( (float)to );
		image.getParentFrame().getControls().getTools().setPaintColor(color[to] );
		image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "MaskToPaint"));

		
		// reset the active image and intensity label
		if (!active.equals(image.getParentFrame().getActiveImage())) 
			image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
		
		selected = to;
		multiButton[selected].setSelected(true);
		listButton[selected].setSelected(true);

		refreshImagePaint(image, obj);	
	}
	
	/** init Paint Mask */
	private void initBlankPaint(int num) {
		if (image==null) {
			System.gc();
			MipavUtil.displayError("image not found");
			return;
		}
		
		// retrieve the mask
		BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
		if (obj==null) {
			MipavUtil.displayError("paint mask not found");
			return;
		}
		
		// select the ID: if not new, update the mask
		if (image.getParentFrame().getImageB()==null) {
			// create the mask image
			image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
		}
		
		// record selected image; set to image B
		ModelImage active = image.getParentFrame().getActiveImage();
		image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);
		
		// create new color
		color[num] = image.getParentFrame().getLUTb().getColor(num);
		multiButton[num].setBackground(color[num]);
		listButton[num].setBackground(color[num]);
		
		// call the paint to mask program for exiting mask		
		image.getParentFrame().getComponentImage().setIntensityDropper( (float)num );
		image.getParentFrame().getControls().getTools().setPaintColor(color[num] );
		
		// reset the active image and intensity label
		if (!active.equals(image.getParentFrame().getActiveImage())) 
			image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
		
		selected = num;
		multiButton[selected].setSelected(true);
		listButton[selected].setSelected(true);

		refreshImagePaint(image, obj);	
	}
	
	/** init Paint Mask */
	private void selectedMaskToPaint(int num) {
		if (image==null) {
			System.gc();
			MipavUtil.displayError("MultiPaint error: image not found");
			return;
		}
		
		// retrieve the mask
		BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
		if (obj==null) {
			MipavUtil.displayError("MultiPaint error: paint mask not found");
			return;
		}
		
		// select the ID: if not new, update the mask
		if (image.getParentFrame().getImageB()==null) {
			// create the mask image
			//Runnable newMaskThread = new Runnable() {
			//	public void run() {
			//System.out.println("creating newmask");
					image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
			//	}
			//};
			
			//try
			//{
			//	SwingUtilities.invokeAndWait(newMaskThread);
			//}
			//catch (Exception e)
			//{
			//	return;
			//}
			
		}
		else
		{
			//System.out.println("not creating newmask");
		}
		
		// record selected image; set to image B
		ModelImage active = image.getParentFrame().getActiveImage();
		
		System.out.println("1. imageB == " + (image.getParentFrame().getImageB() == null ? "null" : "not null"));
		
		image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);
		
		System.out.println("2. imageB == " + (image.getParentFrame().getImageB() == null ? "null" : "not null"));
		
		// create new color
		color[num] = image.getParentFrame().getLUTb().getColor(num);
		multiButton[num].setBackground(color[num]);
		listButton[num].setBackground(color[num]);
		
		// call the paint to mask program for exiting mask		
		image.getParentFrame().getComponentImage().setIntensityDropper( (float)num );
		image.getParentFrame().getControls().getTools().setPaintColor(color[num] );
		image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "MaskToPaint"));

		// reset the active image and intensity label
		if (!active.equals(image.getParentFrame().getActiveImage())) 
			image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
		
		selected = num;
		multiButton[selected].setSelected(true);
		listButton[selected].setSelected(true);

		refreshImagePaint(image, obj);	
	}
	
	/** set Paint as Mask */
	private void commitPaintToMask(int num) {
		multiButton[num].setSelected(false);
		listButton[num].setSelected(false);
			
		if (image==null) {
			System.gc();
			MipavUtil.displayError("image not found");
			return;
		}
		
		// retrieve the mask
		BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
		if (obj==null) {
			MipavUtil.displayError("paint mask not found");
			return;
		}
		
		// select the ID: if not new, update the mask
		if (image.getParentFrame().getImageB()==null) {
			// create the mask image
			image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
		}
		
		// record selected image; set to image B
		ModelImage active = image.getParentFrame().getActiveImage();
		image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);
		
		// create new color
		color[num] = image.getParentFrame().getLUTb().getColor(num);
		multiButton[num].setBackground(color[num]);
		listButton[num].setBackground(color[num]);
		
		// call the paint to mask program for exiting mask		
		image.getParentFrame().getComponentImage().setIntensityDropper( (float)num );
		image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "CommitPaint"));
		
		selected = 0;
		
		refreshImagePaint(image, obj);	
	}
	
	/** reset Paint as Mask */
	private void deselectMask() {
		multiButton[selected].setSelected(false);
		listButton[selected].setSelected(false);	
		selected = 0;
		refreshImagePaint(image, new BitSet());
	}
	
	/** refresh the displayed mask */
	private void refreshImagePaint(ModelImage img, BitSet obj) {
		// replace it by previous
		img.getParentFrame().getComponentImage().setPaintMask(obj);
		img.setMask(obj);
		// show result
		img.getParentFrame().updateImages(true);	
		if (img.getTriImageFrame()!=null) {
			img.getTriImageFrame().getTriImage(0).setPaintMask(obj);
			img.getTriImageFrame().getTriImage(1).setPaintMask(obj);
			img.getTriImageFrame().getTriImage(2).setPaintMask(obj);
			img.getTriImageFrame().updateImages(true);
		}
	}
	
	private void resetLabelList(int Nbx, int Nby) {
		String[] newlabel = new String[Nbx*Nby+1];
		JTextField[] newlabelField = new JTextField[Nbx*Nby+1];
		BorderedButton[] newmultiButton = new BorderedButton[Nbx*Nby+1];
		JToggleButton[] newlistButton = new JToggleButton[Nbx*Nby+1];
		boolean[] newpreserved = new boolean[Nbx*Nby+1];
		Color[]	newcolor = new Color[Nbx*Nby+1];
		JCheckBox[] newpreserveBox = new JCheckBox[Nbx*Nby+1];
		for (int n = 1; n<Nbx*Nby+1;n++) {
			if (n < nbx*nby+1) {
				newlabel[n] = label[n];	
				newlabelField[n] = labelField[n];
				newmultiButton[n] = multiButton[n];
				newlistButton[n] = listButton[n];
				newpreserved[n] = preserved[n];
				newcolor[n] = color[n];
				newpreserveBox[n] = preserveBox[n];
			} else {
				newlabel[n] = new String("Label "+n);
				newlabelField[n] = new JTextField(5);
				newlabelField[n].setText(newlabel[n]);
				newlabelField[n].setFont(serif12);
				newlabelField[n].addActionListener(this);
				newlabelField[n].setActionCommand("Label "+n);
				
				newmultiButton[n] = new BorderedButton(String.valueOf(n));
				newmultiButton[n].addActionListener(this);
				newmultiButton[n].setActionCommand("PaintMask "+n);
				newmultiButton[n].setFont(MipavUtil.font10);
				newmultiButton[n].setSelected(false);
				newmultiButton[n].setMaximumSize(new Dimension(48,20));
				newmultiButton[n].setPreferredSize(new Dimension(48,20));		
				newmultiButton[n].setToolTipText(newlabel[n]);
							
				newlistButton[n] = new JToggleButton(String.valueOf(n));
				newlistButton[n].addActionListener(this);
				newlistButton[n].setActionCommand("PaintMask "+n);
				newlistButton[n].setFont(MipavUtil.font10);
				newlistButton[n].setSelected(false);
				newlistButton[n].setMaximumSize(new Dimension(50,18));
				newlistButton[n].setPreferredSize(new Dimension(50,18));		
				
				newcolor[n] = nullColor;
				
				newpreserved[n] = false;
				newpreserveBox[n] = new JCheckBox("");
				newpreserveBox[n].addActionListener(this);
				newpreserveBox[n].setActionCommand("Preserve "+n);
				newpreserveBox[n].setToolTipText("Lock the paint mask");					
			}
		}
		nbx = Nbx; nby = Nby;
		label = newlabel;
		labelField = newlabelField;
		multiButton = newmultiButton;
		listButton = newlistButton;
		preserved = newpreserved;
		color = newcolor;
		preserveBox = newpreserveBox;
		
		mainPanel.remove(multiPanel);
		
		multiPanel = new JPanel(new GridBagLayout());
		multiPanel.setBorder(buildTitledBorder("Mask Palette"));

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = gbc.WEST;
		gbc.fill = gbc.HORIZONTAL;
		gbc.insets = new Insets(0, 0, 0, 0);
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridx = i;
			gbc.gridy = j;
			multiPanel.add(multiButton[i+nbx*j+1], gbc);
		}
		
		mainPanel.remove(listPanel);

		listPanel = new JPanel(new GridBagLayout());
		listPanel.setBorder(buildTitledBorder("Label list"));
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridy = i+nbx*j;
			gbc.gridwidth = 1;
			gbc.gridx = 0;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(listButton[i+nbx*j+1], gbc);
			gbc.gridx = 1;
			gbc.weightx = 1;
			gbc.fill = gbc.HORIZONTAL;
			listPanel.add(labelField[i+nbx*j+1], gbc);
			gbc.gridx = 2;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(preserveBox[i+nbx*j+1], gbc);		
		}
	
		gbc.gridx = 0;
		gbc.gridwidth = 1;
		gbc.gridy = 0;
		gbc.fill = gbc.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		mainPanel.add(multiPanel, gbc);
		gbc.gridy = 2;
		mainPanel.add(listPanel, gbc);
	
		if (displayModeButton.isSelected()) { 
			multiPanel.setVisible(false); 
			listPanel.setVisible(true);
		} else {
			listPanel.setVisible(false);
			multiPanel.setVisible(true); 
		}
		pack();
		repaint();
	}
	
	private void newLabelList(int Nbx, int Nby) {
		String[] newlabel = new String[Nbx*Nby+1];
		boolean[] newpreserved = new boolean[Nbx*Nby+1];
		Color[]	newcolor = new Color[Nbx*Nby+1];
		for (int n = 1; n<Nbx*Nby+1;n++) {
			if (n < nbx*nby+1) {
				newlabel[n] = label[n];	
				newpreserved[n] = preserved[n];
				newcolor[n] = color[n];
			} else {
				newlabel[n] = new String("Label "+n);
				newcolor[n] = nullColor;
				newpreserved[n] = false;
			}
		}
		nbx = Nbx; nby = Nby;
		label = newlabel;
		preserved = newpreserved;
		color = newcolor;
	}
	
	private void refreshLabelDisplay() {
		
		multiButton = new BorderedButton[nbx*nby+1];
		listButton 	= new JToggleButton[nbx*nby+1];
		preserveBox = new JCheckBox[nbx*nby+1];
		labelField = new JTextField[nbx*nby+1];
		for (int n = 1; n<nbx*nby+1;n++) {
			labelField[n] = new JTextField(5);
			labelField[n].setText(label[n]);
			labelField[n].setFont(serif12);
			labelField[n].addActionListener(this);
			labelField[n].setActionCommand("Label "+n);
			
			multiButton[n] = new BorderedButton(String.valueOf(n));
			multiButton[n].addActionListener(this);
			multiButton[n].setActionCommand("PaintMask "+n);
			multiButton[n].setFont(MipavUtil.font10);
			multiButton[n].setSelected(false);
			multiButton[n].setMaximumSize(new Dimension(48,20));
			multiButton[n].setPreferredSize(new Dimension(48,20));		
			multiButton[n].setToolTipText(label[n]);
			multiButton[n].setBackground(color[n]);
							
			listButton[n] = new JToggleButton(String.valueOf(n));
			listButton[n].addActionListener(this);
			listButton[n].setActionCommand("PaintMask "+n);
			listButton[n].setFont(MipavUtil.font10);
			listButton[n].setSelected(false);
			listButton[n].setMaximumSize(new Dimension(50,18));
			listButton[n].setPreferredSize(new Dimension(50,18));		
			listButton[n].setBackground(color[n]);		
				
			preserveBox[n] = new JCheckBox("");
			preserveBox[n].addActionListener(this);
			preserveBox[n].setActionCommand("Preserve "+n);
			preserveBox[n].setToolTipText("Lock the paint mask");					
		}		
		mainPanel.remove(multiPanel);
		
		multiPanel = new JPanel(new GridBagLayout());
		multiPanel.setBorder(buildTitledBorder("Mask Palette"));

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.anchor = gbc.CENTER;
		gbc.fill = gbc.HORIZONTAL;
		gbc.insets = new Insets(0, 0, 0, 0);
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridx = i;
			gbc.gridy = j;
			multiPanel.add(multiButton[i+nbx*j+1], gbc);
		}
		
		mainPanel.remove(listPanel);

		listPanel = new JPanel(new GridBagLayout());
		listPanel.setBorder(buildTitledBorder("Label list"));
		for (int i=0;i<nbx;i++) for (int j=0;j<nby;j++) {
			gbc.gridy = i+nbx*j;
			gbc.gridwidth = 1;
			gbc.gridx = 0;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(listButton[i+nbx*j+1], gbc);
			gbc.gridx = 1;
			gbc.weightx = 1;
			gbc.fill = gbc.HORIZONTAL;
			listPanel.add(labelField[i+nbx*j+1], gbc);
			gbc.gridx = 2;
			gbc.weightx = 0;
			gbc.fill = gbc.NONE;
			listPanel.add(preserveBox[i+nbx*j+1], gbc);		
		}
	
		gbc.gridx = 0;
		gbc.gridwidth = 1;
		gbc.gridy = 0;
		gbc.fill = gbc.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		mainPanel.add(multiPanel, gbc);
		gbc.gridy = 2;
		mainPanel.add(listPanel, gbc);
	
		if (displayModeButton.isSelected()) { 
			multiPanel.setVisible(false); 
			listPanel.setVisible(true);
		} else {
			listPanel.setVisible(false);
			multiPanel.setVisible(true); 
		}
		// reselect the same object
		if (selected<nbx*nby+1) {
			multiButton[selected].setSelected(true);
			listButton[selected].setSelected(true);
		} else {
			selectedMaskToPaint(1);
		}
		pack();
		repaint();
	}
	
    /** 
    *	This method is required if the AlgorithmPerformed interface is implemented. 
    *   It is called by the algorithm when it has completed or failed to to complete, 
    *   so that the dialog can be display the result image and/or clean up.
    *   @param algorithm   Algorithm that caused the event.
    */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    }  // end AlgorithmPerformed()
    
  
    /**
    *	Use the GUI results to set up the variables needed to run the algorithm.
    *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
    */
    /**
    *	Use the GUI results to set up the variables needed to run the algorithm.
    *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
    */
    private boolean setVariables() {
    	return true;  	
    }   // end setVariables()
    
    /**
    *	Once all the necessary variables are set, call the Gaussian Blur
    *	algorithm based on what type of image this is and whether or not there
    *	is a separate destination image.
    */
    private void callAlgorithm() {
    } // end callAlgorithm()
    
	
}

