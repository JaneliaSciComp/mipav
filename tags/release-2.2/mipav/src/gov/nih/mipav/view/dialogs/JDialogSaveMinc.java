package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
*   Simple dialog to tell the program important information about the
*   MINC file.
*
*
*		@version    1.0 Aug 1, 2000
*		@author     Neva Cherniavsky
*       @see        FileMinc
*
*/
public class JDialogSaveMinc extends JDialogBase {

    // Variables for the actual GUI; not all need to be global, but just as easy.
    // The text fields and radio buttons do need to be global.
    private     JComboBox    combo;
    private     JTextField   xStart;
    private     JTextField   xSpace;
    private     JTextField   yStart;
    private     JTextField   ySpace;
    private     JTextField   zStart;
    private     JTextField   zSpace;
    private     JRadioButton left;
    private     JRadioButton right;
    private     JRadioButton anterior;
    private     JRadioButton posterior;
    private     JRadioButton superior;
    private     JRadioButton inferior;

    // Variables for holding and storing file information.
    private     FileInfoBase fileInfo;
    private		FileWriteOptions options;

    // Variables for holding default values.  Then if the default values are used (as
    // in, the user hit "OK" without changing any values), that option would be set in
    // "options".  This affects the way the script records.
	private		float		 defaultXStart;
	private		float		 defaultYStart;
	private		float		 defaultZStart;
	private		float		 defaultXSpace;
	private		float		 defaultYSpace;
	private		float		 defaultZSpace;
	private		int			 defaultOrient;
	private		boolean		 defaultLR;
	private		boolean		 defaultPA;
	private		boolean 	 defaultIS;
	private		boolean		 defaultSet = false;


    /**
    *  Constructs a new dialog to set necessary information for saving a MINC file.
    *  @param theParentFrame  Parent frame.
    *  @param _fileInfo       File info object to get initialization info from.
    *  @param options	      Storage place for the info recorded in this dialog.
    */
	public JDialogSaveMinc(Frame theParentFrame, FileInfoBase _fileInfo, FileWriteOptions options) {
		super(theParentFrame, true);

        fileInfo = _fileInfo;
        this.options = options;
		init();
	}

	/**
	*	Initializes the GUI components and puts them in the dialog, attaching necessary actions.
	*/
	private void init() {
		setTitle("Attributes to save");
		setResizable(false);
        cancelFlag = false;

		GridBagLayout layout = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.insets = new Insets(1, 1, 1, 1);

		JPanel panel = new JPanel();
		panel.setLayout(layout);
		panel.setForeground(Color.black);
        panel.setBorder(buildTitledBorder("MINC Attributes to Save"));

        createLabel("Image orientation:", layout, setGBC(gbc, 0, 0, 2, 1), panel);
        createComboBox(layout, setGBC(gbc, 2, 0, gbc.REMAINDER, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 1, gbc.REMAINDER, 1), panel);

        createLabel("X Start:", layout, setGBC(gbc, 0, 2, 1, 1), panel);
        xStart = setTextField("", layout, setGBC(gbc, 1, 2, 1, 1), panel);
        createLabel("  X Space:", layout, setGBC(gbc, 2, 2, 1, 1), panel);
        xSpace = setTextField("", layout, setGBC(gbc, 3, 2, 1, 1), panel);

        createLabel("Y Start:", layout, setGBC(gbc, 0, 3, 1, 1), panel);
        yStart = setTextField("", layout, setGBC(gbc, 1, 3, 1, 1), panel);
        createLabel("  Y Space:", layout, setGBC(gbc, 2, 3, 1, 1), panel);
        ySpace = setTextField("", layout, setGBC(gbc, 3, 3, 1, 1), panel);

        createLabel("Z Start:", layout, setGBC(gbc, 0, 4, 1, 1), panel);
        zStart = setTextField("", layout, setGBC(gbc, 1, 4, 1, 1), panel);
        createLabel("  Z Space:", layout, setGBC(gbc, 2, 4, 1, 1), panel);
        zSpace = setTextField("", layout, setGBC(gbc, 3, 4, 1, 1), panel);

        createLabel(" ", layout, setGBC(gbc, 0, 5, gbc.REMAINDER, 1), panel);

        JLabel l = createLabel("In the current image:", layout, setGBC(gbc, 0, 6, gbc.REMAINDER, 1), panel);
	    l.setFont(serif12B);
	    JLabel l2 = createLabel(" The (0,0) pixel is in the lower left hand corner of the image for MINC files. ",
	                            layout,
	                            setGBC(gbc, 0, 7, gbc.REMAINDER, 1),
	                            panel);
	    l2.setFont(MipavUtil.font12I);
	    JLabel l3 = createLabel(" Therefore, the positive directions are left to right, bottom to top,",
	                            layout,
	                            setGBC(gbc, 0, 8, gbc.REMAINDER, 1),
	                            panel);
	    l3.setFont(MipavUtil.font12I);
	    JLabel l4 = createLabel(" and lower slice number to higher slice number.",
	                            layout,
	                            setGBC(gbc, 0, 9, gbc.REMAINDER, 1),
	                            panel);
	    l4.setFont(MipavUtil.font12I);

	    createLabel("   X increases from patient", layout, setGBC(gbc, 0, 10, 2, 1), panel);
        ButtonGroup xGroup = new ButtonGroup();
        left  = setRadio("left to right", layout, setGBC(gbc, 2, 10, 2, 1), panel, xGroup);
        right = setRadio("right to left", layout, setGBC(gbc, 2, 11, 2, 1), panel, xGroup);

        createLabel("   Y increases from patient", layout, setGBC(gbc, 0, 12, 2, 1), panel);
        ButtonGroup yGroup = new ButtonGroup();
        posterior = setRadio("posterior to anterior", layout, setGBC(gbc, 2, 12, 2, 1), panel, yGroup);
        anterior  = setRadio("anterior to posterior", layout, setGBC(gbc, 2, 13, 2, 1), panel, yGroup);

        createLabel("   Z increases from patient", layout, setGBC(gbc, 0, 14, 2, 1), panel);
        ButtonGroup zGroup = new ButtonGroup();
        inferior = setRadio("inferior to superior", layout, setGBC(gbc, 2, 14, 2, 1), panel, zGroup);
        superior = setRadio("superior to inferior", layout, setGBC(gbc, 2, 15, 2, 1), panel, zGroup);

        createLabel(" ", layout, setGBC(gbc, 0, 16, gbc.REMAINDER, 1), panel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        setSpace(fileInfo.getImageOrientation());
        getContentPane().add(panel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
	}

    /**
    *   Initializes the text fields for the dialog. MORE!
    */
    private void setSpace(int orient) {
    	float x = 0, y = 0, z = 0, xRes = 1, yRes = 1, zRes = 1;
        int[] ori = fileInfo.getAxisOrientation();
        for (int i=0; i<ori.length; i++) {
        	switch (ori[i]) {
        		case FileInfoBase.ORI_R2L_TYPE:
        		    right.setSelected(true);
        		    x     = -fileInfo.getOrigin(i);
        		    xRes  = -fileInfo.getResolutions()[i];
        		    break;
        		case FileInfoBase.ORI_L2R_TYPE:
        		    left.setSelected(true);
        		    x     = -fileInfo.getOrigin(i);
        		    xRes  = fileInfo.getResolutions()[i];
        		    break;
        		case FileInfoBase.ORI_P2A_TYPE:
        		    posterior.setSelected(true);
        		    y     = -fileInfo.getOrigin(i);
        		    yRes  = fileInfo.getResolutions()[i];
        		    break;
        		case FileInfoBase.ORI_A2P_TYPE:
        		    anterior.setSelected(true);
        		    y     = -fileInfo.getOrigin(i);
        		    yRes  = -fileInfo.getResolutions()[i];
        		    break;
        		case FileInfoBase.ORI_I2S_TYPE:
        		    inferior.setSelected(true);
        		    z     = fileInfo.getOrigin(i);
        		    zRes  = fileInfo.getResolutions()[i];
        		    break;
        		case FileInfoBase.ORI_S2I_TYPE:
        		    superior.setSelected(true);
        		    z     = fileInfo.getOrigin(i);
        		    zRes  = -fileInfo.getResolutions()[i];
        		    break;
        	}
        }
        if (fileInfo.getExtents().length == 2) {
            zStart.setEnabled(false);
            zSpace.setEnabled(false);
        }

    	if (fileInfo.getFileFormat() == FileBase.MINC && !((FileInfoMinc)fileInfo).resetStartLocationsOrientations()) {
    		float[] steps = ((FileInfoMinc)fileInfo).getStepMinc();
    		xRes = steps[0];
    		yRes = steps[1];
    		if (steps.length > 2)	zRes = steps[2];
    		float[] values = ((FileInfoMinc)fileInfo).getStartMinc();
    		x = values[0];
    		y = values[1];
    		if (values.length > 2)	z = values[2] + zRes*options.getBeginSlice();
            switch (ori[1]) {
        	    // opposite because we flip the MINC image before saving it.  the origin for MINC
        	    // is in the lower left hand corner, whereas our origin is in the upper left hand corner.
        	    case FileInfoBase.ORI_R2L_TYPE:
        	    case FileInfoBase.ORI_L2R_TYPE:
        	        if (left.isSelected()) right.setSelected(true);
        	        else left.setSelected(true);
        		    break;
        	    case FileInfoBase.ORI_P2A_TYPE:
        	    case FileInfoBase.ORI_A2P_TYPE:
        	        if (anterior.isSelected()) posterior.setSelected(true);
        	        else anterior.setSelected(true);
        		    break;
        	    case FileInfoBase.ORI_I2S_TYPE:
        	    case FileInfoBase.ORI_S2I_TYPE:
        	        if (superior.isSelected()) inferior.setSelected(true);
        	        else superior.setSelected(true);
        		    break;
            }
    	}
    	else {
            switch (ori[1]) {
        	    // opposite because we flip the MINC image before saving it.  the origin for MINC
        	    // is in the lower left hand corner, whereas our origin is in the upper left hand corner.
        	    case FileInfoBase.ORI_R2L_TYPE:
        	    case FileInfoBase.ORI_L2R_TYPE:
        	        if (left.isSelected()) right.setSelected(true);
        	        else left.setSelected(true);
                    x += xRes*(fileInfo.getExtents()[1]-1);
                    xRes = -xRes;
        		    break;
        	    case FileInfoBase.ORI_P2A_TYPE:
        	    case FileInfoBase.ORI_A2P_TYPE:
        	        if (anterior.isSelected()) posterior.setSelected(true);
        	        else anterior.setSelected(true);
                    y += yRes*(fileInfo.getExtents()[1]-1);
                    yRes = -yRes;
        		    break;
        	    case FileInfoBase.ORI_I2S_TYPE:
        	    case FileInfoBase.ORI_S2I_TYPE:
        	        if (superior.isSelected()) inferior.setSelected(true);
        	        else superior.setSelected(true);
                    z += zRes*(fileInfo.getExtents()[1]-1);
                    zRes = -zRes;
        		    break;
            }
            switch (ori[2]) {
        	    case FileInfoBase.ORI_R2L_TYPE:
        	    case FileInfoBase.ORI_L2R_TYPE:
				    x += xRes*options.getBeginSlice();
        		    break;
        	    case FileInfoBase.ORI_P2A_TYPE:
        	    case FileInfoBase.ORI_A2P_TYPE:
				    y += yRes*options.getBeginSlice();
        		    break;
        	    case FileInfoBase.ORI_I2S_TYPE:
        	    case FileInfoBase.ORI_S2I_TYPE:
				    z += zRes*options.getBeginSlice();
        		    break;
            }
        }
        xStart.setText("" + x);
        yStart.setText("" + y);
        if (zStart.isEnabled()) zStart.setText("" + z);
        xSpace.setText("" + xRes);
        ySpace.setText("" + yRes);
        if (zSpace.isEnabled())	zSpace.setText("" + zRes);
        if (!defaultSet) {
        	defaultXStart = x;
        	defaultYStart = y;
        	if (zStart.isEnabled()) defaultZStart = z;
        	defaultXSpace = xRes;
        	defaultYSpace = yRes;
        	if (zSpace.isEnabled())	defaultZSpace = zRes;
        	defaultOrient = orient;
        	defaultSet = true;
        	defaultLR = left.isSelected();
        	defaultPA = posterior.isSelected();
        	defaultIS = inferior.isSelected();
        }
    }

    /**
    *	Accessor that returns the write options set up in this dialog.
    *	@return 	The write options.
    */
    public FileWriteOptions getOptions()	{return options;}

    /**
    *    Closes dialog box when the OK button is pressed and sets the information.
    *    @param event      event that triggers this function
    */
    public void actionPerformed(ActionEvent event) {
        if (event.getActionCommand().equals("OK")) {
            cancelFlag = false;
            try {
                float test = Float.valueOf(xStart.getText()).floatValue();
                test = Float.valueOf(yStart.getText()).floatValue();
                if (zStart.isEnabled()) test = Float.valueOf(zStart.getText()).floatValue();
                test = Float.valueOf(xSpace.getText()).floatValue();
                test = Float.valueOf(ySpace.getText()).floatValue();
                if (zSpace.isEnabled()) test = Float.valueOf(zSpace.getText()).floatValue();
                setOptions();
                dispose();
            }
            catch (NumberFormatException error) {
                MipavUtil.displayError("The values in X Space, Y Space, and Z Space must be numbers.");
            }
        }
        else if (event.getActionCommand().equals("OrientationChanged")) {
            setSpace(combo.getSelectedIndex());
        }
        else if (event.getActionCommand().equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
    }

   /**
   *	Sets up the options depending on what the user entered in the dialog.
   */
    private void setOptions() {
    	options.setXStart(Float.valueOf(xStart.getText()).floatValue());
    	options.setYStart(Float.valueOf(yStart.getText()).floatValue());
    	if (zStart.isEnabled()) options.setZStart(Float.valueOf(zStart.getText()).floatValue());
    	options.setXSpace(Float.valueOf(xSpace.getText()).floatValue());
    	options.setYSpace(Float.valueOf(ySpace.getText()).floatValue());
    	if (zSpace.isEnabled()) options.setZSpace(Float.valueOf(zSpace.getText()).floatValue());
    	options.setLeftToRight(left.isSelected());
    	options.setPosToAnt(posterior.isSelected());
    	options.setInfToSup(inferior.isSelected());
    	options.setOrientation(combo.getSelectedIndex());
    	if (zStart.isEnabled()) {
    		if (!(	options.getXStart() == defaultXStart && options.getYStart() == defaultYStart && options.getZStart() == defaultZStart &&
    				options.getXSpace() == defaultXSpace && options.getYSpace() == defaultYSpace && options.getZSpace() == defaultZSpace &&
    				options.getOrientation() == defaultOrient && defaultLR == options.isLeftToRight() && defaultPA == options.isPosToAnt() &&
    				defaultIS == options.isInfToSup())) {
    			options.setDefault(false);
			}
		}
		else {
    		if (!(	options.getXStart() == defaultXStart && options.getYStart() == defaultYStart && options.getXSpace() == defaultXSpace &&
    				options.getYSpace() == defaultYSpace && options.getOrientation() == defaultOrient && defaultLR == options.isLeftToRight()
    				&& defaultPA == options.isPosToAnt() && defaultIS == options.isInfToSup())) {
    			options.setDefault(false);
    		}

    	}
    }

   /**
   *	Sets up the options to the default values as determined by the file info in setSpace and returns the new options.
   *	@return		The default options.
   */
    public FileWriteOptions setOptionsDefault() {
    	options.setXStart(defaultXStart);
    	options.setYStart(defaultYStart);
    	if (zStart.isEnabled()) options.setZStart(defaultZStart);
    	options.setXSpace(defaultXSpace);
    	options.setYSpace(defaultYSpace);
    	if (zSpace.isEnabled()) options.setZSpace(defaultZSpace);
    	options.setLeftToRight(defaultLR);
    	options.setPosToAnt(defaultPA);
    	options.setInfToSup(defaultIS);
    	options.setOrientation(defaultOrient);
    	return options;
    }

    /**
    *   A helper method for adding a component using GridBagLayout, so we don't have
    *   to set up the x, y, width, and height over and over again.
    *   @param gbc  The constraints to set.
    *   @param x    gridx
    *   @param y    gridy
    *   @param w    gridwidth
    *   @param h    gridheight
    *   @return     The new grid bag constraints.
    *   @see        GridBagConstraints
    */
    private GridBagConstraints setGBC(GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        return gbc;
    }

    /**
    *   Makes a label and adds it to the panel.
    *   @param title  The value of the label.
    *   @param layout The layout to add the constraints to.
    *   @param gbc    The constraints for this label.
    *   @param panel  The panel to add the label to.
    */
    private JLabel createLabel(String title, GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
		JLabel label = new JLabel(title);
		label.setFont(serif12);
		label.setForeground(Color.black);
        layout.setConstraints(label, gbc);
		panel.add(label);
		return label;
	}

    /**
    *   Makes a text field and adds it to the panel.
    *   @param title  The initial string in the text field.
    *   @param layout The layout to add the constraints to.
    *   @param gbc    The constraints for this text field.
    *   @param panel  The panel to add the text field to.
    *   @return 	  The text field created.
    */
	private JTextField setTextField(String initial, GridBagLayout layout, GridBagConstraints gbc, JPanel panel) {
	    JTextField field = new JTextField(initial, 5);
	    layout.setConstraints(field, gbc);
	    panel.add(field);
	    return field;
	}

    /**
    *   Makes a radio button and adds it to the panel.
    *   @param title  The title of the radio button.
    *   @param layout The layout to add the constraints to.
    *   @param gbc    The constraints for this radio button.
    *   @param panel  The panel to add the radio button to.
    *   @param group  The group to add the radio button to.
    *   @return 	  The radio button created.
    */
	private JRadioButton setRadio(String title, GridBagLayout layout, GridBagConstraints gbc, JPanel panel, ButtonGroup group) {
	    JRadioButton radio = new JRadioButton(title);
	    radio.setFont(serif12);
		radio.setForeground(Color.black);
		layout.setConstraints(radio, gbc);
	    panel.add(radio);
	    radio.setSelected(false);
	    group.add(radio);
	    return radio;
	}

	/**
	*   Creates a combo box and adds it to the panel.
    *   @param layout The layout to add the constraints to.
    *   @param gbc    The constraints for this combo box.
    *   @param panel  The panel to add the combo box to.
	*/
	private void createComboBox(GridBagLayout layout, GridBagConstraints gbc, JPanel panel){
	    Object[] vector = {"Axial", "Coronal", "Sagittal"};
	    combo = new JComboBox(vector);
	    combo.setFont(serif12);
	    combo.setForeground(Color.black);
	    combo.setBackground(Color.white);
	    layout.setConstraints(combo, gbc);
        switch (fileInfo.getImageOrientation()) {
            case FileInfoBase.AXIAL:    combo.setSelectedItem("Axial");    break;
            case FileInfoBase.SAGITTAL: combo.setSelectedItem("Sagittal"); break;
            case FileInfoBase.CORONAL:  combo.setSelectedItem("Coronal");  break;
            default:                    combo.setSelectedItem("Axial");
        }
        combo.setActionCommand("OrientationChanged");
        combo.addActionListener(this);
	    panel.add(combo);
	}

}




