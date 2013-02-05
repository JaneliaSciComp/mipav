package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogNColors;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.FileNotFoundException;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class JFrameHistogram extends JPanel implements ActionListener, ChangeListener, ViewImageUpdateInterface, WindowListener {

	private static final long serialVersionUID = 6057663900661366920L;

	/** DOCUMENT ME! */
    private ViewJFrameColocalizationEM colEMFrame = null;

    /** DOCUMENT ME! */
    private ViewJFrameColocalizationRegression colRegFrame = null;

	/** DOCUMENT ME! */
	private ModelImage imageA = null; // source image

	/** DOCUMENT ME! */
	private ModelImage imageB = null; // source image

	/** DOCUMENT ME! */
	private ModelStorageBase LUTa;

	/** DOCUMENT ME! */
	private ModelStorageBase LUTb;

	/** false = apply algorithm only to VOI regions. */
	private ViewJComponentRegistration regComponent = null;

	/** true = apply algorithm to the whole image */
	private boolean wholeImage = true; 

	/** Parent frame of this dialog, usually of type ViewJFrameImage. */
	private Frame parentFrame;

	private JDialog voiDialog;

	private JFrame containingFrame;
	private JPanel containingPanel;
	private boolean dualImage;

	ViewMenuBuilder menuObj;


	JTabbedPane tabbedPane = null;


	JPanelHistogram panelA, panelB;
	
	public JFrameHistogram(Frame theParentFrame, ModelImage imA, ModelImage imB, ModelStorageBase _LUTa, ModelStorageBase _LUTb) 
	{
		this( theParentFrame, null, imA, imB, _LUTa, _LUTb );
	}


	/**
	 * Creates new histogram dialog. This is called from ViewJFrameRegistration.
	 *
	 * @param  theParentFrame  Parent frame
	 * @param  _regComponent   Registration component.
	 * @param  imA             Source image A
	 * @param  imB             DOCUMENT ME!
	 * @param  _LUTa           Source image B (can be null)
	 * @param  _LUTb           RGB LUT associated with image A.
	 */
	public JFrameHistogram(Frame theParentFrame, ViewJComponentRegistration _regComponent, ModelImage imA,
			ModelImage imB, ModelStorageBase _LUTa, ModelStorageBase _LUTb)
	{
		imageA = imA;
		imageB = imB;
		LUTa = _LUTa;
		LUTb = _LUTb;
		regComponent = _regComponent;
		parentFrame = theParentFrame;
		dualImage = (imageA != null) && (imageB != null);
		imageA.addImageDisplayListener( this );
		if ( imageB != null )
		{
			imageB.addImageDisplayListener( this );
		}
	}
	
	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 *
	 * @param  event  event that triggers function
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

		if ( containingFrame != null )
		{
			// LUT Menu commands:
			JMenu fileMenu = (JMenu)containingFrame.getJMenuBar().getComponent(0);
			if ( (fileMenu != null) && ViewMenuBar.isMenuCommand(fileMenu, command)) {
				processFileMenu(command);
				return;
			}
			JMenu utilMenu = (JMenu)containingFrame.getJMenuBar().getComponent(1);
			if ( (utilMenu != null) && ViewMenuBar.isMenuCommand(utilMenu, command)) {
				processUtilitiesMenu(event, command);
				return;
			}
		}
		// Dialog for VOI / WholeImage Commands:
		if (command.equals("wholeImage") )
		{
			wholeImage = ((JRadioButton)event.getSource()).isSelected();        	
		}
		else if (command.equals("VOIRegions"))
		{
			wholeImage = !((JRadioButton)event.getSource()).isSelected();  
		}
		else if (command.equals("OK"))
		{
			histogramLUT(wholeImage, true);
			voiDialog.setVisible(false);
			voiDialog.dispose();
		}
		else if (command.equals("Cancel"))
		{
			histogramLUT(wholeImage, true);
			voiDialog.setVisible(false);
			voiDialog.dispose();
		} 
		else if (command.equals("calcThresholdVolume")) {
			if (!menuObj.isMenuItemSelected("Calculate threshold volume")) {
				getSelectedPanel().clearVoxelLabel();
			}
		}
	}
	
	public void setImageB( ModelImage image, ModelStorageBase LUT )
	{
		if ( (imageB == image) || (image == null) )
		{
			return;
		}
		imageB = image;
		LUTb = LUT;
		dualImage = (imageA != null) && (imageB != null);
		if ( imageB != null )
		{
			imageB.addImageDisplayListener( this );
		}
		if ( tabbedPane != null )
		{
			tabbedPane.remove(panelB);
			panelB = null;
			panelB = new JPanelHistogram(this, imageB, LUTb, wholeImage);
			tabbedPane.addTab("ImageB", null, panelB);
			containingPanel.revalidate();
			tabbedPane.setSelectedIndex(1);
		}
		else
		{
			containingPanel.remove(panelA);
			panelB = new JPanelHistogram(this, imageB, LUTb, wholeImage);
			tabbedPane = new JTabbedPane();
			tabbedPane.addTab("ImageA", null, panelA);
			tabbedPane.addTab("ImageB", null, panelB);
			tabbedPane.setFont(MipavUtil.font12B);

			containingPanel.add(tabbedPane);
			containingPanel.revalidate();
			tabbedPane.setSelectedIndex(1);
			tabbedPane.addChangeListener(this);
		}
		containingFrame.pack();
	}
	
	public void closeFrame()
	{
		if ( containingFrame != null )
		{
			containingFrame.setVisible(false);
		}
		disposeLocal();
	}

	public void closeImageB()
	{
		if ( tabbedPane != null )
		{
			containingPanel.remove(tabbedPane);
			containingPanel.add(panelA);
			tabbedPane.remove(panelA);
			tabbedPane.remove(panelB);
			tabbedPane = null;
			panelB = null;
			imageB = null;
			LUTb = null;
			containingPanel.validate();
			containingFrame.pack();
		}
	}    
	
    /**
	 * Creates a dialog to choose if histogram should be over all of image or just VOI regions.
	 */
	public void constructDialog()
	{
		voiDialog = new JDialog(parentFrame, true);
		voiDialog.setTitle("Histogram LUT / RGB TEST");

		JPanel imageVOIPanel = new JPanel(new GridBagLayout());
		imageVOIPanel.setForeground(Color.black);
		imageVOIPanel.setBorder(JDialogBase.buildTitledBorder("Histogram"));
		voiDialog.getContentPane().add(imageVOIPanel);

		ButtonGroup imageVOIGroup = new ButtonGroup();
		JRadioButton wholeImage = new JRadioButton("Whole image", true);
		wholeImage.setActionCommand( "wholeImage" );
		wholeImage.addActionListener( this );        
		wholeImage.setFont(MipavUtil.font12);
		imageVOIGroup.add(wholeImage);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(5, 5, 5, 5);
		imageVOIPanel.add(wholeImage, gbc);

		JRadioButton VOIRegions = new JRadioButton("VOI region(s)", false);
		VOIRegions.setActionCommand( "VOIRegions" );
		VOIRegions.addActionListener( this );        
		VOIRegions.setFont(MipavUtil.font12);
		imageVOIGroup.add(VOIRegions);
		gbc.gridy = 1;
		imageVOIPanel.add(VOIRegions, gbc);

		JPanel buttonPanel = new JPanel();
		buttonPanel.add(JDialogBase.buildOKButton( "OK", this ));
		buttonPanel.add(JDialogBase.buildCancelButton( "Cancel", this ));

		voiDialog.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		voiDialog.pack();
		MipavUtil.centerOnScreen(voiDialog);
		voiDialog.setVisible(true);
	}

	public void disposeLocal()
	{
		imageA.removeImageDisplayListener( this );
		if ( imageB != null )
		{
			imageB.removeImageDisplayListener( this );
		}
	}


	public boolean doCalcThresholdVolume()
	{
		return (menuObj == null) || menuObj.isMenuItemSelected("Calculate threshold volume");
	}


	public JPanel getContainingPanel()
	{
		return containingPanel;
	}

	public float getLowerThreshold()
	{
        if (isImageASelected())
        {
        	return panelA.getLowerThreshold();
        }
        else if (isImageBSelected())
        {
        	return panelB.getLowerThreshold();
        }
        return -1;
	}
	public JPanelHistogram getSelectedPanel() {
		return isImageASelected() ? panelA : panelB;
	}

	public float getUpperThreshold()
	{
        if (isImageASelected())
        {
        	return panelA.getUpperThreshold();
        }
        else if (isImageBSelected())
        {
        	return panelB.getUpperThreshold();
        }
        return -1;
	}
	public void histogramLUT(boolean entireFlag, boolean separateFrame) 
	{

		if ( separateFrame )
		{
			containingFrame = new JFrame();
			// Window title:
			containingFrame.setTitle("Lookup Table: " + imageA.getImageName());
			containingFrame.getContentPane().setLayout(new BorderLayout());

			try {
				containingFrame.setIconImage(MipavUtil.getIconImage("histolut.gif"));
			} catch (final FileNotFoundException error) {
				Preferences.debug("Exception ocurred while getting <" + error.getMessage()
						+ ">.  Check that this file is available.\n");
				System.err.println("Exception ocurred while getting <" + error.getMessage()
						+ ">.  Check that this file is available.\n");
			}

			// create file menu:
			buildMenu(containingFrame);

			containingPanel = createPanel();
			containingFrame.getContentPane().add( containingPanel );

			containingFrame.setLocation(200, 200);
			containingFrame.setBackground(new Color(160, 160, 160));
			containingFrame.pack();
			containingFrame.setResizable(true);
			containingFrame.setVisible(true);    		
			containingFrame.addWindowListener(this);
		}
		else
		{
			containingPanel = createPanel();
		}
	}
	/**
	 * Returns whether the imageA LUT panel is the one being worked on.
	 * 
	 * @return whether the imageA LUT panel is the one being worked on
	 */
	public boolean isImageASelected() {
		return (tabbedPane == null) || ((tabbedPane != null) && (tabbedPane.getSelectedIndex() == 0));
	}




	/**
	 * Returns whether the imageB LUT panel is the one being worked on.
	 * 
	 * @return whether the imageB LUT panel is the one being worked on
	 */
	public boolean isImageBSelected() {
		return (dualImage && (tabbedPane == null) || ((tabbedPane != null) && (tabbedPane.getSelectedIndex() == 1)));
	}

	/**
     * Returns true if comphist A or B is in dual threshold inverse mode (for JDialogConvertType input ranges).
     *
     * @return  boolean is dual threshold inversing
     */
    public boolean isThresholding() {

        if (isImageASelected())
        {
			if ((panelA.getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
                    (panelA.getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD))
			{
                return true;
            }
        } 
        else if (isImageBSelected())
        {
			if ((panelA.getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
                    (panelA.getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD))
			{
                return true;
            }
        }

        return false;
    }

	public void redrawFrames()
	{
		if ( isImageASelected() )
		{
			panelA.updateFrames(true);
		}
		else if ( isImageBSelected() )
		{
			panelB.updateFrames(true);
		}
	}
	
	/**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width.
     * @param  frameHeight  parent frame height.
     */
    public void resizePanel(int panelWidth, int frameHeight) {
    	if ( containingPanel != null )
    	{
    		containingPanel.setPreferredSize(new Dimension(panelWidth, frameHeight));
    		containingPanel.setSize(new Dimension(panelWidth, frameHeight));
    		containingPanel.revalidate();
    	}
    }

	public void setActiveImage( ModelImage  image )
	{
		if ( tabbedPane == null )
		{
			return;
		}
		if ( image == imageA )
		{
			tabbedPane.setSelectedIndex(0);
		}
		else if ( image == imageB )
		{
			tabbedPane.setSelectedIndex(1);
		}
	}
	
    public void setBlueOn( boolean isOn, boolean isImageA )
    {
    	if ( isImageA && panelA != null )
    	{
    		panelA.setBlueOn(isOn);
    	}
    	else if ( panelB != null )
    	{
    		panelB.setBlueOn(isOn);
    	}
    }

	public void setColocalizationEMFrame(ViewJFrameColocalizationEM colocalizationEMFrame) {
		colEMFrame = colocalizationEMFrame;
	}
	
	public void setColocalizationRegFrame(ViewJFrameColocalizationRegression colocalizationRegFrame) { 
		colRegFrame = colocalizationRegFrame;
	}
	
	public void setGreenOn( boolean isOn, boolean isImageA )
    {
    	if ( isImageA && panelA != null )
    	{
    		panelA.setGreenOn(isOn);
    	}
    	else if ( panelB != null )
    	{
    		panelB.setGreenOn(isOn);
    	}
    }
	
	public void setLUT(JPanelHistogram panel, ModelStorageBase LUT) {
		if ( panel == panelA )
		{
			LUTa = LUT;
		}
		else if ( panel == panelB )
		{
			LUTb = LUT;
		}
		updateFrames(false);
	}
	
    public void setLUTA(ModelStorageBase LUT)
    {
    	LUTa = LUT;
    	updateFrames(false);
	}

    public void setLUTB(ModelStorageBase LUT)
    {
    	LUTb = LUT;
    	updateFrames(false);
	}

    public void setRedOn( boolean isOn, boolean isImageA )
    {
    	if ( isImageA && panelA != null )
    	{
    		panelA.setRedOn(isOn);
    	}
    	else if ( panelB != null )
    	{
    		panelB.setRedOn(isOn);
    	}
    }

    @Override
	public void setSlice(int slice) {
		// TODO Auto-generated method stub
		
	} 

    @Override
	public void setTimeSlice(int tSlice) {
		// TODO Auto-generated method stub
		
	} 

	public void setTransferFunctionA(TransferFunction txFunction) {
		ModelLUT selectedLUTa = !imageA.isColorImage() ? (ModelLUT)LUTa : null;
		if ( selectedLUTa != null )
		{
			selectedLUTa.setTransferFunction(txFunction);
		}
    } 


    public void setTransferFunctionB(TransferFunction txFunction) {
		ModelLUT selectedLUTb = !imageB.isColorImage() ? (ModelLUT)LUTb : null;
		if ( selectedLUTb != null )
		{
			selectedLUTb.setTransferFunction(txFunction);
		}
    }

    @Override
	public void stateChanged(ChangeEvent event) {
		Object source = event.getSource();

		// Slider has changed lets update.
		if ( containingFrame != null )
		{
			if ( (source == tabbedPane) && isImageASelected()) {
				setLUT(panelA, LUTa);
				containingFrame.setTitle("Lookup Table: " + imageA.getImageName());

				if (doCalcThresholdVolume()) {
					panelA.calculateThreshold();
				}
			} else if ( (source == tabbedPane) && isImageBSelected() ) {
				setLUT(panelB, LUTb);
				containingFrame.setTitle("Lookup Table: " + imageB.getImageName());

				if (doCalcThresholdVolume()) {
					panelA.calculateThreshold();
				}
			}
		}
	}

	/**
	 * Update registration frames.
	 *
	 * @param  flag  whether to display a reloading of the image in the frames
	 */
	public void updateFrames(boolean flag) {
		ModelLUT selectedLUTa = !imageA.isColorImage() ? (ModelLUT)LUTa : null;
		ModelLUT selectedLUTb = ((imageB != null) && !imageB.isColorImage()) ? (ModelLUT)LUTb : null;
		ModelRGB selectedRGBa = imageA.isColorImage() ? (ModelRGB)LUTa : null;
		ModelRGB selectedRGBb = ((imageB != null) && imageB.isColorImage()) ? (ModelRGB)LUTb : null;

		if ((imageA != null) && (selectedLUTa != null) )
		{
			imageA.notifyImageDisplayListeners(selectedLUTa, flag);
			if ( regComponent != null )
			{
				regComponent.setLUTa(selectedLUTa);
			}
			if (colRegFrame != null) {
				colRegFrame.setLUTdest(selectedLUTa);
			} else if (colEMFrame != null) {
				colEMFrame.setLUTdest(selectedLUTa);
			} 
		}
		if ((imageA != null) && (selectedRGBa != null) )
		{
			imageA.notifyImageDisplayListeners(flag, selectedRGBa);
			if ( regComponent != null )
			{
				regComponent.setRGBTA(selectedRGBa);
			}
		}

		if ((imageB != null) && (selectedLUTb != null) )
		{
			imageB.notifyImageDisplayListeners(selectedLUTb, flag);
			if ( regComponent != null )
			{
				regComponent.setLUTb(selectedLUTb);
			}
		}
		if ((imageB != null) && (selectedRGBb != null) )
		{
			imageB.notifyImageDisplayListeners(flag, selectedRGBb);
			if ( regComponent != null )
			{
				regComponent.setRGBTB(selectedRGBb);
			}
		}
	}


	@Override
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages()
	{
		return false;
	}

	@Override
	public boolean updateImages(boolean flag) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag,
			int interpMode) {
		// TODO Auto-generated method stub
		return false;
	}


	public void updateInterpolation()
	{
		ModelLUT selectedLUTa = !imageA.isColorImage() ? (ModelLUT)LUTa : null;
		ModelLUT selectedLUTb = ((imageB != null) && !imageB.isColorImage()) ? (ModelLUT)LUTb : null;
		ModelLUT selectedLUT = isImageASelected() ? selectedLUTa : selectedLUTb;

		if ( selectedLUT == null )
		{
			return;
		}

		int interpMode = ViewJComponentBase.NEAREST_BOTH;
		boolean interpA = panelA.interpolateImage();
		boolean interpB = dualImage ? panelB.interpolateImage() : false;
		interpMode = interpA ? interpB ? ViewJComponentBase.INTERPOLATE_BOTH : ViewJComponentBase.INTERPOLATE_A :
			interpB ? ViewJComponentBase.INTERPOLATE_B : ViewJComponentBase.NEAREST_BOTH;
		imageA.notifyImageDisplayListeners(selectedLUTa, true, -50, interpMode);
		if ( dualImage )
		{
			imageB.notifyImageDisplayListeners(selectedLUTb, true, -50, interpMode);
		}
	}


	public void updateRealTime( boolean updateRealTime )
	{
		panelA.updateRealTime(updateRealTime);
		if ( dualImage )
		{
			panelB.updateRealTime(updateRealTime);
		}
	}


	/**
	 * This method builds a menu which contains the options for opening/saving a LUT or set of transfer functions,
	 * closing the LUT, and utilities such as CT presets.
	 */
	private void buildMenu( JFrame frame )
	{

		boolean color = imageA.isColorImage() | (dualImage && imageB.isColorImage() );
		boolean gray = !imageA.isColorImage() | (dualImage && !imageB.isColorImage() );

		menuObj = new ViewMenuBuilder(this);
		JMenuBar menuBar = new JMenuBar();


		Vector<JComponent> fileMenu = new Vector<JComponent>();
		if ( gray )
		{
			fileMenu.add(menuObj.buildMenuItem("Open LUT and transfer function", "OpenLUT", 0, "open.gif", true));
			fileMenu.add(menuObj.buildMenuItem("Save LUT and transfer function", "SaveLUT", 0, "save.gif", true));
			fileMenu.add(new JSeparator());
			fileMenu.add(menuObj.buildMenuItem("Open default LUT and transfer function", "OpenUDLUT", 0, "defaultlutopen.gif", true));
			fileMenu.add(menuObj.buildMenuItem("Save default LUT and transfer function", "SaveUDLUT", 0, "defaultlutsave.gif", true)); 
			fileMenu.add(new JSeparator());
		}
		fileMenu.add(menuObj.buildMenuItem("Open transfer function", "OpenFuncts", 0, "open.gif", true));
		fileMenu.add(menuObj.buildMenuItem("Save transfer function", "SaveFuncts", 0, "save.gif", true)); 
		fileMenu.add(new JSeparator());
		fileMenu.add(menuObj.buildMenuItem("Close LUT", "CloseLUT", 0, null, true));


		Vector<JComponent> utilitiesMenu = new Vector<JComponent>();		
		if ( gray )
		{
			utilitiesMenu.add(menuObj.buildMenuItem("Change number of colors", "ChangeNColors", 0, null, true));
			utilitiesMenu.add(menuObj.buildMenuItem("CT function", "ctPresetsLUT", 0, "ctwindow.gif", true));
			utilitiesMenu.add(menuObj.buildMenuItem("Invert LUT", "invertLUT", 0, null, true));
		}
		if ( color )
		{
			utilitiesMenu.add(menuObj.buildMenuItem("Reset transfer function", "Linear", 0, null, true));
		}
		utilitiesMenu.add(menuObj.buildMenuItem("Reset histogram & LUT A", "UpdateA", 0, null, true));
		if ( dualImage )
		{
			utilitiesMenu.add(menuObj.buildMenuItem("Reset histogram & LUT B", "UpdateB", 0, null, true));
		}
		utilitiesMenu.add(menuObj.buildCheckBoxMenuItem("Calculate threshold volume", "calcThresholdVolume", true));


		menuBar.add(menuObj.makeMenu("File", 'F', false, fileMenu ));    	
		menuBar.add(menuObj.makeMenu("Utilities", 'U', false, utilitiesMenu ));
		frame.setJMenuBar(menuBar);
	}


	private JPanel createPanel()
	{
		JPanel panel = new JPanel();

		panelA = new JPanelHistogram(this, imageA, LUTa, wholeImage);

		if ( dualImage )
		{
			panelB = new JPanelHistogram(this, imageB, LUTb, wholeImage);
			tabbedPane = new JTabbedPane();
			tabbedPane.addTab("ImageA", null, panelA);
			tabbedPane.addTab("ImageB", null, panelB);
			tabbedPane.setFont(MipavUtil.font12B);
			tabbedPane.setSelectedIndex(0);
			tabbedPane.addChangeListener(this);

			panel.add(tabbedPane);
		}
		else
		{
			panel.add(panelA);
		}
		return panel;
	}


	private void processFileMenu( String command )
	{
		if (command.equals("OpenLUT") || command.equals("OpenFuncts") || command.equals("OpenUDLUT"))
		{
			String fName = null;
			String dName = null;
			if  ( command.equals("OpenUDLUT") )
			{
				fName = "userdefine.lut";
				dName = Preferences.getPreferencesDir();
			}
			if ( isImageASelected() )
			{
				ViewJFrameBase.loadLUTandTransferFunctionFrom(imageA, LUTa, command.equals("OpenLUT"), 
						fName, dName, false);
				panelA.updateFrames(false);
			}
			if ( isImageBSelected() )
			{
				ViewJFrameBase.loadLUTandTransferFunctionFrom(imageB, LUTb, command.equals("OpenLUT"),
						fName, dName, false );
				panelB.updateFrames(false);
			}
		} 
		else if (command.equals("SaveLUT"))
		{
			if ( isImageASelected() )
			{
				// save both the LUT and the transfer functions
				ViewJFrameBase.saveLUTAs(imageA, LUTa, !imageA.isColorImage());
			}
			else if ( isImageBSelected() )
			{
				// save both the LUT and the transfer functions
				ViewJFrameBase.saveLUTAs(imageB, LUTb, !imageB.isColorImage());
			}
		} 
		else if (command.equals("SaveFuncts"))
		{
			if ( isImageASelected() )
			{
				// save only the transfer functions
				ViewJFrameBase.saveLUTAs(imageA, LUTa, false);
			}
			if ( isImageBSelected() )
			{
				// save only the transfer functions
				ViewJFrameBase.saveLUTAs(imageB, LUTb, false);
			}
		}
		else if (command.equals("SaveUDLUT"))
		{
			String fName = "userdefine.lut";
			String dName = Preferences.getPreferencesDir();
			if ( isImageASelected() )
			{
				// save both the LUT and the transfer functions
				ViewJFrameBase.saveLUTandTransferFunction(imageA, (ModelLUT)LUTa, fName, dName);
			}
			if ( isImageBSelected() )
			{
				// save both the LUT and the transfer functions
				ViewJFrameBase.saveLUTandTransferFunction(imageB, (ModelLUT)LUTb, fName, dName);
			}
		}  		 
		else if (command.equals("CloseLUT") || command.equals("Close")) 
		{
			containingFrame.setVisible(false);
			//dispose();
		} 
	}


	private void processUtilitiesMenu( ActionEvent event, String command )
	{

		if (command.equals("invertLUT")) {
			if ( isImageASelected() )
			{
				panelA.actionPerformed(event);
			}
			else if ( isImageBSelected() )
			{
				panelB.actionPerformed(event);
			}
		} 
		else if (command.equals("ChangeNColors")) {
			JDialogNColors dialog = new JDialogNColors(null);

			if (!dialog.isCancelled()) {

				if ( isImageASelected() )
				{
					panelA.setNColors(dialog.getNColors());
					((ModelLUT)LUTa).makeLUT(dialog.getNColors());
					setLUT(panelA, LUTa);
					panelA.updateFrames(false);
				}
				else if ( isImageBSelected() )
				{
					panelB.setNColors(dialog.getNColors());
					((ModelLUT)LUTb).makeLUT(dialog.getNColors());
					setLUT(panelB, LUTb);
					panelB.updateFrames(false);
				} 

			}
		} 
		else if (command.equals("Linear") )
		{
			if ( isImageASelected() )
			{
				panelA.resetHistoLUT();
			}
			if ( isImageBSelected() )
			{
				panelB.resetHistoLUT();
			}
		}
		else if (command.equals("UpdateA")) {
			if ( dualImage && (tabbedPane != null) )
			{
				tabbedPane.setSelectedIndex(0);
			}
			panelA.resetHistoLUT();
			menuObj.setMenuItemEnabled("Reset histogram & LUT A", false);
		} else if (command.equals("UpdateB") && dualImage) {	
			if ( dualImage && (tabbedPane != null) )
			{
				tabbedPane.setSelectedIndex(1);
			}
			panelB.resetHistoLUT();
			menuObj.setMenuItemEnabled("Reset histogram & LUT B", false);
		} else if (command.equals("ctPresetsLUT")) {
			if ( isImageASelected() )
			{
				panelA.actionPerformed(event);
			}
			else if ( isImageBSelected() )
			{
				panelB.actionPerformed(event);
			}
		} else if (command.equals("calcThresholdVolume")) {
			if (!menuObj.isMenuItemSelected("Calculate threshold volume"))
			{
				if ( isImageASelected() )
				{
					panelA.clearVoxelLabel();
				}
				else if ( isImageBSelected() )
				{
					panelB.clearVoxelLabel();
				}
			}
		}
	}


	@Override
	public void windowActivated(WindowEvent arg0) {}


	@Override
	public void windowClosed(WindowEvent arg0) {}


	@Override
	public void windowClosing(WindowEvent arg0) {
		disposeLocal();		
	}


	@Override
	public void windowDeactivated(WindowEvent arg0) {}


	@Override
	public void windowDeiconified(WindowEvent arg0) {}


	@Override
	public void windowIconified(WindowEvent arg0) {}


	@Override
	public void windowOpened(WindowEvent arg0) {}
}
