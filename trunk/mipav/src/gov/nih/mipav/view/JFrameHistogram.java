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


/**
 * Lookup table interface for either grayscale or color (RGB) images.
 * Either one or two images can be combined in a frame, and this LUT interface
 * enables the user to apply a color look-up table to the images.
 * 
 * Each image is treated separately, with an individual interface in a separate tabbed pane.
 * The images can be grayscale, color, or a combination (one grayscale, one RGB).
 * 
 * @see JPanelHistogram
 *
 */
public class JFrameHistogram extends JPanel implements ActionListener, ChangeListener, ViewImageUpdateInterface, WindowListener 
{

	private static final long serialVersionUID = 6057663900661366920L;

	/** reference to the colocalization frame for updatng the image. */
    private ViewJFrameColocalizationEM colEMFrame = null;

    /** reference to the colocalization frame for updatng the image. */
    private ViewJFrameColocalizationRegression colRegFrame = null;

	/** source image A */
	private ModelImage imageA = null;

	/** source image B */
	private ModelImage imageB = null;

	/** lut a */
	private ModelStorageBase LUTa;

	/** lut b */
	private ModelStorageBase LUTb;

	/** false = apply algorithm only to VOI regions. */
	private ViewJComponentRegistration regComponent = null;

	/** true = apply algorithm to the whole image */
	private boolean wholeImage = true; 

	/** Parent frame of this dialog, usually of type ViewJFrameImage. */
	private Frame parentFrame;

	/** Dialog that enables the user to choose to apply the lut to the entire image, or voi regions. */
	private JDialog voiDialog;

	/** frame containing the lut interface (if display in a stand-alone window) */
	private JFrame containingFrame;
	/** panel containing the lut interface (used in the stand-alone frame or in an outside frame) */
	private JPanel containingPanel;
	/** set to true when the lut interface contains two images (imageA and imageB) */
	private boolean dualImage;
	/** menu builder */
	private ViewMenuBuilder menuObj;
	/** tabbed pane, each separate tab contains the interface for imageA and imageB */
	private JTabbedPane tabbedPane = null;
	/** histogram interface panels for imageA and imageB */
	private JPanelHistogram panelA, panelB;
	
	private boolean useSeparateFrame = false;
	
	/**
	 * Creates the JFrameHistogram class. Initializes the images and LUTs, does not create the interface
	 * until the constructDialog() function is called, which asks the user if the luts should be applied to the VOI region
	 * or to the entire image, or until the histogramLUT() function is called to create the frame and interface panels.
	 * @param theParentFrame
	 * @param imA imageA
	 * @param imB imageB
	 * @param _LUTa lutA (either ModelLUT or ModelRGB)
	 * @param _LUTb lutb (either ModelLUT or ModelRGB)
	 */
	public JFrameHistogram(Frame theParentFrame, ModelImage imA, ModelImage imB, ModelStorageBase _LUTa, ModelStorageBase _LUTb) 
	{
		this( theParentFrame, null, imA, imB, _LUTa, _LUTb );
	}



	/**
	 * Creates the JFrameHistogram class. Initializes the images and LUTs, does not create the interface
	 * until the constructDialog() function is called, which asks the user if the luts should be applied to the VOI region
	 * or to the entire image, or until the histogramLUT() function is called to create the frame and interface panels.
	 * @param theParentFrame
	 * @param _regComponent registration component image to update on lut changes.
	 * @param imA imageA
	 * @param imB imageB
	 * @param _LUTa lutA (either ModelLUT or ModelRGB)
	 * @param _LUTb lutb (either ModelLUT or ModelRGB)
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
	
	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event)
	{
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
			histogramLUT(wholeImage, useSeparateFrame);
			voiDialog.setVisible(false);
			voiDialog.dispose();
		}
		else if (command.equals("Cancel"))
		{
			voiDialog.setVisible(false);
			voiDialog.dispose();
		} 
		else if (command.equals("calcThresholdVolume")) {
			if (!menuObj.isMenuItemSelected("Calculate threshold volume")) {
				getSelectedPanel().clearVoxelLabel();
			}
		}
	}
	
	/**
	 * Closes the stand-alone interface frame.
	 */
	public void closeFrame()
	{
		if ( containingFrame != null )
		{
			containingFrame.setVisible(false);
		}
		disposeLocal();
	}
	
	/**
	 * Closes imageB and removes the tabbed-pane interface, replacing it with a single interface panel.
	 */
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
	public void constructDialog( boolean separateFrame )
	{
		useSeparateFrame = separateFrame;
		
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
	
    /**
	 * Removes this from the image display listeners. Removes and deletes the interface panels.
	 */
	public void disposeLocal()
	{
		imageA.removeImageDisplayListener( this );
		if ( imageB != null )
		{
			imageB.removeImageDisplayListener( this );
		}
		if ( panelA != null )
		{
			panelA.disposeLocal();
		}
		if ( panelB != null )
		{
			panelB.disposeLocal();
		}
		colEMFrame = null;
		colRegFrame = null;
		regComponent = null;
		panelA = null;
		panelB = null;
		tabbedPane = null;
		containingPanel = null;
		containingFrame = null;
	}

	/**
	 * Returns true if 'Calculate threshold volume' is selected.
	 * @return true if 'Calculate threshold volume' is selected, false otherwise.
	 */
	public boolean doCalcThresholdVolume()
	{
		return (menuObj == null) || menuObj.isMenuItemSelected("Calculate threshold volume");
	}


	/**
	 * Returns the main interface panel for display in another interface.
	 * @return the main interface panel for display in another interface.
	 */
	public JPanel getContainingPanel()
	{
		return containingPanel;
	}


	/**
	 * Returns the lower threshold image value for whichever image is current selected.
	 * @return the lower threshold image value for whichever image is current selected.
	 */
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

	/**
	 * Returns the currently selected interface panel.
	 * @return the currently selected interface panel.
	 */
	public JPanelHistogram getSelectedPanel() {
		return isImageASelected() ? panelA : panelB;
	}
	
	/**
	 * Returns the upper threshold image value for whichever image is current selected.
	 * @return the upper threshold image value for whichever image is current selected.
	 */
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

	/**
	 * Creates the user interface for LUT changes. The entieFlag indicates if the entire image
	 * is to be used in the histogram calculation and LUT change, or if only the VOI regions are to
	 * be used. The separateFrame flag indicates if the interface is to be displayed in a stand-along
	 * window or inside another outside interface.
	 * @param entireFlag when true apply the LUT to the entire image, when false apply to VOI regions only.
	 * @param separateFrame when true create and display the LUT interface in a separate, stand-alone window,
	 * when false create the interface for display in another outside frame or panel.
	 */
	public void histogramLUT(boolean entireFlag, boolean separateFrame) 
	{
		// Create in a stand-alone window:
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
	 * Returns whether the imageA LUT panel is currently visible and active.
	 * @return whether the imageA LUT panel is currently visible and active.
	 */
	public boolean isImageASelected()
	{
		return (tabbedPane == null) || ((tabbedPane != null) && (tabbedPane.getSelectedIndex() == 0));
	}
	/**
	 * Returns whether the imageB LUT panel is currently visible and active.	 * 
	 * @return whether the imageB LUT panel is currently visible and active.
	 */
	public boolean isImageBSelected()
	{
		return (dualImage && (tabbedPane == null) || ((tabbedPane != null) && (tabbedPane.getSelectedIndex() == 1)));
	}




	/**
     * Returns true if image A or B is in dual threshold inverse mode (for JDialogConvertType input ranges).
     * @return  boolean is dual threshold inverse mode.
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

	/**
	 * Redraws the histogram interface for the currently selected image.
	 */
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
    public void resizePanel(int panelWidth, int frameHeight)
    {
    	if ( containingPanel != null )
    	{
    		containingPanel.setPreferredSize(new Dimension(panelWidth, frameHeight));
    		containingPanel.setSize(new Dimension(panelWidth, frameHeight));
    		containingPanel.revalidate();
    	}
    }
	
	/**
	 * Sets which image is currently active and makes the corresponding tabbed pane visible.
	 * @param image the image to set as currently active.
	 */
	public void setActiveImage( ModelImage image )
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

	/**
     * Sets the blue flag to be on or off.
     * @param isOn flag, either on or off.
     * @param isImageA when true apply to imageA, when false apply to imageB.
     */
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
	
    /**
	 * Called from ViewJFrameColocalizationEM. Enables LUT changes to be applied to that frame.
	 * @param colocalizationEMFrame
	 */
	public void setColocalizationEMFrame(ViewJFrameColocalizationEM colocalizationEMFrame)
	{
		colEMFrame = colocalizationEMFrame;
	}

	/**
	 * Called from ViewJFrameColocalizationRegression. Enables LUT changes to be applied to that frame.
	 * @param colocalizationEMFrame
	 */
	public void setColocalizationRegFrame(ViewJFrameColocalizationRegression colocalizationRegFrame)
	{ 
		colRegFrame = colocalizationRegFrame;
	}

	/**
     * Sets the green flag to be on or off.
     * @param isOn flag, either on or off.
     * @param isImageA when true apply to imageA, when false apply to imageB.
     */
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

    /**
	 * Sets the imageB for the LUT interface. If the interface is not currently a dual-panel interface, 
	 * it is re-created with the tabbed pane and two interface panels.
	 * @param image
	 * @param LUT
	 */
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
	
	/**
	 * Sets the LUT for the input panel.
	 * @param panel the panel to set the LUT for.
	 * @param LUT the new LUT.
	 */
	public void setLUT(JPanelHistogram panel, ModelStorageBase LUT)
	{
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
	
    /**
     * Set LUTa
     * @param LUT
     */
    public void setLUTA(ModelStorageBase LUT)
    {
    	LUTa = LUT;
    	updateFrames(false);
	}

    /**
     * Set LUTb
     * @param LUT
     */
    public void setLUTB(ModelStorageBase LUT)
    {
    	LUTb = LUT;
    	updateFrames(false);
	}

    /**
     * Sets the red flag to be on or off.
     * @param isOn flag, either on or off.
     * @param isImageA when true apply to imageA, when false apply to imageB.
     */
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
	public void setSlice(int slice) {} 

    @Override
	public void setTimeSlice(int tSlice) {} 

	/**
	 * Set the transfer function for imageA.
	 * @param txFunction
	 */
	public void setTransferFunctionA(TransferFunction txFunction)
	{
		ModelLUT selectedLUTa = !imageA.isColorImage() ? (ModelLUT)LUTa : null;
		if ( selectedLUTa != null )
		{
			selectedLUTa.setTransferFunction(txFunction);
		}
    } 

	/**
	 * Set the transfer function for imageA.
	 * @param txFunction
	 */
    public void setTransferFunctionB(TransferFunction txFunction)
    {
		ModelLUT selectedLUTb = !imageB.isColorImage() ? (ModelLUT)LUTb : null;
		if ( selectedLUTb != null )
		{
			selectedLUTb.setTransferFunction(txFunction);
		}
    }

	/* (non-Javadoc)
	 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
	 */
	public void stateChanged(ChangeEvent event)
    {
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
	 * @param flag
	 */
	public void updateFrames(boolean flag)
	{
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
	public boolean updateImageExtents()
	{
		return false;
	}

	@Override
	public boolean updateImages()
	{
		return false;
	}

	@Override
	public boolean updateImages(boolean flag)
	{
		return false;
	}

	@Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag,
			int interpMode) 
	{
		return false;
	}


	/**
	 * updates the interpolation mode for the images.
	 */
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


	/**
	 * Updates the interface behavior when the user choses to update 
	 * in real-time or on mouse release.
	 * @param updateRealTime
	 */
	public void updateRealTime( boolean updateRealTime )
	{
		panelA.updateRealTime(updateRealTime);
		if ( dualImage )
		{
			panelB.updateRealTime(updateRealTime);
		}
	}


	@Override
	public void windowActivated(WindowEvent arg0) {}


	@Override
	public void windowClosed(WindowEvent arg0) {}


	@Override
	public void windowClosing(WindowEvent arg0)
	{
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


	/**
	 * Creates the display panel. If there are two images the panel
	 * contains a tabbed pane with the two interfaces panels. If there is only
	 * one image the panel contains just the interface.
	 * @return the new display panel.
	 */
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


	/**
	 * Called when the FileMenu is activated. Processes the commands from the file menu.
	 * @param command the file menu command.
	 */
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


	/**
	 * Called when the Utilities Menu is activated. Processes the commands from the utilities menu.
	 * @param command the utilities menu command.
	 */
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
}
