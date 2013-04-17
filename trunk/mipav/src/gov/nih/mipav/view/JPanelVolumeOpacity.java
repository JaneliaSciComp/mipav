package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.AlgorithmHistogram;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelHistogram;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.renderer.JPanelRendererBase;
import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityBase;
import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityListener;
import gov.nih.mipav.view.renderer.ViewJComponentVolOpacityRGB;
import gov.nih.mipav.view.renderer.J3D.ViewJComponentVolOpacity;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


/**
 * Simple panel containing the volume renderer opacity controls.
 */
public class JPanelVolumeOpacity extends JPanel implements ActionListener, ChangeListener, ItemListener, MouseListener, ViewJComponentVolOpacityListener
{

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -5320201609864903620L;
	/** Histogram reference for image A. */
	private ModelHistogram histogramA = null;

	/** Histogram reference for image B. */
	private ModelHistogram histogramB = null;

	/** Histogram reference for imageA GM. */
	private ModelHistogram histogramGM_A = null;

	/** Histogram reference for imageB GM. */
	private ModelHistogram histogramGM_B = null;

	private boolean isActiveImage = true;

	/** The main control. */
	protected JPanel mainPanel = null;

	/** Component tag indicator. */
	public static final int COMP_A = 0, COMP_B = 1, COMP_GM_A = 2, COMP_GM_B = 3;

	/** DOCUMENT ME! */
	protected static final String OPACITY_COMPONENT_TAB_A = "ImgA";

	/** DOCUMENT ME! */
	protected static final String OPACITY_COMPONENT_TAB_B = "ImgB";

	/** DOCUMENT ME! */
	protected static final String OPACITY_COMPONENT_TAB_A_GM = "ImgA_GM";

	/** DOCUMENT ME! */
	protected static final String OPACITY_COMPONENT_TAB_B_GM = "ImgB_GM";

	/** Component tag indicator. */
	public static final int RED = 1, GREEN = 2, BLUE = 3;

	/** Opacity slider of texture 3D volume opacity changes. */
	protected JSlider blendSlider;

	/** Reference to component opacity control A. */
	protected ViewJComponentVolOpacityBase componentOpacityA;

	/** Reference to component opacity control B. */
	protected ViewJComponentVolOpacityBase componentOpacityB;

	/** Reference to component opacity control A GM Rescale. */
	protected ViewJComponentVolOpacityBase componentOpacityGM_A;

	/** Reference to component opacity control B GM Rescale. */
	protected ViewJComponentVolOpacityBase componentOpacityGM_B;

	/** Gradient Magnitude Check box of the grey scale image. */
	protected JCheckBox GMCheckBox;
	/** Model image of the gradient magnitude of image A rescaled to have value in the range [0:255]. */
	protected ModelImage gradMagRescale_A;

	/** Model image of the gradient magnitude of image B rescaled to have value in the range [0:255]. */
	protected ModelImage gradMagRescale_B;
	/** Model image A. */
	protected ModelImage imageA;

	/** Model image B. */
	protected ModelImage imageB;

	/** Historgram dialog slider labels of the imageA, B and GM imageA, B. */
	protected Hashtable<Integer, JLabel> labelsTable, labelsTableB, labelsTableGM_A, labelsTableGM_B;

	/** DOCUMENT ME! */
	protected float middleLabelValueA;

	/** DOCUMENT ME! */
	protected float middleLabelValueB;

	/** DOCUMENT ME! */
	protected float middleLabelValueGM_A;

	//~ Instance fields ------------------------------------------------------------------------------------------------

	/** DOCUMENT ME! */
	protected float middleLabelValueGM_B;

	/** DOCUMENT ME! */
	protected JSlider mouseSlider, mouseSliderB;

	/** DOCUMENT ME! */
	protected JSlider mouseSliderGM_A, mouseSliderGM_B;

	/** The labels below the opacity slider. */
	protected JLabel[] opacitySliderLabelsA, opacitySliderLabelsB;

	/** DOCUMENT ME! */
	protected JLabel[] opacitySliderLabelsGM_A, opacitySliderLabelsGM_B;

	/** Panels that hold the the control components (opacity maps). */
	protected JPanel panelOpacityA;

	/** DOCUMENT ME! */
	protected JPanel panelOpacityB;

	/** DOCUMENT ME! */
	protected JPanel panelOpacityGM_A;

	/** DOCUMENT ME! */
	protected JPanel panelOpacityGM_B;

	/** X range text field in the imageA, B histogram dialog. */
	protected JTextField rangeText, rangeTextB;

	/** X range text field in the Gradient Magnitude imageA, B histogram dialog. */
	protected JTextField rangeTextGM_A, rangeTextGM_B;

	/** Scale range value according to the image min and max. */
	protected float scaleRangeA, scaleRangeB, scaleRangeGM_A, scaleRangeGM_B;

	/** Tabbed pane that contains the list of opacity functions. */
	protected JTabbedPane tabbedPane;

	/**
	 * X range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
	 * tri-planar volume view.
	 */
	protected JTextField xRangeTextA, xRangeTextB, xRangeTextGM_A, xRangeTextGM_B;

	/**
	 * Y range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
	 * tri-planar volume view.
	 */
	protected JTextField yRangeTextA, yRangeTextB, yRangeTextGM_A, yRangeTextGM_B;
	
	private boolean isChanging = false;

	/**
	 * Creates new dialog for converting type of image.
	 *
	 * @param  _imgA           Source imageA.
	 * @param  _imgB           Source imageB.
	 */
	public JPanelVolumeOpacity(ModelImage _imgA, ModelImage _imgB)
	{
		this(_imgA, _imgB, null, null );
	}

	/**
	 * Creates new dialog for converting type of image.
	 *
	 * @param  _imgA           Source imageA.
	 * @param  _imgB           Source imageB.
	 */
	public JPanelVolumeOpacity(ModelImage _imgA, ModelImage _imgB, ModelImage _imageAGM, ModelImage _imageBGM)
	{
		imageA = _imgA;
		imageB = _imgB;
		gradMagRescale_A = _imageAGM;
		gradMagRescale_B = _imageBGM;
		
		initialize();
	}

	/**
	 * Closes dialog box when the OK button is pressed and sets the variables.
	 *
	 * @param  event  Event that triggers this function
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();

		if (command.equals("OK")) { }
		else if (command.equals("Script")) { }
		else if (command.equals("Close")) { }
		else if (command.equals("OpenOpacityFrom")) { }
		else if (command.equals("SaveOpacityAs")) { }
		else if (command.equals("FullRange")) { }
		else if (command.equals("UserRange")) { }
		else if (command.equals("resetLinear")) {
			ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
			selectedComponent.linearMode();
		} else if (command.equals("resetLinearBackSlash")) {
			ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
			selectedComponent.linearBackSlashMode();
		} else if (command.equals("resetHorizon")) {
			ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
			selectedComponent.horizonMode();
		}
	} // end actionPerformed()

	/**
	 * Add the gradient magnitude histogram to the opacity control panel.
	 */
	public void addGM()
	{
		if (panelOpacityGM_A == null) {

			loadGM();
			buildPanelGM_A();

			if (imageB != null) {
				buildPanelGM_B();
			}
		} else {

			if (tabbedPane.indexOfComponent(panelOpacityGM_A) == -1) {
				tabbedPane.addTab(OPACITY_COMPONENT_TAB_A_GM, null, panelOpacityGM_A);
			}

			if (imageB != null) {

				if (tabbedPane.indexOfComponent(panelOpacityGM_B) == -1) {
					tabbedPane.addTab(OPACITY_COMPONENT_TAB_B_GM, null, panelOpacityGM_B);
				}
			}
		}
	}

	/**
	 * Clear memory and garbage collection.
	 */
	public void disposeLocal()
	{
		imageA = null;
		imageB = null;

		if (gradMagRescale_A != null) {
			gradMagRescale_A.disposeLocal();
			gradMagRescale_A = null;
		}

		if (gradMagRescale_B != null) {
			gradMagRescale_B.disposeLocal();
			gradMagRescale_B = null;
		}

		if (histogramA != null) {
			histogramA.disposeLocal();
			histogramA = null;
		}

		if (histogramB != null) {
			histogramB.disposeLocal();
			histogramB = null;
		}

		if (histogramGM_A != null) {
			histogramGM_A.disposeLocal();
			histogramGM_A = null;
		}

		if (histogramGM_B != null) {
			histogramGM_B.disposeLocal();
			histogramGM_B = null;
		}

		if (componentOpacityA != null) {
			componentOpacityA.dispose();
			componentOpacityA = null;
		}

		if (componentOpacityB != null) {
			componentOpacityB.dispose();
			componentOpacityB = null;
		}

		if (componentOpacityGM_A != null) {
			componentOpacityGM_A.dispose();
			componentOpacityGM_A = null;
		}

		if (componentOpacityGM_B != null) {
			componentOpacityGM_B.dispose();
			componentOpacityGM_B = null;
		}
	}

	/**
	 * This function returns the value of the blending slider.
	 * @return  int the value of the slider, or a default of 50 (the median value) if the slider has not been
	 *          initialized
	 */
	public int getAlphaBlendSliderValue()
	{
		if (blendSlider == null) {
			return 50; // 50 is the halfway point of the slider (0 min, 100 max)

			// if it hasn't been initialized yet, assume half opacity
		}

		return blendSlider.getValue();
	}

	/**
	 * Return the opacity histrogram component of the imageA.
	 * @return  componentOpacityA opacity component A.
	 */
	public ViewJComponentVolOpacityBase getCompA()
	{
		return componentOpacityA;
	}

	/**
	 * Return the opacity histrogram component of the Gradient Magnitude imageA.
	 * @return  componentOpacityGM_A gradient magnitude component A.
	 */
	public ViewJComponentVolOpacityBase getCompA_GM()
	{
		return componentOpacityGM_A;
	}

	/**
	 * Return the opacity histrogram component of the imageB.
	 * @return  componentOpacityB opacity component B.
	 */
	public ViewJComponentVolOpacityBase getCompB()
	{
		return componentOpacityB;
	}

	/**
	 * Return the opacity histrogram component of the Gradient Magnitude imageB.
	 * @return  componentOpacityGM_B gradient magnitude component B.
	 */
	public ViewJComponentVolOpacityBase getCompB_GM()
	{
		return componentOpacityGM_B;
	}

	/**
	 * Get the gradient magnitude imageA.
	 * @return  ModelImage GM imageA
	 */
	public ModelImage getGradMagA()
	{
		return gradMagRescale_A;
	}

	/**
	 * Get the gradient magnitude imageB.
	 * @return  ModelImage GM imageB
	 */
	public ModelImage getGradMagB()
	{
		return gradMagRescale_B;
	}

	/**
	 * Accessor that returns the imageA.
	 * @return  image
	 */
	public ModelImage getImageA()
	{
		return imageA;
	}

	/**
	 * Accessor that returns the image.
	 * @return  image
	 */
	public ModelImage getImageB()
	{
		return imageB;
	}

	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen. Change the layout of the volume opacity
	 * change diagram.
	 *
	 * @return  DOCUMENT ME!
	 */
	public JPanel getMainPanel()
	{
		return mainPanel;
	}

	/**
	 * Return the selected tabbbed pane component. Should be overridden in JPanelVolOpacity and JPanelVolOpacityRGB
	 * @return  ViewJComponentHLUTBase the opacity component.
	 */
	public ViewJComponentVolOpacityBase getSelectedComponent()
	{
		int selectedTab = tabbedPane.getSelectedIndex();

		if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A)) {
			return componentOpacityA;
		} else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B)) {
			return componentOpacityB;
		} else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A_GM)) {
			return componentOpacityGM_A;
		} else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B_GM)) {
			return componentOpacityGM_B;
		}

		return null;
	}

	/**
	 * Return the active image component.
	 * @param   _whichComp  true = imageA, false = imageB
	 * @return  componentOpacity A or B.
	 */
	public ViewJComponentVolOpacityBase getSelectedComponent(int _whichComp)
	{
		if (_whichComp == COMP_A) {
			return componentOpacityA;
		} else if (_whichComp == COMP_B) {
			return componentOpacityB;
		} else if (_whichComp == COMP_GM_A) {
			return componentOpacityGM_A;
		} else if (_whichComp == COMP_GM_B) {
			return componentOpacityGM_B;
		}
		return null;
	}

	/**
	 * Returns the index of the selected panel in the tabbed pane.
	 * @return
	 */
	public int getSelectedTabIndex()
	{
		return tabbedPane.getSelectedIndex();
	}

	/**
	 * Accessor to the tabbed pane.
	 * @return  JTabbedPane
	 */
	public JTabbedPane getTabbedPane()
	{
		return tabbedPane;
	}
	
	public boolean isChanging()
	{
		return isChanging;
	}

	/**
	 * Return whether or not the opacity mapping based on gradient magnitude is enabled.
	 * @return  boolean True if using gradient magnitude-based opacity mapping.
	 */
	public boolean isGradientMagnitudeOpacityEnabled()
	{
		if (null != GMCheckBox) {
			return GMCheckBox.isSelected();
		} 
		return false;
	}

	/**
	 * Sets the flags for the checkboxes.
	 * @param  event  event that triggered this function
	 */
	public synchronized void itemStateChanged(ItemEvent event)
	{
		Object source = event.getSource();
		if (source == GMCheckBox) {
			if (GMCheckBox.isSelected() == true) {
				addGM();
			} else {
				removeGM();
			}
			update(false);
		}
	}

	/**
	 * Not used method.
	 * @param  event  MouseEvent
	 */
	public void mouseClicked(MouseEvent event) { }

	/**
	 * Not used method.
	 * @param  event  MouseEvent
	 */
	public void mouseEntered(MouseEvent event) { }

	/**
	 * Not used method.
	 * @param  event  MouseEvent
	 */
	public void mouseExited(MouseEvent event) { }

	/**
	 * Not used method.
	 * @param  event  MouseEvent
	 */
	public void mousePressed(MouseEvent event) { }

	/**
	 * Mouse release event.
	 * @param  event  MouseEvent
	 */
	public void mouseReleased(MouseEvent event)
	{
		if (((event.getSource() == blendSlider) && (blendSlider.isEnabled() == true))) {
			int selectedTab = tabbedPane.getSelectedIndex();

			if ((imageA != null) &&
					(tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A) ||
							tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A_GM))) {
				imageA.notifyImageDisplayListeners();
			}

			if ((imageB != null) &&
					(tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B) ||
							tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B_GM))) {
				imageB.notifyImageDisplayListeners();
			}
		}
	}


	/**
	 * Add the gradient magnitude hitogram to the opacity control panel.
	 */
	public void removeGM()
	{
		if (imageA != null) {
			tabbedPane.remove(panelOpacityGM_A);
		}

		if (imageB != null) {
			tabbedPane.remove(panelOpacityGM_B);
		}
	}
	
	/**
     * Resize the control panel when mouse drag expanding or minimizing the frame.
     *
     * @param  panelWidth   width
     * @param  frameHeight  height
     */
    public void resizePanel(int panelWidth, int frameHeight) { }

	/**
	 * This method will enable or disable the opacity panel's slider component and input text fields, based on the value
	 * of the boolean parameter passed into it.
	 *
	 * @param  enabled  boolean whether the adjusters should be enabled or disabled
	 */
	public void setAdjustersEnabled(boolean enabled)
	{
		if (mouseSlider != null) {
			mouseSlider.setEnabled(enabled);
		}

		if (xRangeTextA != null) {
			xRangeTextA.setEnabled(enabled);
		}

		if (mouseSliderB != null) {
			mouseSliderB.setEnabled(enabled);
		}

		if (xRangeTextB != null) {
			xRangeTextB.setEnabled(enabled);
		}

		if (mouseSliderGM_A != null) {
			mouseSliderGM_A.setEnabled(enabled);
		}

		if (xRangeTextGM_A != null) {
			xRangeTextGM_A.setEnabled(enabled);
		}

		if (mouseSliderGM_B != null) {
			mouseSliderGM_B.setEnabled(enabled);
		}

		if (xRangeTextGM_B != null) {
			xRangeTextGM_B.setEnabled(enabled);
		}
	}

	/**
	 * Sets the blendSlider value.
	 * @param iValue new slider value.
	 */
	public void setAlphaBlendSliderValue(int iValue)
	{
		if (blendSlider != null) {
			blendSlider.setValue(iValue);
		}
	}

	/**
	 * Return whether or not the opacity mapping based on gradient magnitude is enabled.
	 * @return  boolean True if using gradient magnitude-based opacity mapping.
	 */
	public void setGradientMagnitudeOpacityEnabled(boolean value)
	{
		if (null != GMCheckBox) {
			GMCheckBox.setSelected(value); 
			if (GMCheckBox.isSelected() == true) {
				addGM();
			} else {
				removeGM();
			}
		} 
	}

	/**
	 * Sets the tabbed pane selected panel based on the input index.
	 * @param i
	 */
	public void setSelectedTabIndex(int i)
	{
		tabbedPane.setSelectedIndex(i);
	}

	/**
	 * ChangeListener for opacity slider changes.
	 * @param  event  The change event.
	 */
	public void stateChanged(ChangeEvent event)
	{
		if (event.getSource() == blendSlider)
		{
			update(blendSlider.getValueIsAdjusting());
		}
		else if (event.getSource() == mouseSlider) {

			if (mouseSlider.getValueIsAdjusting() == true) {
				return;
			}

			float upperLabel = Float.parseFloat(opacitySliderLabelsA[2].getText());
			float lowerLabel = Float.parseFloat(opacitySliderLabelsA[0].getText());

			scaleRangeA = (upperLabel - lowerLabel) / 2;

			float deltaValue;
			float sliderValue = mouseSlider.getValue();

			if (sliderValue > 50) {
				deltaValue = (sliderValue - 50) / 100.0f * scaleRangeA * 2.0f;
			} else {
				deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeA * 2.0f;
			}

			componentOpacityA.updateCursorXPos(middleLabelValueA + deltaValue);
		} else if (event.getSource() == mouseSliderB) {

			if (mouseSliderB.getValueIsAdjusting() == true) {
				return;
			}

			float upperLabel = Float.parseFloat(opacitySliderLabelsB[2].getText());
			float lowerLabel = Float.parseFloat(opacitySliderLabelsB[0].getText());

			scaleRangeB = (upperLabel - lowerLabel) / 2;

			float deltaValue;
			float sliderValue = mouseSliderB.getValue();

			if (sliderValue > 50) {
				deltaValue = (sliderValue - 50) / 100.0f * scaleRangeB * 2.0f;
			} else {
				deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeB * 2.0f;
			}

			componentOpacityB.updateCursorXPos(middleLabelValueB + deltaValue);

		} else if (event.getSource() == mouseSliderGM_A) {

			if (mouseSliderGM_A.getValueIsAdjusting() == true) {
				return;
			}

			float upperLabel = Float.parseFloat(opacitySliderLabelsGM_A[2].getText());
			float lowerLabel = Float.parseFloat(opacitySliderLabelsGM_A[0].getText());

			scaleRangeGM_A = (upperLabel - lowerLabel) / 2;

			float deltaValue;
			float sliderValue = mouseSliderGM_A.getValue();

			if (sliderValue > 50) {
				deltaValue = (sliderValue - 50) / 100.0f * scaleRangeGM_A * 2.0f;
			} else {
				deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeGM_A * 2.0f;
			}

			componentOpacityGM_A.updateCursorXPos(middleLabelValueGM_A + deltaValue);
		} else if (event.getSource() == mouseSliderGM_B) {

			if (mouseSliderGM_B.getValueIsAdjusting() == true) {
				return;
			}

			float upperLabel = Float.parseFloat(opacitySliderLabelsGM_B[2].getText());
			float lowerLabel = Float.parseFloat(opacitySliderLabelsGM_B[0].getText());

			scaleRangeGM_B = (upperLabel - lowerLabel) / 2;

			float deltaValue;
			float sliderValue = mouseSliderGM_B.getValue();

			if (sliderValue > 50) {
				deltaValue = (sliderValue - 50) / 100.0f * scaleRangeGM_B * 2.0f;
			} else {
				deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeGM_B * 2.0f;
			}

			componentOpacityGM_B.updateCursorXPos(middleLabelValueGM_B + deltaValue);
		}
	}

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.renderer.ViewJComponentVolOpacityListener#update()
	 */
	public void update( boolean isChanging )
	{
		this.isChanging = isChanging;
		firePropertyChange("Opacity", 0, 1);
	}

	/**
	 * Adjust the opacity slider for the minor opacity changes.
	 *
	 * @param  childComponent  ViewJComponentVolOpacityBase reference
	 */
	public void updateSlider(ViewJComponentVolOpacityBase childComponent)
	{
		try {
			if (childComponent == componentOpacityA) {
				mouseSlider.setLabelTable(getLabelTableA());
				mouseSlider.removeChangeListener(this);
				mouseSlider.setValue(50);
				mouseSlider.addChangeListener(this);

				middleLabelValueA = Float.parseFloat(opacitySliderLabelsA[1].getText());
			} else if (childComponent == componentOpacityB) {
				mouseSliderB.setLabelTable(getLabelTableB());
				mouseSliderB.removeChangeListener(this);
				mouseSliderB.setValue(50);
				mouseSliderB.addChangeListener(this);

				middleLabelValueB = Float.parseFloat(opacitySliderLabelsB[1].getText());

			} else if (childComponent == componentOpacityGM_A) {
				mouseSliderGM_A.setLabelTable(getLabelTableGM_A());
				mouseSliderGM_A.removeChangeListener(this);
				mouseSliderGM_A.setValue(50);
				mouseSliderGM_A.addChangeListener(this);

				middleLabelValueGM_A = Float.parseFloat(opacitySliderLabelsGM_A[1].getText());

			} else if (childComponent == componentOpacityGM_B) {
				mouseSliderGM_B.setLabelTable(getLabelTableGM_B());
				mouseSliderGM_B.removeChangeListener(this);
				mouseSliderGM_B.setValue(50);
				mouseSliderGM_B.addChangeListener(this);

				middleLabelValueGM_B = Float.parseFloat(opacitySliderLabelsGM_B[1].getText());

			}
		} catch (Exception e) { } // do nothing, just return
	}

	/**
	 * Build the blend slider control panel.
	 * @return  blendPanel built blend panel.
	 */
	protected JPanel buildBlendPanel()
	{
		Hashtable<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();

		labels.put(new Integer(0), new JLabel("Image A"));
		labels.put(new Integer(25), new JLabel("0.75A"));
		labels.put(new Integer(50), new JLabel("0.5A/B"));
		labels.put(new Integer(75), new JLabel("0.75B"));
		labels.put(new Integer(100), new JLabel("Image B"));

		blendSlider = new JSlider(0, 100, 50);
		blendSlider.setFont(MipavUtil.font12);
		blendSlider.setMinorTickSpacing(25);
		blendSlider.setPaintTicks(true);
		blendSlider.setLabelTable(labels);
		blendSlider.setPaintLabels(true);
		blendSlider.addMouseListener(this);
		blendSlider.addChangeListener(this);
		blendSlider.setEnabled(imageB != null);

		JPanel blendPanel = new JPanel(new GridLayout(1, 1));

		blendPanel.add(blendSlider);
		blendPanel.setBorder(JPanelRendererBase.buildTitledBorder("Blending"));

		return blendPanel;
	}

	/**
	 * Calls dispose.
	 * @throws  Throwable  DOCUMENT ME!
	 */
	protected void finalize() throws Throwable
	{
		disposeLocal();
		super.finalize();
	}

	/**
	 * Build the hash table for the imageA opacity slider.
	 * @return  labels built label hash table.
	 */
	protected Hashtable<Integer, JLabel> getLabelTableA()
	{
		if (opacitySliderLabelsA == null) {
			initBlendSliderLabels();
		}

		int tfActiveIndex = componentOpacityA.getActiveIndex();

		if (tfActiveIndex == ViewJComponentVolOpacityBase.INACTIVE) {
			opacitySliderLabelsA[0].setText(" ");
			opacitySliderLabelsA[1].setText(" ");
			opacitySliderLabelsA[2].setText(" ");
		} else {
			TransferFunction tf = componentOpacityA.getOpacityTransferFunction();

			opacitySliderLabelsA[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 1), 1));
			opacitySliderLabelsA[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
			opacitySliderLabelsA[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 1), 1));
		}

		Hashtable<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();

		// the slider has a range of 0-100. the float values that are used as the text of the labels
		// get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
		// positioned at 3 and 97 to in-set them from the edges and not get cut off
		labels.put(new Integer(3), opacitySliderLabelsA[0]);
		labels.put(new Integer(50), opacitySliderLabelsA[1]);
		labels.put(new Integer(97), opacitySliderLabelsA[2]);

		return labels;
	}

	/**
	 * Build the hash table for the imageB opacity slider.
	 * @return  labels built label hash table.
	 */
	protected Hashtable<Integer, JLabel> getLabelTableB()
	{
		if (opacitySliderLabelsB == null) {
			initBlendSliderLabels();
		}

		int tfActiveIndex = componentOpacityB.getActiveIndex();

		if (tfActiveIndex == ViewJComponentVolOpacityBase.INACTIVE) {
			opacitySliderLabelsB[0].setText(" ");
			opacitySliderLabelsB[1].setText(" ");
			opacitySliderLabelsB[2].setText(" ");
		} else {
			TransferFunction tf = componentOpacityB.getOpacityTransferFunction();

			opacitySliderLabelsB[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
			opacitySliderLabelsB[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
			opacitySliderLabelsB[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
		}

		Hashtable<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();

		// the slider has a range of 0-100. the float values that are used as the text of the labels
		// get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
		// positioned at 3 and 97 to in-set them from the edges and not get cut off
		labels.put(new Integer(3), opacitySliderLabelsB[0]);
		labels.put(new Integer(50), opacitySliderLabelsB[1]);
		labels.put(new Integer(97), opacitySliderLabelsB[2]);

		return labels;
	}

	/**
	 * Build the hash table for the imageA GM opacity slider.
	 * @return  labels built label hash table.
	 */
	protected Hashtable<Integer, JLabel> getLabelTableGM_A()
	{
		if (opacitySliderLabelsGM_A == null) {
			initBlendSliderLabels();
		}

		int tfActiveIndex = componentOpacityGM_A.getActiveIndex();

		if (tfActiveIndex == ViewJComponentVolOpacityBase.INACTIVE) {
			opacitySliderLabelsGM_A[0].setText(" ");
			opacitySliderLabelsGM_A[1].setText(" ");
			opacitySliderLabelsGM_A[2].setText(" ");
		} else {
			TransferFunction tf = componentOpacityGM_A.getOpacityTransferFunction();

			opacitySliderLabelsGM_A[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
			opacitySliderLabelsGM_A[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
			opacitySliderLabelsGM_A[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
		}

		Hashtable<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();

		// the slider has a range of 0-100. the float values that are used as the text of the labels
		// get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
		// positioned at 3 and 97 to in-set them from the edges and not get cut off
		labels.put(new Integer(3), opacitySliderLabelsGM_A[0]);
		labels.put(new Integer(50), opacitySliderLabelsGM_A[1]);
		labels.put(new Integer(97), opacitySliderLabelsGM_A[2]);

		return labels;
	}

	/**
	 * Build the hash table for the imageA GM opacity slider.
	 * @return  labels built label hash table.
	 */
	protected Hashtable<Integer, JLabel> getLabelTableGM_B()
	{
		if (opacitySliderLabelsGM_B == null) {
			initBlendSliderLabels();
		}

		int tfActiveIndex = componentOpacityGM_B.getActiveIndex();

		if (tfActiveIndex == ViewJComponentVolOpacityBase.INACTIVE) {
			opacitySliderLabelsGM_B[0].setText(" ");
			opacitySliderLabelsGM_B[1].setText(" ");
			opacitySliderLabelsGM_B[2].setText(" ");
		} else {
			TransferFunction tf = componentOpacityGM_B.getOpacityTransferFunction();

			opacitySliderLabelsGM_B[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
			opacitySliderLabelsGM_B[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
			opacitySliderLabelsGM_B[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
		}

		Hashtable<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();

		// the slider has a range of 0-100. the float values that are used as the text of the labels
		// get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
		// positioned at 3 and 97 to in-set them from the edges and not get cut off
		labels.put(new Integer(3), opacitySliderLabelsGM_B[0]);
		labels.put(new Integer(50), opacitySliderLabelsGM_B[1]);
		labels.put(new Integer(97), opacitySliderLabelsGM_B[2]);

		return labels;
	}

	/**
	 * Initialize the blend slider labels.
	 */
	protected void initBlendSliderLabels()
	{
		if (opacitySliderLabelsA == null) {
			opacitySliderLabelsA = new JLabel[3];
		}

		opacitySliderLabelsA[0] = new JLabel(" ");
		opacitySliderLabelsA[1] = new JLabel(" ");
		opacitySliderLabelsA[2] = new JLabel(" ");
		opacitySliderLabelsA[0].setFont(MipavUtil.font12);
		opacitySliderLabelsA[1].setFont(MipavUtil.font12);
		opacitySliderLabelsA[2].setFont(MipavUtil.font12);

		if (opacitySliderLabelsB == null) {
			opacitySliderLabelsB = new JLabel[3];
		}

		opacitySliderLabelsB[0] = new JLabel(" ");
		opacitySliderLabelsB[1] = new JLabel(" ");
		opacitySliderLabelsB[2] = new JLabel(" ");
		opacitySliderLabelsB[0].setFont(MipavUtil.font12);
		opacitySliderLabelsB[1].setFont(MipavUtil.font12);
		opacitySliderLabelsB[2].setFont(MipavUtil.font12);

		if (opacitySliderLabelsGM_A == null) {
			opacitySliderLabelsGM_A = new JLabel[3];
		}

		opacitySliderLabelsGM_A[0] = new JLabel(" ");
		opacitySliderLabelsGM_A[1] = new JLabel(" ");
		opacitySliderLabelsGM_A[2] = new JLabel(" ");
		opacitySliderLabelsGM_A[0].setFont(MipavUtil.font12);
		opacitySliderLabelsGM_A[1].setFont(MipavUtil.font12);
		opacitySliderLabelsGM_A[2].setFont(MipavUtil.font12);

		if (opacitySliderLabelsGM_B == null) {
			opacitySliderLabelsGM_B = new JLabel[3];
		}

		opacitySliderLabelsGM_B[0] = new JLabel(" ");
		opacitySliderLabelsGM_B[1] = new JLabel(" ");
		opacitySliderLabelsGM_B[2] = new JLabel(" ");
		opacitySliderLabelsGM_B[0].setFont(MipavUtil.font12);
		opacitySliderLabelsGM_B[1].setFont(MipavUtil.font12);
		opacitySliderLabelsGM_B[2].setFont(MipavUtil.font12);
	}

	/**
	 * Loads the gradient magnitude image instead of recalculating the image.
	 * @param   dName     String User specified directory name.
	 * @param   fName     String GM image file name.
	 * @param   isImageA  boolean Indicates GM imageA or GM imageB
	 * @return  boolean Indicates loading GM image successful or not.
	 */
	protected boolean loadGMImage(String dName, String fName, boolean isImageA)
	{
		FileIO fileIO = new FileIO();

		fileIO.setQuiet(true);

		if (new File(dName + File.separator + fName).exists()) {

			if (isImageA) {
				gradMagRescale_A.disposeLocal();
				gradMagRescale_A = null;
				gradMagRescale_A = fileIO.readImage(fName, dName, false, null, false);
			} else { // is ImageB
				gradMagRescale_B.disposeLocal();
				gradMagRescale_B = null;
				gradMagRescale_B = fileIO.readImage(fName, dName, false, null, false);
			}

			return true;
		}
		return false;
	}


	/**
	 * Method that displays the histogram and controls to manipulate the opacity. Panel for image A.
	 */
	private void buildPanelA()
	{
		int borderSize = 3;
		if ( imageA.isColorImage() )
		{
			histogramA = calcHistogram(imageA, 256, 1);
			//histogramAGreen = calcHistogram(imageA, 2);
			//histogramABlue = calcHistogram(imageA, 3);

			// Make a display version of the histogram
			componentOpacityA = new ViewJComponentVolOpacityRGB(this, histogramA, imageA);
			componentOpacityA.setLocation(borderSize, borderSize);
			componentOpacityA.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityA.setBackground(new Color(190, 208, 230));
			componentOpacityA.setMode(ViewJComponentVolOpacityBase.ALL);
			componentOpacityA.linearMode();
		}
		else
		{
			histogramA = calcHistogram(imageA, 256, 0);
			scaleRangeA = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

			// Make a display version of the histogram
			componentOpacityA = new ViewJComponentVolOpacity(this, histogramA, imageA);
			componentOpacityA.setLocation(borderSize, borderSize);
			componentOpacityA.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityA.setBackground(new Color(190, 208, 230));
			componentOpacityA.linearMode();
		}



		panelOpacityA = new JPanel(new BorderLayout());
		panelOpacityA.setForeground(Color.black);
		panelOpacityA.setBorder(JPanelRendererBase.buildTitledBorder("Opacity function for image A"));

		mouseSlider = new JSlider(0, 100, 50);
		mouseSlider.setMinorTickSpacing(10);
		mouseSlider.setPaintTicks(true);
		mouseSlider.addChangeListener(this);
		mouseSlider.setPaintLabels(true);
		mouseSlider.setFont(MipavUtil.font12);
		mouseSlider.setEnabled((componentOpacityA.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityA.getOpacityTransferFunction().isEndpoint(componentOpacityA.getActiveIndex()));

		mouseSlider.setLabelTable(getLabelTableA());
		mouseSlider.setEnabled((componentOpacityA.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityA.getOpacityTransferFunction().isEndpoint(componentOpacityA.getActiveIndex()));

		JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
		precisionPanel.add(mouseSlider);
		precisionPanel.setBorder(JPanelRendererBase.buildTitledBorder("Transfer function precision adjustment"));

		panelOpacityA.add(componentOpacityA, BorderLayout.CENTER);
		panelOpacityA.add(precisionPanel, BorderLayout.SOUTH);

		tabbedPane.addTab(OPACITY_COMPONENT_TAB_A, null, panelOpacityA);
	}

	/**
	 * Method that displays the histogram and controls to manipulate the opacity. Panel for image B. Reset the layout.
	 */
	private void buildPanelB()
	{
		int borderSize = 3;
		if ( imageB.isColorImage() )
		{
			histogramB = calcHistogram(imageB, 256, 1);
			//histogramAGreen = calcHistogram(imageB, 2);
			//histogramABlue = calcHistogram(imageB, 3);

			// Make a display version of the histogram
			componentOpacityB = new ViewJComponentVolOpacityRGB(this, histogramB, imageB);
			componentOpacityB.setLocation(borderSize, borderSize);
			componentOpacityB.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityB.setBackground(new Color(190, 208, 230));
			componentOpacityB.setMode(ViewJComponentVolOpacityBase.ALL);
			componentOpacityB.linearMode();
		}
		else
		{
			histogramB = calcHistogram(imageB, 256, 0);
			scaleRangeB = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

			// Make a display version of the histogram
			componentOpacityB = new ViewJComponentVolOpacity(this, histogramB, imageB);
			componentOpacityB.setLocation(borderSize, borderSize);
			componentOpacityB.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityB.setBackground(new Color(190, 208, 230));
			componentOpacityB.linearMode();
		}

		panelOpacityB = new JPanel(new BorderLayout());
		panelOpacityB.setForeground(Color.black);
		panelOpacityB.setBorder(JPanelRendererBase.buildTitledBorder("Opacity function for image B"));

		mouseSliderB = new JSlider(0, 100, 50);
		mouseSliderB.setMinorTickSpacing(10);
		mouseSliderB.setPaintTicks(true);
		mouseSliderB.addChangeListener(this);
		mouseSliderB.setPaintLabels(true);
		mouseSliderB.setFont(MipavUtil.font12);
		mouseSliderB.setEnabled((componentOpacityB.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityB.getOpacityTransferFunction().isEndpoint(componentOpacityB.getActiveIndex()));

		mouseSliderB.setLabelTable(getLabelTableB());
		mouseSliderB.setEnabled((componentOpacityB.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityB.getOpacityTransferFunction().isEndpoint(componentOpacityB.getActiveIndex()));

		JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
		precisionPanel.add(mouseSliderB);
		precisionPanel.setBorder(JPanelRendererBase.buildTitledBorder("Transfer function precision adjustment"));

		panelOpacityB.add(componentOpacityB, BorderLayout.CENTER);
		panelOpacityB.add(precisionPanel, BorderLayout.SOUTH);

		tabbedPane.addTab(OPACITY_COMPONENT_TAB_B, null, panelOpacityB);
	}

	/**
	 * Method that displays the histogram and controls to manipulate the opacity. Panel for image GM_A.
	 */
	private void buildPanelGM_A()
	{
		int borderSize = 3;
		if ( gradMagRescale_A.isColorImage() )
		{
			histogramGM_A = calcHistogram( gradMagRescale_A, 1024, 1 );
	        componentOpacityGM_A = new ViewJComponentVolOpacityRGB(this, histogramGM_A, gradMagRescale_A);
	        componentOpacityGM_A.setLocation(3, 3);
	        componentOpacityGM_A.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
	        componentOpacityGM_A.setBackground(new Color(190, 208, 230));
	        componentOpacityGM_A.horizonMode();
	        componentOpacityGM_A.setMode(ViewJComponentVolOpacityBase.ALL);
	        componentOpacityGM_A.linearMode();
		}
		else
		{
			histogramGM_A = calcHistogram( gradMagRescale_A, 1024, 0 );
			scaleRangeGM_A = ((int) Math.round(gradMagRescale_A.getMax() - gradMagRescale_A.getMin()) + 1) / 256;

			// Make a display version of the histogram
			componentOpacityGM_A = new ViewJComponentVolOpacity(this, histogramGM_A, gradMagRescale_A);
			componentOpacityGM_A.setLocation(borderSize, borderSize);
			componentOpacityGM_A.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityGM_A.setBackground(new Color(190, 208, 230));
			componentOpacityGM_A.linearMode();
		}

		panelOpacityGM_A = new JPanel(new BorderLayout());
		panelOpacityGM_A.setForeground(Color.black);
		panelOpacityGM_A.setBorder(JPanelRendererBase.buildTitledBorder("Opacity function for image A gradient magnitude"));

		mouseSliderGM_A = new JSlider(0, 100, 50);
		mouseSliderGM_A.setMinorTickSpacing(10);
		mouseSliderGM_A.setPaintTicks(true);
		mouseSliderGM_A.addChangeListener(this);
		mouseSliderGM_A.setPaintLabels(true);
		mouseSliderGM_A.setFont(MipavUtil.font12);
		mouseSliderGM_A.setEnabled((componentOpacityGM_A.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityGM_A.getOpacityTransferFunction().isEndpoint(componentOpacityGM_A.getActiveIndex()));

		mouseSlider.setLabelTable(getLabelTableGM_A());
		mouseSlider.setEnabled((componentOpacityGM_A.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityGM_A.getOpacityTransferFunction().isEndpoint(componentOpacityGM_A.getActiveIndex()));

		JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
		precisionPanel.add(mouseSliderGM_A);
		precisionPanel.setBorder(JPanelRendererBase.buildTitledBorder("Transfer function precision adjustment"));

		panelOpacityGM_A.add(componentOpacityGM_A, BorderLayout.CENTER);
		panelOpacityGM_A.add(precisionPanel, BorderLayout.SOUTH);

		tabbedPane.addTab(OPACITY_COMPONENT_TAB_A_GM, null, panelOpacityGM_A);
	}

	/**
	 * Method that displays the histogram and controls to manipulate the opacity. Panel for image GM_B.
	 */
	private void buildPanelGM_B()
	{
		int borderSize = 3;
		if ( gradMagRescale_B.isColorImage() )
		{
			histogramGM_B = calcHistogram( gradMagRescale_B, 1024, 1 );
			componentOpacityGM_B = new ViewJComponentVolOpacityRGB(this, histogramGM_B, gradMagRescale_B);
			componentOpacityGM_B.setLocation(3, 3);
			componentOpacityGM_B.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityGM_B.setBackground(new Color(190, 208, 230));
			componentOpacityGM_B.horizonMode();
			componentOpacityGM_B.setMode(ViewJComponentVolOpacityBase.ALL);
			componentOpacityGM_B.linearMode();
		}
		else
		{
			histogramGM_B = calcHistogram( gradMagRescale_B, 1024, 0 );
			scaleRangeGM_B = ((int) Math.round(gradMagRescale_B.getMax() - gradMagRescale_B.getMin()) + 1) / 256;

			// Make a display version of the histogram
			componentOpacityGM_B = new ViewJComponentVolOpacity(this, histogramGM_B, gradMagRescale_B);
			componentOpacityGM_B.setLocation(borderSize, borderSize);
			componentOpacityGM_B.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
			componentOpacityGM_B.setBackground(new Color(190, 208, 230));
			componentOpacityGM_B.linearMode();
		}

		
		panelOpacityGM_B = new JPanel(new BorderLayout());
		panelOpacityGM_B.setForeground(Color.black);
		panelOpacityGM_B.setBorder(JPanelRendererBase.buildTitledBorder("Opacity function for image B gradient magnitude"));

		mouseSliderGM_B = new JSlider(0, 100, 50);
		mouseSliderGM_B.setMinorTickSpacing(10);
		mouseSliderGM_B.setPaintTicks(true);
		mouseSliderGM_B.addChangeListener(this);
		mouseSliderGM_B.setPaintLabels(true);
		mouseSliderGM_B.setFont(MipavUtil.font12);
		mouseSliderGM_B.setEnabled((componentOpacityGM_B.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityGM_B.getOpacityTransferFunction().isEndpoint(componentOpacityGM_B.getActiveIndex()));

		mouseSlider.setLabelTable(getLabelTableGM_B());
		mouseSlider.setEnabled((componentOpacityGM_B.getActiveIndex() != ViewJComponentVolOpacityBase.INACTIVE) &&
				!componentOpacityGM_B.getOpacityTransferFunction().isEndpoint(componentOpacityGM_B.getActiveIndex()));

		JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
		precisionPanel.add(mouseSliderGM_B);
		precisionPanel.setBorder(JPanelRendererBase.buildTitledBorder("Transfer function precision adjustment"));

		panelOpacityGM_B.add(componentOpacityGM_B, BorderLayout.CENTER);
		panelOpacityGM_B.add(precisionPanel, BorderLayout.SOUTH);

		tabbedPane.addTab(OPACITY_COMPONENT_TAB_B_GM, null, panelOpacityGM_B);
	}

	/**
	 * Calculates histogram for the imageA, B.
	 */
	private ModelHistogram calcHistogram(ModelImage image, int dim, int channel)
	{
		int[] dimExtentsA = new int[1];
		dimExtentsA[0] = dim;
		ModelHistogram histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

		AlgorithmHistogram histoAlgo;
		if ( image.isColorImage() )
		{
			histoAlgo = new AlgorithmHistogram(histogram, channel, image, true);
		}
		else
		{
			histoAlgo = new AlgorithmHistogram(histogram, image, true);
		}
		histoAlgo.setRunningInSeparateThread(false);
		histoAlgo.run();

		if (histoAlgo.isCompleted()) {
			histoAlgo.finalize();
			histoAlgo = null;
		}
		return histogram;
	}

	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen. Change the layout of the volume opacity
	 * change diagram.
	 */
	private void initialize()
	{
		if (mainPanel != null) {
			mainPanel.removeAll();
		} else {
			mainPanel = new JPanel();
		}

		setForeground(Color.black);

		JPanel toolbarPanel = new JPanel(new GridLayout(1, 7, 0, 0));
		toolbarPanel.setBorder(BorderFactory.createEtchedBorder());

		JToolBar ctlToolBar = new JToolBar();
		ctlToolBar.setBorder(BorderFactory.createEtchedBorder());
		ctlToolBar.setBorderPainted(true);
		ctlToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

		JButton linearButton = new JButton(MipavUtil.getIcon("linear.gif"));

		linearButton.addActionListener(this);
		linearButton.setRolloverIcon(MipavUtil.getIcon("linearroll.gif"));
		linearButton.setBorderPainted(false);
		linearButton.setToolTipText("Reset transfer function");
		linearButton.setActionCommand("resetLinear");
		ctlToolBar.add(linearButton);

		JButton linearBackSlashButton = new JButton(MipavUtil.getIcon("linearbackslash.gif"));

		linearBackSlashButton.addActionListener(this);
		linearBackSlashButton.setRolloverIcon(MipavUtil.getIcon("linearbackslashroll.gif"));
		linearBackSlashButton.setBorderPainted(false);
		linearBackSlashButton.setToolTipText("Reset transfer function");
		linearBackSlashButton.setActionCommand("resetLinearBackSlash");
		ctlToolBar.add(linearBackSlashButton);

		JButton horizonButton = new JButton(MipavUtil.getIcon("linearhorizon.gif"));

		horizonButton.addActionListener(this);
		horizonButton.setRolloverIcon(MipavUtil.getIcon("linearhorizonroll.gif"));
		horizonButton.setBorderPainted(false);
		horizonButton.setToolTipText("Reset transfer function");
		horizonButton.setActionCommand("resetHorizon");
		ctlToolBar.add(horizonButton);

		JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

		separator.setBorderPainted(false);
		separator.setFocusPainted(false);
		ctlToolBar.add(separator);

		GMCheckBox = new JCheckBox("Gradient Map", false);
		GMCheckBox.setFont(MipavUtil.font12);
		GMCheckBox.addItemListener(this);

		ctlToolBar.add(GMCheckBox);
		toolbarPanel.add(ctlToolBar, BorderLayout.WEST);

		tabbedPane = new JTabbedPane();
		tabbedPane.addChangeListener(this);
		tabbedPane.setFont(MipavUtil.font12B);

		buildPanelA();

		if (imageB != null) {
			buildPanelB();
		}

		JPanel blendPanel = buildBlendPanel();

		GridBagLayout gbLayout = new GridBagLayout();
		GridBagConstraints gbConstraints = new GridBagConstraints();
		JPanel subPanel = new JPanel(gbLayout);
		subPanel.setLayout(gbLayout);

		gbConstraints.weightx = 1;
		gbConstraints.fill = GridBagConstraints.HORIZONTAL;
		gbConstraints.anchor = GridBagConstraints.NORTH;
		subPanel.add(toolbarPanel, gbConstraints);

		gbConstraints.gridy = 1;
		gbConstraints.fill = GridBagConstraints.BOTH;
		subPanel.add(tabbedPane, gbConstraints);

		gbConstraints.gridy = 2;
		gbConstraints.weighty = 1;
		gbConstraints.fill = GridBagConstraints.HORIZONTAL;
		subPanel.add(blendPanel, gbConstraints);

		JScrollPane scrollPaneA = new JScrollPane(subPanel);
		scrollPaneA.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);

		mainPanel.setLayout(new GridLayout(1, 1));
		mainPanel.add(scrollPaneA);
	}

	/**
	 * Calculates histogram for the gradient magnitude imageA, B.
	 */
	private void loadGM()
	{
		if ( gradMagRescale_A == null )
		{
			String kImageName = ModelImage.makeImageName(imageA.getFileInfo(0).getFileName(), "");
			String dir = imageA.getFileInfo()[0].getFileDirectory().concat(kImageName + "_RenderFiles" + File.separator);
			ModelImage gradMag_A = VolumeImage.getGradientMagnitude( imageA, 0, dir );

			/** Scale the intensity range to 1024. */
			gradMagRescale_A = new ModelImage(imageA.getType(), imageA.getExtents(),
					imageA.getImageName() + "_gm_rescale");
			AlgorithmChangeType changeTypeAlgo_A = new AlgorithmChangeType(gradMagRescale_A, gradMag_A,
					gradMag_A.getMin(), gradMag_A.getMax(),
					0, 1023, false);

			changeTypeAlgo_A.setRunningInSeparateThread(false);
			changeTypeAlgo_A.run();
			gradMagRescale_A.calcMinMax();


			ModelImage gradMag_B;
			if ( imageB != null )
			{
				if ( gradMagRescale_B == null )
				{
					kImageName = ModelImage.makeImageName(imageB.getFileInfo(0).getFileName(), "");
					dir = imageB.getFileInfo()[0].getFileDirectory().concat(kImageName + "_RenderFiles" + File.separator);
					gradMag_B = VolumeImage.getGradientMagnitude( imageB, 0, dir );
					/** Scale the intensity range to 1024. */
					gradMagRescale_B = new ModelImage(imageB.getType(), imageB.getExtents(),
							imageB.getImageName() + "_gm_rescale");
					AlgorithmChangeType changeTypeAlgo_B = new AlgorithmChangeType(gradMagRescale_B, gradMag_B,
							gradMag_B.getMin(), gradMag_B.getMax(),
							0, 1023, false);

					changeTypeAlgo_B.setRunningInSeparateThread(false);
					changeTypeAlgo_B.run();
					gradMagRescale_B.calcMinMax();
				}
			}
		}
	}
}
