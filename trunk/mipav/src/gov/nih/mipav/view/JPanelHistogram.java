package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.AlgorithmHistogram;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.structures.ModelHistogram;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.dialogs.JDialogCT;
import gov.nih.mipav.view.dialogs.JDialogThreshold;
import gov.nih.mipav.view.dialogs.JDialogThresholdLUT;
import gov.nih.mipav.view.dialogs.JDialogThresholdRGB;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class JPanelHistogram extends JPanel implements ActionListener, ChangeListener, ItemListener, KeyListener, HistoLUTParent {

	private static final long serialVersionUID = 2049904743460024433L;
	
	/** checkbox for turning on / off the blue component of the color look-up table. */
	private JCheckBox blueCheckBox;
	
	private JDialogCT ctDialog;
	
	/** checkbox for turning on / off the green component of the color look-up table. */
	private JCheckBox greenCheckBox;
	
	/** red, green and bue histograms: */
	protected ModelHistogram histogram, histogramG, histogramB;

	/** histogram panel interface. */
	protected ViewJPanelHistoLUT histoPanel;

	/** displays the selected LUT color */
	private JTextField indexColorTextF;

	/** turns interpolation on/off */
	private JCheckBox interpCheckBox;

	/** turns log display on/off */
	private JCheckBox logCheckBox;

	/** displays the number of colors in the LUT */
	private JTextField nColorsTextF;

	/** sets the minimum LUT value to (1,1,1) */
	private JCheckBox oneBasedLUTCheckBoxImage;

	/** enables the user to set the image output type or LUT-based image extration */
	private JComboBox outputBox;

	/** checkbox for turning on / off the red component of the color look-up table. */
	private JCheckBox redCheckBox;

	/** displays the threshold fill-value */
	private JTextField threshFillF;

	/** displays the threshold lower-value */
	private JTextField threshLowerF;
	
	/** displays the threshold upper-value */
	private JTextField threshUpperF;
	
	/** bottom tool bar */
	protected JToolBar toolBarBottom;

	/** threshold tool bar */
	protected JToolBar toolBarThreshold;

	/** enables the user to set updating the display in real-time or only on mouse-release */
	private JCheckBox updateCheckBox;
	
	/** volume threshold vaue */
	private JLabel voxelVolumeLabel;
	
	/** input image */
	protected ModelImage image = null;
	
	/** input LUT, either ModelLUT or ModelRGB */
	protected ModelStorageBase LUT;
	
	/** containing parent class. */
	protected JFrameHistogram panelParent = null;
	
	/** true = apply algorithm to the whole image */
	protected boolean wholeImage = true;
	
	/** Active mouse cursor index of the imageA, B and GM image A, B. */
	private int cursorIndex;
	
	/** X range value of the imageA, B and GM imageA, B. */
	private float rangeX;
	
	/** slider for small LUT changes */
	private JSlider mouseSlider;
	private JTextField xRangeText, yRangeText;
	protected int scaleRange;
	private JLabel[] mouseSliderLabels;
	private Hashtable<Integer, JLabel>labelsTable;
	
	/**
	 * Build the center part of the LUT toolbar.
	 * @param listener The listener to attach to the created LUT selection combo box.
	 * @return the top part of the LUT toolbar
	 */
	private static final JToolBar buildLUTSelectionList(ActionListener listener)
	{
		JToolBar LUTToolBar = new JToolBar();
		LUTToolBar.setBorderPainted(true);
		LUTToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
		LUTToolBar.setFloatable(false);

		Vector<String> lutStrings = getCustomLUTList();

		JComboBox lutList = new JComboBox(lutStrings);
		lutList.setBackground(Color.white);
		lutList.setSelectedIndex(0);
		lutList.addActionListener(listener);

		LUTToolBar.add(lutList);

		return LUTToolBar;
	}

	/**
	 * Creates a Vector<String> or LUT names representing different color look-up tables.
	 * @return a Vector<String> or LUT names representing different color look-up tables.
	 */
	private static final Vector<String> getCustomLUTList()
	{
		String listingFilename = ModelLUT.customLUTsLocation + "/LUT_listing";

		// use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
		URL listingFileURL = Thread.currentThread().getContextClassLoader().getResource(listingFilename);

		if (listingFileURL == null) {
			Preferences.debug("Unable to open " + listingFilename + ".\n", Preferences.DEBUG_MINOR);
		}

		// use buffering this implementation reads one line at a time
		Vector<String> lutStrings = new Vector<String>();
		try {
			// reading from a buffered reader pointed to a directory should return the files contained within it
			BufferedReader br = new BufferedReader(new InputStreamReader(listingFileURL.openStream()));
			String line = null;
			while ( (line = br.readLine()) != null) {
				lutStrings.add(line);
			}
		} catch (IOException e) {
			Preferences.debug("Unable to create custom LUT list: " + e.getMessage() + ".\n", Preferences.DEBUG_MINOR);
			e.printStackTrace();
		}

		return lutStrings;
	}

	/**
	 * returns true if the first entry in the color look-up table
	 * is (1,1,1), false otherwise.
	 * @param LUT the LUT to query, either a ModelLUT or ModelRGB
	 * @return true if the first entry in the color look-up table
	 * is (1,1,1), false otherwise.
	 */
	protected final static boolean isLUT1Based(ModelStorageBase LUT)
	{
		Color color = null;
		if ( LUT instanceof ModelLUT )
		{
			color = ((ModelLUT)LUT).getColor(0);
		}
		if ( LUT instanceof ModelRGB )
		{
			color = ((ModelRGB)LUT).getColor(0);
		}
		if ( color == null )
		{
			return false;
		}
		if ( (color.getRed() == 1) && (color.getGreen() == 1) && (color.getGreen() == 1))
		{
			return true;
		}
		return false;
	}

	/**
	 * Creates the JPanelHistogram, with the JFrameHistogram as the containing class.
	 * @param _panelParent parent class.
	 * @param _image input image.
	 * @param _LUT input LUT (ModelLUT or ModelRGB).
	 * @param _wholeImage, when true apply the LUT to the entire image, when false apply to VOI regions only.
	 */
	public JPanelHistogram(JFrameHistogram _panelParent, ModelImage _image, ModelStorageBase _LUT, boolean _wholeImage) 
	{
		super(new BorderLayout());
		this.image = _image;
		this.LUT = _LUT;
		this.wholeImage = _wholeImage;
		this.panelParent = _panelParent;
		buildPanel();
	}

	/**
	 * Creates the JPanelHistogram.
	 * @param _image input image.
	 * @param _LUT input LUT (ModelLUT or ModelRGB).
	 * @param _wholeImage, when true apply the LUT to the entire image, when false apply to VOI regions only.
	 */
	public JPanelHistogram(ModelImage _image, ModelStorageBase _LUT, boolean _wholeImage) 
	{
		this(null, _image, _LUT, _wholeImage);
	}


	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();

		// LUT Commands:
		int nColors = 256;
		if ( !image.isColorImage() )
		{
			String text = nColorsTextF.getText();
			if (MipavUtil.testParameter(text, 2, 256)) {
				nColors = Integer.valueOf(text).intValue();
			}
		}
		ModelLUT selectedLUT = !image.isColorImage() ? (ModelLUT)LUT : null;

		if (event.getActionCommand().equals("GrayLUT")) {
			selectedLUT.makeGrayTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("redLUT")) {
			System.out.println("test");
			selectedLUT.makeRedTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("greenLUT")) {
			selectedLUT.makeGreenTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("blueLUT")) {
			selectedLUT.makeBlueTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("graybrLUT")) {
			selectedLUT.makeGrayBRTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("HotMetalLUT")) {
			selectedLUT.makeHotMetalTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("spectrumLUT")) {
			selectedLUT.makeSpectrumTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("coolHotLUT")) {
			selectedLUT.makeCoolHotTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("skinLUT")) {
			selectedLUT.makeSkinTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("boneLUT")) {
			selectedLUT.makeBoneTransferFunctions();
			makeLUT(selectedLUT, nColors);
		} else if (event.getActionCommand().equals("stripedLUT")) {
			selectedLUT.makeStripedLUT();
			makeLUT(selectedLUT, nColors, false);
		} else if (event.getActionCommand().equals("invertLUT")) {
			selectedLUT.invertLUT();
			makeLUT(selectedLUT, nColors, false);
		} else if (command.equals("ctPresetsLUT")) {
			if (histoPanel.getHistoLUTComponent() != null) {
				ctDialog = new JDialogCT(histoPanel, (ModelLUT)LUT);
				ctDialog.setVisible(true);
			}
		} else if (event.getActionCommand().equals("linearLUT")) {
			if ( !image.isColorImage() )
			{
				toolBarBottom.getComponentAtIndex(1).setEnabled(true);
				threshUpperF.setEnabled(false);
				threshLowerF.setEnabled(false);
				threshFillF.setEnabled(false);

				toolBarThreshold.getComponentAtIndex(0).setEnabled(false);
				toolBarThreshold.getComponentAtIndex(1).setEnabled(false);
				toolBarThreshold.getComponentAtIndex(3).setEnabled(false);
				toolBarThreshold.getComponentAtIndex(4).setEnabled(false);

				outputBox.setEnabled(false);
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.LINEAR);
				histoPanel.updateLUTRecorder();
			}
			else if ( image.isColorImage() ) {
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.ALL);
				histoPanel.getHistoLUTComponent().dualThresholdMode(ViewJComponentHLUTBase.NO_THRESHOLD);
				histoPanel.getHistoLUTComponent().linearMode();
			}
		} else if (event.getActionCommand().equals("resetLinearLUT")) {

			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().linearMode();
				histoPanel.updateLUTRecorder();
			}
		} else if (event.getActionCommand().equals("evendistriLUT")) {

			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().evenDistribution();
				histoPanel.updateLUTRecorder();
			}
		} else if (event.getActionCommand().equals("thresholdLUT")) {

			if ( !image.isColorImage() )
			{
				toolBarBottom.getComponentAtIndex(1).setEnabled(false);
				outputBox.setEnabled(true);
				toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
				toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
				toolBarThreshold.getComponentAtIndex(3).setEnabled(true);
				toolBarThreshold.getComponentAtIndex(4).setEnabled(false);
				selectedLUT.makeGrayTransferFunctions();
				makeLUT(selectedLUT, nColors);
				selectedLUT.setColor(255, new Color(200, 0, 0));
			}
			histoPanel.getHistoLUTComponent().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
			threshLowerF.setEnabled(true);
			threshUpperF.setEnabled(true);
			threshFillF.setEnabled(true);

			//float upper = ((selectedLUT.getTransferFunction().getPoint(4))).X;
			//float lower = ((selectedLUT.getTransferFunction().getPoint(1))).X;
			//threshLowerF.setText(Float.toString(lower));
			//threshUpperF.setText(Float.toString(upper));
			threshFillF.setText(Double.toString(image.getMin()));

			//if ((panelParent != null) && panelParent.doCalcThresholdVolume()) {
			//	calculateThreshold(lower, upper);
			//}

			histoPanel.updateLUTRecorder();

		} else if (event.getActionCommand().equals("inverseThresholdLUT")) {

			if ( !image.isColorImage() )
			{
				// turn on the run threshold button
				toolBarBottom.getComponentAtIndex(1).setEnabled(false);

				outputBox.setEnabled(true);
				toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
				toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
				toolBarThreshold.getComponentAtIndex(3).setEnabled(false);
				toolBarThreshold.getComponentAtIndex(4).setEnabled(true);
				selectedLUT.setColor(255, new Color(200, 0, 0));
			}
			histoPanel.getHistoLUTComponent().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD_INV);
			threshLowerF.setEnabled(true);
			threshUpperF.setEnabled(true);
			threshFillF.setEnabled(true);
			threshFillF.setText(Double.toString(image.getMin()));
			histoPanel.updateLUTRecorder();

		} else if (event.getActionCommand().equals("runInverseThreshold")
				|| event.getActionCommand().equals("runThreshold")) {
			boolean isInverse = true;

			if (event.getActionCommand().equals("runThreshold")) {
				isInverse = false;
			}

			if ( image.isColorImage() )
			{
				float[] fillValues = new float[3];
				if (MipavUtil.testParameter(threshFillF.getText(), image.getMin(), image.getMax())) {
					int mode = histoPanel.getHistoLUTComponent().getMode();
					float value = new Float(threshFillF.getText()).floatValue();

					if (mode == ViewJComponentHLUTBase.RED) {
						fillValues[0] = value;
					} 
					else if (mode == ViewJComponentHLUTBase.GREEN) {
						fillValues[1] = value;
					} 
					else if (mode == ViewJComponentHLUTBase.BLUE) {
						fillValues[2] = value;
					} 
					else if (mode == ViewJComponentHLUTBase.ALL) {
						fillValues[0] = value;
						fillValues[1] = value;
						fillValues[2] = value;
					}
				}
                else {
                    threshFillF.requestFocus();
                    threshFillF.selectAll();
                }
	            // set up the threshold fields
	            float[] r = null;
	            float[] g = null;
	            float[] b = null;

	    		ModelRGB RGB = image.isColorImage() ? (ModelRGB)LUT : null;
	            if (RGB.getRedFunction().size() == 6) {
	            	r = new float[2];
	            	r[0] = (RGB.getRedFunction().getPoint(1)).X;
	            	r[1] = (RGB.getRedFunction().getPoint(3)).X;
	            }

	            if (RGB.getGreenFunction().size() == 6) {
	            	g = new float[2];
	            	g[0] = (RGB.getGreenFunction().getPoint(1)).X;
	            	g[1] = (RGB.getGreenFunction().getPoint(3)).X;
	            }

	            if (RGB.getBlueFunction().size() == 6) {
	            	b = new float[2];
	            	b[0] = (RGB.getBlueFunction().getPoint(1)).X;
	            	b[1] = (RGB.getBlueFunction().getPoint(3)).X;
	            }

	            JDialogThresholdRGB dialog = new JDialogThresholdRGB();


                dialog.runFromLUTFrame(image, r, g, b, fillValues, (command.equals("runThresholdInverse")));

			}
			else
			{
			if (MipavUtil.testParameter(threshLowerF.getText(), image.getMin(),
					((selectedLUT.getTransferFunction().getPoint(3))).X)) {

				if (MipavUtil.testParameter(threshUpperF.getText(), ((selectedLUT.getTransferFunction()
						.getPoint(2))).X, image.getMax())) {

					int outputType = outputBox.getSelectedIndex();

					if (outputType == 0) {
						outputType = AlgorithmThresholdDual.ORIGINAL_TYPE;
					} else if (outputType == 1) {
						outputType = AlgorithmThresholdDual.BINARY_TYPE;
					} else { // outputType == 2
						outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
					}

					// run threshold algorithm
					JDialogThreshold threshD = new JDialogThreshold();

					threshD.runFromLUTFrame(image,
							new Float(threshLowerF.getText()).floatValue(), new Float(threshUpperF.getText())
					.floatValue(), new Float(threshFillF.getText()).floatValue(), outputType,
					isInverse);

				} else {
					threshUpperF.requestFocus();
					threshUpperF.selectAll();
				}

			} else {
				threshLowerF.requestFocus();
				threshLowerF.selectAll();
			}
			}
		} else if (event.getActionCommand().equals("otsuThreshold")) {

			int maxBin = histogram.getOtsuThreshold();

			double dif = image.getMax() - image.getMin();

			double factor = dif / histogram.getExtents()[0];

			double otsu = ( (maxBin * factor) + image.getMin());

			if ( (otsu > image.getMin()) && (otsu < image.getMax())) {

				if (histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
					threshLowerF.setText(Double.toString(otsu));
					threshUpperF.setText(Double.toString(image.getMax()));
					histoPanel.getHistoLUTComponent().updateDualThreshold((float) otsu, (float) image.getMax());
					calculateThreshold((float) otsu, (float) image.getMax());
				} else {
					threshUpperF.setText(Double.toString(otsu));
					threshLowerF.setText(Double.toString(image.getMin()));
					histoPanel.getHistoLUTComponent().updateDualThreshold((float) image.getMin(),
							(float) otsu);
					calculateThreshold((float) image.getMin(), (float) otsu);
				}
			}
		} else if (event.getActionCommand().equals("maxEntThreshold")) {

			int maxBin = histogram.getMaxEntropyThreshold();

			double dif = image.getMax() - image.getMin();

			double factor = dif / histogram.getExtents()[0];

			double ent = ( (maxBin * factor) + image.getMin());

			if ( (ent > image.getMin()) && (ent < image.getMax())) {

				if (histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
					threshLowerF.setText(Double.toString(ent));
					threshUpperF.setText(Double.toString(image.getMax()));
					histoPanel.getHistoLUTComponent().updateDualThreshold((float) ent,
							(float) image.getMax());

					if ((panelParent != null) && panelParent.doCalcThresholdVolume()) {
						calculateThreshold((float) ent, (float) image.getMax());
					}
				} else {
					threshUpperF.setText(Double.toString(ent));
					threshLowerF.setText(Double.toString(image.getMin()));
					histoPanel.getHistoLUTComponent().updateDualThreshold((float) image.getMin(),
							(float) ent);

					if ((panelParent != null) && panelParent.doCalcThresholdVolume()) {
						calculateThreshold((float) image.getMin(), (float) ent);
					}
				}

			}
		} else if (event.getActionCommand().equals("alpha")) {

			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.ALPHA);
			}
		} else if (event.getActionCommand().equals("red")) {
			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.RED);
				if ( image.isColorImage() )
				{
					histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogram);
					ViewJComponentHistoRGB histoLUTComponent = (ViewJComponentHistoRGB)histoPanel.getHistoLUTComponent();
					if (histoLUTComponent.getThresholdMode() != ViewJComponentHLUTBase.NO_THRESHOLD) {
						histoLUTComponent.dualThresholdMode(histoLUTComponent.getThresholdMode());
					}
				}
			}
		} else if (event.getActionCommand().equals("green")) {

			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.GREEN);
				if ( image.isColorImage() )
				{
					histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogramG);
					ViewJComponentHistoRGB histoLUTComponent = (ViewJComponentHistoRGB)histoPanel.getHistoLUTComponent();
					if (histoLUTComponent.getThresholdMode() != ViewJComponentHLUTBase.NO_THRESHOLD) {
						histoLUTComponent.dualThresholdMode(histoLUTComponent.getThresholdMode());
					}
				}
			}
		} else if (event.getActionCommand().equals("blue")) {

			if (histoPanel.getHistoLUTComponent() != null) {
				histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.BLUE);
				if ( image.isColorImage() )
				{
					histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogramB);
					ViewJComponentHistoRGB histoLUTComponent = (ViewJComponentHistoRGB)histoPanel.getHistoLUTComponent();
					if (histoLUTComponent.getThresholdMode() != ViewJComponentHLUTBase.NO_THRESHOLD) {
						histoLUTComponent.dualThresholdMode(histoLUTComponent.getThresholdMode());
					}
				}
			}
		}  else if (command.equals("all")) {
            clearVoxelLabel();
                if (histoPanel.getHistoLUTComponent() != null) {
                	histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.ALL);
                }
        } else if (event.getActionCommand().equals("Threshold")) {

			if ((panelParent != null) && panelParent.doCalcThresholdVolume()) {
				calcThreshold();
			}
			/*
			if (image.getType() == ModelStorageBase.BOOLEAN) {
				setVisible(false);
				image.removeImageDisplayListener(panelParent);

				if (panelParent.getImageB() != null) {
					panelParent.getImageB().removeImageDisplayListener(panelParent);
				}

				dispose();
			} */

			updateFrames(false);
		} else if (event.getActionCommand().equals("GenerateLUT")) {
			histoPanel.showLUTRecorder();
		} else if (event.getSource() instanceof JComboBox) {
			JComboBox cb = (JComboBox) event.getSource();
			String lutName = (String) cb.getSelectedItem();
			selectedLUT.makeCustomizedLUT(lutName);
			if ( panelParent != null )
			{
				panelParent.setLUT(this, selectedLUT);
			}
			updateFrames(false);
		} else if (event.getActionCommand().equals("OpenUDLUT") || event.getActionCommand().equals("SaveUDLUT"))
		{
			if ( panelParent != null )
			{
				panelParent.actionPerformed(event);
			}
        } 
	}
	
	/**
	 * Calculates the thresholded image based on the parameters of the threshold transfer function. Image A is
	 * thresholded if the selected panel is for imageA and likewise for image B.
	 */
	public void calcThreshold()
	{
		ModelLUT selectedLUT = image.isColorImage() ? (ModelLUT)LUT : null;
		float[] thresholds = new float[2];

		thresholds[0] = ((selectedLUT.getTransferFunction().getPoint(1))).X;
		thresholds[1] = ((selectedLUT.getTransferFunction().getPoint(4))).X;

		JDialogThresholdLUT dialogLUT = new JDialogThresholdLUT(null, image,
				thresholds[0], thresholds[1]);

		if ( (dialogLUT.cancelFlag == false) && (image.getType() != ModelStorageBase.BOOLEAN)) {
			//updateHistoLUT(image, selectedLUT, null, null, false);
			histoPanel.getHistoLUTComponent().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
		} else if (image.getType() == ModelStorageBase.BOOLEAN) {
			selectedLUT.makeGrayTransferFunctions();
			makeLUT(selectedLUT, 256);
		}
	}
	
	/**
	 * Calculates the volume (for 3D images) or area (for 2D images) of the image between the two values from the upper
	 * and lower bounds text areas.
	 */
	public void calculateThreshold() {
		float upper, lower;

		if ((histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
				histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD)
		{

			try {
				lower = Float.parseFloat(threshLowerF.getText());
				upper = Float.parseFloat(threshUpperF.getText());
			} catch (Exception e) {
				return;
			}
			if (image.getNDims() == 3) {
				calculateThresholdVolume(lower, upper);
			} else {
				calculateThresholdArea(lower, upper);
			}
		}
	}
	
	/**
	 * Calculates the volume or area of the image between the two values from the upper and lower bounds (inclusive).
	 * 
	 * @param lower Lower bound of the threshold (inclusive).
	 * @param upper Upper bound of the threshold (inclusive).
	 */
	public void calculateThreshold(float lower, float upper) {

		if (image.getNDims() == 3) {
			calculateThresholdVolume(lower, upper);
		} else {
			calculateThresholdArea(lower, upper);
		}
	}
	
    /**
     * Resets the threshold volume and area labels.
     */
    public void clearVoxelLabel() {

		if (image.getNDims() == 3) {
			voxelVolumeLabel.setText("Threshold volume(red):");
		} else {
			voxelVolumeLabel.setText("Threshold area(red):");
		}
	}
    
    /**
     * Removes and deletes local memory.
     */
    public void disposeLocal()
    {
		image = null;
		LUT = null;
		panelParent = null;
		histogram.disposeLocal();
		histogram = null;
		if ( histogramG != null )
		{
			histogramG.disposeLocal();
			histogramG = null;
		}
		if ( histogramB != null )
		{
			histogramB.disposeLocal();
			histogramB = null;
		}
		histoPanel.disposeLocal();
		histoPanel = null;
    }
	
	@Override
	public void dragPoint(MouseEvent mouseEvent) {}
	
	/**
	 * @return the lower threshold value.
	 */
	public float getLowerThreshold()
	{
		return new Float(threshLowerF.getText()).floatValue();
	}

	/**
	 * @return he HIstoLUTComponent mode.
	 */
	public int getMode()
	{
		return histoPanel.getHistoLUTComponent().getMode();
	}

	/**
	 * @return the upper threshold value.
	 */
	public float getUpperThreshold()
	{
		return new Float(threshUpperF.getText()).floatValue();
	}


	/**
	 * @return if the image interpolation checkbox is selected.
	 */
	public boolean interpolateImage()
	{
		return interpCheckBox.isSelected();
	}


	/**
	 * @return if the update in real-time checkbox is selected.
	 */
	public boolean isImageUpdate()
	{
        return updateCheckBox.isSelected();
	}


	/* (non-Javadoc)
	 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
	 */
	public void itemStateChanged(ItemEvent event)
	{

		Object source = event.getSource();

		if (source == logCheckBox) {
			histoPanel.getHistoLUTComponent().setLogFlag(logCheckBox.isSelected());
			histoPanel.getHistoLUTComponent().showHistogram();
		}
		else if (source == interpCheckBox && (panelParent != null)) {
			panelParent.updateInterpolation();
		}
		else if (source == updateCheckBox && (panelParent != null)) {
			panelParent.updateRealTime(updateCheckBox.isSelected());
		}
		else if (source == outputBox) {
			threshFillF.setEnabled(outputBox.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE);
		} 
		else if (source == oneBasedLUTCheckBoxImage) {

			ModelLUT selectedLUT = !image.isColorImage() ? (ModelLUT)LUT : null;
			// get the color of the LUT index 0
			Color zeroIndexColor = selectedLUT.getColor(0);

			// test to see if the color is R == 0, G == 0, B == 0
			boolean zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor
					.getBlue() == 0));
			boolean zeroIndexColorIs111 = ( (zeroIndexColor.getRed() == 1) && (zeroIndexColor.getGreen() == 1) && (zeroIndexColor
					.getBlue() == 1));

			// if the user wants a 1-based LUT
			if (oneBasedLUTCheckBoxImage.isSelected() == true) {

				// only change index 0 to 1's if it is currently R == 0, G == 0, B == 0.
				if (zeroIndexColorIs000 == true) {
					selectedLUT.setColor(0, new Color(1, 1, 1));
				}
			} else {

				// only change index 1 to 0's if it is currently R == 1, G == 1, B == 1.
				if (zeroIndexColorIs111 == true) {
					selectedLUT.setColor(0, new Color(0, 0, 0));
				}
			}

			updateFrames(false);
		}
		else if (source == redCheckBox) 
		{
    		ModelRGB RGB = image.isColorImage() ? (ModelRGB)LUT : null;
			RGB.setROn( redCheckBox.isSelected() );
			updateFrames(false);
        }
		else if (source == greenCheckBox)
        {
    		ModelRGB RGB = image.isColorImage() ? (ModelRGB)LUT : null;
			RGB.setGOn( greenCheckBox.isSelected() );
			updateFrames(false);
        }
		else if (source == blueCheckBox)
        {
    		ModelRGB RGB = image.isColorImage() ? (ModelRGB)LUT : null;
			RGB.setBOn( blueCheckBox.isSelected() );
			updateFrames(false);
        } 

	}

	@Override
	public void keyPressed(KeyEvent arg0) {}
	
	@Override
	public void keyReleased(KeyEvent arg0) {}
	
	@Override
	public void keyTyped(KeyEvent event)
	{

        if (event.getKeyChar() == KeyEvent.VK_ENTER) {
    		ModelLUT selectedLUT = !image.isColorImage() ? (ModelLUT)LUT : null;
    		ModelRGB selectedRGB =  image.isColorImage() ? (ModelRGB)LUT : null;
    		TransferFunction function = (selectedLUT != null) ? selectedLUT.getTransferFunction() : null;

            if (event.getSource().equals(threshLowerF)) {

                if (MipavUtil.testParameter(threshLowerF.getText(), image.getMin(),
                        (selectedLUT.getTransferFunction().getPoint(3)).X)) {

                    histoPanel.getHistoLUTComponent().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                            new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshLowerF.requestFocus();
                    threshLowerF.selectAll();
                }
            } else if (event.getSource().equals(threshUpperF)) {

                if (MipavUtil.testParameter(threshUpperF.getText(), (selectedLUT.getTransferFunction()
                        .getPoint(2)).X, image.getMax())) {
                	histoPanel.getHistoLUTComponent().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                            new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshUpperF.requestFocus();
                    threshUpperF.selectAll();
                }
            } else if (event.getSource().equals(threshFillF)) {
                if ( !MipavUtil.testParameter(threshFillF.getText(), image.getMin(), image.getMax())) { 
                    threshFillF.requestFocus();
                    threshFillF.selectAll();
                }
            }
        }

	}

	/**
	 * Resets the histogram and histogram panel.
	 */
	public void resetHistoLUT()
	{

		histogram = calcHistogram(image, wholeImage, 1);
		histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogram);

        int mode = histoPanel.getHistoLUTComponent().getMode();
        if ( image.isColorImage() && (mode != ViewJComponentHLUTBase.RED) )
        {
        	if (mode == ViewJComponentHLUTBase.GREEN)
        	{
        		histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogramG);
        	}
        	else if (mode == ViewJComponentHLUTBase.BLUE)
        	{
        		histoPanel.getHistoLUTComponent().setHistogramInfo(image, histogramB);
        	}
        }
        if ( !image.isColorImage() )
		{
			histoPanel.getLUTComponent().show(null);
		}
        
		histoPanel.getHistoLUTComponent().linearMode();
		histoPanel.getHistoLUTComponent().showHistogram();
	}

	@Override
	public void setAllOff() {}

	/**
	 * Sets the blue checkbox on/off.
	 * @param isOn
	 */
	public void setBlueOn( boolean isOn )
	{
		if ( blueCheckBox != null )
		{
			blueCheckBox.setSelected(isOn);
		}
	}


	/**
	 * Sets the green checkbox on/off.
	 * @param isOn
	 */
	public void setGreenOn( boolean isOn )
	{
		if ( greenCheckBox != null )
		{
			greenCheckBox.setSelected(isOn);
		}
	}


	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#setLUT(gov.nih.mipav.model.structures.ModelLUT)
	 */
	public void setLUT(ModelLUT newLUT)
	{
		if ( panelParent != null )
		{
			panelParent.setLUT(this, newLUT);
		}
	}
	
	/**
	 * Sets the LUT and updates the histogram panel.
	 * @param newLUT
	 */
	public void setLUT(ModelStorageBase newLUT) {
		if ( newLUT == null )
		{
			return;
		}
		LUT = newLUT;
		if ( !image.isColorImage() )
		{
			histoPanel.getLUTComponent().show((ModelLUT)LUT);
			histoPanel.getHistoLUTComponent().showHistogram((ModelLUT)LUT);
		}
		if ( panelParent != null )
		{
			panelParent.setLUT(this, LUT);
		}
	}
	/**
     * Change the text field showing the number of colors.
     * 
     * @param value the number of colors
     */
    public void setNColors(int value) {
        nColorsTextF.setText(String.valueOf(value));
    }

	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#setRangeText(float, float, int)
	 */
	public void setRangeText(float x, float y, int _index) {

		if ((panelParent != null) && panelParent.doCalcThresholdVolume()) {
			calculateThreshold();
		}

		String start, mid, end;

		String str = String.valueOf(x);

		cursorIndex = _index;
		rangeX = x;

		int index = str.indexOf(".");
		int length = str.length();
		int indexE = str.indexOf("E");

		if ( ( (index + 2) < length) && (indexE == -1)) {
			str = str.substring(0, index + 2 + 1);
		} else if (indexE != -1) {
			str = str.substring(0, index + 2 + 1) + str.substring(indexE);
		}

		if (labelsTable == null) {
			//rangeText.setText(str);
			mouseSlider.setValue(255 - (int) y);
		} else {
			xRangeText.setText(str);
			yRangeText.setText(String.valueOf(255 - (int) y));

			// Change slider's labels
			start = MipavUtil.makeFloatString(x - scaleRange, 2);
			mid = MipavUtil.makeFloatString(x, 2);
			end = MipavUtil.makeFloatString(x + scaleRange, 2);
			mouseSliderLabels[0] = MipavUtil.createSliderLabel(start);
			mouseSliderLabels[1] = MipavUtil.createSliderLabel(mid);
			mouseSliderLabels[2] = MipavUtil.createSliderLabel(end);
			labelsTable = new Hashtable<Integer, JLabel>();
			labelsTable.put(0 + (start.length() / 2), mouseSliderLabels[0]);
			labelsTable.put(50 + (mid.length() / 2), mouseSliderLabels[1]);
			labelsTable.put(100 - (mid.length() / 2), mouseSliderLabels[2]);
			mouseSlider.setLabelTable(labelsTable);
			mouseSlider.repaint();
			mouseSlider.setValue(50);
		}

	}

	/**
	 * Sets the red checkbox on/off.
	 * @param isOn
	 */
	public void setRedOn( boolean isOn )
	{
		if ( redCheckBox != null )
		{
			redCheckBox.setSelected(isOn);
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
	 */
	public void stateChanged(ChangeEvent event)
	{
		Object source = event.getSource();
		if (source == mouseSlider) {
			float value, sliderValue;

			// componentOpacityA.updateCursor(rangeX, 100-mouseSlider.getValue(), cursorIndex);
			if (mouseSlider.getValueIsAdjusting() == true) {
				return;
			}

			if (xRangeText != null) {
				sliderValue = mouseSlider.getValue();

				if (mouseSliderLabels[0].getText().equals("0")) {
					value = rangeX + ( (sliderValue) / 100.0f * scaleRange * 2.0f);
				} else {

					if (sliderValue > 50) {
						value = rangeX + ( (sliderValue - 50) / 100.0f * scaleRange * 2.0f);
					} else {
						value = rangeX - ( (50 - sliderValue) / 100.0f * scaleRange * 2.0f);
					}
				}

				// value = (mouseSlider.getValue() / 100.0f * 32.0f) + rangeX;
				xRangeText.setText(MipavUtil.makeFloatString(value, 2));
				((ViewJComponentHistoLUT) histoPanel.getHistoLUTComponent()).updateCursorXPos(value,
						100 - mouseSlider.getValue(), cursorIndex);
			} 
			else 
			{
				((ViewJComponentHistoLUT) histoPanel.getHistoLUTComponent()).updateCursor(rangeX, 100 - mouseSlider.getValue(), cursorIndex);
			}
		}
	}


	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#updateComponentLUT()
	 */
	public void updateComponentLUT() {}


	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#updateFrames(boolean)
	 */
	public void updateFrames(boolean flag)
	{
		if ( histoPanel == null )
		{
			return;
		}
		if ( !image.isColorImage() )
		{
			histoPanel.getLUTComponent().show(null);
		}
		if ( histoPanel.getHistoLUTComponent() != null )
		{
			histoPanel.getHistoLUTComponent().showHistogram();
		}
		if ( panelParent != null )
		{
			panelParent.updateFrames(flag);
		}
	}


	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#updateLUTPositionString(java.lang.String)
	 */
	public void updateLUTPositionString(String str) 
	{
        indexColorTextF.setText(str);
	}

	/**
	 * @param updateRealTime
	 */
	public void updateRealTime( boolean updateRealTime )
	{
		updateCheckBox.removeItemListener(this);
		updateCheckBox.setSelected(updateRealTime);
		updateCheckBox.addItemListener(this);
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.HistoLUTParent#updateThresholdFields(float, float)
	 */
	public void updateThresholdFields(float lower, float upper) 
	{
        threshLowerF.setText(Float.toString(lower));
        threshUpperF.setText(Float.toString(upper));

        if ((histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
                (histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD))
        {
            if ((panelParent != null) && panelParent.doCalcThresholdVolume())
            {
                calculateThreshold(lower, upper);
            }
        }

	}

	/**
	 * Builds the interface panel.
	 * @param image
	 * @param addAdjustment
	 * @return
	 */
	protected JPanel buildControlPanel( ModelImage image, boolean addAdjustment )
	{
		JPanel controlPanel = new JPanel(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		controlPanel.setBorder(new EtchedBorder());

		updateCheckBox = new JCheckBox("Update image (real-time)", Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY));
		updateCheckBox.setFont(MipavUtil.font12);
		updateCheckBox.addItemListener(this);   
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.weightx = 1;
		gbc.weighty = 0;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(0, 5, 0, -5);
		controlPanel.add(updateCheckBox, gbc);


		logCheckBox = new JCheckBox("Log scale (Histogram)", true);
		logCheckBox.setFont(MipavUtil.font12);
		logCheckBox.addItemListener(this);
		gbc.gridx = 0;
		gbc.gridy++;
		gbc.insets = new Insets(0, 5, 0, -5);
		controlPanel.add(logCheckBox, gbc);

		// 
		if ( !image.isColorImage() )
		{
			interpCheckBox = new JCheckBox("Interpolate image display", Preferences.isInterpolateDisplay());
			interpCheckBox.setFont(MipavUtil.font12);
			interpCheckBox.addItemListener(this);

			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(interpCheckBox, gbc);
		}
		if ( addAdjustment )
		{
			oneBasedLUTCheckBoxImage = new JCheckBox("0 to 1 LUT adjustment", false);
			oneBasedLUTCheckBoxImage.setFont(MipavUtil.font12);
			oneBasedLUTCheckBoxImage.setToolTipText("Only relevant when the LUT's first index is either the color (0, 0, 0) or (1, 1, 1)");
			oneBasedLUTCheckBoxImage.setSelected(true);
			oneBasedLUTCheckBoxImage.addItemListener(this);

			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(oneBasedLUTCheckBoxImage, gbc);
		}

		if ( !image.isColorImage() )
		{
			String[] outputChoices = new String[] {image.getTypeString(), "Binary", "Short mask"};
			outputBox = new JComboBox(outputChoices);
			outputBox.setFont(MipavUtil.font12);
			outputBox.addItemListener(this);
			outputBox.setEnabled(false);

			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(outputBox, gbc);
		}

		if ( image.isColorImage() )
		{
			gbc.gridy++;


			JPanel colorPanel = new JPanel(new GridBagLayout());
			colorPanel.setBorder(new TitledBorder(new EtchedBorder(), "Image components", TitledBorder.LEFT,
					TitledBorder.CENTER, MipavUtil.font12B, Color.black));

			redCheckBox = new JCheckBox("Red", true);
			redCheckBox.setFont(MipavUtil.font12);
			redCheckBox.addItemListener(this);
			//RGBTA.setROn(true);

			greenCheckBox = new JCheckBox("Green", true);
			greenCheckBox.setFont(MipavUtil.font12);
			greenCheckBox.addItemListener(this);
			//RGBTA.setGOn(true);

			blueCheckBox = new JCheckBox("Blue", true);
			blueCheckBox.setFont(MipavUtil.font12);
			blueCheckBox.addItemListener(this);
			//RGBTA.setBOn(true);


			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(redCheckBox, gbc);

			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(greenCheckBox, gbc);

			gbc.gridx = 0;
			gbc.gridy++;
			gbc.insets = new Insets(0, 5, 0, -5);
			controlPanel.add(blueCheckBox, gbc);
		}

		gbc.gridy = 0;
		if ( !image.isColorImage() )
		{
			JLabel nColorsLabel = new JLabel("Number of colors: ");
			nColorsLabel.setFont(MipavUtil.font12);
			nColorsLabel.setForeground(Color.black);

			nColorsTextF = new JTextField("256");
			nColorsTextF.setFont(MipavUtil.font12);
			nColorsTextF.setEditable(false);

			gbc.gridx = 1;
			gbc.ipadx = -5;
			gbc.insets = new Insets(0, 10, 0, -20);
			controlPanel.add(nColorsLabel, gbc);
			gbc.gridx = 2;
			gbc.insets = new Insets(0, 0, 0, 5);
			controlPanel.add(nColorsTextF, gbc);

			JLabel indexColorLabel = new JLabel("LUT:");
			indexColorLabel.setFont(MipavUtil.font12);
			indexColorLabel.setForeground(Color.black);

			indexColorTextF = new JTextField(13);
			indexColorTextF.setFont(MipavUtil.font12);
			indexColorTextF.setEditable(false);

			gbc.gridx = 1;
			gbc.gridy++;
			gbc.insets = new Insets(0, 10, 0, -20);
			controlPanel.add(indexColorLabel, gbc);
			gbc.gridx = 2;
			gbc.insets = new Insets(0, 0, 0, 5);
			controlPanel.add(indexColorTextF, gbc);
		}



		JLabel threshLabel2 = new JLabel("Upper threshold:");
		threshLabel2.setFont(MipavUtil.font12);
		threshLabel2.setForeground(Color.black);

		// new textfields for thresholding
		gbc.gridx = 1;
		gbc.gridy++;
		gbc.insets = new Insets(0, 10, 0, -20);
		controlPanel.add(threshLabel2, gbc);

		threshUpperF = new JTextField(5);
		MipavUtil.makeNumericsOnly(threshUpperF, true);
		threshUpperF.setFont(MipavUtil.font12);
		threshUpperF.setEditable(false);
		threshUpperF.addKeyListener(this);

		gbc.gridx = 2;
		gbc.insets = new Insets(0, 0, 0, 5);
		controlPanel.add(threshUpperF, gbc);

		JLabel threshLabel = new JLabel("Lower threshold:");
		threshLabel.setFont(MipavUtil.font12);
		threshLabel.setForeground(Color.black);

		gbc.gridx = 1;
		gbc.gridy++;
		gbc.insets = new Insets(0, 10, 0, -20);
		controlPanel.add(threshLabel, gbc);

		threshLowerF = new JTextField(5);
		MipavUtil.makeNumericsOnly(threshLowerF, true);
		threshLowerF.setFont(MipavUtil.font12);
		threshLowerF.setEditable(false);
		threshLowerF.addKeyListener(this);

		gbc.gridx = 2;
		gbc.insets = new Insets(0, 0, 0, 5);
		controlPanel.add(threshLowerF, gbc);


		JLabel threshFillLabel = new JLabel("Fill value (non-red):");
		threshFillLabel.setFont(MipavUtil.font12);
		threshFillLabel.setForeground(Color.black);

		gbc.gridy++;
		gbc.gridx = 1;
		gbc.insets = new Insets(0, 10, 0, -20);
		controlPanel.add(threshFillLabel, gbc);


		threshFillF = new JTextField(5);
		MipavUtil.makeNumericsOnly(threshFillF, true);
		threshFillF.setFont(MipavUtil.font12);
		threshFillF.setEditable(false);
		threshFillF.addKeyListener(this);

		gbc.gridx = 2;
		gbc.insets = new Insets(0, 0, 0, 5);
		controlPanel.add(threshFillF, gbc);

		return controlPanel;
	}

	/**
	 * Builds the mouse panel.
	 * @param range
	 * @return
	 */
	protected JPanel buildMousePanel( double range )
	{
		JPanel panelMouse = new JPanel();
		JLabel mouseLabel = new JLabel("    X Scale");
		mouseLabel.setFont(MipavUtil.font12B);
		mouseLabel.setForeground(Color.black);

		scaleRange = ((int) Math.round( range ) + 1) / 256;

		mouseSliderLabels = new JLabel[3];
		mouseSliderLabels[0] = MipavUtil.createSliderLabel(String.valueOf(0));
		mouseSliderLabels[1] = MipavUtil.createSliderLabel(String.valueOf(scaleRange));
		mouseSliderLabels[2] = MipavUtil.createSliderLabel(String.valueOf(scaleRange * 2));

		labelsTable = new Hashtable<Integer, JLabel>();

		labelsTable.put(3, mouseSliderLabels[0]);
		labelsTable.put(50, mouseSliderLabels[1]);
		labelsTable.put(100, mouseSliderLabels[2]);

		mouseSlider = new JSlider(0, 100, 50);
		mouseSlider.setFont(MipavUtil.font12);
		mouseSlider.setMinorTickSpacing(10);
		mouseSlider.setPaintTicks(true);
		mouseSlider.addChangeListener(this);
		mouseSlider.setLabelTable(labelsTable);
		mouseSlider.setPaintLabels(true);
		mouseSlider.setAlignmentX(Component.LEFT_ALIGNMENT);
		mouseLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		mouseSlider.setEnabled(true);
		mouseLabel.setEnabled(true);
		mouseSliderLabels[0].setEnabled(true);
		mouseSliderLabels[1].setEnabled(true);
		mouseSliderLabels[2].setEnabled(true);

		JLabel textXRange = new JLabel("X Range");

		textXRange.setFont(MipavUtil.font12B);

		xRangeText = new JTextField(String.valueOf(0), 5);
		xRangeText.setFont(MipavUtil.font12);
		xRangeText.setEnabled(false);

		JLabel textYRange = new JLabel("Y Range");

		textYRange.setFont(MipavUtil.font12B);

		yRangeText = new JTextField(String.valueOf(0), 5);
		yRangeText.setFont(MipavUtil.font12);
		yRangeText.setEnabled(false);

		GridBagLayout cpGBL = new GridBagLayout();

		GridBagConstraints gbc = new GridBagConstraints();

		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 35;
		gbc.weighty = 35;

		panelMouse.setLayout(cpGBL);
		gbc.fill = GridBagConstraints.WEST;
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		panelMouse.add(textXRange, gbc);
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		panelMouse.add(xRangeText, gbc);

		gbc.gridx = 2;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		panelMouse.add(textYRange, gbc);
		gbc.gridx = 3;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		panelMouse.add(yRangeText, gbc);

		gbc.weightx = 70;
		gbc.weighty = 70;

		gbc.fill = GridBagConstraints.BOTH;
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		panelMouse.add(mouseLabel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.gridwidth = 4;
		gbc.gridheight = 1;
		panelMouse.add(mouseSlider, gbc);

		return panelMouse;
	}

	/**
	 * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
	 * 
	 * @param image Model of image
	 * @param LUT Model of LUT
	 * @param entireFlag Flag indicating if histogram should be made of entire image.
	 */
	protected void buildPanel()
	{
		JPanel panel = new JPanel(new BorderLayout());

		JPanel controlPanel = buildControlPanel(image, isLUT1Based(LUT));
		panel.add(controlPanel, BorderLayout.NORTH);

		histogram = calcHistogram(image, wholeImage, 1);
		if ( image.isColorImage() )
		{
			histogramG = calcHistogram(image, wholeImage, 2);
			histogramB = calcHistogram(image, wholeImage, 3);
		}
		histoPanel = new ViewJPanelHistoLUT(this, image, LUT, histogram);
		panel.add(histoPanel, BorderLayout.CENTER);

		if ( !image.isColorImage() )
		{
			JPanel panelMouse = buildMousePanel(image.getMax() - image.getMin());
			panel.add(panelMouse, BorderLayout.SOUTH);
		}
		if ( image.isColorImage() )
		{
        	histoPanel.getHistoLUTComponent().setMode(ViewJComponentHLUTBase.ALL);
		}

		add( buildToolBar(image), BorderLayout.NORTH);
		add( panel, BorderLayout.CENTER);
	}

	/**
	 * Builds the toolbars.
	 * @param image
	 * @return
	 */
	protected JPanel buildToolBar( ModelImage image )
	{
		ViewToolBarBuilder toolBarObj = new ViewToolBarBuilder(this);
		JPanel fullPanel = new JPanel(new BorderLayout());

		if ( image.isColorImage() )
		{
			JToolBar toolBar = toolBarObj.buildRGBToolBar();

			if (image.getNDims() == 3) {
				voxelVolumeLabel = new JLabel("Threshold volume(red):");
			} else {
				voxelVolumeLabel = new JLabel("Threshold area(red):");
			}
			voxelVolumeLabel.setFont(MipavUtil.font12);
			toolBar.add(voxelVolumeLabel);

			fullPanel.add(toolBar, BorderLayout.NORTH);
		}
		else
		{
			JToolBar toolBarTop = toolBarObj.buildLUTToolBarTop();
			JToolBar toolBarCenter = buildLUTSelectionList(this);
			toolBarBottom = toolBarObj.buildLUTToolBarBottom();
			toolBarThreshold = toolBarObj.buildLUTThresholdToolBar();

			if (image.getNDims() == 3) {
				voxelVolumeLabel = new JLabel("Threshold volume(red):");
			} else {
				voxelVolumeLabel = new JLabel("Threshold area(red):");
			}
			voxelVolumeLabel.setFont(MipavUtil.font12);
			toolBarThreshold.add(voxelVolumeLabel);

			JPanel topPanel = new JPanel(new BorderLayout());

			topPanel.add(toolBarTop, BorderLayout.NORTH);
			topPanel.add(toolBarCenter, BorderLayout.CENTER);
			topPanel.add(toolBarBottom, BorderLayout.SOUTH);

			fullPanel.add(topPanel, BorderLayout.NORTH);
			fullPanel.add(toolBarThreshold, BorderLayout.SOUTH);
		}


		return fullPanel;
	}
	/**
	 * Calculates histogram for the image(s).
	 * 
	 * @param imageAorB flag to indicate if histogram is to be calculated for imageA or imageB.
	 * @param entireFlag if true calculate histogram for the entire image. if false uses areas defined by VOI regions.
	 * @param progressFlag passed to calculateHistogram algorithm. If false progress bar is not displayed
	 */
	protected ModelHistogram calcHistogram(ModelImage image, boolean entireFlag, int offset) {

		int[] dimExtents = new int[1];
		dimExtents[0] = 256;

		ModelHistogram histogram = null;
		if ( !image.isColorImage() )
		{
			histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtents);

			AlgorithmHistogram histoAlgo = new AlgorithmHistogram(histogram, image, entireFlag);

			histoAlgo.setRunningInSeparateThread(false);
			histoAlgo.run();
		}
		else
		{
			if (image.getType() != ModelStorageBase.ARGB_USHORT) {
				dimExtents[0] = 256;
			} 
			else {
				dimExtents[0] = (int) (image.getMaxR() - image.getMinR() + 0.5) + 1;

				if (dimExtents[0] < 256) {
					dimExtents[0] = 256;
				}
			}
			histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtents);
			AlgorithmHistogram histoAlgo = new AlgorithmHistogram(histogram, offset, image, entireFlag);
			histoAlgo.run();
		}
		return histogram;
	}
	
	/**
	 * Calculates the area of the image between the two values from the upper and lower bounds (inclusive).
	 * 
	 * @param lower Lower bound of the threshold (inclusive).
	 * @param upper Upper bound of the threshold (inclusive).
	 */
	private void calculateThresholdArea(float lower, float upper) {

		int[] imageBuffer = new int[image.getExtents()[0] * image.getExtents()[1]];
		int numPixels = 0;

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);

			for (int j = 0; j < imageBuffer.length; j++) {

				if ( (imageBuffer[j] >= lower) && (imageBuffer[j] <= upper)) {
					numPixels++;
				}
			}
		} catch (IOException ioe) {
			return;
		}

		float[] res = new float[2];
		res[0] = Math.abs(image.getFileInfo(0).getResolutions()[0]);
		res[1] = Math.abs(image.getFileInfo(0).getResolutions()[1]);

		String units = image.getFileInfo(0).getAreaUnitsOfMeasureStr();

		voxelVolumeLabel.setText("Threshold area(red): " + String.valueOf(numPixels * res[0] * res[1]) + units);
	}
	
	/**
	 * Calculates the volume of the image between the two values from the upper and lower bounds (inclusive).
	 * 
	 * @param lower Lower bound of the threshold (inclusive).
	 * @param upper Upper bound of the threshold (inclusive).
	 */
	private void calculateThresholdVolume(float lower, float upper) {
		boolean isInverse = false;
		if (histoPanel.getHistoLUTComponent().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
			isInverse = true;
		}

		int[] imageBuffer = new int[image.getExtents()[0] * image.getExtents()[1]];
		int numVoxels = 0;

		for (int i = 0; i < image.getExtents()[2]; i++) {

			try {
				image.exportData(i * image.getExtents()[0] * image.getExtents()[1], imageBuffer.length, imageBuffer);

				for (int j = 0; j < imageBuffer.length; j++) {

					if ( !isInverse) {

						if ( (imageBuffer[j] >= lower) && (imageBuffer[j] <= upper)) {
							numVoxels++;
						}
					} else {

						if ( (imageBuffer[j] <= lower) || (imageBuffer[j] >= upper)) {
							numVoxels++;
						}
					}
				}
			} catch (IOException ioe) {
				return;
			}
		}

		float[] res = new float[3];
		res[0] = Math.abs(image.getFileInfo(0).getResolutions()[0]);
		res[1] = Math.abs(image.getFileInfo(0).getResolutions()[1]);
		res[2] = Math.abs(image.getFileInfo(0).getResolutions()[2]);

		String units = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();

		voxelVolumeLabel.setText("Threshold volume(red): " + String.valueOf(numVoxels * res[0] * res[1] * res[2])
				+ units);
	}
	
	/**
	 * Updates the number of colors in the LUT and updates the displays.
	 * @param selectedLUT
	 * @param nColors
	 */
	private void makeLUT( ModelLUT selectedLUT, int nColors )
	{
		makeLUT( selectedLUT, nColors, true );
	}


	/**
	 * Updates the number of colors in the LUT and updates the displays.
	 * @param selectedLUT
	 * @param nColors
	 * @param makeLUT
	 */
	private void makeLUT( ModelLUT selectedLUT, int nColors, boolean makeLUT )
	{
		if ( makeLUT )
		{
			selectedLUT.makeLUT(nColors);
		}
		histoPanel.getLUTComponent().show(selectedLUT);
		histoPanel.getHistoLUTComponent().showHistogram(selectedLUT);
		histoPanel.getLUTRecorder().updateLUT(selectedLUT);
		if ( panelParent != null )
		{
			panelParent.setLUT(this, selectedLUT);
		}
	}
}
