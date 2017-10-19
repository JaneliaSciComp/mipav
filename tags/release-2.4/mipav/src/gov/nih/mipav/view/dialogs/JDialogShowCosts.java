package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import javax.swing.*;

/**
 *   Calculate costs for various voxel similarity cost functions that are used
 *		in registration and output them to the data window.
 *   Algorithm is executed in its own thread.
 */
public class JDialogShowCosts extends JDialogBase {

  private ModelImage firstImage;
  private ModelImage secondImage = null;
  private ModelSimpleImage simpleImg1, simpleImg2;
  private JCheckBox linearCheckbox;
  private boolean doLinearRescale = true;
  private JComboBox imageComboBox;
  private ViewUserInterface UI;
  private JLabel labelImage;


  private int bin1, bin2;
  private JLabel bin1Label, bin2Label;
  private JTextField bin1Text, bin2Text;
  private double possibleIntValues1, possibleIntValues2;
  private boolean bin1Default, bin2Default;

  private AlgorithmCostFunctions algoCost = null;
  private AlgorithmCostFunctions2D algoCost2D = null;
  private TransMatrix tMatrix;
  private float smoothSize=1;
  private String currentCostFunct;

  /**
   *  Creates new dialog.
   *  @param theParentFrame    Parent frame
   *  @param im                Source image
   */
  public JDialogShowCosts(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
    firstImage = im;
    init();
  }

  /**
   *	Initializes GUI components and displays dialog.
   */
  private void init() {
    JPanel imagePanel;

    setForeground(Color.black);
    setTitle("Show pixel similarity cost function values");
    if (firstImage.getNDims() > 2) {
        tMatrix = new TransMatrix(4);
    }
    else {
        tMatrix = new TransMatrix(3);

    }
    /* Set up image panel */
    String matchName = firstImage.getImageName();
    labelImage = new JLabel("Pixel similarity costs between [" + matchName + "] and:");
    labelImage.setForeground(Color.black);
    labelImage.setFont(serif12);

    imageComboBox = buildComboBox(firstImage);
    imageComboBox.addItemListener(this);
    UI = firstImage.getUserInterface();
    String selectedName = (String) imageComboBox.getSelectedItem();
    if (selectedName == null) {
       MipavUtil.displayError("Comparison image must have the same dimensions and extents "
    				+"as the active image.  No comparison image found.");
       return;
    }
    secondImage = UI.getRegisteredImageByName(selectedName);

    imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    imagePanel.add(labelImage);
    imagePanel.add(imageComboBox);

	/* Set up the grid bag */
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridwidth = 2;
    gbc.gridheight = 1;
    gbc.anchor = gbc.WEST;
    gbc.weightx = 1;
    gbc.insets = new Insets(3, 3, 3, 3);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.gridx = 0;
    gbc.gridy = 0;

	/* Set up rescale panel */
    JPanel rescalePanel = new JPanel(new GridBagLayout());
    rescalePanel.setForeground(Color.black);
    rescalePanel.setBorder(buildTitledBorder("Linear rescaling"));
    linearCheckbox = new JCheckBox("Linearly rescale 2nd image");
    linearCheckbox.setFont(serif12);
    linearCheckbox.setForeground(Color.black);

    if (secondImage.getMax() == secondImage.getMin()) {
      linearCheckbox.setSelected(false);
      linearCheckbox.setEnabled(false);
    }
    else {
      linearCheckbox.setSelected(true);
      linearCheckbox.setEnabled(true);
    }
    linearCheckbox.addItemListener(this);
    rescalePanel.add(linearCheckbox, gbc);

    gbc.gridwidth = 1;
    gbc.gridx = 0;
    gbc.gridy = 1;

	/* Initialize the number of bins */
    bin1 = 256;
    possibleIntValues1 = firstImage.getMax() - firstImage.getMin() + 1;
    if ( ( (firstImage.getType() == ModelStorageBase.BYTE) ||
           (firstImage.getType() == ModelStorageBase.UBYTE) ||
           (firstImage.getType() == ModelStorageBase.SHORT) ||
           (firstImage.getType() == ModelStorageBase.USHORT) ||
           (firstImage.getType() == ModelStorageBase.INTEGER) ||
           (firstImage.getType() == ModelStorageBase.UINTEGER) ||
           (firstImage.getType() == ModelStorageBase.LONG)) &&
           (possibleIntValues1 < 256)) {
        bin1 = (int) Math.round(possibleIntValues1);
      }

    if (secondImage.getMax() == secondImage.getMin())
       bin2 = 1;
    else
       bin2 = bin1;

	/* Set up interface for changing number of bins - bin labels and text */
    bin1Label = new JLabel("Image 1 bin number ");
    bin1Label.setForeground(Color.black);
    bin1Label.setFont(serif12);
    rescalePanel.add(bin1Label, gbc);

    gbc.gridx = 1;
    gbc.gridy = 1;
    bin1Text = new JTextField();
    bin1Text.setText(String.valueOf(bin1));
    bin1Text.setFont(serif12);
    rescalePanel.add(bin1Text, gbc);

    gbc.gridx = 0;
    gbc.gridy = 2;
    bin2Label = new JLabel("Image 2 bin number ");
    bin2Label.setForeground(Color.black);
    bin2Label.setFont(serif12);
    rescalePanel.add(bin2Label, gbc);

    gbc.gridx = 1;
    gbc.gridy = 2;
    bin2Text = new JTextField();
    bin2Text.setText(String.valueOf(bin2));
    bin2Text.setFont(serif12);
    rescalePanel.add(bin2Text, gbc);

	/* Set up button panel */
    buildOKButton();
    buildCancelButton();
    JPanel buttonPanel = new JPanel();
    buttonPanel.add(OKButton);
    buttonPanel.add(cancelButton);

    getContentPane().add(imagePanel, BorderLayout.NORTH);
    getContentPane().add(rescalePanel, BorderLayout.CENTER);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    pack();
    setVisible(true);
  }

  /**
   *	Builds a list of images.  Returns combobox.
   *   List must be all color or all black and white.
   *	@return	Newly created combo box.
   */
  private JComboBox buildComboBox(ModelImage image) {
    ViewUserInterface UI;
    ModelImage nextImage;
    boolean doAdd;
    int i;

    JComboBox comboBox = new JComboBox();
    comboBox.setFont(serif12);
    comboBox.setBackground(Color.white);

    UI = image.getUserInterface();
    Enumeration names = UI.getRegisteredImageNames();

    while (names.hasMoreElements()) {
      String name = (String) names.nextElement();
      if (!name.equals(image.getImageName())) {
        nextImage = UI.getRegisteredImageByName(name);
        if (UI.getFrameContainingImage(nextImage) != null) {
          if (image.getNDims() == nextImage.getNDims() ) {
            doAdd = true;
            for (i = 0; i < image.getNDims(); i++) {
              if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                doAdd = false;
              }
            }
            if (doAdd) {
              comboBox.addItem(name);
            }
          }
        }
      }
    }
    return comboBox;
  }

  /**
   *	Closes dialog box when the OK button is pressed and calls the algorithm.
   *	@param event       Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    ViewJProgressBar progressBar;

    if (command.equals("OK")) {
      if (setVariables()) {
        dispose();

        progressBar = new ViewJProgressBar("Calculating costs", " ", 0, 100, false, this, this);
        int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        int yScreen = 100; //Toolkit.getDefaultToolkit().getScreenSize().height;
        progressBar.setLocation(xScreen/2, yScreen/2);
        progressBar.setVisible(true);

        progressBar.setMessage("Calculating correlation ratio");
    	currentCostFunct = "Correlation Ratio Smoothed";
        if (firstImage.getNDims() > 2) {
            callAlgorithm(AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED);
        }
        else {
            callAlgorithm(AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED);
        }
        progressBar.setMessage("Calculating mutual information");
        progressBar.updateValueImmed(25);
    	currentCostFunct = "Mutual Information Smoothed";
        if (firstImage.getNDims() > 2) {
            callAlgorithm(AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED);
        }
        else {
            callAlgorithm(AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED);
        }
        progressBar.setMessage("Calculating normalized mutual information");
        progressBar.updateValueImmed(50);
        currentCostFunct = "Normalized Mutual Information Smoothed";
        if (firstImage.getNDims() > 2) {
            callAlgorithm(AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
        }
        else {
            callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
        }
    	progressBar.setMessage("Calculating normalized cross correlation");
        progressBar.updateValueImmed(75);
        currentCostFunct = "Normalized Cross Correlation Smoothed";
        if (firstImage.getNDims() > 2) {
            callAlgorithm(AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED);
        }
        else {
            callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED);
        }
        progressBar.updateValue(100, true);
        MipavUtil.displayInfo("Calculations are complete.  "
        	+"Select the data tab in the output window to see results.");
        progressBar.dispose();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {
	/* Images */
    UI = firstImage.getUserInterface();
    String selectedName = (String) imageComboBox.getSelectedItem();
    secondImage = UI.getRegisteredImageByName(selectedName);
    if (secondImage == null) {
       return false;
    }

	simpleImg1 = new ModelSimpleImage(firstImage.getExtents(),
									  firstImage.getFileInfo(0).getResolutions(),
									  firstImage);

	simpleImg2 = new ModelSimpleImage(secondImage.getExtents(),
									  secondImage.getFileInfo(0).getResolutions(),
									  secondImage);

	/* Linear rescale */
    doLinearRescale = linearCheckbox.isSelected();

    /* Bins */
    String tmpStr;
    tmpStr = bin1Text.getText();
    bin1 = Integer.parseInt(tmpStr);
    if (bin1 < 1) {
       MipavUtil.displayError("Image 1 must have at least 1 bin");
       bin1Text.requestFocus();
       bin1Text.selectAll();
       return false;
     }
     else if ( (bin1 > Math.round(possibleIntValues1)) &&
             ( (firstImage.getType() == ModelStorageBase.BYTE) ||
                (firstImage.getType() == ModelStorageBase.UBYTE) ||
                (firstImage.getType() == ModelStorageBase.SHORT) ||
                (firstImage.getType() == ModelStorageBase.USHORT) ||
                (firstImage.getType() == ModelStorageBase.INTEGER) ||
                (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                (firstImage.getType() == ModelStorageBase.LONG))) {
        MipavUtil.displayError("Image 1 must not have more than " +
                               Math.round(possibleIntValues1) + " bins");
        bin1Text.requestFocus();
        bin1Text.selectAll();
        return false;
      }

      possibleIntValues2 = secondImage.getMax() - secondImage.getMin() + 1;
      tmpStr = bin2Text.getText();
      bin2 = Integer.parseInt(tmpStr);
      if (bin2 < 1) {
        MipavUtil.displayError("Image 2 must have at least 1 bin");
        bin2Text.requestFocus();
        bin2Text.selectAll();
        return false;
      }
      else if ( (bin2 > Math.round(possibleIntValues2)) &&
               (!doLinearRescale) &&
               ( (secondImage.getType() == ModelStorageBase.BYTE) ||
                (secondImage.getType() == ModelStorageBase.UBYTE) ||
                (secondImage.getType() == ModelStorageBase.SHORT) ||
                (secondImage.getType() == ModelStorageBase.USHORT) ||
                (secondImage.getType() == ModelStorageBase.INTEGER) ||
                (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                (secondImage.getType() == ModelStorageBase.LONG))) {
        MipavUtil.displayError("Image 2 must not have more than " +
                               Math.round(possibleIntValues2) + " bins");
        bin2Text.requestFocus();
        bin2Text.selectAll();
        return false;
      }
    return true;
  }

  private void callAlgorithm(int costChoice) {
    try {
      // Make algorithm
      if (firstImage.getNDims() > 2) {
          algoCost = new AlgorithmCostFunctions(simpleImg1, simpleImg2, costChoice,
                                                bin1, smoothSize);
      } else {
          algoCost2D = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, costChoice,
                                                bin1, smoothSize);
      }
    }
    catch (OutOfMemoryError x) {
      System.gc();
      MipavUtil.displayError("Dialog Show Costs: unable to allocate enough memory");
      return;
    }

    double cost;
    if (firstImage.getNDims() > 2) {
        cost = algoCost.cost(tMatrix);
    }
    else {
        cost = algoCost2D.cost(tMatrix);
    }
    UI.setDataText(currentCostFunct + ":\t" + cost + "\n");

    if (algoCost != null) {
        algoCost.disposeLocal();
        algoCost = null;
    }
    if (algoCost2D != null) {
        algoCost2D.disposeLocal();
        algoCost2D = null;
    }
  }

  /**
  *    Accessor to set the secondImage
  *    @param secondImage
  */
  public void setSecondImage(ModelImage secondImage) {
    this.secondImage = secondImage;
  }

  /**
  *   Accessor that sets whether or not linear rescaling occurs
  *   @param doLinearRescale
  */
  public void setDoLinearRescale(boolean doLinearRescale) {
    this.doLinearRescale = doLinearRescale;
  }

  /**
  *   Accessor that sets bin1
  *   @param bin1
  */
  public void setBin1(int bin1) {
    this.bin1 = bin1;
  }

  /**
  *   Accessor that sets bin2
  *   @param bin2
  */
  public void setBin2(int bin2) {
    this.bin2 = bin2;
  }

  public void setBin1Default(boolean bin1Default) {
	  this.bin1Default = bin1Default;
  }

  public void setBin2Default(boolean bin2Default) {
	  this.bin2Default = bin2Default;
  }


  //************************* Item Events ****************************
   //*******************************************************************

    /**
     *  itemStateChanged
     */
    public void itemStateChanged(ItemEvent event) {
      Object source = event.getSource();
      String tmpStr;

      if (source == imageComboBox) {
        UI = firstImage.getUserInterface();
        String selectedName = (String) imageComboBox.getSelectedItem();
        secondImage = UI.getRegisteredImageByName(selectedName);

        possibleIntValues2 = secondImage.getMax() - secondImage.getMin() + 1;
        if (secondImage.getMax() == secondImage.getMin()) {
          linearCheckbox.setSelected(false);
          linearCheckbox.setEnabled(false);
        }
        else {
          linearCheckbox.setEnabled(true);
        }
        doLinearRescale = linearCheckbox.isSelected();
        tmpStr = bin2Text.getText();
        bin2 = Integer.parseInt(tmpStr);
        if ( (bin2 > Math.round(possibleIntValues2)) &&
            (!doLinearRescale) &&
            ( (secondImage.getType() == ModelStorageBase.BYTE) ||
             (secondImage.getType() == ModelStorageBase.UBYTE) ||
             (secondImage.getType() == ModelStorageBase.SHORT) ||
             (secondImage.getType() == ModelStorageBase.USHORT) ||
             (secondImage.getType() == ModelStorageBase.INTEGER) ||
             (secondImage.getType() == ModelStorageBase.UINTEGER) ||
             (secondImage.getType() == ModelStorageBase.LONG))) {
          bin2 = (int) Math.round(possibleIntValues2);
        }
        bin2Text.setText(String.valueOf(bin2));

      } // if ( source == imageComboBox)

      else if (source == linearCheckbox) {
        doLinearRescale = linearCheckbox.isSelected();
        possibleIntValues2 = secondImage.getMax() - secondImage.getMin() + 1;
        tmpStr = bin2Text.getText();
        bin2 = Integer.parseInt(tmpStr);
        if ( (!doLinearRescale) &&
            ( (secondImage.getType() == ModelStorageBase.BYTE) ||
             (secondImage.getType() == ModelStorageBase.UBYTE) ||
             (secondImage.getType() == ModelStorageBase.SHORT) ||
             (secondImage.getType() == ModelStorageBase.USHORT) ||
             (secondImage.getType() == ModelStorageBase.INTEGER) ||
             (secondImage.getType() == ModelStorageBase.UINTEGER) ||
             (secondImage.getType() == ModelStorageBase.LONG))) {
                bin2 = (int) Math.round(possibleIntValues2);
        }
        else if (doLinearRescale) {
            bin2 = bin1;
        }
        bin2Text.setText(String.valueOf(bin2));
      } // else if (source == linearCheckbox)
    }
}