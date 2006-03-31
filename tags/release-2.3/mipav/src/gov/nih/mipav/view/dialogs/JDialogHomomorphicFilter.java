package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.text.DecimalFormat;

import javax.swing.*;

/**
 *   Dialog to get user input, then call the algorithm. The user has the
 *   option to generate a new image or replace the source image.
 *   It should be noted that the algorithms are executed in their own
 *   threads.
 *
 *		@version    1.0 February 25, 2005
 *		@author     William Gandler
 *       @see        AlgorithmFrequencyFilter
 *
 */
public class JDialogHomomorphicFilter
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, ItemListener {

  private ViewUserInterface userInterface;
  private AlgorithmFrequencyFilter FrequencyFilterAlgo = null;
  private ModelImage image; // source image
  private ModelImage resultImage = null; // result image

  private String titles[];

  private JPanel destinationPanel;
  private ButtonGroup destinationGroup;
  private JRadioButton replaceImage;
  private JRadioButton newImage;


  private JCheckBox image25DCheckbox;
  private boolean image25D = false;

  private JLabel labelLowGain;
  private JTextField textLowGain;
  private JLabel labelHighGain;
  private JTextField textHighGain;
  private JLabel labelLowTruncated;
  private JTextField textLowTruncated;
  private JLabel labelHighTruncated;
  private JTextField textHighTruncated;
  private JLabel labelOrder;
  private JTextField textOrder;
  private int butterworthOrder;

  private JPanel mainPanel;

  private JPanel filterPanel;
  private JLabel labelF1;
  private JTextField textF1;

  private float freq1 = 0.4f;
  private float lowGain = 0.5f;
  private float highGain = 2.0f;
  private float lowTruncated = 0.01f;
  private float highTruncated = 0.05f;
  private DecimalFormat nf = new DecimalFormat( "##0.0##" );

  private int displayLoc; // Flag indicating if a new image is to be generated
  // or if the source image is to be replaced
  /**
   *  @param parent          Parent frame.
   *  @param im              Source image.
   */
  public JDialogHomomorphicFilter(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, false);
    image = im;
    userInterface = ( (ViewJFrameBase) parentFrame).getUserInterface();
    init();
  }

  /**
  *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogHomomorphicFilter(ViewUserInterface UI, ModelImage im) {
    super();
    userInterface = UI;
    image = im;
    parentFrame = image.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogHomomorphicFilter() {}

  /**
    * Run this algorithm from a script.
    * @param parser the script parser we get the state from
    * @throws IllegalArgumentException if there is something wrong with the arguments in the script
    */
   public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
       String srcImageKey = null;
       String destImageKey = null;

       try {
           srcImageKey = parser.getNextString();
       } catch (Exception e) {
           throw new IllegalArgumentException();
       }
       ModelImage im = parser.getImage(srcImageKey);

       image = im;
       userInterface = image.getUserInterface();
       parentFrame = image.getParentFrame();

       // the result image
       try {
           destImageKey = parser.getNextString();
       } catch (Exception e) {
           throw new IllegalArgumentException();
       }

       if (srcImageKey.equals(destImageKey)) {
           this.setDisplayLocReplace();
       } else {
           this.setDisplayLocNew();
       }

       try {
           setImage25D(parser.getNextBoolean());
           setFreq1(parser.getNextFloat());
           setButterworthOrder(parser.getNextInteger());
           setLowGain(parser.getNextFloat());
           setHighGain(parser.getNextFloat());
           setLowTruncated(parser.getNextFloat());
           setHighTruncated(parser.getNextFloat());
       } catch (Exception e) {
           throw new IllegalArgumentException();
       }

       setActiveImage(parser.isActiveImage());
       setSeparateThread(false);
       callAlgorithm();
       if (!srcImageKey.equals(destImageKey)) {
           parser.putVariable(destImageKey, getResultImage().getImageName());
       }
   }

   /**
    * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
    * @param algo the algorithm to make an entry for
    */
   public void insertScriptLine (AlgorithmBase algo) {
       if (algo.isCompleted()) {
           if (userInterface.isScriptRecording()) {
               //check to see if the match image is already in the ImgTable
               if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {
                   if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                       userInterface.getScriptDialog().putActiveVar(image.getImageName());
                   }
               }

               userInterface.getScriptDialog().append("FrequencyFilter " +
                                                      userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                      " ");
               if (displayLoc == NEW) {
                   userInterface.getScriptDialog().putVar(resultImage.getImageName());
                   userInterface.getScriptDialog().append(userInterface.
                                                          getScriptDialog().getVar(resultImage.getImageName()) + " " +
                                                          image25D + " " + freq1 + " " + butterworthOrder +
                                                          " " + lowGain + " " + highGain + " " +
                                                          lowTruncated + " " + highTruncated + "\n");
               }
               else {
                   userInterface.getScriptDialog().append(userInterface.
                                                          getScriptDialog().getVar(image.getImageName()) + " " +
                                                          image25D + " " + freq1 + " " + butterworthOrder +
                                                          " " + lowGain + " " + highGain + " " +
                                                          lowTruncated + " " + highTruncated + "\n");
               }
           }
       }
   }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setTitle("Homomorphic Filter");

    JPanel optionsPanel = new JPanel(new GridLayout(1, 1));
    optionsPanel.setBorder(buildTitledBorder("Options"));

    image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
    image25DCheckbox.setFont(serif12);
    image25DCheckbox.setForeground(Color.black);
    if (image.getNDims() == 3) {
      image25DCheckbox.setEnabled(true);
      image25DCheckbox.setSelected(false);
    }
    else {
      image25DCheckbox.setEnabled(false);
      image25DCheckbox.setSelected(false);
    }
    optionsPanel.add(image25DCheckbox);

    destinationPanel = new JPanel(new GridLayout(2, 1));
    destinationPanel.setForeground(Color.black);
    destinationPanel.setBorder(buildTitledBorder("Destination"));

    destinationGroup = new ButtonGroup();
    newImage = new JRadioButton("New image", true);
    newImage.setFont(serif12);
    newImage.setForeground(Color.black);
    destinationGroup.add(newImage);
    destinationPanel.add(newImage);

    replaceImage = new JRadioButton("Replace image", false);
    replaceImage.setFont(serif12);
    replaceImage.setForeground(Color.black);
    destinationGroup.add(replaceImage);
    destinationPanel.add(replaceImage);

    // Only if the image is unlocked can it be replaced.
    if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
      replaceImage.setEnabled(true);
    }
    else {
      replaceImage.setEnabled(false);
    }

    filterPanel = new JPanel(new GridBagLayout());
    filterPanel.setBorder(buildTitledBorder("Filter specifications"));

    textLowGain = new JTextField(10);
    textLowGain.setText(String.valueOf(lowGain));
    textLowGain.setFont(serif12);
    textLowGain.setEnabled(true);

    labelLowGain = new JLabel("Low frequency gain (<1.0)");
    labelLowGain.setForeground(Color.black);
    labelLowGain.setFont(serif12);
    labelLowGain.setEnabled(true);

    textHighGain = new JTextField(10);
    textHighGain.setText(String.valueOf(highGain));
    textHighGain.setFont(serif12);
    textHighGain.setEnabled(true);

    labelHighGain = new JLabel("High frequency gain (>1.0)");
    labelHighGain.setForeground(Color.black);
    labelHighGain.setFont(serif12);
    labelHighGain.setEnabled(true);

    textF1 = new JTextField(10);
    textF1.setText(String.valueOf(freq1));
    textF1.setFont(serif12);
    textF1.setEnabled(true);

    labelF1 = new JLabel("Cutoff frequency >0.0 to 1.0 ");
    labelF1.setForeground(Color.black);
    labelF1.setFont(serif12);
    labelF1.setEnabled(true);

    textOrder = new JTextField(10);
    textOrder.setText("1");
    textOrder.setFont(serif12);
    textOrder.setForeground(Color.black);
    textOrder.setEnabled(true);

    labelOrder = new JLabel("Butterworth filter order");
    labelOrder.setForeground(Color.black);
    labelOrder.setFont(serif12);
    labelOrder.setEnabled(true);

    textLowTruncated = new JTextField(10);
    textLowTruncated.setText(String.valueOf(nf.format(100.0 * lowTruncated)));
    textLowTruncated.setFont(serif12);
    textLowTruncated.setForeground(Color.black);
    textLowTruncated.setEnabled(true);

    labelLowTruncated = new JLabel("% of low histogram end truncated");
    labelLowTruncated.setForeground(Color.black);
    labelLowTruncated.setFont(serif12);
    labelLowTruncated.setEnabled(true);

    textHighTruncated = new JTextField(10);
    textHighTruncated.setText(String.valueOf(nf.format(100.0 * highTruncated)));
    textHighTruncated.setFont(serif12);
    textHighTruncated.setForeground(Color.black);
    textHighTruncated.setEnabled(true);

    labelHighTruncated = new JLabel("% of high histogram end truncated");
    labelHighTruncated.setForeground(Color.black);
    labelHighTruncated.setFont(serif12);
    labelHighTruncated.setEnabled(true);


    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = gbc.WEST;
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.weightx = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    filterPanel.add(labelLowGain, gbc);
    gbc.gridx = 1;
    filterPanel.add(textLowGain, gbc);
    gbc.gridx = 0;
    gbc.gridy = 1;
    filterPanel.add(labelHighGain, gbc);
    gbc.gridx = 1;
    filterPanel.add(textHighGain, gbc);
    gbc.gridx = 0;
    gbc.gridy = 2;
    filterPanel.add(labelF1, gbc);
    gbc.gridx = 1;
    filterPanel.add(textF1, gbc);
    gbc.gridx = 0;
    gbc.gridy = 3;
    filterPanel.add(labelOrder, gbc);
    gbc.gridx = 1;
    filterPanel.add(textOrder, gbc);
    gbc.gridx = 0;
    gbc.gridy = 4;
    filterPanel.add(labelLowTruncated, gbc);
    gbc.gridx = 1;
    filterPanel.add(textLowTruncated, gbc);
    gbc.gridx = 0;
    gbc.gridy = 5;
    filterPanel.add(labelHighTruncated, gbc);
    gbc.gridx = 1;
    filterPanel.add(textHighTruncated, gbc);

    mainPanel = new JPanel(new GridBagLayout());
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.fill = gbc.HORIZONTAL;
    mainPanel.add(optionsPanel, gbc);
    gbc.gridy = 1;
    mainPanel.add(destinationPanel, gbc);
    gbc.gridy = 2;
    mainPanel.add(filterPanel, gbc);

    getContentPane().add(mainPanel);
    getContentPane().add(buildButtons(), BorderLayout.SOUTH);

    pack();
    setVisible(true);
  }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage getResultImage() {
    return resultImage;
  }

  /**
       *	Accessor that sets the display loc variable to replace, so the current image
   *	is replaced once the algorithm completes.
   */
  public void setDisplayLocReplace() {
    displayLoc = REPLACE;
  }

  /**
   *	Accessor that sets the display loc variable to new, so that a new image
   *	is created once the algorithm completes.
   */
  public void setDisplayLocNew() {
    displayLoc = NEW;
  }


  /**
   *	Accessor that sets the butterworth order.
   *	@param diameter	Value to set the butterworth order to.
   */
  public void setButterworthOrder(int order) {
    butterworthOrder = order;
  }



  /**
   *	Accessor that sets the frequency 1 variable.
   *	@param scale	Value to set frequency 1 to.
   */
  public void setFreq1(float scale) {
    freq1 = scale;
  }

  /**
   * Accessor that sets lowGain variable
   * @param lowGain float
   */
  public void setLowGain(float lowGain) {
    this.lowGain = lowGain;
  }

  /**
   * Accessor that sets highGain variable
   * @param highGain float
   */
  public void setHighGain(float highGain) {
    this.highGain = highGain;
  }

  /**
   * Accessor that sets lowTruncated variable
   * @param lowTruncated float
   */
  public void setLowTruncated(float lowTruncated) {
    this.lowTruncated = lowTruncated;
  }

  /**
   * Acceessor that sets highTruncated variable
   * @param highTruncated float
   */
  public void setHighTruncated(float highTruncated) {
    this.highTruncated = highTruncated;
  }

  /**
  *    Accessor that sets image25D for single slice processing
  *    @param image25D
  */
  public void setImage25D(boolean image25D) {
    this.image25D = image25D;
  }

  /**
   *  Closes dialog box when the OK button is pressed and calls the algorithm.
   *  @param event       Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    Object source = event.getSource();

    if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }

    else if (command.equals("Cancel")) {
      dispose();
    }
    else if (source == helpButton) {
      //MipavUtil.showHelp("");
    }
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {
    String tmpStr;

    if (replaceImage.isSelected())
      displayLoc = REPLACE;
    else if (newImage.isSelected())
      displayLoc = NEW;

    if (image25DCheckbox.isSelected()) {
      image25D = true;
    }
    else {
      image25D = false;
    }

    tmpStr = textLowGain.getText();
    lowGain = Float.valueOf(tmpStr).floatValue();
    if (lowGain <= 0.0) {
      MipavUtil.displayError("Low gain must be greater than 0.0");
      textLowGain.requestFocus();
      textLowGain.selectAll();
      return false;
    }
    else if (lowGain >= 1.0) {
      MipavUtil.displayError("Low gain must be less than 1.0");
      textLowGain.requestFocus();
      textLowGain.selectAll();
      return false;
    }

    tmpStr = textHighGain.getText();
    highGain = Float.valueOf(tmpStr).floatValue();
    if (highGain <= 1.0) {
      MipavUtil.displayError("High gain must be greater than 1.0");
      textHighGain.requestFocus();
      textHighGain.selectAll();
      return false;
    }

    tmpStr = textF1.getText();
    freq1 = Float.valueOf(tmpStr).floatValue();
    if (freq1 <= 0.0) {
      MipavUtil.displayError("F1 must exceed 0.0");
      textF1.requestFocus();
      textF1.selectAll();
      return false;
    }
    else if (freq1 >= 1.0) {
      MipavUtil.displayError("F1 must be less than 1.0");
      textF1.requestFocus();
      textF1.selectAll();
      return false;
    }

    tmpStr = textOrder.getText();
    butterworthOrder = Integer.parseInt(tmpStr);
    if (butterworthOrder < 1) {
      MipavUtil.displayError("Butterworth order must be at least 1");
      textOrder.requestFocus();
      textOrder.selectAll();
      return false;
    }

    tmpStr = textLowTruncated.getText();
    lowTruncated = Float.valueOf(tmpStr).floatValue()/100.0f;
    if (lowTruncated < 0.0) {
      MipavUtil.displayError("% of low end truncated cannot be negative");
      textLowTruncated.requestFocus();
      textLowTruncated.selectAll();
      return false;
    }
    else if (lowTruncated >= 1.0) {
      MipavUtil.displayError("% of low end truncated must be less than 100.0");
      textLowTruncated.requestFocus();
      textLowTruncated.selectAll();
      return false;
    }

    tmpStr = textHighTruncated.getText();
    highTruncated = Float.valueOf(tmpStr).floatValue()/100.0f;
    if (highTruncated < 0.0) {
      MipavUtil.displayError("% of high end truncated cannot be negative");
      textHighTruncated.requestFocus();
      textHighTruncated.selectAll();
      return false;
    }
    else if (highTruncated >= 1.0) {
      MipavUtil.displayError("% of high end truncated must be less than 100");
      textHighTruncated.requestFocus();
      textHighTruncated.selectAll();
      return false;
    }



    return true;
  }

  /**
   *	Once all the necessary variables are set, call the Frequency Filter
   *	algorithm based on what type of image this is and whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {
    String name = makeImageName(image.getImageName(), "_homomorphic");
    if (displayLoc == NEW) {
      try {
        resultImage = (ModelImage) image.clone();
        resultImage.setImageName(name);
        resultImage.resetVOIs();
        // Make algorithm
        FrequencyFilterAlgo = new AlgorithmFrequencyFilter(resultImage, image,
            image25D, freq1, butterworthOrder, lowGain, highGain, lowTruncated,
            highTruncated);
        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed or failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        FrequencyFilterAlgo.addListener(this);
        // Hide dialog since the algorithm is about to run
        setVisible(false);

        if (runInSeparateThread) {
          // Start the thread as a low priority because we wish to still have user interface work fast.
          if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
          }
        }
        else {
          FrequencyFilterAlgo.setActiveImage(isActiveImage);
          if (!userInterface.isAppFrameVisible()) {
          FrequencyFilterAlgo.setProgressBarVisible(false);
        }
          FrequencyFilterAlgo.run();
        }
      }
      catch (OutOfMemoryError x) {
        MipavUtil.displayError("Dialog HomomorphicFilter: unable to allocate enough memory");
        if (resultImage != null) {
          resultImage.disposeLocal(); // Clean up memory of result image
          resultImage = null;
        }
        return;
      }
    }
    else {
      try {
        // No need to make new image space because the user has choosen to replace the source image
        // Make the algorithm class
        FrequencyFilterAlgo = new AlgorithmFrequencyFilter(image, image25D,
            freq1, butterworthOrder, lowGain, lowGain, lowTruncated,
            highTruncated);

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed or failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        FrequencyFilterAlgo.addListener(this);
        // Hide the dialog since the algorithm is about to run.
        setVisible(false);

        // These next lines set the titles in all frames where the source image is displayed to
        // "locked - " image name so as to indicate that the image is now read/write locked!
        // The image frames are disabled and then unregisted from the userinterface until the
        // algorithm has completed.
        Vector imageFrames = image.getImageFrameVector();
        titles = new String[imageFrames.size()];
        for (int i = 0; i < imageFrames.size(); i++) {
          titles[i] = ( (ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
          ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " +
              titles[i]);
          ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
          userInterface.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
        }

        if (runInSeparateThread) {
          // Start the thread as a low priority because we wish to still have user interface work fast.
          if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
          }
        }
        else {
          FrequencyFilterAlgo.setActiveImage(isActiveImage);
          if (!userInterface.isAppFrameVisible()) {
        FrequencyFilterAlgo.setProgressBarVisible(false);
      }
          FrequencyFilterAlgo.run();
        }
      }
      catch (OutOfMemoryError x) {
        MipavUtil.displayError("Dialog HomomorphicFilter: unable to allocate enough memory");
        return;
      }
    }
  }

  //************************************************************************
   //************************** Algorithm Events ****************************
    //************************************************************************

     /**
      *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
      *   algorithms when it has completed or failed to to complete, so that the dialog can be display
      *   the result image and/or clean up.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {

       ViewJFrameImage imageFrame = null;
       if (algorithm instanceof AlgorithmFrequencyFilter) {
         if (algorithm.isCompleted() == true && resultImage != null) {

           updateFileTypeInfo(image, resultImage, ModelStorageBase.FLOAT);
           // resultImage is the same or smaller than image.
           //The algorithm has completed and produced a new image to be displayed.
           try {
             //resultImage.setImageName("Frequency Filtered image");
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             System.gc();
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImage == null) {
           // These next lines set the titles in all frames where the source image is displayed to
           // image name so as to indicate that the image is now unlocked!
           // The image frames are enabled and then registed to the userinterface.
           Vector imageFrames = image.getImageFrameVector();
           for (int i = 0; i < imageFrames.size(); i++) {
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
             if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
               ( (ViewJFrameBase) parentFrame).getUserInterface().registerFrame
                   ( (Frame) (imageFrames.elementAt(i)));
             }
           }
           if (parentFrame != null) {
             ( (ViewJFrameBase) parentFrame).getUserInterface().registerFrame(
                 parentFrame);
             ( (ViewJFrameImage) parentFrame).getComponentImage().
                 setLogMagDisplay(true);
           }
           image.notifyImageDisplayListeners(null, true);
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
         }

         insertScriptLine(algorithm);
       }

       FrequencyFilterAlgo.finalize();
       FrequencyFilterAlgo = null;
       dispose();
     }
}
