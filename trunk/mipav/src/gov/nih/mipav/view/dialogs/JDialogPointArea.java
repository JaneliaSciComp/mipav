package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;

import javax.swing.text.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 * Dialog that will call AlgorithmPointArea in order to calculate
 * the average intensity through a volume around an area with a given size (x by y)
 * at a given point.  The results are then graphed.
 * @author ben link
 * @version 1.0
 */
public class JDialogPointArea
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

  private AlgorithmPointArea pointAlgo;
  private ModelImage srcImage = null; // source image
  private ViewUserInterface userInterface;

  private JTextField locationField;
  private JTextField xSpaceField;
  private JTextField ySpaceField;
  private JCheckBox constrainBox;

  private JCheckBox leftPadBox;
  private JCheckBox topPadBox;

  private JCheckBox thresholdBox;
  private JTextField thresholdField;

  private boolean leftPad;
  private boolean topPad;

  private int xSpacing;
  private int ySpacing;
  private int xLoc = -1;
  private int yLoc = -1;

  private boolean showGraph = false;

  private boolean useThreshold = false;
  private float threshold = 0.0f;

  float [] averageIntensities = null;
  float [][] rgbAverageIntensities = null;


  /**
   * Constructor called from the user interface
   * @param UI ViewUserInterface
   * @param image ModelImage
   */
  public JDialogPointArea(ViewUserInterface UI, ModelImage image) {
    super(false);
    userInterface = UI;
    srcImage = image;
    parentFrame = image.getParentFrame();
  }

  /**
   * Constructor called from a ViewJFrameImage
   * @param theParentFrame Frame
   * @param image ModelImage
   * @param showGraph boolean
   */
  public JDialogPointArea(Frame theParentFrame, ModelImage image, boolean showGraph) {
    super(theParentFrame, false);
    srcImage = image;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    this.showGraph = showGraph;
    init();
  }

  /**
   * Constructor called from ViewJComponentEditImage (by clicking on a point and selecting from menu)
   * @param theParentFrame Frame
   * @param image ModelImage
   * @param xLoc int X location for point
   * @param yLoc int Y location for point
   * @param showGraph boolean whether or not to show the graph
   */
  public JDialogPointArea(Frame theParentFrame, ModelImage image, int xLoc, int yLoc, boolean showGraph) {
    super(theParentFrame, false);
    this.runInSeparateThread = false;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    //setLocation(xLoc, yLoc);
    setXLoc(xLoc);
    setYLoc(yLoc);
    this.showGraph = showGraph;
    srcImage = image;
    init();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogPointArea() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;

      try {
          srcImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(srcImageKey);

      setModal(false);
      srcImage = im;
      userInterface = srcImage.getUserInterface();
      parentFrame = srcImage.getParentFrame();

      try {
          setXLoc(parser.getNextInteger());
          setYLoc(parser.getNextInteger());
          setXSpacing(parser.getNextInteger());
          setYSpacing(parser.getNextInteger());
          setLeftPad(parser.getNextBoolean());
          setTopPad(parser.getNextBoolean());
          setUseThreshold(parser.getNextBoolean());
          setThreshold(parser.getNextFloat());
          setShowGraph(parser.getNextBoolean());
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);
      callAlgorithm();
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the image is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                  }
              }

              userInterface.getScriptDialog().append("PAverageIntensities " +
                                                     userInterface.getScriptDialog().
                                                     getVar(srcImage.getImageName()) +
                                                     " " + xLoc + " " + yLoc + " " +
                                                     xSpacing + " " + ySpacing + " " +
                                                     leftPad + " " + topPad + " " +
                                                     useThreshold + " " + threshold + " " +
                                                     showGraph);
              userInterface.getScriptDialog().append("\n");
          }
      }
  }

  /**
   *   Initializes GUI components and adds them to the dialog.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("Point area average intensities");

    JPanel optionsPanel = new JPanel();
    optionsPanel.setLayout(new GridBagLayout());

    GridBagConstraints gbc = new GridBagConstraints();
    optionsPanel.setForeground(Color.black);
    optionsPanel.setBorder(buildTitledBorder("Options"));

    JLabel locationLabel = new JLabel("Point location (x,y)");
    locationLabel.setFont(MipavUtil.font12B);
    gbc.insets = new Insets(0,5,0,0);
    gbc.anchor = gbc.WEST;
    gbc.gridwidth = 2;
    gbc.gridx = 0;
    gbc.gridy = 0;
    optionsPanel.add(locationLabel, gbc);

    locationField = new JTextField(5);
    JTextFieldFilter filter3 = new JTextFieldFilter(JTextFieldFilter.NUMERIC + ",");
    locationField.setDocument(filter3);
    if (xLoc != -1 && yLoc != -1) {
      locationField.setText(Integer.toString(xLoc) + "," + Integer.toString(yLoc));
      locationField.setEnabled(false);
    }
    gbc.insets = new Insets(0,0,0,0);
    gbc.gridwidth = 2;
    gbc.gridx = 2;
    optionsPanel.add(locationField, gbc);

    JLabel spaceLabel = new JLabel("Area around point");
    spaceLabel.setFont(MipavUtil.font12B);
    gbc.insets = new Insets(0,5,0,0);
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.gridwidth = 2;
    optionsPanel.add(spaceLabel, gbc);

    gbc.gridwidth = 1;
    gbc.insets = new Insets(0,0,0,0);
    xSpaceField = new JTextField(3);
    JTextFieldFilter filter = new JTextFieldFilter(JTextFieldFilter.NUMERIC);

    // for whatever reason, the keylistener wasn't getting the enter key event after the
    // input/actionmap sets were added to jdialogbase, but this seems to work
    xSpaceField.getInputMap( JComponent.WHEN_FOCUSED ).put( KeyStroke.getKeyStroke( "ENTER" ), "updateY" );
    xSpaceField.getActionMap().put( "updateY", new UpdateYSpaceAction() );

    xSpaceField.setDocument(filter);
    gbc.gridx = 2;
    optionsPanel.add(xSpaceField, gbc);

    JLabel crossLabel = new JLabel("x");
    crossLabel.setFont(MipavUtil.font12B);
    gbc.anchor = gbc.CENTER;
    gbc.gridx = 3;
    optionsPanel.add(crossLabel, gbc);

    ySpaceField = new JTextField(3);
    JTextFieldFilter filter2 = new JTextFieldFilter(JTextFieldFilter.NUMERIC);
    ySpaceField.setDocument(filter2);
    gbc.anchor = gbc.EAST;
    gbc.gridx = 4;
    optionsPanel.add(ySpaceField, gbc);


    constrainBox = new JCheckBox("Constrain length-width ratio", false);
    constrainBox.setFont(MipavUtil.font12B);
    constrainBox.addActionListener(this);
    constrainBox.setActionCommand("Constrain");
    gbc.anchor = gbc.WEST;
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.insets = new Insets(0,0,0,0);
    gbc.gridwidth = 2;
    optionsPanel.add(constrainBox, gbc);

    leftPadBox = new JCheckBox("Pad extra column on left", false);
    leftPadBox.setFont(MipavUtil.font12B);
    leftPadBox.setEnabled(false);
    filter.setCheckBox(leftPadBox);
    gbc.gridx = 0;
    gbc.gridy = 3;
    optionsPanel.add(leftPadBox, gbc);

    topPadBox = new JCheckBox("Pad extra row on top", false);
    topPadBox.setFont(MipavUtil.font12B);
    topPadBox.setEnabled(false);
    filter2.setCheckBox(topPadBox);
    gbc.gridx = 0;
    gbc.gridy = 4;
    optionsPanel.add(topPadBox, gbc);

    thresholdBox = new JCheckBox("Use threshold", false);
    thresholdBox.setFont(MipavUtil.font12B);
    thresholdBox.setEnabled(true);
    thresholdBox.addActionListener(this);
    gbc.gridx = 0;
    gbc.gridy = 5;
    gbc.gridwidth = 2;
    optionsPanel.add(thresholdBox, gbc);

    thresholdField = new JTextField(4);
    thresholdField.setEnabled(false);
    JTextFieldFilter filter4 = new JTextFieldFilter(JTextFieldFilter.FLOAT);
    thresholdField.setDocument(filter4);
    gbc.gridx = 2;
    gbc.gridwidth = 1;
    optionsPanel.add(thresholdField, gbc);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buttonPanel.add(OKButton);
    buildCancelButton();
    buttonPanel.add(cancelButton);

    mainDialogPanel.add(optionsPanel);
    mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

    getContentPane().add(mainDialogPanel);

    pack();
    setVisible(true);
  }

  /**
   *  actionPerformed -  Closes dialog box when the OK button is pressed and
   *                     calls the algorithm
   *  @param event       event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    Object source = event.getSource();

    if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Constrain")) {
      if (constrainBox.isSelected()) {
        ySpaceField.setEnabled(false);
        String tmpString = xSpaceField.getText();
        try {
          int xspace = Integer.parseInt(tmpString);
          ySpaceField.setText(Integer.toString(xspace));
        }
        catch (Exception ex) {
          xSpaceField.setText("");
          ySpaceField.setText("");
        }
      }
      else {
        ySpaceField.setEnabled(true);
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if (source == thresholdBox) {
      if (thresholdBox.isSelected()) {
        thresholdField.setEnabled(true);
      }
      else {
        thresholdField.setEnabled(false);
      }
    }
  }

  /**
   * Make the x and y space bounds match if the constrain checkbox is selected.
   */
  protected class UpdateYSpaceAction extends AbstractAction {
      public void actionPerformed( ActionEvent event ) {
          String tmpString = xSpaceField.getText();
          try {
              int xspace = Integer.parseInt(tmpString);
              if (constrainBox.isSelected()) {
                  ySpaceField.setText(Integer.toString(xspace));
              }
          }
          catch (Exception ex) {
              xSpaceField.setText("");
              ySpaceField.setText("");
          }
      }
  }

  /**
   * Shows a graph of the average intensities through a volume
   * (calculated from AlgorithmPointArea)
   */
  private void showIntensityGraph() {

    ViewJFrameGraph graph = null;
    if (srcImage.isColorImage()) {
      float pos[][] = new float[3][rgbAverageIntensities[0].length];
      for (int i = 0; i < 3; i++ ) {
        for (int j = 0; j < rgbAverageIntensities[0].length; j++) {
          pos[i][j] = j;
        }
      }
      graph = new ViewJFrameGraph(pos,rgbAverageIntensities, "Point Area Average Intensities");
      graph.setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(srcImage.getFileInfo(0).getUnitsOfMeasure(0)));
    }
    else {
      float [] pos = null;
      pos = new float[averageIntensities.length];
      for (int i = 0; i < pos.length; i++ ) {
        pos[i] = i;
      }
      graph = new ViewJFrameGraph(pos,averageIntensities, "Point Area Average Intensities");
      graph.setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(srcImage.getFileInfo(0).getUnitsOfMeasure(0)));
    }
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {

    //check location field
    String tmpString = locationField.getText();
    try {
      StringTokenizer tokens = new StringTokenizer(tmpString,",");
      xLoc = Integer.parseInt(tokens.nextToken().trim());
      yLoc = Integer.parseInt(tokens.nextToken().trim());
    }
    catch (Exception ex) {
      MipavUtil.displayError("Location must be in the format: x,y");
      return false;
    }

    try {
      xSpacing = Integer.parseInt(xSpaceField.getText().trim());
      if (constrainBox.isSelected()) {
        ySpacing = xSpacing;
      }
      else {
        ySpacing = Integer.parseInt(ySpaceField.getText().trim());
      }
    }
    catch (Exception ex) {
      MipavUtil.displayError("Length and width around point must be integers");
      return false;
    }

    this.leftPad = leftPadBox.isSelected();
    this.topPad = topPadBox.isSelected();

    if (thresholdBox.isSelected()) {
      useThreshold = true;
      try {
        threshold = Float.parseFloat(thresholdField.getText().trim());
      }
      catch (Exception ex) {
        MipavUtil.displayError("Please enter threshold float value");
        return false;
      }
    }

    return true;
  }

  /**
  *   Accessor to set x point location
  *   @param xLoc
  */
  public void setXLoc(int xLoc) {
      this.xLoc = xLoc;
  }

  /**
  *   Accessor to set y point location
  *   @param yLoc
  */
  public void setYLoc(int yLoc) {
      this.yLoc = yLoc;
  }


  /**
  *   Accessor to set x spacing around point
  *   @param xSpacing
  */
  public void setXSpacing(int xSpacing) {
      this.xSpacing = xSpacing;
  }

  /**
  *   Accessor to set y spacing around point
  *   @param ySpacing
  */
  public void setYSpacing(int ySpacing) {
      this.ySpacing = ySpacing;
  }

  /**
  *   Accessor to set if padded with extra column on left
  *   @param leftPad
  */
  public void setLeftPad(boolean leftPad) {
     this.leftPad = leftPad;
  }

  /**
  *   Accessor to set if padded with extra row on top
  *   @param topPad
  */
  public void setTopPad(boolean topPad) {
      this.topPad = topPad;
  }

  /**
  *   Accessor to set if threshold is used
  *   @param useThreshold
  */
  public void setUseThreshold(boolean useThreshold) {
      this.useThreshold = useThreshold;
  }

  /**
  *   Accessor to set threshold
  *   @param threshold
  */
  public void setThreshold(float threshold) {
      this.threshold = threshold;
  }

  /**
  *   Accessor to set showGraph
  *   @param showGraph
  */
  public void setShowGraph(boolean showGraph) {
      this.showGraph = showGraph;
  }

  /**
   * Accessor that returns the whether or not the algorithm completed
   * successfully
   */
  public boolean isSuccessful() {
    if (pointAlgo.isCompleted())
      return true;
    else
      return false;
  }

  /**
   *
   */
  private void callAlgorithm() {

    System.err.println("Use threshold: " + useThreshold + " threshold: " + threshold);
    pointAlgo = new AlgorithmPointArea(srcImage, xLoc, yLoc,
                                       xSpacing, ySpacing, leftPad, topPad, useThreshold, threshold);

    pointAlgo.addListener(this);
    setVisible(false); // Hide dialog

    if (runInSeparateThread) {
      // Start the thread as a low priority because we wish to still have user interface work fast.
      if (pointAlgo.startMethod(Thread.MIN_PRIORITY) == false){
        MipavUtil.displayError("A thread is already running on this object");
      }
    }
    else {
      pointAlgo.setActiveImage(isActiveImage);
      if (!userInterface.isAppFrameVisible()) {
        pointAlgo.setProgressBarVisible(false);
      }
      pointAlgo.run();
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


       if (pointAlgo.isCompleted() == true) {
           if (srcImage.isColorImage()) {
               rgbAverageIntensities = pointAlgo.getRGBAverageIntensities();
           }
           else {
               averageIntensities = pointAlgo.getAverageIntensities();
           }

           insertScriptLine(algorithm);

           pointAlgo.disposeLocal();
           pointAlgo = null;
           if (showGraph) {
               showIntensityGraph();
           }
           dispose();
       }

     } // end algorithmPerformed()



  /**
   * <p>Title: JTextFieldFilter</p>
   * <p>Description: Filter that allows only integers or floating point numbers
   * into a textfield while monitoring the numbers themselves in order to activate/deactivate
   * related JCheckboxes </p>
   * <p>Copyright: Copyright (c) 2003</p>
   * <p>Company: </p>
   * @author ben link
   * @version 1.0
   */
  public class JTextFieldFilter extends PlainDocument {
    public static final String NUMERIC =
        "0123456789";

    public static final String FLOAT =
        "012345679.";

    protected String acceptedChars = null;
    protected boolean negativeAccepted = false;

    private JCheckBox box = null;
    public JTextFieldFilter(String acceptedchars) {
      acceptedChars = acceptedchars;
    }

    public void setCheckBox(JCheckBox box) {
      this.box = box;
    }

    public void remove(int offs, int len) throws BadLocationException{
      super.remove(offs, len);
      if (box != null) {
        String fullString = super.getText(0, super.getLength());
        try {
          int num = Integer.parseInt(fullString);
          if (num % 2 == 1) {
            box.setEnabled(false);
          }
          else {
            box.setEnabled(true);
          }
        }
        catch (Exception ex) {
          box.setEnabled(false);
        }
      }


    }

    public void insertString
        (int offset, String str, AttributeSet attr) throws BadLocationException {
      if (str == null)
        return;

      for (int i = 0; i < str.length(); i++) {
        if (acceptedChars.indexOf(str.valueOf(str.charAt(i))) == -1)
          return;
      }

      super.insertString(offset, str, attr);

      if (box != null) {
        String fullString = super.getText(0,super.getLength());
        try {
          int num = Integer.parseInt(fullString);
          if (num % 2 == 1) {
            box.setEnabled(false);
          }
          else {
            box.setEnabled(true);
          }
        }
        catch (Exception ex) {
          //nada
        }
      }
    }
  }
} // end class JDialogPointArea
