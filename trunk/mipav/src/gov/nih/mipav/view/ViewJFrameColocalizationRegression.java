package gov.nih.mipav.view;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.text.*;

/**
 *   This class produces a frame surrounding a 2D histogram with a
 *   orthogonal least squares regression line and a threshold point that
 *   slides along the line.  In free range mode this VOI point may be
 *   taken off the line.  Optional region lines that go horizontally
 *   to the right and vertically to the top from the threshold point
 *   may be selected with the region button.
 *   Tool bar buttons are present for reset, magnify,
 *   unmagnify, for generating a histogram LUT dialog,
 *   for generating a dialog to set brightness and contrast,
 *   for switching between linear and log mode, for whether or not
 *   horizontal and vertical region lines are included, for taking
 *   the histogram frame into and out of free range mode, and for outputting
 *   statistics for each slice in a 3D image.
 *   Reset returns the sliding point to its initial position - the point
 *   on the line just above the point where the first negative or
 *   zero linear correlation coefficient is present.
 *   Magnify will double the present magnification and
 *   Unmagnify will half the present magnification.  Magnifications are
 *   only powers of 2.
 *	<p>
 *   The LUT dialog can be used to set colors and transfer functions of the
 *   LUT table used by the 2D histogram display.
 *  <p>
 *
 *   The dialog box for brightness and contrast has brightness and contrast sliders, an Apply button,
 *   and a Cancel button.  The brightness will add an offset ranging from -255 to 255 to every
 *   scaled red, green, and blue in the image.  Contrast will multiply every original red,
 *   green, and blue value by a floating point number ranging from 0.1 to 10.0.  Before
 *   apply is pressed, slider changes are only temporarily made to the currently displayed slice.
 *   If apply is pressed, these changes are permanently made to the histogram image.
 *   Pressing cancel keeps all the histogram image in its original state.
 *	<p>
 *   The file menu only has 1 simple function - a
 *   close ViewJFrameColocalization structure function.
 *   The help menu only has 1 simple function - an
 *   about colocalization function.
 *	<p>
 *   ViewJFrameColocalizationRegression is called in AlgorithmColocalizationRegression.
 *   ViewJFrameColocalizationRegression calls ViewJComponentColocalizationRegression
 *
 */
public class ViewJFrameColocalizationRegression
    extends ViewJFrameBase
    implements ChangeListener, ItemListener {

  private AlgorithmColocalizationRegression alg;
  private ViewUserInterface userInterface;
  private ModelLUT LUTa;

  // The LUT for the 2D histogram
  private ModelLUT LUTdest;
  private ModelImage imageA;
  private ModelImage imageB;
  // The image containing the 2D histogram
  private ModelImage destImage;
  private JPanel statusPanel;
  private boolean useRed;
  private boolean useGreen;
  private boolean useBlue;
  // The slope and offset of the total least squares line
  private float slope;
  private float offset;
  // The linear correlation coefficient for all points
  // with either buffer[i] >= threshold1 or secondBuffer[i] >= threshold2
  private float linearCorrelation;
  // statistic giving the portion of images generated with randomly
  // scrambled blocks of pixels having a linear correlation
  // coefficient less than that of the actual image.
  private float PValue;
  private boolean haveThreshold[]; // true for pixels with calculated rThreshold values;
  // The linear correlation coefficients for all pixels with values either below
  // color1 for buffer or below a*color1 + b for secondBuffer
  private float rThreshold[];
  private float colocSize[];
  private float colocIntensity1[];
  private float colocIntensity2[];
  // true for pixels with calculated freeRangeRThreshold values
  private boolean haveFreeRangeThreshold[] = null;
  // The linear correlation coefficient for all pixels with values either below
  // color1 for buffer or below color2 for secondBuffer
  private float freeRangeRThreshold[] = null;
  private float freeRangeColocSize[] = null;
  private float freeRangeColocIntensity1[] = null;
  private float freeRangeColocIntensity2[] = null;
  //Minimum and maximum buffer and secondBuffer areas
  private double min1, max1, min2, max2;
  // bin1 is generated from Math.round(scale1*(buffer[i]-min1)) and
  // bin2 is generated from Math.round(scale2*(secondBuffer[i]-min2))
  private double scale1,scale2;
  // The endpoints of the line segment in (buffer,secondBuffer) values
  private double lineMin1,lineMax1,lineMin2,lineMax2;
  // If true, the matrix of linear correlation coefficients is taken
  // along color 1 at unity spacings.  If false, along color 2.
  private boolean thresholdOn1;
  // The spaces around the histogram bin area
  private int leftPad;
  private int rightPad;
  private int bottomPad;
  private int topPad;
  private boolean doSecondIteration;
  private boolean pointCalculation;

  private JMenuBar openingMenuBar; // contains File and Help menus
  private ViewMenuBuilder menuObj;

  private JToolBar toolBar;

  private Border pressedBorder = BorderFactory.createLoweredBevelBorder();
  private Border raisedBorder = BorderFactory.createRaisedBevelBorder();
  private Border etchedBorder = BorderFactory.createEtchedBorder();
  private int xScreen, yScreen; // screen width, screen height
  private float zoom = 1; // present magnification - can only be a power of 2
  private float imageBufferA[];
  private float imageBufferB[];
  private float imageBufferDest[];
  private int pixBufferDest[];
  private int paintBufferDest[];
  private JToggleButton logDisplayButton;
  private boolean logMagDisplay = true; // tells if log magnitude displays are use
  private JToggleButton regionLinesDisplayButton;
  private JToggleButton freeRangeButton;
  private boolean freeRangeMode = false;
  private boolean regionLinesDisplay;
  private ViewJComponentColocalizationRegression componentImage;
  private JScrollPane scrollPane;
  private ViewControlsImage controls;

  private JLabel labelLinearCorrelation;

  //private JLabel labelLineFunction;
  private JLabel labelCurrentColoc;
  private JLabel labelCurrentIntensity1;
  private JLabel labelCurrentIntensity2;
  private JLabel labelPValue;
  private JLabel labelCurrent;
  private String dataLine1;
  private String dataLine2;

  private GridBagLayout cpGBL; // control panel grid bag layout
  private GridBagConstraints cpGBC; // control panel grid bag constraints
  private GridBagLayout gbl; // content pane grid bag layout
  private GridBagConstraints gbc; // content pane grid bag constraints
  private int extents[];
  private int componentY; // height of TopPanel + openingMenuBar
  private int structureY; // all totals in Y direction not due to image
  private int minimumToolBarWidth = 400; // minimum scroll pane width
  private int minimumHeight = 100; // minimum scroll pane height
  private NumberFormat nf; // number formatting used in frames per second
  // set to give 6 digits to the right of the decimal
  private ModelRGB RGBTA;
  private ViewJFrameBase controlFrame = null;
  private JPanel innerPanel = null; // componentImage placed in innerPane
  // and innerPanel placed in scrollPane
  private JPanel topPanel = null; // contains toolBar and statusPanel
  private GridBagConstraints gbcTP;
  private int scrollPaneSize = 512;
  // Note that xDim and yDim refer to destImage
  private int xDim, yDim;

  private Vector menuItemVector = new Vector();


  private int origBrightness = 0; // offset added to each scaled
  // red, green, and blue
  // origBrightness remains constant
  // until applyButton is pressed, restores brightness if cancelButton is pressed
  private float origContrast = 1.0f; // scale factor multiplying each
  // red, green, and blue
  // origContrast remains constant
  // until applyButton is pressed, restores contrast if cancelButton is pressed
  private ViewVOIVector VOIs;
  private int nVOI; // number of vois
  private VOI pointVOI;
  private int originalX, originalY;

  /**
   *   Makes a frame of the colocalization histogram
   *   @param alg             AlgorithmColocalizationRegression parent
   *   @param _imageA         Model of imageA
   *   @param _LUTa           Model of LUT for image A
   *   @param _imageB         Model of imageB
   *   @param _LUTb           Model of LUT for image B
   *   @param _RGBTA          Model RGB LUT for color image (A) else null
   *   @param destImage       The destination image
   *   @param controlFrame    ViewJFrameBase passed to ViewJComponentColocalizationRegression
   *   @param useRed
   *   @param useGreen
   *   @param useBlue
   *   @param slope
   *   @param offset
   *   @param linearCorrelation
   *   @param PValue
   *   @param haveThreshold
   *   @param rThreshold
   *   @param colocSize
   *   @param colocIntensity1
   *   @param colocIntensity2
   *   @param min1
   *   @param max1
   *   @param min2
   *   @param max2
   *   @param scale1
   *   @param scale2
   *   @param lineMin1
   *   @param lineMax1
   *   @param lineMin2
   *   @param lineMax2
   *   @param thresholdOn1
   *   @param leftPad
   *   @param rightPad
   *   @param bottomPad
   *   @param topPad
   *   @param doSecondIteration
   *   @param pointCalculation
   */
  public ViewJFrameColocalizationRegression(AlgorithmColocalizationRegression alg,
                           ModelImage _imageA,
                           ModelLUT _LUTa,
                           ModelImage _imageB,
                           ModelLUT _LUTb,
                           ModelRGB _RGBTA,
                           ModelImage destImage,
                           ViewJFrameBase controlFrame,
                           boolean useRed, boolean useGreen, boolean useBlue,
                           float slope, float offset,
                           float linearCorrelation,
                           float PValue, boolean haveThreshold[],
                           float rThreshold[], float colocSize[],
                           float colocIntensity1[], float colocIntensity2[],
                           double min1,
                           double max1, double min2, double max2,
                           double scale1, double scale2,
                           double lineMin1, double lineMax1,
                           double lineMin2, double lineMax2, boolean thresholdOn1,
                           int leftPad, int rightPad,
                           int bottomPad, int topPad, boolean doSecondIteration,
                           boolean pointCalculation) {

    super(_imageA, null);

    buildMenu();
    setJMenuBar(openingMenuBar);

    this.alg = alg;
    userInterface = ViewUserInterface.getReference();
    LUTa = _LUTa;
    imageA = _imageA;
    LUTb = _LUTb;
    imageB = _imageB;
    this.destImage = destImage;
    xDim = destImage.getExtents()[0];
    yDim = destImage.getExtents()[1];
    this.useRed = useRed;
    this.useGreen = useGreen;
    this.useBlue = useBlue;
    this.slope = slope;
    this.offset = offset;
    this.linearCorrelation = linearCorrelation;
    this.PValue = PValue;
    this.haveThreshold = haveThreshold;
    this.rThreshold = rThreshold;
    this.colocSize = colocSize;
    this.colocIntensity1 = colocIntensity1;
    this.colocIntensity2 = colocIntensity2;
    this.min1 = min1;
    this.max1 = max1;
    this.min2 = min2;
    this.max2 = max2;
    this.scale1 = scale1;
    this.scale2 = scale2;
    this.lineMin1 = lineMin1;
    this.lineMax1 = lineMax1;
    this.lineMin2 = lineMin2;
    this.lineMax2 = lineMax2;
    this.thresholdOn1 = thresholdOn1;
    this.controlFrame = controlFrame;
    this.leftPad = leftPad;
    this.rightPad = rightPad;
    this.bottomPad = bottomPad;
    this.topPad = topPad;
    setTitle(imageA.getImageName());
    this.RGBTA = _RGBTA;
    this.doSecondIteration = doSecondIteration;
    this.pointCalculation = pointCalculation;

    xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
    yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;

    setLocation(100, 100);

    toolBar = buildColocalizeToolBar(this);
    buildStatusPanel();
    topPanel = new JPanel();
    topPanel.setLayout(new GridBagLayout());

    gbcTP = new GridBagConstraints();
    gbcTP.gridx = 0;
    gbcTP.gridy = 0;
    gbcTP.gridwidth = 1;
    gbcTP.gridheight = 1;
    gbcTP.fill = GridBagConstraints.BOTH;
    gbcTP.anchor = GridBagConstraints.WEST;
    gbcTP.weightx = 100;
    gbcTP.weighty = 100;
    topPanel.add(toolBar, gbcTP);

    gbcTP.gridx = 0;
    gbcTP.gridy = 1;
    gbcTP.gridwidth = 1;
    gbcTP.gridheight = 3;
    gbcTP.fill = GridBagConstraints.BOTH;
    gbcTP.anchor = GridBagConstraints.WEST;
    gbcTP.weightx = 100;
    gbcTP.weighty = 100;
    topPanel.add(statusPanel, gbcTP);
    getContentPane().add(topPanel, "North");
    buildScrollPane(userInterface);

    componentImage.setPosition(originalX,originalY);

    /* componentY is added so that the previous software for ViewJFrameImage can be
       reused.  There the image was resized without a toolbar, statusPanel, or
       menubar contributing to the vertical length. */
    pack();
    componentY = topPanel.getHeight() + openingMenuBar.getHeight();
    // structureY is the total of all nonimage components in the Y direction
    structureY = getInsets().top + componentY + getInsets().bottom;
    setSize( (int) Math.round(scrollPaneSize + 3 + getInsets().left + getInsets().right),
             (int) Math.round(scrollPaneSize + 3 + structureY));

    addComponentListener(this);

    componentResized(null);
    // The magnification is set to the highest power of 2.0 for which scroll bars are not needed.
    zoom = (float) (scrollPane.getViewportBorderBounds().width) /
                          destImage.getExtents()[0];
    zoom = Math.min(zoom, (float) (scrollPane.getViewportBorderBounds().height) /
                          destImage.getExtents()[1]);
    for (int i = -10; i <= 10; i++) {
      if (zoom >= Math.pow(2.0, (double) i) &&
          zoom <  Math.pow(2.0, (double) (i + 1))) {
          zoom = (float) Math.pow(2.0, (double) i);
      }
    }
    componentImage.setZoom(zoom, zoom);
    setTitle();
    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    destImage.addImageDisplayListener(this);
    imageA.addImageDisplayListener(this);
    userInterface.regFrame(this);
    setResizable(true);
    setVisible(true);
    componentImage.buildImageDestObject(null, true);
    componentImage.update();
  }

  public void pointCalculate() {
      freeRangeButton.setSelected(true);
      freeRangeMode = true;
      componentImage.setFreeRangeMode(freeRangeMode);
      if (freeRangeMode && haveFreeRangeThreshold == null) {
        alg.createFreeRangeArrays();
      }
      componentImage.setPosition(originalX, originalY);
  }

  /**
   *   Modifies a frame of the colocalization histogram after mouseRelased
   *   has indicated the contour VOI in the source image has been moved
   *   @param slope
   *   @param offset
   *   @param linearCorrelation
   *   @param PValue
   *   @param haveThreshold[]
   *   @param rThreshold
   *   @param colocSize
   *   @param colocIntensity1
   *   @param colocIntensity2
   *   @param min1
   *   @param max1
   *   @param min2
   *   @param max2
   *   @param scale1
   *   @param scale2
   *   @param lineMin1
   *   @param lineMax1
   *   @param lineMin2
   *   @param lineMax2
   *   @param thresholdOn1
   */
  public void setNewVar(float slope, float offset,
                           float linearCorrelation,
                           float PValue, boolean haveThreshold[],
                           float rThreshold[], float colocSize[],
                           float colocIntensity1[], float colocIntensity2[],
                           double min1,
                           double max1, double min2, double max2,
                           double scale1, double scale2,
                           double lineMin1, double lineMax1,
                           double lineMin2, double lineMax2, boolean thresholdOn1) {
        this.slope = slope;
        this.offset = offset;
        this.linearCorrelation = linearCorrelation;
        this.PValue = PValue;
        this.haveThreshold = haveThreshold;
        this.rThreshold = rThreshold;
        this.colocSize = colocSize;
        this.colocIntensity1 = colocIntensity1;
        this.colocIntensity2 = colocIntensity2;
        this.min1 = min1;
        this.max1 = max1;
        this.min2 = min2;
        this.max2 = max2;
        this.scale1 = scale1;
        this.scale2 = scale2;
        this.lineMin1 = lineMin1;
        this.lineMax1 = lineMax1;
        this.lineMin2 = lineMin2;
        this.lineMax2 = lineMax2;
        this.thresholdOn1 = thresholdOn1;
        modifyStatusPanel();
        componentImage.setNewVar(slope, offset, haveThreshold, rThreshold,
                                                 colocSize,colocIntensity1,
                                                 colocIntensity2,
                                                 min1, max1, min2, max2,
                                                 scale1,scale2,
                                                 lineMin1, lineMax1,
                                                 lineMin2, lineMax2, thresholdOn1,
                                                 linearCorrelation);
  }

  /**
  *    Passes arrays needed in free range mode
  *    @param haveFreeRangeThreshold
  *    @param freeRangeRThreshold
  *    @param freeRangeColocSize
  *    @param freeRangeColocIntensity1
  *    @paran freeRangeColocIntensity2
  */
  public void passFreeRangeArrays(boolean haveFreeRangeThreshold[],
                                  float freeRangeRThreshold[],
                                  float freeRangeColocSize[],
                                  float freeRangeColocIntensity1[],
                                  float freeRangeColocIntensity2[]) {

      this.haveFreeRangeThreshold = haveFreeRangeThreshold;
      this.freeRangeRThreshold = freeRangeRThreshold;
      this.freeRangeColocSize = freeRangeColocSize;
      this.freeRangeColocIntensity1 = freeRangeColocIntensity1;
      this.freeRangeColocIntensity2 = freeRangeColocIntensity2;
      componentImage.passFreeRangeArrays(haveFreeRangeThreshold,
                                         freeRangeRThreshold,
                                         freeRangeColocSize,
                                         freeRangeColocIntensity1,
                                         freeRangeColocIntensity2);
  }


  /**
   *   Make a scroll frame and puts an image component into it.
   *   @param ui              main user interface frame
   */
  private void buildScrollPane(ViewUserInterface ui) {

    try {
      innerPanel = new JPanel();
      innerPanel.setLayout(new GridBagLayout());
      innerPanel.setBackground(Color.black);

      scrollPaneSize = yScreen - 550;
      if (scrollPaneSize < 400) {
        scrollPaneSize = 400;
      }

      // if not a color image and LUTa is null then make a LUT
      if (imageA.isColorImage() == false) {
        if (LUTa == null) {
          int dimExtentsLUT[] = new int[2];
          dimExtentsLUT[0] = 4;
          dimExtentsLUT[1] = 256;

          LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
          float min, max;
          if (imageA.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
          }
          else if (imageA.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
          }
          else {
            min = (float) imageA.getMin();
            max = (float) imageA.getMax();
          }
          float imgMin = (float) imageA.getMin();
          float imgMax = (float) imageA.getMax();
          LUTa.resetTransferLine(min, imgMin, max, imgMax);
        }
      }

      if (LUTdest == null) {
          int dimExtentsLUT[] = new int[2];
          dimExtentsLUT[0] = 4;
          dimExtentsLUT[1] = 256;

          LUTdest = new ModelLUT(ModelLUT.COOLHOT, 256, dimExtentsLUT);
          float min, max;

          if (logMagDisplay) {
              min = (float) (0.4342944819*Math.log(1.0 + destImage.getMin()));
              max = (float) (0.4342944819*Math.log(1.0 + destImage.getMax()));
          }
          else {
              min = (float)destImage.getMin();
              max = (float)destImage.getMax();
          }

          float imgMin = min;
          float imgMax = max;
          LUTdest.resetTransferLine(min, imgMin, max, imgMax);
      }

      imageA.setImageOrder(ModelImage.IMAGE_A);
      if (imageA.getNDims() == 3) {
          extents = new int[3];
          extents[0] = Math.round(imageA.getExtents()[0]);
          extents[1] = Math.round(imageA.getExtents()[1]);
          extents[2] = Math.round(imageA.getExtents()[2]);
      }
      else {
          extents = new int[2];
          extents[0] = Math.round(imageA.getExtents()[0]);
          extents[1] = Math.round(imageA.getExtents()[1]);
      }

      int bufferFactor = 1;
      if (imageA.isColorImage()) {
        bufferFactor = 4;
      }

      imageBufferA = new float[bufferFactor * imageA.getSliceSize()];
      if (imageB != null) {
        imageBufferB = new float[bufferFactor * imageA.getSliceSize()];
      }
      imageBufferDest = new float[destImage.getSliceSize()];
      pixBufferDest   = new int[destImage.getSliceSize()];
      paintBufferDest = new int[destImage.getSliceSize()];

      if (imageB != null) {
        imageBufferB = new float[bufferFactor * imageA.getSliceSize()];
      }

      componentImage = new ViewJComponentColocalizationRegression(alg,
                                                 controlFrame,
                                                 this, imageA,
                                                 imageB,
                                                 destImage, LUTdest,
                                                 imageBufferDest,
                                                 useRed, useGreen, useBlue,
                                                 slope, offset,
                                                 haveThreshold, rThreshold,
                                                 colocSize,colocIntensity1,
                                                 colocIntensity2,
                                                 min1, max1, min2, max2,
                                                 scale1,scale2,
                                                 lineMin1, lineMax1,
                                                 lineMin2, lineMax2, thresholdOn1,
                                                 pixBufferDest, paintBufferDest,
                                                 zoom, extents,
                                                 logMagDisplay,
                                                 regionLinesDisplay,
                                                 leftPad, rightPad,
                                                 bottomPad, topPad,
                                                 doSecondIteration, linearCorrelation,
                                                 userInterface);


      componentImage.setRGBTA(RGBTA);

      GridBagConstraints gbcIP = new GridBagConstraints();
      gbcIP.gridx = 0;
      gbcIP.gridy = 0;
      gbcIP.gridwidth = 1;
      gbcIP.gridheight = 1;
      gbcIP.fill = GridBagConstraints.NONE;
      gbcIP.anchor = GridBagConstraints.CENTER;
      gbcIP.weightx = 0;
      gbcIP.weighty = 0;
      innerPanel.add(componentImage, gbcIP);
    }
    catch (OutOfMemoryError e) {
      throw (e);
    }

    //The component image will be displayed in a scrollpane.
    scrollPane = new JScrollPane(innerPanel,
                                 JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    scrollPane.setBounds(0, 0, scrollPaneSize + 3, scrollPaneSize + 3);
    getContentPane().add(scrollPane);
    scrollPane.setBackground(Color.black);
    scrollPane.setVisible(true);
    scrollPane.validate();

    setBackground(Color.black);

  }

  /**
   *  Resizes frame and all components
   *  @param event       event that triggered function
   */
  public synchronized void componentResized(ComponentEvent event) {
    int width, height;
    int fullWidth, fullHeight;
    if (getSize().width >= xScreen - 20 || getSize().height >= yScreen - 20) {
      return;
    }

    removeComponentListener(this);

    width = (int) Math.round(Math.max(getSize().width - 2 * getInsets().left,
                                      minimumToolBarWidth));
    height = (int) Math.round(Math.max(getSize().height - getInsets().top -
                                       componentY - getInsets().bottom,
                                       minimumHeight));

    scrollPane.setSize(width, height);
    scrollPane.setPreferredSize(new Dimension(width, height));
    fullWidth = Math.max(scrollPane.getSize().width + getInsets().left +
                         getInsets().right,
                         minimumToolBarWidth + getInsets().left +
                         getInsets().right);
    fullHeight =  Math.max(getInsets().top + componentY +
                           scrollPane.getSize().height +
                           getInsets().bottom,
                           minimumHeight);

    setSize(fullWidth,fullHeight);
    validate();
    setTitle();
    addComponentListener(this);
    updateImages(true);
  }

  /**
   *  Does nothing.
   */
  public void updateFrame(float sX, float sY) {
  }

  /**
   *   Set the title of the frame with the image name and magnification
   */
  public void setTitle() {
    String str;
    str = imageA.getImageName() + "  " + " M:" +
        makeString(componentImage.getZoomX(), 2);
    setTitle(str);
    userInterface.setTitle(str);
  }

  /**
   *   This method builds a menu which contains the option
   *   Close ColocalizationRegression.
   */
  private void buildMenu() {
    //JSeparator separator;
    try {
      //separator = new JSeparator();
      menuObj = new ViewMenuBuilder(this);
      openingMenuBar = new JMenuBar();
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("Out of memory: ViewJFrameColocalizationRegression.buildMenu");
      return;
    }

    openingMenuBar.add(
        menuObj.makeMenu("File", false,
                         new JComponent[] {
                         menuObj.buildMenuItem("Close", "CloseColocalize", 0, null, false)}));
    openingMenuBar.add(
        menuObj.makeMenu("Help", false,
                         new JComponent[] {
                         menuObj.buildMenuItem("About colocalization", "AboutColocalize", 0, null, false)}));

  }

  /**
   *   Method to build the toolbar for the Animate frame
   *   @param al         Action listener (this frame)
   *   @return           The animation toolbar
   */
  private JToolBar buildColocalizeToolBar(ActionListener al) {
    JToolBar colocalizeToolBar = new JToolBar();
    colocalizeToolBar.setBorder(etchedBorder);
    colocalizeToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

    JButton resetButton = new JButton(" Reset ");
    resetButton.addActionListener(al);
    resetButton.setToolTipText("Reset to threshold");
    resetButton.setFont(MipavUtil.font12B);
    resetButton.setActionCommand("Reset");
    resetButton.setFocusPainted(false);
    colocalizeToolBar.add(resetButton);

    colocalizeToolBar.add(makeSeparator());

    JButton zoomInButton = new JButton(MipavUtil.getIcon("zoomin.gif"));
    zoomInButton.addActionListener(al);
    zoomInButton.setToolTipText("Magnify image 2.0x");
    zoomInButton.setActionCommand("MagColocalize");
    zoomInButton.setBorderPainted(false);
    zoomInButton.setRolloverIcon(MipavUtil.getIcon("zoominroll.gif"));
    zoomInButton.setFocusPainted(false);
    colocalizeToolBar.add(zoomInButton);

    JButton zoomOutButton = new JButton(MipavUtil.getIcon("zoomout.gif"));
    zoomOutButton.addActionListener(al);
    zoomOutButton.setToolTipText("Magnify image 0.5x");
    zoomOutButton.setActionCommand("UnMagColocalize");
    zoomOutButton.setBorderPainted(false);
    zoomOutButton.setRolloverIcon(MipavUtil.getIcon("zoomoutroll.gif"));
    zoomOutButton.setFocusPainted(false);
    colocalizeToolBar.add(zoomOutButton);

    colocalizeToolBar.add(makeSeparator());

    JButton histoLUTButton = new JButton(MipavUtil.getIcon("histolut.gif"));
    histoLUTButton.addActionListener(al);
    histoLUTButton.setToolTipText("Displays Lookup Table(LUT)");
    histoLUTButton.setActionCommand("DisplayLUT");
    histoLUTButton.setBorderPainted(false);
    histoLUTButton.setRolloverIcon(MipavUtil.getIcon("histolutroll.gif"));
    histoLUTButton.setFocusPainted(false);
    colocalizeToolBar.add(histoLUTButton);

    JButton brightnessButton = new JButton(" Brightness ");
    brightnessButton.addActionListener(al);
    brightnessButton.setToolTipText("Set brightness/contrast");
    brightnessButton.setActionCommand("Brightness");
    brightnessButton.setFont(MipavUtil.font12B);
    brightnessButton.setFocusPainted(false);
    colocalizeToolBar.add(brightnessButton);

    colocalizeToolBar.add(makeSeparator());
    logDisplayButton = new JToggleButton(" Log ", true);
    logDisplayButton.addActionListener(al);
    logDisplayButton.setToolTipText("log of histogram count");
    logDisplayButton.setFont(MipavUtil.font12B);
    logDisplayButton.setActionCommand("log");
    logDisplayButton.setBorderPainted(true);
    logDisplayButton.setBorder(pressedBorder);
    logDisplayButton.setFocusPainted(false);
    logDisplayButton.addItemListener(this); //
    colocalizeToolBar.add(logDisplayButton);
    logMagDisplay = true;

    colocalizeToolBar.add(makeSeparator());
    regionLinesDisplayButton = new JToggleButton(" Region ", true);
    regionLinesDisplayButton.addActionListener(al);
    regionLinesDisplayButton.setToolTipText("region lines");
    regionLinesDisplayButton.setFont(MipavUtil.font12B);
    regionLinesDisplayButton.setActionCommand("region");
    regionLinesDisplayButton.setBorderPainted(true);
    regionLinesDisplayButton.setBorder(pressedBorder);
    regionLinesDisplayButton.setFocusPainted(false);
    regionLinesDisplayButton.addItemListener(this); //
    colocalizeToolBar.add(regionLinesDisplayButton);
    regionLinesDisplay = true;

    colocalizeToolBar.add(makeSeparator());
    freeRangeButton = new JToggleButton(" Free range ", false);
    freeRangeButton.addActionListener(al);
    freeRangeButton.setToolTipText("free range");
    freeRangeButton.setFont(MipavUtil.font12B);
    freeRangeButton.setActionCommand("freeRange");
    freeRangeButton.setBorderPainted(true);
    freeRangeButton.setBorder(raisedBorder);
    freeRangeButton.setFocusPainted(false);
    freeRangeButton.addItemListener(this); //
    colocalizeToolBar.add(freeRangeButton);
    freeRangeMode = false;

    if (imageA.getNDims() > 2) {
        colocalizeToolBar.add(makeSeparator());
        JButton allSliceButton = new JButton(" Slice calc ");
        allSliceButton.addActionListener(al);
        allSliceButton.setToolTipText("Output slice statistics");
        allSliceButton.setFont(MipavUtil.font12B);
        allSliceButton.setActionCommand("allSlice");
        allSliceButton.setFocusPainted(false);
        colocalizeToolBar.add(allSliceButton);
    } // if (imageA.getNDims() > 2)

    colocalizeToolBar.add(makeSeparator());
    colocalizeToolBar.setFloatable(false);
    return colocalizeToolBar;
  }

  /**
   *   Makes a separator for the use in the toolbars
   *   @return Separator button.
   */
  private JButton makeSeparator() {
    JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
    separator.setBorderPainted(false);
    separator.setFocusPainted(false);
    return (separator);
  }

  //*******************************************************************
   //************************* Item Events ****************************
    //*******************************************************************

     /**
      *   Sets border painted or not painted depending on if the
      *   button was selected or deselected.  Changes the currently
      *   selected script.
      *   @param event    Event that triggered this function.
      */
     public void itemStateChanged(ItemEvent event) {
       Object source = event.getSource();
       int state = event.getStateChange();

       if (source instanceof AbstractButton) {
         if ( ((AbstractButton) source).getBorder().equals(raisedBorder) ) {
              ((AbstractButton) source).setBorder(pressedBorder);
         }
         else {
            ( (AbstractButton) source).setBorder(raisedBorder);
         }
       }
     }


  /**
   *   Panel that builds the status display panel
   */
  private void buildStatusPanel() {
    int bin2;
    float invertedy;
    int index1 =0;
    int index2 = 0;
    float index1f = 0.0f;
    float index2f = 0.0f;
    int ip;
    float colocAreaPercent;
    String currentColoc;
    float colocIntensityPercent1;
    String currentIntensity1;
    float colocIntensityPercent2;
    String currentIntensity2;
    float threshold;
    String currentThreshold;
    cpGBL = new GridBagLayout();
    cpGBC = new GridBagConstraints();
    cpGBC.fill = GridBagConstraints.BOTH;
    cpGBC.weightx = 100;
    cpGBC.weighty = 100;
    statusPanel = new JPanel();
    statusPanel.setBounds(10, 100, 500, 120);
    statusPanel.setBorder(new EtchedBorder());
    statusPanel.setLayout(cpGBL);

    VOIs = destImage.getVOIs();
    if (VOIs != null) {
        nVOI = VOIs.size();
    }
    else {
        nVOI = 0;
    }
    for (int i = 0; i < nVOI; i++) {
        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
             pointVOI = VOIs.VOIAt(i);
        }
    }

    if (pointVOI == null) {
        MipavUtil.displayError("Expected point VOI not found");
        return;
    }

    Point3Df pt = pointVOI.exportPoint();
    originalX = Math.round(pt.x);
    originalY = Math.round(pt.y);

    if (thresholdOn1) {
        index1 = (int)Math.round(((pt.x-leftPad)/scale1) + min1);
        index2f = index1*slope + offset;
        ip = index1 - (int)Math.ceil(lineMin1);
        threshold = rThreshold[ip];
        colocAreaPercent = colocSize[ip];
        colocIntensityPercent1 = colocIntensity1[ip];
        colocIntensityPercent2 = colocIntensity2[ip];
    }
    else {
        bin2 = destImage.getExtents()[1] - bottomPad - topPad;
        invertedy = 2*topPad + bin2 - 1 - pt.y;
        index2 = (int)Math.round(((invertedy-topPad)/scale2) + min2);
        index1f = (index2 - offset)/slope;
        ip = index2 - (int)Math.ceil(lineMin2);
        threshold = rThreshold[ip];
        colocAreaPercent = colocSize[ip];
        colocIntensityPercent1 = colocIntensity1[ip];
        colocIntensityPercent2 = colocIntensity2[ip];
    }

    if (imageB == null) {
        userInterface.setDataText(imageA.getImageName() + "\n");
        userInterface.setGlobalDataText(imageA.getImageName() + "\n");
    }
    else {
        userInterface.setDataText(imageA.getImageName() + " and " +
                                  imageB.getImageName() + "\n");
        userInterface.setGlobalDataText(imageA.getImageName() + " and " +
                                  imageB.getImageName() + "\n");
    }
    if (imageA.getNDims() > 2) {
        currentColoc = "     % colocalization volume = " + colocAreaPercent;
        dataLine1 = "%coloc vol\t";
    }
    else {
        currentColoc = "     % colocalization area = " + colocAreaPercent;
        dataLine1 = "%coloc area\t";
    }
    dataLine2 = colocAreaPercent + "\t";
    labelCurrentColoc = new JLabel(currentColoc);
    labelCurrentColoc.setForeground(Color.black);
    labelCurrentColoc.setFont(MipavUtil.font12);
    labelCurrentColoc.setEnabled(true);
    addStatusPanel(labelCurrentColoc, cpGBC, 0, 0, 3, 1);

    if (useRed && useGreen) {
        currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else if (useRed && useBlue) {
        currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else if (useGreen && useBlue) {
        currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else {
        currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = "
                            + colocIntensityPercent1;
        currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = "
                            + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }

    labelCurrentIntensity1 = new JLabel(currentIntensity1);
    labelCurrentIntensity1.setForeground(Color.black);
    labelCurrentIntensity1.setFont(MipavUtil.font12);
    labelCurrentIntensity1.setEnabled(true);
    addStatusPanel(labelCurrentIntensity1, cpGBC, 0, 1, 3, 1);

    labelCurrentIntensity2 = new JLabel(currentIntensity2);
    labelCurrentIntensity2.setForeground(Color.black);
    labelCurrentIntensity2.setFont(MipavUtil.font12);
    labelCurrentIntensity2.setEnabled(true);
    addStatusPanel(labelCurrentIntensity2, cpGBC, 0, 2, 3, 1);

    /*if ((useRed) && (useGreen)) {
        if (offset >= 0) {
            labelLineFunction = new JLabel("Green = " + slope + "*red + " + offset);
        }
        else {
            labelLineFunction = new JLabel("Green = " + slope + "*red - " +
                                Math.abs(offset));
        }
    }
    else if ((useRed) && (useBlue)) {
        if (offset >= 0) {
            labelLineFunction = new JLabel("Blue = " + slope + "*red + " + offset);
        }
        else {
            labelLineFunction = new JLabel("Blue = " + slope + "*red - " +
                                Math.abs(offset));
        }
    }
    else if ((useGreen) && (useBlue)) {
        if (offset >= 0) {
            labelLineFunction = new JLabel("Blue = " + slope + "*green + "
            + offset);
        }
        else {
            labelLineFunction = new JLabel("Blue = " + slope + "*green - " +
            Math.abs(offset));
        }
    }
    else {
        if (offset >= 0) {
            labelLineFunction = new JLabel(imageB.getImageName() + " = " + slope +
                                    "*" + imageA.getImageName() + " + " + offset);
        }
        else {
            labelLineFunction = new JLabel(imageB.getImageName() + " = " + slope +
                                    "*" + imageA.getImageName() + " - " +
                                    Math.abs(offset));
        }
    }
    labelLineFunction.setForeground(Color.black);
    labelLineFunction.setFont(MipavUtil.font12);
    labelLineFunction.setEnabled(true);
    addStatusPanel(labelLineFunction, cpGBC, 0, 0, 3, 1);
    */

    if (PValue >= 0.0f) {
        labelPValue = new JLabel("     P-value = " + PValue);
        labelPValue.setForeground(Color.black);
        labelPValue.setFont(MipavUtil.font12);
        labelPValue.setEnabled(true);
        addStatusPanel(labelPValue, cpGBC, 0 , 3, 3, 1);
    }

    if (doSecondIteration) {
        labelLinearCorrelation = new JLabel
        ("     Linear correlation coefficient for all pixels not in subthreshold region = " +
                            linearCorrelation);
        dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\t";
        dataLine2 = dataLine2 + linearCorrelation + "\n";
    }
    else {
         labelLinearCorrelation = new JLabel
         ("     Linear correlation coefficient for all pixels above background = " +
                            linearCorrelation);
         dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\t";
         dataLine2 = dataLine2 + linearCorrelation + "\n";
    }
    labelLinearCorrelation.setForeground(Color.black);
    labelLinearCorrelation.setFont(MipavUtil.font12);
    labelLinearCorrelation.setEnabled(true);
    if (PValue >= 0.0) {
        addStatusPanel(labelLinearCorrelation, cpGBC, 0, 4, 3, 1);
    }
    else {
        addStatusPanel(labelLinearCorrelation, cpGBC, 0, 3, 3, 1);
    }

    if (Float.isNaN(threshold)) {
        currentThreshold = "     Linear correlation coefficient is undefined";
        dataLine1 += "Linear correlation coefficient is undefined";
    }
    else {
        currentThreshold = "     Linear correlation coefficient = " + threshold;
        dataLine1 += "Linear correlation coefficient = " + threshold;
    }
    if (thresholdOn1) {
        if ((useRed) && (useGreen)) {
            currentThreshold += " for pixels with red < " + index1 +
                                " or green < " + index2f;
            dataLine1 += " for pixels with red < " + index1 +
                                " or green < " + index2f + "\n";
        }
        else if ((useRed) && (useBlue)) {
            currentThreshold += " for pixels with red < " + index1 +
                                " or blue < " + index2f;
            dataLine1 += " for pixels with red < " + index1 +
                                " or blue < " + index2f + "\n";
        }
        else if ((useGreen) && (useBlue)) {
            currentThreshold += " for pixels with green < " + index1 +
                                " or blue < " + index2f;
            dataLine1 += " for pixels with green < " + index1 +
                                " or blue < " + index2f + "\n";
        }
        else {
            currentThreshold += " for pixels with " +
                                 imageA.getImageName() + " < " + index1 +
                                " or " + imageB.getImageName() + " < " + index2f;
            dataLine1 += " for pixels with " +
                                 imageA.getImageName() + " < " + index1 +
                                " or " + imageB.getImageName() + " < " + index2f + "\n";
        }
    }
    else {
        if ((useRed) && (useGreen)) {
            currentThreshold += " for pixels with red < " + index1f +
                                " or green < " + index2;
            dataLine1 += " for pixels with red < " + index1f +
                                " or green < " + index2 + "\n";
        }
        else if ((useRed) && (useBlue)) {
            currentThreshold += " for pixels with red < " + index1f +
                                " or blue < " + index2;
            dataLine1 += " for pixels with red < " + index1f +
                                " or blue < " + index2 + "\n";
        }
        else if ((useGreen) && (useBlue)) {
            currentThreshold += " for pixels with green < " + index1f +
                                " or blue < " + index2;
            dataLine1 += " for pixels with green < " + index1f +
                                " or blue < " + index2 + "\n";
        }
        else {
            currentThreshold += " for pixels with " +
                                 imageA.getImageName() + " < " + index1f +
                                " or " + imageB.getImageName() + " < " + index2;
            dataLine1 += " for pixels with " +
                                 imageA.getImageName() + " < " + index1f +
                                " or " + imageB.getImageName() + " < " + index2 + "\n";
        }
    }

    labelCurrent = new JLabel(currentThreshold);
    labelCurrent.setForeground(Color.black);
    labelCurrent.setFont(MipavUtil.font12);
    labelCurrent.setEnabled(true);
    if (PValue >= 0.0f) {
        addStatusPanel(labelCurrent, cpGBC, 0, 5, 3, 1);
    }
    else {
        addStatusPanel(labelCurrent, cpGBC, 0, 4, 3, 1);
    }

    if (PValue >= 0.0) {
        userInterface.setDataText("P-value\n");
        userInterface.setDataText(PValue + "\n");
        userInterface.setGlobalDataText("P-value\n");
        userInterface.setGlobalDataText(PValue + "\n");
    }
    if (!pointCalculation) {
        userInterface.setDataText(dataLine1);
        userInterface.setDataText(dataLine2);
        userInterface.setDataText("\n");
        userInterface.setGlobalDataText(dataLine1);
        userInterface.setGlobalDataText(dataLine2);
        userInterface.setGlobalDataText("\n");
    } // if (!pointCalculation)

    nf = NumberFormat.getNumberInstance();
    nf.setMaximumFractionDigits(6);
  }

  /**
   *   Panel that builds the status display panel
   */
  private void modifyStatusPanel() {
    int bin2;
    float invertedy;
    int index1 =0;
    int index2 = 0;
    float index1f = 0.0f;
    float index2f = 0.0f;
    int ip;
    float colocAreaPercent;
    String currentColoc;
    float colocIntensityPercent1;
    String currentIntensity1;
    float colocIntensityPercent2;
    String currentIntensity2;
    float threshold;
    String currentThreshold;


    VOIs = destImage.getVOIs();
    if (VOIs != null) {
        nVOI = VOIs.size();
    }
    else {
        nVOI = 0;
    }
    for (int i = 0; i < nVOI; i++) {
        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
             pointVOI = VOIs.VOIAt(i);
        }
    }

    if (pointVOI == null) {
        MipavUtil.displayError("Expected point VOI not found");
        return;
    }

    Point3Df pt = pointVOI.exportPoint();
    originalX = Math.round(pt.x);
    originalY = Math.round(pt.y);
    if (thresholdOn1) {
        index1 = (int)Math.round(((pt.x-leftPad)/scale1) + min1);
        index2f = index1*slope + offset;
        ip = index1 - (int)Math.ceil(lineMin1);
        threshold = rThreshold[ip];
        colocAreaPercent = colocSize[ip];
        colocIntensityPercent1 = colocIntensity1[ip];
        colocIntensityPercent2 = colocIntensity2[ip];
    }
    else {
        bin2 = destImage.getExtents()[1] - bottomPad - topPad;
        invertedy = 2*topPad + bin2 - 1 - pt.y;
        index2 = (int)Math.round(((invertedy-topPad)/scale2) + min2);
        index1f = (index2 - offset)/slope;
        ip = index2 - (int)Math.ceil(lineMin2);
        threshold = rThreshold[ip];
        colocAreaPercent = colocSize[ip];
        colocIntensityPercent1 = colocIntensity1[ip];
        colocIntensityPercent2 = colocIntensity2[ip];
    }

    if (imageB == null) {
        userInterface.setDataText(imageA.getImageName() + "\n");
        userInterface.setGlobalDataText(imageA.getImageName() + "\n");
    }
    else {
        userInterface.setDataText(imageA.getImageName() + " and " +
                                  imageB.getImageName() + "\n");
        userInterface.setGlobalDataText(imageA.getImageName() + " and " +
                                  imageB.getImageName() + "\n");
    }
    if (imageA.getNDims() > 2) {
        currentColoc = "     % colocalization volume = " + colocAreaPercent;
        dataLine1 = "%coloc vol\t";
    }
    else {
        currentColoc = "     % colocalization area = " + colocAreaPercent;
        dataLine1 = "%coloc area\t";
    }
    dataLine2 = colocAreaPercent + "\t";
    labelCurrentColoc.setText(currentColoc);

    if (useRed && useGreen) {
        currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % green colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% red coloc\t% green coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else if (useRed && useBlue) {
        currentIntensity1 = "     % red colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% red coloc\t% blue coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else if (useGreen && useBlue) {
        currentIntensity1 = "     % green colocalization = " + colocIntensityPercent1;
        currentIntensity2 = "     % blue colocalization = " + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% green coloc\t% blue coloc\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }
    else {
        currentIntensity1 = "     % " + imageA.getImageName() + " colocalization = "
                            + colocIntensityPercent1;
        currentIntensity2 = "     % " + imageB.getImageName() + " colocalization = "
                            + colocIntensityPercent2;
        dataLine1 = dataLine1 + "% coloc1\t% coloc2\t";
        dataLine2 = dataLine2 + colocIntensityPercent1 + "\t"
                    + colocIntensityPercent2 + "\t";
    }

    labelCurrentIntensity1.setText(currentIntensity1);
    labelCurrentIntensity2.setText(currentIntensity2);

    if (PValue >= 0.0f) {
        labelPValue.setText("     P-value = " + PValue);
    }

    if (doSecondIteration) {
        labelLinearCorrelation.setText
        ("     Linear correlation coefficient for all pixels not in subthreshold region = " +
                            linearCorrelation);
        dataLine1 = dataLine1 + "Linear correlation coefficient - 2 iters\t";
        dataLine2 = dataLine2 + linearCorrelation + "\n";
    }
    else {
         labelLinearCorrelation.setText
         ("     Linear correlation coefficient for all pixels above background = " +
                            linearCorrelation);
         dataLine1 = dataLine1 + "Linear correlation coefficient - 1 iter\t";
         dataLine2 = dataLine2 + linearCorrelation + "\n";
    }


    if (Float.isNaN(threshold)) {
        currentThreshold = "     Linear correlation coefficient is undefined";
        dataLine1 += "Linear correlation coefficient is undefined";
    }
    else {
        currentThreshold = "     Linear correlation coefficient = " + threshold;
        dataLine1 += "Linear correlation coefficient = " + threshold;
    }
    if (thresholdOn1) {
        if ((useRed) && (useGreen)) {
            currentThreshold += " for pixels with red < " + index1 +
                                " or green < " + index2f;
            dataLine1 += " for pixels with red < " + index1 +
                                " or green < " + index2f + "\n";
        }
        else if ((useRed) && (useBlue)) {
            currentThreshold += " for pixels with red < " + index1 +
                                " or blue < " + index2f;
            dataLine1 += " for pixels with red < " + index1 +
                                " or blue < " + index2f + "\n";
        }
        else if ((useGreen) && (useBlue)) {
            currentThreshold += " for pixels with green < " + index1 +
                                " or blue < " + index2f;
            dataLine1 += " for pixels with green < " + index1 +
                                " or blue < " + index2f + "\n";
        }
        else {
            currentThreshold += " for pixels with " +
                                 imageA.getImageName() + " < " + index1 +
                                " or " + imageB.getImageName() + " < " + index2f;
            dataLine1 += " for pixels with " +
                                 imageA.getImageName() + " < " + index1 +
                                " or " + imageB.getImageName() + " < " + index2f + "\n";
        }
    }
    else {
        if ((useRed) && (useGreen)) {
            currentThreshold += " for pixels with red < " + index1f +
                                " or green < " + index2;
            dataLine1 += " for pixels with red < " + index1f +
                                " or green < " + index2 + "\n";
        }
        else if ((useRed) && (useBlue)) {
            currentThreshold += " for pixels with red < " + index1f +
                                " or blue < " + index2;
            dataLine1 += " for pixels with red < " + index1f +
                                " or blue < " + index2 + "\n";
        }
        else if ((useGreen) && (useBlue)) {
            currentThreshold += " for pixels with green < " + index1f +
                                " or blue < " + index2;
            dataLine1 += " for pixels with green < " + index1f +
                                " or blue < " + index2 + "\n";
        }
        else {
            currentThreshold += " for pixels with " +
                                 imageA.getImageName() + " < " + index1f +
                                " or " + imageB.getImageName() + " < " + index2;
            dataLine1 += " for pixels with " +
                                 imageA.getImageName() + " < " + index1f +
                                " or " + imageB.getImageName() + " < " + index2 + "\n";
        }
    }

    labelCurrent.setText(currentThreshold);

    userInterface.setDataText(dataLine1);
    userInterface.setDataText(dataLine2);
    userInterface.setDataText("\n");
    userInterface.setGlobalDataText(dataLine1);
    userInterface.setGlobalDataText(dataLine2);
    userInterface.setGlobalDataText("\n");

    nf = NumberFormat.getNumberInstance();
    nf.setMaximumFractionDigits(6);
  }

  public void setCurrentLabels(String str1, String str2, String str3,
                               String str4) {
      labelCurrentColoc.setText(str1);
      labelCurrentIntensity1.setText(str2);
      labelCurrentIntensity2.setText(str3);
      labelCurrent.setText(str4);
  }

  /**
   *  Method that adds components to the control paenl
   *  @param c     component added to the control panel
   *  @param gbc   GridBagConstraints of added component
   *  @param x     grdix location
   *  @param y     gridy location
   *  @param w     gridwidth
   *  @param h     gridheight
   */
  private void addStatusPanel(Component c, GridBagConstraints gbc, int x,
                               int y, int w, int h) {
    gbc.gridx = x;
    gbc.gridy = y;
    gbc.gridwidth = w;
    gbc.gridheight = h;
    statusPanel.add(c, gbc);
  }

  /**
   *  Disposes of components and frame.
   */
  public void dispose() {
    //  System.err.println("In dispose");

    if (alg != null) {
        alg.finalize();
        alg = null;
    }
    setVisible(false);
    imageA.removeImageDisplayListener(this);
    imageA.clearMask();
    if (destImage != null) {
        destImage.disposeLocal();
    }
    if (componentImage != null) {
        componentImage.dispose(false);
    }

    haveThreshold = null;
    rThreshold= null;
    colocSize = null;
    colocIntensity1 = null;
    haveFreeRangeThreshold = null;

    componentImage = null;
    imageBufferA = null;
    imageBufferB = null;
    imageBufferDest = null;
    pixBufferDest = null;
    paintBufferDest = null;
    scrollPane = null;
    statusPanel = null;
    if (controls != null) {
        controls.removeAll();
        controls = null;
    }

    if (menuObj != null) {
        menuObj.finalize();
        menuObj = null;
    }

    toolBar = null;
    topPanel = null;
    innerPanel = null;
    cpGBL = null;
    cpGBC = null;
    gbl = null;
    gbc = null;
    menuItemVector = null;
}


  public void setLUTdest(ModelLUT LUTdest) {
      this.LUTdest = LUTdest;
      float min, max;

      if (logMagDisplay) {
          min = (float) (0.4342944819*Math.log(1.0 + destImage.getMin()));
          max = (float) (0.4342944819*Math.log(1.0 + destImage.getMax()));
      }
      else {
          min = (float)destImage.getMin();
          max = (float)destImage.getMax();
      }
      float imgMin = min;
      float imgMax = max;;
      LUTdest.resetTransferLine(min, imgMin, max, imgMax);
      componentImage.buildImageDestObject(LUTdest,true);
      componentImage.update();
      setTitle();
  }


  //************************************************************************
   //**************************** Action Events *****************************
    //************************************************************************

     /**
      *  Calls various methods depending on the action
      *  @param event      event that triggered function
      */
     public void actionPerformed(ActionEvent event) {

       String command;
       //String tmpStr;
       command = event.getActionCommand();

       if (command.equals("Reset")) {
         componentImage.setPosition(originalX,originalY);
       }
       else if (command.equals("allSlice")) {
        if (imageA.isColorImage()) {
            alg.calc2DColorStats();
        }
        else {
            alg.calc2DBWStats();
        }
       }
       else if (command.equals("MagColocalize")) {
         // Doubles the present magnification.  The zoom is always a power of 2.
         zoom = 2.0f * componentImage.getZoomX();
         componentImage.setZoom(zoom, zoom);
         validate();
         updateImages(true);
         setTitle();
       }
       else if (command.equals("UnMagColocalize")) {
         // Halves the present magnification.  The zoom is always a power of 2.
         zoom = 0.5f * componentImage.getZoomX();
         componentImage.setZoom(zoom, zoom);
         validate();
         updateImages(true);
         setTitle();
       }
       else if (event.getActionCommand().equals("CloseColocalize")) {
         close();
       }
       else if (event.getActionCommand().equals("AboutColocalize")) {
         MipavUtil.showHelp("10052");
       }
       else if (command.equals("DisplayLUT")) {

           if (destImage.getHistoLUTFrame() == null) {
               JDialogHistogramLUT histogramDialog = null;

               try {
                   histogramDialog = new JDialogHistogramLUT(this,destImage, null,
                        LUTdest,null,userInterface);
               }
               catch (OutOfMemoryError error) {
                    MipavUtil.displayError(
                     "Out of memory: unable to open LUT frame.");
               }

               histogramDialog.setColocalizationRegFrame(true);
               histogramDialog.histogramLUT(true);

           }
       }
       else if (command.equals("Brightness")) {
         // The toolbar button to create the dialog for brightness and contrast was pushed
         new JDialogBrightness(this, componentImage, origBrightness,
                               origContrast);
       }
       else if (command.equals("log")) {
          logMagDisplay = logDisplayButton.isSelected();
          componentImage.setLogMagDisplay(logMagDisplay);

          int dimExtentsLUT[] = new int[2];
          dimExtentsLUT[0] = 4;
          dimExtentsLUT[1] = 256;

          LUTdest = new ModelLUT(ModelLUT.COOLHOT, 256, dimExtentsLUT);
          float min, max;

          if (logMagDisplay) {
              min = (float) (0.4342944819*Math.log(1.0 + destImage.getMin()));
              max = (float) (0.4342944819*Math.log(1.0 + destImage.getMax()));
          }
          else {
              min = (float)destImage.getMin();
              max = (float)destImage.getMax();
          }
          float imgMin = min;
          float imgMax = max;;
          LUTdest.resetTransferLine(min, imgMin, max, imgMax);
          componentImage.buildImageDestObject(LUTdest,true);
          componentImage.update();
       }
       else if (command.equals("region")) {
          regionLinesDisplay = regionLinesDisplayButton.isSelected();
          componentImage.setRegionLinesDisplay(regionLinesDisplay);
          componentImage.update();
       }
       else if (command.equals("freeRange")) {
          freeRangeMode = freeRangeButton.isSelected();
          componentImage.setFreeRangeMode(freeRangeMode);
          if (freeRangeMode && haveFreeRangeThreshold == null) {
               alg.createFreeRangeArrays();
          }
       }
     }

  /**
   *   Sets the brightness and contrast of the component image.
   *   Set all slices to have the new brightness and constrast.
   *   Results in createImage producing an Image img[slice] for every slice.
   *   @param brightness   Brightness to set.
   *   @param contrast     Contrast to set.
   */
  public void setBrightness(int brightness, float contrast) {
    origBrightness = brightness;
    origContrast = contrast;
    componentImage.setBrightness(brightness, contrast);
  }

  /**
   *   Resets current slice's brightness and contrast to original.
   */
  public void cancelBrightness() {
    componentImage.setBrightness(origBrightness, origContrast);
  }




  /**
   *   Tests that the entered parameter is in range
   *   @param str        the value entered by the user
   *   @param minValue   the minimum value this variable may be set to
   *   @param maxValue   the maximum value this variable may be set to
   *   @return           boolean result of test
   */
  protected boolean testParameter(String str, double minValue, double maxValue) {
    double tmp;

    try {
      tmp = Double.valueOf(str).doubleValue();
      if (tmp > maxValue || tmp < minValue) {
        MipavUtil.displayError("Value is out of range: " +
                               String.valueOf(minValue) +
                               " , " + String.valueOf(maxValue));
        return false;
      }
      else {
        return true;
      }
    }
    catch (NumberFormatException error) {
      MipavUtil.displayError("Must enter numeric value");
      return false;
    }
  }

  //********************************************************************
   //************************** Item Events *****************************
    //********************************************************************

     /**
      *   Sets values based on knob along slider
      *   @param e    Event that triggered this function
      */
     public void stateChanged(ChangeEvent e) {
       Object source = e.getSource();


     }

  //************************************************************************
   //**************************** Window Events *****************************
    //************************************************************************

     /**
      *   Does nothing.
      */
     public void windowOpened(WindowEvent event) {
     }

  /**
   *  Stops thread, calls close
   *  @param event    event that triggered function
   */
  public void windowClosing(WindowEvent event) {
    ViewJComponentEditImage editImageFrameA;
    ViewJComponentEditImage editImageFrameB = null;
    int t,z,zInitial;
    int zDim,tDim;
    int interpMode = ViewJComponentBase.SMOOTH;

    alg.removeVOIUpdateListener();
    if (imageA.getNDims() >= 3) {
        zDim = imageA.getExtents()[2];
    }
    else {
        zDim = 1;
    }
    if (imageA.getNDims() >=4) {
        tDim = imageA.getExtents()[3];
    }
    else {
        tDim =1;
    }
    editImageFrameA = imageA.getParentFrame().getComponentImage();
    if (imageB != null) {
        editImageFrameB = imageB.getParentFrame().getComponentImage();
    }
    if (imageA.isColorImage()) {
        editImageFrameA.setThresholdColors(false,false,false);
    }
    else {
        editImageFrameA.setImageColocalize(null);
        editImageFrameB.setImageColocalize(null);
        editImageFrameA.setHasThreshold1(false);
        editImageFrameB.setHasThreshold2(false);
    }
    zInitial = imageA.getParentFrame().getComponentImage().getSlice();
    if (imageA.isColorImage()) {
        for (t = 0 ; t < tDim; t++) {
            for (z = zInitial+1; z < zDim; z++) {
                editImageFrameA.show(t,z,true);
            }
            for (z = 0; z <= zInitial; z++) {
                editImageFrameA.show(t,z,true);
            }
        }
    } // if (imageA.isColorImage())
    else { // black and white images
        for (t = 0; t < tDim; t++) {
            for (z = zInitial+1; z < zDim; z++) {
                editImageFrameA.show(t,z,null,null,true,interpMode);
                editImageFrameB.show(t,z,null,null,true,interpMode);
            }
            for (z = 0; z <= zInitial; z++) {
                editImageFrameA.show(t,z,null,null,true,interpMode);
                editImageFrameB.show(t,z,null,null,true,interpMode);
            }
        }
    } // black and white images

    try {
        if (componentImage != null) {

            componentImage.finalize();
        }
    } catch (Throwable e) {
        System.err.println("caught e");
        e.printStackTrace();
    }
    dispose();
   // if (destImage != null) {
   //     System.err.println("disposing dest image");
  //      destImage.disposeLocal();
  //  }
    close();
  }

  /**
   *   Does nothing.
   */
  public void windowClosed(WindowEvent event) {
  }

  /**
   *   Does nothing.
   */
  public void windowIconified(WindowEvent event) {
  }

  /**
   *   Does nothing.
   */
  public void windowDeiconified(WindowEvent event) {
  }

  /**
   *   Does nothing.
   */
  public void windowActivated(WindowEvent event) {
  }

  /**
   *   Does nothing.
   */
  public void windowDeactivated(WindowEvent event) {
  }

  /**
   *   Does nothing.
   */
  public void setEnabled(boolean flag) {
  }

  /**
   *   Does nothing.
   */
  public void setAlphaBlend(int value) {
  }

  /**
   *   Sets the RGB LUT table for ARGB image A
   *   @param RGBT the new RGB LUT to be applied to the image
   */
  public void setRGBTA(ModelRGB RGBT) {
    if (componentImage != null) {
      componentImage.setRGBTA(RGBT);
    }
  }

  /**
   *   Sets the RGB LUT table for ARGB image B
   *   @param RGBT the new RGB LUT to be applied to the image
   */
  public void setRGBTB(ModelRGB RGBT) {
    if (componentImage != null) {
      componentImage.setRGBTB(RGBT);
    }
  }

  /**
   *   This methods calls the componentImage's update method
   *   to redraw the screen. Without LUT changes.
   *   @param forceShow  unused parameter
   *   @return           boolean confirming successful update
   */
  public final boolean updateImages(boolean forceShow) {
    if (componentImage == null) {
      return false;
    }

    componentImage.update();
    return true;
  }

  /**
   *   Does nothing.
   */
  public final boolean updateImages(ModelLUT LUTa, ModelLUT LUTb,
                                    boolean forceShow, int interpMode) {
    return true;
  }

  /**
   *   Does nothing.
   */
  public final boolean updateImages() {
    return true;
  }

  /**
   *   Does nothing.
   */
  public boolean updateImageExtents() {
    return false;
  }

  /**
   *   Does nothing.
   */
  public void setControls() {
  }

  /**
   *   Does nothing.
   */
  public void removeControls() {
  }

  /**
   *   Get control widgets for frame
   *   @return       controls
   */
  public ViewControlsImage getControls() {
    return controls;
  }

  /**
   *   Does nothing.
   */
  public void setActiveImage(int active) {
  }

  /**
   *   Does nothing.
   */
  public void setPaintBitmapSwitch(boolean paintBitmapSwitch) {
  }

  /**
   *   Does nothing
   */
  public void setSlice(int slice) {
  }

  /**
   *   Does nothing.
   */
  public void setTimeSlice(int slice) {
  }

  /**
   *   Accessor that returns the reference to imageA
   *   @return     image
   */
  public ModelImage getImageA() {
    if (componentImage != null) {
      return componentImage.getImageA();
    }
    else {
      return null;
    }
  }

  /**
   *   Accessor that returns the reference to imageB
   *   @return     imageB
   */
  public ModelImage getImageB() {
    if (componentImage != null) {
      return componentImage.getImageB();
    }
    else {
      return null;
    }
  }

  /**
   *   Does nothing.
   */
  public void setImageB(ModelImage _imageB) {
  }

  /**
   *	Helper method to create a label with the proper font and font color.
   *	@param title	Text of the label.
   *	@return			New label.
   */
  private JLabel createLabel(String title) {
    JLabel label = new JLabel(title);
    label.setFont(MipavUtil.font12);
    label.setForeground(Color.black);
    return label;
  }
}
