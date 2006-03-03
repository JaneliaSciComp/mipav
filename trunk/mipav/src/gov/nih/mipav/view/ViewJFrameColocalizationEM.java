package gov.nih.mipav.view;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.util.*;
import java.text.*;

/**
 *   This class produces a frame surrounding a 2D histogram with ellipses
 *   surrounding the most prominent clusters.
 *   Tool bar buttons are present for magnify,
 *   unmagnify, for generating a histogram LUT dialog,
 *   for generating a dialog to set brightness and contrast, and
 *   for switching between linear and log mode.
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
 *   close ViewJFramColocalization structure function.
 *	<p>
 *   ViewJFrameColocalizationEM is called in AlgorithmColocalizationEM.
 *   ViewJFrameColocalizationEM calls ViewJComponentColocalizationEM
 *
 */
public class ViewJFrameColocalizationEM
    extends ViewJFrameBase
    implements ChangeListener, ItemListener {

  private ViewUserInterface userInterface;
  private ModelLUT LUTa;
  private ModelLUT LUTb;
  // The LUT for the 2D histogram
  private ModelLUT LUTdest;
  private ModelImage imageA;
  private ModelImage imageB;
  // The image containing the 2D histogram
  private ModelImage destImage;
  private boolean useRed;
  private boolean useGreen;
  private boolean useBlue;

  //Minimum and maximum buffer and secondBuffer areas
  private double min1, max1, min2, max2;
  // bin1 is generated from Math.round(scale1*(buffer[i]-min1)) and
  // bin2 is generated from Math.round(scale2*(secondBuffer[i]-min2))
  private double scale1,scale2;

  // The spaces around the histogram bin area
  private int leftPad;
  private int rightPad;
  private int bottomPad;
  private int topPad;

  private JMenuBar openingMenuBar; // contains File and Options menus
  private ViewMenuBuilder menuObj;

  private ViewToolBarBuilder toolBarObj;
  private JToolBar toolBar;

  private Font font12 = MipavUtil.font12;
  private Font font12B = MipavUtil.font12B;

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
  private ViewJComponentColocalizationEM componentImage;
  private JScrollPane scrollPane;
  private ViewControlsImage controls;


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
  private JMenu fileMenu; // menu with save as and close commands
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
  private double mean[][];
  private double halfMajor[];
  private double halfMinor[];
  private double theta[];

  /**
   *   Makes a frame of the animated image
   *   @param _imageA         Model of imageA
   *   @param _LUTa           Model of LUT for image A
   *   @param _imageB         Model of imageB
   *   @param _LUTb           Model of LUT for image B
   *   @param _RGBTA          Model RGB LUT for color image (A) else null
   *   @param destImage       The destination image
   *   @param controlFrame    ViewJFrameBase passed to ViewJComponentColocalizationEM
   *   @param useRed
   *   @param useGreen
   *   @param useBlue
   *   @param min1
   *   @param max1
   *   @param min2
   *   @param max2
   *   @param scale1
   *   @param scale2
   *   @param leftPad
   *   @param rightPad
   *   @param bottomPad
   *   @param topPad
   *   @param mean
   *   @param halfMajor
   *   @param halfMinor
   *   @param theta
   */
  public ViewJFrameColocalizationEM(ModelImage _imageA,
                           ModelLUT _LUTa,
                           ModelImage _imageB,
                           ModelLUT _LUTb,
                           ModelRGB _RGBTA,
                           ModelImage destImage,
                           ViewJFrameBase controlFrame,
                           boolean useRed, boolean useGreen, boolean useBlue,
                           double min1,
                           double max1, double min2, double max2,
                           double scale1, double scale2,
                           int leftPad, int rightPad,
                           int bottomPad, int topPad, double mean[][],
                           double halfMajor[], double halfMinor[],
                           double theta[]) {

    super(_imageA, null);

    buildMenu();
    setJMenuBar(openingMenuBar);

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
    this.min1 = min1;
    this.max1 = max1;
    this.min2 = min2;
    this.max2 = max2;
    this.scale1 = scale1;
    this.scale2 = scale2;
    this.controlFrame = controlFrame;
    this.leftPad = leftPad;
    this.rightPad = rightPad;
    this.bottomPad = bottomPad;
    this.topPad = topPad;
    setTitle(imageA.getImageName());
    this.RGBTA = _RGBTA;
    this.mean = mean;
    this.halfMajor = halfMajor;
    this.halfMinor = halfMinor;
    this.theta = theta;



    setLocation(100, 100);

    toolBar = buildColocalizeToolBar(this);
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
    getContentPane().add(topPanel, "North");
    buildScrollPane(userInterface);


        /* componentY is added so that the previous software for ViewJFrameImage can be
         reused.  There the image was resized without a toolbar, statusPanel, or
       menubar contributing to the vertical length. */
    componentY = topPanel.getHeight() + openingMenuBar.getHeight();
    // structureY is the total of all nonimage components in the Y direction
    structureY = getInsets().top + componentY + getInsets().bottom;
    setSize( (int) Math.round(scrollPaneSize + 3 + getInsets().left +
                              getInsets().right),
            (int) Math.round(scrollPaneSize + 3 + structureY));

    addWindowListener(this);
    addComponentListener(this);


    // The magnification is set to the highest power of 2.0 for which scroll bars are not needed.
    zoom = Math.min(zoom, (float) (scrollPane.getViewportBorderBounds().width - 1) /
                          (imageA.getExtents()[0] - 1));
    zoom = Math.min(zoom, (float) (scrollPane.getViewportBorderBounds().height - 1) /
                          (imageA.getExtents()[1] - 1));
    for (int i = -10; i <= 10; i++) {
      if (zoom >= Math.pow(2.0, (double) i) &&
          zoom < Math.pow(2.0, (double) (i + 1))) {
        zoom = (float) Math.pow(2.0, (double) i);
      }
    }
    componentImage.setZoom(zoom, zoom);
    componentResized(null);
    setTitle();
    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    pack();
    destImage.addImageDisplayListener(this);
    imageA.addImageDisplayListener(this);
    userInterface.regFrame(this);
    setResizable(true);
    setVisible(true);
    componentImage.buildImageDestObject(null, true);
    componentImage.update();
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

      if (yScreen < 768) {
        scrollPaneSize = 512 - (768 - yScreen);
        if (scrollPaneSize < 400) {
          scrollPaneSize = 400;
        }
      }
      else {
        scrollPaneSize = 512;
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

          LUTdest = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
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

      componentImage = new ViewJComponentColocalizationEM(controlFrame,
                                                 imageA,
                                                 imageB,
                                                 destImage, LUTdest,
                                                 imageBufferDest,
                                                 useRed, useGreen, useBlue,
                                                 min1, max1, min2, max2,
                                                 scale1,scale2,
                                                 pixBufferDest, paintBufferDest,
                                                 zoom, extents,
                                                 logMagDisplay,
                                                 leftPad, rightPad,
                                                 bottomPad, topPad,
                                                 mean, halfMajor, halfMinor,
                                                 theta);


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
    if (getSize().width >= xScreen - 20 || getSize().height >= yScreen - 20) {
      return;
    }

    removeComponentListener(this);

    width = (int) Math.round(Math.max(getSize().width - 2 * getInsets().left -
                                      3, minimumToolBarWidth));
    height = (int) Math.round(Math.max(getSize().height - getInsets().top -
                                       componentY - getInsets().bottom - 3,
                                       minimumHeight));

    scrollPane.setSize(width, height);
    setSize(Math.max(scrollPane.getSize().width + getInsets().left +
                     getInsets().right,
                     minimumToolBarWidth + getInsets().left + getInsets().right),
            Math.max(getInsets().top + componentY + scrollPane.getSize().height +
                     getInsets().bottom,
                     minimumHeight));

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
   *   Close Colocalization.
   */
  private void buildMenu() {
    //JSeparator separator;
    try {
      //separator = new JSeparator();
      menuObj = new ViewMenuBuilder(this);
      openingMenuBar = new JMenuBar();
    }
    catch (OutOfMemoryError error) {
      MipavUtil.displayError("Out of memory: ViewJFrameColocalizationEM.buildMenu");
      return;
    }

    openingMenuBar.add(
        menuObj.makeMenu("File", false,
                         new JComponent[] {
                         menuObj.buildMenuItem("Close", "CloseColocalize", 0, null, false)
    }));

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
   *  Disposes of components and frame.
   */
  public void dispose() {
    setVisible(false);
    imageA.removeImageDisplayListener(this);

    if (componentImage != null)
      componentImage.dispose(true);
    componentImage = null;
    imageBufferA = null;
    imageBufferB = null;
    pixBufferDest = null;
    paintBufferDest = null;
    scrollPane = null;
    controls = null;

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


       if (command.equals("MagColocalize")) {
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
         dispose();
         close();
         //dispose();
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

               histogramDialog.setColocalizationEMFrame(true);
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

          LUTdest = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
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
    dispose();
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
