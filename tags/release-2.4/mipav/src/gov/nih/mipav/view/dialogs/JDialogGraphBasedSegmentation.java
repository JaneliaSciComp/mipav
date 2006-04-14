package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;


/**
 *   Dialog to get user input, then call the algorithm.
 *   The user has the option to generate a new image or replace the source image.
 *   It should be noted that the algorithms are executed in their own
 *   thread.
 *
 *		@version    0.1 September 30, 2005
 *		@author     William Gandler
 *              @see        AlgorithmGraphBasedSegmentation
 *
 */
public class JDialogGraphBasedSegmentation extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    private AlgorithmGraphBasedSegmentation segAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image
    private ModelImage featureImage = null;
    private JTextField textGauss;
    private float sigma;
    private JTextField textThreshold;
    private float threshold;
    private JTextField textMinComponentSize;
    private int minComponentSize;

    private ViewUserInterface userInterface;

    private String[] titles;

    private JPanel paramPanel;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced

    /**
     *  @param parent          Parent frame.
     *  @param im              Source image.
     */
    public JDialogGraphBasedSegmentation( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, false );
        image = im;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
        loadDefaults();
        setVisible(true);
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogGraphBasedSegmentation( ViewUserInterface UI, ModelImage im ) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogGraphBasedSegmentation() {};

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning( true );

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
            setSigma(parser.getNextFloat());
            setThreshold(parser.getNextFloat());
            setMinComponentSize(parser.getNextInteger());
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

                String line = "GraphBasedSegmentation " + userInterface.getScriptDialog().getVar(image.getImageName()) + " ";
                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    line += userInterface.getScriptDialog().getVar(resultImage.getImageName()) + " " + getParameterString(" ") + "\n";
                } else {
                    line += userInterface.getScriptDialog().getVar(image.getImageName()) + " " + getParameterString(" ") + "\n";

                }

                userInterface.getScriptDialog().append(line);
            }
        }
  }



    /**
     *  Loads the default settings from Preferences to set up the dialog
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                System.err.println(defaultsString);
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                textGauss.setText("" + MipavUtil.getFloat(st));
                textThreshold.setText("" + MipavUtil.getFloat(st));
                textMinComponentSize.setText("" + MipavUtil.getInt(st));

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                }
                else {
                    replaceImage.setSelected(true);
                }
            }
            catch (Exception ex) {
                // since there was a problem parsing the defaults string, start over with the original defaults
                System.out.println( "Resetting defaults for dialog: " + getDialogName() );
                Preferences.removeProperty( getDialogName() );
            }
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     * @param delim  the parameter delimiter (defaults to " " if empty)
     * @return       the parameter string
     */
    public String getParameterString( String delim ) {
        if ( delim.equals( "" ) ) {
            delim = " ";
        }

        String str = new String();
        str += sigma + delim;
        str += threshold + delim;
        str += minComponentSize;

        return str;
    }


    /**
     * Saves the default settings into the Preferences file
     */
    public void saveDefaults() {
        String defaultsString = new String( getParameterString(",") + "," + newImage.isSelected() );
        //System.err.println(defaultsString);
        Preferences.saveDialogDefaults(getDialogName(),defaultsString);
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground( Color.black );

        setTitle( "Graph Based Segmentation" );
        getContentPane().setLayout( new BorderLayout() );

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 3, 3, 3, 3 ) );
        mainPanel.setLayout( new GridBagLayout() );
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets( 3, 3, 3, 3 );
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel( new GridBagLayout() );
        paramPanel.setForeground( Color.black );
        paramPanel.setBorder( buildTitledBorder( "Parameters" ) );

        JLabel labelGauss = new JLabel( "Gaussian smoothing before segmentation (0.0 - 10.0) " );
        labelGauss.setForeground( Color.black );
        labelGauss.setFont( serif12 );
        paramPanel.add( labelGauss, gbc);

        gbc.gridx = 1;
        textGauss = new JTextField(10);
        textGauss.setText( "0.5" );
        textGauss.setFont( serif12 );
        paramPanel.add( textGauss, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel labelThreshold = new JLabel( "Threshold function " );
        labelThreshold.setForeground( Color.black );
        labelThreshold.setFont( serif12 );
        paramPanel.add( labelThreshold, gbc);

        gbc.gridx = 1;
        textThreshold = new JTextField(10);
        textThreshold.setText( "500.0" );
        textThreshold.setFont( serif12 );
        paramPanel.add( textThreshold, gbc );

        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel labelMinComponentSize = new JLabel( "Minimum component size " );
        labelMinComponentSize.setForeground( Color.black );
        labelMinComponentSize.setFont( serif12 );
        paramPanel.add( labelMinComponentSize, gbc);

        gbc.gridx = 1;
        textMinComponentSize = new JTextField(10);
        textMinComponentSize.setText( "20" );
        textMinComponentSize.setFont( serif12 );
        paramPanel.add( textMinComponentSize, gbc );

        destinationPanel = new JPanel( new BorderLayout() );
        destinationPanel.setForeground( Color.black );
        destinationPanel.setBorder( buildTitledBorder( "Destination" ) );

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton( "New image", true );
        newImage.setFont( serif12 );
        destinationGroup.add( newImage );
        destinationPanel.add( newImage, BorderLayout.NORTH );

        replaceImage = new JRadioButton( "Replace image", false );
        replaceImage.setFont( serif12 );
        destinationGroup.add( replaceImage );
        destinationPanel.add( replaceImage, BorderLayout.CENTER );

        // Only if the image is unlocked can it be replaced.
        if ( image.getLockStatus() == ModelStorageBase.UNLOCKED ) {
            replaceImage.setEnabled( true );
        } else {
            replaceImage.setEnabled( false );
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add( paramPanel, gbc );
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add( destinationPanel, gbc );

        getContentPane().add( mainPanel, BorderLayout.CENTER );
        getContentPane().add( buildButtons(), BorderLayout.SOUTH );
        pack();
        setResizable( true );
        setVisible( true );

        System.gc();
    }

    /**
     *	Accessor that sets the sigma.
     *	@param sigma	Value to set sigma to (should be between 0.0 and 10.0).
     */
    public void setSigma( float sigma ) {
        this.sigma = sigma;
    }

    /**
     *	Accessor that sets the threshold.
     *	@param threshold	Value to set threshold to (should be between 0.0 and 50000.0).
     */
    public void setThreshold( float threshold ) {
        this.threshold = threshold;
    }

    /**
     *	Accessor that sets the minimum component size.
     *	@param minComponentSize	Value to set minComponentSize to (should be between 1 and 100000).
     */
    public void setMinComponentSize( int minComponentSize ) {
        this.minComponentSize = minComponentSize;
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
     *  Closes dialog box when the OK button is pressed and calls the algorithm.
     *  @param event       Event that triggers function.
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if ( command.equals( "OK" ) ) {
            if ( setVariables() ) {
                callAlgorithm();
            }
        } else if ( command.equals( "Cancel" ) ) {
            dispose();
        } else if ( command.equals( "Help" ) ) {
            //MipavUtil.showHelp( "" );
        }

    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
      *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
      *   algorithms when it has completed or failed to to complete, so that the dialog can be display
      *   the result image and/or clean up.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {

       ViewJFrameImage imageFrame = null;
       if (algorithm instanceof AlgorithmGraphBasedSegmentation) {
         image.clearMask();
         if (segAlgo.isCompleted() == true && resultImage != null) {
           //The algorithm has completed and produced a new image to be displayed.

           updateFileInfo(image, resultImage);

           try {
             //resultImage.setImageName("Graph Based Segmentation");
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }

             //featureImage  = ((AlgorithmGraphBasedSegmentation)algorithm).getFeatureImage();
             //new ViewJFrameImage(featureImage,null, new Dimension(610, 220));
         }
         else if (resultImage == null) {
             image = segAlgo.getImage();

             try {
               imageFrame = new ViewJFrameImage(image, null, new Dimension(610, 200));
             }
             catch (OutOfMemoryError error) {
               System.gc();
               MipavUtil.displayError("Out of memory: unable to open new frame");
             }

         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
         }
       }
       // Update frame
       //((ViewJFrameBase)parentFrame).updateImages(true);

       insertScriptLine(algorithm);

       segAlgo.finalize();
       segAlgo = null;
       dispose();
     }


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textGauss.getText();
        if ( testParameter( tmpStr, 0.0, 10.0 ) ) {
            sigma = Float.valueOf( tmpStr ).floatValue();
        } else {
            textGauss.requestFocus();
            textGauss.selectAll();
            return false;
        }

        tmpStr = textThreshold.getText();
        if ( testParameter( tmpStr, 0.0, 50000.0 ) ) {
            threshold = Float.valueOf( tmpStr ).floatValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();
            return false;
        }

        tmpStr = textMinComponentSize.getText();
        if ( testParameter( tmpStr, 1.0, 100000.0 ) ) {
            minComponentSize = Integer.valueOf( tmpStr ).intValue();
        } else {
            textMinComponentSize.requestFocus();
            textMinComponentSize.selectAll();
            return false;
        }

        if ( replaceImage.isSelected() ) {
            displayLoc = REPLACE;
        } else if ( newImage.isSelected() ) {
            displayLoc = NEW;
        }

        return true;
    }

    /**
   *	Once all the necessary variables are set, call the Graph Based Segmentation
   *	algorithm based on whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {
    String name = makeImageName(image.getImageName(), "_graphBasedSeg");
      if (displayLoc == NEW) {
        try {
              resultImage = new ModelImage(ModelImage.ARGB,image.getExtents(), name,
                                           userInterface);

              /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM){
                  ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                  ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
                  ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                  ((FileInfoDicom)(resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                               } */
          // Make algorithm
          segAlgo = new AlgorithmGraphBasedSegmentation(resultImage, image, sigma, threshold,
                                                        minComponentSize);
          // This is very important. Adding this object as a listener allows the algorithm to
          // notify this object when it has completed of failed. See algorithm performed event.
          // This is made possible by implementing AlgorithmedPerformed interface
          segAlgo.addListener(this);
          // Hide dialog
          setVisible(false);

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            segAlgo.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              segAlgo.setProgressBarVisible(false);
            }
            segAlgo.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog Graph Based Segmentation: unable to allocate enough memory");
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
          segAlgo = new AlgorithmGraphBasedSegmentation(image, sigma, threshold,
                                                        minComponentSize);
          // This is very important. Adding this object as a listener allows the algorithm to
          // notify this object when it has completed of failed. See algorithm performed event.
          // This is made possible by implementing AlgorithmedPerformed interface
          segAlgo.addListener(this);
          // Hide the dialog since the algorithm is about to run.
          setVisible(false);

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface.
            if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            segAlgo.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              segAlgo.setProgressBarVisible(false);
            }
            segAlgo.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog Graph Based Segemntation: unable to allocate enough memory");
          return;
        }
      }

  }




}
