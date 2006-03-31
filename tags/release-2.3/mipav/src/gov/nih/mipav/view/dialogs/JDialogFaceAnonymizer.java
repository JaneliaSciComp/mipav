package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 *    DialogFaceAnonymizer class creates a dialog which launches the
 *    FaceAnonymizer Algorithm class.  In the dialog the the user can specify
 *    the direction in the (x,y) plane that the face points toward - default
 *    is +x.  The user also specifies the min/max values for "skin", as well
 *    as a maximum thickness and blur factor.
 *
 *    @version    May 17, 2004
 *    @author     Alexandra A Bokinsky, Magic Software.
 *                                      Under contract from NIH.
 *
 */
public class JDialogFaceAnonymizer
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

    public static final String FIND_BUTTON = "Find Face";
    public static final String DELETE_BUTTON = "Delete Face";
    public static final String BLUR_BUTTON = "Blur Face";

    public static final int FACING_RIGHT = 1;
    public static final int FACING_LEFT = 2;
    public static final int FACING_DOWN = 3;
    public static final int FACING_UP = 4;
    public static final int FACING_INTO_SCREEN = 5;
    public static final int FACING_OUT_OF_SCREEN = 6;
    private int pToADirection = FACING_LEFT;

    private AlgorithmFaceAnonymizer m_kFaceAnonymizerAlgorithm;
    private ModelImage image = null; // source image
    private ViewUserInterface userInterface;

    // Radio buttons to specify the direction in (x,y) plane that the face is
    // pointing toward:
    private JRadioButton m_kYPositive;
    private JRadioButton m_kYNegative;
    private JRadioButton m_kXPositive;
    private JRadioButton m_kXNegative;
    private JRadioButton m_kZPositive;
    private JRadioButton m_kZNegative;

    private String titles[];

    // Text fields for entering the parameter values
    private JTextField m_kSkinMin;
    private JTextField m_kSkinMax;
    private JTextField m_kSkinThickness;
    private JTextField m_kBlurFactor;

    private int m_iSkinMin         = -100;
    private int m_iSkinMax         =  100;
    private int m_iSkinThickness   =   5;
    private int m_iBlurFactor      =    3;
    private int m_aiAxis[]         = {-1, 0, 0 };
    private double m_adImageRange[] = { 0, 0 };

    // boolean contains state of the dialog this is done so that the dialog
    // can stay open while the Algorithm is finding and bluring the results so
    // that the user can undo either operation
    private boolean m_bAlgorithmLaunched ;
    private boolean dialogPresent;
    private String command;
    private String sEvent;

    /**
     *	Sets the appropriate variables.
     *	@param parent          Parent frame.
     *	@param im              Source image.
     */
    public JDialogFaceAnonymizer(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        setForeground(Color.black);
        image = im;
        userInterface = ( (ViewJFrameBase) parentFrame).getUserInterface();
        init();
        dialogPresent = true;

        m_bAlgorithmLaunched = false;
        m_adImageRange[0] = image.getMin();
        m_adImageRange[1] = image.getMax();
    }

    /**
     *	Sets the appropriate variables.  Does not actually create a dialog
     *that is visible because no user input is necessary at present.  This
     *constructor is used by the script parser because it doesn't have the
     *parent frame.
     *	@param ui       	User interface.
     *	@param im           	Source image.
     */
    public JDialogFaceAnonymizer(ViewUserInterface ui, ModelImage im) {
        super();
        parentFrame = im.getParentFrame();
        //setForeground(Color.black);
        image = im;
        this.userInterface = ui;
        //init();
        dialogPresent = false;

        m_bAlgorithmLaunched = false;
        m_adImageRange[0] = image.getMin();
        m_adImageRange[1] = image.getMax();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFaceAnonymizer() {}

    /**
    * Run this algorithm from a script.
    * @param parser the script parser we get the state from
    * @throws IllegalArgumentException if there is something wrong with the arguments in the script
    */
   public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
       String srcImageKey = null;

       try {
           srcImageKey = parser.getNextString();
       } catch (Exception e) {
           throw new IllegalArgumentException();
       }
       ModelImage im = parser.getImage(srcImageKey);

       image = im;
       userInterface = image.getUserInterface();
       parentFrame = image.getParentFrame();

       dialogPresent = false;
       m_bAlgorithmLaunched = false;
       m_adImageRange[0] = image.getMin();
       m_adImageRange[1] = image.getMax();

       try {
           setSEvent(parser.getNextString());
           int axis[] = new int[3];
           for (int i = 0; i < 3; i++)
               axis[i] = parser.getNextInteger();
           setAxis(axis);
           setMin(parser.getNextInteger());
           setMax(parser.getNextInteger());
           setThickness(parser.getNextInteger());
           //setBlur(parser.getNextInteger());
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
               //check to see if the match image is already in the ImgTable
               if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {
                   if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                       userInterface.getScriptDialog().putActiveVar(image.getImageName());
                   }
               }

               userInterface.getScriptDialog().append("FaceAnonymizer " +
                                                      userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) +
                                                      " ");
               userInterface.getScriptDialog().append(sEvent             + " " +
                                                      m_aiAxis[0]        + " " +
                                                      m_aiAxis[1]        + " " +
                                                      m_aiAxis[2]        + " " +
                                                      m_iSkinMin         + " " +
                                                      m_iSkinMax         + " " +
                                                      m_iSkinThickness   /*+ " " +
                                                      m_iBlurFactor      */+
                                                      "\n");
           }
       }
   }


    /**
     *	Makes the GUI elements of the dialog.  Not called at present because
     *	it is not necessary.
     */
    private void init() {
        setTitle("Face Anonymizer");
        getContentPane().setLayout(new BorderLayout());

        // Face orientation panel:
        JPanel optionsPanel = new JPanel(new GridLayout(3, 2));
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder
                              ("Face Posterior To Anterior Orientation"));

        // See if the image orientation is known
        int axisOrientation[] = image.getFileInfo(0).getAxisOrientation();
        if (axisOrientation[0] == FileInfoBase.ORI_P2A_TYPE) {
            pToADirection = FACING_RIGHT;
        }
        else if (axisOrientation[0] == FileInfoBase.ORI_A2P_TYPE) {
            pToADirection = FACING_LEFT;
        }
        else if (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) {
            pToADirection = FACING_DOWN;
        }
        else if (axisOrientation[1] == FileInfoBase.ORI_A2P_TYPE) {
            pToADirection = FACING_UP;
        }
        else if (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE) {
            pToADirection = FACING_INTO_SCREEN;
        }
        else if (axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) {
            pToADirection = FACING_OUT_OF_SCREEN;
        }


        //Create the radio buttons.
        m_kXPositive = new JRadioButton("+ x");
        m_kXPositive.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_RIGHT) {
            m_kXPositive.setSelected(true);
        }

        m_kXNegative = new JRadioButton("- x");
        m_kXNegative.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_LEFT) {
            m_kXNegative.setSelected(true);
        }

        m_kYPositive = new JRadioButton("+ y");
        m_kYPositive.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_DOWN) {
            m_kYPositive.setSelected(true);
        }

        m_kYNegative = new JRadioButton("- y");
        m_kYNegative.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_UP) {
            m_kYNegative.setSelected(true);
        }

        m_kZPositive = new JRadioButton("+ z");
        m_kZPositive.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_INTO_SCREEN) {
            m_kZPositive.setSelected(true);
        }

        m_kZNegative = new JRadioButton("- z");
        m_kZNegative.setMnemonic(KeyEvent.VK_B);
        if (pToADirection == FACING_OUT_OF_SCREEN) {
            m_kZNegative.setSelected(true);
        }

        //Group the radio buttons.
        //This is a logical grouping which makes the
        //selection mutually exclusive:
        ButtonGroup group = new ButtonGroup();
        group.add(m_kXPositive);
        group.add(m_kXNegative);
        group.add(m_kYPositive);
        group.add(m_kYNegative);
        group.add(m_kZPositive);
        group.add(m_kZNegative);

        //Register a listener for the radio buttons.
        m_kXPositive.addActionListener(this);
        m_kXNegative.addActionListener(this);
        m_kYPositive.addActionListener(this);
        m_kYNegative.addActionListener(this);
        m_kZPositive.addActionListener(this);
        m_kZNegative.addActionListener(this);
        //Place the buttons in the panel
        optionsPanel.add(m_kXPositive);
        optionsPanel.add(m_kXNegative);
        optionsPanel.add(m_kYPositive);
        optionsPanel.add(m_kYNegative);
        optionsPanel.add(m_kZPositive);
        optionsPanel.add(m_kZNegative);


        // Face parameter panel:
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Face Parameters"));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JLabel kSkinMinLabel = new JLabel("Skin minimum value");
        kSkinMinLabel.setFont(serif12);
        paramPanel.add(kSkinMinLabel, gbc);

        double imageMin = image.getMin();
        double imageMax = image.getMax();
        int skinMin = (int)Math.round(imageMin + 0.02*(imageMax - imageMin));
        int skinMax = (int)Math.round(imageMin + 0.30*(imageMax - imageMin));

        gbc.gridx = 1;
        m_kSkinMin = new JTextField();
        m_kSkinMin.setText(String.valueOf(skinMin));
        m_kSkinMin.setFont(serif12);
        paramPanel.add(m_kSkinMin, gbc);

        gbc.gridx = 0; gbc.gridy = 1;
        JLabel kSkinMaxLabel = new JLabel("Skin maximum value");
        kSkinMaxLabel.setFont(serif12);
        paramPanel.add(kSkinMaxLabel, gbc);

        gbc.gridx = 1;
        m_kSkinMax = new JTextField();
        m_kSkinMax.setText(String.valueOf(skinMax));
        m_kSkinMax.setFont(serif12);
        paramPanel.add(m_kSkinMax, gbc);

        gbc.gridx = 0; gbc.gridy = 2;
        JLabel kSkinThickness = new JLabel("Skin thickness");
        kSkinThickness.setFont(serif12);
        paramPanel.add(kSkinThickness, gbc);

        gbc.gridx = 1;
        m_kSkinThickness = new JTextField();
        m_kSkinThickness.setText("5");
        m_kSkinThickness.setFont(serif12);
        paramPanel.add(m_kSkinThickness, gbc);


        /*gbc.gridx = 0; gbc.gridy = 3;
        JLabel kBlurLabel = new JLabel("Blur in voxel neighborhood");
        kBlurLabel.setFont(serif12);
        paramPanel.add(kBlurLabel, gbc);

        gbc.gridx = 1;
        m_kBlurFactor = new JTextField();
        m_kBlurFactor.setText("3");
        m_kBlurFactor.setFont(serif12);
        paramPanel.add(m_kBlurFactor, gbc);*/


        //Find face button when the find face button is pressed the algorithm
        //computes the face mask, which is then displayed in the image window.
        gbc.gridx = 0; gbc.gridy = 3;
        JButton faceButton = new JButton( FIND_BUTTON );
        paramPanel.add(faceButton, gbc);
        faceButton.addActionListener(this);

        gbc.gridx = 1;
        JButton deleteButton = new JButton( DELETE_BUTTON );
        paramPanel.add(deleteButton, gbc);
        deleteButton.addActionListener(this);

        //Blur face button
        /*gbc.gridx = 2;
        JButton blurButton = new JButton( BLUR_BUTTON );
        paramPanel.add( blurButton, gbc );
        blurButton.addActionListener(this);*/

        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(optionsPanel, BorderLayout.NORTH);

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(false);
        setVisible(true);
    }

    public void setSEvent(String sEvent) {
        this.sEvent = sEvent;
    }

    public void setAxis(int[] m_aiAxis) {
        this.m_aiAxis = m_aiAxis;
    }

    public void setMin(int m_iSkinMin) {
        this.m_iSkinMin = m_iSkinMin;
    }

    public void setMax(int m_iSkinMax) {
        this.m_iSkinMax = m_iSkinMax;
    }

    public void setThickness(int m_iSkinThickness) {
        this.m_iSkinThickness = m_iSkinThickness;
    }

    /*public void setBlur(int m_iBlurFactor) {
        this.m_iBlurFactor = m_iBlurFactor;
    }*/

    /**
     *	Presently only the script function calls this method.  When the script
     *	sends this dialog the action command, this method calls run.
     *	@param event       event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        command = event.getActionCommand();
        Object source = event.getSource();

        if ( command.equals("OK") ||
             command.equals(FIND_BUTTON) ||
             command.equals(DELETE_BUTTON)/* ||
             command.equals(BLUR_BUTTON)*/ )
        {
            if ( setVariables() )
            {
                sEvent = event.getActionCommand();
                callAlgorithm();
            }
        }
        else if ( command.equals("Cancel") )
        {
            sEvent = event.getActionCommand();
            callAlgorithm();
            dispose();
        }
        else if ( command.equals("Help") )
        {
            MipavUtil.showHelp("10045");
        }
    }

    /**
     *	Use the GUI results to set up the variables
     * needed to run the algorithm.
     *	@return <code>true</code> if parameters set successfully,
     *	<code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if ( m_kXPositive.isSelected() )
        {
            m_aiAxis[0] =  1;
            m_aiAxis[1] =  0;
            m_aiAxis[2] =  0;
        }
        else if ( m_kXNegative.isSelected() )
        {
            m_aiAxis[0] = -1;
            m_aiAxis[1] =  0;
            m_aiAxis[2] =  0;
        }
        else if ( m_kYPositive.isSelected() )
        {
            m_aiAxis[0] =  0;
            m_aiAxis[1] =  1;
            m_aiAxis[2] =  0;
        }
        else if ( m_kYNegative.isSelected() )
        {
            m_aiAxis[0] =  0;
            m_aiAxis[1] = -1;
            m_aiAxis[2] =  0;
        }
        else if ( m_kZPositive.isSelected() )
        {
            m_aiAxis[0] =  0;
            m_aiAxis[1] =  0;
            m_aiAxis[2] =  1;
        }
        else if ( m_kZNegative.isSelected() )
        {
            m_aiAxis[0] =  0;
            m_aiAxis[1] =  0;
            m_aiAxis[2] = -1;
        }


        //Get the skin min value from the text box and check that it is within
        //the range of image values:
        tmpStr = m_kSkinMin.getText();
        if ( testParameter( tmpStr, m_adImageRange[0], m_adImageRange[1] ) )
        {
            m_iSkinMin = Integer.valueOf( tmpStr ).intValue();
        }
        else
        {
            m_kSkinMin.requestFocus();
            m_kSkinMin.selectAll();
            return false;
        }

        //Get the skin max value from the text box and check that it is within
        //the range of image values:
        tmpStr = m_kSkinMax.getText();
        if ( testParameter( tmpStr, m_adImageRange[0], m_adImageRange[1] ) )
        {
            m_iSkinMax = Integer.valueOf( tmpStr ).intValue();
        }
        else
        {
            m_kSkinMax.requestFocus();
            m_kSkinMax.selectAll();
            return false;
        }

        //Get the skin thickness value
        //it must be at most the size of the minimum image extent:
        tmpStr = m_kSkinThickness.getText();
        if ( testParameter( tmpStr, 1,
                            Math.min( image.getExtents()[0],
                                      image.getExtents()[1] ) ) )
        {
            m_iSkinThickness = Integer.valueOf( tmpStr ).intValue();
        }
        else
        {
            m_kSkinThickness.requestFocus();
            m_kSkinThickness.selectAll();
            return false;
        }

        //Get the blur factor
        //it must be at most half size of the minimum image extent:
        /*tmpStr = m_kBlurFactor.getText();
        if ( testParameter( tmpStr, 1,
                            Math.min( image.getExtents()[0],
                                      image.getExtents()[1] )/2.0 ) )
        {
            m_iBlurFactor = Integer.valueOf( tmpStr ).intValue();
        }
        else
        {
            m_kBlurFactor.requestFocus();
            m_kBlurFactor.selectAll();
            return false;
        }*/

        return true;
    }

    /**
     *	Calls the algorithm.
     */
    public void callAlgorithm() {

        try
        {
            System.gc();
            // Make algorithm
            if ( m_bAlgorithmLaunched == false )
            {
                m_kFaceAnonymizerAlgorithm = new
                    AlgorithmFaceAnonymizer(image,
                                            m_aiAxis, m_iSkinMin, m_iSkinMax,
                                            m_iSkinThickness/*, m_iBlurFactor*/ );


                // This is very important.  Adding this object as a listener
                // allows the algorithm to notify this object when it has
                // completed of failed.  See algorithm performed event.  This
                // is made possible by implementing AlgorithmedPerformed
                // interface
                m_kFaceAnonymizerAlgorithm.addListener(this);
            }
            else
            {
                m_kFaceAnonymizerAlgorithm.setAxis( m_aiAxis );
                m_kFaceAnonymizerAlgorithm.setMin( m_iSkinMin );
                m_kFaceAnonymizerAlgorithm.setMax( m_iSkinMax );
                m_kFaceAnonymizerAlgorithm.setRange( m_iSkinThickness );
                //m_kFaceAnonymizerAlgorithm.setBlur( m_iBlurFactor );
            }

            // Tell the Algorithm which button was just pressed:
            if ( sEvent.equals( "OK" ) )
            {
                m_kFaceAnonymizerAlgorithm.setOK();

                if (dialogPresent) {
                    // Hide dialog
                    setVisible(false);
                }
            }
            else if ( sEvent.equals( FIND_BUTTON ) )
            {
                m_kFaceAnonymizerAlgorithm.setFind();
            }
            else if ( sEvent.equals( DELETE_BUTTON ) )
            {
                m_kFaceAnonymizerAlgorithm.setDelete();
            }
            /*else if ( sEvent.equals( BLUR_BUTTON ) )
            {
                m_kFaceAnonymizerAlgorithm.setBlur();
            }*/
            else if ( sEvent.equals( "Cancel" ) )
            {
                m_kFaceAnonymizerAlgorithm.setCancel();

                if (dialogPresent) {
                    // Hide dialog
                    setVisible(false);
                }
            }


            // These next lines set the titles in all frames where the source
            // image is displayed to "locked - " image name so as to indicate
            // that the image is now read/write locked!  The image frames are
            // disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];
            for ( int i = 0; i < imageFrames.size(); i++ )
            {
                titles[i] = ( (Frame) (imageFrames.elementAt(i))).getTitle();
                ( (Frame) (imageFrames.elementAt(i))).setTitle("Locked: " +
                                                               titles[i]);
                ( (Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame( (Frame)
                                               (imageFrames.elementAt(i)));
            }

            if ( runInSeparateThread && (m_bAlgorithmLaunched == false) )
            {
                // Start the thread as a low priority because we wish
                // to still have user interface work fast.
                if ( m_kFaceAnonymizerAlgorithm.startMethod(Thread.MIN_PRIORITY)
                    == false )
                {
                    MipavUtil.displayError(
                              "A thread is already running on this object");
                }
                // This is the last step that must be done when the Algorithm
                // is first created: set launched flag to be true:
                m_bAlgorithmLaunched = true;
            }
            else
            {
                m_kFaceAnonymizerAlgorithm.setActiveImage(isActiveImage);
                if ( !userInterface.isAppFrameVisible() )
                {
                    m_kFaceAnonymizerAlgorithm.setProgressBarVisible(false);
                }
                m_kFaceAnonymizerAlgorithm.run();
            }
        }
        catch (OutOfMemoryError x)
        {
            System.gc();
            MipavUtil.displayError(
               "Dialog Anonymize Face : unable to allocate enough memory");
            return;
        }
    }

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************

    /**
     *	This method is required if the AlgorithmPerformed interface is
     *	implemented. It is called by the algorithms when it has completed or
     *	failed to to complete, so that the dialog can display the result image
     *	and/or clean up.
     *   @param algorithm   Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm)
    {
        if ( algorithm instanceof AlgorithmFaceAnonymizer )
        {
            // These next lines set the titles in all frames where the source
            // image is displayed to image name so as to indicate that the
            // image is now unlocked!  The image frames are enabled and then
            // registed to the userinterface.
            Vector imageFrames = image.getImageFrameVector();
            for ( int i = 0; i < imageFrames.size(); i++ )
            {
                ( (Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ( (Frame) (imageFrames.elementAt(i))).setEnabled(true);
                if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame &&
                     parentFrame != null)
                {
                    userInterface.registerFrame( (Frame)
                                                 (imageFrames.elementAt(i)));
                }
            }

            if ( parentFrame != null )
            {
                userInterface.registerFrame(parentFrame);
            }
            image.notifyImageDisplayListeners(null, true);

            insertScriptLine(algorithm);

            if ( m_kFaceAnonymizerAlgorithm.isFinished() == true )
            {
                m_kFaceAnonymizerAlgorithm.finalize();
                m_kFaceAnonymizerAlgorithm = null;
            }
        }
    }

}
