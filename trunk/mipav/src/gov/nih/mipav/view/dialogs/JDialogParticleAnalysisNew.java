package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;


/**
 *       Dialog to get user input, then call the algorithm. The user has the
 *       option to generate a new image or replace the source image. In addition the user
 *       selects if the algorithm is applied to whole image or to the
 *       VOI regions. It should be noted that the algorithms are executed in their own
 *       threads.
 *
 *		@version    1.0 Aug 24, 1999
 *		@author     Matthew J. McAuliffe, Ph.D.
 *       @see        AlgorithmMorphology2D
 *
 */
public class JDialogParticleAnalysisNew extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface, ItemListener {

    private AlgorithmMorphology2D particleAlgo2D = null;
    private AlgorithmMorphology25D particleAlgo25D = null;
    private AlgorithmMorphology3D particleAlgo3D = null;
    private ModelImage image; // source image
    private ModelImage resultImage = null; // result image
    private ViewUserInterface userInterface;

    private String[] titles;

    private JPanel maskPanelOpen;
    private JTextField textNIterOpen;
    private JLabel labelNIterOpen;
    private float value;

    private JComboBox comboBoxKernelOpen;
    private JLabel labelKernelOpen;
    private JLabel labelKernelSizeOpen;
    private JTextField textKernelSizeOpen;
    private float kernelSizeOpen;

    private JPanel destinationPanel;
    private ButtonGroup destinationGroup;
    private JRadioButton replaceImage;
    private JRadioButton newImage;

    private JPanel imageVOIPanel;
    private ButtonGroup imageVOIGroup;
    private JRadioButton wholeImage;
    private JRadioButton VOIRegions;
    private JCheckBox image25D;

    private int displayLoc; // Flag indicating if a new image is to be generated
    // or if the source image is to be replaced
    private int itersClose, itersOpen, itersErode;
    private int kernelOpen = 0;
    private boolean regionFlag = false;
    private boolean do25D = false;

    /** Close declearation */
    private JPanel maskPanelClose;
    private JTextField textNIterClose;
    private JLabel labelNIterClose;

    private JComboBox comboBoxKernelClose;
    private JLabel labelKernelClose;
    private JLabel labelKernelSizeClose;
    private JTextField textKernelSizeClose;
    private float kernelSizeClose;
    private int kernelClose = 0;
    private JCheckBox showResultCB;
    private boolean showFrame = false;

    /** Erode panel */
    private JPanel erodePanel;
    private JTextField textErode;
    private JLabel labelNIterErode;

    /**
     *   Creates new dialog.
     *   @param theParentFrame    Parent frame
     *   @param im                Source image
     */
    public JDialogParticleAnalysisNew( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, true );
        if ( im.getType() != ModelImage.BOOLEAN && im.getType() != ModelImage.UBYTE && im.getType() != ModelImage.USHORT ) {
            MipavUtil.displayError( "Source Image must be Boolean or UByte or UShort" );
            dispose();
            return;
        }

        image = im;
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();
        init();
    }

    /**
     *	Used primarily for the script to store variables and run the algorithm.  No
     *	actual dialog will appear but the set up info and result image will be stored here.
     *	@param UI   The user interface, needed to create the image frame.
     *	@param im	Source image.
     */
    public JDialogParticleAnalysisNew( ViewUserInterface UI, ModelImage im ) {
        super();
        if ( im.getType() != ModelImage.BOOLEAN && im.getType() != ModelImage.UBYTE && im.getType() != ModelImage.USHORT ) {
            MipavUtil.displayError( "Source Image must be Boolean or UByte or UShort" );
            dispose();
            return;
        }
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogParticleAnalysisNew() {}

    /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun( AlgorithmScriptParser parser )
        throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage( srcImageKey );

        if ( im.getType() != ModelImage.BOOLEAN && im.getType() != ModelImage.UBYTE && im.getType() != ModelImage.USHORT ) {
            MipavUtil.displayError( "Source Image must be Boolean or UByte or UShort" );
            dispose();
            return;
        }
        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        if ( srcImageKey.equals( destImageKey ) ) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setRegionFlag( parser.getNextBoolean() );
            setNumOpens( parser.getNextInteger() );
            setNumErode( parser.getNextInteger() );
            setKernelType( parser.getNextInteger() );
            setKernelSize( parser.getNextFloat() );
            setImage25D( parser.getNextBoolean() );
        } catch ( Exception e ) {
            throw new IllegalArgumentException();
        }

        setActiveImage( parser.isActiveImage() );
        setSeparateThread( false );
        callAlgorithm();
        if ( !srcImageKey.equals( destImageKey ) ) {
            parser.putVariable( destImageKey, getResultImage().getImageName() );
        }
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine( AlgorithmBase algo ) {
        if ( algo.isCompleted() ) {
            if ( userInterface.isScriptRecording() ) {
                // check to see if the match image is already in the ImgTable
                if ( userInterface.getScriptDialog().getImgTableVar( image.getImageName() ) == null ) {
                    if ( userInterface.getScriptDialog().getActiveImgTableVar( image.getImageName() ) == null ) {
                        userInterface.getScriptDialog().putActiveVar( image.getImageName() );
                    }
                }

                userInterface.getScriptDialog().append(
                        "ParticleAnalysisNew " + userInterface.getScriptDialog().getVar( image.getImageName() ) + " " );
                    if ( displayLoc == NEW ) {
                       userInterface.getScriptDialog().putVar( resultImage.getImageName() );
                       userInterface.getScriptDialog().append(
                               userInterface.getScriptDialog().getVar( resultImage.getImageName() ) + " " + regionFlag
                               + " " + itersOpen + " " + itersErode + " " + kernelOpen + " " + kernelSizeOpen + " " + do25D + "\n" );
                   } else {
                       userInterface.getScriptDialog().append(
                               userInterface.getScriptDialog().getVar( image.getImageName() ) + " " + regionFlag + " "
                               + itersOpen + " " + itersErode + " " + kernelOpen + " " + kernelSizeOpen + " " + do25D + "\n" );

                   }

            }
        }
    }

    /**
     *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Particle Analysis New" );

        labelNIterOpen = new JLabel( "Number of open (1-20)" );
        labelNIterOpen.setForeground( Color.black );
        labelNIterOpen.setFont( serif12 );

        textNIterOpen = new JTextField( 5 );
        textNIterOpen.setText( "1" );
        textNIterOpen.setFont( serif12 );

        labelKernelOpen = new JLabel( "Kernel selection" );
        labelKernelOpen.setForeground( Color.black );
        labelKernelOpen.setFont( serif12 );

        buildComboBox();
        comboBoxKernelOpen.addItemListener( this );

        String unitString = null;

        unitString = new String( image.getFileInfo()[0].sUnits[
                image.getFileInfo()[0].getUnitsOfMeasure( 0 )] );

        if ( image.getNDims() == 2 ) {
            labelKernelSizeOpen = new JLabel( "Circle diameter - " + unitString );
        } else {
            labelKernelSizeOpen = new JLabel( "Sphere diameter - " + unitString );
        }
        labelKernelSizeOpen.setForeground( Color.black );
        labelKernelSizeOpen.setBounds( 75, 120, 155, 25 );
        labelKernelSizeOpen.setFont( serif12 );
        labelKernelSizeOpen.setEnabled( false );

        textKernelSizeOpen = new JTextField( 5 );
        textKernelSizeOpen.setText( "1" );
        textKernelSizeOpen.setFont( serif12 );
        textKernelSizeOpen.setEnabled( false );

        maskPanelOpen = new JPanel( new GridBagLayout() );
        maskPanelOpen.setForeground( Color.black );
        maskPanelOpen.setBorder( buildTitledBorder( "Open Parameters" ) );

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets( 5, 5, 5, 5 );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelOpen.add( labelNIterOpen, gbc );
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelOpen.add( textNIterOpen, gbc );
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelOpen.add( labelKernelOpen, gbc );
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelOpen.add( comboBoxKernelOpen, gbc );
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelOpen.add( labelKernelSizeOpen, gbc );
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelOpen.add( textKernelSizeOpen, gbc );

        labelNIterClose = new JLabel( "Number of close (1-20)" );
        labelNIterClose.setForeground( Color.black );
        labelNIterClose.setFont( serif12 );

        textNIterClose = new JTextField( 5 );
        textNIterClose.setText( "1" );
        textNIterClose.setFont( serif12 );

        labelKernelClose = new JLabel( "Kernel selection" );
        labelKernelClose.setForeground( Color.black );
        labelKernelClose.setFont( serif12 );

        comboBoxKernelClose = new JComboBox();
        comboBoxKernelClose.setFont( serif12 );
        comboBoxKernelClose.setBackground( Color.white );

        if ( image.getNDims() == 2 ) {
            comboBoxKernelClose.addItem( "3x3 -  4 connected" );
            comboBoxKernelClose.addItem( "5x5 - 12 connected" );
            comboBoxKernelClose.addItem( "User sized circle." );
            comboBoxKernelClose.setSelectedIndex(2);
        } else if ( image.getNDims() == 3 ) {
            comboBoxKernelClose.addItem( "3x3x3 -  6 connected (2.5D: 4)" );
            comboBoxKernelClose.addItem( "5x5x5 - 24 connected (2.5D: 12)" );
            comboBoxKernelClose.addItem( "User sized sphere." );
        }

        comboBoxKernelClose.addItemListener( this );

        unitString = null;

        unitString = new String( image.getFileInfo()[0].sUnits[
                image.getFileInfo()[0].getUnitsOfMeasure( 0 )] );

        if ( image.getNDims() == 2 ) {
            labelKernelSizeClose = new JLabel( "Circle diameter - " + unitString );
        } else {
            labelKernelSizeClose = new JLabel( "Sphere diameter - " + unitString );
        }
        labelKernelSizeClose.setForeground( Color.black );
        labelKernelSizeClose.setBounds( 75, 120, 155, 25 );
        labelKernelSizeClose.setFont( serif12 );
        labelKernelSizeClose.setEnabled( false );

        textKernelSizeClose = new JTextField( 5 );
        textKernelSizeClose.setText( "1" );
        textKernelSizeClose.setFont( serif12 );
        textKernelSizeClose.setEnabled( false );

        maskPanelClose = new JPanel( new GridBagLayout() );
        maskPanelClose.setForeground( Color.black );
        maskPanelClose.setBorder( buildTitledBorder( "Close Parameters" ) );

        gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets( 5, 5, 5, 5 );

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelClose.add( labelNIterClose, gbc );
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelClose.add( textNIterClose, gbc );
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelClose.add( labelKernelClose, gbc );
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelClose.add( comboBoxKernelClose, gbc );
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelClose.add( labelKernelSizeClose, gbc );
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelClose.add( textKernelSizeClose, gbc );


        // build erode panel
        labelNIterErode = new JLabel("Number of erode (1-20)");
        labelNIterErode.setForeground(Color.black);
        labelNIterErode.setFont(serif12);

        textErode = new JTextField(5);
        textErode.setText("1");
        textErode.setFont(serif12);

        erodePanel = new JPanel(new GridBagLayout());
        erodePanel.setForeground(Color.black);
        erodePanel.setBorder(buildTitledBorder("Erode Parameters"));

        gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        erodePanel.add(labelNIterErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        erodePanel.add(textErode, gbc);

        destinationPanel = new JPanel( new GridBagLayout() );
        destinationPanel.setForeground( Color.black );
        destinationPanel.setBorder( buildTitledBorder( "Destination" ) );

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton( "New image", true );
        newImage.setFont( serif12 );
        destinationGroup.add( newImage );

        replaceImage = new JRadioButton( "Replace image", false );
        replaceImage.setBounds( 10, 42, 120, 25 );
        replaceImage.setFont( serif12 );
        destinationGroup.add( replaceImage );

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = gbc.BOTH;
        gbc.insets = new Insets( 0, 0, 0, 0 );
        destinationPanel.add( newImage, gbc );
        gbc.gridy = 1;
        destinationPanel.add( replaceImage, gbc );

        imageVOIPanel = new JPanel( new GridBagLayout() );
        imageVOIPanel.setForeground( Color.black );
        imageVOIPanel.setBorder( buildTitledBorder( "Process" ) );

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton( "Whole image", true );
        wholeImage.setFont( serif12 );
        imageVOIGroup.add( wholeImage );

        VOIRegions = new JRadioButton( "VOI region(s)", false );
        VOIRegions.setFont( serif12 );
        imageVOIGroup.add( VOIRegions );

        image25D = new JCheckBox( "Process image in 2.5D", false );
        image25D.setFont( serif12 );

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = gbc.BOTH;
        imageVOIPanel.add( wholeImage, gbc );
        gbc.gridy = 1;
        imageVOIPanel.add( VOIRegions, gbc );
        // Only if the image is unlocked can it be replaced.
        if ( image.getLockStatus() == ModelStorageBase.UNLOCKED ) {
            replaceImage.setEnabled( true );
        } else {
            replaceImage.setEnabled( false );
        }
        gbc.gridy = 2;
        imageVOIPanel.add( image25D, gbc );
        if ( image.getNDims() == 3 ) {
            image25D.setEnabled( true );
        } else {
            image25D.setEnabled( false );
        }

        JPanel checkBoxPanel = new JPanel();
        showResultCB = new JCheckBox("Show intermediate result frames");
        showResultCB.addItemListener(this);
        showResultCB.setSelected(false);
        checkBoxPanel.add(showResultCB);
        checkBoxPanel.setBorder(buildTitledBorder(""));

        JPanel mainPanel = new JPanel( new GridBagLayout() );

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add( maskPanelOpen, gbc );

        /*
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add( maskPanelClose, gbc );
        */

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add( erodePanel, gbc );

        JPanel rowPanel = new JPanel(new GridBagLayout() );
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.fill = gbc.BOTH;
        rowPanel.add( destinationPanel, gbc );
        gbc.gridx = 1;
        rowPanel.add( imageVOIPanel, gbc );
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        mainPanel.add(rowPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.fill = gbc.BOTH;
        mainPanel.add( checkBoxPanel, gbc );

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add( OKButton );
        buildCancelButton();
        buttonPanel.add( cancelButton );

        getContentPane().add( mainPanel );
        getContentPane().add( buttonPanel, BorderLayout.SOUTH );

        textKernelSizeOpen.setEnabled( true );
        labelKernelSizeOpen.setEnabled( true );
        textKernelSizeClose.setEnabled( true );
        labelKernelSizeClose.setEnabled( true );

        pack();
        setVisible( true );
    }

    /**
     *  Accessor that returns the image
     *  @return          The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     *  Closes dialog box when the OK button is pressed and
     *  calls the algorithm
     *  @param event       Event that triggers function
     */
    public void actionPerformed( ActionEvent event ) {
        String command = event.getActionCommand();

        if ( command.equals( "OK" ) ) {
            if ( setVariables() ) {
                callAlgorithm();
            }
        } else if ( command.equals( "Cancel" ) ) {
            dispose();
        }
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
     *	Accessor that sets the region flag.
     *	@param flag		<code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
     */
    public void setRegionFlag( boolean flag ) {
        regionFlag = flag;
    }

    /*
     * Accessor that sets the number of dilations to perform.
     * @param n  The number of erosions to do.
     */
    public void setNumOpens( int n ) {
        itersOpen = n;
    }


    /*
     * Accessor that sets the number of dilations to perform.
     * @param n  The number of erosions to do.
     */
    public void setNumErode(int n) {
        itersErode = n;
    }


    /*
     * Accessor that sets the kernel type to use.
     * @param krnl   the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, AlgorithmMorphology2D.CONNECTED12,
     *                 AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6, AlgorithmMorphology3D.CONNECTED24,
     *                 AlgorithmMorphology3D.SIZED_SPHERE (or the ones in AlgorithmMorphology25D))
     */
    public void setKernelType( int krnl ) {
        kernelOpen = krnl;
    }

    /*
     * Accessor that sets the kernel size
     * @param s   the desired kernel size
     */
    public void setKernelSize( float s ) {
        kernelSizeOpen = s;
    }

    /**
     * Process the image in 2.5D.
     * @param b whether to do 2.5D morphology
     */
    public void setImage25D( boolean b ) {
        do25D = b;
    }

    /**
     *	Use the GUI results to set up the variables needed to run the algorithm.
     *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        System.gc();
        String tmpStr;

        if ( replaceImage.isSelected() ) {
            displayLoc = REPLACE;
        } else if ( newImage.isSelected() ) {
            displayLoc = NEW;
        }

        if ( wholeImage.isSelected() ) {
            regionFlag = true;
        } else if ( VOIRegions.isSelected() ) {
            regionFlag = false;
        }

        do25D = image25D.isSelected();

        tmpStr = textNIterOpen.getText();
        if ( testParameter( tmpStr, 1, 20 ) ) {
            itersOpen = Integer.valueOf( tmpStr ).intValue();
        } else {
            textNIterOpen.requestFocus();
            textNIterOpen.selectAll();
            return false;
        }

        tmpStr = textKernelSizeOpen.getText();
        float max = (float) ( image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5 );

        // if ( max < 10 ) max = 10;
        if ( textKernelSizeOpen.isEnabled() == true ) {
            if ( testParameter( tmpStr, 0, max ) ) {
                kernelSizeOpen = Float.valueOf( tmpStr ).floatValue();
            } else {
                textKernelSizeOpen.requestFocus();
                textKernelSizeOpen.selectAll();
                return false;
            }
        }

        if ( image.getNDims() == 2 ) {

            if ( comboBoxKernelOpen.getSelectedIndex() == 0 ) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED4;
            } else if ( comboBoxKernelOpen.getSelectedIndex() == 1 ) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED12;
            } else if ( comboBoxKernelOpen.getSelectedIndex() == 2 ) {
                kernelOpen = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if ( image.getNDims() == 3 ) {
            if ( comboBoxKernelOpen.getSelectedIndex() == 0 ) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED6;
            } else if ( comboBoxKernelOpen.getSelectedIndex() == 1 ) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED24;
            } else if ( comboBoxKernelOpen.getSelectedIndex() == 2 ) {
                kernelOpen = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        tmpStr = textNIterClose.getText();
        if ( testParameter( tmpStr, 1, 20 ) ) {
            itersClose = Integer.valueOf( tmpStr ).intValue();
        } else {
            textNIterClose.requestFocus();
            textNIterClose.selectAll();
            return false;
        }

        tmpStr = textErode.getText();
         if ( testParameter( tmpStr, 1, 20 ) ) {
             itersErode = Integer.valueOf( tmpStr ).intValue();
         } else {
             textErode.requestFocus();
             textErode.selectAll();
             return false;
         }


        tmpStr = textKernelSizeClose.getText();
        max = (float) ( image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5 );

        // if ( max < 10 ) max = 10;
        if ( textKernelSizeClose.isEnabled() == true ) {
            if ( testParameter( tmpStr, 0, max ) ) {
                kernelSizeClose = Float.valueOf( tmpStr ).floatValue();
            } else {
                textKernelSizeClose.requestFocus();
                textKernelSizeClose.selectAll();
                return false;
            }
        }

        if ( image.getNDims() == 2 ) {

            if ( comboBoxKernelClose.getSelectedIndex() == 0 ) {
                kernelClose = AlgorithmMorphology2D.CONNECTED4;
            } else if ( comboBoxKernelClose.getSelectedIndex() == 1 ) {
                kernelClose = AlgorithmMorphology2D.CONNECTED12;
            } else if ( comboBoxKernelClose.getSelectedIndex() == 2 ) {
                kernelClose = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if ( image.getNDims() == 3 ) {
            if ( comboBoxKernelClose.getSelectedIndex() == 0 ) {
                kernelClose = AlgorithmMorphology3D.CONNECTED6;
            } else if ( comboBoxKernelClose.getSelectedIndex() == 1 ) {
                kernelClose = AlgorithmMorphology3D.CONNECTED24;
            } else if ( comboBoxKernelClose.getSelectedIndex() == 2 ) {
                kernelClose = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        return true;
    }

    /**
     *	Once all the necessary variables are set, call the Gaussian Blur
     *	algorithm based on what type of image this is and whether or not there
     *	is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName( image.getImageName(), "_particle" );

        if ( image.getNDims() == 2 ) { // source image is 2D
            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if ( displayLoc == NEW ) {
                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName( name );
                    // Make algorithm
                    particleAlgo2D = new AlgorithmMorphology2D( resultImage, kernelOpen, kernelSizeOpen, kernelClose, kernelSizeClose,
                            AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, itersOpen, itersErode, 0, 0, regionFlag, showFrame );
                    if ( regionFlag == false ) {
                        particleAlgo2D.setMask( image.generateVOIMask() );
                    }
                    // particleAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 5,
                    // AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, iters, regionFlag);
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo2D.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if ( particleAlgo2D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo2D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo2D.setProgressBarVisible( false );
                        }
                        particleAlgo2D.run();

                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }
                    return;
                }

            } else {
                try {
                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    // particleAlgo2D = new AlgorithmMorphology2D( image, kernel, kernelSize, AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW,
                      //       itersD, 0, 0, 0, regionFlag );
                    particleAlgo2D = new AlgorithmMorphology2D( image, kernelOpen, kernelSizeOpen, kernelClose, kernelSizeClose,
                            AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, itersOpen, itersErode, 0, 0, regionFlag, showFrame );
                    if ( regionFlag == false ) {
                        particleAlgo2D.setMask( image.generateVOIMask() );
                    }
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo2D.addListener( this );
                    // Hide the dialog since the algorithm is about to run.
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];
                    for ( int i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface.
                        if ( particleAlgo2D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo2D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo2D.setProgressBarVisible( false );
                        }
                        particleAlgo2D.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    return;
                }
            }
        } else if ( image.getNDims() == 3 && !do25D ) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if ( displayLoc == NEW ) {
                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName( name );

                    // Make algorithm
                    particleAlgo3D = new AlgorithmMorphology3D( resultImage, kernelOpen, kernelSizeOpen,
                            AlgorithmMorphology3D.PARTICLE_ANALYSIS, itersClose, itersOpen, 0, 0, regionFlag );
                    if ( regionFlag == false ) {
                        particleAlgo3D.setMask( image.generateVOIMask() );
                    }
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo3D.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( particleAlgo3D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo3D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo3D.setProgressBarVisible( false );
                        }
                        particleAlgo3D.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }
                    return;
                }
            } else {
                try {
                    // Make algorithm
                    particleAlgo3D = new AlgorithmMorphology3D( image, kernelOpen, kernelSizeOpen,
                        AlgorithmMorphology3D.PARTICLE_ANALYSIS, itersClose, itersOpen, 0, 0, regionFlag );
                    if ( regionFlag == false ) {
                        particleAlgo3D.setMask( image.generateVOIMask() );
                    }
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo3D.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];
                    for ( int i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( particleAlgo3D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo3D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo3D.setProgressBarVisible( false );
                        }
                        particleAlgo3D.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    return;
                }
            }
        } else if ( do25D ) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
            destExtents[2] = image.getExtents()[2]; // Z dim

            // convert to 2.5d kernel type
            if ( kernelOpen == AlgorithmMorphology3D.CONNECTED6 ) {
                kernelOpen = AlgorithmMorphology25D.CONNECTED4;
            } else if ( kernelOpen == AlgorithmMorphology3D.CONNECTED24 ) {
                kernelOpen = AlgorithmMorphology25D.CONNECTED12;
            } else if ( kernelOpen == AlgorithmMorphology3D.SIZED_SPHERE ) {
                kernelOpen = AlgorithmMorphology25D.SIZED_CIRCLE;
            }

            if ( displayLoc == NEW ) {
                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName( name );

                    // Make algorithm
                    particleAlgo25D = new AlgorithmMorphology25D( resultImage, kernelOpen, kernelSizeOpen,
                            AlgorithmMorphology25D.PARTICLE_ANALYSIS, itersClose, 0, 0, 0, regionFlag );
                    if ( regionFlag == false ) {
                        particleAlgo25D.setMask( image.generateVOIMask() );
                    }
                    // particleAlgo25D = new AlgorithmMorphology25D(resultImage, kernel, 5,
                    // AlgorithmMorphology25D.PARTICLE_ANALYSIS, iters, regionFlag);
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo25D.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if ( particleAlgo25D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo25D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo25D.setProgressBarVisible( false );
                        }
                        particleAlgo25D.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    if ( resultImage != null ) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }
                    return;
                }
            } else {
                try {
                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    particleAlgo25D = new AlgorithmMorphology25D( image, kernelOpen, kernelSizeOpen,
                            AlgorithmMorphology25D.PARTICLE_ANALYSIS,
                            itersClose, 0, 0, 0, regionFlag );
                    if ( regionFlag == false ) {
                        particleAlgo25D.setMask( image.generateVOIMask() );
                    }
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo25D.addListener( this );
                    // Hide the dialog since the algorithm is about to run.
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];
                    for ( int i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (Frame) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface.
                        if ( particleAlgo25D.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        particleAlgo25D.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            particleAlgo25D.setProgressBarVisible( false );
                        }
                        particleAlgo25D.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog particle analysis new: unable to allocate enough memory" );
                    return;
                }
            }
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
    public void algorithmPerformed( AlgorithmBase algorithm ) {

        ViewJFrameImage imageFrame = null;

        if ( algorithm instanceof AlgorithmMorphology2D ) {
            image.clearMask();
            if ( particleAlgo2D.isCompleted() == true && resultImage != null ) {
                updateFileInfo( image, resultImage );
                resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("Particle analysis image");
                    imageFrame = new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } else if ( resultImage == null ) {
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for ( int i = 0; i < imageFrames.size(); i++ ) {
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( true );
                    if ( ( (Frame) ( imageFrames.elementAt( i ) ) ) != parentFrame ) {
                        userInterface.registerFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }
                }
                if ( parentFrame != null ) {
                    userInterface.registerFrame( parentFrame );
                }
                image.notifyImageDisplayListeners( null, true );
            } else if ( resultImage != null ) {
                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } else if ( algorithm instanceof AlgorithmMorphology25D ) {
            image.clearMask();
            if ( particleAlgo25D.isCompleted() == true && resultImage != null ) {
                updateFileInfo( image, resultImage );
                resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("Particle analysis image");
                    imageFrame = new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } else if ( resultImage == null ) {
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for ( int i = 0; i < imageFrames.size(); i++ ) {
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( true );
                    if ( ( (Frame) ( imageFrames.elementAt( i ) ) ) != parentFrame ) {
                        userInterface.registerFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }
                }
                if ( parentFrame != null ) {
                    userInterface.registerFrame( parentFrame );
                }
                image.notifyImageDisplayListeners( null, true );
            } else if ( resultImage != null ) {
                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } else if ( algorithm instanceof AlgorithmMorphology3D ) {
            image.clearMask();
            if ( particleAlgo3D.isCompleted() == true && resultImage != null ) {
                updateFileInfo( image, resultImage );
                resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("Particle analysis image");
                    imageFrame = new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                } catch ( OutOfMemoryError error ) {
                    MipavUtil.displayError( "Out of memory: unable to open new frame" );
                }
            } else if ( resultImage == null ) {
                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for ( int i = 0; i < imageFrames.size(); i++ ) {
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                    ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( true );
                    if ( ( (Frame) ( imageFrames.elementAt( i ) ) ) != parentFrame ) {
                        userInterface.registerFrame( (Frame) ( imageFrames.elementAt( i ) ) );
                    }
                }
                if ( parentFrame != null ) {
                    userInterface.registerFrame( parentFrame );
                }
                image.notifyImageDisplayListeners( null, true );
            } else if ( resultImage != null ) {
                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        insertScriptLine( algorithm );

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        if ( particleAlgo2D != null ) {
            particleAlgo2D.finalize();
            particleAlgo2D = null;
        }
        if ( particleAlgo25D != null ) {
            particleAlgo25D.finalize();
            particleAlgo25D = null;
        }
        if ( particleAlgo3D != null ) {
            particleAlgo3D.finalize();
            particleAlgo3D = null;
        }
        dispose();
    }

    /**
     *   Builds kernel combo box.
     */
    private void buildComboBox() {

        comboBoxKernelOpen = new JComboBox();
        comboBoxKernelOpen.setFont( serif12 );
        comboBoxKernelOpen.setBackground( Color.white );

        if ( image.getNDims() == 2 ) {
            comboBoxKernelOpen.addItem( "3x3 -  4 connected" );
            comboBoxKernelOpen.addItem( "5x5 - 12 connected" );
            comboBoxKernelOpen.addItem( "User sized circle." );
            comboBoxKernelOpen.setSelectedIndex(2);
        } else if ( image.getNDims() == 3 ) {
            comboBoxKernelOpen.addItem( "3x3x3 -  6 connected (2.5D: 4)" );
            comboBoxKernelOpen.addItem( "5x5x5 - 24 connected (2.5D: 12)" );
            comboBoxKernelOpen.addItem( "User sized sphere." );
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     *   Enables text boxes depending on selection in combo box.
     *   @param event    Event that triggered this function.
     */
    public void itemStateChanged( ItemEvent event ) {
        Object source = event.getSource();

        if ( source == comboBoxKernelOpen ) {
            if ( comboBoxKernelOpen.getSelectedIndex() == 2 ) {
                textKernelSizeOpen.setEnabled( true );
                labelKernelSizeOpen.setEnabled( true );
            } else {
                textKernelSizeOpen.setEnabled( false );
                labelKernelSizeOpen.setEnabled( false );
            }
        } else if ( source == comboBoxKernelClose ) {
            if ( comboBoxKernelClose.getSelectedIndex() == 2 ) {
                textKernelSizeClose.setEnabled( true );
                labelKernelSizeClose.setEnabled( true );
            } else {
                textKernelSizeClose.setEnabled( false );
                labelKernelSizeClose.setEnabled( false );
            }
        }
        if ( source == showResultCB ) {
          if ( showResultCB.isSelected() ) {
            showFrame = true;
          } else {
            showFrame = false;
          }
        }
    }

}
