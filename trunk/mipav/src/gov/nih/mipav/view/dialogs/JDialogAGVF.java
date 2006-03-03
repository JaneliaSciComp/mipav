package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the snake-like algorithm.
 *
 * @see  AlgorithmAGVF
 */
public class JDialogAGVF extends JDialogBase
    implements AlgorithmInterface, ItemListener {

    private AlgorithmAGVF agvfAlgo;
    private ModelImage image; // source image
    private ModelImage resultImage; // magnitude of GVF
    private ViewUserInterface userInterface;

    private String[] titles;

    private JTextField textGaussX;
    private float scaleX;

    private JTextField textGaussY;
    private float scaleY;

    private JTextField textGaussZ;
    private JLabel labelGaussZ;
    private float scaleZ = 1.0f;

    private float[] sigmas;

    private JRadioButton propagate;
    private JRadioButton singleSlice;
    private JCheckBox removeOriginalCheckBox;
    private boolean removeOriginal;
    private JTextField textGVFIterations;
    private JLabel labelGVFIterations;
    private int gvfIterations;

    private JTextField textBoundaryIterations;
    private int boundaryIterations;
    private JTextField textK;
    private float kValue;

    private JTextField textSmoothness;
    private float smoothness;

    private JCheckBox checkboxDisplay;

    private JCheckBox do25DCheckBox;
    private boolean do25D = true;

    private ViewVOIVector VOIs = null;
    private int groupNum;
    private VOI srcVOI;
    private boolean propagationFlag;
    private Color voiColor;

    /**
     *   Creates new dialog for finding the GVF.
     *   @param theParentFrame   Parent frame
     *   @param im               Source image
     */
    public JDialogAGVF( Frame theParentFrame, ModelImage im ) {
        super( theParentFrame, true );
        userInterface = ( (ViewJFrameBase) ( parentFrame ) ).getUserInterface();

        VOIs = im.getVOIs();
        int nVOI;

        nVOI = VOIs.size();
        if ( nVOI == 0 ) {
            MipavUtil.displayError( "There are no VOIs to operate on." );
            dispose();
            return;
        }

        for ( groupNum = 0; groupNum < nVOI; groupNum++ ) {
            if ( VOIs.VOIAt( groupNum ).isActive() == true && VOIs.VOIAt( groupNum ).getCurveType() == VOI.CONTOUR ) {
                break;
            }
        }
        if ( groupNum == nVOI ) {
            MipavUtil.displayError( "VOI must be selected" );
            dispose();
            return;
        }
        voiColor = VOIs.VOIAt( groupNum ).getColor();
        srcVOI = VOIs.VOIAt( groupNum );
        image = im;
        init();
    }

    /**
     *   Initializes GUI variables and displays dialog.
     */
    private void init() {
        setForeground( Color.black );
        setTitle( "Evolve Boundary" );

        JPanel scalePanel = new JPanel( new GridLayout( 3, 2 ) );
        scalePanel.setForeground( Color.black );
        scalePanel.setBorder( buildTitledBorder( "Scale of the Gaussian" ) );

        JLabel labelGaussX = new JLabel( " X Dimension (0.0 - 5.0) " );
        labelGaussX.setForeground( Color.black );
        labelGaussX.setFont( serif12 );
        scalePanel.add( labelGaussX );

        textGaussX = new JTextField( 5 );
        textGaussX.setText( "1.0" );
        textGaussX.setFont( serif12 );
        scalePanel.add( textGaussX );

        JLabel labelGaussY = new JLabel( " Y Dimension (0.0 - 5.0) " );
        labelGaussY.setForeground( Color.black );
        labelGaussY.setFont( serif12 );
        scalePanel.add( labelGaussY );

        textGaussY = new JTextField( 5 );
        textGaussY.setText( "1.0" );
        textGaussY.setFont( serif12 );
        scalePanel.add( textGaussY );

        labelGaussZ = new JLabel( " Z Dimension (0.0 - 5.0) " );
        labelGaussZ.setForeground( Color.black );
        labelGaussZ.setFont( serif12 );
        labelGaussZ.setEnabled( false );
        scalePanel.add( labelGaussZ );

        textGaussZ = new JTextField( 5 );
        textGaussZ.setText( "1.0" );
        textGaussZ.setFont( serif12 );
        textGaussZ.setEnabled( false );
        scalePanel.add( textGaussZ );

        JPanel imageVOIPanel = new JPanel( new GridLayout( 3, 1 ) );
        imageVOIPanel.setForeground( Color.black );
        imageVOIPanel.setBorder( buildTitledBorder( "Evolve Boundary" ) );

        ButtonGroup imageVOIGroup = new ButtonGroup();
        singleSlice = new JRadioButton( " Single slice ", true );
        singleSlice.setFont( serif12 );
        singleSlice.addActionListener( this );
        imageVOIGroup.add( singleSlice );

        propagate = new JRadioButton( " Propagate to adjacent slices ", false );
        propagate.setFont( serif12 );
        propagate.addActionListener( this );
        imageVOIGroup.add( propagate );
        if ( image.getNDims() == 2 ) {
            propagate.setEnabled( false );
        }

        removeOriginalCheckBox = new JCheckBox( "Replace Original Contour" );
        removeOriginalCheckBox.setFont( serif12 );
        removeOriginalCheckBox.setForeground( Color.black );
        removeOriginalCheckBox.setSelected( false );

        imageVOIPanel.add( singleSlice );
        imageVOIPanel.add( propagate );
        imageVOIPanel.add( removeOriginalCheckBox );

        JPanel iterationsPanel = new JPanel( new GridLayout( 3, 2 ) );
        iterationsPanel.setForeground( Color.black );
        iterationsPanel.setBorder( buildTitledBorder( "Maximum Iterations" ) );

        JLabel labelGVFIterations = new JLabel( " GVF field iterations " );
        labelGVFIterations.setForeground( Color.black );
        labelGVFIterations.setFont( serif12 );

        textGVFIterations = new JTextField( 5 );
        textGVFIterations.setText( "200" );
        textGVFIterations.setFont( serif12 );

        JLabel labelBoundaryIterations = new JLabel( " Evolve iterations " );
        labelBoundaryIterations.setForeground( Color.black );
        labelBoundaryIterations.setFont( serif12 );

        textBoundaryIterations = new JTextField( 5 );
        textBoundaryIterations.setText( "300" );
        textBoundaryIterations.setFont( serif12 );

        checkboxDisplay = new JCheckBox( "Display GVF image" );
        checkboxDisplay.setFont( serif12 );
        checkboxDisplay.setForeground( Color.black );

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = gbc.WEST;
        gbc.fill = gbc.HORIZONTAL;
        iterationsPanel.add( labelGVFIterations );
        iterationsPanel.add( textGVFIterations );
        iterationsPanel.add( labelBoundaryIterations );
        iterationsPanel.add( textBoundaryIterations );
        iterationsPanel.add( checkboxDisplay );

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1;
        gbc2.anchor = gbc2.WEST;
        gbc2.fill = gbc2.HORIZONTAL;
        JPanel parametersPanel = new JPanel( new GridBagLayout() );
        parametersPanel.setForeground( Color.black );
        parametersPanel.setBorder( buildTitledBorder( "Parameters" ) );

        JLabel labelK = new JLabel( " GVF k (0.01-0.5) " );
        labelK.setForeground( Color.black );
        labelK.setFont( serif12 );
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        parametersPanel.add( labelK, gbc2 );

        textK = new JTextField( 5 );
        textK.setText( "0.15" );
        textK.setFont( serif12 );
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        parametersPanel.add( textK, gbc2 );

        JLabel labelSmoothness = new JLabel( " Smoothness (0.5 - 2.4) " );
        labelSmoothness.setForeground( Color.black );
        labelSmoothness.setFont( serif12 );
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        parametersPanel.add( labelSmoothness, gbc2 );

        textSmoothness = new JTextField( 5 );
        textSmoothness.setText( "1.7" );
        textSmoothness.setFont( serif12 );
        gbc2.gridx = 1;
        gbc2.gridy = 1;
        parametersPanel.add( textSmoothness, gbc2 );

        do25DCheckBox = new JCheckBox( " Slice by slice processing" );
        do25DCheckBox.setFont( serif12 );
        do25DCheckBox.setForeground( Color.black );
        do25DCheckBox.setSelected( true );
        do25DCheckBox.setEnabled( false );
        do25DCheckBox.addActionListener( this );
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        parametersPanel.add( do25DCheckBox, gbc2 );

        JPanel mainPanel = new JPanel( new GridBagLayout() );
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add( scalePanel, gbc );
        gbc.gridy = 1;
        mainPanel.add( imageVOIPanel, gbc );
        gbc.gridy = 2;
        mainPanel.add( iterationsPanel, gbc );
        gbc.gridy = 3;
        mainPanel.add( parametersPanel, gbc );
        mainPanel.setBorder( BorderFactory.createEmptyBorder( 5, 5, 5, 5 ) );

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add( OKButton );
        buildCancelButton();
        buttonPanel.add( cancelButton );

        mainDialogPanel.add( mainPanel );
        mainDialogPanel.add( buttonPanel, BorderLayout.SOUTH );

        getContentPane().add( mainDialogPanel );

        pack();
        setVisible( true );
    }

    /**
     *   When the OK button is pressed, sets variables and calls the
     *   algorithm.  When the cancel button is pressed, closes the
     *   dialog.
     *   @param event    Event that triggers function
     */
    public void actionPerformed( ActionEvent event ) {
        Object source = event.getSource();
        ;
        propagationFlag = false;
        String tmpStr;
        int i, j;

        if ( ( source == propagate ) || ( source == singleSlice ) ) {
            if ( singleSlice.isSelected() ) {
                do25DCheckBox.setEnabled( false );
                do25DCheckBox.setSelected( true );
                labelGaussZ.setEnabled( false );
                textGaussZ.setEnabled( false );
            } else { // propagate.isSelected()
                do25DCheckBox.setEnabled( true );
            }
        } // if ((source == propagate) || (source == singleSlice))
        else if ( source == do25DCheckBox ) {
            if ( do25DCheckBox.isSelected() ) {
                labelGaussZ.setEnabled( false );
                textGaussZ.setEnabled( false );
            } else {
                labelGaussZ.setEnabled( true );
                textGaussZ.setEnabled( true );
            }
        } // else if (source == do25DCheckBox)
        else if ( source == OKButton ) {

            if ( singleSlice.isSelected() ) {
                propagationFlag = false;
            } else if ( propagate.isSelected() ) {
                propagationFlag = true;
            }

            removeOriginal = removeOriginalCheckBox.isSelected();

            do25D = do25DCheckBox.isSelected();

            tmpStr = textGaussX.getText();
            if ( testParameter( tmpStr, 0.0, 5.0 ) ) {
                scaleX = Float.valueOf( tmpStr ).floatValue();
            } else {
                textGaussX.requestFocus();
                textGaussX.selectAll();
                return;
            }

            tmpStr = textGaussY.getText();
            if ( testParameter( tmpStr, 0.0, 5.0 ) ) {
                scaleY = Float.valueOf( tmpStr ).floatValue();
            } else {
                textGaussY.requestFocus();
                textGaussY.selectAll();
                return;
            }

            if ( !do25D ) {
                tmpStr = textGaussZ.getText();
                if ( testParameter( tmpStr, 0.0, 5.0 ) ) {
                    scaleZ = Float.valueOf( tmpStr ).floatValue();
                } else {
                    textGaussZ.requestFocus();
                    textGaussZ.selectAll();
                    return;
                }
            } // if (!do25D)

            tmpStr = textGVFIterations.getText();
            if ( testParameter( tmpStr, 1.0, 1000000.0 ) ) {
                gvfIterations = Integer.valueOf( tmpStr ).intValue();
            } else {
                textGVFIterations.requestFocus();
                textGVFIterations.selectAll();
                return;
            }

            tmpStr = textBoundaryIterations.getText();
            if ( testParameter( tmpStr, 1.0, 1000000.0 ) ) {
                boundaryIterations = Integer.valueOf( tmpStr ).intValue();
            } else {
                textBoundaryIterations.requestFocus();
                textBoundaryIterations.selectAll();
                return;
            }

            tmpStr = textK.getText();
            if ( testParameter( tmpStr, 0.01, 0.5 ) ) {
                kValue = Float.valueOf( tmpStr ).floatValue();
            } else {
                textK.requestFocus();
                textK.selectAll();
                return;
            }

            tmpStr = textSmoothness.getText();
            if ( testParameter( tmpStr, 0.5, 2.4 ) ) {
                smoothness = Float.valueOf( tmpStr ).floatValue();
            } else {
                textSmoothness.requestFocus();
                textSmoothness.selectAll();
                return;
            }

            if ( image.getNDims() == 2 ) { // source image is 2D

                sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {
                    if ( checkboxDisplay.isSelected() == true ) {
                        resultImage = new ModelImage( ModelStorageBase.FLOAT, image.getExtents(),
                                image.getImageName() + "_gvf", userInterface );
                        // Make the algorithm class
                        agvfAlgo = new AlgorithmAGVF( resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                kValue, smoothness, srcVOI, do25D );
                    } else {
                        // Make the algorithm class
                        agvfAlgo = new AlgorithmAGVF( null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                smoothness, srcVOI, do25D );
                    }
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    agvfAlgo.addListener( this );
                    // Hide the dialog since the algorithm is about to run.
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];
                    for ( i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        ( (ViewJFrameBase) parentFrame ).getUserInterface().unregisterFrame
                                ( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface.
                        if ( agvfAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        agvfAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            agvfAlgo.setProgressBarVisible( false );
                        }
                        agvfAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    if ( resultImage != null ) {
                        resultImage.disposeLocal();
                        resultImage = null;
                    }
                    MipavUtil.displayError( "Dialog GVF: unable to allocate enough memory" );
                    return;
                }
            } else if ( image.getNDims() == 3 ) {
                int[] extents = null;
                if ( do25D ) {
                    sigmas = new float[2];
                    sigmas[0] = scaleX;
                    sigmas[1] = scaleY;
                } else {
                    sigmas = new float[3];
                    sigmas[0] = scaleX;
                    sigmas[1] = scaleY;
                    sigmas[2] = scaleZ;
                }

                ///if (propagationFlag == false) {
                //    extents = new int[2];
                //}
                //else {
                extents = new int[3];

                extents[0] = image.getExtents()[0];
                extents[1] = image.getExtents()[1];
                extents[2] = image.getExtents()[2];

                try {
                    if ( checkboxDisplay.isSelected() == true ) {
                        resultImage = new ModelImage( ModelStorageBase.FLOAT, extents, image.getImageName() + "_gvf",
                                userInterface );
                        // Make algorithm
                        agvfAlgo = new AlgorithmAGVF( resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                kValue, smoothness, srcVOI, do25D );
                    } else {
                        // Make algorithm
                        agvfAlgo = new AlgorithmAGVF( null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                smoothness, srcVOI, do25D );
                    }
                    agvfAlgo.setPropagation( propagationFlag );

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    agvfAlgo.addListener( this );
                    // Hide dialog
                    setVisible( false );

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];
                    for ( i = 0; i < imageFrames.size(); i++ ) {
                        titles[i] = ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).getTitle();
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( "Locked: " + titles[i] );
                        ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                        ( (ViewJFrameBase) parentFrame ).getUserInterface().unregisterFrame
                                ( (Frame) ( imageFrames.elementAt( i ) ) );
                    }

                    if ( runInSeparateThread ) {
                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if ( agvfAlgo.startMethod( Thread.MIN_PRIORITY ) == false ) {
                            MipavUtil.displayError( "A thread is already running on this object" );
                        }
                    } else {
                        agvfAlgo.setActiveImage( isActiveImage );
                        if ( !userInterface.isAppFrameVisible() ) {
                            agvfAlgo.setProgressBarVisible( false );
                        }
                        agvfAlgo.run();
                    }
                } catch ( OutOfMemoryError x ) {
                    MipavUtil.displayError( "Dialog AGVF: unable to allocate enough memory" );
                    return;
                }
            }
        } else if ( source == cancelButton ) {
            dispose();
        }
    }

    //************************************************************************
    //************************** Algorithm Events ****************************
    //************************************************************************

    /**
     *   This method is required if the AlgorithmPerformed
     *   interface is implemented. It is called by the
     *   algorithm when it has completed or failed to
     *   to complete, so that the dialog can be display
     *   the result image and/or clean up.
     *   @param algorithm   Algorithm that caused the event.
     */
    public void algorithmPerformed( AlgorithmBase algorithm ) {
        int slice;
        int element;
        Vector[] contours;
        int sliceNum;
        int nContours;
        VOI resultVOI;

        if ( algorithm instanceof AlgorithmAGVF ) {
            if ( agvfAlgo.isCompleted() == true ) {
                //The algorithm has completed and produced a new image to be displayed.
                resultVOI = agvfAlgo.getResultVOI();
                if ( removeOriginal ) {
                    resultVOI.setColor( voiColor );
                    image.getVOIs().removeElementAt( groupNum );
                }
                image.registerVOI( resultVOI );

                if ( resultImage != null ) {
                    updateFileInfo( image, resultImage );
                    resultImage.clearMask();
                    try {
                        new ViewJFrameImage( resultImage, null, new Dimension( 610, 200 ) );
                    } catch ( OutOfMemoryError error ) {
                        System.gc();
                        JOptionPane.showMessageDialog( null, "Out of memory: unable to open new frame", "Error",
                                JOptionPane.ERROR_MESSAGE );
                    }
                } // if (resultImage != null)
            } // if (agvfAlgo.isCompleted() == true)
            else if ( ( agvfAlgo.isCompleted() == false ) && ( resultImage != null ) ) {
                resultImage.disposeLocal();
                resultImage = null;
                System.gc();
            }

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector imageFrames = image.getImageFrameVector();
            for ( int i = 0; i < imageFrames.size(); i++ ) {
                ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setTitle( titles[i] );
                ( (ViewJFrameBase) ( imageFrames.elementAt( i ) ) ).setEnabled( true );
                ( (ViewJFrameBase) parentFrame ).getUserInterface().registerFrame
                        ( (Frame) ( imageFrames.elementAt( i ) ) );
            }
        }

        // Update frame
        ( (ViewJFrameBase) parentFrame ).updateImages( true );
        dispose();
    }
}
