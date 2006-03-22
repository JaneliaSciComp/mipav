package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.media.*;
import javax.media.format.*;
import com.sun.media.codec.video.vcm.*;

import gov.nih.mipav.model.algorithms.AlgorithmTranscode;
import gov.nih.mipav.model.structures.ModelStorageBase;


/**
 * Confirmation Dialog giving user the choice to write an
 * analyze file or a nifti file.
 * @author not attributable
 * @version 1.0
 */
public class JDialogAnalyzeNIFTIChoice extends JDialogBase {

    
    private ButtonGroup writeGroup;
    private JRadioButton analyzeFile;
    private JRadioButton niftiFile;

    private boolean okayPressed = false;

    /**
     *   Creates new dialog.
     *   @param theParentFrame   Parent frame of dialog.
     */
    public JDialogAnalyzeNIFTIChoice( Frame theParentFrame ) {
        super( theParentFrame, true );
        init();
    }

    /**
     * Creates and displays dialog
     */
    private void init( ) {
        setTitle( "Choose type of file to write" );

        JPanel createPanel = new JPanel( new GridBagLayout() );
        createPanel.setBorder( buildTitledBorder( "Write file as" ) );
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets( 0, 20, 0, 0 );
        
        writeGroup = new ButtonGroup();
        analyzeFile = new JRadioButton("Analyze file", true);
        analyzeFile.setFont(serif12);
        analyzeFile.setForeground(Color.black);
        writeGroup.add(analyzeFile);
        analyzeFile.setEnabled(true);
        createPanel.add( analyzeFile, gbc );
        
        gbc.gridy = 1;
        niftiFile = new JRadioButton("NIFTI file", false);
        niftiFile.setFont(serif12);
        niftiFile.setForeground(Color.black);
        writeGroup.add(niftiFile);
        niftiFile.setEnabled(true);
        createPanel.add(niftiFile, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add( OKButton );

        mainDialogPanel.add( createPanel );
        mainDialogPanel.add( buttonPanel, BorderLayout.SOUTH );

        getContentPane().add( mainDialogPanel );

        pack();
        setVisible( true );
    }
    
    public boolean isAnalyzeFile() {
        if (analyzeFile.isSelected()) {
            return true;
        }
        else {
            return false;
        }
    }

    
    /**
     * Was the okay button pressed
     * @return boolean was okay pressed
     */
    public boolean okayPressed() {
        return okayPressed;
    }

    /**
     *   Checks to see if the OK or Cancel buttons were pressed
     *   @param event    Event that triggered this function.
     */
    public void actionPerformed( ActionEvent event ) {
        if ( event.getSource() == OKButton ) {
            okayPressed = true;
        }
        dispose();
    }
}
