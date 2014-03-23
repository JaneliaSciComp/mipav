package gov.nih.mipav.view.renderer.WildMagic.Interface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionSaveAllVOIs;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

public class JDialogLattice extends JDialogBase {

	private ModelImage image;    
	private JTextField[][] pairFields;
	private JPanel okCancelPanel;    
	private VOI lattice = null;
	private int numPoints = 0;
	private int numPairs = 0;
	private Vector3f[] left_right_markers;
	private JButton saveButton;
	
	public JDialogLattice( ModelImage image )
	{
		this.image = image;
		init();
		setVisible(true);
	}

    public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		Object source = event.getSource();

		if (command.equals("OK")) {
			setVariables();
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if (command.equals("Cancel")) {
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if ( source == saveButton ) {
			saveLattice();
		} else {
			setVariables();
		}
    }
    
    private void saveLattice()
    {
        final JFileChooser chooser = new JFileChooser();

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        final int returnVal = chooser.showSaveDialog(null);

        String fileName = null, directory = null, voiDir;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }

        if (fileName != null) {
            voiDir = new String(directory + fileName + File.separator);                          
            
            VOIVector backUpVOIs = image.getVOIsCopy();

			image.resetVOIs();
			image.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( lattice.getCurves().elementAt(0).elementAt(j) );
				mainAxis.add( lattice.getCurves().elementAt(1).elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
			
			saveAllVOIsTo( voiDir, image );    
            
			image.restoreVOIs(backUpVOIs);
        }

    }
    
    private void saveAllVOIsTo(final String voiDir, ModelImage image) {
        try {
            ViewVOIVector VOIs = image.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                	FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
                	fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                }
                else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(),true);             	
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()
    
    
    private void setVariables()
    {
    	int count = 0;
    	int[][] pairs = new int[numPairs][2];
    	for ( int i = 0; i < numPairs; i++ )
    	{
    		pairs[i][0] = -1;
    		pairs[i][1] = -1;
    		if ( (pairFields[i][0].getText().length() > 0) && (pairFields[i][1].getText().length() > 0) )
    		{
    			if ( testParameter( pairFields[i][0].getText(), 0, numPoints ) );
    			{
    				pairs[i][0] = Integer.valueOf(pairFields[i][0].getText()).intValue();
    				count++;
    			}
    			if ( testParameter( pairFields[i][1].getText(), 0, numPoints ) );
    			{
    				pairs[i][1] = Integer.valueOf(pairFields[i][1].getText()).intValue();
    				count++;
    			}
    		}
    	}    	
    	if ( count > 0 )
    	{
    		VOIVector VOIs = image.getVOIs();
//    		VOIVector VOIs = image.getVOIsCopy();
//    		for ( int i = VOIs.size()-1; i >= 0; i-- )
//    		{
//    			VOI currentVOI = VOIs.elementAt(i);
//    			if ( currentVOI.getCurveType() == VOI.POLYLINE )
//    			{
//    				VOIs.remove(currentVOI);
//    			}
//    		}
    		
    		
    		if ( lattice == null )
    		{
    			short id = (short) image.getVOIs().getUniqueID();
    			lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
    			VOIContour leftSide = new VOIContour( false );
    			VOIContour rightSide = new VOIContour( false );
    			lattice.getCurves().add(leftSide);		
    			lattice.getCurves().add(rightSide);
    		}
    		VOIContour leftSide = (VOIContour) lattice.getCurves().elementAt(0);
    		VOIContour rightSide = (VOIContour) lattice.getCurves().elementAt(1);
    		leftSide.clear();
    		rightSide.clear();
    		for ( int i = 0; i < numPairs; i++ )
    		{
    			if ( (pairs[i][0] == -1) || (pairs[i][1] == -1) )
    			{
    				break;
    			}
    			
    			leftSide.add( left_right_markers[ pairs[i][0] ] );
    			rightSide.add( left_right_markers[ pairs[i][1] ] );
    		}
    		
    		if ( leftSide.size() > 1 )
    		{
    			image.registerVOI(lattice);
    		}
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < leftSide.size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( leftSide.elementAt(j) );
				mainAxis.add( rightSide.elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
//			parent.restoreVOIs(VOIs);
    	}
    }
	
	private void init()
	{
		setResizable(true);
		setForeground(Color.black);
		setTitle("Build 3D Lattice");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		GuiBuilder gui = new GuiBuilder(this);
		
		numPoints = 0;
		VOIVector VOIs = image.getVOIs();
		for ( int i = 0; i < VOIs.size(); i++ )
		{
			VOI currentVOI = VOIs.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				numPoints++;
			}
		}
		if ( numPoints == 0 )
		{
			return;
		}
		left_right_markers = new Vector3f[numPoints];
		for ( int i = 0; i < VOIs.size(); i++ )
		{
			VOI currentVOI = VOIs.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				VOIText textVOI = (VOIText) currentVOI.getCurves().elementAt(0);
	    		Vector3f pt = textVOI.elementAt(0);
	    		String markerID = textVOI.getText();
	    		int id = Integer.valueOf( markerID.substring( 3, markerID.length() ) );				
				
//				System.err.println( markerID + " " + id );
				if ( (id >= 0) && (id < numPoints) )
				{
					left_right_markers[ id ] = pt;
				}
			}
		}
		
		numPairs = numPoints/2;

		JPanel panel = new JPanel(new GridLayout(numPairs, 3));
		panel.setForeground(Color.black);
		
		
		pairFields = new JTextField[numPairs][2];
		for ( int i = 0; i < numPairs; i++ )
		{
			panel.add( new JLabel( "Enter pair: " ) );
			pairFields[i][0] = new JTextField(5);
			pairFields[i][0].addActionListener(this);
			pairFields[i][0].addFocusListener(this);
			panel.add(pairFields[i][0]);
			pairFields[i][1] = new JTextField(5);
			pairFields[i][1].addFocusListener(this);
			panel.add(pairFields[i][1]);
		}
		saveButton = new JButton("Save Lattice");
		saveButton.addActionListener(this);
		
		getContentPane().add(panel, BorderLayout.NORTH);
		getContentPane().add( saveButton, BorderLayout.CENTER );
		okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

		pack();
		setResizable(true);

		System.gc();

	}
	
    public void focusGained(FocusEvent event) { }

    public void focusLost(FocusEvent event) 
    { 
    	setVariables();
    }
}
