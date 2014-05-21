package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;


public class JDialog4DVOI extends JDialogBase
{

	private static final long serialVersionUID = 2476025001402032629L;

	/** This source image is typically set by the constructor */
	private VolumeTriPlanarInterface parent;
	private ModelImage image;    
	private JTextField  baseFileLocText;
	private JTextField  baseFileNameText;
	private JTextField rangeFusionText;
	private JPanel okCancelPanel;    
	private String baseFileDir;
	Vector<Integer> includeRange;


	public JDialog4DVOI( VolumeTriPlanarInterface parent, ModelImage image )
	{
		this.parent = parent;
		this.image = image;
		init();
		setVisible(true);
	}

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 *
	 * @param  event  Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();

		if (command.equals("OK")) {
			if (setVariables()) {
				callAlgorithm();
			}
		} else if (command.equals("Script")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else {
			super.actionPerformed(event);
		}
	} 

	/**
	 * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
	 * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
	 *
	 * @param  algorithm  Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) { } 

	public void itemStateChanged(final ItemEvent event)
	{    
	}


	/**
	 * Once all the necessary variables are set, call the kidney segmentation algorithm
	 */
	protected void callAlgorithm()
	{
		if ( setVariables() )
		{
			Vector<VOIVector> vois = new Vector<VOIVector>();
			if ( includeRange != null )
			{
				for ( int i = 0; i < includeRange.size(); i++ )
				{
			        VOIVector voiVector = new VOIVector();
			        

					int fileCount = 0;
					boolean fileExists = true;
					while ( fileExists )
					{
						String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "headVOIs_" + fileCount++ + File.separator;
						File voiFile = new File(baseFileDir + File.separator + fileName);
						if ( voiFile.exists() )
						{
//							System.err.println( fileName );

							loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
						}    				
						else
						{
							fileExists = false;
						}
					}
			        
			        
			        
					String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "headVOIs" + File.separator;
					File voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}    				

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "left_right_markers" + File.separator;
					voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}


					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "lattice" + File.separator;
					voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}

					
					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "headLines" + File.separator;
					voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}								

					
//					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "head_meshes" + File.separator;
//					voiFile = new File(baseFileDir + File.separator + fileName);
//					if ( voiFile.exists() )
//					{
//						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
//					}								
					
					
					
					vois.add( voiVector );
				}
			}
			else
			{
				int fileCount = 0;
				boolean fileExists = true;
				while ( fileExists )
				{
			        VOIVector voiVector = new VOIVector();
					String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(fileCount) + File.separator + "headVOIs" + File.separator;
					File voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}    				
					else
					{
						fileExists = false;
					}
					

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(fileCount) + File.separator + "pharynxVOIs" + File.separator;
					voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}
					

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(fileCount) + File.separator + "lattice" + File.separator;
					voiFile = new File(baseFileDir + File.separator + fileName);
					if ( voiFile.exists() )
					{
//						System.err.println( fileName );

						loadAllVOIsFrom( image, voiVector, baseFileDir + File.separator + fileName, true);
					}
					vois.add( voiVector );
					fileCount++;
				}
			}
			if ( vois.size() > 0 )
			{
				parent.addVOIS(vois);
			}
		}
		setVisible(false);
	}     

	private void init()
	{
		setResizable(true);
		setForeground(Color.black);
		setTitle("Open 4D VOI");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		GuiBuilder gui = new GuiBuilder(this);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		JPanel panel = new JPanel(new GridBagLayout());
		panel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Directory containing input images: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		panel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		panel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to fuse (ex. 3-7, 12, 18-21, etc.): ", " ");
		panel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		okCancelPanel = gui.buildOKCancelPanel();
		panel.add(okCancelPanel, gbc);


		getContentPane().add(panel, BorderLayout.CENTER);

		pack();
		setResizable(true);

		System.gc();

	}

	/**
	 * This method could ensure everything in your dialog box has been set correctly
	 * 
	 * @return
	 */
	private boolean setVariables()
	{	    
		baseFileDir = baseFileLocText.getText();
		includeRange = new Vector<Integer>();
		String rangeFusion = rangeFusionText.getText();
		if(rangeFusion != null) {  
			String[] ranges = rangeFusion.split("[,;]");
			for(int i=0; i<ranges.length; i++) {
				String[] subset = ranges[i].split("-");
				int lowerBound = -1, bound = -1;
				for(int j=0; j<subset.length; j++) {
					try {
						bound = Integer.valueOf(subset[j].trim());
						if(lowerBound == -1) {
							lowerBound = bound;
							includeRange.add(lowerBound);
						} 
					} catch(NumberFormatException e) {
						Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
					}
				}

				for(int k=lowerBound+1; k<=bound; k++) {
					includeRange.add(k);
				}
			}
		}

		if(includeRange.size() == 0) {
			includeRange = null;
		}

		return true;
	}
	
    /**
     * This method loads all VOIs to the active image from a given directory.
     * @param voiDir the directory to load voi's from
     * @param quietMode if true indicates that warnings should not be displayed.
     */
    private void loadAllVOIsFrom( ModelImage image, VOIVector voiVector, final String voiDir, boolean quietMode) {

        int i, j;
        VOI[] VOIs;
        FileVOI fileVOI;
        try {

            // if voiDir does not exist, then return
            // if voiDir exists, then get list of voi's from directory (*.voi)
            final File voiFileDir = new File(voiDir);
            final Vector<String> filenames = new Vector<String>();
            final Vector<Boolean> isLabel = new Vector<Boolean>();

            if (voiFileDir.exists() && voiFileDir.isDirectory()) {

                // get list of files
                final File[] files = voiFileDir.listFiles();

                for (final File element : files) {

                    if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
                        filenames.add(element.getName());
                        isLabel.add(false);
                    } else if (element.getName().endsWith(".lbl")) {
                        filenames.add(element.getName());
                        isLabel.add(true);
                    }
                }
            } else { // voiFileDir either doesn't exist, or isn't a directory

                if ( !quietMode) {
                    MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
                }

                return;
            }

            // open each voi array, then register voi array to this image
            for (i = 0; i < filenames.size(); i++) {

                fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, image);

                VOIs = fileVOI.readVOI(isLabel.get(i));
                for (j = 0; j < VOIs.length; j++) {
                	voiVector.add(VOIs[j]);
                }
            }

        } catch (final Exception error) {

            if ( !quietMode) {
                MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
            }
        }
        return;
    }

}
