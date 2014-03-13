
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class PlugInDialogWormSegmentation extends JDialogStandalonePlugin implements AlgorithmInterface {
        
	private static final long serialVersionUID = 2476025001402032629L;
	
	/** This source image is typically set by the constructor */
    private ModelImage wormImage;    
    private JTextField  baseFileLocText;
    private JTextField  baseFileNameText;
    private JTextField rangeFusionText;
    private JPanel okCancelPanel;    
    private String baseFileDir;
    private Vector<Integer> includeRange;

    private JCheckBox leftRight;
    private JCheckBox headSegmentation;
    private JCheckBox lattice;
    private JCheckBox maximumIntensityProjection;

    public PlugInDialogWormSegmentation() {}

    public PlugInDialogWormSegmentation(boolean modal)
    {
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
    		if ( includeRange != null )
    		{
    			for ( int i = 0; i < includeRange.size(); i++ )
    			{
//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
    	    		String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
    	            File voiFile = new File(baseFileDir + File.separator + fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	                FileIO fileIO = new FileIO();
    	                if(wormImage != null) {
    	                	wormImage.disposeLocal();
    	                	wormImage = null;
    	                }
    	                wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
    					PlugInAlgorithmWormSegmentation alg = new PlugInAlgorithmWormSegmentation(wormImage);
    					alg.setOutputLattice( lattice.isSelected() );
    					alg.setOutputLeftRight( leftRight.isSelected() );
    					alg.setOutputHead( headSegmentation.isSelected() );
    					alg.setOutputMaxIntensityProjection( maximumIntensityProjection.isSelected() );
    					alg.run();
    	            }    				
    			}
    		}
    		else
    		{
    			int fileCount = 0;
    			boolean fileExists = true;
    			while ( fileExists )
    			{
    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + fileCount++ + ".tif";
    	            File voiFile = new File(fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	            }    				
    	            else
    	            {
    	            	fileExists = false;
    	            }
    			}
    		}
    	}
        if(wormImage != null) {
        	wormImage.disposeLocal();
        	wormImage = null;
        }
    	setVisible(false);
    }     

	private void init()
    {
    	setResizable(true);
    	setForeground(Color.black);
    	setTitle("Worm Segmentation 1.0");
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
    	panel.setBorder(buildTitledBorder("Input Options"));
    	panel.setForeground(Color.black);

    	baseFileLocText = gui.buildFileField("Directory containing input images: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
    	panel.add(baseFileLocText.getParent(), gbc);
    	gbc.gridy++;

    	baseFileNameText = gui.buildField("Base images name: ", "Decon");
    	panel.add(baseFileNameText.getParent(), gbc);
    	gbc.gridy++;

    	rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
    	panel.add(rangeFusionText.getParent(), gbc);
    	gbc.gridy++;

    	getContentPane().add(panel, BorderLayout.NORTH);

    	

    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	panel = new JPanel(new GridBagLayout());
    	panel.setBorder(buildTitledBorder("Output Options"));
    	panel.setForeground(Color.black);

        leftRight = gui.buildCheckBox("left - right markers", true );
    	panel.add(leftRight.getParent(), gbc);
    	gbc.gridy++;

    	lattice = gui.buildCheckBox("lattice", true );
    	panel.add(lattice.getParent(), gbc);
    	gbc.gridy++;

    	headSegmentation = gui.buildCheckBox("head segmentation (triangle mesh)", false );
    	panel.add(headSegmentation.getParent(), gbc);
    	gbc.gridy++;    	

    	maximumIntensityProjection = gui.buildCheckBox("maximum intensity projection", false );
    	panel.add(maximumIntensityProjection.getParent(), gbc);
    	gbc.gridy++;

    	getContentPane().add(panel, BorderLayout.CENTER);

    	okCancelPanel = gui.buildOKCancelPanel();
    	getContentPane().add(okCancelPanel, BorderLayout.SOUTH);
    	
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
	
}
