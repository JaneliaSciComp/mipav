import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import nibib.spim.PlugInAlgorithmGenerateFusion;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;


/**
 
 */
public class PlugInDialogTSPAnalysis extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image;
	
	private PlugInAlgorithmTSPAnalysis TSPAnalysisAlgo = null;
	
	private JTextField pwiImageFileDirectoryText;
	
	private String pwiImageFileDirectory;
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("TSP Analysis");
        
        final GuiBuilder gui = new GuiBuilder(this);

        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));
        
        pwiImageFileDirectoryText = gui.buildFileField("Directory containing pwi image file: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(pwiImageFileDirectoryText.getParent(), gbc);
        
        getContentPane().add(inputPanel, BorderLayout.NORTH);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmTSPAnalysis) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());

            if ( (TSPAnalysisAlgo.isCompleted() == true)) {
                final Collection<ModelImage> list = TSPAnalysisAlgo.getResultImageList();
                synchronized (list) {
                    final Iterator<ModelImage> itr = list.iterator();
                    while (itr.hasNext()) {
                        new ViewJFrameImage(itr.next());
                    }
                }
                insertScriptLine();
            }

            if (TSPAnalysisAlgo != null) {
                TSPAnalysisAlgo.finalize();
                TSPAnalysisAlgo = null;
            }

            // dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    @Override
    protected void callAlgorithm() {

        try {

            TSPAnalysisAlgo = new PlugInAlgorithmTSPAnalysis(resultImage, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            TSPAnalysisAlgo.addListener(this);
            // createProgressBar("Creating plugin", " ...", generateFusionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (TSPAnalysisAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                TSPAnalysisAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {
    	
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	
    }
    
    private boolean setVariables() {
    	pwiImageFileDirectory = pwiImageFileDirectoryText.getText();
    	return true;
    }

}
