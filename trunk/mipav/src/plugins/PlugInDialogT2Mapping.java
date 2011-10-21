import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.*;

/**
 * @version  June 4, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */
public class PlugInDialogT2Mapping extends JDialogScriptableBase implements AlgorithmInterface
{
                                                             
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    ArrayList<JTextField> txtField = new ArrayList<JTextField>();
    
    
    
    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    /** DOCUMENT ME! */
    private AlgorithmBase genericAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogT2Mapping() 
    {
    	
    }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogT2Mapping(Frame theParentFrame, ModelImage im)
    {
        super(theParentFrame, false);
        image = im;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        if (command.equals("OK"))
        {
            //if (setVariables()) {
                callAlgorithm();
            //}
                
                
                
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm)
    {

        if (algorithm instanceof AlgorithmBase)
        {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((genericAlgo.isCompleted() == true) && (resultImage != null)) 
            {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try 
                {
                    new ViewJFrameImage(resultImage);
                } 
                catch (OutOfMemoryError error) 
                {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } 
            else if (resultImage != null)
            {
                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            if (algorithm.isCompleted()) 
            {
                insertScriptLine();
            }

            if (algorithm != null) 
            {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() 
    {
        try 
        {
        	double[] timeVals = new double[txtField.size()];
        	for(int i = 0; i<timeVals.length; i++)
        	{
        		timeVals[i]= Double.parseDouble(txtField.get(i).getText());
        		
        	}
        	
            String name = makeImageName(image.getImageName(), "_T2 fit");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            genericAlgo = new PlugInAlgorithmT2Mapping(resultImage, image, timeVals);
            //genericAlgo = new PlugInAlgorithmNewGeneric2(resultImage, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            genericAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", genericAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) 
            {
                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (genericAlgo.startMethod(Thread.MIN_PRIORITY) == false)
                {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } 
            else 
            {
                genericAlgo.run();
            }
        } 
        catch (OutOfMemoryError x)
        {
            if (resultImage != null)
            {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }
            MipavUtil.displayError("Kidney segmentation: unable to allocate enough memory");
            return;
        }

    } // end callAlgorithm()

    protected void setGUIFromParams() 
    {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException
    {
    // TODO Auto-generated method stub, no params yet
    }
   
    private void init() 
    {
        setForeground(Color.black);
        setTitle("T2 Mapping of the Brain");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder(""));
        //TODO must be changed when not run from Eclipse
        JLabel labelVOI = new JLabel("Begin the T2 mapping of the Brain?");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        getContentPane().add(mainPanel, BorderLayout.NORTH);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        JPanel mainPanel2 = new JPanel(new GridBagLayout());
    	   
        setForeground(Color.black);
        setTitle("Enter T2 Values");

        mainPanel2 = new JPanel();
        mainPanel2.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel2.setLayout(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridLayout(-1, 2));
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));
        mainPanel2.add(paramPanel);

        int numTimeSlices = image.getExtents()[3];
        int base = 80;
        for (int i = 0; i < numTimeSlices; i++) 
        {
        	JLabel labelK = createLabel("Time " +i);
        	paramPanel.add(labelK);
        	JTextField textK = createTextField(String.valueOf(base+i*20));
        	paramPanel.add(textK);
        	txtField.add(textK);
        }
        
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        mainPanel2.add(paramPanel, gbc2);

        mainDialogPanel.add(mainPanel2, BorderLayout.CENTER);

        getContentPane().add(mainDialogPanel, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
        
    } // end init()   
}
