import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;




/**
 * @version  February 22, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           
 */
public class PlugInDialogAVISkip extends JDialogScriptableBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public static final String BROWSE = "BROWSE";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    
    /** Text field for directory of chesire overlay files. */
    private JTextField textName;
    
    /** Button to browse for directory of cheshire overlay files. */
    private JButton browseButton;
    
    /** handle to ViewUserInterface */
    private ViewUserInterface UI;
    
    /** Whether the dialog exited successfully. */
    private boolean successfulExit = false;
    
    /** The AVI skip plugin. */
    private PlugInAVISkip aviSkipPlugin;
    
    private String fileName = null;
    
    private String directory = null;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogAVISkip() { }

    /**
     * Creates new dialog for converting a cheshire overlay file to VOIs.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAVISkip(boolean modal, PlugInAVISkip aviSkipPlugin) {
        super(modal);
        this.aviSkipPlugin = aviSkipPlugin;
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals(BROWSE)) {

            try {
                
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Select AVI file");
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setCurrentDirectory(new File(Preferences.getImageDirectory()));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.AVI));
                chooser.setMultiSelectionEnabled(false);
                
                int returnValue = chooser.showOpenDialog(this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    textName.setText(chooser.getSelectedFile().getAbsolutePath());
                    textName.setToolTipText(null);
                    fileName = chooser.getSelectedFile().getName();
                    directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    Preferences.setImageDirectory(chooser.getCurrentDirectory());
                } else {
                    return;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory");

                return;
            }

        }
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }


    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += textName.getName();    //necessry parameter?

        return str;
    }
    
    public String getFileName() {
        return fileName;
    }
    
    public String getDirectory() {
        return directory;
    }

    /**
     * Accessor that indicates successful completion of dialog
     */
    public boolean isSuccessfulExit() {
        return successfulExit;
    }
    

    
    /**
     * Once all the necessary variables are set, call the cheshire to voi plugin
     */
    protected void callAlgorithm() {

        try {
        
            setVisible(false); // Hide dialog
            successfulExit = true;
            aviSkipPlugin.runPlugin();    //continues execution of plugin with successful exit
        
        } catch (OutOfMemoryError x) {
            
            MipavUtil.displayError("PlugInDialogAVISkip: unable to allocate enough memory");
            successfulExit = false;
        }

    } // end callAlgorithm()
    

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {

        //setFileName(scriptParameters.getParams().getString("file_name"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {


        //scriptParameters.getParams().put(ParameterFactory.newParameter("file_name", fileName));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("AVI skipped frame file");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Select AVI file"));

        JLabel labelType = new JLabel("AVI file");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(30);
        textName.setText("AVI file name");
        textName.setFont(serif12);
        textName.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.setActionCommand(BROWSE);
        browseButton.addActionListener(this);
        
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        mainPanel.add(browseButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(textName, gbc);

        
        getContentPane().add(mainPanel, BorderLayout.CENTER);

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

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        if (fileName == null) {
            return false;
        }
        
        if (directory == null) {
            return false;
        }

        return true;
    } // end setVariables()

}
