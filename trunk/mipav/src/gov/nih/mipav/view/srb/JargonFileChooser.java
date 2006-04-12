package gov.nih.mipav.view.srb;

import edu.sdsc.grid.gui.*;
import edu.sdsc.grid.io.srb.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.net.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;

import gov.nih.mipav.view.components.*;


public class JargonFileChooser extends JComponent implements TreeSelectionListener, ActionListener{
    /** Instruction to display only files. */
    public static final int FILES_ONLY = 0;

    /** Instruction to display only directories. */
    public static final int DIRECTORIES_ONLY = 1;

    /** Instruction to display both files and directories. */
    public static final int FILES_AND_DIRECTORIES = 2;

    /**
     * Return value if cancel is chosen.
     */
    public static final int CANCEL_OPTION = 1;

    /**
     * Return value if approve (yes, ok) is chosen.
     */
    public static final int APPROVE_OPTION = 0;

    /**
     * Return value if an error occured.
     */
    public static final int ERROR_OPTION = -1;

    /******************************
     ***** Instance Variables *****
     ******************************
     */
    private JargonTree jargonTree = null;
    
    private SRBFileSystem fileSystem = null;
    
    private JTextField selectedFileField;
    
    private JDialog dialog = null;
    
    private String dialogTitle = null;
    
    private String approveButtonText = "Open";
    
    private String approveButtonToolTipText = null;
    
    private int approveButtonMnemonic = 0;
 
    private int fileSelectionMode = FILES_ONLY;

    private boolean multiSelectionEnabled = false;

    private int returnValue;
    private String title = "Jargon File Chooser";
    
    /******************************
     ***** Instance Functions *****
     ******************************
     */
    
    public JargonFileChooser(SRBAccount srbAccount) throws IOException{
        if(srbAccount != null){
            SRBFileSystem fs = new SRBFileSystem(srbAccount);
            SRBFile f = new SRBFile(fs, fs.getHomeDirectory());
            if(f != null){
                init(f);
            }
        }
    }
    public JargonFileChooser(SRBAccount srbAccount, String filePath) throws IOException{
        if(srbAccount != null){
            SRBFileSystem fs = new SRBFileSystem(srbAccount);
            SRBFile f = new SRBFile(fs, filePath);
            if(f != null){
                init(f);
            }
        }
    }
    
    public JargonFileChooser(URI uri) throws IOException{
        if(uri != null){
            SRBFile f = new SRBFile(uri);
           
            System.out.print("Get back from the SRBFile constructor.");
            if(f != null){
                init(f);
            }
        }
    }
    
    private SRBFile getRoot(SRBFile f){
        while(!f.getPath().equals("/home"))
            f = (SRBFile)f.getParentFile();
        return f;
    }
    private void init(SRBFile f) throws IOException {
        if(f == null)
            return;
        SRBFile root = getRoot(f);
        
        fileSystem = (SRBFileSystem)f.getFileSystem();
        
        jargonTree = new JargonTree(root.listFiles());
        jargonTree.useDefaultPopupMenu(true);
        jargonTree.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(jargonTree);
        
        scrollPane.setPreferredSize(new Dimension(600, 400));
        
        scrollPane.setMinimumSize(new Dimension(600, 400));
        
        this.setLayout(new BorderLayout(0, 11));
        
        PanelManager manager = new PanelManager();
        GridBagConstraints constraints = manager.getConstraints();
        manager.getConstraints().insets = new Insets(10,3,0,3);
        JLabel selectedFileLabel = WidgetFactory.buildLabel("Selected File");
        selectedFileLabel.setPreferredSize(new Dimension(40, 25));
        //selectedFileLabel.setFont(new Font("serif", Font.PLAIN, 14));
        manager.add(selectedFileLabel);
        selectedFileField = WidgetFactory.buildTextField("");
        selectedFileField.setPreferredSize(new Dimension(20, 25));
        selectedFileField.setColumns(40);
        manager.add(selectedFileField);
        JButton approveButton = WidgetFactory.buildTextButton(approveButtonText, "Open srb or local file.", approveButtonText, this);
        approveButton.setPreferredSize(new Dimension(80, 25));
        manager.add(approveButton);
        JPanel topPanel = manager.getPanel();
        topPanel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        this.add(scrollPane, BorderLayout.CENTER);
        this.add(manager.getPanel(), BorderLayout.NORTH);
        jargonTree.addTreeSelectionListener(this);
    }
    
    protected JDialog createDialog(Component parent) throws HeadlessException {
        JDialog dialog = null;
        if(parent instanceof Frame){
            dialog = new JDialog((Frame)parent, title, true);
        } else if(parent instanceof Dialog) {
            dialog = new JDialog((Dialog)parent, title, true);
        }
        dialog.setComponentOrientation(this.getComponentOrientation());

        Container contentPane = dialog.getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(this, BorderLayout.CENTER);
 
        if (JDialog.isDefaultLookAndFeelDecorated()) {
            boolean supportsWindowDecorations = 
            UIManager.getLookAndFeel().getSupportsWindowDecorations();
            if (supportsWindowDecorations) {
                dialog.getRootPane().setWindowDecorationStyle(JRootPane.FILE_CHOOSER_DIALOG);
            }
        }
        dialog.pack();
        dialog.setLocationRelativeTo(parent);

        return dialog;
    }
    
    public int showDialog(Component parent, String aprroveButtonText){
        if(approveButtonText != null){
            setApproveButtonText(approveButtonText);
            // setDialogType(CUSTOM_DIALOG); ?
        }
        dialog = createDialog(parent);
        dialog.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
            returnValue = CANCEL_OPTION;
            }
        });
        returnValue = ERROR_OPTION;
 
        dialog.setVisible(true);
        dialog.dispose();
        dialog = null;
        return returnValue;
    }
    
    public String getDialogTitle(){
        return dialogTitle;
    }
    
    /**
     * Sets the title of the file chooser. 
     * @param newDialogTitle 
     */
    public void setDialogTitle(String newDialogTitle){
        if(getDialogTitle().equals(newDialogTitle))
            return;
        this.dialogTitle = newDialogTitle;
        dialog.setTitle(this.dialogTitle);
        dialog.validate();
    }

    /**
     * Sets the file chooser to allow multiple file selections.
     * @param b true if multiple files may be selected.
     */
    public void setMultiSelectionEnabled(boolean b) {
        if(multiSelectionEnabled == b) {
            return;
        }
        multiSelectionEnabled = b;
    }

    /**
     * Returns true if multiple files can be selected.
     * 
     * @return true if multiple files can be selected
     * @see #setMultiSelectionEnabled
     */
    public boolean isMultiSelectionEnabled() {
        return multiSelectionEnabled;
    }
    
    public int getFileSelectionMode(){
        return fileSelectionMode;
    }
    
    public void setFileSelectionMode(int mode){
        if(fileSelectionMode == mode) {
            return;
        }

       if ((mode == FILES_ONLY) || (mode == DIRECTORIES_ONLY) || (mode == FILES_AND_DIRECTORIES)) {
           int oldValue = fileSelectionMode;
           fileSelectionMode = mode;
       } else {
           throw new IllegalArgumentException("Incorrect Mode for file selection: " + mode);
       }
    }
    
    public String getApproveButtonText(){
        return approveButtonText;
    }
    
    public void setApproveButtonText(String newApproveButtonText){
        this.approveButtonText = newApproveButtonText;
    }
    
    public String getApproveButtonToolTipText(){
        return approveButtonToolTipText;
    }
    
    public void setApproveButtonToolTipText(String toolTipText){
        if(approveButtonToolTipText.equals(toolTipText))
            return;
        this.approveButtonToolTipText = toolTipText;
    }
    
    public SRBFile getSelectedFile(){
        String fileName = selectedFileField.getText();
        if(fileName.length() > 0 && fileSystem != null){
            SRBFile f = new SRBFile(fileSystem, fileName);
            return f;
        }
        return null;
    }
    
    /**
     * 
     * @param e
     */
    public void valueChanged(TreeSelectionEvent e){
        Object o = e.getSource();
        if(o instanceof JargonTree){
            JargonTree tree = (JargonTree)o;
            Object selectedObj = tree.getLastSelectedPathComponent();
            if(selectedObj instanceof SRBFile){
                SRBFile srbFile = (SRBFile)selectedObj;
                if(getFileSelectionMode() == FILES_ONLY){
                    if(srbFile.isFile()){
                        selectedFileField.setText(srbFile.getPath());
                    }
                } else if (getFileSelectionMode() == DIRECTORIES_ONLY){
                    if(srbFile.isDirectory()){
                        selectedFileField.setText(srbFile.getPath());
                    }
                } else {
                    selectedFileField.setText(srbFile.getPath());
                }
            }
        }
    }
    
    public void actionPerformed(ActionEvent e){
        String actionCommand = e.getActionCommand();
        if(actionCommand.equals(approveButtonText)){
            returnValue = APPROVE_OPTION;
            if(dialog != null){
                dialog.setVisible(false);
            }
        }
    }
    public static void main(String[] args){
        try{
            URI uri = new URI("srb://hwang.nih:NIHBIRN@ncmir-gpop.ucsd.edu:5825/home/hwang.nih");
            JargonFileChooser chooser = new JargonFileChooser(uri);
            int ret = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), null);
        }
        catch(Throwable e){
            e.printStackTrace();
        }
    }
    
}
