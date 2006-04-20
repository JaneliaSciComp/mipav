package gov.nih.mipav.view.srb;

import edu.sdsc.grid.gui.*;
import edu.sdsc.grid.io.srb.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.net.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.StringBuffer;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;

import gov.nih.mipav.view.components.*;

/**
 * This JComponent is used to select file or fold of the SRB server.
 * @author Hailong Wang 04/14/2006
 * @version 1.0
 */

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
    private JSRBTree jsrbTree = null;
    
    private SRBFileSystem fileSystem = null;
    
    private JTextField selectedFileField;
    
    private JButton approveButton;
    
    private JDialog dialog = null;
    
    private String dialogTitle = null;
    
    private String defaultApproveButtonText = "Open";
    
    private String approveButtonText;
    
    private String approveButtonToolTipText = null;
    
    private int approveButtonMnemonic = 0;
 
    private int fileSelectionMode = FILES_ONLY;

    private boolean multiSelectionEnabled = false;

    private int returnValue;
    
    /**
     * The selected files(multi file selection is enabled).
     */
    private SRBFile[] selectedFiles;
    
    /**
     * The selected file.
     */
    private SRBFile selectedFile;
    private String title = "SRB File Chooser";
    
    /***********************
     ***** Constructor *****
     ***********************
     */
    
    public JargonFileChooser(SRBAccount srbAccount) throws IOException{
        this(srbAccount, null);
    }
    
    public JargonFileChooser(SRBAccount srbAccount, String path) throws IOException{
        this(new SRBFileSystem(srbAccount), path);
    }
    
    public JargonFileChooser(SRBFileSystem fs) throws IOException{
        this(fs, null);
    }
    
    public JargonFileChooser(SRBFileSystem fs, String path) throws IOException{
        if (fs != null) {
            fileSystem = fs;
            if(path == null || path.length() == 0){
                path = fs.getHomeDirectory();
            }
            
            if(path == null || path.length() == 0){
                path = "/home";
            }
            SRBFile f = new SRBFile(fileSystem, path);
            if (f != null) {
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
    
    /******************************
     ***** Instance Functions *****
     ******************************
     */

    private SRBFile getRoot(SRBFile f){
        while(!f.getPath().equals("/home"))
            f = (SRBFile)f.getParentFile();
        return f;
    }
    
    /**
     * Initialize the JargonFileChooser.
     * @param f
     * @throws IOException
     */
    private void init(SRBFile f) throws IOException {
        if(f == null)
            return;
        SRBFile root = getRoot(f);
        
        fileSystem = (SRBFileSystem)f.getFileSystem();
        
        jsrbTree = new JSRBTree(root.listFiles());

        jsrbTree.useDefaultPopupMenu(false);
        jsrbTree.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(jsrbTree);
        
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
        approveButton = WidgetFactory.buildTextButton(approveButtonText, "Open srb or local file.", approveButtonText, this);
        approveButton.setPreferredSize(new Dimension(80, 25));
        manager.add(approveButton);
        JPanel topPanel = manager.getPanel();
        topPanel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        this.add(scrollPane, BorderLayout.CENTER);
        this.add(manager.getPanel(), BorderLayout.NORTH);
        jsrbTree.addTreeSelectionListener(this);
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
    
    public int showDialog(Component parent, String approveButtonText){
        if(approveButtonText == null || approveButtonText.length() == 0){
            approveButtonText = defaultApproveButtonText;
        }
        setApproveButtonText(approveButtonText);
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
        if(multiSelectionEnabled != b) {
            multiSelectionEnabled = b;
        }
        if(multiSelectionEnabled){
            jsrbTree.getSelectionModel().setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        }else{
            jsrbTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        }
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
        approveButton.setText(newApproveButtonText);
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
        return selectedFile;
    }
    
    public SRBFile[] getSelectedFiles(){
        return selectedFiles;
    }
    
    /**
     * 
     * @param e
     */
    public void valueChanged(TreeSelectionEvent e){
        Object o = e.getSource();
        if(o instanceof JSRBTree){
            JSRBTree tree = (JSRBTree)o;
            TreePath[] selectedPaths = tree.getSelectionPaths();
            if(selectedPaths == null || selectedPaths.length == 0){
                return;
            }
            StringBuffer sb = new StringBuffer("");
            if(isMultiSelectionEnabled()){
                selectedFileField.setText("");
                for(int i = 0; i < selectedPaths.length; i++){
                    convertSelectedPathToString(selectedPaths[i], getFileSelectionMode(), sb);
                }
            }else{
                convertSelectedPathToString(selectedPaths[selectedPaths.length-1], getFileSelectionMode(), sb);
            }
            selectedFileField.setText(sb.toString());
        }
    }
    
    /**
     * The helper function to set the <code>textField</code> to the selected path.
     * @param treePath
     * @param textField
     */
    private StringBuffer convertSelectedPathToString(TreePath treePath, int selectionMode, StringBuffer sb){
        if(treePath == null){
            return null;
        }
        if(sb == null){
            sb = new StringBuffer("");
        }
        Object selectedObj = treePath.getLastPathComponent();
        if(selectedObj instanceof SRBFile){
            SRBFile srbFile = (SRBFile)selectedObj;
            if(selectionMode == FILES_ONLY){
                if(srbFile.isFile()){
                    if(sb.length() == 0){
                        sb.append(srbFile.getPath());
                    }else{
                        sb.append("," + srbFile.getPath());
                    }
                }
            } else if (selectionMode == DIRECTORIES_ONLY){
                if(srbFile.isDirectory()){
                    if(sb.length() == 0){
                        sb.append(srbFile.getPath());
                    }else{
                        sb.append("," + srbFile.getPath());
                    }
                }
            } else {
                if(sb.length() == 0){
                    sb.append(srbFile.getPath());
                }else{
                    sb.append("," + srbFile.getPath());
                }
            }
        }
        return sb;
    }

    public void actionPerformed(ActionEvent e){
        String actionCommand = e.getActionCommand();
        if(actionCommand.equals(approveButtonText)){
            returnValue = APPROVE_OPTION;
            
            /**
             * Sets the selected files/file.
             */
            String fileName = selectedFileField.getText();
            if(isMultiSelectionEnabled()){
                if(fileName.indexOf(",") >= 0){
                    String[]fileNames = fileName.split(",");
                    if(fileNames == null || fileNames.length == 0){
                        return;
                    }
                    selectedFiles = new SRBFile[fileNames.length];
                    for(int i = 0; i < fileNames.length; i++){
                        if(fileNames[i].length() > 0 && fileSystem != null){
                            selectedFiles[i] = new SRBFile(fileSystem, fileNames[i]);
                        }
                    }
                }else{
                     if(fileName.length() > 0 && fileSystem != null){
                        selectedFile = new SRBFile(fileSystem, fileName);
                        selectedFiles = new SRBFile[1];
                        selectedFiles[0] = selectedFile;
                    }
                }
            }else{
                if(fileName.length() > 0 && fileSystem != null){
                    selectedFile = new SRBFile(fileSystem, fileName);
                    selectedFiles = new SRBFile[1];
                    selectedFiles[0] = selectedFile;
                }
            }
            
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
