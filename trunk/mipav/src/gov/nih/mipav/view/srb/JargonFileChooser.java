package gov.nih.mipav.view.srb;

import edu.sdsc.grid.gui.*;
import edu.sdsc.grid.io.srb.*;

import java.net.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;


public class JargonFileChooser extends JComponent implements TreeSelectionListener, ActionListener{
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

    private JargonTree jargonTree = null;
    private SRBFileSystem fileSystem = null;
    private JTextField selectedFileField;
    private JDialog dialog = null;
    private int returnValue;
    private String title = "Jargon File Chooser";
    
    public JargonFileChooser(URI uri) throws IOException{
        if(uri != null){
            SRBFile f = new SRBFile(uri);
           
            System.out.print("Get back from the SRBFile constructor.");
            if(f != null){
                fileSystem = (SRBFileSystem)f.getFileSystem();
                SRBFile parent = (SRBFile)f.getParentFile();
                jargonTree = new JargonTree(parent.listFiles());
                JScrollPane scrollPane = new JScrollPane(jargonTree);
                scrollPane.setPreferredSize(new Dimension(600, 400));
                scrollPane.setMinimumSize(new Dimension(600, 400));
                this.setLayout(new BorderLayout(0, 11));
                JPanel topPanel = new JPanel(new BorderLayout(11, 11));
                JLabel selectedFileLabel = new JLabel("Selected File");
                selectedFileLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
                selectedFileLabel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
                topPanel.add(selectedFileLabel, BorderLayout.BEFORE_LINE_BEGINS);
                selectedFileField = new JTextField();
                selectedFileField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
                selectedFileField.setAlignmentY(JComponent.CENTER_ALIGNMENT);
                topPanel.add(selectedFileField, BorderLayout.CENTER);
                JButton openButton = new JButton("Open");
                openButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
                openButton.setAlignmentY(JComponent.CENTER_ALIGNMENT);
                openButton.setMargin(new Insets(0, 0, 0, 0));
                openButton.addActionListener(this);
                topPanel.add(openButton, BorderLayout.AFTER_LINE_ENDS);
                topPanel.setAlignmentY(JComponent.CENTER_ALIGNMENT);
                this.add(scrollPane, BorderLayout.CENTER);
                this.add(topPanel, BorderLayout.NORTH);
                jargonTree.addTreeSelectionListener(this);
//                jargonTree.expandPath(new TreePath(f.getFileSystem().getHomeDirectory()));
            }
        }
    }
    
    protected JDialog createDialog(Component parent) throws HeadlessException {
        JDialog dialog = new JDialog((Frame)null, title, true);   
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
    
    public int showDialog(){
        dialog = createDialog(null);
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
                if(srbFile.isFile()){
                    selectedFileField.setText(srbFile.getPath());
                }
            }
        }
    }
    
    public void actionPerformed(ActionEvent e){
        String actionCommand = e.getActionCommand();
        if(actionCommand.equals("Open")){
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
            int ret = chooser.showDialog();
        }
        catch(Throwable e){
            e.printStackTrace();
        }
    }
    
}
