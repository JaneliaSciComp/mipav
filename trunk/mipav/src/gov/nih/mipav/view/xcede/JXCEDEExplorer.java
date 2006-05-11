package gov.nih.mipav.view.xcede;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Point;
import java.awt.*;

import java.awt.event.WindowListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.File;
import java.io.FileNotFoundException;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import gov.nih.mipav.model.file.xcede.*;

public class JXCEDEExplorer extends JFrame implements TreeSelectionListener{
    public static final String defaultTitle = "XCEDE Schema Explorer";
    
    private ViewUserInterface userInterface;
    private JSplitPane  splitPane;
    private JScrollPane leftScrollPane;
    private JScrollPane rightScrollPane;
    private File file;
    private JXCEDETree tree;
    private JTable table;
    private JXCEDETableModel tableModel;
    private transient int defaultCellHeight;
    /**
     * Constructs a <code>JXCEDEExplorer</code> object.
     * @param file the XCEDE file(.bxh)
     * @param root the root element of this XCEDE file.
     */
    public JXCEDEExplorer(File file, XCEDEElement root){
        this(ViewUserInterface.getReference(), defaultTitle, file, root);
    }
    
    public JXCEDEExplorer(ViewUserInterface userInterface, String title, File file, XCEDEElement root){
        super(title);

        this.userInterface = userInterface;
        this.file = file;
        tree = new JXCEDETree(root);
//        tree.createPopupMenu();
        tree.addTreeSelectionListener(this);
        tableModel = new JXCEDETableModel(root);
        table = new JTable(tableModel);
        defaultCellHeight = table.getRowHeight();
        setRowHeight(table);
        table.setDefaultRenderer(String.class, new MultiLineTableCellRenderer());
        init();
        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
        }
        
        /**
         * Tells the swing to dispose this frame when it is closed.
         */
        this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    }
    
    /**
     * Cleans up the memory.
     */
    protected void finalize() throws Throwable{
        System.out.println("Entering the JXCEDEExplorer's finalize() ... ");
        file = null;
        JXCEDETreeModel treeModel = (JXCEDETreeModel)tree.getModel();
        XCEDEElement root = (XCEDEElement)treeModel.getRoot();
        tree.setModel(null);
        root = null;
        tree = null;
        leftScrollPane = null;
        super.finalize();
    }

    /**
     * Initializes the user interface of the <code>JXCEDEExplorer</code>.
     */
    private void init(){
        leftScrollPane = new JScrollPane(tree);
        rightScrollPane = new JScrollPane(table);
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftScrollPane, rightScrollPane);
        splitPane.setOneTouchExpandable(true);
        splitPane.setDividerLocation(180);

        this.getContentPane().add(splitPane);
        int x = userInterface.getMainFrame().getX();
        int y = userInterface.getMainFrame().getY();
        int height = userInterface.getMainFrame().getHeight();
        this.setPreferredSize(new Dimension(800, 300));
        this.setLocation(x, y + height);
        pack();
        this.setVisible(true);
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gs = ge.getScreenDevices();
        Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gs[0].getDefaultConfiguration());
        this.setLocation(new Point(0, (int)screenSize.getHeight()-300-insets.bottom));
    }
    
    /**
     * The <code>TreeSelectionEvent</code> handler.
     */
    public void valueChanged(TreeSelectionEvent e){
        Object obj = e.getPath().getLastPathComponent();
        if(obj instanceof XCEDEElement && ((XCEDEElement)obj).getLevel().equals(XCEDEElement.XCEDE_ELEMENT_DATAREC)){
            XCEDEElement datarec = (XCEDEElement)obj;
            if(datarec != null && datarec.get(XCEDEElement.XCEDE_DATAREC_IMAGEFRAME) != null && ((ViewJFrameImage)datarec.get(XCEDEElement.XCEDE_DATAREC_IMAGEFRAME)).getImageA() != null){
                ViewJFrameImage frame = (ViewJFrameImage)datarec.get(XCEDEElement.XCEDE_DATAREC_IMAGEFRAME);
                if(frame.isVisible()){
                    frame.setVisible(true);
                }
                frame.toFront();
            }else{
                XCEDEElement datarecFrag = (XCEDEElement)datarec.getChildAt(0);
                String fileName = (String)datarecFrag.get(XCEDEElement.XCEDE_ELEMENT_FILENAME);
                ViewUserInterface.getReference().openImageFrame(file.getParentFile().getAbsolutePath()+File.separator+fileName, true);
                datarec.put(XCEDEElement.XCEDE_DATAREC_IMAGEFRAME, ViewUserInterface.getReference().getActiveImageFrame());
            }
        }
        tableModel = new JXCEDETableModel((XCEDEElement)obj);
        table.setModel(tableModel);
        setRowHeight(table);
        table.updateUI();
    }
    
    private void setRowHeight(JTable table){
        JXCEDETableModel tableModel = (JXCEDETableModel)table.getModel();
        for(int i = 0; i < tableModel.getRowCount(); i++){
            String value = (String)tableModel.getValueAt(i, 1);
            if(value.indexOf("\n") >= 0){
                Pattern p = Pattern.compile("\n");
                String[] fields = p.split(value);
                table.setRowHeight(i, fields.length*defaultCellHeight);
            }
        }
        
    }
}
