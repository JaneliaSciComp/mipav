package gov.nih.mipav.view.xcede;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JEditorPane;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Point;
import java.awt.*;

import java.awt.event.WindowListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.File;
import java.io.FileNotFoundException;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.file.xcede.*;

import org.w3c.dom.Document;

public class JXCEDEExplorer extends JFrame implements TreeSelectionListener, ComponentListener, WindowListener{
    public static final String defaultTitle = "XCEDE Schema Explorer";
    
    private ViewUserInterface userInterface;
    private JSplitPane  splitPane;
    private JScrollPane leftScrollPane;
    private JScrollPane rightScrollPane;
    private JXCEDETree tree;
    private JEditorPane inforPane;
    private XCEDEDOMToTreeModelAdapter treeModel;
    private Document document;
    /**
     * Constructs a <code>JXCEDEExplorer</code> object.
     * @param file the XCEDE file(.bxh)
     * @param root the root element of this XCEDE file.
     */
    public JXCEDEExplorer(Document document){
        this(ViewUserInterface.getReference(), defaultTitle, document);
    }
    
    public JXCEDEExplorer(ViewUserInterface userInterface, String title, Document document){
        super(title);

        this.userInterface = userInterface;
        this.userInterface.getMainFrame().addComponentListener(this);
        this.document = document;
        treeModel = new XCEDEDOMToTreeModelAdapter(document);
        tree = new JXCEDETree(document);
//        tree.createPopupMenu();
        tree.addTreeSelectionListener(this);
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
    
    public void setDocument(Document document){
        this.document = document;
        treeModel = new XCEDEDOMToTreeModelAdapter(document);
        tree.setModel(treeModel);
    }
    
    /**
     * Cleans up the memory.
     */
    protected void finalize() throws Throwable{
        System.out.println("Entering the " + JXCEDEExplorer.class.getName() + " finalize() ... ");
        tree = null;
        userInterface = null;
        leftScrollPane = null;
        rightScrollPane = null;
        splitPane = null;
        inforPane = null;
        treeModel = null;
        document = null;
        super.finalize();
    }

    /**
     * Initializes the user interface of the <code>JXCEDEExplorer</code>.
     */
    private void init(){
        inforPane = new JEditorPane("text/html", "");
        leftScrollPane = new JScrollPane(tree);
        rightScrollPane = new JScrollPane(inforPane);
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, leftScrollPane, rightScrollPane);
        splitPane.setOneTouchExpandable(true);

        this.addWindowListener(this);
        this.getContentPane().add(splitPane);
        int x = userInterface.getMainFrame().getX();
        int y = userInterface.getMainFrame().getY();
        int height = userInterface.getMainFrame().getHeight();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gs = ge.getScreenDevices();
        Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gs[0].getDefaultConfiguration());
        
        // JFrame.setPreferredSize() is not available before jvm 1.5
        //this.setPreferredSize(new Dimension(300, (int)screenSize.getHeight()-height-insets.bottom));
        this.setSize(new Dimension(300, (int)screenSize.getHeight()-height-insets.bottom));
        this.setLocation(x, y + height);

//        this.setLocation(new Point(0, ));
        pack();
        splitPane.setDividerLocation((int)(this.getSize().getHeight()*0.80));
        this.setVisible(true);
    }
    
    /**
     * The <code>TreeSelectionEvent</code> handler.
     */
    public void valueChanged(TreeSelectionEvent e) {
        Object obj = e.getPath().getLastPathComponent();
        if (obj != null) {
            XCEDEAdapterNode adapterNode = (XCEDEAdapterNode) obj;
            inforPane.setText(adapterNode.content());
            if (adapterNode.toString().equals("datarec")) {
                if (adapterNode.getOpenedImageFrame() != null && adapterNode.getOpenedImageFrame().getImageA() != null) {
                    adapterNode.getOpenedImageFrame().setVisible(true);
                    adapterNode.getOpenedImageFrame().toFront();
                } else {
                    XCEDEAdapterNode childNode = adapterNode.child("filename").child("#text");
                    ViewUserInterface.getReference().openImageFrame(
                            Preferences.getProperty("ImageDirectory")
                                    + childNode.getNodeValue(), true);
                    adapterNode.setOpenedImageFrame(ViewUserInterface.getReference().getActiveImageFrame());
                }
            }
        }

    }
    public void componentHidden(ComponentEvent e) {
    }
    public void componentMoved(ComponentEvent e) {
    }
    public void componentResized(ComponentEvent e) {
        Object source = e.getSource();
        if(source instanceof JFrame){
            JFrame frame = (JFrame)source;
            Dimension size = this.getPreferredSize();
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            
            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] gs = ge.getScreenDevices();
            int height = (int)frame.getPreferredSize().getHeight();
            Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gs[0].getDefaultConfiguration());
            this.setSize(new Dimension(300, (int)(screenSize.getHeight()- height -insets.bottom)));
            this.setLocation(frame.getLocation().x, frame.getLocation().y + height);
            splitPane.setDividerLocation((int)(this.getSize().getHeight()*0.8));
        }
    }
    public void componentShown(ComponentEvent e) {
    }
    
    public void windowActivated(WindowEvent e) {
    }
    
    public void windowClosed(WindowEvent e) {
    }
    
    public void windowClosing(WindowEvent e) {
        this.userInterface.setXCEDEExplorer(null);
        this.userInterface.getMainFrame().removeComponentListener(this);
    }
    
    public void windowDeactivated(WindowEvent e) {
    }
    
    public void windowDeiconified(WindowEvent e) {
    }
    
    public void windowIconified(WindowEvent e) {
    }
    
    public void windowOpened(WindowEvent e) {
    }
}
